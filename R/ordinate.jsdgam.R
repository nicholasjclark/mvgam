#' Latent variable ordination plots from `jsdgam` objects
#'
#' Plot an ordination of latent variables and their factor
#' loadings from a fitted `jsdgam` model. The two chosen latent
#' variables are first re-rotated via singular value
#' decomposition, then posterior medians of the variables and
#' the species' loadings are scattered in the resulting 2-D
#' space. Site labels and species loading arrows are drawn via
#' the package-wide [ggrepel] helpers; when [ggarrow] and [ggpp]
#' are installed the species arrows are rendered as tapered
#' loadings.
#'
#' @note `jsdgam()` itself is not yet on the
#'   `feature/brms-integration` branch; this function exists for
#'   forward-compatibility with the eventual jsdgam port. It
#'   relies on the same Stan parameters that an LV `mvgam` fit
#'   carries (`lv_trend[t, k]` and `Z[i, k]`) plus a few
#'   `jsdgam`-specific bookkeeping slots — calling it on an
#'   ordinary `mvgam` object will error at those slot accesses.
#'
#' @name ordinate.jsdgam
#'
#' @param object A fitted `jsdgam` object.
#' @param which_lvs Integer vector of length 2 indicating the
#'   two re-rotated latent variables to plot. Defaults to
#'   `c(1, 2)`.
#' @param biplot Logical. When `TRUE` (the default) both site
#'   scores and species loading arrows are drawn; when `FALSE`,
#'   site scores only.
#' @param alpha Proportional numeric scalar in `[0, 1]`
#'   controlling the relative scaling of latent variables vs
#'   loading coefficients.
#' @param label_sites Logical. When `TRUE`, site scores are
#'   drawn as labels (from the `unit` argument of the original
#'   `jsdgam()` call); when `FALSE`, as points only.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @author Nicholas J Clark
#'
#' @seealso [jsdgam()], [residual_cor()]
#'
#' @examples
#' \donttest{
#' # Fit a JSDGAM to the portal_data captures
#' mod <- jsdgam(
#'   formula = captures ~
#'     ndvi_ma12:series + mintemp:series + gp(time, k = 15),
#'   factor_formula = ~ -1,
#'   data = portal_data,
#'   unit = time,
#'   species = series,
#'   family = poisson(),
#'   n_lv = 2,
#'   silent = 2,
#'   chains = 2
#' )
#'
#' # Residual ordination biplot
#' ordinate(mod, alpha = 0.7)
#'
#' # Compare to a residual correlation plot
#' plot(residual_cor(mod))
#' }
#'
#' @export
ordinate <- function(object, ...) {
  UseMethod("ordinate", object)
}

#' @rdname ordinate.jsdgam
#' @method ordinate jsdgam
#' @importFrom grid arrow unit
#' @export
ordinate.jsdgam <- function(
  object,
  which_lvs = c(1L, 2L),
  biplot = TRUE,
  alpha = 0.5,
  label_sites = TRUE,
  ...
) {
  checkmate::assert_integerish(
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE
  )
  validate_proportional(alpha)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  insight::check_if_installed(
    "ggrepel",
    reason = "to adequately plot ordination scores"
  )

  n_lv <- detect_factor_n_lv(object) %||%
    stop(insight::format_error(c(
      "Object does not contain any latent dynamic factors.",
      i = "Ordination requires an LV-factor jsdgam / mvgam fit."
    )))
  if (any(which_lvs > n_lv)) {
    stop(insight::format_error(c(
      "Selected latent variables exceed the model's `n_lv`.",
      x = paste0(
        "Requested which_lvs = ",
        paste(which_lvs, collapse = ", "),
        "; available n_lv = ", n_lv, "."
      )
    )))
  }

  # Posterior medians of the factor paths and the species
  # loadings, sharing the same Stan parameter conventions used
  # by `plot_factors.mvgam` and `residual_cor`.
  per_lv <- extract_lv_trend_matrices(object, n_lv)
  lv_estimates <- do.call(cbind, lapply(per_lv, function(mat) {
    apply(mat, 2L, stats::median, na.rm = TRUE)
  }))
  Z_arr <- extract_factor_loadings_array(object, n_lv)
  if (is.null(Z_arr)) {
    stop(insight::format_error(c(
      "Could not extract Z loadings from the posterior.",
      i = paste0(
        "Ordination requires either sampled Z[i, k] in the ",
        "Stan posterior, or a fixed-loadings matrix attached ",
        "to the fitted object."
      )
    )))
  }
  lv_coefs <- apply(Z_arr, c(2L, 3L), stats::median, na.rm = TRUE)

  # SVD re-rotation of the chosen latent variables.
  # Credit for the rotation math goes to Francis Hui (BORAL).
  testcov <- tcrossprod(lv_estimates, lv_coefs)
  do_svd <- svd(testcov, n_lv, n_lv)
  choose_lvs <- scale(
    do_svd$u *
      matrix(
        do_svd$d[seq_len(n_lv)]^alpha,
        nrow = NROW(lv_estimates),
        ncol = n_lv,
        byrow = TRUE
      ),
    center = TRUE, scale = FALSE
  )
  choose_lv_coefs <- scale(
    do_svd$v *
      matrix(
        do_svd$d[seq_len(n_lv)]^(1 - alpha),
        nrow = NROW(lv_coefs),
        ncol = n_lv,
        byrow = TRUE
      ),
    center = TRUE, scale = FALSE
  )

  sp_dat <- data.frame(choose_lv_coefs)[, which_lvs]
  colnames(sp_dat) <- c("x", "y")
  site_dat <- data.frame(choose_lvs)[, which_lvs]
  colnames(site_dat) <- c("x", "y")
  plot_dat <- rbind(sp_dat, site_dat)

  # Species and site labels: resolved via the canonical helpers
  # so the surface degrades cleanly when the jsdgam slots
  # haven't been populated.
  sp_names <- resolve_series_info(object)$series_levels
  unit_name <- attr(object$model_data, "prepped_trend_model")$unit
  site_names <- unique(object$obs_data[[unit_name]])

  base_plot <- ggplot2::ggplot(plot_dat, ggplot2::aes(x, y)) +
    ggplot2::labs(
      x = paste("Latent variable", which_lvs[1L]),
      y = paste("Latent variable", which_lvs[2L])
    )

  if (label_sites) {
    p <- base_plot +
      mvgam_repel_layer(
        data = site_dat,
        mapping = ggplot2::aes(label = site_names),
        type = "text",
        alpha = 0.75, size = 3, max.overlaps = 20,
        colour = "grey40", segment.color = NA
      ) +
      ggplot2::geom_point(
        data = site_dat,
        pch = 21, fill = "grey20", colour = "white"
      )
  } else {
    p <- base_plot +
      ggplot2::geom_point(
        data = site_dat,
        pch = 21, fill = "grey20", colour = "white"
      )
  }

  if (biplot) {
    p <- p + ordinate_biplot_layers(sp_dat, sp_names)
  }
  p + mvgam_theme()
}


# Internal: species-loading arrow layers for the biplot mode.
# Prefers `ggarrow::geom_arrow` (tapered arrows weighted by
# loading magnitude) when `ggarrow` + `ggpp` are installed;
# falls back to plain `geom_segment` arrows otherwise.
#'@noRd
ordinate_biplot_layers <- function(sp_dat, sp_names) {
  has_arrow <- requireNamespace("ggarrow", quietly = TRUE) &&
    requireNamespace("ggpp", quietly = TRUE)
  if (has_arrow) {
    sp_dat$group <- paste("gr", seq_len(NROW(sp_dat)))
    sp_arrow_dat <- do.call(
      rbind,
      lapply(seq_along(sp_names), function(s) {
        data.frame(
          x = seq(0, sp_dat$x[s], length.out = 20L),
          y = seq(0, sp_dat$y[s], length.out = 20L),
          group = sp_dat$group[s]
        )
      })
    )
    sp_arrow_dat$lw <- abs(sp_arrow_dat$x) + abs(sp_arrow_dat$y)
    list(
      ggarrow::geom_arrow(
        data = sp_arrow_dat,
        ggplot2::aes(
          x = x, y = y, group = group, linewidth = lw
        ),
        colour = "darkred", stroke_colour = "white",
        stroke_width = 0.1, alpha = 0.5, show.legend = FALSE
      ),
      ggplot2::scale_linewidth(range = c(0.45, 1.75)),
      mvgam_repel_layer(
        data = sp_dat,
        mapping = ggplot2::aes(label = sp_names),
        type = "label",
        color = "darkred", box.padding = 0.1, label.size = 0.1,
        alpha = 0.75, max.overlaps = 20, segment.color = NA,
        position = ggpp::position_nudge_center(
          0.025, 0.025, 0, 0
        )
      )
    )
  } else {
    list(
      ggplot2::geom_segment(
        data = sp_dat,
        ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
        arrow = grid::arrow(
          length = grid::unit(0.1, "cm"), type = "closed"
        ),
        alpha = 0.5, color = "darkred"
      ),
      mvgam_repel_layer(
        data = sp_dat,
        mapping = ggplot2::aes(label = sp_names),
        type = "label",
        color = "darkred", box.padding = 0.1, label.size = 0.1,
        alpha = 0.75, max.overlaps = 20
      )
    )
  }
}
