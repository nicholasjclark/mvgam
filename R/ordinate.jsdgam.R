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
#' @note `jsdgam()` itself is not yet ported on this branch;
#'   this function exists for forward-compatibility with the
#'   eventual jsdgam port. It relies on the same Stan parameters
#'   that an LV `mvgam` fit carries — `lv_trend_tilde[t, k]` and
#'   `Z_tilde[i, k]` for free-Z factor models, or `lv_trend` and
#'   `Z` for partial-Z fits — plus a few `jsdgam`-specific
#'   bookkeeping slots. Calling it on an ordinary `mvgam` object
#'   will error at those slot accesses.
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

#' Internal: posterior-median LV trends and Z loadings from a
#' fitted LV-factor `mvgam` / `jsdgam`. Errors if the fit has no
#' latent factors or no Z reachable from the posterior.
#'
#' @noRd
ordinate_extract_medians <- function(object) {
  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv)) {
    stop(insight::format_error(c(
      "Object does not contain any latent dynamic factors.",
      i = "Ordination requires an LV-factor mvgam / jsdgam fit."
    )))
  }
  per_lv <- extract_lv_trend_matrices(object, n_lv)
  lv_estimates <- do.call(cbind, lapply(per_lv, function(mat) {
    apply(mat, 2L, stats::median, na.rm = TRUE)
  }))
  Z_arr <- extract_factor_loadings_array(object, n_lv)
  if (is.null(Z_arr)) {
    stop(insight::format_error(c(
      "Could not extract Z loadings from the posterior.",
      i = paste0(
        "Ordination requires either sampled `Z_tilde[i, k]` ",
        "or `Z[i, k]` in the Stan posterior, or a fixed-",
        "loadings matrix attached to the fitted object."
      )
    )))
  }
  lv_coefs <- apply(Z_arr, c(2L, 3L), stats::median, na.rm = TRUE)
  list(lv_estimates = lv_estimates, lv_coefs = lv_coefs,
        n_lv = n_lv)
}


#' Internal: rotation dispatcher for ordinate methods.
#'
#' Returns `list(scores, loadings, n_lv, rotation, rotmat)` where:
#' - `scores` is `[n_time, n_lv]` site scores in the rotated basis
#' - `loadings` is `[n_series, n_lv]` series loadings in the same
#'   rotated basis
#' - `rotmat` is the rotation matrix used (NULL for `"svd"` and
#'   `"none"` since those don't apply a single rotation matrix to
#'   the input arrays). For varimax / promax `rotmat` is the
#'   `stats::varimax()`/`stats::promax()` rotation.
#'
#' Branches:
#' - `"svd"`: BORAL (Hui 2016) re-rotation. SVD of the
#'   `LV %*% t(Z)` cross-product, then split the singular values
#'   across factor paths (`alpha`) and loadings (`1 - alpha`).
#'   Axes are variance-ordered; `which_lvs = c(1, 2)` picks the
#'   most informative pair.
#' - `"varimax"`: orthogonal rotation maximising loading sparsity.
#'   `alpha` is ignored. Rotation invariance of `LV %*% t(Z)` is
#'   preserved (apply R to Z and R to LV; for orthogonal R,
#'   `R^{-T} = R`).
#' - `"promax"`: oblique extension of varimax. `alpha` ignored.
#' - `"none"`: pass through unrotated medians. `alpha` ignored.
#'
#' @noRd
ordinate_factor_components <- function(object, alpha, rotation) {
  medians <- ordinate_extract_medians(object)
  lv_estimates <- medians$lv_estimates
  lv_coefs <- medians$lv_coefs
  n_lv <- medians$n_lv
  out <- switch(
    rotation,
    "svd" = ordinate_svd_rotate(lv_estimates, lv_coefs, alpha,
                                  n_lv),
    "varimax" = ordinate_orth_rotate(
      lv_estimates, lv_coefs, n_lv, method = "varimax"
    ),
    "promax" = ordinate_orth_rotate(
      lv_estimates, lv_coefs, n_lv, method = "promax"
    ),
    "none" = list(
      scores = scale(lv_estimates, center = TRUE, scale = FALSE),
      loadings = scale(lv_coefs, center = TRUE, scale = FALSE),
      rotmat = NULL
    )
  )
  out$n_lv <- n_lv
  out$rotation <- rotation
  out
}


#' Internal: BORAL SVD rotation branch. See
#' `ordinate_factor_components()` for the broader contract.
#'
#' @noRd
ordinate_svd_rotate <- function(lv_estimates, lv_coefs, alpha,
                                  n_lv) {
  testcov <- tcrossprod(lv_estimates, lv_coefs)
  do_svd <- svd(testcov, n_lv, n_lv)
  scores <- scale(
    do_svd$u *
      matrix(
        do_svd$d[seq_len(n_lv)]^alpha,
        nrow = NROW(lv_estimates),
        ncol = n_lv,
        byrow = TRUE
      ),
    center = TRUE, scale = FALSE
  )
  loadings <- scale(
    do_svd$v *
      matrix(
        do_svd$d[seq_len(n_lv)]^(1 - alpha),
        nrow = NROW(lv_coefs),
        ncol = n_lv,
        byrow = TRUE
      ),
    center = TRUE, scale = FALSE
  )
  list(scores = scores, loadings = loadings, rotmat = NULL)
}


#' Internal: varimax / promax rotation branch. Applies the
#' rotation matrix returned by `stats::varimax()` / `stats::promax()`
#' to both the loadings AND the latent paths, preserving the
#' fitted `LV %*% t(Z)` product. For orthogonal varimax,
#' `R^{-T} = R`. For oblique promax, the inverse-transpose is
#' computed explicitly so the product invariance holds.
#'
#' @noRd
ordinate_orth_rotate <- function(lv_estimates, lv_coefs, n_lv,
                                   method) {
  if (n_lv < 2L) {
    stop(insight::format_error(c(
      paste0("Rotation '", method, "' requires at least 2 factors."),
      x = paste0("Got n_lv = ", n_lv, "."),
      i = paste0(
        "Refit with n_lv >= 2, or use rotation = 'none' / 'svd'."
      )
    )))
  }
  rot <- switch(
    method,
    "varimax" = stats::varimax(lv_coefs, normalize = TRUE),
    "promax" = stats::promax(lv_coefs)
  )
  rotmat <- rot$rotmat
  loadings_rot <- lv_coefs %*% rotmat
  # Preserve LV %*% t(Z): for orthogonal R, t(solve(t(R))) = R;
  # promax's rotmat is oblique so we solve directly.
  lv_transform <- if (method == "varimax") {
    rotmat
  } else {
    t(solve(t(rotmat)))
  }
  scores_rot <- lv_estimates %*% lv_transform
  list(
    scores = scale(scores_rot, center = TRUE, scale = FALSE),
    loadings = scale(loadings_rot, center = TRUE, scale = FALSE),
    rotmat = rotmat
  )
}


#' Internal: build the ordination biplot from the SVD components
#' and caller-supplied site / species labels. Every ordinate.*
#' method ends here so the layered ggplot output is identical
#' across object classes.
#'
#' @noRd
ordinate_build_plot <- function(svd_comp, which_lvs, biplot,
                                 label_sites, site_names,
                                 species_names) {
  if (any(which_lvs > svd_comp$n_lv)) {
    suggestion <- if (svd_comp$n_lv == 1L) {
      "Refit with n_lv >= 2 or inspect factors via 'plot_factors()'."
    } else {
      paste0(
        "Choose two factors from 1..", svd_comp$n_lv, "."
      )
    }
    stop(insight::format_error(c(
      "Selected latent variables exceed the model's `n_lv`.",
      x = paste0(
        "Requested which_lvs = ",
        paste(which_lvs, collapse = ", "),
        "; available n_lv = ", svd_comp$n_lv, "."
      ),
      i = suggestion
    )))
  }
  if (length(site_names) != NROW(svd_comp$scores)) {
    stop(insight::format_error(c(
      "Site-label count does not match factor-score row count.",
      x = paste0(
        "labels: ", length(site_names),
        ", lv_trend rows: ", NROW(svd_comp$scores), "."
      ),
      i = paste0(
        "Often a symptom of imputed time points in 'obs_data'. ",
        "Either trim to the training grid or supply matching ",
        "labels manually."
      )
    )))
  }
  sp_dat <- data.frame(svd_comp$loadings)[, which_lvs]
  colnames(sp_dat) <- c("x", "y")
  site_dat <- data.frame(svd_comp$scores)[, which_lvs]
  colnames(site_dat) <- c("x", "y")
  plot_dat <- rbind(sp_dat, site_dat)

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
    p <- p + ordinate_biplot_layers(sp_dat, species_names)
  }
  p <- p + mvgam_theme()
  # Attach rotated arrays as a structured attribute so power users
  # can extract Z / LV in the rotated basis without re-running.
  attr(p, "rotation") <- list(
    method = svd_comp$rotation,
    n_lv = svd_comp$n_lv,
    scores = svd_comp$scores,
    loadings = svd_comp$loadings,
    rotmat = svd_comp$rotmat
  )
  p
}


#' @rdname ordinate.jsdgam
#' @param rotation Character. Post-hoc rotation of the
#'   posterior-median LV trends and Z loadings before plotting.
#'   One of:
#'   \describe{
#'     \item{`"svd"` (default)}{BORAL convention: SVD of
#'       `LV %*% t(Z)` re-orders axes by singular-value variance.
#'       Use `alpha` to split the variance between site scores and
#'       loadings.}
#'     \item{`"varimax"`}{Orthogonal rotation maximising loading
#'       sparsity. Each series tends to load strongly on one
#'       factor and near-zero on others. `alpha` is ignored.
#'       Requires `n_lv >= 2`.}
#'     \item{`"promax"`}{Oblique extension of varimax (allows
#'       correlated factors). `alpha` ignored.}
#'     \item{`"none"`}{No rotation. Plot raw posterior-median LV
#'       and Z, centred. `alpha` ignored.}
#'   }
#'   For varimax / promax the axes are NOT variance-ordered, so
#'   the choice of `which_lvs` matters in a different way than
#'   under SVD: any pair of rotated factors is a valid pair to
#'   plot.
#' @method ordinate jsdgam
#' @importFrom grid arrow unit
#' @export
ordinate.jsdgam <- function(
  object,
  which_lvs = c(1L, 2L),
  biplot = TRUE,
  alpha = 0.5,
  rotation = c("svd", "varimax", "promax", "none"),
  label_sites = TRUE,
  ...
) {
  checkmate::assert_integerish(
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE
  )
  validate_proportional(alpha)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  rotation <- match.arg(rotation)
  insight::check_if_installed(
    "ggrepel",
    reason = "to adequately plot ordination scores"
  )

  comp <- ordinate_factor_components(object, alpha, rotation)
  sp_names <- resolve_series_info(object)$series_levels
  unit_name <- attr(object$model_data, "prepped_trend_model")$unit
  site_names <- unique(object$obs_data[[unit_name]])
  ordinate_build_plot(
    comp, which_lvs, biplot, label_sites,
    site_names = site_names, species_names = sp_names
  )
}


#' Latent variable ordination plot from a fitted `mvgam`
#'
#' Generalises [ordinate.jsdgam()] to any LV-factor `mvgam` fit
#' (default sampled-Z factor models OR fixed Z via `trend_map`).
#' "Sites" are training time points, "species" are the model's
#' series. The SVD re-rotation and the biplot layout are
#' identical to the `jsdgam` method.
#'
#' @param object A fitted `mvgam` object with latent dynamic
#'   factors (`n_lv` set on an `AR()` / `RW()` / `VAR()` /
#'   `ZMVN()` trend constructor, or a `trend_map` argument).
#' @param which_lvs Integer vector of length 2 indicating the
#'   two re-rotated latent variables to plot. Defaults to
#'   `c(1, 2)`.
#' @param biplot Logical. When `TRUE` (default) both site scores
#'   and series loading arrows are drawn; when `FALSE`, site
#'   scores only.
#' @param alpha Proportional numeric scalar in `[0, 1]`
#'   controlling how the SVD singular value variance is split
#'   between site scores and series loadings. `alpha = 0.5`
#'   (default) splits equally; distances between sites and
#'   between series are approximately comparably scaled. Use
#'   `alpha` close to `1` to emphasise separation between time
#'   points, or close to `0` to emphasise separation between
#'   series. Matches the BORAL convention. Ignored for
#'   `rotation` values other than `"svd"`.
#' @param rotation Character. Post-hoc rotation of the
#'   posterior-median LV trends and Z loadings before plotting.
#'   One of:
#'   \describe{
#'     \item{`"svd"` (default)}{BORAL convention: SVD of
#'       `LV %*% t(Z)` re-orders axes by singular-value variance.
#'       Most informative when `n_lv > 2` and you want the two
#'       leading gradients.}
#'     \item{`"varimax"`}{Orthogonal rotation maximising loading
#'       sparsity (each series loads strongly on one factor and
#'       near-zero on others). Use this when you want
#'       interpretable factor "names" rather than variance-
#'       ordered axes. Requires `n_lv >= 2`. `alpha` ignored.}
#'     \item{`"promax"`}{Oblique extension of varimax that allows
#'       correlated rotated factors. `alpha` ignored.}
#'     \item{`"none"`}{Skip rotation entirely; plot raw
#'       posterior-median LV / Z, centred. The axes correspond
#'       directly to the Stan parameters
#'       `lv_trend_tilde[t, k]` and `Z_tilde[i, k]` (free-Z
#'       factor models) or `lv_trend` / `Z` (partial-Z fits),
#'       as discussed in Details. `alpha` ignored.}
#'   }
#' @param label_sites Logical. When `TRUE`, site scores are
#'   drawn as text labels (the training time values); when
#'   `FALSE`, as points only.
#' @param ... Ignored.
#'
#' @return A `ggplot` object. The returned object carries a
#'   `"rotation"` attribute (a list with `method`, `n_lv`,
#'   `scores`, `loadings`, `rotmat`) so users can extract the
#'   rotated factor scores and loadings without re-running
#'   `ordinate()`. Access via `attr(p, "rotation")`.
#'
#' @details
#' For sampled-Z factor models the Stan model samples an
#' unconstrained loading matrix `Z` under a structured prior and
#' then applies a thin-QR decomposition in generated quantities
#' to produce a lower-triangular, positive-diagonal `Z_tilde`
#' together with the rotated factor paths `lv_trend_tilde`. The
#' identified `Z_tilde` and `lv_trend_tilde` are what
#' `ordinate()` reads. Under `rotation = "svd"` (default) the
#' axes are SVD-rotated ordination gradients, NOT the original
#' Stan factors. Under `rotation = "varimax"` / `"promax"` the
#' axes are rotated for sparsity rather than variance; the
#' identified lower-triangular pattern is preserved in the
#' underlying fit but not visible in the plot. Use
#' `rotation = "none"` (or [plot_factors()]) to view the
#' un-rotated identified factors directly. See Heaps and Jermyn
#' (2024) for the structured-prior + post-hoc QR framework.
#'
#' For partial-Z fits (free entries marked `NA` in `trend_map`)
#' no QR rotation is applied, since rotating would overwrite
#' the user-supplied entries on `Z`. Ordination reads `Z` and
#' `lv_trend` directly in those fits.
#'
#' For fully fixed-Z fits supplied via `trend_map`, ANY
#' non-`"none"` rotation discards the structural loadings the
#' user encoded. A one-time warning is emitted when called on
#' a fixed-Z fit; `plot_factors(fit)` shows the raw user-
#' supplied loadings.
#'
#' @section Known limitations:
#' \itemize{
#'   \item Posterior-median plug-in: the rotation operates on
#'     per-element medians of `lv_trend` and `Z`. Uncertainty in
#'     site scores and loading positions is not propagated to
#'     the biplot.
#'   \item Sign indeterminacy: SVD and varimax columns are sign-
#'     arbitrary, so comparing ordinations from independently-
#'     fit models may require a manual sign-flip alignment.
#' }
#'
#' @author Nicholas J Clark
#'
#' @seealso [ordinate.jsdgam()], [plot_factors()],
#'   [residual_cor()]
#'
#' @method ordinate mvgam
#' @export
ordinate.mvgam <- function(
  object,
  which_lvs = c(1L, 2L),
  biplot = TRUE,
  alpha = 0.5,
  rotation = c("svd", "varimax", "promax", "none"),
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

  # Fixed-Z fits: the SVD re-rotation discards the structural
  # loading pattern the user encoded via trend_map, so the biplot
  # cannot be read as a depiction of those constraints. Warn once
  # per session to keep the message visible without spamming.
  if (!is.null(object$trend_metadata$fixed_Z) &&
      !identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      message = c(
        paste0(
          "SVD re-rotation in 'ordinate()' overrides the ",
          "structural loadings supplied via 'trend_map'."
        ),
        i = paste0(
          "The biplot shows ordination gradients, not the ",
          "user-specified factor structure. Use ",
          "'plot_factors()' to see the raw fixed loadings."
        )
      ),
      .frequency = "once",
      .frequency_id = "mvgam_ordinate_fixed_Z"
    )
  }

  svd_comp <- ordinate_svd_components(object, alpha)
  series_info <- resolve_series_info(object)
  species_names <- series_info$series_levels
  data <- object$obs_data %||% object$data
  site_names <- if (!is.null(data) && "time" %in% names(data)) {
    sort(unique(data$time))
  } else {
    # Last-resort fallback: 1..n_time bare integer labels.
    seq_len(NROW(svd_comp$scores))
  }
  ordinate_build_plot(
    svd_comp, which_lvs, biplot, label_sites,
    site_names = site_names, species_names = species_names
  )
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
