#' Compare posterior loadings between two latent-factor fits
#'
#' @description
#' Side-by-side comparison of the posterior-median species loadings
#' (`Z_tilde`) from two latent-factor `mvgam` / `jsdgam` fits on
#' the same set of species. The two posteriors are identified only
#' up to rotation of the factor basis, so a direct overlay would
#' be misleading without an alignment step. `compare_loadings()`
#' applies the orthogonal Procrustes solution (Schoenemann 1966)
#' that rotates `fit_b`'s posterior-median `Z_tilde` to best match
#' `fit_a`'s in least-squares sense, then either overlays both
#' onto a single biplot with displacement segments (default) or
#' splits them into two facets sharing axes.
#'
#' Useful when teaching the effect of a structured loadings prior:
#' fit one model with `loadings_prior = NULL` and a second with
#' `traits =` or `phylo =`, then plot the comparison to show how
#' the trait or phylogeny similarity reshapes the latent positions.
#'
#' @param fit_a,fit_b Two fitted `mvgam` / `jsdgam` objects, each
#'   with `n_lv >= 2` and the same species set in the same factor
#'   order.
#' @param labels Optional length-2 character vector of fit labels
#'   shown in the legend / facet strips. Defaults to
#'   `c("fit_a", "fit_b")`.
#' @param which_lvs Integer vector of length 2 indicating which
#'   two latent factors to plot. Defaults to `c(1, 2)`.
#' @param align Logical. When `TRUE` (default) `fit_b`'s
#'   posterior-median `Z_tilde` is rotated onto `fit_a`'s basis via
#'   orthogonal Procrustes before plotting. Set to `FALSE` to keep
#'   both fits in their own QR-identified basis (rarely useful
#'   except for sanity checks on rotation invariance).
#' @param facet Logical. When `FALSE` (default) both fits are
#'   overlaid on the same axes in different colours, with
#'   segments connecting the same species across fits. When
#'   `TRUE`, fits are drawn as side-by-side facets on shared axes.
#' @param label_species Character. Controls which species are
#'   labelled. `"all"` (default) labels every species; `"shift"`
#'   labels the top 10 species by post-alignment displacement
#'   between fits; `"none"` draws unlabelled points only.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @details
#' The orthogonal Procrustes rotation is the closed-form solution
#' \deqn{R = U V^T} where \eqn{U \Sigma V^T = SVD(Z_a^T Z_b)} and
#' \eqn{Z_a, Z_b} are the posterior-median loading matrices
#' restricted to `which_lvs`. The aligned `Z_b` is `Z_b %*% t(R)`.
#' Alignment is a sufficient but not unique solution to factor
#' rotation indeterminacy; alternative alignments (varimax on the
#' joint matrix, sign-flip only) are possible but typically less
#' interpretable for the prior-comparison use case here.
#'
#' Both fits must report the same species in the same factor
#' order; the function errors otherwise. To compare fits with
#' different species sets, subset before fitting.
#'
#' @author Nicholas J Clark
#'
#' @seealso [ordinate.mvgam()], [ordinate.jsdgam()],
#'   [residual_cor()], [active_factors()]
#'
#' @references
#' Schoenemann PH (1966). A generalized solution of the orthogonal
#' Procrustes problem. *Psychometrika* 31, 1-10.
#' \doi{10.1007/BF02289451}
#'
#' Heaps SE and Jermyn IH (2024). Structured prior distributions
#' for the covariance matrix in latent factor models. *Statistics
#' and Computing* 34, 143. \doi{10.1007/s11222-024-10454-0}
#'
#' @export
compare_loadings <- function(
  fit_a, fit_b,
  labels = c("fit_a", "fit_b"),
  which_lvs = c(1L, 2L),
  align = TRUE,
  facet = FALSE,
  label_species = c("all", "shift", "none"),
  ...
) {
  checkmate::assert_class(fit_a, "mvgam")
  checkmate::assert_class(fit_b, "mvgam")
  checkmate::assert_character(labels, len = 2L, any.missing = FALSE)
  checkmate::assert_integerish(
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE
  )
  checkmate::assert_flag(align)
  checkmate::assert_flag(facet)
  label_species <- match.arg(label_species)
  insight::check_if_installed(
    "ggrepel", reason = "to label species without overplotting"
  )

  z_a <- extract_median_Z(fit_a)
  z_b <- extract_median_Z(fit_b)
  validate_loadings_alignment(z_a, z_b, which_lvs)

  z_a <- z_a[, which_lvs, drop = FALSE]
  z_b <- z_b[, which_lvs, drop = FALSE]
  if (align) {
    z_b <- procrustes_rotate(z_a, z_b)
  }

  species_names <- rownames(z_a)
  fit_levels <- as.character(labels)
  shift <- sqrt(rowSums((z_a - z_b)^2))
  shift_rank <- rank(-shift, ties.method = "first")
  to_label <- switch(
    label_species,
    all   = rep(TRUE, length(species_names)),
    shift = shift_rank <= 10L,
    none  = rep(FALSE, length(species_names))
  )

  df <- data.frame(
    species = rep(species_names, 2L),
    fit     = factor(
      rep(fit_levels, each = length(species_names)),
      levels = fit_levels
    ),
    LV1   = c(z_a[, 1L], z_b[, 1L]),
    LV2   = c(z_a[, 2L], z_b[, 2L]),
    label = rep(ifelse(to_label, species_names, ""), 2L),
    shift = rep(shift, 2L)
  )

  pal <- mvgam_categorical_palette(2L)
  axis_labs <- c(
    sprintf("Z loading on LV%d", which_lvs[1L]),
    sprintf("Z loading on LV%d", which_lvs[2L])
  )

  if (facet) {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$LV1, y = .data$LV2, colour = .data$fit)
    ) +
      ggplot2::geom_point(size = 2.2, alpha = 0.85) +
      ggplot2::facet_wrap(~ .data$fit) +
      ggplot2::scale_colour_manual(values = pal, guide = "none")
  } else {
    seg_df <- data.frame(
      species = species_names,
      x = z_a[, 1L], y = z_a[, 2L],
      xend = z_b[, 1L], yend = z_b[, 2L]
    )
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = seg_df,
        mapping = ggplot2::aes(
          x = .data$x, y = .data$y,
          xend = .data$xend, yend = .data$yend
        ),
        colour = "grey70", linewidth = 0.3
      ) +
      ggplot2::geom_point(
        data = df,
        mapping = ggplot2::aes(
          x = .data$LV1, y = .data$LV2, colour = .data$fit
        ),
        size = 2.2, alpha = 0.85
      ) +
      ggplot2::scale_colour_manual(values = pal, name = NULL)
  }

  if (label_species != "none") {
    p <- p + ggrepel::geom_text_repel(
      data = df[df$label != "", , drop = FALSE],
      mapping = ggplot2::aes(
        x = .data$LV1, y = .data$LV2,
        label = .data$label, colour = .data$fit
      ),
      size = 2.6, max.overlaps = Inf,
      show.legend = FALSE
    )
  }

  p +
    ggplot2::labs(
      x = axis_labs[1L], y = axis_labs[2L],
      title = sprintf(
        "Posterior-median species loadings: %s vs %s",
        labels[1L], labels[2L]
      ),
      subtitle = if (align) {
        "Procrustes-aligned to a shared basis"
      } else {
        "No alignment applied (raw QR-identified bases)"
      }
    ) +
    mvgam_theme()
}


# Posterior-median species loadings matrix [n_species x n_lv] from
# the QR-identified `Z_tilde` slice of the posterior. Errors with
# a friendly message when the fit has no factor model.
#'@noRd
extract_median_Z <- function(object) {
  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv) || n_lv < 1L) {
    stop(insight::format_error(c(
      "compare_loadings() requires a latent-factor fit (n_lv > 0).",
      i = paste0(
        "Refit with `n_lv > 0` (jsdgam(...) or mvgam(..., trend_map = ...))",
        " or compare existing fits that both carry a factor model."
      )
    )))
  }
  n_series <- loading_series_count(object)
  draws_mat <- posterior::as_draws_matrix(object$fit)
  # `resolve_Z_loadings()` answers for both kinds of factor fit: the
  # sampled loadings where they are free, and the `trend_map` matrix
  # broadcast across draws where they were supplied as data. Reading
  # the posterior directly found no loadings at all on a fixed-Z fit,
  # because there are none to find.
  z_arr <- resolve_Z_loadings(
    object,
    draws_mat,
    n_series = as.integer(n_series),
    n_lv = as.integer(n_lv)
  )
  med <- apply(z_arr, c(2L, 3L), stats::median)
  rownames(med) <- resolve_series_info(object)$series_levels
  med
}


# Cross-fit shape compatibility check. Errors when the two fits
# have different species sets or insufficient factors for the
# requested `which_lvs`.
#'@noRd
validate_loadings_alignment <- function(z_a, z_b, which_lvs) {
  if (!identical(rownames(z_a), rownames(z_b))) {
    stop(insight::format_error(c(
      "Species do not align between the two fits.",
      x = paste0(
        "fit_a species: ",
        paste(head(rownames(z_a), 3), collapse = ", "), "..."
      ),
      x = paste0(
        "fit_b species: ",
        paste(head(rownames(z_b), 3), collapse = ", "), "..."
      ),
      i = "Refit on the same species set in the same factor order."
    )))
  }
  n_lv_min <- min(ncol(z_a), ncol(z_b))
  if (max(which_lvs) > n_lv_min) {
    stop(insight::format_error(c(
      "Both fits must have at least `max(which_lvs)` latent factors.",
      x = sprintf(
        "fit_a n_lv = %d, fit_b n_lv = %d, requested max = %d.",
        ncol(z_a), ncol(z_b), max(which_lvs)
      ),
      i = "Pick a smaller pair via `which_lvs` or refit with larger n_lv."
    )))
  }
  invisible(NULL)
}


# Orthogonal Procrustes rotation aligning `z_b` to `z_a`.
# Returns the rotated `z_b` (NOT the rotation matrix).
#'@noRd
procrustes_rotate <- function(z_a, z_b) {
  m <- crossprod(z_a, z_b)
  s <- svd(m)
  r <- s$u %*% t(s$v)
  z_b %*% t(r)
}
