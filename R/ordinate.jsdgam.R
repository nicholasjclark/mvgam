#' Latent variable ordination plots from `jsdgam` objects
#'
#' Plot an ordination of latent variables and their factor
#' loadings from a fitted `jsdgam` model. The two chosen latent
#' variables are first re-rotated via singular value
#' decomposition, then posterior medians of the variables and
#' the species' loadings are scattered in the resulting 2-D
#' space. Site labels and species loading arrows are drawn via
#' the package-wide [ggrepel] helpers; when \pkg{ggarrow} and \pkg{ggpp}
#' are installed the species arrows are rendered as tapered
#' loadings.
#'
#' @note This method needs a fitted factor model. It reads the
#'   identified latent variables and their loadings from the
#'   posterior (`lv_trend_tilde[t, k]` and `Z_tilde[i, k]` for
#'   free-loading factor models, or `lv_trend` and `Z` when the
#'   loadings follow a fixed pattern). For an `mvgam()` fit that
#'   included latent factors, use [ordinate.mvgam()].
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
#' @param traits Optional trait overlay. One of:
#'   \describe{
#'     \item{`NULL` (default)}{No trait arrows; the previous
#'       biplot behaviour.}
#'     \item{`"auto"`}{If the fit was trait-informed via
#'       `jsdgam(traits = ...)`, pull the trait frame back off
#'       the fit and use it as the overlay. Emits a one-time
#'       warning and skips the overlay if no traits are found.}
#'     \item{a `data.frame` or `matrix`}{Per-species trait values
#'       (one row per species; columns are traits). Rows are
#'       aligned via `rownames(traits)` when set; otherwise rows
#'       must already match the fit's series-level order. Non-
#'       numeric columns are ignored.}
#'   }
#'   When a non-NULL frame is in play, each numeric trait is
#'   regressed on the rotated species loadings to give its
#'   direction in the LV space; the resulting arrows are overlaid
#'   on the biplot in steelblue. The regression-on-loadings recipe
#'   works whether the fit was trait-informed via
#'   `jsdgam(traits = ...)` or not, so it can either visualise the
#'   structural trait gradient (informed) or serve as a post-hoc
#'   overlay (naive). Only meaningful when `biplot = TRUE`.
#' @param trait_arrow_scale Positive numeric. Visual scaling
#'   factor for trait-arrow lengths. The default `1` places the
#'   longest trait arrow at the same radius as the longest species
#'   arrow; values `> 1` lengthen, values `< 1` shorten.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @author Nicholas J Clark
#'
#' @seealso [jsdgam()], [residual_cor()],
#'   [plot.mvgam()] (especially `type = "factors"` and
#'   `type = "latent_state"`)
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 4L,
#'   n_sites   = 50L,
#'   n_visits  = 4L,
#'   type      = 2L
#' )
#' mod <- jsdgam(
#'   formula        = bf(y ~ env, p ~ tod_c),
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   family         = occ(),
#'   n_lv           = 2L,
#'   chains         = 2,
#'   silent         = 2
#' )
#'
#' # SVD biplot of site + species scores in the two-factor space.
#' ordinate(mod)
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


#' Internal: try to pull the trait frame the user passed to
#' `jsdgam(traits = ...)` back off the fitted object. Searches a
#' short list of known persistence paths and returns the first
#' non-NULL match; returns `NULL` when the fit wasn't trait-
#' informed (no `loadings_prior$features` payload anywhere).
#'
#' @noRd
ordinate_extract_fit_traits <- function(object) {
  ts <- object$mv_spec$trend_specs
  # Single-trend (typical jsdgam ZMVN) shape: trend_specs is one
  # mvgam_trend object with the loadings_prior attached directly.
  if (inherits(ts, "mvgam_trend") && !is.null(ts$loadings_prior)) {
    f <- ts$loadings_prior$features
    if (!is.null(f)) return(f)
  }
  # Multi-trend shape: list of mvgam_trend objects. Walk them and
  # return the first features payload encountered.
  if (is.list(ts) && !inherits(ts, "mvgam_trend")) {
    for (spec in ts) {
      f <- spec$loadings_prior$features
      if (!is.null(f)) return(f)
    }
  }
  NULL
}


#' Internal: per-trait arrow coordinates in the rotated LV space.
#' Each numeric trait column is regressed on the species' rotated
#' loadings for `which_lvs`; the slope pair `(b_lv1, b_lv2)` is
#' the trait's direction in the biplot. Arrow lengths are rescaled
#' so the longest trait arrow matches `arrow_scale` times the
#' longest species-loading radius, keeping both layers visually
#' comparable. Returns `NULL` when `traits` is `NULL`, when no
#' numeric columns remain, or when the alignment fails.
#'
#' The same regression-on-loadings recipe works whether the fit
#' was trait-informed (`jsdgam(traits = ...)`) or not. For trait-
#' informed fits the arrows visualise the trait gradient already
#' encoded in `factor_formula`; for naive fits they are a post-hoc
#' overlay showing which direction each trait grows in LV space.
#'
#' @noRd
ordinate_trait_arrows <- function(traits, loadings_2d,
                                  species_names, arrow_scale) {
  if (is.null(traits)) return(NULL)
  if (!is.data.frame(traits) && !is.matrix(traits)) {
    stop(insight::format_error(c(
      "Argument 'traits' must be a data.frame or matrix.",
      i = paste0(
        "Rows correspond to species; columns are trait values. ",
        "Either set 'rownames(traits)' to the fit's species ",
        "labels, or supply one row per species in series_levels ",
        "order."
      )
    )))
  }
  traits <- as.data.frame(traits)
  if (!is.null(rownames(traits)) &&
      any(species_names %in% rownames(traits))) {
    missing_sp <- setdiff(species_names, rownames(traits))
    if (length(missing_sp) > 0L) {
      stop(insight::format_error(c(
        "Some species are missing from 'traits' rownames.",
        x = paste0(
          "Missing: ",
          paste(missing_sp[seq_len(min(5L, length(missing_sp)))],
                collapse = ", "),
          if (length(missing_sp) > 5L) ", ..." else ""
        ),
        i = "Supply a trait row for every species in the fit."
      )))
    }
    traits <- traits[species_names, , drop = FALSE]
  }
  if (NROW(traits) != length(species_names)) {
    stop(insight::format_error(c(
      "Trait row count does not match species count.",
      x = paste0(
        "traits: ", NROW(traits),
        ", species: ", length(species_names), "."
      ),
      i = paste0(
        "Set 'rownames(traits)' to the species labels, or supply ",
        "one row per species in the same order as the fit's ",
        "series levels."
      )
    )))
  }
  numeric_cols <- vapply(traits, is.numeric, logical(1L))
  if (!any(numeric_cols)) return(NULL)
  traits_num <- traits[, numeric_cols, drop = FALSE]
  scores <- as.matrix(loadings_2d)
  arrow_dat <- do.call(rbind, lapply(
    seq_along(traits_num),
    function(j) {
      t_raw <- traits_num[, j]
      if (!is.finite(stats::sd(t_raw)) ||
          stats::sd(t_raw) == 0) {
        return(NULL)
      }
      t_std <- as.numeric(scale(t_raw, center = TRUE, scale = TRUE))
      fit <- stats::lm.fit(x = cbind(1, scores), y = t_std)
      coefs <- fit$coefficients[-1L]
      data.frame(
        x = coefs[1L], y = coefs[2L],
        trait_name = colnames(traits_num)[j]
      )
    }
  ))
  if (is.null(arrow_dat) || NROW(arrow_dat) == 0L) return(NULL)
  loading_radii <- sqrt(rowSums(loadings_2d^2))
  trait_radii <- sqrt(arrow_dat$x^2 + arrow_dat$y^2)
  max_trait_r <- max(trait_radii)
  if (max_trait_r <= 0 || !is.finite(max_trait_r)) return(NULL)
  target_r <- arrow_scale * max(loading_radii)
  factor_scale <- target_r / max_trait_r
  arrow_dat$x <- arrow_dat$x * factor_scale
  arrow_dat$y <- arrow_dat$y * factor_scale
  arrow_dat
}


#' Internal: trait-arrow plot layers (segment + repel label).
#' Drawn in steelblue to differentiate from species-loading
#' darkred arrows. Returns `NULL` (an empty layer list) when
#' `trait_dat` is `NULL` so `+ NULL` is a safe no-op in the build.
#'
#' @noRd
ordinate_trait_layers <- function(trait_dat) {
  if (is.null(trait_dat) || NROW(trait_dat) == 0L) return(NULL)
  list(
    ggplot2::geom_segment(
      data = trait_dat,
      ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
      arrow = grid::arrow(
        length = grid::unit(0.12, "cm"), type = "closed"
      ),
      alpha = 0.85, color = "steelblue", linewidth = 0.6
    ),
    mvgam_repel_layer(
      data = trait_dat,
      mapping = ggplot2::aes(label = trait_name),
      type = "label",
      color = "steelblue", box.padding = 0.15, label.size = 0.1,
      alpha = 0.85, max.overlaps = 20
    )
  )
}


#' Internal: build the ordination biplot from the SVD components
#' and caller-supplied site / species labels. Every ordinate.*
#' method ends here so the layered ggplot output is identical
#' across object classes. When `traits` is non-NULL, per-trait
#' regression on the rotated loadings yields arrows overlaid on
#' the same biplot in a contrasting colour.
#'
#' @noRd
ordinate_build_plot <- function(svd_comp, which_lvs, biplot,
                                 label_sites, site_names,
                                 species_names,
                                 traits = NULL,
                                 trait_arrow_scale = 1) {
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
    trait_dat <- ordinate_trait_arrows(
      traits = traits,
      loadings_2d = as.matrix(sp_dat[, c("x", "y")]),
      species_names = species_names,
      arrow_scale = trait_arrow_scale
    )
    p <- p + ordinate_trait_layers(trait_dat)
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
  traits = NULL,
  trait_arrow_scale = 1,
  ...
) {
  checkmate::assert_integerish(
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE
  )
  validate_proportional(alpha)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  checkmate::assert_number(trait_arrow_scale, lower = 0)
  rotation <- match.arg(rotation)
  insight::check_if_installed(
    "ggrepel",
    reason = "to adequately plot ordination scores"
  )

  traits <- resolve_auto_traits(traits, object)
  comp <- ordinate_factor_components(object, alpha, rotation)
  sp_names <- resolve_series_info(object)$series_levels
  unit_name <- attr(object$model_data, "prepped_trend_model")$unit
  site_names <- unique(object$obs_data[[unit_name]])
  ordinate_build_plot(
    comp, which_lvs, biplot, label_sites,
    site_names = site_names, species_names = sp_names,
    traits = traits, trait_arrow_scale = trait_arrow_scale
  )
}


#' Latent variable ordination plot from a fitted `mvgam`
#'
#' Generalises `ordinate.jsdgam()` to any LV-factor `mvgam` fit
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
#' @param traits Optional trait overlay. Accepts `NULL` (default,
#'   no overlay), the literal string `"auto"` (pull the trait
#'   frame off the fit if `mvgam()` was called with a trait-
#'   informed `loadings_prior`), or an explicit `data.frame` /
#'   `matrix` with one row per series. Numeric columns are
#'   regressed on the rotated series loadings to give each trait's
#'   direction in the LV space; the resulting arrows are overlaid
#'   in steelblue. Row alignment follows `rownames(traits)` when
#'   set; otherwise rows must match the fit's series-level order.
#'   Only meaningful when `biplot = TRUE`.
#' @param trait_arrow_scale Positive numeric. Visual scaling
#'   factor for trait-arrow lengths. The default `1` matches the
#'   longest trait arrow to the longest series-loading radius.
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
#' `rotation = "none"` (or `plot_factors()`) to view the
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
#' @seealso [ordinate.jsdgam()], [residual_cor()],
#'   [plot.mvgam()] (especially `type = "factors"` and
#'   `type = "latent_state"`)
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
  traits = NULL,
  trait_arrow_scale = 1,
  ...
) {
  checkmate::assert_integerish(
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE
  )
  validate_proportional(alpha)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  checkmate::assert_number(trait_arrow_scale, lower = 0)
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

  rotation <- match.arg(rotation)
  svd_comp <- ordinate_factor_components(object, alpha, rotation)
  series_info <- resolve_series_info(object)
  species_names <- series_info$series_levels
  data <- object$obs_data %||% object$data
  site_names <- if (!is.null(data) && "time" %in% names(data)) {
    sort(unique(data$time))
  } else {
    # Last-resort fallback: 1..n_time bare integer labels.
    seq_len(NROW(svd_comp$scores))
  }
  traits <- resolve_auto_traits(traits, object)
  ordinate_build_plot(
    svd_comp, which_lvs, biplot, label_sites,
    site_names = site_names, species_names = species_names,
    traits = traits, trait_arrow_scale = trait_arrow_scale
  )
}


#' Internal: resolve `traits = "auto"` to the trait frame stored
#' on a trait-informed fit, warn once if none is found. Returns
#' the input unchanged when it is `NULL` or already a frame /
#' matrix, so the downstream `ordinate_trait_arrows()` validator
#' handles the structural checks.
#'
#' @noRd
resolve_auto_traits <- function(traits, object) {
  if (is.null(traits)) return(NULL)
  if (!is.character(traits)) return(traits)
  if (!identical(traits, "auto")) {
    stop(insight::format_error(c(
      "Argument 'traits' string value must be 'auto'.",
      x = paste0("Got '", traits[1L], "'."),
      i = "Use traits = NULL, traits = 'auto', or a data.frame."
    )))
  }
  found <- ordinate_extract_fit_traits(object)
  if (is.null(found) &&
      !identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      message = c(
        "traits = 'auto' requested but the fit carries no traits.",
        i = paste0(
          "Pass an explicit data.frame to 'traits', or refit ",
          "with jsdgam(traits = ...). Skipping the overlay."
        )
      ),
      .frequency = "once",
      .frequency_id = "mvgam_ordinate_auto_traits"
    )
  }
  found
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
