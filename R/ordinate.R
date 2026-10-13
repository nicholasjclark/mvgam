#' Latent variable ordination plots from `mvgam` and `jsdgam` fits
#'
#' Plot an ordination of latent variables and their factor
#' loadings from a fitted factor model. The two chosen latent
#' variables are first re-rotated, then posterior medians of the
#' variables and the series' loadings are scattered in the
#' resulting 2-D space. "Sites" are the training time points and
#' "species" are the model's series. Site labels and series
#' loading arrows are drawn via the package-wide \pkg{ggrepel}
#' helpers; when \pkg{ggarrow} and \pkg{ggpp} are installed the
#' arrows are rendered as tapered loadings.
#'
#' @note This method needs a fitted factor model. It uses the
#'   identified latent variables and their loadings. A model
#'   identified by rotation supplies `lv_trend_tilde` and `Z_tilde`.
#'   Every other factor model supplies `lv_trend` and `Z`.
#'
#' @name ordinate
#'
#' @param object A fitted `mvgam` or `jsdgam` object with latent
#'   dynamic factors (`n_lv` set on an `AR()` / `RW()` / `VAR()` /
#'   `ZMVN()` trend constructor, or a `trend_map` argument).
#' @param which_lvs Integer vector of length 2 indicating the
#'   two re-rotated latent variables to plot. Defaults to
#'   `c(1, 2)`.
#' @param biplot Logical. When `TRUE` (the default) both site
#'   scores and series loading arrows are drawn; when `FALSE`,
#'   site scores only.
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
#'       leading gradients. Use `alpha` to split the variance
#'       between site scores and loadings.}
#'     \item{`"varimax"`}{Orthogonal rotation maximising loading
#'       sparsity (each series loads strongly on one factor and
#'       near-zero on others). Use this to give each factor an
#'       interpretable name. The axes have no variance ordering.
#'       Requires `n_lv >= 2`. `alpha` ignored.}
#'     \item{`"promax"`}{Oblique extension of varimax that allows
#'       correlated rotated factors. `alpha` ignored.}
#'     \item{`"none"`}{Plot the centred posterior medians of the
#'       identified latent variables and loadings, as described
#'       in Details. `alpha` ignored.}
#'   }
#'   Varimax and promax axes have no variance ordering. Any pair of
#'   rotated factors is then a valid choice for `which_lvs`.
#' @param label_sites Logical. When `TRUE`, site scores are
#'   drawn as text labels (the training time values); when
#'   `FALSE`, as points only.
#' @param traits Optional trait overlay. Accepts `NULL` (default,
#'   no overlay), the literal string `"auto"` (pull the trait
#'   frame off the fit if it was fitted with a trait-informed
#'   `loadings_prior`), or an explicit `data.frame` / `matrix`
#'   with one row per series. Numeric columns are regressed on
#'   the rotated series loadings to give each trait's direction
#'   in the LV space; the resulting arrows are overlaid in
#'   steelblue. Row alignment follows `rownames(traits)` when
#'   set; otherwise rows must match the fit's series-level order.
#'   Non-numeric columns are ignored. A fit carrying no traits
#'   warns and the overlay is skipped. Only meaningful when
#'   `biplot = TRUE`.
#' @param trait_arrow_scale Positive numeric. Visual scaling
#'   factor for trait-arrow lengths. The default `1` matches the
#'   longest trait arrow to the longest series-loading radius;
#'   values `> 1` lengthen, values `< 1` shorten.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object. The returned object carries a
#'   `"rotation"` attribute (a list with `method`, `n_lv`,
#'   `scores`, `loadings`, `rotmat`) so users can extract the
#'   rotated factor scores and loadings without re-running
#'   `ordinate()`. Access via `attr(p, "rotation")`.
#'
#' @details
#' `ordinate()` starts from the identified loadings and latent
#' variables of the fit. Factors that share their dynamics are
#' identified by rotation to the lower-triangular `Z_tilde` and its
#' factor paths `lv_trend_tilde` (Heaps and Jermyn 2024). Factors
#' that take their own coefficients are identified by one ordering
#' and orientation of `Z` and `lv_trend`. The Identification section
#' of the trend constructors describes both. With
#' `rotation = "svd"` (default) the axes are ordination gradients
#' ordered by the variance they explain. With `"varimax"` or
#' `"promax"` each axis loads on as few series as it can.
#' `rotation = "none"` and `plot_factors()` show the identified
#' factors themselves.
#'
#' A `trend_map` supplies the loading pattern as data. Any rotation
#' other than `"none"` replaces that pattern with one derived from the
#' fitted covariance. Each such call warns and names the rotation it
#' applied. `plot_factors(fit)` shows the loadings the map declared.
#'
#' @section Known limitations:
#' \itemize{
#'   \item Posterior-median plug-in: the rotation operates on
#'     per-element medians of `lv_trend` and `Z`. Uncertainty in
#'     site scores and loading positions is not propagated to
#'     the biplot.
#'   \item Sign indeterminacy: SVD and varimax columns have arbitrary
#'     signs. Ordinations from independently fitted models may need a
#'     manual sign flip to align.
#' }
#'
#' @author Nicholas J Clark
#'
#' @seealso [jsdgam()], [residual_cor()],
#'   [plot.mvgam()] (especially `type = "factors"` and
#'   `type = "latent_state"`)
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' simdat <- sim_closure_unit_data(
#'   family = occ(),
#'   n_species = 4L,
#'   n_sites = 50L,
#'   n_visits = 4L,
#'   type = 2L
#' )
#' mod <- jsdgam(
#'   formula = bf(y ~ env, p ~ tod_c),
#'   factor_formula = ~ -1,
#'   data = simdat$data_train,
#'   family = occ(),
#'   n_lv = 2L,
#'   chains = 2,
#'   silent = 2
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

#' Posterior medians of the factor paths and loadings of a fit
#'
#' @param object Fitted `mvgam` or `jsdgam` factor model
#' @return List holding `lv_estimates` (`[n_time, n_lv]`), `lv_coefs`
#'   (`[n_series, n_lv]`) and `n_lv`
#' @noRd
ordinate_extract_medians <- function(object) {
  checkmate::assert_class(object, "mvgam")
  n_lv <- require_factor_n_lv(object, "ordinate()")
  per_lv <- extract_lv_trend_matrices(object, n_lv)
  lv_estimates <- do.call(cbind, lapply(per_lv, function(mat) {
    apply(mat, 2L, stats::median, na.rm = TRUE)
  }))
  Z_arr <- extract_factor_loadings_array(object, n_lv)
  if (is.null(Z_arr)) {
    stop(insight::format_error(c(
      "Could not extract Z loadings from the posterior.",
      i = paste0(
        "Ordination requires draws of 'Z_tilde[i, k]' or 'Z[i, k]' ",
        "or a fixed loadings matrix on the fit."
      )
    )), call. = FALSE)
  }
  lv_coefs <- apply(Z_arr, c(2L, 3L), stats::median, na.rm = TRUE)
  list(lv_estimates = lv_estimates, lv_coefs = lv_coefs,
        n_lv = n_lv)
}


#' Site scores and series loadings in the requested rotated basis
#'
#' Branches:
#' - `"svd"`: BORAL (Hui 2016) re-rotation. SVD of the
#'   `LV %*% t(Z)` cross-product, then split the singular values
#'   across factor paths (`alpha`) and loadings (`1 - alpha`).
#'   Axes are variance-ordered; `which_lvs = c(1, 2)` picks the
#'   most informative pair.
#' - `"varimax"`: orthogonal rotation maximising loading sparsity.
#'   `alpha` is ignored. Rotating Z and LV by the same orthogonal R
#'   keeps `LV %*% t(Z)` unchanged, since `R^{-T} = R`.
#' - `"promax"`: oblique extension of varimax. `alpha` ignored.
#' - `"none"`: centred, unrotated medians. `alpha` ignored.
#'
#' @param object Fitted `mvgam` or `jsdgam` factor model
#' @param alpha Numeric share of the singular values given to the site
#'   scores
#' @param rotation One of `"svd"`, `"varimax"`, `"promax"` or `"none"`
#' @return List holding `scores` (`[n_time, n_lv]`), `loadings`
#'   (`[n_series, n_lv]`), `rotmat`, `n_lv` and `rotation`. `rotmat`
#'   is the `stats::varimax()` or `stats::promax()` rotation matrix,
#'   or `NULL` for `"svd"` and `"none"`.
#' @noRd
ordinate_factor_components <- function(object, alpha, rotation) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_choice(rotation, c("svd", "varimax", "promax", "none"))
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


#' SVD rotation of the factor medians
#'
#' @param lv_estimates Numeric matrix of factor path medians,
#'   `[n_time, n_lv]`
#' @param lv_coefs Numeric matrix of loading medians,
#'   `[n_series, n_lv]`
#' @param alpha Numeric share of the singular values given to the site
#'   scores
#' @param n_lv Integer number of latent factors
#' @return List holding `scores`, `loadings` and a `NULL` `rotmat`
#' @noRd
ordinate_svd_rotate <- function(lv_estimates, lv_coefs, alpha,
                                  n_lv) {
  checkmate::assert_matrix(lv_estimates, mode = "numeric")
  checkmate::assert_matrix(lv_coefs, mode = "numeric")
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_int(n_lv, lower = 1L)
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


#' Varimax or promax rotation of the factor medians
#'
#' Applies the rotation matrix of `stats::varimax()` or
#' `stats::promax()` to the loadings and the factor paths. The fitted
#' `LV %*% t(Z)` product is unchanged. For orthogonal varimax,
#' `R^{-T} = R`. For oblique promax the inverse-transpose is computed
#' explicitly.
#'
#' @param lv_estimates Numeric matrix of factor path medians,
#'   `[n_time, n_lv]`
#' @param lv_coefs Numeric matrix of loading medians,
#'   `[n_series, n_lv]`
#' @param n_lv Integer number of latent factors
#' @param method Either `"varimax"` or `"promax"`
#' @return List holding `scores`, `loadings` and `rotmat`
#' @noRd
ordinate_orth_rotate <- function(lv_estimates, lv_coefs, n_lv,
                                   method) {
  checkmate::assert_matrix(lv_estimates, mode = "numeric")
  checkmate::assert_matrix(lv_coefs, mode = "numeric")
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_choice(method, c("varimax", "promax"))
  if (n_lv < 2L) {
    stop(insight::format_error(c(
      paste0("Rotation '", method, "' requires at least 2 factors."),
      x = paste0("Got n_lv = ", n_lv, "."),
      i = "Refit with n_lv >= 2 or use rotation = 'none' / 'svd'."
    )), call. = FALSE)
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


#' The trait frame a trait-informed fit stores
#'
#' Searches the trend specifications for the `features` of a
#' `loadings_prior`, as `jsdgam(traits = ...)` records them.
#'
#' @param object Fitted `mvgam` object, or a list holding `mv_spec`
#' @return The first trait frame found, or `NULL` for a fit without
#'   traits
#' @noRd
ordinate_extract_fit_traits <- function(object) {
  checkmate::assert_list(object)
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


#' Trait arrow coordinates in the rotated factor space
#'
#' Each numeric trait column is regressed on the species' rotated
#' loadings for `which_lvs`. The slope pair `(b_lv1, b_lv2)` is the
#' trait's direction in the biplot. Arrow lengths are rescaled to
#' make the longest trait arrow `arrow_scale` times the longest
#' species-loading radius.
#'
#' The regression applies to any factor fit. For a fit with
#' `jsdgam(traits = ...)` the arrows show the trait gradient the
#' loadings prior encodes. For any other fit they show the direction
#' in which each trait increases in the factor space.
#'
#' @param traits Data frame or matrix with one row per species, or
#'   `NULL`
#' @param loadings_2d Numeric matrix of the two plotted loading
#'   columns, one row per species
#' @param species_names Character vector of species labels
#' @param arrow_scale Positive number scaling the arrow lengths
#' @return Data frame holding `x`, `y` and `trait_name`, or `NULL`
#'   when `traits` is `NULL` or holds no usable numeric column
#' @noRd
ordinate_trait_arrows <- function(traits, loadings_2d,
                                  species_names, arrow_scale) {
  checkmate::assert_matrix(loadings_2d, mode = "numeric", ncols = 2L)
  checkmate::assert_character(species_names, min.len = 1L)
  checkmate::assert_number(arrow_scale, lower = 0)
  if (is.null(traits)) return(NULL)
  if (!is.data.frame(traits) && !is.matrix(traits)) {
    stop(insight::format_error(c(
      "Argument 'traits' must be a data.frame or matrix.",
      i = "Give one row per species, named by 'rownames(traits)'."
    )), call. = FALSE)
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
      )), call. = FALSE)
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
        "Set 'rownames(traits)' to the species labels or supply ",
        "one row per species in the same order as the fit's ",
        "series levels."
      )
    )), call. = FALSE)
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


#' Trait arrow layers of an ordination biplot
#'
#' Draws a steelblue segment and label for each trait. The species
#' loading arrows are dark red.
#'
#' @param trait_dat Data frame from `ordinate_trait_arrows()`, or
#'   `NULL`
#' @return List of a segment layer and a label layer, or `NULL` when
#'   `trait_dat` is empty. Adding `NULL` to a ggplot leaves it
#'   unchanged.
#' @noRd
ordinate_trait_layers <- function(trait_dat) {
  checkmate::assert_data_frame(trait_dat, null.ok = TRUE)
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


#' The ordination biplot
#'
#' Every `ordinate()` method builds its plot here. A non-`NULL`
#' `traits` adds the trait arrows in a contrasting colour.
#'
#' @param svd_comp List from `ordinate_factor_components()`
#' @param which_lvs Integer vector of the two factors to plot
#' @param biplot Logical. `TRUE` adds the series loading arrows.
#' @param label_sites Logical. `TRUE` labels the site scores.
#' @param site_names Vector of site labels, one per score row
#' @param species_names Character vector of series labels
#' @param traits Data frame or matrix of traits, or `NULL`
#' @param trait_arrow_scale Positive number scaling the trait arrows
#' @return A `ggplot` object with a `"rotation"` attribute
#' @noRd
ordinate_build_plot <- function(svd_comp, which_lvs, biplot,
                                 label_sites, site_names,
                                 species_names,
                                 traits = NULL,
                                 trait_arrow_scale = 1) {
  checkmate::assert_list(svd_comp)
  checkmate::assert_integerish(which_lvs, len = 2L, lower = 1L,
                               any.missing = FALSE)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  checkmate::assert_atomic_vector(site_names)
  checkmate::assert_character(species_names, min.len = 1L)
  checkmate::assert_number(trait_arrow_scale, lower = 0)
  if (any(which_lvs > svd_comp$n_lv)) {
    stop(insight::format_error(c(
      "Selected latent variables exceed the model's `n_lv`.",
      x = paste0(
        "Requested which_lvs = ", paste(which_lvs, collapse = ", "),
        " from a model with n_lv = ", svd_comp$n_lv, "."
      ),
      i = if (svd_comp$n_lv == 1L) {
        "A one-factor model plots with 'plot_factors()'."
      }
    )), call. = FALSE)
  }
  if (length(site_names) != NROW(svd_comp$scores)) {
    stop(insight::format_error(c(
      paste0(
        "The number of site labels does not match the number of ",
        "factor-score rows."
      ),
      x = paste0(
        "labels: ", length(site_names),
        ", lv_trend rows: ", NROW(svd_comp$scores), "."
      ),
      x = "Imputed time points in the training data commonly cause this.",
      i = "Trim to the training grid or supply matching labels."
    )), call. = FALSE)
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


#' @rdname ordinate
#' @method ordinate mvgam
#' @importFrom grid arrow unit
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
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_integerish(
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE
  )
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  checkmate::assert_number(trait_arrow_scale, lower = 0)
  rlang::check_dots_empty()
  insight::check_if_installed(
    "ggrepel",
    reason = "to adequately plot ordination scores"
  )

  rotation <- match.arg(rotation)

  # A trend_map supplies the loading pattern as data. Every rotation
  # but "none" replaces it with a pattern derived from the fitted
  # covariance. The plotted axes then lose the declared structure.
  # The warning describes the returned plot and is raised on every
  # call.
  if (!is.null(object$trend_metadata$fixed_Z) && rotation != "none") {
    insight::format_warning(c(
      paste0(
        "'rotation = \"", rotation, "\"' re-rotates the ",
        "structural loadings supplied via 'trend_map'."
      ),
      x = paste0("The biplot shows ordination gradients in place of ",
                 "the declared factor structure."),
      i = paste0("Pass 'rotation = \"none\"' to keep the declared axes ",
                 "or call 'plot_factors()' for the fixed loadings.")
    ))
  }

  svd_comp <- ordinate_factor_components(object, alpha, rotation)
  species_names <- fitted_series_levels(object)
  # The score rows are the rows of `lv_trend`, one per time on the
  # fit's time axis. The recorded time values label them. A fit
  # without a recorded time axis labels them by position.
  site_names <- mvgam_axes(object)$time$values %||%
    seq_len(NROW(svd_comp$scores))
  traits <- resolve_auto_traits(traits, object)
  ordinate_build_plot(
    svd_comp, which_lvs, biplot, label_sites,
    site_names = site_names, species_names = species_names,
    traits = traits, trait_arrow_scale = trait_arrow_scale
  )
}


#' Resolve `traits = "auto"` to the trait frame of the fit
#'
#' Warns when the fit stores no traits. A `NULL`, data frame or matrix
#' input is returned unchanged. `ordinate_trait_arrows()` checks its
#' structure.
#'
#' @param traits `NULL`, the string `"auto"`, or a data frame or matrix
#' @param object Fitted `mvgam` object, or a list holding `mv_spec`
#' @return The trait frame, or `NULL`
#' @noRd
resolve_auto_traits <- function(traits, object) {
  checkmate::assert_list(object)
  if (is.null(traits)) return(NULL)
  if (!is.character(traits)) return(traits)
  if (!identical(traits, "auto")) {
    stop(insight::format_error(c(
      "Argument 'traits' string value must be 'auto'.",
      x = paste0("Got '", traits[1L], "'."),
      i = "Use traits = NULL, traits = 'auto' or a data.frame."
    )), call. = FALSE)
  }
  found <- ordinate_extract_fit_traits(object)
  # The warning describes the returned plot and is raised on every
  # call.
  if (is.null(found)) {
    insight::format_warning(c(
      "Argument 'traits = \"auto\"' requires a fit built with traits.",
      x = "The plot omits the trait overlay.",
      i = paste0("Pass a data.frame to 'traits' or refit with ",
                 "'jsdgam(traits = ...)'.")
    ))
  }
  found
}


#' Species-loading arrow layers for an ordination biplot
#'
#' Draws tapered arrows weighted by loading magnitude with
#' `ggarrow::geom_arrow()` when ggarrow and ggpp are installed. Plain
#' `ggplot2::geom_segment()` arrows are drawn otherwise.
#'
#' @param sp_dat Data frame of species loadings holding `x` and `y`
#' @param sp_names Character vector of species labels, one per row of
#'   `sp_dat`
#' @return List of ggplot2 layers
#' @noRd
ordinate_biplot_layers <- function(sp_dat, sp_names) {
  checkmate::assert_data_frame(sp_dat)
  checkmate::assert_character(sp_names, min.len = 1L)
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
