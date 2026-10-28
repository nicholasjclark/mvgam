#' Ordination biplot of a latent factor model
#'
#' Plots the site scores and series loadings of a factor model on two
#' axes. Sites are the points on the fit's unit or time axis, labelled
#' by their values. Series, such as the species of a [jsdgam()] fit,
#' are arrows from the origin. Series that point the same way share
#' their residual variation. Arrows are tapered when \pkg{ggarrow} and
#' \pkg{ggpp} are installed.
#'
#' @name ordinate
#'
#' @param object A fitted `mvgam` or `jsdgam` model with at least two
#'   latent factors, set by `n_lv` in [jsdgam()] or in a trend
#'   constructor
#' @param which_lvs Two distinct integers: the axes to plot
#' @param biplot Logical. `TRUE` draws the series loadings with the site
#'   scores. `FALSE` draws the site scores alone.
#' @param alpha Number in `[0, 1]`: the share of each singular value
#'   given to the site scores under `rotation = "svd"`. The loadings
#'   take the rest. `0.5` scales sites and series alike. Values near 1
#'   spread the sites, values near 0 spread the series (Hui 2016).
#' @param rotation The basis of the axes:
#'   \describe{
#'     \item{`"svd"`}{Axes ordered by the variance of
#'       `LV %*% t(Z)` they explain. The first two show the leading
#'       gradients.}
#'     \item{`"varimax"`}{An orthogonal rotation under which each
#'       series loads on as few axes as it can. The axes then lend
#'       themselves to names.}
#'     \item{`"promax"`}{An oblique rotation, as varimax with
#'       correlated axes.}
#'     \item{`"none"`}{The identified factors themselves, centred.}
#'   }
#'   Varimax and promax axes carry no variance order. Any two of them
#'   make a valid `which_lvs`.
#' @param label_sites Logical. `TRUE` labels each site score with its
#'   unit or time value.
#' @param traits `NULL`, `"auto"`, or a data frame or matrix of series
#'   traits. `"auto"` takes the traits a `jsdgam(traits = ...)` fit
#'   stores. Each numeric trait is regressed on the plotted loadings.
#'   Its arrow points the way the trait increases. Rows match the
#'   series by `rownames(traits)` when those name every series and by
#'   position otherwise. Drawn only with `biplot = TRUE`.
#' @param trait_arrow_scale Positive number. The longest trait arrow is
#'   `trait_arrow_scale` times the longest loading arrow.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object. `attr(p, "rotation")` holds the rotated
#'   `scores` and `loadings`, the rotation matrix `rotmat` (`NULL` for
#'   `"svd"` and `"none"`), the `method` and `n_lv`.
#'
#' @details
#' The plot starts from the posterior medians of the identified factors
#' and loadings. Factors with shared dynamics are identified by a
#' rotation to lower-triangular loadings, `Z_tilde` and
#' `lv_trend_tilde` (Heaps and Jermyn 2024). Factors with their own
#' dynamics are identified by an ordering and orientation of `Z` and
#' `lv_trend`. The Identification section of [trend_constructors]
#' describes both. Every rotation keeps the product `LV %*% t(Z)` the
#' model fitted.
#'
#' The medians carry no posterior uncertainty into the plot. Axis signs
#' are arbitrary. Two fits may need an axis flipped before they agree.
#' [compare_loadings()] aligns the loadings of two fits.
#'
#' A `trend_map` supplies the loadings as data. A rotation other than
#' `"none"` replaces them and warns on every call.
#' `plot(object, type = "factors")` shows the loadings the map declared.
#'
#' @references
#' Hui, F. K. C. (2016). boral: Bayesian ordination and regression
#'   analysis of multivariate abundance data in R. *Methods in Ecology
#'   and Evolution*, 7, 744-750. \doi{10.1111/2041-210X.12514}
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#'   distributions for the covariance matrix in latent factor models.
#'   *Statistics and Computing*, 34, 143.
#'   \doi{10.1007/s11222-024-10454-0}
#'
#' @author Nicholas J Clark
#'
#' @seealso [plot.mvgam()] with `type = "factors"` plots each factor's
#'   path. [residual_cor()] gives the correlations among series the
#'   factors imply. The
#'   `r mvgam_article("jsdgam", "JSDM article")` interprets a biplot
#'   of species loadings.
#'
#' @examples
#' \dontrun{
#' # Gaussian responses of 10 species at 60 sites, driven by two
#' # latent factors
#' simdat <- sim_jsdgam(family = gaussian(), n_species = 10L,
#'                      n_sites = 60L, n_lv = 2L, seed = 3L)
#' mod <- jsdgam(y ~ env * series, data = simdat$data_train,
#'               family = gaussian(), n_lv = 2L, chains = 2, silent = 2)
#'
#' # Sites as points and species loadings as arrows
#' ordinate(mod)
#'
#' # Varimax axes, on which each species loads on as few as it can
#' p <- ordinate(mod, rotation = "varimax", label_sites = FALSE)
#' p
#' attr(p, "rotation")$loadings
#'
#' # Next, residual_cor(mod) gives the species correlations the
#' # factors imply
#' }
#'
#' @family latent dynamics
#' @export
ordinate <- function(object, ...) {
  UseMethod("ordinate", object)
}


#' Site scores and series loadings in the requested basis
#'
#' `"svd"` takes the singular value decomposition of `LV %*% t(Z)`. The
#' singular values are split between scores (`alpha`) and loadings
#' (`1 - alpha`), as boral does. `"varimax"` and `"promax"` rotate the
#' loadings by `R` and the scores by `t(solve(R))`. Their product is
#' unchanged. `"none"` centres the medians.
#'
#' @param object Fitted `mvgam` factor model
#' @param n_lv Integer number of latent factors
#' @param alpha Numeric share of the singular values given to the site
#'   scores
#' @param rotation One of `"svd"`, `"varimax"`, `"promax"` or `"none"`
#' @return List holding the centred `scores` (`[n_time, n_lv]`) and
#'   `loadings` (`[n_series, n_lv]`), the rotation matrix `rotmat`,
#'   `n_lv` and the rotation `method`
#' @noRd
ordinate_factor_components <- function(object, n_lv, alpha, rotation) {
  lv <- do.call(cbind, lapply(extract_lv_trend_matrices(object, n_lv),
                              function(mat) {
                                apply(mat, 2L, stats::median)
                              }))
  Z <- factor_loading_medians(object, n_lv)
  rotmat <- NULL
  if (rotation == "svd") {
    dec <- svd(tcrossprod(lv, Z), n_lv, n_lv)
    d <- dec$d[seq_len(n_lv)]
    lv <- sweep(dec$u, 2L, d^alpha, `*`)
    Z <- sweep(dec$v, 2L, d^(1 - alpha), `*`)
  } else if (rotation != "none") {
    rotmat <- switch(rotation,
                     varimax = stats::varimax(Z)$rotmat,
                     promax = stats::promax(Z)$rotmat)
    Z <- Z %*% rotmat
    lv <- lv %*% t(solve(rotmat))
  }
  centre <- function(m) sweep(m, 2L, colMeans(m))
  list(
    scores = centre(lv),
    loadings = centre(Z),
    rotmat = rotmat,
    n_lv = n_lv,
    method = rotation
  )
}


#' Trait arrow coordinates in the rotated factor space
#'
#' Each numeric trait column is regressed on the series' rotated
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
  if (is.null(traits)) return(NULL)
  if (!is.data.frame(traits) && !is.matrix(traits)) {
    stop(insight::format_error(c(
      "Argument 'traits' must be a data.frame or matrix.",
      i = "Give one row per series, named by 'rownames(traits)'."
    )), call. = FALSE)
  }
  traits <- align_feature_rows(as.data.frame(traits), species_names,
                               "traits")
  numeric_cols <- vapply(traits, is.numeric, logical(1L))
  if (!any(numeric_cols)) return(NULL)
  traits_num <- traits[, numeric_cols, drop = FALSE]
  arrow_dat <- do.call(rbind, lapply(
    seq_along(traits_num),
    function(j) {
      t_raw <- traits_num[, j]
      if (!is.finite(stats::sd(t_raw)) ||
          stats::sd(t_raw) == 0) {
        return(NULL)
      }
      t_std <- as.numeric(scale(t_raw, center = TRUE, scale = TRUE))
      fit <- stats::lm.fit(x = cbind(1, loadings_2d), y = t_std)
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
#' Draws a segment and label for each trait in the Okabe-Ito blue. The
#' species loading arrows are dark red.
#'
#' @param trait_dat Data frame from `ordinate_trait_arrows()`, or
#'   `NULL`
#' @return List of a segment layer and a label layer, or `NULL` for a
#'   `NULL` `trait_dat`. Adding `NULL` to a ggplot leaves it unchanged.
#' @noRd
ordinate_trait_layers <- function(trait_dat) {
  if (is.null(trait_dat)) return(NULL)
  trait_colour <- okabe_ito_palette()[2L]
  list(
    ggplot2::geom_segment(
      data = trait_dat,
      ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
      arrow = grid::arrow(
        length = grid::unit(0.12, "cm"), type = "closed"
      ),
      alpha = 0.85, color = trait_colour, linewidth = 0.6
    ),
    mvgam_repel_layer(
      data = trait_dat,
      mapping = ggplot2::aes(label = trait_name),
      type = "label",
      color = trait_colour, box.padding = 0.15, label.size = 0.1,
      alpha = 0.85, max.overlaps = 20
    )
  )
}


#' The ordination biplot
#'
#' A non-`NULL` `traits` adds the trait arrows in a contrasting colour.
#'
#' @param comp List from `ordinate_factor_components()`
#' @param which_lvs Integer vector of the two factors to plot
#' @param biplot Logical. `TRUE` adds the series loading arrows.
#' @param label_sites Logical. `TRUE` labels the site scores.
#' @param site_names Vector of site labels, one per score row
#' @param species_names Character vector of series labels
#' @param traits Data frame or matrix of traits, or `NULL`
#' @param trait_arrow_scale Positive number scaling the trait arrows
#' @return A `ggplot` object with a `"rotation"` attribute
#' @noRd
ordinate_build_plot <- function(comp, which_lvs, biplot, label_sites,
                                site_names, species_names,
                                traits = NULL, trait_arrow_scale = 1) {
  sp_dat <- stats::setNames(
    data.frame(comp$loadings[, which_lvs, drop = FALSE]), c("x", "y")
  )
  site_dat <- stats::setNames(
    data.frame(comp$scores[, which_lvs, drop = FALSE]), c("x", "y")
  )
  p <- ggplot2::ggplot(rbind(sp_dat, site_dat), ggplot2::aes(x, y)) +
    ggplot2::labs(
      x = paste("Latent variable", which_lvs[1L]),
      y = paste("Latent variable", which_lvs[2L])
    )
  if (label_sites) {
    p <- p + mvgam_repel_layer(
      data = site_dat,
      mapping = ggplot2::aes(label = site_names),
      type = "text",
      alpha = 0.75, size = 3, max.overlaps = 20,
      colour = "grey40", segment.color = NA
    )
  }
  p <- p + ggplot2::geom_point(
    data = site_dat, pch = 21, fill = "grey20", colour = "white"
  )
  if (biplot) {
    p <- p +
      ordinate_biplot_layers(sp_dat, species_names) +
      ordinate_trait_layers(ordinate_trait_arrows(
        traits = traits,
        loadings_2d = as.matrix(sp_dat),
        species_names = species_names,
        arrow_scale = trait_arrow_scale
      ))
  }
  p <- p + mvgam_theme()
  attr(p, "rotation") <- comp[c("method", "n_lv", "scores", "loadings",
                                "rotmat")]
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
    which_lvs, len = 2L, lower = 1L, any.missing = FALSE, unique = TRUE
  )
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_flag(biplot)
  checkmate::assert_flag(label_sites)
  checkmate::assert_number(trait_arrow_scale, lower = 0)
  rlang::check_dots_empty()
  rotation <- match.arg(rotation)
  n_lv <- require_factor_n_lv(object, "ordinate()")
  if (max(which_lvs) > n_lv) {
    stop(insight::format_error(c(
      paste0("'which_lvs' selects factor ", max(which_lvs),
             " of a model with ", n_lv, "."),
      i = if (n_lv == 1L) {
        "A one-factor model plots with 'plot(fit, type = \"factors\")'."
      } else {
        paste0("Choose two factors from 1 to ", n_lv, ".")
      }
    )), call. = FALSE)
  }

  # A trend_map supplies the loading pattern as data. Every rotation
  # but "none" replaces it with a pattern derived from the fitted
  # covariance. The warning describes the returned plot and is raised
  # on every call.
  if (!is.null(object$trend_metadata$fixed_Z) && rotation != "none") {
    insight::format_warning(c(
      paste0("'rotation = \"", rotation, "\"' re-rotates the ",
             "loadings supplied by 'trend_map'."),
      x = "The biplot shows ordination gradients.",
      i = paste0("Pass 'rotation = \"none\"' to keep the declared axes ",
                 "or 'plot(fit, type = \"factors\")' for the fixed ",
                 "loadings.")
    ))
  }

  comp <- ordinate_factor_components(object, n_lv, alpha, rotation)
  ordinate_build_plot(
    comp, which_lvs, biplot, label_sites,
    site_names = factor_time_values(object, NROW(comp$scores)),
    species_names = fitted_series_levels(object),
    traits = resolve_auto_traits(traits, object),
    trait_arrow_scale = trait_arrow_scale
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
  if (is.null(traits)) return(NULL)
  if (!is.character(traits)) return(traits)
  if (!identical(traits, "auto")) {
    stop(insight::format_error(c(
      "Argument 'traits' string value must be 'auto'.",
      x = paste0("Got '", traits[1L], "'."),
      i = "Use traits = NULL, traits = 'auto' or a data.frame."
    )), call. = FALSE)
  }
  # `jsdgam(traits = ...)` stores the traits as the `features` of the
  # loadings prior. The warning describes the returned plot and is
  # raised on every call.
  found <- first_trend_spec(object)$loadings_prior$features
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
  species_colour <- mvgam_colour("dark", "red")
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
        colour = species_colour, stroke_colour = "white",
        stroke_width = 0.1, alpha = 0.5, show.legend = FALSE
      ),
      ggplot2::scale_linewidth(range = c(0.45, 1.75)),
      mvgam_repel_layer(
        data = sp_dat,
        mapping = ggplot2::aes(label = sp_names),
        type = "label",
        color = species_colour, box.padding = 0.1, label.size = 0.1,
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
        alpha = 0.5, color = species_colour
      ),
      mvgam_repel_layer(
        data = sp_dat,
        mapping = ggplot2::aes(label = sp_names),
        type = "label",
        color = species_colour, box.padding = 0.1, label.size = 0.1,
        alpha = 0.75, max.overlaps = 20
      )
    )
  }
}
