#### Functions to ensure gratia methods work with mvgam, using the Enhance functionality
# in the Description ####

# Add eval_smooth and draw methods to gratia namespace
# on load
.onLoad <- function(libname, pkgname) {
  if(requireNamespace("gratia", quietly = TRUE)){
    registerS3method("eval_smooth",
                     "moi.smooth",
                     eval_smoothDotmoiDotsmooth,
                     envir = asNamespace("gratia"))
    registerS3method("eval_smooth",
                     "mod.smooth",
                     eval_smoothDotmodDotsmooth,
                     envir = asNamespace("gratia"))
    registerS3method("eval_smooth",
                     "hilbert.smooth",
                     eval_smoothDothilbertDotsmooth,
                     envir = asNamespace("gratia"))
    registerS3method("draw",
                     "mvgam",
                     drawDotmvgam,
                     envir = asNamespace("gratia"))
  }
}

#' Enhance post-processing of \pkg{mvgam} models using \pkg{gratia} functionality
#'
#' These evaluation and plotting functions exist to allow some popular `gratia`
#' methods to work with `mvgam` or `jsdgam` models
#' @name gratia_mvgam_enhancements
#' @param object a fitted mvgam, the result of a call to [mvgam()].
#' @param model a fitted `mgcv` model of clas `gam` or `bam`.
#' @param data a data frame of covariate values at which to evaluate the
#'   model's smooth functions.
#' @param smooth a smooth object of class `"gp.smooth"` (returned from a model using either the
#' `dynamic()` function or the `gp()` function) or of class `"moi.smooth"` or `"mod.smooth"`
#' (returned from a model using the 'moi' or 'mod' basis).
#' @param trend_effects logical specifying whether smooth terms from the `trend_formula` should
#' be drawn. If `FALSE`, only terms from the observation formula are drawn. If `TRUE`, only
#' terms from the `trend_formula` are drawn.
#' @param select character, logical, or numeric; which smooths to plot. If
#'   `NULL`, the default, then all model smooths are drawn.
#'   Character `select` matches the labels for smooths
#'   as shown for example in the output from `summary(object)`. Logical
#'   `select` operates as per numeric `select` in the order that smooths are
#'   stored.
#' @param parametric logical; plot parametric terms also? Note that `select` is
#'   used for selecting which smooths to plot. The `terms` argument is used to
#'   select which parametric effects are plotted. The default, as with
#'   [mgcv::plot.gam()], is to not draw parametric effects.
#' @param terms character; which model parametric terms should be drawn? The
#'   Default of `NULL` will plot all parametric terms that can be drawn.
#' @param residuals currently ignored for `mvgam` models.
#' @param scales character; should all univariate smooths be plotted with the
#'   same y-axis scale? If `scales = "free"`, the default, each univariate
#'   smooth has its own y-axis scale. If `scales = "fixed"`, a common y axis
#'   scale is used for all univariate smooths.
#'
#'   Currently does not affect the y-axis scale of plots of the parametric
#'   terms.
#' @param constant numeric; a constant to add to the estimated values of the
#'   smooth. `constant`, if supplied, will be added to the estimated value
#'   before the confidence band is computed.
#' @param fun function; a function that will be applied to the estimated values
#'   and confidence interval before plotting. Can be a function or the name of a
#'   function. Function `fun` will be applied after adding any `constant`, if
#'   provided.
#' @param ci_level numeric between 0 and 1; the coverage of credible interval.
#' @param n numeric; the number of points over the range of the covariate at
#'   which to evaluate the smooth.
#' @param n_3d,n_4d numeric; the number of points over the range of last
#'   covariate in a 3D or 4D smooth. The default is `NULL` which achieves the
#'   standard behaviour of using `n` points over the range of all covariate,
#'   resulting in `n^d` evaluation points, where `d` is the dimension of the
#'   smooth. For `d > 2` this can result in very many evaluation points and slow
#'   performance. For smooths of `d > 4`, the value of `n_4d` will be used for
#'   all dimensions `> 4`, unless this is `NULL`, in which case the default
#'   behaviour (using `n` for all dimensions) will be observed.
#' @param unconditional ignored for `mvgam` models as all appropriate
#' uncertainties are already included in the posterior estimates.
#' @param overall_uncertainty ignored for `mvgam` models as all appropriate
#' uncertainties are already included in the posterior estimates.
#' @param dist numeric; if greater than 0, this is used to determine when
#'   a location is too far from data to be plotted when plotting 2-D smooths.
#'   The data are scaled into the unit square before deciding what to exclude,
#'   and `dist` is a distance within the unit square. See
#'   [mgcv::exclude.too.far()] for further details.
#' @param rug logical; draw a rug plot at the bottom of each plot for 1-D
#'   smooths or plot locations of data for higher dimensions.
#' @param contour logical; should contours be draw on the plot using
#'   [ggplot2::geom_contour()].
#' @param grouped_by logical; should factor by smooths be drawn as one panel
#'   per level of the factor (`FALSE`, the default), or should the individual
#'   smooths be combined into a single panel containing all levels (`TRUE`)?
#' @param ci_alpha numeric; alpha transparency for confidence or simultaneous
#'   interval.
#' @param ci_col colour specification for the confidence/credible intervals
#'   band. Affects the fill of the interval.
#' @param smooth_col colour specification for the smooth line.
#' @param resid_col colour specification for residual points. Ignored.
#' @param contour_col colour specification for contour lines.
#' @param n_contour numeric; the number of contour bins. Will result in
#'   `n_contour - 1` contour lines being drawn. See [ggplot2::geom_contour()].
#' @param partial_match logical; should smooths be selected by partial matches
#'   with `select`? If `TRUE`, `select` can only be a single string to match
#'   against.
#' @param discrete_colour a suitable colour scale to be used when plotting
#'   discrete variables.
#' @param discrete_fill a suitable fill scale to be used when plotting
#'   discrete variables.
#' @param continuous_colour a suitable colour scale to be used when plotting
#'   continuous variables.
#' @param continuous_fill a suitable fill scale to be used when plotting
#'   continuous variables.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param angle numeric; the angle at which the x axis tick labels are to be
#'   drawn passed to the `angle` argument of [ggplot2::guide_axis()].
#' @param ncol,nrow numeric; the numbers of rows and columns over which to
#'   spread the plots
#' @param guides character; one of `"keep"` (the default), `"collect"`, or
#'   `"auto"`. Passed to [patchwork::plot_layout()]
#' @param widths,heights The relative widths and heights of each column and
#'   row in the grid. Will get repeated to match the dimensions of the grid. If
#'   there is more than 1 plot and `widths = NULL`, the value of `widths` will
#'   be set internally to `widths = 1` to accommodate plots of smooths that
#'   use a fixed aspect ratio.
#' @param crs the coordinate reference system (CRS) to use for the plot. All
#'   data will be projected into this CRS. See [ggplot2::coord_sf()] for
#'   details.
#' @param default_crs the coordinate reference system (CRS) to use for the
#'   non-sf layers in the plot. If left at the default `NULL`, the CRS used is
#'   4326 (WGS84), which is appropriate for spline-on-the-sphere smooths, which
#'   are parameterized in terms of latitude and longitude as coordinates. See
#'   [ggplot2::coord_sf()] for more details.
#' @param lims_method character; affects how the axis limits are determined. See
#'   [ggplot2::coord_sf()]. Be careful; in testing of some examples, changing
#'   this to `"orthogonal"` for example with the chlorophyll-a example from
#'   Simon Wood's GAM book quickly used up all the RAM in my test system and the
#'   OS killed R. This could be incorrect usage on my part; right now the grid
#'   of points at which SOS smooths are evaluated (if not supplied by the user)
#'   can produce invalid coordinates for the corners of tiles as the grid is
#'   generated for tile centres without respect to the spacing of those tiles.
#' @param wrap logical; wrap plots as a patchwork? If \code{FALSE}, a list of
#'   ggplot objects is returned, 1 per term plotted.
#' @param envir an environment to look up the data within.
#' @param ... additional arguments passed to other methods.
#'
#' @details These methods allow `mvgam` models to be *Enhanced* if users have the `gratia`
#' package installed, making available the popular `draw()` function to plot partial effects
#' of `mvgam` smooth functions using [ggplot2::ggplot()] utilities
#' @author Nicholas J Clark
#' @examples
#' \donttest{
#' # Fit a simple GAM and draw partial effects of smooths using gratia
#' set.seed(0)
#' dat <- mgcv::gamSim(1, n = 200, scale = 2)
#' mod <- mvgam(y ~ s(x1, bs = 'moi') +
#'               te(x0, x2),
#'              data = dat,
#'              family = gaussian(),
#'              chains = 2,
#'              silent = 2)
#'
#' if(require("gratia")){
#'  gratia::draw(mod)
#' }
#'
#'}

NULL



#' @rdname gratia_mvgam_enhancements
#' @aliases draw.mvgam
#' @export
#'
`drawDotmvgam` <- function(
    object,
    trend_effects = FALSE,
    data = NULL,
    select = NULL,
    parametric = FALSE,
    terms = NULL,
    residuals = FALSE,
    scales = c("free", "fixed"),
    ci_level = 0.95,
    n = 100,
    n_3d = 16,
    n_4d = 4,
    unconditional = FALSE,
    overall_uncertainty = TRUE,
    constant = NULL,
    fun = NULL,
    dist = 0.1,
    rug = TRUE,
    contour = TRUE,
    grouped_by = FALSE,
    ci_alpha = 0.2,
    ci_col = "black",
    smooth_col = "black",
    resid_col = "steelblue3",
    contour_col = "black",
    n_contour = NULL,
    partial_match = FALSE,
    discrete_colour = NULL,
    discrete_fill = NULL,
    continuous_colour = NULL,
    continuous_fill = NULL,
    position = "identity",
    angle = NULL,
    ncol = NULL, nrow = NULL,
    guides = "keep", widths = NULL, heights = NULL,
    crs = NULL,
    default_crs = NULL,
    lims_method = "cross",
    wrap = TRUE,
    envir = environment(formula(object)),
    ...
) {
  if(trend_effects){
    if(is.null(object$trend_call)){
      stop('no trend_formula exists so there are no trend-level terms to plot')
    }

    object$trend_mgcv_model <- relabel_gps(object$trend_mgcv_model)
    object$trend_mgcv_model$call$data <- NULL
    object$trend_mgcv_model$cmX <- object$trend_mgcv_model$coefficients
    sm_plots <- gratia::draw(object = object$trend_mgcv_model,
                             data = data,
                             select = select,
                             parametric = parametric,
                             terms = terms,
                             residuals = FALSE,
                             scales = scales,
                             ci_level = ci_level,
                             n = n,
                             n_3d = n_3d,
                             n_4d = n_4d,
                             unconditional = FALSE,
                             overall_uncertainty = FALSE,
                             constant = constant,
                             fun = fun,
                             dist = dist,
                             rug = rug,
                             contour = contour,
                             grouped_by = grouped_by,
                             ci_alpha = ci_alpha,
                             ci_col = ci_col,
                             smooth_col = smooth_col,
                             resid_col = "steelblue3",
                             contour_col = contour_col,
                             n_contour = n_contour,
                             partial_match = partial_match,
                             discrete_colour = discrete_colour,
                             discrete_fill = discrete_fill,
                             continuous_colour = continuous_colour,
                             continuous_fill = continuous_fill,
                             position = position,
                             angle = angle,
                             ncol = ncol,
                             nrow = nrow,
                             guides = guides,
                             widths = widths,
                             heights = heights,
                             crs = crs,
                             default_crs = default_crs,
                             lims_method = lims_method,
                             wrap = wrap,
                             envir = envir,
                             ...)
  } else {
    object$mgcv_model <- relabel_gps(object$mgcv_model)
    object$mgcv_model$call$data <- NULL
    object$mgcv_model$cmX <- object$mgcv_model$coefficients
    sm_plots <- gratia::draw(object = object$mgcv_model,
                             data = data,
                             select = select,
                             parametric = parametric,
                             terms = terms,
                             residuals = FALSE,
                             scales = scales,
                             ci_level = ci_level,
                             n = n,
                             n_3d = n_3d,
                             n_4d = n_4d,
                             unconditional = FALSE,
                             overall_uncertainty = FALSE,
                             constant = constant,
                             fun = fun,
                             dist = dist,
                             rug = rug,
                             contour = contour,
                             grouped_by = grouped_by,
                             ci_alpha = ci_alpha,
                             ci_col = ci_col,
                             smooth_col = smooth_col,
                             resid_col = "steelblue3",
                             contour_col = contour_col,
                             n_contour = n_contour,
                             partial_match = partial_match,
                             discrete_colour = discrete_colour,
                             discrete_fill = discrete_fill,
                             continuous_colour = continuous_colour,
                             continuous_fill = continuous_fill,
                             position = position,
                             angle = angle,
                             ncol = ncol,
                             nrow = nrow,
                             guides = guides,
                             widths = widths,
                             heights = heights,
                             crs = crs,
                             default_crs = default_crs,
                             lims_method = lims_method,
                             wrap = wrap,
                             envir = envir,
                             ...)
  }
  sm_plots
}

#' @rdname gratia_mvgam_enhancements
#' @aliases eval_smooth.hilbert.smooth
#' @export
eval_smoothDothilbertDotsmooth = function(smooth,
                                          model,
                                          n = 100,
                                          n_3d = NULL,
                                          n_4d = NULL,
                                          data = NULL,
                                          unconditional = FALSE,
                                          overall_uncertainty = TRUE,
                                          dist = NULL,
                                          ...) {
  insight::check_if_installed("gratia")
  model$cmX <- model$coefficients


  # deal with data if supplied
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = which_smooth(
      model,
      gratia::smooth_label(smooth)
    )
  )

  # by variables
  by_var <- gratia::by_variable(smooth)
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  # Compute the gp() eigenfunctions for newdata using the supplied brms_mock object
  # Requires a dataframe of all relevant variables for the gp effects
  mock_terms <- brms::brmsterms(attr(model, 'brms_mock')$formula)
  terms_needed <- unique(all.vars(mock_terms$formula)[-1])

  # Only use actual values of those covariates needed for this smooth
  terms_smooth <- intersect(terms_needed, colnames(data))
  newdata_mock <- data.frame(data[[terms_smooth[1]]])
  if(length(terms_smooth) > 1L){
    for(i in 2:length(terms_smooth)){
      newdata_mock <- cbind(newdata_mock,
                            data.frame(data[[terms_smooth[i]]]))
    }
  }
  colnames(newdata_mock) <- terms_smooth
  newdata_mock$.fake_gp_y <- rnorm(NROW(newdata_mock))

  # Fill in other covariates as fixed values from the original data
  other_terms <- setdiff(terms_needed, colnames(data))
  if(length(other_terms) > 0){
    newdata_mock <- cbind(newdata_mock,
                          do.call(cbind, lapply(seq_along(other_terms), function(x){
                            df <- data.frame(var = rep(model$model[[other_terms[x]]][1],
                                                       NROW(newdata_mock)))
                            colnames(df) <- other_terms[x]
                            df
                          })))
  }

  brms_mock_data <- brms::standata(attr(model, 'brms_mock'),
                                   newdata = newdata_mock,
                                   internal = TRUE)

  # Extract GP attributes
  gp_att_table <- attr(model, 'gp_att_table')
  bys <- unlist(purrr::map(gp_att_table, 'by'),
                use.names = FALSE)
  lvls <- unlist(purrr::map(gp_att_table, 'level'),
                 use.names = FALSE)

  # Extract eigenfunctions for each gp effect
  eigenfuncs <- eigenfunc_list(stan_data = brms_mock_data,
                               mock_df = newdata_mock,
                               by = bys,
                               level = lvls)

  # Which GP term are we plotting?
  gp_covariate <- smooth$term
  level <- ifelse(is.null(smooth$by.level), NA, smooth$by.level)
  gp_names <- gsub(' ', '', unlist(purrr::map(gp_att_table, 'name')))
  if(!is.na(level)){
    gp_select <- which(gp_names == smooth$label &
                         unlist(purrr::map(gp_att_table, 'level')) == level)
  } else {
    gp_select <- which(gp_names == smooth$label &
                         which(bys %in% by_var))
  }

  # Compute eigenfunctions for this GP term
  X <- eigenfuncs[[gp_select]]

  # Extract mean coefficients
  start <- purrr::map(gp_att_table, 'first_coef')[[gp_select]]
  end <- purrr::map(gp_att_table, 'last_coef')[[gp_select]]
  betas <- model$coefficients[start:end]
  fit <- as.vector(X %*% betas)

  ## want full vcov for component-wise CI
  V <- model$Vp

  ## variables for component-wise CIs for smooths
  column_means <- model[["cmX"]]
  lcms <- length(column_means)
  nc <- ncol(V)
  meanL1 <- smooth[["meanL1"]]
  eta_idx <- lss_eta_index(model)
  para.seq <- start:end

  if (isTRUE(overall_uncertainty) && attr(smooth, "nCons") > 0L) {
    if (lcms < nc) {
      column_means <- c(column_means, rep(0, nc - lcms))
    }
    Xcm <- matrix(column_means, nrow = nrow(X), ncol = nc, byrow = TRUE)
    if (!is.null(meanL1)) {
      Xcm <- Xcm / meanL1
    }
    Xcm[, para.seq] <- X
    # only apply the uncertainty from linear predictors of which this smooth
    # is a part of
    idx <- vapply(eta_idx, function(i, beta) any(beta %in% i),
                  FUN.VALUE = logical(1L), beta = para.seq
    )
    idx <- unlist(eta_idx[idx])
    rs <- rowSums((Xcm[, idx, drop = FALSE] %*%
                     V[idx, idx, drop = FALSE]) * Xcm[, idx, drop = FALSE])
  } else {
    rs <- rowSums((X %*% V[para.seq, para.seq, drop = FALSE]) * X)
  }

  ## standard error of the estimate
  se.fit <- sqrt(pmax(0, rs))

  # convert to the gratia tidy format
  label <- smooth$label

  ## identify which vars are needed for this smooth...
  keep_vars <- c(smooth$term, smooth$by)
  keep_vars <- keep_vars[!keep_vars %in% 'NA']

  ## ... then keep only those vars
  data <- dplyr::select(data, dplyr::all_of(keep_vars))

  ## tibble object
  tbl <- tibble::tibble(.smooth = rep(label, nrow(X)), .estimate = fit, .se = se.fit)

  ## bind on the data
  tbl <- dplyr::bind_cols(tbl, data)

  ## nest all columns with varying data
  eval_sm <- tidyr::nest(tbl, data = tidyr::all_of(c(".estimate", ".se", names(data))))

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)

  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = "GP")

  # set some values to NA if too far from the data
  if (gratia::smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- gratia::too_far_to_na(smooth,
                                     input = eval_sm,
                                     reference = model[["model"]],
                                     cols = c(".estimate", ".se"),
                                     dist = dist
    )
  }

  return(eval_sm)
}

#' @rdname gratia_mvgam_enhancements
#' @aliases eval_smooth.mod.smooth
#' @export
eval_smoothDotmodDotsmooth = function(smooth,
                                      model,
                                      n = 100,
                                      n_3d = NULL,
                                      n_4d = NULL,
                                      data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      dist = NULL,
                                      ...) {
  insight::check_if_installed("gratia")
  model$cmX <- model$coefficients

  ## deal with data if supplied
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = which_smooth(
      model,
      gratia::smooth_label(smooth)
    )
  )

  by_var <- gratia::by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  ## values of spline at data
  eval_sm <- gratia::spline_values(smooth,
                                   data = data,
                                   unconditional = unconditional,
                                   model = model,
                                   overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = "Mono dec P spline")

  # set some values to NA if too far from the data
  if (gratia::smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- gratia::too_far_to_na(smooth,
                                     input = eval_sm,
                                     reference = model[["model"]],
                                     cols = c(".estimate", ".se"),
                                     dist = dist
    )
  }
  ## return
  eval_sm
}

#' @rdname gratia_mvgam_enhancements
#' @aliases eval_smooth.moi.smooth
#' @export
eval_smoothDotmoiDotsmooth = function(smooth,
                                      model,
                                      n = 100,
                                      n_3d = NULL,
                                      n_4d = NULL,
                                      data = NULL,
                                      unconditional = FALSE,
                                      overall_uncertainty = TRUE,
                                      dist = NULL,
                                      ...) {

  insight::check_if_installed("gratia")
  model$cmX <- model$coefficients

  ## deal with data if supplied
  data <- process_user_data_for_eval(
    data = data, model = model,
    n = n, n_3d = n_3d, n_4d = n_4d,
    id = which_smooth(
      model,
      gratia::smooth_label(smooth)
    )
  )

  by_var <- gratia::by_variable(smooth) # even if not a by as we want NA later
  if (by_var == "NA") {
    by_var <- NA_character_
  }

  ## values of spline at data
  eval_sm <- gratia::spline_values(smooth,
                                   data = data,
                                   unconditional = unconditional,
                                   model = model,
                                   overall_uncertainty = overall_uncertainty
  )

  ## add on info regarding by variable
  eval_sm <- add_by_var_column(eval_sm, by_var = by_var)
  ## add on spline type info
  eval_sm <- add_smooth_type_column(eval_sm, sm_type = "Mono inc P spline")

  # set some values to NA if too far from the data
  if (gratia::smooth_dim(smooth) == 2L && (!is.null(dist) && dist > 0)) {
    eval_sm <- gratia::too_far_to_na(smooth,
                                     input = eval_sm,
                                     reference = model[["model"]],
                                     cols = c(".estimate", ".se"),
                                     dist = dist
    )
  }
  ## return
  eval_sm
}

#' Utility functions; full credit goes to Gavin Simpson, the developer and
#' maintainer of the gratia package
#' @noRd
`is.gamm` <- function(object) {
  inherits(object, "gamm")
}

#' @noRd
`is.gamm4` <- function(object) {
  is.list(object) & (!is.null(object[["gam"]]))
}

#' @noRd
`is.gam` <- function(object) {
  inherits(object, "gam")
}

#' @noRd
`is.bam` <- function(object) {
  inherits(object, "bam")
}

#' @noRd
`which_smooth` <- function(object, term) {
  if (is.gamm(object) || is.gamm4(object)) {
    object <- object[["gam"]]
  }
  smooths <- gratia::smooths(object)
  which(term == smooths)
}

#' @noRd
`process_user_data_for_eval` <- function(
    data, model, n, n_3d, n_4d, id,
    var_order = NULL) {
  if (is.null(data)) {
    data <- gratia::smooth_data(
      model = model,
      n = n,
      n_3d = n_3d,
      n_4d = n_4d,
      id = id,
      var_order = var_order
    )
  } else {
    smooth <- gratia::get_smooths_by_id(model, id)[[1L]]
    vars <- smooth_variable(smooth)
    by_var <- gratia::by_variable(smooth)
    if (!identical(by_var, "NA")) {
      vars <- append(vars, by_var)
    }
    ## if this is a by variable, filter the by variable for the required
    ## level now
    if (gratia::is_factor_by_smooth(smooth)) {
      data <- data %>% dplyr::filter(.data[[by_var]] == gratia::by_level(smooth))
    }
  }
  data
}

#' @noRd
`add_by_var_column` <- function(object, by_var, n = NULL) {
  if (is.null(n)) {
    n <- NROW(object)
  }
  insight::check_if_installed("tibble")
  tibble::add_column(object, .by = rep(by_var, times = n), .after = 1L)
}

#' @noRd
`add_smooth_type_column` <- function(object, sm_type, n = NULL) {
  if (is.null(n)) {
    n <- NROW(object)
  }
  insight::check_if_installed("tibble")
  tibble::add_column(object, .type = rep(sm_type, times = n), .after = 1L)
}

#' @noRd
lss_eta_index <- function(object){
  function (object)
  {
    lpi <- attr(formula(object), "lpi")
    if (is.null(lpi)) {
      lpi <- list(seq_along(coef(object)))
    }
    attr(lpi, "overlap") <- NULL
    lpi
  }
}

#' @noRd
smooth_variable <- function (smooth) {
  gratia::check_is_mgcv_smooth(smooth)
  smooth[["term"]]
}
