#' Posterior predictive checks for \pkg{mvgam} models
#'
#' Compares the observed response with replicates drawn from the fitted
#' model through the \pkg{bayesplot} `ppc_*` functions. Further types
#' plot randomised quantile residuals and, for `nmix()` and `occ()`
#' fits, a discrepancy statistic. Returns a \pkg{ggplot2} object.
#'
#' @aliases pp_check
#'
#' @inheritParams brms::pp_check
#' @inheritParams brms::prepare_predictions.brmsfit
#'
#' @importFrom stats terms formula weights
#' @importFrom brms do_call
#' @importFrom bayesplot pp_check color_scheme_set color_scheme_get
#'
#' @param object An object of class \code{mvgam}
#'
#' @param type Type of the check. A \pkg{bayesplot} `ppc_*` or `ppd_*`
#'   function named without its prefix, or one of the residual types
#'   mvgam adds: `"resid_hist"`, `"resid_hist_grouped"`,
#'   `"resid_ribbon"`, `"resid_ribbon_grouped"`, `"resid_qq"`,
#'   `"resid_vs_fitted"`, `"resid_acf"` and `"resid_pacf"`. Type
#'   `"fit_stat"` serves `nmix()` and `occ()` fits. The default is
#'   `"bars"` for a bernoulli or ordinal response and `"dens_overlay"`
#'   otherwise. Multi-response families take the defaults under
#'   "Multi-response families".
#'
#' @param newdata Optional data frame of the variables in the model.
#'   `NULL` (the default) checks the training data. The residual types
#'   describe the training data and refuse `newdata`.
#'
#' @param per_obs Logical. For `type = "resid_vs_fitted"`, `TRUE` (the
#'   default) draws each observation at its posterior median on both
#'   axes. `FALSE` draws every draw of every observation, which shows the
#'   posterior spread.
#'
#' @param series For the residual types `resid_hist`, `resid_ribbon`,
#'   `resid_qq`, `resid_vs_fitted`, `resid_acf` and `resid_pacf`, the
#'   one series whose residuals are drawn, as a name or an index. The
#'   default is the first series. Its residuals are taken in time order.
#'   `resid_acf` keeps missing responses in place. Its lags count time
#'   steps. `resid_pacf` drops them. Its lags count observations. A
#'   fit with several series names its series in the subtitle.
#'
#' @param ... Further arguments of [posterior_predict.mvgam()] and of
#'   the \pkg{bayesplot} function `type` names. Any other name is
#'   refused. `type = "fit_stat"` takes `stat`, either `"chi_squared"`
#'   (the default) or `"freeman_tukey"`.
#'
#' @return A ggplot object. A multivariate model given no `resp`
#'   returns one figure with a panel per response. `type = "fit_stat"`
#'   returns an `mvgam_ppc_fit_stat` object with print and plot methods.
#'
#' @details
#' Replicates of the training data are drawn under the latent trend
#' state the model inferred at each time, as [residuals.mvgam()] and
#' [hindcast.mvgam()] use. The check then compares the fitted
#' trajectory with the series that produced it. Replicates of `newdata`
#' take the covariate surface of [posterior_predict.mvgam()]. Passing
#' `incl_autocor` sets the surface explicitly. The `loo_*` types always
#' take the fitted state, which is the state their importance weights
#' come from.
#'
#' Overlay types default to 10 draws. Interval, ribbon and statistic
#' types use every draw. The residual histograms and ribbons default to
#' 500 draws, the other residual types to 100 and `fit_stat` to 500. A
#' posterior with fewer draws gives all of them.
#' `resid_hist` draws one histogram per draw and shows the first eight.
#'
#' The \code{\link[bayesplot:PPC-overview]{PPC}} documentation of
#' \pkg{bayesplot} describes each `ppc_*` type. A check restricted to
#' one series or covariate slice takes `newdata` filtered to those
#' rows. The `_grouped` types take `group = "series"` to draw one panel
#' per series.
#'
#' @section Multi-response families:
#'   `diri()`, `multi()`, `categ()`, `mvn()` and `mvt()` record several
#'   outcomes per unit. A pooled density would merge them into one
#'   curve. Without a `group` from the caller, `pp_check()` draws one
#'   panel per species through the type's `_grouped` variant. The
#'   default type is `"ribbon"` for a composition and `"dens_overlay"`
#'   for `mvn()` and `mvt()`. Passing a column of `newdata` as `group`
#'   draws the pooled plot.
#'
#' @section Detection families (`nmix()`, `occ()`):
#'   The visits of a closure unit share one latent state. `pp_check()`
#'   sums the observed and replicated visits of each unit and compares
#'   the sums. The count types (`bars`, `rootogram`, `hist`,
#'   `freqpoly`), the density and ECDF overlays, the statistic types,
#'   `intervals`, `ribbon`, `resid_hist` and `resid_qq` apply. Types
#'   that plot rows, residuals against fitted values or residuals over
#'   time are refused. A `group` or `x` column must be constant within
#'   each unit. `type = "fit_stat"` computes the chi-squared or
#'   Freeman-Tukey discrepancy of Gelman et al. (1996) from the unit
#'   sums.
#'
#' @seealso [residuals.mvgam()] returns the residuals the residual types
#'   plot. [loo.mvgam()] scores the fit. The
#'   `r mvgam_article("mvgam_overview", "overview article")` explains a
#'   rootogram. The `r mvgam_article("nmix", "N-mixture article")` runs
#'   the discrepancy check.
#'
#' @family model checking
#' @export pp_check
#'
#' @references
#' Gabry, J., Simpson, D., Vehtari, A., Betancourt, M. and
#' Gelman, A. (2019). Visualization in Bayesian workflow.
#' \emph{Journal of the Royal Statistical Society A}, 182:389-402.
#' \doi{10.1111/rssa.12378}
#'
#' Gelman, A., Meng, X.-L. and Stern, H. (1996). Posterior predictive
#' assessment of model fitness via realized discrepancies.
#' \emph{Statistica Sinica}, 6:733-760.
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # Observed counts against the predictive distribution. A bar
#' # hanging below zero marks a count the model under-predicts
#' pp_check(mod, type = "rootogram")
#'
#' # Randomised quantile residuals over time. A trend the model missed
#' # shows as serial structure
#' pp_check(mod, type = "resid_ribbon")
#'
#' # Next: plot(mod, type = "residuals") shows the residual grid
#' }
#'
#' @export
pp_check.mvgam <- function(
  object,
  type,
  ndraws = NULL,
  prefix = c("ppc", "ppd"),
  group = NULL,
  x = NULL,
  newdata = NULL,
  resp = NULL,
  draw_ids = NULL,
  per_obs = TRUE,
  series = NULL,
  ...
) {
  require_fitted_model(object, "pp_check")
  set_color_scheme_local()
  prefix <- match.arg(prefix)
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_flag(per_obs)
  checkmate::assert_string(group, null.ok = TRUE)
  checkmate::assert_string(x, null.ok = TRUE)
  dots <- list(...)
  type_missing <- missing(type)
  if (type_missing) {
    type <- "dens_overlay"
  }
  checkmate::assert_string(type)
  ndraws_given <- !missing(ndraws)

  resid_types <- c(
    "resid_hist", "resid_hist_grouped", "resid_ribbon",
    "resid_ribbon_grouped", "resid_acf", "resid_pacf", "resid_qq",
    "resid_vs_fitted"
  )
  resid_pooled <- resid_types[1:4]
  resid_diagnostic <- resid_types[5:8]
  resid_by_series <- c(
    "resid_hist", "resid_ribbon", "resid_acf", "resid_pacf",
    "resid_qq", "resid_vs_fitted"
  )
  closure_unit <- is_closure_unit_family(object$family)

  # Residuals are computed on the rows the model was fitted to.
  if (type %in% resid_types && !is.null(newdata)) {
    stop(insight::format_error(c(
      "The residual types describe the training data.",
      i = "Drop 'newdata' from this call."
    )), call. = FALSE)
  }
  if (!is.null(series)) {
    if (!type %in% resid_by_series) {
      stop(insight::format_error(c(
        "'series' selects the series of a residual plot.",
        x = paste0("Got type = '", type, "'."),
        i = paste0(
          "Use it with ", paste0("'", resid_by_series, "'", collapse = ", "),
          ", or subset 'newdata' for the other checks."
        )
      )), call. = FALSE)
    }
    refuse_closure_unit_series(object, series)
  }
  # Whether the fit saw these rows decides the surface of every panel.
  in_sample <- is.null(newdata)
  if (in_sample) {
    newdata <- object$data
  }

  # A multi-response family draws one panel per species. A `group` from
  # the caller keeps the pooled plot.
  mv_cat <- NULL
  if (is.null(group) && is_multi_response_family(object$family)) {
    mv_cat <- pp_check_mv_category(object, newdata)
    if (!is.null(mv_cat)) {
      if (type_missing) {
        type <- if (is_simplex_response_family(object$family)) {
          "ribbon"
        } else {
          "dens_overlay"
        }
      }
      grouped_type <- paste0(type, "_grouped")
      if (paste0("ppc_", grouped_type) %in% bayesplot::available_ppc("")) {
        type <- grouped_type
        newdata[["_mvgam_pp_category"]] <- mv_cat
        group <- "_mvgam_pp_category"
      } else {
        mv_cat <- NULL
      }
    }
  }

  # A model with several responses returns one figure with a panel per
  # response when `resp` is `NULL`.
  fan <- mv_resp_fan_out(object, resp, combine = stack_response_plots)
  if (!is.null(fan)) return(fan)
  resolve_resp(object, resp)

  # A density overlay of a binary or ordinal response draws smooth
  # curves over a few discrete values. Bars show the share of each.
  if (type_missing && is.null(mv_cat)) {
    fam <- model_families(object, resp)
    if (identical(resolve_family_name(fam), "bernoulli") ||
          is_ordinal_family(fam)) {
      type <- "bars"
    }
  }

  valid_types <- sub(
    "^pp[cd]_", "",
    if (prefix == "ppc") {
      c(bayesplot::available_ppc(""), resid_types, "fit_stat")
    } else {
      bayesplot::available_ppd("")
    }
  )
  if (!type %in% valid_types) {
    stop(insight::format_error(c(
      paste0("'type' must be a valid '", prefix, "' type."),
      x = paste0("Got '", type, "'."),
      i = paste0("Valid types: ",
                 paste0("'", sort(valid_types), "'", collapse = ", "), ".")
    )), call. = FALSE)
  }

  # The bayesplot kernel that draws this type. `fit_stat` and the four
  # diagnostic residual types draw themselves.
  bptype <- sub("^resid_hist", "error_hist",
                sub("^resid_ribbon", "ribbon", type))
  ppc_fun <- if (type %in% c("fit_stat", resid_diagnostic)) {
    NULL
  } else {
    get(paste0(prefix, "_", bptype), asNamespace("bayesplot"))
  }
  ppc_formals <- if (is.null(ppc_fun)) character(0L) else {
    names(formals(ppc_fun))
  }

  # `...` holds the prediction method's arguments and the kernel's.
  # `stat` stays in `...`. Ten bayesplot kernels declare one of their
  # own.
  method <- if (identical(type, "error_binned")) {
    "posterior_epred"
  } else {
    "posterior_predict"
  }
  pred_formals <- names(formals(switch(
    method,
    posterior_epred = posterior_epred.mvgam,
    posterior_predict = posterior_predict.mvgam
  )))
  allowed <- c(pred_formals, ppc_formals,
               if (identical(type, "fit_stat")) "stat")
  refuse_unread_dots(dots, allowed, "pp_check")

  if (closure_unit) {
    closure_unit_blocked <- c(
      "scatter_avg", "scatter_avg_grouped",
      "error_scatter_avg", "error_scatter_avg_vs_x",
      "error_binned",
      "resid_acf", "resid_pacf", "resid_vs_fitted",
      "resid_ribbon", "resid_ribbon_grouped"
    )
    if (type %in% closure_unit_blocked) {
      stop(insight::format_error(c(
        paste0(
          "pp_check(type = '", type, "') is not supported for ",
          "closure-unit family '", resolve_family_name(object$family), "'."
        ),
        x = paste0(
          "Row scatter plots, residuals against fitted values and ",
          "residuals over time need one value per row."
        ),
        i = paste0(
          "Use type = 'bars', 'rootogram', 'dens_overlay', ",
          "'ecdf_overlay', 'intervals', 'stat', 'resid_hist' ",
          "or 'resid_qq' for closure-unit fits."
        )
      )), call. = FALSE)
    }
  } else if (identical(type, "fit_stat")) {
    # A composition's units sum to one whatever the fit. Its discrepancy
    # would give a p-value near 0.5 for every model.
    stop(insight::format_error(c(
      "pp_check(type = 'fit_stat') applies to the detection families.",
      x = paste0(
        "Family '", resolve_family_name(object$family),
        "' records one observation per row."
      ),
      i = "Use family = occ() or family = nmix() for this check."
    )), call. = FALSE)
  }

  for (v in c(group, x)) {
    if (!v %in% names(newdata)) {
      stop(insight::format_error(
        paste0("Variable '", v, "' is absent from the data.")
      ), call. = FALSE)
    }
  }
  if ("group" %in% ppc_formals && is.null(group)) {
    stop(insight::format_error(
      paste0("Argument 'group' is required for ppc type '", type, "'.")
    ), call. = FALSE)
  }

  # Default draw counts. A family without a distribution spec takes its
  # residuals from the empirical PIT. Below about 50 draws an
  # observation in the predictive tail falls outside the PIT support.
  # Its residual then clamps near 8 in magnitude.
  if (!ndraws_given && is.null(draw_ids)) {
    all_draw_types <- c(
      "error_scatter_avg", "error_scatter_avg_vs_x", "intervals",
      "intervals_grouped", "loo_intervals", "loo_pit", "loo_pit_overlay",
      "loo_pit_qq", "loo_ribbon", "pit_ecdf", "pit_ecdf_grouped",
      "ribbon", "ribbon_grouped", "rootogram", "scatter_avg",
      "scatter_avg_grouped", "stat", "stat_2d", "stat_freqpoly_grouped",
      "stat_grouped", "violin_grouped"
    )
    ndraws <- if (type %in% all_draw_types) {
      NULL
    } else if (type %in% c(resid_pooled, "fit_stat")) {
      500L
    } else if (type %in% resid_diagnostic) {
      100L
    } else {
      10L
    }
    # A posterior smaller than the default gives every draw.
    if (!is.null(ndraws) && ndraws >= posterior::ndraws(object)) {
      ndraws <- NULL
    }
  } else if (type %in% resid_pooled &&
               !has_analytic_pit(model_families(object, resp))) {
    n_used <- if (is.null(draw_ids)) ndraws else length(draw_ids)
    if (!is.null(n_used) && n_used < 50L) {
      warn_once(
        c(
          paste0("'", type, "' from fewer than 50 draws may show ",
                 "spurious extreme residuals."),
          x = paste0("An empirical PIT from few draws underflows in ",
                     "the predictive tail."),
          i = "Use ndraws = 500 for a stable PIT."
        ),
        "mvgam_pp_check_resid_low_ndraws"
      )
    }
  }
  # Replicates, expectations, residuals and importance weights all take
  # these draws.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  if (type %in% resid_diagnostic) {
    return(resid_diagnostic_panels(
      object, type, draw_ids = draw_ids, resp = resp, series = series,
      per_obs = per_obs
    )[[1L]])
  }
  if (identical(type, "fit_stat")) {
    return(closure_unit_fit_stat_ppc(
      object = object, newdata = newdata,
      stat = dots$stat %||% "chi_squared", group = group,
      draw_ids = draw_ids, in_sample = in_sample
    ))
  }

  y <- NULL
  if (prefix == "ppc") {
    y <- newdata[[response_column(object, resp)]]
    # bayesplot takes a numeric `y`. An ordered factor becomes its codes.
    if (is.factor(y)) {
      y <- as.integer(y)
    }
  }

  # A `loo_*` check weights its replicates by importance weights from
  # the likelihood. The likelihood scores each observation under the
  # fitted latent state. The replicates take the same state.
  psis_weighted <- any(c("psis_object", "lw") %in% ppc_formals)
  for_pred <- names(dots) %in% pred_formals
  series_sel <- NULL
  if (type %in% resid_types) {
    # A residual plot takes `y` as zero and `yrep` as minus the residual.
    # bayesplot's error convention then points each residual its own way.
    y[!is.na(y)] <- 0
    yrep <- -1 * residuals(
      object, summary = FALSE, draw_ids = draw_ids, resp = resp
    )
    # Residual columns follow the rows of the training frame. An
    # autocorrelation or a ribbon describes one series in time order.
    if (type %in% resid_by_series && !closure_unit) {
      series_sel <- resid_series_rows(object, newdata, series)
      y <- y[series_sel$rows]
      yrep <- yrep[, series_sel$rows, drop = FALSE]
    }
    if (type %in% c("resid_hist", "resid_hist_grouped")) {
      yrep <- yrep[seq_len(min(nrow(yrep), 8L)), , drop = FALSE]
    }
  } else {
    pred_args <- c(
      list(object, newdata = newdata, draw_ids = draw_ids, resp = resp),
      dots[for_pred]
    )
    yrep <- do_call(method, diagnostic_surface_args(
      pred_args, in_sample, weighted = psis_weighted
    ))
  }

  # A detection family compares unit sums. Residuals arrive per unit
  # already. `group` and `x` take each unit's first visit.
  closure_unit_lookup <- NULL
  if (closure_unit) {
    cu <- closure_unit_pp_check_setup(
      object = object, newdata = newdata, yrep = yrep, type = type
    )
    y <- cu$y
    yrep <- cu$yrep
    closure_unit_lookup <- cu$first_visits
    for (v in c(group, x)) {
      refuse_unit_varying(newdata[[v]], cu$arrays, v, PP_CHECK_UNIT_REASON)
    }
  }

  # Missing responses are dropped at the grain `y` now has: the unit for
  # a detection family, the row otherwise.
  take <- NULL
  if (anyNA(y)) {
    inform_once(
      "Observations with a missing response are omitted from the plot.",
      id = "mvgam_pp_check_missing_response"
    )
    take <- !is.na(y)
    y <- y[take]
    yrep <- yrep[, take, drop = FALSE]
  }
  # A covariate column at the grain of `y`
  column_at_grain <- function(v) {
    out <- newdata[[v]]
    if (!is.null(closure_unit_lookup)) out <- out[closure_unit_lookup]
    if (!is.null(take)) out <- out[take]
    out
  }

  ppc_args <- if (prefix == "ppc") {
    list(y = y, yrep = yrep)
  } else {
    list(ypred = yrep)
  }
  if (!is.null(group)) {
    ppc_args$group <- column_at_grain(group)
  }
  if (!is.null(x)) {
    ppc_args$x <- column_at_grain(x)
    if (!is.factor(ppc_args$x) && !is.character(ppc_args$x) &&
          !is.logical(ppc_args$x)) {
      ppc_args$x <- as.numeric(ppc_args$x)
    }
  }
  if (psis_weighted) {
    ll <- clean_ll(log_lik(object, newdata = newdata, incl_autocor = TRUE,
                           resp = resp, draw_ids = draw_ids))
    # `loo::psis()` warns on unusable weights. The LOO-PIT panel shows no
    # other sign of them.
    psis_obj <- loo::psis(
      -ll, r_eff = mvgam_r_eff_log_lik(object, ll, draw_ids = draw_ids)
    )
    if ("psis_object" %in% ppc_formals) ppc_args$psis_object <- psis_obj
    if ("lw" %in% ppc_formals) ppc_args$lw <- stats::weights(psis_obj)
  }

  out_plot <- do_call(ppc_fun, c(ppc_args, dots[!for_pred]))
  if ("x" %in% ppc_formals && !is.null(x)) {
    out_plot <- out_plot + ggplot2::labs(x = x)
  }
  if (type %in% c("resid_hist", "resid_hist_grouped")) {
    out_plot <- out_plot + ggplot2::labs(x = "DS residuals")
  }
  if (type %in% c("resid_ribbon", "resid_ribbon_grouped")) {
    out_plot <- out_plot +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "DS residuals")
  }
  label_resid_series(out_plot, series_sel)
}


#' Refuse a series selection for a closure-unit fit
#'
#' @param object Fitted `mvgam` object
#' @param series Series name or index, or `NULL`
#' @return `NULL`, invisibly
#' @noRd
refuse_closure_unit_series <- function(object, series) {
  if (!is.null(series) && is_closure_unit_family(object$family)) {
    stop(insight::format_error(c(
      "'series' is not supported for closure-unit residuals.",
      x = "A closure-unit family has one residual per closure unit."
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' The diagnostic residual panels of one series
#'
#' `pp_check()` draws one of these panels. `plot(type = "residuals")`
#' draws all four. The randomised quantile residuals are drawn once.
#' Every panel of the grid then shows the same residual draws.
#'
#' @param object Fitted `mvgam` object
#' @param types Character vector of panel types: `"resid_vs_fitted"`,
#'   `"resid_qq"`, `"resid_acf"` or `"resid_pacf"`
#' @param draw_ids Integer draw indices, or `NULL` for every draw
#' @param resp Character response name, or `NULL`
#' @param series Series name or index, or `NULL`
#' @param per_obs Logical flag passed to `build_resid_vs_fitted_panel()`
#' @return List of `ggplot` objects named by type
#' @noRd
resid_diagnostic_panels <- function(object, types, draw_ids = NULL,
                                    resp = NULL, series = NULL,
                                    per_obs = TRUE) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_subset(
    types, c("resid_vs_fitted", "resid_qq", "resid_acf", "resid_pacf"),
    empty.ok = FALSE
  )
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_flag(per_obs)
  refuse_closure_unit_series(object, series)
  data <- object$data
  resid <- residuals(object, summary = FALSE, draw_ids = draw_ids,
                     resp = resp)
  # A closure-unit residual describes a unit. Its columns follow the
  # units and none is missing.
  series_sel <- NULL
  observed <- rep(TRUE, ncol(resid))
  if (!is_closure_unit_family(object$family)) {
    # An autocorrelation describes one series in time order
    series_sel <- resid_series_rows(object, data, series)
    resid <- resid[, series_sel$rows, drop = FALSE]
    observed <- !is.na(data[[response_column(object, resp)]][series_sel$rows])
  }
  # The lag panels keep a missing response in place. Its lags count
  # time steps.
  if (!all(observed) &&
        any(types %in% c("resid_vs_fitted", "resid_qq"))) {
    inform_once(
      "Observations with a missing response are omitted from the plot.",
      id = "mvgam_pp_check_missing_response"
    )
  }
  panels <- lapply(stats::setNames(nm = types), function(type) {
    switch(
      type,
      resid_acf = ,
      resid_pacf = build_resid_lag_panel(
        resid, lag_type = sub("resid_", "", type)
      ),
      resid_qq = build_resid_qq_panel(resid[, observed, drop = FALSE]),
      resid_vs_fitted = {
        # Both axes take the surface `residuals()` conditions on
        fitted_draws <- do.call(posterior_epred, diagnostic_surface_args(
          list(object, newdata = data, draw_ids = draw_ids, resp = resp),
          in_sample = TRUE
        ))
        if (!is.null(series_sel)) {
          fitted_draws <- fitted_draws[, series_sel$rows, drop = FALSE]
        }
        build_resid_vs_fitted_panel(
          resid[, observed, drop = FALSE],
          fitted_draws[, observed, drop = FALSE],
          per_obs = per_obs
        )
      }
    )
  })
  lapply(panels, label_resid_series, series_sel = series_sel)
}


#' The rows of one series in the residual frame, in time order
#'
#' A fit without a series axis keeps every row. A response-keyed frame
#' is one such fit.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame the residuals are computed on
#' @param series Series name or index, or `NULL` for the first series
#' @return List holding the row indices in `rows`, the series name in
#'   `label` and the number of series in `n_series`. `label` is `NULL`
#'   for a fit without a series axis.
#' @noRd
resid_series_rows <- function(object, data, series = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data)
  )
  row_series <- axis_row_series(object, data)
  levs <- if (is.null(row_series)) character(0L) else levels(row_series)
  if (length(levs) == 0L) {
    if (!is.null(series)) {
      stop(insight::format_error(c(
        "'series' selects among the series a model records.",
        x = "The fit records no series axis."
      )))
    }
    rows <- seq_len(nrow(data))
    label <- NULL
  } else {
    idx <- resolve_series(series %||% 1L, levs)
    if (length(idx) != 1L) {
      stop(insight::format_error(c(
        "A residual plot shows one series.",
        x = paste0("Got ", length(idx), " of ", length(levs), " series."),
        i = "Pass one series name or index to 'series'."
      )))
    }
    label <- levs[idx]
    rows <- which(row_series == label)
    if (length(rows) == 0L) {
      stop(insight::format_error(
        paste0("Series '", label, "' has no rows in the training data.")
      ))
    }
  }
  time_var <- axis_vars(object)$time_var
  if (time_var %in% names(data)) {
    rows <- rows[order(data[[time_var]][rows])]
  }
  list(rows = rows, label = label, n_series = length(levs))
}


#' A residual plot subtitled with the series it shows
#'
#' Only fits with more than one series get the subtitle.
#'
#' @param p A `ggplot` object
#' @param series_sel List from `resid_series_rows()`, or `NULL`
#' @return `p`, with a subtitle when the fit has several series
#' @noRd
label_resid_series <- function(p, series_sel) {
  checkmate::assert_class(p, "ggplot")
  checkmate::assert_list(series_sel, null.ok = TRUE)
  if (is.null(series_sel$label) || series_sel$n_series < 2L) {
    return(p)
  }
  p + ggplot2::labs(subtitle = paste0("Series: ", series_sel$label))
}


#' The category of each row of a multi-response frame
#'
#' A posterior predictive check on a multi-response family draws one
#' panel per category. A composition's categories (`diri()`, `multi()`,
#' `categ()`) and a multivariate normal's responses (`mvn()`, `mvt()`)
#' are its species. The species axis of the fit names each row's
#' species and orders the levels. A site missing one species keeps the
#' labels of the others.
#'
#' @param object Fitted `mvgam` object
#' @param newdata Data frame of the prediction rows
#' @return Factor of categories, one per row, or `NULL` for a family
#'   without categories
#' @noRd
pp_check_mv_category <- function(object, newdata) {
  fam <- object$family
  if (!is_multi_response_family(fam)) return(NULL)
  series <- axis_row_series(object, newdata)
  if (is.null(series)) {
    return(NULL)
  }
  # bayesplot draws one panel per level. A species absent from the
  # frame would draw as an empty panel.
  droplevels(series)
}


# Why `pp_check()` refuses a `group` or `x` that varies within a unit
PP_CHECK_UNIT_REASON <- "A closure-unit check plots one value per unit."

#' Observations and replicates of a closure-unit fit at the unit grain
#'
#' A closure unit's visits share one latent state, making the unit the
#' grain a posterior predictive check compares. A unit's observed value
#' is the sum over its visits, the sufficient statistic for that state.
#' Residual types arrive already aggregated by `residuals.mvgam()`.
#' Those plots treat `y` as zero. The observed side then needs only
#' zeros of unit length.
#'
#' @param object Fitted `mvgam` object with a closure-unit family
#' @param newdata Data frame of the prediction rows
#' @param yrep Numeric matrix of replicates, one row per draw
#' @param type Character `pp_check()` plot type
#' @return List holding `y`, `yrep`, `arrays` and `first_visits`. The
#'   caller uses `first_visits` to keep one covariate value per unit.
#' @noRd
closure_unit_pp_check_setup <- function(object, newdata, yrep,
                                          type) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert(
    checkmate::check_data_frame(newdata),
    checkmate::check_list(newdata)
  )
  checkmate::assert_matrix(yrep)
  checkmate::assert_string(type)
  arrays <- closure_unit_arrays_for(object, newdata)
  if (grepl("resid", type)) {
    # `yrep` holds per-unit residuals. The observed side is zero.
    y_unit <- rep(0, arrays$N_unit)
    yrep_unit <- yrep
  } else {
    agg <- aggregate_closure_unit_visits(
      object, newdata = newdata, yrep_visit = yrep
    )
    y_unit <- agg$y_unit
    yrep_unit <- agg$yrep_unit
  }
  list(
    y = y_unit, yrep = yrep_unit, arrays = arrays,
    first_visits = closure_unit_first_rows(arrays)
  )
}


#' An ACF or pACF panel of residual draws
#'
#' Computes the ACF or pACF of each draw, then summarises across draws
#' with nested quantile segments at each integer lag. Segments mark the
#' discrete lags. The dashed band is the asymptotic 95% white-noise
#' interval `+/- 1.96 / sqrt(n)`.
#'
#' @param resid_draws Numeric residual matrix with dimensions
#'   `[ndraws, nobs]`
#' @param lag_type Character panel type, `"acf"` or `"pacf"`
#' @return A `ggplot` object
#' @noRd
build_resid_lag_panel <- function(
  resid_draws, lag_type = c("acf", "pacf")
) {
  checkmate::assert_matrix(resid_draws)
  lag_type <- match.arg(lag_type)
  acf_fn <- if (lag_type == "acf") stats::acf else stats::pacf
  ndraws <- nrow(resid_draws)
  # The ACF estimates each lag from the pairs both observed. A missing
  # value stays in place. Its lags count time steps. The pACF recursion
  # needs an ACF from a complete series. A series with gaps gives
  # partial correlations outside [-1, 1]. The pACF drops the missing
  # values. Its lags count observations.
  per_draw <- lapply(seq_len(ndraws), function(d) {
    x <- resid_draws[d, ]
    if (lag_type == "pacf") {
      x <- x[!is.na(x)]
    }
    out <- acf_fn(x, plot = FALSE, na.action = stats::na.pass)
    data.frame(
      value = out$acf[, , 1L],
      lag = out$lag[, 1L, 1L],
      n_used = sum(!is.na(x))
    )
  })
  lag_df <- do.call(rbind, per_draw)
  lag_df <- lag_df[lag_df$lag > 0, , drop = FALSE]

  by_lag <- split(lag_df$value, lag_df$lag)
  qfun <- function(p) {
    vapply(by_lag, stats::quantile, numeric(1L),
      probs = p, na.rm = TRUE, names = FALSE
    )
  }
  bands <- data.frame(
    lag = as.numeric(names(by_lag)),
    q025 = qfun(0.025), q975 = qfun(0.975),
    q100 = qfun(0.10),  q900 = qfun(0.90),
    q250 = qfun(0.25),  q750 = qfun(0.75)
  )

  n_used <- per_draw[[1L]]$n_used[1L]
  ci_bound <- stats::qnorm(0.975) / sqrt(n_used)
  palette <- mvgam_palette()
  # The title names the statistic. The residual figure stacks this
  # panel under the Q-Q panel, where a y title would join the Q-Q one.
  title <- if (lag_type == "acf") "ACF" else "pACF"

  ggplot2::ggplot(bands, ggplot2::aes(x = lag)) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1) * ci_bound,
      linetype = "dashed", colour = "black"
    ) +
    ggplot2::geom_hline(
      yintercept = 0, colour = palette[6L], linewidth = 0.25
    ) +
    ggplot2::geom_segment(
      colour = palette[1L], linewidth = 1.5,
      ggplot2::aes(y = q025, yend = q975)
    ) +
    ggplot2::geom_segment(
      colour = palette[3L], linewidth = 1.5,
      ggplot2::aes(y = q100, yend = q900)
    ) +
    ggplot2::geom_segment(
      colour = palette[6L], linewidth = 1.5,
      ggplot2::aes(y = q250, yend = q750)
    ) +
    ggplot2::labs(title = title, x = "Lag", y = NULL) +
    mvgam_theme()
}


#' A normal Q-Q panel of residual draws
#'
#' Pools every draw under one reference line.
#'
#' @param resid_draws Numeric residual matrix, one row per draw
#' @return A `ggplot` object
#' @noRd
build_resid_qq_panel <- function(resid_draws) {
  checkmate::assert_numeric(resid_draws)
  palette <- mvgam_palette()
  df <- data.frame(resids = as.numeric(resid_draws))
  ggplot2::ggplot(df, ggplot2::aes(sample = resids)) +
    ggplot2::stat_qq_line(colour = palette[5L], linewidth = 1) +
    ggplot2::stat_qq(
      shape = 16, colour = "white", size = 1.25, alpha = 0.4
    ) +
    ggplot2::stat_qq(
      shape = 16, colour = "black", size = 1, alpha = 0.4
    ) +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    mvgam_theme()
}


#' A residuals-against-fitted panel of residual draws
#'
#' With `per_obs = TRUE` each observation is its posterior median on
#' both axes, one point per observation as in `plot.lm()`. With
#' `per_obs = FALSE` every draw of every observation is plotted. That
#' view shows the per-draw spread of discrete-PIT residuals at low
#' fitted values. A thin-plate GAM smooth traces any trend in the mean.
#'
#' @param resid_draws Numeric residual matrix, one row per draw
#' @param fitted_draws Numeric fitted matrix shaped as `resid_draws`
#' @param per_obs Logical flag to plot one median point per observation
#' @return A `ggplot` object
#' @noRd
build_resid_vs_fitted_panel <- function(
  resid_draws, fitted_draws, per_obs = TRUE
) {
  checkmate::assert_matrix(resid_draws)
  checkmate::assert_matrix(fitted_draws)
  checkmate::assert_flag(per_obs)
  palette <- mvgam_palette()
  if (per_obs) {
    df <- data.frame(
      preds = apply(fitted_draws, 2L, stats::median, na.rm = TRUE),
      resids = apply(resid_draws, 2L, stats::median, na.rm = TRUE)
    )
    point_size <- 1.5
    point_alpha <- 1
  } else {
    df <- data.frame(
      preds = as.numeric(fitted_draws),
      resids = as.numeric(resid_draws)
    )
    point_size <- 1
    point_alpha <- 0.4
  }
  df <- df[stats::complete.cases(df), , drop = FALSE]
  ggplot2::ggplot(df, ggplot2::aes(x = preds, y = resids)) +
    ggplot2::geom_point(
      shape = 16, colour = "white",
      size = point_size + 0.25, alpha = point_alpha
    ) +
    ggplot2::geom_point(
      shape = 16, colour = "black",
      size = point_size, alpha = point_alpha
    ) +
    ggplot2::geom_smooth(
      method = "gam", formula = y ~ s(x, bs = "cs"),
      colour = paste0(palette[6L], "60"),
      fill = paste0(palette[6L], "40")
    ) +
    ggplot2::labs(
      title = "Resids vs Fitted",
      x = "Fitted values",
      y = "DS residuals"
    ) +
    mvgam_theme()
}


#' A discrepancy check of a closure-unit fit
#'
#' Follows Gelman et al. (1996). For each draw `s` the observed
#' discrepancy is `D(y_obs, E[y | theta_s])`. The replicated
#' discrepancy is `D(y_rep_s, E[y | theta_s])`. `D` is the
#' chi-squared statistic `sum((y - e)^2 / (e + eps))` or the
#' Freeman-Tukey statistic `sum((sqrt(y) - sqrt(e))^2)`. Replicates and
#' expectations are summed to the unit grain with
#' `aggregate_closure_unit_visits()`. A `group` column of unit-constant
#' values sums them further by level. The Bayesian p-value is the
#' posterior probability of `T_rep >= T_obs`.
#'
#' @param object Fitted `mvgam` object with a closure-unit family
#' @param newdata Data frame of the checked rows
#' @param stat Character statistic, `"chi_squared"` or
#'   `"freeman_tukey"`
#' @param group Name of a unit-constant grouping column, or `NULL`
#' @param draw_ids Integer vector of draw indices, or `NULL` for all
#' @param in_sample Logical flag for a check of the training data
#' @return A `mvgam_ppc_fit_stat` list holding the discrepancies
#'   `T_obs` and `T_rep` of each draw and the p-value `bayes_p`
#' @noRd
closure_unit_fit_stat_ppc <- function(object, newdata, stat, group,
                                      draw_ids, in_sample) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata)
  checkmate::assert_string(stat)
  checkmate::assert_string(group, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_flag(in_sample)
  stat <- match.arg(stat, c("chi_squared", "freeman_tukey"))

  # Each replicate is paired with the expectation of its own draw, on
  # the surface the other checks use.
  args <- diagnostic_surface_args(
    list(object, newdata = newdata, draw_ids = draw_ids), in_sample
  )
  yrep_visit <- do.call(posterior_predict, args)
  epred_visit <- do.call(posterior_epred, args)

  # The second call sums the expectations to the unit grain. Its
  # `y_unit` repeats the first call's.
  agg_y <- aggregate_closure_unit_visits(
    object, newdata = newdata, yrep_visit = yrep_visit
  )
  agg_e <- aggregate_closure_unit_visits(
    object, newdata = newdata, yrep_visit = epred_visit
  )
  y_unit     <- agg_y$y_unit
  yrep_unit  <- agg_y$yrep_unit
  epred_unit <- agg_e$yrep_unit
  arrays     <- agg_y$arrays

  if (!is.null(group)) {
    refuse_unit_varying(newdata[[group]], arrays, group,
                        PP_CHECK_UNIT_REASON)
    group_keys <- as.character(
      newdata[[group]][closure_unit_first_rows(arrays)]
    )
    unit_to_grp <- match(group_keys, unique(group_keys))
    y_unit     <- as.numeric(tapply(y_unit, unit_to_grp, sum))
    yrep_unit  <- t(rowsum(t(yrep_unit),  group = unit_to_grp))
    epred_unit <- t(rowsum(t(epred_unit), group = unit_to_grp))
    grain <- group
  } else {
    grain <- "closure unit"
  }

  D <- if (identical(stat, "chi_squared")) {
    function(y, e) sum((y - e)^2 / (e + 1e-6))
  } else {
    function(y, e) sum((sqrt(pmax(0, y)) - sqrt(pmax(0, e)))^2)
  }
  S <- nrow(yrep_unit)
  T_obs <- vapply(seq_len(S),
                  function(s) D(y_unit, epred_unit[s, ]),
                  numeric(1L))
  T_rep <- vapply(seq_len(S),
                  function(s) D(yrep_unit[s, ], epred_unit[s, ]),
                  numeric(1L))
  bayes_p <- mean(T_rep >= T_obs)

  structure(
    list(
      stat    = stat,    group   = group,   grain   = grain,
      T_obs   = T_obs,   T_rep   = T_rep,   bayes_p = bayes_p,
      n_draws = S,       family  = resolve_family_name(object$family)
    ),
    class = c("mvgam_ppc_fit_stat", "list")
  )
}


#' Print a closure-unit GOF discrepancy result
#'
#' @param x A `mvgam_ppc_fit_stat` object returned by
#'   `pp_check(fit, type = "fit_stat")`.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @author Nicholas J Clark
#' @method print mvgam_ppc_fit_stat
#' @export
print.mvgam_ppc_fit_stat <- function(x, ...) {
  stat_lab <- fit_stat_label(x$stat)
  cat("Posterior predictive check (closure-unit discrepancy)\n")
  cat(sprintf("  Family:           %s\n",       x$family))
  cat(sprintf("  Statistic:        %s\n",       stat_lab))
  cat(sprintf("  Aggregation:      %s\n",       x$grain))
  cat(sprintf("  Posterior draws:  %d\n",       x$n_draws))
  cat(sprintf("  Bayesian p-value: %.3f\n",     x$bayes_p))
  # A small p-value means the replicates rarely reach the observed
  # discrepancy. A large one means the observed data deviate less from
  # the fitted expectation than replicates do. Terms flexible enough to
  # follow the data give that.
  if (x$bayes_p < 0.05) {
    cat("  Note: replicates rarely reach the observed discrepancy.",
        "The model may not fit.\n")
  } else if (x$bayes_p > 0.95) {
    cat("  Note: the observed data deviate less from the fitted",
        "expectation than replicates do.\n")
  }
  invisible(x)
}


#' Display label of a closure-unit discrepancy statistic
#'
#' @param stat Character statistic, `"chi_squared"` or
#'   `"freeman_tukey"`
#' @return Character label
#' @noRd
fit_stat_label <- function(stat) {
  if (identical(stat, "chi_squared")) "chi-squared" else "Freeman-Tukey"
}


#' Plot a closure-unit GOF discrepancy result
#'
#' Scatter of `T(y_rep, theta)` against `T(y_obs, theta)` per
#' posterior draw, with the 1:1 reference line and the Bayesian
#' p-value annotated in the subtitle.
#'
#' @param x A `mvgam_ppc_fit_stat` object.
#' @param ... Unused. Anything passed here is refused.
#' @return A `ggplot` object.
#' @author Nicholas J Clark
#' @method plot mvgam_ppc_fit_stat
#' @export
plot.mvgam_ppc_fit_stat <- function(x, ...) {
  rlang::check_dots_empty()
  d <- data.frame(T_obs = x$T_obs, T_rep = x$T_rep)
  rng <- range(c(d$T_obs, d$T_rep))
  stat_lab <- fit_stat_label(x$stat)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$T_obs, y = .data$T_rep)) +
    ggplot2::geom_point(alpha = 0.4, colour = mvgam_colour("dark")) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = 2, colour = "grey40") +
    ggplot2::coord_equal(xlim = rng, ylim = rng) +
    ggplot2::labs(
      title = paste0(
        "PPC: ", stat_lab,
        " discrepancy (", x$grain, " grain)"
      ),
      subtitle = paste0("Bayesian p-value = ",
                        format(round(x$bayes_p, 3), nsmall = 3)),
      x = "T(y_obs, theta)", y = "T(y_rep, theta)"
    ) +
    mvgam_theme()
}


#' The residual diagnostic grid of one series
#'
#' `plot.mvgam(x, type = "residuals")` calls it. The grid shows
#' residuals against fitted values, the Q-Q plot, the ACF and the pACF,
#' all from one set of draws. A closure-unit family has one residual per
#' unit and shows the Q-Q panel alone.
#'
#' @param object Fitted `mvgam` object
#' @param series Series name or index, or `NULL`
#' @param ndraws Integer number of draws
#' @param resp Character response name, or `NULL`
#' @param ... Refused if supplied
#' @return A `patchwork` object
#' @noRd
mvgam_resid_panel <- function(
  object, series = NULL, ndraws = 100L, resp = NULL, ...
) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1L)
  checkmate::assert_string(resp, null.ok = TRUE)
  # `plot.mvgam(type = "residuals")` calls this function. The panels
  # take its named arguments alone.
  rlang::check_dots_empty()
  # One grid per response, stacked into one figure.
  fan <- mv_resp_fan_out(object, resp, combine = stack_response_plots)
  if (!is.null(fan)) return(fan)
  # Every panel takes the same posterior draws.
  draw_ids <- resolve_draw_ids(object, ndraws, NULL)
  closure_unit <- is_closure_unit_family(object$family)
  types <- if (closure_unit) {
    "resid_qq"
  } else {
    c("resid_vs_fitted", "resid_qq", "resid_acf", "resid_pacf")
  }
  panels <- lapply(
    resid_diagnostic_panels(object, types, draw_ids = draw_ids,
                            resp = resp, series = series),
    function(p) p + ggplot2::labs(subtitle = NULL)
  )
  out <- patchwork::wrap_plots(panels, ncol = min(2L, length(panels)))
  label <- if (closure_unit) {
    NULL
  } else {
    resid_series_rows(object, object$data, series)$label
  }
  if (is.null(label)) {
    return(out)
  }
  out + patchwork::plot_annotation(title = paste0("Residuals for ", label))
}
