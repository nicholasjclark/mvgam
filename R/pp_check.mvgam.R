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
#'   `"resid_vs_fitted"`, `"resid_acf"`, `"resid_pacf"` and
#'   `"resid_variogram"`. Type
#'   `"fit_stat"` serves `nmix()` and `occ()` fits. A bernoulli or
#'   ordinal response defaults to `"bars"`. Another discrete response,
#'   such as a count, defaults to `"rootogram"`. A continuous response
#'   defaults to `"dens_overlay"`. Multi-response families take the
#'   defaults under "Multi-response families".
#'
#' @param ndraws Number of posterior draws to plot. `NULL` (the default)
#'   takes the default of the type, listed under Details.
#'
#' @param draw_ids Optional integer vector naming the posterior draws to
#'   plot. `NULL` (the default) lets `ndraws` choose them.
#'
#' @param prefix `"ppc"` (the default) plots the observed data with the
#'   replicates. `"ppd"` plots the replicates alone.
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
#'   `resid_qq`, `resid_vs_fitted`, `resid_acf`, `resid_pacf` and
#'   `resid_variogram`, the one series whose residuals are drawn, as a
#'   name or an index. The default is the first series. Its residuals
#'   are taken in time order. `resid_acf` keeps missing responses in
#'   place. Its lags count time steps. `resid_pacf` drops them. Its lags
#'   count observations. A fit with several series names its series in
#'   the title.
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
#' use the covariate part of the model, as [posterior_predict.mvgam()]
#' does. Passing `incl_autocor` chooses between the two. The `loo_*`
#' types use the fitted state. Their importance weights come from it.
#'
#' Overlay types default to 10 draws. Interval, ribbon and statistic
#' types use every draw. The residual histograms and ribbons default to
#' 500 draws, the other residual types to 100 and `fit_stat` to 500. A
#' posterior with fewer draws gives all of them.
#' `resid_hist` draws one histogram per draw and shows the first eight.
#'
#' `resid_acf` and `resid_pacf` count lags in rows. They refuse a series
#' observed at unevenly spaced times, such as one a [CAR()] trend
#' models. `resid_variogram` measures lags in time units. It plots half
#' the mean squared difference of two residuals against the time
#' between them. Residuals without serial dependence give a flat
#' semivariogram at their variance, the dashed line (Diggle et al.
#' 2002). `plot(type = "residuals")` draws it in place of the ACF and pACF for
#' such a series.
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
#'   plot. [CAR()] models series observed at unevenly spaced times.
#'   [loo.mvgam()] scores the fit. The
#'   `r mvgam_article("mvgam_overview", "overview article")` explains a
#'   rootogram. The `r mvgam_article("nmix", "N-mixture article")` runs
#'   the discrepancy check.
#'
#' @family model checking
#' @export pp_check
#'
#' @references
#' Diggle, P. J., Heagerty, P., Liang, K.-Y. and Zeger, S. L. (2002).
#' \emph{Analysis of Longitudinal Data}, 2nd edition. Oxford
#' University Press.
#'
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
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # A hanging rootogram, on the square-root scale. Each bar shows how
#' # often a count was observed. It hangs from the curve of its expected
#' # frequency under the model. A bar ending below zero marks a count the
#' # model under-predicts
#' pp_check(mod, type = "rootogram", style = "hanging")
#'
#' # Randomised quantile residuals over time. A trend the model missed
#' # shows as serial structure
#' pp_check(mod, type = "resid_ribbon")
#'
#' # Half the mean squared difference of residuals by the time between
#' # them. A flat band at the dashed variance shows no dependence left
#' pp_check(mod, type = "resid_variogram")
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

  resid_pooled <- c(
    "resid_hist", "resid_hist_grouped", "resid_ribbon",
    "resid_ribbon_grouped"
  )
  resid_diagnostic <- c(
    "resid_acf", "resid_pacf", "resid_variogram", "resid_qq",
    "resid_vs_fitted"
  )
  resid_types <- c(resid_pooled, resid_diagnostic)
  resid_by_series <- c("resid_hist", "resid_ribbon", resid_diagnostic)
  closure_unit <- is_closure_unit_family(object$family)

  # Residuals are computed on the rows the model was fitted to.
  if (type %in% resid_types && !is.null(newdata)) {
    stop(insight::format_error(c(
      "The residual types describe the training data.",
      i = "Drop 'newdata' from this call."
    )), call. = FALSE)
  }
  # A `loo_*` check weights its replicates by importance weights from
  # the fitted latent state. Replicates of the covariate part would not
  # match those weights.
  if (startsWith(type, "loo_") && isFALSE(dots$incl_autocor)) {
    stop(insight::format_error(c(
      "'incl_autocor = FALSE' is not supported for the 'loo_*' types.",
      i = "The 'loo_*' importance weights come from the fitted latent state."
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

  # A density overlay of a discrete response draws smooth curves that
  # ripple between the integers. Bars show the share of each value of a
  # binary or ordinal response. A rootogram compares the frequency of
  # each count.
  if (type_missing && is.null(mv_cat)) {
    fam <- model_families(object, resp)
    if (identical(resolve_family_name(fam), "bernoulli") ||
          is_ordinal_family(fam)) {
      type <- "bars"
    } else if (identical(fam$type, "int")) {
      type <- "rootogram"
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

  # The bayesplot kernel that draws this type. `fit_stat` and the
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
      "resid_acf", "resid_pacf", "resid_variogram", "resid_vs_fitted",
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

  # bayesplot resolves a `stat` given by name from the frame that calls
  # its kernel. That frame encloses the caller's environment, where the
  # user defined the statistic.
  out_plot <- do_call(ppc_fun, c(ppc_args, dots[!for_pred]),
                      envir = parent.frame())
  if ("x" %in% ppc_formals && !is.null(x)) {
    out_plot <- out_plot + ggplot2::labs(x = x)
  }
  # Each bar marks an integer value. Pretty breaks between them, such
  # as -0.5 and 0.5 for a binary response, label no value.
  if (type %in% c("bars", "bars_grouped")) {
    x_scale <- out_plot$scales$get_scales("x")
    x_scale$breaks <- function(limits) {
      breaks <- pretty(limits)
      breaks[breaks == round(breaks)]
    }
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
#' draws the grid. The randomised quantile residuals are drawn once.
#' Every panel of the grid then shows the same residual draws. A
#' series observed at unevenly spaced times refuses the ACF and pACF.
#' Their lags count rows.
#'
#' @param object Fitted `mvgam` object
#' @param types Character vector of panel types: `"resid_vs_fitted"`,
#'   `"resid_qq"`, `"resid_acf"`, `"resid_pacf"` or
#'   `"resid_variogram"`
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
    types, c("resid_vs_fitted", "resid_qq", "resid_acf", "resid_pacf",
             "resid_variogram"),
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
    lag_types <- intersect(types, c("resid_acf", "resid_pacf"))
    if (series_sel$irregular && length(lag_types)) {
      stop(insight::format_error(c(
        paste0("'", lag_types[1L], "' needs a series observed at evenly ",
               "spaced times."),
        x = "The lags of an ACF count rows. This series has unequal gaps.",
        i = "Use type = 'resid_variogram'. Its lags are in time units."
      )), call. = FALSE)
    }
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
      resid_variogram = build_resid_variogram_panel(
        resid, series_sel$times
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
#' @return List of the row indices in `rows`, their times in `times`,
#'   the series name in `label` and the number of series in
#'   `n_series`. `irregular` is `TRUE` when the gaps between
#'   consecutive rows differ. `label` is `NULL` for a fit without a
#'   series axis. A frame without the time column takes row positions
#'   as times.
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
  times <- seq_along(rows)
  if (time_var %in% names(data)) {
    rows <- rows[order(data[[time_var]][rows])]
    times <- as.numeric(data[[time_var]][rows])
  }
  list(rows = rows, times = times,
       irregular = length(unique(signif(diff(times), 10L))) > 1L,
       label = label, n_series = length(levs))
}


#' A residual plot titled with the series it shows
#'
#' Only fits with more than one series get the title.
#'
#' @param p A `ggplot` object
#' @param series_sel List from `resid_series_rows()`, or `NULL`
#' @return `p`, with a title when the fit has several series
#' @noRd
label_resid_series <- function(p, series_sel) {
  checkmate::assert_class(p, "ggplot")
  checkmate::assert_list(series_sel, null.ok = TRUE)
  if (is.null(series_sel$label) || series_sel$n_series < 2L) {
    return(p)
  }
  p + ggplot2::labs(title = resid_series_title(series_sel$label))
}

#' The title naming the series a residual plot shows
#'
#' @param label Character series label
#' @return Character scalar
#' @noRd
resid_series_title <- function(label) {
  checkmate::assert_string(label)
  paste0("Residuals for ", label)
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


#' Nested quantile bands of a residual statistic at each lag
#'
#' @param values Numeric matrix `[ndraws, nlags]`, the statistic of
#'   each draw at each lag
#' @param lag Numeric lags, one per column of `values`
#' @return Data frame of `lag` and the 2.5, 10, 25, 75, 90 and 97.5%
#'   quantiles across draws
#' @noRd
resid_lag_bands <- function(values, lag) {
  checkmate::assert_matrix(values, mode = "numeric")
  checkmate::assert_numeric(lag, len = ncol(values))
  q <- function(p) {
    apply(values, 2L, stats::quantile, probs = p, na.rm = TRUE,
          names = FALSE)
  }
  data.frame(
    lag = lag,
    q025 = q(0.025), q975 = q(0.975),
    q100 = q(0.10),  q900 = q(0.90),
    q250 = q(0.25),  q750 = q(0.75)
  )
}


#' A panel of nested quantile segments at each lag
#'
#' The ACF, pACF and semivariogram panels share it. A segment marks
#' each lag. The widest spans 95% of the draws, the narrowest 50%.
#'
#' @param bands Data frame from `resid_lag_bands()`
#' @param reference List of `ggplot2` layers drawn beneath the bands
#' @param title Character panel title
#' @param x_label Character x-axis label
#' @return A `ggplot` object
#' @noRd
resid_lag_plot <- function(bands, reference, title, x_label) {
  checkmate::assert_data_frame(bands)
  checkmate::assert_list(reference)
  checkmate::assert_string(title)
  checkmate::assert_string(x_label)
  palette <- mvgam_palette()
  segment <- function(colour, lo, hi) {
    ggplot2::geom_segment(
      ggplot2::aes(y = .data[[lo]], yend = .data[[hi]]),
      colour = colour, linewidth = 1.5
    )
  }
  # The title names the statistic. The residual figure stacks this
  # panel under the Q-Q panel, where a y title would join the Q-Q one.
  ggplot2::ggplot(bands, ggplot2::aes(x = .data$lag, xend = .data$lag)) +
    reference +
    segment(palette[1L], "q025", "q975") +
    segment(palette[3L], "q100", "q900") +
    segment(palette[6L], "q250", "q750") +
    ggplot2::labs(title = title, x = x_label, y = NULL) +
    mvgam_theme()
}


#' An ACF or pACF panel of residual draws
#'
#' Computes the ACF or pACF of each draw, then summarises across draws
#' with nested quantile segments at each integer lag. The dashed band
#' is the asymptotic 95% white-noise interval `+/- 1.96 / sqrt(n)`.
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
  # The ACF estimates each lag from the pairs both observed. A missing
  # value stays in place. Its lags count time steps. The pACF recursion
  # needs an ACF from a complete series. A series with gaps gives
  # partial correlations outside [-1, 1]. The pACF drops the missing
  # values. Its lags count observations. Every draw misses the same
  # responses. Each draw then gives the same lags.
  keep <- if (lag_type == "pacf") {
    !is.na(resid_draws[1L, ])
  } else {
    rep(TRUE, ncol(resid_draws))
  }
  per_draw <- function(d) {
    acf_fn(resid_draws[d, keep], plot = FALSE, na.action = stats::na.pass)
  }
  lag <- per_draw(1L)$lag[, 1L, 1L]
  values <- matrix(vapply(seq_len(nrow(resid_draws)), function(d) {
    per_draw(d)$acf[, 1L, 1L]
  }, numeric(length(lag))), ncol = length(lag), byrow = TRUE)
  values <- values[, lag > 0, drop = FALSE]
  lag <- lag[lag > 0]

  palette <- mvgam_palette()
  ci_bound <- stats::qnorm(0.975) / sqrt(sum(!is.na(resid_draws[1L, ])))
  reference <- list(
    ggplot2::geom_hline(
      yintercept = c(-1, 1) * ci_bound,
      linetype = "dashed", colour = "black"
    ),
    ggplot2::geom_hline(
      yintercept = 0, colour = palette[6L], linewidth = 0.25
    )
  )
  resid_lag_plot(
    resid_lag_bands(values, lag), reference,
    title = if (lag_type == "acf") "ACF" else "pACF",
    x_label = "Lag"
  )
}


#' A semivariogram panel of residual draws
#'
#' The semivariance at lag `h` is half the mean squared difference of
#' the residuals observed `h` time units apart. It measures dependence
#' by the time between observations. An ACF counts rows. On unevenly
#' spaced times one row lag mixes short and long gaps. Pairs up to
#' half the observed span enter. Each distinct lag is its own bin when
#' there are at most `max_bins` of them. Otherwise the pairs fall into
#' `max_bins` bins of near-equal count, each placed at its mean lag.
#' Residuals without serial dependence give a flat semivariogram at
#' their variance, the dashed line. Dependence the model missed shows
#' as semivariance below it at short lags.
#'
#' @param resid_draws Numeric residual matrix with dimensions
#'   `[ndraws, nobs]`
#' @param times Numeric observation times, one per column of
#'   `resid_draws`
#' @param max_bins Integer maximum number of lag bins
#' @return A `ggplot` object
#' @noRd
build_resid_variogram_panel <- function(resid_draws, times,
                                        max_bins = 20L) {
  checkmate::assert_matrix(resid_draws, mode = "numeric")
  checkmate::assert_numeric(times, len = ncol(resid_draws),
                            any.missing = FALSE)
  checkmate::assert_int(max_bins, lower = 2L)
  # Every draw misses the same responses
  seen <- !is.na(resid_draws[1L, ])
  ord <- order(times[seen])
  resid_draws <- resid_draws[, seen, drop = FALSE][, ord, drop = FALSE]
  times <- times[seen][ord]
  n <- length(times)
  max_lag <- (times[n] - times[1L]) / 2
  # Each observation pairs with the later ones within `max_lag`
  n_later <- findInterval(times + max_lag, times) - seq_len(n)
  if (n < 3L || sum(n_later) == 0L) {
    stop(insight::format_error(c(
      "A semivariogram needs observations at three or more times.",
      x = paste0("The series has ", n, " observed residuals.")
    )), call. = FALSE)
  }
  i <- rep(seq_len(n), n_later)
  j <- i + sequence(n_later)
  h <- times[j] - times[i]

  lags <- sort(unique(signif(h, 10L)))
  if (length(lags) <= max_bins) {
    bin <- match(signif(h, 10L), lags)
  } else {
    breaks <- unique(stats::quantile(
      h, seq(0, 1, length.out = max_bins + 1L), names = FALSE
    ))
    bin <- findInterval(h, breaks, rightmost.closed = TRUE,
                        all.inside = TRUE)
    bin <- match(bin, sort(unique(bin)))
    lags <- as.numeric(rowsum(h, bin)) / tabulate(bin)
  }
  n_pairs <- tabulate(bin, nbins = length(lags))
  values <- matrix(vapply(seq_len(nrow(resid_draws)), function(d) {
    r <- resid_draws[d, ]
    as.numeric(rowsum(0.5 * (r[j] - r[i])^2, bin)) / n_pairs
  }, numeric(length(lags))), ncol = length(lags), byrow = TRUE)

  sill <- stats::median(apply(resid_draws, 1L, stats::var))
  reference <- list(ggplot2::geom_hline(
    yintercept = sill, linetype = "dashed", colour = "black"
  ))
  resid_lag_plot(resid_lag_bands(values, lags), reference,
                 title = "Semivariogram", x_label = "Time lag")
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
#' p-value printed in the top-left corner.
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
    ggplot2::annotate(
      "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
      label = paste0("Bayesian p-value = ",
                     format(round(x$bayes_p, 3), nsmall = 3))
    ) +
    ggplot2::labs(
      title = paste0(
        "PPC: ", stat_lab,
        " discrepancy (", x$grain, " grain)"
      ),
      x = "T(y_obs, theta)", y = "T(y_rep, theta)"
    ) +
    mvgam_theme()
}


#' The residual diagnostic grid of one series
#'
#' `plot.mvgam(x, type = "residuals")` calls it. The grid shows
#' residuals against fitted values, the Q-Q plot, the ACF and the pACF,
#' all from one set of draws. A series observed at unevenly spaced
#' times takes the semivariogram in place of the ACF and pACF. A
#' closure-unit family has one residual per unit and shows the Q-Q
#' panel alone.
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
  set_color_scheme_local()
  # One grid per response, stacked into one figure.
  fan <- mv_resp_fan_out(object, resp, combine = stack_response_plots)
  if (!is.null(fan)) return(fan)
  # Every panel takes the same posterior draws.
  draw_ids <- resolve_draw_ids(object, ndraws, NULL)
  series_sel <- NULL
  types <- "resid_qq"
  if (!is_closure_unit_family(object$family)) {
    series_sel <- resid_series_rows(object, object$data, series)
    types <- c("resid_vs_fitted", "resid_qq", if (series_sel$irregular) {
      "resid_variogram"
    } else {
      c("resid_acf", "resid_pacf")
    })
  }
  panels <- lapply(
    resid_diagnostic_panels(object, types, draw_ids = draw_ids,
                            resp = resp, series = series),
    function(p) p + ggplot2::labs(title = NULL)
  )
  out <- patchwork::wrap_plots(panels, ncol = min(2L, length(panels)))
  label <- series_sel$label
  if (is.null(label)) {
    return(out)
  }
  out + patchwork::plot_annotation(title = resid_series_title(label),
                                   theme = mvgam_theme())
}
