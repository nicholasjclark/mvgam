#' Posterior Predictive Checks for \code{mvgam} models
#'
#' Perform unconditional posterior predictive checks with the help
#' of the \pkg{bayesplot} package. Returns a \pkg{ggplot2} object that
#' can be further customised.
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
#' @param type Type of the check, the name of a \pkg{bayesplot} `ppc_*`
#'   function without its prefix or one of the residual types below.
#'   The default is `"bars"` for a bernoulli or ordinal response and
#'   `"dens_overlay"` otherwise. Multi-response families take the
#'   defaults described under "Multi-response families".
#'
#' @param newdata Optional \code{dataframe} or \code{list} of test data
#'   containing the variables included in the linear predictor of
#'   \code{formula}. If not supplied, predictions are generated for the
#'   original observations used for the model fit. The residual types
#'   describe the training data and refuse it.
#'
#' @param per_obs Logical. For `type = 'resid_vs_fitted'`, whether each
#'   observation collapses to its posterior median on both axes, giving
#'   one point per observation. `FALSE` keeps the scatter of every draw
#'   for every observation, which shows the posterior spread. Other
#'   types ignore it.
#'
#' @param series For the residual types `resid_hist`, `resid_ribbon`,
#'   `resid_qq`, `resid_vs_fitted`, `resid_acf` and `resid_pacf`, the
#'   one series whose residuals are drawn, as a name or an index. The
#'   default is the first series. Its residuals are taken in time order.
#'   `resid_acf` keeps missing responses in place. Its lags count time
#'   steps. `resid_pacf` drops them. Its lags count observations. A
#'   plot from a fit with several series names its series in the
#'   subtitle.
#'
#' @param ... Further arguments passed to \code{\link{predict.mvgam}}
#'   as well as to the PPC function specified in \code{type}. A name
#'   belonging to neither is refused, naming the argument. Ten
#'   bayesplot types take a `stat` of their own. `type = 'fit_stat'`
#'   takes one here too, either `'chi_squared'` (default) or
#'   `'freeman_tukey'`, naming the discrepancy it computes.
#'
#' @return A ggplot object that can be further
#'   customized using the \pkg{ggplot2} package. A multivariate model
#'   given no `resp` returns one figure with a panel per response, each
#'   titled by its response.
#'
#' @details
#' Replicates are drawn against the latent trend state the model
#' inferred at each time. The check then compares the model's fitted
#' trajectory with the series that produced it, on the same basis as
#' [residuals.mvgam()] and [hindcast.mvgam()]. Supplying `newdata` the
#' fit never saw integrates over the trend dynamics instead. The
#' `loo_*` types always use the conditional state, whatever data they
#' cover. Their importance weights come from the likelihood, which
#' uses the conditional state too.
#'
#' The residual histograms and ribbons take their residuals from 500
#' draws by default. `resid_hist` and `resid_hist_grouped` draw one
#' histogram per draw and show the first eight.
#'
#' For a detailed explanation of each of the ppc functions,
#'   see the \code{\link[bayesplot:PPC-overview]{PPC}} documentation of the
#'   \pkg{\link[bayesplot:bayesplot-package]{bayesplot}} package. LOO-PIT
#'   bayesplot variants (\code{loo_pit}, \code{loo_pit_overlay},
#'   \code{loo_pit_qq}, \code{loo_intervals}, \code{loo_ribbon}) compute
#'   PSIS weights internally via [log_lik.mvgam()].
#'
#'   To produce conditional posterior predictive checks restricted to a
#'   particular series or covariate slice, filter `newdata` to the rows
#'   of interest and pass it to `pp_check()`. For series-grouped variants
#'   use the `group = "series"` argument of the underlying bayesplot
#'   functions (e.g. `type = "ecdf_overlay_grouped"`, `type =
#'   "bars_grouped"`). For per-observation summaries against a covariate,
#'   `marginaleffects::plot_predictions()` provides a complementary
#'   conditional surface.
#'
#' @section Multi-response families:
#'   `diri()` / `multi()` / `categ()` / `mvn()` / `mvt()` place
#'   K outcomes on each closure unit
#'   (simplex families) or each (site, species) row (mv-normal /
#'   mv-T). A pooled `dens_overlay` would collapse every
#'   category / species into one curve and hide per-category
#'   fit. When the user does not pass `group`, `pp_check()`
#'   auto-groups by the per-family category axis (within-unit
#'   position for the simplex families, the `series` column for
#'   `mvn()` / `mvt()`) and switches `type` to its `_grouped`
#'   variant so bayesplot facets per category. Default `type`
#'   becomes `"ribbon"` for simplex families and `"dens_overlay"`
#'   for `mvn()` / `mvt()`. To opt out and recover the pooled
#'   plot, pass `group = "<existing-column>"` explicitly
#'   (e.g. `group = "series"`).
#'
#' @section Closure-unit families (`nmix()`, `occ()`):
#'   Closure-unit observation families compute predictions at
#'   the closure-unit grain (one per site x season) because
#'   visits within a unit share the latent state. `pp_check()`
#'   aggregates the per-visit response and per-visit posterior
#'   predictive draws to the unit grain (sum across visits) via
#'   the shared `aggregate_closure_unit_visits()` helper, then
#'   hands the resulting `(y_unit, yrep_unit)` to the relevant
#'   `bayesplot::ppc_*` function. Discrete-friendly types are
#'   the natural fit (`bars`, `bars_grouped`, `rootogram`,
#'   `hist`, `freqpoly`, `freqpoly_grouped`); density / ECDF
#'   variants (`dens_overlay`, `ecdf_overlay`,
#'   `ecdf_overlay_grouped`), per-unit stats (`stat`, `stat_2d`,
#'   `stat_grouped`), intervals / ribbons over a unit-constant
#'   covariate (`intervals`, `ribbon`) and the residual
#'   histograms (`resid_hist`, `resid_qq`) all work as
#'   expected. Per-row scatter / fitted-vs-residual / per-row
#'   time-axis types (`scatter_avg`, `error_binned`,
#'   `resid_acf`, `resid_pacf`, `resid_vs_fitted`,
#'   `resid_ribbon`) are blocked because they do not match the
#'   closure-unit grain. `group =` and `x =` covariates must be
#'   constant within every closure unit; per-visit covariates
#'   (`tod`, observer, weather) are rejected with a clear
#'   error.
#'
#' @seealso \code{\link{predict.mvgam}}, [log_lik.mvgam()]
#'
#' @export pp_check
#'
#' @references
#' Gabry, J., Simpson, D., Vehtari, A., Betancourt, M. and
#' Gelman, A. (2019). Visualization in Bayesian workflow.
#' \emph{Journal of the Royal Statistical Society A}, 182:389-402.
#' \doi{10.1111/rssa.12378}
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # Density overlay: posterior predictive replicates on the
#' # observed response density. Mismatch between the dark line
#' # (observed) and the pale ribbon (predictive) signals
#' # family or trend mis-specification.
#' pp_check(mod, type = "dens_overlay", ndraws = 50L)
#'
#' # Rootogram: square-root of count vs predicted, with bars
#' # hanging from the predicted height to the axis. Bars dropping
#' # below zero are observed counts under-predicted by the model;
#' # bars rising above are over-predicted. Designed for count
#' # families.
#' pp_check(mod, type = "rootogram", ndraws = 50L)
#'
#' # PIT-ECDF: probability-integral transform values pooled across
#' # observations. A well-calibrated fit traces the diagonal;
#' # mass bulging above or below the band indicates under- or
#' # over-dispersion. Bump `ndraws` for a stable PIT support.
#' pp_check(mod, type = "pit_ecdf", ndraws = 500L)
#'
#' # Residual ribbon: per-observation randomised quantile residuals
#' # over the observation index, with 50% / 90% predictive ribbons.
#' # Trend mis-specification shows up here as serial structure in
#' # the residuals (auto-correlation, drift) that the marginal
#' # density check would miss.
#' pp_check(mod, type = "resid_ribbon", ndraws = 500L)
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
  set_color_scheme_local("red")

  dots <- list(...)
  type_missing <- missing(type)
  if (type_missing) {
    type <- "dens_overlay"
  }

  prefix <- match.arg(prefix)
  validate_draw_selectors(ndraws, draw_ids)
  ndraws_given <- "ndraws" %in% names(match.call())

  # Residuals are computed on the rows the model was fitted to. The
  # observed values of a frame the caller supplies belong to other
  # rows.
  if (grepl("resid", type) && !is.null(newdata)) {
    stop(insight::format_error(c(
      "The residual types describe the training data.",
      i = "Drop 'newdata' from this call."
    )))
  }
  resid_by_series <- c(
    "resid_hist", "resid_ribbon", "resid_acf", "resid_pacf",
    "resid_qq", "resid_vs_fitted"
  )
  if (!is.null(series)) {
    if (!type %in% resid_by_series) {
      stop(insight::format_error(c(
        "'series' selects the series of a residual plot.",
        x = paste0("Got type = '", type, "'."),
        i = paste0(
          "Use it with ", paste0("'", resid_by_series, "'", collapse = ", "),
          ", or subset 'newdata' for the other checks."
        )
      )))
    }
    if (is_closure_unit_family(object$family)) {
      stop(insight::format_error(c(
        "'series' is not supported for closure-unit residuals.",
        x = "A closure-unit family has one residual per closure unit."
      )))
    }
  }
  # Whether the fit saw these rows decides which surface every panel
  # uses. The test runs before the training frame takes the place of
  # `newdata` below. Run afterwards, it treated the training frame as
  # new data. The residual axis was then conditional and the fitted
  # axis marginal.
  in_sample <- is.null(newdata)
  if (in_sample) {
    newdata <- object$data
  }

  # Multi-response custom families (diri / multi / categ / mvn /
  # mvt): a default `dens_overlay` would pool every category or
  # species into a single curve and hide the per-category fit.
  # Without a `group` from the caller, the function picks a default
  # type for the family. It builds a category vector at the grain of
  # the long-form predictions and attaches it to `newdata` under a
  # reserved name. `type` becomes its `_grouped` variant. Passing
  # `group = "<existing-column>"` keeps the pooled plot.
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
      available <- as.character(bayesplot::available_ppc(""))
      if (paste0("ppc_", grouped_type) %in% available) {
        type <- grouped_type
        newdata[["_mvgam_pp_category"]] <- mv_cat
        group <- "_mvgam_pp_category"
      } else {
        mv_cat <- NULL
      }
    }
  }

  # Multivariate fits (mvbind / mvbrmsformula). When `resp` is
  # NULL, fan out per response via the shared helper and return one
  # figure with a panel per response. When the caller supplied
  # `resp`, validate it and fall through to the univariate path
  # scoped to that response.
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

  if (prefix == "ppc") {
    # No type checking for prefix 'ppd' yet
    valid_types <- sort(
      c(
        as.character(bayesplot::available_ppc("")),
        "ppc_resid_hist",
        "ppc_resid_hist_grouped",
        "ppc_resid_ribbon",
        "ppc_resid_ribbon_grouped",
        "ppc_resid_acf",
        "ppc_resid_pacf",
        "ppc_resid_qq",
        "ppc_resid_vs_fitted",
        # Chi-squared or Freeman-Tukey discrepancy for closure-unit
        # families (Gelman et al. 1996). It returns a
        # `mvgam_ppc_fit_stat` object with print and plot methods. The
        # bayesplot dispatch does not apply to it.
        "ppc_fit_stat"
      )
    )
    valid_types <- sub("^ppc_", "", valid_types)
    if (!type %in% valid_types) {
      stop(insight::format_error(c(
        "Argument 'type' must be a valid ppc type.",
        x = paste0("Given: '", type, "'."),
        i = paste0(
          "Valid types are: ",
          paste0("'", valid_types, "'", collapse = ", "), "."
        )
      )))
    }
  }

  # The bayesplot kernel that draws this type. Its formals are the
  # arguments it accepts. `fit_stat` and the four diagnostic residual
  # types draw themselves. They name no kernel and skip the lookup. The
  # split of `...` below needs both argument sets.
  bptype <- type
  if (bptype %in% c("resid_hist", "resid_hist_grouped")) {
    bptype <- sub("resid", "error", bptype)
  }
  if (bptype %in% c("resid_ribbon", "resid_ribbon_grouped")) {
    bptype <- sub("resid_", "", bptype)
  }
  ppc_fun <- if (type %in% c(
    "fit_stat", "resid_acf", "resid_pacf", "resid_qq", "resid_vs_fitted"
  )) {
    NULL
  } else {
    get(paste0(prefix, "_", bptype), asNamespace("bayesplot"))
  }
  ppc_formals <- if (is.null(ppc_fun)) {
    character(0L)
  } else {
    names(formals(ppc_fun))
  }

  # `...` holds two argument sets: the prediction method's for this
  # type and those of the kernel that draws it. A name in neither set
  # reached bayesplot. bayesplot named it in a warning and drew the
  # plot on the default the caller meant to override.
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
  # `stat` stays in `...`. Ten bayesplot kernels declare one of their
  # own. A formal `stat` here bound it to mvgam. Then
  # `pp_check(type = 'stat', stat = 'median')` drew the default.
  # `type = 'fit_stat'` has no kernel to declare it. It is allowed here
  # for that type alone.
  allowed <- c(pred_formals, ppc_formals)
  if (identical(type, "fit_stat")) {
    allowed <- c(allowed, "stat")
  }
  refuse_unread_dots(dots, allowed, "pp_check")

  # The closure-unit fit statistic computes a per-draw discrepancy
  # from epred and yrep. It returns its own `mvgam_ppc_fit_stat`
  # object outside the bayesplot dispatch.
  if (identical(type, "fit_stat")) {
    # The discrepancy is computed on a unit's aggregated visits. It
    # applies to the families that aggregate, the ones the refusal's
    # hint names. A test on the wire format admitted the composition
    # families. Their units sum to one whatever the fit. Every model
    # then returned a Bayesian p-value near 0.5.
    if (!is_closure_unit_family(object$family)) {
      stop(insight::format_error(c(
        paste0(
          "pp_check(type = 'fit_stat') is only available for the ",
          "detection families."
        ),
        x = paste0(
          "Family '", resolve_family_name(object$family),
          "' does not aggregate repeat visits within a unit."
        ),
        i = "Use family = occ() or family = nmix() for this check."
      )))
    }
    # The default below tests `is.null(draw_ids)`, as the residual
    # default does. Without it a caller who named draws also received
    # a count. The prediction refused the pair.
    if (!ndraws_given && is.null(draw_ids)) {
      ndraws <- 500L
    }
    stat <- dots$stat %||% "chi_squared"
    return(closure_unit_fit_stat_ppc(
      object   = object,
      newdata  = newdata,
      stat     = stat,
      group    = group,
      ndraws   = ndraws,
      draw_ids = draw_ids
    ))
  }

  # The detection families plot at the closure-unit grain. The
  # aggregation below collapses a unit's visits (see
  # `closure_unit_pp_check_setup()`). Per-row types that assume
  # exchangeable observations or a per-row time axis are blocked.
  #
  # The gate tests the aggregation predicate. The aggregation creates
  # the mismatch, whatever the wire format. `mvn()`, `mvt()`, `diri()`,
  # `multi()` and `categ()` share the closure-unit pipeline without
  # being aggregated here. Their plots stay per row, where these ten
  # types apply. A test on the wire format refused all ten for them.
  if (is_closure_unit_family(object$family)) {
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
          "pp_check(type = '", type, "') is not available for ",
          "closure-unit family '",
          resolve_family_name(object$family), "'."
        ),
        x = paste0(
          "Row scatter plots, residuals against fitted values and ",
          "residuals over time do not describe data aggregated to ",
          "closure units."
        ),
        i = paste0(
          "Use type = 'bars', 'rootogram', 'dens_overlay', ",
          "'ecdf_overlay', 'intervals', 'stat', 'resid_hist' ",
          "or 'resid_qq' for closure-unit fits."
        )
      )))
    }
  }
  # `group` and `x` are checked against the column names of newdata.
  # The diagnostic residual types skip the check. They have no
  # bayesplot formals to query.
  valid_vars <- names(newdata)
  # A `loo_*` check reweights its replicates by importance weights from
  # the likelihood. The likelihood scores each observation under the
  # latent state the model inferred at that time. The replicates are
  # drawn under that same state. Otherwise the weights and the draws
  # describe different series.
  psis_weighted <- any(c("psis_object", "lw") %in% ppc_formals)
  if ("group" %in% ppc_formals) {
    if (is.null(group)) {
      stop(insight::format_error(
        paste0("Argument 'group' is required for ppc type '", type, "'.")
      ))
    }
    if (!group %in% valid_vars) {
      stop(insight::format_error(
        paste0("Variable '", group, "' is absent from the data.")
      ))
    }
  }
  if ("x" %in% ppc_formals) {
    if (!is.null(x) && !x %in% valid_vars) {
      stop(insight::format_error(
        paste0("Variable '", x, "' is absent from the data.")
      ))
    }
  }
  for_pred <- names(dots) %in% pred_formals
  # Draw counts for the residual types. A family without a
  # distribution spec takes its residuals from the empirical PIT,
  # which needs about 50 draws or more. With fewer, observations in
  # the predictive tail underflow the PIT support and clamp near
  # +/-8, which looks like misfit. The ribbons and histograms default
  # to 500 draws. A histogram draws one panel per draw and shows the
  # first eight.
  resid_hist_panels <- 8L
  resid_pooled <- c(
    "resid_hist", "resid_hist_grouped",
    "resid_ribbon", "resid_ribbon_grouped"
  )
  resid_diagnostic <- c(
    "resid_acf", "resid_pacf", "resid_qq", "resid_vs_fitted"
  )
  if (!ndraws_given && is.null(draw_ids)) {
    aps_types <- c(
      "error_scatter_avg",
      "error_scatter_avg_vs_x",
      "intervals",
      "intervals_grouped",
      "loo_intervals",
      "loo_pit",
      "loo_pit_overlay",
      "loo_pit_qq",
      "loo_ribbon",
      "pit_ecdf",
      "pit_ecdf_grouped",
      "ribbon",
      "ribbon_grouped",
      "rootogram",
      "scatter_avg",
      "scatter_avg_grouped",
      "stat",
      "stat_2d",
      "stat_freqpoly_grouped",
      "stat_grouped",
      "violin_grouped"
    )
    # Each check type takes a default draw count without a message, as
    # in `bayesplot` and `brms::pp_check()`. The returned object
    # records the count.
    if (type %in% aps_types) {
      ndraws <- NULL
    } else if (type %in% resid_pooled) {
      ndraws <- 500L
    } else if (type %in% resid_diagnostic) {
      ndraws <- 100L
    } else {
      ndraws <- 10
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

  y <- NULL
  if (prefix == "ppc") {
    # y is ignored in prefix 'ppd' plots.
    y <- newdata[[response_column(object, resp)]]
    # Ordinal responses arrive as ordered factors. bayesplot's ppc_*
    # functions assert a numeric y. Factors become their integer codes
    # (1..nlevels).
    if (is.factor(y)) {
      y <- as.integer(y)
    }
  }

  # A subsample of draws is pinned once here. yrep and the PSIS
  # log-weights below then use the same posterior draws.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  series_sel <- NULL
  if (grepl("resid", type)) {
    # A Dunn-Smyth residual plot takes y as zero and yrep as minus the
    # residual. bayesplot's error convention then points each residual
    # in its own direction.
    y[!is.na(y)] <- 0
    # residuals(summary = FALSE) returns [ndraws x nobs], as
    # posterior_predict does below. `resp` holds one response here,
    # from the fan-out or from the caller. residuals() then returns one
    # matrix where a multivariate fit would return a list.
    yrep <- -1 * residuals(
      object, summary = FALSE,
      ndraws = if (!is.null(draw_ids)) NULL else ndraws,
      draw_ids = draw_ids, resp = resp
    )
    # Residual columns follow the rows of the training frame. Its
    # series may be interleaved or shuffled. An autocorrelation or a
    # residual ribbon describes one series in time order.
    if (type %in% resid_by_series &&
          !is_closure_unit_family(object$family)) {
      series_sel <- resid_series_rows(object, newdata, series)
      y <- y[series_sel$rows]
      yrep <- yrep[, series_sel$rows, drop = FALSE]
    }
    if (type %in% c("resid_hist", "resid_hist_grouped")) {
      yrep <- yrep[seq_len(min(nrow(yrep), resid_hist_panels)), ,
                   drop = FALSE]
    }
  } else {
    pred_args <- c(
      list(
        object,
        newdata = newdata,
        ndraws = ndraws,
        draw_ids = draw_ids,
        resp = resp
      ),
      dots[for_pred]
    )
    pred_args <- diagnostic_surface_args(
      pred_args, in_sample, weighted = psis_weighted
    )
    yrep <- do_call(method, pred_args)
  }

  # Closure-unit families: y and yrep collapse to the per-unit grain
  # through the shared aggregator. For resid_* types yrep is already
  # per unit (residuals.mvgam aggregates internally). Only y changes
  # length then. `group` and `x` are checked to be constant within each
  # unit. They map to the first visit of each unit, one value per
  # closure unit for bayesplot.
  closure_unit_lookup <- NULL
  # The multivariate closure-unit families (mvn, mvt) hold one
  # observation per (site, species) row. Their checks run at the row
  # grain bayesplot expects. The per-unit aggregation in
  # `closure_unit_pp_check_setup()` serves nmix and occ, whose visits
  # within a site collapse to a sufficient statistic before scoring.
  if (is_closure_unit_family(object$family)) {
    cu <- closure_unit_pp_check_setup(
      object  = object,
      newdata = newdata,
      yrep    = yrep,
      type    = type
    )
    y <- cu$y
    yrep <- cu$yrep
    closure_unit_lookup <- cu$first_visits
    if (!is.null(group)) {
      check_closure_unit_var_unit_constant(
        newdata[[group]], cu$arrays, var_name = group
      )
    }
    if (!is.null(x)) {
      check_closure_unit_var_unit_constant(
        newdata[[x]], cu$arrays, var_name = x
      )
    }
  }

  # Missing responses are dropped once, at the grain every object now
  # shares. A closure-unit family has one value per unit. A unit keeps
  # its place when one of its visits was never made. Dropping visits
  # before the aggregation left the matrix one width and the frame
  # another. For every other family the grain is the row.
  # `build_resid_lag_panel()` handles missing responses itself. It
  # keeps them in place for the ACF and drops them for the pACF.
  # A missing response is usually a gap in the sampling design, such as
  # one response of several recorded on fewer occasions. The omission
  # is reported once per session.
  if (anyNA(y) && !type %in% c("resid_acf", "resid_pacf")) {
    inform_once(
      "Observations with a missing response are omitted from the plot.",
      id = "mvgam_pp_check_missing_response"
    )
    take <- !is.na(y)
    y <- y[take]
    yrep <- yrep[, take, drop = FALSE]
  } else {
    take <- NULL
  }

  # Diagnostic resid types build their plot directly from the
  # residual draws (and, for resid_vs_fitted, posterior_epred
  # values at the same draw_ids). They never route through
  # bayesplot's ppc_* dispatcher.
  if (type %in% resid_diagnostic) {
    # yrep currently holds -1 * residuals (bayesplot error
    # convention); restore the natural-sign DS residual draws.
    resid_draws <- -1 * yrep
    if (type %in% c("resid_acf", "resid_pacf")) {
      return(label_resid_series(build_resid_lag_panel(
        resid_draws, lag_type = sub("resid_", "", type)
      ), series_sel))
    }
    if (type == "resid_qq") {
      return(label_resid_series(
        build_resid_qq_panel(resid_draws), series_sel
      ))
    }
    if (type == "resid_vs_fitted") {
      # The fitted values take their surface from the same helper as
      # the residuals. Both axes of the panel then describe one
      # surface of the fit.
      fitted_draws <- do.call(posterior_epred, diagnostic_surface_args(
        list(
          object, newdata = newdata,
          ndraws = NULL, draw_ids = draw_ids,
          resp = resp
        ),
        in_sample
      ))
      if (!is.null(series_sel)) {
        fitted_draws <- fitted_draws[, series_sel$rows, drop = FALSE]
      }
      if (!is.null(take)) {
        fitted_draws <- fitted_draws[, take, drop = FALSE]
      }
      # `per_obs` (default TRUE) collapses each observation to
      # its posterior median for both fitted and residual,
      # matching the one-dot-per-observation `plot.lm` style.
      # Pass `per_obs = FALSE` to retain the pooled draw x obs
      # scatter that exposes posterior uncertainty.
      per_obs <- isTRUE(per_obs)
      return(label_resid_series(build_resid_vs_fitted_panel(
        resid_draws, fitted_draws, per_obs = per_obs
      ), series_sel))
    }
  }

  # Prepare plotting arguments
  ppc_args <- list()
  if (prefix == "ppc") {
    ppc_args$y <- y
    ppc_args$yrep <- yrep
  } else if (prefix == "ppd") {
    ppc_args$ypred <- yrep
  }
  if (!is.null(group)) {
    if (!exists(group, newdata)) {
      stop(insight::format_error(
        paste0("Variable '", group, "' is absent from 'newdata'.")
      ))
    }
    ppc_args$group <- newdata[[group]]

    # Closure-unit: one value per unit, from its first visit.
    # `closure_unit_lookup` indexes the whole frame. It applies before
    # any narrowing. `take` is at the grain the lookup produces.
    if (!is.null(closure_unit_lookup)) {
      ppc_args$group <- ppc_args$group[closure_unit_lookup]
    }
    if (!is.null(take)) {
      ppc_args$group <- ppc_args$group[take]
    }
  }

  is_like_factor <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
  }

  if (!is.null(x)) {
    ppc_args$x <- newdata[[x]]
    if (!is_like_factor(ppc_args$x)) {
      ppc_args$x <- as.numeric(ppc_args$x)
    }

    if (!is.null(closure_unit_lookup)) {
      ppc_args$x <- ppc_args$x[closure_unit_lookup]
    }
    if (!is.null(take)) {
      ppc_args$x <- ppc_args$x[take]
    }
  }

  needs_psis <- any(c("psis_object", "lw") %in%
                    setdiff(ppc_formals, names(ppc_args)))
  if (needs_psis) {
    ll <- log_lik(object, newdata = newdata, incl_autocor = TRUE,
                  resp = resp, draw_ids = draw_ids)
    # Importance sampling refuses a column with no density in it, so
    # the unscorable rows a missing response leaves go first. The
    # observed values and replicates were narrowed to the same rows
    # above, where the NA responses were dropped from `y`.
    ll <- clean_ll(object, ll)
    r_eff <- mvgam_r_eff_log_lik(object, ll, draw_ids = draw_ids)
    # The panel below is drawn from these weights. `loo()` on the same
    # fit reports the Pareto diagnostic to the user. Muted here, the
    # LOO-PIT plot gave no sign that its weights were unusable. The
    # cached AR test fit has k above the threshold in 100 of its 160
    # scored columns.
    psis_obj <- loo::psis(-ll, r_eff = r_eff)
    if ("psis_object" %in% ppc_formals) {
      ppc_args$psis_object <- psis_obj
    }
    if ("lw" %in% ppc_formals) {
      ppc_args$lw <- stats::weights(psis_obj)
    }
  }

  ppc_args <- c(ppc_args, dots[!for_pred])

  # Generate plot
  out_plot <- do_call(ppc_fun, ppc_args)

  if ("x" %in% ppc_formals && !is.null(x)) {
    out_plot <- out_plot +
      ggplot2::labs(x = x)
  }

  # Improve labels for residual plots
  if (type %in% c("resid_hist", "resid_hist_grouped")) {
    out_plot <- out_plot +
      ggplot2::labs(x = "DS residuals")
  }

  if (type %in% c("resid_ribbon", "resid_ribbon_grouped")) {
    out_plot <- out_plot +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "DS residuals")
  }
  label_resid_series(out_plot, series_sel)
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
#' are its species. The species axis names them for all five families.
#' Numbering each unit's rows 1..K took a position for an identity. A
#' site missing one species shifted every later species onto another's
#' label.
#'
#' The labels come from the species axis. `as.factor()` on the frame's
#' column ordered the groups alphabetically over the values that frame
#' held. A fit declaring its species in another order, or a frame
#' missing one, then labelled the panels against the wrong species.
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
  # Multi-season families return `c("series", "site", "time")` here;
  # single-season families return NULL and fall back to the 2-axis
  # default inside `build_closure_unit_arrays()`.
  arrays <- closure_unit_arrays_for(object, newdata)
  if (grepl("resid", type)) {
    # `yrep` is already `[ndraws x N_unit]` (per-unit residuals).
    # These plots take the observed side as zero. Only its length is
    # set here.
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


#' Refuse a covariate that varies within a closure unit
#'
#' Guards the `group` and `x` arguments of `pp_check()` before they
#' map to the unit grain.
#'
#' @param values Vector of covariate values, one per visit row
#' @param arrays Closure-unit arrays holding `N_unit`, `n_rep` and
#'   `visit_row`
#' @param var_name Character name of the covariate, for the message
#' @return `NULL`, invisibly, or an error
#' @noRd
check_closure_unit_var_unit_constant <- function(values, arrays,
                                                   var_name) {
  checkmate::assert_atomic_vector(values)
  checkmate::assert_list(arrays)
  checkmate::assert_string(var_name)
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
    if (length(unique(values[idx])) > 1L) {
      stop(insight::format_error(c(
        paste0(
          "Covariate '", var_name,
          "' is not constant within every closure unit."
        ),
        i = "Closure-unit checks plot one value per unit."
      )))
    }
  }
  invisible(NULL)
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
#' @param newdata Data frame of new data, or `NULL` for the training
#'   data
#' @param stat Character statistic, `"chi_squared"` or
#'   `"freeman_tukey"`
#' @param group Character name of a unit-constant grouping column, or
#'   `NULL`
#' @param ndraws Integer number of draws, or `NULL`
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @return A `ggplot` object
#' @noRd
closure_unit_fit_stat_ppc <- function(object, newdata, stat,
                                        group, ndraws, draw_ids) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_string(stat)
  checkmate::assert_string(group, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  stat <- match.arg(stat, c("chi_squared", "freeman_tukey"))
  newdata <- prediction_frame(object, newdata)

  # Per-visit yrep + epred. Both arrive as [ndraws x N_visit] so they
  # share the closure-unit aggregator below.
  yrep_visit <- posterior_predict(
    object, newdata = newdata,
    ndraws = ndraws, draw_ids = draw_ids
  )
  epred_visit <- posterior_epred(
    object, newdata = newdata,
    ndraws = ndraws, draw_ids = draw_ids
  )

  # aggregate_closure_unit_visits() is the single shared per-visit
  # to per-unit summer used by every closure-unit post-fit surface
  # (pp_check bars / dens_overlay / etc., log_lik, residuals). Calling
  # it twice keeps the indexing and array bookkeeping in one place;
  # the y_unit field on the second call is discarded.
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

  # Optional further aggregation by user-supplied group columns.
  # Group columns must be unit-constant; the existing guard surfaces
  # a typed error if not.
  if (!is.null(group)) {
    checkmate::assert_character(group, min.len = 1L, any.missing = FALSE)
    bad <- setdiff(group, names(newdata))
    if (length(bad) > 0L) {
      stop(insight::format_error(c(
        "Group column(s) not found in newdata.",
        x = paste0(paste(bad, collapse = ", "), " missing.")
      )))
    }
    first_visits <- closure_unit_first_rows(arrays)
    for (g in group) {
      check_closure_unit_var_unit_constant(
        as.character(newdata[[g]]), arrays, g
      )
    }
    group_keys <- do.call(paste, c(
      lapply(group, function(g) {
        as.character(newdata[[g]][first_visits])
      }),
      sep = "|"
    ))
    grp_levels <- unique(group_keys)
    unit_to_grp <- match(group_keys, grp_levels)
    y_unit     <- as.numeric(tapply(y_unit, unit_to_grp, sum))
    yrep_unit  <- t(rowsum(t(yrep_unit),  group = unit_to_grp))
    epred_unit <- t(rowsum(t(epred_unit), group = unit_to_grp))
    grain <- paste(group, collapse = " x ")
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
#' @param ... Unused.
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
#' @param ... Unused.
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
#' @param newdata Data frame of new data, or `NULL`
#' @param ndraws Integer number of draws
#' @param resp Character response name, or `NULL`
#' @param ... Refused if supplied
#' @return A `patchwork` object
#' @noRd
mvgam_resid_panel <- function(
  object, series = NULL, newdata = NULL, ndraws = 100L, resp = NULL, ...
) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L)
  checkmate::assert_string(resp, null.ok = TRUE)
  # The panels are built from this function's own arguments. `...` is
  # refused. `plot.mvgam(type = "residuals")` calls this function. An
  # argument accepted here would have no effect.
  rlang::check_dots_empty()
  # The shared fan-out builds one grid per response, stacked into one
  # figure. Without it each inner `pp_check()` call would return a
  # figure per response. `patchwork::wrap_plots()` refuses nested
  # lists.
  fan <- mv_resp_fan_out(object, resp, combine = stack_response_plots)
  if (!is.null(fan)) return(fan)
  # Every panel uses the same posterior draws. Drawn separately, the
  # Q-Q panel and the ACF described different subsets of the posterior.
  draw_ids <- resolve_draw_ids(object, ndraws, NULL)
  closure_unit <- is_closure_unit_family(object$family)
  types <- if (closure_unit) {
    "resid_qq"
  } else {
    c("resid_vs_fitted", "resid_qq", "resid_acf", "resid_pacf")
  }
  # The panels run the same residual extraction over the same rows.
  # A condition it raises about the data applies to the whole grid,
  # and it is raised once.
  panels <- warn_once_per_call(lapply(types, function(type) {
    pp_check(
      object, type = type, newdata = newdata, draw_ids = draw_ids,
      resp = resp, series = series
    ) + ggplot2::labs(subtitle = NULL)
  }))
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
