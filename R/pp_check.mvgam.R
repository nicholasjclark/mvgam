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
#' @param newdata Optional \code{dataframe} or \code{list} of test data
#'   containing the variables included in the linear predictor of
#'   \code{formula}. If not supplied, predictions are generated for the
#'   original observations used for the model fit. Ignored if using one
#'   of the residual plots (i.e. 'resid_hist')
#'
#' @param ... Further arguments passed to \code{\link{predict.mvgam}}
#'   as well as to the PPC function specified in \code{type}
#'
#' @return A ggplot object that can be further
#'   customized using the \pkg{ggplot2} package.
#'
#' @details For a detailed explanation of each of the ppc functions,
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
#'   covariate (`intervals`, `ribbon`), and the residual
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
#' \donttest{
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
  ...
) {
  set_color_scheme_local("red")

  dots <- list(...)
  type_missing <- missing(type)
  if (type_missing) {
    type <- "dens_overlay"
  }

  prefix <- match.arg(prefix)
  ndraws_given <- "ndraws" %in% names(match.call())

  if (is.null(newdata)) {
    # Fitting data lives on $data; some objects also expose $obs_data
    # as an alias and may have it empty.
    newdata <- object$data %||% object$obs_data
  }

  # Multi-response custom families (diri / multi / categ / mvn /
  # mvt): a default `dens_overlay` would pool every category /
  # species into a single curve and hide the per-category fit.
  # When the user has not asked for a specific `group` or `type`,
  # pick a per-family default type, build a category vector
  # aligned with the long-form prediction grain, attach it to
  # `newdata` under a reserved name, and swap `type` to its
  # `_grouped` variant. The user can opt out by passing
  # `group = "<existing-column>"` explicitly.
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
  # NULL, fan out per response via the shared helper and return
  # a named list. When the caller supplied `resp`, validate it
  # and fall through to the univariate path scoped to that
  # response.
  is_mv <- brms::is.mvbrmsformula(object$formula)
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) return(fan)
  if (is_mv) {
    resp_names <- object$formula$responses
    if (length(resp) != 1L) {
      stop(insight::format_error(c(
        "{.field resp} must be a single response name.",
        i = cli::format_inline(
          "Available responses: {.val {resp_names}}."
        )
      )))
    }
    if (!resp %in% resp_names) {
      stop(insight::format_error(
        cli::format_inline(
          "Invalid {.field resp}: {.val {resp}}. Valid choices: {.val {resp_names}}."
        )
      ))
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
        # Closure-unit-only chi-squared / Freeman-Tukey discrepancy
        # GOF (Gelman et al. 1996). Returns a `mvgam_ppc_fit_stat`
        # object with print + plot methods rather than a bare ggplot;
        # the rest of the bayesplot-dispatch path is bypassed.
        "ppc_fit_stat"
      )
    )
    valid_types <- sub("^ppc_", "", valid_types)
    if (!type %in% valid_types) {
      stop(
        "Type '",
        type,
        "' is not a valid ppc type. ",
        "Valid types are:\n",
        paste0("'", valid_types, "'", collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Short-circuit for the closure-unit fit-statistic GOF: it has a
  # custom computation (per-draw discrepancy on epred + yrep) and
  # returns a dedicated `mvgam_ppc_fit_stat` object, so it bypasses
  # the bayesplot dispatch entirely.
  if (identical(type, "fit_stat")) {
    if (!is_closure_unit_family(object$family)) {
      stop(insight::format_error(c(
        "pp_check(type = 'fit_stat') is only available for closure-unit families.",
        i = "Use family = occ() or family = nmix() to enable this discrepancy GOF."
      )))
    }
    if (!ndraws_given) {
      ndraws <- 500L
      message(
        "Using 500 posterior draws for ppc type 'fit_stat' by default."
      )
    }
    stat <- list(...)$stat %||% "chi_squared"
    return(closure_unit_fit_stat_ppc(
      object   = object,
      newdata  = newdata,
      stat     = stat,
      group    = group,
      ndraws   = ndraws,
      draw_ids = draw_ids
    ))
  }

  bptype <- type

  # Residual ppc types: residuals() is computed on-the-fly below
  # (line ~323). Just rename the bptype to the corresponding
  # `error_*` bayesplot function.
  if (bptype %in% c("resid_hist", "resid_hist_grouped")) {
    bptype <- sub("resid", "error", bptype)
  }
  if (bptype %in% c("resid_ribbon", "resid_ribbon_grouped")) {
    bptype <- sub("resid_", "", bptype)
  }

  # The four diagnostic resid types skip bayesplot entirely (see
  # the dispatch later in this function); short-circuit so the
  # bayesplot lookup doesn't fail on `ppc_resid_acf` etc.
  ppc_fun <- if (type %in% c(
    "resid_acf", "resid_pacf", "resid_qq", "resid_vs_fitted"
  )) {
    NULL
  } else {
    get(paste0(prefix, "_", bptype), asNamespace("bayesplot"))
  }

  # Closure-unit families (nmix, occ) operate at the closure-unit
  # grain (one per site x season). Per-row PPC types that assume
  # exchangeable observations or a per-row time axis do not match
  # that grain and are blocked up front; the rest of the ppc_*
  # surface routes through a per-unit aggregation injected below
  # (see closure_unit_pp_check_setup()).
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
          "Per-row scatter, fitted-vs-residual, and per-row ",
          "time-axis types do not match the closure-unit grain."
        ),
        i = paste0(
          "Use type = 'bars', 'rootogram', 'dens_overlay', ",
          "'ecdf_overlay', 'intervals', 'stat', 'resid_hist', ",
          "or 'resid_qq' for closure-unit fits."
        )
      )))
    }
  }
  # Validate group / x against the column names of newdata. insight's
  # get_predictors does not dispatch on mvgam fits, and the variable-name
  # check is all we need here. The diagnostic resid types skip this
  # block; they have no bayesplot formals to query.
  valid_vars <- names(newdata)
  ppc_formals <- if (is.null(ppc_fun)) {
    character(0L)
  } else {
    names(formals(ppc_fun))
  }
  if ("group" %in% ppc_formals) {
    if (is.null(group)) {
      stop(
        "Argument 'group' is required for ppc type '",
        type,
        "'.",
        call. = FALSE
      )
    }
    if (!group %in% valid_vars) {
      stop(
        "Variable '",
        group,
        "' could not be found in the data.",
        call. = FALSE
      )
    }
  }
  if ("x" %in% ppc_formals) {
    if (!is.null(x) && !x %in% valid_vars) {
      stop("Variable '", x, "' could not be found in the data.", call. = FALSE)
    }
  }
  if (type == "error_binned") {
    method <- "posterior_epred"
  } else {
    method <- "posterior_predict"
  }
  # Type-specific draw count defaults + warnings.
  #
  # Non-grouped resid plots use empirical PIT residuals: ndraws
  # below ~50 makes the PIT support underflow for observations in
  # the predictive tail, producing apparent ±8 extreme values
  # that are floating-point clamp artefacts rather than model
  # misfit. Default to 500 and warn if the user passes < 50.
  #
  # Grouped resid plots route through bayesplot facets that put
  # one panel per draw × group. With ndraws much above ~12 the
  # panels collapse to invisible at typical canvas sizes. Default
  # to 8 and warn if the user passes > 12.
  resid_nongrouped <- c("resid_hist", "resid_ribbon")
  resid_grouped <- c("resid_hist_grouped", "resid_ribbon_grouped")
  resid_diagnostic <- c(
    "resid_acf", "resid_pacf", "resid_qq", "resid_vs_fitted"
  )
  if (!ndraws_given) {
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
    if (type %in% aps_types) {
      ndraws <- NULL
      message("Using all posterior draws for ppc type '", type, "' by default.")
    } else if (type %in% resid_nongrouped) {
      ndraws <- 500L
      message("Using 500 posterior draws for ppc type '", type, "' by default.")
    } else if (type %in% resid_grouped) {
      ndraws <- 8L
      message("Using 8 posterior draws for ppc type '", type, "' by default.")
    } else if (type %in% resid_diagnostic) {
      ndraws <- 100L
      message(
        "Using 100 posterior draws for ppc type '", type,
        "' by default."
      )
    } else {
      ndraws <- 10
      message("Using 10 posterior draws for ppc type '", type, "' by default.")
    }
  } else {
    if (type %in% resid_nongrouped && !is.null(ndraws) &&
          ndraws < 50L) {
      rlang::warn(
        paste0(
          "ndraws < 50 for '", type, "' may produce apparent ",
          "extreme residuals from PIT support underflow on ",
          "observations in the predictive tail. Consider ",
          "ndraws >= 500 for a stable empirical PIT."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_pp_check_resid_low_ndraws"
      )
    }
    if (type %in% resid_grouped && !is.null(ndraws) &&
          ndraws > 12L) {
      rlang::warn(
        paste0(
          "ndraws > 12 for '", type, "' may collapse the ",
          "bayesplot facet panels (one panel per draw x group) ",
          "to invisible at typical canvas sizes. Consider ",
          "ndraws <= 8."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_pp_check_resid_grouped_high_ndraws"
      )
    }
  }

  y <- NULL
  if (prefix == "ppc") {
    # y is ignored in prefix 'ppd' plots. Pull the response name from
    # the formula's lhs; for multivariate fits use the per-resp form.
    # Handles the cbind(success, failure) binomial and the
    # `y | trials(trials)` aterms convention by taking the leftmost
    # variable in the lhs expression.
    lhs <- if (is_mv) {
      object$formula$forms[[resp]]$formula[[2L]]
    } else if (inherits(object$formula, "brmsformula")) {
      object$formula$formula[[2L]]
    } else {
      object$formula[[2L]]
    }
    out_name <- all.vars(lhs)[1L]
    y <- newdata[[out_name]]
    # Ordinal responses arrive as ordered factors; bayesplot's ppc_*
    # functions assert numeric y, so coerce factors to their integer
    # codes (1..nlevels).
    if (is.factor(y)) {
      y <- as.integer(y)
    }
  }

  # For plotting DS residuals, set y to zero and take
  # -1 * residual so that errors are in the correct direction
  # If a subsample of draws is requested, pin it once here so yrep and
  # the PSIS log-weights below refer to the same posterior draws.
  if (!is.null(ndraws) && is.null(draw_ids)) {
    total_draws <- posterior::ndraws(posterior::as_draws_array(object$fit))
    draw_ids <- sort(sample.int(total_draws, min(ndraws, total_draws)))
    ndraws <- NULL
  }

  if (grepl("resid", type)) {
    y[!is.na(y)] <- 0
    # residuals(summary = FALSE) returns [ndraws x nobs] (brms
    # convention; matches posterior_predict output below). On
    # multivariate fits residuals() returns a per-response list
    # when `resp` is NULL; the upstream pp_check.mvgam path either
    # set `resp` to a single response (from the mv fan-out) or
    # the user did, so forwarding here scopes residuals to a
    # single matrix the `-1 *` and subsequent ppc kernels accept.
    yrep <- -1 * residuals(
      object, summary = FALSE,
      ndraws = if (!is.null(draw_ids)) NULL else ndraws,
      draw_ids = draw_ids, resp = resp
    )
  } else {
    pred_args <- list(
      object,
      newdata = newdata,
      ndraws = ndraws,
      draw_ids = draw_ids,
      resp = resp,
      ...
    )
    yrep <- do_call(method, pred_args)
  }

  if (anyNA(y)) {
    warning("NA responses are not shown in 'pp_check'.")
    take <- !is.na(y)
    y <- y[take]
    yrep <- yrep[, take, drop = FALSE]
  } else {
    take <- NULL
  }

  # Closure-unit families: collapse y and yrep to the per-unit
  # grain via the shared aggregator. For resid_* types yrep is
  # already per-unit (residuals.mvgam aggregates internally) so
  # only y needs the length swap. group / x covariates are
  # validated as unit-constant and remapped to the first-visit
  # row per unit so bayesplot sees one value per closure unit.
  closure_unit_lookup <- NULL
  # Mv-response closure-unit families (mvn, mvt) carry one
  # observation per (site, species) row, so pp_check operates at
  # the row grain that bayesplot expects by default. The per-unit
  # aggregation in `closure_unit_pp_check_setup()` is for
  # nmix / occ where multiple visits within a site need a
  # sufficient-statistic collapse before scoring.
  if (needs_closure_unit_aggregation(object$family)) {
    cu <- closure_unit_pp_check_setup(
      object  = object,
      newdata = newdata,
      y       = y,
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

  # Diagnostic resid types build their plot directly from the
  # residual draws (and, for resid_vs_fitted, posterior_epred
  # values at the same draw_ids). They never route through
  # bayesplot's ppc_* dispatcher.
  if (type %in% resid_diagnostic) {
    # yrep currently holds -1 * residuals (bayesplot error
    # convention); restore the natural-sign DS residual draws.
    resid_draws <- -1 * yrep
    if (type %in% c("resid_acf", "resid_pacf")) {
      return(build_resid_lag_panel(
        resid_draws, lag_type = sub("resid_", "", type)
      ))
    }
    if (type == "resid_qq") {
      return(build_resid_qq_panel(resid_draws))
    }
    if (type == "resid_vs_fitted") {
      fitted_draws <- posterior_epred(
        object, newdata = newdata,
        ndraws = NULL, draw_ids = draw_ids,
        resp = resp
      )
      if (!is.null(take)) {
        fitted_draws <- fitted_draws[, take, drop = FALSE]
      }
      # `per_obs` (default TRUE) collapses each observation to
      # its posterior median for both fitted and residual,
      # matching the one-dot-per-observation `plot.lm` style.
      # Pass `per_obs = FALSE` to retain the pooled draw x obs
      # scatter that exposes posterior uncertainty.
      per_obs <- isTRUE(dots$per_obs %||% TRUE)
      return(build_resid_vs_fitted_panel(
        resid_draws, fitted_draws, per_obs = per_obs
      ))
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
      stop(paste0("Variable ", group, " not in newdata"), call. = FALSE)
    }
    ppc_args$group <- newdata[[group]]

    if (!is.null(take)) {
      ppc_args$group <- ppc_args$group[take]
    }
    # Closure-unit: select one value per unit (first-visit row).
    if (!is.null(closure_unit_lookup)) {
      ppc_args$group <- ppc_args$group[closure_unit_lookup]
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

    if (!is.null(take)) {
      ppc_args$x <- ppc_args$x[take]
    }
    if (!is.null(closure_unit_lookup)) {
      ppc_args$x <- ppc_args$x[closure_unit_lookup]
    }
  }

  needs_psis <- any(c("psis_object", "lw") %in%
                    setdiff(ppc_formals, names(ppc_args)))
  if (needs_psis) {
    ll <- log_lik(object, newdata = newdata, process_error = TRUE,
                  resp = resp, draw_ids = draw_ids)
    chains <- posterior::nchains(posterior::as_draws_array(object$fit))
    n_per_chain <- NROW(ll) / chains
    r_eff <- loo::relative_eff(
      exp(ll),
      chain_id = sort(rep(seq_len(chains), n_per_chain))
    )
    psis_obj <- suppressWarnings(
      loo::psis(-ll, r_eff = r_eff)
    )
    if ("psis_object" %in% ppc_formals) {
      ppc_args$psis_object <- psis_obj
    }
    if ("lw" %in% ppc_formals) {
      ppc_args$lw <- stats::weights(psis_obj)
    }
  }

  # Most ... arguments are meant for the prediction function
  for_pred <- names(dots) %in% names(formals(posterior_predict.mvgam))
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
  out_plot
}


# Internal: closure-unit pp_check setup.
#
# Collapses the per-visit response and per-visit posterior
# predictive draws to the closure-unit grain via the shared
# `aggregate_closure_unit_visits()` helper, so bayesplot's ppc_*
# functions operate on `(y_unit, yrep_unit)`. Per-visit
# residuals would treat visits within a closure unit as
# exchangeable, which they are not (visits share the latent
# state). For `resid_*` types, `yrep` arrives already at the
# unit grain because `residuals.mvgam` aggregates internally; we
# only synthesise a length-matched `y_unit` vector (set to zeros
# Build a factor mapping each long-form prediction column to a
# category, used to auto-group pp_check on multi-response custom
# families (diri / multi / categ on the per-unit K-vector axis;
# mvn / mvt on the per-row species axis). Returns NULL when the
# family is not multi-response or when no sensible mapping
# exists, in which case pp_check falls through to its default
# pooled behaviour.
#
# Simplex families (diri / multi / categ): every closure unit
# packs K rows into the long layout via
# `build_closure_unit_arrays()$visit_idx`. The within-unit
# position 1..K of each row is its category label.
#
# mv-normal / mv-T: one row per (site, species); the `series`
# column on the fit's data is the natural category axis.
#'@noRd
pp_check_mv_category <- function(object, newdata) {
  fam <- object$family
  if (!is_multi_response_family(fam)) return(NULL)
  if (is_simplex_response_family(fam)) {
    # Simplex families default to single-axis `time` grouping at
    # fit time (R/families.R:3534 in prepare_closure_unit_family).
    # Mirror that fallback here so the category vector matches the
    # per-unit K-row layout that posterior_predict actually emits.
    arrs <- build_closure_unit_arrays(
      newdata,
      response_var = closure_unit_response_var(object$formula),
      compute_y_max = FALSE,
      unit_grouping_vars = closure_unit_grouping(fam) %||% "time"
    )
    cat_int <- integer(nrow(newdata))
    for (g in seq_len(arrs$N_unit)) {
      Kg <- arrs$n_rep[g]
      idx <- arrs$visit_idx[g, seq_len(Kg)]
      cat_int[idx] <- seq_len(Kg)
    }
    levels_int <- sort(unique(cat_int))
    return(factor(
      paste0("cat_", cat_int),
      levels = paste0("cat_", levels_int)
    ))
  }
  if ("series" %in% names(newdata)) {
    return(as.factor(newdata$series))
  }
  NULL
}


# by the caller for the residual histograms).
#
# Returns a list `(y, yrep, arrays, first_visits)`. The
# `first_visits` lookup is used by the caller to dedup any
# `group` / `x` covariate the user supplied so bayesplot sees
# one covariate value per closure unit.
#'@noRd
closure_unit_pp_check_setup <- function(object, newdata, y, yrep,
                                          type) {
  resp_var <- closure_unit_response_var(object$formula)
  default_cap <- closure_unit_default_cap(object$family)
  # Multi-season families return `c("series", "site", "time")` here;
  # single-season families return NULL and fall back to the 2-axis
  # default inside `build_closure_unit_arrays()`.
  arrays <- build_closure_unit_arrays(
    newdata, response_var = resp_var, default_cap = default_cap,
    unit_grouping_vars = closure_unit_grouping(object$family)
  )
  if (grepl("resid", type)) {
    # `yrep` is already `[ndraws x N_unit]` (per-unit residuals);
    # `y` is set to zeros downstream for resid_* types so only
    # the length matters here.
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
    first_visits = arrays$visit_idx[, 1L]
  )
}


# Internal: assert that a per-visit covariate is constant within
# every closure unit. Used by the closure-unit pp_check setup to
# guard the `group` and `x` ppc_* arguments before they get
# remapped to the per-unit grain.
#'@noRd
check_closure_unit_var_unit_constant <- function(values, arrays,
                                                   var_name) {
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    if (length(unique(values[idx])) > 1L) {
      stop(insight::format_error(c(
        paste0(
          "Covariate '", var_name,
          "' is not constant within every closure unit."
        ),
        x = paste0(
          "Closure-unit pp_check operates at the unit grain ",
          "(one value per site x season); '", var_name,
          "' varies within at least one closure unit."
        ),
        i = paste0(
          "Drop '", var_name,
          "' from `group =` / `x =`, or use a unit-constant ",
          "covariate (e.g. a site-level trait)."
        )
      )))
    }
  }
  invisible(NULL)
}


# Internal: ACF / pACF panel for DS residual draws.
# `resid_draws` is `[ndraws x nobs]`; computes per-draw ACF (or
# pACF) via stats::acf / stats::pacf, then summarises across
# draws with nested quantile segments at each integer lag.
# Bands are drawn as `geom_segment` instead of the time-axis
# ribbons in P0 because the x-axis is discrete lag positions,
# not continuous time. The dashed band shows the asymptotic
# 95% white-noise interval `+/- 1.96 / sqrt(n)`.
#'@noRd
build_resid_lag_panel <- function(
  resid_draws, lag_type = c("acf", "pacf")
) {
  lag_type <- match.arg(lag_type)
  acf_fn <- if (lag_type == "acf") stats::acf else stats::pacf
  ndraws <- nrow(resid_draws)
  per_draw <- lapply(seq_len(ndraws), function(d) {
    out <- acf_fn(
      resid_draws[d, ], plot = FALSE, na.action = stats::na.pass
    )
    data.frame(
      value = out$acf[, , 1L],
      lag = out$lag[, 1L, 1L],
      n_used = out$n.used
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
  ylab <- if (lag_type == "acf") "Autocorrelation" else
    "Partial autocorrelation"
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
    ggplot2::labs(title = title, x = "Lag", y = ylab) +
    mvgam_theme()
}


# Internal: Q-Q panel for DS residual draws. Pools all draws,
# overlays a single reference line and double-plots points
# (white outline + dark fill) for the mvgam point overlay style.
#'@noRd
build_resid_qq_panel <- function(resid_draws) {
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


# Internal: Resids-vs-fitted panel. `per_obs = TRUE` (the default
# at the call site in `pp_check.mvgam`) collapses each
# observation to its posterior median for both fitted and
# residual — one point per observation, matching the
# `plot.lm` convention. `per_obs = FALSE` retains the pooled
# (draw x obs) scatter that exposes the per-draw spread of
# discrete-PIT residuals at low fitted values. The thin-plate
# gam smoother ribbon highlights any systematic mean trend in
# either view.
#'@noRd
build_resid_vs_fitted_panel <- function(
  resid_draws, fitted_draws, per_obs = TRUE
) {
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


# Internal: closure-unit chi-squared / Freeman-Tukey discrepancy
# GOF (Gelman et al. 1996). For each posterior draw s:
#   T_obs_s = D(y_obs, E[y|theta_s]),  T_rep_s = D(y_rep_s, E[y|theta_s])
# with D either chi-squared (sum((y - e)^2 / (e + eps))) or
# Freeman-Tukey (sum((sqrt(y) - sqrt(e))^2)). Per-visit yrep + epred
# are aggregated to the closure-unit grain via the shared
# `aggregate_closure_unit_visits()` helper. An optional `group =`
# vector of unit-constant covariate columns further sums per group
# level before applying D. Bayesian p-value is the posterior
# probability P(T_rep >= T_obs).
#'@noRd
closure_unit_fit_stat_ppc <- function(object, newdata, stat,
                                        group, ndraws, draw_ids) {
  stat <- match.arg(stat, c("chi_squared", "freeman_tukey"))
  if (is.null(newdata)) newdata <- object$data %||% object$obs_data

  # Per-visit yrep + epred. Both arrive as [ndraws x N_visit] so they
  # share the closure-unit aggregator below.
  yrep_visit <- posterior_predict(
    object, newdata = newdata, summary = FALSE,
    ndraws = ndraws, draw_ids = draw_ids
  )
  epred_visit <- posterior_epred(
    object, newdata = newdata, summary = FALSE,
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
    first_visits <- arrays$visit_idx[, 1L]
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
  stat_lab <- if (identical(x$stat, "chi_squared")) {
    "chi-squared"
  } else {
    "Freeman-Tukey"
  }
  cat("Posterior predictive check (closure-unit discrepancy)\n")
  cat(sprintf("  Family:           %s\n",       x$family))
  cat(sprintf("  Statistic:        %s\n",       stat_lab))
  cat(sprintf("  Aggregation:      %s\n",       x$grain))
  cat(sprintf("  Posterior draws:  %d\n",       x$n_draws))
  cat(sprintf("  Bayesian p-value: %.3f\n",     x$bayes_p))
  if (x$bayes_p < 0.05 || x$bayes_p > 0.95) {
    cat(
      "  Note: extreme p-value indicates poor model fit at this grain.\n"
    )
  }
  invisible(x)
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
  d <- data.frame(T_obs = x$T_obs, T_rep = x$T_rep)
  rng <- range(c(d$T_obs, d$T_rep))
  stat_lab <- if (identical(x$stat, "chi_squared")) {
    "chi-squared"
  } else {
    "Freeman-Tukey"
  }
  ggplot2::ggplot(d, ggplot2::aes(x = .data$T_obs, y = .data$T_rep)) +
    ggplot2::geom_point(alpha = 0.4, colour = "steelblue") +
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
    ggplot2::theme_classic()
}


# Internal: 4-panel residual diagnostic patchwork. Called by
# `plot.mvgam(x, type = "residuals")` in the dispatcher. Bundles
# the four diagnostic pp_check types into a single ggplot via
# `patchwork::wrap_plots`.
#'@noRd
mvgam_resid_panel <- function(
  object, newdata = NULL, ndraws = 100L, resp = NULL, ...
) {
  # Multivariate fan-out via the shared helper: returns one
  # 4-panel grid per response in a named list. Without this each
  # inner `pp_check` call would itself return a list per response
  # and `patchwork::wrap_plots` refuses nested lists.
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) return(fan)
  p1 <- pp_check(
    object, type = "resid_vs_fitted", newdata = newdata,
    ndraws = ndraws, resp = resp, ...
  )
  p2 <- pp_check(
    object, type = "resid_qq", newdata = newdata,
    ndraws = ndraws, resp = resp, ...
  )
  p3 <- pp_check(
    object, type = "resid_acf", newdata = newdata,
    ndraws = ndraws, resp = resp, ...
  )
  p4 <- pp_check(
    object, type = "resid_pacf", newdata = newdata,
    ndraws = ndraws, resp = resp, ...
  )
  patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2L, nrow = 2L)
}
