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
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(seasonality = "hierarchical")
#' mod <- mvgam(
#'   y ~ series +
#'     s(season, bs = "cc", k = 6) +
#'     s(season, series, bs = "fs", k = 4),
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Use pp_check(mod, type = "xyz") for a list of available plot types
#'
#' # Default is a density overlay for all observations
#' pp_check(mod)
#'
#' # Rootograms particularly useful for count data
#' pp_check(mod, type = "rootogram")
#'
#' # Grouping plots by series is useful
#' pp_check(mod,
#'   type = "bars_grouped",
#'   group = "series", ndraws = 50
#' )
#' pp_check(mod,
#'   type = "ecdf_overlay_grouped",
#'   group = "series", ndraws = 50
#' )
#' pp_check(mod,
#'   type = "stat_freqpoly_grouped",
#'   group = "series", ndraws = 50
#' )
#'
#' # Several types can be used to plot distributions of randomized
#' # quantile residuals
#' pp_check(
#'   object = mod,
#'   x = "season",
#'   type = "resid_ribbon"
#' )
#' pp_check(
#'   object = mod,
#'   x = "season",
#'   group = "series",
#'   type = "resid_ribbon_grouped"
#' )
#' pp_check(mod,
#'   ndraws = 5,
#'   type = "resid_hist_grouped",
#'   group = "series"
#' )
#'
#' # PSIS-LOO bayesplot variants
#' pp_check(mod, type = "loo_pit_overlay")
#'
#' # Custom functions accepted
#' pp_check(mod, type = "stat", stat = function(x) mean(x == 0))
#' pp_check(mod,
#'   type = "stat_grouped",
#'   stat = function(x) mean(x == 0),
#'   group = "series"
#' )
#'
#' # Some functions accept covariates to set the x-axes
#' pp_check(mod,
#'   x = "season",
#'   type = "ribbon_grouped",
#'   prob = 0.5,
#'   prob_outer = 0.8,
#'   group = "series"
#' )
#'
#' # Many plots can be made without the observed data
#' pp_check(mod, prefix = "ppd")
#' }
#'
#' \dontrun{
#' # Closure-unit families: pp_check aggregates per-visit y and
#' # yrep to the per-unit grain (sum of detections per site) and
#' # then dispatches to bayesplot. Default `type = "bars"` gives
#' # the per-site detection-count distribution; pair with a
#' # unit-constant grouping covariate (e.g. site elevation) for
#' # faceted variants.
#' occ_fit <- mvgam(bf(y ~ elev, p ~ tod), family = occ(),
#'                  data = closure_unit_data)
#' pp_check(occ_fit, type = "bars", ndraws = 200)
#' pp_check(occ_fit, type = "rootogram")
#' pp_check(occ_fit, type = "stat",
#'          stat = function(x) mean(x == 0))
#' pp_check(occ_fit, type = "resid_hist", ndraws = 8)
#' }
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
  if (missing(type)) {
    type <- "dens_overlay"
  }

  prefix <- match.arg(prefix)
  ndraws_given <- "ndraws" %in% names(match.call())

  if (is.null(newdata)) {
    # Fitting data lives on $data; some objects also expose $obs_data
    # as an alias and may have it empty.
    newdata <- object$data %||% object$obs_data
  }

  # brms parity: for multivariate fits require a single resp argument.
  is_mv <- brms::is.mvbrmsformula(object$formula)
  resp_names <- if (is_mv) object$formula$responses else character(0)
  if (is_mv) {
    if (is.null(resp) || length(resp) != 1L) {
      stop(insight::format_error(c(
        "{.field resp} must be a single response name for a multivariate model.",
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
        "ppc_resid_vs_fitted"
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
    # convention; matches posterior_predict output below).
    yrep <- -1 * residuals(
      object, summary = FALSE,
      ndraws = if (!is.null(draw_ids)) NULL else ndraws,
      draw_ids = draw_ids
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
  if (is_closure_unit_family(object$family)) {
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
  arrays <- build_closure_unit_arrays(
    newdata, response_var = resp_var, default_cap = default_cap
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


# Internal: 4-panel residual diagnostic patchwork. Called by
# `plot.mvgam(x, type = "residuals")` in the dispatcher. Bundles
# the four diagnostic pp_check types into a single ggplot via
# `patchwork::wrap_plots`.
#'@noRd
mvgam_resid_panel <- function(
  object, newdata = NULL, ndraws = 100L, ...
) {
  p1 <- pp_check(
    object, type = "resid_vs_fitted", newdata = newdata,
    ndraws = ndraws, ...
  )
  p2 <- pp_check(
    object, type = "resid_qq", newdata = newdata,
    ndraws = ndraws, ...
  )
  p3 <- pp_check(
    object, type = "resid_acf", newdata = newdata,
    ndraws = ndraws, ...
  )
  p4 <- pp_check(
    object, type = "resid_pacf", newdata = newdata,
    ndraws = ndraws, ...
  )
  patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2L, nrow = 2L)
}
