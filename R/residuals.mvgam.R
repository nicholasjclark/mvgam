#' Posterior draws of residuals from \pkg{mvgam} models
#'
#' @description
#' Posterior draws of the residuals of a fitted `mvgam` model, of two
#' types:
#'
#' * `"quantile"` (default): randomised quantile residuals (Dunn and
#'   Smyth 1996), also called Dunn-Smyth or PIT residuals. Each
#'   observation passes through the distribution function of its
#'   predictive distribution, then through the standard normal quantile
#'   function. Under a correctly specified model the residuals are
#'   independent N(0, 1) draws for any response family.
#' * `"ordinary"`: the predictive error, `y` minus a
#'   [posterior_predict.mvgam()] draw, per draw.
#'
#' In sample, both types compare each observation with predictions that
#' condition on the latent trend state the model inferred at that time,
#' the state [hindcast.mvgam()] returns. For `newdata` the fit has no
#' such state. The predictions then use the covariate part of the
#' model, without the trend's innovations. Passing
#' `process_error = TRUE` adds them. [pp_check.mvgam()] and
#' [predictive_error.mvgam()] use the same predictions.
#'
#' Pearson residuals are not supported. A Pearson residual of a count or
#' a bounded response is skewed and discrete even under a correctly
#' specified model.
#'
#' @section Multivariate families (`mvn()`, `mvt()`, `diri()`):
#'   Each row of a unit gets its own residual. Residuals of `mvn()` and
#'   `mvt()` condition on the latent factor scores and test the
#'   observation model. They stay N(0, 1) when the fit has too few
#'   factors. [active_factors()] and [residual_cor.mvgam()] judge the
#'   number of factors.
#'
#'   A `diri()` residual is the exact marginal of one component,
#'   `Beta(alpha_j, alpha_0 - alpha_j)`. Each is N(0, 1) on its own.
#'   Because the shares of a unit sum to one, the residuals of a unit of
#'   `K` species are negatively dependent, with `K - 1` degrees of
#'   freedom. With `K = 2` the two residuals are exactly antithetic.
#'   Judge a QQ-plot or an ACF at the unit grain.
#'
#' @section Closure-unit families (`nmix()`, `occ()`):
#'   Visits within a closure unit (site by season) share the latent state,
#'   `N` for `nmix()` and `z` for `occ()`. The visits are independent
#'   given that state, but dependent marginally. Because residuals per
#'   visit would carry that dependence into every diagnostic,
#'   `residuals()` returns one residual per closure unit. The observed
#'   counts or detections are summed within each unit. The visit-level
#'   [posterior_predict.mvgam()] draws are summed the same way. Columns
#'   (`summary = FALSE`) or rows (`summary = TRUE`) are labelled by the
#'   unit's grouping values joined with `_`.
#'
#'   Each draw simulates the latent state from `lambda` or `psi` without
#'   conditioning on the unit's detection history. The residuals then
#'   test the model across the population of units.
#'   `predict(type = "latent_state")` gives the state given each unit's
#'   detection history.
#'
#'   Under `occ()` a unit sum takes values in `0, ..., n_rep`. With few
#'   visits the empirical PIT has coarse support. QQ-plot tails then look
#'   heavier than N(0, 1) under a correctly specified model. A
#'   once-per-session warning is raised when any unit has 4 or fewer visits.
#'   Compare QQ-plots with envelopes simulated from N(0, 1) in that case.
#'
#' @inheritParams predict.mvgam
#' @param object A fitted `mvgam` object.
#' @param newdata Optional `data.frame` to compute residuals on. Defaults
#'   to the training data.
#' @param type Character. Either `"quantile"` (default) or
#'   `"ordinary"`.
#' @param summary Logical. `TRUE` (the default) returns a summary per
#'   observation. `FALSE` returns the residual draws.
#' @param probs Two probabilities for the lower and upper quantile
#'   columns of the summary. Default `c(0.025, 0.975)`, a 95% interval.
#' @param ... Further arguments of [posterior_predict.mvgam()], such as
#'   `resp` or `re_formula`.
#'
#' @details Each draw places an observation in a PIT interval
#'   `[lower, upper]`. The residual is `qnorm(u)` for a `u` drawn
#'   uniformly from that interval. Two routes give the interval.
#'
#'   The analytic route evaluates the family's distribution function at
#'   each draw's parameters. A discrete family gives the interval of the
#'   observed atom, `[F(y - 1), F(y)]`. A continuous family gives the
#'   point `F(y)`. A `tweedie()` zero gives `[0, F(0)]`. Censored rows
#'   span their censoring interval. A truncated model renormalises over
#'   its truncation bounds. [log_lik.mvgam()] takes its density from the
#'   same parameterisation. The route covers `gaussian`, `student`,
#'   `asym_laplace`, `lognormal`, `gamma`, `weibull`, `exponential`,
#'   `beta`, `bernoulli`, `binomial`, `beta_binomial`, `poisson`,
#'   `negbinomial`, `geometric`, `com_binomial()`, `beta_nb()`,
#'   `tweedie()`, the ordinal families, and the multivariate `mvn()`,
#'   `mvt()`, `diri()`, `multi()` and `categ()`. A composition family uses
#'   the marginal of one component: a Beta for `diri()`, a Binomial
#'   against the site's own total for `multi()` and a Bernoulli for
#'   `categ()`.
#'
#'   Every other family takes the empirical route of DHARMa (Hartig 2024),
#'   among them the zero-inflated, hurdle and mixture families, `occ()`
#'   and `nmix()`. The bounds are `mean(yrep < y)` and `mean(yrep <= y)`
#'   over the pooled [posterior_predict.mvgam()] draws. The pooled
#'   predictive carries the posterior uncertainty in the latent state.
#'   Its residuals are narrower than N(0, 1). A QQ-plot of them is
#'   conservative.
#'
#'   The PIT value is clamped to `[eps, 1 - eps]`, with
#'   `eps = .Machine$double.eps`, to keep the residual finite. The clamp
#'   bounds a residual at about +/-8.13. On the analytic route one draw
#'   can place an observation beyond that bound. Such isolated values
#'   describe that draw. A misfit shifts the residual across most draws.
#'   The empirical route reaches the clamp whenever an observation lies
#'   outside every simulated value. With few draws that happens often.
#'   Use `ndraws >= 500` there, the default of the residual histograms
#'   and ribbons of [pp_check.mvgam()].
#'
#' @return With `summary = FALSE`, an `[ndraws, nobs]` matrix of residual
#'   draws. A continuous family on the empirical route has one interval
#'   per observation. Its rows are then identical. With `summary = TRUE`,
#'   an `[nobs, 2 + length(probs)]` matrix. Its columns are `Estimate`
#'   (the mean, or the median with `robust = TRUE`), `Est.Error` (the
#'   standard deviation, or the MAD) and one quantile per `probs`.
#'
#' @references
#' Dunn, P. K. and Smyth, G. K. (1996). Randomized quantile
#'   residuals. *Journal of Computational and Graphical
#'   Statistics*, 5(3), 236-244.
#'
#' Hartig, F. (2024). DHARMa: Residual diagnostics for
#'   hierarchical (multi-level / mixed) regression models.
#'   R package. \url{https://CRAN.R-project.org/package=DHARMa}
#'
#' @seealso [augment.mvgam()], [posterior_predict.mvgam()],
#'   [pp_check.mvgam()]
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
#' # Quantile residuals summarised per observation
#' r <- residuals(mod)
#' head(r)
#'
#' # One draw of the residuals should look like N(0, 1) draws
#' r1 <- residuals(mod, summary = FALSE, draw_ids = 1)
#' qqnorm(r1[1, ])
#' qqline(r1[1, ])
#'
#' # Next: plot(mod, type = "residuals") draws the QQ-plot, ACF and
#' # residuals against fitted values over many draws. A series observed
#' # at unevenly spaced times gets a semivariogram in place of the ACF
#' }
#'
#' @family model checking
#' @export
residuals.mvgam <- function(object,
                              newdata = NULL,
                              type = c("quantile", "ordinary"),
                              ndraws = NULL,
                              draw_ids = NULL,
                              summary = TRUE,
                              robust = FALSE,
                              probs = c(0.025, 0.975),
                              ...) {
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "residuals")
  type <- match.arg(type)
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, len = 2L,
                              lower = 0, upper = 1,
                              any.missing = FALSE,
                              unique = TRUE)

  # A multivariate fit without 'resp' returns a named list, one
  # residual matrix per response. Each element comes from a call back
  # into this method with 'resp' set.
  dots <- list(...)
  resp <- dots$resp
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) return(fan)

  # The analytic PIT takes the predictor and the distributional
  # parameters in separate calls. Fixed draw indices give every call
  # the same posterior iterations.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  if (is_closure_unit_family(object$family)) {
    resids <- compute_closure_unit_residuals(
      object   = object,
      newdata  = newdata,
      type     = type,
      draw_ids = draw_ids,
      ndraws   = ndraws,
      ...
    )
    return(residuals_finalise(resids, summary = summary,
                                robust = robust, probs = probs))
  }

  d <- prediction_frame(object, newdata)
  y <- as.numeric(d[[response_column(object, resp)]])
  pp_args <- c(list(object = object, newdata = newdata,
                     ndraws = ndraws, draw_ids = draw_ids), dots)

  resids <- switch(
    type,
    "quantile" = compute_quantile_residuals(
      object = object, y = y, pp_args = pp_args,
      d = d, draw_ids = draw_ids, resp = resp
    ),
    "ordinary" = -sweep(
      do.call(posterior_predict, residuals_pred_args(pp_args, resp)),
      2L, y
    )
  )
  residuals_finalise(resids, summary = summary,
                       robust = robust, probs = probs)
}


#' Residuals of a closure-unit fit at the unit grain
#'
#' Visits within a closure unit share the latent state. They are
#' independent given the state and dependent marginally (MacKenzie et
#' al. 2002; Royle 2004). `aggregate_closure_unit_visits()` sums the
#' observations and the per-visit predictive draws to unit totals.
#' Quantile residuals take the empirical PIT of those totals. Their
#' support is coarse when a unit has few visits. A once-per-session
#' warning reports that case.
#'
#' @param object Fitted `mvgam` object with an `occ()` or `nmix()`
#'   family
#' @param newdata Data frame of new data, or `NULL` for the training
#'   data
#' @param type Character residual type, `"quantile"` or `"ordinary"`
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @param ndraws Integer number of draws, or `NULL`
#' @param ... Further arguments to `posterior_predict()`
#' @return Numeric matrix with dimensions `[ndraws, N_unit]`
#' @noRd
compute_closure_unit_residuals <- function(object, newdata, type,
                                             draw_ids, ndraws, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_choice(type, c("quantile", "ordinary"))
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  # Recorded before the training frame replaces a NULL 'newdata'.
  in_sample <- is.null(newdata)
  newdata <- prediction_frame(object, newdata)
  pp_args <- c(
    list(object = object, newdata = newdata,
         draw_ids = draw_ids, ndraws = ndraws),
    list(...)
  )
  yrep_visit <- do.call(
    posterior_predict,
    diagnostic_surface_args(pp_args, in_sample)
  )
  agg <- aggregate_closure_unit_visits(
    object, newdata = newdata, yrep_visit = yrep_visit
  )
  # An occupancy unit total takes values in 0, ..., n_rep. With few
  # visits the randomised PIT has coarse support (Dunn and Smyth 1996,
  # section 3).
  if (type == "quantile" && any(agg$arrays$n_rep <= 4L)) {
    warn_once(
      c(
        paste0("Quantile residuals of a closure-unit fit have coarse ",
               "PIT support when a unit has 4 or fewer visits."),
        i = "Compare QQ-plots with envelopes simulated from N(0, 1)."
      ),
      "mvgam_closure_unit_residuals_low_n_rep"
    )
  }
  switch(
    type,
    "quantile" = compute_quantile_residuals_empirical(
      agg$y_unit, agg$yrep_unit
    ),
    "ordinary" = -sweep(agg$yrep_unit, 2L, agg$y_unit)
  )
}


#' Prediction arguments for a residual
#'
#' `diagnostic_surface_args()` names the prediction surface. The
#' response the multivariate fan-out scoped the call to is kept.
#'
#' @param pp_args Named list of prediction arguments
#' @param resp Character response name, or `NULL`
#' @return Named list of prediction arguments
#' @noRd
residuals_pred_args <- function(pp_args, resp) {
  checkmate::assert_list(pp_args)
  checkmate::assert_string(resp, null.ok = TRUE)
  args <- diagnostic_surface_args(pp_args, is.null(pp_args$newdata))
  if (!"resp" %in% names(args)) {
    args$resp <- resp
  }
  args
}


#' Whether a family's quantile residuals use analytic PIT bounds
#'
#' An ordinal family takes its bounds from its category
#' probabilities. Any other family needs a distribution spec. A
#' family with neither takes the empirical PIT over
#' `posterior_predict()` draws.
#'
#' @param family_obj A response's family object
#' @return Logical scalar
#' @noRd
has_analytic_pit <- function(family_obj) {
  is_ordinal_family(family_obj) ||
    family_has_dist_spec(resolve_family_name(family_obj), family_obj$link)
}


#' The per-draw PIT bounds an analytic family supplies
#'
#' `family_dist_spec()` defines how each family is parametrised.
#' `log_lik()` takes the density from it. The `cens()` and `trunc()`
#' terms take the distribution function from it. Residuals use the same
#' spec to keep one table of CDFs. A discrete family has an atom at each
#' observed value. The Dunn-Smyth construction then randomises over
#' `[F(y - 1), F(y)]`. A continuous family gives `F(y)` for both
#' bounds. An ordinal family takes its bounds from the category
#' probabilities of `ordinal_category_probs()`.
#'
#' @param object Fitted `mvgam` object
#' @param y Numeric vector of observed responses
#' @param pp_args Named list of prediction arguments
#' @param d Data frame of the prediction rows
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @param resp Character response name, or `NULL`
#' @return List holding the `lower` and `upper` bound matrices, or
#'   `NULL` for a family without a distribution spec. `NULL` sends the
#'   family to the empirical PIT.
#' @details `compute_quantile_residuals()`, its one caller, checks the
#'   arguments.
#' @noRd
analytic_pit_bounds <- function(object, y, pp_args, d,
                                draw_ids = NULL, resp = NULL) {
  family_obj <- model_families(object, resp)
  family_name <- resolve_family_name(family_obj)
  # Checked before the predictor is computed, which the empirical
  # route does not use.
  if (!has_analytic_pit(family_obj)) {
    return(NULL)
  }
  # The spec applies the family's inverse link itself. It takes the
  # predictor on the link scale, as in `log_lik()`.
  linpred <- do.call(
    posterior_linpred,
    c(residuals_pred_args(pp_args, resp), list(transform = FALSE))
  )
  # `residuals.mvgam()` splits a multivariate fit by response first,
  # leaving one response and one predictor matrix.
  checkmate::assert_matrix(linpred)
  if (is_ordinal_family(family_obj)) {
    return(ordinal_pit_bounds(
      ordinal_category_probs(
        object, linpred, family_obj, draw_ids = draw_ids, newdata = d,
        resp = resp, re_formula = pp_args$re_formula,
        allow_new_levels = pp_args$allow_new_levels %||% FALSE,
        sample_new_levels = pp_args$sample_new_levels %||% "uncertainty"
      ),
      y
    ))
  }
  # A multi-response family holds per-row parameters: a scale and
  # degrees of freedom, or a softmax probability and the unit of each
  # row. `log_lik()` takes them from the same helper.
  family_pars <- if (is_multi_response_family(family_obj)) {
    mv_response_family_pars(
      object, d, linpred, family_obj, family_name, draw_ids
    )
  } else {
    resolve_family_pars(
      object,
      dpar_names = get_family_dpars(family_name),
      ndraws = nrow(linpred),
      nobs = ncol(linpred),
      draw_ids = draw_ids,
      newdata = d,
      resp = resp,
      # The predictor above used the user's group-level settings. The
      # parameters take the same settings.
      re_formula = pp_args$re_formula,
      allow_new_levels = pp_args$allow_new_levels %||% FALSE,
      sample_new_levels = pp_args$sample_new_levels %||% "uncertainty"
    )
  }
  spec <- family_dist_spec(
    family_name,
    family_obj$link,
    linpred,
    family_pars,
    extract_trials_for_family(object, family_obj, d, resp)
  )
  # Each observation occupies the interval `(q_lo, q_hi]` of the
  # response. A count's interval spans its atom, `(y - 1, y]`. A
  # family that is continuous except for a point mass, as a Tweedie is
  # at zero, gives an observation on the mass the interval below it.
  # Treated as continuous there, every such observation would take the
  # PIT value F(0) exactly.
  discrete <- family_uses_integers(family_name)
  q_lo <- if (discrete) y - 1 else y
  if (!is.null(spec$atom)) {
    q_lo[!is.na(y) & y == spec$atom] <- -Inf
  }
  censored_pit_bounds(
    spec, linpred, q_lo, y,
    response_addition_terms(object, d, resp, family_obj), discrete
  )
}


#' PIT bounds of each observation's interval of the response
#'
#' A censored value lies anywhere in its censoring interval, as the
#' likelihood scores it. A right-censored row spans
#' `(y, Inf]`, a left-censored row `(-Inf, y]` and an interval row
#' `(y, rcens]`. A truncated model's distribution function is
#' renormalised over the window `truncation_window()` gives.
#'
#' @param spec Distribution spec from `family_dist_spec()`
#' @param linpred Numeric linear predictor matrix
#' @param q_lo,q_hi Each observation's interval of the response
#' @param terms Addition-term values from `addition_term_values()`
#' @param discrete Logical flag for an integer-valued family
#' @return List holding the `[ndraws x nobs]` `lower` and `upper` bound
#'   matrices
#' @noRd
censored_pit_bounds <- function(spec, linpred, q_lo, q_hi, terms,
                                discrete) {
  checkmate::assert_list(spec)
  checkmate::assert_matrix(linpred)
  checkmate::assert_list(terms)
  cens <- terms$cens
  right <- which(cens == 1 | cens == 2)
  q_lo[right] <- q_hi[right]
  q_hi[right] <- Inf
  interval <- which(cens == 2)
  q_hi[interval] <- terms$rcens[interval]
  q_lo[which(cens == -1)] <- -Inf

  cdf <- spec_cdf(spec, linpred)
  window <- truncation_window(terms$lb, terms$ub, ncol(linpred), discrete)
  log_mass <- if (is.null(window)) 0 else {
    log_interval_mass(cdf, window$lo, window$hi)
  }
  window <- window %||% list(lo = -Inf, hi = Inf)
  bound <- function(q) {
    q <- pmin(pmax(q, window$lo), window$hi)
    exp(log_interval_mass(cdf, window$lo, q) - log_mass)
  }
  upper <- bound(q_hi)
  lower <- if (identical(q_lo, q_hi)) upper else bound(q_lo)
  list(lower = lower, upper = upper)
}


#' Randomised quantile residuals of a fit
#'
#' A family with a distribution spec gets the Dunn-Smyth construction,
#' with the PIT evaluated from each draw's own parameters. Every other
#' family takes the empirical PIT over `posterior_predict()` draws. Both
#' use the surface `diagnostic_surface_args()` names, which in sample
#' is the per-draw latent trend the model inferred.
#'
#' @param object Fitted `mvgam` object
#' @param y Numeric vector of observed responses
#' @param pp_args Named list of prediction arguments
#' @param d Data frame of the prediction rows
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @param resp Character response name, or `NULL`
#' @return Numeric matrix of residuals with one column per observation
#' @noRd
compute_quantile_residuals <- function(object, y, pp_args,
                                         d, draw_ids = NULL,
                                         resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(y)
  checkmate::assert_list(pp_args)
  checkmate::assert(
    checkmate::check_data_frame(d),
    checkmate::check_list(d)
  )
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_string(resp, null.ok = TRUE)
  bounds <- analytic_pit_bounds(
    object = object, y = y, pp_args = pp_args, d = d,
    draw_ids = draw_ids, resp = resp
  )
  if (!is.null(bounds)) {
    return(randomised_quantile_residuals(
      bounds$lower, bounds$upper, y
    ))
  }
  yrep <- do.call(
    posterior_predict, residuals_pred_args(pp_args, resp)
  )
  compute_quantile_residuals_empirical(y, yrep)
}


#' Randomised quantile residuals from PIT intervals
#'
#' The analytic and empirical routes both end here. A uniform value
#' drawn from each interval is clamped away from 0 and 1 to keep
#' `qnorm()` finite. `runif()` on a degenerate interval returns its
#' endpoint, making a continuous family the special case of a discrete
#' one.
#'
#' @param lower Numeric matrix of lower PIT bounds, one column per
#'   observation
#' @param upper Numeric matrix of upper PIT bounds, shaped as `lower`
#' @param y Numeric vector of observed responses. A missing value gives
#'   a missing residual column.
#' @return Numeric matrix of residuals shaped as `lower`
#' @noRd
randomised_quantile_residuals <- function(lower, upper, y) {
  checkmate::assert_matrix(lower)
  checkmate::assert_matrix(upper)
  checkmate::assert_numeric(y)
  lo <- pmin(lower, upper)
  hi <- pmax(lower, upper)
  ok <- is.finite(lo) & is.finite(hi)
  u <- rep(NA_real_, length(lo))
  u[ok] <- stats::runif(sum(ok), min = lo[ok], max = hi[ok])
  eps <- .Machine$double.eps
  resids <- matrix(
    stats::qnorm(pmin(pmax(u, eps), 1 - eps)),
    nrow = nrow(lower)
  )
  na_obs <- is.na(y)
  if (any(na_obs)) resids[, na_obs] <- NA_real_
  resids
}


#' Empirical-PIT quantile residuals
#'
#' The PIT bounds of each observation are `mean(yrep < y)` and
#' `mean(yrep <= y)`, pooled over draws, following the `getQuantile()`
#' convention of DHARMa (Hartig 2024). One interval serves every draw.
#' A family without an analytic distribution function takes this
#' route. Its residuals have less spread than the per-draw construction
#' gives. The pooled predictive absorbs the posterior uncertainty in
#' the latent state that the conditional CDF holds fixed.
#'
#' @param y Numeric vector of observed responses
#' @param yrep Numeric matrix of predictive draws, one column per
#'   observation
#' @return Numeric matrix of residuals shaped as `yrep`
#' @noRd
compute_quantile_residuals_empirical <- function(y, yrep) {
  checkmate::assert_numeric(y)
  checkmate::assert_matrix(yrep, ncols = length(y))
  nobs <- length(y)
  bound <- function(cmp) {
    vapply(seq_len(nobs), function(i) {
      if (is.na(y[i])) return(NA_real_)
      mean(cmp(yrep[, i], y[i]), na.rm = TRUE)
    }, numeric(1L))
  }
  randomised_quantile_residuals(
    data2draws(bound(`<`), dim(yrep)),
    data2draws(bound(`<=`), dim(yrep)),
    y
  )
}



#' Residual draws returned as draws or as a summary
#'
#' The summary matches what `mvgam_post_summary()` gives every other
#' accessor. With `robust = TRUE` the spread is `stats::mad()`, which
#' includes the 1.4826 consistency constant. A column with any missing
#' draw gets a missing summary.
#'
#' @param resids Numeric matrix of residual draws
#' @param summary Logical flag to summarise the draws
#' @param robust Logical flag to report the median and MAD
#' @param probs Numeric vector of quantile probabilities
#' @return `resids`, or a summary matrix with one row per observation
#' @noRd
residuals_finalise <- function(resids, summary, robust, probs) {
  checkmate::assert_matrix(resids)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, lower = 0, upper = 1)
  if (!summary) return(resids)
  mvgam_post_summary(resids, robust = robust, probs = probs)
}
