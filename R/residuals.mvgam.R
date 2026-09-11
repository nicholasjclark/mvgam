#' Posterior draws of residuals from \pkg{mvgam} models
#'
#' Extracts posterior draws of residuals for a fitted `mvgam`
#' model, with two options for residual type:
#'
#' * `"quantile"` (default) -- randomized quantile residuals
#'   (Dunn & Smyth 1996), often also called Dunn-Smyth or PIT
#'   residuals. Each observation is transformed through the
#'   cumulative distribution function of its predictive
#'   distribution and then through the standard-normal quantile,
#'   so under a correctly-specified model the residuals are
#'   independent N(0, 1) draws regardless of the response
#'   family. The transform is evaluated **per draw**, against
#'   that draw's own parameters, which is what makes the result
#'   standard normal: a PIT taken against the posterior
#'   predictive pooled over draws absorbs the uncertainty in the
#'   latent state and comes back too narrow.
#'
#'   A family \pkg{mvgam} holds a distribution function for is
#'   transformed through that function directly. This covers the
#'   single-parameter and location-scale families, the counts,
#'   `beta` and `beta_binomial`, `com_binomial()` and `tweedie()`,
#'   and the multivariate `mvn()`, `mvt()` and `diri()`. A discrete
#'   family randomises within the interval its atom occupies,
#'   `[F(y - 1), F(y)]`; a continuous one has no atom and the
#'   interval collapses to `F(y)`.
#'
#'   The mixtures and the closure-unit families -- zero-inflated,
#'   hurdle, ordinal, `mixture()`, `occ()`, `nmix()`, `multi()` and
#'   `categ()` -- have no single distribution function to name, and
#'   take an empirical PIT over [posterior_predict.mvgam()] draws
#'   instead (the DHARMa / Hartig 2024 approach). The `Details`
#'   below give what that route costs.
#'
#' * `"ordinary"` -- the predictive error `y - posterior_predict(y)`
#'   per draw. Matches `type = "ordinary"` in
#'   [brms::residuals.brmsfit()].
#'
#' Residuals are computed conditionally on each posterior draw
#' of the model parameters so the returned object carries the
#' full posterior uncertainty in the residual distribution.
#'
#' Both types compare a prediction against the observation that was
#' actually recorded, so in sample they read the latent trend state
#' the model inferred at that time, the state [hindcast.mvgam()]
#' returns. Given `newdata` the fit never saw there is no such state,
#' and the prediction integrates over the trend dynamics instead,
#' which widens the residual as it should. [pp_check.mvgam()] and
#' [predictive_error.mvgam()] follow the same rule.
#'
#' Pearson residuals are not provided. In a state-space model
#' with a stochastic trend the posterior absorbs most of the
#' low-frequency residual structure, so the per-observation
#' variance denominator standardises against the wrong
#' quantity and Pearson residuals end up systematically too
#' tight. Use `"quantile"` instead -- it is the correct
#' diagnostic for this model class and is N(0, 1) under a
#' correctly-specified model regardless of family.
#'
#' @section Multivariate families (`mvn()`, `mvt()`, `diri()`):
#'   These share the closure-unit data layout but not its grain:
#'   each row of a unit gets its own residual, because each row
#'   has its own distribution function. What that residual can be
#'   asked differs by family.
#'
#'   `mvn()` and `mvt()` are row-wise conditional on the latent
#'   factor scores, so their residuals test the observation model
#'   rather than the factor structure. Too few factors still
#'   leaves them N(0, 1), because the fitted scores absorb the
#'   cross-species correlation a residual diagnostic would
#'   otherwise reveal; use [residual_cor.mvgam()] to ask whether
#'   the factor dimension is adequate.
#'
#'   `diri()`'s residual is the exact marginal of one component,
#'   `Beta(alpha_j, alpha_0 - alpha_j)`, so it answers how
#'   surprising that species' share is at that site. Each is
#'   standard normal on its own, but a unit's shares sum to one,
#'   so the rows of a unit are negatively dependent by
#'   construction and a unit of `K` species carries `K - 1`
#'   degrees of freedom. With `K = 2` the two residuals are
#'   exactly antithetic. Read a QQ-plot or an ACF at the unit
#'   grain rather than treating the rows of a unit as independent
#'   draws.
#'
#' @section Closure-unit families (`nmix()`, `occ()`):
#'   Closure-unit observation families treat the closure unit
#'   (site x season) as the conditionally iid block: visits
#'   within a unit share the latent state (`N_g` for nmix,
#'   `z_g` for occ) and are only marginally independent.
#'   Per-visit residuals would carry an apparent within-unit
#'   correlation and bias every standard diagnostic (ACF, QQ,
#'   residual-vs-fitted), so `residuals()` returns one residual
#'   per closure unit:
#'
#'   * The observed response is aggregated per unit by `sum()`.
#'     For nmix this is the unit's total observed count (the
#'     marginal sufficient statistic under fixed `N_g`); for
#'     occ this is the per-unit detection count (the sufficient
#'     statistic for the per-unit Bernoulli marginal under
#'     fixed `z_g`).
#'   * The per-visit posterior predictive draws are aggregated
#'     the same way before the empirical PIT (`type = "quantile"`)
#'     or the predictive error (`type = "ordinary"`) is computed.
#'
#'   The returned matrix is shaped `[ndraws x N_unit]`
#'   (`summary = FALSE`) or `[N_unit x E]` (`summary = TRUE`),
#'   with rows / columns labelled by the unit IDs from
#'   `build_closure_unit_arrays()`.
#'
#'   The PIT comparison is against the **marginal** posterior
#'   predictive: each draw re-simulates the latent state
#'   (`z_g` for occ, `N_g` for nmix) from `psi_g` / `lambda_g`
#'   per draw, not conditional on the observed detection
#'   history for that unit. This is the DHARMa / flocker / ubms
#'   convention and tests model adequacy at the population
#'   level rather than site-level prediction accuracy; see
#'   `posterior_occupancy(conditional = TRUE)` and
#'   `posterior_latent_N(conditional = TRUE)` when the
#'   conditional state posterior is the quantity of interest.
#'
#'   The empirical PIT randomises uniformly across the discrete
#'   support, so per-unit residuals on fits with small `n_rep`
#'   (~ 2-4 visits) have heavier QQ-tails than analytic
#'   `qnorm(N(0, 1))` lines under a correctly-specified model
#'   (Dunn & Smyth 1996 §3; Hartig 2024 DHARMa vignette). A
#'   one-time warning fires when any closure unit has 4 or
#'   fewer visits; interpret QQ-plots against simulated N(0, 1)
#'   envelopes rather than the analytic line in that regime.
#'
#' @param object An object of class `mvgam`.
#' @param newdata Optional `data.frame` to compute residuals on
#'   (defaults to the training data).
#' @param type Character. Either `"quantile"` (default) or
#'   `"ordinary"`.
#' @param ndraws Positive integer; subsample this many posterior
#'   draws. `NULL` (the default) uses every draw.
#' @param draw_ids Integer vector selecting specific posterior
#'   draws. Mutually exclusive with `ndraws`.
#' @param summary Logical. If `TRUE` (the default), return a
#'   per-observation summary matrix; if `FALSE`, return the full
#'   `ndraws x nobs` matrix of per-draw residuals (which can be
#'   handed to `DHARMa::createDHARMa()` for further diagnostics).
#' @param robust Logical. If `TRUE`, the per-observation summary
#'   uses the median and MAD instead of the mean and SD.
#' @param probs Length-2 numeric. Quantile probabilities for the
#'   per-observation summary intervals.
#' @param ... Additional arguments forwarded to
#'   [posterior_predict.mvgam()].
#'
#' @details Both routes end at the same construction. Each draw
#'   places the observation in a PIT interval, the residual is
#'   `qnorm(Uniform(lower, upper))` over that interval, and a
#'   degenerate interval returns its endpoint. What separates
#'   them is where the interval comes from.
#'
#'   The analytic route reads the family's own distribution
#'   function at that draw's parameters, so the interval is
#'   `[F(y_i - 1 | theta_d), F(y_i | theta_d)]` for a discrete
#'   family and the point `F(y_i | theta_d)` for a continuous
#'   one. The same parameterisation supplies the density
#'   [log_lik.mvgam()] reads, so the residual and the likelihood
#'   cannot disagree about what a family is.
#'
#'   The empirical route follows DHARMa (Hartig 2024): `lower`
#'   and `upper` are `mean(yrep < y_i)` and `mean(yrep <= y_i)`
#'   over the pooled draws. Pooling is what makes it work
#'   without a family CDF, and also what costs it: the pooled
#'   predictive carries the posterior uncertainty in the latent
#'   state, so the interval sits closer to 0.5 than the
#'   conditional one and the residuals come back narrower than
#'   standard normal. Read a QQ-plot from this route as
#'   conservative.
#'
#'   The PIT u is clamped to `[.Machine$double.eps,
#'   1 - .Machine$double.eps]` before `qnorm` so the residual
#'   stays finite even when an observation lies in the extreme
#'   floating-point tail of the predictive distribution. This
#'   bound corresponds to `|qnorm(u)| ≈ 8.13`.
#'
#'   *ndraws sensitivity.* The two routes react differently to
#'   the posterior draw count:
#'
#'   * The analytic route can produce isolated clamp events
#'     on individual posterior draws whose `(mu_d, dpar_d)`
#'     happen to place the observed `y_i` more than ~38 SDs
#'     from the predictive mean -- `pnorm` underflows to exactly
#'     0 or 1 and the clamp triggers. These look like extreme
#'     `±8.13` residuals in the per-draw matrix but are a
#'     property of the posterior, not a misfit signal. They get
#'     proportionally rarer with more draws but never vanish.
#'
#'   * The empirical route needs enough simulations for the
#'     empirical predictive CDF to cover the data. With small
#'     `ndraws` many observations land outside the simulated
#'     range entirely, so `u` is exactly 0 or 1 deterministically
#'     and the clamp turns every draw for that observation into
#'     `±8.13`. Setting `ndraws >= 500` resolves the empirical
#'     PIT well enough that this artefact disappears in
#'     typical cases.
#'
#'   For diagnostic plotting via [pp_check.mvgam()] with
#'   `type = "resid_*"`, use `ndraws >= 500` (the default for
#'   those types).
#'
#' @return An array of residuals.
#'
#'   * `summary = FALSE`: an `ndraws x nobs` numeric matrix of
#'     per-draw residuals (the same shape as
#'     [posterior_predict.mvgam()] output). A continuous family
#'     on the empirical route has one pooled interval per
#'     observation, so every row is identical and the matrix
#'     reduces to a single deterministic residual per
#'     observation; on the analytic route the interval moves
#'     with the draw and the column carries the posterior
#'     spread.
#'   * `summary = TRUE`: an `nobs x E` numeric matrix where
#'     `E = 2 + length(probs)`. Column `Estimate` is the mean
#'     (or median when `robust = TRUE`); column `Est.Error` is
#'     the standard deviation (or MAD); the remaining columns
#'     are quantile estimates labelled `Q<min*100>` and
#'     `Q<max*100>`.
#'
#' @references
#' Dunn, P. K. and Smyth, G. K. (1996). Randomized quantile
#'   residuals. *Journal of Computational and Graphical
#'   Statistics*, 5(3), 236-244.
#'
#' Hartig, F. (2024). DHARMa: Residual diagnostics for
#'   hierarchical (multi-level / mixed) regression models.
#'   R package.
#'
#' @seealso [augment.mvgam()], [posterior_predict.mvgam()],
#'   [posterior_epred.mvgam()]
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(y ~ s(x),
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Randomised quantile residuals on the response scale.
#' # Summarised columns are Estimate, Est.Error, Q2.5, Q97.5.
#' r <- residuals(mod)
#' head(r)
#' }
#'
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
  type <- match.arg(type)
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1L,
                                null.ok = TRUE)
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, len = 2L,
                              lower = 0, upper = 1,
                              any.missing = FALSE,
                              unique = TRUE)

  # Multivariate fan-out: when no resp was given, return a named
  # list of per-response residual matrices. Shared helper threads
  # the user's call back through this method with resp scoped per
  # response; downstream the body's `resp` argument captures it
  # and the univariate machinery runs unchanged.
  dots <- list(...)
  resp <- dots$resp
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) return(fan)

  # Pin draw_ids once so the analytic / empirical / closure-unit
  # paths all see the same posterior subsample. Pinning is
  # essential for `type = "quantile"` on continuous families
  # (analytic PIT needs aligned mu_d, sigma_d) and matters on
  # the closure-unit path too: `posterior_predict.mvgam`'s
  # closure-unit intercept threads `draw_ids` through but
  # silently ignores `ndraws`, so the conversion has to happen
  # here for the requested subsample to take effect.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  # Closure-unit families (nmix, occ) violate the per-visit
  # exchangeability assumption: visits within a closure unit
  # share the latent state and are only marginally independent.
  # Residuals collapse to the unit grain (one residual per
  # closure unit) using a sufficient summary statistic
  # (per-unit detection / count totals); the empirical-PIT path
  # then operates on the aggregated draws so DHARMa-style
  # diagnostics on the returned matrix carry the correct
  # exchangeability (Hartig 2024 §3; Vehtari et al. 2017 §4.2).
  # Mv-response closure-unit families (mvn, mvt) have one
  # observation per (site, species) row, so residuals at the row
  # grain are conditionally independent given lv. Routes through
  # the standard non-closure-unit residual path, which calls
  # posterior_predict and subtracts y row-wise.
  if (needs_closure_unit_aggregation(object$family)) {
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

  d <- newdata %||% mvgam_training_data(object)
  # `resp` is populated when the mv fan-out scoped this call (or
  # the user supplied it explicitly via `...`); otherwise fall
  # back to the single response on a univariate fit.
  y <- as.numeric(d[[response_column(object, resp)]])
  pp_args <- c(list(object = object, newdata = newdata,
                     ndraws = ndraws, draw_ids = draw_ids,
                     summary = FALSE), dots)

  resids <- switch(
    type,
    "quantile" = compute_quantile_residuals(
      object = object, y = y, pp_args = pp_args,
      d = d, draw_ids = draw_ids, resp = resp
    ),
    "ordinary" = {
      yrep <- do.call(
        posterior_predict,
        diagnostic_surface_args(pp_args, is.null(newdata))
      )
      sweep(yrep, 2L, y, FUN = function(yh, yi) yi - yh)
    }
  )
  residuals_finalise(resids, summary = summary,
                       robust = robust, probs = probs)
}


# Internal: per-closure-unit residuals for closure-unit families
# (nmix, occ). Aggregates the per-visit posterior predictive
# matrix to the unit grain via `aggregate_closure_unit_visits()`
# (the unit total of detections / counts is the sufficient
# summary for the per-unit Bernoulli-binomial / Poisson-binomial
# marginal), then routes the resulting `[ndraws x N_unit]`
# matrix through the same empirical-PIT or ordinary path used
# for any other family. Per-visit residuals are not produced
# because visits within a unit share the latent state and are
# only marginally independent (Royle 2004; MacKenzie et al.
# 2002; flocker_format vignette).
#'@noRd
compute_closure_unit_residuals <- function(object, newdata, type,
                                             draw_ids, ndraws, ...) {
  # Whether the fit saw these rows is settled before the training
  # frame is substituted for them, since afterwards every test on
  # `newdata` answers as though the user had supplied new data.
  in_sample <- is.null(newdata)
  newdata <- newdata %||% mvgam_training_data(object)
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
  # The empirical-PIT randomisation widens the per-residual
  # variance above 1 whenever the discrete support is coarse
  # (Dunn & Smyth 1996 §3; DHARMa vignette "Residuals for
  # discrete distributions"). For closure-unit families that
  # surfaces when n_rep is small: the per-unit sum lives on
  # {0, ..., n_rep}, so QQ-tails can read as heavier than N(0, 1)
  # under a correctly-specified model.
  if (type == "quantile" &&
        !identical(Sys.getenv("TESTTHAT"), "true") &&
        any(agg$arrays$n_rep <= 4L)) {
    rlang::warn(
      paste0(
        "Quantile residuals at the unit grain of a closure-unit fit ",
        "have coarse PIT support when 'n_rep' is small. ",
        "Compare QQ-plots with simulated N(0, 1) envelopes ",
        "when any closure unit has <= 4 visits."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_closure_unit_residuals_low_n_rep"
    )
  }
  switch(
    type,
    "quantile" = compute_quantile_residuals_empirical(
      agg$y_unit, agg$yrep_unit
    ),
    "ordinary" = sweep(
      agg$yrep_unit, 2L, agg$y_unit,
      FUN = function(yh, yi) yi - yh
    )
  )
}


# Internal: prediction arguments for a residual. Names the surface
# through the shared rule, and carries the response the mv fan-out
# scoped this call to; the marginal branch must not drop it.
#'@noRd
residuals_pred_args <- function(pp_args, resp) {
  args <- diagnostic_surface_args(pp_args, is.null(pp_args$newdata))
  if (!"resp" %in% names(args)) {
    args$resp <- resp
  }
  args
}


# Internal: the per-draw PIT bounds an analytic family supplies.
#
# `family_dist_spec()` is the package's one account of how a family
# is parameterised: `log_lik()` reads it for the density and the
# `cens()` / `trunc()` terms read it for the distribution function,
# so a residual reads it too rather than keeping a second table of
# CDFs that can drift from it.
#
# A discrete family carries an atom at each observed value, so the
# Dunn-Smyth construction randomises over `[F(y - 1), F(y)]`. A
# continuous one has no atom, both bounds are `F(y)`, and the
# residual is a deterministic function of the draw. Returns NULL for
# a family the spec does not name, which is what sends that family
# to the empirical PIT.
#'@noRd
analytic_pit_bounds <- function(object, y, pp_args, d,
                                draw_ids = NULL, resp = NULL) {
  family_obj <- get_family_for_resp(object, resp)
  family_name <- resolve_family_name(family_obj)
  # Whether a family has a spec depends on its name alone, so it is
  # settled before the predictor is computed: a family without one
  # takes the empirical route and must not pay for a prediction it
  # will not use.
  if (!family_has_dist_spec(family_name, family_obj$link)) {
    return(NULL)
  }
  # The spec applies the family's own inverse link, so it takes the
  # predictor on the link scale, which is the scale `log_lik()`
  # hands it.
  linpred <- do.call(
    posterior_linpred,
    c(residuals_pred_args(pp_args, resp), list(transform = FALSE))
  )
  # `residuals.mvgam()` fans a multivariate fit out before reaching
  # here, so the response is always named and the prediction is one
  # arm's matrix. Stating that is enough; a second code path to
  # narrow a list would be a branch nothing takes.
  checkmate::assert_matrix(linpred)
  # A multi-response family keeps its parameters somewhere else: a
  # per-row scale and degrees of freedom, or a softmax probability
  # and the units its rows are grouped into. `log_lik()` reads them
  # through the same helper, so the residual and the density it is
  # standardising cannot describe different models.
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
      resp = resp
    )
  }
  spec <- family_dist_spec(
    family_name,
    family_obj$link,
    linpred,
    family_pars,
    extract_trials_for_family(object, family_obj, d)
  )
  upper <- dist_cdf(spec, linpred, y)
  lower <- if (family_uses_integers(family_name)) {
    dist_cdf(spec, linpred, y - 1)
  } else {
    upper
  }
  # A family that is continuous except for a point mass, as a Tweedie
  # is at zero, needs the interval that mass occupies wherever an
  # observation lands on it. Treated as continuous there, every such
  # observation sits at the top of its own interval rather than
  # spread across it.
  if (!is.null(spec$atom)) {
    on_atom <- !is.na(y) & y == spec$atom
    if (any(on_atom)) lower[, on_atom] <- 0
  }
  list(lower = lower, upper = upper)
}


# Internal: top-level dispatcher for `type = "quantile"`. A family
# the distribution spec names gets its PIT evaluated per draw, from
# that draw's own parameters, which is the Dunn-Smyth construction.
# Every other family falls through to the empirical PIT over
# `posterior_predict` draws. Both read the surface
# `diagnostic_surface_args()` names, which in sample is the per-draw
# `trend[t, s]` the model inferred.
#'@noRd
compute_quantile_residuals <- function(object, y, pp_args,
                                         d, draw_ids = NULL,
                                         resp = NULL) {
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


# Internal: the randomised quantile residual itself, given the PIT
# interval each draw places the observation in. Both the analytic
# and the empirical route end here, so the randomisation and the
# clamp that keeps `qnorm` finite are written once.
#
# `runif` on a degenerate interval returns its endpoint, which is
# what makes a continuous family the special case of the discrete
# one rather than a separate path.
#'@noRd
randomised_quantile_residuals <- function(lower, upper, y) {
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


# Internal: empirical-PIT quantile residuals (DHARMa / Hartig
# 2024 helper.R::getQuantile convention). Per obs the lower /
# upper PIT bounds are `mean(yrep < y)` and `mean(yrep <= y)`,
# pooled over draws, so one interval serves every draw. This is
# the route for a family with no analytic distribution function,
# and it is why such a family's residuals carry less spread than
# the per-draw construction: the pooled predictive absorbs the
# posterior uncertainty in the latent state that the conditional
# CDF holds fixed.
#
# The draw count is read from `yrep` rather than taken as an
# argument: both callers passed `nrow()` of the matrix they were
# already passing, so the argument could only ever agree with it or
# be wrong.
#'@noRd
compute_quantile_residuals_empirical <- function(y, yrep) {
  ndraws_used <- nrow(yrep)
  nobs <- length(y)
  bound <- function(cmp) {
    vapply(seq_len(nobs), function(i) {
      if (is.na(y[i])) return(NA_real_)
      mean(cmp(yrep[, i], y[i]), na.rm = TRUE)
    }, numeric(1L))
  }
  as_draw_matrix <- function(v) {
    matrix(rep(v, each = ndraws_used), nrow = ndraws_used)
  }
  randomised_quantile_residuals(
    as_draw_matrix(bound(`<`)),
    as_draw_matrix(bound(`<=`)),
    y
  )
}



# Internal: shape the residual draws matrix into the form
# requested by `summary` / `robust` / `probs`. Mirrors the
# brms / mvgam summary convention so its callers
# (augment.mvgam, plot.mvgam_resids) see a stable shape.
#'@noRd
residuals_finalise <- function(resids, summary, robust, probs) {
  if (!summary) return(resids)
  # The summary every other accessor reports, so a residual and a
  # prediction describe their spread the same way. Under
  # `robust = TRUE` that means `stats::mad()`, which carries the
  # 1.4826 consistency constant a bare median absolute deviation
  # does not, and it calls a column with any missing draw missing
  # rather than summarising the draws that remain.
  mvgam_post_summary(resids, robust = robust, probs = probs)
}
