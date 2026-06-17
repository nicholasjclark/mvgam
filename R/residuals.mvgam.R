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
#'   family. For the standard continuous families
#'   (`gaussian`, `student`, `lognormal`, `Gamma`, `beta`)
#'   the per-draw analytic CDF is used so each posterior
#'   parameter draw produces its own residual realisation,
#'   which preserves the parameter uncertainty in the per-draw
#'   matrix. For every other family -- including discrete,
#'   zero-inflated, hurdle, ordinal, beta-binomial and custom
#'   families -- the residuals are computed via the empirical
#'   PIT over [posterior_predict.mvgam()] draws (the DHARMa /
#'   Hartig 2024 approach), which inherits family coverage
#'   from `posterior_predict`.
#'
#' * `"ordinary"` -- the predictive error `y - posterior_predict(y)`
#'   per draw. Matches `type = "ordinary"` in
#'   [brms::residuals.brmsfit()].
#'
#' Residuals are computed conditionally on each posterior draw
#' of the model parameters so the returned object carries the
#' full posterior uncertainty in the residual distribution.
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
#' @details The empirical-PIT path follows the DHARMa approach
#'   (Hartig 2024) of computing residuals directly from the
#'   posterior predictive draws:
#'
#'   * `lower = mean(yrep < y_i)`, `upper = mean(yrep <= y_i)`.
#'   * If `lower == upper` (continuous response, no ties) the
#'     residual is `qnorm(lower)`; otherwise it is
#'     `qnorm(Uniform(lower, upper))`.
#'
#'   This formulation needs no analytic family CDF, so it
#'   inherits the family coverage of
#'   [posterior_predict.mvgam()] -- mixture families
#'   (zero-inflated, hurdle), beta-binomial, tweedie, ordinal
#'   families and custom families are all supported.
#'
#'   The PIT u is clamped to `[.Machine$double.eps,
#'   1 - .Machine$double.eps]` before `qnorm` so the residual
#'   stays finite even when an observation lies in the extreme
#'   floating-point tail of the predictive distribution. This
#'   bound corresponds to `|qnorm(u)| ≈ 8.13`.
#'
#'   *ndraws sensitivity.* The two paths react differently to
#'   the posterior draw count:
#'
#'   * The analytic-CDF path can produce isolated clamp events
#'     on individual posterior draws whose `(mu_d, dpar_d)`
#'     happen to place the observed `y_i` more than ~38 SDs
#'     from the predictive mean -- `pnorm` underflows to exactly
#'     0 or 1 and the clamp triggers. These look like extreme
#'     `±8.13` residuals in the per-draw matrix but are a
#'     property of the posterior, not a misfit signal. They get
#'     proportionally rarer with more draws but never vanish.
#'
#'   * The empirical-PIT path needs enough simulations for the
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
#'     [posterior_predict.mvgam()] output). For continuous
#'     responses (where the empirical PIT does not need
#'     randomization) every row is identical and the matrix
#'     reduces to a single deterministic residual per
#'     observation.
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
#' \donttest{
#' simdat <- sim_mvgam(n_series = 1L, trend_model = AR())
#' mod <- mvgam(
#'   y ~ s(season, bs = "cc"),
#'   trend_formula = ~ AR(),
#'   noncentred = TRUE,
#'   data = simdat$data_train,
#'   chains = 2L,
#'   silent = 2L
#' )
#' resids <- residuals(mod)
#' str(resids)
#' }
#'
#' \dontrun{
#' # Closure-unit families: residuals collapse to the per-unit
#' # grain (one row per site x season), using the per-unit sum
#' # as the summary statistic for the empirical PIT.
#' occ_fit <- mvgam(bf(y ~ elev, p ~ tod), family = occ(),
#'                  data = closure_unit_data)
#' rs <- residuals(occ_fit)
#' nrow(rs) # one row per closure unit
#' # Pair with the per-visit posterior_predict draws when
#' # within-unit prediction detail is needed.
#' yrep_visit <- posterior_predict(occ_fit)
#' dim(yrep_visit) # [ndraws x n_visit]
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

  # Pin draw_ids once so the analytic / empirical / closure-unit
  # paths all see the same posterior subsample. Pinning is
  # essential for `type = "quantile"` on continuous families
  # (analytic PIT needs aligned mu_d, sigma_d) and matters on
  # the closure-unit path too: `posterior_predict.mvgam`'s
  # closure-unit intercept threads `draw_ids` through but
  # silently ignores `ndraws`, so the conversion has to happen
  # here for the requested subsample to take effect.
  if (is.null(draw_ids) && !is.null(ndraws)) {
    total_draws <- posterior::ndraws(
      posterior::as_draws(object$fit)
    )
    if (ndraws < total_draws) {
      draw_ids <- sort(sample.int(total_draws, ndraws))
      ndraws <- NULL
    }
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
  resp <- mvgam_response_name(object)
  y <- as.numeric(d[[resp]])
  pp_args <- c(list(object = object, newdata = newdata,
                     ndraws = ndraws, draw_ids = draw_ids,
                     summary = FALSE), list(...))

  resids <- switch(
    type,
    "quantile" = compute_quantile_residuals(
      object = object, y = y, pp_args = pp_args,
      d = d, draw_ids = draw_ids
    ),
    "ordinary" = {
      yrep <- do.call(posterior_predict, pp_args)
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
  newdata <- newdata %||% mvgam_training_data(object)
  pp_args <- c(
    list(object = object, newdata = newdata,
         draw_ids = draw_ids, ndraws = ndraws),
    list(...)
  )
  yrep_visit <- do.call(posterior_predict, pp_args)
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
        "Per-unit quantile residuals on closure-unit fits ",
        "have coarse PIT support when 'n_rep' is small. ",
        "Interpret QQ-plots against simulated N(0, 1) ",
        "envelopes rather than analytic quantile lines ",
        "when any closure unit has <= 4 visits."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_closure_unit_residuals_low_n_rep"
    )
  }
  switch(
    type,
    "quantile" = compute_quantile_residuals_empirical(
      agg$y_unit, agg$yrep_unit, nrow(agg$yrep_unit)
    ),
    "ordinary" = sweep(
      agg$yrep_unit, 2L, agg$y_unit,
      FUN = function(yh, yi) yi - yh
    )
  )
}


# Internal: continuous-family CDF dispatch table for per-draw
# analytic quantile residuals. Each entry maps a brms family
# string to `function(y_mat, mu_mat, dpars) -> [ndraws x nobs]
# PIT matrix`, where `y_mat` is `y` recycled across draws and
# `dpars` is the list of per-draw distribution parameters
# returned by `compute_family_variance_for_residuals` (we reuse
# the same dpar broadcasting). All other families fall through
# to the empirical-PIT path (Hartig 2024 / DHARMa convention),
# which inherits coverage from `posterior_predict.mvgam`.
#'@noRd
quantile_family_specs <- list(
  gaussian = function(y, mu, dpars) {
    stats::pnorm(y, mean = mu, sd = dpars$sigma)
  },
  student = function(y, mu, dpars) {
    stats::pt((y - mu) / dpars$sigma, df = dpars$nu)
  },
  lognormal = function(y, mu, dpars) {
    stats::plnorm(y, meanlog = log(mu), sdlog = dpars$sigma)
  },
  Gamma = function(y, mu, dpars) {
    stats::pgamma(y, shape = dpars$shape,
                    rate = dpars$shape / mu)
  },
  beta = function(y, mu, dpars) {
    stats::pbeta(y, shape1 = mu * dpars$phi,
                   shape2 = (1 - mu) * dpars$phi)
  }
)


# Internal: top-level dispatcher for `type = "quantile"`.
# Continuous standard families use the analytic per-draw CDF
# (Dunn & Smyth 1996 in its original form); all other families
# use the empirical PIT over `posterior_predict` draws.
#'@noRd
compute_quantile_residuals <- function(object, y, pp_args,
                                         d, draw_ids = NULL) {
  fam <- object$family$family
  spec <- quantile_family_specs[[fam]]
  if (!is.null(spec)) {
    return(compute_quantile_residuals_analytic(
      object = object, y = y, spec = spec,
      pp_args = pp_args, d = d, draw_ids = draw_ids
    ))
  }
  yrep <- do.call(posterior_predict, pp_args)
  compute_quantile_residuals_empirical(y, yrep, nrow(yrep))
}


# Internal: per-draw analytic quantile residuals for continuous
# standard families. `spec(y_mat, mu_mat, dpars) -> PIT matrix`.
# `y` is broadcast across draws; `dpars` are reused from the
# pearson path's helper so the dpar broadcasting logic is shared.
#'@noRd
compute_quantile_residuals_analytic <- function(object, y, spec,
                                                  pp_args, d,
                                                  draw_ids = NULL) {
  mu <- do.call(posterior_epred, pp_args)
  dpars <- residuals_dpars(object, draw_ids = draw_ids,
                            d = d, n_obs = length(y))
  y_mat <- matrix(rep(y, nrow(mu)), nrow = nrow(mu), byrow = TRUE)
  u <- spec(y_mat, mu, dpars)
  # Clip u away from {0, 1} so qnorm never returns +/-Inf for
  # observations deep in the predictive tail. Matches the
  # continuity-corrected empirical-PIT path.
  eps <- .Machine$double.eps
  u <- pmin(pmax(u, eps), 1 - eps)
  resids <- stats::qnorm(u)
  na_obs <- is.na(y)
  if (any(na_obs)) resids[, na_obs] <- NA_real_
  resids
}


# Internal: empirical-PIT quantile residuals (DHARMa / Hartig
# 2024 helper.R::getQuantile convention). Per obs the lower /
# upper PIT bounds are `mean(yrep < y)` and `mean(yrep <= y)`.
# When they coincide (continuous response, no ties) the
# residual is deterministic; otherwise it is randomized
# per draw between the bounds. `qnorm` -> N(0, 1).
#'@noRd
compute_quantile_residuals_empirical <- function(y, yrep,
                                                   ndraws_used) {
  nobs <- length(y)
  # DHARMa::getQuantile formulation: lower / upper are the
  # empirical CDF bounds `mean(yrep < y)` / `mean(yrep <= y)`.
  # When they coincide (continuous response, no ties) the
  # residual is deterministic; otherwise it is randomized per
  # draw between the bounds. Clamping below keeps `qnorm` finite
  # for boundary observations.
  lower <- vapply(seq_len(nobs), function(i) {
    if (is.na(y[i])) return(NA_real_)
    mean(yrep[, i] < y[i], na.rm = TRUE)
  }, numeric(1L))
  upper <- vapply(seq_len(nobs), function(i) {
    if (is.na(y[i])) return(NA_real_)
    mean(yrep[, i] <= y[i], na.rm = TRUE)
  }, numeric(1L))
  needs_rand <- !is.na(lower) & lower != upper
  eps <- .Machine$double.eps
  resids <- matrix(NA_real_, nrow = ndraws_used, ncol = nobs)
  for (i in seq_len(nobs)) {
    if (is.na(lower[i])) next
    u <- if (needs_rand[i]) {
      stats::runif(ndraws_used, min = lower[i], max = upper[i])
    } else {
      rep(lower[i], ndraws_used)
    }
    u <- pmin(pmax(u, eps), 1 - eps)
    resids[, i] <- stats::qnorm(u)
  }
  resids
}


# Internal: shared per-draw dpar extraction + obs-level
# broadcasting. Used by both the analytic-quantile path and
# the pearson path so dpar shape logic stays in one place.
#'@noRd
residuals_dpars <- function(object, ndraws = NULL, draw_ids = NULL,
                            d, n_obs) {
  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)
  draw_idx <- if (!is.null(draw_ids)) {
    draw_ids
  } else if (!is.null(ndraws) && ndraws < total_draws) {
    sample.int(total_draws, ndraws)
  } else {
    seq_len(total_draws)
  }
  per_series <- extract_family_pars_for_draws(
    object, draws_mat, draw_idx
  )
  lapply(per_series, function(mat) {
    nc <- ncol(mat)
    if (nc == 1L) {
      matrix(rep(as.numeric(mat), n_obs),
              nrow = nrow(mat), byrow = FALSE)
    } else if (nc == n_obs) {
      mat
    } else {
      reps <- n_obs %/% nc
      mat[, rep(seq_len(nc), each = reps), drop = FALSE]
    }
  })
}




# Internal: shape the residual draws matrix into the form
# requested by `summary` / `robust` / `probs`. Mirrors the
# brms / mvgam summary convention so downstream consumers
# (augment.mvgam, plot.mvgam_resids, etc.) see a stable shape.
#'@noRd
residuals_finalise <- function(resids, summary, robust, probs) {
  if (!summary) return(resids)
  Qlower <- apply(resids, 2L, stats::quantile,
                    probs = min(probs), na.rm = TRUE)
  Qupper <- apply(resids, 2L, stats::quantile,
                    probs = max(probs), na.rm = TRUE)
  if (robust) {
    estimates <- apply(resids, 2L, stats::median, na.rm = TRUE)
    errors <- apply(
      resids, 2L,
      function(col) {
        m <- stats::median(col, na.rm = TRUE)
        stats::median(abs(col - m), na.rm = TRUE)
      }
    )
  } else {
    estimates <- apply(resids, 2L, mean, na.rm = TRUE)
    errors <- apply(resids, 2L, stats::sd, na.rm = TRUE)
  }
  out <- cbind(estimates, errors, Qlower, Qupper)
  colnames(out) <- c(
    "Estimate", "Est.Error",
    paste0("Q", 100 * min(probs)),
    paste0("Q", 100 * max(probs))
  )
  out
}
