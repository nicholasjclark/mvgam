#' Draws from the Posterior Distribution of an mvgam Model
#'
#' Compute posterior predictions for mvgam models. Dispatches by `type`
#' to the appropriate prediction surface:
#' [posterior_predict.mvgam()] for `"response"` (default),
#' [posterior_linpred.mvgam()] for `"link"`,
#' [posterior_epred.mvgam()] for `"expected"`, and an internal
#' family-aware helper for `"variance"`.
#'
#' @param object An object of class `mvgam`.
#' @param newdata An optional data.frame containing new predictor values.
#'   If `NULL` (the default), the original training data is used.
#' @param type Character; one of `"response"` (default), `"link"`,
#'   `"expected"`, `"variance"`, `"terms"`, `"latent_N"`, `"detection"`.
#'   See Details.
#' @param process_error Logical. If `FALSE` (the default) the trend is
#'   fixed at its posterior mean and only parameter uncertainty
#'   propagates, treating the latent trend as a nuisance random
#'   effect for scenario-based prediction from the GAM components
#'   alone. If `TRUE`, the trend's stochastic dynamics are integrated
#'   by Monte Carlo. Use [forecast.mvgam()] / [hindcast.mvgam()] for
#'   predictions that respect the temporal dynamics of the latent
#'   trend at specific time points.
#' @param ndraws Positive integer specifying the number of posterior draws
#'   to use. If `NULL` (the default), all draws are used.
#' @param draw_ids Integer vector specifying which draws to use. If `NULL`,
#'   draws are selected based on `ndraws`.
#' @param re_formula Formula for random effects. If `NULL` (the default),
#'   all random effects are included. Use `NA` to exclude all random
#'   effects.
#' @param allow_new_levels Logical. If `TRUE`, allows predictions for new
#'   factor levels not seen during training. Default is `FALSE`.
#' @param sample_new_levels Character specifying how to sample new levels.
#'   Either `"uncertainty"` (default), `"gaussian"`, or `"old_levels"`.
#' @param resp Character specifying which response variable to predict for
#'   multivariate models. If `NULL`, predictions are returned for all
#'   responses.
#' @param summary Logical. If `TRUE` (the default), returns summary
#'   statistics. If `FALSE`, returns the full matrix of posterior draws.
#' @param robust Logical. If `FALSE` (the default), uses mean and standard
#'   deviation for summaries. If `TRUE`, uses median and median absolute
#'   deviation (MAD).
#' @param probs Numeric vector of probabilities for quantile computation.
#'   Default is `c(0.025, 0.975)` for 95% credible intervals. Can be a
#'   single value or multiple values.
#' @param ... Additional arguments forwarded to the underlying
#'   `posterior_predict` / `posterior_epred` / `posterior_linpred` method.
#'
#' @return If `summary = FALSE`, returns a numeric matrix of posterior
#'   draws with dimensions ``\\[ndraws x nobs\\]``. For multivariate fits with
#'   `resp = NULL`, returns a named list of such matrices.
#'
#'   If `summary = TRUE`, returns a matrix with columns:
#'   \itemize{
#'     \item `Estimate`: point estimate (mean or median depending on
#'       `robust`)
#'     \item `Est.Error`: uncertainty estimate (sd or mad depending on
#'       `robust`)
#'     \item `Q*`: quantile columns corresponding to values in `probs`
#'   }
#'
#' @details
#' The `type` argument selects which posterior summary to return:
#' \itemize{
#'   \item `"response"` (default): draws from the posterior predictive
#'     distribution on the outcome scale. Routes through
#'     [posterior_predict.mvgam()].
#'   \item `"link"`: linear predictor draws on the link scale (no
#'     inverse-link applied). Routes through
#'     [posterior_linpred.mvgam()].
#'   \item `"expected"`: posterior expectation E\[Y | X\] on the response
#'     scale. Routes through [posterior_epred.mvgam()].
#'   \item `"variance"`: conditional variance Var\[Y | theta\] per draw,
#'     using family-specific mean-variance formulas. Supported
#'     families: gaussian, student, lognormal, poisson, bernoulli,
#'     binomial, negbinomial, gamma, beta. Other families raise an
#'     informative error. When `process_error = TRUE`, the returned
#'     variance is conditional on both the parameter draw and that
#'     draw's sampled trend innovation, so it reflects observation
#'     noise at the trend-integrated mean rather than pure
#'     observation noise at the posterior-mean trend. Set
#'     `process_error = FALSE` for the latter.
#'   \item `"terms"`: per-term decomposition of the linear predictor.
#'     Not yet ported on this branch; use [posterior_smooths.mvgam()]
#'     for per-smooth draws and [fixef.mvgam()] / [ranef.mvgam()] for
#'     parametric and random-effect components.
#'   \item `"latent_N"`, `"detection"`: N-mixture-only. Not yet
#'     supported (n-mixture port pending).
#' }
#'
#' @seealso [posterior_predict.mvgam()], [posterior_epred.mvgam()],
#'   [posterior_linpred.mvgam()], [fitted.mvgam()],
#'   [posterior_smooths.mvgam()].
#'
#' @examples
#' \dontrun{
#' # Fit a model
#' fit <- mvgam(count ~ s(time), data = data, family = poisson())
#'
#' # Posterior predictive summary on the response scale (default)
#' pred <- predict(fit)
#'
#' # Raw posterior predictive draws
#' draws <- predict(fit, summary = FALSE)
#'
#' # Linear predictor (link scale)
#' lp <- predict(fit, type = "link")
#'
#' # Expected values (response scale)
#' mu <- predict(fit, type = "expected")
#'
#' # Conditional variance Var\[Y|theta\] per draw
#' v <- predict(fit, type = "variance")
#' }
#'
#' @export
predict.mvgam <- function(object,
                          newdata = NULL,
                          type = c("response", "link", "expected",
                                   "variance", "terms", "latent_N",
                                   "detection"),
                          process_error = FALSE,
                          ndraws = NULL,
                          draw_ids = NULL,
                          re_formula = NULL,
                          allow_new_levels = FALSE,
                          sample_new_levels = "uncertainty",
                          resp = NULL,
                          summary = TRUE,
                          robust = FALSE,
                          probs = c(0.025, 0.975),
                          ...) {

  # Input validation
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  type <- match.arg(type)
  checkmate::assert_logical(process_error, len = 1, any.missing = FALSE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_class(re_formula, "formula"),
    checkmate::check_true(is.na(re_formula)),
    checkmate::check_null(re_formula)
  )
  checkmate::assert_logical(allow_new_levels, len = 1, any.missing = FALSE)
  checkmate::assert_choice(
    sample_new_levels,
    choices = c("uncertainty", "gaussian", "old_levels")
  )
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(summary, len = 1, any.missing = FALSE)
  checkmate::assert_logical(robust, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(
    probs,
    lower = 0,
    upper = 1,
    min.len = 1,
    any.missing = FALSE
  )

  # N-mixture-only types: dispatch via the closure-unit
  # extractors in families.R. Both types require an nmix()-family
  # fit; reject early for any other family.
  if (type %in% c("latent_N", "detection")) {
    if (!is_closure_unit_family(object$family)) {
      stop(insight::format_error(c(
        paste0("type = '", type,
               "' is only available for closure-unit families."),
        x = paste0(
          "Family '", resolve_family_name(object$family),
          "' has no latent abundance or detection layer."
        ),
        i = paste0(
          "Refit with family = nmix() to enable type = '",
          type, "'."
        )
      )))
    }
    if (identical(type, "latent_N")) {
      pred <- posterior_latent_N(
        object,
        newdata     = newdata,
        draw_ids    = draw_ids,
        conditional = TRUE
      )
    } else {
      pred <- posterior_detection(
        object,
        newdata  = newdata,
        draw_ids = draw_ids
      )
    }
    if (!summary) return(pred)
    return(summarize_predictions(pred, probs = probs, robust = robust))
  }

  # Per-term decomposition: defer with a clear migration pointer.
  if (type == "terms") {
    stop(insight::format_error(c(
      "type = 'terms' is not implemented on this branch.",
      i = paste0(
        "Use posterior_smooths.mvgam() for per-smooth draws, ",
        "fixef.mvgam() / ranef.mvgam() for parametric and random ",
        "components, and posterior_linpred.mvgam() for the combined ",
        "linear predictor."
      )
    )))
  }

  # Dispatch by type. posterior_predict / posterior_epred /
  # posterior_linpred each handle multivariate (resp = NULL returns
  # a named list of matrices) the same way, so summary downstream
  # treats them uniformly.
  draws <- switch(
    type,
    "response" = posterior_predict(
      object,
      newdata = newdata,
      process_error = process_error,
      ndraws = ndraws,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp,
      ...
    ),
    "link" = posterior_linpred(
      object,
      newdata = newdata,
      transform = FALSE,
      process_error = process_error,
      ndraws = ndraws,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp,
      ...
    ),
    "expected" = posterior_epred(
      object,
      newdata = newdata,
      process_error = process_error,
      ndraws = ndraws,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp,
      ...
    ),
    "variance" = predict_variance(
      object,
      newdata = newdata,
      process_error = process_error,
      ndraws = ndraws,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp
    )
  )

  # Return raw draws if summary not requested
  if (!summary) {
    return(draws)
  }

  # Compute summary statistics. Multivariate paths return a named
  # list of matrices (one per response); summarise each element
  # separately to match brms's output convention.
  if (is.list(draws) && !is.matrix(draws)) {
    return(lapply(draws, summarize_predictions,
                  probs = probs, robust = robust))
  }
  summarize_predictions(draws, probs = probs, robust = robust)
}


#' Posterior conditional variance Var\[Y | theta\] for predict.mvgam
#'
#' Internal helper that computes per-draw conditional variances using
#' family-specific mean-variance formulas. Called only from
#' [predict.mvgam()] when `type = "variance"`.
#'
#' Draw alignment: `posterior_epred()` and the dpar extraction must
#' index the same posterior rows; otherwise `mu` from draw `i` is
#' paired with `sigma` from a different draw and the conditional
#' variance is statistically incoherent. Because `posterior_epred()`
#' subsamples randomly when `ndraws < total_draws`, we compute `mu`
#' over the full posterior and then subsample `mu` and the dpar
#' matrices with the same `draw_idx`. The fix is independent of
#' draw_ids plumbing through `posterior_epred()`.
#'
#' @noRd
predict_variance <- function(object, newdata, process_error, ndraws,
                             re_formula, allow_new_levels,
                             sample_new_levels, resp) {
  # Compute mu over the full posterior so row i of mu_full corresponds
  # to draws_mat row i. Without this, posterior_epred(ndraws=K) would
  # randomly subsample, and dpars (which we extract sequentially)
  # would not align with mu.
  mu_full <- posterior_epred(
    object,
    newdata = newdata,
    process_error = process_error,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # Multivariate guard: we don't yet have the per-response sigma /
  # shape / phi extraction wired for variance. Error clearly with a
  # pointer at the per-response workaround.
  if (is.list(mu_full) && !is.matrix(mu_full)) {
    stop(insight::format_error(c(
      "type = 'variance' is not yet implemented for multivariate fits.",
      i = paste0(
        "Call predict(object, type = 'variance', resp = '<response>') ",
        "for each response separately."
      )
    )))
  }

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)

  if (nrow(mu_full) != total_draws) {
    stop(insight::format_error(c(
      "Internal: posterior_epred row count does not match total draws.",
      x = paste0("Expected ", total_draws, " rows in mu; got ",
                 nrow(mu_full), ".")
    )))
  }

  # Pick the draw subsample once, use it for both mu and dpars so they
  # align. NULL ndraws or ndraws >= total_draws means use everything.
  draw_idx <- if (is.null(ndraws) || ndraws >= total_draws) {
    seq_len(total_draws)
  } else {
    sort(sample.int(total_draws, ndraws))
  }

  mu <- mu_full[draw_idx, , drop = FALSE]
  ndraws_mu <- nrow(mu)
  nobs_mu <- ncol(mu)

  fpars <- extract_family_pars_for_draws(object, draws_mat, draw_idx)

  # broadcast: dpar matrices arrive as [ndraws x n_series] for
  # multivariate (already errored above) or [ndraws x 1] / [ndraws x
  # nobs] for univariate. Expand to [ndraws x nobs] for the variance
  # helper's dim check.
  broadcast <- function(x, n_rows, n_cols) {
    if (is.null(x)) return(NULL)
    checkmate::assert_matrix(x)
    if (nrow(x) != n_rows) {
      stop(insight::format_error(c(
        "Internal: dpar row count does not match mu row count.",
        x = paste0("dpar has ", nrow(x), " rows; mu has ", n_rows, ".")
      )))
    }
    if (ncol(x) == n_cols) return(x)
    if (ncol(x) == 1L) {
      return(matrix(x[, 1L], nrow = n_rows, ncol = n_cols,
                    byrow = FALSE))
    }
    stop(insight::format_error(c(
      "Internal: unexpected dpar column count.",
      x = paste0("Got ", ncol(x), " columns; expected 1 or ",
                 n_cols, ".")
    )))
  }

  sigma_mat <- broadcast(fpars$sigma, ndraws_mu, nobs_mu)
  shape_mat <- broadcast(fpars$shape, ndraws_mu, nobs_mu)
  phi_mat   <- broadcast(fpars$phi,   ndraws_mu, nobs_mu)
  nu_mat    <- broadcast(fpars$nu,    ndraws_mu, nobs_mu)

  family <- object$family
  family_name <- family$family

  # Binomial / beta_binomial need trial counts (per-observation).
  trials_vec <- NULL
  if (family_name %in% c("binomial", "beta_binomial")) {
    trials_vec <- extract_trials_for_family(object, family, newdata)
  }

  compute_family_variance(
    mu = mu,
    family = family,
    sigma = sigma_mat,
    shape = shape_mat,
    phi = phi_mat,
    nu = nu_mat,
    trials = trials_vec
  )
}


#' Summarize Posterior Prediction Draws
#'
#' Computes summary statistics from a matrix of posterior draws following
#'   the brms output format.
#'
#' @param draws Matrix of posterior draws with dimensions ``\\[ndraws x nobs\\]``.
#' @param probs Numeric vector of probabilities for quantile computation.
#' @param robust Logical. If `FALSE`, uses mean and sd. If `TRUE`, uses
#'   median and mad.
#'
#' @return Matrix with columns for point estimate, uncertainty, and
#'   quantiles.
#'
#' @noRd
summarize_predictions <- function(draws, probs, robust) {
  checkmate::assert_matrix(
    draws,
    mode = "numeric",
    min.rows = 1,
    min.cols = 1
  )
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1)
  checkmate::assert_logical(robust, len = 1)

  # Compute point estimates and uncertainty
  if (robust) {
    estimate <- apply(draws, 2, stats::median)
    est_error <- apply(draws, 2, stats::mad)
  } else {
    estimate <- colMeans(draws)
    est_error <- apply(draws, 2, stats::sd)
  }

  # Compute quantiles
  quantiles <- apply(draws, 2, stats::quantile, probs = probs)

  # Reshape vector to matrix when single quantile requested
  if (length(probs) == 1) {
    quantiles <- matrix(quantiles, nrow = 1)
  }

  # Create quantile column names following brms convention (Q2.5, Q97.5)
  quantile_names <- paste0("Q", probs * 100)

  # Combine into output matrix
  out <- cbind(
    Estimate = estimate,
    Est.Error = est_error,
    t(quantiles)
  )
  colnames(out)[3:ncol(out)] <- quantile_names

  out
}
