#' Diagnostic quantities for mvgam models
#'
#' S3 methods that surface Stan / NUTS diagnostics for fitted
#' [`mvgam`] objects. Each method delegates either to `bayesplot::`
#' on the underlying stanfit (`nuts_params`, `log_posterior`) or to
#' `posterior::` summaries on the draws array.
#'
#' @name mvgam_diagnostics
#' @aliases coef.mvgam rhat.mvgam neff_ratio.mvgam nuts_params.mvgam
#'   log_posterior.mvgam fixef.mvgam bayes_R2.mvgam prior_summary.mvgam
#'   ndraws.mvgam nchains.mvgam niterations.mvgam nvariables.mvgam
#'   posterior_summary.mvgam getCall.mvgam
#'
#' @param object,x A fitted `mvgam` object.
#' @param pars Optional character vector of parameter names. For
#'   `nuts_params` these are sampler-parameter names; otherwise they
#'   are model-parameter names. `NULL` returns all parameters.
#' @param summarise Logical. If `TRUE` (the default) `coef` returns
#'   posterior means; if `FALSE` it returns the full chain matrix.
#' @param summary Logical. If `TRUE` (the default), summary statistics
#'   are returned; if `FALSE`, the raw posterior draws are returned.
#' @param robust Logical. If `TRUE`, use median + median absolute
#'   deviation instead of mean + standard deviation. Default `FALSE`.
#' @param probs Numeric vector of length 2 giving the lower and upper
#'   quantile probabilities for credible intervals. Defaults to
#'   `c(0.025, 0.975)` (95% CI).
#' @param resp Character. Response name for multivariate models.
#'   Ignored for univariate fits.
#' @param ... Additional arguments forwarded to the underlying
#'   `bayesplot::*` / `posterior::*` function.
#'
#' @return Shape depends on the method (see method details).
#'
#' @seealso
#'   [mvgam_draws] for raw posterior-draw extraction,
#'   [summary.mvgam()] for a printed model summary,
#'   [mcmc_plot.mvgam()] for diagnostic visualisations,
#'   [pp_check.mvgam()] for posterior predictive checks,
#'   [loo.mvgam()], [waic.mvgam()] for information criteria,
#'   [conditional_effects.mvgam()] for partial-effect plots,
#'   [posterior_epred.mvgam()], [posterior_linpred.mvgam()],
#'   [posterior_predict.mvgam()] for prediction primitives,
#'   \code{\link[bayesplot:bayesplot-extractors]{bayesplot-extractors}},
#'   [posterior::rhat()], [posterior::ess_bulk()],
#'   [brms::bayes_R2.brmsfit()], [brms::fixef.brmsfit()],
#'   [brms::prior_summary.brmsfit()].
#'
#' @author Nicholas J Clark
NULL


# Internal: posterior-mean / median / quantile summary for a draws
# matrix, in brms-style column layout.
mvgam_post_summary <- function(draws, robust = FALSE,
                               probs = c(0.025, 0.975)) {
  if (isTRUE(robust)) {
    centre <- apply(draws, 2L, stats::median)
    spread <- apply(draws, 2L, stats::mad)
  } else {
    centre <- colMeans(draws)
    spread <- apply(draws, 2L, stats::sd)
  }
  q <- t(apply(draws, 2L, stats::quantile, probs = probs))
  out <- cbind(Estimate = centre, Est.Error = spread, q)
  colnames(out) <- c("Estimate", "Est.Error",
                     paste0("Q", probs * 100))
  out
}


#' @rdname mvgam_diagnostics
#' @method coef mvgam
#' @export
coef.mvgam <- function(object, summarise = TRUE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(summarise, len = 1L)
  # Reuse the `betas` keyword so the b_trend[*] block is filtered
  # consistently with as.matrix(object, variable = "betas").
  draws_mat <- as_draws_matrix(object, variable = "betas")
  if (ncol(draws_mat) == 0L) {
    return(if (isTRUE(summarise)) numeric() else matrix(0, 0, 0))
  }
  if (isTRUE(summarise)) {
    return(colMeans(draws_mat))
  }
  draws_mat
}


#' @rdname mvgam_diagnostics
#' @importFrom bayesplot nuts_params
#' @method nuts_params mvgam
#' @export nuts_params
#' @export
nuts_params.mvgam <- function(object, pars = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  bayesplot::nuts_params(object$fit, pars = pars, ...)
}


#' @rdname mvgam_diagnostics
#' @importFrom bayesplot log_posterior
#' @method log_posterior mvgam
#' @export log_posterior
#' @export
log_posterior.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  bayesplot::log_posterior(object$fit, ...)
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior rhat
#' @method rhat mvgam
#' @export rhat
#' @export
rhat.mvgam <- function(x, pars = NULL, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(pars, null.ok = TRUE)
  drws <- as_draws_array(x, variable = pars)
  summ <- posterior::summarise_draws(drws, rhat = posterior::rhat)
  out <- summ$rhat
  names(out) <- summ$variable
  out
}


#' @rdname mvgam_diagnostics
#' @importFrom bayesplot neff_ratio
#' @method neff_ratio mvgam
#' @export neff_ratio
#' @export
neff_ratio.mvgam <- function(object, pars = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(pars, null.ok = TRUE)
  drws <- as_draws_array(object, variable = pars)
  summ <- posterior::summarise_draws(
    drws,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )
  # min(ess_bulk, ess_tail) mirrors bayesplot's neff_ratio convention.
  ess <- pmin(summ$ess_bulk, summ$ess_tail)
  names(ess) <- summ$variable
  ess / posterior::ndraws(drws)
}


# ---- Tier 1 additions -----------------------------------------------

#' @rdname mvgam_diagnostics
#' @importFrom brms fixef
#' @method fixef mvgam
#' @export fixef
#' @export
fixef.mvgam <- function(object, summary = TRUE, robust = FALSE,
                         probs = c(0.025, 0.975), pars = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(summary, len = 1L)
  checkmate::assert_logical(robust, len = 1L)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L)
  # Reuse the `betas` keyword to share the b_trend[*] filter logic
  # with as.matrix.mvgam / coef.mvgam.
  mat <- as_draws_matrix(object, variable = "betas")
  if (!is.null(pars)) {
    keep <- paste0("b_", pars)
    mat <- mat[, intersect(colnames(mat), keep), drop = FALSE]
  }
  if (ncol(mat) == 0L) {
    return(matrix(0, 0, 0))
  }
  # Strip the `b_` prefix so column / row names match brms output.
  colnames(mat) <- sub("^b_", "", colnames(mat))
  if (!isTRUE(summary)) {
    return(mat)
  }
  mvgam_post_summary(mat, robust = robust, probs = probs)
}


#' @rdname mvgam_diagnostics
#' @importFrom brms bayes_R2
#' @method bayes_R2 mvgam
#' @export bayes_R2
#' @export
bayes_R2.mvgam <- function(object, resp = NULL, summary = TRUE,
                            robust = FALSE,
                            probs = c(0.025, 0.975), ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(summary, len = 1L)
  checkmate::assert_logical(robust, len = 1L)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L)
  # brms binomial fits carry a trailing `trials` entry in
  # response_names that is structural, not a second response;
  # is.mvbrmsformula is the authoritative MV indicator.
  is_mv <- brms::is.mvbrmsformula(object$formula)
  if (is_mv && is.null(resp)) {
    stop(insight::format_error(c(
      "bayes_R2 requires 'resp' for multivariate models.",
      i = paste0(
        "Available responses: ",
        paste(shQuote(object$response_names), collapse = ", "), "."
      )
    )))
  }
  resp_use <- if (is.null(resp)) object$response_names[1L] else resp
  # Bayesian R^2 of Gelman et al. (2019): var(epred) / (var(epred) +
  # var(residual)) per draw, where residuals are y - epred. The
  # `resp` argument is only meaningful for multivariate fits;
  # passing it to a univariate posterior_epred is a hard error.
  epred <- if (is_mv) {
    posterior_epred(object, resp = resp_use, ...)
  } else {
    posterior_epred(object, ...)
  }
  y <- object$data[[resp_use]]
  if (is.null(y) || !is.numeric(y)) {
    stop(insight::format_error(c(
      paste0(
        "bayes_R2 requires a numeric response. Response '",
        resp_use, "' is not numeric."
      ),
      i = "Bayesian R^2 is undefined for ordinal / categorical fits."
    )))
  }
  resid <- sweep(epred, 2L, y, FUN = "-")
  var_ep <- apply(epred, 1L, stats::var)
  var_re <- apply(resid, 1L, stats::var)
  r2 <- var_ep / (var_ep + var_re)
  if (!isTRUE(summary)) {
    return(r2)
  }
  mat <- matrix(r2, ncol = 1L)
  colnames(mat) <- "R2"
  out <- mvgam_post_summary(mat, robust = robust, probs = probs)
  rownames(out) <- "R2"
  out
}


#' @rdname mvgam_diagnostics
#' @importFrom brms prior_summary
#' @method prior_summary mvgam
#' @export prior_summary
#' @export
prior_summary.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  if (is.null(object$prior)) {
    stop(insight::format_error(
      "Fit was not stored with a prior table (object$prior is NULL)."
    ))
  }
  object$prior
}


# ---- Tier 2 additions -----------------------------------------------

#' @rdname mvgam_diagnostics
#' @importFrom posterior ndraws
#' @method ndraws mvgam
#' @export ndraws
#' @export
ndraws.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::ndraws(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior nchains
#' @method nchains mvgam
#' @export nchains
#' @export
nchains.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::nchains(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior niterations
#' @method niterations mvgam
#' @export niterations
#' @export
niterations.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::niterations(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior nvariables
#' @method nvariables mvgam
#' @export nvariables
#' @export
nvariables.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::nvariables(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom brms posterior_summary
#' @method posterior_summary mvgam
#' @export posterior_summary
#' @export
posterior_summary.mvgam <- function(object, pars = NULL,
                                     probs = c(0.025, 0.975),
                                     robust = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(pars, null.ok = TRUE)
  drws <- as_draws_array(object, variable = pars)
  mat <- posterior::as_draws_matrix(drws)
  mvgam_post_summary(mat, robust = robust, probs = probs)
}


#' @rdname mvgam_diagnostics
#' @method getCall mvgam
#' @export
getCall.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  x$call
}
