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
#'   posterior_summary.mvgam getCall.mvgam vcov.mvgam
#'
#' @param object,x A fitted `mvgam` object.
#' @param pars Optional character vector of parameter names. For
#'   `nuts_params` these are sampler-parameter names; otherwise they
#'   are model-parameter names. `NULL` returns all parameters.
#' @param summary Logical. If `TRUE` (the default), summary statistics
#'   are returned; if `FALSE`, the raw posterior draws are returned.
#'   `coef` summarises to posterior means rather than to the
#'   Estimate / Est.Error / quantile matrix its siblings return.
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
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and
#' Burkner, P.-C. (2021). Rank-normalization, folding, and
#' localization: An improved Rhat for assessing convergence of
#' MCMC. \emph{Bayesian Analysis}, 16(2):667-718.
#' \doi{10.1214/20-BA1221}
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' coef(mod)
#' fixef(mod)
#' vcov(mod)
#' bayes_R2(mod)
#' }
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
  # A column with no value to summarise gives NA, which is what
  # `colMeans()` and `sd()` above already answer for one. `quantile()`
  # raises instead, so the four statistics on one row disagreed about
  # the same column: a closure unit that lost every visit leaves rows
  # belonging to no unit, and `fitted()` and `augment()` stopped on a
  # message naming neither the row nor the column.
  #
  # Built column by column at a fixed width so one probability gives
  # a one-column matrix rather than a vector. `apply()` drops that
  # dimension, and the `cbind()` below then recycled the estimates
  # against it and failed on the dimnames.
  q <- matrix(
    vapply(
      seq_len(ncol(draws)),
      function(j) {
        col <- draws[, j]
        if (anyNA(col)) {
          rep(NA_real_, length(probs))
        } else {
          unname(stats::quantile(col, probs = probs))
        }
      },
      numeric(length(probs))
    ),
    nrow = ncol(draws), byrow = TRUE
  )
  out <- cbind(Estimate = centre, Est.Error = spread, q)
  colnames(out) <- c("Estimate", "Est.Error",
                     paste0("Q", probs * 100))
  out
}


#' @rdname mvgam_diagnostics
#' @method coef mvgam
#' @export
coef.mvgam <- function(object, summary = TRUE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(summary, len = 1L)
  # Reuse the `betas` keyword so the b_trend[*] block is filtered
  # consistently with as.matrix(object, variable = "betas").
  draws_mat <- as_draws_matrix(object, variable = "betas")
  if (ncol(draws_mat) == 0L) {
    return(if (isTRUE(summary)) numeric() else matrix(0, 0, 0))
  }
  if (isTRUE(summary)) {
    return(colMeans(draws_mat))
  }
  draws_mat
}


#' @rdname mvgam_diagnostics
#' @importFrom bayesplot nuts_params
#' @method nuts_params mvgam
#' @export
nuts_params.mvgam <- function(object, pars = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  bayesplot::nuts_params(object$fit, pars = pars, ...)
}


#' @rdname mvgam_diagnostics
#' @importFrom bayesplot log_posterior
#' @method log_posterior mvgam
#' @export
log_posterior.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  bayesplot::log_posterior(object$fit, ...)
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior rhat
#' @method rhat mvgam
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
  assert_resp_for_mv(object, resp, "bayes_R2")
  resp_use <- scored_response_name(object, resp)
  # Bayesian R^2 of Gelman et al. (2019): var(epred) / (var(epred) +
  # var(residual)) per draw, where residuals are y - epred. The
  # expectation is taken conditional on the latent state, because the
  # residual it is measured against is the one the observed series
  # leaves; marginalising over the trend would score the fit against a
  # series the model never saw. The `resp` argument is only meaningful for
  # multivariate fits; passing it to a univariate posterior_epred is
  # a hard error.
  epred <- if (is_mv) {
    posterior_epred(object, resp = resp_use,
                    incl_autocor = TRUE, ...)
  } else {
    posterior_epred(object, incl_autocor = TRUE, ...)
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
  # A row with no response has no residual, so the variances are taken
  # over the observations that do.
  var_ep <- apply(epred, 1L, stats::var, na.rm = TRUE)
  var_re <- apply(resid, 1L, stats::var, na.rm = TRUE)
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
#' @export
prior_summary.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  if (is.null(object$prior)) {
    stop(insight::format_error(
      "Fit was not stored with a prior table (object$prior is NULL)."
    ))
  }
  backfill_declared_bounds(object$prior, object$stancode)
}


#' Fill in a stored prior table's missing bounds from the program
#'
#' A fit saved before the table carried bounds has them as `NA`,
#' which reads as though a parameter were sampled unbounded when the
#' program declares otherwise. The declaration is stored on the fit
#' alongside the table, so the support can be recovered rather than
#' requiring the model to be fitted again. Rows that already name a
#' bound are left alone, so this only ever supplies what is absent.
#'
#' @param prior The stored prior table.
#' @param stancode The stored Stan program, or `NULL`.
#' @return The table, with absent bounds filled in where the program
#'   declares one.
#' @noRd
backfill_declared_bounds <- function(prior, stancode) {
  if (!is.data.frame(prior) || nrow(prior) == 0L) return(prior)
  if (!all(c("class", "lb", "ub") %in% names(prior))) return(prior)
  if (is.null(stancode)) return(prior)
  sc <- paste(as.character(stancode), collapse = "\n")
  if (!nzchar(sc)) return(prior)
  declared <- stancode_declared_bounds(sc)
  if (!length(declared)) return(prior)
  for (i in seq_len(nrow(prior))) {
    bound <- declared[[prior$class[i]]]
    if (is.null(bound)) next
    if (is.na(prior$lb[i])) prior$lb[i] <- bound$lb
    if (is.na(prior$ub[i])) prior$ub[i] <- bound$ub
  }
  prior
}


# ---- Tier 2 additions -----------------------------------------------

#' @rdname mvgam_diagnostics
#' @importFrom posterior ndraws
#' @method ndraws mvgam
#' @export
ndraws.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::ndraws(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior nchains
#' @method nchains mvgam
#' @export
nchains.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::nchains(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior niterations
#' @method niterations mvgam
#' @export
niterations.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::niterations(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior nvariables
#' @method nvariables mvgam
#' @export
nvariables.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  posterior::nvariables(as_draws_array(x))
}


#' @rdname mvgam_diagnostics
#' @importFrom brms posterior_summary
#' @method posterior_summary mvgam
#' @export
posterior_summary.mvgam <- function(x, pars = NULL,
                                     probs = c(0.025, 0.975),
                                     robust = FALSE, ...) {
  object <- x
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
  call <- x$call
  # A fit saved before the call was captured at the user-facing
  # entry point carries the `do.call()` frame's version, whose head
  # is the function object rather than its name. Left as it is,
  # `deparse()` prints the whole of mvgam's source instead of the
  # call, so the head is named here for a fit that can no longer be
  # re-stamped. The arguments such a call inlined are not
  # recoverable; refit to record them as written.
  if (is.call(call) && is.function(call[[1L]])) {
    call[[1L]] <- as.name("mvgam")
  }
  call
}


#' @rdname mvgam_diagnostics
#' @param correlation Logical. If `TRUE`, return the correlation
#'   matrix of fixed-effect coefficients; otherwise the covariance
#'   matrix. Defaults to `FALSE`.
#' @method vcov mvgam
#' @export
vcov.mvgam <- function(object, correlation = FALSE, pars = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(correlation, len = 1L)
  checkmate::assert_character(pars, null.ok = TRUE)
  # Reuse the `betas` keyword so the `b_trend[*]` filter matches
  # `coef.mvgam` / `fixef.mvgam` exactly.
  mat <- as_draws_matrix(object, variable = "betas")
  if (!is.null(pars)) {
    keep <- paste0("b_", pars)
    mat <- mat[, intersect(colnames(mat), keep), drop = FALSE]
  }
  if (ncol(mat) == 0L) {
    return(matrix(0, 0, 0))
  }
  colnames(mat) <- sub("^b_", "", colnames(mat))
  if (isTRUE(correlation)) stats::cor(mat) else stats::cov(mat)
}


# Internal: detect the n_lv = n_series + iid-Z funnel pattern on a
# fitted mvgam. Returns TRUE when the trend is a by_lv factor model
# at the full-rank boundary, the loadings prior is the default iid
# (no structured prior via MGP / features / distances), and AT
# LEAST ONE of:
#   * any divergent transition recorded in NUTS diagnostics; OR
#   * at least one watched parameter has rank-normalised Rhat > 1.05.
#
# The rank-normalised Rhat estimator recommends 1.01 as the ceiling,
# so 1.05 is the conservative middle ground between that
# recommendation and the traditional 1.1 threshold. Divergent
# transitions are a faster early signal than Rhat for the rotational
# funnel; the structural pre-checks already constrain the call to a
# narrow high-risk class, so divergence inside that class is nearly
# pathology-specific. The watch-list covers the declared (`A_trend`,
# `L_Omega_trend`) and transformed (`Sigma_trend`, `L_Sigma_trend`)
# variance-block parameters plus the latent state and process
# noise. A single parameter exceeding threshold is sufficient: the
# funnel affects all rotation-orbit parameters jointly, so one hit
# implies the others.
#
# Used by the post-fit advisor wired into `mvgam_core.R` to suggest
# `loadings_prior = "mgp"` or a fixed-Z `trend_map` when convergence
# breaks.
#'@noRd
flag_by_lv_full_rank_funnel <- function(mvgam_fit) {
  md <- mvgam_fit$trend_metadata
  if (!isTRUE(md$has_by_lv)) return(FALSE)
  n_lv <- md$n_lv_for_grain %||% md$n_lv
  n_series <- length(md$levels$series %||% character(0))
  if (is.null(n_lv) || length(n_series) == 0L ||
      n_series == 0L || as.integer(n_lv) != as.integer(n_series)) {
    return(FALSE)
  }
  # Reason: `uses_loadings_prior()` returns TRUE only for the
  # structured prior families (MGP via `N_features_trend`, kernel via
  # `dist_*`). Iid Z leaves both markers absent, so the funnel
  # only applies when this helper returns FALSE.
  if (isTRUE(uses_loadings_prior(mvgam_fit))) return(FALSE)

  # Divergent-transition early signal.
  np <- tryCatch(bayesplot::nuts_params(mvgam_fit$fit),
                  error = function(e) NULL)
  if (!is.null(np) && "Parameter" %in% names(np)) {
    div_rows <- np[np$Parameter == "divergent__", , drop = FALSE]
    if (nrow(div_rows) > 0L && any(div_rows$Value > 0, na.rm = TRUE)) {
      return(TRUE)
    }
  }

  rh <- tryCatch(bayesplot::rhat(mvgam_fit$fit),
                  error = function(e) numeric())
  if (length(rh) == 0L) return(FALSE)
  watched <- grep(
    paste0(
      "^(init_trend|lv_trend|sigma_trend|Sigma_trend|",
      "L_Sigma_trend|L_Omega_trend|A_trend)\\["
    ),
    names(rh)
  )
  if (length(watched) == 0L) return(FALSE)
  any(rh[watched] > 1.05, na.rm = TRUE)
}


# Internal: fire the post-fit advisor warning when
# `flag_by_lv_full_rank_funnel()` is TRUE and the user did not
# silence runtime output via `silent = 2`. Suppressed under
# `TESTTHAT = true` by `mvgam_warn_once_user()`.
#'@noRd
warn_by_lv_full_rank_funnel <- function(mvgam_fit, silent) {
  if (identical(as.integer(silent), 2L)) return(invisible(NULL))
  if (!flag_by_lv_full_rank_funnel(mvgam_fit)) return(invisible(NULL))
  mvgam_warn_once_user(
    message = paste0(
      "Convergence diagnostics suggest 'Z' is weakly identified at ",
      "'n_lv = n_series' with the default iid prior. Consider ",
      "'loadings_prior = \"mgp\"' for column shrinkage, or pin ",
      "loadings with a 'trend_map' (e.g. diag(n_series)) for a ",
      "non-factor model."
    ),
    class = "mvgam_by_lv_full_rank_funnel"
  )
}


# Internal: one-shot user-facing rlang warning that is suppressed
# under `testthat`. Centralises the
# `Sys.getenv("TESTTHAT")` + `.frequency = "once"` idiom used by
# several runtime advisors. The `class` argument doubles as the
# rlang `.frequency_id` so each call site gets its own one-shot
# counter.
#'@noRd
mvgam_warn_once_user <- function(message, class) {
  checkmate::assert_string(message)
  checkmate::assert_string(class)
  if (identical(Sys.getenv("TESTTHAT"), "true")) return(invisible(NULL))
  rlang::warn(
    message,
    class = class,
    .frequency = "once",
    .frequency_id = class
  )
  invisible(NULL)
}
