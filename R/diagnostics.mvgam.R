#' Posterior summaries and sampler diagnostics of mvgam models
#'
#' The \pkg{brms} and \pkg{bayesplot} accessors for a fitted model.
#' `fixef()`, `coef()` and `vcov()` summarise the population-level
#' coefficients of the observation formula, or of the trend formula
#' with `trend_effects = TRUE`. `posterior_summary()` summarises any
#' parameters. `rhat()` and `neff_ratio()` report convergence.
#' `nuts_params()` and `log_posterior()` return the sampler's own
#' quantities.
#' `bayes_R2()` is the Bayesian R^2 of Gelman et al. (2019).
#' `ndraws()`, `nchains()`, `niterations()` and `nvariables()` count
#' the posterior.
#'
#' @name mvgam_diagnostics
#' @aliases coef.mvgam rhat.mvgam neff_ratio.mvgam nuts_params.mvgam
#'   log_posterior.mvgam fixef.mvgam bayes_R2.mvgam prior_summary.mvgam
#'   ndraws.mvgam nchains.mvgam niterations.mvgam nvariables.mvgam
#'   posterior_summary.mvgam getCall.mvgam vcov.mvgam
#'
#' @param object,x A fitted `mvgam` object.
#' @param pars Optional character vector of parameter names, or of
#'   sampler parameters such as `"divergent__"` for `nuts_params()`.
#'   `NULL` takes all of them.
#' @param variable Parameter names for `posterior_summary()`, the
#'   \pkg{brms} name of the argument. There `pars` is its deprecated
#'   alias.
#' @param summary Logical. `TRUE` (the default) returns a summary.
#'   `FALSE` returns the draws. `coef()` summarises to posterior means.
#' @param robust Logical. `TRUE` summarises by the median and the median
#'   absolute deviation. Defaults to `FALSE`, the mean and the standard
#'   deviation.
#' @param probs Numeric vector of the two interval probabilities,
#'   `c(0.025, 0.975)` by default.
#' @param resp Optional name of one response of a multivariate model.
#' @param trend_effects Logical. `coef()`, `fixef()` and `vcov()`
#'   report the coefficients of the observation formula by default.
#'   `TRUE` reports those of the `trend_formula` instead. Their names
#'   end in `_trend`.
#' @param ... Further arguments. `nuts_params()` and `log_posterior()`
#'   pass them to \pkg{bayesplot}. `bayes_R2()` passes them to
#'   [posterior_epred.mvgam()]. The other methods refuse them.
#'
#' @return `fixef()`, `posterior_summary()` and `bayes_R2()` return a
#'   matrix with columns `Estimate`, `Est.Error` and one per quantile,
#'   or the draws with `summary = FALSE`. `coef()` returns the posterior
#'   means. `vcov()` returns the covariance matrix of the coefficients.
#'   `rhat()` and `neff_ratio()` return a named vector with one value
#'   per parameter. `nuts_params()` and `log_posterior()` return the
#'   long data frames of \pkg{bayesplot}. The counting methods return
#'   an integer.
#'
#' @seealso [summary.mvgam()] prints these summaries for every
#'   parameter. [mcmc_plot.mvgam()] plots the draws. [mvgam_draws]
#'   extracts them. [loo_R2.mvgam()] is the leave-one-out R^2.
#'
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and
#' Burkner, P.-C. (2021). Rank-normalization, folding, and
#' localization: An improved Rhat for assessing convergence of
#' MCMC. \emph{Bayesian Analysis}, 16(2):667-718.
#' \doi{10.1214/20-BA1221}
#'
#' Gelman, A., Goodrich, B., Gabry, J. and Vehtari, A. (2019).
#' R-squared for Bayesian regression models. \emph{The American
#' Statistician}, 73(3):307-309. \doi{10.1080/00031305.2018.1549100}
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
#' # The population coefficients with 95% intervals
#' fixef(mod)
#'
#' # Rhat near 1 and effective sample ratios above 0.1 indicate mixing
#' rhat(mod, pars = c("ar1_trend", "sigma_trend"))
#' neff_ratio(mod, pars = c("ar1_trend", "sigma_trend"))
#'
#' # The share of variance in the counts the model explains
#' bayes_R2(mod)
#'
#' # Next, mcmc_plot() draws the posterior of these parameters
#' }
#'
#' @author Nicholas J Clark
#' @family model checking
NULL


#' Posterior summaries of each column of a draws matrix
#'
#' Columns follow the brms layout of `Estimate`, `Est.Error` and one
#' quantile column per probability. A column holding a missing value
#' gets missing quantiles.
#'
#' @inheritParams predict.mvgam
#' @param draws Numeric matrix of draws, one column per quantity
#' @return Numeric matrix with one row per column of `draws`
#' @noRd
mvgam_post_summary <- function(draws, robust = FALSE,
                               probs = c(0.025, 0.975)) {
  checkmate::assert_matrix(draws)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1L)
  if (isTRUE(robust)) {
    centre <- apply(draws, 2L, stats::median)
    spread <- apply(draws, 2L, stats::mad)
  } else {
    centre <- colMeans(draws)
    spread <- apply(draws, 2L, stats::sd)
  }
  # `quantile()` errors on a missing value where `colMeans()` and
  # `sd()` return NA. A closure unit without visits leaves such
  # columns. Their quantiles are set to NA to match.
  #
  # The matrix is filled at a fixed width. `apply()` would drop the
  # dimension of a single probability.
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


#' The posterior median and two quantiles of each column of draws
#'
#' Columns are named `<prefix>Q50`, `<prefix>Q<lower>` and
#' `<prefix>Q<upper>`. The forecast, impulse-response and
#' variance-decomposition summaries report them.
#'
#' @param draws Numeric matrix of draws, one column per quantity
#' @param probs Numeric vector of the two interval probabilities
#' @param prefix Character prefix for the column names
#' @return Data frame with one row per column of `draws`
#' @noRd
median_interval_cols <- function(draws, probs, prefix) {
  checkmate::assert_string(prefix)
  checkmate::assert_matrix(draws, mode = "numeric")
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L,
                            any.missing = FALSE)
  probs <- sort(probs)
  q <- apply(draws, 2L, stats::quantile, probs = c(0.5, probs),
             na.rm = TRUE, names = FALSE)
  out <- as.data.frame(t(matrix(q, nrow = 3L)))
  names(out) <- paste0(prefix, "Q", c(50, 100 * probs))
  out
}


#' @rdname mvgam_diagnostics
#' @method coef mvgam
#' @export
coef.mvgam <- function(object, summary = TRUE, trend_effects = FALSE,
                       ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(summary)
  rlang::check_dots_empty()
  draws_mat <- population_draws(object, trend_effects)
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
  forward_to_stanfit("nuts_params", object, pars = pars, ...)
}


#' @rdname mvgam_diagnostics
#' @importFrom bayesplot log_posterior
#' @method log_posterior mvgam
#' @export
log_posterior.mvgam <- function(object, ...) {
  forward_to_stanfit("log_posterior", object, ...)
}


#' Call a bayesplot extractor on the fit's stanfit
#'
#' A name outside the method's formals is refused. bayesplot would
#' otherwise ignore it.
#'
#' @param generic Name of the bayesplot generic
#' @param object Fitted `mvgam` object
#' @param ... Arguments to the `stanfit` method
#' @return The value of the `stanfit` method
#' @noRd
forward_to_stanfit <- function(generic, object, ...) {
  checkmate::assert_string(generic)
  checkmate::assert_class(object, "mvgam")
  method <- utils::getS3method(generic, "stanfit",
                               envir = asNamespace("bayesplot"))
  refuse_unread_dots(list(...), names(formals(method)), generic)
  method(object$fit, ...)
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior rhat
#' @method rhat mvgam
#' @export
rhat.mvgam <- function(x, pars = NULL, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(pars, null.ok = TRUE)
  rlang::check_dots_empty()
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
  rlang::check_dots_empty()
  drws <- as_draws_array(object, variable = pars)
  summ <- without_ess_cap_notice(posterior::summarise_draws(
    drws,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  ))
  # The smaller of the bulk and tail ESS, as in brms
  ess <- pmin(summ$ess_bulk, summ$ess_tail)
  names(ess) <- summ$variable
  ess / posterior::ndraws(drws)
}


#' @rdname mvgam_diagnostics
#' @importFrom brms fixef
#' @method fixef mvgam
#' @export
fixef.mvgam <- function(object, summary = TRUE, robust = FALSE,
                         probs = c(0.025, 0.975), pars = NULL,
                         trend_effects = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L)
  rlang::check_dots_empty()
  mat <- select_fixef_draws(population_draws(object, trend_effects), pars)
  if (ncol(mat) == 0L) {
    return(matrix(0, 0, 0))
  }
  colnames(mat) <- fixef_name(colnames(mat))
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
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L)
  resolve_resp(object, resp, required = TRUE, caller = "bayes_R2()")
  y <- r2_response(object, resp, "bayes_R2()")
  # The expectation conditions on the inferred latent state. Its
  # residual is then the one the observed series leaves.
  epred <- do.call(posterior_epred, c(
    list(object), r2_resp_arg(object, resp), list(incl_autocor = TRUE),
    list(...)
  ))
  # As brms does, the rows without a response are left out
  observed <- !is.na(y)
  epred <- epred[, observed, drop = FALSE]
  resid <- sweep(epred, 2L, y[observed], FUN = "-")
  var_ep <- apply(epred, 1L, stats::var)
  r2_result(var_ep / (var_ep + apply(resid, 1L, stats::var)),
            summary, robust, probs)
}


#' The `resp` argument of a prediction for an R-squared
#'
#' A univariate prediction method refuses `resp`.
#'
#' @param object Fitted `mvgam` object
#' @param resp Response name, or `NULL`
#' @return List holding `resp` for a multivariate fit, otherwise empty
#' @noRd
r2_resp_arg <- function(object, resp) {
  if (is_multivariate_formula(object$formula)) list(resp = resp) else list()
}


#' The observed response of an R-squared
#'
#' @param object Fitted `mvgam` object
#' @param resp Response name, or `NULL`
#' @param caller Name of the calling function, for the message
#' @return Numeric vector of the response, one value per row
#' @noRd
r2_response <- function(object, resp, caller) {
  checkmate::assert_string(caller)
  y_col <- response_column(object, resp)
  y <- object$data[[y_col]]
  if (!is.numeric(y)) {
    stop(insight::format_error(c(
      paste0("'", caller, "' requires a numeric response."),
      x = paste0("Response '", y_col, "' has class '", class(y)[1L], "'."),
      i = "R^2 is undefined for ordinal and categorical fits."
    )), call. = FALSE)
  }
  y
}


#' The draws or the summary of an R-squared
#'
#' @param r2 Numeric vector of R-squared draws
#' @param summary,robust,probs As in `bayes_R2()`
#' @return A one-column matrix of draws, or a one-row summary
#' @noRd
r2_result <- function(r2, summary, robust, probs) {
  r2 <- matrix(r2, ncol = 1L, dimnames = list(NULL, "R2"))
  if (!summary) {
    return(r2)
  }
  out <- mvgam_post_summary(r2, robust = robust, probs = probs)
  rownames(out) <- "R2"
  out
}


#' @rdname mvgam_diagnostics
#' @importFrom brms prior_summary
#' @method prior_summary mvgam
#' @export
prior_summary.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  object$prior
}


#' A posterior count method for mvgam fits
#'
#' @param count A posterior function counting draws, chains or
#'   iterations, such as `posterior::ndraws`
#' @return An S3 method of `x` and `...`
#' @noRd
posterior_count_method <- function(count) {
  force(count)
  function(x, ...) {
    checkmate::assert_class(x, "mvgam")
    require_fitted_model(x)
    rlang::check_dots_empty()
    count(fit_draws_shape(x$fit))
  }
}


#' @rdname mvgam_diagnostics
#' @importFrom posterior ndraws
#' @method ndraws mvgam
#' @export
ndraws.mvgam <- posterior_count_method(posterior::ndraws)


#' @rdname mvgam_diagnostics
#' @importFrom posterior nchains
#' @method nchains mvgam
#' @export
nchains.mvgam <- posterior_count_method(posterior::nchains)


#' @rdname mvgam_diagnostics
#' @importFrom posterior niterations
#' @method niterations mvgam
#' @export
niterations.mvgam <- posterior_count_method(posterior::niterations)


#' @rdname mvgam_diagnostics
#' @importFrom posterior nvariables
#' @method nvariables mvgam
#' @export
nvariables.mvgam <- function(x, ...) {
  length(variables(x, ...))
}


#' @rdname mvgam_diagnostics
#' @importFrom brms posterior_summary
#' @method posterior_summary mvgam
#' @export
posterior_summary.mvgam <- function(x, pars = NULL, variable = NULL,
                                     probs = c(0.025, 0.975),
                                     robust = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(pars, null.ok = TRUE)
  checkmate::assert_character(variable, null.ok = TRUE)
  rlang::check_dots_empty()
  variable <- mvgam_use_alias(variable, pars, c("variable", "pars"))
  mat <- as_draws_matrix(x, variable = variable)
  mvgam_post_summary(mat, robust = robust, probs = probs)
}


#' @rdname mvgam_diagnostics
#' @method getCall mvgam
#' @export
getCall.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  rlang::check_dots_empty()
  x$call
}


#' @rdname mvgam_diagnostics
#' @param correlation Logical. `TRUE` returns the correlation matrix of
#'   the coefficients. Defaults to `FALSE`, the covariance matrix.
#' @method vcov mvgam
#' @export
vcov.mvgam <- function(object, correlation = FALSE, pars = NULL,
                       trend_effects = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(correlation)
  checkmate::assert_character(pars, null.ok = TRUE)
  rlang::check_dots_empty()
  mat <- select_fixef_draws(population_draws(object, trend_effects), pars)
  if (ncol(mat) == 0L) {
    return(matrix(0, 0, 0))
  }
  colnames(mat) <- fixef_name(colnames(mat))
  if (isTRUE(correlation)) stats::cor(mat) else stats::cov(mat)
}


#' Draws of the population-level coefficients of one formula
#'
#' The `betas` keyword is the set `brms::fixef()` reports for the
#' observation formula. `trend_betas` is the same set for the trend
#' formula. `coef()`, `fixef()`, `vcov()` and `get_coef()` all take
#' their draws from this function and agree on which coefficients
#' belong to which formula.
#'
#' @param object A fitted `mvgam` object
#' @param trend_effects Logical. `TRUE` selects the trend formula.
#' @return A draws matrix
#' @noRd
population_draws <- function(object, trend_effects = FALSE) {
  checkmate::assert_flag(trend_effects)
  if (trend_effects && is.null(object$trend_formula)) {
    stop(insight::format_error(
      "'trend_effects = TRUE' needs a model fitted with a 'trend_formula'."
    ), call. = FALSE)
  }
  as_draws_matrix(
    object, variable = if (trend_effects) "trend_betas" else "betas"
  )
}


#' The fixed-effect draws a caller named
#'
#' A name the fit has no coefficient for is refused. A misspelt term
#' would otherwise lose its row without a message.
#'
#' @param mat Draws matrix of the population-level coefficients
#' @param pars Coefficient names as `fixef_name()` gives them, or
#'   `NULL` for all of them
#' @return `mat`, restricted to `pars` in the order given
#' @noRd
select_fixef_draws <- function(mat, pars) {
  if (is.null(pars)) {
    return(mat)
  }
  checkmate::assert_character(pars, any.missing = FALSE, min.len = 1L)
  names <- fixef_name(colnames(mat))
  keep <- match(pars, names)
  unknown <- pars[is.na(keep)]
  if (length(unknown)) {
    stop(insight::format_error(c(
      "Unknown fixed effects in 'pars'.",
      x = paste0(
        "Not found: ", paste0("'", unknown, "'", collapse = ", "), "."
      ),
      i = paste0(
        "Fixed effects: ",
        paste0("'", names, "'", collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  mat[, keep, drop = FALSE]
}


#' Whether a factor fit shows the full-rank loading funnel
#'
#' The funnel arises in a `lv_axis()` factor model with as many factors
#' as series under the default independent loadings prior. Any
#' divergent transition then marks it, as does a rank-normalised Rhat
#' above 1.05 on any watched parameter. The 1.05 cut is a compromise
#' between the recommended 1.01 and the traditional 1.1. The structural
#' checks confine the test to a narrow high-risk class of models. A
#' divergence inside that class points to the funnel. The watched
#' parameters are the variance-block parameters, the latent states and
#' the process noise. The funnel moves every parameter of the rotation
#' orbit together. One exceedance suffices. A fit without MCMC draws
#' returns `FALSE`.
#'
#' @param mvgam_fit Fitted `mvgam` object
#' @return Logical scalar
#' @noRd
flag_by_lv_full_rank_funnel <- function(mvgam_fit) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  md <- mvgam_fit$trend_metadata
  if (!isTRUE(md$has_by_lv)) return(FALSE)
  if (md$n_lv_for_grain != md$axes$series$n) {
    return(FALSE)
  }
  # The MGP and kernel loadings priors shrink the loadings. The funnel
  # arises under the independent prior alone.
  if (isTRUE(uses_loadings_prior(mvgam_fit))) return(FALSE)

  # Divergences and Rhat exist only for an MCMC run
  if (is.null(mvgam_fit$fit) ||
      !identical(mvgam_fit$algorithm, "sampling")) {
    return(FALSE)
  }

  np <- bayesplot::nuts_params(mvgam_fit$fit)
  if (!is.null(np) && "Parameter" %in% names(np)) {
    div_rows <- np[np$Parameter == "divergent__", , drop = FALSE]
    if (nrow(div_rows) > 0L && any(div_rows$Value > 0, na.rm = TRUE)) {
      return(TRUE)
    }
  }

  watched <- grep(
    paste0(
      "^(init_trend|lv_trend|sigma_trend|Sigma_trend|",
      "L_Sigma_trend|L_Omega_trend|Phi_trend)\\["
    ),
    names(mvgam_fit$fit), value = TRUE
  )
  if (length(watched) == 0L) return(FALSE)
  any(rhat(mvgam_fit, pars = watched) > 1.05, na.rm = TRUE)
}


#' Warn once when a factor fit shows the full-rank loading funnel
#'
#' The warning suggests the MGP loadings prior or fixed loadings
#' through `trend_map`.
#'
#' @param mvgam_fit Fitted `mvgam` object
#' @return `NULL`, invisibly
#' @noRd
warn_by_lv_full_rank_funnel <- function(mvgam_fit) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  if (!flag_by_lv_full_rank_funnel(mvgam_fit)) return(invisible(NULL))
  warn_once(
    c(
      "'Z' is weakly identified at 'n_lv = n_series' under the default prior.",
      i = "Use 'loadings_prior = \"mgp\"' to shrink columns.",
      i = "Or pin the loadings with 'trend_map' to fit a non-factor model."
    ),
    "mvgam_by_lv_full_rank_funnel"
  )
}


