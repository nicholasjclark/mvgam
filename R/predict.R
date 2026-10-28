#' Posterior predictions from an mvgam model
#'
#' Predicts new outcomes, their expectation or the linear predictor
#' from every posterior draw, for the training data or for new data.
#' The section 'Which prediction function?' compares `predict()` with
#' [fitted.mvgam()], [hindcast.mvgam()] and [forecast.mvgam()].
#'
#' @param object An object of class `mvgam`.
#' @param type Character; one of `"response"` (default), `"link"`,
#'   `"expected"`, `"variance"`, `"terms"`, `"latent_state"`,
#'   `"detection"`. See Details.
#' @inheritParams posterior_epred.mvgam
#' @param summary Logical. If `TRUE` (the default), returns summary
#'   statistics. If `FALSE`, returns the full matrix of posterior draws.
#' @param robust Logical. If `FALSE` (the default), uses mean and standard
#'   deviation for summaries. If `TRUE`, uses median and median absolute
#'   deviation (MAD).
#' @param probs Numeric vector of probabilities for quantile computation.
#'   Default is `c(0.025, 0.975)` for 95% credible intervals.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return With `summary = FALSE`, an `[ndraws, nobs]` matrix of
#'   draws. With `summary = TRUE`, a matrix with one row per
#'   observation and the columns:
#'   \itemize{
#'     \item `Estimate`: the posterior mean, or the median under
#'       `robust = TRUE`.
#'     \item `Est.Error`: the posterior SD, or the MAD under
#'       `robust = TRUE`.
#'     \item `Q*`: one quantile column per value of `probs`.
#'   }
#'   A multivariate model with `resp = NULL` gives a named list of
#'   these, one per response.
#'
#' @details
#' `type` selects the quantity returned:
#' \itemize{
#'   \item `"response"` (default): draws from the posterior predictive
#'     distribution on the outcome scale, from
#'     [posterior_predict.mvgam()].
#'   \item `"link"`: the linear predictor on the link scale, from
#'     [posterior_linpred.mvgam()].
#'   \item `"expected"`: the expectation E\[Y | X\] on the response
#'     scale, from [posterior_epred.mvgam()].
#'   \item `"variance"`: the conditional variance Var\[Y | theta\] per
#'     draw, from the family's mean-variance relation. It covers the
#'     gaussian, student, lognormal, poisson, bernoulli, binomial,
#'     negbinomial, gamma and beta families. Other families raise an
#'     error. By default the variance is the observation noise around
#'     the mean of the covariate part. `process_error = TRUE` also
#'     conditions on each draw's sampled trend innovation.
#'   \item `"terms"`: refused with a pointer to the functions that
#'     decompose a prediction. [posterior_smooths.mvgam()] returns
#'     per-smooth draws. [fixef.mvgam()] and [ranef.mvgam()] return the
#'     parametric and random-effect components.
#'   \item `"latent_state"`: for closure-unit families, the posterior of
#'     the latent state of each closure unit given its detection
#'     history:
#'     \itemize{
#'       \item `nmix()`: the latent abundance `N` given the observed
#'         visit counts (Royle 2004).
#'       \item `occ()`: the occupancy probability `P(z = 1 | y_g)`.
#'     }
#'   \item `"detection"`: for `nmix()` and `occ()`, the per-visit
#'     detection probability \eqn{p_{g,j}} on the response scale.
#' }
#'
#' @section Which prediction function?:
#' Every prediction adds the latent trend to the observation model. The
#' functions differ in the version of the trend they use and in whether
#' they add observation noise.
#'
#' | Function | Times | Trend by default | Observation noise |
#' |:---------|:------|:-----------------|:------------------|
#' | `fitted()` | training data | fitted state | no |
#' | `predict()` | any | covariate part | under `type = "response"` |
#' | `posterior_predict()` | any | covariate part | yes |
#' | `posterior_epred()`, `posterior_linpred()` | any | covariate part | no |
#' | `hindcast()` | training data | fitted state | under `type = "response"` |
#' | `forecast()` | after the training data | fitted state, carried forward | under `type = "response"` |
#' | `residuals()`, `pp_check()` | training data | fitted state | yes |
#'
#' The fitted state is the value the trend took at each observed time,
#' as the model inferred it. The covariate part keeps the effects of the
#' observation and trend formulas and leaves out the trend's
#' innovations. Questions about covariates use it, as
#' [conditional_effects.mvgam()] and the \pkg{marginaleffects} functions
#' do. `incl_autocor = TRUE` switches `predict()` and the
#' `posterior_*()` methods to the fitted state, the default of their
#' \pkg{brms} counterparts. `fitted()` uses the covariate part for
#' `newdata`.
#'
#' @references
#' Royle, J. A. (2004). N-mixture models for estimating
#'   population size from spatially replicated counts.
#'   *Biometrics*, 60, 108-115.
#'   \doi{10.1111/j.0006-341X.2004.00142.x}
#'
#' @seealso [fitted.mvgam()] returns expected values with the inferred
#'   trend state. [forecast.mvgam()] predicts future times from the
#'   fitted state. [conditional_effects.mvgam()] plots predictions
#'   against a covariate.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Predicted counts with their 95% intervals
#' head(predict(mod))
#'
#' # The same fit on the link scale, as an expected count and as the
#' # variance of a count. A Poisson's variance equals its mean.
#' head(predict(mod, type = "link"))
#' head(cbind(
#'   expected = predict(mod, type = "expected")[, "Estimate"],
#'   variance = predict(mod, type = "variance")[, "Estimate"]
#' ))
#'
#' # `summary = FALSE` returns the draws, one row per draw
#' dim(predict(mod, summary = FALSE))
#'
#' # Next: forecast() predicts beyond the training times from the
#' # fitted trend state
#' }
#'
#' @family predictions
#' @export
predict.mvgam <- function(object,
                          newdata = NULL,
                          type = c("response", "link", "expected",
                                   "variance", "terms", "latent_state",
                                   "detection"),
                          process_error = FALSE,
                          incl_autocor = FALSE,
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

  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "predict")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  type <- match.arg(type)
  checkmate::assert_logical(process_error, len = 1, any.missing = FALSE)
  checkmate::assert_logical(incl_autocor, len = 1, any.missing = FALSE)
  validate_draw_selectors(ndraws, draw_ids)
  validate_group_level_args(re_formula, allow_new_levels, sample_new_levels)
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
  rlang::check_dots_empty()

  # Every extraction below takes the same draw indices. A requested
  # count becomes indices here.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  if (type %in% c("latent_state", "detection")) {
    require_closure_unit_predict_type(object$family, type)
    pred <- if (identical(type, "latent_state")) {
      posterior_latent_N(object, newdata = newdata, draw_ids = draw_ids)
    } else {
      posterior_detection(object, newdata = newdata, draw_ids = draw_ids)
    }
    return(summarise_or_pass(pred, summary = summary, probs = probs,
                             robust = robust))
  }

  # mgcv's `predict(type = "terms")` returns one column per term. An
  # mvgam fit takes each kind of term's draws from its own function.
  # The message names them.
  if (identical(type, "terms")) {
    stop(insight::format_error(c(
      "type = 'terms' has no single equivalent in mvgam.",
      i = paste0(
        "Use posterior_smooths.mvgam() for per-smooth draws, ",
        "fixef.mvgam() / ranef.mvgam() for parametric and random ",
        "components and posterior_linpred.mvgam() for the combined ",
        "linear predictor."
      )
    )), call. = FALSE)
  }

  draws_from <- function(fun, ...) {
    fun(object, newdata = newdata, process_error = process_error,
        incl_autocor = incl_autocor, draw_ids = draw_ids,
        re_formula = re_formula, allow_new_levels = allow_new_levels,
        sample_new_levels = sample_new_levels, resp = resp, ...)
  }
  draws <- switch(
    type,
    response = draws_from(posterior_predict),
    link = draws_from(posterior_linpred, transform = FALSE),
    expected = draws_from(posterior_epred),
    variance = draws_from(predict_variance)
  )
  summarise_or_pass(draws, summary = summary, probs = probs,
                    robust = robust)
}


#' Conditional variance of the response at each posterior draw
#'
#' `Var[Y | theta]` from the family's mean-variance relation. The
#' expected values and the distributional parameters come from the
#' same draws. Each variance then pairs a mean with the dispersion of
#' its own iteration.
#'
#' @inheritParams predict.mvgam
#' @param draw_ids Draw indices to use, or `NULL` for all
#' @return An `[ndraws, nobs]` matrix
#'
#' @noRd
predict_variance <- function(object, newdata, process_error,
                             incl_autocor, draw_ids, re_formula,
                             allow_new_levels, sample_new_levels, resp) {
  # Each response of a multivariate model has its own family and
  # dispersion. A variance belongs to one response.
  resolve_resp(object, resp, required = TRUE,
               caller = "predict(type = 'variance')")
  family <- model_families(object, resp)
  family_name <- resolve_family_name(family)
  draw_ids <- resolve_draw_indices(posterior::ndraws(object), NULL,
                                   draw_ids)

  # The multi-response families take their per-cell moments from the
  # unit arrays.
  if (is_multi_response_family(family)) {
    comp <- mv_response_family_pars(
      object, newdata, linpred = NULL, family_obj = family,
      family_name = family_name, draw_ids = draw_ids
    )
    if (!is_simplex_response_family(family)) {
      return(comp$variance_row)
    }
    # A softmax probability p_k gives a categorical cell the variance
    # p_k (1 - p_k). A multinomial cell scales it by the unit total N.
    # A Dirichlet cell divides it by phi + 1. `comp$phi` already holds
    # one column per row.
    prob <- comp$prob_row
    base_var <- prob * (1 - prob)
    if (identical(family_name, "diri")) {
      return(base_var / (comp$phi + 1))
    }
    if (identical(family_name, "multi")) {
      total_mat <- matrix(
        multinomial_unit_totals(object, newdata, comp$arrays),
        nrow = comp$ndraws, ncol = comp$N_obs, byrow = TRUE
      )
      return(base_var * total_mat)
    }
    return(base_var)
  }

  mu <- posterior_epred(
    object,
    newdata = newdata,
    process_error = process_error,
    incl_autocor = incl_autocor,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # A detection is Bernoulli with variance E(1 - E). A thinned Poisson
  # count has variance E.
  if (uses_closure_unit_layout(family)) {
    if (is_binary_response_family(family)) {
      return(mu * (1 - mu))
    }
    return(mu)
  }

  # An ordinal family predicts a probability per category, which gives
  # its expectation a third margin. posterior_predict() draws the
  # ordered levels as the integers 1..K. The variance of that draw is
  # the one the category probabilities imply.
  if (is_ordinal_family(family)) {
    return(ordinal_category_variance(mu))
  }

  # Each parameter arrives as `[ndraws, nobs]`, from its own formula or
  # from a scalar draw.
  fpars <- resolve_family_pars(
    object,
    dpar_names = get_family_dpars(family_name),
    ndraws = nrow(mu),
    nobs = ncol(mu),
    draw_ids = draw_ids,
    newdata = newdata,
    resp = resp,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  compute_family_variance(
    mu = mu,
    family = family,
    sigma = fpars$sigma,
    shape = fpars$shape,
    phi = fpars$phi,
    nu = fpars$nu,
    trials = extract_trials_for_family(object, family, newdata, resp)
  )
}


#' Prediction draws, summarised when asked
#'
#' @param draws Draws from a prediction function: one matrix or array,
#'   or a list of them named by response
#' @param summary Logical. `TRUE` summarises each element with
#'   `summarise_predictions()`. `FALSE` returns `draws` unchanged.
#' @inheritParams summarise_predictions
#' @return `draws`, or its summaries in the same structure
#' @noRd
summarise_or_pass <- function(draws, summary, probs, robust) {
  if (!summary) {
    return(draws)
  }
  if (is_per_response(draws)) {
    return(lapply(draws, summarise_predictions, probs = probs,
                  robust = robust))
  }
  summarise_predictions(draws, probs = probs, robust = robust)
}


#' Summaries of prediction draws, one per observation
#'
#' @inheritParams mvgam_post_summary
#' @param draws An `[ndraws, nobs]` matrix of draws or, for the ordinal
#'   and categorical families, an `[ndraws, nobs, ncat]` array
#'
#' @return The matrix of `mvgam_post_summary()`. An array input gives
#'   an `[nobs, nstat, ncat]` array holding that matrix per category.
#'
#' @noRd
summarise_predictions <- function(draws, probs, robust) {
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1)
  checkmate::assert_logical(robust, len = 1)

  # The ordinal and categorical families predict a probability per
  # category. Their draws carry a third margin. Each category is
  # summarised on its own and the results are stacked back along that
  # margin.
  if (length(dim(draws)) == 3L) {
    checkmate::assert_array(draws, mode = "numeric", d = 3L)
    cats <- seq_len(dim(draws)[3L])
    per_cat <- lapply(cats, function(k) {
      slice <- matrix(
        draws[, , k], nrow = dim(draws)[1L], ncol = dim(draws)[2L],
        dimnames = dimnames(draws)[1:2]
      )
      summarise_predictions(slice, probs = probs, robust = robust)
    })
    out <- array(
      unlist(per_cat, use.names = FALSE),
      dim = c(dim(per_cat[[1L]]), length(cats)),
      dimnames = list(
        dimnames(draws)[[2L]],
        colnames(per_cat[[1L]]),
        dimnames(draws)[[3L]]
      )
    )
    return(out)
  }

  checkmate::assert_matrix(
    draws,
    mode = "numeric",
    min.rows = 1,
    min.cols = 1
  )

  mvgam_post_summary(draws, robust = robust, probs = probs)
}
