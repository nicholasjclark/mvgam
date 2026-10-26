#' Pointwise log-likelihoods from a fitted mvgam model
#'
#' The log density of each observation under each posterior draw, as
#' [brms::log_lik.brmsfit()] returns it. [loo.mvgam()], [waic.mvgam()],
#' [lfo_cv.mvgam()] and [kfold.mvgam()] score models from it.
#'
#' @inheritParams posterior_epred.mvgam
#' @param object A fitted `mvgam` object.
#' @param newdata Optional data frame holding the response and every
#'   covariate of the model. `NULL` (the default) takes the training
#'   data.
#' @param resp Optional name of one response of a multivariate model.
#' @param incl_autocor Logical. `TRUE` (the default) evaluates each
#'   density under the latent trend state the model inferred at that
#'   time, the state [hindcast.mvgam()] returns. The ELPD then describes
#'   the series that was observed. `FALSE` keeps only the trend's
#'   deterministic terms and leaves the autocorrelation out.
#' @param process_error Superseded by `incl_autocor` and still
#'   accepted. `incl_autocor` takes precedence when both are given.
#' @param allow_new_levels Logical. `TRUE` scores rows whose grouping
#'   level the training data lacked, as [kfold.mvgam()] needs when a
#'   fold holds out whole groups. Defaults to `FALSE`.
#' @param sample_new_levels How a new level's coefficients are drawn:
#'   `"uncertainty"` (the default), `"gaussian"` or `"old_levels"`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A numeric `[ndraws, nobs]` matrix of log densities. A missing
#'   response gives a column of `NA`. For a multivariate fit with
#'   `resp = NULL`, each column sums the densities of the responses
#'   measured in that row, as brms does. An occupancy, N-mixture or
#'   composition family has one density per site, carried on the site's
#'   first row.
#'
#' @details
#' Each family uses the brms parameterisation. For example,
#' `negbinomial()` takes a mean and shape, `Beta()` a mean and precision.
#' Hurdle and zero-inflated families mix a mass at zero with their base
#' family. The `weights()`, `cens()` and `trunc()` addition terms change
#' each row's contribution as they do in the Stan model.
#'
#' The [posterior_epred.mvgam()] family of methods integrates over the
#' trend dynamics. A covariate effect then has the same meaning at every
#' time. Those predictions describe no particular observed series and
#' do not give an ELPD.
#'
#' @seealso [loo.mvgam()] and [waic.mvgam()] estimate the ELPD from this
#'   matrix. [hindcast.mvgam()] returns the latent states it conditions
#'   on. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares models by cross-validation.
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
#' # One row per draw, one column per observation
#' ll <- log_lik(mod)
#' dim(ll)
#'
#' # Without the latent trend, the densities are far lower. The trend
#' # explains most of the series
#' ll_fixed <- log_lik(mod, incl_autocor = FALSE)
#' c(with_trend = mean(rowSums(ll)), without = mean(rowSums(ll_fixed)))
#'
#' # Next, loo() estimates the ELPD from these densities
#' }
#'
#' @family model comparison
#' @method log_lik mvgam
#' @export
log_lik.mvgam <- function(object,
                          newdata = NULL,
                          re_formula = NULL,
                          resp = NULL,
                          ndraws = NULL,
                          draw_ids = NULL,
                          incl_autocor = TRUE,
                          process_error = NULL,
                          allow_new_levels = FALSE,
                          sample_new_levels = "uncertainty",
                          ...) {
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "log_lik")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_string(resp, null.ok = TRUE)
  validate_group_level_args(re_formula, allow_new_levels,
                            sample_new_levels)
  rlang::check_dots_empty()
  incl_autocor <- resolve_incl_autocor(
    incl_autocor = incl_autocor,
    legacy = process_error,
    autocor_supplied = !missing(incl_autocor)
  )

  newdata <- prediction_frame(object, newdata)

  # The linear predictor and the distributional parameters are drawn
  # by separate extractions, so `ndraws` is materialised as concrete
  # `draw_ids` first. Left as a count, each extraction would subsample
  # independently and pair a dispersion, or a detection probability,
  # with a mean from an unrelated iteration.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  # The link-scale predictor under the inferred trend state. The
  # density of an observation is evaluated under the state the model
  # holds at its time.
  linpred <- get_combined_linpred(
    mvgam_fit = object,
    newdata = newdata,
    process_error = FALSE,
    trend_state = autocor_to_trend_state(incl_autocor),
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # A multivariate fit returns one matrix per response. With
  # `resp = NULL` brms sums them into the joint density of each row.
  if (is.list(linpred) && !is.matrix(linpred)) {
    if (!is.null(resp)) {
      linpred <- linpred[[resp]]
    } else {
      # Every response here is scored per row.
      # `resolve_observation_family()` refuses a closure-unit family as
      # one response of several.
      resp_names <- names(linpred)
      per_resp <- lapply(resp_names, function(r) {
        log_lik_single_response(
          object = object,
          newdata = newdata,
          linpred = linpred[[r]],
          resp = r,
          draw_ids = draw_ids,
          re_formula = re_formula,
          allow_new_levels = allow_new_levels,
          sample_new_levels = sample_new_levels
        )
      })
      if (length(per_resp) == 1L) {
        return(per_resp[[1L]])
      }
      # A response not measured at an occasion has an `NA` density.
      # The joint sums the measured responses. It is missing only at
      # an occasion where no response was measured.
      return(sum_measured(simplify2array(per_resp), dims = 2L))
    }
  }

  log_lik_single_response(
    object = object,
    newdata = newdata,
    linpred = linpred,
    resp = resp,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
}


#' The distributional parameters of a multi-response family
#'
#' `mvn()` and `mvt()` take a per-row scale, with degrees of freedom for
#' `mvt()`. The simplex families take a per-row softmax probability,
#' the closure-unit arrays naming the rows of each unit and a precision
#' for `diri()`. The likelihood, the quantile residual and the
#' predicted variance all call it. One extractor keeps the residual
#' standardising the density `log_lik()` evaluates. Each caller uses a
#' different part of the extractor's list. The list is returned whole.
#'
#' @param object Fitted `mvgam` object
#' @param newdata Data frame of the prediction rows
#' @param linpred Numeric linear predictor matrix, or `NULL`
#' @param family_obj Family object of the response
#' @param family_name Character family name
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @param ndraws Integer number of draws, or `NULL`
#' @return List of distributional parameters from
#'   `extract_simplex_response_components()` or
#'   `extract_mv_response_components()`
#' @noRd
mv_response_family_pars <- function(object, newdata, linpred,
                                    family_obj, family_name,
                                    draw_ids, ndraws = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_matrix(linpred, null.ok = TRUE)
  checkmate::assert_list(family_obj)
  checkmate::assert_string(family_name)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  if (is_simplex_response_family(family_obj)) {
    return(extract_simplex_response_components(
      object, newdata, draw_ids, ndraws = ndraws,
      needs_phi = identical(family_name, "diri"), linpred = linpred
    ))
  }
  extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws,
    needs_nu = identical(family_name, "mvt"), linpred = linpred
  )
}


#' Log densities of one response
#'
#' @param object Fitted `mvgam` object
#' @param newdata Data frame of the rows to score
#' @param linpred Numeric linear predictor matrix of the response
#' @param resp Response name, or `NULL`
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @param re_formula,allow_new_levels,sample_new_levels As in
#'   `log_lik.mvgam()`
#' @return Numeric `[ndraws, nobs]` matrix, or `[ndraws, n_units]` for
#'   a closure-unit family
#' @noRd
log_lik_single_response <- function(object, newdata, linpred, resp,
                                    draw_ids,
                                    re_formula = NULL,
                                    allow_new_levels = FALSE,
                                    sample_new_levels = "uncertainty") {
  family_obj <- model_families(object, resp)
  family_name <- resolve_family_name(family_obj)
  family_link <- family_obj$link

  y <- extract_response_for_log_lik(object, newdata, resp)

  # A closure-unit family is scored per unit. The Stan likelihood
  # marginalises the latent abundance or occupancy over the unit's
  # visits. The units are the conditionally independent blocks. The
  # unit arrays are rebuilt from `newdata`, which may change the caps.
  if (uses_closure_unit_layout(family_obj)) {
    # The families with several responses per unit have no latent
    # count. `mvn` and `mvt` score each row, independent given the
    # factors in the mean. `diri`, `multi` and `categ` score one joint
    # density per unit on its first row.
    if (is_multi_response_family(family_obj)) {
      family_pars_mv <- mv_response_family_pars(
        object, newdata, linpred, family_obj, family_name, draw_ids
      )
      return(dispatch_closure_unit_method(family_obj, "log_lik")(
        linpred     = linpred,
        link        = family_link,
        y           = y,
        family_pars = family_pars_mv,
        trials      = NULL
      ))
    }
    # The accessor resolves the unit key and the family's caps.
    arrays <- closure_unit_arrays_for(object, newdata)
    # The detection probability per visit, a scalar or a predictor of
    # its own under `bf(p ~ ...)`, as `resolve_family_pars()` gives it.
    p_mat <- extract_p_for_closure_unit(
      object   = object,
      newdata  = newdata,
      draw_ids = draw_ids,
      n_visit  = ncol(linpred),
      ndraws   = nrow(linpred)
    )
    family_pars_cu <- list(closure_arrays = arrays, p = p_mat)
    return(dispatch_closure_unit_method(family_obj, "log_lik")(
      linpred     = linpred,
      link        = family_link,
      y           = y,
      family_pars = family_pars_cu,
      trials      = NULL
    ))
  }

  if (is_ordinal_family(family_obj)) {
    # The observed level's probability, from the same category
    # probabilities `posterior_epred()` reports.
    family_pars <- list()
    trials <- NULL
    ll <- ordinal_log_lik(
      ordinal_category_probs(
        object, linpred, family_obj, draw_ids = draw_ids,
        newdata = newdata, resp = resp
      ),
      y
    )
  } else {
    # Distributional parameters (sigma, shape, hu, zi, ...) as
    # [ndraws x nobs], whether each was sampled as a scalar or
    # predicted by a formula of its own.
    family_pars <- resolve_family_pars(
      object,
      dpar_names = get_family_dpars(family_name),
      ndraws = nrow(linpred),
      nobs = ncol(linpred),
      draw_ids = draw_ids,
      newdata = newdata,
      resp = resp,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
    trials <- extract_trials_for_family(object, family_obj, newdata)
    ll <- dispatch_log_lik(
      family_name = family_name,
      link = family_link,
      linpred = linpred,
      y = y,
      family_pars = family_pars,
      trials = trials
    )
  }

  # `weights()`, `cens()` and `trunc()` change a row's contribution.
  # Every information criterion takes this matrix.
  apply_addition_terms(
    ll = ll,
    object = object,
    resp = resp,
    family_name = family_name,
    link = family_link,
    linpred = linpred,
    y = y,
    family_pars = family_pars,
    trials = trials
  )
}

#' @importFrom rstantools log_lik
#' @export
rstantools::log_lik


#' The observed response of the rows to score
#'
#' @param object Fitted `mvgam` object
#' @param newdata Data frame of the rows to score
#' @param resp Response name, or `NULL`
#' @return Numeric vector of the response
#' @noRd
extract_response_for_log_lik <- function(object, newdata, resp) {
  y_name <- response_column(object, resp)
  if (!y_name %in% names(newdata)) {
    stop(insight::format_error(c(
      paste0("Response '", y_name, "' is missing from 'newdata'."),
      i = "A log likelihood needs the observed response."
    )), call. = FALSE)
  }
  as.numeric(newdata[[y_name]])
}


#' Log densities of a family scored row by row
#'
#' A family with a `log_lik_<family>()` kernel is scored by it. Any
#' other family takes the distribution `family_dist_spec()` names, the
#' one its censoring, truncation and residuals use. Closure-unit and
#' ordinal families are scored before this is reached.
#'
#' @param family_name Character family name
#' @param link Character link name
#' @param linpred Numeric linear predictor matrix
#' @param y Numeric response vector
#' @param family_pars Named list of distributional parameter matrices
#' @param trials Numeric vector of trials, or `NULL`
#' @return Numeric `[ndraws, nobs]` matrix
#' @noRd
dispatch_log_lik <- function(family_name, link, linpred, y,
                             family_pars, trials) {
  fn_name <- paste0("log_lik_", family_name)
  if (!exists(fn_name, mode = "function", envir = asNamespace("mvgam"))) {
    spec <- family_dist_spec(family_name, link, linpred, family_pars,
                             trials)
    if (is.null(spec)) {
      refuse_missing_family_dispatch(family_name, "a log-likelihood")
    }
    return(dist_log_density(spec, linpred, y))
  }
  fn <- get(fn_name, mode = "function", envir = asNamespace("mvgam"))
  fn(
    linpred = linpred,
    link = link,
    y = y,
    family_pars = family_pars,
    trials = trials
  )
}


#' Log densities of one response, column by column
#'
#' Every per-family kernel takes `linpred` as an `[ndraws, nobs]` link
#' scale matrix, the observed `y`, the distributional parameters as a
#' list of `[ndraws, nobs]` matrices, the trials and the link name. It
#' returns the `[ndraws, nobs]` log densities of the brms `_lpdf` or
#' `_lpmf`.
#'
#' A missing response has no density. Its column stays `NA`, which
#' `clean_ll()` drops. The hurdle and zero-inflated kernels branch on
#' the response value and never see an `NA`.
#'
#' @param linpred Numeric linear predictor matrix
#' @param y Numeric response vector, one value per column
#' @param fn Function of one response value and its column index,
#'   returning the log density of each draw
#' @return Numeric `[ndraws, nobs]` matrix
#' @noRd
.apply_log_density <- function(linpred, y, fn) {
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = ncol(linpred))
  for (j in which(!is.na(y))) {
    out[, j] <- fn(y[j], j)
  }
  out
}


#' Log density of a hurdle family
#'
#' A zero has probability `hu`. A positive value has the base density,
#' scaled by `1 - hu`. A discrete base is truncated at zero, dividing
#' by its probability of a positive value.
#'
#' @param linpred Numeric linear predictor matrix
#' @param y Numeric response vector
#' @param hu Numeric hurdle probability matrix
#' @param log_base Function of `(yj, j)` giving the base log density
#' @param log_p0 Function of `j` giving the base log probability of
#'   zero, or `NULL` for a continuous base
#' @return Numeric `[ndraws, nobs]` matrix
#' @noRd
hurdle_log_density <- function(linpred, y, hu, log_base, log_p0 = NULL) {
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      return(log(hu[, j]))
    }
    out <- log1p(-hu[, j]) + log_base(yj, j)
    if (is.null(log_p0)) out else out - log1mexp(-log_p0(j))
  })
}


#' Log density of a zero-inflated family
#'
#' A zero comes from the inflation, with probability `zi`, or from the
#' base distribution. A base without mass at zero, such as the beta,
#' gives a zero the probability `zi` alone.
#'
#' @param linpred Numeric linear predictor matrix
#' @param y Numeric response vector
#' @param zi Numeric zero-inflation probability matrix
#' @param log_base Function of `(yj, j)` giving the base log density
#' @param base_has_zero Logical. Whether the base gives zero a mass
#' @return Numeric `[ndraws, nobs]` matrix
#' @noRd
zero_inflated_log_density <- function(linpred, y, zi, log_base,
                                      base_has_zero = TRUE) {
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0 && !base_has_zero) {
      return(log(zi[, j]))
    }
    from_base <- log1p(-zi[, j]) + log_base(yj, j)
    if (yj == 0) log_add_exp(log(zi[, j]), from_base) else from_base
  })
}


log_lik_hurdle_poisson <- function(linpred, link, y, family_pars, trials) {
  mu <- inv_link(linpred, link)
  hurdle_log_density(
    linpred, y, family_pars$hu,
    log_base = function(yj, j) stats::dpois(yj, mu[, j], log = TRUE),
    log_p0 = function(j) -mu[, j]
  )
}

log_lik_hurdle_negbinomial <- function(linpred, link, y, family_pars,
                                       trials) {
  mu <- inv_link(linpred, link)
  shape <- family_pars$shape
  log_nb <- function(yj, j) {
    stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE)
  }
  hurdle_log_density(
    linpred, y, family_pars$hu,
    log_base = log_nb, log_p0 = function(j) log_nb(0, j)
  )
}

log_lik_hurdle_gamma <- function(linpred, link, y, family_pars, trials) {
  mu <- inv_link(linpred, link)
  shape <- family_pars$shape
  hurdle_log_density(
    linpred, y, family_pars$hu,
    log_base = function(yj, j) {
      stats::dgamma(yj, shape = shape[, j], rate = shape[, j] / mu[, j],
                    log = TRUE)
    }
  )
}

log_lik_hurdle_lognormal <- function(linpred, link, y, family_pars,
                                     trials) {
  sigma <- family_pars$sigma
  hurdle_log_density(
    linpred, y, family_pars$hu,
    log_base = function(yj, j) {
      stats::dlnorm(yj, meanlog = linpred[, j], sdlog = sigma[, j],
                    log = TRUE)
    }
  )
}

log_lik_zero_inflated_poisson <- function(linpred, link, y,
                                          family_pars, trials) {
  mu <- inv_link(linpred, link)
  zero_inflated_log_density(
    linpred, y, family_pars$zi,
    log_base = function(yj, j) stats::dpois(yj, mu[, j], log = TRUE)
  )
}

log_lik_zero_inflated_negbinomial <- function(linpred, link, y,
                                              family_pars, trials) {
  mu <- inv_link(linpred, link)
  shape <- family_pars$shape
  zero_inflated_log_density(
    linpred, y, family_pars$zi,
    log_base = function(yj, j) {
      stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE)
    }
  )
}

log_lik_zero_inflated_binomial <- function(linpred, link, y,
                                           family_pars, trials) {
  prob <- inv_link(linpred, link)
  trials <- as.integer(trials)
  zero_inflated_log_density(
    linpred, y, family_pars$zi,
    log_base = function(yj, j) {
      stats::dbinom(yj, size = trials[j], prob = prob[, j], log = TRUE)
    }
  )
}

log_lik_zero_inflated_beta <- function(linpred, link, y,
                                       family_pars, trials) {
  mu <- inv_link(linpred, link)
  phi <- family_pars$phi
  zero_inflated_log_density(
    linpred, y, family_pars$zi,
    log_base = function(yj, j) {
      shapes <- beta_shapes(mu[, j], phi[, j])
      stats::dbeta(yj, shapes$shape1, shapes$shape2, log = TRUE)
    },
    base_has_zero = FALSE
  )
}


#' Log-likelihood of a fitted mvgam model
#'
#' The posterior mean of the summed log likelihood, with the `df` and
#' `nobs` attributes that [stats::AIC()] and [stats::BIC()] use.
#'
#' @param object A fitted `mvgam` object.
#' @param pointwise Logical. `TRUE` returns the `[ndraws, nobs]` matrix
#'   of [log_lik.mvgam()]. Defaults to `FALSE`.
#' @param ... Further arguments to [log_lik.mvgam()], such as `newdata`
#'   or `ndraws`.
#'
#' @return A `logLik` object. `df` counts the parameters [variables()]
#'   lists. With `pointwise = TRUE`, the matrix of [log_lik.mvgam()].
#'
#' @details
#' Each latent state counts as a parameter, which makes AIC and BIC
#' coarse for a state-space model. [loo.mvgam()] estimates the ELPD
#' with diagnostics for its reliability.
#'
#' @seealso [log_lik.mvgam()] returns the pointwise matrix.
#'
#' @method logLik mvgam
#' @export
logLik.mvgam <- function(object, pointwise = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(pointwise)

  ll_mat <- log_lik(object, ...)

  if (pointwise) {
    return(ll_mat)
  }

  # The columns `clean_ll()` keeps, which `loo()` and `waic()` score.
  # A composition carries each site's density on its first row and
  # leaves the others `NA`.
  per_draw_sum <- rowSums(clean_ll(ll_mat))
  ll_scalar <- mean(per_draw_sum)

  # The parameters `variables()` lists. That list leaves out `lp__`,
  # `lprior` and the sampler's diagnostics. It names each off-diagonal
  # cell of a symmetric matrix once.
  df <- length(variables(object))

  structure(
    ll_scalar,
    df = df,
    nobs = nobs(object),
    class = "logLik"
  )
}


#' Resolve the autocorrelation switch from its two spellings
#'
#' `incl_autocor` is the brms name. `process_error` and `incl_dynamics`
#' are the mvgam 1.x names, still accepted. A supplied `incl_autocor`
#' takes precedence.
#'
#' @param incl_autocor Value of the documented argument
#' @param legacy Value of the superseded argument, or `NULL` when the
#'   caller did not name it
#' @param autocor_supplied Whether the caller named `incl_autocor`
#' @return A single logical
#'
#' @noRd
resolve_incl_autocor <- function(incl_autocor, legacy, autocor_supplied) {
  checkmate::assert_logical(incl_autocor, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(legacy, len = 1L, any.missing = FALSE,
                            null.ok = TRUE)
  checkmate::assert_logical(autocor_supplied, len = 1L)
  if (is.null(legacy) || isTRUE(autocor_supplied)) {
    return(incl_autocor)
  }
  legacy
}
