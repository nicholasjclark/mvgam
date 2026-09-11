#' Posterior Linear Predictor for mvgam Models
#'
#' @description
#' Extract linear predictor values from fitted mvgam models. Combines
#' observation and trend components on the link scale with proper
#' uncertainty propagation.
#'
#' @name posterior_linpred
NULL


#' Translate the user-facing surface selector to the internal one
#'
#' `incl_autocor` is how every user-facing method spells the choice of
#' prediction surface; `trend_state` is the name
#' `get_combined_linpred()` reads. Translating in one place keeps the
#' two from drifting apart, which is how a method came to offer the
#' choice under two spellings whose defaults disagreed.
#'
#' @param incl_autocor Logical; `TRUE` conditions on the fitted latent
#'   state, `FALSE` answers from the deterministic submodels.
#'
#' @return Either `"conditional"` or `"marginal"`.
#'
#' @noRd
autocor_to_trend_state <- function(incl_autocor) {
  checkmate::assert_logical(incl_autocor, len = 1L, any.missing = FALSE)
  if (isTRUE(incl_autocor)) "conditional" else "marginal"
}


#' Get Combined Linear Predictor
#'
#' Extracts and combines observation and trend linear predictors from an
#' mvgam model. Observation and trend effects are added on the link scale.
#'
#' @param mvgam_fit mvgam object from mvgam()
#' @param newdata data.frame with prediction covariates
#' @param process_error Logical; if TRUE, the trend carries a sampled
#'   innovation on top of its deterministic submodel. If FALSE the
#'   submodel contributes alone, still at its own per-draw values.
#'   Read only under `trend_state = "marginal"`.
#' @param trend_state Which prediction surface the trend contribution
#'   comes from. `"marginal"` integrates over the trend dynamics by
#'   sampling innovations, the semantic the `posterior_*` methods carry.
#'   `"conditional"` reads the fitted `trend[t, s]` draws instead, the
#'   semantic `hindcast()` and the likelihood surfaces carry.
#' @param draw_ids Integer vector of posterior draws to use (NULL = all).
#'   Indices rather than a count, so every extraction combined below
#'   reads the same iterations.
#' @param re_formula Group-level terms to include: `NULL` for every one,
#'   `NA` for none
#' @param allow_new_levels Logical; accepted for brms compatibility.
#'   A grouping level the model never saw is refused whatever this is
#'   set to, because a new level has no fitted random effect and, for
#'   a new series, no latent state to propagate. Predict for levels
#'   the fit knows, or refit with the new levels included.
#' @param sample_new_levels Character; accepted for brms
#'   compatibility and not used, since new levels are refused. See
#'   `allow_new_levels`.
#' @param resp Character; response name for multivariate models (NULL = all)
#'
#' @return Matrix `\\[ndraws x nobs\\]` of combined linear predictor values on
#'   link scale. For multivariate models with resp = NULL, returns named
#'   list of matrices.
#'
#' @noRd
get_combined_linpred <- function(mvgam_fit, newdata,
                                 process_error = TRUE,
                                 trend_state = c("marginal",
                                                  "conditional"),
                                 draw_ids = NULL,
                                 re_formula = NULL,
                                 allow_new_levels = FALSE,
                                 sample_new_levels = "uncertainty",
                                 resp = NULL) {
  trend_state <- match.arg(trend_state)
  # The observation predictor, the trend predictor and the process
  # errors are three separate extractions whose results are added
  # together. They take draw indices rather than a count precisely so
  # that a row of the answer cannot pair an observation-side effect
  # with a trend from an unrelated iteration; the count was resolved
  # at the boundary the caller came through.

  # Check if trend model exists and has required structure
  has_trend <- !is.null(mvgam_fit$trend_model) &&
               !is.null(mvgam_fit$trend_model$formula)

  # Extract observation linear predictor (validation handled internally)
  obs_linpred <- extract_component_linpred(
    mvgam_fit = mvgam_fit,
    newdata = newdata,
    component = "obs",
    resp = resp,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )

  # If no trend model, return observation linpred only
  if (!has_trend) {
    return(obs_linpred)
  }

  # A conditional read asks for the state the model actually inferred
  # at each time, so it takes the fitted `trend[t, s]` draws. Those
  # already carry the trend formula's contribution, because the trend
  # kernel is written on the centred convention `(trend - mu_trend)`,
  # and adding the deterministic submodel on top would count it twice.
  # A row whose time falls outside the fitted grid has no such state
  # and receives the per-series marginal instead.
  #
  # On a response-keyed fit the state is read once per response, since
  # each response is its own series and holds its own `trend[t, s]`
  # column. Reading it once and sharing it gave every response the
  # first one's trajectory. A multivariate fit whose series axis is a
  # column of the frame holds one state that every response reads, so
  # there it is built once rather than once per response with the same
  # answer. The process noise below follows the same rule.
  per_response <- is_response_keyed(mvgam_fit) &&
    is.list(obs_linpred) && !is.matrix(obs_linpred)
  conditional_state <- if (identical(trend_state, "conditional")) {
    draws_mat <- posterior::as_draws_matrix(mvgam_fit$fit)
    if (!is.null(draw_ids)) {
      draws_mat <- draws_mat[draw_ids, , drop = FALSE]
    }
    state_for <- function(r) {
      extract_trend_latent_states(
        mvgam_fit = mvgam_fit, newdata = newdata,
        full_draws = draws_mat, resp = r
      )
    }
    if (per_response) {
      stats::setNames(
        lapply(names(obs_linpred), state_for), names(obs_linpred)
      )
    } else {
      state_for(resp)
    }
  } else {
    NULL
  }

  if (!is.null(conditional_state)) {
    # Either one matrix or one per response; the multivariate branch
    # below reads both shapes, and a state already carries the trend
    # formula's contribution so there is no noise to add on top.
    trend_linpred <- conditional_state
    trend_noise <- NULL
  } else {
    # Marginal read: the deterministic submodel, plus one sample of
    # the trend's own process noise. The noise is skipped when
    # `process_error = FALSE` or when the trend kernel has no
    # stochastic component (PW, none), so neither path pays for an
    # observation structure it will not use.
    trend_linpred <- extract_component_linpred(
      mvgam_fit = mvgam_fit,
      newdata = newdata,
      component = "trend",
      resp = resp,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
    trend_noise <- if (isTRUE(process_error) &&
                         has_stochastic_trend(mvgam_fit)) {
      sample_process_errors(
        mvgam_fit, newdata = newdata, draw_ids = draw_ids,
        resp = if (per_response) names(obs_linpred) else resp
      )
    } else {
      NULL
    }
  }

  # Detect structure: list indicates multivariate, matrix indicates univariate
  is_multivariate <- is.list(obs_linpred) && !is.matrix(obs_linpred)

  if (is_multivariate) {
    # Multivariate: combine each response separately
    combined <- lapply(names(obs_linpred), function(resp_name) {
      # The trend and its noise each arrive as one matrix or as one per
      # response. A matrix is shared by every response, which is what a
      # marginal read of a wide fit gives for the deterministic part:
      # the trend design of a wide frame runs at time grain, since a
      # covariate column there holds one value per time and cannot name
      # a response. A list holds each response's own, which the
      # conditional state and a wide frame's noise both are.
      own <- function(x) {
        if (is.list(x) && !is.matrix(x)) x[[resp_name]] else x
      }
      trend_mat <- own(trend_linpred)
      checkmate::assert_matrix(trend_mat)
      compose_linpred_with_noise(
        obs_mat = obs_linpred[[resp_name]], trend_mat = trend_mat,
        trend_noise = own(trend_noise), resp_name = resp_name
      )
    })
    names(combined) <- names(obs_linpred)
    return(combined)
  }

  # Univariate case: both components are matrices
  checkmate::assert_matrix(obs_linpred)
  checkmate::assert_matrix(trend_linpred)
  compose_linpred_with_noise(
    obs_mat = obs_linpred, trend_mat = trend_linpred,
    trend_noise = trend_noise, resp_name = NULL
  )
}


# Internal: combine the per-draw obs and trend deterministic linpreds
# and optionally add the marginal trend-noise sample. Single helper
# shared by the univariate and multivariate branches of
# `get_combined_linpred`. Validates shapes and emits a contextual
# message for the multivariate response when `resp_name` is supplied.
#'@noRd
compose_linpred_with_noise <- function(obs_mat, trend_mat, trend_noise,
                                        resp_name = NULL) {
  if (nrow(trend_mat) != nrow(obs_mat) ||
      ncol(trend_mat) != ncol(obs_mat)) {
    msg <- if (!is.null(resp_name)) {
      cli::format_inline(
        "Dimension mismatch for response {.val {resp_name}}: ",
        "obs_linpred is [{nrow(obs_mat)} x {ncol(obs_mat)}] but ",
        "trend_linpred is [{nrow(trend_mat)} x {ncol(trend_mat)}]."
      )
    } else {
      cli::format_inline(
        "Dimension mismatch: obs_linpred is ",
        "[{nrow(obs_mat)} x {ncol(obs_mat)}] but trend_linpred is ",
        "[{nrow(trend_mat)} x {ncol(trend_mat)}]."
      )
    }
    stop(insight::format_error(msg))
  }
  out <- obs_mat + trend_mat
  if (!is.null(trend_noise)) {
    if (nrow(trend_noise) != nrow(out) ||
        ncol(trend_noise) != ncol(out)) {
      stop(insight::format_error(
        cli::format_inline(
          "Trend-noise dimension mismatch: noise is ",
          "[{nrow(trend_noise)} x {ncol(trend_noise)}] but linpred is ",
          "[{nrow(out)} x {ncol(out)}]."
        )
      ))
    }
    out <- out + trend_noise
  }
  out
}


#' Extract Posterior Linear Predictor from mvgam Models
#'
#' @description
#' Extract linear predictor values (eta, on link scale) from fitted mvgam
#' models. Combines observation model effects with State-Space trend
#' components. For Poisson models this is log-scale; for binomial models
#' this is logit-scale.
#'
#' @param object A fitted mvgam object from [mvgam()].
#' @param newdata Optional data frame with covariates for prediction. If
#'   NULL, uses original training data stored in the model object.
#' @param transform Logical; if `FALSE` (default), values are returned
#'   on the link scale. If `TRUE`, the inverse link of `mu` is applied,
#'   so the result is on that parameter's own scale. This is not the
#'   mean of the response: a binomial `mu` is a probability rather than
#'   a count, and a hurdle or zero-inflated `mu` is the base
#'   distribution's parameter before any mass is moved to zero. Use
#'   [posterior_epred.mvgam()] for `E[Y]`. Mirrors the `transform`
#'   argument of [brms::posterior_linpred()], which sets `dpar = "mu"`
#'   and answers on the response scale.
#' @inheritParams posterior_epred.mvgam
#' @param ndraws Positive integer specifying number of posterior draws to
#'   use. NULL (default) uses all available draws. Mutually exclusive
#'   with `draw_ids`; supply one or the other.
#' @param draw_ids Optional integer vector of specific draw indices to
#'   use. `NULL` (default) selects draws via `ndraws` (or all draws when
#'   both are NULL). Useful for keeping multiple downstream extractions
#'   aligned to the same posterior subset.
#' @param re_formula Group-level terms to include: `NULL` (the default)
#'   for every one, `NA` for none. A formula choosing some of them is not
#'   supported.
#' @param allow_new_levels Logical; accepted for brms compatibility.
#'   A grouping level the model never saw is refused whatever this is
#'   set to, because a new level has no fitted random effect and, for
#'   a new series, no latent state to propagate. Predict for levels
#'   the fit knows, or refit with the new levels included.
#' @param sample_new_levels Character; accepted for brms
#'   compatibility and not used, since new levels are refused. See
#'   `allow_new_levels`.
#' @param resp Character specifying which response variable for
#'   multivariate models. NULL (default) returns predictions for all
#'   responses.
#' @param dpar Character naming a distributional parameter, such as
#'   `"sigma"` or `"nu"`. The default `NULL` answers for the mean.
#'   Naming a parameter that carries a formula of its own returns its
#'   linear predictor, which has no latent trend term; naming one that
#'   was sampled as a scalar returns those draws across the rows.
#' @param ... Additional arguments passed to internal methods.
#'
#' @return Matrix with dimensions `\\[ndraws x nobs\\]` containing linear
#'   predictor values. Each row is one posterior draw, each column is
#'   one observation from newdata. Values are on link scale.
#'
#'   For multivariate models with resp = NULL, returns a named list of
#'   matrices (one per response variable).
#'
#' @details
#' The linear predictor combines:
#' \itemize{
#'   \item Observation model effects: fixed effects, random effects,
#'     smooth terms, GP terms from the observation formula
#'   \item Trend effects: State-Space trend contributions from the
#'     trend formula (if present)
#' }
#'
#' For models without a trend component (trend_formula = NULL), behavior
#' matches [brms::posterior_linpred()].
#'
#' Innovations are sampled once, on the linear predictor, so
#' `process_error = TRUE` here carries the same state that
#' [posterior_epred.mvgam()] and [posterior_predict.mvgam()] carry
#' under that setting; the latter two then apply the inverse link and
#' the observation family on top. Under `FALSE` the predictor still
#' varies draw to draw, because every coefficient in both submodels
#' does; what it leaves out is the latent process.
#'
#' @seealso [brms::posterior_linpred()] for the brms generic,
#'   [posterior_epred.mvgam()] for expected values on response scale,
#'   [posterior_predict.mvgam()] for posterior predictive samples,
#'   \[forecast.mvgam\] and \[hindcast.mvgam\] for the deterministic
#'   state-extrapolating prediction surface.
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
#' # Link-scale linear predictor draws (log lambda for the Poisson).
#' lp <- posterior_linpred(mod, ndraws = 50L)
#' dim(lp)
#'
#' # Apply the inverse link manually to get the per-draw rate. With
#' # `process_error = FALSE` on both sides this equals what
#' # `posterior_epred()` returns (`process_error = TRUE` adds
#' # marginal trend noise so the relationship is approximate).
#' lp_det <- posterior_linpred(mod, ndraws = 50L, process_error = FALSE)
#' ep_det <- posterior_epred(mod,  ndraws = 50L, process_error = FALSE)
#' all.equal(colMeans(exp(lp_det)), colMeans(ep_det), tolerance = 1e-6)
#' }
#'
#' @importFrom brms posterior_linpred
#' @method posterior_linpred mvgam
#' @export
posterior_linpred.mvgam <- function(object, transform = FALSE,
                                    newdata = NULL,
                                    process_error = FALSE,
                                    incl_autocor = FALSE,
                                    ndraws = NULL,
                                    draw_ids = NULL,
                                    re_formula = NULL,
                                    allow_new_levels = FALSE,
                                    sample_new_levels = "uncertainty",
                                    resp = NULL,
                                    dpar = NULL,
                                    ...) {
  # Validate mvgam-specific parameters only (other validation delegated)
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(transform)
  checkmate::assert_logical(process_error, len = 1)
  trend_state <- autocor_to_trend_state(incl_autocor)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE,
                                any.missing = FALSE)
  checkmate::assert_string(dpar, null.ok = TRUE)

  # A count becomes indices here, so nothing below is left to choose
  # its own draws.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  # A named distributional parameter answers for that parameter rather
  # than for the mean, and carries no latent trend: mvgam does not
  # accept a trend formula on a distributional parameter.
  if (!is.null(dpar)) {
    return(dpar_posterior_linpred(
      object, dpar = dpar, transform = transform, newdata = newdata,
      draw_ids = draw_ids, resp = resp
    ))
  }

  # Handle newdata = NULL (use training data)
  if (is.null(newdata)) {
    if (is.null(object$data)) {
      stop(insight::format_error(
        cli::format_inline(
          "No training data found in model object. ",
          "Please provide {.field newdata} explicitly."
        )
      ))
    }
    newdata <- object$data
  }

  # Delegate to get_combined_linpred (all other validation handled there)
  linpred <- get_combined_linpred(
    mvgam_fit = object,
    newdata = newdata,
    process_error = process_error,
    trend_state = trend_state,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )
  if (!transform) {
    return(linpred)
  }
  # `transform = TRUE` answers for `mu` on its own scale, which is the
  # inverse of that parameter's link and nothing more. It is not the
  # mean of the response: a binomial `mu` is a probability rather than
  # a count, and a hurdle or zero-inflated `mu` is the base
  # distribution's parameter before any mass is moved to zero.
  # `posterior_epred()` is what answers for `E[Y]`.
  #
  # A model written with `brms::mvbf()` names a family per response,
  # so the link to invert is the one belonging to the response asked
  # for. Unscoped, `family()` answers with the named list that matches
  # the list of predictors.
  apply_mu_linkinv(linpred, model_families(object, resp))
}


#' Apply the inverse link of `mu` to a linear predictor
#'
#' The one rule for putting a mean predictor on the response scale,
#' shared by every response of a multivariate fit.
#'
#' @param linpred A matrix, or a named list of them for a multivariate
#'   fit
#' @param family A family object shared by every response, or a named
#'   list of them keyed by response name
#' @return The same shape, with each element on its parameter's scale
#'
#' @noRd
apply_mu_linkinv <- function(linpred, family) {
  if (is.list(linpred) && !is.matrix(linpred)) {
    # A multivariate fit reaches here two ways. One family named for
    # every response is shared by all of them; families named per
    # response arrive as a list keyed by response name. A family
    # object carries `linkinv`, and a list of them does not, which is
    # what tells the two apart.
    shared <- is.function(family$linkinv)
    out <- lapply(names(linpred), function(resp_name) {
      apply_mu_linkinv(
        linpred[[resp_name]],
        if (shared) family else family[[resp_name]]
      )
    })
    names(out) <- names(linpred)
    return(out)
  }
  family$linkinv(linpred)
}


#' Linear predictor of one distributional parameter
#'
#' Answers for the named parameter rather than for the mean. A
#' parameter given a formula of its own has a linear predictor, which
#' is returned on the link scale unless `transform` asks for the
#' parameter's own scale. A parameter sampled as a scalar has no linear
#' predictor, so its draws are returned broadcast across the rows,
#' which is what brms does in the same position.
#'
#' @inheritParams posterior_linpred.mvgam
#' @param dpar Name of the distributional parameter
#' @param draw_ids Draw indices to answer for, or `NULL` for all
#' @return A `[ndraws x nobs]` matrix
#'
#' @noRd
dpar_posterior_linpred <- function(object, dpar, transform = FALSE,
                                   newdata = NULL, draw_ids = NULL,
                                   resp = NULL) {
  # A distributional parameter belongs to one response's family, and
  # its link is that family's.
  resolve_resp(object, resp, required = TRUE,
               caller = "posterior_linpred(dpar = )")
  family <- model_families(object, resp)
  family_name <- resolve_family_name(family)
  valid <- get_family_dpars(family_name)
  if (!dpar %in% valid) {
    stop(insight::format_error(c(
      paste0(
        "'", dpar, "' is not a distributional parameter of family '",
        family_name, "'."
      ),
      i = if (length(valid) > 0) {
        paste0("Available: ", paste0("'", valid, "'", collapse = ", "), ".")
      } else {
        paste0("Family '", family_name,
               "' has no distributional parameters.")
      }
    )))
  }
  newdata <- newdata %||% mvgam_training_data(object)
  predicted <- predicted_dpar_names(object, dpar, resp = resp)
  if (length(predicted) == 0) {
    # Sampled as a scalar, so there is nothing to transform.
    out <- resolve_family_pars(
      object, dpar_names = dpar,
      ndraws = length(draw_ids %||% seq_len(ndraws(object))),
      nobs = nrow(newdata), draw_ids = draw_ids, newdata = newdata,
      resp = resp
    )
    return(out[[dpar]])
  }
  linpred <- extract_component_linpred(
    mvgam_fit = object, newdata = newdata, component = dpar,
    draw_ids = draw_ids, resp = resp
  )
  if (!transform) {
    return(linpred)
  }
  inv_link(linpred, dpar_link(family, dpar))
}
