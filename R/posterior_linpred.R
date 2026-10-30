#' The internal surface name for a user's `incl_autocor`
#'
#' Every user-facing method takes `incl_autocor`.
#' `get_combined_linpred()` takes `trend_state`. The translation lives
#' here alone. Two spellings with separate defaults once offered one
#' choice that disagreed with itself.
#'
#' @param incl_autocor Logical. `TRUE` conditions on the fitted latent
#'   state. `FALSE` takes the deterministic submodels.
#'
#' @return Either `"conditional"` or `"marginal"`.
#'
#' @noRd
autocor_to_trend_state <- function(incl_autocor) {
  checkmate::assert_logical(incl_autocor, len = 1L, any.missing = FALSE)
  if (isTRUE(incl_autocor)) "conditional" else "marginal"
}


#' The observation and trend predictors summed on the link scale
#'
#' The observation predictor, the trend predictor and the process
#' errors come from three extractions. Each takes the same draw
#' indices. A row of the sum then pairs effects from one iteration.
#'
#' @param mvgam_fit mvgam object from mvgam()
#' @param newdata data.frame with prediction covariates
#' @param process_error Logical. `TRUE` adds sampled innovations, one
#'   per time and series, to the trend's covariate part. Used under
#'   `trend_state = "marginal"` alone.
#' @param trend_state Which surface the trend contribution comes from.
#'   `"marginal"` takes the deterministic submodel, as the
#'   `posterior_*()` methods do. `"conditional"` takes the fitted
#'   `trend[t, s]` draws, as `hindcast()` and the likelihood do.
#' @param draw_ids Integer vector of posterior draws to use (NULL = all)
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   [posterior_epred.mvgam()] takes them
#' @param resp Character; response name for multivariate models (NULL = all)
#'
#' @return Matrix `\\[ndraws x nobs\\]` of combined linear predictor
#'   values on the link scale. A multivariate model with `resp = NULL`
#'   gives a named list of matrices.
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
  has_trend <- has_trend_formula(mvgam_fit)

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
  if (!has_trend) {
    return(obs_linpred)
  }

  # The conditional surface takes the fitted `trend[t, s]` draws. The
  # trend kernel is written on the centred convention
  # `(trend - mu_trend)`. Those draws then already carry the trend
  # formula's contribution. Adding the deterministic submodel would
  # count it twice. A row whose time falls outside the fitted grid is
  # refused.
  #
  # A response-keyed fit holds one `trend[t, s]` column per response.
  # The function extracts its state once per response. A multivariate
  # fit whose series axis is a column of the frame holds one state that
  # every response takes. The function extracts that state once. The
  # process noise below follows the same rule.
  per_response <- is_response_keyed(mvgam_fit) &&
    is_per_response(obs_linpred)
  trend_noise <- NULL
  if (identical(trend_state, "conditional")) {
    draw_idx <- resolve_draw_indices(posterior::ndraws(mvgam_fit), NULL,
                                     draw_ids)
    state_for <- function(r) {
      trend_state_rows(mvgam_fit, newdata, draw_idx, resp = r)
    }
    trend_linpred <- if (per_response) {
      lapply(stats::setNames(nm = names(obs_linpred)), state_for)
    } else {
      state_for(resp)
    }
    # Every trend writes `trend[t, s]` and every fit keeps it
    if (is.null(trend_linpred) || any(vapply(trend_linpred, is.null,
                                             logical(1L)))) {
      stop_mvgam_fault(
        "A fit with a trend must hold the fitted 'trend[t, s]' draws.",
        "The conditional surface is built from the fitted trend."
      )
    }
  } else {
    # The deterministic submodel, plus one sample of the trend's process
    # noise. A trend without a stochastic component, such as `PW()`,
    # takes no noise.
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
    }
  }

  if (is_per_response(obs_linpred)) {
    # The trend and its noise each arrive as one matrix or as one per
    # response. One matrix serves every response. The marginal surface
    # of a wide fit gives that for the deterministic part. The trend
    # design of a wide frame runs at time grain. A covariate column
    # there holds one value per time. A list holds each response's own
    # trend, as the conditional state and a wide frame's noise do.
    own <- function(x, r) {
      if (is_per_response(x)) x[[r]] else x
    }
    return(lapply(stats::setNames(nm = names(obs_linpred)), function(r) {
      compose_linpred_with_noise(
        obs_mat = obs_linpred[[r]], trend_mat = own(trend_linpred, r),
        trend_noise = own(trend_noise, r), resp_name = r
      )
    }))
  }
  compose_linpred_with_noise(
    obs_mat = obs_linpred, trend_mat = trend_linpred,
    trend_noise = trend_noise, resp_name = NULL
  )
}


#' Whether a fit carries a trend model with a formula
#'
#' @param mvgam_fit A fitted `mvgam` object
#' @return A single logical
#' @noRd
has_trend_formula <- function(mvgam_fit) {
  !is.null(mvgam_fit$trend_model$formula)
}


#' Each part of the conditional linear predictor, on the link scale
#'
#' The parts are the observation formula's terms, the trend formula's
#' terms and the latent state, as `latent_trend_parts()` parts it.
#' Within each draw they sum to
#' `get_combined_linpred(trend_state = "conditional")`. The latent state
#' is the fitted one. Every row must fall on a fitted time. An ordinal
#' response's category-specific terms shift its thresholds. Those
#' shifts form a fourth group, as `extract_threshold_shifts()` gives
#' them. They add nothing to the predictor.
#'
#' @inheritParams get_combined_linpred
#' @return A list of `obs`, `trend`, `latent` and `cs`. The first three
#'   are named lists of `[ndraws x nobs]` matrices. `cs` is a named list
#'   of `[ndraws x nobs x nthres]` threshold shifts. A fit without a
#'   trend leaves `trend` and `latent` empty. A response without a
#'   category-specific term leaves `cs` empty. A multivariate model with
#'   `resp = NULL` gives one such list per response.
#' @noRd
get_linpred_parts <- function(mvgam_fit, newdata, draw_ids = NULL,
                              re_formula = NULL, allow_new_levels = FALSE,
                              sample_new_levels = "uncertainty",
                              resp = NULL) {
  parts_of <- function(component) {
    extract_component_parts(
      mvgam_fit = mvgam_fit, newdata = newdata, component = component,
      resp = resp, draw_ids = draw_ids, re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
  }
  obs <- parts_of("obs")
  has_trend <- has_trend_formula(mvgam_fit)
  # The trend formula has one response, which every response shares
  trend <- if (has_trend) parts_of("trend") else list()
  draw_idx <- resolve_draw_indices(posterior::ndraws(mvgam_fit), NULL,
                                   draw_ids)
  latent_for <- function(r) {
    if (has_trend) latent_trend_parts(mvgam_fit, newdata, draw_idx, r)
    else list()
  }
  shifts_for <- function(r) {
    extract_threshold_shifts(
      mvgam_fit, newdata, resp = r, draw_ids = draw_ids,
      re_formula = re_formula, allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
  }
  if (!asks_each_response(mvgam_fit, resp)) {
    return(list(obs = obs, trend = trend, latent = latent_for(resp),
                cs = shifts_for(resp)))
  }
  # A response-keyed fit holds each response's own state. Any other
  # fit holds one state that every response takes.
  keyed <- is_response_keyed(mvgam_fit)
  shared <- if (!keyed) latent_for(NULL)
  out <- list()
  for (r in names(obs)) {
    out[[r]] <- list(obs = obs[[r]], trend = trend,
                     latent = if (keyed) latent_for(r) else shared,
                     cs = shifts_for(r))
  }
  out
}


#' Each part of the conditional expectation, on the response scale
#'
#' A part's contribution is the fit's expectation from the whole
#' predictor less its expectation from the predictor without that part.
#' The expectation is the family's own, as `epred_map()` gives it, with
#' an ordinal response's expected ordered level. A category-specific
#' term's contribution is the expectation less the expectation with its
#' threshold shift removed. Under the identity link
#' of a family whose mean is its predictor, the contributions are the
#' link-scale parts and sum to the expectation. Under any other link
#' each contribution holds the others at their fitted values. Those
#' contributions need not sum to the expectation.
#'
#' @inheritParams get_linpred_parts
#' @return As `get_linpred_parts()`, each part on the response scale,
#'   with `expected`, the `[ndraws x nobs]` expectation from the whole
#'   predictor
#' @noRd
get_epred_parts <- function(mvgam_fit, newdata, draw_ids = NULL,
                            re_formula = NULL, allow_new_levels = FALSE,
                            sample_new_levels = "uncertainty",
                            resp = NULL) {
  parts <- get_linpred_parts(
    mvgam_fit, newdata, draw_ids = draw_ids, re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels, resp = resp
  )
  on_response <- function(p, r) {
    whole <- Reduce(`+`, c(p$obs, p$trend, p$latent))
    expect <- epred_map(
      mvgam_fit, newdata, draw_ids, r, dim(whole), ordinal = "mean",
      re_formula = re_formula, allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    )
    expected <- expect(whole)
    contribution <- function(part) expected - expect(whole - part)
    shift <- if (length(p$cs)) Reduce(`+`, p$cs)
    shift_contribution <- function(part) {
      expected - expect(whole, shift = shift - part)
    }
    list(
      expected = expected,
      obs = lapply(p$obs, contribution),
      trend = lapply(p$trend, contribution),
      latent = lapply(p$latent, contribution),
      cs = lapply(p$cs, shift_contribution)
    )
  }
  if (!asks_each_response(mvgam_fit, resp)) {
    return(on_response(parts, resp))
  }
  Map(on_response, parts, names(parts))
}


#' The observation and trend linear predictors summed per draw
#'
#' Adds the marginal trend-noise sample when one is supplied. A shape
#' mismatch names the response when `resp_name` is set.
#'
#' @param obs_mat Numeric draws-by-observations matrix of the
#'   observation linear predictor
#' @param trend_mat Numeric matrix of the trend linear predictor, shaped
#'   as `obs_mat`
#' @param trend_noise Numeric matrix of trend noise shaped as `obs_mat`,
#'   or `NULL`
#' @param resp_name Character response name for the error message, or
#'   `NULL`
#' @return Numeric matrix shaped as `obs_mat`
#' @noRd
compose_linpred_with_noise <- function(obs_mat, trend_mat, trend_noise,
                                        resp_name = NULL) {
  checkmate::assert_matrix(obs_mat)
  checkmate::assert_matrix(trend_mat)
  checkmate::assert_matrix(trend_noise, null.ok = TRUE)
  checkmate::assert_string(resp_name, null.ok = TRUE)
  what <- if (is.null(resp_name)) "" else paste0(" of '", resp_name, "'")
  assert_dims(trend_mat, paste0("The trend predictor", what), dim(obs_mat))
  out <- obs_mat + trend_mat
  if (!is.null(trend_noise)) {
    assert_dims(trend_noise, paste0("The trend noise", what), dim(out))
    out <- out + trend_noise
  }
  out
}


#' Linear predictor draws for each posterior draw
#'
#' @description
#' Draws of the linear predictor \eqn{\eta}, the sum of the observation
#' and trend predictors on the link scale. A Poisson predictor is on the
#' log scale and a binomial predictor on the logit scale.
#' [posterior_epred.mvgam()] maps it onto the response scale.
#'
#' @param object A fitted mvgam object from [mvgam()].
#' @param transform Logical. `FALSE` (the default) returns the link
#'   scale. `TRUE` applies the inverse link of `mu`. That gives `mu` on
#'   its own scale. A binomial `mu` is a probability. A hurdle or
#'   zero-inflated `mu` is the base distribution's parameter. Neither is
#'   the mean of the response, which [posterior_epred.mvgam()] returns.
#' @inheritParams posterior_epred.mvgam
#' @param dpar Character naming a distributional parameter, such as
#'   `"sigma"` or `"nu"`. The default `NULL` returns the predictor of
#'   the mean. A parameter with a formula of its own returns its linear
#'   predictor. That predictor holds no trend term. A parameter sampled
#'   as a scalar has no link scale. Its draws fill every row on their
#'   own scale, whatever `transform` is.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An `[ndraws, nobs]` matrix, one row per draw and one column
#'   per row of `newdata`. It is on the link scale unless `transform =
#'   TRUE` or `dpar` names a scalar parameter. A multivariate model
#'   with `resp = NULL` gives a named list, one element per response.
#'
#' @details
#' The observation predictor holds the fixed, group-level, smooth and
#' Gaussian process terms of `formula`. The trend predictor holds the
#' terms of `trend_formula`. `process_error = TRUE` adds a sampled
#' deviation at each time and series, on the link scale.
#' [posterior_epred.mvgam()] and [posterior_predict.mvgam()] apply the
#' family's mean and noise to that same predictor.
#'
#' An ordinal response's category-specific `cs()` terms give each
#' threshold its own shift. The predictor holds the terms every
#' threshold shares. [posterior_epred.mvgam()] and
#' [posterior_predict.mvgam()] subtract the shifts from the thresholds,
#' as brms does.
#'
#' @template prediction-trend-state
#'
#' @seealso [posterior_epred.mvgam()] returns expected values.
#'   [posterior_predict.mvgam()] returns predictive draws.
#'   [hindcast.mvgam()] and [forecast.mvgam()] take the trend from its
#'   fitted state. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   scores predictions of held-out times.
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
#' # Log rates, one row per draw and one column per observation
#' lp <- posterior_linpred(mod, draw_ids = 1:50)
#' dim(lp)
#'
#' # A Poisson mean is the inverse link of the predictor
#' all.equal(exp(lp), posterior_epred(mod, draw_ids = 1:50))
#'
#' # The predictor under the fitted trend state tracks the series
#' lp_state <- posterior_linpred(mod, incl_autocor = TRUE)
#' library(ggplot2)
#' ggplot(simdat$data_train, aes(time, log1p(y))) +
#'   geom_point(colour = "grey60") +
#'   geom_line(aes(y = colMeans(lp_state)), colour = "#8F2727",
#'             linewidth = 1) +
#'   labs(y = "log(1 + count)") +
#'   mvgam_theme()
#'
#' # Next: posterior_epred() maps the predictor to expected counts
#' }
#'
#' @importFrom brms posterior_linpred
#' @family predictions
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
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "posterior_linpred")
  checkmate::assert_flag(transform)
  checkmate::assert_flag(process_error)
  trend_state <- autocor_to_trend_state(incl_autocor)
  checkmate::assert_string(dpar, null.ok = TRUE)
  validate_draw_selectors(ndraws, draw_ids)
  rlang::check_dots_empty()

  # A count becomes indices here. Every extraction below takes them.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

  # A distributional parameter carries no trend. mvgam refuses a trend
  # formula on one.
  if (!is.null(dpar)) {
    return(dpar_posterior_linpred(
      object, dpar = dpar, transform = transform, newdata = newdata,
      draw_ids = draw_ids, resp = resp,
      re_formula = re_formula, allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels
    ))
  }

  linpred <- get_combined_linpred(
    mvgam_fit = object,
    newdata = prediction_frame(object, newdata),
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
  # Each response takes the inverse link of its own family.
  families <- model_families(object, resp)
  if (is_per_response(linpred)) {
    return(lapply(stats::setNames(nm = names(linpred)), function(r) {
      families[[r]]$linkinv(linpred[[r]])
    }))
  }
  families$linkinv(linpred)
}


#' Linear predictor of one distributional parameter
#'
#' A parameter with a formula of its own has a linear predictor. It is
#' returned on the link scale unless `transform` asks for the
#' parameter's own scale. A parameter sampled as a scalar has no linear
#' predictor. Its draws fill every row on their own scale.
#'
#' @inheritParams posterior_linpred.mvgam
#' @param dpar Name of the distributional parameter
#' @param draw_ids Draw indices to use, or `NULL` for all
#' @return A `[ndraws x nobs]` matrix
#'
#' @noRd
dpar_posterior_linpred <- function(object, dpar, transform = FALSE,
                                   newdata = NULL, draw_ids = NULL,
                                   resp = NULL,
                                   re_formula = NULL,
                                   allow_new_levels = FALSE,
                                   sample_new_levels = "uncertainty") {
  # A distributional parameter belongs to one response's family. Its
  # link is that family's.
  resolve_resp(object, resp, required = TRUE,
               caller = "posterior_linpred(dpar = )")
  family <- model_families(object, resp)
  family_name <- resolve_family_name(family)
  valid <- get_family_dpars(family_name)
  if (!dpar %in% valid) {
    stop(insight::format_error(c(
      paste0("'dpar' must be a distributional parameter of family '",
             family_name, "'."),
      x = paste0("Got '", dpar, "'."),
      i = if (length(valid) > 0) {
        paste0("Available: ", paste0("'", valid, "'", collapse = ", "), ".")
      } else {
        paste0("Family '", family_name,
               "' has only its mean parameter.")
      }
    )), call. = FALSE)
  }
  newdata <- prediction_frame(object, newdata)
  if (length(predicted_dpar_names(object, dpar, resp = resp)) == 0) {
    draw_ids <- resolve_draw_indices(posterior::ndraws(object), NULL,
                                     draw_ids)
    out <- resolve_family_pars(
      object, dpar_names = dpar,
      ndraws = length(draw_ids),
      nobs = nrow(newdata), draw_ids = draw_ids, newdata = newdata,
      resp = resp
    )
    return(out[[dpar]])
  }
  linpred <- extract_component_linpred(
    mvgam_fit = object, newdata = newdata, component = dpar,
    draw_ids = draw_ids, resp = resp,
    re_formula = re_formula, allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )
  if (!transform) {
    return(linpred)
  }
  inv_link(linpred, dpar_link(family, dpar))
}
