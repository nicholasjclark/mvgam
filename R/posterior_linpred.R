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
#' @param process_error Logical. `TRUE` adds one sampled innovation to
#'   the trend's deterministic submodel. Used under
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
  has_trend <- !is.null(mvgam_fit$trend_model) &&
               !is.null(mvgam_fit$trend_model$formula)

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
  #
  # A deterministic trend (PW, none) stores no `trend[t, s]` draws.
  # The conditional surface then has no state to take and falls back
  # to the marginal one.
  per_response <- is_response_keyed(mvgam_fit) &&
    is_per_response(obs_linpred)
  trend_linpred <- NULL
  trend_noise <- NULL
  if (identical(trend_state, "conditional")) {
    draws_mat <- subset_draws_rows(fit_draws(mvgam_fit$fit, "trend"),
                                   draw_ids = draw_ids)
    state_for <- function(r) {
      extract_trend_latent_states(
        mvgam_fit = mvgam_fit, newdata = newdata,
        full_draws = draws_mat, resp = r
      )
    }
    trend_linpred <- if (per_response) {
      states <- lapply(stats::setNames(nm = names(obs_linpred)), state_for)
      if (!is.null(states[[1L]])) states
    } else {
      state_for(resp)
    }
  }
  if (is.null(trend_linpred)) {
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
  if (!identical(dim(trend_mat), dim(obs_mat))) {
    stop_shape_fault(paste0("The trend predictor", what), dim(trend_mat),
                     dim(obs_mat))
  }
  out <- obs_mat + trend_mat
  if (!is.null(trend_noise)) {
    if (!identical(dim(trend_noise), dim(out))) {
      stop_shape_fault(paste0("The trend noise", what), dim(trend_noise),
                       dim(out))
    }
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
#' terms of `trend_formula`. `process_error = TRUE` adds one sampled
#' innovation per posterior draw, on the link scale.
#' [posterior_epred.mvgam()] and [posterior_predict.mvgam()] apply the
#' family's mean and noise to that same predictor.
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
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
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
