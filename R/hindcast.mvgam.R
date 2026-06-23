# hindcast.mvgam: posterior hindcasts at the training time grid.
# Thin wrapper around the existing build_hindcast_arms() helper
# in R/forecast.mvgam.R; returns an mvgam_forecast object with
# train_* / hindcasts slots populated and test_* / forecasts
# slots NULL.
#
# Semantic note: by default hindcasts read the Stan-fitted latent
# state directly from the posterior
# (`resample_innovations = FALSE`). `trend[t, s]`,
# `mu_trend[t, s]`, and `innovations_trend[t, s]` are all stored
# in the Stan output, so reconstruction is exact. This surfaces
# overfit-vs-predict gaps directly: a random walk with sigma -> 0
# absorbs the response into the trend, so its hindcast response
# draws hug the training values even when the corresponding
# forecast() would be flat. Set `resample_innovations = TRUE` to
# draw fresh innovations on top of the linpred instead (the
# marginal MC convention that `posterior_predict(newdata = ...)`
# uses by default).


#' Generic for posterior hindcasts
#'
#' @param object A model fit object.
#' @param ... Method-specific arguments.
#'
#' @return Method-specific.
#'
#' @export
hindcast <- function(object, ...) {
  UseMethod("hindcast", object)
}


#' Posterior hindcasts for an mvgam state-space model
#'
#' Draw posterior hindcasts at the training time points. The
#' fitted latent state is read directly from the posterior draws
#' and the observation family then maps the trajectory back to
#' the response scale. For predictions at unseen times use
#' [forecast.mvgam].
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param ... Currently unused.
#' @param type One of `"response"`, `"link"`, `"expected"`,
#'   `"trend"`. `"response"` samples from the observation family
#'   (the default); `"expected"` returns the family's mean
#'   (`linkinv(eta)`); `"link"` returns the link-scale linpred;
#'   `"trend"` returns the latent-trend trajectory on the link
#'   scale.
#' @param ndraws Optional integer; the number of posterior draws
#'   to use. Defaults to all available draws.
#' @param obs_uncertainty Logical. When `FALSE`, skips
#'   observation-family sampling for `type = "response"`,
#'   returning the family mean (`linkinv(eta)`) instead.
#'   Defaults to `TRUE`.
#' @param resample_innovations Logical. When `FALSE` (the
#'   default), hindcasts use the Stan-fitted latent state directly
#'   (`trend[t, s]` and `mu_trend[t, s]` read from the posterior);
#'   `type = "response"` then samples only observation-family
#'   noise on top of the exact fitted state. When `TRUE`, fresh
#'   innovations are drawn from the trend's covariance structure
#'   and added to the linpred, matching the marginal Monte Carlo
#'   convention used by `posterior_epred()` /
#'   `posterior_predict()` with `process_error = TRUE`.
#'
#' @return An object of class [mvgam_forecast-class][mvgam].
#'   Hindcasts are returned as a named list of `[ndraws, n_times]`
#'   matrices, one per series; `forecasts` / `test_*` slots are
#'   `NULL`.
#'
#' @seealso [mvgam_forecast-class][mvgam], [forecast.mvgam],
#'   [posterior_predict.mvgam] and [posterior_epred.mvgam] for the
#'   alternative marginal-MC prediction surface that integrates
#'   over the trend's stochastic dynamics instead of reading the
#'   fitted latent state.
#' @examples
#' \donttest{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 80L, trend_model = AR())
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' hc <- hindcast(mod)
#' plot(hc)
#' }
#'
#' @method hindcast mvgam
#' @export
hindcast.mvgam <- function(object,
                           ...,
                           type = c("response", "link",
                                    "expected", "trend"),
                           ndraws = NULL,
                           obs_uncertainty = TRUE,
                           resample_innovations = FALSE,
                           resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  type <- match.arg(type)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(obs_uncertainty)
  checkmate::assert_flag(resample_innovations)
  checkmate::assert_string(resp, null.ok = TRUE)

  # Multivariate fan-out via the shared helper: hindcast operates
  # on the obs-side posterior, which returns a per-response list
  # on mv fits; scope per response and return a named list of
  # `mvgam_forecast` objects.
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) return(fan)

  series_info <- resolve_series_info(object)
  series_levels <- series_info$series_levels

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)
  if (is.null(ndraws)) ndraws <- total_draws
  if (ndraws > total_draws) {
    stop(insight::format_error(c(
      "'ndraws' exceeds the number of posterior draws.",
      x = paste0("Got ndraws = ", ndraws,
                 ", total draws = ", total_draws, "."),
      i = "Use a smaller value or set ndraws = NULL to use all draws."
    )))
  }
  draw_idx <- if (ndraws == total_draws) {
    seq_len(total_draws)
  } else {
    sort(sample.int(total_draws, ndraws))
  }

  training <- build_training_arms(object, series_levels)
  hindcasts <- build_hindcast_arms(
    object, training, type, draw_idx, obs_uncertainty,
    resample_innovations = resample_innovations,
    resp = resp
  )

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draws_mat, draw_idx,
                                    resp = resp)
  } else {
    NULL
  }

  structure(
    list(
      family = object$family$family,
      family_pars = family_pars,
      type = type,
      series_names = factor(series_levels,
                            levels = series_levels),
      train_observations = training$observations,
      train_times = training$times,
      test_observations = NULL,
      test_times = NULL,
      hindcasts = hindcasts,
      forecasts = NULL
    ),
    class = "mvgam_forecast"
  )
}
