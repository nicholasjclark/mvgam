#' Combine forecasts from \pkg{mvgam} models into evenly weighted ensembles
#'
#' Generate evenly weighted ensemble forecast distributions from \code{mvgam_forecast} objects
#'
#'@name ensemble.mvgam_forecast
#'@param object \code{list} object of class \code{mvgam_forecast}. See [forecast.mvgam()]
#'@param ... More \code{mvgam_forecast} objects.
#'@details It is widely recognised in the forecasting literature that combining forecasts
#'from different models often results in improved forecast accuracy. The simplest way to create
#'an ensemble is to use evenly weighted combinations of forecasts from the different models.
#' This is straightforward to do in a Bayesian setting with \pkg{mvgam} as the posterior MCMC draws
#' contained in each \code{mvgam_forecast} object will already implicitly capture correlations among
#' the temporal posterior predictions.
#'@return An object of class \code{mvgam_forecast} containing the ensemble predictions. This
#'object can be readily used with the supplied S3 functions \code{plot} and \code{score}
#'@author Nicholas J Clark
#'@seealso \code{\link{plot.mvgam_forecast}}, \code{\link{score.mvgam_forecast}}
#' @examples
#' \donttest{
#' # Simulate some series and fit a few competing dynamic models
#' set.seed(1)
#' simdat <- sim_mvgam(n_series = 1,
#'                     prop_trend = 0.6,
#'                     mu = 1)
#'
#' plot_mvgam_series(data = simdat$data_train,
#'                  newdata = simdat$data_test)
#'
#' m1 <- mvgam(y ~ 1,
#'             trend_formula = ~ time +
#'               s(season, bs = 'cc', k = 9),
#'             trend_model = AR(p = 1),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             newdata = simdat$data_test,
#'             chains = 2,
#'             silent = 2)
#'
#' m2 <- mvgam(y ~ time,
#'             trend_model = RW(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             newdata = simdat$data_test,
#'             chains = 2,
#'             silent = 2)
#'
#' # Calculate forecast distributions for each model
#' fc1 <- forecast(m1)
#' fc2 <- forecast(m2)
#'
#' # Generate the ensemble forecast
#' ensemble_fc <- ensemble(fc1, fc2)
#'
#' # Plot forecasts
#' plot(fc1)
#' plot(fc2)
#' plot(ensemble_fc)
#'
#' # Score forecasts
#' score(fc1)
#' score(fc2)
#' score(ensemble_fc)
#' }
#'@export
ensemble <- function(object, ...) {
  UseMethod("ensemble", object)
}

#'@rdname ensemble.mvgam_forecast
#'@method ensemble mvgam_forecast
#'@param ndraws Positive integer specifying the number of draws to use from each
#'forecast distribution for creating the ensemble. If some of the ensemble members have
#'fewer draws than `ndraws`, their forecast distributions will be resampled with replacement
#'to achieve the correct number of draws
#'@export
ensemble.mvgam_forecast <- function(object, ..., ndraws = 5000) {
  models <- split_fc_dots(object, ..., model_names = NULL)
  n_models <- length(models)

  # Check that series names and key dimensions match for all forecasts
  allsame <- function(x) length(unique(x)) == 1
  if (!allsame(purrr::map(models, 'series_names'))) {
    stop('Names of series must match for all forecast objects.', call. = FALSE)
  }

  if (!allsame(lapply(models, function(x) length(x$forecasts)))) {
    stop(
      'The number of forecast distributions must match for all forecast objects.',
      call. = FALSE
    )
  }

  if (!allsame(lapply(models, function(x) length(x$test_observations)))) {
    stop('Validation data must match for all forecast objects.', call. = FALSE)
  }

  if (
    !allsame(lapply(models, function(x) {
      unlist(lapply(x$forecasts, function(y) dim(y)[2]), use.names = FALSE)
    }))
  ) {
    stop(
      'Forecast horizons must match for all forecast objects.',
      call. = FALSE
    )
  }

  validate_pos_integer(ndraws)

  # End of checks; now proceed with ensembling
  n_series <- length(models[[1]]$series_names)

  # Calculate total number of forecast draws to sample from for each model
  n_mod_draws <- lapply(seq_len(n_models), function(x) {
    NROW(models[[x]]$forecasts[[1]])
  })

  # Calculate model weights (only option at the moment is even weighting,
  # but this may be relaxed in future)
  mod_weights <- data.frame(
    mod = paste0('mod', 1:n_models),
    orig_weight = 1 / n_models,
    ndraws = unlist(n_mod_draws, use.names = FALSE)
  ) %>%
    # Adjust weights by the number of draws available per
    # forecast, ensuring that models with fewer draws aren't
    # under-represented in the final weighted ensemble
    dplyr::mutate(weight = (orig_weight / ndraws) * 100) %>%
    dplyr::mutate(mod = as.factor(mod)) %>%
    dplyr::select(mod, weight)

  # Create draw indices
  mod_inds <- as.factor(unlist(
    lapply(seq_len(n_models), function(x) {
      rep(paste0('mod', x), NROW(models[[x]]$forecasts[[1]]))
    }),
    use.names = FALSE
  ))
  all_draw_inds <- 1:sum(unlist(n_mod_draws, use.names = FALSE))
  mod_inds_draws <- split(all_draw_inds, mod_inds)

  # Add model-specific weights to the draw indices
  draw_weights <- data.frame(draw = all_draw_inds, mod = mod_inds) %>%
    dplyr::left_join(mod_weights, by = 'mod')

  # Perform multinomial sampling using draw-specific weights
  fc_draws <- sample(
    all_draw_inds,
    size = ndraws,
    replace = max(all_draw_inds) < ndraws,
    prob = draw_weights$weight
  )

  # Create weighted ensemble hindcasts and forecasts
  ens_hcs <- lapply(seq_len(n_series), function(series) {
    all_hcs <- do.call(rbind, lapply(models, function(x) x$hindcasts[[series]]))
    all_hcs[fc_draws, ]
  })

  ens_fcs <- lapply(seq_len(n_series), function(series) {
    all_fcs <- do.call(rbind, lapply(models, function(x) x$forecasts[[series]]))
    all_fcs[fc_draws, ]
  })

  # Initiate the ensemble forecast object
  ens_fc <- models[[1]]

  # Add in hindcasts and forecasts
  ens_fc$hindcasts <- ens_hcs
  ens_fc$forecasts <- ens_fcs
  names(ens_fc$hindcasts) <- names(models[[1]]$hindcasts)
  names(ens_fc$forecasts) <- names(models[[1]]$forecasts)

  # Return
  return(ens_fc)
}

#'@noRd
split_fc_dots = function(x, ..., model_names = NULL, other = TRUE) {
  dots <- list(x, ...)
  names <- substitute(list(x, ...), env = parent.frame())[-1]
  names <- ulapply(names, deparse)

  if (!is.null(model_names)) {
    names <- model_names
  }

  if (length(names)) {
    if (!length(names(dots))) {
      names(dots) <- names
    } else {
      has_no_name <- !nzchar(names(dots))
      names(dots)[has_no_name] <- names[has_no_name]
    }
  }
  is_mvgam_fc <- unlist(lapply(dots, function(y) inherits(y, 'mvgam_forecast')))
  models <- dots[is_mvgam_fc]
  out <- dots[!is_mvgam_fc]

  if (length(out)) {
    stop(
      "Only mvgam_forecast objects can be passed to '...' for this method.",
      call. = FALSE
    )
  }
  models
}
