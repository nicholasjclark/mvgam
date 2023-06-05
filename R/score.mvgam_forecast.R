#'@title Compute probabilistic forecast scores for mvgam objects
#'@name score.mvgam_forecast
#'@param object \code{list} object returned from \code{forecast.mvgam}. If the test data
#'supplied to \code{forecast.mvgam} contained out of sample test observations, the calibration
#'of probabilistic forecasts can be scored using proper scoring rules
#'@param score \code{character} specifying the type of ranked probability score to use for evaluation. Options are:
#'`variogram`, `drps` or `crps`
#'@param log \code{logical}. Should the forecasts and truths be logged prior to scoring?
#'This is often appropriate for comparing
#'performance of models when series vary in their observation ranges
#'@param weights optional \code{vector} of weights (where \code{length(weights) == n_series})
#'for weighting pairwise correlations when evaluating the variogram score for multivariate
#'forecasts. Useful for down-weighting series that have larger magnitude observations or that
#'are of less interest when forecasting. Ignored if \code{score != 'variogram'}
#'@param interval_width proportional value on `[0.05,0.95]` defining the forecast interval
#'for calculating coverage
#'@return a \code{list} containing scores and 90% interval coverages per forecast horizon.
#'If \code{score %in% c('drps', 'crps')},
#'the list will also contain return the sum of all series-level scores per horizon. If
#'\code{score == 'variogram'}, no series-level scores are computed and the only score returned
#'will be for all series. For all scores, the `in_interval` column in each series-level
#'slot is a binary indicator of whether or not the true value was within the forecast's corresponding
#'posterior empirical quantiles
NULL
#'@export
score <- function(x, what, ...){
  UseMethod("score")
}

#'@rdname score.mvgam_forecast
#'@method score mvgam_forecast
#'@export
score.mvgam_forecast = function(object, score, log = FALSE, weights,
                                interval_width = 0.9){

  if(object$type == 'trend'){
    stop('cannot evaluate accuracy of latent trend forecasts. Use "type == response" when forecasting instead',
         call. = FALSE)
  }

  if(interval_width < 0.05 || interval_width > 0.95){
    stop('interval width must be between 0.05 and 0.95, inclusive')
  }

  # Get truths (out of sample) into correct format
  n_series <- length(object$series_names)
  truths <- do.call(rbind, lapply(seq_len(n_series), function(series){
    object$test_observations[[series]]
  }))

  if(score == 'variogram'){

    if(missing(weights)){
      weights <- rep(1, length(object$series_names))
    }

    # Calculate coverage using one of the univariate scores
    series_score <- lapply(seq_len(n_series), function(series){
      DRPS <- data.frame(drps_mcmc_object(truths[series,],
                                                  object$forecasts[[series]],
                                                  log = log,
                                                  interval_width = interval_width))
      colnames(DRPS) <- c('score','in_interval')
      DRPS$interval_width <- interval_width
      DRPS$eval_horizon <- seq(1, NCOL(object$forecasts[[1]]))
      DRPS[,2:4]
    })
    names(series_score) <- object$series_names

    var_score <- variogram_mcmc_object(truths = truths,
                                                  fcs = object$forecasts,
                                                  log = log,
                                                  weights = weights)
    series_score$all_series <- data.frame(score = var_score,
                                      eval_horizon = 1:NCOL(object$forecasts[[1]]),
                                      score_type = 'variogram')
  }

  if(score == 'drps'){
    series_score <- lapply(seq_len(n_series), function(series){
      DRPS <- data.frame(drps_mcmc_object(truths[series,],
                                          object$forecasts[[series]],
                                          log = log,
                                          interval_width = interval_width))
      colnames(DRPS) <- c('score','in_interval')
      DRPS$interval_width <- interval_width
      DRPS$eval_horizon <- seq(1, NCOL(object$forecasts[[1]]))
      DRPS$score_type <- 'drps'
      DRPS
    })
    names(series_score) <- object$series_names
    all_scores <- data.frame(score = rowSums(do.call(cbind, lapply(seq_len(n_series), function(series){
      series_score[[series]]$score
    }))), eval_horizon = seq(1, NCOL(object$forecasts[[1]])), score_type = 'sum_drps')
    series_score$all_series <- all_scores
  }

  if(score == 'crps'){
    series_score <- lapply(seq_len(n_series), function(series){
      CRPS <- data.frame(crps_mcmc_object(truths[series,],
                                                  object$forecasts[[series]],
                                                  log = log,
                                                  interval_width = interval_width))
      colnames(CRPS) <- c('score','in_interval')
      CRPS$interval_width <- interval_width
      CRPS$eval_horizon <- seq(1, NCOL(object$forecasts[[1]]))
      CRPS$score_type <- 'crps'
      CRPS
    })
    names(series_score) <- object$series_names
    all_scores <- data.frame(score = rowSums(do.call(cbind, lapply(seq_len(n_series), function(series){
      series_score[[series]]$score
    }))), eval_horizon = seq(1, NCOL(object$forecasts[[1]])), score_type = 'sum_crps')
    series_score$all_series <- all_scores
  }

  series_score
}
