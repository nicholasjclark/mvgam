#'@title Compute probabilistic forecast scores for mvgam objects
#'@param object `mvgam_forecast` object. See [forecast.mvgam()].
#'If the test data
#'supplied to \code{forecast.mvgam} contained out of sample test observations, the calibration
#'of probabilistic forecasts can be scored using proper scoring rules
#'@param ... Ignored
#'@param score \code{character} specifying the type of proper scoring rule to use for evaluation. Options are:
#'`sis` (i.e. the Scaled Interval Score), `energy`, `variogram`, `elpd`
#'(i.e. the Expected log pointwise Predictive Density),
#'`drps` (i.e. the Discrete Rank Probability Score) or `crps` (the Continuous Rank Probability Score).
#'Note that when choosing `elpd`, the supplied object must have forecasts on the `link` scale so that
#'expectations can be calculated prior to scoring. For all other scores, forecasts should be supplied
#'on the `response` scale (i.e. posterior predictions)
#'@param log \code{logical}. Should the forecasts and truths be logged prior to scoring?
#'This is often appropriate for comparing
#'performance of models when series vary in their observation ranges
#'@param weights optional \code{vector} of weights (where \code{length(weights) == n_series})
#'for weighting pairwise correlations when evaluating the variogram score for multivariate
#'forecasts. Useful for down-weighting series that have larger magnitude observations or that
#'are of less interest when forecasting. Ignored if \code{score != 'variogram'}
#'@param interval_width proportional value on `[0.05,0.95]` defining the forecast interval
#'for calculating coverage and, if `score = 'sis'`, for calculating the interval score
#'@param n_cores \code{integer} specifying number of cores for calculating scores in parallel
#'@param ... Ignored
#'@return a \code{list} containing scores and interval coverages per forecast horizon.
#'If \code{score %in% c('drps', 'crps', 'elpd')},
#'the list will also contain return the sum of all series-level scores per horizon. If
#'\code{score %in% c('energy','variogram')}, no series-level scores are computed and the only score returned
#'will be for all series. For all scores apart from `elpd`, the `in_interval` column in each series-level
#'slot is a binary indicator of whether or not the true value was within the forecast's corresponding
#'posterior empirical quantiles. Intervals are not calculated when using `elpd` because forecasts
#'will only contain the linear predictors
#'@examples
#'\dontrun{
#'# Simulate observations for three count-valued time series
#'data <- sim_mvgam()
#'# Fit a dynamic model using 'newdata' to automatically produce forecasts
#'mod <- mvgam(y ~ 1,
#'             trend_model = RW(),
#'             data = data$data_train,
#'             newdata = data$data_test,
#'             burnin = 300,
#'             samples = 300,
#'             chains = 2)
#'
#'# Extract forecasts into a 'mvgam_forecast' object
#'fc <- forecast(mod)
#'
#'# Compute Discrete Rank Probability Scores and 0.90 interval coverages
#'fc_scores <- score(fc, score = 'drps')
#'str(fc_scores)
#'}
#'@method score mvgam_forecast
#'@seealso \code{\link{forecast.mvgam}}
#'@export
score.mvgam_forecast = function(object, score = 'crps',
                                log = FALSE, weights,
                                interval_width = 0.9,
                                n_cores = 1,
                                ...){

  score <- match.arg(arg = score,
                           choices = c('crps',
                                       'drps',
                                       'elpd',
                                       'sis',
                                       'energy',
                                       'variogram'))

  if(object$type == 'trend'){
    stop('cannot evaluate accuracy of latent trend forecasts. Use "type == response" when forecasting instead',
         call. = FALSE)
  }

  if(object$type != 'link' & score == 'elpd'){
    stop('cannot evaluate elpd scores unless linear predictors are supplied. Use "type == link" when forecasting instead',
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

  if(score == 'elpd'){
    # Get linear predictor forecasts into the correct format
    linpreds <- do.call(cbind, object$forecasts)

    # Build a dataframe for indexing which series each observation belongs to
    newdata <- data.frame(series = factor(sort(rep(object$series_names,
                                                   NCOL(object$forecasts[[1]]))),
                                          levels = levels(object$series_names)),
                          y = unname(unlist(object$test_observations)))

    class(object) <- c('mvgam', class(object))

    # Calculate log-likelihoods
    elpd_score <- logLik(object = object,
                         linpreds = linpreds,
                         newdata = newdata,
                         family_pars = object$family_pars,
                         n_cores = n_cores)
    elpd_score <- apply(elpd_score, 2, log_mean_exp)

    # Construct series-level score dataframes
    series_score <- lapply(seq_len(n_series), function(series){
      DRPS <- data.frame(drps_mcmc_object(truths[series,],
                                          object$forecasts[[series]],
                                          log = log,
                                          interval_width = interval_width))
      data.frame(score = elpd_score[which(newdata$series ==
                                            levels(object$series_names)[series])],
                 eval_horizon = seq(1, NCOL(object$forecasts[[1]])),
                 score_type = 'elpd')
    })
    names(series_score) <- object$series_names
    all_scores <- data.frame(score = rowSums(do.call(cbind, lapply(seq_len(n_series), function(series){
      series_score[[series]]$score
    }))), eval_horizon = seq(1, NCOL(object$forecasts[[1]])), score_type = 'sum_elpd')
    series_score$all_series <- all_scores
  }

  if(score %in% c('energy', 'variogram')){

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

    if(score == 'variogram'){
      var_score <- variogram_mcmc_object(truths = truths,
                                         fcs = object$forecasts,
                                         log = log,
                                         weights = weights)
      series_score$all_series <- data.frame(score = var_score,
                                            eval_horizon = 1:NCOL(object$forecasts[[1]]),
                                            score_type = 'variogram')
    }

    if(score == 'energy'){
      en_score <- energy_mcmc_object(truths = truths,
                                     fcs = object$forecasts,
                                     log = log)
      series_score$all_series <- data.frame(score = en_score,
                                            eval_horizon = 1:NCOL(object$forecasts[[1]]),
                                            score_type = 'energy')
    }
  }

  if(score == 'sis'){
    series_score <- lapply(seq_len(n_series), function(series){
      SIS <- data.frame(sis_mcmc_object(truths[series,],
                                          object$forecasts[[series]],
                                          log = log,
                                          interval_width = interval_width))
      colnames(SIS) <- c('score','in_interval')
      SIS$interval_width <- interval_width
      SIS$eval_horizon <- seq(1, NCOL(object$forecasts[[1]]))
      SIS$score_type <- 'sis'
      SIS
    })
    names(series_score) <- object$series_names
    all_scores <- data.frame(score = rowSums(do.call(cbind, lapply(seq_len(n_series), function(series){
      series_score[[series]]$score
    }))), eval_horizon = seq(1, NCOL(object$forecasts[[1]])), score_type = 'sum_sis')
    series_score$all_series <- all_scores
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

#'@name score.mvgam_forecast
#'@param object `mvgam_forecast` object. See [forecast.mvgam()].
#'@param ... Ignored
#'@export
score = function(object, ...) UseMethod("score", object)
