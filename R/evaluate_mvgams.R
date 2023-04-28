#'Evaluate forecasts from fitted mvgam objects
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@param n_samples \code{integer} specifying the number of samples to generate from the model's
#'posterior distribution
#'@param eval_timepoint \code{integer} indexing the timepoint that represents our last 'observed'
#'set of outcome data
#'@param fc_horizon \code{integer} specifying the length of the forecast horizon for evaluating forecasts
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@param score \code{character} specifying the type of ranked probability score to use for evaluation. Options are:
#'`variogram`, `drps` or `crps`
#'@param log \code{logical}. Should the scores be logged? This is often appropriate for comparing
#'performance of models when series vary in their observation ranges
#'@details `eval_mvgam` generates a set of samples representing fixed parameters estimated from the full
#'\code{mvgam} model and latent trend states at a given point in time. The trends are rolled forward
#'a total of \code{fc_horizon} timesteps according to their estimated state space dynamics to
#'generate an 'out-of-sample' forecast that is evaluated against the true observations in the horizon window.
#'This function therefore simulates a situation where the model's parameters had already been estimated but
#'we have only observed data up to the evaluation timepoint and would like to generate forecasts from the
#'latent trends that have been observed up to that timepoint. Evaluation involves calculating an
#'appropriate Rank Probability Score and a binary indicator
#'for whether or not the true value lies within the forecast's 90% prediction interval
#'
#'`roll_eval_mvgam` sets up a sequence of evaluation timepoints along a rolling window and iteratively
#'calls \code{eval_mvgam} to evaluate 'out-of-sample' forecasts.
#'Evaluation involves calculating the Discrete Rank Probability Score and a binary indicator
#'for whether or not the true value lies within the forecast's 90% prediction interval
#'
#'`compare_mvgams` automates the evaluation to compare two fitted models using rolling window forecast evaluation and
#'provides a series of summary plots to facilitate model selection. It is essentially a wrapper for
#'\code{roll_eval_mvgam}
#'@return For `eval_mvgam`, a \code{list} object containing information on specific evaluations for each series
#'(if using `drps` or `crps` as the score) or a vector of scores when using `variogram`.
#'
#'For `roll_eval_mvgam`, a \code{list} object containing information on specific evaluations for each series as well as
#'a total evaluation summary (taken by summing the forecast score for each series at each evaluation and averaging
#'the coverages at each evaluation)
#'
#'For `compare_mvgams`, a series of plots comparing forecast Rank Probability Scores for each competing
#'model. A lower score is preferred. Note however that it is possible to select a model that ultimately
#'would perform poorly in true out-of-sample forecasting. For example if a wiggly smooth function of 'year'
#'is included in the model then this function will be learned prior to evaluating rolling window forecasts,
#'and the model could generate very tight predictions as a result. But when forecasting ahead to timepoints
#'that the model has not seen (i.e. next year), the smooth function will end up extrapolating, sometimes
#'in very strange and unexpected ways. It is therefore recommended to only use smooth functions for
#'covariates that are adequately measured in the data (i.e. 'seasonality', for example) to reduce possible
#'extrapolation of smooths and let the latent trends in the \code{mvgam} model capture any
#'temporal dependencies in the data. These trends are time series models and so will provide much more
#'stable forecasts
#'
#' @name evaluate_mvgams
NULL

#' @rdname evaluate_mvgams
#' @export
eval_mvgam = function(object,
                      n_samples = 5000,
                      eval_timepoint = 3,
                      fc_horizon = 3,
                      n_cores = 2,
                      score = 'drps',
                      log = TRUE){

  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(object$trend_model == 'None'){
    stop('cannot compute rolling forecasts for mvgams that have no trend model',
         call. = FALSE)
  }

  if(sign(fc_horizon) != 1){
    stop('argument "fc_horizon" must be a positive integer',
         call. = FALSE)
  } else {
    if(fc_horizon%%1 != 0){
      stop('argument "fc_horizon" must be a positive integer',
           call. = FALSE)
    }
  }

  if(sign(eval_timepoint) != 1){
    stop('argument "eval_timepoint" must be a positive integer',
         call. = FALSE)
  } else {
    if(eval_timepoint%%1 != 0){
      stop('argument "eval_timepoint" must be a positive integer',
           call. = FALSE)
    }
  }

  if(eval_timepoint < 3){
    stop('argument "eval_timepoint" must be >= 3',
         call. = FALSE)
  }


  #### 1. Generate linear predictor matrix for covariates and extract trend estimates at timepoint
  data_train <- object$obs_data
  n_series <- NCOL(object$ytimes)

  # Check evaluation timepoint
  if(class(object$obs_data)[1] == 'list'){
    all_times <- (data.frame(time = object$obs_data$time)  %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)

  } else {
    all_times <- (object$obs_data %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)
  }

  if(!eval_timepoint %in% all_times){
    stop('Evaluation timepoint does not exist in original training data')
  }

  # Filter training data to correct point (just following evaluation timepoint)
  data.frame(time = object$obs_data$time,
             series = object$obs_data$series) %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::left_join(data.frame(time = object$obs_data$time,
                                series = object$obs_data$series),
                     by = c('time', 'series')) %>%
    dplyr::arrange(time, series) %>%
    dplyr::filter(time > (eval_timepoint) &
                    time <= (eval_timepoint + fc_horizon)) %>%
    dplyr::pull(row_number) -> assim_rows

  if(class(object$obs_data)[1] == 'list'){

    data_assim <- lapply(object$obs_data, function(x){
      if(is.matrix(x)){
        matrix(x[assim_rows, ],
               ncol = NCOL(x))
      } else {
        x[assim_rows]
      }

    })


  } else {
    object$obs_data[assim_rows, ] -> data_assim
  }

  # Generate linear predictor matrix from fitted mgcv model
  suppressWarnings(Xp  <- try(predict(object$mgcv_model,
                                      newdata = data_assim,
                                      type = 'lpmatrix'),
                              silent = TRUE))

  if(inherits(Xp, 'try-error')){
    testdat <- data.frame(series = data_assim$series)

    terms_include <- names(object$mgcv_model$coefficients)[which(!names(object$mgcv_model$coefficients)
                                                                 %in% '(Intercept)')]
    if(length(terms_include) > 0){
      newnames <- vector()
      newnames[1] <- 'series'
      for(i in 1:length(terms_include)){
        testdat <- cbind(testdat, data.frame(data_assim[[terms_include[i]]]))
        newnames[i+1] <- terms_include[i]
      }
      colnames(testdat) <- newnames
    }

    suppressWarnings(Xp  <- predict(object$mgcv_model,
                                    newdata = testdat,
                                    type = 'lpmatrix'))
  }

  # Beta coefficients for GAM component
  betas <- mvgam:::mcmc_chains(object$model_output, 'b')

  # Family-specific parameters
  family <- object$family
  family_pars <- mvgam:::extract_family_pars(object = object)

  # Trend model
  trend_model <- object$trend_model
  use_lv <- object$use_lv

  # Trend-specific parameters; keep only the trend / lv estimates
  # up to the specific evaluation timepoint
  trend_pars <- mvgam:::extract_trend_pars(object = object,
                                           keep_all_estimates = FALSE,
                                           ending_time = eval_timepoint)

  # Generate sample sequence for n_samples
  if(n_samples < dim(betas)[1]){
    sample_seq <- sample(seq_len(dim(betas)[1]), size = n_samples, replace = F)
  } else {
    sample_seq <- sample(seq_len(dim(betas)[1]), size = n_samples, replace = T)
  }

  #### 2. Run trends forward fc_horizon steps to generate the forecast distribution ####
  use_lv <- object$use_lv
  upper_bounds <- object$upper_bounds
  trend_model <- object$trend_model

  # Run particles forward in time to generate their forecasts
  if(n_cores > 1){
    cl <- parallel::makePSOCKcluster(n_cores)
    setDefaultCluster(cl)
    clusterExport(NULL, c('use_lv',
                          'family',
                          'fc_horizon',
                          'data_assim',
                          'Xp',
                          'betas',
                          'trend_model',
                          'trend_pars',
                          'family_pars',
                          'n_series',
                          'upper_bounds'),
                  envir = environment())

    pbapply::pboptions(type = "none")
    draw_fcs <- pbapply::pblapply(sample_seq, function(x){

      samp_index <- x

      # Sample beta coefs
      betas <- betas[samp_index, ]

      # Sample general trend-specific parameters
      general_trend_pars <- mvgam:::extract_general_trend_pars(trend_pars = trend_pars,
                                                               samp_index = samp_index)

      if(use_lv || trend_model == 'VAR1'){
        # Propagate the lvs forward using the sampled trend parameters
        trends <- mvgam:::forecast_trend(trend_model = trend_model,
                                         use_lv = use_lv,
                                         trend_pars = general_trend_pars,
                                         h = fc_horizon)
      }

      # Loop across series and produce the next trend estimate
      trend_states <- do.call(cbind, (lapply(seq_len(n_series), function(series){

        # Sample series- and trend-specific parameters
        trend_extracts <- mvgam:::extract_series_trend_pars(series = series,
                                                            samp_index = samp_index,
                                                            trend_pars = trend_pars,
                                                            use_lv = use_lv)

        if(use_lv || trend_model == 'VAR1'){
          if(use_lv){
            # Multiply lv states with loadings to generate the series' forecast trend state
            out <- as.numeric(trends %*% trend_extracts$lv_coefs)
          }

          if(trend_model == 'VAR1'){
            out <- trends[, series]
          }

        } else {
          # Propagate the series-specific trends forward
          out <- mvgam:::forecast_trend(trend_model = trend_model,
                                        use_lv = FALSE,
                                        trend_pars = trend_extracts,
                                        h = fc_horizon)
        }

        out
      })))

      series_fcs <- lapply(seq_len(n_series), function(series){

        Xpmat <- cbind(Xp[which(as.numeric(data_assim$series) == series),],
                       trend_states[, series])
        attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')

        # Family-specific parameters
        family_extracts <- lapply(seq_along(family_pars), function(x){
          if(is.matrix(family_pars[[x]])){
            family_pars[[x]][samp_index, series]
          } else {
            family_pars[[x]][samp_index]
          }
        })
        names(family_extracts) <- names(family_pars)

        mvgam:::mvgam_predict(family = family,
                      Xp = Xpmat,
                      type = 'response',
                      betas = c(betas, 1),
                      family_pars = family_extracts)
      })

      series_fcs
    }, cl = cl)
    stopCluster(cl)

  } else {
    #### For operating on a single core ####
    draw_fcs <- pbapply::pblapply(sample_seq, function(x){

      samp_index <- x

      # Sample beta coefs
      betas <- betas[samp_index, ]

      # Sample general trend-specific parameters
      general_trend_pars <- mvgam:::extract_general_trend_pars(trend_pars = trend_pars,
                                                       samp_index = samp_index)

      if(use_lv || trend_model == 'VAR1'){
        # Propagate the lvs forward using the sampled trend parameters
        trends <- mvgam:::forecast_trend(trend_model = trend_model,
                                 use_lv = use_lv,
                                 trend_pars = general_trend_pars,
                                 h = fc_horizon)
      }

      # Loop across series and produce the next trend estimate
      trend_states <- do.call(cbind, (lapply(seq_len(n_series), function(series){

        # Sample series- and trend-specific parameters
        trend_extracts <- mvgam:::extract_series_trend_pars(series = series,
                                                            samp_index = samp_index,
                                                            trend_pars = trend_pars,
                                                            use_lv = use_lv)

        if(use_lv || trend_model == 'VAR1'){
          if(use_lv){
            # Multiply lv states with loadings to generate the series' forecast trend state
            out <- as.numeric(trends %*% trend_extracts$lv_coefs)
          }

          if(trend_model == 'VAR1'){
            out <- trends[, series]
          }

        } else {
          # Propagate the series-specific trends forward
          out <- mvgam:::forecast_trend(trend_model = trend_model,
                                use_lv = FALSE,
                                trend_pars = trend_extracts,
                                h = fc_horizon)
        }

        out
      })))

      series_fcs <- lapply(seq_len(n_series), function(series){

        Xpmat <- cbind(Xp[which(as.numeric(data_assim$series) == series),],
                       trend_states[, series])
        attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')

        # Family-specific parameters
        family_extracts <- lapply(seq_along(family_pars), function(x){
          if(is.matrix(family_pars[[x]])){
            family_pars[[x]][samp_index, series]
          } else {
            family_pars[[x]][samp_index]
          }
        })
        names(family_extracts) <- names(family_pars)

        mvgam:::mvgam_predict(family = family,
                      Xp = Xpmat,
                      type = 'response',
                      betas = c(betas, 1),
                      family_pars = family_extracts)
      })

      series_fcs
    })
  }

  # Final forecast distribution
  series_fcs <- lapply(seq_len(n_series), function(series){
    indexed_forecasts <- do.call(rbind, lapply(seq_along(draw_fcs), function(x){
      draw_fcs[[x]][[series]]
    }))
    indexed_forecasts
  })
  names(series_fcs) <- levels(data_assim$series)

  # If variogram score is chosen
  if(score == 'variogram'){

    # Get truths (out of sample) into correct format
    truths <- do.call(rbind, lapply(seq_len(n_series), function(series){
      s_name <- levels(data_assim$series)[series]
      data.frame(series = data_assim$series,
                 y = data_assim$y,
                 time = data_assim$time) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    }))

    if(log){
      series_score <- log(mvgam:::variogram_mcmc_object(truths = truths,
                                                    fcs = series_fcs) + 0.0001)
    } else {
      series_score <- mvgam:::variogram_mcmc_object(truths = truths,
                                                    fcs = series_fcs)
    }

  }

  # If not using variogram score
  if(score != 'variogram'){

    # Evaluate against the truth
    series_truths <- lapply(seq_len(n_series), function(series){
      if(class(object$obs_data)[1] == 'list'){
        data_assim[['y']][which(as.numeric(data_assim$series) == series)]
      } else {
        data_assim[which(as.numeric(data_assim$series) == series),'y']
      }
    })

    # Calculate score and interval coverage per series
    if(object$family %in% c('poisson', 'negative binomial')){
      series_score <- lapply(seq_len(n_series), function(series){
        DRPS <- data.frame(drps_mcmc_object(as.vector(as.matrix(series_truths[[series]])),
                                            series_fcs[[series]]))
        colnames(DRPS) <- c('score','in_interval')
        if(log){
          DRPS$score <- log(DRPS$score + 0.0001)
        }
        DRPS$eval_horizon <- seq(1, fc_horizon)
        DRPS
      })
      names(series_score) <- levels(data_assim$series)
    } else {
      series_score <- lapply(seq_len(n_series), function(series){
        CRPS <- data.frame(crps_mcmc_object(as.vector(as.matrix(series_truths[[series]])),
                                            series_fcs[[series]]))
        colnames(CRPS) <- c('score','in_interval')
        if(log){
          CRPS$score <- log(CRPS$score + 0.0001)
        }
        CRPS$eval_horizon <- seq(1, fc_horizon)
        CRPS
      })
      names(series_score) <- levels(data_assim$series)
    }
  }

  return(series_score)
}


#'@param object \code{list} object returned from \code{mvgam}
#'@param n_samples \code{integer} specifying the number of samples to generate from the model's
#'posterior distribution
#'@param evaluation_seq Optional \code{integer sequence} specifying the exact set of timepoints for
#'evaluating the model's forecasts. This sequence cannot have values
#'\code{<3} or \code{> max(training timepoints) - fc_horizon}
#'@param n_evaluations \code{integer} specifying the total number of evaluations to perform
#'(ignored if \code{evaluation_seq} is supplied)
#'@param fc_horizon \code{integer} specifying the length of the forecast horizon for evaluating forecasts
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@rdname evaluate_mvgams
#'@export
roll_eval_mvgam = function(object,
                           n_evaluations = 5,
                           evaluation_seq,
                           n_samples = 5000,
                           fc_horizon = 3,
                           n_cores = 2,
                           score = 'drps',
                           log = TRUE){

  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(object$trend_model == 'None'){
    stop('cannot compute rolling forecasts for mvgams that have no trend model',
         call. = FALSE)
  }

  # Generate time variable from training data
  if(class(object$obs_data)[1] == 'list'){
    all_timepoints <- (data.frame(time = object$obs_data$time)  %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)

  } else {
    all_timepoints <- (object$obs_data %>%
                         dplyr::select(time) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::mutate(time = dplyr::row_number())) %>%
      dplyr::pull(time)
  }


  # Generate evaluation sequence if not supplied
  if(missing(evaluation_seq)){
    evaluation_seq <- floor(seq(from = 3, to = (max(all_timepoints) - fc_horizon),
                                length.out = n_evaluations))
  }

  # Check evaluation sequence
  if(min(evaluation_seq) < 3){
    stop('Evaluation sequence cannot start before timepoint 3')
  }

  if(max(evaluation_seq) > (max(all_timepoints) - fc_horizon)){
    stop('Maximum of evaluation sequence is too large for fc_horizon evaluations')
  }

  # Loop across evaluation sequence and calculate evaluation metrics
  cl <- parallel::makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('all_timepoints',
                        'evaluation_seq',
                        'object',
                        'n_samples',
                        'fc_horizon',
                        'eval_mvgam',
                        'score',
                        'log'),
                envir = environment())
  parallel::clusterEvalQ(cl, library(mgcv))
  parallel::clusterEvalQ(cl, library(rstan))

  pbapply::pboptions(type = "none")
  evals <- pbapply::pblapply(evaluation_seq, function(timepoint){
    eval_mvgam(object = object,
               n_samples = n_samples,
               n_cores = 1,
               eval_timepoint = timepoint,
               fc_horizon = fc_horizon,
               score = score,
               log = log)
  },
  cl = cl)
  stopCluster(cl)

  # Take sum of score at each evaluation point for multivariate models
  sum_or_na = function(x){
    if(all(is.na(x))){
      NA
    } else {
      sum(x, na.rm = T)
    }
  }

  if(score == 'variogram'){
    eval_horizons <- do.call(rbind, lapply(seq_along(evals), function(x){
      data.frame(seq_along(evals[[x]]))
    }))

    scores <- do.call(rbind, lapply(seq_along(evals), function(x){
      data.frame(evals[[x]])
    }))

    evals_df <- data.frame(score = scores,
                           eval_horizon = eval_horizons,
                           in_interval = NA)
    colnames(evals_df) <- c('score', 'eval_horizon', 'in_interval')

    # Calculate summary statistics for each series
    out <- list(sum_score = sum_or_na(evals_df$score),
                score_summary = summary(evals_df$score),
                score_horizon_summary = evals_df %>%
                  dplyr::group_by(eval_horizon) %>%
                  dplyr::summarise(median_score = median(score, na.rm = T)),
                interval_coverage = NA,
                series_evals = NA,
                all_scores = evals_df)

  } else {
    evals_df <- do.call(rbind, do.call(rbind, evals)) %>%
      dplyr::group_by(eval_horizon) %>%
      dplyr::summarise(score = sum_or_na(score),
                       in_interval = mean(in_interval, na.rm = T))

    # Calculate summary statistics for each series
    tidy_evals <- lapply(seq_len(length(levels(object$obs_data$series))), function(series){
      all_evals <- do.call(rbind, purrr::map(evals, levels(object$obs_data$series)[series]))
      list(sum_drps = sum_or_na(all_evals$score),
           score_summary = summary(all_evals$score),
           score_horizon_summary = all_evals %>%
             dplyr::group_by(eval_horizon) %>%
             dplyr::summarise(median_score = mean(score, na.rm = T)),
           interval_coverage = mean(all_evals$in_interval, na.rm = T),
           all_scores = all_evals)

    })
    names(tidy_evals) <- levels(object$obs_data$series)

    out <- list(sum_score = sum_or_na(evals_df$score),
                score_summary = summary(evals_df$score),
                score_horizon_summary = evals_df %>%
                  dplyr::group_by(eval_horizon) %>%
                  dplyr::summarise(median_score = median(score, na.rm = T)),
                interval_coverage = mean(evals_df$in_interval, na.rm = T),
                series_evals = tidy_evals)
  }

  # Return score summary statistics
  return(out)

}


#'@param model1 \code{list} object returned from \code{mvgam} representing the first model to be
#'evaluated
#'@param model2 \code{list} object returned from \code{mvgam} representing the second model to be
#'evaluated
#'@param n_samples \code{integer} specifying the number of samples to generate from the model's
#'posterior distribution
#'@param fc_horizon \code{integer} specifying the length of the forecast horizon for evaluating forecasts
#'@param n_evaluations \code{integer} specifying the total number of evaluations to perform
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@rdname evaluate_mvgams
#'@export
compare_mvgams = function(model1,
                          model2,
                          n_samples = 1000,
                          fc_horizon = 3,
                          n_evaluations = 10,
                          n_cores = 2,
                          score = 'drps',
                          log = TRUE){

  # Check arguments
  if(class(model1) != 'mvgam'){
    stop('argument "model1" must be of class "mvgam"')
  }

  if(class(model2) != 'mvgam'){
    stop('argument "model2" must be of class "mvgam"')
  }

  if(model2$trend_model == 'None'){
    stop('cannot compare rolling forecasts for mvgams that have no trend model',
         call. = FALSE)
  }

  if(model1$trend_model == 'None'){
    stop('cannot compare rolling forecasts for mvgams that have no trend model',
         call. = FALSE)
  }

  if(sign(fc_horizon) != 1){
    stop('argument "fc_horizon" must be a positive integer',
         call. = FALSE)
  } else {
    if(fc_horizon%%1 != 0){
      stop('argument "fc_horizon" must be a positive integer',
           call. = FALSE)
    }
  }

  if(sign(n_evaluations) != 1){
    stop('argument "n_evaluations" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_evaluations%%1 != 0){
      stop('argument "n_evaluations" must be a positive integer',
           call. = FALSE)
    }
  }

  # Evaluate the two models
  mod1_eval <- roll_eval_mvgam(model1,
                               n_samples = n_samples,
                               fc_horizon = fc_horizon,
                               n_cores = n_cores,
                               n_evaluations = n_evaluations,
                               score = score,
                               log = log)
  mod2_eval <- roll_eval_mvgam(model2,
                               n_samples = n_samples,
                               fc_horizon = fc_horizon,
                               n_cores = n_cores,
                               n_evaluations = n_evaluations,
                               score = score,
                               log = log)

  # Generate a simple summary of forecast scores for each model
  model_summary <- rbind(mod1_eval$score_summary, mod2_eval$score_summary)
  rownames(model_summary) <- c('Model 1', 'Model 2')
  cat('RPS summaries per model (lower is better)\n')
  print(model_summary)

  # Print 90% interval coverages for each model
  if(score != 'variogram'){
    cat('\n90% interval coverages per model (closer to 0.9 is better)\n')
    cat('Model 1', mod1_eval$interval_coverage, '\n')
    cat('Model 2', mod2_eval$interval_coverage)
  }

  # Set up plotting loop and return summary plots of DRPS
  ask <- TRUE

  for(i in 1:3) {
    if(i == 1){
      barplot(c('model 1' = mod1_eval$sum_score,
                'model 2' = mod2_eval$sum_score),
              col = c("#B97C7C",  "#7C0000"),
              border = NA,
              ylab = 'Sum RPS (lower is better)',
              lwd = 2)
    } else if(i == 2){
      boxplot(list('model 1' = mod1_eval$score_summary,
                   'model 2' = mod2_eval$score_summary),
              border = c("#B97C7C",  "#7C0000"),
              ylab = 'Sum RPS per evaluation', axes = FALSE)
      axis(side = 2, lwd = 2)
      axis(side = 1, at = c(1, 2), labels = c('model 1', 'model 2'), lwd = 0)
    } else {
      plot_dat <- rbind(mod1_eval$score_horizon_summary$median_score,
                        mod2_eval$score_horizon_summary$median_score)
      colnames(plot_dat) <- seq(1:NCOL(plot_dat))

      ylim = c(min(0, min(plot_dat, na.rm = TRUE)), max(plot_dat, na.rm = T) * 1.4)

      barplot(plot_dat,
              ylim = ylim,
              beside = T,
              xlab = 'Forecast horizon',
              ylab = 'Median RPS',
              col = c("#B97C7C",  "#7C0000"),
              lwd = 2,
              border = NA,
              legend.text = c('Model 1', 'Model 2'),
              args.legend = list(x = "top", ncol = 2, border = NA,
                                 bty = 'n'))
    }

    if(ask){
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
      ask <- FALSE
    }

  }
  invisible()
}

#' @noRd
crps_edf <- function(y, dat, w = NULL) {
  if (is.null(w)) {
    c_1n <- 1 / length(dat)
    x <- sort(dat)
    a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
    f <- function(s) 2 * c_1n * sum(((s < x) - a) * (x - s))
  } else {
    if (!identical(length(dat), length(w)) || any(w < 0, na.rm = TRUE)) {
      return(rep(NaN, length(y)))
    }
    ord <- order(dat)
    x <- dat[ord]
    w <- w[ord]
    p <- cumsum(w)
    P <- p[length(p)]
    a <- (p - 0.5 * w) / P
    f <- function(s) 2 / P * sum(w * ((s < x) - a) * (x - s))
  }
  sapply(y, f)
}

# Calculate out of sample CRPS
# code borrowed from scoringRules: https://github.com/FK83/scoringRules/blob/master/R/scores_sample_univ.R
#' @noRd
crps_score <- function(truth, fc, method = "edf", w = NULL,
                       interval_width = 0.9){
  if (identical(length(truth), 1L) && is.vector(fc)) {
    score <- crps_edf(truth, fc, w)
  } else {
    score <- sapply(seq_along(truth),
                    function(i) crps_edf(y[i], fc[i, ], w[i, ]))
  }

  # Is value within empirical interval?
  interval <- quantile(fc, probs = c((1-interval_width)/2,
                                     (interval_width + (1-interval_width)/2)),
                       na.rm = TRUE)
  in_interval <- ifelse(truth <= interval[2] & truth >= interval[1], 1, 0)
  return(c(score, in_interval))
}


# Calculate out of sample DRPS
#' @noRd
drps_score <- function(truth, fc, interval_width = 0.9){
  nsum <- 1000
  Fy = ecdf(fc)
  ysum <- 0:nsum
  indicator <- ifelse(ysum - truth >= 0, 1, 0)
  score <- sum((indicator - Fy(ysum))^2)

  # Is value within empirical interval?
  interval <- quantile(fc, probs = c((1-interval_width)/2,
                                     (interval_width + (1-interval_width)/2)),
                       na.rm = TRUE)
  in_interval <- ifelse(truth <= interval[2] & truth >= interval[1], 1, 0)
  return(c(score, in_interval))
}

#' Compute the variogram score, using the median pairwise difference
#' from the forecast distribution (scoringRules::vs_sample uses the
#' mean, which is not appropriate for skewed distributions)
#' @noRd
variogram_score = function(truth, fc){
  out <- matrix(NA, length(truth), length(truth))
  for(i in 1:length(truth)){
    for(j in 1:length(truth)){
      if(i == j){
        out[i,j] <- 0
      } else {
        v_fc <- quantile(abs(fc[i,] - fc[j,]) ^ 0.5, 0.5)
        v_dat <- abs(truth[i] - truth[j]) ^ 0.5
        out[i,j] <- 2 * ((v_dat - v_fc) ^ 2)
      }
    }
  }
  # Divide by two as we have (inefficiently) computed each pairwise
  # comparison twice
  score <- sum(out) / 2

}

#' Wrapper to calculate variogram score on all observations in fc_horizon
#' @noRd
variogram_mcmc_object <- function(truths, fcs){
  fc_horizon <- length(fcs[[1]][1,])
  fcs_per_horizon <- lapply(seq_len(fc_horizon), function(horizon){
    do.call(rbind, lapply(seq_along(fcs), function(fc){
      fcs[[fc]][,horizon]
    }))
  })

  unlist(lapply(seq_len(fc_horizon), function(horizon){
    variogram_score(truth = truths[,horizon],
                    fc = fcs_per_horizon[[horizon]])
  }))
}

# Wrapper to calculate scores on all observations in fc_horizon
#' @noRd
drps_mcmc_object <- function(truth, fc, interval_width = 0.9){
  indices_keep <- which(!is.na(truth))
  if(length(indices_keep) == 0){
    scores = data.frame('drps' = rep(NA, length(truth)),
                        'interval' = rep(NA, length(truth)))
  } else {
    scores <- matrix(NA, nrow = length(truth), ncol = 2)
    for(i in indices_keep){
      scores[i,] <- drps_score(truth = as.vector(truth)[i],
                               fc = fc[,i], interval_width)
    }
  }
  scores
}

#' @noRd
crps_mcmc_object <- function(truth, fc, interval_width = 0.9){
  indices_keep <- which(!is.na(truth))
  if(length(indices_keep) == 0){
    scores = data.frame('drps' = rep(NA, length(truth)),
                        'interval' = rep(NA, length(truth)))
  } else {
    scores <- matrix(NA, nrow = length(truth), ncol = 2)
    for(i in indices_keep){
      scores[i,] <- crps_score(truth = as.vector(truth)[i],
                               fc = fc[,i], interval_width)
    }
  }
  scores
}
