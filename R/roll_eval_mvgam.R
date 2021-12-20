#'Evaluate forecasts from a fitted mvjagam object using a rolling window
#'
#'This function sets up a sequence of evaluation timepoints along a rolling window and iteratively
#'calls \code{eval_mvgam} to evaluate 'out-of-sample' forecasts.
#'Evaluation involves calculating the Discrete Rank Probability Score and a binary indicator
#'for whether or not the true value lies within the forecast's 90% prediction interval
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param n_samples \code{integer} specifying the number of samples to generate from the model's
#'posterior distribution
#'@param evaluation_seq Optional \code{integer sequence} specifying the exact set of timepoints for
#'evaluating the model's forecasts. This sequence cannot have values
#'\code{<3} or \code{> max(training timepoints) - fc_horizon}
#'@param n_evaluations \code{integer} specifying the total number of evaluations to perform
#'(ignored if \code{evaluation_seq} is supplied)
#'@param fc_horizon \code{integer} specifying the length of the forecast horizon for evaluating forecasts
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object containing information on specific evaluations for each series as well as
#'a total evaluation summary (taken by summing the DRPS for each series at each evaluation and averaging
#'the coverages at each evaluation)
#'@export
roll_eval_mvgam = function(object,
                           n_evaluations,
                           evaluation_seq,
                           n_samples = 5000,
                           fc_horizon = 3,
                           n_cores = 2){

  # Generate time variable from training data
  all_timepoints <- (object$obs_data %>%
                       dplyr::select(year, season) %>%
                       dplyr::distinct() %>%
                       dplyr::arrange(year, season) %>%
                       dplyr::mutate(time = dplyr::row_number())) %>%
    dplyr::pull(time)

  # Generate evaluation sequence if not supplied
  if(missing(evaluation_seq)){
    n_evaluations <- 5
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
  evals <- lapply(evaluation_seq, function(timepoint){
    eval_mvgam(object = object,
               n_samples = n_samples,
               n_cores = n_cores,
               eval_timepoint = timepoint,
               fc_horizon = fc_horizon)
  })

  # Take sum of DRPS at each evaluation point for multivariate models
  evals_df <- do.call(rbind, do.call(rbind, evals)) %>%
    dplyr::group_by(eval_season, eval_year, eval_horizon) %>%
    dplyr::summarise(drps = sum(drps),
                     in_interval = mean(in_interval))

  # Calculate summary statistics for each series
  tidy_evals <- lapply(seq_len(length(levels(object$obs_data$series))), function(series){
    all_evals <- do.call(rbind, purrr::map(evals, levels(object$obs_data$series)[series]))
    list(sum_drps = sum(all_evals$drps),
         drps_summary = summary(all_evals$drps),
         drps_horizon_summary = all_evals %>%
           dplyr::group_by(eval_horizon) %>%
           dplyr::summarise(mean_drps = mean(drps)),
         interval_coverage = mean(all_evals$in_interval),
         all_drps = all_evals)

  })
  names(tidy_evals) <- levels(object$obs_data$series)

  # Return series-specific summaries and the total summary statistics
  return(list(sum_drps = sum(evals_df$drps),
              drps_summary = summary(evals_df$drps),
              drps_horizon_summary = evals_df %>%
                dplyr::group_by(eval_horizon) %>%
                dplyr::summarise(mean_drps = mean(drps)),
              interval_coverage = mean(evals_df$in_interval),
              series_evals = tidy_evals))

}
