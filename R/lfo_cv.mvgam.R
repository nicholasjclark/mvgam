#'@title Approximate leave-future-out cross-validation of fitted `mvgam` objects
#'@name lfo_cv.mvgam
#'@param object \code{list} object returned from \code{mvgam}
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''series' (character or factor index of the series IDs)
#''time' (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param min_t Integer specifying the minimum training time required before making predictions
#'from the data. Default is either `30`, or whatever training time allows for at least
#'`10` lfo-cv calculations (i.e. `pmin(max(data$time) - 10, 30)`)
#'@param fc_horizon Integer specifying the number of time steps ahead for evaluating forecasts
#'@param pareto_k_threshold Proportion specifying the threshold over which the Pareto shape parameter
#'is considered unstable, triggering a model refit. Default is `0.6`
#'@param n_cores \code{integer} specifying number of cores for calculating likelihoods in parallel
#'@details Approximate leave-future-out cross-validation uses an expanding training window scheme
#' to evaluate a model on its forecasting ability. The steps used in this function mirror those laid out
#' in the [lfo vignette from the `loo` package](https://mc-stan.org/loo/articles/loo2-lfo.html),
#' written by Paul Bürkner, Jonah Gabry, Aki Vehtari. First, we refit the model using the first `min_t`
#' observations to perform a single exact `fc_horizon`-ahead forecast step. This forecast is evaluated against
#' the `min_t + fc_horizon` out of sample observations using the Expected Log Predictive Density (ELPD).
#' Next, we approximate each successive round of
#' expanding window forecasts by moving forward one step at a time `for i in 1:N_evaluations` and re-weighting
#' draws from the model's posterior predictive distribution using Pareto Smoothed
#' Importance Sampling (PSIS). In each iteration `i`, PSIS weights are obtained for the next observation
#' that would have been included in the model if we had re-fit (i.e. the last observation that would have
#' been in the training data, or `min_t + i`). If these importance ratios are stable, we consider the
#' approximation adequate and use the re-weighted posterior's forecast for evaluating the next holdout
#' set of testing observations (`(min_t + i + 1):(min_t + i + fc_horizon)`). At some point the
#' importance ratio variability will become too large and importance sampling will fail. This is
#' indicated by the estimated shape parameter `k` of the generalized Pareto distribution
#' crossing a certain threshold `pareto_k_threshold`. Only then do we refit the model using
#' all of the observations up to the time of the failure. We then restart the process and iterate forward
#' until the next refit is triggered (Bürkner et al. 2020).
#'@return A `list` containing the sum of approximate ELPD scores, the Pareto-k shape values and
#'the specified `pareto_k_threshold`
#'@references Paul-Christian Bürkner, Jonah Gabry & Aki Vehtari (2020). Approximate leave-future-out cross-validation for Bayesian time series models
#'Journal of Statistical Computation and Simulation. 90:14, 2499-2523.
#'@author Nicholas J Clark
NULL
#'@export
lfo_cv <- function(x, what, ...){
  UseMethod("lfo_cv")
}

#'@rdname lfo_cv.mvgam
#'@method lfo_cv mvgam
#'@export
lfo_cv.mvgam = function(object,
                        data,
                        min_t,
                        fc_horizon = 1,
                        pareto_k_threshold = 0.6,
                        n_cores = 1){

  if(missing(data)){
    all_data <- object$obs_data
  } else {
    all_data <- data
  }
  N <- max(all_data$time)

  # Default minimum training time is 30, or
  # whatever training time allows for lfo_cv calculations
  if(missing(min_t)){
    min_t <- pmin(max(all_data$time) - 10, 30)
  }

  # Store the Expected Log Predictive Density (EPLD) at each time point
  approx_elpds <- rep(NA, N)

  # Initialize the process for i = min_t, generating a
  # conditional forecast for all of the future data
  data_splits <- mvgam:::cv_split(all_data, last_train = min_t)

  # Fit model to training and forecast the testing
  fit_past <- update(object,
                    data = data_splits$data_train,
                    newdata = data_splits$data_test)

  # Calculate log likelihoods of forecast observations for the next
  # fc_horizon ahead observations
  fc_indices <- which(c(data_splits$data_train$time,
                        data_splits$data_test$time) %in%
                        (min_t + 1):(min_t + fc_horizon))
  loglik_past <- logLik(fit_past, n_cores = n_cores)

  # Store the EPLD estimate
  approx_elpds[min_t + 1] <- log_mean_exp(rowSums(loglik_past[,fc_indices],
                                                  na.rm = TRUE))

  # Iterate over i > min_t
  i_refit <- min_t
  refits <- min_t
  ks <- NULL

  for (i in (min_t + 1):(N - fc_horizon)) {

    # Get log likelihoods of what would be the
    # last training observations for calculating Pareto k values
    last_obs_indices <- which(c(data_splits$data_train$time,
                                data_splits$data_test$time) %in%
                                (i_refit + 1):i)
    logratio <- rowSums(loglik_past[, last_obs_indices],
                        na.rm = TRUE)

    # Use PSIS to estimate whether the Pareto shape parameter of the
    # importance weights is below the specified threshold; a lower value
    # indicates the importance ratios have finite variance and can be
    # used for approximating prediction error
    psis_obj <- suppressWarnings(loo::psis(logratio))
    k <- loo::pareto_k_values(psis_obj)
    ks <- c(ks, k)

    # If k is too high, refit the model based on the first i observations;
    # in other words, the last refit did not provide stable enough predictions
    # of what would be the last set of training observations; we instead need
    # to include these in the training data, resulting in a slightly larger
    # model
    if (k > pareto_k_threshold) {
      i_refit <- i
      refits <- c(refits, i)

      # Subset the data to now include the last set of training observations
      data_splits <- mvgam:::cv_split(all_data, last_train = i)

      # Re-fit the model
      fit_past <- update(fit_past,
                         data = data_splits$data_train,
                         newdata = data_splits$data_test)

      # Calculate ELPD as before
      fc_indices <- which(c(data_splits$data_train$time,
                            data_splits$data_test$time) %in%
                            (i + 1):(i + fc_horizon))
      loglik_past <- logLik(fit_past, n_cores = n_cores)
      approx_elpds[i + 1] <- log_mean_exp(rowSums(loglik_past[,fc_indices],
                                                  na.rm = TRUE))
    } else {
      # If k below threshold, calculate log likelihoods for the
      # forecast observations using the normalised importance weights
      # to weight the posterior draws
      fc_indices <- which(c(data_splits$data_train$time,
                            data_splits$data_test$time) %in%
                            (i + 1):(i + fc_horizon))
      lw <- loo::weights.importance_sampling(psis_obj, normalize = TRUE)[, 1]
      approx_elpds[i + 1] <- log_sum_exp(lw + rowSums(loglik_past[,fc_indices],
                                                      na.rm = TRUE))
    }
  }
return(list(elpds = approx_elpds[(min_t + 1):(N - fc_horizon)],
            sum_ELPD = sum(approx_elpds, na.rm = TRUE),
            pareto_ks = ks,
            eval_timepoints = (min_t + 1):(N - fc_horizon),
            pareto_k_threshold = pareto_k_threshold))
}

#' Function to generate training and testing splits
#' @noRd
cv_split = function(data, last_train){
  if(class(data)[1] == 'list'){

    # Find indices of training and testing splits
    temp_dat = data.frame(time = data$time,
                          series = data$series) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::arrange(time, series)
    indices_train <- temp_dat %>%
      dplyr::filter(time <= last_train) %>%
      dplyr::pull(index)

    # Split
    data_train <- lapply(data, function(x){
      if(is.matrix(x)){
        matrix(x[indices_train,], ncol = NCOL(x))
      } else {
        x[indices_train]
      }
    })

    data_test <- lapply(data, function(x){
      if(is.matrix(x)){
        matrix(x[-indices_train,], ncol = NCOL(x))
      } else {
        x[-indices_train]
      }
    })

  } else {
    data_train <- data %>%
      dplyr::filter(time <= last_train) %>%
      dplyr::arrange(time, series)
    data_test <- data %>%
      dplyr::filter(time > last_train) %>%
      dplyr::arrange(time, series)
  }

  return(list(data_train = data_train,
              data_test = data_test))
}

#' More stable version of log(sum(exp(x)))
#' @noRd
log_sum_exp <- function(x) {
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x)))
}

#' More stable version of log(mean(exp(x)))
#' @noRd
log_mean_exp <- function(x) {
  log_sum_exp(x) - log(length(x))
}
