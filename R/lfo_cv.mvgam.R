#'@title Approximate leave-future-out cross-validation of fitted `mvgam` objects
#'@name lfo_cv.mvgam
#'@importFrom stats update logLik
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
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
#'is considered unstable, triggering a model refit. Default is `0.7`
#'@param n_cores \code{integer} specifying number of cores for calculating likelihoods in parallel
#'@param ... Ignored
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
#'@return A `list` of class `mvgam_lfo` containing the approximate ELPD scores,
#'the Pareto-k shape values and 'the specified `pareto_k_threshold`
#'@references Paul-Christian Bürkner, Jonah Gabry & Aki Vehtari (2020). Approximate leave-future-out cross-validation for Bayesian time series models
#'Journal of Statistical Computation and Simulation. 90:14, 2499-2523.
#'@author Nicholas J Clark
#'@export
lfo_cv <- function(object, ...){
  UseMethod("lfo_cv", object)
}

#'@rdname lfo_cv.mvgam
#'@method lfo_cv mvgam
#'@export
lfo_cv.mvgam = function(object,
                        data,
                        min_t,
                        fc_horizon = 1,
                        pareto_k_threshold = 0.7,
                        n_cores = 1,
                        ...){

  if(pareto_k_threshold < 0 || pareto_k_threshold > 1){
    stop('Argument "pareto_k_threshold" must be a proportion ranging from 0 to 1, inclusive',
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

  if(sign(n_cores) != 1){
    stop('argument "n_cores" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_cores%%1 != 0){
      stop('argument "n_cores" must be a positive integer',
           call. = FALSE)
    }
  }

  if(missing(data)){
    all_data <- object$obs_data
  } else {
    all_data <- data
  }
  N <- max(all_data$time)

  # Default minimum training time is 30, or
  # whatever training time allows for at least 10 lfo_cv calculations
  if(missing(min_t)){
    min_t <- pmin(N - 10 - fc_horizon, 30)
  }

  if(min_t < 0){
    min_t <- 1
  }

  if(sign(min_t) != 1){
    stop('argument "min_t" must be a positive integer',
         call. = FALSE)
  } else {
    if(min_t%%1 != 0){
      stop('argument "min_t" must be a positive integer',
           call. = FALSE)
    }
  }

  # Store the Expected Log Predictive Density (EPLD) at each time point
  approx_elpds <- rep(NA, N)

  # Initialize the process for i = min_t, generating a
  # conditional forecast for all of the future data
  data_splits <- cv_split(all_data, last_train = min_t)

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
  approx_elpds[min_t + 1] <- log_mean_exp(sum_rows(loglik_past[,fc_indices]))

  # Iterate over i > min_t
  i_refit <- min_t
  refits <- min_t
  ks <- NULL

  for(i in (min_t + 1):(N - fc_horizon)) {

    # Get log likelihoods of what would be the
    # last training observations for calculating Pareto k values
    last_obs_indices <- which(c(data_splits$data_train$time,
                                data_splits$data_test$time) %in%
                                (i_refit + 1):i)
    logratio <- sum_rows(loglik_past[, last_obs_indices])

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
      data_splits <- cv_split(all_data, last_train = i)

      # Re-fit the model
      fit_past <- update(fit_past,
                         data = data_splits$data_train,
                         newdata = data_splits$data_test)

      # Calculate ELPD as before
      fc_indices <- which(c(data_splits$data_train$time,
                            data_splits$data_test$time) %in%
                            (i + 1):(i + fc_horizon))
      loglik_past <- logLik(fit_past, n_cores = n_cores)
      approx_elpds[i + 1] <- log_mean_exp(sum_rows(loglik_past[,fc_indices]))
    } else {
      # If k below threshold, calculate log likelihoods for the
      # forecast observations using the normalised importance weights
      # to weight the posterior draws
      fc_indices <- which(c(data_splits$data_train$time,
                            data_splits$data_test$time) %in%
                            (i + 1):(i + fc_horizon))
      lw <- loo::weights.importance_sampling(psis_obj, normalize = TRUE)[, 1]
      approx_elpds[i + 1] <- log_sum_exp(lw + sum_rows(loglik_past[,fc_indices]))
    }
  }
  return(structure(list(elpds = approx_elpds[(min_t + 1):(N - fc_horizon)],
                        sum_ELPD = sum(approx_elpds, na.rm = TRUE),
                        pareto_ks = ks,
                        eval_timepoints = (min_t + 1):(N - fc_horizon),
                        pareto_k_threshold = pareto_k_threshold),
                   class = 'mvgam_lfo'))
}

#' Plot Pareto-k and ELPD values from a leave-future-out object
#'
#' This function takes an object of class `mvgam_lfo` and create several
#' informative diagnostic plots
#' @importFrom graphics layout axis lines abline polygon points
#' @param x An object of class `mvgam_lfo`
#' @param ... Ignored
#' @return A base `R` plot of Pareto-k and ELPD values over the
#' evaluation timepoints. For the Pareto-k plot, a dashed red line indicates the
#' specified threshold chosen for triggering model refits. For the ELPD plot,
#' a dashed red line indicated the bottom 10% quantile of ELPD values. Points below
#' this threshold may represent outliers that were more difficult to forecast
#' @export
plot.mvgam_lfo = function(x, ...){

  object <- x

  # Graphical parameters
  layout(matrix(1:2, nrow = 2))

  # Plot Pareto-k values over time
  plot(1, type = "n", bty = 'L',
       xlab = '',
       ylab = 'Pareto k',
       xaxt = 'n',
       xlim = range(object$eval_timepoints),
       ylim = c(min(object$pareto_ks) - 0.1,
                max(object$pareto_ks) + 0.1))
  axis(side = 1, labels = NA, lwd = 2)

  lines(x = object$eval_timepoints,
        y = object$pareto_ks,
        lwd = 2.5)

  abline(h = object$pareto_k_threshold, col = 'white', lwd = 2.85)
  abline(h = object$pareto_k_threshold, col = "#A25050", lwd = 2.5, lty = 'dashed')

  points(x = object$eval_timepoints,
         y = object$pareto_ks, pch = 16, col = "white", cex = 1.25)
  points(x = object$eval_timepoints,
         y = object$pareto_ks, pch = 16, col = "black", cex = 1)

  points(x = object$eval_timepoints[which(object$pareto_ks > object$pareto_k_threshold)],
         y = object$pareto_ks[which(object$pareto_ks > object$pareto_k_threshold)],
         pch = 16, col = "white", cex = 1.5)
  points(x = object$eval_timepoints[which(object$pareto_ks > object$pareto_k_threshold)],
         y = object$pareto_ks[which(object$pareto_ks > object$pareto_k_threshold)],
         pch = 16, col = "#7C0000", cex = 1.25)

  box(bty = 'l', lwd = 2)

  # Plot ELPD values over time
  plot(1, type = "n", bty = 'L',
       xlab = 'Time point',
       ylab = 'ELPD',
       xlim = range(object$eval_timepoints),
       ylim = c(min(object$elpds) - 0.1,
                max(object$elpds) + 0.1))

  lines(x = object$eval_timepoints,
        y = object$elpds,
        lwd = 2.5)

  lower_vals <- quantile(object$elpds, probs = c(0.15))
  abline(h = lower_vals, col = 'white', lwd = 2.85)
  abline(h = lower_vals, col = "#A25050", lwd = 2.5, lty = 'dashed')

  points(x = object$eval_timepoints,
         y = object$elpds, pch = 16, col = "white", cex = 1.25)
  points(x = object$eval_timepoints,
         y = object$elpds, pch = 16, col = "black", cex = 1)

  points(x = object$eval_timepoints[which(object$elpds < lower_vals)],
         y = object$elpds[which(object$elpds < lower_vals)],
         pch = 16, col = "white", cex = 1.5)
  points(x = object$eval_timepoints[which(object$elpds < lower_vals)],
         y = object$elpds[which(object$elpds < lower_vals)],
         pch = 16, col = "#7C0000", cex = 1.25)

  box(bty = 'l', lwd = 2)

  layout(1)
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

#' Summing without NAs
#' @noRd
sum_rows = function(x){
  if(NCOL(x) > 1){
    out <- rowSums(x, na.rm = TRUE)
  } else {
    out <- x[!is.na(x)]
  }
  return(out)
}
