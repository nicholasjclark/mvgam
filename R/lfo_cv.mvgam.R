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
#'`10` lfo-cv calculations (i.e. `pmin(max(data$time) - 10, 30)`). This value is essentially
#'arbitrary so it is highly recommended to change it to something that is more suitable to the
#'data and models being evaluated
#'@param fc_horizon Integer specifying the number of time steps ahead for evaluating forecasts
#'@param pareto_k_threshold Proportion specifying the threshold over which the Pareto shape parameter
#'is considered unstable, triggering a model refit. Default is `0.7`
#'@param silent Verbosity level between `0` and `2`. If `1` (the default), most of the informational
#'messages of compiler and sampler are suppressed. If `2`, even more messages are suppressed. The
#'actual sampling progress is still printed. Set `refresh = 0` to turn this off as well. If using
#'`backend = "rstan"` you can also set open_progress = FALSE to prevent opening additional
#'progress bars.
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
#'@seealso \code{\link{forecast}}, \code{\link{score}}, \code{\link{compare_mvgams}}
#'@references Paul-Christian Bürkner, Jonah Gabry & Aki Vehtari (2020). Approximate leave-future-out cross-validation for Bayesian time series models
#'Journal of Statistical Computation and Simulation. 90:14, 2499-2523.
#'@examples
#'\dontrun{
#'# Simulate from a Poisson-AR2 model with a seasonal smooth
#'set.seed(100)
#'dat <- sim_mvgam(T = 75,
#'                 n_series = 1,
#'                 prop_trend = 0.75,
#'                 trend_model = 'AR2',
#'                 family = poisson())
#'
#'# Plot the time series
#'plot_mvgam_series(data = dat$data_train,
#'                  newdata = dat$data_test,
#'                  series = 1)
#'
#'# Fit an appropriate model
#'mod_ar2 <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'                trend_model = AR(p = 2),
#'                family = poisson(),
#'                data = dat$data_train,
#'                newdata = dat$data_test,
#'                burnin = 300,
#'                samples = 300,
#'                chains = 2)
#'
#'# Fit a less appropriate model
#'mod_rw <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'               trend_model = RW(),
#'               family = poisson(),
#'               data = dat$data_train,
#'               newdata = dat$data_test,
#'               burnin = 300,
#'               samples = 300,
#'               chains = 2)
#'
#'# Compare Discrete Ranked Probability Scores for the testing period
#'fc_ar2 <- forecast(mod_ar2)
#'fc_rw <- forecast(mod_rw)
#'score_ar2 <- score(fc_ar2, score = 'drps')
#'score_rw <- score(fc_rw, score = 'drps')
#'sum(score_ar2$series_1$score)
#'sum(score_rw$series_1$score)
#'
#'# Now use approximate leave-future-out CV to compare
#'# rolling forecasts; start at time point 40 to reduce
#'# computational time and to ensure enough data is available
#'# for estimating model parameters
#'lfo_ar2 <- lfo_cv(mod_ar2,
#'                  min_t = 40,
#'                  fc_horizon = 3)
#'lfo_rw <- lfo_cv(mod_rw,
#'                 min_t = 40,
#'                 fc_horizon = 3)
#'
#'# Plot Pareto-K values and ELPD estimates
#'plot(lfo_ar2)
#'plot(lfo_rw)
#'
#'# Proportion of timepoints in which AR2 model gives better forecasts
#'length(which((lfo_ar2$elpds - lfo_rw$elpds) > 0)) /
#'       length(lfo_ar2$elpds)
#'
#'# A higher total ELPD is preferred
#'lfo_ar2$sum_ELPD
#'lfo_rw$sum_ELPD
#'}
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
                        silent = 1,
                        ...){

  validate_proportional(pareto_k_threshold)
  validate_pos_integer(fc_horizon)

  if(missing(data)){
    all_data <- object$obs_data
  } else {
    all_data <- validate_series_time(data,
                                     name = 'data',
                                     trend_model = attr(object$model_data, 'trend_model'))
  }
  N <- max(all_data$index..time..index)

  # Default minimum training time is 30, or
  # whatever training time allows for at least 10 lfo_cv calculations
  if(missing(min_t)){
    min_t <- pmin(N - 10 - fc_horizon, 30)
  }

  if(min_t < 0){
    min_t <- 1
  }
  validate_pos_integer(min_t)
  if(min_t >= N){
    stop('Argument "min_t" is >= the maximum training time',
         call. = FALSE)
  }

  # Store the Expected Log Predictive Density (EPLD) at each time point
  approx_elpds <- rep(NA, N)

  # Initialize the process for i = min_t, generating a
  # conditional forecast for all of the future data
  data_splits <- cv_split(all_data, last_train = min_t,
                          fc_horizon = fc_horizon)

  # Fit model to training and forecast all remaining testing observations
  noncentred <- if(is.null(attr(object$model_data, 'noncentred'))){
    FALSE
  } else {
    TRUE
  }

  if(silent < 1L){
    cat('Approximating elpd for training point', min_t, '...\n')
  }

  fit_past <- update(object,
                    data = data_splits$data_train,
                    newdata = data_splits$data_test,
                    lfo = TRUE,
                    noncentred = noncentred,
                    silent = silent)

  # Calculate log likelihoods of forecast observations for the next
  # fc_horizon ahead observations
  fc_indices <- which(c(data_splits$data_train$time,
                        data_splits$data_test$time) %in%
                        (min_t + 1):(min_t + fc_horizon))
  loglik_past <- logLik(fit_past)

  # Store the EPLD estimate
  approx_elpds[min_t + 1] <- log_mean_exp(sum_rows(loglik_past[,fc_indices]))

  # Iterate over i > min_t
  i_refit <- min_t
  refits <- min_t
  ks <- 0

  for(i in (min_t + 1):(N - fc_horizon)) {
    if(silent < 1L){
      cat('Approximating elpd for training point', i, '...\n')
    }

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
      data_splits <- cv_split(all_data, last_train = i,
                              fc_horizon = fc_horizon)

      # Re-fit the model
      fit_past <- update(fit_past,
                         data = data_splits$data_train,
                         newdata = data_splits$data_test,
                         lfo = TRUE,
                         noncentred = noncentred,
                         silent = silent)

      # Calculate ELPD as before
      fc_indices <- which(c(data_splits$data_train$time,
                            data_splits$data_test$time) %in%
                            (i + 1):(i + fc_horizon))
      loglik_past <- logLik(fit_past)
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
  return(structure(list(elpds = approx_elpds[(min_t + 1):N],
                        sum_ELPD = sum(approx_elpds, na.rm = TRUE),
                        pareto_ks = ks,
                        eval_timepoints = (min_t + 1):N,
                        pareto_k_threshold = pareto_k_threshold),
                   class = 'mvgam_lfo'))
}

#' Plot Pareto-k and ELPD values from a leave-future-out object
#'
#' This function takes an object of class `mvgam_lfo` and creates several
#' informative diagnostic plots
#' @importFrom graphics layout axis lines abline polygon points
#' @param x An object of class `mvgam_lfo`
#' @param ... Ignored
#' @return A base `R` plot of Pareto-k and ELPD values over the
#' evaluation timepoints. For the Pareto-k plot, a dashed red line indicates the
#' specified threshold chosen for triggering model refits. For the ELPD plot,
#' a dashed red line indicates the bottom 10% quantile of ELPD values. Points below
#' this threshold may represent outliers that were more difficult to forecast
#' @export
plot.mvgam_lfo = function(x, ...){

  object <- x

  # Graphical parameters
  .pardefault <- par(no.readonly=T)
  on.exit(par(.pardefault))
  par(mfrow = c(2, 1))

  # Plot Pareto-k values over time
  object$pareto_ks[which(is.infinite(object$pareto_ks))] <-
    max(object$pareto_ks[which(!is.infinite(object$pareto_ks))])
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
}

#' Function to generate informative priors based on the posterior
#' of a previously fitted model (EXPERIMENTAL!!)
#' @noRd
summarize_posterior = function(object){

  # Extract the trend model
  trend_model <- attr(object$model_data, 'trend_model')
  if(trend_model == 'VAR1'){
    trend_model <- 'VAR1cor'
  }

  # Get params that can be modified for this model
  if(is.null(object$trend_call)){
    update_priors <- get_mvgam_priors(formula = formula(object),
                                      family = object$family,
                                      data = object$obs_data,
                                      trend_model = trend_model)
  } else {
    update_priors <- get_mvgam_priors(formula = formula(object),
                                      family = object$family,
                                      trend_formula = as.formula(object$trend_call),
                                      data = object$obs_data,
                                      trend_model = trend_model)
  }

  pars_keep <- c('smooth parameter|process error|fixed effect|Intercept|pop mean|pop sd|AR1|AR2|AR3|amplitude|length scale')
  update_priors <- update_priors[grepl(pars_keep, update_priors$param_info), ]

  # Get all possible parameters to summarize
  pars_to_prior <- vector()
  for(i in 1:NROW(update_priors)){
    pars_to_prior[i] <- trimws(strsplit(update_priors$prior[i], "[~]")[[1]][1])
  }

  # Summarize parameter posteriors as Normal distributions
  pars_posterior <- list()
  for(i in seq_along(pars_to_prior)){
    if(pars_to_prior[i] == '(Intercept)'){
      post_samps <- mcmc_chains(object$model_output, 'b[1]',
                                ISB = FALSE)
    } else {

      suppressWarnings(post_samps <- try(mcmc_chains(object$model_output,
                                                     pars_to_prior[i]),
                                         silent = TRUE))

      if(inherits(post_samps, 'try-error')){
        suppressWarnings(post_samps <- try(as.matrix(mod,
                                                     variable = pars_to_prior[i],
                                                     regex = TRUE),
                                           silent = TRUE))
      }

      if(inherits(post_samps, 'try-error')) next
    }

    new_lowers <- round(apply(post_samps, 2, min), 3)
    new_uppers <- round(apply(post_samps, 2, max), 3)
    means <- round(apply(post_samps, 2, mean), 3)
    sds <- round(apply(post_samps, 2, sd), 3)
    priors <- paste0('normal(', means, ', ', sds, ')')
    parametric <- grepl('Intercept|fixed effect',
                        update_priors$param_info[i])
    parnames <- dimnames(post_samps)[[2]]

    priorstring <- list()
    for(j in 1:NCOL(post_samps)){
      priorstring[[j]] <- prior_string(priors[j],
                                       class = parnames[j],
                                       resp = parametric,
                                       ub = new_uppers[j],
                                       lb = new_lowers[j],
                                       group = pars_to_prior[i])
    }

    pars_posterior[[i]] <- do.call(rbind, priorstring)
  }
  pars_posterior <- do.call(rbind, pars_posterior)
  pars_posterior <- pars_posterior[(pars_posterior$ub == 0 &
                                      pars_posterior$lb == 0)
                                   == FALSE, ]
  pars_posterior <- pars_posterior[(pars_posterior$ub == 1 &
                                      pars_posterior$lb == 1)
                                   == FALSE, ]
  pars_posterior$param_name <- NA
  pars_posterior$parametric <- pars_posterior$resp
  pars_posterior$resp <- NULL
  attr(pars_posterior, 'posterior_to_prior') <- TRUE

  return(pars_posterior)
}

#' Function for a leave-future-out update that uses informative priors
#' @noRd
lfo_update = function(object, ...){
  # Get informative priors
  priors <- summarize_posterior(object)

  # Run the update using the informative priors
  update.mvgam(object,
               priors = priors,
               ...)
}

#' Function to generate training and testing splits
#' @noRd
cv_split = function(data, last_train, fc_horizon = 1){
  if(inherits(data, 'list')){

    # Find indices of training and testing splits
    temp_dat = data.frame(time = data$index..time..index,
                          series = data$series) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::arrange(time, series)

    indices_train <- temp_dat %>%
      dplyr::filter(time <= last_train) %>%
      dplyr::pull(index)

    indices_test <- temp_dat %>%
      dplyr::filter(time > last_train) %>%
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
        matrix(x[indices_test,], ncol = NCOL(x))
      } else {
        x[indices_test]
      }
    })

  } else {
    data_train <- data %>%
      dplyr::filter(index..time..index <= last_train) %>%
      dplyr::arrange(index..time..index, series)

    data_test <- data %>%
      dplyr::filter(index..time..index > last_train) %>%
      dplyr::arrange(index..time..index, series)

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
