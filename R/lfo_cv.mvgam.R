#'@title Approximate leave-future-out cross-validation of fitted \pkg{mvgam} objects
#'@name lfo_cv.mvgam
#'@importFrom stats update logLik
#'@param object \code{list} object of class \code{mvgam}. See [mvgam()]
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''series' (character or factor index of the series IDs)
#''time' (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param min_t Integer specifying the minimum training time required before making predictions
#'from the data. Default is either the `30`th timepoint in the observational data,
#'or whatever training time allows for at least
#'`10` lfo-cv calculations, if possible.
#'This value is essentially arbitrary so it is highly recommended to change it
#'to something that is more suitable to the
#'data and models being evaluated.
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
#'\donttest{
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
#'                chains = 2,
#'                silent = 2)
#'
#'# Fit a less appropriate model
#'mod_rw <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'               trend_model = RW(),
#'               family = poisson(),
#'               data = dat$data_train,
#'               newdata = dat$data_test,
#'               chains = 2,
#'               silent = 2)
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
#'                  fc_horizon = 3,
#'                  silent = 2)
#'lfo_rw <- lfo_cv(mod_rw,
#'                 min_t = 40,
#'                 fc_horizon = 3,
#'                 silent = 2)
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
lfo_cv <- function(object, ...) {
  UseMethod("lfo_cv", object)
}

#'@rdname lfo_cv.mvgam
#'@method lfo_cv mvgam
#'@export
lfo_cv.mvgam = function(
  object,
  data,
  min_t,
  fc_horizon = 1,
  pareto_k_threshold = 0.7,
  silent = 1,
  ...
) {
  validate_proportional(pareto_k_threshold)
  validate_pos_integer(fc_horizon)

  if (missing(data)) {
    all_data <- object$obs_data
  } else {
    all_data <- validate_series_time(
      data,
      name = 'data',
      trend_model = object$trend_model
    )
  }
  N <- max(all_data$index..time..index)
  all_unique_times <- sort(unique(all_data$index..time..index))

  # Default minimum training time is the 30th timepoint, or
  # whatever training time allows for at least 10 lfo_cv calculations
  if (missing(min_t)) {
    if (length(all_unique_times) > 30) {
      min_t <- pmin(max(1, N - 10 - fc_horizon), all_unique_times[30])
    } else if (length(all_unique_times) < 30 & length(all_unique_times) > 20) {
      min_t <- pmin(max(1, N - 10 - fc_horizon), all_unique_times[20])
    } else if (length(all_unique_times) < 20 & length(all_unique_times) > 10) {
      min_t <- pmin(max(1, N - 10 - fc_horizon), all_unique_times[10])
    } else {
      min_t <- 1
    }
  }

  if (min_t < 0) {
    min_t <- 1
  }
  validate_pos_integer(min_t)
  if (min_t >= N) {
    stop('Argument "min_t" is >= the maximum training time', call. = FALSE)
  }

  # Store the Expected Log Predictive Density (EPLD) at each time point
  approx_elpds <- rep(NA, N)

  # Initialize the process for i = min_t, generating a
  # conditional forecast for all of the future data
  data_splits <- cv_split(all_data, last_train = min_t, fc_horizon = fc_horizon)

  # Fit model to training and forecast all remaining testing observations
  noncentred <- if (is.null(attr(object$model_data, 'noncentred'))) {
    FALSE
  } else {
    TRUE
  }

  if (silent < 1L) {
    cat('Approximating elpd for training point', min_t, '...\n')
  }

  fit_past <- update(
    object,
    data = data_splits$data_train,
    newdata = data_splits$data_test,
    lfo = TRUE,
    noncentred = noncentred,
    silent = silent
  )

  # Calculate log likelihoods of forecast observations for the next
  # fc_horizon ahead observations
  fc_indices <- which(
    c(data_splits$data_train$time, data_splits$data_test$time) %in%
      (min_t + 1):(min_t + fc_horizon)
  )
  loglik_past <- logLik(fit_past)

  # Store the EPLD estimate
  approx_elpds[min_t + 1] <- log_mean_exp(sum_rows(loglik_past[, fc_indices]))

  # Iterate over i > min_t
  i_refit <- min_t
  refits <- min_t
  ks <- 0

  for (i in (min_t + 1):(N - fc_horizon)) {
    if (silent < 1L) {
      cat('Approximating elpd for training point', i, '...\n')
    }

    # Get log likelihoods of what would be the
    # last training observations for calculating Pareto k values
    last_obs_indices <- which(
      c(data_splits$data_train$time, data_splits$data_test$time) %in%
        (i_refit + 1):i
    )
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
      data_splits <- cv_split(all_data, last_train = i, fc_horizon = fc_horizon)

      # Re-fit the model
      fit_past <- update(
        fit_past,
        data = data_splits$data_train,
        newdata = data_splits$data_test,
        lfo = TRUE,
        noncentred = noncentred,
        silent = silent
      )

      # Calculate ELPD as before
      fc_indices <- which(
        c(data_splits$data_train$time, data_splits$data_test$time) %in%
          (i + 1):(i + fc_horizon)
      )
      loglik_past <- logLik(fit_past)
      approx_elpds[i + 1] <- log_mean_exp(sum_rows(loglik_past[, fc_indices]))
    } else {
      # If k below threshold, calculate log likelihoods for the
      # forecast observations using the normalised importance weights
      # to weight the posterior draws
      fc_indices <- which(
        c(data_splits$data_train$time, data_splits$data_test$time) %in%
          (i + 1):(i + fc_horizon)
      )
      lw <- loo::weights.importance_sampling(psis_obj, normalize = TRUE)[, 1]
      approx_elpds[i + 1] <- log_sum_exp(
        lw + sum_rows(loglik_past[, fc_indices])
      )
    }
  }
  return(structure(
    list(
      elpds = approx_elpds[(min_t + 1):(N - fc_horizon)],
      sum_ELPD = sum(approx_elpds, na.rm = TRUE),
      pareto_ks = ks[-1],
      eval_timepoints = (min_t + 1):(N - fc_horizon),
      pareto_k_threshold = pareto_k_threshold
    ),
    class = 'mvgam_lfo'
  ))
}

#' Plot Pareto-k and ELPD values from a `mvgam_lfo` object
#'
#' This function takes an object of class `mvgam_lfo` and creates several
#' informative diagnostic plots
#' @importFrom graphics layout axis lines abline polygon points
#' @param x An object of class `mvgam_lfo`
#' @param ... Ignored
#' @return A `ggplot` object presenting Pareto-k and ELPD values over the
#' evaluation timepoints. For the Pareto-k plot, a dashed red line indicates the
#' specified threshold chosen for triggering model refits. For the ELPD plot,
#' a dashed red line indicates the bottom 10% quantile of ELPD values. Points below
#' this threshold may represent outliers that were more difficult to forecast
#' @export
plot.mvgam_lfo = function(x, ...) {
  object <- x

  # Plot Pareto-k values over time
  object$pareto_ks[which(is.infinite(object$pareto_ks))] <-
    max(object$pareto_ks[which(!is.infinite(object$pareto_ks))])

  dplyr::tibble(
    eval_timepoints = object$eval_timepoints,
    elpds = object$elpds,
    pareto_ks = object$pareto_ks
  ) -> obj_tribble

  # Hack so we don't have to import tidyr just to use pivot_longer once
  dplyr::bind_rows(
    obj_tribble %>%
      dplyr::select(eval_timepoints, elpds) %>%
      dplyr::mutate(name = 'elpds', value = elpds) %>%
      dplyr::select(-elpds),
    obj_tribble %>%
      dplyr::select(eval_timepoints, pareto_ks) %>%
      dplyr::mutate(name = 'pareto_ks', value = pareto_ks) %>%
      dplyr::select(-pareto_ks)
  ) %>%
    dplyr::left_join(
      dplyr::tribble(
        ~name,
        ~threshold,
        "elpds",
        quantile(object$elpds, probs = 0.15),
        "pareto_ks",
        object$pareto_k_threshold
      ),
      by = "name"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      colour = dplyr::case_when(
        name == 'elpds' & value < threshold ~ "outlier",
        name == 'pareto_ks' & value > threshold ~ "outlier",
        TRUE ~ "inlier"
      )
    ) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(eval_timepoints, value)) +
    ggplot2::facet_wrap(
      ~ factor(
        name,
        levels = c("pareto_ks", "elpds"),
        labels = c("Pareto K", "ELPD")
      ),
      ncol = 1,
      scales = "free_y"
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = threshold),
      colour = "#A25050",
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::geom_line(linewidth = 0.5, col = "grey30") +
    ggplot2::geom_point(shape = 16, colour = 'white', size = 2) +
    ggplot2::geom_point(
      ggplot2::aes(colour = colour),
      shape = 16,
      show.legend = F,
      size = 1.5
    ) +
    ggplot2::scale_colour_manual(values = c("grey30", "#8F2727")) +
    ggplot2::labs(x = "Evaluation time", y = NULL) +
    ggplot2::theme_bw()
}

#' Function to generate training and testing splits
#' @noRd
cv_split = function(data, last_train, fc_horizon = 1) {
  if (inherits(data, 'list')) {
    # Find indices of training and testing splits
    temp_dat = data.frame(
      time = data$index..time..index,
      series = data$series
    ) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::arrange(time, series)

    indices_train <- temp_dat %>%
      dplyr::filter(time <= last_train) %>%
      dplyr::pull(index)

    indices_test <- temp_dat %>%
      dplyr::filter(time > last_train) %>%
      dplyr::pull(index)

    # Split
    data_train <- lapply(data, function(x) {
      if (is.matrix(x)) {
        matrix(x[indices_train, ], ncol = NCOL(x))
      } else {
        x[indices_train]
      }
    })

    data_test <- lapply(data, function(x) {
      if (is.matrix(x)) {
        matrix(x[indices_test, ], ncol = NCOL(x))
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

  return(list(data_train = data_train, data_test = data_test))
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
sum_rows = function(x) {
  if (NCOL(x) > 1) {
    out <- rowSums(x, na.rm = TRUE)
  } else {
    out <- x[!is.na(x)]
  }
  return(out)
}
