#'Plot posterior forecast predictions from \pkg{mvgam} models
#'@importFrom stats formula terms
#'@param object \code{list} object of class \code{mvgam}. See [mvgam()]
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of the original \code{formula}. If included, the
#'covariate information in \code{newdata} will be used to generate forecasts from the fitted model equations. If
#'this same \code{newdata} was originally included in the call to \code{mvgam}, then forecasts have already been
#'produced by the generative model and these will simply be extracted and plotted. However if no \code{newdata} was
#'supplied to the original model call, an assumption is made that the \code{newdata} supplied here comes sequentially
#'after the data supplied as \code{data} in the original model (i.e. we assume there is no time gap between the last
#'observation of series 1 in \code{data} and the first observation for series 1 in \code{newdata}). If
#'\code{newdata} contains observations in column \code{y}, these observations will be used to compute a Discrete Rank
#'Probability Score for the forecast distribution
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param realisations \code{logical}. If \code{TRUE}, forecast realisations are shown as a spaghetti plot,
#'making it easier to visualise the diversity of possible forecasts. If \code{FALSE}, the default,
#'empirical quantiles of the forecast distribution are shown
#'@param n_realisations \code{integer} specifying the number of posterior realisations to plot, if
#'\code{realisations = TRUE}. Ignored otherwise
#'@param n_cores \code{integer} specifying number of cores for generating forecasts in parallel
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}
#'@param xlab label for x axis.
#'@param ylab label for y axis.
#'@param ylim Optional \code{vector} of y-axis limits (min, max)
#'@param ... further \code{\link[graphics]{par}} graphical parameters.
#'@param return_forecasts \code{logical}. If \code{TRUE}, the function will plot the forecast
#'as well as returning the forecast object (as a \code{matrix} of dimension \code{n_samples} x \code{horizon})
#'@param return_score \code{logical}. If \code{TRUE} and out of sample test data is provided as
#'\code{newdata}, a probabilistic score will be calculated and returned. The score used will depend on the
#'observation family from the fitted model. Discrete families (\code{poisson}, \code{negative binomial}, \code{tweedie})
#'use the Discrete Rank Probability Score. Other families use the Continuous Rank Probability Score. The value
#'returned is the \code{sum} of all scores within the out of sample forecast horizon
#'@details `plot_mvgam_fc` generates posterior predictions from an object of class \code{mvgam}, calculates posterior
#' empirical quantiles and plots them against the observed data. If `realisations = FALSE`, the returned plot shows
#' 90, 60, 40 and 20 percent posterior quantiles (as ribbons of increasingly darker shades or red)
#' as well as the posterior median (as a dark red line). If `realisations = FALSE`, a set of `n_realisations` posterior
#' draws are shown. This function produces an older style base \code{R} plot, as opposed to `plot.mvgam_forecast`
#'
#'`plot.mvgam_forecast` takes an object of class `mvgam_forecast`, in which forecasts have already
#'been computed, and plots the resulting forecast distribution as a `ggplot` object. This function is therefore more
#'versatile and is recommended over the older and clunkier `plot_mvgam_fc` version
#'
#'If \code{realisations = FALSE}, these posterior quantiles are plotted along
#'with the true observed data that was used to train the model. Otherwise, a spaghetti plot is returned
#'to show possible forecast paths.
#'@return A base \code{R} graphics plot (for `plot_mvgam_fc`) or a `ggplot` object (for `plot.mvgam_forecast`) and an optional \code{list} containing the forecast distribution
#'and the out of sample probabilistic forecast score
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(n_series = 3, trend_model = AR())
#' mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'             trend_model = AR(),
#'             noncentred = TRUE,
#'             data = simdat$data_train,
#'             chains = 2,
#'             silent = 2)
#'
#' # Hindcasts on response scale
#' hc <- hindcast(mod)
#' str(hc)
#' plot(hc, series = 1)
#' plot(hc, series = 2)
#' plot(hc, series = 3)
#'
#' # Forecasts on response scale
#' fc <- forecast(mod, newdata = simdat$data_test)
#' str(fc)
#' plot(fc, series = 1)
#' plot(fc, series = 2)
#' plot(fc, series = 3)
#'
#' # Forecasts as expectations
#' fc <- forecast(mod, newdata = simdat$data_test, type = 'expected')
#' plot(fc, series = 1)
#' plot(fc, series = 2)
#' plot(fc, series = 3)
#'
#' # Dynamic trend extrapolations
#' fc <- forecast(mod, newdata = simdat$data_test, type = 'trend')
#' plot(fc, series = 1)
#' plot(fc, series = 2)
#' plot(fc, series = 3)
#' }
#' @name plot_mvgam_forecasts
NULL

#' @rdname plot_mvgam_forecasts
#' @export
plot_mvgam_fc = function(
  object,
  series = 1,
  newdata,
  data_test,
  realisations = FALSE,
  n_realisations = 15,
  hide_xlabels = FALSE,
  xlab,
  ylab,
  ylim,
  n_cores = 1,
  return_forecasts = FALSE,
  return_score = FALSE,
  ...
) {
  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  if (sign(series) != 1) {
    stop('argument "series" must be a positive integer', call. = FALSE)
  } else {
    if (series %% 1 != 0) {
      stop('argument "series" must be a positive integer', call. = FALSE)
    }
  }

  if (series > NCOL(object$ytimes)) {
    stop(
      paste0(
        'object only contains data / predictions for ',
        NCOL(object$ytimes),
        ' series'
      ),
      call. = FALSE
    )
  }

  if (sign(n_realisations) != 1) {
    stop('argument "n_realisations" must be a positive integer', call. = FALSE)
  } else {
    if (n_realisations %% 1 != 0) {
      stop(
        'argument "n_realisations" must be a positive integer',
        call. = FALSE
      )
    }
  }

  if (return_score) {
    return_forecasts <- TRUE
  }

  if (missing(data_test) & missing("newdata")) {
    # Check if newdata already included in the model
    if (!is.null(object$test_data)) {
      data_test <- object$test_data
    }
  }

  if (!missing("newdata")) {
    data_test <- newdata

    # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
    if (terms(formula(object$call))[[2]] != 'y') {
      data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
    }
  }

  # Prediction indices for the particular series
  data_train <- object$obs_data
  ends <- seq(
    0,
    dim(mcmc_chains(object$model_output, 'ypred'))[2],
    length.out = NCOL(object$ytimes) + 1
  )
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes) + 1))])
  ends <- ends[-1]

  if (object$fit_engine == 'stan') {
    # For stan objects, ypred is stored as a vector in column-major order
    preds <- mcmc_chains(object$model_output, 'ypred')[,
      seq(
        series,
        dim(mcmc_chains(object$model_output, 'ypred'))[2],
        by = NCOL(object$ytimes)
      ),
      drop = FALSE
    ]
  } else {
    preds <- mcmc_chains(object$model_output, 'ypred')[,
      starts[series]:ends[series],
      drop = FALSE
    ]
  }

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
  if (!missing(data_test)) {
    # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
    if (terms(formula(object$call))[[2]] != 'y') {
      if (object$family %in% c('binomial', 'beta_binomial')) {
        resp_terms <- as.character(terms(formula(object$call))[[2]])
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        trial_name <- resp_terms[2]
        data_test$y <- data_test[[resp_terms[1]]]

        if (!exists(trial_name, data_test)) {
          stop(
            paste0('Variable ', trial_name, ' not found in newdata'),
            call. = FALSE
          )
        }
      } else {
        data_test$y <- data_test[[terms(formula(object$call))[[2]]]]
      }
    }

    if (!'y' %in% names(data_test)) {
      data_test$y <- rep(NA, NROW(data_test))
    }

    if (inherits(data_test, 'list')) {
      if (!'time' %in% names(data_test)) {
        stop('data_test does not contain a "time" column')
      }

      if (!'series' %in% names(data_test)) {
        data_test$series <- factor('series1')
      }
    } else {
      if (!'time' %in% colnames(data_test)) {
        stop('data_test does not contain a "time" column')
      }

      if (!'series' %in% colnames(data_test)) {
        data_test$series <- factor('series1')
      }
    }

    # If the posterior predictions do not already cover the data_test period, the forecast needs to be
    # generated using the latent trend dynamics; note, this assumes that there is no gap between the training and
    # testing datasets
    if (inherits(data_test, 'list')) {
      all_obs <- c(
        data.frame(
          y = data_train$y,
          series = data_train$series,
          time = data_train$time
        ) %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y),
        data.frame(
          y = data_test$y,
          series = data_test$series,
          time = data_test$time
        ) %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y)
      )
    } else {
      all_obs <- c(
        data_train %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y),
        data_test %>%
          dplyr::filter(series == s_name) %>%
          dplyr::select(time, y) %>%
          dplyr::distinct() %>%
          dplyr::arrange(time) %>%
          dplyr::pull(y)
      )
    }

    if (dim(preds)[2] != length(all_obs)) {
      s_name <- levels(object$obs_data$series)[series]

      if (attr(object$model_data, 'trend_model') == 'None') {
        if (class(object$obs_data)[1] == 'list') {
          series_obs <- which(data_test$series == s_name)
          series_test <- lapply(data_test, function(x) {
            if (is.matrix(x)) {
              matrix(x[series_obs, ], ncol = NCOL(x))
            } else {
              x[series_obs]
            }
          })
        } else {
          series_test = data_test %>%
            dplyr::filter(series == s_name)
        }

        fc_preds <- predict.mvgam(
          object,
          newdata = series_test,
          type = 'response',
          n_cores = n_cores
        )
      } else {
        fc_preds <- forecast.mvgam(
          object,
          data_test = data_test,
          n_cores = n_cores
        )$forecasts[[series]]
      }
      preds <- cbind(preds, fc_preds)
    }
  }

  # Plot quantiles of the forecast distribution
  preds_last <- preds[1, ]
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(
    1:NCOL(preds),
    function(n) quantile(preds[, n], probs = probs, na.rm = TRUE)
  )

  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  if (missing(ylim)) {
    ytrain <- data.frame(
      series = data_train$series,
      time = data_train$time,
      y = data_train$y
    ) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)

    if (tolower(object$family) %in% c('beta', 'bernoulli')) {
      ylim <- c(
        min(cred, min(ytrain, na.rm = TRUE)),
        max(cred, max(ytrain, na.rm = TRUE))
      )
      ymin <- max(0, ylim[1])
      ymax <- min(1, ylim[2])
      ylim <- c(ymin, ymax)
    } else if (tolower(object$family) %in% c('lognormal', 'gamma')) {
      ylim <- c(
        min(cred, min(ytrain, na.rm = TRUE)),
        max(cred, max(ytrain, na.rm = TRUE))
      )
      ymin <- max(0, ylim[1])
      ymax <- max(ylim)
      ylim <- c(ymin, ymax)
    } else {
      ylim <- c(
        min(cred, min(ytrain, na.rm = TRUE)),
        max(cred, max(ytrain, na.rm = TRUE))
      )
    }
  }

  if (missing(ylab)) {
    ylab <- paste0('Predicitons for ', levels(data_train$series)[series])
  }

  if (missing(xlab)) {
    xlab <- 'Time'
  }

  pred_vals <- seq(1:length(preds_last))
  if (hide_xlabels) {
    plot(
      1,
      type = "n",
      bty = 'L',
      xlab = '',
      xaxt = 'n',
      ylab = ylab,
      xlim = c(0, length(preds_last)),
      ylim = ylim,
      ...
    )
  } else {
    plot(
      1,
      type = "n",
      bty = 'L',
      xlab = xlab,
      ylab = ylab,
      xaxt = 'n',
      xlim = c(0, length(preds_last)),
      ylim = ylim,
      ...
    )

    if (!missing(data_test)) {
      axis(
        side = 1,
        at = floor(seq(
          0,
          max(data_test$time) -
            (min(object$obs_data$time) - 1),
          length.out = 6
        )),
        labels = floor(seq(
          min(object$obs_data$time),
          max(data_test$time),
          length.out = 6
        ))
      )
    } else {
      axis(
        side = 1,
        at = floor(seq(
          0,
          max(object$obs_data$time) -
            (min(object$obs_data$time) - 1),
          length.out = 6
        )),
        labels = floor(seq(
          min(object$obs_data$time),
          max(object$obs_data$time),
          length.out = 6
        ))
      )
    }
  }

  if (realisations) {
    for (i in 1:n_realisations) {
      lines(x = pred_vals, y = preds[i, ], col = 'white', lwd = 2.5)
      lines(
        x = pred_vals,
        y = preds[i, ],
        col = sample(
          c("#DCBCBC", "#C79999", "#B97C7C", "#A25050", "#7C0000"),
          1
        ),
        lwd = 2.25
      )
    }
  } else {
    polygon(
      c(pred_vals, rev(pred_vals)),
      c(cred[1, ], rev(cred[9, ])),
      col = c_light,
      border = NA
    )
    polygon(
      c(pred_vals, rev(pred_vals)),
      c(cred[2, ], rev(cred[8, ])),
      col = c_light_highlight,
      border = NA
    )
    polygon(
      c(pred_vals, rev(pred_vals)),
      c(cred[3, ], rev(cred[7, ])),
      col = c_mid,
      border = NA
    )
    polygon(
      c(pred_vals, rev(pred_vals)),
      c(cred[4, ], rev(cred[6, ])),
      col = c_mid_highlight,
      border = NA
    )
    lines(pred_vals, cred[5, ], col = c_dark, lwd = 2.5)
  }
  box(bty = 'L', lwd = 2)

  if (!missing(data_test)) {
    if (class(data_train)[1] == 'list') {
      data_train <- data.frame(
        series = data_train$series,
        y = data_train$y,
        time = data_train$time
      )
      data_test <- data.frame(
        series = data_test$series,
        y = data_test$y,
        time = data_test$time
      )
    }

    last_train <- (NROW(data_train) / NCOL(object$ytimes))

    # Show historical (hindcast) distribution in grey
    if (!realisations) {
      polygon(
        c(
          pred_vals[1:(NROW(data_train) / NCOL(object$ytimes))],
          rev(pred_vals[1:(NROW(data_train) / NCOL(object$ytimes))])
        ),
        c(
          cred[1, 1:(NROW(data_train) / NCOL(object$ytimes))],
          rev(cred[9, 1:(NROW(data_train) / NCOL(object$ytimes))])
        ),
        col = 'grey70',
        border = NA
      )
      lines(
        pred_vals[1:(NROW(data_train) / NCOL(object$ytimes))],
        cred[5, 1:(NROW(data_train) / NCOL(object$ytimes))],
        col = 'grey70',
        lwd = 2.5
      )
    }

    # Plot training and testing points
    points(
      dplyr::bind_rows(data_train, data_test) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y),
      pch = 16,
      col = "white",
      cex = 0.8
    )
    points(
      dplyr::bind_rows(data_train, data_test) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y),
      pch = 16,
      col = "black",
      cex = 0.65
    )
    abline(v = last_train, col = '#FFFFFF60', lwd = 2.85)
    abline(v = last_train, col = 'black', lwd = 2.5, lty = 'dashed')

    # Calculate out of sample probabilistic score
    truth <- as.matrix(
      data_test %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    )
    last_train <- length(
      data_train %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)
    )

    fc <- preds[, (last_train + 1):NCOL(preds)]

    if (all(is.na(truth))) {
      score <- NULL
      message(
        'No non-missing values in data_test$y; cannot calculate forecast score'
      )
    } else {
      if (
        object$family %in%
          c(
            'poisson',
            'negative binomial',
            'tweedie',
            'binomial',
            'beta_binomial'
          )
      ) {
        if (max(fc, na.rm = TRUE) > 50000) {
          score <- sum(
            crps_mcmc_object(as.vector(truth), fc)[, 1],
            na.rm = TRUE
          )
          message(paste0('Out of sample CRPS:\n', score))
        } else {
          score <- sum(
            drps_mcmc_object(as.vector(truth), fc)[, 1],
            na.rm = TRUE
          )
          message(paste0('Out of sample DRPS:\n', score))
        }
      } else {
        score <- sum(crps_mcmc_object(as.vector(truth), fc)[, 1], na.rm = TRUE)
        message(paste0('Out of sample CRPS:\n', score))
      }
    }
  } else {
    if (class(data_train)[1] == 'list') {
      data_train <- data.frame(
        series = data_train$series,
        y = data_train$y,
        time = data_train$time
      )
    }

    points(
      data_train %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y),
      pch = 16,
      col = "white",
      cex = 0.8
    )
    points(
      data_train %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y),
      pch = 16,
      col = "black",
      cex = 0.65
    )
  }

  if (return_forecasts) {
    if (return_score) {
      if (!missing(data_test)) {
        return(list(
          forecast = preds[, (last_train + 1):NCOL(preds)],
          score = score
        ))
      } else {
        return(list(forecast = preds, score = NULL))
      }
    } else {
      if (!missing(data_test)) {
        return(preds[, (last_train + 1):NCOL(preds)])
      } else {
        return(preds)
      }
    }
  }
}

#' @rdname plot_mvgam_forecasts
#' @param x Object of class `mvgam_forecast`
#' @method plot mvgam_forecast
#' @export
plot.mvgam_forecast = function(
  x,
  series = 1,
  realisations = FALSE,
  n_realisations = 15,
  xlab,
  ylab,
  ylim,
  ...
) {
  object <- x
  validate_pos_integer(series)
  validate_pos_integer(n_realisations)

  if (series > length(object$series_names)) {
    stop(
      paste0(
        'object only contains data / predictions for ',
        length(object$series_names),
        ' series'
      ),
      call. = FALSE
    )
  }

  s_name <- object$series_names[series]
  if (!s_name %in% names(object$hindcasts)) {
    stop(
      paste0('forecasts for ', s_name, ' have not yet been computed'),
      call. = FALSE
    )
  }

  # Extract hindcast and forecast predictions
  type <- object$type

  preds <- cbind(
    object$hindcasts[[which(names(object$hindcasts) == s_name)]],
    object$forecasts[[which(names(object$forecasts) == s_name)]]
  )

  # Plot quantiles of the forecast distribution
  probs <- c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(
    1:NCOL(preds),
    function(n) quantile(preds[, n], probs = probs, na.rm = TRUE)
  )

  if (type == 'trend') {
    if (missing(ylab)) {
      ylab <- paste0('Estimated trend for ', s_name)
    }
  }

  if (type == 'link') {
    if (missing(ylab)) {
      ylab <- paste0('Linear predictions for ', s_name)
    }
  }

  if (type == 'expected') {
    if (missing(ylab)) {
      ylab <- paste0('Expectations for ', s_name)
    }
  }

  if (type == 'detection') {
    if (missing(ylab)) {
      ylab <- paste0('Pr(detection) for ', s_name)
    }
  }

  if (type == 'latent_N') {
    if (missing(ylab)) {
      ylab <- paste0('Latent abundance for ', s_name)
    }
  }

  if (type == 'response') {
    if (missing(ylab)) {
      ylab <- paste0('Predictions for ', s_name)
    }
  }

  if (missing(xlab)) {
    xlab <- 'Time'
  }

  # Create a base plot using posterior credible intervals and observations
  # for the specified series
  plot_dat <- data.frame(
    time = c(
      object$train_times[[which(names(object$hindcasts) == s_name)]],
      object$test_times[[which(names(object$hindcasts) == s_name)]]
      ),
    med = cred[5, ],
    lower1 = cred[1, ],
    lower2 = cred[2, ],
    lower3 = cred[3, ],
    lower4 = cred[4, ],
    upper1 = cred[9, ],
    upper2 = cred[8, ],
    upper3 = cred[7, ],
    upper4 = cred[6, ],
    truth = c(
      object$train_observations[[s_name]],
      object$test_observations[[s_name]]
    )
  )

  base_plot <- ggplot2::ggplot(
    data = plot_dat,
    mapping = ggplot2::aes(x = time, y = truth)
  ) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = xlab, y = ylab)

  # Add to the base plot accordingly
  if (realisations) {
    for (i in 1:n_realisations) {
      base_plot <- base_plot +
        ggplot2::geom_line(
          data = data.frame(
            y = preds[i, ],
            time = c(
              object$train_times[[which(names(object$hindcasts) == s_name)]],
              object$test_times[[which(names(object$hindcasts) == s_name)]]
            )
          ),
          mapping = ggplot2::aes(x = time, y = y),
          col = "white",
          linewidth = 1
        ) +
        ggplot2::geom_line(
          data = data.frame(
            y = preds[i, ],
            time = c(
              object$train_times[[which(names(object$hindcasts) == s_name)]],
              object$test_times[[which(names(object$hindcasts) == s_name)]]
            )
          ),
          mapping = ggplot2::aes(x = time, y = y),
          col = sample(
            c("#DCBCBC", "#C79999", "#B97C7C", "#A25050", "#7C0000"),
            1
          ),
          linewidth = 0.75
        )
    }
  } else {
    base_plot <- base_plot +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(ymin = lower1, ymax = upper1),
        fill = "#DCBCBC"
      ) +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(ymin = lower2, ymax = upper2),
        fill = "#C79999"
      ) +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(ymin = lower3, ymax = upper3),
        fill = "#B97C7C"
      ) +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(ymin = lower4, ymax = upper4),
        fill = "#A25050"
      ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(x = time, y = med),
        col = "#8F2727",
        linewidth = 1
      )
  }

  # Show historical (hindcast) distribution in grey if this object
  # contains forecasts
  train_times <- object$train_times[[s_name]]
  last_train <- length(object$train_observations[[s_name]])
  if (type == 'response' & !is.null(object$forecasts)) {
    if (!realisations) {
      base_plot <- base_plot +
        ggplot2::geom_line(
          data = data.frame(
            time = train_times,
            lower1 = cred[1, 1:last_train],
            upper1 = cred[9, 1:last_train],
            med = cred[5, 1:last_train],
            truth = 0
          ),
          mapping = ggplot2::aes(x = time, y = med),
          col = "white",
          linewidth = 1
        ) +
        ggplot2::geom_ribbon(
          data = data.frame(
            time = train_times,
            lower1 = cred[1, 1:last_train],
            upper1 = cred[9, 1:last_train],
            truth = 0
          ),
          mapping = ggplot2::aes(ymin = lower1, ymax = upper1),
          fill = "grey70"
        )
    }
  }

  if (
    type == 'response' || c(type == 'expected' & object$family == 'bernoulli')
  ) {
    # Plot training and testing points
    base_plot <- base_plot +
      ggplot2::geom_point(pch = 21, col = 'white', fill = 'black')

    # Calculate out of sample probabilistic score
    fc <- object$forecasts[[s_name]]
    truth <- object$test_observations[[s_name]]

    if (all(is.na(truth))) {
      score <- NULL
      message(paste0(
        'No non-missing values in test_observations; cannot calculate forecast score\n'
      ))
    } else {
      if (
        object$family %in%
          c(
            'poisson',
            'negative binomial',
            'tweedie',
            'binomial',
            'beta_binomial'
          )
      ) {
        if (max(fc, na.rm = TRUE) > 50000) {
          score <- sum(
            crps_mcmc_object(as.vector(truth), fc)[, 1],
            na.rm = TRUE
          )
          message(paste0('Out of sample CRPS:\n', score))
        } else {
          score <- sum(
            drps_mcmc_object(as.vector(truth), fc)[, 1],
            na.rm = TRUE
          )
          message(paste0('Out of sample DRPS:\n', score))
        }
      } else if (object$family == 'bernoulli') {
        score <- sum(brier_mcmc_object(as.vector(truth), fc)[, 1], na.rm = TRUE)
        message(paste0('Out of sample Brier:\n', score))
      } else {
        score <- sum(crps_mcmc_object(as.vector(truth), fc)[, 1], na.rm = TRUE)
        message(paste0('Out of sample CRPS:\n', score))
      }
    }
  }

  if (!is.null(object$forecasts)) {
    base_plot <- base_plot +
      ggplot2::geom_vline(
        xintercept = max(train_times), linetype = 'dashed'
      )
  }

  if (!missing(ylim)) {
    base_plot <- base_plot +
      ggplot2::scale_y_continuous(limits = ylim)
  }

  base_plot
}
