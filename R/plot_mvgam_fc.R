#'Plot mvgam posterior predictions for a specified series
#'@param object \code{list} object returned from \code{mvgam}
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
#'@details Posterior predictions are drawn from the fitted \code{mvgam} and used to calculate posterior
#'empirical quantiles. If \code{realisations = FALSE}, these posterior quantiles are plotted along
#'with the true observed data that was used to train the model. Otherwise, a spaghetti plot is returned
#'to show possible forecast paths.
#'@return A base \code{R} graphics plot and an optional \code{matrix} of the forecast distribution
#'@export
plot_mvgam_fc = function(object, series = 1, newdata, data_test,
                         realisations = FALSE, n_realisations = 15,
                         hide_xlabels = FALSE, xlab, ylab, ylim,
                         n_cores = 1,
                         return_forecasts = FALSE, ...){

  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(sign(series) != 1){
    stop('argument "series" must be a positive integer',
         call. = FALSE)
  } else {
    if(series%%1 != 0){
      stop('argument "series" must be a positive integer',
           call. = FALSE)
    }
  }

  if(sign(n_realisations) != 1){
    stop('argument "n_realisations" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_realisations%%1 != 0){
      stop('argument "n_realisations" must be a positive integer',
           call. = FALSE)
    }
  }

  if(!missing("newdata")){
    data_test <- newdata
  }

  # Prediction indices for the particular series
  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  if(object$fit_engine == 'stan'){

    # For stan objects, ypred is stored as a vector in column-major order
    preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,seq(series,
                                                                    dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
                                                                    by = NCOL(object$ytimes))]
  } else {
    preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]]
  }

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
  if(!missing(data_test)){

    if(!'y' %in% names(data_test)){
      data_test$y <- rep(NA, NROW(data_test))
    }

    if(class(data_test)[1] == 'list'){
      if(!'time' %in% names(data_test)){
        stop('data_test does not contain a "time" column')
      }

      if(!'series' %in% names(data_test)){
        data_test$series <- factor('series1')
      }

    } else {
      if(!'time' %in% colnames(data_test)){
        stop('data_test does not contain a "time" column')
      }

      if(!'series' %in% colnames(data_test)){
        data_test$series <- factor('series1')
      }
    }

  # If the posterior predictions do not already cover the data_test period, the forecast needs to be
  # generated using the latent trend dynamics; note, this assumes that there is no gap between the training and
  # testing datasets
    if(class(data_train)[1] == 'list'){
      all_obs <- c(data.frame(y = data_train$y,
                              series = data_train$series,
                              time = data_train$time) %>%
                     dplyr::filter(series == s_name) %>%
                     dplyr::select(time, y) %>%
                     dplyr::distinct() %>%
                     dplyr::arrange(time) %>%
                     dplyr::pull(y),
                   data.frame(y = data_test$y,
                              series = data_test$series,
                              time = data_test$time) %>%
                     dplyr::filter(series == s_name) %>%
                     dplyr::select(time, y) %>%
                     dplyr::distinct() %>%
                     dplyr::arrange(time) %>%
                     dplyr::pull(y))
    } else {
      all_obs <- c(data_train %>%
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
                     dplyr::pull(y))
    }


  if(dim(preds)[2] != length(all_obs)){
    if(object$trend_model == 'None'){

      if(class(object$obs_data)[1] == 'list'){
        series_obs <- which(data_test$series == !!(levels(object$obs_data$series)[series]))
        series_test <- lapply(data_assim, function(x){
          if(is.matrix(x)){
            matrix(x[series_obs,], ncol = NCOL(x))
          } else {
            x[series_obs]
          }

        })
      } else {
        series_test = data_test %>%
          dplyr::filter(series == !!(levels(object$obs_data$series)[series]))
      }

      fc_preds <- predict.mvgam(object, newdata = series_test,
                                type = 'response',
                                n_cores = n_cores)
    } else {
      fc_preds <- forecast.mvgam(object, data_test = data_test,
                                 n_cores = n_cores)
    }
    preds <- cbind(preds, fc_preds)
  }

  }

  # Plot quantiles of the forecast distribution
  preds_last <- preds[1,]
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs, na.rm = TRUE))

  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  if(missing(ylim)){
    ytrain <- data.frame(series = data_train$series,
                         time = data_train$time,
                         y = data_train$y) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)
    ylim <- c(min(cred, min(ytrain, na.rm = TRUE)),
              max(cred, max(ytrain, na.rm = TRUE)) + 2)
  }

  if(missing(ylab)){
    ylab <- paste0('Predicitons for ', levels(data_train$series)[series])
  }

  if(missing(xlab)){
    xlab <- 'Time'
  }

  pred_vals <- seq(1:length(preds_last))
  if(hide_xlabels){
    plot(1, type = "n", bty = 'L',
         xlab = '',
         xaxt = 'n',
         ylab = ylab,
         xlim = c(0, length(preds_last)),
         ylim = ylim, ...)
  } else {
    plot(1, type = "n", bty = 'L',
         xlab = xlab,
         ylab = ylab,
         xlim = c(0, length(preds_last)),
         ylim = ylim, ...)
  }

  if(realisations){
    for(i in 1:n_realisations){
      lines(x = pred_vals,
            y = preds[i,],
            col = 'white',
            lwd = 2.5)
      lines(x = pred_vals,
            y = preds[i,],
            col = sample(c("#DCBCBC",
                           "#C79999",
                           "#B97C7C",
                           "#A25050",
                           "#7C0000"), 1),
            lwd = 2.25)
    }
  } else {
    polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
  }
  box(bty = 'L', lwd = 2)

  if(!missing(data_test)){

    if(class(data_train)[1] == 'list'){
      data_train <- data.frame(series = data_train$series,
                          y = data_train$y,
                          time = data_train$time)
      data_test <- data.frame(series = data_test$series,
                               y = data_test$y,
                               time = data_test$time)
    }

    last_train <- (NROW(data_train) / NCOL(object$ytimes))

    # Show historical (hindcast) distribution in grey
    if(!realisations){
      polygon(c(pred_vals[1:(NROW(data_train) / NCOL(object$ytimes))],
                rev(pred_vals[1:(NROW(data_train) / NCOL(object$ytimes))])),
              c(cred[1,1:(NROW(data_train) / NCOL(object$ytimes))],
                rev(cred[9,1:(NROW(data_train) / NCOL(object$ytimes))])),
              col = 'grey70', border = NA)
      lines(pred_vals[1:(NROW(data_train) / NCOL(object$ytimes))],
            cred[5,1:(NROW(data_train) / NCOL(object$ytimes))],
            col = 'grey70', lwd = 2.5)
    }

    # Plot training and testing points
    points(dplyr::bind_rows(data_train, data_test) %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(time, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(time) %>%
             dplyr::pull(y), pch = 16, col = "white", cex = 0.8)
    points(dplyr::bind_rows(data_train, data_test) %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(time, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(time) %>%
             dplyr::pull(y), pch = 16, col = "black", cex = 0.65)
    abline(v = last_train, col = '#FFFFFF60', lwd = 2.85)
    abline(v = last_train, col = 'black', lwd = 2.5, lty = 'dashed')

    # Calculate out of sample DRPS and print the score
    drps_score <- function(truth, fc, interval_width = 0.9){
      nsum <- 1000
      Fy = ecdf(fc)
      ysum <- 0:nsum
      indicator <- ifelse(ysum - truth >= 0, 1, 0)
      score <- sum((indicator - Fy(ysum))^2)

      # Is value within empirical interval?
      interval <- quantile(fc, probs = c((1-interval_width)/2, (interval_width + (1-interval_width)/2)),
                           na.rm = TRUE)
      in_interval <- ifelse(truth <= interval[2] & truth >= interval[1], 1, 0)
      return(c(score, in_interval))
    }

    # Wrapper to operate on all observations in fc_horizon
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

    truth <- as.matrix(data_test %>%
                         dplyr::filter(series == s_name) %>%
                         dplyr::select(time, y) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::pull(y))
    last_train <- length(data_train %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y))

    fc <- preds[,(last_train+1):NCOL(preds)]


    if(all(is.na(truth))){
      message('No non-missing values in data_test$y; cannot calculate DRPS')
      message()
    } else {
      message('Out of sample DRPS:')
      print(sum(drps_mcmc_object(as.vector(truth),
                                 fc)[,1], na.rm = TRUE))
      message()
    }

  } else {
    if(class(data_train)[1] == 'list'){
      data_train <- data.frame(series = data_train$series,
                               y = data_train$y,
                               time = data_train$time)
    }

    points(data_train %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(time, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(time) %>%
             dplyr::pull(y),pch = 16, col = "white", cex = 0.8)
    points(data_train %>%
            dplyr::filter(series == s_name) %>%
            dplyr::select(time, y) %>%
            dplyr::distinct() %>%
            dplyr::arrange(time) %>%
            dplyr::pull(y),pch = 16, col = "black", cex = 0.65)
  }

  if(return_forecasts){
    if(!missing(data_test)){
      return(preds[,(last_train+1):NCOL(preds)])
    } else {
      return(preds)
    }

  }

}
