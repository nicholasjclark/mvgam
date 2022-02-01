#'Plot mvjagam posterior predictions for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_test Optional \code{dataframe} or \code{list} of test data containing at least 'series', 'season' and 'year'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}. If
#'included, the test values are shown as points on the plot to help visualise the accuracy of the model's forecast.
#'Note this is only useful if the same \code{data_test} was also included when fitting the original model.
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}
#'@param ylab Optional \code{character} string specifying the y-axis label
#'@param ylim Optional \code{vector} of y-axis limits (min, max)
#'@details Posterior predictions are drawn from the fitted \code{mvjagam} and used to calculate posterior
#'empirical quantiles. These are plotted along with the true observed data
#'that was used to train the model.
#'@return A base \code{R} graphics plot
#'@export
plot_mvgam_fc = function(object, series = 1, data_test, hide_xlabels = FALSE, ylab, ylim){

  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
  preds_last <- preds[1,]

  # Plot quantiles of the forecast distribution
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs))

  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  if(missing(ylim)){
    ylim <- c(min(cred), max(cred) + 2)
  }

  if(missing(ylab)){
    ylab <- paste0('Predicitons for ', levels(data_train$series)[series])
  }

  pred_vals <- seq(1:length(preds_last))
  if(hide_xlabels){
    plot(1, type = "n",
         xlab = '',
         xaxt = 'n',
         ylab = ylab,
         xlim = c(0, length(preds_last)),
         ylim = ylim)
  } else {
    plot(1, type = "n",
         xlab = 'Time',
         ylab = ylab,
         xlim = c(0, length(preds_last)),
         ylim = ylim)
  }

  polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
          col = c_light, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
          col = c_light_highlight, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
          col = c_mid, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
          col = c_mid_highlight, border = NA)
  lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)

  s_name <- levels(data_train$series)[series]
  if(!missing(data_test)){

    if(class(data_train) == 'list'){
      data_train <- data.frame(series = data_train$series,
                          y = data_train$y,
                          season = data_train$season,
                          year = data_train$year)
      data_test <- data.frame(series = data_test$series,
                               y = data_test$y,
                               season = data_test$season,
                               year = data_test$year)
    }

    points(dplyr::bind_rows(data_train, data_test) %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(year, season, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(year, season) %>%
             dplyr::pull(y), pch = 16, col = "white", cex = 0.65)
    points(dplyr::bind_rows(data_train, data_test) %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(year, season, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(year, season) %>%
             dplyr::pull(y), pch = 16, col = "black", cex = 0.55)
    abline(v = NROW(data_train) / NCOL(object$ytimes), lty = 'dashed')
  } else {
    if(class(data_train) == 'list'){
      data_train <- data.frame(series = data_train$series,
                               y = data_train$y,
                               season = data_train$season,
                               year = data_train$year)
    }

    points(data_train %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(year, season, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(year, season) %>%
             dplyr::pull(y),pch = 16, col = "white", cex = 0.65)
    points(data_train %>%
            dplyr::filter(series == s_name) %>%
            dplyr::select(year, season, y) %>%
            dplyr::distinct() %>%
            dplyr::arrange(year, season) %>%
            dplyr::pull(y),pch = 16, col = "black", cex = 0.55 )
  }

}
