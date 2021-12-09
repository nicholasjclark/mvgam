#'Plot mvjagam posterior predictions for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_test Optional \code{dataframe} of test data containing at least 'series', 'season', 'year' and
#''in_season' for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}
#'@param ylim Optional \code{vector} of y-axis limits (min, max)
#'@export
plot_mvgam_fc = function(object, series, data_test, hide_xlabels = FALSE, ylim){

  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
  preds_last <- preds[1,]
  int <- apply(preds,
               2, hpd, 0.95)

  if(missing(ylim)){
    ylim <- c(0, max(int) + 2)
  }

  if(hide_xlabels){
    plot(preds_last,
         type = 'l', ylim = ylim ,
         col = rgb(1,0,0, alpha = 0),
         ylab = paste0('Estimated counts for ', levels(data_train$series)[series]),
         xlab = '', xaxt = 'n')
  } else {
    plot(preds_last,
         type = 'l', ylim = ylim,
         col = rgb(1,0,0, alpha = 0),
         ylab = paste0('Estimated counts for ', levels(data_train$series)[series]),
         xlab = 'Time')
  }

  int[int<0] <- 0
  polygon(c(seq(1:(NCOL(int))), rev(seq(1:NCOL(int)))),
          c(int[1,],rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
  int <- apply(preds,
               2, hpd, 0.68)
  int[int<0] <- 0
  polygon(c(seq(1:(NCOL(int))), rev(seq(1:NCOL(int)))),
          c(int[1,],rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
  lines(int[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')

  s_name <- levels(data_train$series)[series]
  if(!missing(data_test)){
    points(rbind(data_train, data_test) %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(year, season, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(year, season) %>%
             dplyr::pull(y), pch = 16)
    abline(v = NROW(data_train) / NCOL(object$ytimes), lty = 'dashed')
  } else {
    points(data_train %>%
             dplyr::filter(series == s_name) %>%
             dplyr::select(year, season, y) %>%
             dplyr::distinct() %>%
             dplyr::arrange(year, season) %>%
             dplyr::pull(y), pch = 16)
  }

}
