#'Plot mvjagam latent trend for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_test Optional \code{dataframe} of test data containing at least 'series', 'season' and 'year'
#'forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}
#'@export
plot_mvgam_trend = function(object, series, data_test, hide_xlabels = FALSE){
  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  preds <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[series]:ends[series]]
  preds_last <- preds[1,]
  pred_vals <- seq(1:length(preds_last))

  # Plot quantiles of the smooth function, along with observed values
  # if specified
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

  if(hide_xlabels){
    plot(1, type = "n",
         xlab = '',
         xaxt = 'n',
         ylab = paste0('Estimated trend for ', levels(data_train$series)[series]),
         xlim = c(0, length(preds_last)),
         ylim = range(cred))

  } else {
    plot(1, type = "n",
         xlab = 'Time',
         ylab = paste0('Estimated trend for ', levels(data_train$series)[series]),
         xlim = c(0, length(preds_last)),
         ylim = range(cred))
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

  if(!missing(data_test)){
    abline(v = NROW(data_train) / NCOL(object$ytimes), lty = 'dashed')
  }

}
