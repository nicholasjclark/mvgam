#'Pairs plot for latent trend parameters from a fitted mvgam model
#'
#'This function takes a fitted \code{mvjagam} object and returns a pairs plot for the trend parameters. Note,
#'only models without dynamic factor trends (\code{use_lv == FALSE}) are currently supported
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@author Nicholas J Clark
#'@details A total base \code{R} plot is generated to examine possible correlations in posterior
#'estimates for latent trend parameters
#'@return A series of base \code{R} plots
#'@export
plot_mvgam_pairs = function(object, series = 1){
  if(object$use_lv == T){
    stop('trend pairs plots not available for dynamic factor models')
  }

  # Extract trend estimates for the particular series
  if(NCOL(object$ytimes) > 1){
    params <- paste0(c('phi', 'ar1', 'ar2', 'ar3'), '\\[', series, '\\]')
  } else {
    params <- c('phi', 'ar1', 'ar2', 'ar3')
  }
  phis <- MCMCvis::MCMCchains(object$jags_output, params, ISB = F)

  # Set graphical parameters and build the pairs plot
  .pardefault <- par(no.readonly=T)
  par(.pardefault)

  par(mfrow = c(4,4))
  hist(phis[,1], col = "#B97C7C", xlab = '', ylab = '', main = 'drift')
  plot(phis[,2], phis[,1], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'drift', xlab = 'ar1')
  plot(phis[,3], phis[,1], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'drift', xlab = 'ar2')
  plot(phis[,4], phis[,1], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'drift', xlab = 'ar3')
  plot(phis[,1], phis[,2], pch = 16, col = "#8F2727", cex = 0.6,
       xlab = 'drift', ylab = 'ar1')
  hist(phis[,2], col = "#B97C7C", xlab = '', ylab = '', main = 'ar1')
  plot(phis[,3], phis[,2], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'ar1', xlab = 'ar2')
  plot(phis[,4], phis[,2], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'ar1', xlab = 'ar3')
  plot(phis[,1], phis[,3], pch = 16, col = "#8F2727", cex = 0.6,
       xlab = 'drift', ylab = 'ar2')
  plot(phis[,2], phis[,3], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'ar2', xlab = 'ar1')
  hist(phis[,3], col = "#B97C7C", xlab = '', ylab = '', main = 'ar2')
  plot(phis[,4], phis[,3], pch = 16, col = "#8F2727", cex = 0.6,
       ylab = 'ar2', xlab = 'ar3')
  plot(phis[,1], phis[,4], pch = 16, col = "#8F2727", cex = 0.6,
       xlab = 'drift', ylab = 'ar3')
  plot(phis[,2], phis[,4], pch = 16, col = "#8F2727", cex = 0.6,
       xlab = 'ar1', ylab = 'ar3')
  plot(phis[,3], phis[,4], pch = 16, col = "#8F2727", cex = 0.6,
       xlab = 'ar2', ylab = 'ar3')
  hist(phis[,4], col = "#B97C7C", xlab = '', ylab = '', main = 'ar3')

invisible()
par(.pardefault)

}
