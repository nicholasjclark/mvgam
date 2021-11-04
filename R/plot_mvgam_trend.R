#'Plot mvjagam latent trend for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_train A \code{dataframe} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; NAs allowed)
#''series' (character or factor index of the series IDs)
#''season' (numeric index of the seasonal time point for each observation; should not have any missing)
#''year' the numeric index for year
#''in_season' indicator for whether the observation is in season or not. If the counts tend to go to zero
#'during the off season (as in tick counts for example), setting this to zero can be useful as trends won't contribute during
#'during this time but they continue to evolve, allowing the trend from the past season to continue evolving rather than forcing
#'it to zero
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_test A \code{dataframe} containing at least 'series', 'season', 'year' and 'in_season' for the forecast horizon, in
#'addition to any other variables included in the linear predictor of \code{formula}
#'@export
plot_mvgam_trend = function(object, series, data_test, data_train){
  pred_indices <- seq(0, length(object$ytimes),
                      length.out = NCOL(object$ytimes) + 1)
  preds <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,pred_indices[series]:pred_indices[series + 1]]
  preds_last <- preds[1,]
  plot(preds_last,
       type = 'l', ylim = range(preds),
       col = rgb(1,0,0, alpha = 0),
       ylab = paste0('Estimated trend for ', levels(data_train$series)[series]),
       xlab = 'Time')
  int <- apply(preds,
               2, hpd, 0.95)
  polygon(c(seq(1:(NCOL(int))), rev(seq(1:NCOL(int)))),
          c(int[1,],rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
  int <- apply(preds,
               2, hpd, 0.68)
  polygon(c(seq(1:(NCOL(int))), rev(seq(1:NCOL(int)))),
          c(int[1,],rev(int[3,])),
          col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
  lines(int[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
  abline(v = NROW(data_train) / NCOL(object$ytimes))

}
