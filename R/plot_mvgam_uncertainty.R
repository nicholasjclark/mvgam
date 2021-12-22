#'Plot mvjagam forecast uncertainty contributions for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_test A \code{dataframe} containing at least 'series', 'season' and 'year' for the forecast horizon, in
#'addition to any other variables included in the linear predictor of \code{formula}
#'@param legend_position The location may also be specified by setting x to a single keyword from the
#'list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location.
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}
#'@export
plot_mvgam_uncertainty = function(object, series, data_test, legend_position = 'topleft',
                                  hide_xlabels = FALSE){
  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  # Generate linear predictor matrix for specified series
  series_test <- data_test[which(data_test$series ==
                                   levels(data_train$series)[series]),]
  Xp <- predict(object$mgcv_model, newdata = series_test,
                type = 'lpmatrix')

  # Extract beta coefs
  betas <- MCMCvis::MCMCchains(object$jags_output, 'b')

  # Extract current trend estimates
  trend <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[series]:ends[series]]

  if(length(unique(data_train$series)) == 1){
    trend <- matrix(trend[, NCOL(trend)])
  } else {
    trend <- trend[,(NROW(data_train) / NCOL(object$ytimes)+1):NCOL(trend)]
  }

  n_samples <- NROW(trend)
  size <- MCMCvis::MCMCsummary(object$jags_output, 'r')$mean

  # Full uncertainty interval
  preds <- matrix(NA, nrow = n_samples, ncol = NROW(series_test))
  for(i in 1:n_samples){
    preds[i,] <- rnbinom(NROW(series_test), mu = exp(Xp %*% betas[i,] + trend[i,]),
                         size = size)
  }
  full_int <- apply(preds,
                    2, hpd, 0.8)
  full_int[full_int<0] <- 0

  # GAM only interval
  preds <- matrix(NA, nrow = n_samples, ncol = NROW(series_test))
  for(i in 1:n_samples){
    preds[i,] <- rnbinom(NROW(series_test), mu = exp(Xp %*% betas[i,]),
                         size = size)
  }
  gam_int <- apply(preds,
                   2, hpd, 0.8)
  gam_int[gam_int<0] <- 0

  # GAM uncertainty contribution
  gam_cont <- (gam_int[3,] - gam_int[1,]) /
    (full_int[3,] - full_int[1,])
  gam_cont[is.na(gam_cont)] <- 0.5
  gam_cont[gam_cont > 1] <- 1

  # Plot and return
  if(hide_xlabels){
  plot(gam_cont,
       ylim=c(0,1),type='n',
       ylab=paste0('Uncertainty contributions for ', levels(data_train$series)[series]),
       xlab="", xaxt = 'n')
   } else {
      plot(gam_cont,
           ylim=c(0,1),type='n',
           ylab=paste0('Uncertainty contributions for ', levels(data_train$series)[series]),
           xlab="Forecast horizon")
    }

  polygon(c(seq(1:(NCOL(gam_int))), rev(seq(1:NCOL(gam_int)))),
          c(gam_cont, rep(0, NCOL(gam_int))),
          col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
  polygon(c(seq(1:(NCOL(gam_int))), rev(seq(1:NCOL(gam_int)))),
          c(gam_cont, rep(1, NCOL(gam_int))),
          col = 'gray70', border = NA)
  legend(legend_position,legend=c("Trend","GAM"),
         bg = 'white',
         col=c('gray70',
               rgb(150, 0, 0, max = 255, alpha = 180)),lty=1,lwd=6)


}
