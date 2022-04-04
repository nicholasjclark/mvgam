#'Plot mvjagam forecast uncertainty contributions for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_test A \code{dataframe} or \code{list} containing at least 'series', 'season' and 'year' for the forecast horizon, in
#'addition to any other variables included in the linear predictor of \code{formula}
#'@param legend_position The location may also be specified by setting x to a single keyword from the
#'list: "none", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location (if it is not "none").
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
  if(class(data_test) == 'list'){
    list_names <- names(data_test)
    indices_keep <- which(data_test$series ==
                            levels(data_train$series)[series])
    series_test <- lapply(data_test, function(x){
        if(is.matrix(x)){
          matrix(x[indices_keep,], ncol = NCOL(x))
        } else {
          x[indices_keep]
        }

      })
    names(series_test) <- list_names

  } else {
    series_test <- data_test[which(data_test$series ==
                                     levels(data_train$series)[series]),]
  }

  Xp <- predict(object$mgcv_model, newdata = series_test,
                type = 'lpmatrix')

  # Extract beta coefs
  betas <- MCMCvis::MCMCchains(object$jags_output, 'b')

  # Extract current trend estimates
  trend <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[series]:ends[series]]

  if(length(unique(data_train$series)) == 1){
    trend <- matrix(trend[, NCOL(trend)])
  } else {
    if(class(data_test) == 'list'){
      trend <- trend[,(length(data_train$series) / NCOL(object$ytimes)+1):NCOL(trend)]
    } else {
      trend <- trend[,(NROW(data_train) / NCOL(object$ytimes)+1):NCOL(trend)]

    }
  }

  n_samples <- NROW(trend)

  # Full uncertainty interval for the mean
  if(class(data_test) == 'list'){
    ncols <- length(series_test$series)
  } else {
    ncols <- NROW(series_test)
  }

  preds <- matrix(NA, nrow = n_samples, ncol = ncols)
  for(i in 1:n_samples){
    preds[i,] <- Xp %*% betas[i,] + trend[i,]
  }
  full_int <- apply(preds,
                    2, hpd, 0.8)
  full_int[full_int<0] <- 0

  # GAM only interval
  preds <- matrix(NA, nrow = n_samples, ncol = ncols)
  for(i in 1:n_samples){
    preds[i,] <- Xp %*% betas[i,]
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
          col = "#7C0000", border = NA)
  polygon(c(seq(1:(NCOL(gam_int))), rev(seq(1:NCOL(gam_int)))),
          c(gam_cont, rep(1, NCOL(gam_int))),
          col = '#DCBCBC', border = NA)
  if(legend_position != 'none'){
    legend(legend_position,legend=c("Trend","GAM"),
           bg = 'white',
           col=c('#DCBCBC',
                 "#7C0000"),lty=1,lwd=6)
  }

}
