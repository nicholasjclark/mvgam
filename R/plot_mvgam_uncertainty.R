#'Plot mvgam forecast uncertainty contributions for a specified series
#'@importFrom graphics legend
#'@importFrom stats predict
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata A \code{dataframe} or \code{list} containing at least 'series' and 'time' for the forecast horizon, in
#'addition to any other variables included in the linear predictor of \code{formula}
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param legend_position The location may also be specified by setting x to a single keyword from the
#'list: "none", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#'This places the legend on the inside of the plot frame at the given location (if it is not "none").
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}
#'@return A base \code{R} graphics plot
#'@export
plot_mvgam_uncertainty = function(object, series = 1, newdata,
                                  data_test, legend_position = 'topleft',
                                  hide_xlabels = FALSE){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
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

  if(series > NCOL(object$ytimes)){
    stop(paste0('object only contains data / predictions for ',
                NCOL(object$ytimes), ' series'),
         call. = FALSE)
  }

  if(!missing(newdata)){
    data_test <- newdata
  }

  if(missing(data_test) & missing(newdata)){
    if(!is.null(object$test_data)){
      data_test <- object$test_data
    } else {
      stop('No newdata supplied; cannot calculate uncertainty contributions',
           call. = FALSE)
    }
  }

  # Prediction indices for the particular series
  data_train <- object$obs_data
  ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  # Add series factor variable if missing
  if(class(data_train)[1] != 'list'){
    if(!'series' %in% colnames(data_test)){
      data_test$series <- factor('series1')
    }
  }

  if(class(data_train)[1] == 'list'){
    if(!'series' %in% names(data_test)){
      data_test$series <- factor('series1')
    }
  }

  # Generate linear predictor matrix for specified series
  if(class(data_test)[1] == 'list'){
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

  Xp <- obs_Xp_matrix(newdata = series_test, object$mgcv_model)

  # Extract beta coefs
  betas <- mcmc_chains(object$model_output, 'b')

  # Extract current trend estimates
  if(object$fit_engine == 'stan'){
    trend <- mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                    dim(mcmc_chains(object$model_output,
                                                                                            'trend'))[2],
                                                                    by = NCOL(object$ytimes))]
  } else {
    trend <- mcmc_chains(object$model_output, 'trend')[,starts[series]:ends[series]]
  }

  if(length(unique(data_train$series)) == 1){
    trend <- matrix(trend[, NCOL(trend)])
  } else {
    if(class(data_test)[1] == 'list'){
      trend <- trend[,(length(data_train$series) / NCOL(object$ytimes)+1):NCOL(trend)]
    } else {
      trend <- trend[,(NROW(data_train) / NCOL(object$ytimes)+1):NCOL(trend)]

    }
  }

  # Function to calculate intersection of two uncertainty distributions
  intersect_hist = function(fullpreds, gampreds){
    from <- min(min(fullpreds, na.rm = T),
                min(gampreds, na.rm = T))
    to <- max(max(fullpreds, na.rm = T),
              max(gampreds, na.rm = T))

    fullhist <- hist(fullpreds, breaks = seq(from, to, length.out = 100),
         plot = F)
    gamhist <- hist(gampreds, breaks = seq(from, to, length.out = 100),
         plot = F)

    sum(gamhist$density / max(gamhist$density)) /
    sum(fullhist$density / max(fullhist$density))
  }

  # Full predictions
  n_samples <- NROW(trend)
  if(class(data_test)[1] == 'list'){
    ncols <- length(series_test$series)
  } else {
    ncols <- NROW(series_test)
  }
  fullpreds <- matrix(NA, nrow = n_samples, ncol = ncols)
  for(i in 1:n_samples){
    fullpreds[i,] <- Xp %*% betas[i,] + trend[i,] +
      attr(Xp, 'model.offset')
  }

  # GAM only predictions
  gampreds <- matrix(NA, nrow = n_samples, ncol = ncols)
  for(i in 1:n_samples){
    gampreds[i,] <- Xp %*% betas[i,] +
      attr(Xp, 'model.offset')
  }

  # GAM uncertainty contributions at each forecast horizon
  gam_cont <- vector()
  for(i in 1:NCOL(fullpreds)){
    gam_cont[i] <- intersect_hist(fullpreds[,i], gampreds[,i])
  }
  gam_cont[is.na(gam_cont)] <- 0.5
  gam_cont[gam_cont > 1] <- 1

  # Plot and return
  if(hide_xlabels){
  plot(gam_cont, bty = "L",
       ylim=c(0,1),type='n',
       ylab=paste0('Uncertainty contributions for ', levels(data_train$series)[series]),
       xlab="", xaxt = 'n')
   } else {
      plot(gam_cont, bty = 'L',
           ylim=c(0,1),type='n',
           ylab=paste0('Uncertainty contributions for ', levels(data_train$series)[series]),
           xlab="Forecast horizon")
    }

  polygon(c(seq(1:(NCOL(gampreds))), rev(seq(1:NCOL(gampreds)))),
          c(gam_cont, rep(0, NCOL(gampreds))),
          col = "#7C0000", border = NA)
  polygon(c(seq(1:(NCOL(gampreds))), rev(seq(1:NCOL(gampreds)))),
          c(gam_cont, rep(1, NCOL(gampreds))),
          col = '#DCBCBC', border = NA)
  box(bty = 'L', lwd = 2)
  if(legend_position != 'none'){
    legend(legend_position,legend=c("Trend","GAM"),
           bg = 'white',
           col=c('#DCBCBC',
                 "#7C0000"),lty=1,lwd=6)
  }

}
