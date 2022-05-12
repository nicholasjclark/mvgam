#'Plot mvjagam latent trend for a specified series
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param data_test Optional \code{dataframe} of test data containing at least 'series', 'season' and 'year'
#'forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}
#'@param derivatives \code{logical}. If \code{TRUE}, an additional plot will be returned to show the
#'estimated 1st derivative for the estimated trend
#'@param hide_xlabels \code{logical}. If \code{TRUE}, no xlabels are printed to allow the user to add custom labels using
#'\code{axis} from base \code{R}. Ignored if \code{derivatives = TRUE}
#'@export
plot_mvgam_trend = function(object, series = 1, data_test,
                            derivatives = FALSE, hide_xlabels = FALSE){

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

  # Prediction indices for the particular series
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

  if(derivatives){
    .pardefault <- par(no.readonly=T)
    par(.pardefault)
    par(mfrow = c(2, 1),
        mgp = c(2.5, 1, 0),
        mai = c(0.8, 0.8, 0.4, 0.4))

    plot(1, type = "n",
           xlab = 'Time',
           ylab = paste0('Estimated trend for ', levels(data_train$series)[series]),
           xlim = c(0, length(preds_last)),
           ylim = range(cred))

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
      if(class(data_train)[1] == 'list'){
        abline(v = length(data_train$y) / NCOL(object$ytimes), lty = 'dashed')
      } else {
        abline(v = NROW(data_train) / NCOL(object$ytimes), lty = 'dashed')
      }

    }

    # Compute 1st derivative from posterior draws
    first_derivs <- cbind(rep(NA, NROW(preds)), t(apply(preds, 1, diff)))
    cred <- sapply(1:NCOL(first_derivs),
                   function(n) quantile(first_derivs[,n],
                                        probs = probs, na.rm = T))

    plot(1, type = "n",
         xlab = 'Time',
         ylab = '1st derivative',
         xlim = c(min(pred_vals), max(pred_vals)),
         ylim = c(min(cred, na.rm = T) - sd(first_derivs, na.rm = T),
                  max(cred, na.rm = T) + sd(first_derivs, na.rm = T)))
    polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
            col = c_light, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
            col = c_light_highlight, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
            col = c_mid, border = NA)
    polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
            col = c_mid_highlight, border = NA)
    lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
    abline(h = 0, lty = 'dashed', col = 'grey70', lwd = 2)

    invisible()
    par(.pardefault)

  } else {
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
      if(class(data_train)[1] == 'list'){
        abline(v = length(data_train$y) / NCOL(object$ytimes), lty = 'dashed')
      } else {
        abline(v = NROW(data_train) / NCOL(object$ytimes), lty = 'dashed')
      }

    }
  }




}
