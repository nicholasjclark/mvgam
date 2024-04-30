#'Residual diagnostics for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and returns various residual diagnostic plots
#'
#'@importFrom graphics layout title
#'@importFrom stats complete.cases qqnorm qqline acf pacf na.pass
#'@importFrom mgcv bam
#'@param object \code{list} object returned from \code{mvgam}. See [mvgam()]
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series', 'y', and 'time'
#'in addition to any other variables included in the linear predictor of \code{formula}. If included, the
#'covariate information in \code{newdata} will be used to generate forecasts from the fitted model equations. If
#'this same \code{newdata} was originally included in the call to \code{mvgam}, then forecasts have already been
#'produced by the generative model and these will simply be extracted and used to calculate residuals.
#'However if no \code{newdata} was supplied to the original model call, an assumption is made that
#'the \code{newdata} supplied here comes sequentially after the data supplied as \code{data} in
#'the original model (i.e. we assume there is no time gap between the last
#'observation of series 1 in \code{data_train} and the first observation for series 1 in \code{newdata}).
#'@param data_test Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@author Nicholas J Clark
#'@details A total of four base \code{R} plots are generated to examine Dunn-Smyth residuals for
#'the specified series. Plots include a residuals vs fitted values plot,
#'a Q-Q plot, and two plots to check for any remaining temporal autocorrelation in the residuals.
#'Note, all plots use posterior medians of fitted values / residuals, so uncertainty is not represented.
#'@return A series of base \code{R} plots
#'@export
plot_mvgam_resids = function(object, series = 1,
                             newdata, data_test){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  validate_pos_integer(series)

  if(series > NCOL(object$ytimes)){
    stop(paste0('object only contains data / predictions for ',
                NCOL(object$ytimes), ' series'),
         call. = FALSE)
  }

  if(!missing("newdata")){
    data_test <- validate_series_time(newdata,
                                      trend_model = attr(object$model_data,
                                                         'trend_model'))
  }

# Plotting colours
probs <- c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

# Prediction indices for the particular series
data_train <- validate_series_time(object$obs_data,
                                   trend_model = attr(object$model_data,
                                                      'trend_model'))
ends <- seq(0, dim(mcmc_chains(object$model_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

# Pull out series' residuals
series_residuals <- object$resids[[series]]

# Get indices of training horizon
if(class(data_train)[1] == 'list'){
  data_train_df <- data.frame(time = data_train$index..time..index,
                              y = data_train$y,
                              series = data_train$series)
  obs_length <- length(data_train_df %>%
                         dplyr::filter(series == !!(levels(data_train_df$series)[series])) %>%
                         dplyr::select(time, y) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::pull(y))
} else {
  obs_length <- length(data_train %>%
                         dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
                         dplyr::select(index..time..index, y) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(index..time..index) %>%
                         dplyr::pull(y))
}

if(missing(data_test)){

} else {

  if(object$fit_engine == 'stan'){
    linkfun <- family_invlinks(object$family)
    preds <- linkfun(mcmc_chains(object$model_output, 'mus')[,seq(series,
                                                                    dim(mcmc_chains(object$model_output, 'mus'))[2],
                                                                    by = NCOL(object$ytimes))])
  } else {
    preds <- mcmc_chains(object$model_output, 'mus')[,starts[series]:ends[series]]
  }

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
  if(!missing(data_test)){

    if(!'y' %in% names(data_test)){
      data_test$y <- rep(NA, NROW(data_test))
    }

    if(class(data_test)[1] == 'list'){
      if(!'time' %in% names(data_test)){
        stop('data_train does not contain a "time" column')
      }

      if(!'series' %in% names(data_test)){
        data_test$series <- factor('series1')
      }

    } else {
      if(!'time' %in% colnames(data_test)){
        stop('data_train does not contain a "time" column')
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
                              time = data_train$index..time..index) %>%
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
                     dplyr::select(index..time..index, y) %>%
                     dplyr::distinct() %>%
                     dplyr::arrange(index..time..index) %>%
                     dplyr::pull(y),
                   data_test %>%
                     dplyr::filter(series == s_name) %>%
                     dplyr::select(time, y) %>%
                     dplyr::distinct() %>%
                     dplyr::arrange(time) %>%
                     dplyr::pull(y))
    }

    if(dim(preds)[2] != length(all_obs)){
      linkfun <- family_invlinks(object$family)
      fc_preds <- linkfun(forecast.mvgam(object, series = series,
                                             data_test = data_test,
                                 type = 'link'))
      preds <- cbind(preds, fc_preds)
    }

    # Calculate out of sample residuals
    preds <- preds[,tail(1:dim(preds)[2], length(data_test$time))]
    truth <- data_test$y
    n_obs <- length(truth)

   if(NROW(preds) > 2000){
     sample_seq <- sample(1:NROW(preds), 2000, F)
   } else {
     sample_seq <- 1:NROW(preds)
   }

    series_residuals <- get_forecast_resids(object = object,
                                            series = series,
                                            truth = truth,
                                            preds = preds,
                                            family = object$family,
                                            sample_seq = sample_seq)

  }
}

# Graphical parameters
.pardefault <- par(no.readonly=T)
on.exit(par(.pardefault))
layout(matrix(1:4, ncol = 2, nrow = 2, byrow = TRUE))
oldpar <- par(mar=c(2.5, 2.3, 2, 2),
              oma = c(1, 1, 0, 0),
              mgp = c(2, 0.5, 0))

# Extract expectation (fitted) values
preds <- hindcast(object, type = 'expected')$hindcasts[[series]]
series_residuals <- object$resids[[series]]
limits <- quantile(preds, probs = c(0.025, 0.975), na.rm = TRUE)

# Plot resids vs fitted
plot(1,
     xlim = limits,
     bty = 'L',
     xlab = '',
     ylab = '',
     pch = 16,
     col = 'white',
     cex = 1,
     ylim = range(series_residuals, na.rm = T))

draws <- sample(1:dim(preds)[1],
                min(150, dim(preds)[1]),
                replace = FALSE)
moddata <- vector(mode = 'list')
for(i in draws){
  points(x = preds[i,],
         y = series_residuals[i,],
         pch = 16,
         cex = 0.7,
         col = '#80808020')
  moddata[[which(draws == i)]] <- data.frame(y = series_residuals[i,],
                             x = preds[i,])
}
title('Resids vs Fitted', line = 0)
title(ylab = "DS residuals", line = 1.5)
title(xlab = "Fitted values", line = 1.5)

# Plot a fitted line
moddata <- do.call(rbind, moddata)

predvals <- seq(min(preds[draws, ], na.rm = TRUE),
                max(preds[draws, ], na.rm = TRUE),
                length.out = 200)
medmod <- bam(y ~ s(x), data = moddata, discrete = TRUE)
medpreds <- predict(medmod, newdata = data.frame(x = predvals),
                      type = 'response', se.fit = TRUE)

polygon(c(predvals, rev(predvals)), c(medpreds$fit + 3*medpreds$se.fit,
                                        rev(medpreds$fit - 3*medpreds$se.fit)),
        col = "#7C000040",
        border = NA)
lines(x = predvals,
      y = medpreds$fit,
      col = "#7C000060",
      lwd = 3)

# Q-Q plot
coords <- qqnorm(series_residuals[1,], plot.it = F)
resid_coords_y <- matrix(NA, nrow = NROW(series_residuals), ncol = length(coords$y))
for(i in 1:NROW(series_residuals)){
  if(all(is.na(series_residuals[i,]))){
    resid_coords_y[i,] <- rep(NA, length(coords$y))
  } else {
    norm_coords <- qqnorm(series_residuals[i,], plot.it = FALSE)
    coords_y <- norm_coords$y
    coords_y[abs(coords_y) > 3.75] <- NA
    resid_coords_y[i,] <- coords_y[order(norm_coords$x)]
  }
}

cred <- sapply(1:NCOL(resid_coords_y),
               function(n) quantile(resid_coords_y[,n],
                                    probs = probs,
                                    na.rm = TRUE))
pred_vals <- coords$x[order(coords$x)]
pred_vals <- pred_vals[complete.cases(cred[1,])]
plot(x = pred_vals,
     y = cred[5,][complete.cases(cred[1,])],
     bty = 'L',
     xlab = '',
     ylab = '',
     pch = 16,
     col = 'white',
     cex = 1,
     ylim = range(cred, na.rm = T),
     tck = -0.04)
title('Normal Q-Q Plot', line = 0)
title(ylab = "Sample Quantiles", line = 1.5)
title(xlab = "Theoretical Quantiles", line = 1.5)
polygon(c(pred_vals, rev(pred_vals)), c(cred[1,][complete.cases(cred[1,])],
                                        rev(cred[9,][complete.cases(cred[1,])])),
        col = c_light, border = NA)
polygon(c(pred_vals, rev(pred_vals)), c(cred[2,][complete.cases(cred[1,])],
                                        rev(cred[8,][complete.cases(cred[1,])])),
        col = c_light_highlight, border = NA)
polygon(c(pred_vals, rev(pred_vals)), c(cred[3,][complete.cases(cred[1,])],
                                        rev(cred[7,][complete.cases(cred[1,])])),
        col = c_mid, border = NA)
polygon(c(pred_vals, rev(pred_vals)), c(cred[4,][complete.cases(cred[1,])],
                                        rev(cred[6,][complete.cases(cred[1,])])),
        col = c_mid_highlight, border = NA)
lines(pred_vals, cred[5,][complete.cases(cred[1,])], col = c_dark, lwd = 2.5)
qqline(cred[5,][complete.cases(cred[1,])], col = '#FFFFFF60', lwd = 3)
qqline(cred[5,][complete.cases(cred[1,])], col = 'black', lwd = 2.5)

# ACF plot
acf1 <- acf(series_residuals[1,], plot = F,
            na.action = na.pass)
resid_acf <- matrix(NA, nrow = NROW(series_residuals),
                    ncol = length(acf1$acf[,,1]))
for(i in 1:NROW(series_residuals)){
  resid_acf[i, ] <- acf(series_residuals[i,], plot = F,
                        na.action = na.pass)$acf[,,1]
}

sorted_x <- seq(1:NCOL(resid_acf))
N <- length(sorted_x)
idx <- rep(1:N, each = 2)
repped_x <- rep(sorted_x, each = 2)

x <- sapply(1:length(idx),
            function(k) if(k %% 2 == 0)
              repped_x[k] + min(diff(sorted_x))/2 else
                repped_x[k] - min(diff(sorted_x))/2)
cred <- sapply(1:NCOL(resid_acf),
               function(n) quantile(resid_acf[,n],
                                    probs = probs, na.rm = T))
cred <- cred[, -1]
clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
plot(1, type = "n", bty = 'L',
     xlab = '',
     ylab = '',
     xlim = c(1, N-1),
     xaxt = 'n',
     ylim = range(c(cred,
                    -clim - 0.05,
                    clim + 0.05), na.rm = TRUE))
axis(1, at = seq(1, NCOL(cred), by = 2))
title('ACF', line = 0)
title(ylab = "Autocorrelation", line = 1.5)
title(xlab = "Lag", line = 1.5)

N <- N - 1
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[9,],
     ybottom =  cred[1,],
     col = c_light,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[8,],
     ybottom =  cred[2,],
     col = c_light_highlight,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[7,],
     ybottom =  cred[3,],
     col = c_mid,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[6,],
     ybottom =  cred[4,],
     col = c_mid_highlight,
     border = 'transparent')

for (k in 1:N) {
  lines(x = c(x[seq(1, N*2, by = 2)][k],x[seq(2, N*2, by = 2)][k]),
        y = c(cred[5,k], cred[5,k]),
        col = c_dark, lwd = 2)
}
abline(h = clim,  col = '#FFFFFF60', lwd = 2.85)
abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
abline(h = -clim,  col = '#FFFFFF60', lwd = 2.85)
abline(h = -clim, col = 'black', lwd = 2.5, lty = 'dashed')

# PACF plot
pacf1 <- pacf(series_residuals[1,], plot = F,
            na.action = na.pass)
resid_pacf <- matrix(NA, nrow = NROW(series_residuals),
                    ncol = length(pacf1$acf[,,1]))
for(i in 1:NROW(series_residuals)){
  resid_pacf[i, ] <- pacf(series_residuals[i,], plot = F,
                        na.action = na.pass)$acf[,,1]
}

sorted_x <- seq(1:NCOL(resid_pacf))
N <- length(sorted_x)
idx <- rep(1:N, each = 2)
repped_x <- rep(sorted_x, each = 2)

x <- sapply(1:length(idx),
            function(k) if(k %% 2 == 0)
              repped_x[k] + min(diff(sorted_x))/2 else
                repped_x[k] - min(diff(sorted_x))/2)
cred <- sapply(1:NCOL(resid_pacf),
               function(n) quantile(resid_pacf[,n],
                                    probs = probs, na.rm = T))

clim <- qnorm((1 + .95)/2)/sqrt(pacf1$n.used)
plot(1, type = "n", bty = 'L',
     xlab = '',
     ylab = '',
     xlim = c(1, length(sorted_x)),
     xaxt = 'n',
     ylim = range(c(cred,
                    -clim - 0.05,
                    clim + 0.05), na.rm = TRUE))
axis(1, at = seq(1, NCOL(cred), by = 2))
title('pACF', line = 0)
title(ylab = "Autocorrelation", line = 1.5)
title(xlab = "Lag", line = 1.5)

rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[9,],
     ybottom =  cred[1,],
     col = c_light,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[8,],
     ybottom =  cred[2,],
     col = c_light_highlight,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[7,],
     ybottom =  cred[3,],
     col = c_mid,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  cred[6,],
     ybottom =  cred[4,],
     col = c_mid_highlight,
     border = 'transparent')

for (k in 1:N) {
  lines(x = c(x[seq(1, N*2, by = 2)][k],x[seq(2, N*2, by = 2)][k]),
        y = c(cred[5,k], cred[5,k]),
        col = c_dark, lwd = 2)
}
abline(h = clim, col = '#FFFFFF60', lwd = 2.85)
abline(h = clim, col = 'black', lwd = 2.5, lty = 'dashed')
abline(h = -clim, col = '#FFFFFF60', lwd = 2.85)
abline(h = -clim, col = 'black', lwd = 2.5, lty = 'dashed')

layout(1)

}
