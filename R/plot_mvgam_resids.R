#'Residual diagnostics for a fitted mvjagam object
#'
#'This function takes a fitted \code{mvjagam} object and returns various residual diagnostic plots
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@author Nicholas J Clark
#'@details A total of four base \code{R} plots are generated to examine Dunn-Smyth residuals for
#'the specified series. Plots include a residuals vs fitted values plot,
#'a Q-Q plot, and two plots to check for any remaining temporal autocorrelation in the residuals.
#'Note, all plots use posterior medians of fitted values / residuals, so uncertainty is not represented.
#'@return A series of base \code{R} plots
#'@export
plot_mvgam_resids = function(object, series = 1){

data_train <- object$obs_data
ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
median_preds <- apply(preds, 2, function(x) hpd(x)[2])
.pardefault <- par(no.readonly=T)
par(.pardefault)
par(mfrow = c(2, 2))

# Fitted vs redisuals plot
plot(median_preds[1:length(object$resids[[series]])],
     object$resids[[series]],
     main = 'Resids vs Fitted Values',
     xlab = 'Fitted values',
     ylab = 'Residuals',
     pch = 16,
     col = 'white',
     cex = 1)
points(x = median_preds[1:length(object$resids[[series]])],
       y = object$resids[[series]],
       pch = 16,
       cex = 0.75,
       col = 'black')

# Q-Q plot
coords <- qqnorm(object$resids[[series]], plot.it = F)
qqnorm(object$resids[[series]], pch = 16, col = 'white', cex = 1)
qqline(object$resids[[series]], col = "#8F2727", lwd = 2)
points(x = coords$x, y = coords$y, pch = 16, col = 'black', cex = 0.75)

# ACF plot
acf(object$resids[[series]], main = 'ACF', na.action = na.pass,
    ci.col = "#8F2727", lwd = 2)

# PACF plot (only works if there are no missing values in the residual series)
if(!any(is.na(object$resids[[series]]))){
  pacf(object$resids[[series]], main = 'pACF', na.action = na.fail,
       ci.col = "#8F2727", lwd = 2)
}

invisible()
par(.pardefault)

}
