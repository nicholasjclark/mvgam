#'Residual diagnostics for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and returns various residual diagnostic plots
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@param series \code{integer} specifying which series in the set is to be plotted
#'@param n_bins \code{integer} specifying the number of bins to use for binning fitted values
#'@author Nicholas J Clark
#'@details A total of four base \code{R} plots are generated to examine Dunn-Smyth residuals for
#'the specified series. Plots include a residuals vs fitted values plot,
#'a Q-Q plot, and two plots to check for any remaining temporal autocorrelation in the residuals.
#'Note, all plots use posterior medians of fitted values / residuals, so uncertainty is not represented.
#'@return A series of base \code{R} plots
#'@export
plot_mvgam_resids = function(object, series = 1, n_bins = 25){

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

  if(sign(n_bins) != 1){
    stop('argument "n_bins" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_bins%%1 != 0){
      stop('argument "n_bins" must be a positive integer',
           call. = FALSE)
    }
  }

# Plotting colours
probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

# Prediction indices for the particular series
data_train <- object$obs_data
ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

# Pull out series' residuals
series_residuals <- object$resids[[series]]

# Get indices of training horizon
if(class(data_train)[1] == 'list'){
  data_train_df <- data.frame(time = data_train$time,
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
                         dplyr::select(time, y) %>%
                         dplyr::distinct() %>%
                         dplyr::arrange(time) %>%
                         dplyr::pull(y))
}

# Resids and predictions for only the training period
series_residuals <- series_residuals[, 1:obs_length]
preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]][, 1:obs_length]
median_preds <- apply(preds, 2, function(x) quantile(x, 0.5))

# Graphical parameters
layout(matrix(1:4, ncol = 2, nrow = 2, byrow = TRUE))

# Fitted vs redisuals plot
n_fitted_bins = n_bins

# Get x-axis values and bin if necessary to prevent overplotting
sorted_x <- sort(unique(round(median_preds, 6)))

if(length(sorted_x) > n_fitted_bins){
  sorted_x <- seq(min(sorted_x), max(sorted_x), length.out = n_fitted_bins)
  resid_probs <- do.call(rbind, lapply(2:n_fitted_bins, function(i){
    quantile(as.vector(series_residuals[,which(round(median_preds, 6) <= sorted_x[i] &
                                               round(median_preds, 6) > sorted_x[i-1])]),
             probs = probs)
  }))
  resid_probs <- rbind(quantile(as.vector(series_residuals[,which(round(median_preds, 6) == sorted_x[1])]),
                                probs = probs),
                       resid_probs)

} else {
  resid_probs <- do.call(rbind, lapply(sorted_x, function(i){
    quantile(as.vector(series_residuals[,which(round(median_preds, 6) == i)]),
             probs = probs)
  }))
}

# Get polygon coordinates and plot
N <- length(sorted_x)
idx <- rep(1:N, each = 2)
repped_x <- rep(sorted_x, each = 2)

x <- sapply(1:length(idx),
            function(k) if(k %% 2 == 0)
              repped_x[k] + min(diff(sorted_x))/2 else
                repped_x[k] - min(diff(sorted_x))/2)

# Plot
plot(median_preds[1:length(object$resids[[series]])],
     object$resids[[series]],
     main = 'Resids vs Fitted Values',
     xlab = 'Fitted values',
     ylab = 'Residuals',
     pch = 16,
     col = 'white',
     cex = 1,
     ylim = range(resid_probs, na.rm = T))

rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  resid_probs[,9],
     ybottom =  resid_probs[,1],
     col = c_light,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  resid_probs[,8],
     ybottom =  resid_probs[,2],
     col = c_light_highlight,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  resid_probs[,7],
     ybottom =  resid_probs[,3],
     col = c_mid,
     border = 'transparent')
rect(xleft = x[seq(1, N*2, by = 2)],
     xright = x[seq(2, N*2, by = 2)],
     ytop =  resid_probs[,6],
     ybottom =  resid_probs[,4],
     col = c_mid_highlight,
     border = 'transparent')

for (k in 1:N) {
  lines(x = c(x[seq(1, N*2, by = 2)][k],x[seq(2, N*2, by = 2)][k]),
        y = c(resid_probs[k,5], resid_probs[k,5]),
        col = c_dark, lwd = 2)
}
abline(h = 0, lty = 'dashed', col = 'white', lwd = 2.85)
abline(h = 0, lty = 'dashed', col = 'black', lwd = 2.5)

# Q-Q plot
coords <- qqnorm(series_residuals[1,], plot.it = F)
resid_coords_y <- matrix(NA, nrow = NROW(series_residuals), ncol = length(coords$y))
for(i in 1:NROW(series_residuals)){
  norm_coords <- qqnorm(series_residuals[i,], plot.it = F)
  coords_y <- norm_coords$y
  coords_y[abs(coords_y) > 3.75] <- NA
  resid_coords_y[i,] <- coords_y[order(norm_coords$x)]
}

cred <- sapply(1:NCOL(resid_coords_y),
               function(n) quantile(resid_coords_y[,n],
                                    probs = probs,
                                    na.rm = T))
pred_vals <- coords$x[order(coords$x)]
pred_vals <- pred_vals[complete.cases(cred[1,])]
plot(x = pred_vals,
     y = cred[5,][complete.cases(cred[1,])],
     main = 'Normal Q-Q Plot',
     xlab = 'Theoretical Quantiles',
     ylab = 'Sample Quantiles',
     pch = 16,
     col = 'white',
     cex = 1,
     ylim = range(cred, na.rm = T))
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
qqline(cred[5,][complete.cases(cred[1,])], col = 'white', lwd = 3)
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
plot(1, type = "n",
     xlab = 'Lag',
     ylab = 'ACF',
     main = 'ACF',
     xlim = c(1, N-1),
     xaxt = 'n',
     ylim = range(c(cred,
                    -clim - 0.05,
                    clim + 0.05)))
axis(1, at = seq(1, NCOL(cred), by = 2))

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
abline(h = clim, lty = 'dashed', col = 'white', lwd = 2.85)
abline(h = clim, lty = 'dashed', col = 'black', lwd = 2.5)
abline(h = -clim, lty = 'dashed', col = 'white', lwd = 2.85)
abline(h = -clim, lty = 'dashed', col = 'black', lwd = 2.5)

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
plot(1, type = "n",
     xlab = 'Lag',
     ylab = 'Partial ACF',
     main = 'pACF',
     xlim = c(1, length(sorted_x)),
     xaxt = 'n',
     ylim = range(c(cred,
                    -clim - 0.05,
                    clim + 0.05)))
axis(1, at = seq(1, NCOL(cred), by = 2))

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
abline(h = clim, lty = 'dashed', col = 'white', lwd = 2.85)
abline(h = clim, lty = 'dashed', col = 'black', lwd = 2.5)
abline(h = -clim, lty = 'dashed', col = 'white', lwd = 2.85)
abline(h = -clim, lty = 'dashed', col = 'black', lwd = 2.5)

layout(1)

}
