# Showing how smoothing parameter influences fit
library('mgcv')
data(mcycle, package = 'MASS')

png('lambdas.png', width = 1220, height = 1020, res = 160)
m1 <- gam(accel ~ s(times, k = 50), data = mcycle, method = 'REML', sp = 5)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot(m1, scheme = 1, residuals = TRUE, pch= 16,
     ylab = expression(lambda == 5), xlab = '',
     shade.col = scales::alpha("#B97C7C", 0.6))

m2 <- gam(accel ~ s(times, k = 50), data = mcycle, method = 'REML', sp = 1)
plot(m2, scheme = 1, residuals = TRUE, pch= 16,
     ylab = expression(lambda == 1), xlab = '',
     shade.col = scales::alpha("#B97C7C", 0.6))

m3 <- gam(accel ~ s(times, k = 50), data = mcycle, method = 'REML', sp = 0.01)
plot(m3, scheme = 1, residuals = TRUE, pch= 16,
     ylab = expression(lambda == 0.01), xlab = '',
     shade.col = scales::alpha("#B97C7C", 0.6))

m4 <- gam(accel ~ s(times, k = 50), data = mcycle, method = 'REML', sp = 0.0000001)
plot(m4, scheme = 1, residuals = TRUE, pch= 16,
     ylab = expression(lambda == 0.0000001), xlab = '',
     shade.col = scales::alpha("#B97C7C", 0.6))
dev.off()

png('air_example.png', width = 1220, height = 1020, res = 160)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot_mvgam_fc(mod1, series = 1, data_test = fake_data$data_test,
              ylab = '', hide_xlabels = T,
              ylim = c(100, 1300))
mtext('Extrapolated linear trend')
text(x = 111,
     y = 920, labels = 'Forecast horizon', srt = -90)
plot_mvgam_fc(mod2, series = 1, data_test = fake_data$data_test, ylab = '',
              hide_xlabels = T,
              ylim = c(100, 1300))
mtext('Extrapolated B spline')
plot_mvgam_fc(mod4, series = 1, data_test = fake_data$data_test, ylab = '',
              hide_xlabels = T,
              ylim = c(100, 1300))
mtext('Random walk')
plot_mvgam_fc(mod3, series = 1, data_test = fake_data$data_test, ylab = '',
              hide_xlabels = T,
              ylim = c(100, 1300))
mtext('Dynamic trend')
dev.off()

# A rather long-winded script for producing animations of partially pooled smooths
library(mvgam)
library(dplyr)
portal_dat <- read.csv('https://raw.githubusercontent.com/nicholasjclark/mvgam/master/NEON_manuscript/Case studies/rodents_data.csv', as.is = T)
portal_dat %>%
  dplyr::filter(year >= 2004) %>%
  dplyr::group_by(year, month) %>%
  dplyr::slice_head(n = 1) -> portal_dat_all
lagard <- function(x, n.lag = 6) {
  n <- length(x); X <- matrix(NA, n, n.lag)
  for (i in 1:n.lag) X[i:n, i] <- x[i:n - i + 1]
  X
}
data_all <- list(lag=matrix(0:5,nrow(portal_dat_all),6,byrow=TRUE),
                 y = portal_dat_all$PP,
                 season = portal_dat_all$month,
                 year = portal_dat_all$year,
                 series = rep(as.factor('series1'), nrow(portal_dat_all)))
data_all$precip <- lagard(portal_dat_all$precipitation)
data_all$mintemp <- lagard(portal_dat_all$mintemp)
data_train <- list(lag = data_all$lag[7:174,],
                   y = data_all$y[7:174],
                   series = data_all$series[7:174],
                   season = data_all$season[7:174],
                   year = data_all$year[7:174],
                   precip = data_all$precip[7:174,],
                   mintemp = data_all$mintemp[7:174,])
data_test <- list(lag = data_all$lag[175:length(data_all$y),],
                  y = data_all$y[175:length(data_all$y)],
                  series = data_all$series[175:length(data_all$y)],
                  season = data_all$season[175:length(data_all$y)],
                  year = data_all$year[175:length(data_all$y)],
                  precip = data_all$precip[175:length(data_all$y),],
                  mintemp = data_all$mintemp[175:length(data_all$y),])
test <- mvjagam(formula =  y ~ te(mintemp, lag, k = c(8, 4)) +
                  te(precip, lag, k = c(8, 4)),
                data_train = data_train,
                data_test = data_test,
                family = 'poisson',
                chains = 4,
                burnin = 12000,
                trend_model = 'AR1')
newdata <- data_test
newdata$year <- rep(0, length(newdata$year))
newdata$season <- rep(0, length(newdata$season))
newdata$precip <- matrix(0, ncol = ncol(newdata$precip),
                         nrow = nrow(newdata$precip))
library(animation)
cols <- rev(viridis::plasma(10)[3:8])

# Calculate predictions for when mintemp is all zeros to find the baseline
# value for centring the plot
newdata$mintemp <- matrix(0, ncol = ncol(newdata$mintemp),
                          nrow = nrow(newdata$mintemp))
preds <- predict_mvgam(test, series = 1, newdata = newdata, type = 'link')
offset <- mean(preds)

# Set up prediction objects
ran_points <- matrix(NA, ncol = 6, nrow = length(newdata$year))
loess_preds <- matrix(NA, ncol = 6, nrow = length(newdata$year))
pred_upper <- matrix(NA, ncol = 6, nrow = length(newdata$year))
pred_lower <- matrix(NA, ncol = 6, nrow = length(newdata$year))
pred_med <- matrix(NA, ncol = 6, nrow = length(newdata$year))
pred_vals <- seq(min(data_train$mintemp),
                 max(data_train$mintemp),
                 length.out = length(newdata$year))
for(i in 1:6){
  newdata$mintemp <- matrix(0, ncol = ncol(newdata$precip),
                            nrow = nrow(newdata$precip))
  newdata$mintemp[,i] <- seq(min(data_train$mintemp),
                             max(data_train$mintemp),
                             length.out = length(newdata$year))

  # Predict on the link scale and shift by the offset so that values are roughly centred at zero
  preds <- predict_mvgam(test, series = 1, newdata = newdata, type = 'link') - offset

  # Calculate empirical prediction quantiles
  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs))

  # Plot expected function posterior intervals (40-60%) and medians in varying colours per lag
  pred_upper[,i] <- cred[3,]
  pred_lower[,i] <- cred[7,]
  pred_med[,i] <- cred[5,]

  # Generate some noisy points and fit a loess smooth
  ran_points[,i] <- rnorm(length(cred[5,]), mean = cred[5,],
                          sd = runif(length(cred[5,]), 0.25, 0.4))
  loess_preds[,i] <- predict(loess(ran_points[,i] ~ pred_vals,
                                   span = 0.5))
}

n_cuts <- 15
starts = floor(seq.int(1, length(pred_vals), length.out = n_cuts))
starts = starts[-length(starts)]
ends = c(starts - 1, length(pred_vals))
ends = ends[-1]

saveGIF({
  plot_seq <- seq(1, 6)
  for(i in plot_seq){

    for(x in 1:5){
      plot(1, type = "n",
           xlab = 'Covariate',
           ylab = 'Independent response functions',
           xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
           ylim = c(-1.6, 1.6))

      if(i > 1){
        for(k in 1:(i - 1)){
          lines(pred_vals, loess_preds[,k],
                col = scales::alpha(cols[k], 0.2),
                lwd = 3)
        }
      }

      points(x = pred_vals,
             y = ran_points[,i],
             col = scales::alpha(cols[i], 0.8),
             pch = 16, cex = 0.9)
      abline(h = 0, lty = 'dashed')
      mtext('Functions wiggle indepentently with no pooling')
    }

    for(j in 1:(n_cuts-1)){
    plot(1, type = "n",
         xlab = 'Covariate',
         ylab = 'Independent response functions',
         xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
         ylim = c(-1.6, 1.6))

      if(i > 1){
        for(k in 1:(i - 1)){
          lines(pred_vals, loess_preds[,k],
                col = scales::alpha(cols[k], 0.2),
                lwd = 3)
        }
      }

    points(x = pred_vals,
           y = ran_points[,i],
           col = scales::alpha(cols[i], 0.8),
           pch = 16, cex = 0.9)
    lines(pred_vals[1:ends[j]], loess_preds[,i][1:ends[j]],
          col = scales::alpha(cols[i], 0.8),
          lwd = 3)
    abline(h = 0, lty = 'dashed')
    mtext('Functions wiggle indepentently with no pooling')
    }

    for(x in 1:5){
      plot(1, type = "n",
           xlab = 'Covariate',
           ylab = 'Independent response functions',
           xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
           ylim = c(-1.6, 1.6))

      if(i > 1){
        for(k in 1:(i - 1)){
          lines(pred_vals, loess_preds[,k],
                col = scales::alpha(cols[k], 0.2),
                lwd = 3)
        }
      }

      points(x = pred_vals,
             y = ran_points[,i],
             col = scales::alpha(cols[i], 0.8),
             pch = 16, cex = 0.9)
      lines(pred_vals, loess_preds[,i],
            col = scales::alpha(cols[i], 0.8),
            lwd = 3)
      abline(h = 0, lty = 'dashed')
      mtext('Functions wiggle indepentently with no pooling')
    }

  }

  for(x in 1:20){
    plot(1, type = "n",
         xlab = 'Covariate',
         ylab = 'Independent response functions',
         xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
         ylim = c(-1.6, 1.6))

    for(k in 1:6){
      lines(pred_vals, loess_preds[,k],
            col = scales::alpha(cols[k], 0.2),
            lwd = 3)
    }
    abline(h = 0, lty = 'dashed')
    mtext('Functions wiggle indepentently with no pooling')
  }
}, movie.name = 'hierarchical1.gif', interval = 0.1,
   ani.width = 720, ani.height = 680, ani.res = 150)



saveGIF({
  plot_seq <- seq(1, 6)
  for(i in plot_seq){

    for(x in 1:5){
      plot(1, type = "n",
           xlab = 'Covariate',
           ylab = 'Partially pooled functions (60% CI)',
           xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
           ylim = c(-1.6, 1.6))

      if(i > 1){
        for(k in 1:(i - 1)){
          polygon(c(pred_vals, rev(pred_vals)), c(pred_upper[,k], rev(pred_lower[,k])),
                  col = scales::alpha(cols[k], 0.2),
                  border = scales::alpha(cols[k], 0.2))
          lines(pred_vals, pred_med[,k],
                col = scales::alpha(cols[k], 0.3),
                lwd = 3)
        }
      }

      points(x = pred_vals,
             y = ran_points[,i],
             col = scales::alpha(cols[i], 0.8),
             pch = 16, cex = 0.9)
      abline(h = 0, lty = 'dashed')
      mtext('Function shape and wiggliness partially pooled')
    }

    for(j in 1:(n_cuts-1)){
      plot(1, type = "n",
           xlab = 'Covariate',
           ylab = 'Partially pooled functions (60% CI)',
           xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
           ylim = c(-1.6, 1.6))

      if(i > 1){
        for(k in 1:(i - 1)){
          polygon(c(pred_vals, rev(pred_vals)), c(pred_upper[,k], rev(pred_lower[,k])),
                  col = scales::alpha(cols[k], 0.2),
                  border = scales::alpha(cols[k], 0.2))
          lines(pred_vals, pred_med[,k],
                col = scales::alpha(cols[k], 0.3),
                lwd = 3)
        }
      }

      polygon(c(pred_vals[1:ends[j]], rev(pred_vals[1:ends[j]])),
              c(pred_upper[,i][1:ends[j]], rev(pred_lower[,i][1:ends[j]])),
              col = scales::alpha(cols[i], 0.5),
              border = scales::alpha(cols[i], 0.5))
      lines(pred_vals[1:ends[j]], pred_med[,i][1:ends[j]],
            col = scales::alpha(cols[i], 0.6),
            lwd = 3)
      points(x = pred_vals,
             y = ran_points[,i],
             col = scales::alpha(cols[i], 0.8),
             pch = 16, cex = 0.9)
      abline(h = 0, lty = 'dashed')
      mtext('Function shape and wiggliness partially pooled')
    }

    for(x in 1:5){
      plot(1, type = "n",
           xlab = 'Covariate',
           ylab = 'Partially pooled functions (60% CI)',
           xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
           ylim = c(-1.6, 1.6))

      if(i > 1){
        for(k in 1:(i - 1)){
          polygon(c(pred_vals, rev(pred_vals)), c(pred_upper[,k], rev(pred_lower[,k])),
                  col = scales::alpha(cols[k], 0.2),
                  border = scales::alpha(cols[k], 0.2))
          lines(pred_vals, pred_med[,k],
                col = scales::alpha(cols[k], 0.3),
                lwd = 3)
        }
      }

      polygon(c(pred_vals, rev(pred_vals)),
              c(pred_upper[,i], rev(pred_lower[,i])),
              col = scales::alpha(cols[i], 0.5),
              border = scales::alpha(cols[i], 0.5))
      lines(pred_vals, pred_med[,i],
            col = scales::alpha(cols[i], 0.6),
            lwd = 3)
      points(x = pred_vals,
             y = ran_points[,i],
             col = scales::alpha(cols[i], 0.8),
             pch = 16, cex = 0.9)
      abline(h = 0, lty = 'dashed')
      mtext('Function shape and wiggliness partially pooled')
    }

  }

  for(x in 1:20){
    plot(1, type = "n",
         xlab = 'Covariate',
         ylab = 'Partially pooled functions (60% CI)',
         xlim = c(min(data_train$mintemp), max(data_train$mintemp)),
         ylim = c(-1.6, 1.6))

    for(k in 1:6){
      polygon(c(pred_vals, rev(pred_vals)), c(pred_upper[,k], rev(pred_lower[,k])),
              col = scales::alpha(cols[k], 0.2),
              border = scales::alpha(cols[k], 0.2))
      lines(pred_vals, pred_med[,k],
            col = scales::alpha(cols[k], 0.3),
            lwd = 3)
    }
    abline(h = 0, lty = 'dashed')
    mtext('Function shape and wiggliness partially pooled')
  }
}, movie.name = 'hierarchical2.gif', interval = 0.1,
ani.width = 720, ani.height = 680, ani.res = 150)




