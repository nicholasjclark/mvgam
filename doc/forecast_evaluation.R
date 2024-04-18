## ----echo = FALSE-------------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = NOT_CRAN
)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,   
  dpi = 150,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center")
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = 'serif'))

## -----------------------------------------------------------------------------
set.seed(2345)
simdat <- sim_mvgam(T = 100, 
                    n_series = 3, 
                    trend_model = 'GP',
                    prop_trend = 0.75,
                    family = poisson(),
                    prop_missing = 0.10)

## -----------------------------------------------------------------------------
str(simdat)

## ----fig.alt = "Simulating data for dynamic GAM models in mvgam"--------------
plot(simdat$global_seasonality[1:12], 
     type = 'l', lwd = 2,
     ylab = 'Relative effect',
     xlab = 'Season',
     bty = 'l')

## ----fig.alt = "Plotting time series features for GAM models in mvgam"--------
plot_mvgam_series(data = simdat$data_train, 
                  series = 'all')

## ----fig.alt = "Plotting time series features for GAM models in mvgam"--------
plot_mvgam_series(data = simdat$data_train, 
                  newdata = simdat$data_test,
                  series = 1)
plot_mvgam_series(data = simdat$data_train, 
                  newdata = simdat$data_test,
                  series = 2)
plot_mvgam_series(data = simdat$data_train, 
                  newdata = simdat$data_test,
                  series = 3)

## ----include=FALSE------------------------------------------------------------
mod1 <- mvgam(y ~ s(season, bs = 'cc', k = 8) + 
                s(time, by = series, bs = 'cr', k = 20),
              knots = list(season = c(0.5, 12.5)),
              trend_model = 'None',
              data = simdat$data_train)

## ----eval=FALSE---------------------------------------------------------------
#  mod1 <- mvgam(y ~ s(season, bs = 'cc', k = 8) +
#                  s(time, by = series, bs = 'cr', k = 20),
#                knots = list(season = c(0.5, 12.5)),
#                trend_model = 'None',
#                data = simdat$data_train)

## -----------------------------------------------------------------------------
summary(mod1, include_betas = FALSE)

## ----fig.alt = "Plotting GAM smooth functions using mvgam"--------------------
plot(mod1, type = 'smooths')

## ----include=FALSE------------------------------------------------------------
mod2 <- mvgam(y ~ s(season, bs = 'cc', k = 8) + 
                gp(time, by = series, c = 5/4, k = 20,
                   scale = FALSE),
              knots = list(season = c(0.5, 12.5)),
              trend_model = 'None',
              data = simdat$data_train,
              adapt_delta = 0.98)

## ----eval=FALSE---------------------------------------------------------------
#  mod2 <- mvgam(y ~ s(season, bs = 'cc', k = 8) +
#                  gp(time, by = series, c = 5/4, k = 20,
#                     scale = FALSE),
#                knots = list(season = c(0.5, 12.5)),
#                trend_model = 'None',
#                data = simdat$data_train)

## -----------------------------------------------------------------------------
summary(mod2, include_betas = FALSE)

## ----fig.alt = "Summarising latent Gaussian Process parameters in mvgam"------
mcmc_plot(mod2, variable = c('alpha_gp'), regex = TRUE, type = 'areas')

## ----fig.alt = "Summarising latent Gaussian Process parameters in mvgam"------
mcmc_plot(mod2, variable = c('rho_gp'), regex = TRUE, type = 'areas')

## ----fig.alt = "Plotting Gaussian Process effects in mvgam"-------------------
plot(mod2, type = 'smooths')

## ----fig.alt = "Summarising latent Gaussian Process parameters in mvgam and marginaleffects"----
require('ggplot2')
plot_predictions(mod2, 
                 condition = c('time', 'series', 'series'),
                 type = 'link') +
  theme(legend.position = 'none')

## -----------------------------------------------------------------------------
fc_mod1 <- forecast(mod1, newdata = simdat$data_test)
fc_mod2 <- forecast(mod2, newdata = simdat$data_test)

## -----------------------------------------------------------------------------
str(fc_mod1)

## -----------------------------------------------------------------------------
plot(fc_mod1, series = 1)
plot(fc_mod2, series = 1)

plot(fc_mod1, series = 2)
plot(fc_mod2, series = 2)

plot(fc_mod1, series = 3)
plot(fc_mod2, series = 3)

## ----include=FALSE------------------------------------------------------------
mod2 <- mvgam(y ~ s(season, bs = 'cc', k = 8) + 
                gp(time, by = series, c = 5/4, k = 20,
                   scale = FALSE),
              knots = list(season = c(0.5, 12.5)),
              trend_model = 'None',
              data = simdat$data_train,
              newdata = simdat$data_test,
              adapt_delta = 0.98)

## ----eval=FALSE---------------------------------------------------------------
#  mod2 <- mvgam(y ~ s(season, bs = 'cc', k = 8) +
#                  gp(time, by = series, c = 5/4, k = 20,
#                     scale = FALSE),
#                knots = list(season = c(0.5, 12.5)),
#                trend_model = 'None',
#                data = simdat$data_train,
#                newdata = simdat$data_test)

## -----------------------------------------------------------------------------
fc_mod2 <- forecast(mod2)

## ----warning=FALSE, fig.alt = "Plotting posterior forecast distributions using mvgam and R"----
plot(fc_mod2, series = 1)

## ----warning=FALSE------------------------------------------------------------
crps_mod1 <- score(fc_mod1, score = 'crps')
str(crps_mod1)
crps_mod1$series_1

## ----warning=FALSE------------------------------------------------------------
crps_mod1 <- score(fc_mod1, score = 'crps', interval_width = 0.6)
crps_mod1$series_1

## -----------------------------------------------------------------------------
link_mod1 <- forecast(mod1, newdata = simdat$data_test, type = 'link')
score(link_mod1, score = 'elpd')$series_1

## -----------------------------------------------------------------------------
energy_mod2 <- score(fc_mod2, score = 'energy')
str(energy_mod2)

## -----------------------------------------------------------------------------
energy_mod2$all_series

## -----------------------------------------------------------------------------
crps_mod1 <- score(fc_mod1, score = 'crps')
crps_mod2 <- score(fc_mod2, score = 'crps')

diff_scores <- crps_mod2$series_1$score -
  crps_mod1$series_1$score
plot(diff_scores, pch = 16, cex = 1.25, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(CRPS[GP]~-~CRPS[spline]))
abline(h = 0, lty = 'dashed', lwd = 2)
gp_better <- length(which(diff_scores < 0))
title(main = paste0('GP better in ', gp_better, ' of 25 evaluations',
                    '\nMean difference = ', 
                    round(mean(diff_scores, na.rm = TRUE), 2)))


diff_scores <- crps_mod2$series_2$score -
  crps_mod1$series_2$score
plot(diff_scores, pch = 16, cex = 1.25, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(CRPS[GP]~-~CRPS[spline]))
abline(h = 0, lty = 'dashed', lwd = 2)
gp_better <- length(which(diff_scores < 0))
title(main = paste0('GP better in ', gp_better, ' of 25 evaluations',
                    '\nMean difference = ', 
                    round(mean(diff_scores, na.rm = TRUE), 2)))

diff_scores <- crps_mod2$series_3$score -
  crps_mod1$series_3$score
plot(diff_scores, pch = 16, cex = 1.25, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(CRPS[GP]~-~CRPS[spline]))
abline(h = 0, lty = 'dashed', lwd = 2)
gp_better <- length(which(diff_scores < 0))
title(main = paste0('GP better in ', gp_better, ' of 25 evaluations',
                    '\nMean difference = ', 
                    round(mean(diff_scores, na.rm = TRUE), 2)))


