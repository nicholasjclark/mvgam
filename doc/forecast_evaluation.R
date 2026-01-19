params <-
  list(EVAL = TRUE)

## ----echo = FALSE----------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  dpi = 100,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center"
)
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = "serif"))


## --------------------------------------------------------------------------------
set.seed(1)
simdat <- sim_mvgam(
  T = 100,
  n_series = 3,
  mu = 2,
  trend_model = GP(),
  prop_trend = 0.75,
  family = poisson(),
  prop_missing = 0.10
)


## --------------------------------------------------------------------------------
str(simdat)


## ----fig.alt = "Plotting time series features for GAM models in mvgam"-----------
plot_mvgam_series(
  data = simdat$data_train,
  series = "all"
)


## ----fig.alt = "Plotting time series features for GAM models in mvgam"-----------
plot_mvgam_series(
  data = simdat$data_train,
  newdata = simdat$data_test,
  series = 1
)


## ----include=FALSE---------------------------------------------------------------
mod1 <- mvgam(
  y ~ s(season, bs = "cc", k = 8) +
    s(time, by = series, k = 20),
  knots = list(season = c(0.5, 12.5)),
  trend_model = "None",
  data = simdat$data_train,
  newdata = simdat$data_test
)


## ----eval=FALSE------------------------------------------------------------------
# mod1 <- mvgam(
#   y ~ s(season, bs = "cc", k = 8) +
#     s(time, by = series, bs = "cr", k = 20),
#   knots = list(season = c(0.5, 12.5)),
#   trend_model = "None",
#   data = simdat$data_train,
#   silent = 2
# )

## --------------------------------------------------------------------------------
summary(mod1, include_betas = FALSE)


## ----fig.alt = "Plotting GAM smooth functions using mvgam"-----------------------
conditional_effects(mod1, type = "link")


## ----include=FALSE, message=FALSE------------------------------------------------
mod2 <- mvgam(
  y ~ 1,
  trend_formula = ~ s(season, bs = "cc", k = 8) - 1,
  trend_knots = list(season = c(0.5, 12.5)),
  trend_model = AR(cor = TRUE),
  noncentred = TRUE,
  data = simdat$data_train,
  silent = 1
)


## ----eval=FALSE------------------------------------------------------------------
# mod2 <- mvgam(y ~ 1,
#   trend_formula = ~ s(season, bs = "cc", k = 8) - 1,
#   trend_knots = list(season = c(0.5, 12.5)),
#   trend_model = AR(cor = TRUE),
#   noncentred = TRUE,
#   data = simdat$data_train,
#   silent = 1
# )

## --------------------------------------------------------------------------------
summary(mod2, include_betas = FALSE)


## ----fig.alt = "Summarising latent Gaussian Process parameters in mvgam"---------
mcmc_plot(mod2, variable = "ar", regex = TRUE, type = "areas")


## ----fig.alt = "Summarising latent Gaussian Process parameters in mvgam"---------
mcmc_plot(mod2, variable = "sigma", regex = TRUE, type = "areas")


## ----fig.alt = "Plotting latent Gaussian Process effects in mvgam and marginaleffects"----
conditional_effects(mod2, type = "link")


## --------------------------------------------------------------------------------
fc_mod1 <- forecast(mod1, newdata = simdat$data_test)
fc_mod2 <- forecast(mod2, newdata = simdat$data_test)


## --------------------------------------------------------------------------------
str(fc_mod1)


## --------------------------------------------------------------------------------
plot(fc_mod1, series = 1)
plot(fc_mod2, series = 1)

plot(fc_mod1, series = 2)
plot(fc_mod2, series = 2)


## ----include=FALSE---------------------------------------------------------------
mod2 <- mvgam(
  y ~ 1,
  trend_formula = ~ s(season, bs = "cc", k = 8) - 1,
  trend_knots = list(season = c(0.5, 12.5)),
  trend_model = AR(cor = TRUE),
  noncentred = TRUE,
  data = simdat$data_train,
  newdata = simdat$data_test,
  silent = 2
)


## ----eval=FALSE------------------------------------------------------------------
# mod2 <- mvgam(y ~ 1,
#   trend_formula = ~ s(season, bs = "cc", k = 8) - 1,
#   trend_knots = list(season = c(0.5, 12.5)),
#   trend_model = AR(cor = TRUE),
#   noncentred = TRUE,
#   data = simdat$data_train,
#   newdata = simdat$data_test,
#   silent = 2
# )

## --------------------------------------------------------------------------------
fc_mod2 <- forecast(mod2)


## ----warning=FALSE, fig.alt = "Plotting posterior forecast distributions using mvgam and R"----
plot(fc_mod2, series = 1)


## ----warning=FALSE---------------------------------------------------------------
crps_mod1 <- score(fc_mod1, score = "crps")
str(crps_mod1)
crps_mod1$series_1


## ----warning=FALSE---------------------------------------------------------------
crps_mod1 <- score(fc_mod1, score = "crps", interval_width = 0.6)
crps_mod1$series_1


## --------------------------------------------------------------------------------
link_mod1 <- forecast(mod1, newdata = simdat$data_test, type = "link")
score(link_mod1, score = "elpd")$series_1


## --------------------------------------------------------------------------------
energy_mod2 <- score(fc_mod2, score = "energy")
str(energy_mod2)


## --------------------------------------------------------------------------------
energy_mod2$all_series


## --------------------------------------------------------------------------------
crps_mod1 <- score(fc_mod1, score = "crps")
crps_mod2 <- score(fc_mod2, score = "crps")

diff_scores <- crps_mod2$series_1$score -
  crps_mod1$series_1$score
plot(
  diff_scores,
  pch = 16,
  cex = 1.25,
  col = "darkred",
  ylim = c(
    -1 * max(abs(diff_scores), na.rm = TRUE),
    max(abs(diff_scores), na.rm = TRUE)
  ),
  bty = "l",
  xlab = "Forecast horizon",
  ylab = expression(CRPS[AR1] ~ -~ CRPS[spline])
)
abline(h = 0, lty = "dashed", lwd = 2)
ar1_better <- length(which(diff_scores < 0))
title(
  main = paste0(
    "AR(1) better in ",
    ar1_better,
    " of ",
    length(diff_scores),
    " evaluations",
    "\nMean difference = ",
    round(mean(diff_scores, na.rm = TRUE), 2)
  )
)


diff_scores <- crps_mod2$series_2$score -
  crps_mod1$series_2$score
plot(
  diff_scores,
  pch = 16,
  cex = 1.25,
  col = "darkred",
  ylim = c(
    -1 * max(abs(diff_scores), na.rm = TRUE),
    max(abs(diff_scores), na.rm = TRUE)
  ),
  bty = "l",
  xlab = "Forecast horizon",
  ylab = expression(CRPS[AR1] ~ -~ CRPS[spline])
)
abline(h = 0, lty = "dashed", lwd = 2)
ar1_better <- length(which(diff_scores < 0))
title(
  main = paste0(
    "AR(1) better in ",
    ar1_better,
    " of ",
    length(diff_scores),
    " evaluations",
    "\nMean difference = ",
    round(mean(diff_scores, na.rm = TRUE), 2)
  )
)

diff_scores <- crps_mod2$series_3$score -
  crps_mod1$series_3$score
plot(
  diff_scores,
  pch = 16,
  cex = 1.25,
  col = "darkred",
  ylim = c(
    -1 * max(abs(diff_scores), na.rm = TRUE),
    max(abs(diff_scores), na.rm = TRUE)
  ),
  bty = "l",
  xlab = "Forecast horizon",
  ylab = expression(CRPS[AR1] ~ -~ CRPS[spline])
)
abline(h = 0, lty = "dashed", lwd = 2)
ar1_better <- length(which(diff_scores < 0))
title(
  main = paste0(
    "AR(1) better in ",
    ar1_better,
    " of ",
    length(diff_scores),
    " evaluations",
    "\nMean difference = ",
    round(mean(diff_scores, na.rm = TRUE), 2)
  )
)
