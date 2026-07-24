params <-
list(EVAL = TRUE)

## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include = FALSE---------------------------------------------------
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


## ----sim-data-----------------------------------------------------------------
set.seed(7)
sim <- sim_mvgam(
  type               = 7L,
  family             = gaussian(),
  n_series           = 1L,
  n_timepoints       = 240L,
  proportional_train = 12 / 13
)
train <- sim$data_train
test  <- sim$data_test
nrow(train)
nrow(test)
head(train)


## ----sim-plot, fig.asp = 0.85, out.width = "85%"------------------------------
mvgam_data(train, family = gaussian(), newdata = test)


## ----model-step1, results = "hide"--------------------------------------------
step1 <- mvgam(
  y ~ s(season, bs = "cc", k = 12),
  knots   = list(season = c(0.5, 12.5)),
  family  = gaussian(),
  data    = train,
  newdata = test,
  chains  = 2,
  samples = 400,
  burnin  = 400,
  silent  = 2
)


## -----------------------------------------------------------------------------
summary(step1, include_betas = FALSE)


## ----step1-cond-effects, fig.asp = 0.5, out.width = "70%"---------------------
conditional_effects(step1)


## ----step1-resid, fig.asp = 0.5, out.width = "80%"----------------------------
plot(step1, type = "residuals")


## ----model-step2, results = "hide"--------------------------------------------
step2 <- mvgam(
  y ~ s(season, bs = "cc", k = 12),
  knots         = list(season = c(0.5, 12.5)),
  trend_formula = ~ AR(p = c(1, 12)),
  family        = gaussian(),
  data          = train,
  newdata       = test,
  chains        = 2,
  samples       = 400,
  burnin        = 400,
  silent        = 2
)


## -----------------------------------------------------------------------------
summary(step2, include_betas = FALSE)


## ----step2-ar-recovery--------------------------------------------------------
ar1  <- as.numeric(as.array(step2, variable = "ar1_trend[1]"))
ar12 <- as.numeric(as.array(step2, variable = "ar12_trend[1]"))
data.frame(
  parameter = c("ar1_trend", "ar12_trend"),
  truth     = c(0.55, 0.40),
  median    = c(median(ar1),  median(ar12)),
  lower_90  = c(quantile(ar1,  0.05), quantile(ar12, 0.05)),
  upper_90  = c(quantile(ar1,  0.95), quantile(ar12, 0.95)),
  prob_gt_0 = c(mean(ar1 > 0), mean(ar12 > 0))
)


## ----step2-mcmc, fig.asp = 0.45, out.width = "70%"----------------------------
mcmc_plot(step2, variable = "ar[0-9]+_trend",
          regex = TRUE, type = "areas")


## ----step2-resid, fig.asp = 0.5, out.width = "80%"----------------------------
plot(step2, type = "residuals")


## ----step2-cond-effects, fig.asp = 0.5, out.width = "70%"---------------------
conditional_effects(step2)


## ----hindcast-forecast--------------------------------------------------------
fc <- forecast(step2, newdata = test)
dim(fc$hindcasts[[1]])
dim(fc$forecasts[[1]])


## ----forecast-plot, fig.asp = 0.55, out.width = "80%"-------------------------
plot(fc, series = 1)


## ----forecast-score-----------------------------------------------------------
fc1 <- forecast(step1, newdata = test)
fc2 <- forecast(step2, newdata = test)
score_rules <- c("crps", "logs", "dss", "drps")
scores <- vapply(score_rules, function(rule) {
  s1 <- score(fc1, score = rule)
  s2 <- score(fc2, score = rule)
  c(no_trend = sum(s1[[1]]$score, na.rm = TRUE),
    ar_1_12  = sum(s2[[1]]$score, na.rm = TRUE))
}, numeric(2))
round(scores, 2)


## ----lfo-example, fig.asp = 0.55, out.width = "80%", results = "hide"---------
lfo_step2 <- lfo_cv(step2, min_t = 200, fc_horizon = 12,
                    silent = 2)
plot(lfo_step2)


## ----how-to-cite--------------------------------------------------------------
how_to_cite(step2)

