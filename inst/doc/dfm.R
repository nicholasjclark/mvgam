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
  fig.asp = 0.7,
  fig.width = 6,
  out.width = "70%",
  fig.align = "center"
)
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = "serif"))


## ----simulate-----------------------------------------------------------------
set.seed(3)

# Dimensions, true AR(1) parameters and time split
n_time <- 80
n_series <- 6
n_lv <- 3
n_train <- 65
phi_true <- c(0.85, 0.15, 0.55)
sig_true <- c(0.5, 0.35, 0.8)

# Simulate the three latent trends; first state drawn from the
# AR(1) stationary marginal so the process starts in equilibrium
lv <- matrix(NA, n_time, n_lv)
lv[1, ] <- rnorm(n_lv, 0, sig_true / sqrt(1 - phi_true^2))
for (t in 2:n_time) {
  lv[t, ] <- phi_true * lv[t - 1L, ] + rnorm(n_lv, 0, sig_true)
}

# Fixed loadings: series 1-3 -> trend 1, series 4-5 -> trend 2,
# series 6 alone on trend 3
Z_true <- matrix(0, nrow = n_series, ncol = n_lv)
Z_true[1, 1] <- 1
Z_true[2, 1] <- 1
Z_true[3, 1] <- 1
Z_true[4, 2] <- 1
Z_true[5, 2] <- 1
Z_true[6, 3] <- 1

# Observations: series-level mean plus Gaussian noise (sigma = 0.2)
mu <- lv %*% t(Z_true)
y_mat <- mu + matrix(rnorm(n_time * n_series, 0, 0.2), n_time, n_series)

# Long-format data frame + train / test split
sim_data <- data.frame(
  time = rep(seq_len(n_time), n_series),
  series = factor(
    rep(paste0("s", seq_len(n_series)), each = n_time),
    levels = paste0("s", seq_len(n_series))
  ),
  y = as.numeric(y_mat)
)
train <- sim_data[sim_data$time <= n_train, ]
test  <- sim_data[sim_data$time >  n_train, ]


## ----plot-observed, fig.cap = "Six simulated series with the last 15 timepoints held out for forecast scoring. Series 1-3 track a long-memory trend, series 4-5 track a short-memory trend, and series 6 rides its own medium-memory trend with a larger amplitude."----
mvgam_data(train, y = "y", family = gaussian(), newdata = test)


## ----trend-map----------------------------------------------------------------
tm_correct <- data.frame(
  series = paste0("s", seq_len(n_series)),
  trend = c(1L, 1L, 1L, 2L, 2L, 3L)
)
tm_correct


## ----fit-correct, results = "hide"--------------------------------------------
fit_correct <- mvgam(
  formula = y ~ 1,
  trend_formula = ~ AR(p = 1),
  trend_map = tm_correct,
  data = train,
  newdata = test,
  family = gaussian(),
  chains = 2L,
  samples = 500L,
  burnin = 500L,
  silent = 2L,
  refresh = 0
)


## ----summary------------------------------------------------------------------
summary(fit_correct)


## ----plot-trend-hindcast, fig.cap = "Trend-scale hindcasts. Series 1-3 (top row) share one latent path; series 4-5 (bottom left, centre) share another; series 6 (bottom right) rides its own trend."----
plot(hindcast(fit_correct, type = "trend"))


## ----plot-factors-correct, fig.cap = "The three latent AR(1) factors recovered by the correct-loadings fit, with the percentage of series-level variance each explains. Trend 1 (three series) contributes most, trend 3 (the outlier series) contributes a substantial share, and trend 2 (two short-memory series) contributes least."----
plot(fit_correct, type = "factors")


## ----plot-forecast-correct, fig.cap = "Forecasts under the correct trend_map. Series 1-3 reproduce the same latent forecast; series 4-5 do the same for the short-memory trend; series 6 forecasts its own dynamics. The long-memory ribbon fans out gradually; the short-memory ribbon returns to zero within a few timesteps."----
plot(forecast(fit_correct, newdata = test))


## ----free-z-fit, results = "hide"---------------------------------------------
Z_free <- matrix(NA_real_, nrow = n_series, ncol = n_lv)
fit_free <- mvgam(
  formula = y ~ 1,
  trend_formula = ~ AR(p = 1),
  trend_map = Z_free,
  data = train,
  newdata = test,
  family = gaussian(),
  chains = 2L,
  samples = 500L,
  burnin = 500L,
  silent = 2L,
  refresh = 0
)


## ----plot-factors-free, fig.cap = "The three latent factors recovered by the free-loadings fit. The leading factor is the same long-memory pattern the correct-loadings fit found; the remaining two capture the same faster dynamics, with a slightly different variance allocation because the QR identification does not know which sharing structure was used to generate the data."----
plot(fit_free, type = "factors")


## ----shared-fit, results = "hide"---------------------------------------------
fit_shared <- mvgam(
  formula = y ~ 1,
  trend_formula = ~ AR(p = 1),
  trend_map = "shared",
  data = train,
  newdata = test,
  family = gaussian(),
  chains = 2L,
  samples = 500L,
  burnin = 500L,
  silent = 2L,
  refresh = 0
)


## ----plot-forecast-shared, fig.cap = "Forecasts under the misspecified fit that pools every series onto one shared trend. Compared with the correct-loadings forecasts above, both the hindcast and forecast ribbons are visibly wider on every series because one latent process is trying to carry three distinct dynamical regimes."----
plot(forecast(fit_shared, newdata = test))


## ----scores-------------------------------------------------------------------
score_totals <- function(fit) {
  fc <- forecast(fit, newdata = test)
  c(
    crps = sum(sapply(
      score(fc, score = "crps")[levels(train$series)],
      function(x) sum(x$score, na.rm = TRUE)
    )),
    logs = sum(sapply(
      score(fc, score = "logs")[levels(train$series)],
      function(x) sum(x$score, na.rm = TRUE)
    )),
    energy = sum(score(fc, score = "energy")$all_series$score,
                  na.rm = TRUE)
  )
}
scores_tbl <- rbind(
  correct = score_totals(fit_correct),
  shared  = score_totals(fit_shared),
  free    = score_totals(fit_free)
)
round(scores_tbl, 2)

