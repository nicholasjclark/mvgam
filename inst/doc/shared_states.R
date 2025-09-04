params <-
list(EVAL = TRUE)

## ----echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include=FALSE-----------------------------------------------
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


## -----------------------------------------------------------------------
set.seed(122)
simdat <- sim_mvgam(
  trend_model = AR(),
  prop_trend = 0.6,
  mu = c(0, 1, 2),
  family = poisson()
)
trend_map <- data.frame(
  series = unique(simdat$data_train$series),
  trend = c(1, 1, 2)
)
trend_map


## -----------------------------------------------------------------------
all.equal(levels(trend_map$series), 
          levels(simdat$data_train$series))


## -----------------------------------------------------------------------
fake_mod <- mvgam(
  y ~
    # observation model formula, which has a
    # different intercept per series
    series - 1,

  # process model formula, which has a shared seasonal smooth
  # (each latent process model shares the SAME smooth)
  trend_formula = ~ s(season, bs = "cc", k = 6),

  # AR1 dynamics (each latent process model has DIFFERENT)
  # dynamics; processes are estimated using the noncentred
  # parameterisation for improved efficiency
  trend_model = AR(),
  noncentred = TRUE,

  # supplied trend_map
  trend_map = trend_map,

  # data and observation family
  family = poisson(),
  data = simdat$data_train,
  run_model = FALSE
)


## -----------------------------------------------------------------------
stancode(fake_mod)


## -----------------------------------------------------------------------
fake_mod$model_data$Z


## ----full_mod, include = FALSE, results='hide'--------------------------
full_mod <- mvgam(
  y ~ series - 1,
  trend_formula = ~ s(season, bs = "cc", k = 6),
  trend_model = AR(),
  noncentred = TRUE,
  trend_map = trend_map,
  family = poisson(),
  data = simdat$data_train,
  silent = 2
)


## ----eval=FALSE---------------------------------------------------------
# full_mod <- mvgam(
#   y ~ series - 1,
#   trend_formula = ~ s(season, bs = "cc", k = 6),
#   trend_model = AR(),
#   noncentred = TRUE,
#   trend_map = trend_map,
#   family = poisson(),
#   data = simdat$data_train,
#   silent = 2
# )


## -----------------------------------------------------------------------
summary(full_mod)


## -----------------------------------------------------------------------
plot(full_mod, type = "trend", series = 1)
plot(full_mod, type = "trend", series = 2)
plot(full_mod, type = "trend", series = 3)


## -----------------------------------------------------------------------
set.seed(123)
# simulate a nonlinear relationship using the mgcv function gamSim
signal_dat <- mgcv::gamSim(n = 100, eg = 1, scale = 1)

# productivity is one of the variables in the simulated data
productivity <- signal_dat$x2

# simulate the true signal, which already has a nonlinear relationship
# with productivity; we will add in a fairly strong AR1 process to
# contribute to the signal
true_signal <- as.vector(scale(signal_dat$y) +
  arima.sim(100, model = list(ar = 0.8, sd = 0.1)))


## -----------------------------------------------------------------------
plot(
  true_signal,
  type = "l",
  bty = "l", lwd = 2,
  ylab = "True signal",
  xlab = "Time"
)


## -----------------------------------------------------------------------
# Function to simulate a monotonic response to a covariate
sim_monotonic <- function(x, a = 2.2, b = 2) {
  out <- exp(a * x) / (6 + exp(b * x)) * -1
  return(2.5 * as.vector(scale(out)))
}

# Simulated temperature covariate
temperature <- runif(100, -2, 2)

# Simulate the three series
sim_series <- function(n_series = 3, true_signal) {
  temp_effects <- mgcv::gamSim(n = 100, eg = 7, scale = 0.05)
  alphas <- rnorm(n_series, sd = 2)

  do.call(rbind, lapply(seq_len(n_series), function(series) {
    data.frame(
      observed = rnorm(length(true_signal),
        mean = alphas[series] +
          sim_monotonic(temperature, 
                            runif(1, 2.2, 3),
                            runif(1, 2.2, 3)) +
          true_signal,
        sd = runif(1, 1, 2)
      ),
      series = paste0("sensor_", series),
      time = 1:length(true_signal),
      temperature = temperature,
      productivity = productivity,
      true_signal = true_signal
    )
  }))
}
model_dat <- sim_series(true_signal = true_signal) %>%
  dplyr::mutate(series = factor(series))


## -----------------------------------------------------------------------
plot_mvgam_series(
  data = model_dat, y = "observed",
  series = "all"
)


## -----------------------------------------------------------------------
plot(
  observed ~ temperature,
  data = model_dat %>%
    dplyr::filter(series == "sensor_1"),
  pch = 16, bty = "l",
  ylab = "Sensor 1",
  xlab = "Temperature"
)
plot(
  observed ~ temperature,
  data = model_dat %>%
    dplyr::filter(series == "sensor_2"),
  pch = 16, bty = "l",
  ylab = "Sensor 2",
  xlab = "Temperature"
)
plot(
  observed ~ temperature,
  data = model_dat %>%
    dplyr::filter(series == "sensor_3"),
  pch = 16, bty = "l",
  ylab = "Sensor 3",
  xlab = "Temperature"
)


## ----sensor_mod, include = FALSE, results='hide'------------------------
mod <- mvgam(
  formula =
  # formula for observations, allowing for different
  # intercepts and smooth effects of temperature
    observed ~ series +
      s(temperature, k = 10) +
      s(series, temperature, bs = "sz", k = 8),
  trend_formula =
  # formula for the latent signal, which can depend
  # nonlinearly on productivity
    ~ s(productivity, k = 8) - 1,
  trend_model =
  # in addition to productivity effects, the signal is
  # assumed to exhibit temporal autocorrelation
    AR(),
  noncentred = TRUE,
  trend_map =
  # trend_map forces all sensors to track the same
  # latent signal
    data.frame(
      series = unique(model_dat$series),
      trend = c(1, 1, 1)
    ),

  # informative priors on process error
  # and observation error will help with convergence
  priors = c(
    prior(normal(2, 0.5), class = sigma),
    prior(normal(1, 0.5), class = sigma_obs)
  ),

  # Gaussian observations
  family = gaussian(),
  burnin = 600,
  control = list(adapt_delta = 0.95),
  data = model_dat,
  silent = 2
)


## ----eval=FALSE---------------------------------------------------------
# mod <- mvgam(
#   formula =
#   # formula for observations, allowing for different
#   # intercepts and hierarchical smooth effects of temperature
#     observed ~ series +
#       s(temperature, k = 10) +
#       s(series, temperature, bs = "sz", k = 8),
#   trend_formula =
#   # formula for the latent signal, which can depend
#   # nonlinearly on productivity
#     ~ s(productivity, k = 8) - 1,
#   trend_model =
#   # in addition to productivity effects, the signal is
#   # assumed to exhibit temporal autocorrelation
#     AR(),
#   noncentred = TRUE,
#   trend_map =
#   # trend_map forces all sensors to track the same
#   # latent signal
#     data.frame(
#       series = unique(model_dat$series),
#       trend = c(1, 1, 1)
#     ),
# 
#   # informative priors on process error
#   # and observation error will help with convergence
#   priors = c(
#     prior(normal(2, 0.5), class = sigma),
#     prior(normal(1, 0.5), class = sigma_obs)
#   ),
# 
#   # Gaussian observations
#   family = gaussian(),
#   data = model_dat,
#   silent = 2
# )


## -----------------------------------------------------------------------
summary(mod, include_betas = FALSE)


## -----------------------------------------------------------------------
conditional_effects(mod, type = "link")


## -----------------------------------------------------------------------
plot_predictions(
  mod,
  condition = c("temperature", "series", "series"),
  points = 0.5
) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------
plot(mod, 
     type = "trend") +
  ggplot2::geom_point(data = data.frame(time = 1:100,
                                        y = true_signal),
                      mapping = ggplot2::aes(x = time,
                                             y = y))

