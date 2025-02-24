source("setup_tests_local.R")

#### Simulated data to test post-processing ####
gaus_data <- sim_mvgam(
  family = gaussian(),
  T = 60,
  trend_model = 'AR1',
  seasonality = 'shared',
  mu = c(-1, 0, 1),
  prop_trend = 0.5,
  prop_missing = 0.2
)
pois_data <- sim_mvgam(
  family = poisson(),
  trend_model = AR(),
  prop_trend = 0.5,
  mu = c(1, 3, 5),
  T = 60
)
beta_data <- sim_mvgam(
  family = betar(),
  trend_model = GP(),
  mu = c(-1.5, 0, 1.5),
  prop_trend = 0.75,
  T = 60
)

#### Simple models, trying meanfield and sampling ####
gaus_ar <- mvgam(
  y ~
    s(series, bs = 're') +
      s(season, bs = 'cc', k = 5) -
      1,
  trend_model = AR(),
  data = gaus_data$data_train,
  family = gaussian(),
  algorithm = 'meanfield',
  samples = 200
)
gaus_arfc <- mvgam(
  y ~
    s(series, bs = 're') +
      s(season, bs = 'cc', k = 5) -
      1,
  trend_model = AR(),
  data = gaus_data$data_train,
  newdata = gaus_data$data_test,
  family = gaussian(),
  algorithm = 'meanfield',
  samples = 200
)
pois_ar <- mvgam(
  y ~ series,
  trend_formula = ~ s(season, bs = 'cc', k = 5),
  trend_model = AR(),
  data = pois_data$data_train,
  family = poisson(),
  samples = 200
)
pois_arfc <- mvgam(
  y ~ series,
  trend_formula = ~ s(season, bs = 'cc', k = 5),
  trend_model = AR(),
  data = pois_data$data_train,
  newdata = pois_data$data_test,
  family = poisson(),
  samples = 200
)
beta_gp <- mvgam(
  y ~ s(series, bs = 're'),
  trend_formula = ~ gp(time, by = trend),
  data = beta_data$data_train,
  family = betar(),
  priors = prior(normal(0, 0.1), class = ar1),
  trend_model = AR(),
  samples = 200
)

#### Tests for the simple models ####
test_that("lfo_cv working properly", {
  lfcv <- lfo_cv(gaus_arfc, min_t = 42)
  expect_true(inherits(lfcv, 'mvgam_lfo'))
  expect_true(all.equal(lfcv$eval_timepoints, c(43, 44)))
})

test_that('mvgam poisson forecasts agree with Stan', {
  # Forecasts made from within Stan should broadly agree with forecasts
  # made from forecast()
  score_stan <- plot_mvgam_fc(
    pois_arfc,
    series = 1,
    newdata = gaus_data$data_test,
    return_score = TRUE
  )
  score_mvgam <- plot_mvgam_fc(
    pois_ar,
    series = 1,
    newdata = gaus_data$data_test,
    return_score = TRUE
  )
  expect_equal(score_mvgam$score, score_stan$score, tolerance = 3)
})

test_that("loo working properly", {
  loomod <- SW(loo(pois_arfc))
  expect_true(inherits(loomod, 'psis_loo'))

  loomod <- SW(loo(gaus_arfc))
  expect_true(inherits(loomod, 'psis_loo'))
})

test_that("gp model gives correct predictions", {
  p <- conditional_effects(beta_gp)
  expect_true(inherits(p, 'mvgam_conditional_effects'))
  expect_ggplot(conditional_effects(beta_gp)[[1]])
  expect_ggplot(conditional_effects(beta_gp)[[2]])

  post_modes <- coef(beta_gp)[, 2]
  expect_true(post_modes[2] < post_modes[3])
  expect_true(post_modes[3] < post_modes[4])

  ar <- colMeans(as.matrix(beta_gp, variable = "ar1", regex = TRUE))
  expect_range(ar[1], -0.15, 0.15)
  expect_range(ar[2], -0.15, 0.15)
  expect_range(ar[3], -0.15, 0.15)

  expect_equal(dim(fitted(beta_gp)), c(NROW(beta_data$data_train), 4))
})

#### A continuous time AR example ####
sim_corcar1 = function(n = 120, phi = 0.5, sigma = 1, sigma_obs = 0.75) {
  # Sample irregularly spaced time intervals
  time_dis <- c(0, runif(n - 1, -0.1, 1))
  time_dis[time_dis < 0] <- 0
  time_dis <- time_dis * 5

  # Set up the latent dynamic process
  x <- vector(length = n)
  x[1] <- -0.3
  for (i in 2:n) {
    # zero-distances will cause problems in sampling, so mvgam uses a
    # minimum threshold; this simulation function emulates that process
    if (time_dis[i] == 0) {
      x[i] <- rnorm(1, mean = (phi^1e-12) * x[i - 1], sd = sigma)
    } else {
      x[i] <- rnorm(1, mean = (phi^time_dis[i]) * x[i - 1], sd = sigma)
    }
  }

  # Add 12-month seasonality
  cov1 <- sin(2 * pi * (1:n) / 12)
  cov2 <- cos(2 * pi * (1:n) / 12)
  beta1 <- runif(1, 0.3, 0.7)
  beta2 <- runif(1, 0.2, 0.5)
  seasonality <- beta1 * cov1 + beta2 * cov2

  # Take Gaussian observations with error and return
  data.frame(
    y = rnorm(n, mean = x + seasonality, sd = sigma),
    season = rep(1:12, 20)[1:n],
    time = cumsum(time_dis)
  )
}

# Sample two time series
dat <- rbind(
  dplyr::bind_cols(
    sim_corcar1(phi = 0.65, sigma_obs = 0.55),
    data.frame(series = 'series1')
  ),
  dplyr::bind_cols(
    sim_corcar1(phi = 0.8, sigma_obs = 0.35),
    data.frame(series = 'series2')
  )
) %>%
  dplyr::mutate(series = as.factor(series))

test_that("CAR model runs properly", {
  # mvgam with CAR(1) trends and series-level seasonal smooths
  mod <- mvgam(
    formula = y ~ s(season, bs = 'cc', k = 5, by = series),
    trend_model = CAR(),
    data = dat,
    family = gaussian(),
    samples = 200
  )
  expect_true(inherits(mod, 'mvgam'))

  p <- conditional_effects(mod)
  expect_true(inherits(p, 'mvgam_conditional_effects'))
  expect_ggplot(conditional_effects(mod)[[1]])

  ar <- colMeans(as.matrix(mod, variable = "ar1", regex = TRUE))
  expect_range(ar[1], 0.35, 0.75)
  expect_range(ar[1], 0.55, 0.95)

  expect_equal(dim(fitted(mod)), c(NROW(dat), 4))

  # State-space formulation should also work
  mod <- mvgam(
    formula = y ~ 1,
    trend_formula = ~ s(season, bs = 'cc', k = 5, by = trend),
    trend_model = CAR(),
    data = dat,
    family = gaussian()
  )
  expect_true(inherits(mod, 'mvgam'))

  p <- conditional_effects(mod)
  expect_true(inherits(p, 'mvgam_conditional_effects'))
  expect_ggplot(conditional_effects(mod)[[1]])

  ar <- colMeans(as.matrix(mod, variable = "ar1", regex = TRUE))
  expect_range(ar[1], 0.35, 0.75)
  expect_range(ar[1], 0.55, 0.95)

  expect_equal(dim(fitted(mod)), c(NROW(dat), 4))
})

#### A monotonic smooth example ####
# 'by' terms that produce a different smooth for each level of the 'by'
# factor
set.seed(123123)
x <- runif(80) * 4 - 1
x <- sort(x)

# Two different monotonic smooths, one for each factor level
f <- exp(4 * x) / (1 + exp(4 * x))
f2 <- exp(3.5 * x) / (1 + exp(3 * x))
fac <- c(rep('a', 80), rep('b', 80))
y <- c(f + rnorm(80) * 0.1, f2 + rnorm(80) * 0.2)

# Gather all data into a data.frame, including the factor 'by' variable
mod_data <- data.frame(y, x, fac = as.factor(fac))
mod_data$time <- 1:NROW(mod_data)

test_that("monotonic smooths behave properly", {
  # Fit a model with different smooths per factor level
  mod <- mvgam(
    y ~ s(x, bs = 'moi', by = fac, k = 8),
    data = mod_data,
    family = gaussian(),
    samples = 200
  )

  expect_true(inherits(mod$mgcv_model$smooth[[1]], 'moi.smooth'))
  expect_ggplot(SM(pp_check(mod)))

  # First derivatives (on link scale) should never be
  # negative for either factor level
  derivs <- slopes(mod, variables = 'x', by = c('x', 'fac'), type = 'link')
  expect_true(all(derivs$estimate > 0))
})
