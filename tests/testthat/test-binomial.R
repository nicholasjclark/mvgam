context("binomial")

# Simulate two time series of Binomial trials
trials <- sample(c(20:25), 50, replace = TRUE)
x <- rnorm(50)
detprob1 <- plogis(-0.5 + 0.9*x)
detprob2 <- plogis(-0.1 -0.7*x)
dat <- rbind(data.frame(y = rbinom(n = 50, size = trials,
                                   prob = detprob1),
                        time = 1:50,
                        series = 'series1',
                        x = x,
                        ntrials = trials),
             data.frame(y = rbinom(n = 50, size = trials,
                                   prob = detprob2),
                        time = 1:50,
                        series = 'series2',
                        x = x,
                        ntrials = trials)) %>%
  dplyr::mutate(series = as.factor(series)) %>%
  dplyr::arrange(time, series)

# Throw in some NAs
dat$y[c(1,5,9)] <- NA

# Training and testing splits
dat_train <- dat %>%
  dplyr::filter(time <= 40)
dat_test <- dat %>%
  dplyr::filter(time > 40)

test_that("cbind() syntax required for binomial()", {
  # Initial warning should be issued when calling binomial or beta-binomial
  expect_warning(mvgam(cbind(y, ntrials) ~ s(series, bs = 're') +
                   gp(x, by = series, c = 5/4, k = 5),
                 family = binomial(),
                 data = dat_train,
                 run_model = FALSE))

  expect_error(mvgam(y ~ series + s(x, by = series),
                     family = binomial(),
                     data = dat_train,
                     run_model = FALSE),
               'Binomial family requires cbind() syntax in the formula left-hand side',
               fixed = TRUE)

  # Should work if correctly specified
  mod <- mvgam(cbind(y, ntrials) ~ s(series, bs = 're') +
                 gp(x, by = series, c = 5/4, k = 5),
               family = binomial(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ binomial(',
                        mod$model_file, fixed = TRUE)))

  # Also with a trend_formula
  mod <- mvgam(cbind(y, ntrials) ~ series,
               trend_formula = ~ s(x, by = trend),
               family = binomial(),
               trend_model = AR(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ binomial(',
                        mod$model_file, fixed = TRUE)))

  # Also with no predictors
  mod <- mvgam(cbind(y, ntrials) ~ 1,
               family = binomial(),
               trend_model = AR(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ binomial(',
                        mod$model_file, fixed = TRUE)))
})

test_that("binomial() post-processing works", {
  skip_on_cran()
  mod <- mvgam(cbind(y, ntrials) ~ series,
               trend_formula = ~ s(x, by = trend),
               family = binomial(),
               trend_model = AR(),
               data = dat_train,
               burnin = 200,
               samples = 200)
  expect_no_error(summary(mod))
  expect_no_error(print(mod))

  preds <- predict(mod, summary = FALSE, type = 'response')
  expect_true(NCOL(preds) == NROW(dat_train))
  expect_true(all(preds >= 0L))

  preds <- predict(mod, newdata = dat_test, summary = FALSE)
  expect_true(NCOL(preds) == NROW(dat_test))

  expect_no_error(ppc(mod))
  expect_no_error(ppc(mod, type = 'density'))
  expect_no_error(ppc(mod, type = 'mean'))
  expect_no_error(ppc(mod, type = 'pit'))
  expect_no_error(ppc(mod, type = 'cdf'))
  expect_no_error(ppc(mod, type = 'rootogram'))

  expect_no_error(plot(mod, type = 'residuals'))

  expect_no_error(plot_mvgam_series(object = mod))
  expect_no_error(plot_mvgam_series(object = mod, series = 'all'))

  expect_no_error(plot(mod, type = 'forecast'))
  expect_no_error(plot(mod, type = 'forecast',
                       newdata = dat_test))
  expect_no_error(plot(mod, type = 'trend'))
  expect_no_error(plot(mod, type = 'trend',
                       newdata = dat_test))
  expect_true(inherits(hindcast(mod), 'mvgam_forecast'))

  expect_no_error(plot(mod, type = 'smooths', trend_effects = TRUE))
  expect_no_error(plot(mod, type = 'smooths',
                       realisations = TRUE, trend_effects = TRUE))
  expect_no_error(plot(mod, type = 'smooths',
                       residuals = TRUE, trend_effects = TRUE))

  expect_true(inherits(SM(conditional_effects(mod)),
                       'mvgam_conditional_effects'))
  expect_true(inherits(SM(conditional_effects(mod,
                                              type = 'link')),
                       'mvgam_conditional_effects'))
  expect_loo(SW(loo(mod)))
})

# All tests should apply to beta_binomial as well
test_that("cbind() syntax required for beta_binomial()", {
  expect_error(mvgam(y ~ series + s(x, by = series),
                     family = beta_binomial(),
                     data = dat_train,
                     run_model = FALSE),
               'Binomial family requires cbind() syntax in the formula left-hand side',
               fixed = TRUE)

  # Should work if correctly specified
  mod <- mvgam(cbind(y, ntrials) ~ s(series, bs = 're') +
                 gp(x, by = series, c = 5/4, k = 5),
               family = beta_binomial(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ beta_binomial(',
                        mod$model_file, fixed = TRUE)))

  # Also with a trend_formula
  mod <- mvgam(cbind(y, ntrials) ~ series,
               trend_formula = ~ s(x, by = trend),
               family = beta_binomial(),
               trend_model = AR(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ beta_binomial(',
                        mod$model_file, fixed = TRUE)))

  # Also with no predictors and with a prior on phi
  mod <- mvgam(cbind(y, ntrials) ~ 0,
               family = beta_binomial(),
               priors = prior(normal(0, 3), class = phi),
               trend_model = AR(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('beta_binomial(',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl("b[1] = 0;", mod$model_file,
                        fixed = TRUE)))
  expect_true(any(grepl("phi ~ normal(0, 3);", mod$model_file,
                        fixed = TRUE)))
})

test_that("trials variable must be in data for binomial()", {
  expect_error(mvgam(cbind(y, mytrials) ~ series + s(x, by = series),
                     family = binomial(),
                     data = dat_train,
                     run_model = FALSE),
               'variable mytrials not found in data',
               fixed = TRUE)
})

# Simulate two time series of Bernoulli draws
x <- rnorm(50)
detprob1 <- plogis(-0.5 + 0.9*x)
detprob2 <- plogis(-0.1 -0.7*x)
dat <- rbind(data.frame(y = rbinom(n = 50, size = 1, prob = detprob1),
                        time = 1:50,
                        series = 'series1',
                        x = x),
             data.frame(y = rbinom(n = 50, size = 1, prob = detprob2),
                        time = 1:50,
                        series = 'series2',
                        x = x)) %>%
  dplyr::mutate(series = as.factor(series)) %>%
  dplyr::arrange(time, series)

# Throw in some NAs
dat$y[c(1,5,9)] <- NA

# Training and testing splits
dat_train <- dat %>%
  dplyr::filter(time <= 40)
dat_test <- dat %>%
  dplyr::filter(time > 40)

set.seed(100)
gaus_data <- sim_mvgam(family = gaussian(),
                       T = 60,
                       trend_model = 'AR1',
                       seasonality = 'shared',
                       mu = c(-1, 0, 1),
                       trend_rel = 0.5,
                       prop_missing = 0.2)

test_that("bernoulli() behaves appropriately", {
  expect_error(mvgam(y ~ series + s(x, by = series),
                     family = bernoulli(),
                     data = gaus_data$data_train,
                     run_model = FALSE),
               'y values must be 0 <= y <= 1',
               fixed = TRUE)

  mod <- mvgam(y ~ s(series, bs = 're') +
                 gp(x, by = series, c = 5/4, k = 5),
               family = bernoulli(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ bernoulli_logit_glm(',
                        mod$model_file, fixed = TRUE)))

  # Also with a trend_formula
  mod <- mvgam(y ~ series,
               trend_formula = ~ gp(x, by = trend, c = 5/4),
               trend_model = AR(),
               family = bernoulli(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ bernoulli_logit_glm(',
                        mod$model_file, fixed = TRUE)))
})

test_that("bernoulli() post-processing works", {
  skip_on_cran()
  mod <- mvgam(y ~ s(series, bs = 're') +
                 gp(x, by = series, c = 5/4, k = 5),
               trend_model = AR(),
               priors = prior(normal(0, 0.1), class = ar1),
               family = bernoulli(),
               data = dat_train,
               burnin = 200,
               samples = 200)

  expect_no_error(summary(mod))
  expect_no_error(print(mod))

  preds <- predict(mod, summary = FALSE, type = 'response')
  expect_true(NCOL(preds) == NROW(dat_train))
  expect_true(all(preds >= 0L))

  preds <- predict(mod, newdata = dat_test, summary = FALSE)
  expect_true(NCOL(preds) == NROW(dat_test))

  expect_no_error(ppc(mod))
  expect_no_error(ppc(mod, type = 'density'))
  expect_no_error(ppc(mod, type = 'mean'))
  expect_no_error(ppc(mod, type = 'pit'))
  expect_no_error(ppc(mod, type = 'cdf'))
  expect_no_error(ppc(mod, type = 'rootogram'))

  expect_no_error(plot(mod, type = 'residuals'))

  expect_no_error(plot_mvgam_series(object = mod))
  expect_no_error(plot_mvgam_series(object = mod, series = 'all'))

  expect_no_error(plot(mod, type = 'forecast'))
  expect_no_error(plot(mod, type = 'forecast',
                       newdata = dat_test))
  expect_no_error(plot(mod, type = 'trend'))
  expect_no_error(plot(mod, type = 'trend',
                       newdata = dat_test))
  expect_true(inherits(hindcast(mod), 'mvgam_forecast'))

  expect_no_error(plot(mod, type = 'smooths', trend_effects = TRUE))
  expect_no_error(plot(mod, type = 'smooths',
                       realisations = TRUE, trend_effects = TRUE))
  expect_no_error(plot(mod, type = 'smooths',
                       residuals = TRUE, trend_effects = TRUE))

  expect_true(inherits(SM(conditional_effects(mod)),
                       'mvgam_conditional_effects'))
  expect_true(inherits(SM(conditional_effects(mod,
                                              type = 'link')),
                       'mvgam_conditional_effects'))
  expect_loo(SW(loo(mod)))
})
