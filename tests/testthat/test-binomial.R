context("binomial")

# Simulate two time series of Binomial trials
trials <- sample(c(20:25), 50, replace = TRUE)
x <- rnorm(50)
detprob1 <- plogis(-0.5 + 0.9*x)
detprob2 <- plogis(-0.1 -0.7*x)
dat <- rbind(data.frame(y = rbinom(n = 50, size = trials, prob = detprob1),
                        time = 1:50,
                        series = 'series1',
                        x = x,
                        ntrials = trials),
             data.frame(y = rbinom(n = 50, size = trials, prob = detprob2),
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
  expect_error(mvgam(y ~ series + s(x, by = series),
                     family = binomial(),
                     data = dat_train,
                     run_model = FALSE),
               'Binomial family requires cbind() syntax in the formula left-hand side',
               fixed = TRUE)

  mod <- mvgam(cbind(y, ntrials) ~ s(series, bs = 're') +
                 gp(x, by = series, c = 5/4, k = 5),
               family = binomial(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ binomial_logit_glm(',
                        mod$model_file, fixed = TRUE)))

  # Also with a trend_formula
  mod <- mvgam(cbind(y, ntrials) ~ series,
               trend_formula = ~ s(x, by = trend),
               family = binomial(),
               trend_model = AR(),
               data = dat_train,
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(any(grepl('flat_ys ~ binomial_logit_glm(',
                        mod$model_file, fixed = TRUE)))
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
                        x = x,
                        ntrials = trials),
             data.frame(y = rbinom(n = 50, size = 1, prob = detprob2),
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
