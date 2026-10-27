# CI tests for the typed catalog in `R/sim_mvgam.R`. The local suite
# checks numerical recovery against a fitted mvgam.


test_that("every type returns its covariates, trend and truths", {
  covariates <- list("x", c("x", "z"), c("x", "grp"), "x", c("x", "z"),
                     "season", "x")
  for (type_id in 1:7) {
    out <- sim_mvgam(type = type_id, n_series = 2L, n_timepoints = 60L,
                     seed = 11L)
    expect_true(all(c("y", "series", "time", covariates[[type_id]]) %in%
                      colnames(out$data_train)))
    expect_identical(dim(out$true_trend), c(60L, 2L))
    expect_true(all(is.finite(out$true_trend)))
    expect_gte(length(out$true_smooths), 1L)
  }
})


test_that("each family simulates values in its own support", {
  draw <- function(fam) {
    sim_mvgam(type = 1L, family = fam, n_timepoints = 60L,
              seed = 7L)$data_train$y
  }
  for (fam in list(poisson(), brms::brmsfamily("negbinomial"),
                   beta_nb())) {
    y <- draw(fam)
    expect_true(all(y >= 0 & y == round(y)))
  }
  expect_true(all(draw(binomial()) <= 10L))
  expect_true(all(draw(bernoulli()) %in% c(0L, 1L)))
  expect_true(all(draw(com_binomial()) <= 10L))
  y_beta <- draw(brms::brmsfamily("beta"))
  expect_true(all(y_beta > 0 & y_beta < 1))
  expect_true(all(draw(Gamma(link = "log")) > 0))
  expect_true(all(is.finite(draw(brms::brmsfamily("student")))))
})


test_that("tweedie draws hold zeros and positive values", {
  out <- sim_mvgam(type = 1L, family = tweedie(), n_timepoints = 80L,
                   trend_model = AR(), seed = 11L)
  y <- out$data_train$y
  expect_true(all(y >= 0))
  expect_gt(sum(y == 0), 0L)
  expect_gt(sum(y > 0), 0L)
})


test_that("tweedie draws take the Tweedie mean, variance and zeros", {
  set.seed(5L)
  mu <- 2
  phi <- 1.2
  power <- 1.4
  out <- sim_family_rng(rep(log(mu), 2e4), tweedie(),
                        list(phi = phi, power = power))
  expect_equal(mean(out), mu, tolerance = 0.02)
  expect_equal(stats::var(out), phi * mu^power, tolerance = 0.05)
  # P(Y = 0) = exp(-lambda)
  lambda <- mu^(2 - power) / (phi * (2 - power))
  expect_equal(mean(out == 0), exp(-lambda), tolerance = 0.05)
})


test_that("trend_model accepts any supported constructor", {
  for (tm in list(RW(), AR(p = 1L), AR(p = c(1L, 3L, 12L)),
                  AR(p = 2L, ma = TRUE))) {
    out <- sim_mvgam(type = 1L, trend_model = tm, n_timepoints = 60L,
                     seed = 7L)
    expect_true(all(is.finite(out$true_trend)))
    expect_gt(out$true_trend_sigma, 0)
  }
  none <- sim_mvgam(type = 1L, trend_model = "None",
                    n_timepoints = 50L, seed = 7L)
  expect_true(all(none$true_trend == 0))
  # A trend given by name is refused before any series is drawn
  expect_error(sim_mvgam(trend_model = "AR"), "trend_model")
})


test_that("sim_mvgam is reproducible under a seed", {
  a <- sim_mvgam(type = 2L, n_series = 2L, n_timepoints = 40L,
                 seed = 99L)
  b <- sim_mvgam(type = 2L, n_series = 2L, n_timepoints = 40L,
                 seed = 99L)
  expect_identical(a$data_train, b$data_train)
  expect_identical(a$true_trend, b$true_trend)
  expect_identical(a$true_smooths, b$true_smooths)
})


test_that("the split is by time and the missing values are in training", {
  out <- sim_mvgam(type = 1L, n_series = 2L, n_timepoints = 80L,
                   proportional_train = 0.75, prop_missing = 0.2,
                   seed = 7L)
  expect_identical(max(out$data_train$time),
                   min(out$data_test$time) - 1L)
  expect_identical(nrow(out$data_train) + nrow(out$data_test), 160L)
  expect_identical(sum(is.na(out$data_train$y)),
                   as.integer(floor(0.2 * nrow(out$data_train))))
  expect_false(anyNA(out$data_test$y))
  expect_null(sim_mvgam(type = 1L, n_timepoints = 40L,
                        proportional_train = 1, seed = 7L)$data_test)
})


test_that("true smooths are centred and match the data", {
  out <- sim_mvgam(type = 2L, n_timepoints = 60L, seed = 7L)
  for (nm in names(out$true_smooths)) {
    expect_lt(abs(mean(out$true_smooths[[nm]]$f_true)), 1e-6)
  }
  # With no trend and little noise the response follows the recorded
  # GP. A truth simulated separately at the grid would not.
  gp <- sim_mvgam(type = 4L, trend_model = "None", prop_trend = 0,
                  n_timepoints = 100L, family_pars = list(sigma = 0.05),
                  seed = 3L)
  truth <- gp$true_smooths$`gp(x)`
  f_at_x <- stats::approx(truth$x, truth$f_true, gp$data_train$x)$y
  expect_gt(stats::cor(f_at_x, gp$data_train$y), 0.95)
})


test_that("types 6 and 7 carry the dynamics their recipes name", {
  # Type 6 records irregular times and derives the season from them
  car_sim <- sim_mvgam(type = 6L, n_timepoints = 30L, seed = 3L)
  car_times <- sort(unique(car_sim$data_train$time))
  expect_true(all(diff(car_times) >= 1 & diff(car_times) <= 6))
  expect_false(all(abs(diff(car_times) - 1) < 1e-8))
  expect_equal(car_sim$data_train$season,
               (car_sim$data_train$time %% 12) + 1)

  # Type 7's sparse AR(1, 12) state persists at lags 1 and 12. A
  # 240-point series keeps the sample ACF stable across seeds.
  ar_sim <- sim_mvgam(type = 7L, n_timepoints = 240L, seed = 3L)
  ac <- stats::acf(ar_sim$true_trend[, 1L], plot = FALSE,
                   lag.max = 12L)$acf[, 1L, 1L]
  expect_gt(ac[2L], 0.2)
  expect_gt(ac[13L], 0.1)
})


test_that("summary names the simulated trend and print shows it", {
  label <- function(type, trend_model = NULL) {
    summary(sim_mvgam(type = type, n_timepoints = 30L, seed = 3L,
                      trend_model = trend_model))$trend
  }
  # NULL takes the type's default trend
  expect_identical(label(1L), "RW")
  expect_identical(label(6L), "CAR")
  expect_identical(label(1L, AR(p = 2)), "AR")
  expect_identical(label(1L, "None"), "None")
  sim <- sim_mvgam(type = 2L, n_series = 2L, n_timepoints = 25L,
                   seed = 5L)
  expect_output(print(sim), "Trend        : AR")
})
