context("dynamic")

skip_on_cran()

test_that("dynamic to gp spline is working properly", {
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 1,
                                                                        stationary = FALSE),
                                                  N = 100,
                                                  family = gaussian())), 'term.labels'),
               's(time, by = covariate, bs = "gp", m = c(2, 1, 2), k = 50)',
               fixed = TRUE)

  # k will decrease as rho increases
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 11),
                                                  N = 100,
                                                  family = gaussian())), 'term.labels'),
               's(time, by = covariate, bs = "gp", m = c(-2, 11, 2), k = 11)',
               fixed = TRUE)

  # k will be fixed at N if N <= 8
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 5),
                                                  N = 7,
                                                  family = gaussian())), 'term.labels'),
               's(time, by = covariate, bs = "gp", m = c(-2, 5, 2), k = 7)',
               fixed = TRUE)
})

test_that("dynamic to gp Hilbert is working properly", {
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate),
                                                  N = 100,
                                                  family = gaussian())), 'term.labels'),
               'gp(time, by = covariate, c = 5/4, k = 40, scale = TRUE)',
               fixed = TRUE)

  # k should come across just fine
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, k = 17),
                                                  N = 100,
                                                  family = gaussian())), 'term.labels'),
               'gp(time, by = covariate, c = 5/4, k = 17, scale = TRUE)',
               fixed = TRUE)

  # k will be fixed at N-1 if N <= 8
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate),
                                                  N = 7,
                                                  family = gaussian())), 'term.labels'),
               'gp(time, by = covariate, c = 5/4, k = 6, scale = TRUE)',
               fixed = TRUE)
})

test_that("rho argument must be positive numeric", {
  data = data.frame(out = rnorm(100),
                    temp = rnorm(100),
                    time = 1:100)
  expect_error(mod <- mvgam(formula = out ~ dynamic(temp, rho = -1),
                            data = data,
                            family = gaussian(),
                            run_model = FALSE),
               'Argument "rho" in dynamic() must be a positive value',
               fixed = TRUE)
})

test_that("rho argument cannot be larger than N - 1", {
  data = data.frame(out = rnorm(100),
                    temp = rnorm(100),
                    time = 1:100)
  expect_error(mod <- mvgam(formula = out ~ dynamic(temp, rho = 110),
                            data = data,
                            family = gaussian(),
                            run_model = FALSE),
               'Argument "rho" in dynamic() cannot be larger than (max(time) - 1)',
               fixed = TRUE)

  expect_error(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 120),
                                       N = 100,
                                       family = gaussian()),
               'Argument "rho" in dynamic() cannot be larger than (max(time) - 1)',
               fixed = TRUE)
})

test_that("dynamic to spline works for trend_formulas", {
  beta_data$data_train$random <- rnorm(NROW(beta_data$data_train))
  mod <- mvgam(y ~ dynamic(random, rho = 5),
               trend_formula = ~ dynamic(random, rho = 15),
               trend_model = 'RW',
               data = beta_data$data_train,
               family = betar(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))

  # trend_idx should be in the model file and in the model data
  expect_true(any(grepl('trend_idx', mod$model_file)))
  expect_true(!is.null(mod$model_data$trend_idx1))
})

test_that("dynamic to Hilbert works for trend_formulas", {
  beta_data$data_train$random <- rnorm(NROW(beta_data$data_train))
  mod <- mvgam(y ~ dynamic(random),
               trend_formula = ~ dynamic(random, k = 22),
               trend_model = 'RW',
               data = beta_data$data_train,
               family = betar(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(code(mod))

  # Model file should have prior lines for observationgp terms
  expect_true(any(grepl('// prior for gp(time):random...',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl("b[b_idx_gp_time_byrandom] = sqrt(spd_cov_exp_quad(",
                        mod$model_file, fixed = TRUE)))

  # Model file should have prior lines for trend gp terms
  expect_true(any(grepl('// prior for gp(time):random_trend...',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl("b_trend[b_trend_idx_gp_time_byrandom] = sqrt(spd_cov_exp_quad(",
                        mod$model_file, fixed = TRUE)))

  # Observation-level Gp data structures should be in the model_data
  expect_true("l_gp_time_byrandom" %in% names(mod$model_data))
  expect_true("b_idx_gp_time_byrandom" %in% names(mod$model_data))
  expect_true("k_gp_time_byrandom" %in% names(mod$model_data))

  # Trend-level Gp data structures should be in the model_data
  expect_true("l_gp_trend_time_byrandom" %in% names(mod$model_data))
  expect_true("b_trend_idx_gp_time_byrandom" %in% names(mod$model_data))
  expect_true("k_gp_trend_time_byrandom" %in% names(mod$model_data))
})

