context("dynamic")

test_that("dynamic to gp is working properly", {
expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 1,
                                                                      stationary = FALSE),
                                                N = 100)), 'term.labels'),
             's(time, by = covariate, bs = "gp", m = c(2, 1, 2), k = 50)',
             fixed = TRUE)

  # k will decrease as rho increases
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 11),
                                                  N = 100)), 'term.labels'),
               's(time, by = covariate, bs = "gp", m = c(-2, 11, 2), k = 11)',
               fixed = TRUE)

  # k will be fixed at N if N <= 8
  expect_match(attr(terms(mvgam:::interpret_mvgam(formula = y ~ dynamic(covariate, rho = 5),
                                                  N = 7)), 'term.labels'),
               's(time, by = covariate, bs = "gp", m = c(-2, 5, 2), k = 7)',
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
                                       N = 100),
               'Argument "rho" in dynamic() cannot be larger than (max(time) - 1)',
               fixed = TRUE)
})
