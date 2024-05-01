context("Tests for family functions")

test_that("distributions work correctly", {
  fam <- tweedie()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'log')

  fam <- student_t()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'identity')

  fam <- nmix()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'log')
})

test_that("family setups work correctly", {
  skip_on_cran()
  simdat <- sim_mvgam(family = poisson())
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = poisson(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = nb(),
                      n_lv = 2)
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = nb(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = lognormal(),
                      trend_model = VAR())
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = lognormal(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = bernoulli(),
                      trend_model = VAR(cor = TRUE))
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = bernoulli(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = student_t(),
                      trend_model = GP())
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = student_t(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = Gamma(),
                      seasonality = 'shared',
                      trend_model = AR(p = 3))
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = Gamma(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = betar(),
                      seasonality = 'hierarchical',
                      trend_model = AR(p = 2))
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = betar(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))
})
