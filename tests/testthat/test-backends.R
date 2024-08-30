context("backends")

skip_on_cran()

test_that("variationals are converted to stanfit appropriately", {
  # dat <- data.frame(y = rnorm(100, 0, 1),
  #                   series = as.factor('series1'))
  # # mod <- SM(mvgam(y ~ 1,
  # #              data = dat,
  # #              family = gaussian(),
  # #              algorithm = 'meanfield',
  # #              silent = 2))
  # # expect_true(inherits(mod, 'mvgam'))
  #
  # mod <- SM(mvgam(y ~ 1,
  #              data = dat,
  #              family = gaussian(),
  #              algorithm = 'fullrank',
  #              silent = 2))
  # expect_true(inherits(mod, 'mvgam'))
  #
  # mod <- SM(mvgam(y ~ 1,
  #              data = dat,
  #              family = gaussian(),
  #              algorithm = 'laplace',
  #              silent = 2))
  # expect_true(inherits(mod, 'mvgam'))
})
