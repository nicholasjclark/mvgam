context("RW,AR,VAR")

test_that("MA and cor options should work for trends other than VAR", {
  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = AR(p = 1, ma = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))

  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = AR(p = 1, cor = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
  expect_true(inherits(test, 'mvgam_prefit'))

  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = RW(ma = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)

  test <- mvgam(y ~ s(series, bs = 're') +
                       s(season, bs = 'cc') - 1,
                     trend_model = RW(cor = TRUE),
                     data = gaus_data$data_train,
                     family = gaussian(),
                     run_model = FALSE)
})

test_that("VARMAs are set up correctly", {
varma <- mvgam(y ~ s(series, bs = 're') +
                 s(season, bs = 'cc') - 1,
               trend_model = 'VARMA',
               data = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)

expect_true(any(grepl('// unconstrained ma inverse partial autocorrelations',
                      varma$model_file, fixed = TRUE)))

varma <- mvgam(y ~ s(series, bs = 're'),
               trend_formula = ~ s(season, bs = 'cc'),
               trend_model = VAR(ma = TRUE),
               data = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)

expect_true(any(grepl('// unconstrained ma inverse partial autocorrelations',
                      varma$model_file, fixed = TRUE)))
})
