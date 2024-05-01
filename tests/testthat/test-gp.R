context("gp")

skip_on_cran()

test_that("gp_to_s is working properly", {
  # All true gp() terms should be changed to s() with k = k+1
  formula <- y ~ s(series) + gp(banana) +
    infect:you + gp(hardcourt)

  expect_equal(attr(terms(mvgam:::gp_to_s(formula), keep.order = TRUE),
                    'term.labels'),
               attr(terms(formula(y ~ s(series) +
                                    s(banana, k = 11) +
                                    infect:you +
                                    s(hardcourt, k = 11)),
                          keep.order = TRUE),
                    'term.labels'))

  # Characters that match to 'gp' should not be changed
  formula <- y ~ gp(starwars) + s(gp)
  expect_equal(attr(terms(mvgam:::gp_to_s(formula), keep.order = TRUE),
                    'term.labels'),
               attr(terms(formula(y ~ s(starwars, k = 11) + s(gp)),
                          keep.order = TRUE),
                    'term.labels'))
})

test_that("gp for observation models working properly", {
  mod <- mvgam(y ~ s(series, bs = 're') +
                 gp(time, by = series) +
                 year:season,
               data = gaus_data$data_train,
               family = gaussian(),
               run_model = FALSE)

  # Gp data structures should be in the model_data
  expect_true("l_gp_time_byseriesseries_1" %in% names(mod$model_data))
  expect_true("b_idx_gp_time_byseriesseries_1" %in% names(mod$model_data))
  expect_true("k_gp_time_byseriesseries_1" %in% names(mod$model_data))

  # These should match to the eigenvalues and eigenfunctions created by
  # a similar brms call
  brms_dat <- suppressWarnings(brms::make_standata(y ~ s(series, bs = 're') +
                                                     gp(time, by = series, k = 10,
                                                        c = 5/4) +
                                                     year:season,
                                                   data = gaus_data$data_train,
                                                   family = gaussian()))
  # Eigenvalues should be identical
  all.equal(as.vector(brms_dat$slambda_1_1),
            mod$model_data$l_gp_time_byseriesseries_1)

  # Eigenfunctions will be nearly identical
  row_s1 <- which(gaus_data$data_train$series == 'series_1' &
                    !is.na(gaus_data$data_train$y))
  col_s1 <- grep('gp(time):seriesseries_1', names(coef(mod$mgcv_model)),
                 fixed = TRUE)

  expect_true(identical(dim(brms_dat$Xgp_1_1),
                        dim(mod$model_data$X[row_s1,col_s1])))

  expect_true(max(abs(brms_dat$Xgp_1_1 -
                        mod$model_data$X[row_s1,col_s1])) < 0.5)

  # The mgcv model formula should contain s() in place of gp()
  expect_equal(attr(terms(mod$mgcv_model$formula, keep.order = TRUE),
                    'term.labels'),
               attr(terms(formula(y ~ s(time, by = series, k = 11) +
                                    year:season +
                                    s(series, bs = "re")),
                          keep.order = TRUE),
                    'term.labels'))
})

test_that("gp for process models working properly", {
  mod <- mvgam(y ~ s(series, bs = 're'),
               trend_formula = ~
                 gp(time, by = trend) +
                 year:season,
               data = beta_data$data_train,
               family = betar(),
               trend_model = 'AR1',
               run_model = FALSE)

  # Model file should have prior lines for gp terms
  expect_true(any(grepl('// prior for gp(time):trendtrend1_trend...',
                        mod$model_file, fixed = TRUE)))

  expect_true(any(grepl("b_trend[b_trend_idx_gp_time_bytrendtrend1] = sqrt(spd_cov_exp_quad(",
                        mod$model_file, fixed = TRUE)))

  # Gp data structures should be in the model_data
  expect_true("l_gp_trend_time_bytrendtrend1" %in% names(mod$model_data))
  expect_true("b_trend_idx_gp_time_bytrendtrend1" %in% names(mod$model_data))
  expect_true("k_gp_trend_time_bytrendtrend1" %in% names(mod$model_data))

  # These should match to the eigenvalues and eigenfunctions created by
  # a similar brms call
  brms_dat <- suppressWarnings(brms::make_standata(y ~ gp(time, by = series, k = 10,
                                                        c = 5/4),
                                                   data = beta_data$data_train,
                                                   family = gaussian()))
  # Eigenvalues should be identical
  expect_true(all.equal(as.vector(brms_dat$slambda_1_1),
            mod$model_data$l_gp_trend_time_bytrendtrend1))

  # Eigenfunctions will be nearly identical
  row_s1 <- mod$model_data$ytimes_trend[,1]
  col_s1 <- grep('gp(time):trendtrend1', names(coef(mod$trend_mgcv_model)),
                 fixed = TRUE)

  expect_true(identical(dim(brms_dat$Xgp_1_1),
                        dim(mod$model_data$X_trend[row_s1,col_s1])))

  expect_true(max(abs(brms_dat$Xgp_1_1 -
                        mod$model_data$X_trend[row_s1,col_s1])) < 0.01)

  # The mgcv model formula should contain s() in place of gp()
  expect_equal(attr(terms(mod$trend_mgcv_model$formula, keep.order = TRUE),
                    'term.labels'),
               attr(terms(formula(y ~ s(time, by = series, k = 11) +
                                    year:season),
                          keep.order = TRUE),
                    'term.labels'))
})
