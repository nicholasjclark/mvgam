context("update.mvgam")

skip_on_cran()

test_that("update() working correctly", {
  # Can update trend_model
  mod <- update(mvgam:::mvgam_example1,
                trend_model = AR(p = 2),
                control = list(max_treedepth = 11),
                run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(attr(mod$model_data, 'trend_model') == 'AR2')
  expect_true(any(grepl('ar2~std_normal();',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(mod$call == mvgam:::mvgam_example1$call)

  # Update trend_model and formula
  mod <- update(mvgam:::mvgam_example1,
                formula = y ~ s(season, k = 6) - 1,
                trend_model = PW(),
                run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_true(attr(mod$model_data, 'trend_model') == 'PWlinear')
  expect_true(any(grepl('to_vector(delta_trend)~double_exponential(0,changepoint_scale);',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
  expect_true(mod$call != mvgam:::mvgam_example1$call)
  expect_true(rlang::f_rhs(mod$call) == 's(season, k = 6) - 1')

  # Errors should pass from mvgam()
  expect_error(update(mvgam:::mvgam_example1,
                      formula = y ~ s(season, k = 6) - 1,
                      trend_model = PW(growth = 'logistic'),
                      run_model = FALSE),
               'Capacities must be supplied as a variable named "cap" for logistic growth')

  # Update to include shared observation params
  mod <- update(mvgam:::mvgam_example1,
                share_obs_params = TRUE,
                run_model = FALSE)
  expect_true(any(grepl('real<lower=0>sigma_obs;',
                        gsub(' ', '', mod$model_file), fixed = TRUE)))
})

test_that("update() passes original knots correctly", {
  # Simulate some data and fit a Poisson AR1 model
  simdat <- sim_mvgam(n_series = 1, trend_model = AR())
  mod <- SM(mvgam(y ~ s(season, bs = 'cc'),
               knots = list(season = c(0.5, 12.5)),
               trend_model = AR(),
               noncentred = TRUE,
               data = simdat$data_train,
               chains = 2,
               silent = 2))
  expect_true(identical(attr(mod$mgcv_model, 'knots'),
                        list(season = c(0.5, 12.5))))

  # Update to an AR2 model
  updated_mod <- update(mod,
                        trend_model = AR(p = 2),
                        noncentred = TRUE,
                        run_model = FALSE)
  expect_true(identical(attr(updated_mod$mgcv_model, 'knots'),
                        list(season = c(0.5, 12.5))))
})
