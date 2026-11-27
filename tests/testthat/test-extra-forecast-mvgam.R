# The tests of test-forecast-mvgam.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


# ----- Class shape + type dispatch --------------------------------

test_that("Returns mvgam_forecast with 12 contractual fields", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  newdata <- data.frame(time = 11:12,
                         series = factor("s1", levels = "s1"),
                         y = NA_integer_)

  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    ndraws = function(...) nrow(draws),
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      matrix(0.1, nrow = 3L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(...) {
      matrix(c(0.5, 0.6), nrow = 2L, ncol = 1L)
    },
    extract_last_states = function(...) list(),
    last_state_of_draw = function(...) {
      list(
        params = list(sigma = 0.1, ar = matrix(0.5, 1L, 1L)),
        last_state = list(
          trends = matrix(0, 1L, 1L),
          errors = NULL
        )
      )
    },
    posterior_predict = function(...) {
      matrix(1L, nrow = 3L, ncol = 10L)
    },
    posterior_epred = function(...) {
      matrix(1, nrow = 3L, ncol = 10L)
    }
  )

  # `type = "expected"` avoids the family sampling path, which
  # would require a real stanfit object for dpar extraction; the
  # class-shape contract is the same for every type.
  fc <- forecast(fit, newdata = newdata, type = "expected")
  required <- c("family", "family_pars", "type", "series_names",
                "train_observations", "train_times",
                "test_observations", "test_times",
                "hindcasts", "forecasts", "site_units", "component")
  expect_equal(sort(names(fc)), sort(required))
  # Held-out data stored by `mvgam(newdata = )` drive the horizon
  # when `forecast()` is given none.
  fit$test_data <- newdata
  expect_equal(forecast(fit, type = "expected"), fc)
})


test_that("the step check names the grid, not an observation", {
  # `assert_forecast_times_steppable()` reads the training *grid*,
  # and its own comment says why: the latent state runs on one grid
  # shared by every series, and reading each series' observed times
  # instead would refuse a legitimate forecast on a frame padded with
  # unobserved rows, which is the shape mvgam asks users to supply.
  #
  # The horizon rule answers a different question from the axis
  # record, which counts an occasion observed only where a response
  # was seen. On a padded frame the two part company over the
  # occasions between the last response and the end of the grid, so
  # the refusal must not call the grid's last position an
  # observation: it named a time the series was never seen at and
  # then offered a horizon starting past it.
  set.seed(12L)
  d <- data.frame(time = 1:40, series = factor("s1"),
                  y = c(rnorm(30), rep(NA, 10)))
  # The padding rows leave the likelihood, and brms says so.
  expect_warning(
    prefit <- mvgam(y ~ 1, trend_formula = ~ AR(p = 1), data = d,
                    family = gaussian(), run_model = FALSE, silent = 2,
                    validate = FALSE),
    "Rows containing NAs were excluded"
  )
  err <- conditionMessage(expect_error(
    mvgam:::assert_forecast_times_steppable(list(s1 = 31), prefit)
  ))
  expect_match(err, "training grid runs to time 40", fixed = TRUE)
  # The word that was wrong. The series was last seen at 30 here, so
  # nothing may claim it was observed at 40.
  expect_false(grepl("observed to time", err, fixed = TRUE))
  # And the reader is pointed at the surface that does read an
  # occasion sitting inside the grid.
  expect_match(err, "hindcast()", fixed = TRUE)

  # The control: a frame that does continue the grid is accepted, so
  # this is not a check that refuses everything.
  expect_true(mvgam:::assert_forecast_times_steppable(list(s1 = 41),
                                                      prefit))
  # A gap in the horizon is named as the time skipped
  expect_error(
    mvgam:::assert_forecast_times_steppable(list(s1 = c(41, 43)), prefit),
    "Series 's1' skips time 42."
  )
})


test_that("a wide frame padded at the end is stepped from its grid", {
  # The grid was gathered from each response's training rows. Where
  # the responses are the series, those hold its observed rows alone,
  # and the grid seemed to end at the last observation. Times inside
  # the grid then passed as a continuation, and the trend stepped on
  # from the grid's true end under them.
  set.seed(13L)
  d <- data.frame(time = 1:12,
                  y1 = c(rnorm(10), NA, NA), y2 = c(rnorm(10), NA, NA))
  expect_warning(
    prefit <- mvgam(
      brms::bf(brms::mvbind(y1, y2) ~ 1) + brms::set_rescor(FALSE),
      trend_formula = ~ AR(p = 1), data = d, family = gaussian(),
      run_model = FALSE, silent = 2, validate = FALSE
    ),
    "Rows containing NAs were excluded"
  )
  series_levels <- mvgam:::fitted_series_levels(prefit)
  training <- mvgam:::build_training_arms(prefit, series_levels)
  expect_identical(max(unlist(training$times)), 10L)
  fc <- mvgam:::resolve_forecast_grid(
    prefit, data.frame(time = 11:12), training, series_levels
  )
  expect_error(
    mvgam:::assert_forecast_times_steppable(fc$times, prefit),
    "training grid runs to time 12"
  )
  fc_past <- mvgam:::resolve_forecast_grid(
    prefit, data.frame(time = 13:14), training, series_levels
  )
  expect_true(mvgam:::assert_forecast_times_steppable(fc_past$times,
                                                      prefit))
  # The tail that centres the stored state is the end of the same
  # grid. Taken from the training rows it ended at the last
  # observation.
  tail_rows <- mvgam:::build_training_tail_data(prefit, training, 2L)
  expect_equal(tail_rows$time, c(11, 12))
})


test_that("a component forecast carries its own path and the trend's mean", {
  # The fitted trend is 3 and its stochastic level 1.8. The trend
  # formula's predictor, 1.5 over the horizon, adds to the propagated
  # level of 2. The slope adds nothing to the trend and takes none.
  fit <- make_mock_mvgam(trend_type = "LLT")
  n_time <- 10L
  draws <- posterior::as_draws_matrix(matrix(
    rep(c(rep(3, n_time), rep(1.8, n_time), rep(0.25, n_time)),
        each = 3L),
    nrow = 3L,
    dimnames = list(NULL, c(sprintf("trend[%d,1]", seq_len(n_time)),
                            sprintf("lv_trend[%d,1]", seq_len(n_time)),
                            sprintf("slope_trend[%d,1]", seq_len(n_time))))
  ))
  newdata <- mock_future_data(fit, h = 2L)

  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    ndraws = function(...) nrow(draws),
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                         component, ...) {
      value <- if (identical(component, "trend")) 1.5 else 0
      matrix(value, nrow = 3L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(trend_model, params, h, n_series, ...) {
      structure(matrix(2, nrow = h, ncol = n_series),
                components = list(level = matrix(2, h, n_series),
                                  slope = matrix(0.25, h, n_series)))
    },
    extract_last_states = function(...) list(),
    last_state_of_draw = function(...) {
      list(params = list(sigma = 0.1),
           last_state = list(trends = matrix(0, 1L, 1L), errors = NULL))
    }
  )

  fc <- function(component) {
    forecast(fit, newdata = newdata, type = "trend", component = component)
  }
  level <- fc("level")
  expect_equal(unname(as.numeric(level$forecasts$s1)), rep(2, 6L))
  expect_equal(unname(as.numeric(level$hindcasts$s1)), rep(1.8, 30L))
  predictor <- fc("predictor")
  expect_equal(unname(as.numeric(predictor$forecasts$s1)), rep(1.5, 6L))
  expect_equal(unname(as.numeric(predictor$hindcasts$s1)), rep(1.2, 30L))
  both <- fc(c("level", "predictor"))
  expect_equal(unname(as.numeric(both$forecasts$s1)), rep(3.5, 6L))
  expect_identical(plot(both)$labels$y, "Latent level + predictor")
  slope <- forecast(fit, newdata = newdata, type = "trend",
                    component = "slope")
  expect_equal(unname(as.numeric(slope$forecasts$s1)), rep(0.25, 6L))
  expect_equal(unname(as.numeric(slope$hindcasts$s1)), rep(0.25, 30L))

  # The record names the component on every surface
  expect_identical(slope$component, "slope")
  expect_identical(unique(summary(slope)$component), "slope")
  expect_true(any(grepl("component 'slope'", capture.output(print(slope)),
                        fixed = TRUE)))
  expect_identical(plot(slope)$labels$y, "Latent slope")
})
