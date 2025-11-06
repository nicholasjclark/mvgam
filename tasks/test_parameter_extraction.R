# Deep Tests for Parameter Extraction Functions
# Uses pre-saved fixtures from tasks/fixtures/
# Tests for sub-task 2.2.5

devtools::load_all()
library(testthat)

test_model_parameters <- function(fit) {
  all_vars <- variables(fit)
  obs_pars <- extract_obs_parameters(fit)
  trend_pars <- extract_trend_parameters(fit)
  excluded <- fit$exclude
  if (is.null(excluded)) excluded <- character(0)

  expect_true(is.character(obs_pars))
  expect_true(is.character(trend_pars))

  overlap <- intersect(obs_pars, trend_pars)
  expect_equal(length(overlap), 0)

  expect_equal(length(obs_pars), length(unique(obs_pars)))
  expect_equal(length(trend_pars), length(unique(trend_pars)))

  expect_true(all(obs_pars %in% all_vars))
  expect_true(all(trend_pars %in% all_vars))

  combined <- c(obs_pars, trend_pars, excluded)
  computed_states <- all_vars[grepl(
    "^(trend|lv_trend|innovations_trend|scaled_innovations_trend|mu_trend)\\[",
    all_vars
  )]
  generated_quantities <- all_vars[grepl(
    "^(P_var|result_var|P_ma|result_ma)\\[",
    all_vars
  )]
  intentionally_excluded <- c("b_Intercept_trend")
  accounted_vars <- c(combined, computed_states, generated_quantities,
    intentionally_excluded)
  unaccounted <- setdiff(all_vars, accounted_vars)
  expect_equal(length(unaccounted), 0)

  obs_with_trend <- obs_pars[grepl("_trend", obs_pars)]
  expect_equal(length(obs_with_trend), 0)

  # Most trend parameters have _trend suffix, except bridge parameters
  # Z and Z_raw map observations to latent trends (bridge parameters)
  bridge_params <- trend_pars[grepl("^(Z\\[|Z_raw\\[)", trend_pars)]
  trend_without_suffix <- trend_pars[
    !grepl("_trend", trend_pars) &
      !grepl("^(Z\\[|Z_raw\\[)", trend_pars)
  ]
  expect_equal(length(trend_without_suffix), 0)

  expect_false(any(grepl("^trend\\[", trend_pars)))
  expect_false(any(grepl("^lv_trend\\[", trend_pars)))
  expect_false(any(grepl("^innovations_trend\\[", trend_pars)))
  expect_false(any(grepl("^scaled_innovations_trend\\[", trend_pars)))
  expect_false(any(grepl("^mu_trend\\[", trend_pars)))

  invisible(list(obs = obs_pars, trend = trend_pars))
}

fit1 <- readRDS("tasks/fixtures/fit1.rds")
test_model_parameters(fit1)

fit2 <- readRDS("tasks/fixtures/fit2.rds")
test_model_parameters(fit2)

fit3 <- readRDS("tasks/fixtures/fit3.rds")
test_model_parameters(fit3)

fit4 <- readRDS("tasks/fixtures/fit4.rds")
test_model_parameters(fit4)

fit5 <- readRDS("tasks/fixtures/fit5.rds")
test_model_parameters(fit5)

fit6 <- readRDS("tasks/fixtures/fit6.rds")
test_model_parameters(fit6)

expect_error(
  extract_obs_parameters("not_an_mvgam"),
  "Assertion on 'mvgam_fit' failed"
)

expect_error(
  extract_trend_parameters(list(not = "mvgam")),
  "Assertion on 'mvgam_fit' failed"
)

expect_error(
  extract_obs_parameters(NULL),
  "Assertion on 'mvgam_fit' failed"
)

expect_error(
  extract_trend_parameters(123),
  "Assertion on 'mvgam_fit' failed"
)
