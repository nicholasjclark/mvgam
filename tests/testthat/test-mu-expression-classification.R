
# GLM detection and trend injection ----

stan_program <- function(...) paste(c(...), collapse = "\n")

test_that("the trend joins a built predictor after its last term", {
  code <- stan_program(
    "model {",
    "  if (!prior_only) {",
    "    vector[N] mu = rep_vector(0.0, N);",
    "    mu += Intercept + Xc * b;",
    "    target += normal_lpdf(Y | mu, sigma);",
    "  }",
    "}"
  )
  expect_identical(
    mvgam:::inject_trend_into_linear_predictors(code, ""),
    stan_program(
      "model {",
      "  if (!prior_only) {",
      "    vector[N] mu = rep_vector(0.0, N);",
      "    mu += Intercept + Xc * b;",
      "    mu += to_vector(trend)[obs_trend_idx];",
      "    target += normal_lpdf(Y | mu, sigma);",
      "  }",
      "}"
    )
  )
})

test_that("the trend joins before an inverse link inside a loop", {
  # brms applies some inverse links row by row inside the likelihood
  # loop. The trend enters on the link scale, before that loop opens.
  code <- stan_program(
    "model {",
    "  vector[N_y] mu_y = rep_vector(0.0, N_y);",
    "  mu_y += Intercept_y;",
    "  for (n in 1 : N_y) {",
    "    mu_y[n] = inv_logit(mu_y[n]);",
    "    target += beta_lpdf(Y_y[n] | mu_y[n] * phi_y, phi_y);",
    "  }",
    "}"
  )
  expect_identical(
    mvgam:::inject_trend_into_linear_predictors(code, "y"),
    stan_program(
      "model {",
      "  vector[N_y] mu_y = rep_vector(0.0, N_y);",
      "  mu_y += Intercept_y;",
      "  mu_y += to_vector(trend)[obs_trend_idx_y];",
      "  for (n in 1 : N_y) {",
      "    mu_y[n] = inv_logit(mu_y[n]);",
      "    target += beta_lpdf(Y_y[n] | mu_y[n] * phi_y, phi_y);",
      "  }",
      "}"
    )
  )
})

test_that("a GLM likelihood is rewritten to take the trend", {
  code <- stan_program(
    "model {",
    "  target += poisson_log_glm_lpmf(Y | Xc, Intercept, b);",
    "}"
  )
  expect_identical(
    mvgam:::inject_trend_into_linear_predictors(code, ""),
    stan_program(
      "model {",
      paste0("  target += poisson_log_glm_lpmf(Y | Xc, Intercept + ",
             "to_vector(trend)[obs_trend_idx], b);"),
      "}"
    )
  )
})

test_that("an unplaceable trend is reported as an mvgam fault", {
  # A GLM family without argument roles would leave the trend computed and
  # never added to the predictor.
  unknown_glm <- stan_program(
    "model {",
    "  target += mystery_glm_lpmf(Y | Xc, Intercept, b);",
    "}"
  )
  expect_error(
    mvgam:::inject_trend_into_linear_predictors(unknown_glm, ""),
    "family could not be identified"
  )
  no_predictor <- stan_program(
    "model {",
    "  target += normal_lpdf(Y | 0, sigma);",
    "}"
  )
  expect_error(
    mvgam:::inject_trend_into_linear_predictors(no_predictor, ""),
    "lacks a statement building 'mu'"
  )
})
