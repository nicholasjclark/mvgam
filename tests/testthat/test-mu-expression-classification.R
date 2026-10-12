test_that("mu statements come with the variables they use", {
  set.seed(123)
  d <- data.frame(Y = rnorm(60), x = runif(60, 0, 10),
                  mono = sample(1:5, 60, TRUE), g = sample(1:6, 60, TRUE))
  mu_of <- function(f) {
    mvgam:::extract_mu_construction(
      as.character(brms::stancode(f, data = d, family = gaussian()))
    )
  }

  # A gaussian GLM takes `Xc * b` as arguments. The statement it never
  # wrote joins the others. The call `mo()` stays out of the variables.
  glm <- mu_of(Y ~ x + mo(mono) + (1 | g))
  expect_identical(utils::tail(glm$mu_construction, 1L), "mu += Xc * b;")
  expect_identical(
    glm$referenced_variables,
    c("N", "mu", "Intercept", "bsp", "simo_1", "Xmo_1", "r_1_1", "J_1",
      "Z_1_1", "Xc", "b")
  )

  # An approximate GP's prediction follows the basis it is built from
  gp <- mu_of(Y ~ gp(x, c = 5 / 4, k = 20))
  expect_identical(
    sub("^vector\\[[^]]*\\] (\\w+) =.*$", "\\1",
        gp$supporting_declarations),
    c("mu", "rgp_1", "gp_pred_1")
  )

  no_mu <- mvgam:::extract_mu_construction(
    "model { target += normal_lpdf(y | 0, 1); }"
  )
  expect_length(no_mu$mu_construction, 0L)
})

# GLM detection and trend injection ----

test_that("glm_calls_present names the GLM likelihoods a program calls", {
  glm <- "model {\n  target += poisson_log_glm_lpmf(Y | X, alpha, beta);\n}"
  plain <- "model {\n  target += normal_lpdf(Y | mu, sigma);\n}"
  present <- mvgam:::glm_calls_present(glm)
  expect_identical(names(present)[present], "poisson_log_glm")
  expect_false(any(mvgam:::glm_calls_present(plain)))
})

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
      "  vector[N] mu = Xc * b;",
      "  mu += Intercept + to_vector(trend)[obs_trend_idx];",
      "  target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);",
      "}"
    )
  )
})

test_that("an unplaceable trend is reported as an mvgam fault", {
  # A GLM family without a layout would leave the trend computed and
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
