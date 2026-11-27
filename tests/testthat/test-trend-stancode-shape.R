

# ----- VAR sparse-lag p (refused) -------------------------------

test_that("VAR(p = c(2, 4)) errors with an informative message", {
  # The Heaps (2023) stationary initialisation is defined on the
  # companion form of consecutive lags. A sparse lag set has no
  # companion form with the same stationary covariance. The
  # constructor refuses a vector 'p' and points to AR(p = c(...)).
  expect_error(
    VAR(p = c(2, 4)),
    "Sparse-lag VAR"
  )
})


test_that("assert_trend_df() rejects values with no finite variance", {
  # The AR stationary initialisation divides by sqrt(1 - phi^2), which
  # presumes a finite second moment; a t has one only above 2.
  expect_error(AR(df = 2), "greater than 2")
  expect_error(AR(df = 1.5), "greater than 2")
  expect_error(AR(df = -Inf), "positive")
  expect_error(AR(df = "a"), "single number")
  expect_error(AR(df = c(3, 4)), "single number")
  expect_identical(AR(df = Inf)$df, Inf)
  expect_identical(AR(df = NA)$df, NA_real_)
  expect_identical(AR(df = 7)$df, 7)
})

test_that("trend types without innovations do not accept df", {
  # VAR samples its states directly. A piecewise trend is
  # deterministic. Neither has innovations to make heavy-tailed.
  expect_error(VAR(df = 5), "unused argument")
  expect_error(PW(df = 5), "unused argument")
})

test_that("draw_trend_innovations() matches the Stan innovation distribution", {
  set.seed(9)
  g <- mvgam:::draw_trend_innovations(20000L, 3L, Inf)
  expect_equal(var(g[, 1]), 1, tolerance = 0.05)

  t5 <- mvgam:::draw_trend_innovations(50000L, 3L, 5)
  # variance of a standard t is df / (df - 2)
  expect_equal(var(t5[, 1]), 5 / 3, tolerance = 0.15)

  # Series are uncorrelated but share the mixing value. A shock then
  # reaches all of them at once. Element-wise student_t() would leave
  # the absolute values independent.
  expect_equal(cor(t5[, 1], t5[, 2]), 0, tolerance = 0.05)
  expect_true(cor(abs(t5[, 1]), abs(t5[, 2])) > 0.1)
  expect_true(cor(abs(g[, 1]), abs(g[, 2])) < 0.05)
})

test_that("draw_trend_innovations() validates its arguments", {
  expect_error(mvgam:::draw_trend_innovations(10L, 2L, 2), "df")
  expect_error(mvgam:::draw_trend_innovations(10L, 0L, Inf), "n_series")
})


# ----- writing a prior statement ----------------------------------

test_that("stan_prior_statement writes both spellings", {
  # Stan drops the normalising constant from the tilde form. A
  # normalised program spells the density call out.
  expect_identical(
    stan_prior_statement("sigma_trend", "exponential(2)"),
    "target += exponential_lpdf(sigma_trend | 2);"
  )
  expect_identical(
    stan_prior_statement("sigma_trend", "exponential(2)",
                           normalize = FALSE),
    "sigma_trend ~ exponential(2);"
  )
})


test_that("stan_prior_statement handles the awkward shapes", {
  # A parameter-free density takes no bar.
  expect_identical(
    stan_prior_statement("to_vector(Z)", "std_normal()"),
    "target += std_normal_lpdf(to_vector(Z));"
  )
  # Arguments may nest parentheses.
  expect_identical(
    stan_prior_statement("Amu_trend[lag]", "normal(0, sqrt(0.455))"),
    "target += normal_lpdf(Amu_trend[lag] | 0, sqrt(0.455));"
  )
  # A slice passes through verbatim. So does a transpose.
  expect_identical(
    stan_prior_statement("varrho_inv[2:N_lv_trend]",
                           "inv_gamma(mgp_a2, 1)"),
    "target += inv_gamma_lpdf(varrho_inv[2:N_lv_trend] | mgp_a2, 1);"
  )
  expect_identical(
    stan_prior_statement("lv_trend[t, :]'", "multi_normal(mu, Sigma)"),
    "target += multi_normal_lpdf(lv_trend[t, :]' | mu, Sigma);"
  )
})


test_that("stan_prior_statement refuses a distribution it cannot parse", {
  # A resolver returning nothing would otherwise reach Stan as a
  # statement it cannot compile, with the parameter unnamed.
  expect_error(stan_prior_statement("sigma_trend", ""),
                 "at least 1 characters")
  expect_error(stan_prior_statement("sigma_trend", "exponential"),
               "must be a Stan distribution call")
  expect_error(stan_prior_statement("sigma_trend", "normal(0, 1"),
               "must be a Stan distribution call")
})


test_that("the normaliser leaves comments and brms lines alone", {
  prog <- paste(
    "model {",
    "  // bf(y ~ s(x)) names a formula inside a comment",
    "  sigma_trend ~ exponential(2);",
    "  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);",
    "  target += lprior;",
    "}", sep = "\n"
  )
  out <- normalise_mvgam_sampling_statements(prog, normalize = TRUE)
  expect_true(grepl("// bf(y ~ s(x))", out, fixed = TRUE))
  expect_true(grepl("lprior += student_t_lpdf(Intercept | 3, 0, 2.5);",
                      out, fixed = TRUE))
  expect_true(grepl("target += exponential_lpdf(sigma_trend | 2);",
                      out, fixed = TRUE))
  # Nothing to do when the program keeps the tilde form.
  expect_identical(
    normalise_mvgam_sampling_statements(prog, normalize = FALSE), prog
  )
})


test_that("a wrapped statement inside a loop is rewritten in place", {
  prog <- paste(
    "model {",
    "  for (lag in 1 : 2) {",
    "    diagonal(A_trend[lag]) ~ normal(Amu_trend[1, lag],",
    "                                        1 / sqrt(Ao[1, lag]));",
    "  }",
    "}", sep = "\n"
  )
  out <- normalise_mvgam_sampling_statements(prog, normalize = TRUE)
  ln <- strsplit(out, "\n", fixed = TRUE)[[1L]]
  stmt <- grep("normal_lpdf", ln)
  expect_length(stmt, 1L)
  # It stays between the loop's braces.
  expect_true(stmt > grep("for \\(lag", ln))
  expect_true(stmt < which(trimws(ln) == "}")[1L])
})
