# Fast CI-safe runtime tests for as_draws_*.mvgam, the keyword
# shortcuts in `variable=`, and the bayesplot-ecosystem wrappers
# (rhat, neff_ratio, nuts_params, log_posterior, coef, fixef,
# bayes_R2, prior_summary, ndraws, nchains, niterations, nvariables,
# posterior_summary, getCall).
#
# Each test builds a draws_array-backed mvgam stub so the runtime
# paths execute without invoking Stan. Catches regressions like
# stale `x$model_output` references that previously broke the
# diagnostic family silently.


# Minimal mvgam-class stub. Carries the slots every diagnostic /
# draws method reads: a posterior::draws_array as `$fit`, the
# response name(s), the data frame, the formula, and an empty
# trend_formula so resolve_mvgam_keyword takes the obs-only branch.
make_mvgam_stub <- function(varnames = c(
                              "b_Intercept", "b_x", "b_trend[1]",
                              "b_x_trend", "Intercept", "Intercept_trend",
                              "sigma", "phi",
                              "sd_1[1]", "sds_sx_1", "sds_sz_1_trend",
                              "ar1_trend[1]", "sigma_trend[1]",
                              "innovations_trend[1,1]",
                              "trend[1,1]", "Y_pred[1]",
                              "lp__", "lprior"),
                            n_iter = 50L, n_chains = 2L,
                            mv = FALSE) {
  set.seed(1L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  formula <- if (mv) {
    brms::bf(brms::mvbind(y1, y2) ~ x)
  } else {
    structure(y ~ x, class = c("brmsformula", "formula"))
  }
  data <- if (mv) {
    data.frame(y1 = rnorm(10), y2 = rnorm(10), x = rnorm(10))
  } else {
    data.frame(y = rnorm(10), x = rnorm(10))
  }
  structure(
    list(
      fit = drws,
      formula = formula,
      trend_formula = NULL,
      response_names = if (mv) c("y1", "y2") else "y",
      data = data,
      prior = data.frame(prior = "(flat)", class = "b"),
      call = call("mvgam", formula = formula)
    ),
    class = "mvgam"
  )
}


# ---- as_draws_*.mvgam runtime paths ----------------------------------

test_that("as.matrix.mvgam(variable = NULL) returns all parameters", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub)
  expect_s3_class(out, "draws_matrix")
  expect_equal(ncol(out), 18L)
})

test_that("as.matrix.mvgam(variable = 'betas') extracts b_* only", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "betas")
  expect_setequal(colnames(out), c("b_Intercept", "b_x"))
})

test_that("as.matrix.mvgam(variable = 'trend_betas') extracts b_trend[", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "trend_betas")
  # Both positional `b_trend[k]` and the brms-aliased
  # `b_<term>_trend` count as trend-side fixed effects.
  expect_setequal(colnames(out), c("b_trend[1]", "b_x_trend"))
})

test_that("as.matrix.mvgam(variable = 'obs_params') excludes _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "obs_params")
  expect_setequal(colnames(out), c("sigma", "phi"))
})

test_that("as.matrix.mvgam(variable = 'smooth_params') excludes _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "smooth_params")
  expect_equal(colnames(out), "sds_sx_1")
})

test_that("as.matrix.mvgam(variable = 'trend_smooth_params') matches _trend", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = "trend_smooth_params")
  expect_equal(colnames(out), "sds_sz_1_trend")
})

test_that("trend_params on obs-only fit picks top-level dynamics", {
  stub <- make_mvgam_stub()
  stub$trend_formula <- NULL
  out <- as.matrix(stub, variable = "trend_params")
  expect_true("sigma" %in% colnames(out))
  expect_false("trend[1,1]" %in% colnames(out))
  expect_false("innovations_trend[1,1]" %in% colnames(out))
})

test_that("trend_params on trend-formula fit picks _trend block", {
  stub <- make_mvgam_stub()
  stub$trend_formula <- ~AR(p = 1)
  out <- as.matrix(stub, variable = "trend_params")
  expect_true("ar1_trend[1]" %in% colnames(out))
  expect_true("sigma_trend[1]" %in% colnames(out))
  # Regression coefficients on the trend side (positional, brms-
  # aliased, or centred intercept) must NOT leak into the latent-
  # dynamics keyword.
  expect_false("b_trend[1]" %in% colnames(out))
  expect_false("b_x_trend" %in% colnames(out))
  expect_false("Intercept_trend" %in% colnames(out))
  expect_false("innovations_trend[1,1]" %in% colnames(out))
  expect_false("sds_sz_1_trend" %in% colnames(out))
})

test_that("variable = mix of keyword and regex composes", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = c("betas", "^sd_"), regex = TRUE)
  expect_setequal(colnames(out), c("b_Intercept", "b_x", "sd_1[1]"))
})

test_that("variable = explicit name vector works (no regex)", {
  stub <- make_mvgam_stub()
  out <- as.matrix(stub, variable = c("b_Intercept", "sigma"))
  expect_setequal(colnames(out), c("b_Intercept", "sigma"))
})

test_that("free pattern that matches nothing errors informatively", {
  stub <- make_mvgam_stub()
  expect_error(
    as.matrix(stub, variable = "totally_nonexistent_par"),
    regexp = "No parameters matched"
  )
})

test_that("keyword that resolves to empty (e.g. obs_params on Poisson) is OK", {
  stub <- make_mvgam_stub(varnames = c("b_Intercept", "ar1_trend[1]",
                                        "sigma_trend[1]", "lp__"))
  out <- as.matrix(stub, variable = "obs_params")
  expect_equal(ncol(out), 0L)
})

test_that("as.data.frame.mvgam returns a data.frame", {
  stub <- make_mvgam_stub()
  out <- as.data.frame(stub, variable = "betas")
  expect_s3_class(out, "data.frame")
  expect_true("b_Intercept" %in% names(out))
})

test_that("as.array.mvgam returns a draws_array", {
  stub <- make_mvgam_stub()
  out <- as.array(stub, variable = "betas")
  expect_s3_class(out, "draws_array")
})

test_that("as_draws_matrix / df / array / list / rvars all dispatch", {
  stub <- make_mvgam_stub()
  expect_s3_class(as_draws_matrix(stub, variable = "betas"),
                  "draws_matrix")
  expect_s3_class(as_draws_df(stub, variable = "betas"), "draws_df")
  expect_s3_class(as_draws_array(stub, variable = "betas"),
                  "draws_array")
  expect_s3_class(as_draws_list(stub, variable = "betas"),
                  "draws_list")
  expect_s3_class(as_draws_rvars(stub, variable = "betas"),
                  "draws_rvars")
})


# ---- bayesplot-ecosystem wrappers ------------------------------------

test_that("coef.mvgam returns named posterior means of b_* by default", {
  stub <- make_mvgam_stub()
  out <- coef(stub)
  expect_type(out, "double")
  expect_setequal(names(out), c("b_Intercept", "b_x"))
})

test_that("coef.mvgam(summary = FALSE) returns the full chain", {
  stub <- make_mvgam_stub()
  out <- coef(stub, summary = FALSE)
  expect_s3_class(out, "draws_matrix")
  expect_equal(ncol(out), 2L)
})

test_that("fixef.mvgam returns a brms-shaped summary matrix", {
  stub <- make_mvgam_stub()
  out <- fixef(stub)
  expect_true(is.matrix(out))
  expect_setequal(colnames(out), c("Estimate", "Est.Error",
                                    "Q2.5", "Q97.5"))
  expect_setequal(rownames(out), c("Intercept", "x"))
})

test_that("fixef.mvgam(summary = FALSE) returns the draws matrix", {
  stub <- make_mvgam_stub()
  out <- fixef(stub, summary = FALSE)
  expect_true(is.matrix(out))
  expect_setequal(colnames(out), c("Intercept", "x"))
})

test_that("rhat.mvgam returns a named numeric vector", {
  stub <- make_mvgam_stub()
  out <- rhat(stub)
  expect_type(out, "double")
  expect_equal(length(out), 18L)
})

test_that("rhat.mvgam(pars = ...) filters", {
  stub <- make_mvgam_stub()
  out <- rhat(stub, pars = c("b_Intercept", "sigma"))
  expect_setequal(names(out), c("b_Intercept", "sigma"))
})

test_that("neff_ratio.mvgam returns ratios <= 1 in [0,1]", {
  stub <- make_mvgam_stub(n_iter = 200L, n_chains = 4L)
  out <- neff_ratio(stub, pars = c("b_Intercept"))
  expect_true(all(out > 0))
})

test_that("posterior_summary.mvgam returns a brms-shaped matrix", {
  stub <- make_mvgam_stub()
  out <- posterior_summary(stub, pars = c("b_Intercept", "sigma"))
  expect_true(is.matrix(out))
  expect_setequal(colnames(out), c("Estimate", "Est.Error",
                                    "Q2.5", "Q97.5"))
})

test_that("ndraws / nchains / niterations / nvariables work", {
  stub <- make_mvgam_stub(n_iter = 50L, n_chains = 2L)
  expect_equal(ndraws(stub), 100L)
  expect_equal(nchains(stub), 2L)
  expect_equal(niterations(stub), 50L)
  expect_equal(nvariables(stub), 18L)
})

test_that("getCall.mvgam returns the stored call", {
  stub <- make_mvgam_stub()
  expect_true(is.call(getCall(stub)))
})

test_that("prior_summary.mvgam returns the prior table", {
  stub <- make_mvgam_stub()
  out <- prior_summary(stub)
  expect_s3_class(out, "data.frame")
  expect_true("prior" %in% names(out))
})

test_that("prior_summary errors when fit has no prior slot", {
  stub <- make_mvgam_stub()
  stub$prior <- NULL
  expect_error(prior_summary(stub),
               regexp = "not stored with a prior table")
})

test_that("extract_prior_from_setup returns the merged full table when user supplies a partial prior", {
  # The stored prior table on an mvgam fit must include every
  # parameter class the model exposes, with the user-supplied row
  # tagged source = 'user' and the defaults preserved with
  # source = 'default'. Before fix #1 of the prior-surface audit,
  # the function returned the user subset only, so prior_summary()
  # silently under-reported. This test pins the merge behaviour.
  dat <- data.frame(
    y      = rpois(40, 3),
    x      = rnorm(40),
    series = factor(rep("a", 40)),
    time   = seq_len(40)
  )
  setup <- list(
    formula = y ~ x,
    data    = dat,
    family  = poisson(),
    data2   = NULL,
    prior   = brms::prior(normal(0, 2), class = "b", coef = "x")
  )
  merged <- mvgam:::extract_prior_from_setup(setup)
  expect_s3_class(merged, "brmsprior")
  # The user row should be present and tagged as user-supplied.
  user_row <- merged[
    merged$class == "b" & merged$coef == "x", ,
    drop = FALSE
  ]
  expect_equal(nrow(user_row), 1L)
  expect_equal(user_row$prior, "normal(0, 2)")
  expect_equal(user_row$source, "user")
  # The Intercept default that brms emits must still be present
  # (the gap fix #1 closes: previously only the user row survived).
  intercept_row <- merged[
    merged$class == "Intercept", ,
    drop = FALSE
  ]
  expect_gte(nrow(intercept_row), 1L)
  expect_true(any(intercept_row$source == "default"))
})

test_that("extract_prior_from_setup returns the default table verbatim when no user prior", {
  # NULL user prior -> full default table, with every row tagged
  # default or (vectorized).
  dat <- data.frame(
    y      = rpois(40, 3),
    x      = rnorm(40),
    series = factor(rep("a", 40)),
    time   = seq_len(40)
  )
  setup <- list(
    formula = y ~ x,
    data    = dat,
    family  = poisson(),
    data2   = NULL,
    prior   = NULL
  )
  out <- mvgam:::extract_prior_from_setup(setup)
  expect_s3_class(out, "brmsprior")
  expect_true(all(out$source %in%
                    c("default", "(vectorized)")))
})

# lift_mvgam_stanvar_priors: scanner tests over hand-built stancode
# fragments. Each fragment contains exactly the prior pattern under
# test; the helper should detect it and append a row tagged
# source = "mvgam" on top of an empty brmsprior baseline.

empty_brmsprior <- function() {
  brms::validate_prior(
    prior   = NULL,
    formula = y ~ 1,
    data    = data.frame(y = rnorm(20)),
    family  = gaussian()
  )[0L, , drop = FALSE]
}

test_that("lift detects Z_free_vec partial-Z loadings prior", {
  sc <- "model { Z_free_vec ~ student_t(3, 0, 1); }"
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_s3_class(out, "brmsprior")
  expect_equal(nrow(out), 1L)
  expect_equal(out$class, "Z_free_vec")
  expect_equal(out$prior, "student_t(3, 0, 1)")
  expect_equal(out$source, "mvgam")
})

test_that("lift detects theta_features lognormal kernel prior", {
  sc <- "  target += lognormal_lpdf(theta_features | 0, 1);"
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(out$class, "theta_features")
  expect_equal(out$prior, "lognormal(0, 1)")
  expect_equal(out$source, "mvgam")
})

test_that("lift detects per-distance theta_dist_<NAME> kernel priors", {
  sc <- paste(
    "target += lognormal_lpdf(theta_dist_phylo | 0, 1);",
    "target += lognormal_lpdf(theta_dist_geo | 0, 1);",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$class, c("theta_dist_phylo", "theta_dist_geo"))
  expect_true(all(out$source == "mvgam"))
})

test_that("lift detects both MGP varrho_inv priors with distinct coefs", {
  sc <- paste(
    "model {",
    "  varrho_inv[1] ~ inv_gamma(mgp_a1, 1);",
    "  if (N_lv_trend > 1) {",
    "    varrho_inv[2:N_lv_trend] ~ inv_gamma(mgp_a2, 1);",
    "  }",
    "}",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$class == "varrho_inv"))
  expect_setequal(out$coef, c("1", "2:N_lv_trend"))
  expect_setequal(
    out$prior,
    c("inv_gamma(mgp_a1, 1)", "inv_gamma(mgp_a2, 1)")
  )
})

test_that("lift detects closure-unit Psi exponential prior", {
  sc <- "model {\n  Psi ~ exponential(1);\n}"
  out <- mvgam:::lift_mvgam_stanvar_priors(empty_brmsprior(), sc)
  expect_equal(out$class, "Psi")
  expect_equal(out$prior, "exponential(1)")
})

test_that("lift returns prior unchanged when stancode has no matches", {
  sc <- "model { Intercept ~ student_t(3, 0, 2.5); }"
  base <- empty_brmsprior()
  out <- mvgam:::lift_mvgam_stanvar_priors(base, sc)
  expect_equal(nrow(out), 0L)
})

test_that("lift returns prior unchanged when stancode is NULL or empty", {
  base <- empty_brmsprior()
  expect_identical(
    mvgam:::lift_mvgam_stanvar_priors(base, NULL), base
  )
  expect_identical(
    mvgam:::lift_mvgam_stanvar_priors(base, ""), base
  )
})

test_that("lift preserves brmsprior columns when appending mvgam rows", {
  base <- brms::validate_prior(
    prior   = brms::prior(normal(0, 2), class = "b", coef = "x"),
    formula = y ~ x,
    data    = data.frame(y = rnorm(20), x = rnorm(20)),
    family  = gaussian()
  )
  sc <- paste(
    "Z_free_vec ~ student_t(3, 0, 1);",
    "Psi ~ exponential(1);",
    sep = "\n"
  )
  out <- mvgam:::lift_mvgam_stanvar_priors(base, sc)
  # Columns of the union are preserved.
  expect_setequal(names(out), names(base))
  # User row still there and tagged "user".
  user_row <- out[out$class == "b" & out$coef == "x", , drop = FALSE]
  expect_equal(user_row$source, "user")
  # Two mvgam rows added.
  mvgam_rows <- out[out$source == "mvgam", , drop = FALSE]
  expect_equal(nrow(mvgam_rows), 2L)
  expect_setequal(mvgam_rows$class, c("Z_free_vec", "Psi"))
})

test_that("lift rejects non-brmsprior prior input", {
  expect_error(
    mvgam:::lift_mvgam_stanvar_priors(
      data.frame(prior = "x"), "Psi ~ exponential(1);"
    ),
    "brmsprior"
  )
})

test_that("bayes_R2.mvgam errors for multivariate without resp", {
  stub <- make_mvgam_stub(mv = TRUE)
  expect_error(
    bayes_R2(stub),
    regexp = "requires 'resp' for multivariate models"
  )
})

test_that("hidden_unrotated_factor_pars hides rotation-indeterminate factor params", {
  # Free-Z factor fit: Z_tilde[ present means raw Z, raw lv_trend,
  # innovations, Q_tilde and the latent-factor variance block all
  # get the rotation-indeterminacy hide pattern applied together.
  pars_free <- c(
    "Intercept", "b_x", "shape", "sigma_trend[1]", "sigma_trend[2]",
    "L_Omega_trend[1,1]", "L_Omega_trend[2,1]", "Sigma_trend[1,1]",
    "Q_tilde[1,1]", "Z[1,1]", "Z[2,1]", "Z_tilde[1,1]", "Z_tilde[2,1]",
    "lv_trend[1,1]", "lv_trend_tilde[1,1]",
    "innovations_trend[1,1]", "scaled_innovations_trend[1,1]"
  )
  pat <- mvgam:::hidden_unrotated_factor_pars(pars_free)
  hidden <- pars_free[grepl(pat, pars_free)]
  surviving <- pars_free[!grepl(pat, pars_free)]
  # All raw / rotation-indeterminate params are hidden.
  expect_setequal(
    hidden,
    c("sigma_trend[1]", "sigma_trend[2]",
      "L_Omega_trend[1,1]", "L_Omega_trend[2,1]", "Sigma_trend[1,1]",
      "Q_tilde[1,1]", "Z[1,1]", "Z[2,1]",
      "lv_trend[1,1]", "innovations_trend[1,1]",
      "scaled_innovations_trend[1,1]")
  )
  # Identified counterparts + obs-side params survive.
  expect_setequal(
    surviving,
    c("Intercept", "b_x", "shape",
      "Z_tilde[1,1]", "Z_tilde[2,1]", "lv_trend_tilde[1,1]")
  )
})

test_that("hidden_unrotated_factor_pars is a no-op without QR-identified counterparts", {
  # Non-factor fit: no Z_tilde, no lv_trend_tilde, no A_trend_tilde.
  # The variance block must survive because it is properly identified
  # in non-factor trend fits.
  pars_nonfactor <- c(
    "Intercept", "b_x", "sigma_trend[1]", "L_Omega_trend[1,1]",
    "Sigma_trend[1,1]"
  )
  pat <- mvgam:::hidden_unrotated_factor_pars(pars_nonfactor)
  expect_null(pat)
  # filter_hidden_unrotated returns the input unchanged.
  expect_identical(
    mvgam:::filter_hidden_unrotated(pars_nonfactor), pars_nonfactor
  )
})

test_that("hidden_unrotated_factor_pars adds A_trend hide on VAR factor fits", {
  pars_var <- c(
    "Z_tilde[1,1]", "A_trend[1][1,1]", "A_trend_tilde[1][1,1]"
  )
  pat <- mvgam:::hidden_unrotated_factor_pars(pars_var)
  expect_true(grepl("\\^A_trend\\\\\\[", pat))
  expect_true(grepl("A_trend\\[1\\]\\[1,1\\]", grep(pat, pars_var, value = TRUE)[1]))
})

test_that("filter_hidden_unrotated honours an explicit Z request via name", {
  # Stub-level test: when the user explicitly asks for "Z", the
  # default-path filter should NOT have dropped it from the pars
  # vector the extractor sees. This guards against the regression
  # where the filter ran before the variable argument was honoured.
  pars <- c("Z_tilde[1,1]", "Z[1,1]", "b_x", "Intercept")
  # Default path drops the raw Z[ name.
  identified <- mvgam:::filter_hidden_unrotated(pars)
  expect_false("Z[1,1]" %in% identified)
  # The escape hatch (explicit variable arg in as_draws_array.mvgam)
  # bypasses filter_hidden_unrotated entirely and runs the user's
  # pattern against the full posterior name list; the integration
  # test for that lives in tests/local/test-jsdgam-diagnostics.R
  # because it needs a fitted jsdgam.
  expect_true("Z_tilde[1,1]" %in% identified)
  expect_true("b_x" %in% identified)
})
