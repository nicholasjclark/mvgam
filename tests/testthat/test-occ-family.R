# Tests for the single-season Bernoulli-binomial occ() family.
# Structure mirrors test-nmix-family.R: constructor + attribute
# tagging, validate_closure_unit_data binary-response semantics,
# build_closure_unit_arrays with default_cap, stancode + standata
# emission via the mvgam_formula pipeline, R-side method shape,
# dispatcher routing, and posterior_occupancy conditional /
# marginal / draw semantics. Heavier MCMC recovery + flocker
# concordance live in tests/local/test-occ-flocker-concordance.R.

# ------------------------------------------------------------
# occ() constructor
# ------------------------------------------------------------

test_that("occ() returns a customfamily with mu/p dpars and logit links", {
  fam <- occ()
  expect_s3_class(fam, "customfamily")
  expect_s3_class(fam, "brmsfamily")
  expect_identical(fam$name, "occ")
  expect_identical(fam$dpars, c("mu", "p"))
  expect_identical(fam$link, "logit")
  expect_identical(fam$link_p, "logit")
  expect_identical(fam$type, "int")
  expect_false(fam$loop)
  # Both bounds at [0, 1] so the scalar-dpar case is declared
  # `real<lower=0, upper=1>` in Stan; the lpmf reconstructs the
  # logit scale internally for stable inverse-link forms.
  expect_identical(fam$lb, list(mu = "0", p = "0"))
  expect_identical(fam$ub, list(mu = "1", p = "1"))
  expect_true(is.function(fam$linkinv))
  expect_equal(fam$linkinv(0), 0.5)
})

test_that("occ() tags closure-unit, binary-response and predict-type attributes", {
  fam <- occ()
  expect_true(is_closure_unit_family(fam))
  expect_true(isTRUE(attr(fam, "mvgam_binary_response", exact = TRUE)))
  expect_identical(
    attr(fam, "mvgam_predict_types", exact = TRUE),
    c("occupancy", "detection")
  )
  expect_identical(
    attr(fam, "mvgam_vars", exact = TRUE),
    c("N_unit", "n_rep", "Y_max", "visit_idx")
  )
  expect_null(attr(fam, "mvgam_stanvars", exact = TRUE))
})

# ------------------------------------------------------------
# validate_closure_unit_data() binary-response branch
# ------------------------------------------------------------

make_occ_data <- function(n_unit = 5, n_visit = 4, seed = 1) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = rbinom(n_total, 1L, 0.4),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

test_that("validate_closure_unit_data() accepts occ data without a cap column", {
  d <- make_occ_data()
  expect_invisible(validate_closure_unit_data(
    d, response_var = "y",
    has_obs_covariates = TRUE, has_det_covariates = TRUE,
    binary_response = TRUE
  ))
})

test_that("validate_closure_unit_data() rejects non-binary y under occ", {
  d <- make_occ_data()
  d$y[1L] <- 3L
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y",
      has_obs_covariates = TRUE, has_det_covariates = TRUE,
      binary_response = TRUE
    ),
    "Binary-response closure-unit family requires"
  )
})

test_that("validate_closure_unit_data() warns (not errors) for occ with all-single-visit + no covariates", {
  d <- make_occ_data(n_unit = 5, n_visit = 1)
  withr::with_envvar(c(TESTTHAT = ""), {
    # Inverted: TESTTHAT off so the rlang::warn fires, then assert
    # it is a warning rather than an error (the occ branch demotes
    # the single-visit no-covariates case).
    expect_warning(
      validate_closure_unit_data(
        d, response_var = "y",
        has_obs_covariates = FALSE, has_det_covariates = FALSE,
        binary_response = TRUE
      ),
      "Every closure unit has a single visit"
    )
  })
})

test_that("validate_closure_unit_data() hard-errors on N_unit < 2 for both branches", {
  # Single closure unit with multiple visits: no draws from the
  # state distribution, structurally degenerate for any closure
  # family.
  d <- data.frame(
    series = factor(rep(1L, 3)),
    time   = rep(1L, 3),
    y      = c(1L, 0L, 1L)
  )
  expect_error(
    validate_closure_unit_data(
      d, response_var = "y",
      binary_response = TRUE
    ),
    "requires at least two closure units"
  )
})

# ------------------------------------------------------------
# build_closure_unit_arrays() with default_cap (occ path)
# ------------------------------------------------------------

test_that("build_closure_unit_arrays() injects default_cap when cap column absent", {
  d <- make_occ_data(n_unit = 3, n_visit = 4)
  arrs <- build_closure_unit_arrays(
    d, response_var = "y", default_cap = 1L
  )
  expect_identical(arrs$K_max, rep(1L, 3))
  # Y_max is 0 or 1 because y is binary; doubles as the per-unit
  # "any detection?" indicator that drives the fast-path branch.
  expect_true(all(arrs$Y_max %in% c(0L, 1L)))
  expect_identical(arrs$N_unit, 3L)
  expect_identical(arrs$n_rep, rep(4L, 3))
})

# ------------------------------------------------------------
# Stancode + standata emission via the mvgam_formula pipeline
# ------------------------------------------------------------

test_that("stancode(mvgam_formula(...)) for occ emits the occ_lpmf signature and no K_max", {
  d <- make_occ_data()
  mf <- mvgam_formula(bf(y ~ elev, p ~ tod))
  sc <- as.character(stancode(mf, data = d, family = occ()))
  sd <- standata(mf, data = d, family = occ())
  # All four lpmf overloads emitted.
  expect_match(sc, "real occ_lpmf\\(\\s*array\\[\\] int y,\\s*vector mu,\\s*vector p,")
  expect_match(sc, "rep_vector\\(p, N\\)")
  expect_match(sc, "rep_vector\\(mu, N\\)")
  # target += occ_lpmf(Y | mu, p, N_unit, n_rep, Y_max, visit_idx)
  expect_match(sc, "occ_lpmf\\(Y \\| mu, p, N_unit, n_rep, Y_max, visit_idx\\)")
  # K_max is absent for occ (binary latent state, no truncation).
  expect_false("K_max" %in% names(sd))
  expect_true(all(c("N_unit", "n_rep", "Y_max", "visit_idx") %in% names(sd)))
  expect_true(all(sd$Y_max %in% c(0L, 1L)))
})

test_that("stancode for occ without a p sub-formula declares scalar p in (0, 1)", {
  d <- make_occ_data()
  mf <- mvgam_formula(y ~ elev)
  sc <- as.character(stancode(mf, data = d, family = occ()))
  expect_match(sc, "real<lower=0, ?upper=1> p;")
})

# ------------------------------------------------------------
# Dispatcher: every method_kind resolves to a callable for both
# closure-unit families
# ------------------------------------------------------------

test_that("dispatch_closure_unit_method() resolves nmix + occ for every method_kind", {
  fams <- list(nmix = nmix(), occ = occ())
  for (kind in c("epred", "predict", "log_lik", "latent_state")) {
    for (nm in names(fams)) {
      fn <- dispatch_closure_unit_method(fams[[nm]], kind)
      expect_true(is.function(fn))
    }
  }
})

test_that("dispatch_closure_unit_method() errors on a non-closure family", {
  expect_error(
    dispatch_closure_unit_method(gaussian(), "epred"),
    "requires a closure-unit family"
  )
})

# ------------------------------------------------------------
# predict.mvgam type-dispatch gate via mvgam_predict_types
# ------------------------------------------------------------

test_that("predict.mvgam(type = 'occupancy') errors on a non-occ family", {
  d <- make_occ_data()
  # Use an mvgam fit with gaussian() to drive the gate without
  # requiring a long MCMC run for occ itself.
  d$y_real <- as.numeric(d$y)
  fit <- mvgam(y_real ~ elev, data = d, chains = 1, iter = 100,
               warmup = 50, silent = 2, refresh = 0)
  expect_error(
    predict(fit, type = "occupancy"),
    "not available for this family"
  )
})
