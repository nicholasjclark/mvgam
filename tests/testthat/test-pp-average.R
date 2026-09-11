# CI-safe tests for the Bayesian model-averaging surface in
# `R/pp_average.mvgam.R` and the shared helpers in
# `R/mvgam_model_helpers.R`. Locks in S3 dispatch, signature
# parity, helper behaviour, and error paths. Numerical concordance
# against brms lives in `tests/local/test-marginaleffects-concordance.R`.


make_pp_average_stub <- function(n_iter = 60L, n_chains = 2L,
                                  varnames = NULL,
                                  seed = 1L) {
  if (is.null(varnames)) {
    varnames <- c("b_Intercept", "b_x", "sigma", "lp__")
  }
  set.seed(seed)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  X <- model.matrix(~ x, data.frame(x = rnorm(8L)))
  attr(X, "assign") <- NULL
  attr(X, "contrasts") <- NULL
  colnames(X)[1L] <- "Intercept"
  structure(
    list(
      fit = drws,
      formula = brms::bf(y ~ x),
      trend_formula = NULL,
      data = data.frame(y = rnorm(8L), x = rnorm(8L)),
      standata = list(X = X, K = ncol(X), Kc = ncol(X) - 1L,
                       Y = rnorm(8L), N = 8L),
      family = brms::brmsfamily("gaussian"),
      prior = data.frame(prior = "(flat)", class = "b"),
      call = call("mvgam", formula = y ~ x)
    ),
    class = "mvgam"
  )
}


# ---- Dispatch + signature parity -----------------------------------

test_that("posterior_average + pp_average have S3 entries on mvgam", {
  for (m in c("posterior_average", "pp_average")) {
    expect_true(
      !is.null(getS3method(m, "mvgam", optional = TRUE)),
      label = paste0("getS3method('", m, "', 'mvgam')")
    )
  }
})


test_that("posterior_average.mvgam matches brms signature", {
  expected <- names(formals(
    getS3method("posterior_average", "brmsfit")
  ))
  actual <- names(formals(
    getS3method("posterior_average", "mvgam")
  ))
  expect_true(all(expected %in% actual))
})


test_that("pp_average.mvgam matches brms signature", {
  expected <- names(formals(getS3method("pp_average", "brmsfit")))
  actual <- names(formals(getS3method("pp_average", "mvgam")))
  expect_true(all(expected %in% actual))
})


# ---- mvgam_round_largest_remainder ---------------------------------

test_that("mvgam_round_largest_remainder preserves the total", {
  # Standard non-integer split: 30/30/40
  expect_identical(
    mvgam_round_largest_remainder(c(0.3, 0.3, 0.4) * 100),
    c(30, 30, 40)
  )
  # Tied fractional parts must still sum to the requested total.
  out <- mvgam_round_largest_remainder(c(0.333, 0.333, 0.334) * 99)
  expect_identical(sum(out), 99)
  # Single weight: identity.
  expect_identical(mvgam_round_largest_remainder(7), 7)
})


# ---- mvgam_use_alias ----------------------------------------------

test_that("mvgam_use_alias returns alias when supplied, else x", {
  expect_identical(mvgam_use_alias("primary", NULL), "primary")
  expect_identical(mvgam_use_alias(NULL, "alias"), "alias")
  expect_null(mvgam_use_alias(NULL, NULL))
})


test_that("mvgam_use_alias errors when both are supplied", {
  expect_error(
    mvgam_use_alias("primary", "alias"),
    "deprecated alias"
  )
})


# ---- mvgam_validate_pp_method --------------------------------------

test_that("mvgam_validate_pp_method accepts the three valid choices", {
  for (m in c("posterior_predict", "posterior_epred",
              "posterior_linpred")) {
    expect_identical(mvgam_validate_pp_method(m), m)
  }
})


test_that("mvgam_validate_pp_method rejects unknown methods", {
  expect_error(
    mvgam_validate_pp_method("posterior_fit"),
    "should be one of"
  )
})


# ---- mvgam_match_response ------------------------------------------

test_that("mvgam_match_response returns TRUE for matching responses", {
  stub <- make_pp_average_stub()
  expect_true(mvgam_match_response(list(stub, stub)))
})


test_that("mvgam_match_response returns FALSE for mismatched responses", {
  a <- make_pp_average_stub()
  b <- make_pp_average_stub()
  b$formula <- brms::bf(z ~ x)
  expect_false(mvgam_match_response(list(a, b)))
})


test_that("mvgam_match_response is TRUE for a single model", {
  expect_true(mvgam_match_response(list(make_pp_average_stub())))
})


# ---- mvgam_split_models --------------------------------------------

test_that("mvgam_split_models captures variadic mvgam fits", {
  a <- make_pp_average_stub()
  b <- make_pp_average_stub()
  out <- mvgam_split_models(a, b)
  expect_named(out, c("models", "other"))
  expect_length(out$models, 2L)
  expect_length(out$other, 0L)
})


test_that("mvgam_split_models honours model_names", {
  a <- make_pp_average_stub()
  b <- make_pp_average_stub()
  out <- mvgam_split_models(
    a, b, model_names = c("alpha", "beta")
  )
  expect_identical(names(out$models), c("alpha", "beta"))
})


test_that("mvgam_split_models rejects unnamed non-model args", {
  a <- make_pp_average_stub()
  expect_error(
    mvgam_split_models(a, 42),
    "must be named"
  )
})


test_that("mvgam_split_models routes named non-model args to $other", {
  a <- make_pp_average_stub()
  out <- mvgam_split_models(a, ndraws_extra = 10L)
  expect_length(out$models, 1L)
  expect_identical(out$other$ndraws_extra, 10L)
})


# ---- mvgam_validate_weights ----------------------------------------

test_that("mvgam_validate_weights normalises numeric weights", {
  stubs <- list(make_pp_average_stub(), make_pp_average_stub())
  w <- mvgam_validate_weights(c(2, 3), stubs)
  expect_equal(w, c(0.4, 0.6))
})


test_that("mvgam_validate_weights errors on wrong-length numeric", {
  stubs <- list(make_pp_average_stub(), make_pp_average_stub())
  expect_error(
    mvgam_validate_weights(c(1, 2, 3), stubs),
    "one entry per model"
  )
})


test_that("mvgam_validate_weights errors on negative numeric", {
  stubs <- list(make_pp_average_stub(), make_pp_average_stub())
  expect_error(
    mvgam_validate_weights(c(1, -1), stubs),
    "non-negative"
  )
})


test_that("mvgam_validate_weights errors on all-zero numeric", {
  stubs <- list(make_pp_average_stub(), make_pp_average_stub())
  expect_error(
    mvgam_validate_weights(c(0, 0), stubs),
    "positive total"
  )
})


test_that("mvgam_validate_weights rejects 'kfold' informatively", {
  stubs <- list(make_pp_average_stub(), make_pp_average_stub())
  expect_error(
    mvgam_validate_weights("kfold", stubs),
    "'kfold' is not supported"
  )
})


test_that("mvgam_validate_weights match.args the named strategy", {
  stubs <- list(make_pp_average_stub(), make_pp_average_stub())
  expect_error(
    mvgam_validate_weights("nonsense", stubs),
    "should be one of"
  )
})


# ---- pp_average reserved-args guard --------------------------------

test_that("pp_average rejects 'draw_ids' / 'subset' in dots", {
  stub <- make_pp_average_stub()
  expect_error(
    pp_average(stub, draw_ids = 1L:5L),
    "draws its own per-model subsample"
  )
  expect_error(
    pp_average(stub, subset = 1L:5L),
    "draws its own per-model subsample"
  )
})


# ---- pp_average response-mismatch guard ----------------------------

test_that("pp_average errors on mismatched responses", {
  a <- make_pp_average_stub()
  b <- make_pp_average_stub()
  b$formula <- brms::bf(z ~ x)
  expect_error(
    pp_average(a, b, weights = c(0.5, 0.5)),
    "same response"
  )
})


# ---- posterior_average: missing argument branches ------------------

test_that("posterior_average errors when explicit variable is not shared", {
  a <- make_pp_average_stub(varnames = c("b_Intercept", "b_x",
                                          "sigma", "lp__"))
  b <- make_pp_average_stub(varnames = c("b_Intercept", "b_z",
                                          "sigma", "lp__"),
                             seed = 2L)
  expect_error(
    posterior_average(
      a, b, weights = c(0.5, 0.5),
      variable = c("b_Intercept", "b_x")
    ),
    "not present in every model"
  )
})


test_that("posterior_average default missing silently intersects", {
  a <- make_pp_average_stub(varnames = c("b_Intercept", "b_x",
                                          "sigma", "lp__"))
  b <- make_pp_average_stub(varnames = c("b_Intercept", "b_z",
                                          "sigma", "lp__"),
                             seed = 2L)
  out <- posterior_average(
    a, b, weights = c(0.5, 0.5), seed = 7L
  )
  # `b_x` and `b_z` are unique to one fit each — must be dropped.
  expect_setequal(colnames(out), c("b_Intercept", "sigma"))
  # 'lp__' must be excluded by default.
  expect_false("lp__" %in% colnames(out))
})


test_that("posterior_average errors when missing-list omits a default", {
  a <- make_pp_average_stub(varnames = c("b_Intercept", "b_x",
                                          "sigma", "lp__"))
  b <- make_pp_average_stub(varnames = c("b_Intercept", "b_z",
                                          "sigma", "lp__"),
                             seed = 2L)
  expect_error(
    posterior_average(
      a, b, weights = c(0.5, 0.5),
      variable = c("b_x", "b_z"),
      missing = list(b_x = 0)
    ),
    "no entry for some parameters"
  )
})


# ---- as.data.frame.mvgam draw subsetting ---------------------------

test_that("as.data.frame.mvgam(draw = ...) subsets rows", {
  stub <- make_pp_average_stub()
  full <- as.data.frame(stub)
  sub <- as.data.frame(stub, draw = c(1L, 5L, 10L))
  expect_identical(nrow(sub), 3L)
  expect_identical(colnames(sub), colnames(full))
})
