# CI-safe tests for the Tier-3 LOO / WAIC / vcov methods exposed in
# `R/loo_extras.mvgam.R` and the `vcov.mvgam` method appended to
# `R/diagnostics.mvgam.R`. The numerical concordance against
# `brms::*` is exercised by `tests/local/test-marginaleffects-concordance.R`;
# here we lock in S3 dispatch, signature parity, and the error paths
# that should fire deterministically.


# Build a stub that carries the four standata + formula slots every
# alias / draws / dispatch path consults. The variable layout
# mirrors a univariate Gaussian fit with one fixed effect (`x`) and
# AR(1) trend dynamics.
make_loo_extras_stub <- function(n_iter = 80L, n_chains = 2L) {
  varnames <- c(
    "b_Intercept", "b_x", "Intercept",
    "sigma", "sigma_trend[1]", "ar1_trend[1]",
    "trend[1,1]", "lp__"
  )
  set.seed(2L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  X <- model.matrix(~ x, data.frame(x = rnorm(10)))
  attr(X, "assign") <- NULL
  attr(X, "contrasts") <- NULL
  colnames(X)[1L] <- "Intercept"
  structure(
    list(
      fit = drws,
      formula = structure(y ~ x, class = c("brmsformula", "formula")),
      trend_formula = NULL,
      response_names = "y",
      data = data.frame(y = rnorm(10), x = rnorm(10)),
      standata = list(X = X, K = ncol(X), Kc = ncol(X) - 1L),
      prior = data.frame(prior = "(flat)", class = "b"),
      call = call("mvgam", formula = y ~ x)
    ),
    class = "mvgam"
  )
}


# ---- S3 dispatch lookup --------------------------------------------

test_that("every Tier-3 method has an S3 method on `mvgam`", {
  for (m in c(
    "vcov", "LOO", "WAIC", "loo_R2", "loo_predict",
    "loo_epred", "loo_linpred", "loo_predictive_interval",
    "loo_subsample", "loo_moment_match",
    "loo_model_weights", "add_criterion"
  )) {
    expect_true(
      !is.null(getS3method(m, "mvgam", optional = TRUE)),
      label = paste0("getS3method('", m, "', 'mvgam')")
    )
  }
})


# ---- Signature parity vs brms / loo --------------------------------

test_that("Tier-3 method signatures match brms / loo reference", {
  expected <- list(
    vcov = c("object", "correlation", "pars", "..."),
    LOO = c(
      "x", "...", "compare", "resp", "pointwise",
      "moment_match", "reloo", "k_threshold", "save_psis",
      "moment_match_args", "reloo_args", "model_names"
    ),
    WAIC = c(
      "x", "...", "compare", "resp", "pointwise", "model_names"
    ),
    loo_R2 = c(
      "object", "resp", "summary", "robust", "probs",
      "seed", "args_epred", "args_loglik", "..."
    ),
    loo_predict = c(
      "object", "type", "probs", "psis_object", "resp", "..."
    ),
    loo_epred = c(
      "object", "type", "probs", "psis_object", "resp", "..."
    ),
    loo_linpred = c(
      "object", "type", "probs", "psis_object", "resp", "..."
    ),
    loo_predictive_interval = c(
      "object", "prob", "psis_object", "..."
    ),
    loo_subsample = c("x", "...", "compare", "resp", "model_names"),
    loo_moment_match = c(
      "x", "loo", "k_threshold", "newdata", "resp",
      "check", "recompile", "..."
    ),
    loo_model_weights = c("x", "...", "model_names"),
    add_criterion = c(
      "x", "criterion", "model_name", "overwrite",
      "file", "force_save", "..."
    )
  )
  for (m in names(expected)) {
    formals_names <- names(formals(getS3method(m, "mvgam")))
    expect_identical(
      formals_names, expected[[m]],
      label = paste0("formals of ", m, ".mvgam")
    )
  }
})


# ---- vcov.mvgam -----------------------------------------------------

test_that("vcov.mvgam returns square covariance over fixed effects", {
  stub <- make_loo_extras_stub()
  v <- vcov(stub)
  expect_true(is.matrix(v))
  expect_identical(dim(v), c(2L, 2L))
  expect_identical(colnames(v), c("Intercept", "x"))
  # Symmetric.
  expect_equal(v, t(v))
})


test_that("vcov.mvgam(correlation = TRUE) gives identity diagonal", {
  stub <- make_loo_extras_stub()
  cmat <- vcov(stub, correlation = TRUE)
  expect_true(is.matrix(cmat))
  expect_equal(diag(cmat), c(Intercept = 1, x = 1))
})


test_that("vcov.mvgam(pars =) filters fixed effects", {
  stub <- make_loo_extras_stub()
  v <- vcov(stub, pars = "x")
  expect_identical(dim(v), c(1L, 1L))
  expect_identical(colnames(v), "x")
})


test_that("vcov.mvgam returns empty matrix when pars filter is empty", {
  stub <- make_loo_extras_stub()
  v <- vcov(stub, pars = "no_such_term")
  expect_identical(dim(v), c(0L, 0L))
})


# ---- LOO / WAIC aliases --------------------------------------------

test_that("LOO.mvgam forwards to loo() via S3 dispatch", {
  meth <- getS3method("LOO", "mvgam")
  # Body of the alias must reference `loo(`, not duplicate logic.
  expect_match(deparse(body(meth)), "loo\\(x", all = FALSE)
})


test_that("WAIC.mvgam forwards to waic() via S3 dispatch", {
  meth <- getS3method("WAIC", "mvgam")
  expect_match(deparse(body(meth)), "waic\\(x", all = FALSE)
})


# ---- Error paths (loo_moment_match + loo_subsample) ----------------

test_that("loo_moment_match.mvgam errors informatively", {
  stub <- make_loo_extras_stub()
  expect_error(
    loo_moment_match(stub),
    "loo_moment_match.*not currently supported"
  )
})


test_that("loo_subsample.mvgam errors informatively", {
  stub <- make_loo_extras_stub()
  expect_error(
    loo_subsample(stub),
    "loo_subsample.*not currently supported"
  )
})


# ---- add_criterion --------------------------------------------------

test_that("add_criterion.mvgam errors on unsupported criterion", {
  stub <- make_loo_extras_stub()
  expect_error(
    add_criterion(stub, "kfold"),
    "Unsupported criterion"
  )
})


test_that("add_criterion.mvgam errors on multiple unsupported criteria", {
  stub <- make_loo_extras_stub()
  expect_error(
    add_criterion(stub, c("kfold", "marglik")),
    "Unsupported criterion"
  )
})


test_that("add_criterion.mvgam initialises $criteria when absent", {
  # Verify the loop guards the slot creation, not the criterion call.
  meth <- getS3method("add_criterion", "mvgam")
  body_chr <- deparse(body(meth))
  expect_true(expect_match2(body_chr, "is.null(x$criteria)"))
  expect_true(expect_match2(body_chr, "x$criteria[[cname]]"))
})


# ---- loo_model_weights argument splitting --------------------------

test_that("loo_model_weights.mvgam rejects unnamed non-model args", {
  stub <- make_loo_extras_stub()
  # Pass a positional non-mvgam non-named arg.
  expect_error(
    loo_model_weights(stub, 42),
    "must be named"
  )
})


# ---- LOO trio: shared helper + pure-delegation forms ---------------

test_that("loo_predict / loo_epred / loo_linpred delegate to shared helper", {
  for (m in c("loo_predict", "loo_epred", "loo_linpred")) {
    body_chr <- paste(deparse(body(getS3method(m, "mvgam"))),
                       collapse = "\n")
    expect_match(body_chr, "mvgam_loo_E_loo")
  }
})


test_that("loo_predictive_interval delegates to loo_predict with quantile", {
  body_chr <- paste(
    deparse(body(getS3method("loo_predictive_interval", "mvgam"))),
    collapse = "\n"
  )
  expect_match(body_chr, "loo_predict")
  expect_match(body_chr, "quantile")
})


test_that("loo_predictive_interval validates 'prob' in (0, 1)", {
  stub <- make_loo_extras_stub()
  expect_error(loo_predictive_interval(stub, prob = -0.1),
               "Element 1 is not >= 0")
  expect_error(loo_predictive_interval(stub, prob = 1.5),
               "Element 1 is not <= 1")
})


test_that("mvgam_loo_E_loo asserts its 'posterior_fn' is a function", {
  stub <- make_loo_extras_stub()
  expect_error(
    mvgam_loo_E_loo(stub, posterior_fn = "not_a_function"),
    "function"
  )
})
