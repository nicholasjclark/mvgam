# CI-safe tests for update.mvgam. Numerical refits live in
# `tests/local/test-marginaleffects-concordance.R`; here we lock in
# S3 dispatch, signature parity with brms::update.brmsfit, the
# error paths that should fire deterministically, and the
# slot-inheritance helper (mvgam_update_call) on a stub.


make_update_stub <- function(n_iter = 50L, n_chains = 2L) {
  varnames <- c(
    "b_Intercept", "b_x", "Intercept",
    "sigma", "sigma_trend[1]", "ar1_trend[1]",
    "trend[1,1]", "lp__"
  )
  set.seed(3L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  X <- model.matrix(~ x, data.frame(x = rnorm(10)))
  colnames(X)[1L] <- "Intercept"
  structure(
    list(
      fit = drws,
      formula = structure(y ~ x, class = c("brmsformula", "formula")),
      trend_formula = NULL,
      family = stats::gaussian(),
      prior = data.frame(prior = "(flat)", class = "b"),
      data = data.frame(y = rnorm(10), x = rnorm(10)),
      standata = list(X = X, K = ncol(X), Kc = ncol(X) - 1L),
      stancode = "data { int N; } parameters { real mu; } model { mu ~ normal(0,1); }",
      backend = "cmdstanr",
      algorithm = "sampling",
      response_names = "y",
      call = call("mvgam", formula = y ~ x)
    ),
    class = "mvgam"
  )
}


# ---- Dispatch + signature parity -----------------------------------

test_that("update.mvgam has an S3 method on `mvgam`", {
  expect_true(
    !is.null(getS3method("update", "mvgam", optional = TRUE))
  )
})


test_that("update.mvgam signature matches brms::update.brmsfit", {
  expected <- names(formals(getS3method("update", "brmsfit")))
  actual <- names(formals(getS3method("update", "mvgam")))
  expect_identical(actual, expected)
})


# ---- Error paths ----------------------------------------------------

test_that("update.mvgam rejects `data` in dots with a newdata hint", {
  stub <- make_update_stub()
  expect_error(
    update(stub, data = data.frame(y = 1, x = 1)),
    "newdata"
  )
})


test_that("update.mvgam rejects pooled multiple-imputation fits", {
  stub <- make_update_stub()
  attr(stub, "is_pooled") <- TRUE
  expect_error(
    update(stub),
    "pooled multiple-imputation"
  )
})


test_that("update.mvgam rejects non-data.frame newdata", {
  stub <- make_update_stub()
  expect_error(
    update(stub, newdata = list(y = 1, x = 1)),
    "data.frame"
  )
})


test_that("update.mvgam rejects non-logical recompile", {
  stub <- make_update_stub()
  expect_error(
    update(stub, recompile = "yes"),
    "logical"
  )
})


# ---- mvgam_update_call helper --------------------------------------

test_that("mvgam_update_call inherits formula from object when formula. is NULL", {
  stub <- make_update_stub()
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL, dots = list())
  expect_identical(out$formula, stub$formula)
})


test_that("mvgam_update_call applies formula. via stats::update.formula", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = ~ . + z, newdata = NULL, dots = list()
  )
  expect_identical(deparse(out$formula), "y ~ x + z")
})


test_that("mvgam_update_call inherits data when newdata is NULL", {
  stub <- make_update_stub()
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL, dots = list())
  expect_identical(out$data, stub$data)
})


test_that("mvgam_update_call overrides data when newdata is supplied", {
  stub <- make_update_stub()
  nd <- stub$data[1:5, ]
  out <- mvgam_update_call(stub, formula. = NULL, newdata = nd, dots = list())
  expect_identical(out$data, nd)
})


test_that("mvgam_update_call inherits family / prior / backend / algorithm", {
  stub <- make_update_stub()
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL, dots = list())
  expect_identical(out$family, stub$family)
  expect_identical(out$prior, stub$prior)
  expect_identical(out$backend, stub$backend)
  expect_identical(out$algorithm, stub$algorithm)
})

test_that("mvgam_update_call inherits the initial-value specification", {
  # A refit that silently reverted to random starts would undo the
  # reason the original fit asked for Pathfinder in the first place
  stub <- make_update_stub()
  stub$init <- "pathfinder"
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list())
  expect_identical(out$init, "pathfinder")
})

test_that("mvgam_update_call drops the inherited warmup when iter is overridden", {
  # `warmup` cannot be inherited on its own: mvgam derives it as
  # `iter %/% 2`, so pairing the original fit's warmup with a smaller
  # user-supplied `iter` asks Stan for a negative sampling count and
  # fails with a message that names neither argument.
  stub <- make_update_stub()
  local_mocked_bindings(
    mvgam_sampler_inheritance = function(object) {
      list(chains = 2L, iter = 1500L, warmup = 750L, thin = 1L)
    },
    .package = "mvgam"
  )

  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list(iter = 400L))
  expect_identical(out$iter, 400L)
  expect_null(out$warmup)

  # An explicit warmup is still honoured alongside a new iter
  out2 <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                            dots = list(iter = 400L, warmup = 100L))
  expect_identical(out2$warmup, 100L)

  # and both are still inherited when neither is overridden
  out3 <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                            dots = list())
  expect_identical(out3$iter, 1500L)
  expect_identical(out3$warmup, 750L)
})

test_that("mvgam_update_call lets the caller override the inherited init", {
  stub <- make_update_stub()
  stub$init <- "pathfinder"
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list(init = "0"))
  expect_identical(out$init, "0")
})

test_that("mvgam_update_call lets the caller override the inherited prior", {
  # The lfo_cv refit story relies on update.mvgam reusing the
  # original fit's literal prior table by default (so brms doesn't
  # regenerate adaptive Intercept / sigma priors and bust the
  # compiled-model cache). Conversely, users must still be able to
  # supply `prior = ...` to override that table for a single refit.
  stub <- make_update_stub()
  new_prior <- structure(
    data.frame(
      prior = "normal(0, 0.5)", class = "b",
      coef = "", group = "", resp = "", dpar = "",
      nlpar = "", lb = NA_character_, ub = NA_character_,
      source = "user", stringsAsFactors = FALSE
    ),
    class = c("brmsprior", "data.frame")
  )
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(prior = new_prior)
  )
  expect_identical(out$prior, new_prior)
  expect_false(identical(out$prior, stub$prior))
})


test_that("mvgam_update_call lets `dots` override inherited slots", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(
      family = stats::poisson(),
      backend = "rstan",
      cores = 4L
    )
  )
  # family() captures an environment, so compare by family name
  # rather than via identical().
  expect_identical(out$family$family, "poisson")
  expect_identical(out$backend, "rstan")
  expect_identical(out$cores, 4L)
})


test_that("mvgam_update_call passes through unrelated dots", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(seed = 42L, init = "random", silent = 2L)
  )
  expect_identical(out$seed, 42L)
  expect_identical(out$init, "random")
  expect_identical(out$silent, 2L)
})


# ---- mvgam_normalise_stancode --------------------------------------

test_that("mvgam_normalise_stancode collapses trailing whitespace", {
  s1 <- "data { int N; }\n\n"
  s2 <- "data { int N; }"
  expect_identical(
    mvgam_normalise_stancode(s1),
    mvgam_normalise_stancode(s2)
  )
})


test_that("mvgam_normalise_stancode handles NULL input", {
  expect_identical(mvgam_normalise_stancode(NULL), character(0L))
})


test_that("mvgam_normalise_stancode strips a leading // comment line", {
  s1 <- "// Generated with mvgam 2.0.0 using brms 2.23.0\ndata { int N; }"
  s2 <- "// Generated with mvgam 9.9.9 using brms 9.9.9\ndata { int N; }"
  expect_identical(
    mvgam_normalise_stancode(s1),
    mvgam_normalise_stancode(s2)
  )
})


test_that("mvgam_normalise_stancode keeps stancode without a header", {
  s <- "data { int N; }\nparameters { real mu; }"
  expect_identical(mvgam_normalise_stancode(s), trimws(s))
})


# ---- Legacy fit detection ------------------------------------------

test_that("update.mvgam errors on legacy fits lacking trend_call", {
  stub <- make_update_stub()
  # Simulate a legacy fit: no trend_call slot, but trend_components
  # present (the construct that flags trend dynamics).
  stub$trend_components <- list(types = "AR")
  expect_error(
    update(stub),
    "trend_call"
  )
})


test_that("update.mvgam accepts legacy fits if trend_formula is supplied", {
  stub <- make_update_stub()
  stub$trend_components <- list(types = "AR")
  # mvgam_update_call should run without error when trend_formula
  # is in dots, even if trend_call is absent.
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(trend_formula = ~ AR(p = 1))
  )
  expect_identical(deparse(out$trend_formula), "~AR(p = 1)")
})
