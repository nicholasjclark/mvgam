# The leave-one-out extras and `vcov.mvgam()` on stub fits. The brms
# concordance runs on fitted models in
# tests/local/test-marginaleffects-concordance.R.


# A stub with the draws, standata and formula of a Gaussian fit with
# one fixed effect `x` and an AR(1) trend
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
      data = data.frame(y = rnorm(10), x = rnorm(10)),
      standata = list(X = X, K = ncol(X), Kc = ncol(X) - 1L),
      prior = data.frame(prior = "(flat)", class = "b"),
      call = call("mvgam", formula = y ~ x)
    ),
    class = "mvgam"
  )
}


# ---- Signature parity vs brms / loo --------------------------------

test_that("each method takes the brms or loo signature", {
  expected <- list(
    vcov = c("object", "correlation", "pars", "trend_effects", "..."),
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

test_that("vcov.mvgam is the covariance of the fixed-effect draws", {
  stub <- make_loo_extras_stub()
  draws <- posterior::as_draws_matrix(stub$fit)[, c("b_Intercept", "b_x")]
  want <- stats::cov(unclass(draws))
  dimnames(want) <- list(c("Intercept", "x"), c("Intercept", "x"))
  expect_equal(vcov(stub), want)
  expect_equal(vcov(stub, correlation = TRUE), stats::cov2cor(want))
  expect_equal(vcov(stub, pars = "x"), want["x", "x", drop = FALSE])
})


test_that("vcov.mvgam refuses a 'pars' entry the model does not have", {
  # A misspelt term would otherwise drop its row without a message
  stub <- make_loo_extras_stub()
  expect_error(vcov(stub, pars = "no_such_term"), "no_such_term")
  expect_error(vcov(stub, pars = c("x", "no_such_term")), "no_such_term")
  # The names that exist come back in the order they were asked for.
  v <- vcov(stub, pars = c("x", "Intercept"))
  expect_identical(colnames(v), c("x", "Intercept"))
})


# ---- LOO / WAIC aliases --------------------------------------------

test_that("LOO and WAIC forward only the arguments the caller wrote", {
  # `loo()` and `waic()` refuse a supplied `compare`. An alias passing
  # its defaults on would be refused on every call.
  seen <- NULL
  record <- function(x, ...) {
    seen <<- names(list(...))
    "called"
  }
  # The generics called inside the package find the methods there
  local_mocked_bindings(loo.mvgam = record, waic.mvgam = record)
  stub <- structure(list(), class = "mvgam")
  expect_identical(LOO(stub, resp = "y"), "called")
  expect_identical(seen, "resp")
  expect_identical(WAIC(stub), "called")
  expect_null(seen)
})


test_that("loo_moment_match and loo_subsample are refused", {
  stub <- make_loo_extras_stub()
  expect_error(loo_moment_match(stub), "loo_moment_match.*not supported")
  expect_error(loo_subsample(stub), "loo_subsample.*not supported")
})


test_that("add_criterion stores each criterion once unless overwritten", {
  calls <- 0L
  # The generic called inside the package finds the method there
  local_mocked_bindings(loo.mvgam = function(x, ...) {
    calls <<- calls + 1L
    calls
  })
  stub <- make_loo_extras_stub()
  out <- add_criterion(stub, "loo", model_name = "m1")
  expect_identical(out$criteria$loo, 1L)
  expect_identical(out$model_name, "m1")
  expect_identical(add_criterion(out, "loo")$criteria$loo, 1L)
  expect_identical(add_criterion(out, "loo", overwrite = TRUE)$criteria$loo,
                   2L)
  expect_error(add_criterion(stub, c("kfold", "loo_subsample")),
               "'kfold', 'loo_subsample' are not supported")
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


test_that("loo_predictive_interval asks for the central quantiles", {
  seen <- NULL
  local_mocked_bindings(loo_predict.mvgam = function(object, ...) {
    seen <<- list(...)
  })
  loo_predictive_interval(make_loo_extras_stub(), prob = 0.8, resp = "y")
  expect_identical(seen$type, "quantile")
  expect_equal(seen$probs, c(0.1, 0.9))
  expect_identical(seen$resp, "y")
})


# ---------------------------------------------------------------
# by_series split for loo() / waic(): column mapping + aggregator
# ---------------------------------------------------------------

# Build a non-closure-unit stub whose data + family give a direct
# (row -> series) mapping for per_obs_series_labels.
make_by_series_stub <- function() {
  stub <- make_loo_extras_stub()
  stub$data <- data.frame(
    series = factor(rep(c("a", "b"), each = 4L)),
    time = rep(1:4, 2L),
    y = rnorm(8L),
    x = rnorm(8L)
  )
  stub$family <- gaussian()
  stub$formula <- y ~ x
  stub
}

test_that("per_obs_series_labels maps log_lik columns to series", {
  stub <- make_by_series_stub()
  labels <- mvgam:::per_obs_series_labels(stub)
  expect_identical(
    labels,
    c("a", "a", "a", "a", "b", "b", "b", "b")
  )
})

test_that("per_obs_series_labels refuses a frame naming no series", {
  # The axis record assigns rows to series. This frame has neither a
  # series column nor grouping columns.
  stub <- make_by_series_stub()
  stub$data$series <- NULL
  expect_error(
    mvgam:::per_obs_series_labels(stub),
    "lack the series this model was fitted on"
  )
})

test_that("per_obs_series_labels rejects mv-custom families", {
  stub <- make_by_series_stub()
  # The attribute diri(), mvn(), multi(), categ() and mvt() set
  stub$family <- structure(
    list(family = "mvgam_dirichlet", name = "diri"),
    class = "customfamily",
    mvgam_multi_response = TRUE
  )
  expect_error(
    mvgam:::per_obs_series_labels(stub),
    "not supported for multi-response families"
  )
})

test_that("per_series_ic returns one elpd row per series", {
  stub <- make_by_series_stub()
  set.seed(7L)
  # One row per stub draw, 2 chains of 80. Autocorrelated draws keep
  # every Pareto k under the threshold.
  logliks <- -1 + 0.1 * apply(
    matrix(rnorm(160L * 8L), nrow = 160L), 2L,
    function(z) as.numeric(stats::filter(z, 0.3, "recursive"))
  )
  out_loo <- mvgam:::per_series_ic(stub, logliks, criterion = "loo")
  expect_named(out_loo, c("series", "elpd", "se_elpd", "p", "n_obs"))
  expect_identical(out_loo$series, c("a", "b"))
  expect_identical(out_loo$n_obs, c(4L, 4L))
  # Each series is scored on its own columns
  ll_b <- logliks[, 5:8]
  ref <- loo::loo(ll_b, r_eff = mvgam:::mvgam_r_eff_log_lik(stub, ll_b))
  expect_equal(out_loo$elpd[2L], ref$estimates["elpd_loo", "Estimate"])
  out_waic <- mvgam:::per_series_ic(stub, logliks, criterion = "waic")
  expect_equal(out_waic$p[1L],
               loo::waic(logliks[, 1:4])$estimates["p_waic", "Estimate"])
})
