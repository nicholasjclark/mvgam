# CI-safe tests for the thin brms-parity wrappers landed in
# `R/brms_wrappers.mvgam.R`. Locks in S3 dispatch, signature
# parity, return shapes, and a few semantic edge cases. Numerical
# concordance against brms lives in tests/local/.


make_wrapper_stub <- function(with_re = FALSE) {
  varnames <- c(
    "b_Intercept", "b_x", "Intercept", "sigma",
    if (with_re) c("sd_grp__Intercept", "r_grp[a,Intercept]") else NULL,
    "lp__"
  )
  set.seed(2L)
  arr <- array(
    rnorm(50L * 2L * length(varnames)),
    dim = c(50L, 2L, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  std <- list(N = 8L, X = matrix(rnorm(16L), 8L, 2L,
                                  dimnames = list(NULL,
                                                  c("Intercept", "x"))))
  if (with_re) {
    std$M_1 <- 1L
    std$N_1 <- 1L
  }
  form <- if (with_re) brms::bf(y ~ x + (1 | grp)) else brms::bf(y ~ x)
  structure(
    list(
      fit = drws,
      formula = form,
      data = data.frame(y = rnorm(8L), x = rnorm(8L),
                         grp = factor("a")),
      family = brms::brmsfamily("gaussian"),
      standata = std,
      response_names = "y"
    ),
    class = "mvgam"
  )
}


# ---- Dispatch + signature parity -----------------------------------

test_that("Tier-7 methods have S3 entries on mvgam", {
  for (m in c("posterior_interval", "predictive_interval", "ngrps",
              "predictive_error")) {
    expect_true(
      !is.null(getS3method(m, "mvgam", optional = TRUE)),
      info = NULL
    )
  }
})


test_that("posterior_interval.mvgam matches brms signature", {
  expected <- names(formals(getS3method("posterior_interval", "brmsfit")))
  actual <- names(formals(getS3method("posterior_interval", "mvgam")))
  expect_true(all(expected %in% actual))
})


test_that("predictive_interval.mvgam matches brms signature", {
  expected <- names(formals(getS3method("predictive_interval", "brmsfit")))
  actual <- names(formals(getS3method("predictive_interval", "mvgam")))
  expect_true(all(expected %in% actual))
})


test_that("ngrps.mvgam matches brms signature", {
  expected <- names(formals(getS3method("ngrps", "brmsfit")))
  actual <- names(formals(getS3method("ngrps", "mvgam")))
  expect_true(all(expected %in% actual))
})


test_that("predictive_error.mvgam matches brms signature", {
  expected <- names(formals(getS3method("predictive_error", "brmsfit")))
  actual <- names(formals(getS3method("predictive_error", "mvgam")))
  expect_true(all(expected %in% actual))
})


# ---- ngrps gate ----------------------------------------------------

test_that("ngrps.mvgam returns NULL on a no-RE fit", {
  set.seed(3L); n <- 12L
  df <- data.frame(y = rnorm(n), x = rnorm(n))
  sd_ <- brms::standata(
    brms::bf(y ~ x), data = df,
    family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms::bf(y ~ x), data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  expect_null(ngrps(stub))
})


# ---- Aliasing semantics on wrappers --------------------------------

test_that("posterior_interval.mvgam respects pars / variable / prob", {
  stub <- make_wrapper_stub()
  pi_default <- posterior_interval(stub)
  expect_true(is.matrix(pi_default))
  expect_identical(ncol(pi_default), 2L)
  # Default brms prob = 0.95 -> columns are 2.5% / 97.5%.
  expect_identical(colnames(pi_default), c("2.5%", "97.5%"))
  pi_50 <- posterior_interval(stub, prob = 0.5)
  expect_identical(colnames(pi_50), c("25%", "75%"))
  pi_one <- posterior_interval(stub, variable = "b_x")
  expect_identical(nrow(pi_one), 1L)
  expect_identical(rownames(pi_one), "b_x")
})


test_that("posterior_interval pars alias falls back to variable", {
  stub <- make_wrapper_stub()
  pi_pars <- posterior_interval(stub, pars = "b_x")
  expect_identical(rownames(pi_pars), "b_x")
})


# ---- predictive_error response-column check ------------------------

test_that("predictive_error.mvgam errors when newdata lacks the response", {
  set.seed(5L); n <- 10L
  stub <- make_wrapper_stub()
  nd <- data.frame(x = rnorm(n))  # no `y` column
  expect_error(
    predictive_error(stub, newdata = nd),
    "response"
  )
})


# ---- mvgam_response_name -------------------------------------------

test_that("mvgam_response_name returns the LHS variable", {
  stub <- make_wrapper_stub()
  expect_identical(mvgam_response_name(stub), "y")
})


# ---- hypothesis.mvgam ----------------------------------------------

test_that("hypothesis.mvgam returns a brmshypothesis on a single test", {
  stub <- make_wrapper_stub()
  h <- hypothesis(stub, "b_x > 0")
  expect_s3_class(h, "brmshypothesis")
  expect_named(
    h$hypothesis,
    c("Hypothesis", "Estimate", "Est.Error", "CI.Lower",
      "CI.Upper", "Evid.Ratio", "Post.Prob", "Star")
  )
  expect_identical(nrow(h$hypothesis), 1L)
  # Estimate equals the posterior mean of b_x drawn from the stub.
  draws_df <- as.data.frame(posterior::as_draws_df(stub$fit))
  expect_equal(
    h$hypothesis$Estimate,
    mean(draws_df$b_x),
    tolerance = 1e-8
  )
})


test_that("hypothesis.mvgam accepts multiple hypotheses", {
  stub <- make_wrapper_stub()
  h <- hypothesis(stub, c("b_x > 0", "b_Intercept < 1"))
  expect_s3_class(h, "brmshypothesis")
  expect_identical(nrow(h$hypothesis), 2L)
})


test_that("hypothesis.mvgam validates input types", {
  stub <- make_wrapper_stub()
  expect_error(hypothesis(stub, 1L), "character")
  expect_error(hypothesis(stub, "b_x > 0", alpha = 2),
               "alpha")
  expect_error(hypothesis(stub, "b_x > 0", robust = "yes"),
               "robust")
})
