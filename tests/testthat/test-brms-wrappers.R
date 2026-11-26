# The methods of brms and rstantools generics, on stub fits. Their
# agreement with brms on real fits is checked in tests/local/.


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
      standata = std
    ),
    class = "mvgam"
  )
}


# ---- Signature parity ----------------------------------------------

test_that("each method takes every argument its brmsfit method takes", {
  for (m in c("posterior_interval", "predictive_interval", "ngrps",
              "predictive_error", "loo", "waic")) {
    expected <- names(formals(getS3method(m, "brmsfit")))
    actual <- names(formals(getS3method(m, "mvgam")))
    expect_identical(setdiff(expected, actual), character(0L), label = m)
  }
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
  # The brms default `prob = 0.95`
  expect_identical(colnames(posterior_interval(stub)), c("2.5%", "97.5%"))
  expect_identical(colnames(posterior_interval(stub, prob = 0.5)),
                   c("25%", "75%"))
  expect_identical(rownames(posterior_interval(stub, variable = "b_x")),
                   "b_x")
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


# ---- response_column -----------------------------------------------

test_that("response_column returns the LHS variable", {
  stub <- make_wrapper_stub()
  expect_identical(response_column(stub), "y")
})


# ---- hypothesis.mvgam ----------------------------------------------

test_that("hypothesis.mvgam tests a coefficient by its formula name", {
  stub <- make_wrapper_stub()
  h <- hypothesis(stub, "x > 0")
  # The estimate is the posterior mean of `b_x`
  draws_df <- as.data.frame(posterior::as_draws_df(stub$fit))
  expect_equal(
    h$hypothesis$Estimate,
    mean(draws_df$b_x),
    tolerance = 1e-8
  )
})


test_that("hypothesis.mvgam accepts multiple hypotheses", {
  stub <- make_wrapper_stub()
  h <- hypothesis(stub, c("x > 0", "Intercept < 1"))
  # As in brms, the default class tests `b_Intercept`. `class = NULL`
  # takes any parameter by its full name.
  draws_df <- as.data.frame(posterior::as_draws_df(stub$fit))
  expect_equal(h$hypothesis$Estimate[2L] + 1, mean(draws_df$b_Intercept),
               tolerance = 1e-8)
  expect_equal(hypothesis(stub, "sigma > 0", class = NULL)$hypothesis$Estimate,
               mean(draws_df$sigma), tolerance = 1e-8)
})


test_that("hypothesis.mvgam validates input types", {
  stub <- make_wrapper_stub()
  expect_error(hypothesis(stub, 1L), "character")
  expect_error(hypothesis(stub, "b_x > 0", alpha = 2),
               "alpha")
  expect_error(hypothesis(stub, "b_x > 0", robust = "yes"),
               "robust")
})


# ---- bridge_sampler.mvgam gate -------------------------------------

test_that("bridge_sampler.mvgam refuses and names an unnormalised density", {
  stub <- make_wrapper_stub()
  stub$stancode <- "model { target += normal_lupdf(y | mu, sigma); }"
  expect_error(bridge_sampler(stub), "_lupdf")
  stub$stancode <- "model { target += poisson_lupmf(y | lambda); }"
  expect_error(bridge_sampler(stub), "_lupmf")
})


test_that("bridge_sampler.mvgam rejects an mvgam tilde and names it", {
  # brms drops a constant with `_lupdf`. A Stan tilde drops it too.
  # The parameter named tells the user which prior to look at.
  stub <- make_wrapper_stub()
  stub$stancode <- "model { sigma_trend ~ exponential(2); }"
  expect_error(bridge_sampler(stub), "sigma_trend")
})


test_that("bayes_factor.mvgam gates through the same normalization check", {
  stub <- make_wrapper_stub()
  stub$stancode <- "model { target += normal_lupdf(y | mu, sigma); }"
  expect_error(bayes_factor(stub, stub), "normalized")
})


test_that("bayes_factor.mvgam rejects non-mvgam/non-bridge x2", {
  stub <- make_wrapper_stub()
  expect_error(bayes_factor(stub, "not an mvgam"), "x2")
})


test_that("bayes_factor.mvgam validates the log flag", {
  stub <- make_wrapper_stub()
  expect_error(bayes_factor(stub, stub, log = "yes"), "log")
})


test_that("warn_once_per_call() drops repeats but never a new message", {
  # Building a model runs brms's code generator more than once over
  # the same data, so the same warning would otherwise reach the user
  # once per pass.
  seen <- character()
  withCallingHandlers(
    warn_once_per_call({
      warning("same message")
      warning("same message")
      warning("same message")
      warning("a different message")
      "value"
    }),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(seen, c("same message", "a different message"))

  # The value of the expression is passed through untouched.
  expect_equal(warn_once_per_call(41L + 1L), 42L)
})


test_that("without_ess_cap_notice() drops the cap notice and keeps the rest", {
  # Anticorrelated draws are what push an ESS estimate above the bound
  # posterior caps it at. The jitter separates the ranks, without which
  # ties hold the estimate under the bound and nothing is capped.
  set.seed(1)
  draws <- rep(c(-1, 1), 500) + stats::rnorm(1000, 0, 1e-3)

  # Matched on posterior's own wording, so a reworded notice fails
  # here instead of passing through mvgam unnoticed.
  expect_warning(posterior::ess_bulk(draws), "ESS has been capped")
  expect_silent(without_ess_cap_notice(posterior::ess_bulk(draws)))

  # The capped figure is the one reported: S * log10(S) at S = 1000.
  expect_equal(without_ess_cap_notice(posterior::ess_bulk(draws)), 3000)

  # A second, unrelated warning still reaches the caller.
  expect_warning(
    without_ess_cap_notice({
      warning("something else")
      posterior::ess_bulk(draws)
    }),
    "something else"
  )

  # The value of the expression is passed through untouched.
  expect_equal(without_ess_cap_notice(41L + 1L), 42L)
})
