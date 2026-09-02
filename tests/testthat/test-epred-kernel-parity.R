# mvgam computes the mean of an observation family itself rather than
# asking brms for it, so for every family the two both know, the two
# have to return the same number. A mean is where a family's extra
# parameter enters: a binomial mean is `trials * p`, a zero-inflated
# mean `(1 - zi) * mu`, and a hurdle mean divides by the probability
# of a non-zero draw. Applying the inverse link alone answers a
# different question for each of those, and answers it silently.
#
# The kernels are driven on a synthetic `prep` so every family is
# covered without sampling. Agreement on real posterior draws is
# checked in tests/local/test-predictions-brms-concordance.R.

epred_kernel_names <- function(ns) {
  grep("^posterior_epred_", ls(asNamespace(ns), all.names = TRUE),
       value = TRUE)
}

# Values a mean kernel can be evaluated at for any family: `mu` on
# the response scale, positive where a scale parameter is wanted, and
# probabilities strictly inside the unit interval.
mk_prep <- function(ndraws = 5L, nobs = 4L, trials = NULL) {
  set.seed(7L)
  n <- ndraws * nobs
  m <- function(v) matrix(v, nrow = ndraws, ncol = nobs)
  prep <- list(
    ndraws = ndraws,
    nobs = nobs,
    dpars = list(
      mu = m(runif(n, 0.4, 3.0)),
      zi = m(runif(n, 0.1, 0.4)),
      hu = m(runif(n, 0.1, 0.4)),
      shape = m(runif(n, 1.5, 4.0)),
      nu = m(runif(n, 2.5, 6.0)),
      sigma = m(runif(n, 0.5, 1.5)),
      phi = m(runif(n, 1.5, 4.0)),
      xi = m(rep(0, n))
    ),
    data = list()
  )
  if (!is.null(trials)) prep$data$trials <- trials
  prep
}

# Families whose mean is a closed form in `dpars` (plus trials).
# Multi-category and ordinal kernels need a category axis and are
# compared through the fixtures instead.
kernel_is_pointwise <- function(nm) {
  !sub("^posterior_epred_", "", nm) %in% c(
    "categorical", "multinomial", "dirichlet", "dirichlet2",
    "dirichlet_multinomial", "logistic_normal", "ordinal",
    "cumulative", "sratio", "cratio", "acat", "custom", "mixture",
    "gaussian_mv", "student_mv"
  )
}


test_that("every shared pointwise epred kernel matches brms", {
  shared <- sort(Filter(kernel_is_pointwise, intersect(
    epred_kernel_names("mvgam"), epred_kernel_names("brms")
  )))
  # An empty set would satisfy the loop without comparing anything.
  expect_gt(length(shared), 10L)

  prep <- mk_prep(trials = c(10L, 20L, 5L, 8L))
  checked <- 0L
  for (nm in shared) {
    mv <- get(nm, envir = asNamespace("mvgam"))
    br <- get(nm, envir = asNamespace("brms"))
    a <- tryCatch(mv(prep), error = function(e) e)
    b <- tryCatch(br(prep), error = function(e) e)
    # A family this synthetic prep cannot drive errors on both sides
    # and is left to the fixture comparison.
    if (inherits(a, "error") || inherits(b, "error")) next
    checked <- checked + 1L
    expect_equal(unname(as.matrix(a)), unname(as.matrix(b)))
  }
  # The families carrying an extra parameter in their mean are the
  # ones worth this test, so a run that reached only the trivial
  # `mu` kernels has not covered it.
  expect_gt(checked, 10L)
})


test_that("the mean of a zero-inflated or hurdle family deflates mu", {
  # Pinning the three shapes directly, so a kernel that quietly
  # became the inverse link is caught by value rather than only by
  # disagreeing with brms if brms changed too.
  prep <- mk_prep()
  mu <- prep$dpars$mu
  zi <- prep$dpars$zi
  hu <- prep$dpars$hu
  shape <- prep$dpars$shape
  ns <- asNamespace("mvgam")

  expect_equal(
    get("posterior_epred_zero_inflated_poisson", envir = ns)(prep),
    mu * (1 - zi)
  )
  expect_equal(
    get("posterior_epred_hurdle_poisson", envir = ns)(prep),
    mu / (1 - exp(-mu)) * (1 - hu)
  )
  expect_equal(
    get("posterior_epred_hurdle_negbinomial", envir = ns)(prep),
    mu / (1 - (shape / (mu + shape))^shape) * (1 - hu)
  )
  # None of the three is the inverse link on its own.
  expect_false(isTRUE(all.equal(
    get("posterior_epred_zero_inflated_poisson", envir = ns)(prep), mu
  )))
})


test_that("a binomial mean scales the probability by the trials", {
  trials <- c(10L, 20L, 5L, 8L)
  prep <- mk_prep(trials = trials)
  prep$dpars$mu <- matrix(runif(20L), nrow = 5L, ncol = 4L)
  ns <- asNamespace("mvgam")
  got <- get("posterior_epred_binomial", envir = ns)(prep)
  expect_equal(
    unname(as.matrix(got)),
    unname(prep$dpars$mu * matrix(trials, 5L, 4L, byrow = TRUE))
  )
})
