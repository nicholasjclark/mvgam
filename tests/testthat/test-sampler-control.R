# `lift_sampler_control()` moves the NUTS settings a user wrote beside
# the formula into `control`, which is where both backends read them:
# rstan passes the list to `rstan::sampling()`, and the cmdstanr path
# flattens its names into `model$sample()`.
#
# `fit_model()` is called with an explicit argument list and no dots,
# so a bare spelling reached neither backend. Measured on a poisson
# AR(1) before the lift, `adapt_delta = 0.99` sampled at delta 0.8
# while `control = list(adapt_delta = 0.99)` sampled at 0.99, and the
# call looked like it had addressed the divergences it was raised for.

test_that("lift_sampler_control carries control through untouched", {
  # Nothing bare to move: a NULL control stays NULL and a supplied one
  # is returned as it stands.
  expect_null(mvgam:::lift_sampler_control(list(iter = 500)))
  ctl <- list(adapt_delta = 0.9)
  expect_identical(
    mvgam:::lift_sampler_control(list(iter = 500), ctl),
    ctl
  )
})


test_that("lift_sampler_control moves a bare NUTS setting into control", {
  out <- mvgam:::lift_sampler_control(
    list(adapt_delta = 0.99, max_treedepth = 13L, iter = 500)
  )
  expect_identical(out$adapt_delta, 0.99)
  expect_identical(out$max_treedepth, 13L)
  # An argument that is not a sampler setting stays where it was.
  expect_null(out$iter)
})


test_that("lift_sampler_control keeps a supplied control beside the lift", {
  out <- mvgam:::lift_sampler_control(
    list(max_treedepth = 13L),
    list(adapt_delta = 0.99)
  )
  expect_identical(out$adapt_delta, 0.99)
  expect_identical(out$max_treedepth, 13L)
})


test_that("lift_sampler_control refuses one setting spelled twice", {
  # Two spellings of one setting name two different values, and the
  # resolver would have honoured whichever it read second.
  expect_error(
    mvgam:::lift_sampler_control(
      list(adapt_delta = 0.99),
      list(adapt_delta = 0.95)
    ),
    "given twice"
  )
})
