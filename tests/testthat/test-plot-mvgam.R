# plot.mvgam dispatch tests — mock each downstream target so we
# don't need a Stan fit, then confirm the right one was called
# with the right args.

.make_stub_mvgam <- function() {
  obj <- list(
    formula = stats::as.formula("y ~ 1"),
    family = stats::gaussian(),
    data = data.frame(y = rnorm(20L)),
    obs_data = data.frame(y = rnorm(20L)),
    fit = list()
  )
  class(obj) <- c("mvgam", "brmsfit")
  obj
}

# Internal helper: install a sentinel mock for `fn_name` in
# mvgam's namespace, returning a flag list mutated when called.
.install_route_mock <- function(fn_name, value = "called") {
  flag <- new.env(parent = emptyenv())
  flag$called <- FALSE
  flag$args <- NULL
  stub <- function(...) {
    flag$called <- TRUE
    flag$args <- list(...)
    value
  }
  bindings <- list(stub)
  names(bindings) <- fn_name
  do.call(
    testthat::local_mocked_bindings,
    c(bindings, list(.env = parent.frame()))
  )
  flag
}

test_that("default type is 'residuals' and routes to mvgam_resid_panel", {
  f <- .install_route_mock("mvgam_resid_panel", value = "panel")
  out <- plot(.make_stub_mvgam())
  expect_true(f$called)
  expect_equal(out, "panel")
})

test_that("type = 'smooths' routes to conditional_smooths", {
  # Mock conditional_smooths so the routing test does not need
  # a real fit. Returning a minimal mvgam_conditional_smooths
  # lets `wrap_effects_list()`'s S3 `plot(eff_list, plot = FALSE)`
  # dispatch build a themed ggplot; asserting a ggplot class
  # documents the contract that `plot(mod, type = "smooths")`
  # returns a renderable plot object.
  dummy_df <- data.frame(
    env = seq(-1, 1, length.out = 5L),
    estimate__ = c(-0.4, 0.1, 0.5, 0.1, -0.4),
    lower__    = c(-0.9, -0.4, 0.0, -0.4, -0.9),
    upper__    = c( 0.1,  0.6, 1.0,  0.6,  0.1)
  )
  attr(dummy_df, "effects")   <- "env"
  attr(dummy_df, "surface")   <- FALSE
  attr(dummy_df, "response")  <- "s(env)"
  attr(dummy_df, "spaghetti") <- NULL
  attr(dummy_df, "points")    <- NULL
  cs_mock <- structure(
    list(`s(env)` = dummy_df),
    class = c("mvgam_conditional_smooths", "list")
  )
  f <- .install_route_mock("conditional_smooths", value = cs_mock)
  out <- plot(.make_stub_mvgam(), type = "smooths")
  expect_true(f$called)
  expect_s3_class(out, "ggplot")
})

test_that("type = 'factors' routes to plot_factors", {
  f <- .install_route_mock("plot_factors", value = "fac")
  expect_equal(plot(.make_stub_mvgam(), type = "factors"), "fac")
  expect_true(f$called)
})

test_that("type = 'series' routes to plot_mvgam_series", {
  f <- .install_route_mock("plot_mvgam_series", value = "ts")
  expect_equal(plot(.make_stub_mvgam(), type = "series"), "ts")
  expect_true(f$called)
})

test_that("type = 'trend' calls hindcast then plot.mvgam_forecast", {
  fc_stub <- structure(list(), class = "mvgam_forecast")
  f_hc <- .install_route_mock("hindcast", value = fc_stub)
  f_plot <- .install_route_mock("plot.mvgam_forecast", value = "tr_plot")
  expect_equal(plot(.make_stub_mvgam(), type = "trend"), "tr_plot")
  expect_true(f_hc$called)
  expect_true(f_plot$called)
})

test_that("unknown type errors via match.arg", {
  expect_error(
    plot(.make_stub_mvgam(), type = "uncertainty")
  )
})

test_that("non-mvgam input errors via checkmate assertion", {
  expect_error(plot.mvgam(list()))
})
