# What a `run_model = FALSE` stub gives the caller.
#
# A prefit carries the program, its data, the formula and the axis
# record, and carries no posterior. A method needing draws has to say
# so. Seventeen said so and twenty-five met the caller with
# `posterior::as_draws_matrix()` failing on the empty slot:
#
#     Don't know how to transform an object of class 'NULL' to any
#     supported draws format.
#
# That names neither the state nor the argument that produced it, and
# a list of the twenty-five would have gone stale. The claim below is
# stated over the whole S3 registry instead: whatever a method does
# with a prefit, that internal failure is never what the caller gets.
#
# Methods refusing for their own reasons stay as they are. A VAR
# surface needs a VAR trend and an ordination needs latent factors,
# and neither is about the posterior.

internal_draws_error <- "Don't know how to transform an object of class"

build_prefit <- function() {
  set.seed(3)
  dat <- data.frame(
    y = rpois(40, 3),
    time = rep(1:20, times = 2L),
    x = rnorm(40),
    series = factor(rep(c("a", "b"), each = 20L))
  )
  suppressWarnings(mvgam(
    y ~ x, data = dat, family = poisson(),
    run_model = FALSE, silent = 2
  ))
}


test_that("a prefit failure names the state it is in", {
  pf <- build_prefit()
  expect_s3_class(pf, "mvgam_prefit")

  # Arguments a method needs before it can be asked anything.
  needs <- list(posterior_smooths = list(smooth = "s(x)"))
  # These refit a model, which belongs in tests/local. `update()` on
  # a prefit reaches the sampler once the recorded `algorithm` stops
  # blocking it, which is the fix below and the reason it stays out
  # of this loop.
  refits <- c("update", "kfold", "lfo_cv", "add_criterion",
              "loo_subsample", "loo_moment_match")
  gens <- sort(sub("\\.mvgam$", "", as.character(
    utils::.S3methods(class = "mvgam")
  )))
  gens <- setdiff(gens, refits)
  # The registry is the source. A method added later is driven here
  # without an edit to this file.
  expect_gt(length(gens), 50L)

  offenders <- character(0)
  # The registry lists `print()`, which emits the prefit block.
  # Capturing that keeps the console quiet while every method still
  # runs.
  capture.output(
    for (gen in gens) {
      args <- c(list(pf), needs[[gen]])
      out <- suppressWarnings(caught_error(do.call(gen, args)))
      if (!is.null(out) &&
            grepl(internal_draws_error, conditionMessage(out),
                  fixed = TRUE)) {
        offenders <- c(offenders, gen)
      }
    }
  )
  # Named rather than counted: a failure here states which methods.
  expect_identical(paste(offenders, collapse = ", "), "")
})


test_that("a prefit states what it has and what it lacks", {
  pf <- build_prefit()

  # The program and its data are why the mode exists.
  expect_type(stancode(pf), "character")
  expect_type(standata(pf), "list")
  expect_identical(nobs(pf), 40L)
  expect_identical(nrow(model.frame(pf)), 40L)

  # A method needing draws names the state and the argument that
  # produced it, rather than failing inside posterior.
  err <- expect_error(summary(pf))
  expect_match(conditionMessage(err), "run_model", fixed = TRUE)
  expect_match(conditionMessage(err), "No fitted model", fixed = TRUE)
})


test_that("a VAR prefit names its trend and the posterior it needs", {
  # The registry loop above builds a trendless prefit. The VAR
  # methods stop at the trend-type gate there, before their draws.
  # A VAR prefit drives them one step further.
  set.seed(11)
  dat <- data.frame(
    y = rnorm(60), time = rep(1:20, times = 3L),
    series = factor(rep(c("a", "b", "c"), each = 20L))
  )
  pf <- suppressWarnings(mvgam(
    y ~ 1, trend_formula = ~ VAR(cor = TRUE), data = dat,
    family = gaussian(), run_model = FALSE, silent = 2
  ))
  expect_s3_class(pf, "mvgam_prefit")

  # `trend_components` is empty until a fit runs, and the type is in
  # `trend_metadata`. Both resolvers take the second.
  expect_null(pf$trend_components$types)
  expect_identical(get_trend_type(pf), "VAR")
  expect_identical(detect_var_trend(pf), "VAR")

  # Each method states the posterior it needs, in place of the
  # trend-type refusal and in place of the internal draws error.
  for (meth in list(function(x) irf(x, h = 2L),
                    function(x) fevd(x, h = 2L),
                    posterior_transition_matrix)) {
    err <- expect_error(meth(pf), "requires a fitted Stan model")
    expect_match(conditionMessage(err), "run_model", fixed = TRUE)
    expect_false(grepl(internal_draws_error, conditionMessage(err),
                       fixed = TRUE))
  }
})


test_that("mvgam_multiple gives one prefit when run_model is FALSE", {
  set.seed(9)
  mk <- function() {
    data.frame(
      y = rpois(20, 3),
      time = seq_len(20L),
      series = factor(rep("s1", 20L))
    )
  }
  # Pooling compares `variables()` across the fits. A stub refuses
  # that call, and an unfitted request that reached the pooler failed
  # inside it. One stub comes back from the first dataset, under
  # either `combine`.
  for (comb in c(TRUE, FALSE)) {
    pf <- suppressWarnings(mvgam_multiple(
      y ~ 1, data_list = list(mk(), mk()), family = poisson(),
      combine = comb, run_model = FALSE, silent = 2
    ))
    expect_s3_class(pf, "mvgam_prefit")
    expect_false(inherits(pf, "mvgam_pooled"))
    expect_null(pf$fit)
    expect_type(stancode(pf), "character")
    expect_identical(nobs(pf), 20L)
  }
})


test_that("a prefit built inside a function omits that function's locals", {
  # A formula records the environment it was written in, and a model
  # frame's `terms` attribute records the one it was built in.
  # `saveRDS()` writes an unnamed environment by value, and a model
  # fitted inside a function then writes every local of that function
  # into the file. `utils::object.size()` does not follow an
  # environment; the saved file's size does.
  set.seed(5)
  dat <- data.frame(
    y = rpois(20, 3),
    time = seq_len(20L),
    x = rnorm(20),
    series = factor(rep("s1", 20L))
  )
  build <- function(d) {
    ballast <- rnorm(5e5)
    suppressWarnings(mvgam(
      y ~ x, trend_formula = ~ AR(p = 1), data = d,
      family = poisson(), run_model = FALSE, silent = 2
    ))
  }
  pf <- build(dat)

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(pf, path)
  ballast_mb <- as.numeric(utils::object.size(numeric(5e5))) / 1024^2
  expect_gt(ballast_mb, 3)
  expect_lt(file.size(path) / 1024^2, ballast_mb / 3)

  # Each carrier is named. A regression then states which one came
  # back, beyond the file having grown.
  expect_false(exists("ballast", envir = environment(pf$formula),
                      inherits = TRUE))
  expect_false(exists("ballast", envir = environment(pf$trend_call),
                      inherits = TRUE))
  tm <- attr(pf$obs_model$data, "terms")
  expect_s3_class(tm, "terms")
  expect_false(exists("ballast", envir = environment(tm),
                      inherits = TRUE))
})


test_that("update rebuilds a prefit past the algorithm it recorded", {
  pf <- build_prefit()
  # A prefit records `algorithm = "none"`, which no backend lists.
  # Handing that back refused the very call that takes a prefit on to
  # a fit. `run_model = FALSE` keeps the check free of sampling.
  again <- suppressWarnings(update(pf, run_model = FALSE))
  expect_s3_class(again, "mvgam_prefit")
  expect_type(stancode(again), "character")
})
