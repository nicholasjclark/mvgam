# The tests of test-update.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# ---- mvgam_update_call helper --------------------------------------

test_that("mvgam_update_call inherits what the call leaves out", {
  stub <- make_update_stub()
  # A refit that reverted to random starts would undo the reason the
  # original fit asked for Pathfinder
  stub$init <- "pathfinder"
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list())
  for (slot in c("formula", "data", "family", "prior", "backend",
                 "algorithm", "init")) {
    expect_identical(out[[slot]], stub[[slot]], label = slot)
  }
  # A caller's own value wins
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list(init = "0"))
  expect_identical(out$init, "0")
})

test_that("mvgam_update_call applies formula. through brms", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = ~ . + z, newdata = NULL, dots = list()
  )
  expect_identical(deparse(out$formula$formula), "y ~ x + z")
  # A dropped term takes its inherited prior rows with it, as in brms.
  # brms refuses a row with no parameter.
  out <- mvgam_update_call(
    stub, formula. = ~ . - x, newdata = NULL, dots = list()
  )
  expect_false("x" %in% out$prior$coef)
  expect_true("Intercept" %in% out$prior$class)
})

test_that(
  "mvgam_update_call drops the inherited warmup when iter is overridden", {
  # `warmup` cannot be inherited on its own: mvgam derives it as
  # `iter %/% 2`. Pairing the original fit's warmup with a smaller
  # user-supplied `iter` asks Stan for a negative sampling count. The fit
  # then fails with a message that names neither argument.
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

  # An explicit warmup is kept when a new iter is supplied
  out2 <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                            dots = list(iter = 400L, warmup = 100L))
  expect_identical(out2$warmup, 100L)

  # and both are still inherited when neither is overridden
  out3 <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                            dots = list())
  expect_identical(out3$iter, 1500L)
  expect_identical(out3$warmup, 750L)
})

test_that("update() refits a jsdgam through jsdgam()", {
  set.seed(1)
  dat <- expand.grid(week = 1:20, species = factor(paste0("sp", 1:4)))
  dat$y <- rpois(nrow(dat), 2)
  # An empty observation formula, which mvgam fills with a pinned
  # placeholder column the refit has to rebuild.
  prefit <- jsdgam(y ~ 0, data = dat, unit = week, species = species,
                   family = poisson(), n_lv = 2L, run_model = FALSE,
                   silent = 2, validate = FALSE)
  # The summary names the data as the call did
  expect_identical(prefit$data.name, "dat")
  # The refit frame is the user's own, without the 'time' and 'series'
  # columns jsdgam() added for 'week' and 'species'.
  args <- jsdgam_update_call(prefit, NULL, NULL, list())
  expect_setequal(names(args$data), names(dat))
  # An override reaches jsdgam() and changes the factor count alone.
  refit <- update(prefit, n_lv = 1L, run_model = FALSE)
  expect_equal(refit$standata$N_lv_trend, 1)
  # The refit names its data as the original call did. Its
  # reproduction call writes a literal argument as given.
  mgp <- update(prefit, loadings_prior = "mgp", run_model = FALSE)
  expect_identical(mgp$data.name, "dat")
  expect_match(methods_md(mgp), "loadings_prior += \"mgp\"")
  # The factor count is Stan data. With `recompile = FALSE`, the unchanged
  # program is passed. A family that adds a parameter changes the program.
  expect_s3_class(
    update(prefit, n_lv = 1L, recompile = FALSE, run_model = FALSE),
    "jsdgam"
  )
  expect_error(
    update(prefit, family = brms::negbinomial(), recompile = FALSE),
    "recompile = FALSE"
  )
  # The priors are stored under one name. A new prior joins them. A
  # dropped term takes its rows with it.
  dat$x <- rnorm(nrow(dat))
  with_prior <- jsdgam(y ~ x, data = dat, unit = week, species = species,
                       family = poisson(), n_lv = 2L,
                       priors = brms::prior(normal(0, 1), class = b),
                       run_model = FALSE, silent = 2, validate = FALSE)
  joined <- jsdgam_update_call(
    with_prior, NULL, NULL,
    list(prior = brms::prior(normal(0, 2), class = Intercept))
  )$prior
  expect_setequal(joined$class, c("Intercept", "b"))
  dropped <- jsdgam_update_call(with_prior, ~ . - x, NULL, list())$prior
  expect_false("b" %in% dropped$class)
  # A fit without the stored arguments cannot be rebuilt.
  prefit$jsdgam_args <- NULL
  expect_error(update(prefit), "arguments stored on the fit")
})

test_that("getCall reports the call the user wrote", {
  # Every entry point reaches the fitting pipeline through `do.call()`,
  # which substitutes each argument's value for its symbol. The stored
  # call must keep the user's symbols, so `deparse(getCall(fit))` shows
  # the call as written.
  set.seed(1L)
  d <- data.frame(time = 1:30, series = factor("s1"), x = rnorm(30))
  d$y <- rpois(30, 3)
  fit <- mvgam(y ~ x, data = d, family = poisson(), run_model = FALSE,
               validate = FALSE)

  cl <- getCall(fit)
  expect_identical(cl[[1L]], as.name("mvgam"))
  # The arguments are the symbols and calls that were typed. They are not
  # the objects they evaluate to. This keeps the deparse short and
  # re-evaluable.
  expect_identical(cl$data, as.name("d"))
  expect_identical(cl$family, quote(poisson()))
  expect_lt(length(deparse(cl)), 5L)
})

test_that("a by-factor GP with a class-wide prior refits", {
  # The class prior was copied into each level's row and stored as the
  # user's. A refit handed the rows back. brms then stopped with
  # `all(index == 1L) is not TRUE`, which broke every `lfo_cv()` refit.
  set.seed(1)
  dat <- expand.grid(time = 1:20, series = factor(c("a", "b")))
  dat$y <- rpois(nrow(dat), 5)
  tf <- ~ gp(time, k = 6, by = lv_axis()) + AR(p = 1, cor = TRUE)
  prefit <- mvgam(y ~ 1, trend_formula = tf, data = dat, family = poisson(),
                  prior = brms::prior(exponential(1), class = sdgp_trend),
                  run_model = FALSE, silent = 2, validate = FALSE)
  expect_s3_class(
    update(prefit, newdata = dat[dat$time <= 15, ], run_model = FALSE,
           validate = FALSE),
    "mvgam"
  )
  refit <- update(prefit, run_model = FALSE, validate = FALSE,
                  prior = brms::prior(exponential(2), class = sdgp_trend))
  expect_identical(refit$prior$prior[refit$prior$source == "user"],
                   "exponential(2)")
  # brms takes no prior on one level. The refusal names the class form.
  expect_error(
    update(prefit, run_model = FALSE, validate = FALSE,
           prior = brms::prior(exponential(2), class = sdgp_trend,
                               coef = gptimeseriesa)),
    "class = sdgp_trend)"
  )
})
