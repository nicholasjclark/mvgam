# One question, asked of the whole `mvgam` method surface: does a
# method that names every argument it reads refuse one it does not?
#
# A misspelling lands in `...`, and a method that drops `...` then
# runs on the default it was asked to override. Nothing is raised and
# the answer is finite and plausible, so the caller reads it as an
# answer to the question they asked. `ordinate(fit, axes = c(1, 5))`
# drew factors 1 and 2 that way, and `incl_autocor` misspelt returned
# a marginal prediction where a conditional one was wanted.
#
# The list below is derived from the package's own S3 registry rather
# than written out, so a method added later without the guard fails
# here, and so does one whose guard is removed. A method that belongs
# outside the rule is named in `forwards_dots` with the reason: either
# its generic's owner passes arguments of its own through dispatch, or
# the method forwards `...` to a callee whose formals are the contract.
#
# Run with:
#   testthat::test_file("tests/local/test-dots-refusal.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

# A smooth and a grouping, so the methods that need either reach
# their own body rather than refusing the fit.
fit_path <- cache_path("val_dots_refusal.rds")
fit <- if (file.exists(fit_path)) {
  message("[cache] Loading dots-refusal fit.")
  readRDS(fit_path)
} else {
  set.seed(7)
  dat <- data.frame(
    time = rep(1:30, times = 2L),
    series = factor(rep(c("a", "b"), each = 30L)),
    g = factor(rep(letters[1:4], length.out = 60L)),
    x = rnorm(60)
  )
  dat$y <- rpois(60, exp(0.6 + 0.3 * dat$x))
  f <- mvgam(y ~ s(x, k = 4) + (1 | g), data = dat,
             family = poisson(), chains = 2, silent = 2)
  saveRDS(f, fit_path)
  f
}

# Arguments a method needs before it can be asked anything, so the
# call reaches the guard instead of stopping on a missing argument.
needs_args <- list(
  posterior_smooths = list(smooth = "s(x)"),
  add_criterion = list(criterion = "loo")
)

# Methods outside the rule, each with the reason it is outside.
forwards_dots <- c(
  # insight and marginaleffects call these through their own generics
  # and pass arguments of their own (`effects`, `component`,
  # `verbose`, `newdata`), so refusing here refuses them.
  "find_formula", "find_predictors", "find_response", "find_variables",
  "get_data", "model_info", "get_coef", "get_vcov", "set_coef",
  "get_group_names", "get_predict",
  # These forward `...` to a callee whose formals are the contract.
  "nuts_params", "log_posterior", "hypothesis", "bridge_sampler",
  "bayes_factor", "posterior_interval", "predictive_interval",
  "predictive_error", "loo_predict", "loo_epred", "loo_linpred",
  "loo_predictive_interval", "LOO", "WAIC", "loo", "waic",
  "loo_compare", "loo_model_weights", "loo_R2", "add_criterion",
  "pp_average", "posterior_average", "logLik", "bayes_R2", "parnames",
  "mcmc_plot", "pairs", "pp_check", "plot", "conditional_effects",
  "update", "residuals", "ensemble", "score", "smooths", "glance",
  # Refused already, by their own hand-written checks.
  "forecast", "hindcast", "family", "get_prior", "default_prior",
  # Needs a refit to answer, so it is driven in its own file.
  "kfold", "lfo_cv", "loo_subsample", "loo_moment_match"
)

registered <- sub("\\.mvgam$", "", as.character(
  utils::.S3methods(class = "mvgam")
))
covered <- sort(setdiff(registered, forwards_dots))


test_that("every closed method on the mvgam class refuses an unknown argument", {
  # The set is read from the registry, so this states the rule rather
  # than a list: a method added without the guard arrives here on its
  # own and fails until it is guarded or given a reason above.
  # Two refusals are possible and both name the argument. Where the
  # generic itself declares `...` the method's own
  # `check_dots_empty()` answers; where the generic does not, such as
  # `posterior::ndraws(x)`, R refuses at dispatch before the method
  # runs. What matters to a caller is the same either way.
  expect_gt(length(covered), 30L)
  for (gen in covered) {
    args <- c(list(fit), needs_args[[gen]], list(zzz_unknown = 1))
    err <- expect_error(do.call(gen, args), label = gen)
    expect_match(conditionMessage(err), "zzz_unknown", label = gen)
  }
})


test_that("the argument a refusal names is the one the caller wrote", {
  # `check_dots_empty()` names each offending argument, which is what
  # a caller acts on; a bare refusal would leave them hunting.
  err <- expect_error(summary(fit, zzz_unknown = 1))
  expect_match(conditionMessage(err), "zzz_unknown")
})


test_that("a method takes a draw count or draw indices, never both", {
  # One boundary guard, reached through every method that takes the
  # pair. Given both, the resolver honours the indices and the count
  # is dropped, which is this file's subject in another spelling.
  for (call in list(
    function(...) fitted(fit, ...),
    function(...) predict(fit, ...),
    function(...) residuals(fit, ...),
    function(...) log_lik(fit, ...),
    function(...) posterior_epred(fit, ...),
    function(...) posterior_linpred(fit, ...),
    function(...) posterior_predict(fit, ...),
    function(...) conditional_smooths(fit, ...)
  )) {
    expect_error(call(ndraws = 5, draw_ids = 1:5), "Specify only one")
  }
  # Either alone is what the methods are for, so the guard cannot be
  # reading the pair as present when one is absent.
  expect_equal(nrow(posterior_epred(fit, draw_ids = 1:5)), 5L)
  expect_equal(nrow(posterior_epred(fit, ndraws = 5)), 5L)
})


test_that("a smooth view can be scoped to one response", {
  # `plot(type = "smooths")` passes `resp` on, and the method had no
  # argument of that name to receive it. A univariate fit carries no
  # response on its terms, and naming its one response keeps them.
  one <- names(response_columns(fit))
  expect_length(one, 1L)
  expect_named(
    conditional_smooths(fit, resp = one),
    names(conditional_smooths(fit))
  )
  expect_error(conditional_smooths(fit, resp = "zzz_absent"),
               "not a response")
})


test_that("loo and waic refuse the parity arguments they cannot honour", {
  # Both accept the brms signature. `resp` and `save_psis` are
  # honoured; the rest name refits and multi-model ranking that this
  # package answers elsewhere, and each is refused where supplied.
  expect_error(loo(fit, k_threshold = 1), "cannot honour")
  expect_error(loo(fit, compare = FALSE), "cannot honour")
  expect_error(loo(fit, model_names = "m"), "cannot honour")
  expect_error(loo(fit, moment_match_args = list(k = 1)), "cannot honour")
  expect_error(waic(fit, compare = FALSE), "cannot honour")
  expect_error(waic(fit, model_names = "m"), "cannot honour")
  # The two that are honoured still answer.
  expect_s3_class(suppressWarnings(loo(fit, save_psis = TRUE)), "psis_loo")
})
