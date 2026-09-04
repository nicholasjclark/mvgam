# A post-fit answer is assembled from several reads of the posterior:
# the observation predictor, the trend predictor, the process errors,
# the distributional parameters, the ordinal thresholds. They only
# describe the same model if they describe the same iterations.
#
# The failure this guards against is silent. Handed a draw count
# rather than draw indices, each read subsampled on its own, and the
# terms that were then added together came from different iterations.
# Nothing in the output looks wrong: the rows are all finite, all
# plausible, and all mismatched. On a fit with a trend-side formula,
# asking for the whole posterior once returned a thousand rows of
# which one paired a predictor with its own trend.
#
# The test below states the invariant that catches it. Every row a
# subsampled prediction produces must be a row the fully specified
# prediction also produces: subsetting draws may drop rows and reorder
# them, but it cannot invent a combination that no single draw gives.
#
# Four models, because the failure needs somewhere to hide. A trend
# carrying only fixed effects composes two reads; one carrying a
# smooth and a random effect composes four, and misaligning any of
# them stays finite. The ordinal fit adds thresholds, which are read
# separately again. The plain fit is the one whose latent state can
# be checked against a window of itself.
#
# The same four carry the marginaleffects and insight surface at the
# end of the file. It needs a covariate on either side of the model,
# a response with categories, and a frame holding columns the formula
# never reads, which is what these already are.
#
# Run with:
#   testthat::test_file("tests/local/test-draw-alignment.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

# Fits are cached because every assertion here is about which draws
# came back, not about what the sampler found, so refitting changes
# nothing a check reads. Written under a temporary name and moved into
# place, so an interrupted run cannot leave a truncated file behind.
fit_cached <- function(name, ...) {
  # The cache is named for this file rather than shared, so the models
  # asserted on are the ones built here. A shared name would let a
  # differently specified fit of the same name answer instead.
  path <- cache_path(paste0("val_align_", name, ".rds"))
  if (file.exists(path)) {
    cat("[cache]", name, "\n")
    return(readRDS(path))
  }
  cat("[fit  ]", name, "\n")
  fit <- mvgam(
    ..., chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  fit
}

sim_ar1 <- function(n, ar, sd) {
  out <- numeric(n)
  out[1] <- rnorm(1, 0, sd / sqrt(1 - ar^2))
  for (t in 2:n) out[t] <- ar * out[t - 1] + rnorm(1, 0, sd)
  out
}

set.seed(42L)
n_time <- 30L
latent <- sim_ar1(n_time, 0.7, 0.5)
z <- seq(-2, 2, length.out = n_time)
z_effect <- 0.5 * sin(z * pi)
dat <- data.frame(
  y = rpois(n_time, exp(2 + latent + z_effect)),
  x = rnorm(n_time),
  z = z,
  time = seq_len(n_time),
  series = factor("s1"),
  grp = factor(rep(letters[1:6], each = 5))
)

set.seed(456L)
n_ord <- 30L
ord_latent <- 1.0 + 0.5 * rnorm(n_ord)
dat_ord <- data.frame(
  y = ordered(cut(ord_latent, breaks = c(-Inf, -0.5, 0.5, 1.5, Inf),
                  labels = c("Low", "Med", "High", "VHigh"))),
  x = rnorm(n_ord),
  z = rnorm(n_ord),
  time = seq_len(n_ord),
  series = factor("s1")
)


# -- Fits -------------------------------------------------------------

fit_plain <- fit_cached(
  "ar1_fx", formula = y ~ 1 + x, trend_formula = ~ AR(p = 1),
  data = dat, family = poisson()
)
fit_trend <- fit_cached(
  "ar1_fx_trend", formula = y ~ 1, trend_formula = ~ x + AR(p = 1),
  data = dat, family = poisson()
)
fit_re_smooth <- fit_cached(
  "ar1_re_smooth_trend", formula = y ~ 1,
  trend_formula = ~ x + (1 | grp) + s(z) + AR(p = 1),
  data = dat, family = poisson()
)
fit_ord <- fit_cached(
  "cumulative_fx", formula = y ~ 1 + x + z, trend_formula = ~ ZMVN(),
  data = dat_ord, family = cumulative()
)

# The alignment claims need more draws than the indices they name.
stopifnot(ndraws(fit_plain) >= 300L, ndraws(fit_trend) >= 300L)


# Rows as comparable keys. Rounding guards against a last-bit
# difference between two routes to the same arithmetic.
draw_rows <- function(x) {
  unname(apply(round(as.matrix(x), 10), 1, paste, collapse = "|"))
}


test_that("a subsampled linear predictor pairs its own trend", {
  for (fit in list(fit_trend, fit_re_smooth)) {
    total <- ndraws(fit)
    # The reference: every draw, named explicitly, so each row is the
    # observation predictor and the trend predictor of one iteration.
    reference <- draw_rows(
      posterior_linpred(fit, draw_ids = seq_len(total),
                        process_error = FALSE)
    )
    # A count below the total.
    subset_rows <- draw_rows(
      posterior_linpred(fit, ndraws = 20L, process_error = FALSE)
    )
    expect_length(subset_rows, 20L)
    expect_true(all(subset_rows %in% reference))
    # A count covering the whole posterior. This is the case that used
    # to fail hardest: both reads claimed every draw, and each
    # returned them in its own random order.
    all_rows <- draw_rows(
      posterior_linpred(fit, ndraws = total, process_error = FALSE)
    )
    expect_setequal(all_rows, reference)
  }
})


test_that("a count and the indices it stands for agree", {
  fit <- fit_trend
  ids <- c(3L, 11L, 47L, 300L)
  by_ids <- posterior_linpred(fit, draw_ids = ids, process_error = FALSE)
  expect_equal(nrow(by_ids), length(ids))
  # Naming the same draws twice gives the same answer, so nothing
  # below the boundary is re-drawing.
  expect_equal(
    by_ids,
    posterior_linpred(fit, draw_ids = ids, process_error = FALSE)
  )
  # Every surface honours the count it was given.
  for (n in c(15L, ndraws(fit))) {
    expect_equal(nrow(posterior_epred(fit, ndraws = n)), n)
    expect_equal(nrow(posterior_predict(fit, ndraws = n)), n)
    expect_equal(nrow(log_lik(fit, ndraws = n)), n)
    expect_equal(nrow(predict(fit, ndraws = n, summary = FALSE)), n)
  }
  # And refuses one it cannot honour, rather than quietly using all.
  expect_error(posterior_epred(fit, ndraws = ndraws(fit) + 1L),
               "more draws than the posterior holds")
})


test_that("ordinal thresholds follow the draws of their predictor", {
  fit <- fit_ord
  draws <- posterior::as_draws_matrix(fit$fit)
  ids <- c(2L, 9L, 40L)
  thres <- mvgam:::extract_ordinal_thresholds(
    fit, ndraws = length(ids), draw_ids = ids
  )
  expect_equal(nrow(thres), length(ids))
  # The thresholds are the ones sampled at those iterations, not the
  # first few rows of the posterior.
  expect_equal(as.numeric(thres[, 1]),
               as.numeric(draws[ids, "Intercept[1]"]))
  expect_false(isTRUE(all.equal(
    as.numeric(draws[ids, "Intercept[1]"]),
    as.numeric(draws[seq_along(ids), "Intercept[1]"])
  )))
  # And the surfaces built on them honour a count.
  for (n in c(25L, ndraws(fit))) {
    expect_equal(nrow(log_lik(fit, ndraws = n)), n)
    expect_equal(nrow(posterior_predict(fit, ndraws = n)), n)
  }
})


# The three invariants below guard the composition of the trend's
# contribution rather than the choice of draws. A violation of any
# one stays finite and plausibly scaled, so nothing but the check
# itself catches it.


test_that("a scored row reads the state of its own time", {
  # Looking the latent state up by position within whatever frame it
  # is handed would score the later half of a series against the
  # state of the earlier half, with the right shape and no warning.
  # Scoring a window has to agree with scoring everything and keeping
  # that window's columns.
  fit <- fit_plain
  n <- nrow(fit$data)
  window <- seq.int(n - 9L, n)
  full <- log_lik(fit)
  part <- log_lik(fit, newdata = fit$data[window, , drop = FALSE])
  expect_equal(dim(part), c(nrow(full), length(window)))
  expect_equal(unname(as.matrix(part)),
                unname(as.matrix(full)[, window]),
                tolerance = 1e-8)
})


test_that("the trend's innovations are composed once", {
  # `posterior_epred()` and `posterior_predict()` each drew a second,
  # independent set of innovations on top of the set the linear
  # predictor already carried, so a marginal prediction held twice the
  # process variance. Drawn from one seed, the expectation has to be
  # exactly the inverse link of the predictor: a second draw anywhere
  # in the chain breaks the identity.
  fit <- fit_plain
  set.seed(99L)
  ep <- posterior_epred(fit)
  set.seed(99L)
  lp <- posterior_linpred(fit)
  expect_equal(unname(as.matrix(ep)),
                unname(as.matrix(fit$family$linkinv(lp))),
                tolerance = 1e-10)
})


test_that("only a sampled trend makes a prediction differ across calls", {
  # Conditioning reads the state the model inferred, so it holds no
  # RNG and answers the same way twice. The default marginal surface
  # holds none either: `process_error = FALSE` contributes the trend's
  # deterministic submodel, so it repeats too. Drawing innovations is
  # what makes an answer vary, and is the documented reason a seed is
  # needed for a reproducible one.
  fit <- fit_plain
  expect_equal(
    posterior_epred(fit, incl_autocor = TRUE),
    posterior_epred(fit, incl_autocor = TRUE)
  )
  expect_equal(log_lik(fit), log_lik(fit))
  expect_equal(
    posterior_epred(fit, incl_autocor = FALSE),
    posterior_epred(fit, incl_autocor = FALSE)
  )
  expect_false(isTRUE(all.equal(
    posterior_epred(fit, incl_autocor = FALSE, process_error = TRUE),
    posterior_epred(fit, incl_autocor = FALSE, process_error = TRUE)
  )))
})


# -- The marginaleffects and insight surface --------------------------
#
# These four fits are what that surface needs to be told apart: an
# observation-side covariate, the same covariate on the trend side,
# a response with categories, and a frame carrying columns the model
# never reads. Each claim is stated against the fit it runs on rather
# than against another package's output.

library(marginaleffects)
options("marginaleffects_model_classes" = "mvgam")


test_that("insight reads this fit's own frame and response", {
  # Identity to the fit's own frame is the whole claim, and it
  # subsumes any check of the class or the column names.
  expect_identical(insight::get_data(fit_plain), fit_plain$data)
  expect_equal(insight::find_response(fit_plain), "y")
  expect_equal(insight::find_response(fit_ord), "y")
})


test_that("find_predictors reaches a covariate on either side", {
  # `x` sits in the observation formula of one fit and in the trend
  # formula of the other. A predictor list built from the observation
  # side alone answers correctly for the first and drops `x` from
  # the second, which is what makes the pair the test.
  expect_true("x" %in% insight::find_predictors(fit_plain)$conditional)
  expect_true("x" %in% insight::find_predictors(fit_trend)$conditional)
  expect_true("time" %in% insight::find_predictors(fit_trend)$conditional)
  # Both fits carry one series, so `series` supports neither a slope
  # nor a contrast and must not be offered.
  expect_identical(length(unique(fit_plain$data$series)), 1L)
  expect_false("series" %in%
                 insight::find_predictors(fit_plain)$conditional)
  # Nor may a column the formula never mentions appear: this frame
  # carries `z` and `grp`, which only the ordinal and smooth fits
  # read.
  expect_false("grp" %in% insight::find_predictors(fit_plain)$conditional)
  expect_true("z" %in% insight::find_predictors(fit_ord)$conditional)
})


test_that("model.frame returns the model's variables and no others", {
  # The claim is the word "only". Listing two columns that are
  # present says nothing about the ones that should not be: this
  # frame carries `series`, `z` and `grp`, which `y ~ 1 + x` never
  # reads, and a model.frame that passed the data through unchanged
  # would satisfy any membership check.
  mf <- model.frame(fit_plain)
  expect_setequal(names(mf), c("y", "x", "time"))
  expect_false(any(c("series", "z", "grp") %in% names(mf)))
  expect_identical(nrow(mf), nrow(fit_plain$data))
  expect_equal(mf$y, fit_plain$data$y)
  expect_equal(mf$x, fit_plain$data$x)
})


test_that("avg_slopes reports one slope per predictor", {
  out <- avg_slopes(fit_plain)
  # "It returned rows" is satisfied by a single row for the wrong
  # term. The terms are the model's own predictors, one slope each.
  preds <- insight::find_predictors(fit_plain)$conditional
  expect_setequal(as.character(out$term), preds)
  expect_identical(nrow(out), length(preds))
  expect_true(all(is.finite(out$estimate)))
  expect_true(all(out$conf.low <= out$estimate))
  expect_true(all(out$estimate <= out$conf.high))

  # The reported estimate is the median of the draws the same call
  # attached, exactly. Comparing it against a separately computed
  # slope would compare two independent stochastic draws, since the
  # default type samples the observation family; comparing it with
  # its own draws is exact and still fails a summary taken over the
  # wrong margin.
  dr <- marginaleffects::posterior_draws(out, shape = "DxP")
  expect_identical(ncol(dr), nrow(out))
  for (j in seq_len(ncol(dr))) {
    expect_equal(out$estimate[j], stats::median(dr[, j]))
  }
})


test_that("get_predict summarises the draws it attaches", {
  out <- get_predict(fit_plain, newdata = fit_plain$data,
                     type = "response")
  expect_true(all(c("rowid", "group", "estimate") %in% names(out)))
  expect_identical(nrow(out), nrow(fit_plain$data))
  draws <- attr(out, "posterior_draws")
  # marginaleffects stores draws as [nobs x ndraws], the transpose
  # of mvgam's own layout, so the orientation is a real claim: a
  # matrix handed over untransposed passes any is.matrix check.
  expect_identical(dim(draws),
                   c(nrow(fit_plain$data), as.integer(ndraws(fit_plain))))
  # And `estimate` is the median of those draws, exactly. A column
  # summarised over the wrong margin, or taken from a second call,
  # fails this while keeping every dimension above.
  expect_equal(out$estimate, apply(draws, 1L, stats::median))
  expect_identical(out$rowid, seq_len(nrow(fit_plain$data)))
})


test_that("each get_predict type is the surface it names", {
  nd <- fit_plain$data
  link <- attr(get_predict(fit_plain, newdata = nd, type = "link"),
               "posterior_draws")
  expected <- attr(get_predict(fit_plain, newdata = nd, type = "expected"),
                   "posterior_draws")

  # The dispatch surface has to hand back the same numbers the
  # public method does, transposed and nothing else. A type quietly
  # routed to its neighbour returns a matrix of the same shape on
  # the wrong scale.
  expect_equal(link, t(posterior_linpred(fit_plain, newdata = nd)),
               ignore_attr = TRUE)
  expect_equal(expected, t(posterior_epred(fit_plain, newdata = nd)),
               ignore_attr = TRUE)
  # And the two are separated by the family's inverse link, exactly.
  # This is what fails when the link scale is returned where the
  # response scale was asked for.
  expect_equal(exp(link), expected, ignore_attr = TRUE)
  expect_true(all(expected > 0))
  # The two scales are not the same numbers, so the identity above
  # is a constraint rather than a restatement.
  expect_gt(max(abs(link - expected)), 1)
})


test_that("only the sampled type redraws between calls", {
  # `"response"` draws from the observation family, so it is not
  # repeatable; `"expected"` and `"link"` are deterministic functions
  # of the posterior and must be. A type that quietly answered with
  # another's quantity would break one of these two halves.
  det <- function(ty) {
    a <- predictions(fit_plain, type = ty)$estimate
    b <- predictions(fit_plain, type = ty)$estimate
    max(abs(a - b))
  }
  expect_identical(det("expected"), 0)
  expect_identical(det("link"), 0)
  expect_gt(det("response"), 1e-6)
})


test_that("avg_predictions averages the draws predictions returns", {
  # On `type = "expected"` the two calls are deterministic, which is
  # what makes an exact tie possible. The default `type = "response"`
  # samples the observation family, so no cross-call identity holds
  # there; that difference is the contract asserted above.
  p <- predictions(fit_plain, type = "expected")
  ap <- avg_predictions(fit_plain, type = "expected")
  dp <- marginaleffects::posterior_draws(p, shape = "DxP")
  dap <- marginaleffects::posterior_draws(ap, shape = "DxP")
  expect_identical(dim(dp),
                   c(as.integer(ndraws(fit_plain)), nrow(fit_plain$data)))
  expect_identical(nrow(ap), 1L)

  # The averaged draws are the row means of the per-row draws, so
  # the average is taken over observations within a draw and not
  # over draws within an observation. Averaging the wrong margin
  # returns one number of the right magnitude and fails here.
  expect_equal(as.numeric(dap), unname(rowMeans(dp)))
  # And the reported estimate is the median of those draws, which is
  # why it is not the mean of the per-row estimates.
  expect_equal(ap$estimate, stats::median(as.numeric(dap)))
  expect_false(isTRUE(all.equal(ap$estimate, mean(p$estimate))))
})


test_that("process_error moves a marginal prediction", {
  # The trend's innovations are what `process_error` adds, so
  # switching it on has to move the answer. An argument read and
  # dropped leaves every dimension intact. `FALSE` is the default, so
  # its repeatability is the claim the block above already makes and
  # is not restated here.
  set.seed(1L)
  p_off <- predictions(fit_plain, type = "expected",
                       process_error = FALSE)
  set.seed(1L)
  p_on <- predictions(fit_plain, type = "expected",
                      process_error = TRUE)
  expect_gt(max(abs(p_off$estimate - p_on$estimate)), 1e-6)
})


test_that("get_predict refuses a type the model has no surface for", {
  # Matched on its wording. A bare `expect_error()` passes on any
  # failure at all, including a typo in the call it was meant to
  # exercise, so it cannot tell a refusal from a mistake.
  err <- expect_error(
    get_predict(fit_plain, newdata = fit_plain$data, type = "bogus"),
    "Must be element of set"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "'bogus'", fixed = TRUE)
  # The refusal lists what the model does offer, so a user can act on
  # it rather than guess.
  for (ty in c("response", "link", "expected")) {
    expect_match(msg, ty, fixed = TRUE)
  }
})


test_that("an ordinal fit predicts and names one row per category", {
  # `type = "expected"` routes through posterior_epred, which returns
  # [ndraws x nobs x ncat] for an ordinal family, and the 3D branch
  # in get_predict flattens it to one row per (observation,
  # category).
  cats <- levels(fit_ord$data$y)
  expect_length(cats, 3L)
  out <- get_predict(fit_ord, newdata = fit_ord$data, type = "expected")
  expect_identical(nrow(out), nrow(fit_ord$data) * length(cats))
  expect_setequal(as.character(unique(out$group)), cats)
  # The categories are the ones the fit models, which is not the
  # same as the ones the raw factor declared: the simulation's
  # lowest bin drew no observations and the level left with it, so
  # a method reading the declared levels reports one category the
  # model has no threshold for.
  expect_identical(as.integer(fit_ord$standata$nthres),
                   length(cats) - 1L)
  expect_equal(get_group_names(fit_ord), cats)
  # A family with one surface per row answers with the default
  # label instead.
  expect_equal(get_group_names(fit_plain), "main_marginaleffect")

  # Each observation's categories are a simplex within a draw, so a
  # flattened table that lost the category margin, or that paired a
  # row with another observation's categories, fails here. The claim
  # is made on the draws rather than on `estimate`: that column is a
  # per-cell median, and medians of dependent components do not sum
  # to one (measured, they fall short by 2 to 8 per cent).
  draws <- attr(out, "posterior_draws")
  for (d in c(1L, ncol(draws) %/% 2L, ncol(draws))) {
    expect_equal(as.numeric(tapply(draws[, d], out$rowid, sum)),
                 rep(1, nrow(fit_ord$data)), tolerance = 1e-8)
  }
})


test_that("the entry points agree with each other on these fits", {
  # Three response types through one code path. Asserting that each
  # call returns an object of its own class says only that nothing
  # raised. These claims tie the entry points to each other and to
  # the fit, so a family routed through the wrong branch fails
  # rather than returning a well-formed table of wrong numbers.
  for (fit in list(fit_plain, fit_trend, fit_re_smooth)) {
    pred <- predictions(fit)
    avg <- avg_predictions(fit)
    # Both marginal summaries are taken on the expectation rather
    # than on the default sampled response. A Poisson draw is a whole
    # number, so a median of sampled differences collapses onto 0 or
    # +/-1 and carries no sign the derivative below can be held to.
    slope <- avg_slopes(fit, variables = "x", type = "expected")
    comp <- avg_comparisons(fit, variables = "x", type = "expected")

    # One prediction per row of the training frame, in its order.
    expect_identical(nrow(pred), nrow(fit$data))
    expect_true(all(is.finite(pred$estimate)))
    expect_identical(nrow(avg), 1L)
    expect_true(is.finite(avg$estimate))

    expect_identical(nrow(slope), 1L)
    expect_identical(as.character(slope$term), "x")
    expect_true(slope$conf.low <= slope$estimate)
    expect_true(slope$estimate <= slope$conf.high)

    expect_identical(nrow(comp), 1L)
    expect_identical(as.character(comp$term), "x")
    # A comparison over a unit step and a slope at a point move
    # together in sign; a comparison that lost the covariate
    # returns zero and fails here.
    expect_gt(abs(comp$estimate), 1e-8)
    expect_identical(sign(comp$estimate), sign(slope$estimate))
  }
})


test_that("loo_epred and loo_linpred part at a non-identity link", {
  # The complement of the identity-link claim in
  # test-distributional-dpars.R. A Poisson mean is the exponential
  # of its predictor, so these two must not agree; a method that
  # skipped the inverse link returns the right dimensions on the
  # wrong scale and passes every shape check.
  e <- loo_epred(fit_plain, type = "mean")
  l <- loo_linpred(fit_plain, type = "mean")
  expect_identical(dim(e), c(nrow(fit_plain$data), 1L))
  expect_identical(dim(l), dim(e))
  expect_true(all(is.finite(e)))
  expect_true(all(e > 0))
  expect_false(isTRUE(all.equal(as.numeric(e), as.numeric(l))))
  # And the two are separated by the family's own link, not by an
  # arbitrary offset.
  expect_gt(stats::cor(log(as.numeric(e)), as.numeric(l)), 0.9)

  # A leave-one-out expectation reweights the posterior one rather
  # than reproducing it, so the two must differ.
  ep <- colMeans(posterior_epred(fit_plain))
  expect_false(isTRUE(all.equal(as.numeric(e), unname(ep))))
  # How closely it still tracks the posterior expectation is not
  # asserted here. Measured, the two correlate at 0.31 on this fit
  # against better than 0.5 on the trend-free gaussian in
  # test-distributional-dpars.R, which is finding 11: dropping an
  # observation moves the latent state it was scored against, the
  # importance ratios have no finite variance, and the reweighting is
  # dominated by single draws. Pinning a number here would enshrine
  # that regime rather than describe it.
})


cat("\nDone.\n")
