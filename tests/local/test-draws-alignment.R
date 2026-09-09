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
#   testthat::test_file("tests/local/test-draws-alignment.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# Several blocks below state what the package does not yet do, and
# testthat stops a file after ten failures by default, which would
# leave the blocks after them unrun and looking clean. The limit is
# read when the reporter is built, before this file is sourced, so it
# has to come from the environment:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "..."

# Resolved from where this file is running rather than from what is
# already on disk. testthat sets the working directory to the test
# file's own, so asking whether `fixtures` exists picks the wrong
# branch on a clean tree and writes tests/local/tests/local/fixtures.
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
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

# A scale with a formula of its own, and a response bounded below.
# Both are ordinary brms spellings that nothing here drove, and each
# reaches a surface through machinery the other fits never exercise:
# a distributional parameter is computed per observation rather than
# sampled under its own name, and a truncated response has to be
# drawn from the truncated law rather than merely inside it.
set.seed(202L)
dat_dpar <- data.frame(
  x = rnorm(n_time), z = seq(-2, 2, length.out = n_time),
  time = seq_len(n_time), series = factor("s1")
)
# sigma spans an order of magnitude across the frame, so a prediction
# that ignored the sub-formula cannot land on the right answer by
# accident.
dat_dpar$y <- rnorm(n_time, 1 + 0.5 * dat_dpar$x,
                    exp(-0.5 + 0.9 * dat_dpar$z))

set.seed(303L)
dat_trunc <- data.frame(
  x = rnorm(n_time), time = seq_len(n_time), series = factor("s1")
)
# The untruncated predictive puts real mass below zero, so the bound
# does work rather than sitting decoratively outside the data.
dat_trunc$y <- pmax(rnorm(n_time, 2 + 0.6 * dat_trunc$x, 1.5), 0.01)

fit_dpar <- fit_cached(
  "gaussian_sigma_dpar", formula = brms::bf(y ~ x, sigma ~ z),
  trend_formula = ~ AR(p = 1), data = dat_dpar, family = gaussian()
)
fit_trunc <- fit_cached(
  "gaussian_trunc", formula = y | trunc(lb = 0) ~ x,
  trend_formula = ~ AR(p = 1), data = dat_trunc, family = gaussian()
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


test_that("a misspelt argument does not pass for the default", {
  # This file exists because a prediction assembled from mismatched
  # reads is finite, plausible and wrong. An argument that lands in
  # `...` is the same failure reached by a typo: every method below
  # names each of its arguments and forwards none of them onward, so
  # anything left over is dead and can be refused.
  #
  # The cost is measured rather than assumed. On this fit the
  # conditional expectation spans 0.17 to 70.17 and the marginal one
  # 1.66 to 103.80, so the two answers differ by up to 102.6 counts.
  # Writing `incl_autoccor` returns the marginal one exactly.
  set.seed(7L)
  asked <- posterior_epred(fit_plain, incl_autocor = TRUE)
  set.seed(7L)
  typo <- posterior_epred(fit_plain, incl_autoccor = TRUE)
  set.seed(7L)
  default <- posterior_epred(fit_plain)
  expect_gt(max(abs(asked - default)), 1)
  # The typo is the default, to the last bit, and nothing said so.
  expect_equal(unname(as.matrix(typo)), unname(as.matrix(default)))

  for (m in c("posterior_epred", "posterior_linpred", "posterior_predict",
              "log_lik", "residuals", "predict", "fitted")) {
    expect_error(
      do.call(m, list(fit_plain, ndraws = 5L, zzz_unknown = 1)),
      "zzz_unknown"
    )
  }
})


test_that("quantile residuals are standard normal on both families", {
  # A randomised quantile residual is standard normal by
  # construction, whatever the family, which is what makes it the
  # residual to read a fit through. The ordinal fit is the control:
  # its four categories supply enough ties for the empirical PIT to
  # spread properly, and it answers at 0.908 with 0.22 per cent
  # beyond three standard deviations.
  #
  # The poisson arm does not. It reads 0.454 with 0.05 per cent
  # beyond three, so a QQ plot of this fit is too narrow to show a
  # departure that is really there. The control is asserted first so
  # that it runs.
  scale_of <- function(fit) {
    r <- suppressWarnings(
      residuals(fit, type = "quantile", summary = FALSE, ndraws = 200L)
    )
    c(sd = stats::sd(r, na.rm = TRUE),
      tail = mean(abs(r) > 3, na.rm = TRUE))
  }
  ord_scale <- scale_of(fit_ord)
  expect_gt(ord_scale[["sd"]], 0.8)
  expect_lt(abs(ord_scale[["sd"]] - 1), 0.25)

  pois_scale <- scale_of(fit_plain)
  expect_lt(abs(pois_scale[["sd"]] - 1), 0.25)
  expect_gt(pois_scale[["tail"]], 0.001)
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
  #
  # Both calls raise a marginaleffects notice saying `process_error`
  # is not known to be supported for this class. It is mvgam's own
  # argument and it is honoured, so the notice is wrong and the
  # class has not been registered on that whitelist. Captured here
  # rather than left to leak, and asserted as the absence it should
  # be, so this reports the defect instead of the argument.
  warned <- character(0)
  grab <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }
  set.seed(1L)
  p_off <- grab(predictions(fit_plain, type = "expected",
                            process_error = FALSE))
  set.seed(1L)
  p_on <- grab(predictions(fit_plain, type = "expected",
                           process_error = TRUE))
  expect_gt(max(abs(p_off$estimate - p_on$estimate)), 1e-6)

  # Nothing about a supported argument should be reported as unknown.
  expect_identical(
    grep("not known to be supported", warned, value = TRUE),
    character(0)
  )
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
  # A Poisson mean is the exponential of its predictor, so these
  # two must not agree on a log link; a method that
  # skipped the inverse link returns the right dimensions on the
  # wrong scale and passes every shape check.
  # Both run PSIS, and both warn that some Pareto k are too high,
  # which on a latent-trend fit is the truth rather than
  # noise: dropping an observation moves the state it is scored
  # against, so the ratios have no finite variance. The notice is
  # captured and held to being that one, so an unrelated warning
  # cannot pass unseen behind it.
  psis_warnings <- character(0)
  grab_k <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      psis_warnings <<- c(psis_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  }
  e <- grab_k(loo_epred(fit_plain, type = "mean"))
  l <- grab_k(loo_linpred(fit_plain, type = "mean"))
  expect_true(all(grepl("Pareto k", psis_warnings)))
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
  # asserted here. Measured, the two correlate at 0.31 on this fit,
  # which is finding 11: dropping an observation moves the latent
  # state it was scored against, the importance ratios have no
  # finite variance, and the reweighting is dominated by single
  # draws. Pinning a number here would enshrine that regime rather
  # than describe it.
})


cat("\nDone.\n")


test_that("a variance reads a scale written with its own formula", {
  # `sigma ~ z` is computed per observation rather than sampled under
  # that name, so a reader that matched names in the stanfit found
  # nothing and the method refused a gaussian fit for want of a
  # `sigma` the model has. Asserting only that the answer is finite
  # would not have caught it, and asserting only that it runs would
  # not catch a scale read from the wrong observation.
  d <- mvgam:::mvgam_training_data(fit_dpar)
  ids <- 1:100
  sigma <- mvgam:::resolve_family_pars(
    fit_dpar, dpar_names = "sigma", ndraws = length(ids),
    nobs = nrow(d), draw_ids = ids, newdata = d, resp = NULL
  )$sigma

  # The scale has to vary across the frame for the claim below to
  # discriminate; a constant one would be matched by any broadcast.
  expect_gt(max(colMeans(sigma)) / min(colMeans(sigma)), 5)

  v <- predict(fit_dpar, type = "variance", incl_autocor = TRUE,
               summary = FALSE, draw_ids = ids)
  expect_identical(dim(v), dim(sigma))
  # A gaussian variance is that draw's own sigma squared, cell for
  # cell, so this is an identity rather than a tolerance.
  expect_equal(v, sigma^2, tolerance = 1e-12)
})


test_that("a truncated prediction follows the truncated law", {
  # A draw outside the bound is replaced, and the replacement has to
  # come from this observation's own predictive restricted to the
  # bound. Resolving the distribution by name and calling it with no
  # parameters drew from the standard member of the family instead,
  # and rejecting-then-clamping piled mass exactly on the bound.
  # Both are invisible to `all(y >= lb)`, which is why the check is
  # made against the truncated distribution itself.
  d <- mvgam:::mvgam_training_data(fit_trunc)
  yrep <- posterior_predict(fit_trunc, incl_autocor = TRUE)
  expect_true(all(yrep >= 0))

  # Clamping is the tell: it puts a spike of mass on the bound.
  expect_lt(mean(yrep == 0), 1e-8)

  mu <- posterior_linpred(fit_trunc, transform = TRUE,
                          incl_autocor = TRUE)
  sigma <- mvgam:::resolve_family_pars(
    fit_trunc, dpar_names = "sigma", ndraws = nrow(mu),
    nobs = ncol(mu), draw_ids = NULL, newdata = d, resp = NULL
  )$sigma

  # The probability-integral transform of a draw against the law it
  # is supposed to follow is uniform. A replacement drawn from a
  # standard normal, where this predictive sits near 2, would pile
  # this statistic against zero.
  p_lb <- stats::pnorm(0, mu, sigma)
  u <- (stats::pnorm(yrep, mu, sigma) - p_lb) / (1 - p_lb)
  expect_true(all(u >= -1e-8 & u <= 1 + 1e-8))
  expect_equal(mean(u), 0.5, tolerance = 0.02)
  expect_equal(stats::sd(u), 1 / sqrt(12), tolerance = 0.02)
})


test_that("both prior accessors report the support Stan declares", {
  # `get_prior()` carried each trend parameter's bounds and
  # `prior_summary()` reported them as NA, because the stored table is
  # rebuilt by reading the emitted Stan code and the reader took the
  # sampling statement while ignoring the declaration beside it. A
  # reader of the summary saw `normal(0, 0.5)` unbounded for a
  # coefficient the sampler holds inside (-1, 1).
  declared <- function(sc, par) {
    line <- grep(paste0("[ ]", par, ";"), strsplit(sc, "\n")[[1]],
                 value = TRUE)[1]
    c(if (grepl("lower", line)) {
        trimws(sub(".*lower[ ]*=[ ]*([^,>]+).*", "\\1", line))
      } else "",
      if (grepl("upper", line)) {
        trimws(sub(".*upper[ ]*=[ ]*([^,>]+).*", "\\1", line))
      } else "")
  }
  for (fit in list(fit_plain, fit_trend, fit_dpar, fit_trunc)) {
    sc <- as.character(stancode(fit))
    tab <- as.data.frame(prior_summary(fit))
    checked <- 0L
    for (par in c("ar1_trend", "sigma_trend")) {
      row <- tab[tab$class == par, , drop = FALSE]
      if (!nrow(row)) next
      reported <- c(row$lb[1], row$ub[1])
      reported[is.na(reported)] <- NA_character_
      expect_identical(reported, declared(sc, par))
      checked <- checked + 1L
    }
    # The loop has to have asserted something, or a table that lost
    # its trend rows entirely would pass in silence.
    expect_gt(checked, 0L)
  }
})


test_that("an off-centre asymmetric Laplace reports its own mean", {
  # `posterior_epred()` routes a family through its mean kernel only
  # when `epred_extra_dpars_for()` names that family, and a family
  # missing from that registry falls through to the inverse link
  # instead. Nothing about the result says so: it is finite, it has
  # the right shape, and for a symmetric family it is even correct.
  #
  # An asymmetric Laplace is the case that separates them. Its mean
  # is `mu + sigma (1 - 2q) / (q (1 - q))`, which equals `mu` only at
  # `q = 0.5`, so fitting at `q = 0.25` makes the two answers differ
  # by a wide margin -- 1.98 against 4.29 when this was found. The
  # data's own mean is the third opinion, and it sides with the
  # kernel.
  set.seed(7L)
  n <- 120L
  d <- data.frame(
    time = seq_len(n), series = factor("s1"), x = rnorm(n)
  )
  q <- 0.25
  d$y <- 1.5 + 0.8 * d$x +
    brms::rasym_laplace(n, mu = 0, sigma = 1, quantile = q)

  fit <- fit_cached(
    "asym_laplace_q25",
    formula = bf(y ~ x, quantile = q),
    data = d, family = brms::asym_laplace()
  )

  ids <- 1:200
  ep <- posterior_epred(fit, draw_ids = ids)
  lp <- posterior_linpred(fit, draw_ids = ids, transform = TRUE)
  pars <- mvgam:::resolve_family_pars(
    fit, dpar_names = mvgam:::get_family_dpars("asym_laplace"),
    ndraws = nrow(lp), nobs = ncol(lp), draw_ids = ids,
    newdata = NULL, resp = NULL
  )

  # The mean the family defines, at the same draws.
  expect_equal(
    ep, lp + pars$sigma * (1 - 2 * q) / (q * (1 - q))
  )
  # And it is not the linear predictor, which is what a fall-through
  # returns and what every shape check would accept.
  expect_false(isTRUE(all.equal(unname(ep), unname(lp))))
  # The predictive draws are the independent opinion: their mean
  # tracks the expectation, not the predictor.
  yrep <- posterior_predict(fit, draw_ids = ids)
  expect_lt(abs(mean(ep) - mean(yrep)), 0.2)
  expect_gt(abs(mean(lp) - mean(yrep)), 1)
})


test_that("a fold splits the simplest frame there is", {
  # 30 consecutive occasions on one series, no gaps at all. A fold
  # used to be held out by deleting its rows, so the refit was handed
  # a frame whose occasions jumped and the guard demanding a regular
  # grid refused it: the irregularity was the split's own, and the
  # message named the user's `time` column for it.
  #
  # This frame is the control that says so, because there is nothing
  # about it a guard could legitimately object to.
  d <- mvgam:::mvgam_training_data(fit_plain)
  expect_identical(length(unique(d$series)), 1L)
  expect_false(any(diff(sort(unique(d$time))) != 1L))

  kf <- suppressWarnings(kfold(fit_plain, K = 2L, silent = 2L))
  expect_true(is.finite(kf$estimates["elpd_kfold", "Estimate"]))
})
