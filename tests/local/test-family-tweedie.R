# Tweedie, checked against the moments the distribution is defined by.
#
# Tweedie is the one exported family that nothing in `tests/local`
# fitted. It is also the one that needs its own Stan code: `tweedie()`
# builds a `brms::custom_family()` with parameters `mphi` and
# `mtheta`, and hangs the Stan function definitions off the family as
# `attr(fam, "mvgam_stanvars")`, which mvgam applies while assembling
# the model. So this file is the only place that path is exercised
# end to end.
#
# Checking the density directly would be circular. mvgam's post-fit
# `log_lik` calls `mgcv::ldTweedie`, so comparing the two would
# re-run one implementation against itself. The in-fit Stan code is a
# separate series expansion truncated at `M` terms, and the useful
# question is whether the two agree.
#
# They can be made to answer one elementary quantity. Tweedie on
# 1 < p < 2 is a compound Poisson sum of Gammas, so the number of
# summands is Poisson with mean mu^(2-p) / (phi * (2-p)) and the
# probability of drawing none of them is
#
#   P(Y = 0) = exp(-mu^(2-p) / (phi * (2-p)))
#
# The zero rows and the draws reach it by different routes. At
# `y = 0` the agreement is bitwise, because `mgcv::ldTweedie`
# evaluates that same expression there and the Stan branch sums the
# same lambda -- so what it establishes is not that two derivations
# concur but that `mphi` and `mtheta` arrive in the right slots of
# it. They do: swapping the two moves the density by 1e5, holding
# phi at 1 by 0.24, and holding p at 1.5 by 0.20. The draws are the
# separate route, since an rng parameterised on the wrong scale
# reproduces the mean and still misses the mass at zero.
#
# The support is the family's other distinguishing feature: an atom
# at exactly zero and a continuous positive part, which no other
# family mvgam fits has. E[Y] = mu on the whole range, including both
# boundaries, so the mean carries no Jensen correction.
#
#   truth: 60 occasions, latent AR(1), p = 1.4, phi = 1.2, giving
#          about one zero in ten
#   model: y ~ x, trend_formula = ~ AR(p = 1), tweedie()
#
# Run with:
#   testthat::test_file("tests/local/test-family-tweedie.R")

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

set.seed(53L)

n_time <- 60L
x <- seq(-1.2, 1.2, length.out = n_time)
latent <- as.numeric(arima.sim(list(ar = 0.5), n_time, sd = 0.25))
mu_true <- exp(1.0 + 0.5 * x + latent)
power_true <- 1.4
phi_true <- 1.2

y <- mgcv::rTweedie(mu = mu_true, p = power_true, phi = phi_true)

dat <- data.frame(
  y = y, x = x, time = seq_len(n_time), series = factor("s1")
)
# The design is only informative if both parts of the support are
# populated, so a run that produced no zeros would be testing an
# ordinary Gamma model.
stopifnot(any(y == 0), any(y > 0), any(y[y > 0] != floor(y[y > 0])))


# -- What the prefit carries ------------------------------------------
#
# `M` truncates the series the Stan code sums, and it reaches Stan as
# data rather than as a literal, so it is checked on the prefit where
# no sampling is involved.

test_that("the custom family reaches Stan, and M with it", {
  pre30 <- mvgam(
    y ~ x, trend_formula = ~ AR(p = 1), data = dat,
    family = tweedie(M = 30L), run_model = FALSE
  )
  pre40 <- mvgam(
    y ~ x, trend_formula = ~ AR(p = 1), data = dat,
    family = tweedie(M = 40L), run_model = FALSE
  )

  code <- as.character(stancode(pre30))
  # The function definitions come off the family attribute; without
  # them the model would call an lpdf that was never declared.
  expect_true(grepl("real tweedie_lpdf", code))
  # The data declaration, not the `int M` that also appears in the
  # function signatures: deleting the stanvar would leave those.
  expect_true(grepl("int<lower=1> M;", code, fixed = TRUE))
  # And the trend is still there, so the custom family did not
  # displace the part of the model this package exists for.
  expect_true(grepl("lv_trend", code))

  # `M` is data, so the two models share their code and differ in
  # what they are given. Asserting both ways round is what
  # distinguishes a respected argument from an ignored one.
  expect_identical(as.character(stancode(pre40)), code)
  expect_identical(as.integer(standata(pre30)$M), 30L)
  expect_identical(as.integer(standata(pre40)$M), 40L)
})


# -- The two implementations of the density -------------------------
#
# In the fit, Stan evaluates a series truncated at `M` terms; after
# it, mvgam calls `mgcv::ldTweedie`. Nothing compares them, and they
# are the two halves of the same model. The comparison needs no fit
# and no Stan: the series is short enough to write out.

log_sum_exp <- function(v) {
  m <- max(v)
  m + log(sum(exp(v - m)))
}

# The series `tweedie_stan_funs()` sums, transcribed. Zeros
# contribute `-lambda`; each positive value contributes a Poisson
# mixture of Gammas over the number of summands.
stan_tweedie_lpdf <- function(y, mu, phi, power, M) {
  lambda <- mu^(2 - power) / ((2 - power) * phi)
  alpha <- (2 - power) / (power - 1)
  beta <- mu^(1 - power) / ((power - 1) * phi)
  lp <- -sum(lambda[y == 0])
  for (i in which(y != 0)) {
    terms <- vapply(seq_len(M), function(m) {
      dpois(m, lambda[i], log = TRUE) +
        dgamma(y[i], shape = m * alpha, rate = beta[i], log = TRUE)
    }, numeric(1L))
    lp <- lp + log_sum_exp(terms)
  }
  lp
}


test_that("the Stan series and mgcv agree, once M is large enough", {
  phi <- 1.26
  power <- 1.43
  reference <- sum(mgcv::ldTweedie(y = dat$y, mu = mu_true,
                                   p = power, phi = phi)[, 1L])
  at <- function(M) stan_tweedie_lpdf(dat$y, mu_true, phi, power, M)

  # At the default the two are the same number to machine precision,
  # which is the claim the fit rests on: what Stan maximised and what
  # `log_lik()` reports are one density.
  expect_equal(at(30L), reference, tolerance = 1e-9)
  expect_equal(at(20L), reference, tolerance = 1e-9)

  # And `M` is not decoration. Truncating the series too early moves
  # the density by whole log units, so a model built with a small `M`
  # is fitting something else.
  expect_gt(abs(at(3L) - reference), 10)
  expect_gt(abs(at(5L) - reference), 1)
  expect_lt(abs(at(10L) - reference), 0.01)
  # Adding terms only improves it.
  expect_lt(abs(at(10L) - reference), abs(at(5L) - reference))
  expect_lt(abs(at(5L) - reference), abs(at(3L) - reference))

  # The truncation point has to clear the number of summands the data
  # actually imply, and on this design it does by a wide margin.
  lambda <- mu_true^(2 - power) / ((2 - power) * phi)
  expect_lt(max(lambda), 30 / 3)
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_tweedie.rds")
if (file.exists(cache)) {
  cat("[cache] Loading tweedie fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(y ~ x, AR(p = 1), tweedie())\n")
  fit <- mvgam(
    y ~ x, trend_formula = ~ AR(p = 1), data = dat,
    family = tweedie(),
    chains = 2L, iter = 1000L, warmup = 500L,
    control = list(adapt_delta = 0.95),
    silent = 2, backend = "cmdstanr"
  )
  part <- paste0(cache, ".part")
  saveRDS(fit, part)
  file.rename(part, cache)
}

ids <- 1:400


test_that("the mean is the linear predictor, with no correction", {
  # E[Y] = mu across the whole range of p, so `epred` is the inverse
  # link and nothing else. A family that applied a Jensen or
  # truncation correction here would still be positive and finite.
  lp <- posterior_linpred(fit, draw_ids = ids, incl_autocor = TRUE)
  mu <- posterior_linpred(fit, transform = TRUE, draw_ids = ids,
                          incl_autocor = TRUE)
  ep <- posterior_epred(fit, draw_ids = ids, incl_autocor = TRUE)

  expect_equal(unname(mu), unname(exp(lp)), tolerance = 1e-12)
  expect_equal(unname(ep), unname(mu), tolerance = 1e-12)
  expect_true(all(ep > 0))
})


test_that("the truncation diagnostic reads M from the Stan data", {
  # `M` is Stan data. It was read from `model_data` first, which a
  # `jsdgam()` fit sets to its frame: a frame has no `M`, and the
  # diagnostic refused every jsdgam tweedie fit.
  expect_message(check_tweedie_truncation(fit),
                 paste0("M = ", fit$standata$M, ","), fixed = TRUE)
  framed <- fit
  framed$model_data <- mvgam:::mvgam_training_data(fit)
  expect_message(check_tweedie_truncation(framed),
                 paste0("M = ", fit$standata$M, ","), fixed = TRUE)
})


test_that("the density at zero is the compound-Poisson zero mass", {
  # The centrepiece. `log_lik` comes from one implementation and the
  # expression below from the distribution's definition, so an
  # agreement here ties the fitted density to something neither the
  # Stan series nor `mgcv::ldTweedie` evaluates directly.
  mu <- posterior_linpred(fit, transform = TRUE, draw_ids = ids,
                          incl_autocor = TRUE)
  dm <- posterior::as_draws_matrix(fit)
  phi <- as.numeric(dm[, "mphi"])[ids]
  power <- as.numeric(dm[, "mtheta"])[ids]
  expect_true(all(phi > 0))
  expect_true(all(power > 1 & power < 2))

  p_zero <- exp(-mu^(2 - power) / (phi * (2 - power)))
  ll <- log_lik(fit, draw_ids = ids, incl_autocor = TRUE)
  zero <- dat$y == 0
  expect_true(any(zero))

  expect_equal(unname(exp(ll[, zero])), unname(p_zero[, zero]),
               tolerance = 1e-10)
  # What the agreement above is worth. Each substitution below is a
  # way the two custom parameters could reach the density wrongly
  # while every value stayed finite and positive, and each one
  # breaks it by orders of magnitude more than the tolerance.
  swapped <- exp(-mu^(2 - phi) / (power * (2 - phi)))
  expect_gt(max(abs(exp(ll[, zero]) - swapped[, zero])), 1)
  flat_phi <- exp(-mu^(2 - power) / (2 - power))
  expect_gt(max(abs(exp(ll[, zero]) - flat_phi[, zero])), 0.05)
  flat_power <- exp(-mu^0.5 / (phi * 0.5))
  expect_gt(max(abs(exp(ll[, zero]) - flat_power[, zero])), 0.05)

  # And the positive rows are a different branch of the density, so
  # the equality above is reading the zero branch and not a constant.
  expect_gt(max(abs(exp(ll[, !zero]) - p_zero[, !zero])), 0.1)
})


test_that("the draws put the same mass at zero", {
  # The third route to the same number, through the rng rather than
  # the density. An rng parameterised on the wrong scale reproduces
  # the mean and still fails this.
  mu <- posterior_linpred(fit, transform = TRUE, draw_ids = ids,
                          incl_autocor = TRUE)
  dm <- posterior::as_draws_matrix(fit)
  phi <- as.numeric(dm[, "mphi"])[ids]
  power <- as.numeric(dm[, "mtheta"])[ids]
  p_zero <- exp(-mu^(2 - power) / (phi * (2 - power)))

  set.seed(3L)
  pp <- posterior_predict(fit, draw_ids = ids)
  expect_equal(mean(pp == 0), mean(p_zero), tolerance = 0.02)
})


test_that("the support has an atom at zero and is continuous above it", {
  set.seed(3L)
  pp <- posterior_predict(fit, draw_ids = ids)
  expect_true(all(pp >= 0))
  expect_true(any(pp == 0))
  # Every positive draw is continuous. A count family would fail
  # this, and so would a draw rounded anywhere in the pipeline.
  positives <- pp[pp > 0]
  expect_true(all(positives != floor(positives)))

  # The rng tracks the fitted mean observation by observation, not
  # merely in aggregate: a mu permuted across the timeline would
  # keep the pooled mean and destroy this.
  ep <- posterior_epred(fit, draw_ids = ids, incl_autocor = TRUE)
  expect_gt(cor(colMeans(pp), colMeans(ep)), 0.9)
})


test_that("the prediction types this family answers, and the one it does not", {
  for (ty in c("response", "link", "expected")) {
    v <- predict(fit, type = ty, ndraws = 200L)
    expect_identical(nrow(v), nrow(dat))
    expect_true(all(is.finite(v[, "Estimate"])))
  }
  expect_true(all(predict(fit, type = "response", ndraws = 200L)[, "Estimate"] >= 0))

  # Tweedie has a closed-form variance, phi * mu^p, but it is not in
  # mvgam's table, and the refusal says so plainly and names the way
  # round it rather than failing obscurely.
  err <- expect_error(predict(fit, type = "variance", ndraws = 100L),
                      "not implemented for family")
  msg <- conditionMessage(err)
  expect_match(msg, "tweedie", fixed = TRUE)
  # Matched with the wrapping collapsed: `insight::format_error()`
  # breaks lines at `getOption("width")`, so pinning the break pins
  # the console width the suite happens to run at.
  expect_match(gsub("[[:space:]]+", " ", msg),
               "compute the variance empirically", fixed = TRUE)
})


test_that("residuals and loo are built on the same fit", {
  res <- residuals(fit)
  expect_identical(nrow(res), nrow(dat))
  expect_true(all(is.finite(res[, "Estimate"])))

  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_identical(length(ic$diagnostics$pareto_k), nrow(dat))
  # loo picks its warning threshold from the number of draws (about
  # 0.66 here), not from the 0.7 the documentation quotes, so the
  # claim is bracketed either side of it. Pinning 0.7 passes today
  # and fails the moment a k lands between the two with nothing
  # wrong.
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  expect_true(!any(pareto_k > 0.75) || length(loo_warnings) > 0L)
  expect_true(length(loo_warnings) == 0L || max(pareto_k) > 0.6)
  # Whatever was raised was the Pareto notice, so nothing else is
  # being muted by the handler above.
  expect_true(all(grepl("[Pp]areto[ _]k", loo_warnings)))
})


test_that("the hindcast is the fitted series on the outcome scale", {
  hc <- hindcast(fit, type = "expected")
  expect_s3_class(hc, "mvgam_forecast")
  block <- hc$hindcasts[[1L]]
  expect_identical(ncol(block), nrow(dat))

  # The hindcast is a second route to the fitted expectation over the
  # training grid, so it has to be that expectation cell for cell.
  # Checking the shape and the sign would pass on a block that read
  # the occasions in another order.
  expect_equal(unname(block),
               unname(posterior_epred(fit, incl_autocor = TRUE)),
               tolerance = 1e-12)

  # Drawn on the outcome scale it carries the family's own support:
  # an atom at zero, and everything above it continuous.
  drawn <- hindcast(fit, type = "response")$hindcasts[[1L]]
  expect_true(all(drawn >= 0))
  expect_true(any(drawn == 0))
  positives <- drawn[drawn > 0]
  expect_true(all(positives != floor(positives)))
})


test_that("quantile residuals carry posterior spread", {
  # FAILS TODAY -- finding 21, left standing so the sweep shows it.
  #
  # `residuals()` defaults to `type = "quantile"`. The families with
  # an analytic CDF evaluate it per draw, so the residual moves with
  # that draw's parameters. Tweedie has no entry in that table and
  # falls through to the empirical PIT, which pools `yrep` over all
  # draws to make one bound per observation and then repeats it down
  # every row wherever the bounds coincide. For a continuous response
  # they always coincide, so only the rows at the zero atom vary: 54
  # of 60 columns come back constant and `Est.Error` is 0 there.
  #
  # The per-draw matrix is the documented hand-off to
  # `DHARMa::createDHARMa()` and the roxygen says it carries the full
  # posterior uncertainty, so this asserts what is documented.
  per_draw <- residuals(fit, summary = FALSE)
  expect_identical(ncol(per_draw), nrow(dat))
  spread <- apply(per_draw, 2L, sd)
  expect_true(all(spread > 0))

  # `type = "ordinary"` is unaffected, which locates the fault in the
  # quantile path rather than in the fit or the draws.
  ord <- residuals(fit, type = "ordinary", summary = FALSE)
  expect_true(all(apply(ord, 2L, sd) > 0))
})


# -- What the fit says about itself -----------------------------------
#
# Tweedie is built through `brms::custom_family()`, which is the route
# the closure-unit families take too. That shared route is what these
# blocks pin: a family named by its implementation rather than by
# itself, and a parameter block half of whose names survive the read.

test_that("the fit names its own family", {
  # `glance()` reads the name mvgam recorded and answers `tweedie`,
  # so the name is on the object. `family()` returns the brms family
  # object whole, whose own `$family` element is the string
  # `custom_family()` wrote. `family()` is the accessor other
  # packages call, so it is the one that has to answer.
  expect_identical(as.character(glance(fit)$family), "tweedie")
  expect_identical(family(fit)$family, "tweedie")
})


test_that("the frame accessors a caller pairs both answer", {
  # `model.frame()` answers with the 60-row training frame. `terms()`
  # raises R's own "no terms component nor attribute", so a caller
  # discovering the model's structure without knowing the class gets
  # half of the pair. Ordered so the failing half is last.
  mf <- model.frame(fit)
  expect_identical(nrow(mf), nrow(dat))
  expect_true(all(c("y", "x") %in% names(mf)))
  expect_s3_class(terms(fit), "terms")
})


test_that("one parameter block reads the same way through every method", {
  # The raw stanfit writes the population slope as `b[1]`, and mvgam
  # aliases it to `b_x` on the way out. Three methods do that and
  # `tidy()` does not, so a reader moving between two tables of the
  # same fit meets two names for one parameter.
  #
  # The custom parameters are the control. `mphi` and `mtheta` are
  # written out under their own names and need no alias, and every
  # method below reports them identically.
  aliased <- function(z) grep("^b\\[|^b_x$", z, value = TRUE)
  for (nm in c("variables", "posterior_summary", "rhat")) {
    got <- switch(nm,
      variables = variables(fit),
      posterior_summary = rownames(posterior_summary(fit)),
      rhat = names(rhat(fit))
    )
    expect_true(all(c("mphi", "mtheta") %in% got))
    expect_identical(aliased(got), "b_x")
  }
  expect_identical(aliased(tidy(fit, effects = "all")$term), "b_x")
})


test_that("hypothesis reaches every parameter variables lists", {
  # `mphi` survives unaliased and is accepted. `b_x` exists only as
  # the alias, and `hypothesis()` reads the stanfit rather than the
  # aliased list, so it refuses a name the line above just listed.
  # Two parameters of one model, told apart by nothing a user sees.
  expect_true(all(c("mphi", "b_x") %in% variables(fit)))
  expect_s3_class(hypothesis(fit, "mphi = 1"), "brmshypothesis")
  expect_s3_class(hypothesis(fit, "b_x = 0"), "brmshypothesis")
})


test_that("an argument this family's methods cannot read is refused", {
  # Every method below names each of its arguments and forwards none
  # onward, so anything left in `...` is dead. `M` is the argument
  # that makes this concrete here: a reader who writes it on a
  # post-fit call rather than on `tweedie()` is silently answered at
  # whatever the fit was built with.
  for (m in c("posterior_epred", "residuals", "predict", "summary")) {
    expect_error(do.call(m, list(fit, zzz_unknown = 1)), "zzz_unknown")
  }
})


cat("\nDone.\n")
