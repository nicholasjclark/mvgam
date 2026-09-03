# Structure and post-fit coverage for a closure-unit family, fitted
# in this file.
#
# An occupancy model has two grains at once. A closure unit is a
# (series, time) cell holding one latent state, and a visit is a row
# within it holding one detection attempt. Which grain a method
# answers on is its whole contract: a likelihood term belongs to a
# unit, a detection probability to a visit, and a method that
# confuses them returns a plausible matrix of the wrong width.
#
# The three files this replaces all drove one cached fixture whose
# frame set `time = 1L` for every row. There the closure unit *was*
# the series, so every assertion claiming to check the unit grain was
# comparing two identical numbers and could not have failed. Here 25
# sites are visited over 3 occasions, so units (75), series (25) and
# rows (300) are three different counts and each claim is separable.
#
#   truth: 25 sites x 3 occasions x 4 visits, occupancy driven by
#          elevation and detection by time of day
#   model: bf(y ~ elev, p ~ tod_c), family = occ()
#
# Occasions are numbered from 3, so a rank is never a time.
#
# The flocker cross-package comparison lives in
# `test-occ-flocker-concordance.R`, which is single-season by
# construction and is not folded in here.
#
# Run with:
#   testthat::test_file("tests/local/test-occ-closure-units.R")

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

with_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = seen)
}

set.seed(808L)

n_site <- 25L
n_season <- 3L
n_visit <- 4L
n_unit <- n_site * n_season
site_levels <- paste0("site_", sprintf("%02d", seq_len(n_site)))
time_vals <- seq_len(n_season) + 2L

grid <- expand.grid(
  visit = seq_len(n_visit), season = seq_len(n_season),
  site = site_levels, stringsAsFactors = FALSE
)
elev <- rnorm(n_site)
dat <- data.frame(
  series = factor(grid$site, levels = site_levels),
  time = as.integer(grid$season) + 2L,
  visit = grid$visit,
  elev = rep(elev, each = n_season * n_visit),
  tod_c = runif(nrow(grid), -6, 6)
)

# The latent state is drawn once per unit and every visit in that
# unit sees it, which is what makes the unit the grain the likelihood
# runs on.
psi_true <- plogis(-0.3 + 1.1 * dat$elev[!duplicated(paste(grid$site,
                                                           grid$season))])
z_true <- rbinom(n_unit, 1L, psi_true)
p_true <- plogis(0.4 + 0.2 * dat$tod_c)
dat$y <- as.integer(rep(z_true, each = n_visit) *
                      rbinom(nrow(dat), 1L, p_true))

# The three counts this file separates. Every grain claim below names
# one of them, and no two are equal.
stopifnot(length(unique(c(nrow(dat), n_unit, n_site))) == 3L)

# Unit identity taken from the user's own columns, never from
# anything the package derives.
unit_key <- paste(dat$series, dat$time)
unit_keys <- unique(unit_key)
detected <- as.logical(
  tapply(dat$y, unit_key, function(v) any(v > 0))[unit_keys]
)
stopifnot(any(detected), any(!detected))

sim_truth <- list(
  n_site = n_site, n_season = n_season, n_visit = n_visit,
  n_unit = n_unit, site_levels = site_levels, time_vals = time_vals,
  elev = elev, z_true = z_true
)

obs_formula <- bf(y ~ elev, p ~ tod_c)


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_occ_units.rds")
if (file.exists(cache)) {
  cat("[cache] Loading occupancy fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(bf(y ~ elev, p ~ tod_c), occ(), 75 closure units)\n")
  fit <- with_warnings(mvgam(
    formula = obs_formula, family = occ(), data = dat,
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  ))$value
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}


test_that("the program counts units, not rows and not series", {
  sd <- fit$standata
  expect_identical(as.integer(sd$N), nrow(dat))
  expect_identical(as.integer(sd$N_unit), n_unit)
  # One repeat count per unit, and every unit is visited the same
  # number of times on this frame.
  expect_length(as.integer(sd$n_rep), n_unit)
  expect_true(all(as.integer(sd$n_rep) == n_visit))
  expect_identical(sum(as.integer(sd$n_rep)), nrow(dat))

  # `visit_idx` reorders the rows into the layout the likelihood
  # walks, so it names every row exactly once. A mapping that named
  # one twice and skipped another keeps its length, keeps every index
  # in range, and quietly counts one observation twice.
  expect_length(as.integer(sd$visit_idx), nrow(dat))
  expect_setequal(as.integer(sd$visit_idx), seq_len(nrow(dat)))
})


test_that("the generated program divides its grainsize as an integer", {
  # `grainsize` is an `int`, so rounding is what is wanted, but a `/`
  # between two integers makes stanc say so at every compile of a
  # closure-unit model. `%/%` states the intent and silences it.
  # Fails today; recorded as finding 18.
  code <- paste(as.character(stancode(fit)), collapse = "\n")
  expect_true(grepl("grainsize", code, fixed = TRUE))
  expect_false(grepl("N_unit / 8", code, fixed = TRUE))
})


test_that("each surface answers on the grain it belongs to", {
  # The claim the whole file rests on. A likelihood term and a latent
  # state belong to a unit; a detection probability and a fitted
  # value belong to a visit. Because units, series and rows are three
  # different counts here, a method answering on the wrong one is
  # visible in the width alone -- which it was not on the fixture
  # this file replaces, where units and series coincided.
  per_unit <- list(
    residuals = residuals(fit),
    residual_draws = residuals(fit, summary = FALSE, ndraws = 20L),
    log_lik = log_lik(fit, ndraws = 20L),
    latent_state = predict(fit, type = "latent_state", ndraws = 20L),
    occupancy = posterior_occupancy(fit, conditional = TRUE,
                                    draw = FALSE)
  )
  expect_identical(nrow(per_unit$residuals), n_unit)
  expect_identical(ncol(per_unit$residual_draws), n_unit)
  expect_identical(ncol(per_unit$log_lik), n_unit)
  expect_identical(nrow(per_unit$latent_state), n_unit)
  expect_identical(ncol(per_unit$occupancy), n_unit)

  per_visit <- list(
    epred = posterior_epred(fit, ndraws = 20L),
    predict = posterior_predict(fit, ndraws = 20L),
    fitted = fitted(fit, ndraws = 20L),
    detection = predict(fit, type = "detection", ndraws = 20L)
  )
  expect_identical(ncol(per_visit$epred), nrow(dat))
  expect_identical(ncol(per_visit$predict), nrow(dat))
  expect_identical(nrow(per_visit$fitted), nrow(dat))
  expect_identical(nrow(per_visit$detection), nrow(dat))

  # And neither grain is the series count, which is what the
  # replaced fixture could not say.
  expect_false(n_unit == n_site)
  expect_false(nrow(dat) == n_unit)
})


test_that("seeing the species settles the state at that unit", {
  # The one claim occupancy makes that arithmetic cannot soften: a
  # unit where the species was detected is occupied, so its
  # conditional probability is exactly one and not merely close to
  # it. A model conditioning on the wrong unit's history puts a
  # number below one somewhere in this set, and every value stays in
  # [0, 1] while it does.
  cond <- colMeans(posterior_occupancy(fit, conditional = TRUE,
                                       draw = FALSE))
  uncond <- colMeans(posterior_occupancy(fit, conditional = FALSE,
                                         draw = FALSE))
  expect_length(cond, n_unit)
  expect_true(all(cond >= 0 & cond <= 1))
  expect_true(all(uncond > 0 & uncond < 1))

  expect_equal(as.numeric(cond[detected]),
               rep(1, sum(detected)), tolerance = 1e-10)

  # Non-detection is evidence of absence, so conditioning has to
  # move an undetected unit down rather than leaving it alone. The
  # two directions together are what say the detection history
  # reached the state at all: a conditional surface that ignored it
  # returns the unconditional one and fails both.
  expect_true(all(cond[detected] > uncond[detected]))
  expect_true(all(cond[!detected] < uncond[!detected]))

  # `predict(type = "latent_state")` reports the same quantity and
  # has to agree with it.
  ls <- predict(fit, type = "latent_state", ndraws = 500L)
  expect_identical(colnames(ls),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_true(all(ls[, "Estimate"] >= 0 & ls[, "Estimate"] <= 1))
  expect_equal(as.numeric(ls[detected, "Estimate"]),
               rep(1, sum(detected)), tolerance = 1e-10)
})


test_that("the saturation table agrees with the detections", {
  # A unit is saturated when its latent state is pinned at the
  # ceiling, which for occupancy is one and happens exactly when the
  # species was seen. So this table is checkable against the data
  # rather than against another posterior summary.
  sat <- latent_N_saturation(fit)
  expect_true(is.data.frame(sat))
  expect_identical(nrow(sat), n_unit)
  expect_true(all(c("unit", "label", "K_max", "p_saturated",
                    "saturated") %in% names(sat)))
  # Occupancy is presence or absence, so the ceiling is one.
  expect_true(all(sat$K_max == 1L))
  expect_identical(as.logical(sat$saturated), unname(detected))
  expect_identical(sat$p_saturated == 1, unname(detected))
})


test_that("the saturation table names its units", {
  # The labels come back as `1_1`, `1_2`, `1_3`: the series index
  # and the occasion index. This fit's units are `site_01` at times
  # 3, 4 and 5, so a reader has no way from the table back to a site
  # or a date, and the mapping is positional and undocumented.
  #
  # Fails today. It is the same fault as the `Process_k` shock
  # labels, in a table a reader is more likely to act on, since a
  # saturated unit is one whose survey effort was insufficient.
  # Recorded as finding 19.
  sat <- latent_N_saturation(fit)
  labs <- as.character(sat$label)
  expect_true(any(grepl(site_levels[1L], labs, fixed = TRUE)))
  expect_false(any(grepl("^[0-9]+_[0-9]+$", labs)))
})


test_that("residuals are bounded by what their definition allows", {
  # A quantile residual is a normal deviate through an empirical
  # PIT, which clamps at about 8.13; an ordinary residual is an
  # observed count minus a replicated one, so on a unit of four
  # visits it cannot exceed four. Values outside either bound mean
  # the residual was computed on the wrong scale or the wrong grain.
  qr <- residuals(fit, summary = FALSE, ndraws = 100L)
  expect_identical(dim(qr), c(100L, n_unit))
  expect_true(all(is.finite(qr)))
  expect_true(all(abs(qr) <= 8.5))

  orl <- residuals(fit, type = "ordinary", summary = FALSE,
                   ndraws = 100L)
  expect_identical(dim(orl), c(100L, n_unit))
  expect_true(all(is.finite(orl)))
  expect_true(all(abs(orl) <= n_visit))

  rs <- residuals(fit)
  expect_identical(colnames(rs),
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_true(all(rs[, "Q2.5"] <= rs[, "Estimate"]))
  expect_true(all(rs[, "Estimate"] <= rs[, "Q97.5"]))
})


test_that("augment recycles a unit's residual across its visits", {
  # The residual belongs to the unit and the fitted value to the
  # visit, so a per-visit frame carries one repeated across four rows
  # and the other varying within them. A frame that recycled both, or
  # neither, has the right number of rows either way.
  got <- with_warnings(augment(fit))
  aug <- got$value
  expect_true(is.data.frame(aug))
  expect_identical(nrow(aug), nrow(dat))
  expect_true(all(c(".fitted", ".resid", ".resid.se", ".unit") %in%
                    names(aug)))
  expect_identical(length(unique(aug$.unit)), n_unit)

  # Every unit, not merely the first one.
  by_unit <- split(seq_len(nrow(aug)), aug$.unit)
  expect_true(all(lengths(by_unit) == n_visit))
  for (idx in by_unit) {
    expect_identical(length(unique(aug$.resid[idx])), 1L)
    expect_identical(length(unique(aug$.resid.se[idx])), 1L)
  }
  # And the fitted value moves with the detection covariate, so it
  # was not recycled too.
  varies <- vapply(by_unit, function(idx) {
    length(unique(aug$.fitted[idx])) > 1L
  }, logical(1))
  expect_true(all(varies))

  # The notice this fit owes is about the coarse PIT support, once.
  pit <- grep("coarse PIT support", got$warnings, value = TRUE)
  expect_lte(length(pit), 1L)
  expect_identical(setdiff(got$warnings, pit), character(0))
})


test_that("a covariate that moves inside a unit cannot group a check", {
  # A posterior predictive check on this family compares observed
  # against replicated counts per unit, so a grouping has to be
  # constant within one. `tod_c` changes at every visit and `elev`
  # is a property of the site, which is the distinction the refusal
  # is drawing.
  err <- expect_error(
    pp_check(fit, type = "bars_grouped", group = "tod_c",
             ndraws = 20L),
    "not constant within every closure unit"
  )
  expect_match(conditionMessage(err), "tod_c", fixed = TRUE)

  got <- with_warnings(
    pp_check(fit, type = "bars_grouped", group = "elev", ndraws = 20L)
  )
  expect_s3_class(got$value, "ggplot")
})


test_that("the checks that read a unit's count all render", {
  # 100 draws rather than 30: a residual histogram built from few
  # draws shows apparent outliers that are an artefact of the PIT
  # support, and the package says so. The count is chosen to satisfy
  # that advice rather than to dodge it, and the advice itself is
  # pinned in the block below.
  got <- with_warnings({
    lapply(c("bars", "rootogram", "ecdf_overlay", "hist", "freqpoly",
             "dens_overlay", "stat_2d", "resid_hist", "resid_qq"),
           function(ty) pp_check(fit, type = ty, ndraws = 100L))
  })
  for (p in got$value) expect_s3_class(p, "ggplot")
  expect_s3_class(
    pp_check(fit, type = "stat", stat = function(x) mean(x == 0)),
    "ggplot"
  )
  # The coarse-PIT notice is owed once per session, and nothing else
  # is owed at all.
  other <- grep("coarse PIT support", got$warnings, value = TRUE,
                invert = TRUE)
  expect_identical(other, character(0))
})


test_that("too few draws for a residual histogram is called out", {
  # The advice that made the block above ask for a hundred draws. A
  # quantile residual on a coarse discrete support needs enough draws
  # for the PIT to fill in, and below that the histogram shows
  # outliers that are an artefact of the method rather than of the
  # fit. Silence here would let a reader take those for a finding.
  got <- with_warnings(pp_check(fit, type = "resid_hist", ndraws = 8L))
  expect_s3_class(got$value, "ggplot")
  expect_true(any(grepl("PIT support underflow", got$warnings)))

  # And the advice stops once the count is adequate.
  ample <- with_warnings(pp_check(fit, type = "resid_hist",
                                  ndraws = 100L))
  expect_false(any(grepl("PIT support underflow", ample$warnings)))
})


test_that("the checks that need a per-row grain are refused", {
  # Each of these compares one observation against one replicate, and
  # a closure-unit fit has no such pairing: the likelihood is a unit's
  # whole detection history. Refusing by name beats plotting the
  # first visit of each unit and calling it the fit.
  for (ty in c("scatter_avg", "error_binned", "resid_acf",
               "resid_vs_fitted")) {
    expect_error(pp_check(fit, type = ty), "not available for")
  }
})


test_that("the two extractions are drawn from the same iterations", {
  # A closure-unit family reads a detection probability and a latent
  # state from the posterior separately. Unless a count is resolved
  # to concrete draw indices first, each subsamples on its own and
  # pairs a probability with a state from an unrelated iteration.
  for (nd in c(5L, 25L)) {
    ll <- log_lik(fit, ndraws = nd)
    expect_identical(nrow(ll), nd)
    expect_true(all(is.finite(ll)))
  }
  ids <- c(2L, 9L, 41L, 300L)
  a <- log_lik(fit, draw_ids = ids)
  expect_identical(nrow(a), length(ids))
  # Naming the same draws twice gives the same answer, which it
  # cannot if the two reads sample apart.
  expect_identical(a, log_lik(fit, draw_ids = ids))

  expect_identical(nrow(posterior_epred(fit, ndraws = 5L)), 5L)
  expect_identical(nrow(posterior_predict(fit, ndraws = 5L)), 5L)
  expect_identical(nrow(residuals(fit, summary = FALSE, ndraws = 5L)),
                   5L)
})


test_that("a detection draw is a detection, and an epred a probability", {
  # The observation is binary, so a draw on the response scale is in
  # {0, 1} while the expectation is strictly between them. A surface
  # returning one where the other was asked for is finite, correctly
  # shaped and wrong.
  pp <- posterior_predict(fit, ndraws = 50L)
  expect_true(all(as.numeric(pp) %in% c(0, 1)))
  ep <- posterior_epred(fit, ndraws = 50L)
  expect_true(all(ep > 0 & ep < 1))
  # The expectation of a visit is occupancy times detection, so it
  # cannot exceed either.
  det <- predict(fit, type = "detection", ndraws = 500L)
  expect_true(all(det[, "Estimate"] > 0 & det[, "Estimate"] < 1))
  expect_true(all(colMeans(ep) <= det[, "Estimate"] + 1e-6))
})


test_that("the criticism surface runs on a closure-unit fit", {
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  # One likelihood term per closure unit, so the diagnostic is
  # counted on units rather than on visits.
  expect_identical(length(ic$diagnostics$pareto_k), n_unit)
  expect_identical(
    any(ic$diagnostics$pareto_k > 0.7),
    any(grepl("Pareto k", loo_warnings))
  )
  expect_true(all(grepl("Pareto k", loo_warnings)))
})


test_that("summary and the tidiers name the occupancy structure", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  # Both submodels are reported, so a reader can tell occupancy from
  # detection rather than seeing one block of coefficients.
  expect_true(any(grepl("elev", txt, fixed = TRUE)))
  expect_true(any(grepl("tod_c", txt, fixed = TRUE)))

  vars <- variables(fit)
  expect_true("b_elev" %in% vars)
  expect_true("b_p_tod_c" %in% vars)

  expect_true(is.data.frame(tidy(fit)))
  expect_true(is.data.frame(glance(fit)))
})


cat("\nDone.\n")
