# The closure-unit grain, and what happens at its edges.
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
# Six fits follow the occupancy one, each answering a question the
# first cannot. Two of them put the closure unit under a factor
# model, which is where the trend design moves to the latent-factor
# axis while an observation still has to read a species cell.
#
# The rest: An `nmix()` fit gives the ceiling claim
# a range to be wrong across, since a population size can sit below
# an observed count where a boolean occupancy cannot. A pair of occ
# fits, one complete and one with every sixth visit unmade, say what
# happens when the repeat-visit arrays and the likelihood disagree
# about how many rows there are.
#
# Run with:
#   testthat::test_file("tests/local/test-closure-units.R")

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

# A ggplot is returned whether or not a layer received any data, so
# asserting the class passes on the empty panel it looks like it is
# guarding. Building the plot is what forces the layers to resolve.
expect_drawn <- function(p) {
  expect_s3_class(p, "ggplot")
  layers <- ggplot2::ggplot_build(p)$data
  expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  invisible(layers)
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

# ----------------------------------------------------------------------
# An abundance ceiling, and the grid the kernel numbers
# ----------------------------------------------------------------------
#
# `occ()` pins its latent state at one, so the ceiling claim above is
# about a boolean. `nmix()` estimates a population size, where the
# same claim has a range to be wrong across: N is the population a
# binomial count was drawn from, so it can never sit below the
# largest count seen at that unit. A permuted unit grid puts a small
# site's ceiling on a large one and breaks it.

nmix_sim <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(4242L)
    n_site <- 12L
    n_season <- 2L
    n_visit <- 3L
    sites <- paste0("s", sprintf("%02d", seq_len(n_site)))
    # Time-major, so the frame's own unit order interleaves sites
    # across occasions and differs from sorting by (series, time).
    # Built site-major the two coincide, and the ordering claim below
    # cannot fail -- which is what its own guard checks.
    grid <- expand.grid(
      visit = seq_len(n_visit), site = sites,
      season = seq_len(n_season), stringsAsFactors = FALSE
    )
    # Abundance varies strongly across sites so the ceiling claim has
    # room to fail: a grid off by one puts a site of ~2 animals
    # against a count from a site of ~30.
    elev <- seq(-1.5, 1.5, length.out = n_site)
    lambda <- exp(1.4 + 1.3 * elev)
    # Units run site-fastest within a season, matching the frame's
    # own row order, so `rep(N_true, each = n_visit)` below lands
    # each unit's population on its own visits.
    N_true <- rpois(n_site * n_season, rep(lambda, times = n_season))
    d <- data.frame(
      series = factor(grid$site, levels = sites),
      time = as.integer(grid$season) + 4L,
      elev = elev[match(grid$site, sites)],
      y = as.integer(rbinom(nrow(grid), rep(N_true, each = n_visit),
                            0.65))
    )
    # `nmix()` marginalises the latent population up to a ceiling, so
    # the frame carries one. It sits well above the largest N drawn
    # here, since a cap that binds would truncate the very quantity
    # the ceiling claim below is about.
    d$cap <- as.integer(max(N_true) * 3L)
    cached <<- list(data = d, n_unit = n_site * n_season,
                    n_site = n_site, N_true = N_true)
    cached
  }
})

nmix_cache <- cache_path("val_mvgam_closure_nmix_units.rds")
if (file.exists(nmix_cache)) {
  nmix_fit <- readRDS(nmix_cache)
} else {
  nmix_fit <- with_warnings(mvgam(
    formula = y ~ elev, family = nmix(), data = nmix_sim()$data,
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  ))$value
  saveRDS(nmix_fit, nmix_cache)
}


test_that("no unit is given fewer animals than were counted there", {
  # N is the population a binomial count is drawn from, so the
  # posterior for a unit cannot sit below the largest count observed
  # at that unit. This is the one claim an abundance model makes that
  # holds whatever the seed or the sampler settings.
  d <- nmix_sim()$data
  state <- as.data.frame(hindcast(nmix_fit, type = "latent_state"))
  observed <- stats::aggregate(y ~ series + time, data = d, FUN = max)
  merged <- merge(state, observed, by = c("series", "time"))
  expect_identical(nrow(merged), nrow(state))
  expect_identical(nrow(merged), nmix_sim()$n_unit)
  expect_true(all(merged$median >= merged$y))
  expect_true(all(merged$upper_95 >= merged$y))
  # The counts have to differ across units, or a single ceiling would
  # satisfy the comparison everywhere.
  expect_gt(stats::sd(merged$y), 1)
})


test_that("the unit grid keeps the kernel's own ordering", {
  # `build_closure_unit_arrays()` numbers units by first appearance
  # in the time-major frame and the draw matrix columns follow that,
  # so the grid must not be re-sorted on the way out.
  #
  # The defect this guards is a permutation, which `expect_setequal()`
  # cannot see: both orders hold the same labels. So the check is on
  # the sequence, and it is paired with a claim that the frame's own
  # order is not the sorted one -- otherwise the two coincide and the
  # first check says nothing.
  d <- nmix_sim()$data
  state <- as.data.frame(hindcast(nmix_fit, type = "latent_state"))
  units <- unique(d[, c("series", "time")])
  expect_identical(nrow(state), nrow(units))
  expect_identical(paste(state$series, state$time),
                   paste(units$series, units$time))
  expect_false(identical(
    paste(units$series, units$time),
    paste(units$series, units$time)[order(units$series, units$time)]
  ))
})


test_that("the abundance ceiling is reported against real labels", {
  # The same table finding 19 covers for occupancy. Here the ceiling
  # is an estimated count rather than one, so the label is what tells
  # a reader which site to revisit.
  sat <- latent_N_saturation(nmix_fit)
  expect_identical(nrow(sat), nmix_sim()$n_unit)
  expect_true(all(sat$K_max >= 1L))
  labs <- as.character(sat$label)
  expect_true(any(grepl(levels(nmix_sim()$data$series)[1L], labs,
                        fixed = TRUE)))
  expect_false(any(grepl("^[0-9]+_[0-9]+$", labs)))
})


# ----------------------------------------------------------------------
# Occasions that were never visited
# ----------------------------------------------------------------------
#
# A missing response is a visit that did not happen, which is the
# normal case in repeat-visit data. These families used to refuse it:
# the Stan code aggregates visits per unit through `visit_idx` and
# `n_rep`, both built from the raw frame while brms sized the
# likelihood to the observed rows, so the indices pointed past the
# end of the response.

gappy_fits <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    complete <- dat
    gappy <- complete
    # Every sixth visit goes unmade, spread across sites rather than
    # clustered, so no unit loses all of its visits.
    gappy$y[seq(2L, nrow(gappy), by = 6L)] <- NA_integer_
    fit_one <- function(d, nm) {
      path <- cache_path(paste0("val_mvgam_occ_visits_", nm, ".rds"))
      if (file.exists(path)) return(readRDS(path))
      # brms reports the rows it dropped, once per internal pass.
      out <- withCallingHandlers(
        mvgam(y ~ 1, data = d, family = occ(), chains = 2L,
              iter = 800L, warmup = 400L, silent = 2, seed = 11L,
              backend = "cmdstanr"),
        warning = function(w) {
          if (grepl("Rows containing NAs", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      )
      saveRDS(out, path)
      out
    }
    cached <<- list(complete = fit_one(complete, "complete"),
                    gappy = fit_one(gappy, "gappy"),
                    data_gappy = gappy)
    cached
  }
})


test_that("the unit arrays cover the visits that happened", {
  obj <- gappy_fits()
  sd <- obj$gappy$standata
  n_obs <- sum(!is.na(obj$data_gappy$y))
  expect_identical(as.integer(sd$N), n_obs)
  # Every repeat count adds up to the observed rows, and no index
  # reaches past the response the likelihood was given.
  expect_identical(sum(as.integer(sd$n_rep)), n_obs)
  expect_lte(max(as.integer(sd$visit_idx)), n_obs)
  expect_setequal(as.integer(sd$visit_idx), seq_len(n_obs))
  # The unit count is unchanged: a unit that lost a visit is still a
  # unit, which is the whole point of the padded grid.
  expect_identical(as.integer(sd$N_unit), n_unit)
  # And the gaps are real, or this file is testing the complete fit
  # twice.
  expect_gt(sum(is.na(obj$data_gappy$y)), 0L)
  expect_lt(n_obs, nrow(dat))
})


test_that("dropping visits widens the estimate without moving it", {
  # Fewer detections carry less information about occupancy, so the
  # gappy fit stays compatible with the complete one rather than
  # drifting somewhere else.
  obj <- gappy_fits()
  full <- as.numeric(as.array(obj$complete, variable = "b_Intercept"))
  gaps <- as.numeric(as.array(obj$gappy, variable = "b_Intercept"))
  expect_lte(stats::quantile(gaps, 0.05), stats::median(full))
  expect_gte(stats::quantile(gaps, 0.95), stats::median(full))
  # Less data cannot sharpen the estimate.
  expect_gte(stats::sd(gaps), stats::sd(full) * 0.9)

  # Predictions still cover every row the frame supplies, including
  # the ones the likelihood never saw.
  pp <- posterior_predict(obj$gappy, ndraws = 20L)
  expect_identical(ncol(pp), nrow(obj$data_gappy))
  gaps_at <- which(is.na(obj$data_gappy$y))
  expect_true(all(is.finite(pp[, gaps_at])))
  expect_true(all(as.numeric(pp) %in% c(0, 1)))
})

# ----------------------------------------------------------------------
# Closure units under a factor model
# ----------------------------------------------------------------------
#
# The fits above give each series its own latent state. A `jsdgam()`
# gives K species N_lv shared factors instead, and puts the closure
# unit underneath: the trend design moves to the factor axis while an
# observation still has to read a species cell. Two families reach
# that composition, and they differ only in what the latent state is
# -- binary occupancy for `occ()`, a population size for `nmix()` --
# so the questions are asked once, in `closure_jsdm_battery()`, and
# what each family recovers is written out beneath it.
#
# `s(env, by = lv_axis())` puts one smooth on the factor grain, which
# is the second thing these fits carry that nothing above does: the
# trend design has `n_sites * N_lv` rows where the observation side
# has `n_sites * K * n_visits`.
#
# Sites are numbered from 3 in both, so a site identifier never
# equals its own rank.

jsdm_K <- 4L
jsdm_N_lv <- 2L
jsdm_visits <- 3L
jsdm_p_true <- 0.6
jsdm_species <- paste0("sp", seq_len(jsdm_K))

# Two orthogonal smooth shapes in env, standardised, so the factor
# model has something to tell apart on the factor axis.
jsdm_lv_true <- function(env) {
  lv <- cbind(sin(env), env^2 - mean(env^2))
  scale(lv, center = TRUE, scale = apply(lv, 2L, sd))
}


# Verbatim from the file this replaces, seed and random calls in the
# same order, so the cached fit stays valid.
sim_jsdm_occ <- function() {
  set.seed(607L)
  n_sites <- 50L
  env <- sort(runif(n_sites, -2, 2))
  lv_true <- jsdm_lv_true(env)
  Z_true <- matrix(rnorm(jsdm_K * jsdm_N_lv, sd = 1.0),
                   nrow = jsdm_K, ncol = jsdm_N_lv)
  Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
  attr(Z_true, "scaled:center") <- NULL
  b_int <- rnorm(jsdm_K, mean = 0, sd = 0.5)

  logit_psi <- matrix(NA_real_, nrow = jsdm_K, ncol = n_sites)
  for (s in seq_len(jsdm_K)) {
    for (i in seq_len(n_sites)) {
      logit_psi[s, i] <- b_int[s] + sum(Z_true[s, ] * lv_true[i, ])
    }
  }
  z_latent <- matrix(rbinom(jsdm_K * n_sites, 1L,
                            1 / (1 + exp(-logit_psi))),
                     nrow = jsdm_K, ncol = n_sites)
  site_ids <- seq_len(n_sites) + 2L

  rows <- list()
  for (s in seq_len(jsdm_K)) {
    for (i in seq_len(n_sites)) {
      for (v in seq_len(jsdm_visits)) {
        rows[[length(rows) + 1L]] <- data.frame(
          species = jsdm_species[s], site = site_ids[i],
          env = env[i], visit = v,
          y = if (z_latent[s, i] == 1L) {
            rbinom(1L, 1L, jsdm_p_true)
          } else {
            0L
          }
        )
      }
    }
  }
  d <- do.call(rbind, rows)
  d$species <- factor(d$species, levels = jsdm_species)
  sigma_true_cov <- tcrossprod(Z_true)
  list(
    data = d, n_sites = n_sites, K = jsdm_K, N_lv = jsdm_N_lv,
    n_visits = jsdm_visits, species_levels = jsdm_species,
    Z_true = Z_true, z_latent = z_latent, env = env,
    sigma_true_cor = cov2cor(sigma_true_cov + diag(1e-8, jsdm_K))
  )
}


sim_jsdm_nmix <- function() {
  set.seed(606L)
  n_sites <- 30L
  env <- sort(runif(n_sites, -2, 2))
  lv_true <- jsdm_lv_true(env)
  Z_true <- matrix(rnorm(jsdm_K * jsdm_N_lv, sd = 0.4),
                   nrow = jsdm_K, ncol = jsdm_N_lv)
  Z_true <- scale(Z_true, center = TRUE, scale = FALSE)
  attr(Z_true, "scaled:center") <- NULL
  b_int <- rnorm(jsdm_K, mean = 0.5, sd = 0.3)

  log_lambda <- matrix(NA_real_, nrow = jsdm_K, ncol = n_sites)
  for (s in seq_len(jsdm_K)) {
    for (i in seq_len(n_sites)) {
      log_lambda[s, i] <- b_int[s] + sum(Z_true[s, ] * lv_true[i, ])
    }
  }
  N_latent <- matrix(rpois(jsdm_K * n_sites, exp(log_lambda)),
                     nrow = jsdm_K, ncol = n_sites)
  cap_true <- max(N_latent) + 5L
  site_ids <- seq_len(n_sites) + 2L

  rows <- list()
  for (s in seq_len(jsdm_K)) {
    for (i in seq_len(n_sites)) {
      for (v in seq_len(jsdm_visits)) {
        rows[[length(rows) + 1L]] <- data.frame(
          species = jsdm_species[s], site = site_ids[i],
          env = env[i], visit = v,
          y = rbinom(1L, N_latent[s, i], jsdm_p_true),
          cap = cap_true
        )
      }
    }
  }
  d <- do.call(rbind, rows)
  d$species <- factor(d$species, levels = jsdm_species)
  sigma_true_cov <- tcrossprod(Z_true)
  list(
    data = d, n_sites = n_sites, K = jsdm_K, N_lv = jsdm_N_lv,
    n_visits = jsdm_visits, species_levels = jsdm_species,
    Z_true = Z_true, N_latent = N_latent, cap_true = cap_true,
    env = env,
    sigma_true_cor = cov2cor(sigma_true_cov + diag(1e-8, jsdm_K))
  )
}


fit_jsdm_closure <- function(nm, sim, family) {
  cache <- cache_path(paste0("val_mvgam_jsdgam_mv_", nm, ".rds"))
  if (file.exists(cache)) return(readRDS(cache))
  fit <- jsdgam(
    formula = y ~ species,
    factor_formula = ~ s(env, by = lv_axis(), k = 5) - 1,
    data = sim$data, unit = site, species = species,
    family = family, n_lv = sim$N_lv,
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  )
  saveRDS(fit, cache)
  fit
}


closure_jsdm_battery <- function(nm, sim, fit, threshold_cor,
                                 state_ok) {
  K <- sim$K
  N_lv <- sim$N_lv
  lev <- sim$species_levels
  n_sites <- sim$n_sites
  n_visits <- sim$n_visits
  # `unit = site` makes mvgam synthesise its own `time` and `series`
  # columns, so the frame every prediction call is given is the one
  # the fit kept rather than the one the simulation built. Handing
  # over the simulation frame raises on a missing `time`, which is
  # how the file this replaces once had its central check erroring in
  # place of running.
  d <- as.data.frame(fit$obs_data)
  raw <- sim$data
  n_unit_jsdm <- K * n_sites
  says <- function(claim) paste0(nm, ": ", claim)
  dm <- as_draws_matrix(fit$fit)
  post_cor <- residual_cor(fit)$cor
  true_off <- sim$sigma_true_cor[upper.tri(sim$sigma_true_cor)]
  post_off <- post_cor[upper.tri(post_cor)]

  test_that(says("the loadings recover the simulated covariance"), {
    expect_gt(stats::cor(true_off, post_off), threshold_cor)
    expect_lt(mean(abs(true_off - post_off)), 0.6)
  })

  test_that(says("the closure units are the species-site cells"), {
    # A closure unit is a (species, site) cell with three visits. It
    # is neither a species nor a row, and all three counts differ
    # here, so a marginalisation taken over the wrong grouping shows
    # in the count alone.
    expect_identical(as.integer(fit$standata$N_unit), n_unit_jsdm)
    expect_identical(nrow(unique(raw[, c("species", "site")])),
                     n_unit_jsdm)
    expect_identical(as.integer(fit$standata$N), nrow(raw))
    expect_false(n_unit_jsdm == K)
    expect_false(n_unit_jsdm == nrow(d))
  })

  test_that(says("the trend design is split by factor, on one basis"), {
    # `by = lv_axis()` puts the design on the factor axis. It has to
    # come out block-complementary across the factors and evaluated
    # at one shared basis. Read off the data Stan is handed rather
    # than the program text, since a design built on the species axis
    # and relabelled has correct dimensions and wrong content.
    sd <- fit$standata
    expect_identical(as.integer(sd$N_trend), n_sites * N_lv)
    expect_identical(dim(sd$times_trend), c(n_sites, N_lv))
    r1 <- as.integer(sd$times_trend[, 1L])
    r2 <- as.integer(sd$times_trend[, 2L])
    expect_length(intersect(r1, r2), 0L)
    expect_identical(sort(c(r1, r2)), seq_len(n_sites * N_lv))

    X <- sd$Xs_trend
    expect_identical(nrow(X), n_sites * N_lv)
    expect_identical(ncol(X), N_lv)
    expect_true(all(X[r1, 2L] == 0))
    expect_true(all(X[r2, 1L] == 0))
    # One smooth split two ways: the same covariate value reaches
    # both factors, in their own columns.
    expect_equal(unname(X[r1, 1L]), unname(X[r2, 2L]))

    zs <- grep("^Zs_[0-9]+_[0-9]+_trend$", names(sd), value = TRUE)
    expect_length(zs, N_lv)
    Z1 <- sd$Zs_1_1_trend
    Z2 <- sd$Zs_2_1_trend
    expect_identical(dim(Z1), dim(Z2))
    expect_true(all(Z1[r2, ] == 0))
    expect_true(all(Z2[r1, ] == 0))
    expect_equal(unname(Z1[r1, ]), unname(Z2[r2, ]))
    expect_equal(as.integer(sd$knots_1_trend),
                 as.integer(sd$knots_2_trend))
  })

  test_that(says("an observation reads a species cell, not a factor"), {
    # `times_trend` moved to the factor axis, but `trend[t, s]` stays
    # species-grained because the program folds through `Z`. Running
    # `obs_trend_series` over factors stays in range, samples, and
    # silently gives four species two states.
    s_rec <- as.integer(fit$standata$obs_trend_series)
    expect_identical(sort(unique(s_rec)), seq_len(K))
    expect_identical(s_rec, match(as.character(d$species), lev))
    expect_identical(as.integer(table(s_rec)),
                     rep(n_sites * n_visits, K))
  })

  test_that(says("every prediction surface answers at the row grain"), {
    ep <- posterior_epred(fit, draw_ids = 1:20)
    pp <- posterior_predict(fit, draw_ids = 1:20)
    expect_identical(dim(ep), c(20L, nrow(d)))
    expect_identical(dim(pp), c(20L, nrow(d)))
    expect_true(all(is.finite(ep)))
    expect_true(all(is.finite(pp)))
    # `fitted()` summarises the draws `posterior_epred()` returns, so
    # its Estimate column is their column mean exactly.
    ft <- fitted(fit, draw_ids = 1:20)
    expect_identical(nrow(ft), nrow(d))
    expect_equal(unname(ft[, "Estimate"]), unname(colMeans(ep)),
                 tolerance = 1e-8)
    # The density belongs to the unit, not the row: one term per
    # closure unit is what the marginalisation produces.
    ll <- log_lik(fit, draw_ids = 1:20)
    expect_identical(ncol(ll), n_unit_jsdm)
    expect_true(all(is.finite(ll)))
  })

  test_that(says("the closure-unit prediction types answer per unit"), {
    # The two quantities these families exist to separate: the latent
    # state and the detection probability. Confusing their grain is
    # what makes a state estimate read as a detection rate, so the
    # width is checked as well as the scale.
    ls <- predict(fit, type = "latent_state", ndraws = 20L)
    det <- predict(fit, type = "detection", ndraws = 20L)
    expect_identical(nrow(ls), n_unit_jsdm)
    expect_identical(nrow(det), nrow(d))
    expect_true(all(det[, "Estimate"] >= 0 & det[, "Estimate"] <= 1))
    expect_true(state_ok(as.numeric(ls[, "Estimate"])))
  })

  test_that(says("each row reads the latent cell the sampler drew"), {
    t_rec <- as.integer(fit$standata$obs_trend_time)
    s_rec <- as.integer(fit$standata$obs_trend_series)
    expect_length(t_rec, nrow(d))
    want <- vapply(paste0("trend[", t_rec, ",", s_rec, "]"),
                   function(k) mean(dm[, k]), numeric(1))
    got <- colMeans(
      mvgam:::extract_trend_latent_states(fit, newdata = d,
                                          full_draws = dm)
    )
    expect_equal(unname(got), unname(want))
    expect_gt(stats::sd(want), 1e-6)
  })

  test_that(says("a shuffled frame answers the same, in the new order"), {
    set.seed(19L)
    perm <- sample(nrow(d))
    base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                            incl_autocor = TRUE)
    shuf <- posterior_epred(fit, newdata = d[perm, , drop = FALSE],
                            draw_ids = 1:10, incl_autocor = TRUE)
    expect_equal(unname(base[, perm, drop = FALSE]), unname(shuf))
    expect_gt(stats::sd(colMeans(base)), 1e-8)
  })

  test_that(says("a frame holding whole units for some species maps"), {
    # Closure units have to stay whole, so the cut is by species
    # rather than by row. A frame carrying some of the species in a
    # different order is what separates an axis read off the record
    # from one rebuilt out of the levels the frame happens to carry.
    base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                            incl_autocor = TRUE)
    for (subset in list(lev[c(2L, 4L)], lev[c(4L, 1L, 3L)])) {
      rows <- which(as.character(d$species) %in% subset)
      sub <- d[rows, , drop = FALSE]
      sub$series <- factor(as.character(sub$series), levels = subset)
      sub$species <- factor(as.character(sub$species),
                            levels = subset)
      expect_true(all(table(paste(sub$species, sub$time)) == n_visits))
      got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                             incl_autocor = TRUE)
      expect_equal(unname(got), unname(base[, rows, drop = FALSE]))
    }
  })

  test_that(says("a frame naming an unknown species is refused"), {
    nd <- d
    nd$series <- factor(
      ifelse(seq_len(nrow(nd)) == 1L, "sp_unseen",
             as.character(nd$series)),
      levels = c(lev, "sp_unseen")
    )
    err <- expect_error(
      posterior_epred(fit, newdata = nd, draw_ids = 1:5),
      "Series levels in newdata not found in training data"
    )
    expect_match(conditionMessage(err), "sp_unseen", fixed = TRUE)
    for (s in lev) {
      expect_match(conditionMessage(err), s, fixed = TRUE)
    }
  })

  test_that(says("hindcast arms are the species, in order, distinct"), {
    arms <- hindcast(fit, ndraws = 20L)$hindcasts
    expect_identical(names(arms), lev)
    same <- character(0)
    for (i in seq_along(arms)) {
      for (j in seq_along(arms)) {
        if (j <= i) next
        if (isTRUE(all.equal(arms[[i]], arms[[j]]))) {
          same <- c(same, paste(names(arms)[i], names(arms)[j],
                                sep = "="))
        }
      }
    }
    expect_identical(same, character(0))
  })

  test_that(says("residual_cor is labelled by the species axis"), {
    expect_identical(rownames(post_cor), lev)
    expect_identical(colnames(post_cor), lev)
    expect_equal(unname(diag(post_cor)), rep(1, K))
    expect_equal(unname(post_cor), unname(t(post_cor)))
    expect_gt(max(abs(post_off)), 0.05)
  })

  test_that(says("the factor methods report the fit's own loadings"), {
    af <- active_factors(fit)
    expect_s3_class(af, "mvgam_active_factors")
    expect_identical(as.integer(af$n_lv), N_lv)
    expect_identical(nrow(af$per_factor), N_lv)

    sv <- shared_variation(fit)
    expect_identical(as.character(sv$series_names), lev)
    expect_identical(as.integer(sv$n_series), K)
    expect_identical(dim(sv$delta), c(K, K))
    expect_equal(unname(sv$delta), unname(t(sv$delta)))
    expect_true(all(diag(sv$delta) > 0))

    Z_m <- apply(
      mvgam:::extract_Z_loadings(dm, n_obs_series = K, n_lv = N_lv),
      c(2L, 3L), mean
    )
    expect_identical(dim(Z_m), c(K, N_lv))
    # `Z` is [species, factor], so a permutation of its rows gives
    # every species another's loadings while leaving the recovery
    # correlation above almost unchanged. Every pair, not just the
    # opening one.
    same <- character(0)
    for (i in seq_len(K)) {
      for (j in seq_len(K)) {
        if (j <= i) next
        if (isTRUE(all.equal(Z_m[i, ], Z_m[j, ]))) {
          same <- c(same, paste(lev[i], lev[j], sep = "="))
        }
      }
    }
    expect_identical(same, character(0))
    # The distinctness above holds vacuously on loadings that are all
    # near zero, so they have to be materially non-zero. `nmix()`
    # draws its truth at half the occupancy fit's scale and centres
    # it, so the floor is the smaller of the two.
    expect_gt(max(abs(Z_m)), 0.05)
  })

  test_that(says("the env smooth is drawn once per latent factor"), {
    # `s(env, by = lv_axis())` is the only smooth, and it is indexed
    # by latent factor rather than by species. Checking that the call
    # returns leaves the two ways it can be wrong untouched: an empty
    # grid, which comes back correctly named with no rows, and a grid
    # built over the observation frame, which carries no `.trend`
    # column and cannot separate the factors.
    sm <- smooths(fit)
    expect_length(sm, 1L)
    expect_match(sm[1L], ".trend", fixed = TRUE)

    ps <- posterior_smooths(fit, smooth = sm[1L], ndraws = 20L)
    expect_identical(dim(ps), c(20L, n_sites * N_lv))
    expect_true(all(is.finite(ps)))

    cs <- conditional_smooths(fit)
    expect_length(cs, 1L)
    cd <- cs[[1L]]
    expect_s3_class(cd, "data.frame")
    expect_gt(nrow(cd), 0L)
    expect_true(all(is.finite(cd$estimate__)))
    expect_true(all(cd$lower__ <= cd$estimate__))
    expect_true(all(cd$estimate__ <= cd$upper__))
    # One curve per latent factor, and the two differ: a design that
    # collapsed the factor axis draws one shape twice.
    curves <- split(cd$estimate__, cd$cond__)
    expect_length(curves, N_lv)
    expect_false(isTRUE(all.equal(curves[[1L]], curves[[2L]])))
    # Drawn over the covariate the trend side actually saw.
    trend_env <- fit$trend_model$data$env
    expect_gte(min(cd$effect1__), min(trend_env) - 1e-8)
    expect_lte(max(cd$effect1__), max(trend_env) + 1e-8)
  })

  test_that(says("summary and the criticism methods run on this fit"), {
    txt <- capture.output(summary(fit))
    expect_true(any(grepl(paste0("Series:\\s*", K), txt)))
    seen <- character(0)
    ic <- withCallingHandlers(loo(fit), warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
    # One likelihood term per closure unit, so the diagnostic is
    # counted on units rather than on visits.
    expect_identical(length(ic$diagnostics$pareto_k), n_unit_jsdm)
    expect_true(all(is.finite(ic$diagnostics$pareto_k)))
    expect_true(all(grepl("Pareto", seen)))
  })

  test_that(says("pp_check and the plotting methods render"), {
    expect_drawn(pp_check(fit, ndraws = 40L))
    for (ty in c("trend", "factors")) {
      expect_drawn(plot(fit, type = ty))
    }
    ce <- conditional_effects(fit)
    expect_s3_class(ce, "mvgam_conditional_effects")
    expect_gt(length(ce), 0L)
    for (eff in names(ce)) {
      cd <- ce[[eff]]$data
      expect_true(all(is.finite(cd$estimate)))
      expect_true(all(cd$conf.low <= cd$estimate))
      expect_true(all(cd$estimate <= cd$conf.high))
      expect_gt(max(cd$conf.high - cd$conf.low), 0)
    }
  })

  invisible(NULL)
}


occ_jsdm <- sim_jsdm_occ()
occ_jsdm_fit <- fit_jsdm_closure("occ", occ_jsdm, occ())
closure_jsdm_battery(
  "jsdgam occ", occ_jsdm, occ_jsdm_fit, threshold_cor = 0.5,
  # Occupancy is a probability.
  state_ok = function(x) all(x >= 0 & x <= 1)
)

nmix_jsdm <- sim_jsdm_nmix()
nmix_jsdm_fit <- fit_jsdm_closure("nmix", nmix_jsdm, nmix())
closure_jsdm_battery(
  "jsdgam nmix", nmix_jsdm, nmix_jsdm_fit, threshold_cor = 0.7,
  # A latent population is a non-negative count under the cap.
  state_ok = function(x) {
    all(x >= 0) && all(x <= nmix_jsdm$cap_true)
  }
)


test_that("jsdgam occ: the detection probability recovers the truth", {
  # Printed and unchecked in the file this replaces, so a detection
  # probability that had run to 0 or 1 would have been reported
  # without comment.
  dm <- as_draws_matrix(occ_jsdm_fit$fit)
  p_cols <- grep("^b_p_Intercept$|^Intercept_p$|^p$", colnames(dm),
                 value = TRUE)
  expect_gt(length(p_cols), 0L)
  p_post <- as.numeric(dm[, p_cols[1L]])
  p_resp <- if (grepl("^b_|^Intercept", p_cols[1L])) {
    1 / (1 + exp(-p_post))
  } else {
    p_post
  }
  expect_true(all(p_resp > 0 & p_resp < 1))
  expect_lt(abs(mean(p_resp) - jsdm_p_true), 0.2)
})


test_that("jsdgam nmix: detection and the identified mode both hold", {
  dm <- as_draws_matrix(nmix_jsdm_fit$fit)
  p_cols <- grep("^b_p_Intercept$|^Intercept_p$|^p$", colnames(dm),
                 value = TRUE)
  expect_gt(length(p_cols), 0L)
  p_post <- as.numeric(dm[, p_cols[1L]])
  p_resp <- if (grepl("^b_|^Intercept", p_cols[1L])) {
    1 / (1 + exp(-p_post))
  } else {
    p_post
  }
  expect_lt(abs(mean(p_resp) - jsdm_p_true), 0.2)

  # `nmix()` has no simplex constraint, so the loadings are not
  # pinned to sum to zero the way the softmax families are. They do
  # have to stay near the identified mode the Heaps QR targets: a
  # column sum that has wandered means they are drifting along an
  # unidentified direction, which is a claim the file this replaces
  # made in a comment and never checked.
  Z_m <- apply(
    mvgam:::extract_Z_loadings(dm, n_obs_series = nmix_jsdm$K,
                               n_lv = nmix_jsdm$N_lv),
    c(2L, 3L), mean
  )
  expect_lt(max(abs(colSums(Z_m))), 0.5)
  expect_gt(max(abs(Z_m)), 0.05)
})
