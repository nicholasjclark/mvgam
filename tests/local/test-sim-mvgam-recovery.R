# Recoverability gate for sim_mvgam (Phase D of the rewrite).
# For each (type, family) pair listed in RECOVERY_GRID, simulate
# from the documented default, fit a SMALL mvgam, and assert the
# posterior mean of each true smooth correlates with the truth
# above `min_cor`.
#
# Runs LOCAL only (Stan fits are too heavy for CI). Invoke via:
#   testthat::test_file("tests/local/test-sim-mvgam-recovery.R")
#
# Adding a new family or type: append a row to RECOVERY_GRID and
# pass any family-specific arguments via `family_pars`. The helper
# `run_recovery_case()` is family-agnostic.


# Map each catalog type to the formula a user would fit. The
# trend_model lives on the simulated mvgam_sim$trend_model slot
# so the recovery fit always matches the generative trend.
# gp() smooths require an explicit k argument
# (Hilbert-space GP basis); fully-exact GP is not supported as
# a fit-time term.
type_formulas <- list(
  `1` = y ~ s(x),
  `2` = y ~ s(x) + s(z),
  `3` = y ~ s(x) + (1 | grp),
  `4` = y ~ gp(x, k = 20),
  `5` = y ~ x,
  `6` = y ~ s(season, bs = "cc")
)


# Family-specific formula adjustment: binomial needs the
# `| trials(trials)` aterm so brms knows the trial-count column.
adjust_formula_for_family <- function(formula, family) {
  fam_name <- tolower(family$family)
  if (fam_name == "binomial") {
    return(stats::update(
      formula, y | trials(trials) ~ .
    ))
  }
  formula
}


# Build a newdata frame for posterior_smooths: starts from a
# representative row of data_train (so non-focal covariates
# carry valid baseline values), then varies the focal covariate
# across the truth grid.
make_newdata <- function(sim, focal_col, focal_grid) {
  ref <- sim$data_train[1L, , drop = FALSE]
  out <- ref[rep(1L, length(focal_grid)), , drop = FALSE]
  out[[focal_col]] <- focal_grid
  rownames(out) <- NULL
  out
}


# Compute correlation between the posterior mean of one smooth
# and the simulated ground truth. Uses `posterior_smooths()` to
# evaluate the smooth on the truth grid directly. Matches the
# smooth in the fit by FOCAL COVARIATE NAME so the lookup is
# robust to mvgam's full-form naming
# (e.g. simulator stores "s(season)", fitter exposes
# "s(season, bs = \"cc\")"; both reference covariate "season").
smooth_recovery_cor <- function(fit, sim, sim_smooth_name) {
  truth_df <- sim$true_smooths[[sim_smooth_name]]
  cov_col <- setdiff(colnames(truth_df), "f_true")[1L]
  available <- smooths(fit)
  # Match by regex: smooth must reference the focal covariate
  # as its first argument, e.g. "s(x, ...)" or "gp(x, ...)".
  pat <- paste0("\\(", cov_col, "[,)]")
  fit_label <- available[grepl(pat, available)][1L]
  if (is.na(fit_label)) {
    return(list(cor = NA_real_, found = FALSE,
                 label = NA_character_))
  }
  newdata <- make_newdata(sim, cov_col, truth_df[[cov_col]])
  draws <- suppressMessages(posterior_smooths(
    fit, smooth = fit_label, newdata = newdata
  ))
  fit_mean <- colMeans(draws)
  ok <- is.finite(fit_mean) & is.finite(truth_df$f_true)
  if (sum(ok) < 5L) {
    return(list(cor = NA_real_, found = TRUE,
                 label = fit_label))
  }
  list(
    cor = stats::cor(fit_mean[ok], truth_df$f_true[ok]),
    found = TRUE,
    label = fit_label
  )
}


# Fit-time helper: builds the simulated data + fits a small mvgam.
# `family_arg` is the actual family object the FIT uses; usually
# the same as the sim family but parameterised for future cases
# (e.g. fit Gamma data with a Gamma family).
run_recovery_case <- function(type, family, family_arg,
                                n_timepoints = 120L,
                                seed = 42L, min_cor = 0.6) {
  sim <- sim_mvgam(
    type = type, family = family, n_series = 1L,
    n_timepoints = n_timepoints, seed = seed
  )
  formula <- adjust_formula_for_family(
    type_formulas[[as.character(type)]], family_arg
  )
  fit <- mvgam(
    formula = formula, trend_model = sim$trend_model,
    data = sim$data_train, family = family_arg,
    chains = 1L, iter = 1000L, warmup = 500L,
    refresh = 0L, silent = 2L, backend = "cmdstanr"
  )
  cors <- vapply(
    names(sim$true_smooths),
    function(nm) {
      out <- smooth_recovery_cor(fit, sim, nm)
      if (!out$found) return(NA_real_)
      out$cor
    },
    numeric(1L)
  )
  list(sim = sim, fit = fit, cors = cors,
       all_recovered = all(cors > min_cor, na.rm = TRUE) &
         !any(is.na(cors)))
}


# Configuration: each row is one (type, family, family_arg) case.
# Add rows here to extend coverage. The `family_arg` is whatever
# the user would pass to `mvgam(family = ...)`; usually identical
# to `family` but split out so we can probe robustness when the
# sim and fit families differ (e.g. simulate negbinomial, fit
# Poisson).
RECOVERY_GRID <- list(
  list(label = "type 1 gaussian", type = 1L,
       family = gaussian(), family_arg = gaussian(),
       min_cor = 0.7),
  list(label = "type 2 gaussian", type = 2L,
       family = gaussian(), family_arg = gaussian(),
       min_cor = 0.6),
  list(label = "type 1 poisson", type = 1L,
       family = poisson(), family_arg = poisson(),
       min_cor = 0.5),
  list(label = "type 1 binomial", type = 1L,
       family = binomial(), family_arg = binomial(),
       min_cor = 0.4),
  list(label = "type 3 gaussian", type = 3L,
       family = gaussian(), family_arg = gaussian(),
       min_cor = 0.55),
  # Type 4 (gp covariate) skipped: mvgam exposes gp() terms via
  # a separate API from smooths() / posterior_smooths(), so the
  # generic smooth-correlation recovery check doesn't apply.
  # GP recovery should be validated separately via
  # conditional_effects() comparison; deferred.
  list(label = "type 6 gaussian", type = 6L,
       family = gaussian(), family_arg = gaussian(),
       min_cor = 0.55)
)


# ---- Per-case tests ------------------------------------------------

for (case in RECOVERY_GRID) {
  local({
    cc <- case
    test_that(paste0(cc$label, " recovers true smooths"), {
      out <- run_recovery_case(
        type = cc$type, family = cc$family,
        family_arg = cc$family_arg,
        min_cor = cc$min_cor
      )
      for (i in seq_along(out$cors)) {
        nm <- names(out$cors)[[i]]
        cor_i <- out$cors[[i]]
        expect_false(is.na(cor_i))
        expect_gt(cor_i, cc$min_cor)
      }
    })
  })
}


# ---- Edge cases ----------------------------------------------------

# Edge: short series (n_timepoints = 40) — confirms recovery
# degrades gracefully but doesn't error.
test_that("short series still produces a finite fit", {
  sim <- sim_mvgam(type = 1L, family = gaussian(),
                    n_timepoints = 40L, seed = 99L)
  fit <- mvgam(
    formula = y ~ s(x), trend_model = sim$trend_model,
    data = sim$data_train, family = gaussian(),
    chains = 1L, iter = 600L, warmup = 300L,
    refresh = 0L, silent = 2L, backend = "cmdstanr"
  )
  expect_s3_class(fit, "mvgam")
})


# Edge: prop_missing > 0 — ensure NA injection doesn't break the
# fit.
test_that("missing-data injection doesn't break recovery", {
  sim <- sim_mvgam(type = 1L, family = gaussian(),
                    n_timepoints = 100L, prop_missing = 0.15,
                    seed = 88L)
  fit <- mvgam(
    formula = y ~ s(x), trend_model = sim$trend_model,
    data = sim$data_train, family = gaussian(),
    chains = 1L, iter = 1000L, warmup = 500L,
    refresh = 0L, silent = 2L, backend = "cmdstanr"
  )
  out <- smooth_recovery_cor(fit, sim, "s(x)")
  expect_true(out$found)
  expect_gt(out$cor, 0.5)
})


# Edge: AR(p = c(1, 3, 12)) sparse-lag trend override should still
# produce recoverable s(x) (the smooth is unaffected by trend
# spec).
test_that("sparse-lag AR trend override doesn't break recovery", {
  sim <- sim_mvgam(
    type = 1L, family = gaussian(),
    trend_model = AR(p = c(1L, 3L, 12L)),
    n_timepoints = 120L, seed = 77L
  )
  fit <- mvgam(
    formula = y ~ s(x), trend_model = sim$trend_model,
    data = sim$data_train, family = gaussian(),
    chains = 1L, iter = 1000L, warmup = 500L,
    refresh = 0L, silent = 2L, backend = "cmdstanr"
  )
  out <- smooth_recovery_cor(fit, sim, "s(x)")
  expect_true(out$found)
  expect_gt(out$cor, 0.5)
})
