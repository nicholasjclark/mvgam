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


# Map each catalog type to the observation formula a user would
# fit. gp() smooths require an explicit k argument
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


# The latent dynamics each type is simulated and fitted under.
# These reproduce the per-type defaults in the sim_mvgam catalog;
# one entry drives both sides of a case (see
# `trend_model_from_formula()`), so a fit never sees a different
# kernel from the one that generated its data.
type_trends <- list(
  `1` = ~ RW(),
  `2` = ~ AR(p = 1),
  `3` = ~ AR(p = 1),
  `4` = ~ AR(p = 1),
  `5` = ~ AR(p = 1),
  `6` = ~ CAR()
)


# `mvgam()` reads the trend constructor from the right-hand side
# of `trend_formula`, while `sim_mvgam()` wants the evaluated
# `mvgam_trend` object. Evaluating that right-hand side here lets
# a single formula specify both, rather than two spellings that
# can drift apart.
trend_model_from_formula <- function(trend_formula) {
  eval(trend_formula[[2L]], envir = environment(trend_formula))
}


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


# Fits are cached under a prefix of this file's own, so a fit built
# for another file cannot answer here. Delete the files to refit.
cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, paste0("val_recovery_", name, ".rds"))
}

fit_recovery_cached <- function(name, ...) {
  path <- cache_path(name)
  if (file.exists(path)) {
    cat("[cache]", name, "\n")
    return(readRDS(path))
  }
  cat("[fit  ]", name, "\n")
  fit <- mvgam(..., refresh = 0L, silent = 2L, backend = "cmdstanr")
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  fit
}


# Fit-time helper: builds the simulated data + fits a small mvgam.
# `family_arg` is the actual family object the FIT uses; usually
# the same as the sim family but parameterised for future cases
# (e.g. fit Gamma data with a Gamma family).
run_recovery_case <- function(label, type, family, family_arg,
                                n_timepoints = 120L,
                                seed = 42L, min_cor = 0.6) {
  trend_formula <- type_trends[[as.character(type)]]
  sim <- sim_mvgam(
    type = type, family = family, n_series = 1L,
    n_timepoints = n_timepoints,
    trend_model = trend_model_from_formula(trend_formula),
    seed = seed
  )
  formula <- adjust_formula_for_family(
    type_formulas[[as.character(type)]], family_arg
  )
  fit <- fit_recovery_cached(
    gsub(" ", "_", label, fixed = TRUE),
    formula = formula, trend_formula = trend_formula,
    data = sim$data_train, family = family_arg,
    chains = 1L, iter = 1000L, warmup = 500L
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
  # generic smooth-correlation recovery check doesn't apply. GP
  # recovery belongs in a separate conditional_effects() comparison.
  list(label = "type 6 gaussian", type = 6L,
       family = gaussian(), family_arg = gaussian(),
       min_cor = 0.55)
)


# ---- Per-case tests ------------------------------------------------

# Each case's fit is kept so the blocks below can ask more of it than
# a smooth correlation.
cases <- new.env(parent = emptyenv())

for (case in RECOVERY_GRID) {
  local({
    cc <- case
    test_that(paste0(cc$label, " recovers true smooths"), {
      out <- run_recovery_case(
        label = cc$label, type = cc$type, family = cc$family,
        family_arg = cc$family_arg,
        min_cor = cc$min_cor
      )
      assign(cc$label, out, envir = cases)
      for (i in seq_along(out$cors)) {
        nm <- names(out$cors)[[i]]
        cor_i <- out$cors[[i]]
        expect_false(is.na(cor_i))
        expect_gt(cor_i, cc$min_cor)
      }
    })
  })
}


test_that("a trial count reaches a prediction through datagrid", {
  # The binomial case is the only fit here carrying an aterm, and
  # `trials` is data the family needs rather than a predictor. A
  # grid built without it leaves the expectation to fall back on
  # some other row's trial count, which is finite, bounded and
  # wrong.
  library(marginaleffects)
  options("marginaleffects_model_classes" = "mvgam")
  out <- get("type 1 binomial", envir = cases)
  mv <- out$fit

  grid <- datagrid(x = 0, trials = c(10, 50, 100), model = mv)
  expect_true("trials" %in% names(grid))
  expect_identical(nrow(grid), 3L)

  p <- predictions(mv, newdata = grid, type = "expected",
                   process_error = FALSE)
  # A binomial expectation is p * trials, and `x` is held fixed
  # across the grid, so the three estimates stand in the ratio of
  # their trial counts. A prediction that dropped the aterm returns
  # three equal numbers and satisfies any bounds check.
  expect_lt(abs(p$estimate[2L] / p$estimate[1L] - 5), 0.05)
  expect_lt(abs(p$estimate[3L] / p$estimate[1L] - 10), 0.05)
  # And the implied probability is the same at every trial count,
  # which is what says the trials entered as a denominator rather
  # than as a covariate.
  probs <- p$estimate / grid$trials
  expect_lt(diff(range(probs)), 1e-6)
  expect_true(all(probs > 0 & probs < 1))
})


# ---- The degraded case ---------------------------------------------
#
# Forty occasions with fifteen per cent of the responses missing: the
# hardest frame the simulator can put in front of a smooth, and the
# one where the observation axis and the trend axis have most room to
# disagree, since the rows that leave the likelihood are exactly the
# ones the trend still has to hold a state for.
#
# Sparse-lag trends are covered against their own lags in
# test-ar-multilag.R, which is a sharper claim than asking whether a
# smooth survives one.

test_that("a short, gappy series still recovers its smooth", {
  trend_formula <- type_trends[["1"]]
  sim <- sim_mvgam(
    type = 1L, family = gaussian(), n_timepoints = 40L,
    prop_missing = 0.15,
    trend_model = trend_model_from_formula(trend_formula),
    seed = 88L
  )
  n_rows <- nrow(sim$data_train)
  n_missing <- sum(is.na(sim$data_train$y))
  # The premise: rows really are missing, so the claims below are
  # about a gappy frame rather than a complete one that happens to
  # pass.
  expect_gt(n_missing, 0L)
  expect_lt(n_missing, n_rows)

  # brms announces the response rows it drops, and says so once.
  dropped <- character(0)
  fit <- withCallingHandlers(
    fit_recovery_cached(
      "short_gappy",
      formula = y ~ s(x), trend_formula = trend_formula,
      data = sim$data_train, family = gaussian(),
      chains = 1L, iter = 1000L, warmup = 500L
    ),
    warning = function(w) {
      dropped <<- c(dropped, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # The notice arrives on the run that fits and not on a cached read,
  # so its absence is not asserted; what is asserted is that nothing
  # other than that notice was raised.
  expect_true(all(grepl("Rows containing NAs", dropped)))

  # The smooth is still recovered. The bar is lower than the
  # full-length cases above because forty occasions is the point of
  # this block, not an oversight.
  out <- smooth_recovery_cor(fit, sim, "s(x)")
  expect_true(out$found)
  expect_gt(out$cor, 0.5)

  # And this is why it survives: the response rows leave the
  # likelihood while the trend keeps its own time grid, so the latent
  # state is still defined at every occasion the frame supplied. A
  # trend axis rebuilt from the rows that reached brms would be
  # shorter, and the smooth would then be evaluated against a grid
  # that had quietly lost its gaps.
  ax <- mvgam:::mvgam_axes(fit)
  n_times <- length(unique(sim$data_train$time))
  expect_identical(as.integer(fit$standata$N_time_trend), n_times)
  expect_identical(as.integer(ax$time$values),
                   sort(unique(as.integer(sim$data_train$time))))
  # The two counts are the whole point, and they differ: the rows
  # that reached the likelihood are short by the gaps, while the
  # trend holds a state at every occasion the frame supplied.
  expect_identical(as.integer(fit$standata$N), n_rows - n_missing)
  expect_lt(as.integer(fit$standata$N), n_times)
})
