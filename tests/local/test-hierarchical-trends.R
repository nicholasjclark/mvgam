# End-to-end post-fit coverage for hierarchical trends.
#
# `gr` and `subgr` make mvgam derive the series identifier itself
# rather than read a column, and that derived value has to agree with
# the levels recorded at fit time. When it did not, every post-fit
# method on a hierarchical fit failed at the level validator while
# `summary()` kept working, so nothing in the suite noticed. These
# tests drive the whole surface against the cached fit.
#
# Run with:
#   testthat::test_file("tests/local/test-hierarchical-trends.R")

source("setup_tests_local.R")
source("concordance_helpers.R")


test_that("a superseded series column is reported in the derived spelling", {
  # `warn_series_superseded()` tells the user the fit will label this
  # series `r1_sp1`. Reporting the column that was superseded instead
  # contradicts the warning and hands back a name that no other
  # surface answers to.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  derived <- fit$trend_metadata$levels$series
  reported <- mvgam:::resolve_series_info(fit)$series_levels
  expect_setequal(reported, derived)
  expect_false(any(grepl(".", reported, fixed = TRUE)))

  hc <- hindcast(fit, type = "expected", ndraws = 5L)
  expect_identical(names(hc$hindcasts), reported)
  expect_identical(as.character(hc$series_names), reported)
  # Naming the arms one way and cutting them another empties them.
  expect_true(all(vapply(hc$hindcasts, ncol, integer(1)) > 0L))
})


test_that("the fitted series levels match the trend metadata", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  derived <- levels(droplevels(mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )))
  expect_setequal(derived, fit$trend_metadata$levels$series)
})


test_that("the prediction stack runs on a hierarchical fit", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  n_obs <- nrow(fit$data)

  expect_equal(dim(posterior_epred(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(dim(posterior_predict(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(dim(posterior_linpred(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(dim(log_lik(fit, ndraws = 5L)), c(5L, n_obs))
  expect_equal(nrow(predict(fit, ndraws = 5L)), n_obs)
  expect_equal(nrow(fitted(fit, ndraws = 5L)), n_obs)
  expect_equal(nrow(residuals(fit, ndraws = 5L)), n_obs)
})


test_that("feeding the training data back as newdata is a no-op", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  # Compared on the deterministic path. With `process_error = TRUE`
  # the expectation is marginalised over the trend by drawing fresh
  # innovations, so two calls differ by construction.
  bare <- posterior_epred(fit, draw_ids = 1:5, process_error = FALSE)
  with_nd <- posterior_epred(
    fit, newdata = fit$data, draw_ids = 1:5, process_error = FALSE
  )
  expect_equal(dim(bare), dim(with_nd))
  expect_equal(bare, with_nd)

  lp <- posterior_linpred(fit, draw_ids = 1:5, process_error = FALSE)
  lp_nd <- posterior_linpred(
    fit, newdata = fit$data, draw_ids = 1:5, process_error = FALSE
  )
  expect_equal(lp, lp_nd)
})


test_that("the marginal expectation redraws innovations each call", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  # `process_error = TRUE` integrates over the trend by Monte Carlo,
  # so repeating the call on the same draws gives a different answer.
  # Reproducible output needs an explicit seed. The argument has to be
  # passed to get that: the default leaves the trend at its
  # deterministic submodel, and this asserted a difference without it,
  # so it had been comparing one fixed answer against itself.
  a <- posterior_epred(fit, draw_ids = 1:5, process_error = TRUE)
  b <- posterior_epred(fit, draw_ids = 1:5, process_error = TRUE)
  expect_false(isTRUE(all.equal(a, b)))

  # The default is the other half of the contract, and it is the half
  # a user relies on when they call the same surface twice.
  expect_equal(
    posterior_epred(fit, draw_ids = 1:5),
    posterior_epred(fit, draw_ids = 1:5)
  )
})


test_that("the criticism surface runs on a hierarchical fit", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  ic <- SW(loo(fit))
  expect_s3_class(ic, "loo")
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_ggplot(SW(pp_check(fit, ndraws = 10L)))
  expect_ggplot(SW(pp_check(fit, type = "resid_qq", ndraws = 50L)))
  hc <- hindcast(fit, ndraws = 5L)
  expect_s3_class(hc, "mvgam_forecast")
  expect_equal(length(hc$hindcasts), fit$series_info$n_series)
})


test_that("a grouping combination absent from training is rejected", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  nd <- fit$data
  # Each level below is known on its own, but this pairing never
  # appeared, so the derived series identifier is new.
  nd[[vars$subgr_var]] <- "sp_unseen"
  expect_error(
    posterior_epred(fit, newdata = nd, ndraws = 5L),
    "Series levels in newdata not found in training data"
  )
})


test_that("the derived series identifier is stable and lexical", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  vals <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  # Underscore-joined, grouping variable first, lexically ordered, so
  # the labels sort predictably in post-fit output.
  expect_true(all(grepl("_", levels(vals), fixed = TRUE)))
  expect_equal(levels(vals), sort(levels(vals)))
})


test_that("a superseded series column warns once, and obeys silent", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  derived <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  # The warning is silent under TESTTHAT so the suite stays quiet;
  # clear it here to exercise the path a user actually meets.
  withr::local_envvar(TESTTHAT = "")

  count_warnings <- function() {
    n <- 0L
    withCallingHandlers(
      mvgam:::warn_series_superseded(
        fit$data, "series", derived, vars$gr_var, vars$subgr_var
      ),
      warning = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    n
  }

  # One notice per event: insight::format_warning() would raise its
  # own on top of the one rlang::warn() emits.
  withr::local_options(mvgam.silent = 0L)
  expect_equal(count_warnings(), 1L)

  rlang::reset_warning_verbosity("mvgam_series_superseded")
  withr::local_options(mvgam.silent = 2L)
  expect_equal(count_warnings(), 0L)
})


test_that("a series column matching the derived one is not flagged", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  derived <- mvgam:::hierarchical_series_values(
    fit$data, vars$gr_var, vars$subgr_var
  )
  dat <- fit$data
  dat$series <- derived
  withr::local_envvar(TESTTHAT = "")
  rlang::reset_warning_verbosity("mvgam_series_superseded")
  expect_silent(
    mvgam:::warn_series_superseded(
      dat, "series", derived, vars$gr_var, vars$subgr_var
    )
  )
})


test_that("the resolved series index is the one the fit sampled with", {
  # `standata$obs_trend_series` is the column index the fit gave each
  # training row, so it settles what `trend[t, s]` means. Deriving
  # that order a second time from the data is how a prediction comes
  # to disagree with it, and on a hierarchical fit the two derivations
  # are a permutation of one another: the rebuilt series sorts
  # region-major, the column it superseded sorts otherwise. Both carry
  # every label, so the mismatch raises nothing and every series reads
  # another series' state. This test reads the record rather than
  # asking two derivations whether they agree with each other.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  recorded <- as.integer(fit$standata$obs_trend_series)
  expect_length(recorded, nrow(d))
  expect_gt(length(unique(recorded)), 1L)

  resolved <- as.integer(
    mvgam:::get_observation_structure(fit, newdata = d)$series_int
  )
  expect_identical(resolved, recorded)

  # And the state each row reads is the cell Stan sampled for it.
  dm <- posterior::as_draws_matrix(fit$fit)
  t_rec <- as.integer(fit$standata$obs_trend_time)
  want <- vapply(seq_len(nrow(d)), function(j) {
    mean(dm[, paste0("trend[", t_rec[j], ",", recorded[j], "]")])
  }, numeric(1))
  got <- colMeans(
    mvgam:::extract_trend_latent_states(fit, newdata = d, full_draws = dm)
  )
  expect_equal(unname(got), unname(want))
})


test_that("each series reads its own latent state, not the first one's", {
  # A prediction frame holding one series is the shape every
  # per-series hindcast arm passes down, and it is the shape that
  # exposed the defect: the series index was read off the levels the
  # frame carried rather than the levels the model was fitted on, so
  # a single-series frame numbered its one series 1 whatever it was.
  # A `series` column survives subsetting with its levels intact and
  # so hid this; a hierarchical series, rebuilt from `gr` and `subgr`
  # on whatever rows it is handed, does not.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  os <- mvgam:::get_observation_structure(fit, newdata = d)
  levs <- os$series_levels
  expect_gt(length(levs), 1L)

  resolved <- vapply(seq_along(levs), function(k) {
    rows <- which(as.character(os$series) == levs[k])
    unique(mvgam:::get_observation_structure(
      fit, newdata = d[rows, , drop = FALSE]
    )$series_int)
  }, integer(1L))
  expect_identical(resolved, seq_along(levs))
})


test_that("a hierarchical hindcast agrees with the conditional epred", {
  # `hindcast()` and `posterior_epred(incl_autocor = TRUE)` are two
  # routes to one quantity: the mean given the latent state the model
  # inferred. They compose it differently, `hindcast()` per series
  # and `posterior_epred()` across the whole frame, so they agree
  # only when both resolve the same state for the same cell.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  time_var <- fit$trend_metadata$variables$time_var

  hc <- hindcast(fit, type = "expected")
  blocks <- hc$hindcasts
  expect_gt(length(blocks), 1L)

  lab <- mvgam:::training_series_labels(fit, d)
  cells <- unlist(lapply(names(blocks), function(s) {
    rows <- which(lab == s)
    rows[order(d[[time_var]][rows])]
  }))
  ep <- posterior_epred(fit, incl_autocor = TRUE)
  expect_equal(
    unname(ep[, cells, drop = FALSE]),
    unname(do.call(cbind, blocks))
  )

  # Every series carrying the first one's state is what the defect
  # looked like, and it agreed on shape throughout. Two series of a
  # fit with per-series dynamics do not hindcast identically.
  expect_false(isTRUE(all.equal(blocks[[1L]], blocks[[2L]])))
})
