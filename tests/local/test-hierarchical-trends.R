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
  derived <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  reported <- mvgam:::resolve_series_info(fit)$series_levels
  # Order, not membership. The defect this file exists for is a
  # permutation: both lists carry every label, so a set comparison
  # passes on exactly the arrangement that hands each series
  # another's trend column.
  expect_identical(reported, derived)
  expect_false(any(grepl(".", reported, fixed = TRUE)))

  hc <- hindcast(fit, type = "expected", ndraws = 5L)
  expect_identical(names(hc$hindcasts), reported)
  expect_identical(as.character(hc$series_names), reported)
  # Naming the arms one way and cutting them another empties them.
  expect_true(all(vapply(hc$hindcasts, ncol, integer(1)) > 0L))
})


test_that("the recorded axis is the grouping the frame states", {
  # Ground truth is built from the user's own columns rather than
  # from `hierarchical_series_values()`. Comparing the record against
  # the function that produced it asks one derivation whether it
  # agrees with itself, and passes however both are wrong.
  #
  # The rule a user is given: the grouping variable first, joined by
  # an underscore, ordered so a group's subgroups sit together.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  vars <- fit$trend_metadata$variables
  d <- as.data.frame(fit$data)
  pairs <- unique(d[, c(vars$gr_var, vars$subgr_var)])
  pairs <- pairs[order(pairs[[vars$gr_var]], pairs[[vars$subgr_var]]), ]
  expected <- paste(pairs[[vars$gr_var]], pairs[[vars$subgr_var]],
                    sep = "_")

  recorded <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  expect_identical(recorded, expected)
  # The count the trend matrix was built with agrees with it.
  expect_identical(as.integer(fit$standata$N_series_trend),
                   length(expected))
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

  # Column j of every surface above is row j of the frame. A
  # dimension check passes on any permutation of the series, which is
  # the defect a derived axis is prone to, so the identity each row
  # resolves to is compared against the grouping the frame states.
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  os <- mvgam:::get_observation_structure(fit, newdata = d)
  expect_identical(as.character(os$series), stated)
  expect_identical(os$series_levels, levs)
  expect_identical(as.integer(os$series_int), match(stated, levs))
  expect_identical(as.integer(os$time),
                   match(d[[vars$time_var]],
                         sort(unique(d[[vars$time_var]]))))
})


test_that("group_inds_trend puts each series in its own group", {
  # This array is the only place two series are told they share a
  # group, so it is what decides which series are correlated with
  # which. A wrong entry pools a series with another region's and
  # costs no error: every index is in range and the model samples.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  sd <- fit$standata
  axes <- mvgam:::mvgam_axes(fit)
  groups <- as.character(axes$series$groups)
  expect_length(groups, as.integer(sd$N_series_trend))

  # Group 1 is the first group to appear on the axis, so the integer
  # each series carries is the position of its own group label.
  expect_identical(as.integer(sd$group_inds_trend),
                   match(groups, unique(groups)))
  # Series in one region share an index; series in different regions
  # do not.
  by_group <- split(seq_along(groups), groups)
  for (idx in by_group) {
    expect_length(unique(as.integer(sd$group_inds_trend)[idx]), 1L)
  }
  expect_length(unique(as.integer(sd$group_inds_trend)),
                as.integer(sd$N_groups_trend))
})


test_that("the group and subgroup counts are the frame's own", {
  # An unbalanced design that is read as balanced draws a slice of a
  # correlation matrix built for more subgroups than the group has,
  # and every index stays in range.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  sd <- fit$standata
  vars <- fit$trend_metadata$variables
  d <- as.data.frame(fit$data)

  n_gr <- length(unique(d[[vars$gr_var]]))
  n_sub <- length(unique(d[[vars$subgr_var]]))
  expect_identical(as.integer(sd$N_groups_trend), n_gr)
  expect_identical(as.integer(sd$N_subgroups_trend), n_sub)

  # Every group carries every subgroup on this frame, so the series
  # count is the product. A design where it is not has to be refused
  # rather than folded into this shape.
  per_group <- tapply(d[[vars$subgr_var]], d[[vars$gr_var]],
                      function(x) length(unique(x)))
  expect_true(all(per_group == n_sub))
  expect_identical(as.integer(sd$N_series_trend), n_gr * n_sub)
})


test_that("the axis keeps each group's subgroups together", {
  # `group_inds_trend` runs in blocks, and it can only do that while
  # a group's subgroups are adjacent on the axis. An axis ordered
  # subgroup-major interleaves the regions, and anything reading the
  # grouping as contiguous then reads across a boundary.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  groups <- as.character(mvgam:::mvgam_axes(fit)$series$groups)
  expect_identical(groups, rep(unique(groups), each = 3L))
  expect_identical(as.integer(fit$standata$group_inds_trend),
                   rep(seq_along(unique(groups)), each = 3L))
})


test_that("the correlation block is sized by subgroup, not by series", {
  # The pooled correlation is between subgroups within a group, so it
  # is `N_subgroups` square. Built at the series dimension it would
  # be six by six and would correlate species across regions, which
  # is a different model that samples perfectly well.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  sd <- fit$standata
  n_sub <- as.integer(sd$N_subgroups_trend)
  dm <- posterior::as_draws_matrix(fit$fit)

  glob <- grep("^L_Omega_global_trend\\[", colnames(dm), value = TRUE)
  expect_length(glob, n_sub * n_sub)
  grp <- grep("^L_Omega_group_trend\\[", colnames(dm), value = TRUE)
  expect_length(grp, n_sub * n_sub * as.integer(sd$N_groups_trend))
})


test_that("the pooled correlation is a correlation matrix", {
  # A Cholesky factor of a correlation matrix is lower triangular
  # with unit-norm rows. Draws that drift off that are not a
  # correlation at all, and the entries stay finite and plausible
  # while every interval computed from them is wrong.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  n_sub <- as.integer(fit$standata$N_subgroups_trend)
  dm <- posterior::as_draws_matrix(fit$fit)

  # Draw by draw. The average of several Cholesky factors is not a
  # Cholesky factor -- its rows lose unit norm -- so a mean taken
  # first would report a violation that is only the averaging.
  draw_L <- function(k) {
    L <- matrix(0, n_sub, n_sub)
    for (i in seq_len(n_sub)) {
      for (j in seq_len(n_sub)) {
        L[i, j] <- dm[k, paste0("L_Omega_global_trend[", i, ",", j, "]")]
      }
    }
    L
  }
  for (k in unique(round(seq(1, nrow(dm), length.out = 10L)))) {
    L <- draw_L(k)
    # Upper triangle is structurally zero.
    expect_equal(L[upper.tri(L)], rep(0, sum(upper.tri(L))))
    # Unit-norm rows, so `L L'` is a correlation matrix.
    Omega <- tcrossprod(L)
    expect_equal(unname(diag(Omega)), rep(1, n_sub), tolerance = 1e-6)
    expect_true(all(abs(Omega[upper.tri(Omega)]) <= 1 + 1e-8))
    expect_true(all(eigen(Omega, only.values = TRUE)$values > -1e-8))
  }

  # The weight blending each group's correlation toward the pooled
  # one is a proportion.
  alpha <- as.numeric(dm[, "alpha_cor_trend"])
  expect_true(all(alpha >= 0 & alpha <= 1))
})


test_that("a shuffled newdata answers the same, in the new order", {
  # A prediction placing rows by position rather than by content
  # agrees with every check that hands back the training frame in its
  # own order, and disagrees here.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  set.seed(21L)
  perm <- sample(nrow(d))
  base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  shuf <- posterior_epred(fit, newdata = d[perm, , drop = FALSE],
                          draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(base[, perm, drop = FALSE]), unname(shuf))
})


test_that("a newdata holding one series reads that series' state", {
  # A hierarchical series is rebuilt from `gr` and `subgr` on
  # whatever rows it is handed, so a one-series frame carries only
  # its own grouping levels. An index taken from those levels
  # numbers it 1 whatever it is, and it reads the first series'
  # latent column. A `series` column would have survived subsetting
  # with its levels intact and hidden this.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  for (s in levs) {
    rows <- which(stated == s)
    sub <- d[rows, , drop = FALSE]
    sub[[vars$gr_var]] <- droplevels(factor(sub[[vars$gr_var]]))
    sub[[vars$subgr_var]] <- droplevels(factor(sub[[vars$subgr_var]]))
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("a hierarchical fit forecasts on its own axis", {
  # The forecast grid is rebuilt from the frame rather than read from
  # the record. This fit carries a `series` column that the grouping
  # superseded, so the two spellings differ and the rebuild has to
  # use the one the model was fitted on.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  h <- 3L
  last_t <- max(d[[vars$time_var]])

  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- last_t + seq_len(h)
    out
  }))
  expect_identical(nrow(future), length(levs) * h)

  fc <- forecast(fit, newdata = future, ndraws = 10L)
  expect_s3_class(fc, "mvgam_forecast")
  expect_identical(names(fc$forecasts), levs)
  for (s in levs) {
    expect_identical(dim(fc$forecasts[[s]]), c(10L, h))
  }
  # Per-series dynamics mean two series do not forecast identically.
  expect_false(isTRUE(all.equal(fc$forecasts[[1L]], fc$forecasts[[2L]])))
})


test_that("the forecast grid is cut by the axis, not by the column", {
  # The same fit and the same future frame, with the superseded
  # `series` column absent. The grouping is then the only spelling of
  # the axis, so the training tail is cut by the identity the model
  # was fitted on. This separates a fault in the hierarchical
  # forecast recursion from a fault in which column is read to reach
  # it, and it is the only route by which this fixture forecasts.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  h <- 3L
  last_t <- max(d[[vars$time_var]])

  bare <- fit
  bare$data[[vars$series_var]] <- NULL
  if (!is.null(bare$obs_data)) {
    bare$obs_data[[vars$series_var]] <- NULL
  }
  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- last_t + seq_len(h)
    out[[vars$series_var]] <- NULL
    out
  }))

  fc <- forecast(bare, newdata = future, ndraws = 20L)
  expect_identical(names(fc$forecasts), levs)
  for (s in levs) {
    expect_identical(dim(fc$forecasts[[s]]), c(20L, h))
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
  expect_false(isTRUE(all.equal(fc$forecasts[[1L]], fc$forecasts[[2L]])))
})


test_that("the one-step trend forecast follows this fit's own AR", {
  # Shape and keying say the forecast reached the right series; they
  # say nothing about whether the number is right. An AR(1) trend
  # steps as `ar1[s] * trend[T, s]` plus a mean-zero innovation, so
  # averaged over draws the first forecast step has to land on that
  # product. A recursion applied with another series' coefficient,
  # or started from another series' last state, gives a finite
  # trajectory of the right width and fails only here.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  h <- 1L
  last_t <- max(d[[vars$time_var]])
  n_time <- as.integer(fit$standata$N_time_trend)

  bare <- fit
  bare$data[[vars$series_var]] <- NULL
  if (!is.null(bare$obs_data)) {
    bare$obs_data[[vars$series_var]] <- NULL
  }
  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- last_t + seq_len(h)
    out[[vars$series_var]] <- NULL
    out
  }))

  n_draw <- 600L
  fc <- forecast(bare, newdata = future, ndraws = n_draw, type = "trend")
  dm <- posterior::as_draws_matrix(fit$fit)
  for (k in seq_along(levs)) {
    ar1 <- as.numeric(dm[, paste0("ar1_trend[", k, "]")])
    last <- as.numeric(dm[, paste0("trend[", n_time, ",", k, "]")])
    expected <- mean(ar1 * last)
    got <- mean(fc$forecasts[[levs[k]]][, 1L])
    # The innovation is mean zero, so the gap is Monte Carlo noise on
    # the innovation scale rather than a systematic offset.
    expect_lt(abs(got - expected), 0.35)
  }
})


test_that("summary, residual_cor and shared_variation name the axis", {
  # Everything a user reads off a hierarchical fit is labelled by the
  # derived identity rather than by the column it superseded. A
  # correct estimate under the wrong label is indistinguishable, to
  # the reader, from a wrong estimate.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)

  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  expect_true(any(grepl(paste0("Series:\\s*", length(levs)), txt)))

  # The pooled correlation is between subgroups within a group, so
  # it is the subgroup set that labels it rather than the six series.
  # A matrix built at the series dimension would correlate species
  # across regions, and the labels are how a reader tells which.
  vars <- fit$trend_metadata$variables
  subgr <- levels(factor(fit$data[[vars$subgr_var]]))
  rc <- residual_cor(fit)
  expect_identical(rownames(rc$cor), subgr)
  expect_identical(colnames(rc$cor), subgr)
  expect_identical(dim(rc$cor),
                   c(length(subgr), length(subgr)))
  expect_equal(unname(diag(rc$cor)), rep(1, length(subgr)))
  expect_equal(unname(rc$cor), unname(t(rc$cor)))

  # This fit has no latent factors, so the shared-variation
  # decomposition has nothing to decompose and says so rather than
  # returning a zero it would be read as a result.
  expect_error(shared_variation(fit), "requires a latent-factor fit")
})


test_that("the tidiers keep this fit's own row order", {
  # A tidier that re-sorts its output pairs each fitted value with
  # another row's observation while every column keeps its length.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables

  resp <- fit$response_names[1L]
  aug <- augment(fit)
  expect_true(is.data.frame(aug))
  expect_identical(nrow(aug), nrow(d))
  expect_equal(as.numeric(aug[[vars$time_var]]),
               as.numeric(d[[vars$time_var]]))
  expect_equal(as.numeric(aug$.observed), as.numeric(d[[resp]]))
  # The grouping columns travel with the row, so a reader can tell
  # which series each fitted value belongs to.
  expect_identical(as.character(aug[[vars$gr_var]]),
                   as.character(d[[vars$gr_var]]))
  expect_identical(as.character(aug[[vars$subgr_var]]),
                   as.character(d[[vars$subgr_var]]))

  expect_true(is.data.frame(tidy(fit)))
  expect_true(is.data.frame(glance(fit)))
  expect_true(any(grepl("^ar1_trend\\[", variables(fit))))
})


test_that("the plotting methods render for a hierarchical fit", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  for (ty in c("residuals", "trend", "series")) {
    # `plot()` returns a ggplot, so that is what is asserted. The
    # alternation this replaced ended in `is.list(p)`, which an empty
    # list satisfies: any method returning `list()` passed it.
    p <- plot(fit, type = ty)
    expect_s3_class(p, "ggplot")
  }
  expect_s3_class(mcmc_plot(fit), "ggplot")
})


test_that("conditional_effects has nothing to condition on here", {
  # The observation formula is intercept-only, so there is no
  # predictor to vary and the answer is no effects at all rather
  # than an empty panel. The formula is asserted alongside it, so a
  # fixture that gains a covariate fails here instead of quietly
  # turning this into a check of nothing.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  expect_identical(deparse1(stats::formula(fit$formula)), "y ~ 1")

  ce <- conditional_effects(fit)
  expect_s3_class(ce, "mvgam_conditional_effects")
  expect_length(ce, 0L)
})


test_that("marginaleffects reports the expected response", {
  # `predictions(type = "response")` reports the expected response,
  # which is what `posterior_epred()` returns. Handing back draws in
  # its place gives whole numbers on this count family, and the
  # comparison is what sees it whatever the family.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  withr::local_options(marginaleffects_model_classes = "mvgam")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  keyed <- paste(d[[vars$gr_var]], d[[vars$subgr_var]])
  grid <- d[!duplicated(keyed), , drop = FALSE]

  pr <- marginaleffects::predictions(fit, newdata = grid,
                                     type = "response")
  expect_identical(nrow(pr), nrow(grid))
  expect_true(all(is.finite(pr$estimate)))
  expect_true(all(pr$estimate > 0))
  ep <- colMeans(posterior_epred(fit, newdata = grid, ndraws = 400L))
  expect_equal(as.numeric(pr$estimate), as.numeric(ep),
               tolerance = 0.05)
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


test_that("a series column matching the derived one is left alone", {
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


test_that("the axis maps a hierarchical newdata with no draws at all", {
  # The completeness claim, on the frame that has no series column to
  # fall back to. A `gr` / `subgr` model derives the identifier from
  # two columns, so every draw-free resolver has to rebuild it rather
  # than read it, and each of these was a layer that once refused
  # such a frame outright.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")

  ids <- mvgam:::axis_row_series(fit, d)
  expect_identical(levels(ids), levs)
  expect_identical(as.character(ids), stated)

  # Row by row, not by position.
  set.seed(61L)
  perm <- sample(nrow(d))
  expect_identical(
    as.character(mvgam:::axis_row_series(fit, d[perm, , drop = FALSE])),
    stated[perm]
  )

  # And with the superseded column gone, which is the only spelling
  # a user of this model needs to supply.
  bare_d <- d
  bare_d[[vars$series_var]] <- NULL
  expect_false(vars$series_var %in% names(bare_d))
  expect_identical(as.character(mvgam:::axis_row_series(fit, bare_d)),
                   stated)

  # The training arms are cut by the derived identity.
  training <- mvgam:::build_training_arms(fit, levs)
  expect_identical(names(training$times), levs)
  user_times <- sort(unique(as.integer(d[[vars$time_var]])))
  for (s in levs) {
    expect_identical(as.integer(training$times[[s]]), user_times)
    expect_length(training$observations[[s]], length(user_times))
  }
  # Each arm holds its own series' observations, in time order.
  resp <- fit$response_names[1L]
  for (s in levs) {
    rows <- which(stated == s)
    rows <- rows[order(d[[vars$time_var]][rows])]
    expect_equal(training$observations[[s]],
                 as.numeric(d[[resp]][rows]))
  }

  # A frame past the training grid yields the new occasions per
  # series; one inside it yields none.
  h <- 4L
  future_times <- max(user_times) + seq_len(h)
  future <- do.call(rbind, lapply(levs, function(s) {
    proto <- d[which(stated == s)[1L], , drop = FALSE]
    out <- proto[rep(1L, h), , drop = FALSE]
    out[[vars$time_var]] <- future_times
    out
  }))
  grid <- mvgam:::resolve_forecast_grid(fit, future, training, levs)
  expect_false(is.null(grid))
  expect_identical(names(grid$times), levs)
  for (s in levs) {
    expect_identical(as.integer(grid$times[[s]]), future_times)
  }
  expect_null(mvgam:::resolve_forecast_grid(fit, d, training, levs))
})


test_that("a newdata holding a subset of series reads each of them", {
  # One series at a time is already covered. A frame carrying some
  # but not all of them is what separates an axis read off the record
  # from one rebuilt out of the groupings present, because here the
  # levels in hand are a proper subset in a different order.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)

  # Two cuts: one within a single group, one spanning both, so a
  # resolver that happens to work block-wise is not let through.
  subsets <- list(levs[c(1L, 2L)], levs[c(5L, 1L, 4L)])
  for (subset in subsets) {
    rows <- which(stated %in% subset)
    sub <- d[rows, , drop = FALSE]
    sub[[vars$gr_var]] <- droplevels(factor(sub[[vars$gr_var]]))
    sub[[vars$subgr_var]] <- droplevels(factor(sub[[vars$subgr_var]]))
    got <- posterior_epred(fit, newdata = sub, draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})


test_that("how the grouping columns are typed does not move an answer", {
  # The identifier is rebuilt from `gr` and `subgr`, so the way those
  # two columns happen to be stored is exactly the sort of thing that
  # can leak into the axis. Character columns are what a user gets
  # from a frame built without `stringsAsFactors`; a reversed level
  # order is what a user gets from `factor(levels = ...)`; a level
  # with no rows is what survives subsetting a larger frame.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  base <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)

  chr <- d
  chr[[vars$gr_var]] <- as.character(chr[[vars$gr_var]])
  chr[[vars$subgr_var]] <- as.character(chr[[vars$subgr_var]])
  expect_type(chr[[vars$gr_var]], "character")
  expect_equal(
    unname(posterior_epred(fit, newdata = chr, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(base)
  )

  rev_nd <- d
  rev_nd[[vars$subgr_var]] <- factor(
    as.character(rev_nd[[vars$subgr_var]]),
    levels = rev(levels(factor(d[[vars$subgr_var]])))
  )
  expect_equal(
    unname(posterior_epred(fit, newdata = rev_nd, draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(base)
  )

  # A declared level with no rows is refused here, which is the
  # opposite of what the same shape does on a fit whose series is a
  # column: there an unused level is carried without comment. The
  # refusal is asserted as the contract this path actually has, and
  # it is a good refusal, naming the level and listing the ones that
  # would have worked. Whether the two paths ought to differ is the
  # open question recorded in FINDINGS.md, not something to settle by
  # writing the assertion either way.
  extra <- d
  extra[[vars$subgr_var]] <- factor(
    as.character(extra[[vars$subgr_var]]),
    levels = c(levels(factor(d[[vars$subgr_var]])), "sp_absent")
  )
  expect_identical(sum(extra[[vars$subgr_var]] == "sp_absent"), 0L)
  err <- expect_error(
    posterior_epred(fit, newdata = extra, draw_ids = 1:10,
                    incl_autocor = TRUE),
    "levels not in training data"
  )
  expect_match(conditionMessage(err), "sp_absent", fixed = TRUE)
  for (lv in levels(factor(d[[vars$subgr_var]]))) {
    expect_match(conditionMessage(err), lv, fixed = TRUE)
  }
})


test_that("single and repeated rows read the cell they name", {
  # The smallest frames there are. A mapping that numbers series by
  # order of appearance is right by accident for whichever series
  # comes first and wrong for the rest, and duplicate rows are what
  # every prediction grid is built from.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  levs <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
  stated <- paste(d[[vars$gr_var]], d[[vars$subgr_var]], sep = "_")
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)

  for (s in levs) {
    j <- which(stated == s)[1L]
    one <- posterior_epred(fit, newdata = d[j, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_identical(dim(one), c(10L, 1L))
    expect_equal(unname(one), unname(full[, j, drop = FALSE]))
  }

  j <- which(stated == levs[4L])[1L]
  rep_nd <- d[c(j, j, j), , drop = FALSE]
  got <- posterior_epred(fit, newdata = rep_nd, draw_ids = 1:10,
                         incl_autocor = TRUE)
  expect_identical(dim(got), c(10L, 3L))
  expect_equal(unname(got[, 1L]), unname(got[, 2L]))
  expect_equal(unname(got[, 1L]), unname(full[, j]))
})


test_that("a newdata holding one occasion reads that occasion", {
  # The complement of the one-series cut: hold the series and cut the
  # time axis. An AR trend indexes `trend[t, s]` by both, so a cut
  # that renumbers the occasions from 1 reads the wrong rows.
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  d <- as.data.frame(fit$data)
  vars <- fit$trend_metadata$variables
  full <- posterior_epred(fit, newdata = d, draw_ids = 1:10,
                          incl_autocor = TRUE)
  times <- sort(unique(d[[vars$time_var]]))
  for (tv in times[c(1L, 2L, length(times))]) {
    rows <- which(d[[vars$time_var]] == tv)
    expect_gt(length(rows), 0L)
    got <- posterior_epred(fit, newdata = d[rows, , drop = FALSE],
                           draw_ids = 1:10, incl_autocor = TRUE)
    expect_equal(unname(got), unname(full[, rows, drop = FALSE]))
  }
})
