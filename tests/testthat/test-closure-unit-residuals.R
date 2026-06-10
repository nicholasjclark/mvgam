# Tests for the per-unit residuals path on closure-unit families
# (nmix, occ). The aggregator is family-agnostic (sum over visits
# within a unit), so structural tests live in one place rather than
# duplicated across test-nmix-family.R and test-occ-family.R. Heavier
# end-to-end residual coverage on actual MCMC fits lives in
# tests/local/.

# ------------------------------------------------------------
# aggregate_closure_unit_visits(): pure data helper
# ------------------------------------------------------------

make_closure_unit_grid <- function(n_unit = 3L, n_visit = 4L,
                                   binary = TRUE, seed = 11L) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  y_pool  <- if (binary) c(0L, 1L) else 0L:5L
  d <- data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = sample(y_pool, n_total, replace = TRUE)
  )
  # Non-binary tests exercise the nmix-shape path that requires the
  # `cap` column on the input frame; binary tests rely on the occ
  # default_cap = 1L injection so `cap` stays absent.
  if (!binary) d$cap <- max(d$y) + 1L
  d
}

# Mock the smallest object surface aggregate_closure_unit_visits()
# actually touches. The helper reads `object$formula` (to find the
# response column), `object$data` (when newdata is NULL), and the
# `mvgam_binary_response` family attribute (drives the default cap
# injection on the per-call array build). No Stan fit required.
mock_closure_unit_object <- function(data, formula = y ~ 1,
                                      family = occ()) {
  structure(
    list(data = data, formula = formula, family = family),
    class = "mvgam"
  )
}

test_that("aggregate_closure_unit_visits() sums per-visit observations within each unit", {
  d <- make_closure_unit_grid(n_unit = 3, n_visit = 4, binary = TRUE)
  obj <- mock_closure_unit_object(d)
  yrep_visit <- matrix(d$y, nrow = 5L, ncol = nrow(d), byrow = TRUE)
  agg <- aggregate_closure_unit_visits(
    obj, newdata = d, yrep_visit = yrep_visit
  )
  expected_y <- tapply(d$y, d$series, sum)
  expect_equal(as.numeric(agg$y_unit), as.numeric(expected_y))
  # Every yrep row was a copy of y, so the unit-summed rows equal y_unit.
  expect_true(all(apply(agg$yrep_unit, 1L, identical, agg$y_unit)))
  expect_identical(ncol(agg$yrep_unit), 3L)
  expect_identical(colnames(agg$yrep_unit), agg$arrays$unit_labels)
})

test_that("aggregate_closure_unit_visits() routes single-visit units through the short-circuit branch", {
  d <- make_closure_unit_grid(n_unit = 4, n_visit = 1, binary = FALSE)
  obj <- mock_closure_unit_object(d, family = nmix())
  yrep_visit <- matrix(seq_len(nrow(d)), nrow = 3L,
                        ncol = nrow(d), byrow = TRUE)
  agg <- aggregate_closure_unit_visits(
    obj, newdata = d, yrep_visit = yrep_visit
  )
  expect_equal(as.numeric(agg$y_unit), as.numeric(d$y))
  # Single-visit per unit: the per-draw aggregated matrix is the
  # per-visit matrix verbatim, modulo the unit ordering.
  expect_equal(unname(agg$yrep_unit),
               yrep_visit[, agg$arrays$visit_idx[, 1L]],
               ignore_attr = TRUE)
})

test_that("aggregate_closure_unit_visits() errors when yrep_visit width does not match newdata", {
  d <- make_closure_unit_grid()
  obj <- mock_closure_unit_object(d)
  bad <- matrix(0, nrow = 2L, ncol = nrow(d) - 1L)
  expect_error(
    aggregate_closure_unit_visits(obj, newdata = d, yrep_visit = bad),
    "does not match 'newdata'"
  )
})

test_that("aggregate_closure_unit_visits() carries unit labels through colnames", {
  d <- make_closure_unit_grid(n_unit = 2, n_visit = 3)
  d$series <- factor(c(rep("A", 3), rep("B", 3)))
  obj <- mock_closure_unit_object(d)
  yrep_visit <- matrix(0, nrow = 4L, ncol = nrow(d))
  agg <- aggregate_closure_unit_visits(
    obj, newdata = d, yrep_visit = yrep_visit
  )
  expect_identical(length(agg$arrays$unit_labels), 2L)
  expect_identical(colnames(agg$yrep_unit), agg$arrays$unit_labels)
  expect_identical(names(agg$y_unit), agg$arrays$unit_labels)
})

# ------------------------------------------------------------
# residuals.mvgam() / augment.mvgam() routing for closure-unit
# families. The MCMC integration check lives in tests/local/.
# ------------------------------------------------------------

# augment.mvgam() recycles the per-unit residual rows back to the
# per-visit obs frame and adds a `.unit` column making the grain
# explicit. The MCMC integration cover for this path lives in
# tests/local/test-closure-unit-residuals.R.

test_that("compute_closure_unit_residuals() returns [ndraws x N_unit] for both type arms", {
  d <- make_closure_unit_grid(n_unit = 4, n_visit = 3, binary = TRUE)
  obj <- structure(
    list(data = d, formula = y ~ 1, family = occ()),
    class = "mvgam"
  )
  # Stub posterior_predict on the namespace so the helper resolves it
  # without needing a Stan fit; mirror the integer shape the real
  # closure-unit posterior_predict returns.
  ndraws <- 6L
  fake_yrep <- matrix(
    sample(c(0L, 1L), ndraws * nrow(d), replace = TRUE),
    nrow = ndraws, ncol = nrow(d)
  )
  with_mocked_bindings(
    posterior_predict = function(object, ...) fake_yrep,
    .package = "mvgam",
    {
      r_quant <- compute_closure_unit_residuals(
        obj, newdata = NULL, type = "quantile",
        draw_ids = NULL, ndraws = NULL
      )
      r_ord <- compute_closure_unit_residuals(
        obj, newdata = NULL, type = "ordinary",
        draw_ids = NULL, ndraws = NULL
      )
    }
  )
  arrs <- build_closure_unit_arrays(d, response_var = "y",
                                      default_cap = 1L)
  expect_identical(dim(r_quant), c(ndraws, arrs$N_unit))
  expect_identical(dim(r_ord), c(ndraws, arrs$N_unit))
  # Quantile residuals are N(0, 1) under correctly specified model;
  # the stub draws from the prior so the residual range stays inside
  # the empirical-PIT clamp |8.13|.
  expect_true(all(is.finite(r_quant)))
  expect_true(all(abs(r_quant) < 9))
  # Ordinary residuals = y_unit - yrep_unit, so each draw row equals
  # the difference between the observed and that draw's aggregated yrep.
  arrs <- build_closure_unit_arrays(d, response_var = "y",
                                      default_cap = 1L)
  y_unit <- vapply(seq_len(arrs$N_unit), function(g) {
    sum(d$y[arrs$visit_idx[g, seq_len(arrs$n_rep[g])]])
  }, numeric(1L))
  yrep_unit <- t(vapply(seq_len(ndraws), function(s) {
    vapply(seq_len(arrs$N_unit), function(g) {
      sum(fake_yrep[s, arrs$visit_idx[g, seq_len(arrs$n_rep[g])]])
    }, numeric(1L))
  }, numeric(arrs$N_unit)))
  expected_ord <- sweep(yrep_unit, 2L, y_unit,
                          function(yh, yi) yi - yh)
  r_ord_unnamed <- unname(r_ord)
  dimnames(r_ord_unnamed) <- NULL
  expect_equal(r_ord_unnamed, expected_ord)
})
