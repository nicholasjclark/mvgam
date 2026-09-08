# Contract tests for the kfold.mvgam helpers. These exercise pure
# linear algebra and partition logic, so no Stan fit is required;
# end-to-end refit verification lives in tests/local/.

test_that("resolve_kfold_group() defaults to row-level keys for non-closure families", {
  obj <- list(family = stats::gaussian())
  data <- data.frame(x = 1:5, y = rnorm(5))
  out <- mvgam:::resolve_kfold_group(obj, group = NULL, data = data)
  expect_equal(out$key, as.character(1:5))
  expect_equal(out$n_groups, 5L)
  expect_null(out$group_names)
})

test_that("resolve_kfold_group() builds composite keys via paste", {
  obj <- list(family = stats::gaussian())
  data <- data.frame(
    site = c("a", "a", "b", "b", "c"),
    season = c(1L, 2L, 1L, 2L, 1L),
    y = rnorm(5)
  )
  out <- mvgam:::resolve_kfold_group(
    obj, group = c("site", "season"), data = data
  )
  expect_equal(out$key,
               c("a_1", "a_2", "b_1", "b_2", "c_1"))
  expect_equal(out$n_groups, 5L)
  expect_equal(out$group_names, c("site", "season"))
})

test_that("resolve_kfold_group() errors on missing columns", {
  obj <- list(family = stats::gaussian())
  data <- data.frame(site = letters[1:3], y = 1:3)
  expect_error(
    mvgam:::resolve_kfold_group(obj, group = "missing", data = data),
    "Some 'group' columns are missing"
  )
})

test_that("build_kfold_partition() respects group integrity", {
  set.seed(7L)
  group_key <- rep(c("a", "b", "c", "d"), each = 3L)
  folds <- mvgam:::build_kfold_partition(
    group_key, K = 2L, fold_split = "grouped"
  )
  # Each group must land in exactly one fold.
  per_group <- tapply(folds, group_key, function(f) length(unique(f)))
  expect_true(all(per_group == 1L))
  expect_setequal(unique(folds), c(1L, 2L))
  # loo::kfold_split_* returns numeric; storage mode must be
  # coerced to integer so downstream `vapply(..., integer(1L))`
  # in build_group_to_fold() succeeds.
  expect_identical(typeof(folds), "integer")
})

test_that("build_kfold_partition() honours explicit folds vector", {
  group_key <- letters[1:6]
  folds_in <- c(1L, 1L, 2L, 2L, 3L, 3L)
  folds_out <- mvgam:::build_kfold_partition(
    group_key, folds = folds_in
  )
  expect_identical(folds_out, folds_in)
})

test_that("build_kfold_partition() errors when K > n_groups", {
  group_key <- rep(c("a", "b"), each = 3L)
  expect_error(
    mvgam:::build_kfold_partition(group_key, K = 5L),
    "exceeds the number of distinct groups"
  )
})

test_that("build_kfold_partition() defaults K = n_groups when group set", {
  group_key <- letters[1:5]
  folds <- mvgam:::build_kfold_partition(group_key, K = NULL)
  expect_length(unique(folds), 5L)
})

test_that("aggregate_loglik_by_group() sums log-densities within groups", {
  set.seed(1L)
  loglik <- matrix(rnorm(8L), nrow = 2L, ncol = 4L)
  col_group <- c("g1", "g1", "g2", "g2")
  agg <- mvgam:::aggregate_loglik_by_group(loglik, col_group)
  expect_equal(dim(agg), c(2L, 2L))
  expect_equal(colnames(agg), c("g1", "g2"))
  expect_equal(as.numeric(agg[, "g1"]),
               loglik[, 1L] + loglik[, 2L])
  expect_equal(as.numeric(agg[, "g2"]),
               loglik[, 3L] + loglik[, 4L])
})

test_that("aggregate_loglik_by_group() handles single-member groups", {
  loglik <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2L, ncol = 2L)
  agg <- mvgam:::aggregate_loglik_by_group(loglik, c("only", "another"))
  expect_equal(dim(agg), c(2L, 2L))
  expect_equal(as.numeric(agg[, "only"]),
               as.numeric(loglik[, 1L]))
})

test_that("build_mvgam_kfold() returns correctly-classed loo object", {
  pointwise <- c(g1 = -1.2, g2 = -0.8, g3 = -2.4)
  pareto_k <- c(g1 = 0.3, g2 = NA, g3 = 0.5)
  out <- mvgam:::build_mvgam_kfold(
    pointwise = pointwise, pareto_k = pareto_k,
    refit_groups = "g2", n_refit_folds = 1L, K = 3L, group = "site",
    pareto_k_threshold = 0.7, exact = FALSE
  )
  expect_s3_class(out, c("mvgam_kfold", "kfold", "loo"),
                  exact = TRUE)
  expect_equal(out$K, 3L)
  expect_equal(out$n_refits, 1L)
  expect_equal(out$elpd_kfold, sum(pointwise))
  expect_equal(rownames(out$estimates),
               c("elpd_kfold", "kfoldic"))
  expect_equal(colnames(out$estimates), c("Estimate", "SE"))
  expect_true(out$elpd_kfold < 0)
})

test_that("print.mvgam_kfold() emits headline elpd + refit info", {
  pointwise <- c(g1 = -1.2, g2 = -0.8)
  pareto_k <- c(g1 = 0.3, g2 = NA)
  out <- mvgam:::build_mvgam_kfold(
    pointwise = pointwise, pareto_k = pareto_k,
    refit_groups = "g2", n_refit_folds = 1L, K = 2L, group = "site",
    pareto_k_threshold = 0.7, exact = FALSE
  )
  printed <- utils::capture.output(print(out))
  expect_true(any(grepl("mvgam k-fold", printed)))
  expect_true(any(grepl("elpd_kfold", printed)))
  expect_true(any(grepl("refits     = 1 of 2", printed)))
})

test_that("summary.mvgam_kfold() returns per-group tibble with diagnostics", {
  pointwise <- c(g1 = -1.2, g2 = -0.5, g3 = -2.4)
  pointwise_psis <- c(g1 = -1.2, g2 = -2.0, g3 = -2.4)
  pareto_k <- c(g1 = 0.3, g2 = 0.85, g3 = 0.5)
  out <- mvgam:::build_mvgam_kfold(
    pointwise = pointwise, pointwise_psis = pointwise_psis,
    pareto_k = pareto_k, refit_groups = "g2", n_refit_folds = 1L,
    K = 3L, group = "site",
    pareto_k_threshold = 0.7, exact = FALSE
  )
  s <- summary(out)
  expect_equal(NROW(s), 3L)
  expect_named(s, c("group", "elpd", "elpd_share", "elpd_psis",
                    "lift", "pareto_k", "refit"))
  expect_equal(s$refit, c(FALSE, TRUE, FALSE))
  # lift defined only for refit folds.
  expect_equal(s$lift,
               c(NA_real_, -0.5 - -2.0, NA_real_))
  expect_equal(sum(s$elpd_share), 1, tolerance = 1e-9)
})

test_that("plot.mvgam_kfold() returns a ggplot with two facets", {
  pointwise <- c(g1 = -1.2, g2 = -0.8, g3 = -2.4)
  pareto_k <- c(g1 = 0.3, g2 = NA, g3 = 0.5)
  out <- mvgam:::build_mvgam_kfold(
    pointwise = pointwise, pareto_k = pareto_k,
    refit_groups = "g2", n_refit_folds = 1L, K = 3L, group = "site",
    pareto_k_threshold = 0.7, exact = FALSE
  )
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  # Two facets: ELPD + Pareto k. ggplot2 layout returns one row
  # per facet * panel; here facet_wrap with two levels yields two.
  built <- ggplot2::ggplot_build(p)
  expect_equal(NROW(built$layout$layout), 2L)
})
