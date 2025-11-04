# Local tests for multiple imputation mvgam models
# Tests mvgam_multiple() with combine=TRUE/FALSE

source("setup_tests_local.R")

# ==============================================================================
# Setup test data (ONCE)
# ==============================================================================

setup_mi_test_data <- function() {
  set.seed(42)
  n_time <- 24

  # Univariate dataset (matches test-models-single.R pattern)
  base_data <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rnorm(n_time, mean = 3, sd = 1),
    season = 1:n_time
  )

  # Create 3 pseudo-imputed datasets by adding small noise
  imputed_list <- lapply(1:3, function(i) {
    data_copy <- base_data
    data_copy$y <- data_copy$y + rnorm(nrow(data_copy), 0, 0.1)
    data_copy
  })

  list(
    base = base_data,
    imputed = imputed_list
  )
}

# Get test data once
mi_data <- setup_mi_test_data()

# ==============================================================================
# FIT MODELS ONCE (reused across multiple tests)
# ==============================================================================

# Fit with combine=TRUE (pooled posteriors)
fit_pooled <- SW(SM(mvgam_multiple(
  formula = y ~ s(season, bs = "cc", k = 5),
  trend_formula = ~ 1,
  data_list = mi_data$imputed,
  family = gaussian(),
  backend = "rstan",
  combine = TRUE,
  chains = 2,
  iter = 500,
  silent = 2
)))

# Fit with combine=FALSE (list of fits)
fit_list <- SW(SM(mvgam_multiple(
  formula = y ~ s(season, bs = "cc", k = 5),
  trend_formula = ~ 1,
  data_list = mi_data$imputed,
  family = gaussian(),
  backend = "rstan",
  combine = FALSE,
  chains = 2,
  iter = 500,
  silent = 2
)))

# ==============================================================================
# Test 1: Pooled fit structure and class
# ==============================================================================

test_that("mvgam_multiple with combine=TRUE has correct structure", {
  expect_s3_class(fit_pooled, "mvgam_pooled")
  expect_s3_class(fit_pooled, "mvgam")
  expect_s3_class(fit_pooled, "brmsfit")

  expect_true(!is.null(attr(fit_pooled, "individual_fits")))
  expect_true(!is.null(attr(fit_pooled, "n_imputations")))
  expect_true(!is.null(attr(fit_pooled, "combination_method")))

  expect_equal(attr(fit_pooled, "n_imputations"), 3)
  expect_equal(attr(fit_pooled, "combination_method"), "sflist2stanfit")
  expect_length(attr(fit_pooled, "individual_fits"), 3)

  individual_fits <- attr(fit_pooled, "individual_fits")
  expect_true(all(sapply(individual_fits, inherits, "mvgam")))
})

# ==============================================================================
# Test 2: Methods work on pooled fit
# ==============================================================================

test_that("variables(), summary(), print() work on pooled fit", {
  all_vars <- variables(fit_pooled)
  expect_type(all_vars, "character")
  expect_true(length(all_vars) > 0)
  expect_true(any(grepl("_trend", all_vars)))

  summ <- summary(fit_pooled)
  expect_s3_class(summ, "mvgam_pooled_summary")
  expect_s3_class(summ, "mvgam_summary")
  expect_true(!is.null(summ$mi_diagnostics))

  expect_output(print(fit_pooled), "Family:")
  expect_output(print(summ), "Multiple Imputation Diagnostics")
})

# ==============================================================================
# Test 3: MI diagnostics structure
# ==============================================================================

test_that("MI diagnostics have correct structure", {
  summ <- summary(fit_pooled)
  mi_diag <- summ$mi_diagnostics

  expect_equal(mi_diag$n_imputations, 3)
  expect_type(mi_diag$total_draws, "integer")
  expect_type(mi_diag$draws_per_imputation, "integer")
  expect_equal(mi_diag$combination_method, "sflist2stanfit")

  expect_length(mi_diag$per_imputation_convergence, 3)
  expect_type(mi_diag$max_rhat_across_imputations, "double")
  expect_type(mi_diag$min_ess_across_imputations, "double")

  conv <- mi_diag$per_imputation_convergence[[1]]
  expect_equal(conv$imputation, 1)
  expect_type(conv$max_rhat, "double")
  expect_type(conv$min_bulk_ess, "double")
  expect_type(conv$min_tail_ess, "double")
  expect_type(conv$n_params, "integer")
})

# ==============================================================================
# Test 4: Draws arithmetic validation
# ==============================================================================

test_that("Combined draws equal sum of individual draws", {
  individual_fits <- attr(fit_pooled, "individual_fits")
  total_draws_pooled <- posterior::ndraws(
    posterior::as_draws(fit_pooled$fit)
  )
  individual_draws <- sapply(individual_fits, function(f) {
    posterior::ndraws(posterior::as_draws(f$fit))
  })
  expected_total <- sum(individual_draws)

  expect_equal(total_draws_pooled, expected_total)
})

# ==============================================================================
# Test 5: combine=FALSE returns list
# ==============================================================================

test_that("mvgam_multiple with combine=FALSE returns list", {
  expect_type(fit_list, "list")
  expect_length(fit_list, 3)
  expect_true(all(sapply(fit_list, inherits, "mvgam")))

  for (i in seq_along(fit_list)) {
    expect_s3_class(fit_list[[i]], "mvgam")
    expect_s3_class(fit_list[[i]], "brmsfit")
    expect_false(inherits(fit_list[[i]], "mvgam_pooled"))
  }

  vars1 <- variables(fit_list[[1]])
  expect_type(vars1, "character")
  expect_true(length(vars1) > 0)

  summ1 <- summary(fit_list[[1]])
  expect_s3_class(summ1, "mvgam_summary")
  expect_false(inherits(summ1, "mvgam_pooled_summary"))
})

# ==============================================================================
# Test 6: Documentation @examples code runs without errors
# ==============================================================================

test_that("mvgam_multiple() @examples code runs correctly", {
  # Create pseudo-imputed data (3 imputations) - exact code from @examples
  base_data <- data.frame(
    time = 1:24,
    series = factor(rep("series1", 24)),
    y = rnorm(24, mean = 3, sd = 1),
    season = 1:24
  )

  imputed_list <- lapply(1:3, function(i) {
    data_copy <- base_data
    data_copy$y <- data_copy$y + rnorm(nrow(data_copy), 0, 0.1)
    data_copy
  })

  # Fit with combined posteriors (recommended) - from @examples
  fit_pooled_doc <- SW(SM(mvgam_multiple(
    formula = y ~ s(season, bs = "cc", k = 5),
    trend_formula = ~ 1,
    data_list = imputed_list,
    family = gaussian(),
    backend = "rstan",
    combine = TRUE,
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check MI diagnostics - from @examples
  summ <- summary(fit_pooled_doc)
  expect_s3_class(summ, "mvgam_pooled_summary")
  expect_true(!is.null(summ$mi_diagnostics))

  # Check print works - from @examples
  expect_output(print(fit_pooled_doc), "Family:")

  # Fit separately (for advanced use cases) - from @examples
  fit_list_doc <- SW(SM(mvgam_multiple(
    formula = y ~ s(season, bs = "cc", k = 5),
    trend_formula = ~ 1,
    data_list = imputed_list,
    family = gaussian(),
    backend = "rstan",
    combine = FALSE,
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Verify list structure - implied by @examples
  expect_type(fit_list_doc, "list")
  expect_length(fit_list_doc, 3)
  expect_true(all(sapply(fit_list_doc, inherits, "mvgam")))
})
