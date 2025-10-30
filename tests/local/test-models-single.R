# Local tests for single dataset mvgam models
# Tests all major trend types and model patterns from target_generation.R

source("setup_tests_local.R")

# Setup test data function
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )

  # Multivariate dataset with balanced design
  time_presence <- rbinom(n_time, size = 1, prob = 0.7)
  time_x <- rnorm(n_time)
  time_habitat <- factor(sample(c("forest", "grassland"), n_time, replace = TRUE))

  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rep(time_presence, n_series),
    x = rep(time_x, n_series),
    habitat = rep(time_habitat, n_series)
  )

  list(
    univariate = univariate,
    multivariate = multivariate
  )
}

# Get test data once for all tests
test_data <- setup_stan_test_data()

# ==============================================================================
# FIT MODELS ONCE (reused across multiple tests)
# ==============================================================================

# Fit Target 1: Basic RW model
fit1 <- SW(SM(mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

# Fit Target 2: Multivariate shared RW model
fit2 <- SW(SM(mvgam(
  bf(mvbind(count, biomass) ~ x) + set_rescor(FALSE),
  trend_formula = ~ RW(cor = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)))

# Fit Target 3: VARMA with smooths
fit3 <- SW(SM(mvgam(
  bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
  trend_formula = ~ presence + VAR(p = 2, ma = TRUE),
  data = test_data$multivariate,
  chains = 2,
  iter = 500,
  silent = 2
)))

# ==============================================================================
# TARGET 1: RW trends (basic structure)
# ==============================================================================
test_that("Target 1: mvgam fits basic RW model", {
  # Check object structure
  expect_s3_class(fit1, "mvgam")
  expect_s3_class(fit1, "brmsfit")
  expect_true(!is.null(fit1$fit))
  expect_true(!is.null(fit1$exclude))
  expect_true(!is.null(fit1$formula))
  expect_true(!is.null(fit1$trend_formula))
  expect_true(!is.null(fit1$family))
  expect_true(!is.null(fit1$data))

  # Check methods work
  expect_output(print(fit1))
  summ <- summary(fit1)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit1)
  expect_type(vars, "character")
  expect_true(length(vars) > 0)
})

# ==============================================================================
# TARGET 2: Shared RW trends (multivariate)
# ==============================================================================
test_that("Target 2: mvgam fits multivariate shared RW model", {
  # Check object structure
  expect_s3_class(fit2, "mvgam")
  expect_true(!is.null(fit2$response_names))
  expect_equal(length(fit2$response_names), 2)
  expect_equal(fit2$response_names, c("count", "biomass"))

  # Check methods work
  expect_output(print(fit2))
  summ <- summary(fit2)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit2)
  expect_true(any(grepl("_trend", vars)))
})

# ==============================================================================
# TARGET 3: VARMA trends (complex functions)
# ==============================================================================
test_that("Target 3: mvgam fits VARMA model with covariates", {
  # Check object structure
  expect_s3_class(fit3, "mvgam")
  expect_true(!is.null(fit3$trend_formula))

  # Check methods work
  expect_output(print(fit3))
  summ <- summary(fit3)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit3)
  expect_true(any(grepl("A.*_trend", vars)))
})

# ==============================================================================
# TARGET 4: Factor AR trends (Z matrix patterns)
# ==============================================================================
test_that("Target 4: mvgam fits factor AR model", {
  fit <- SW(SM(mvgam(
    formula = bf(count ~ x, family = poisson()) +
      bf(presence ~ x, family = bernoulli()) +
      bf(biomass ~ x, family = Gamma(link = log)),
    trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE),
    data = test_data$multivariate,
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_equal(length(fit$response_names), 3)

  # Check Z matrix parameters present
  vars <- variables(fit)
  expect_true(any(grepl("Z", vars)))

  # Check methods work
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mvgam_summary")
})

# ==============================================================================
# TARGET 5: PW trends (Prophet functions)
# ==============================================================================
test_that("Target 5: mvgam fits piecewise trend model", {
  fit <- SW(SM(mvgam(
    y ~ x,
    trend_formula = ~ PW(n_changepoints = 10),
    data = test_data$univariate,
    family = poisson(),
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_true(!is.null(fit$trend_formula))

  # Check methods work
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit)
  expect_type(vars, "character")
})

# ==============================================================================
# TARGET 6: CAR trends (GP + irregular time)
# ==============================================================================
test_that("Target 6: mvgam fits CAR model with GP", {
  fit <- SW(SM(mvgam(
    y ~ (1 | series),
    trend_formula = ~ gp(x) + CAR(),
    data = test_data$univariate,
    family = poisson(),
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_true(!is.null(fit$trend_formula))

  # Check methods work
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit)
  expect_true(any(grepl("_trend", vars)))
})

# ==============================================================================
# TARGET 7: CAR trends (monotonic + irregular time)
# ==============================================================================
test_that("Target 7: mvgam fits CAR model with monotonic effects", {
  # Add ordered factor to data
  income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
  test_data$univariate$income <- factor(
    sample(income_options, nrow(test_data$univariate), TRUE),
    levels = income_options,
    ordered = TRUE
  )

  fit <- SW(SM(mvgam(
    y ~ (1 | series),
    trend_formula = ~ mo(income) + CAR(),
    data = test_data$univariate,
    family = poisson(),
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_true(!is.null(fit$trend_formula))

  # Check methods work
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit)
  expect_type(vars, "character")
})

# ==============================================================================
# TARGET 8: Seasonal AR trends
# ==============================================================================
test_that("Target 8: mvgam fits seasonal AR model", {
  fit <- SW(SM(mvgam(
    y ~ gp(x, k = 5),
    trend_formula = ~ -1 + gp(temperature, k = 6) + AR(p = c(1, 12)),
    data = test_data$univariate,
    family = poisson(),
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_true(!is.null(fit$trend_formula))

  # Check methods work
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit)
  expect_true(any(grepl("ar.*_trend", vars)))
})

# ==============================================================================
# TARGET 9: Nonlinear with AR trends
# ==============================================================================
test_that("Target 9: mvgam fits nonlinear model with AR trends", {
  prior9 <- prior(normal(1, 2), nlpar = "b1") +
    prior(normal(0, 2), nlpar = "b2")

  fit <- SW(SM(mvgam(
    bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
    trend_formula = ~ AR(),
    data = test_data$univariate,
    family = poisson(),
    prior = prior9,
    chains = 2,
    iter = 500,
    silent = 2
  )))

  # Check object structure
  expect_s3_class(fit, "mvgam")
  expect_true(!is.null(fit$trend_formula))

  # Check methods work
  expect_output(print(fit))
  summ <- summary(fit)
  expect_s3_class(summ, "mvgam_summary")
  vars <- variables(fit)
  expect_type(vars, "character")
})

# ==============================================================================
# SUMMARY METHOD DIAGNOSTIC TESTS
# ==============================================================================
test_that("summary.mvgam includes MCMC diagnostics", {
  summ <- summary(fit1)

  # Check structure and class
  expect_s3_class(summ, "mvgam_summary")
  expect_true(!is.null(summ$fixed))
  expect_true(!is.null(summ$trend))

  # Check diagnostics present in fixed effects
  if (!is.null(summ$fixed) && nrow(summ$fixed) > 0) {
    expect_true("Rhat" %in% names(summ$fixed))
    expect_true("Bulk_ESS" %in% names(summ$fixed))
    expect_true("Tail_ESS" %in% names(summ$fixed))

    # Check parameters are in rownames, not a column
    expect_false("variable" %in% names(summ$fixed))
    expect_true(!is.null(rownames(summ$fixed)))
  }

  # Check diagnostics present in trend parameters
  if (!is.null(summ$trend) && nrow(summ$trend) > 0) {
    expect_true("Rhat" %in% names(summ$trend))
    expect_true("Bulk_ESS" %in% names(summ$trend))
    expect_true("Tail_ESS" %in% names(summ$trend))
  }

  # Check credible interval naming follows "l-95% CI" pattern
  fixed_names <- names(summ$fixed)
  expect_true(any(grepl("^l-.*% CI$", fixed_names)))
  expect_true(any(grepl("^u-.*% CI$", fixed_names)))

  # Capture printed output and verify diagnostics appear
  output <- capture.output(print(summ))
  expect_true(any(grepl("Rhat", output)))
  expect_true(any(grepl("ESS", output)))
})

test_that("summary.mvgam respects prob argument", {
  summ_95 <- summary(fit1, probs = c(0.025, 0.975))
  expect_s3_class(summ_95, "mvgam_summary")

  if (!is.null(summ_95$fixed) && nrow(summ_95$fixed) > 0) {
    col_names_95 <- names(summ_95$fixed)
    expect_true("l-95% CI" %in% col_names_95)
    expect_true("u-95% CI" %in% col_names_95)
  }

  summ_90 <- summary(fit1, probs = c(0.05, 0.95))
  expect_s3_class(summ_90, "mvgam_summary")

  if (!is.null(summ_90$fixed) && nrow(summ_90$fixed) > 0) {
    col_names_90 <- names(summ_90$fixed)
    expect_true("l-90% CI" %in% col_names_90)
    expect_true("u-90% CI" %in% col_names_90)

    param_name <- rownames(summ_90$fixed)[1]
    width_90 <- summ_90$fixed[param_name, "u-90% CI"] -
                summ_90$fixed[param_name, "l-90% CI"]
    width_95 <- summ_95$fixed[param_name, "u-95% CI"] -
                summ_95$fixed[param_name, "l-95% CI"]
    expect_true(width_90 < width_95)
  }

  summ_99 <- summary(fit1, probs = c(0.005, 0.995))
  expect_s3_class(summ_99, "mvgam_summary")

  if (!is.null(summ_99$fixed) && nrow(summ_99$fixed) > 0) {
    col_names_99 <- names(summ_99$fixed)
    expect_true("l-99% CI" %in% col_names_99)
    expect_true("u-99% CI" %in% col_names_99)

    param_name <- rownames(summ_99$fixed)[1]
    width_99 <- summ_99$fixed[param_name, "u-99% CI"] -
                summ_99$fixed[param_name, "l-99% CI"]
    width_95 <- summ_95$fixed[param_name, "u-95% CI"] -
                summ_95$fixed[param_name, "l-95% CI"]
    expect_true(width_99 > width_95)
  }

  summ_custom <- summary(fit1, probs = c(0.1, 0.9))
  if (!is.null(summ_custom$fixed) && nrow(summ_custom$fixed) > 0) {
    col_names_custom <- names(summ_custom$fixed)
    expect_true("l-80% CI" %in% col_names_custom)
    expect_true("u-80% CI" %in% col_names_custom)
  }
})

test_that("summary.mvgam categorizes parameters correctly", {
  summ1 <- summary(fit1)
  summ2 <- summary(fit2)
  summ3 <- summary(fit3)

  # All should have fixed effects and trend parameters
  expect_true(!is.null(summ1$fixed))
  expect_true(!is.null(summ1$trend))
  expect_true(nrow(summ1$fixed) > 0)
  expect_true(nrow(summ1$trend) > 0)

  # Multivariate model should have family-specific parameters
  expect_true(!is.null(summ2$spec))
  expect_true(nrow(summ2$spec) > 0)
  spec_names <- rownames(summ2$spec)
  expect_true(any(grepl("^sigma", spec_names)))

  # Smooth model should categorize smooth terms separately
  if (!is.null(summ3$smooth) && nrow(summ3$smooth) > 0) {
    smooth_names <- rownames(summ3$smooth)
    expect_true(any(grepl("^s(ds)?_", smooth_names)))
  }

  # Fixed effects should not contain smooth or trend parameters
  if (!is.null(summ3$fixed)) {
    fixed_names <- rownames(summ3$fixed)
    expect_false(any(grepl("^s(ds)?_", fixed_names)))
    expect_false(any(grepl("_trend$", fixed_names)))
  }

  # Trend parameters should have _trend suffix
  trend_names <- rownames(summ1$trend)
  expect_true(any(grepl("_trend", trend_names)))
})

# ==============================================================================
# PRINT METHOD TESTS
# ==============================================================================

test_that("print.mvgam displays all required sections", {
  # Capture print output
  output <- capture.output(print(fit1))
  output_text <- paste(output, collapse = "\n")

  # Section 1: Formula (with trend)
  expect_match(output_text, "GAM observation formula:")
  expect_match(output_text, "GAM process formula:")

  # Section 2: Family and link
  expect_match(output_text, "Family:")
  expect_match(output_text, "Link function:")

  # Section 3: Trend model
  expect_match(output_text, "Trend model:")

  # Section 4: N series
  expect_match(output_text, "N series:")

  # Section 5: N timepoints
  expect_match(output_text, "N timepoints:")

  # Section 6: Sampling status
  expect_match(output_text, "Status:")
  expect_match(output_text, "chains")
})

test_that("print.mvgam returns object invisibly", {
  result <- withVisible(print(fit1))

  # Should return invisibly
  expect_false(result$visible)

  # Should return the original object unchanged
  expect_s3_class(result$value, "mvgam")
  expect_identical(result$value, fit1)
})

test_that("print.summary.mvgam displays mvgam-specific metadata", {
  summ <- summary(fit1)
  output <- capture.output(print(summ))
  output_text <- paste(output, collapse = "\n")

  # Metadata sections should appear
  expect_match(output_text, "GAM observation formula:")
  expect_match(output_text, "Family:")
  expect_match(output_text, "Trend model:")
  expect_match(output_text, "N series:")
  expect_match(output_text, "N timepoints:")
  expect_match(output_text, "Draws:")

  # Parameter tables should also appear
  expect_match(output_text, "Population-Level Effects")
})

test_that("print.mvgam displays correct values", {
  output <- capture.output(print(fit1))
  output_text <- paste(output, collapse = "\n")

  # Extract expected values from object
  sim_info <- extract_mcmc_info(fit1)

  # Check exact family and link
  expect_match(output_text, fit1$family$family)
  expect_match(output_text, fit1$family$link)

  # Check exact MCMC info with full pattern
  expect_match(output_text,
               paste0(sim_info$chains, " chains, each with iter = ",
                      sim_info$iter))
  expect_match(output_text,
               paste0("Total post-warmup draws = ", sim_info$total_draws))

  # Check dimensions appear in output
  if (!is.null(fit1$series_info) &&
      !is.null(fit1$series_info$n_series)) {
    expect_match(output_text, "N series:")
    expect_match(output_text, as.character(fit1$series_info$n_series))
  }

  if (!is.null(fit1$time_info) &&
      !is.null(fit1$time_info$n_timepoints)) {
    expect_match(output_text, "N timepoints:")
    expect_match(output_text,
                 as.character(fit1$time_info$n_timepoints))
  }

  # Check trend model type appears (if present)
  if (!is.null(fit1$trend_components) &&
      !is.null(fit1$trend_components$types) &&
      length(fit1$trend_components$types) > 0) {
    expect_match(output_text, fit1$trend_components$types[1])
  }
})

test_that("variables.mvgam returns all parameter names", {
  vars1 <- variables(fit1)

  # Return type is character vector
  expect_type(vars1, "character")
  expect_true(length(vars1) > 0)

  # No duplicates in parameter names
  expect_equal(length(vars1), length(unique(vars1)))

  # Key observation parameters present
  expect_true(any(grepl("^Intercept$|^b_Intercept$", vars1)))

  # Family-specific parameters present for gaussian
  expect_true(any(grepl("^sigma$", vars1)))
})

test_that("variables.mvgam includes trend parameters", {
  vars1 <- variables(fit1)

  # Parameters with "_trend" suffix present
  trend_params <- vars1[grepl("_trend", vars1)]
  expect_true(length(trend_params) > 0)

  # RW model has sigma_trend
  expect_true(any(grepl("sigma_trend", trend_params)))

  # Trend states present (trend[i,s])
  expect_true(any(grepl("^trend\\[", vars1)))
})

test_that("variables.mvgam works with multivariate models", {
  vars2 <- variables(fit2)

  # Return type is character vector
  expect_type(vars2, "character")
  expect_true(length(vars2) > 0)

  # Multivariate models have response-specific sigma
  expect_true(any(grepl("sigma_count|sigma_biomass", vars2)))

  # Response-specific parameters present
  expect_true(any(grepl("count|biomass", vars2)))
})
