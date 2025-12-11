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
# FIT MODELS
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

  # Test prediction system
  newdata <- data.frame(
    x = c(0, 1),
    time = c(25, 26),
    series = factor(c("series1", "series1"))
  )

  obs_pred <- extract_component_linpred(fit1, newdata, component = "obs")
  expect_true(is.matrix(obs_pred))
  expect_equal(ncol(obs_pred), 2)
  expect_true(all(is.finite(obs_pred)))

  trend_pred <- extract_component_linpred(fit1, newdata, component = "trend")
  expect_true(is.matrix(trend_pred))
  expect_equal(ncol(trend_pred), 2)
  expect_true(all(is.finite(trend_pred)))
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

  # Test multivariate prediction system
  mv_newdata <- expand.grid(
    x = c(0, 1),
    time = c(25, 26)
  )

  # Test all responses together
  obs_pred_all <- extract_component_linpred(fit2, mv_newdata, component = "obs")
  expect_type(obs_pred_all, "list")
  expect_equal(names(obs_pred_all), c("count", "biomass"))
  expect_true(all(sapply(obs_pred_all, is.matrix)))

  # Test response-specific predictions
  obs_pred_count <- extract_component_linpred(fit2, mv_newdata,
                                             component = "obs", resp = "count")
  expect_true(is.matrix(obs_pred_count))
  expect_equal(ncol(obs_pred_count), 4)
  expect_true(all(is.finite(obs_pred_count)))

  trend_pred_all <- extract_component_linpred(fit2, mv_newdata, component = "trend")
  expect_true(is.matrix(trend_pred_all))
  expect_equal(ncol(trend_pred_all), 4)
  expect_true(all(is.finite(trend_pred_all)))
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

  # Test prediction with smooth and VARMA model
  varma_newdata <- data.frame(
    x = seq(0, 1, length.out = 3),
    presence = c(0, 1, 0),
    time = c(73, 74, 75)
  )

  # Test observation predictions with smooth
  obs_pred_varma <- extract_component_linpred(fit3, varma_newdata, component = "obs")
  expect_type(obs_pred_varma, "list")
  expect_equal(names(obs_pred_varma), c("count", "biomass"))
  expect_true(all(sapply(obs_pred_varma, function(x) ncol(x) == 3)))

  # Test trend predictions with covariates
  trend_pred_varma <- extract_component_linpred(fit3, varma_newdata, component = "trend")
  expect_type(trend_pred_varma, "double")
  expect_true(all(sapply(trend_pred_varma, function(x) all(is.finite(x)))))
  expect_true(all(trend_pred_varma[,1] == 0))
  expect_true(all(trend_pred_varma[,3] == 0))
  expect_false(all(trend_pred_varma[,2] == 0))
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

  # Test factor model predictions
  factor_newdata <- data.frame(
    x = c(0.5, -0.5),
    time = c(73, 74)
  )

  # Test predictions for factor loading model
  obs_pred_factor <- extract_component_linpred(fit, factor_newdata, component = "obs")
  expect_type(obs_pred_factor, "list")
  expect_equal(names(obs_pred_factor), c("count", "presence", "biomass"))
  expect_true(all(sapply(obs_pred_factor, function(x) ncol(x) == 2)))

  trend_pred_factor <- extract_component_linpred(fit, factor_newdata, component = "trend")
  expect_type(trend_pred_factor, "double")
  expect_true(all(sapply(trend_pred_factor, function(x) all(x == 0))))
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

  # Test piecewise trend predictions
  pw_newdata <- data.frame(
    x = c(-1, 0, 1),
    time = c(25, 26, 27),
    series = factor(rep("series1", 3))
  )

  # Test piecewise model predictions
  obs_pred_pw <- extract_component_linpred(fit, pw_newdata, component = "obs")
  expect_true(is.matrix(obs_pred_pw))
  expect_equal(ncol(obs_pred_pw), 3)
  expect_true(all(is.finite(obs_pred_pw)))

  trend_pred_pw <- extract_component_linpred(fit, pw_newdata, component = "trend")
  expect_true(is.matrix(trend_pred_pw))
  expect_true(all(is.finite(trend_pred_pw)))
})

# ==============================================================================
# TARGET 6: CAR trends (GP + irregular time)
# ==============================================================================
test_that("Target 6: mvgam fits CAR model with GP", {
  fit <- SW(SM(mvgam(
    y ~ (1 | series),
    trend_formula = ~ gp(x, k = 5) + CAR(),
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

  # Test CAR + GP predictions
  car_newdata <- data.frame(
    x = c(0, 0.5, 1),
    time = c(25, 26, 27),
    series = factor(rep("series1", 3))
  )

  # Test predictions with GP in trend formula
  obs_pred_car <- extract_component_linpred(fit, car_newdata, component = "obs")
  expect_true(is.matrix(obs_pred_car))
  expect_equal(ncol(obs_pred_car), 3)
  expect_true(all(is.finite(obs_pred_car)))

  trend_pred_car <- extract_component_linpred(fit, car_newdata, component = "trend")
  expect_true(is.matrix(trend_pred_car))
  expect_equal(ncol(trend_pred_car), 3)
  expect_true(all(is.finite(trend_pred_car)))
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

  # Test seasonal AR predictions with dual GPs
  seasonal_newdata <- data.frame(
    x = c(0, 0.5),
    temperature = c(15, 18),
    time = c(25, 26),
    series = factor(rep("series1", 2))
  )

  # Test predictions with GP in both observation and trend formula
  obs_pred_seasonal <- extract_component_linpred(fit, seasonal_newdata, component = "obs")
  expect_true(is.matrix(obs_pred_seasonal))
  expect_equal(ncol(obs_pred_seasonal), 2)
  expect_true(all(is.finite(obs_pred_seasonal)))

  trend_pred_seasonal <- extract_component_linpred(fit, seasonal_newdata, component = "trend")
  expect_true(is.matrix(trend_pred_seasonal))
  expect_true(all(is.finite(trend_pred_seasonal)))
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

  # Test nonlinear model predictions
  nl_newdata <- data.frame(
    x = c(-0.5, 0, 0.5),
    time = c(25, 26, 27),
    series = factor(rep("series1", 3))
  )

  # Test predictions with nonlinear formulas
  obs_pred_nl <- extract_component_linpred(fit, nl_newdata, component = "obs")
  expect_true(is.matrix(obs_pred_nl))
  expect_equal(ncol(obs_pred_nl), 3)
  expect_true(all(is.finite(obs_pred_nl)))

  trend_pred_nl <- extract_component_linpred(fit, nl_newdata, component = "trend")
  expect_true(is.matrix(trend_pred_nl))
  expect_true(all(is.finite(trend_pred_nl)))
})

# ==============================================================================
# SUMMARY METHOD DIAGNOSTIC TESTS
# ==============================================================================
test_that("summary.mvgam includes MCMC diagnostics", {
  summ <- summary(fit1)

  # Check structure and class
  expect_s3_class(summ, "mvgam_summary")
  expect_true(!is.null(summ$fixed))
  # Trend parameters are subdivided following brms design into trend_spec,

  # trend_fixed, trend_smooth, trend_random. RW model has trend_spec only.
  expect_true(!is.null(summ$trend_spec))

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
  if (!is.null(summ$trend_spec) && nrow(summ$trend_spec) > 0) {
    expect_true("Rhat" %in% names(summ$trend_spec))
    expect_true("Bulk_ESS" %in% names(summ$trend_spec))
    expect_true("Tail_ESS" %in% names(summ$trend_spec))
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
  # Trend params are in $trend_spec (sigma_trend, ar1_trend, etc.)
  expect_true(!is.null(summ1$fixed))
  expect_true(!is.null(summ1$trend_spec))
  expect_true(nrow(summ1$fixed) > 0)
  expect_true(nrow(summ1$trend_spec) > 0)

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

  # Trend-specific params (sigma_trend, ar1_trend) retain _trend suffix
  trend_names <- rownames(summ1$trend_spec)
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
  expect_match(output_text, "Formula:")
  expect_match(output_text, "Family:")
  expect_match(output_text, "Trends:")
  expect_match(output_text, "Number of observations:")
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

test_that("variables.mvgam returns correct structure and delegates properly", {
  vars1 <- variables(fit1)

  # Correct structure: unnamed character vector
  expect_type(vars1, "character")
  expect_true(is.vector(vars1))
  expect_false(is.list(vars1))
  expect_null(names(vars1))
  expect_true(length(vars1) > 0)

  # No duplicates in parameter names
  expect_equal(length(vars1), length(unique(vars1)))

  # Definitive test: should equal underlying fit minus exclusions
  raw_vars <- posterior::variables(posterior::as_draws(fit1$fit))
  expected_vars <- setdiff(raw_vars, fit1$exclude)
  expect_setequal(vars1, expected_vars)
})

test_that("variables.mvgam correctly excludes diagnostic parameters", {
  vars1 <- variables(fit1)

  # Excluded parameters should not appear
  expect_false("lprior" %in% vars1)
  expect_false("lp__" %in% vars1)

  # Verify exclusion list is actually used
  if (!is.null(fit1$exclude) && length(fit1$exclude) > 0) {
    expect_true(all(!fit1$exclude %in% vars1))
  }
})

test_that("variables.mvgam includes required observation parameters", {
  vars1 <- variables(fit1)

  # beta 1
  expect_true("b[1]" %in% vars1)

  # Intercept parameter (brms names it "Intercept" not "b_Intercept")
  expect_true("Intercept" %in% vars1)
})

test_that("variables.mvgam includes required trend parameters", {
  vars1 <- variables(fit1)

  # Parameters with "_trend" suffix present
  trend_params <- vars1[grepl("_trend", vars1)]
  expect_true(length(trend_params) > 0)

  # RW model must have sigma_trend
  expect_true("sigma_trend[1]" %in% vars1)

  # Trend states present (trend[i,s])
  trend_states <- vars1[grepl("^trend\\[", vars1)]
  expect_true(length(trend_states) > 0)

  # Latent variables for trend (lv_trend[i,k])
  lv_trend <- vars1[grepl("^lv_trend\\[", vars1)]
  expect_true(length(lv_trend) > 0)
})

test_that("variables.mvgam works with multivariate models", {
  vars2 <- variables(fit2)

  # Correct structure
  expect_type(vars2, "character")
  expect_true(is.vector(vars2))
  expect_null(names(vars2))

  # Definitive test
  raw_vars <- posterior::variables(posterior::as_draws(fit2$fit))
  expected_vars <- setdiff(raw_vars, fit2$exclude)
  expect_setequal(vars2, expected_vars)

  # Multivariate models have response-specific sigma (not sigma_y1)
  expect_true("sigma_count" %in% vars2)
  expect_true("sigma_biomass" %in% vars2)

  # Response-specific intercepts
  expect_true("Intercept_count" %in% vars2 || "b_count_Intercept" %in% vars2)
  expect_true("Intercept_biomass" %in% vars2 || "b_biomass_Intercept" %in% vars2)
})

test_that("variables.mvgam works with smooth parameters", {
  vars3 <- variables(fit3)

  # Should have smooth coefficients s_*
  smooth_params <- vars3[grepl("^s_", vars3)]
  expect_true(length(smooth_params) > 0)

  # Should have smooth SDs sds_*
  smooth_sds <- vars3[grepl("^sds_", vars3)]
  expect_true(length(smooth_sds) > 0)
})

# ==============================================================================
# POSTERIOR_LINPRED TESTS
# ==============================================================================
test_that("posterior_linpred returns matrix for univariate models", {
  linpred <- posterior_linpred(fit1, newdata = test_data$univariate)

  # Univariate models return a matrix
  expect_true(is.matrix(linpred))
  expect_false(is.list(linpred))

  # Dimensions: [ndraws x nobs]
  n_draws <- nrow(posterior::as_draws_matrix(fit1$fit))
  n_obs <- nrow(test_data$univariate)
  expect_equal(nrow(linpred), n_draws)
  expect_equal(ncol(linpred), n_obs)
})

test_that("posterior_linpred returns named list for multivariate models", {
  linpred <- posterior_linpred(fit2, newdata = test_data$multivariate)

  # Multivariate models return named list
  expect_true(is.list(linpred))
  expect_false(is.matrix(linpred))

  # Names match response variables from mvbind(count, biomass)
  expect_named(linpred, c("count", "biomass"))

  # Each element is a matrix
  expect_true(is.matrix(linpred$count))
  expect_true(is.matrix(linpred$biomass))

  # Dimensions: [ndraws x n_obs_per_response]
  n_draws <- nrow(posterior::as_draws_matrix(fit2$fit))
  expect_equal(nrow(linpred$count), n_draws)
  expect_equal(nrow(linpred$biomass), n_draws)
})

test_that("posterior_linpred ndraws subsetting works", {
  linpred_100 <- posterior_linpred(fit1, newdata = test_data$univariate,
                                   ndraws = 100)

  expect_true(is.matrix(linpred_100))
  expect_equal(nrow(linpred_100), 100)
})

test_that("posterior_linpred process_error toggle affects variance", {
  linpred_full <- posterior_linpred(fit1, newdata = test_data$univariate,
                                    process_error = TRUE)
  linpred_mean <- posterior_linpred(fit1, newdata = test_data$univariate,
                                    process_error = FALSE)

  # Same dimensions
  expect_equal(dim(linpred_full), dim(linpred_mean))

  # process_error=FALSE should reduce or equal variance (trend fixed at mean)
  var_full <- mean(apply(linpred_full, 2, var))
  var_mean <- mean(apply(linpred_mean, 2, var))
  expect_true(var_mean <= var_full * 1.01)
})

test_that("posterior_linpred resp argument filters multivariate response", {
  # Request single response
  linpred_count <- posterior_linpred(fit2, newdata = test_data$multivariate,
                                     resp = "count")

  # Returns matrix when resp specified
  expect_true(is.matrix(linpred_count))
  expect_false(is.list(linpred_count))

  n_draws <- nrow(posterior::as_draws_matrix(fit2$fit))
  expect_equal(nrow(linpred_count), n_draws)
})

# ==============================================================================
# POSTERIOR_EPRED TESTS
# ==============================================================================
test_that("posterior_epred returns matrix for univariate Poisson models", {
  epred <- posterior_epred(fit1, newdata = test_data$univariate)

  # Univariate models return a matrix
  expect_true(is.matrix(epred))
  expect_false(is.list(epred))

  # Dimensions: [ndraws x nobs]
  n_draws <- nrow(posterior::as_draws_matrix(fit1$fit))
  n_obs <- nrow(test_data$univariate)
  expect_equal(nrow(epred), n_draws)
  expect_equal(ncol(epred), n_obs)

  # Poisson epred must be non-negative (response scale)
  expect_true(all(epred >= 0))
})

test_that("posterior_epred equals exp(linpred) for Poisson models", {
  # For Poisson with log link: E[Y] = exp(eta)
  linpred <- posterior_linpred(fit1, newdata = test_data$univariate)
  epred <- posterior_epred(fit1, newdata = test_data$univariate)

  # Should be exact transformation

  expect_equal(epred, exp(linpred), tolerance = 1e-10)
})

test_that("posterior_epred returns named list for multivariate models", {
  epred <- posterior_epred(fit2, newdata = test_data$multivariate)

  # Multivariate models return named list
  expect_true(is.list(epred))
  expect_false(is.matrix(epred))

  # Names match response variables from mvbind(count, biomass)
  expect_named(epred, c("count", "biomass"))

  # Each element is a matrix
  expect_true(is.matrix(epred$count))
  expect_true(is.matrix(epred$biomass))

  # Dimensions: [ndraws x n_obs_per_response]
  n_draws <- nrow(posterior::as_draws_matrix(fit2$fit))
  expect_equal(nrow(epred$count), n_draws)
  expect_equal(nrow(epred$biomass), n_draws)

  # Response-scale constraints: count (Poisson) must be non-negative
  expect_true(all(epred$count >= 0))
})

test_that("posterior_epred ndraws subsetting works", {
  epred_100 <- posterior_epred(fit1, newdata = test_data$univariate,
                               ndraws = 100)

  expect_true(is.matrix(epred_100))
  expect_equal(nrow(epred_100), 100)

  # All values still valid
  expect_true(all(epred_100 >= 0))
  expect_true(all(is.finite(epred_100)))
})

test_that("posterior_epred process_error toggle affects variance", {
  epred_full <- posterior_epred(fit1, newdata = test_data$univariate,
                                process_error = TRUE)
  epred_mean <- posterior_epred(fit1, newdata = test_data$univariate,
                                process_error = FALSE)

  # Same dimensions
  expect_equal(dim(epred_full), dim(epred_mean))

  # process_error=FALSE should reduce or equal variance (trend fixed at mean)
  var_full <- mean(apply(epred_full, 2, var))
  var_mean <- mean(apply(epred_mean, 2, var))
  expect_true(var_mean <= var_full * 1.01)
})

test_that("posterior_epred resp argument filters multivariate response", {
  # Request single response
  epred_count <- posterior_epred(fit2, newdata = test_data$multivariate,
                                 resp = "count")

  # Returns matrix when resp specified
  expect_true(is.matrix(epred_count))
  expect_false(is.list(epred_count))

  n_draws <- nrow(posterior::as_draws_matrix(fit2$fit))
  expect_equal(nrow(epred_count), n_draws)

  # Poisson constraint
  expect_true(all(epred_count >= 0))
})

test_that("posterior_epred uses training data when newdata is NULL", {
  # Should work without explicitly passing newdata
  epred_default <- posterior_epred(fit1)

  expect_true(is.matrix(epred_default))
  n_obs <- nrow(test_data$univariate)
  expect_equal(ncol(epred_default), n_obs)
})

test_that("posterior_epred handles multivariate with different families", {
  # fit2 has count (Poisson) and biomass (Gaussian implied by continuous data)
  epred <- posterior_epred(fit2, newdata = test_data$multivariate)

  # Both should be finite
  expect_true(all(is.finite(epred$count)))
  expect_true(all(is.finite(epred$biomass)))

  # Count (Poisson) non-negative
  expect_true(all(epred$count >= 0))
})

test_that("posterior_epred matches linpred transformation for Gaussian", {
  # For Gaussian with identity link: E[Y] = eta (no transformation)
  linpred <- posterior_linpred(fit2, newdata = test_data$multivariate,
                               resp = "biomass")
  epred <- posterior_epred(fit2, newdata = test_data$multivariate,
                           resp = "biomass")

  # Should be identical for identity link
  expect_equal(epred, linpred, tolerance = 1e-10)
})
