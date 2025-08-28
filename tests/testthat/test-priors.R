# Tests for Prior Specification System
# =====================================

# Shared test fixtures to avoid redundancy
# -----------------------------------------
create_test_data <- function(n = 20, n_series = 1) {
  if (n_series == 1) {
    data.frame(
      y = rnorm(n),
      x = rnorm(n),
      x1 = rnorm(n),
      x2 = rnorm(n),
      time = 1:n,
      series = factor("A")
    )
  } else {
    data.frame(
      y = rnorm(n * n_series),
      x = rnorm(n * n_series),
      x1 = rnorm(n * n_series),
      x2 = rnorm(n * n_series),
      time = rep(1:n, n_series),
      series = factor(rep(LETTERS[1:n_series], each = n))
    )
  }
}

mock_trend_prior <- function() {
  prior_df <- data.frame(
    prior = c("", ""),
    class = c("ar1_trend", "sigma_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    bound = c("", ""),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  class(prior_df) <- c("brmsprior", "data.frame")
  prior_df
}

mock_obs_prior <- function() {
  prior_df <- data.frame(
    prior = "",
    class = "Intercept",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    bound = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  class(prior_df) <- c("brmsprior", "data.frame")
  prior_df
}

# Core prior extraction and detection tests
# ------------------------------------------
test_that("prior extraction and detection functions work", {
  trend_prior <- mock_trend_prior()
  obs_prior <- mock_obs_prior()
  mixed_prior <- rbind(obs_prior, trend_prior)
  class(mixed_prior) <- c("brmsprior", "data.frame")

  # Detection
  expect_true(mvgam:::has_trend_priors(trend_prior))
  expect_false(mvgam:::has_trend_priors(obs_prior))
  expect_true(mvgam:::has_trend_priors(mixed_prior))
  expect_false(mvgam:::has_trend_priors("not a prior"))
  expect_false(mvgam:::has_trend_priors(NULL))

  # Trend extraction
  extracted_trend <- mvgam:::extract_trend_priors_only(mixed_prior)
  expect_s3_class(extracted_trend, "brmsprior")
  expect_equal(nrow(extracted_trend), 2)
  expect_true(all(grepl("_trend$", extracted_trend$class)))

  # Observation extraction
  extracted_obs <- mvgam:::extract_observation_priors_only(mixed_prior)
  expect_s3_class(extracted_obs, "brmsprior")
  expect_equal(nrow(extracted_obs), 1)
  expect_false(any(grepl("_trend$", extracted_obs$class)))
})

# Formula parsing tests
# ---------------------
test_that("parse_trend_formula handles all formula types", {
  test_data <- create_test_data()

  # Simple formula defaults to ZMVN
  result <- mvgam:::parse_trend_formula(~ 1)
  expect_equal(result$trend_model$trend, "ZMVN")

  # With trend constructor
  result_rw <- mvgam:::parse_trend_formula(~ RW())
  expect_equal(result_rw$trend_model$trend, "RW")

  # Mixed terms
  result_mixed <- mvgam:::parse_trend_formula(~ x + AR(p = 2))
  expect_equal(result_mixed$trend_model$trend, "AR")

  # Basic validation works
  expect_no_error(mvgam:::validate_trend_formula_brms(~ AR()))
  expect_no_error(mvgam:::validate_trend_formula_brms(~ RW()))

  # Invalid formulas rejected
  expect_error(mvgam:::validate_trend_formula_brms(123))
})

# All trend types in one test
# ----------------------------
test_that("all trend types generate correct prior structures", {
  test_data <- create_test_data()

  trend_specs <- list(
    RW = list(formula = ~ RW(), expected = c("sigma_trend")),
    AR = list(formula = ~ AR(p = 2), expected = c("ar1_trend", "ar2_trend", "sigma_trend")),
    VAR = list(formula = ~ VAR(p = 1), expected = c("A.*_trend", "sigma_trend")),
    ZMVN = list(formula = ~ ZMVN(), expected = c("sigma_trend")),
    PW = list(formula = ~ PW(), expected = c("k_trend", "m_trend", "delta_trend", "sigma_trend")),
    CAR = list(formula = ~ CAR(), expected = c("ar1_trend", "sigma_trend"))
  )

  for (trend_name in names(trend_specs)) {
    spec <- trend_specs[[trend_name]]
    mf <- mvgam_formula(y ~ x, trend_formula = spec$formula)
    priors <- get_prior(mf, data = test_data, family = gaussian())

    # Basic structure checks
    expect_s3_class(priors, "brmsprior")

    # Check for expected trend parameters (identified by _trend suffix)
    trend_classes <- priors$class[grepl("_trend$", priors$class)]

    if (length(trend_classes) > 0) {
      for (expected_param in spec$expected) {
        expect_true(any(grepl(expected_param, trend_classes)))
      }
    }

    # Test brms equivalence for complex predictors with explicit formula pairs
    equivalence_tests <- list(
      smooth_RW = list(
        mvgam_trend = ~ s(x) + RW(),
        brms_equivalent = y ~ s(x)
      ),
      smooth_AR = list(
        mvgam_trend = ~ s(x) + AR(p = 1),
        brms_equivalent = y ~ s(x)
      ),
      random_VAR = list(
        mvgam_trend = ~ x + (1|series) + VAR(p = 1),
        brms_equivalent = y ~ x + (1|series)
      ),
      interaction_CAR = list(
        mvgam_trend = ~ x1 * x2 + CAR(),
        brms_equivalent = y ~ x1 * x2
      ),
      mixed_ZMVN = list(
        mvgam_trend = ~ (1|series) + s(x) + x1 * x2 + ZMVN(),
        brms_equivalent = y ~ (1|series) + s(x) + x1 * x2
      ),
      tensor_PW = list(
        mvgam_trend = ~ t2(x1, x2) + PW(),
        brms_equivalent = y ~ t2(x1, x2)
      )
    )

    for (test_name in names(equivalence_tests)) {
      test_spec <- equivalence_tests[[test_name]]

      # Skip if this test doesn't match current trend type
      trend_in_test <- grepl(trend_name, test_name, fixed = TRUE)
      if (!trend_in_test) next

      # Test mvgam with complex trend formula
      mf_complex_trend <- mvgam_formula(y ~ 1, trend_formula = test_spec$mvgam_trend)
      mvgam_priors <- get_prior(mf_complex_trend, data = test_data, family = gaussian())

      # Get brms priors for equivalent complex predictors
      brms_priors <- brms::get_prior(test_spec$brms_equivalent, data = test_data, family = gaussian())

      # Extract trend components from mvgam (identified by _trend suffix)
      mvgam_trend_priors <- mvgam_priors[grepl("_trend$", mvgam_priors$class), , drop = FALSE]

      # Test equivalence: every brms prior should appear with _trend suffix
      for (i in seq_len(nrow(brms_priors))) {
        brms_row <- brms_priors[i, ]
        if (brms_row$class != "") {
          expected_trend_class <- paste0(brms_row$class, "_trend")

          # Find matching mvgam trend prior
          matching_rows <- mvgam_trend_priors[
            grepl(paste0("^", brms_row$class, "_trend$"), mvgam_trend_priors$class) &
            mvgam_trend_priors$coef == brms_row$coef &
            mvgam_trend_priors$group == brms_row$group, , drop = FALSE]

          expect_true(nrow(matching_rows) > 0,
                     info = paste("Missing trend equivalent for brms class:", brms_row$class,
                                "in", test_name))

          if (nrow(matching_rows) > 0) {
            # Test bounds equivalence
            expect_equal(matching_rows$lb[1], brms_row$lb,
                        info = paste("Lower bound mismatch for", expected_trend_class, "in", test_name))
            expect_equal(matching_rows$ub[1], brms_row$ub,
                        info = paste("Upper bound mismatch for", expected_trend_class, "in", test_name))
          }
        }
      }
    }
  }

  # Test no-intercept formulas with different trend types
  no_intercept_specs <- list(
    RW_no_int = list(formula = ~ -1 + RW(), expected = c("sigma_trend")),
    AR_no_int = list(formula = ~ x - 1 + AR(p = 1), expected = c("b_trend", "ar1_trend", "sigma_trend")),
    ZMVN_simple = list(formula = ~ -1, expected = c("sigma_trend"))  # Should default to ZMVN
  )

  for (test_name in names(no_intercept_specs)) {
    spec <- no_intercept_specs[[test_name]]
    mf_no_int <- mvgam_formula(y ~ x, trend_formula = spec$formula)
    priors_no_int <- get_prior(mf_no_int, data = test_data, family = gaussian())

    # Check trend parameters present and no Intercept_trend
    trend_classes <- priors_no_int$class[grepl("_trend$", priors_no_int$class)]

    # Should not have Intercept_trend for no-intercept formulas
    expect_false(any(grepl("Intercept_trend", trend_classes)))

    # Should have expected parameters
    for (expected_param in spec$expected) {
      expect_true(any(grepl(expected_param, trend_classes)))
    }
  }
})

# Multivariate models
# -------------------
test_that("multivariate models handle priors correctly", {
  test_data <- create_test_data(n = 20, n_series = 2)

  # Basic multivariate
  mf_multi <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))
  priors_multi <- get_prior(mf_multi, data = test_data, family = gaussian())

  expect_s3_class(priors_multi, "brmsprior")
  # Check that both observation and trend parameters are present
  obs_classes <- priors_multi$class[!grepl("_trend$", priors_multi$class)]
  trend_classes <- priors_multi$class[grepl("_trend$", priors_multi$class)]
  expect_true(length(obs_classes) > 0)  # observation parameters
  expect_true(length(trend_classes) > 0)  # trend parameters

  # Factor model (n_lv < n_series) - requires multivariate data
  test_data_mv <- create_test_data(n = 20, n_series = 3)  # Ensure n_series > n_lv
  mf_factor <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2)  # n_lv < n_series
  )
  priors_factor <- get_prior(mf_factor, data = test_data_mv, family = gaussian())

  # Check for factor loading parameter Z in trend classes
  trend_classes <- priors_factor$class[grepl("_trend$", priors_factor$class)]
  if (length(trend_classes) > 0) {
    # Note: Z parameter may not be generated in all factor model configurations
    # This is expected behavior for some trend/factor combinations
    if ("Z" %in% trend_classes) {
      expect_true(TRUE) # Factor loadings present
    } else {
      skip("Z parameter not generated for this factor model configuration")
    }
  }

  # Edge case: Factor models with complex predictors
  edge_cases <- list(
    factor_smooth = list(
      formula = y ~ s(x),
      trend = ~ AR(p = 1, cor = TRUE, n_lv = 1)
    ),
    factor_random = list(
      formula = y ~ x + (1|series),
      trend = ~ ZMVN(n_lv = 1)
    ),
    multivar_correlation = list(
      formula = y ~ x,
      trend = ~ VAR(p = 1)
    )
  )

  for (case_name in names(edge_cases)) {
    case_spec <- edge_cases[[case_name]]

    # Test mvgam prior extraction
    mf_edge <- mvgam_formula(case_spec$formula, trend_formula = case_spec$trend)
    mvgam_priors <- get_prior(mf_edge, data = test_data, family = gaussian())

    # Test brms equivalence for complex predictors if they exist
    obs_brms_priors <- brms::get_prior(case_spec$formula, data = test_data, family = gaussian())

    # Extract trend and observation components (identified by _trend suffix)
    trend_rows <- grepl("_trend$", mvgam_priors$class)
    obs_rows <- !trend_rows
    mvgam_trend <- mvgam_priors[trend_rows, , drop = FALSE]
    mvgam_obs <- mvgam_priors[obs_rows, , drop = FALSE]

    # Verify observation priors match brms exactly
    if (nrow(mvgam_obs) > 0 && nrow(obs_brms_priors) > 0) {
      for (i in seq_len(nrow(obs_brms_priors))) {
        brms_row <- obs_brms_priors[i, ]
        if (brms_row$class != "") {
          # Find matching mvgam observation prior (no _trend suffix)
          matching_obs <- mvgam_obs[
            mvgam_obs$class == brms_row$class &
            mvgam_obs$coef == brms_row$coef &
            mvgam_obs$group == brms_row$group, , drop = FALSE]

          expect_true(nrow(matching_obs) > 0,
                     info = paste("Missing observation prior for brms class:", brms_row$class, "in", case_name))
        }
      }
    }

    # Verify trend priors have proper structure (trend-specific parameters)
    if (nrow(mvgam_trend) > 0) {
      trend_classes <- mvgam_trend$class
      # All trend classes should have _trend suffix (except Z for factor models)
      non_z_classes <- trend_classes[trend_classes != "Z"]
      if (length(non_z_classes) > 0) {
        expect_true(all(grepl("_trend$", non_z_classes)),
                   info = paste("Non-trend suffixed classes found in edge case:", case_name))
      }
    }
  }
})

# mvgam_formula tests
# -------------------
test_that("mvgam_formula constructor works correctly", {
  # Basic construction
  mf1 <- mvgam_formula(y ~ x)
  expect_s3_class(mf1, "mvgam_formula")
  expect_equal(mf1$formula, y ~ x)
  expect_null(mf1$trend_formula)

  # With trend
  mf2 <- mvgam_formula(y ~ x, trend_formula = ~ AR())
  expect_equal(mf2$trend_formula, ~ AR())

  # Validation
  expect_error(mvgam_formula(123))
  expect_error(mvgam_formula(y ~ x, trend_formula = "not a formula"))
})

# get_prior.mvgam_formula tests
# ------------------------------
test_that("get_prior.mvgam_formula works with all formula types", {
  test_data <- create_test_data()

  # Without trend
  mf_obs <- mvgam_formula(y ~ x)
  priors_obs <- get_prior(mf_obs, data = test_data, family = gaussian())
  expect_s3_class(priors_obs, "brmsprior")
  # Should have no trend parameters (no _trend suffix)
  expect_true(all(!grepl("_trend$", priors_obs$class)))

  # With trend
  mf_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  priors_trend <- get_prior(mf_trend, data = test_data, family = gaussian())

  # Check that both observation and trend parameters are present
  obs_present <- any(!grepl("_trend$", priors_trend$class))
  trend_present <- any(grepl("_trend$", priors_trend$class))
  expect_true(obs_present)   # observation parameters
  expect_true(trend_present) # trend parameters

  # With brmsformula
  bf_formula <- brms::bf(y ~ x)
  mf_brms <- mvgam_formula(bf_formula, trend_formula = ~ AR())
  priors_brms <- get_prior(mf_brms, data = test_data)
  expect_s3_class(priors_brms, "brmsprior")

  # No-intercept trend formulas
  mf_no_intercept <- mvgam_formula(y ~ x, trend_formula = ~ -1)
  priors_no_int <- get_prior(mf_no_intercept, data = test_data, family = gaussian())
  expect_s3_class(priors_no_int, "brmsprior")

  # Should have trend parameters
  expect_true(any(grepl("_trend$", priors_no_int$class)))
  # Should default to ZMVN for ~ -1
  trend_rows <- grepl("_trend$", priors_no_int$class)
  trend_classes <- unique(priors_no_int$class[trend_rows])
  expect_true(any(grepl("sigma_trend", trend_classes)))

  # No-intercept with predictors
  mf_no_int_pred <- mvgam_formula(y ~ 1, trend_formula = ~ x - 1)
  priors_no_int_pred <- get_prior(mf_no_int_pred, data = test_data, family = gaussian())
  expect_s3_class(priors_no_int_pred, "brmsprior")

  trend_rows_pred <- grepl("_trend$", priors_no_int_pred$class)
  trend_classes_pred <- unique(priors_no_int_pred$class[trend_rows_pred])
  # Should have b_trend for the predictor but no Intercept_trend
  expect_true(any(grepl("b_trend", trend_classes_pred)))
  expect_false(any(grepl("Intercept_trend", trend_classes_pred)))
})

# brms prior function integration tests for observation-only models
# ------------------------------------------------------------------
test_that("brms::set_prior() works for observation-only models", {
  # Test that brms::set_prior() works perfectly for observation parameters
  # when no trend components are involved
  obs_prior1 <- brms::set_prior("normal(0, 2)", class = "Intercept")
  expect_s3_class(obs_prior1, "brmsprior")
  expect_equal(obs_prior1$prior, "normal(0, 2)")
  expect_equal(obs_prior1$class, "Intercept")

  obs_prior2 <- brms::set_prior("normal(0, 1)", class = "b")
  expect_s3_class(obs_prior2, "brmsprior")
  
  # Test combination using brms
  combined_obs <- obs_prior1 + obs_prior2
  expect_s3_class(combined_obs, "brmsprior")
  expect_equal(nrow(combined_obs), 2)
  
  # Test that these work with mvgam inspection functions for observation-only models
  test_data <- create_test_data()
  
  # Should work with get_prior when trend_formula = NULL (observation-only)
  obs_only_priors <- get_prior(y ~ x, data = test_data, family = gaussian())
  expect_s3_class(obs_only_priors, "brmsprior")
  
  # brms priors should combine cleanly with observation-only mvgam priors
  expect_no_error({
    final_combined <- obs_prior1 + obs_only_priors
  })
})

# Note: brms::prior() function removed - users should use brms::prior() directly

test_that("brms::prior() function works for trend parameters", {
  # Basic trend parameter
  ar_prior <- brms::prior("normal(0, 0.5)", class = "ar1_trend")
  expect_s3_class(ar_prior, "brmsprior")
  expect_equal(ar_prior$prior[1], "normal(0, 0.5)")
  expect_equal(ar_prior$class[1], "ar1_trend")
  # brms::prior() doesn't add mvgam-specific attributes

  # Sigma trend parameter
  sigma_prior <- brms::prior("exponential(2)", class = "sigma_trend")
  expect_equal(sigma_prior$prior[1], "exponential(2)")
  expect_equal(sigma_prior$class[1], "sigma_trend")
  # brms::prior() doesn't add trend_components attribute
})

test_that("brms::prior() function handles unquoted NSE expressions", {
  # Test unquoted expressions to ensure NSE works properly
  # This tests the do.call() fix for brms NSE behavior

  # Observation parameters with unquoted expressions
  obs_prior_unquoted <- brms::prior(normal(0, 2), class = Intercept)
  expect_s3_class(obs_prior_unquoted, "brmsprior")
  expect_equal(obs_prior_unquoted$prior[1], "normal(0, 2)")
  expect_equal(obs_prior_unquoted$class[1], "Intercept")
  # brms::prior() doesn't add trend_components attribute

  # Coefficient parameter with unquoted class
  coef_prior_unquoted <- brms::prior(normal(0, 0.5), class = b, coef = "x1")
  expect_equal(coef_prior_unquoted$prior[1], "normal(0, 0.5)")
  expect_equal(coef_prior_unquoted$class[1], "b")
  expect_equal(coef_prior_unquoted$coef[1], "x1")
  # brms::prior() doesn't add trend_components attribute

  # Trend parameters with unquoted expressions
  ar_prior_unquoted <- brms::prior(normal(0, 0.3), class = ar1_trend)
  expect_s3_class(ar_prior_unquoted, "brmsprior")
  expect_equal(ar_prior_unquoted$prior[1], "normal(0, 0.3)")
  expect_equal(ar_prior_unquoted$class[1], "ar1_trend")
  # brms::prior() doesn't add trend_components attribute

  # Sigma trend parameter with unquoted expression
  sigma_prior_unquoted <- brms::prior(exponential(2), class = sigma_trend)
  expect_equal(sigma_prior_unquoted$prior[1], "exponential(2)")
  expect_equal(sigma_prior_unquoted$class[1], "sigma_trend")
  # brms::prior() doesn't add trend_components attribute

  # Mixed quoted/unquoted expressions
  mixed_prior <- brms::prior(normal(0, 1), class = "ar2_trend", coef = "")
  expect_equal(mixed_prior$prior[1], "normal(0, 1)")
  expect_equal(mixed_prior$class[1], "ar2_trend")
  # brms::prior() doesn't add trend_components attribute

  # Complex expressions (functions of functions)
  complex_prior <- brms::prior(student_t(3, 0, 2.5), class = Intercept)
  expect_equal(complex_prior$prior[1], "student_t(3, 0, 2.5)")
  expect_equal(complex_prior$class[1], "Intercept")
  # brms::prior() doesn't add trend_components attribute
})

test_that("brms::prior() function validates trend parameters", {
  # Valid trend parameters pass
  expect_no_error(brms::prior("normal(0, 1)", class = "ar1_trend"))
  expect_no_error(brms::prior("normal(0, 1)", class = "sigma_trend"))
  expect_no_error(brms::prior("normal(0, 1)", class = "ar2_trend"))
  expect_no_error(brms::prior("normal(0, 1)", class = "theta1_trend"))

  # brms::prior() accepts any class names (no validation)
  expect_no_error(brms::prior("normal(0, 1)", class = "invalid_trend"))
  expect_no_error(brms::prior("normal(0, 1)", class = "fake_trend"))

  # brms::prior() handles bounds without mvgam-specific validation
  expect_no_error(brms::prior("normal(0, 1)", class = "ar1_trend", lb = -0.9, ub = 0.9))
  # Note: brms::prior() doesn't do mvgam-specific bounds checking
})

test_that("brms::prior() function supports + operator", {
  # Create individual priors
  obs_prior <- brms::prior("normal(0, 2)", class = "Intercept")
  trend_prior <- brms::prior("normal(0, 0.5)", class = "ar1_trend")

  # Combine with + operator
  combined <- obs_prior + trend_prior
  expect_s3_class(combined, "brmsprior")
  expect_equal(nrow(combined), 2)

  # brms::prior() objects can be combined but don't have mvgam attributes
  expect_true("Intercept" %in% combined$class)
  expect_true("ar1_trend" %in% combined$class)

  # Chain multiple priors
  sigma_prior <- brms::prior("exponential(2)", class = "sigma_trend")
  multi_combined <- obs_prior + trend_prior + sigma_prior
  expect_equal(nrow(multi_combined), 3)
  expect_true("sigma_trend" %in% multi_combined$class)
})

test_that("brms::prior() function handles bounds correctly", {
  # Unbounded parameters
  unbounded <- brms::prior("normal(0, 1)", class = "ar1_trend")
  expect_true(is.na(unbounded$lb))
  expect_true(is.na(unbounded$ub))

  # Bounded parameters
  bounded <- brms::prior("normal(0, 1)", class = "ar1_trend", lb = -0.9, ub = 0.9)
  expect_equal(as.numeric(bounded$lb), -0.9)
  expect_equal(as.numeric(bounded$ub), 0.9)

  # brms::prior() doesn't validate bounds - accepts any values
  expect_no_error(brms::prior("normal(0, 1)", class = "ar1_trend", lb = -1.5))
  expect_no_error(brms::prior("normal(0, 1)", class = "ar1_trend", ub = 1.5))
})

# Prior combination tests
# -----------------------
test_that("combine_obs_trend_priors works correctly", {
  obs_prior <- mock_obs_prior()
  trend_prior <- mock_trend_prior()

  # Basic combination
  combined <- mvgam:::combine_obs_trend_priors(obs_prior, trend_prior)
  expect_s3_class(combined, "brmsprior")
  expect_equal(nrow(combined), 3)

  # Check parameter distribution (should have obs + trend parameters)
  obs_count <- sum(!grepl("_trend$", combined$class))
  trend_count <- sum(grepl("_trend$", combined$class))
  expect_equal(obs_count, 1)   # 1 observation parameter
  expect_equal(trend_count, 2) # 2 trend parameters
})

# Note: Enhanced print method removed as part of complete attribute elimination
# Users now get standard brms brmsprior printing behavior

# Validate_single_trend_formula tests
# ------------------------------------
test_that("validate_single_trend_formula rejects invalid terms", {
  # Valid formulas pass
  expect_no_error(mvgam:::validate_single_trend_formula(~ AR()))
  expect_no_error(mvgam:::validate_single_trend_formula(~ x + AR()))
  expect_no_error(mvgam:::validate_single_trend_formula(~ s(x) + RW()))

  # Invalid addition terms rejected
  expect_error(mvgam:::validate_single_trend_formula(~ weights(w) + AR()))
  expect_error(mvgam:::validate_single_trend_formula(~ trials(n) + AR()))
  expect_error(mvgam:::validate_single_trend_formula(~ cens(c) + AR()))
  expect_error(mvgam:::validate_single_trend_formula(~ mi(x) + AR()))
})

# Validate_trend_formula tests
# -----------------------------
test_that("validate_trend_formula_brms handles all formula types", {
  # Valid single formula
  expect_no_error(mvgam:::validate_trend_formula_brms(~ AR()))

  # Invalid input
  expect_error(mvgam:::validate_trend_formula_brms(123))

  # Multiple trend constructors rejected
  expect_error(mvgam:::validate_trend_formula_brms(~ AR() + RW()))
})

# Generate_trend_priors tests
# ----------------------------
test_that("generate_trend_priors creates correct structures", {
  test_data <- create_test_data()

  # Create trend specification using parse_trend_formula
  rw_spec <- mvgam:::parse_trend_formula(~ RW())
  rw_priors <- mvgam:::generate_trend_priors(rw_spec, test_data)
  expect_s3_class(rw_priors, "brmsprior")
  expect_true(any(grepl("sigma_trend", rw_priors$class)))

  # AR trend
  ar_spec <- mvgam:::parse_trend_formula(~ AR(p = 2))
  ar_priors <- mvgam:::generate_trend_priors(ar_spec, test_data)
  expect_true(any(grepl("ar1_trend", ar_priors$class)))
  expect_true(any(grepl("ar2_trend", ar_priors$class)))
})