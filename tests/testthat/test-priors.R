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
  prior <- data.frame(
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
  class(prior) <- c("brmsprior", "data.frame")
  prior
}

mock_obs_prior <- function() {
  prior <- data.frame(
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
  class(prior) <- c("brmsprior", "data.frame")
  prior
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
    expect_true(isTRUE(attr(priors, "mvgam_enhanced")))

    # Check for expected parameters
    trend_comp <- attr(priors, "trend_components")
    trend_rows <- which(trend_comp == "trend")

    if (length(trend_rows) > 0) {
      trend_classes <- priors$class[trend_rows]
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

      # Extract trend components from mvgam
      trend_comp <- attr(mvgam_priors, "trend_components")
      trend_rows <- which(trend_comp == "trend")
      mvgam_trend_priors <- mvgam_priors[trend_rows, , drop = FALSE]

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
    trend_comp <- attr(priors_no_int, "trend_components")
    trend_rows <- which(trend_comp == "trend")
    trend_classes <- priors_no_int$class[trend_rows]

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
  trend_comp <- attr(priors_multi, "trend_components")
  expect_true("observation" %in% trend_comp)
  expect_true("trend" %in% trend_comp)

  # Factor model (n_lv < n_series)
  mf_factor <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 1)
  )
  priors_factor <- get_prior(mf_factor, data = test_data, family = gaussian())

  trend_rows <- which(attr(priors_factor, "trend_components") == "trend")
  if (length(trend_rows) > 0) {
    trend_classes <- priors_factor$class[trend_rows]
    expect_true("Z" %in% trend_classes) # Factor loadings
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

    # Extract trend and observation components
    trend_comp <- attr(mvgam_priors, "trend_components")
    trend_rows <- which(trend_comp == "trend")
    obs_rows <- which(trend_comp == "observation")
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
  expect_true(all(attr(priors_obs, "trend_components") == "observation"))

  # With trend
  mf_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  priors_trend <- get_prior(mf_trend, data = test_data, family = gaussian())

  trend_comp <- attr(priors_trend, "trend_components")
  expect_true("observation" %in% trend_comp)
  expect_true("trend" %in% trend_comp)

  # With brmsformula
  bf_formula <- brms::bf(y ~ x)
  mf_brms <- mvgam_formula(bf_formula, trend_formula = ~ AR())
  priors_brms <- get_prior(mf_brms, data = test_data)
  expect_s3_class(priors_brms, "brmsprior")

  # No-intercept trend formulas
  mf_no_intercept <- mvgam_formula(y ~ x, trend_formula = ~ -1)
  priors_no_int <- get_prior(mf_no_intercept, data = test_data, family = gaussian())
  expect_s3_class(priors_no_int, "brmsprior")

  trend_comp_no_int <- attr(priors_no_int, "trend_components")
  expect_true("trend" %in% trend_comp_no_int)
  # Should default to ZMVN for ~ -1
  trend_rows <- which(trend_comp_no_int == "trend")
  trend_classes <- unique(priors_no_int$class[trend_rows])
  expect_true(any(grepl("sigma_trend", trend_classes)))

  # No-intercept with predictors
  mf_no_int_pred <- mvgam_formula(y ~ 1, trend_formula = ~ x - 1)
  priors_no_int_pred <- get_prior(mf_no_int_pred, data = test_data, family = gaussian())
  expect_s3_class(priors_no_int_pred, "brmsprior")

  trend_comp_pred <- attr(priors_no_int_pred, "trend_components")
  trend_rows_pred <- which(trend_comp_pred == "trend")
  trend_classes_pred <- unique(priors_no_int_pred$class[trend_rows_pred])
  # Should have b_trend for the predictor but no Intercept_trend
  expect_true(any(grepl("b_trend", trend_classes_pred)))
  expect_false(any(grepl("Intercept_trend", trend_classes_pred)))
})

# set_prior() function tests
# ---------------------------
test_that("set_prior handles all input types", {
  # Character strings
  obs_prior <- set_prior("normal(0, 2)", class = "Intercept")
  expect_s3_class(obs_prior, "brmsprior")
  expect_true(isTRUE(attr(obs_prior, "mvgam_enhanced")))
  expect_equal(attr(obs_prior, "trend_components"), "observation")

  trend_prior <- set_prior("normal(0, 0.5)", class = "ar1_trend")
  expect_equal(attr(trend_prior, "trend_components"), "trend")

  # brmsprior objects
  enhanced <- set_prior(obs_prior)
  expect_identical(attr(enhanced, "mvgam_enhanced"), TRUE)

  # Lists
  prior_list <- set_prior(list("normal(0, 1)", "exponential(2)"),
                         class = c("b", "sigma_trend"))
  expect_equal(nrow(prior_list), 2)

  # Vectorized
  vec_prior <- set_prior("normal(0, 1)", class = c("b", "Intercept"))
  expect_equal(nrow(vec_prior), 2)

  # Bounds
  expect_error(set_prior("normal(0, 1)", class = "b", lb = 1, ub = 0),
              "Lower bound must be less than upper bound")

  bounded <- set_prior("normal(0, 1)", class = "sigma_trend", lb = 0)
  expect_match(bounded$bound, "<lower=0>")

  # Invalid inputs
  expect_error(set_prior(123), "Invalid.*prior.*input type")
  expect_error(set_prior(list()), "Empty list")
})

test_that("set_prior validates parameters correctly", {
  # Vector length mismatch
  expect_error(
    set_prior("normal(0, 1)", class = c("b", "Intercept"),
             coef = c("x1", "x2", "x3")),
    "consistent lengths"
  )

  # Consistent lengths work
  expect_no_error(
    set_prior("normal(0, 1)", class = c("b", "b"), coef = c("x1", "x2"))
  )

  # Large vectors
  large_classes <- paste0("b", 1:20, "_trend")
  large_vec <- set_prior("normal(0, 1)", class = large_classes)
  expect_equal(nrow(large_vec), 20)
})

test_that("set_prior integrates with prior workflow", {
  # Combination with +
  p1 <- set_prior("normal(0, 1)", class = "b")
  p2 <- set_prior("exponential(2)", class = "sigma_trend")
  combined <- p1 + p2

  expect_equal(nrow(combined), 2)
  expect_s3_class(combined, "brmsprior")

  # Attributes preserved
  components <- attr(combined, "trend_components")
  expect_equal(components[1], "observation")
  expect_equal(components[2], "trend")

  # Mixed parameters work (no warning in tests per CLAUDE.md guidelines)
  mixed_result <- set_prior("normal(0, 1)", class = c("b", "ar1_trend"))
  expect_s3_class(mixed_result, "brmsprior")
  expect_true(nrow(mixed_result) > 0)
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

  # Attributes
  expect_true(isTRUE(attr(combined, "mvgam_enhanced")))
  components <- attr(combined, "trend_components")
  expect_equal(sum(components == "observation"), 1)
  expect_equal(sum(components == "trend"), 2)

  # Empty trend priors
  empty_trend <- mvgam:::create_empty_brmsprior()
  combined_empty <- mvgam:::combine_obs_trend_priors(obs_prior, empty_trend)
  expect_equal(nrow(combined_empty), 1)
})

# Enhanced print method
# ---------------------
test_that("print.brmsprior shows enhanced information", {
  p1 <- set_prior("normal(0, 1)", class = "b")
  p2 <- set_prior("exponential(2)", class = "sigma_trend")
  combined <- p1 + p2

  # Capture output
  output <- capture.output(print(combined))
  expect_true(any(grepl("mvgam", output)))
  expect_true(any(grepl("Observation", output)))
  expect_true(any(grepl("Trend", output)))
})

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
