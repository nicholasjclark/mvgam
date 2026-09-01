# Tests for Prior Specification System
# =====================================

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

test_that("all trend types generate correct prior structures", {
  test_data <- create_test_data()

  trend_specs <- list(
    RW = list(formula = ~ RW(), expected = c("sigma_trend")),
    AR = list(formula = ~ AR(p = 2), expected = c("ar1_trend", "ar2_trend", "sigma_trend")),
    VAR = list(formula = ~ VAR(p = 1), expected = c("A.*_trend", "sigma_trend")),
    ZMVN = list(formula = ~ ZMVN(), expected = c("sigma_trend")),
    # No `sigma_trend`: the piecewise path is a deterministic
    # function of its changepoints, so there is no innovation.
    PW = list(formula = ~ PW(),
              expected = c("k_trend", "m_trend", "delta_trend")),
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
      # A trend without an innovation must not advertise a scale.
      trend_model <- mvgam:::parse_trend_formula(spec$formula)$trend_model
      expect_equal(
        "sigma_trend" %in% trend_classes,
        mvgam:::samples_innovation_scale(trend_model)
      )
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

      # Test equivalence: every brms prior (except Intercept) should appear with _trend suffix
      # Trend formulas exclude intercepts by default
      # brms's `sigma` maps onto the trend's innovation scale, so a
      # trend that samples no innovation has nothing to map it to.
      trend_model <-
        mvgam:::parse_trend_formula(test_spec$mvgam_trend)$trend_model
      unmapped <- if (mvgam:::samples_innovation_scale(trend_model)) {
        c("", "Intercept")
      } else {
        c("", "Intercept", "sigma")
      }

      for (i in seq_len(nrow(brms_priors))) {
        brms_row <- brms_priors[i, ]
        if (!brms_row$class %in% unmapped) {
          expected_trend_class <- paste0(brms_row$class, "_trend")

          # Find matching mvgam trend prior
          matching_rows <- mvgam_trend_priors[
            grepl(paste0("^", brms_row$class, "_trend$"), mvgam_trend_priors$class) &
            mvgam_trend_priors$coef == brms_row$coef &
            mvgam_trend_priors$group == brms_row$group, , drop = FALSE]

          expect_true(
            nrow(matching_rows) > 0,
            label = paste("trend equivalent of brms class", brms_row$class,
                          "in", test_name)
          )

          if (nrow(matching_rows) > 0) {
            # Test bounds equivalence
            expect_equal(
              matching_rows$lb[1], brms_row$lb,
              label = paste("lower bound of", expected_trend_class,
                            "in", test_name)
            )
            expect_equal(
              matching_rows$ub[1], brms_row$ub,
              label = paste("upper bound of", expected_trend_class,
                            "in", test_name)
            )
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

  # Check for factor loading parameter Z (should exist for factor models)
  all_classes <- priors_factor$class
  trend_classes <- priors_factor$class[grepl("_trend$", priors_factor$class)]

  # Z parameter should exist for factor models (not with _trend suffix)
  # A factor model prices its loadings, so `Z` earns a prior row of its
  # own alongside the suffixed trend parameters.
  expect_true("Z" %in% all_classes)
  expect_true(length(trend_classes) > 0)

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

          expect_true(
            nrow(matching_obs) > 0,
            label = paste("observation prior for brms class",
                          brms_row$class, "in", case_name)
          )
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
                    label = paste("trend suffixes in edge case", case_name))
      }
    }
  }
})

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

test_that("validate_single_trend_formula rejects invalid terms", {
  # Valid formulas pass
  expect_no_error(mvgam:::validate_single_trend_formula(~ AR()))
  expect_no_error(mvgam:::validate_single_trend_formula(~ x + AR()))
  expect_no_error(mvgam:::validate_single_trend_formula(~ s(x) + RW()))

  # Invalid addition terms rejected
  expect_error(mvgam:::validate_single_trend_formula(~ weights(w) + AR()))
  expect_error(mvgam:::validate_single_trend_formula(~ trials(n) + AR()))
  expect_error(mvgam:::validate_single_trend_formula(~ cens(c) + AR()))
  # mi() is the one brms addition-term special that IS allowed on
  # the trend side (the gatekeeper in validate_single_trend_formula
  # whitelists it so users can impute missing latent-scale
  # predictors).
  expect_no_error(mvgam:::validate_single_trend_formula(~ mi(x) + AR()))
})

test_that("validate_trend_formula_brms handles all formula types", {
  # Valid single formula
  expect_no_error(mvgam:::validate_trend_formula_brms(~ AR()))

  # Invalid input
  expect_error(mvgam:::validate_trend_formula_brms(123))

  # Multiple trend constructors rejected
  expect_error(mvgam:::validate_trend_formula_brms(~ AR() + RW()))
})

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

test_that("distributional models work correctly with trends", {
  test_data <- create_test_data(n = 30)

  # bf() with sigma predictor and embedded family
  bf_sigma <- brms::bf(y ~ x, sigma ~ x1, family = gaussian())
  mf_distrib <- mvgam_formula(bf_sigma, trend_formula = ~ AR(p = 1))
  priors_distrib <- get_prior(mf_distrib, data = test_data)

  expect_s3_class(priors_distrib, "brmsprior")

  # Check observation parameters (should match brms exactly)
  brms_distrib_priors <- brms::get_prior(bf_sigma, data = test_data)
  obs_classes <- priors_distrib$class[!grepl("_trend$", priors_distrib$class)]
  trend_classes <- priors_distrib$class[grepl("_trend$", priors_distrib$class)]

  # Should have both observation and trend parameters
  expect_true(length(obs_classes) > 0)
  expect_true(length(trend_classes) > 0)

  # Verify observation priors match brms exactly
  for (i in seq_len(nrow(brms_distrib_priors))) {
    brms_row <- brms_distrib_priors[i, ]
    if (brms_row$class != "") {
      matching_obs <- priors_distrib[
        priors_distrib$class == brms_row$class &
        priors_distrib$coef == brms_row$coef &
        priors_distrib$group == brms_row$group &
        priors_distrib$dpar == brms_row$dpar, , drop = FALSE]

      expect_true(
        nrow(matching_obs) > 0,
        label = paste("distributional parameter", brms_row$class,
                      "for dpar", brms_row$dpar)
      )
    }
  }

  # bf() with multiple parameters
  bf_multi_param <- brms::bf(y ~ x, sigma ~ x1, nu ~ x2, family = student())
  mf_multi <- mvgam_formula(bf_multi_param, trend_formula = ~ RW())
  priors_multi <- get_prior(mf_multi, data = test_data)

  # Should have parameters for mu, sigma, and nu
  expect_true(any(grepl("Intercept", priors_multi$class[priors_multi$dpar == ""])))     # mu
  expect_true(any(grepl("Intercept", priors_multi$class[priors_multi$dpar == "sigma"]))) # sigma
  expect_true(any(grepl("Intercept", priors_multi$class[priors_multi$dpar == "nu"])))    # nu
  expect_true(any(grepl("_trend$", priors_multi$class)))  # trend parameters
})

test_that("multivariate responses with mvbind work correctly", {
  # Create multivariate data
  mv_data <- data.frame(
    count = rpois(40, 5),
    biomass = rnorm(40, 10, 2),
    x = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )

  # Combined bf() objects with different families (Pattern 4 from quick-reference)
  mv_formula <- brms::bf(count ~ x, family = poisson()) +
                brms::bf(biomass ~ x, family = gaussian())
  mf_mvbind <- mvgam_formula(mv_formula, trend_formula = ~ VAR(p = 1))
  priors_mvbind <- get_prior(mf_mvbind, data = mv_data)

  expect_s3_class(priors_mvbind, "brmsprior")

  # Should have response-specific parameters
  response_names <- c("count", "biomass")
  for (resp in response_names) {
    resp_params <- priors_mvbind[priors_mvbind$resp == resp, ]
    expect_true(nrow(resp_params) > 0,
                label = paste("parameters for response", resp))
  }

  # Should have trend parameters
  trend_classes <- priors_mvbind$class[grepl("_trend$", priors_mvbind$class)]
  expect_true(length(trend_classes) > 0)

  # Verify brms equivalence
  brms_mvbind_priors <- brms::get_prior(mv_formula, data = mv_data)
  obs_priors_mv <- priors_mvbind[!grepl("_trend$", priors_mvbind$class), ]

  # Each brms parameter should appear in mvgam observation priors
  for (i in seq_len(nrow(brms_mvbind_priors))) {
    brms_row <- brms_mvbind_priors[i, ]
    if (brms_row$class != "") {
      matching <- obs_priors_mv[
        obs_priors_mv$class == brms_row$class &
        obs_priors_mv$coef == brms_row$coef &
        obs_priors_mv$group == brms_row$group &
        obs_priors_mv$resp == brms_row$resp, , drop = FALSE]

      expect_true(
        nrow(matching) > 0,
        label = paste("mvbind parameter", brms_row$class,
                      "for response", brms_row$resp)
      )
    }
  }
})

test_that("non-Gaussian families work with trends", {
  test_data <- create_test_data()
  test_data$count <- rpois(nrow(test_data), 5)
  test_data$binary <- rbinom(nrow(test_data), 1, 0.3)

  family_tests <- list(
    poisson = list(formula = count ~ x, family = poisson()),
    binomial = list(formula = binary ~ x, family = binomial()),
    gamma = list(formula = y ~ x, family = Gamma()),
    exponential = list(formula = y ~ x, family = exponential()),
    beta = list(formula = I((y + 10) / 20) ~ x, family = Beta())
  )

  for (family_name in names(family_tests)) {
    family_spec <- family_tests[[family_name]]

    # Test with trend
    mf_family <- mvgam_formula(family_spec$formula, trend_formula = ~ AR(p = 1))
    priors_family <- get_prior(mf_family, data = test_data, family = family_spec$family)

    expect_s3_class(priors_family, "brmsprior")

    # Should have trend parameters
    trend_classes <- priors_family$class[grepl("_trend$", priors_family$class)]
    expect_true(length(trend_classes) > 0,
                label = paste("trend parameters for family", family_name))

    # Verify observation parameters match brms
    brms_family_priors <- brms::get_prior(family_spec$formula,
                                         data = test_data,
                                         family = family_spec$family)
    obs_priors_fam <- priors_family[!grepl("_trend$", priors_family$class), ]

    # Basic structure check
    expect_true(
      nrow(obs_priors_fam) > 0,
      label = paste("observation parameters for family", family_name)
    )
    expect_true(
      nrow(brms_family_priors) > 0,
      label = paste("brms parameters for family", family_name)
    )
  }
})

test_that("embedded family edge cases work correctly", {
  test_data <- create_test_data(n = 25)
  test_data$count <- rpois(nrow(test_data), 3)

  # bf() with embedded family - no explicit family argument needed
  bf_embedded <- brms::bf(count ~ x, family = poisson())
  mf_embedded <- mvgam_formula(bf_embedded, trend_formula = ~ RW())

  # Should not require family argument
  expect_no_error({
    priors_embedded <- get_prior(mf_embedded, data = test_data)
  })

  priors_embedded <- get_prior(mf_embedded, data = test_data)
  expect_s3_class(priors_embedded, "brmsprior")

  # Should have both observation and trend parameters
  expect_true(any(!grepl("_trend$", priors_embedded$class))) # obs
  expect_true(any(grepl("_trend$", priors_embedded$class)))  # trend

  # Complex embedded case with multiple parameters
  bf_complex_embedded <- brms::bf(count ~ x, zi ~ x1, family = zero_inflated_poisson())
  mf_complex_embedded <- mvgam_formula(bf_complex_embedded, trend_formula = ~ AR(p = 1))

  expect_no_error({
    priors_complex_embedded <- get_prior(mf_complex_embedded, data = test_data)
  })

  # Should have zi parameters
  # An embedded zero-inflated family has to price its `zi` parameter,
  # which is the half a plain family lookup would miss.
  zi_params <- priors_complex_embedded[priors_complex_embedded$dpar == "zi", ]
  expect_true(nrow(zi_params) > 0)
})


# get_prior.mvgam: post-fit prior inspection should surface the full
# adjustable set (obs + trend + lifted stanvar rows), not just the
# user-supplied overrides stored on `object$prior`. The merge marks
# overridden rows with `source = "user"` so callers can tell which
# rows the user touched. Uses a stub mvgam object to avoid fitting
# Stan in CI (per the package's no-fits-in-testthat rule).

test_that("get_prior.mvgam re-derives full table and marks user rows", {
  test_data <- create_test_data()
  user_prior <- c(
    brms::prior(normal(0, 1), class = b),
    brms::prior(exponential(2), class = sigma_trend)
  )
  obs_formula <- y ~ x
  trend_call <- ~ AR(p = 1)

  # Build the stub: replicate the slots get_prior.mvgam needs without
  # fitting Stan. The user-prior table emulates what mvgam() stores
  # on `object$prior` at fit time.
  stub <- structure(
    list(
      formula = obs_formula,
      trend_call = trend_call,
      data = test_data,
      family = gaussian(),
      prior = user_prior
    ),
    class = c("mvgam", "brmsfit")
  )

  out <- suppressWarnings(get_prior(stub))
  expect_s3_class(out, "brmsprior")
  expect_true(nrow(out) > nrow(user_prior),
                label = "re-derived table should expand beyond user rows")
  expect_true("sigma_trend" %in% out$class,
                label = "trend-side rows should be visible")
  user_marked <- out[out$source == "user", , drop = FALSE]
  expect_true(any(user_marked$class == "b"),
                label = "obs-side override should be marked source=user")
})

test_that("get_prior.mvgam falls back to object$prior when re-derive fails", {
  stub <- structure(
    list(
      formula = NULL,  # missing slot triggers fallback
      prior = c(brms::prior(normal(0, 1), class = b))
    ),
    class = c("mvgam", "brmsfit")
  )
  out <- suppressWarnings(get_prior(stub))
  expect_s3_class(out, "brmsprior")
  expect_identical(nrow(out), 1L)
})

test_that("get_prior.mvgam errors clearly when fit lacks any prior info", {
  stub <- structure(
    list(formula = NULL, prior = NULL),
    class = c("mvgam", "brmsfit")
  )
  expect_error(get_prior(stub), "prior table")
})


# ---- mvgam's injected defaults must not block user priors -----------

prior_test_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:3)))
  d$y <- rpois(nrow(d), 5)
  d
}

grouped_prior_test_data <- function() {
  set.seed(1)
  d <- expand.grid(time = 1:60, series = factor(paste0("s", 1:4)))
  d$grp <- factor(rep(c("a", "a", "b", "b"), each = 60))
  d$y <- rpois(nrow(d), 5)
  d
}

prior_code <- function(mf, fam, pr = NULL, data = prior_test_data()) {
  paste(unlist(stancode(mf, data = data, family = fam, prior = pr,
                        backend = "cmdstanr")), collapse = "\n")
}

test_that("merge_default_priors() drops defaults the user has claimed", {
  # Concatenating leaves two rows for the same class, and brms refuses
  # the whole set with "Duplicated prior specifications are not
  # allowed", so a user trying to override a default got an error
  # rather than their prior.
  defaults <- c(brms::prior("gamma(2, 0.5)", class = "shape"),
                brms::prior("gamma(2, 0.25)", class = "mtail"))
  user <- brms::prior("gamma(3, 1)", class = "shape")

  merged <- mvgam:::merge_default_priors(defaults, user, y ~ 1)
  expect_equal(sum(merged$class == "shape"), 1L)
  expect_true("gamma(3, 1)" %in% merged$prior)
  expect_false("gamma(2, 0.5)" %in% merged$prior)
  # the default the user did not name survives
  expect_true("gamma(2, 0.25)" %in% merged$prior)
})

test_that("merge_default_priors() passes both sides through untouched", {
  defaults <- brms::prior("gamma(2, 0.5)", class = "shape")
  expect_equal(
    nrow(mvgam:::merge_default_priors(defaults, NULL, y ~ 1)), 1L
  )
  user <- brms::prior("normal(0, 1)", class = "b")
  merged <- mvgam:::merge_default_priors(defaults, user, y ~ 1)
  expect_setequal(merged$class, c("shape", "b"))
})

test_that("beta_nb() default priors can be overridden", {
  mf <- mvgam_formula(y ~ 1)
  sc_default <- prior_code(mf, beta_nb())
  expect_true(grepl("gamma_lpdf(shape | 2, 0.5)", sc_default, fixed = TRUE))
  expect_true(grepl("gamma_lpdf(mtail | 2, 0.25)", sc_default, fixed = TRUE))

  sc_shape <- prior_code(mf, beta_nb(),
                         brms::prior("gamma(3, 1)", class = "shape"))
  expect_true(grepl("gamma_lpdf(shape | 3, 1)", sc_shape, fixed = TRUE))
  expect_false(grepl("gamma_lpdf(shape | 2, 0.5)", sc_shape, fixed = TRUE))
  # the parameter the user left alone keeps its default
  expect_true(grepl("gamma_lpdf(mtail | 2, 0.25)", sc_shape, fixed = TRUE))

  sc_both <- prior_code(mf, beta_nb(),
                        c(brms::prior("gamma(3, 1)", class = "shape"),
                          brms::prior("gamma(5, 2)", class = "mtail")))
  expect_true(grepl("gamma_lpdf(shape | 3, 1)", sc_both, fixed = TRUE))
  expect_true(grepl("gamma_lpdf(mtail | 5, 2)", sc_both, fixed = TRUE))
})

test_that("com_binomial() default prior can be overridden", {
  set.seed(4)
  dat <- data.frame(
    y = rbinom(40, 10, 0.5), trials = 10L, time = 1:40,
    series = factor(rep("series1", 40))
  )
  sc <- prior_code(mvgam_formula(y | trials(trials) ~ 1), com_binomial(),
                   brms::prior("normal(2, 1)", class = "nu"), data = dat)
  expect_true(grepl("normal_lpdf(nu | 2, 1)", sc, fixed = TRUE))
  expect_false(grepl("normal_lpdf(nu | 1, 1)", sc, fixed = TRUE))

  # get_prior() must advertise the prior the model samples under.
  # brms's own fallback for `nu` is the Student-t degrees of
  # freedom `gamma(2, 0.1)`, which has positive support only and
  # would misreport a parameter declared with `lb = -5`.
  tab <- get_prior(mvgam_formula(y | trials(trials) ~ 1), data = dat,
                   family = com_binomial())
  expect_identical(tab$prior[tab$class == "nu"], "normal(1, 1)")
})

test_that("nu_trend default prior can be overridden", {
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = NA))
  sc_default <- prior_code(mf, poisson())
  expect_true(grepl("nu_trend ~ gamma(4, 0.3)", sc_default, fixed = TRUE))

  sc_user <- prior_code(mf, poisson(),
                        brms::prior("gamma(2, 0.1)", class = "nu_trend"))
  expect_true(grepl("nu_trend ~ gamma(2, 0.1)", sc_user, fixed = TRUE))
  expect_false(grepl("gamma(4, 0.3)", sc_user, fixed = TRUE))
})

test_that("beta_nb() distributional parameters are discoverable", {
  p <- get_prior(mvgam_formula(y ~ 1), data = prior_test_data(),
                 family = beta_nb())
  expect_true("shape" %in% p$class)
  expect_true("mtail" %in% p$class)
})


test_that("nu_trend is discoverable through get_prior()", {
  # Being able to override a prior is not much use if the class name
  # only appears in a help page. It must be listed alongside
  # sigma_trend and the autoregressive coefficients.
  d <- prior_test_data()
  p_est <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = NA)),
    data = d, family = poisson()
  )
  expect_true("nu_trend" %in% p_est$class)
  row <- p_est[p_est$class == "nu_trend", ]
  expect_identical(row$prior, "gamma(4, 0.3)")
  # the bound is required, not conventional: below 2 the innovations
  # have no finite variance
  expect_identical(row$lb, "2")
})

test_that("nu_trend is absent unless the degrees of freedom are estimated", {
  d <- prior_test_data()
  p_gauss <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1)),
    data = d, family = poisson()
  )
  expect_false("nu_trend" %in% p_gauss$class)

  # a fixed df has no parameter to place a prior on either
  p_fixed <- get_prior(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = 7)),
    data = d, family = poisson()
  )
  expect_false("nu_trend" %in% p_fixed$class)
})

test_that("nu_trend appears for every trend type that supports df", {
  d <- prior_test_data()
  for (tf in list(~ AR(p = 1, df = NA), ~ RW(df = NA), ~ ZMVN(df = NA))) {
    p <- get_prior(mvgam_formula(y ~ 1, trend_formula = tf),
                   data = d, family = poisson())
    expect_true("nu_trend" %in% p$class)
  }
})


# ---- what is reported must be what is sampled -----------------------
#
# A default written in more than one place drifts. `sigma_trend` was
# sampled under `exponential(2)` while the table reported brms's
# `student_t(3, 0, 2.5)`, and because `update()` passes the stored
# table back through `mvgam()`, every update re-specified the model.
# `ar1_trend` was sampled under a prior the table did not mention at
# all, and `L_Omega_trend` was emitted as a literal, so a user prior on
# it was discarded without a word.


test_that("every shared default names a distribution", {
  # `LV` sat here with a default for a parameter the generator never
  # emits, reachable only from a function with no caller.
  for (par in names(mvgam:::common_trend_priors)) {
    expect_true(nzchar(mvgam:::common_trend_priors[[par]]$default))
  }
})


# The trend a model is written with decides which parameters it
# samples, so a prior that goes missing goes missing for one trend
# type alone. Checking a single AR fit is what let `delta_trend` and
# the four VARMA hyperpriors sit blank in the reported table while the
# Stan sampled them under real priors. The parsing is the package's
# own `mvgam_stancode_prior_rows()`, the same reader the fitted object
# uses, rather than a regex written a second time for the test.
trend_formulas_by_type <- list(
  AR = list(obs = y ~ 1, trend = ~ AR(p = 1, cor = TRUE)),
  RW = list(obs = y ~ 1, trend = ~ RW()),
  VAR = list(obs = y ~ 1, trend = ~ VAR(p = 1, cor = TRUE)),
  VARMA = list(obs = y ~ 1, trend = ~ VAR(p = 1, ma = TRUE, cor = TRUE)),
  ZMVN = list(obs = y ~ 1, trend = ~ ZMVN()),
  CAR = list(obs = y ~ 1, trend = ~ CAR()),
  # PW's own offset competes with an observation intercept, which
  # warns; the no-intercept form is the identified one.
  PW = list(obs = y ~ -1, trend = ~ PW()),
  factor = list(obs = y ~ 1, trend = ~ AR(p = 1, n_lv = 2)),
  hierarchical = list(obs = y ~ 1,
                      trend = ~ AR(p = 1, gr = grp, cor = TRUE))
)

test_that("the trend priors reported are the ones sampled", {
  for (type in names(trend_formulas_by_type)) {
    spec <- trend_formulas_by_type[[type]]
    dat <- if (type == "hierarchical") {
      grouped_prior_test_data()
    } else {
      prior_test_data()
    }
    mf <- mvgam_formula(spec$obs, trend_formula = spec$trend)
    tab <- as.data.frame(get_prior(mf, data = dat, family = poisson()))
    sampled <- mvgam:::mvgam_stancode_prior_rows(
      prior_code(mf, poisson(), data = dat)
    )
    expect_gt(length(sampled), 0L)
    for (row in sampled) {
      reported <- tab$prior[tab$class == row$class]
      expect_true(length(reported) > 0L)
      expect_true(row$prior %in% reported)
    }
  }
})


test_that("a prior a trend argument sets is reported with that value", {
  # `delta_trend`'s scale is the user's own `changepoint_scale`, so it
  # is the one default the shared registry cannot hold.
  reported_delta <- function(scale) {
    mf <- mvgam_formula(y ~ -1,
                        trend_formula = ~ PW(changepoint_scale = scale))
    tab <- as.data.frame(get_prior(mf, data = prior_test_data(),
                                    family = poisson()))
    tab$prior[tab$class == "delta_trend"]
  }
  expect_identical(reported_delta(0.05), "double_exponential(0, 0.05)")
  expect_identical(reported_delta(0.4), "double_exponential(0, 0.4)")
})


test_that("a user prior reaches the parameters that reported nothing", {
  # Each of these was resolved from a literal at the emission site, so
  # the user's own value never entered the Stan.
  mf <- mvgam_formula(y ~ 1,
                      trend_formula = ~ VAR(p = 1, ma = TRUE, cor = TRUE))
  user <- brms::prior_string("normal(0, 3)", class = "Amu_trend") +
    brms::prior_string("gamma(5, 2)", class = "Domega_trend")
  code <- prior_code(mf, poisson(), pr = user)
  expect_true(grepl("Amu_trend[lag] ~ normal(0, 3)", code, fixed = TRUE))
  expect_true(grepl("Domega_trend[1, 1] ~ gamma(5, 2)", code, fixed = TRUE))

  pw <- mvgam_formula(y ~ -1, trend_formula = ~ PW())
  pw_user <- brms::prior_string("double_exponential(0, 1)",
                                class = "delta_trend")
  expect_true(grepl(
    "to_vector(delta_trend) ~ double_exponential(0, 1)",
    prior_code(pw, poisson(), pr = pw_user), fixed = TRUE
  ))
})


test_that("a user prior on the trend correlation reaches the model", {
  # Emitted as a literal, this one ignored the user entirely.
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, cor = TRUE))
  user <- brms::prior_string("lkj_corr_cholesky(9)", class = "L_Omega_trend")
  expect_true(grepl("L_Omega_trend ~ lkj_corr_cholesky(9)",
                     prior_code(mf, poisson(), pr = user), fixed = TRUE))
})


test_that("brms's trend-side sigma is not filed under sigma_trend", {
  # The trend submodel goes to brms as a gaussian, so brms hands back
  # a `sigma` row carrying its own student_t(3, 0, 2.5). That residual
  # scale is bookkeeping; the trend's process noise is `sigma_trend`,
  # which the Stan generator samples under `common_trend_priors` and
  # which reaches the stored table from the compiled code. Suffixing
  # brms's row filed its value under mvgam's name, so a fitted model
  # reported a prior it never sampled under and `update()` fed that
  # back, re-specifying the model.
  trend_priors <- brms::prior_string("student_t(3, 0, 2.5)",
                                      class = "sigma")
  out <- add_trend_suffix_to_priors(trend_priors)
  expect_equal(nrow(out), 0L)
  expect_false(any(grepl("student_t", out$prior)))

  # `suffix_trend_prior_classes()` is the one implementation of the
  # rule; both callers reach the same answer.
  expect_equal(nrow(suffix_trend_prior_classes(trend_priors)), 0L)
})


test_that("suffix_trend_prior_classes carries both guards", {
  # The rule was written twice and the copies drifted: one turned an
  # empty class into "_trend", the other suffixed an already-suffixed
  # class twice.
  empty_class <- brms::prior_string("normal(0, 1)", class = "b")
  empty_class$class <- ""
  expect_equal(suffix_trend_prior_classes(empty_class)$class, "")

  already <- brms::prior_string("exponential(2)", class = "sigma_trend")
  expect_equal(suffix_trend_prior_classes(already)$class, "sigma_trend")
})


test_that("every surface names a trend prior class the same way", {
  # `Z` is the Stan parameter's own name, so `Z_trend` names nothing.
  # The suffix rule was written twice and only one copy knew that,
  # so the model description rendered a parameter the program has no
  # equivalent of.
  cls <- c("sigma", "ar1_trend", "Z", "Psi", "theta_dist_phy", "")
  expect_equal(
    apply_trend_class_suffix(cls),
    c("sigma_trend", "ar1_trend", "Z", "Psi", "theta_dist_phy", "")
  )

  tp <- brms::prior(student_t(3, 0, 0.5), class = "Z") +
    brms::prior(normal(0, 1), class = "ar1")
  rendered <- merge_trend_priors(
    list(prior = NULL, trend_model = list(prior = tp))
  )
  # `setequal` rather than `sort`: collation puts "Z" before
  # "ar1_trend" in the C locale and after it elsewhere, and the
  # claim here is about membership, not order.
  expect_setequal(rendered$class, c("ar1_trend", "Z"))
})


test_that("a reported innovation scale is one the model samples", {
  # Two ways to lose `sigma_trend`, and the table has to know about
  # both. `PW()` never had one: its path is a function of the
  # changepoints. Multiplicative gamma process shrinkage derives it
  # as `sqrt(Psi_diag)` instead, so a prior set on it is refused.
  # Either way, offering the row invites a prior the fit cannot take.
  dat <- data.frame(
    y = rpois(40, 5), time = rep(1:20, 2),
    series = factor(rep(c("a", "b"), each = 20))
  )
  cases <- list(
    list(tf = ~ PW(), args = list(), sampled = FALSE),
    list(tf = ~ RW(), args = list(), sampled = TRUE),
    list(tf = ~ AR(p = 1), args = list(), sampled = TRUE),
    list(tf = ~ ZMVN(n_lv = 2), args = list(), sampled = TRUE),
    list(tf = ~ ZMVN(n_lv = 2),
         args = list(loadings_prior = list(column_shrinkage = "mgp")),
         sampled = FALSE)
  )
  for (case in cases) {
    mf <- mvgam_formula(y ~ -1, case$tf)
    common <- list(mf, data = dat, family = poisson())
    tab <- suppressWarnings(
      do.call(get_prior, c(common, case$args))
    )
    sc <- suppressWarnings(do.call(stancode, c(common, case$args)))
    expect_equal("sigma_trend" %in% tab$class, case$sampled)
    expect_equal(grepl("sigma_trend ~", sc), case$sampled)
  }
})


test_that("the registry decides whether a trend has an innovation scale", {
  pw <- PW()
  rw <- RW()
  expect_false(samples_innovation_scale(pw))
  expect_true(samples_innovation_scale(rw))

  # Attaching MGP shrinkage takes the scale away from a trend that
  # otherwise has one, and refreshes the cached parameter list that
  # was built before any loadings prior existed.
  spec <- list(column_shrinkage = "mgp")
  rw_mgp <- attach_loadings_spec_to_trend(rw, spec)
  expect_false(samples_innovation_scale(rw_mgp))
  expect_false("sigma_trend" %in% rw_mgp$monitor_params)
  expect_true("sigma_trend" %in% rw$monitor_params)
})


test_that("loadings traits answer once for every consumer", {
  # A kernel takes precedence in the `Z` branch, but column
  # shrinkage still has to reach the program through its own
  # hyperpriors, so the two questions stay separate.
  kernel_mgp <- list(N_features_trend = 3L, column_shrinkage = "mgp")
  traits <- loadings_spec_traits(kernel_mgp)
  expect_true(traits$kernel)
  expect_true(traits$mgp)
  expect_equal(loadings_z_branch(kernel_mgp), "kernel")

  # A spec missing the count fields answers FALSE rather than
  # raising on a zero-length comparison.
  bare <- loadings_spec_traits(list(column_shrinkage = "iid"))
  expect_false(bare$kernel)
  expect_false(bare$mgp)
  expect_equal(loadings_z_branch(NULL), "unstructured")
})


test_that("add_trend_suffix_to_priors leaves other classes alone", {
  trend_priors <- rbind(
    brms::prior_string("normal(0, 1)", class = "b"),
    brms::prior_string("student_t(3, 0, 2.5)", class = "sds")
  )
  out <- add_trend_suffix_to_priors(trend_priors)
  expect_equal(sort(out$class), c("b_trend", "sds_trend"))
  expect_equal(sort(out$prior),
               sort(c("normal(0, 1)", "student_t(3, 0, 2.5)")))
  # A row already suffixed is not suffixed twice.
  already <- brms::prior_string("exponential(2)", class = "sigma_trend")
  expect_equal(add_trend_suffix_to_priors(already)$class, "sigma_trend")
})


test_that("a coef-scoped sigma row is not mistaken for the bookkeeping one", {
  # Only the bare `sigma` row is brms's residual scale. A row scoped
  # to a coefficient belongs to a distributional sub-formula and keeps
  # whatever the user or brms set.
  scoped <- brms::prior_string("normal(0, 3)", class = "sigma",
                                coef = "x")
  out <- add_trend_suffix_to_priors(scoped)
  expect_equal(out$prior, "normal(0, 3)")
  expect_equal(out$class, "sigma_trend")
})


test_that("a class mvgam manages is routed to the trend side", {
  # Splitting a prior table on the `_trend` suffix alone files `Z`
  # as observation-side, so brms is handed a class it has no
  # parameter for and refuses it, while `default_prior()` is where
  # the user read the class name. One predicate decides.
  cls <- c("b", "Intercept", "sigma", "sigma_trend", "ar1_trend",
           "Z", "Z_free_vec", "varrho_inv", "theta_features",
           "theta_dist_phylo")
  managed <- mvgam:::is_mvgam_managed_class(cls)
  expect_equal(cls[managed],
               c("sigma_trend", "ar1_trend", "Z", "Z_free_vec",
                 "varrho_inv", "theta_features", "theta_dist_phylo"))
  expect_equal(cls[!managed], c("b", "Intercept", "sigma"))
})


test_that("a user prior on Z reaches the emitted Stan", {
  # `default_prior()` advertises the class, so setting it has to
  # work. It previously failed with brms' "do not correspond to any
  # model parameter", naming the class its own table supplied.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 4L,
                  n_timepoints = 20L)$data_train
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2))
  expect_true(any(get_prior(mf, data = d, family = poisson())$class == "Z"))
  sc <- stancode(mf, data = d, family = poisson(),
                 prior = prior(normal(0, 3), class = Z))
  ln <- strsplit(paste(as.character(sc), collapse = "\n"), "\n")[[1]]
  expect_true(any(grepl("to_vector(Z) ~ normal(0, 3);", ln, fixed = TRUE)))
})


test_that("the suffix rule leaves deliberately unsuffixed classes alone", {
  # `Z` names the Stan parameter directly; `Z_trend` names nothing.
  p <- brms::prior(normal(0, 1), class = "Z")
  p <- rbind(p, brms::prior(exponential(2), class = "sigma"))
  out <- mvgam:::suffix_trend_prior_classes(p)
  expect_true("Z" %in% out$class)
  expect_false("Z_trend" %in% out$class)
})


test_that("the reported Z prior is the one the model samples", {
  # `Z` is drawn from one of three branches depending on the
  # loadings prior. The table reported the unstructured default for
  # all three, so two of them described a model the user was not
  # fitting. Both surfaces now read `loadings_z_branch()`.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 4L,
                  n_timepoints = 20L)$data_train
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2))
  traits <- data.frame(sp = factor(paste0("series_", 1:4)),
                        t1 = rnorm(4))
  agrees <- function(...) {
    p <- get_prior(mf, data = d, family = poisson(), ...)
    reported <- as.character(p$prior[p$class == "Z"])[1]
    sc <- gsub("[[:space:]]+", " ", paste(
      as.character(stancode(mf, data = d, family = poisson(), ...)),
      collapse = " "
    ))
    grepl(gsub("[[:space:]]+", " ", reported), sc, fixed = TRUE)
  }
  expect_true(agrees())
  expect_true(agrees(loadings_prior = list(column_shrinkage = "mgp")))
  expect_true(agrees(loadings_prior = list(features = traits)))
  expect_true(agrees(loadings_prior = list(features = traits,
                                            column_shrinkage = "mgp")))
})


test_that("the branch classifier names each loadings path once", {
  expect_equal(mvgam:::loadings_z_branch(NULL), "unstructured")
  expect_equal(
    mvgam:::loadings_z_branch(list(column_shrinkage = "mgp")), "mgp"
  )
  expect_equal(
    mvgam:::loadings_z_branch(list(N_features_trend = 2L)), "kernel"
  )
  # A kernel takes precedence: the emitted statement is the
  # multivariate normal either way.
  expect_equal(
    mvgam:::loadings_z_branch(
      list(N_features_trend = 2L, column_shrinkage = "mgp")
    ),
    "kernel"
  )
  expect_null(mvgam:::loadings_z_prior_string("unstructured"))
})


test_that("get_prior refuses a trend_map it cannot describe", {
  # A fully fixed `trend_map` moves `Z` to the data block and a
  # partial one replaces it with `Z_free_vec` under its own prior,
  # so the free-loadings table describes neither. Refusing beats
  # returning a table for a model the user is not fitting.
  set.seed(1L)
  d <- sim_mvgam(family = poisson(), n_series = 4L,
                  n_timepoints = 20L)$data_train
  mf <- mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, n_lv = 2))
  tm <- data.frame(series = factor(paste0("series_", 1:4)),
                    trend = c(1L, 1L, 2L, 2L))
  expect_error(
    get_prior(mf, data = d, family = poisson(), trend_map = tm),
    "cannot describe a fit that supplies 'trend_map'"
  )
  # The free-loadings table is unaffected.
  expect_gt(nrow(get_prior(mf, data = d, family = poisson())), 0L)
})
