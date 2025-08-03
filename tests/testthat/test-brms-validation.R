context("Tests for brms formula validation")

# Test observation formula validation
test_that("validate_obs_formula_brms accepts brms-compatible formulas", {
  # Standard formula
  expect_identical(
    mvgam:::validate_obs_formula_brms(y ~ x + s(time)),
    y ~ x + s(time)
  )

  # NULL formula
  expect_null(mvgam:::validate_obs_formula_brms(NULL))
})

test_that("validate_obs_formula_brms rejects mvgam trend constructors", {
  expect_error(
    mvgam:::validate_obs_formula_brms(y ~ x + RW()),
    "mvgam trend constructors found in observation"
  )

  expect_error(
    mvgam:::validate_obs_formula_brms(y ~ x + AR(p = 1)),
    "Found: AR\\(\\)"
  )

  expect_error(
    mvgam:::validate_obs_formula_brms(y ~ VAR() + x),
    "Found: VAR\\(\\)"
  )
})

test_that("validate_obs_formula_brms handles complex brms formulas", {
  # Should pass - no trend constructors, but should warn about brms autocor
  expect_warning(mvgam:::validate_obs_formula_brms(y ~ x + ar(p = 1)), "Using brms autocorrelation")
  expect_warning(mvgam:::validate_obs_formula_brms(y ~ x + ma(q = 1)), "Using brms autocorrelation")

  # Should pass silently - no autocorrelation
  expect_silent(mvgam:::validate_obs_formula_brms(y ~ x + s(time)))
})

# Test trend formula validation
test_that("validate_trend_formula_brms accepts valid trend formulas", {
  # Single formula
  expect_identical(
    mvgam:::validate_trend_formula_brms(~ RW()),
    ~ RW()
  )

  # NULL formula
  expect_null(mvgam:::validate_trend_formula_brms(NULL))

  # Named list
  trend_list <- list(count = ~ AR(p = 1), biomass = ~ RW(), presence = NULL)
  expect_identical(
    mvgam:::validate_trend_formula_brms(trend_list),
    trend_list
  )
})

test_that("validate_trend_formula_brms accepts empty trend formulas", {
  # Empty formula with intercept only
  expect_identical(
    mvgam:::validate_trend_formula_brms(~ 1),
    ~ 1
  )

  # Empty formula with no intercept
  expect_identical(
    mvgam:::validate_trend_formula_brms(~ -1),
    ~ -1
  )

  # Empty formula alternative notation
  expect_identical(
    mvgam:::validate_trend_formula_brms(~ 0),
    ~ 0
  )

  # Named list with mix of empty and non-empty formulas
  mixed_list <- list(
    count = ~ AR(p = 1),     # Has trend
    biomass = ~ 1,           # Intercept only (static)
    presence = ~ -1,         # No intercept (no trend)
    abundance = NULL         # NULL (no trend)
  )
  expect_identical(
    mvgam:::validate_trend_formula_brms(mixed_list),
    mixed_list
  )
})

test_that("validate_trend_formula_brms handles response variables correctly", {
  # Single formulas should not have response variables
  expect_error(
    mvgam:::validate_trend_formula_brms(y ~ RW()),
    "should not have a response variable"
  )

  # bf() objects SHOULD allow response variables for multivariate identification
  expect_silent(mvgam:::validate_trend_formula_brms(
    bf(count ~ AR(p = 1), biomass ~ RW(cor = TRUE))
  ))

  # Mixed bf() with and without response variables
  expect_silent(mvgam:::validate_trend_formula_brms(
    bf(count ~ AR(p = 1), biomass ~ RW())
  ))
})

test_that("validate_trend_formula_brms rejects 'series' identifier", {
  expect_error(
    mvgam:::validate_trend_formula_brms(~ s(time, by = series)),
    "should use.*trend.*instead of.*series"
  )
})

test_that("validate_trend_formula_brms rejects brms autocorrelation", {
  expect_error(
    mvgam:::validate_trend_formula_brms(~ ar(p = 1)),
    "brms autocorrelation terms not allowed"
  )

  expect_error(
    mvgam:::validate_trend_formula_brms(~ ma(q = 1) + x),
    "Found: ma\\(\\)"
  )
})

test_that("validate_trend_formula_brms handles named lists", {
  # Valid named list
  trend_list <- list(
    count = ~ AR(p = 1),
    biomass = ~ RW(cor = TRUE),
    presence = NULL
  )
  expect_silent(mvgam:::validate_trend_formula_brms(trend_list))

  # Invalid - unnamed list
  expect_error(
    mvgam:::validate_trend_formula_brms(list(~ AR(), ~ RW())),
    "must be a named list"
  )
})

# Test autocorrelation separation
test_that("validate_autocor_separation works correctly", {
  obs_formula <- y ~ x + s(time)
  trend_formula <- ~ AR(p = 1)

  result <- mvgam:::validate_autocor_separation(obs_formula, trend_formula)

  expect_identical(result$obs_formula, obs_formula)
  expect_identical(result$trend_formula, trend_formula)
})

test_that("validate_autocor_separation handles empty trend formulas", {
  # Empty trend formulas should pass
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = y ~ x + s(time),
      trend_formula = ~ 1  # Intercept only
    )
  })

  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = y ~ x + s(time),
      trend_formula = ~ -1  # No intercept
    )
  })

  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = y ~ x + s(time),
      trend_formula = NULL  # No trend formula
    )
  })

  # Multivariate with mix of empty and non-empty trends
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = cbind(y1, y2, y3) ~ x + s(time),
      trend_formula = list(
        y1 = ~ AR(p = 1),     # Dynamic trend
        y2 = ~ 1,             # Static (intercept only)
        y3 = NULL             # No trend
      )
    )
  })
})

test_that("validate_autocor_separation catches conflicts", {
  expect_error(
    mvgam:::validate_autocor_separation(y ~ x + RW(), ~ AR()),
    "mvgam trend constructors found in observation"
  )

  expect_error(
    mvgam:::validate_autocor_separation(y ~ x, ~ ar(p = 1)),
    "brms autocorrelation terms not allowed"
  )
})

# Test utility functions
test_that("formula2str_mvgam extracts strings correctly", {
  # Basic formula
  expect_equal(
    mvgam:::formula2str_mvgam(y ~ x + s(time)),
    "y ~ x + s(time)"
  )

  # NULL handling
  expect_null(mvgam:::formula2str_mvgam(NULL))

  # Empty formulas
  expect_equal(
    mvgam:::formula2str_mvgam(~ 1),
    "~1"
  )

  expect_equal(
    mvgam:::formula2str_mvgam(~ -1),
    "~-1"
  )

  # Whitespace handling
  formula_with_spaces <- as.formula("y ~    x   +   s(time)")
  result <- mvgam:::formula2str_mvgam(formula_with_spaces)
  expect_false(grepl("  ", result))  # Should not have multiple spaces
})

# Integration tests mimicking real usage patterns
test_that("validation supports expected mvgam-brms usage patterns", {
  # Standard univariate model
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = y ~ x + s(time),
      trend_formula = ~ RW()
    )
  })

  # Multivariate with named list trends
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = cbind(y1, y2) ~ x + s(time),
      trend_formula = list(
        y1 = ~ AR(p = 1),
        y2 = ~ RW(cor = TRUE)
      )
    )
  })

  # brms autocorrelation in observation (should warn but pass)
  expect_warning({
    mvgam:::validate_autocor_separation(
      obs_formula = y ~ x + ar(p = 1),
      trend_formula = ~ RW()
    )
  }, "Using brms autocorrelation in observation formula")

  # Mixed dynamic and static trends
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = cbind(count, biomass, presence) ~ temp + precip,
      trend_formula = list(
        count = ~ AR(p = 1),        # Population dynamics
        biomass = ~ RW(cor = TRUE), # Correlated biomass trends
        presence = ~ 1              # Static occupancy
      )
    )
  })
})

test_that("validation handles edge cases gracefully", {
  # Empty trend formula variations
  expect_silent(mvgam:::validate_trend_formula_brms(~ 1))
  expect_silent(mvgam:::validate_trend_formula_brms(~ -1))
  expect_silent(mvgam:::validate_trend_formula_brms(~ 0))

  # Complex observation formulas
  expect_silent(mvgam:::validate_obs_formula_brms(
    cbind(success, trials) ~ x + s(time) + (1|group)
  ))

  # Mixed valid/invalid in multivariate
  expect_error(
    mvgam:::validate_trend_formula_brms(list(
      y1 = ~ AR(p = 1),      # Valid
      y2 = ~ ar(p = 1)       # Invalid - brms autocor
    )),
    "brms autocorrelation terms not allowed"
  )

  # Empty formulas in named lists should not cause errors
  expect_silent(mvgam:::validate_trend_formula_brms(list(
    y1 = ~ AR(p = 1),
    y2 = ~ 1,          # Static
    y3 = ~ -1,         # No intercept
    y4 = NULL          # No trend
  )))
})

# Test advanced brms formula patterns
test_that("validate_obs_formula_brms handles distributional regression formulas", {
  # Basic distributional regression with bf()
  # Should pass - no trend constructors in any parameter
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(y ~ x + s(time), sigma ~ group)
  ))

  # Complex distributional regression
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(count ~ temp + s(season),
       sigma ~ habitat,
       nu ~ elevation)
  ))

  # Distributional regression with autocorrelation (should warn)
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(y ~ x + ar(p = 1), sigma ~ group)
  ), "Using brms autocorrelation")

  # Should reject trend constructors in main parameter
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(y ~ x + RW(), sigma ~ group)
    ),
    "mvgam trend constructors found in observation"
  )

  # Should reject trend constructors in distributional parameter
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(y ~ x, sigma ~ group + AR(p = 1))
    ),
    "mvgam trend constructors found in observation"
  )

  # Multiple distributional parameters with trend constructors
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(y ~ x, sigma ~ RW(), nu ~ VAR())
    ),
    "Found: RW\\(\\), VAR\\(\\)"
  )
})

test_that("validate_obs_formula_brms handles nonlinear formulas", {
  # Basic nonlinear formula with nl=TRUE
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE)
  ))

  # Nonlinear with multiple parameters
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(height ~ a * age / (age + b),
       a ~ treatment,
       b ~ 1,
       nl = TRUE)
  ))

  # Nonlinear with distributional parameters
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(y ~ a * exp(b * x), a + b ~ 1, sigma ~ group, nl = TRUE)
  ))

  # Should reject trend constructors in nonlinear formula
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(y ~ a * exp(b * x), a ~ RW(), b ~ 1, nl = TRUE)
    ),
    "mvgam trend constructors found in observation"
  )

  # Should reject trend constructors in nonlinear main formula
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(y ~ AR(p = 1) * exp(b * x), b ~ 1, nl = TRUE)
    ),
    "mvgam trend constructors found in observation"
  )
})

test_that("validate_obs_formula_brms handles complex mixed formulas", {
  # Multivariate distributional regression
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(mvbind(y1, y2) ~ x + s(time)) +
    bf(sigma1 ~ group) +
    bf(sigma2 ~ habitat) +
    set_rescor(TRUE)
  ))

  # Mixed multivariate with autocorrelation (should warn)
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(mvbind(count, biomass) ~ temp + ar(p = 1)) +
    bf(sigma1 ~ habitat) +
    set_rescor(FALSE)
  ), "Using brms autocorrelation")

  # Should reject trend constructors in multivariate distributional
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(mvbind(y1, y2) ~ x + RW()) +
      bf(sigma1 ~ group)
    ),
    "mvgam trend constructors found in observation"
  )

  # Complex case: nonlinear with distributional parameters
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(biomass ~ a * temp^b, a ~ species, b ~ 1, sigma ~ habitat, nl = TRUE)
  ))

  # Should reject trend constructors in complex nonlinear distributional
  expect_error(
    mvgam:::validate_obs_formula_brms(
      bf(biomass ~ a * temp^b, a ~ species + VAR(), b ~ 1, sigma ~ habitat, nl = TRUE)
    ),
    "mvgam trend constructors found in observation"
  )
})

test_that("validation preserves brms formula structure for complex cases", {
  # Distributional regression formula should pass through unchanged
  distreg_formula <- bf(y ~ x + s(time), sigma ~ group, nu ~ elevation)
  expect_identical(
    mvgam:::validate_obs_formula_brms(distreg_formula),
    distreg_formula
  )

  # Nonlinear formula should pass through unchanged
  nonlinear_formula <- bf(y ~ a * exp(b * x), a + b ~ 1, nl = TRUE)
  expect_identical(
    mvgam:::validate_obs_formula_brms(nonlinear_formula),
    nonlinear_formula
  )

  # Complex multivariate should pass through unchanged
  complex_formula <- bf(mvbind(y1, y2) ~ x + s(time)) +
                     bf(sigma1 ~ group) +
                     set_rescor(TRUE)
  expect_identical(
    mvgam:::validate_obs_formula_brms(complex_formula),
    complex_formula
  )
})

test_that("validation handles edge cases in advanced formulas", {
  # Empty distributional parameters
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(y ~ x, sigma ~ 1, nu ~ -1)
  ))

  # Nonlinear with simple parameters
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(y ~ a + b * x, a ~ 1, b ~ 1, nl = TRUE)
  ))

  # Mixed autocorrelation contexts
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(y ~ x + ar(p = 1), sigma ~ group + ma(q = 1))
  ), "Using brms autocorrelation")

  # Should handle NULL components gracefully
  expect_silent(mvgam:::validate_obs_formula_brms(
    bf(y ~ x + s(time))  # Simple bf() without additional parameters
  ))
})

# Test multivariate trend formula patterns with response variables
test_that("validate_trend_formula_brms supports multivariate response identification", {
  # bf() with response variables for multivariate identification
  expect_silent(mvgam:::validate_trend_formula_brms(
    bf(count ~ AR(p = 1), biomass ~ RW(cor = TRUE))
  ))

  # Complex multivariate trends with different patterns
  expect_silent(mvgam:::validate_trend_formula_brms(
    bf(count ~ AR(p = c(1, 12)),
       biomass ~ RW(),
       presence ~ GP())
  ))

  # Mixed static and dynamic trends in multivariate
  expect_silent(mvgam:::validate_trend_formula_brms(
    bf(count ~ AR(p = 1),     # Dynamic trend
       biomass ~ 1,           # Static (intercept only)
       presence ~ -1)         # No trend
  ))

  # Should still reject brms autocorrelation in bf() trends
  expect_error(
    mvgam:::validate_trend_formula_brms(
      bf(count ~ AR(p = 1), biomass ~ ar(p = 1))
    ),
    "brms autocorrelation terms not allowed"
  )

  # Should still reject 'series' identifier in bf() trends
  expect_error(
    mvgam:::validate_trend_formula_brms(
      bf(count ~ AR(p = 1), biomass ~ s(time, by = series))
    ),
    "should use.*trend.*instead of.*series"
  )
})

test_that("validation handles mixed multivariate trend specifications", {
  # Integration test: observation + multivariate trend formulas
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = bf(mvbind(count, biomass) ~ temp + precip),
      trend_formula = bf(count ~ AR(p = 1), biomass ~ RW(cor = TRUE))
    )
  })

  # Named list vs bf() consistency check
  named_list_trends <- list(
    count = ~ AR(p = 1),
    biomass = ~ RW(cor = TRUE)
  )

  expect_silent(mvgam:::validate_trend_formula_brms(named_list_trends))

  # Both approaches should work for multivariate
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = cbind(count, biomass) ~ temp,
      trend_formula = named_list_trends
    )
  })

  # Complex multivariate: distributional observation + trend identification
  expect_silent({
    mvgam:::validate_autocor_separation(
      obs_formula = bf(mvbind(count, biomass) ~ temp,
                      sigma1 ~ habitat,
                      sigma2 ~ elevation),
      trend_formula = bf(count ~ AR(p = 1),
                        biomass ~ RW(cor = TRUE))
    )
  })
})

test_that("multivariate trend validation provides helpful error messages", {
  # Clear guidance when single formulas have response variables
  expect_error(
    mvgam:::validate_trend_formula_brms(count ~ AR(p = 1)),
    "For multivariate models, use.*bf\\(y1.*AR\\(\\), y2.*RW\\(\\)\\)"
  )

  # Should catch trend constructors in multivariate responses
  expect_error(
    mvgam:::validate_autocor_separation(
      obs_formula = bf(mvbind(count, biomass) ~ temp + RW()),
      trend_formula = bf(count ~ AR(p = 1), biomass ~ RW())
    ),
    "mvgam trend constructors found in observation"
  )
})

# Test offset handling
test_that("validate_obs_formula_brms handles offsets appropriately", {
  # Offsets should be allowed in observation formulas (with informational warning)
  expect_warning(mvgam:::validate_obs_formula_brms(
    y ~ x + s(time) + offset(log_exposure)
  ), "Offset terms detected in observation formula")

  # Multiple offsets should be handled
  expect_warning(mvgam:::validate_obs_formula_brms(
    count ~ temp + offset(log_area) + s(season) + offset(effort)
  ), "Offset terms detected")

  # Offsets in distributional regression should work
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(y ~ x + offset(log_exposure), sigma ~ group + offset(baseline))
  ), "Offset terms detected")

  # Offsets in multivariate models
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(mvbind(count, biomass) ~ temp + offset(log_area))
  ), "Offset terms detected")

  # Should still reject trend constructors even with offsets
  expect_error(
    mvgam:::validate_obs_formula_brms(
      y ~ x + offset(log_exposure) + RW()
    ),
    "mvgam trend constructors found in observation"
  )
})

test_that("validate_trend_formula_brms rejects offsets", {
  # Single trend formula with offset should be rejected
  expect_error(
    mvgam:::validate_trend_formula_brms(~ RW() + offset(baseline)),
    "Offset terms not allowed in.*trend_formula"
  )

  # Named list with offsets should be rejected
  expect_error(
    mvgam:::validate_trend_formula_brms(list(
      count = ~ AR(p = 1) + offset(log_baseline),
      biomass = ~ RW()
    )),
    "Offset terms not allowed"
  )

  # bf() trend formulas with offsets should be rejected
  expect_error(
    mvgam:::validate_trend_formula_brms(
      bf(count ~ AR(p = 1) + offset(baseline), biomass ~ RW())
    ),
    "Offset terms not allowed"
  )

  # Should provide helpful error message with alternative
  expect_error(
    mvgam:::validate_trend_formula_brms(~ RW() + offset(z)),
    "Include offsets in the main observation.*formula.*instead"
  )
})

test_that("offset validation handles complex formula patterns", {
  # Nonlinear formulas with offsets in observation (should warn but pass)
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(y ~ a * exp(b * x) + offset(baseline),
       a + b ~ 1,
       nl = TRUE)
  ), "Offset terms detected")

  # Complex multivariate distributional with offsets
  expect_warning(mvgam:::validate_obs_formula_brms(
    bf(mvbind(count, biomass) ~ temp + offset(log_area)) +
    bf(sigma1 ~ habitat + offset(baseline1)) +
    bf(sigma2 ~ elevation) +
    set_rescor(TRUE)
  ), "Offset terms detected")

  # Integration test: offsets in obs + trends without offsets
  expect_warning({
    result <- mvgam:::validate_autocor_separation(
      obs_formula = y ~ x + s(time) + offset(log_exposure),
      trend_formula = ~ AR(p = 1)
    )
  }, "Offset terms detected")

  # Should work without issues
  expect_identical(result$trend_formula, ~ AR(p = 1))
})

test_that("offset validation edge cases", {
  # No offsets should pass silently
  expect_silent(mvgam:::validate_obs_formula_brms(y ~ x + s(time)))
  expect_silent(mvgam:::validate_trend_formula_brms(~ RW()))

  # Empty formulas should not trigger offset errors
  expect_silent(mvgam:::validate_trend_formula_brms(~ 1))
  expect_silent(mvgam:::validate_trend_formula_brms(~ -1))

  # NULL formulas should not trigger offset errors
  expect_null(mvgam:::validate_trend_formula_brms(NULL))

  # Named list with NULL components should work
  expect_silent(mvgam:::validate_trend_formula_brms(list(
    count = ~ AR(p = 1),
    biomass = NULL
  )))

  # Mixed offset patterns in multivariate observation
  expect_warning(mvgam:::validate_obs_formula_brms(
    cbind(count, trials) ~ x + offset(log_exposure) + s(time) + (1|group)
  ), "Offset terms detected")
})
