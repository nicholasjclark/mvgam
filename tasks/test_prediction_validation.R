# Test Prediction Validation Scenarios
#
# Explores what validation is needed for newdata in prediction functions.
# Key insight: ensure_mvgam_variables() already supports metadata parameter
# for prediction context. brms standata() handles most validation.
#
# This script tests:
# 1. What metadata is stored in mvgam objects
# 2. Various prediction scenarios (good/bad newdata)
# 3. Error messages from existing validation

devtools::load_all()
library(brms)
library(posterior)

cat("=== PREDICTION VALIDATION TEST SCRIPT ===\n\n")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Safe test wrapper
run_test <- function(name, expr) {
  cat("\n--- Test:", name, "---\n")
  result <- tryCatch(
    {
      res <- expr
      cat("  Result: SUCCESS\n")
      if (!is.null(res) && is.matrix(res)) {
        cat("  Output dims:", dim(res), "\n")
      }
      list(passed = TRUE, result = res)
    },
    error = function(e) {
      cat("  Result: ERROR\n")
      cat("  Message:", conditionMessage(e), "\n")
      list(passed = FALSE, error = conditionMessage(e))
    },
    warning = function(w) {
      cat("  Warning:", conditionMessage(w), "\n")
      list(passed = TRUE, warning = conditionMessage(w))
    }
  )
  return(result)
}

# =============================================================================
# LOAD FIXTURES
# =============================================================================

cat("Loading validation fixtures...\n")
FIXTURE_DIR <- "tasks/fixtures"

# Check what fixtures exist
fixture_files <- list.files(FIXTURE_DIR, pattern = "^val_.*\\.rds$", full.names = TRUE)
cat("Found", length(fixture_files), "validation fixtures:\n")
for (f in fixture_files) cat("  -", basename(f), "\n")

# Load a few key models for testing
models <- list()

# Simple AR model (univariate)
if (file.exists(file.path(FIXTURE_DIR, "val_mvgam_ar1_int.rds"))) {
  models$ar1_int <- readRDS(file.path(FIXTURE_DIR, "val_mvgam_ar1_int.rds"))
  cat("\nLoaded: ar1_int (univariate intercept-only AR)\n")
}

# AR with fixed effect
if (file.exists(file.path(FIXTURE_DIR, "val_mvgam_ar1_fx.rds"))) {
  models$ar1_fx <- readRDS(file.path(FIXTURE_DIR, "val_mvgam_ar1_fx.rds"))
  cat("Loaded: ar1_fx (AR with fixed effect x)\n")
}

# AR with random effects
if (file.exists(file.path(FIXTURE_DIR, "val_mvgam_ar1_re.rds"))) {
  models$ar1_re <- readRDS(file.path(FIXTURE_DIR, "val_mvgam_ar1_re.rds"))
  cat("Loaded: ar1_re (AR with random intercept by group)\n")
}

# AR with smooth
if (file.exists(file.path(FIXTURE_DIR, "val_mvgam_ar1_re_smooth.rds"))) {
  models$ar1_smooth <- readRDS(file.path(FIXTURE_DIR, "val_mvgam_ar1_re_smooth.rds"))
  cat("Loaded: ar1_smooth (AR with smooth + random effects)\n")
}

if (length(models) == 0) {
  stop("No fixtures found. Run tasks/validate_extraction_vs_brms.R first to generate fixtures.")
}

# =============================================================================
# INSPECT MVGAM OBJECT STRUCTURE
# =============================================================================

cat("\n\n=== INSPECTING MVGAM OBJECT STRUCTURE ===\n")

model <- models[[1]]
cat("\nModel class:", class(model), "\n")
cat("Top-level slots:", names(model), "\n")

# Check for trend metadata
cat("\n--- Trend Metadata ---\n")
if ("trend_metadata" %in% names(model)) {
  cat("trend_metadata found:\n")
  str(model$trend_metadata, max.level = 2)
} else {
  cat("No trend_metadata slot\n")
}

# Check trend_model structure
cat("\n--- Trend Model Structure ---\n")
if ("trend_model" %in% names(model)) {
  cat("trend_model class:", class(model$trend_model), "\n")
  if (inherits(model$trend_model, "brmsfit")) {
    cat("  Is brmsfit: YES\n")
    cat("  Formula:", deparse(model$trend_model$formula$formula), "\n")
  }
} else {
  cat("No trend_model slot\n")
}

# Check obs_model structure
cat("\n--- Observation Model Structure ---\n")
if ("obs_model" %in% names(model)) {
  cat("obs_model class:", class(model$obs_model), "\n")
  if (inherits(model$obs_model, "brmsfit")) {
    cat("  Is brmsfit: YES\n")
    cat("  Formula:", deparse(model$obs_model$formula$formula), "\n")
  }
} else {
  cat("No obs_model slot\n")
}

# Check for stored data
cat("\n--- Stored Data ---\n")
if ("data" %in% names(model)) {
  cat("Original data stored: YES\n")
  cat("  Dimensions:", dim(model$data), "\n")
  cat("  Columns:", paste(names(model$data), collapse = ", "), "\n")
} else {
  cat("No data slot (checking obs_model$data)\n")
  if (!is.null(model$obs_model$data)) {
    cat("  obs_model$data dims:", dim(model$obs_model$data), "\n")
    cat("  Columns:", paste(names(model$obs_model$data), collapse = ", "), "\n")
  }
}

# =============================================================================
# RECREATE ORIGINAL TRAINING DATA
# =============================================================================

cat("\n\n=== RECREATING TRAINING DATA ===\n")

set.seed(42)
n_time <- 30

# Generate AR(1) latent process
ar_coef <- 0.7
sigma <- 0.5
latent <- numeric(n_time)
latent[1] <- rnorm(1, 0, sigma / sqrt(1 - ar_coef^2))
for (t in 2:n_time) {
  latent[t] <- ar_coef * latent[t - 1] + rnorm(1, 0, sigma)
}

# Create covariate with nonlinear effect
z <- seq(-2, 2, length.out = n_time)
z_effect <- 0.5 * sin(z * pi)

# Create base data with grouping
train_data <- data.frame(
  y = rpois(n_time, exp(2 + latent + z_effect)),
  x = rnorm(n_time),
  z = z,
  time = 1:n_time,
  series = factor("s1"),
  group = factor(rep(letters[1:6], each = 5))
)

cat("Training data created: n =", nrow(train_data), "\n")
cat("Columns:", paste(names(train_data), collapse = ", "), "\n")

# =============================================================================
# TEST 1: VALID PREDICTION DATA
# =============================================================================

cat("\n\n=== TEST SUITE 1: VALID PREDICTION DATA ===\n")

# Test 1.1: Exact same data (in-sample)
test_1_1 <- run_test("In-sample prediction (same data)", {
  extract_component_linpred(models$ar1_fx, train_data, component = "obs")
})

# Test 1.2: Subset of data
test_1_2 <- run_test("Subset prediction (first 10 rows)", {
  newdata <- train_data[1:10, ]
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 1.3: New time points (future)
test_1_3 <- run_test("Future prediction (new time points)", {
  newdata <- data.frame(
    y = NA,  # Unknown response
    x = rnorm(5),
    z = seq(2.1, 3, length.out = 5),
    time = 31:35,  # Future times
    series = factor("s1", levels = "s1"),
    group = factor("a", levels = letters[1:6])  # Existing group level
  )
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# =============================================================================
# TEST 2: MISSING VARIABLES
# =============================================================================

cat("\n\n=== TEST SUITE 2: MISSING VARIABLES ===\n")

# Test 2.1: Missing covariate (x)
test_2_1 <- run_test("Missing covariate (x)", {
  newdata <- train_data[1:5, c("y", "z", "time", "series", "group")]
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 2.2: Missing time variable
test_2_2 <- run_test("Missing time variable", {
  newdata <- train_data[1:5, c("y", "x", "z", "series", "group")]
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 2.3: Missing series variable
test_2_3 <- run_test("Missing series variable", {
  newdata <- train_data[1:5, c("y", "x", "z", "time", "group")]
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# =============================================================================
# TEST 3: FACTOR LEVEL ISSUES
# =============================================================================

cat("\n\n=== TEST SUITE 3: FACTOR LEVEL ISSUES ===\n")

# Test 3.1: New group level (should fail or warn)
if (!is.null(models$ar1_re)) {
  test_3_1 <- run_test("New group level (unseen 'z')", {
    newdata <- train_data[1:5, ]
    newdata$group <- factor("z", levels = c(letters[1:6], "z"))  # New level
    extract_component_linpred(models$ar1_re, newdata, component = "obs")
  })

  # Test 3.2: New group level with allow_new_levels
  test_3_2 <- run_test("New group level with allow_new_levels=TRUE", {
    newdata <- train_data[1:5, ]
    newdata$group <- factor("z", levels = c(letters[1:6], "z"))
    extract_component_linpred(
      models$ar1_re, newdata, component = "obs",
      allow_new_levels = TRUE
    )
  })
}

# Test 3.3: Series level mismatch
test_3_3 <- run_test("Wrong series level", {
  newdata <- train_data[1:5, ]
  newdata$series <- factor("s2", levels = "s2")  # Different series
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# =============================================================================
# TEST 4: DATA TYPE ISSUES
# =============================================================================

cat("\n\n=== TEST SUITE 4: DATA TYPE ISSUES ===\n")

# Test 4.1: Non-factor series
test_4_1 <- run_test("Character series (not factor)", {
  newdata <- train_data[1:5, ]
  newdata$series <- "s1"  # Character, not factor
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 4.2: Non-factor group
if (!is.null(models$ar1_re)) {
  test_4_2 <- run_test("Character group (not factor)", {
    newdata <- train_data[1:5, ]
    newdata$group <- "a"  # Character, not factor
    extract_component_linpred(models$ar1_re, newdata, component = "obs")
  })
}

# Test 4.3: Wrong type for covariate
test_4_3 <- run_test("Character covariate (should be numeric)", {
  newdata <- train_data[1:5, ]
  newdata$x <- as.character(newdata$x)
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# =============================================================================
# TEST 5: EDGE CASES
# =============================================================================

cat("\n\n=== TEST SUITE 5: EDGE CASES ===\n")

# Test 5.1: Single row
test_5_1 <- run_test("Single row prediction", {
  newdata <- train_data[1, , drop = FALSE]
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 5.2: Empty data frame
test_5_2 <- run_test("Empty data frame", {
  newdata <- train_data[0, ]
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 5.3: NA in covariate
test_5_3 <- run_test("NA in covariate", {
  newdata <- train_data[1:5, ]
  newdata$x[3] <- NA
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# Test 5.4: NA in response (should be fine for prediction)
test_5_4 <- run_test("NA in response (expected for prediction)", {
  newdata <- train_data[1:5, ]
  newdata$y <- NA
  extract_component_linpred(models$ar1_fx, newdata, component = "obs")
})

# =============================================================================
# TEST 6: TREND COMPONENT EXTRACTION
# =============================================================================

cat("\n\n=== TEST SUITE 6: TREND COMPONENT EXTRACTION ===\n")

# Test 6.1: Extract trend component
test_6_1 <- run_test("Extract trend component", {
  extract_component_linpred(models$ar1_fx, train_data, component = "trend")
})

# Test 6.2: Combined obs + trend
test_6_2 <- run_test("Combined obs + trend extraction", {
  obs <- extract_component_linpred(models$ar1_fx, train_data, component = "obs")
  trend <- extract_component_linpred(models$ar1_fx, train_data, component = "trend")
  combined <- obs + trend
  cat("  Combined dims:", dim(combined), "\n")
  combined
})

# =============================================================================
# TEST 7: SMOOTH TERMS IN NEWDATA
# =============================================================================

cat("\n\n=== TEST SUITE 7: SMOOTH TERMS ===\n")

if (!is.null(models$ar1_smooth)) {
  # Test 7.1: Valid smooth prediction
  test_7_1 <- run_test("Smooth prediction (in-range z)", {
    newdata <- train_data[1:10, ]
    extract_component_linpred(models$ar1_smooth, newdata, component = "obs")
  })

  # Test 7.2: Extrapolation beyond smooth range
  test_7_2 <- run_test("Smooth extrapolation (z beyond training range)", {
    newdata <- train_data[1:5, ]
    newdata$z <- seq(5, 6, length.out = 5)  # Way beyond training range [-2, 2]
    extract_component_linpred(models$ar1_smooth, newdata, component = "obs")
  })
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n\n=== VALIDATION SUMMARY ===\n")
cat("\nKey findings for validate_newdata_for_predictions():\n\n")

cat("1. VARIABLE EXISTENCE:\n")
cat("   - brms standata() validates covariate existence\n")
cat("   - time/series validation handled by ensure_mvgam_variables()\n")
cat("   - gr/subgr validated if hierarchical trend used\n")

cat("\n2. FACTOR LEVELS:\n")
cat("   - brms handles factor level validation\n")
cat("   - allow_new_levels parameter for random effects\n")
cat("   - Series levels must match training data\n")

cat("\n3. DATA TYPES:\n")
cat("   - brms coerces compatible types\n")
cat("   - Factor/character handled flexibly\n")

cat("\n4. RECOMMENDED VALIDATION CHECKS:\n")
cat("   - newdata is data.frame\n")
cat("   - Required variables exist (from model formulas)\n")
cat("   - time/series/gr/subgr exist if trend model present\n")
cat("   - Factor levels are subset of training levels\n")
cat("   - Let brms standata() handle detailed validation\n")

cat("\n=== COMPLETE ===\n")
