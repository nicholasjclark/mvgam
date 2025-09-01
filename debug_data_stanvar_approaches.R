# Debug script to test different approaches for data stanvar creation
# Tests various trend formula complexities to determine best approach

library(brms)
devtools::load_all()

cat("=== TESTING DATA STANVAR APPROACHES ===\n\n")

# Test data with different complexities
data_simple <- data.frame(
  y = rpois(50, 10),
  time = 1:50,
  series = gl(1, 50, labels = 'series_1')
)

data_complex <- data.frame(
  y = rpois(100, 10),
  x1 = rnorm(100),
  x2 = factor(rep(c("A", "B"), 50)),
  time = rep(1:50, 2),
  series = gl(2, 50, labels = c('series_1', 'series_2'))
)

# Test cases with increasing complexity
test_cases <- list(
  list(
    name = "Simple intercept-only",
    data = data_simple,
    obs_formula = y ~ 1,
    trend_formula = ~ 1,
    family = poisson()
  ),
  list(
    name = "Continuous predictor",
    data = data_complex,
    obs_formula = y ~ x1,
    trend_formula = ~ x1,
    family = poisson()
  ),
  list(
    name = "Factor predictor", 
    data = data_complex,
    obs_formula = y ~ x2,
    trend_formula = ~ x2,
    family = poisson()
  ),
  list(
    name = "Multiple predictors",
    data = data_complex,
    obs_formula = y ~ x1 + x2,
    trend_formula = ~ x1 + x2,
    family = poisson()
  ),
  list(
    name = "Complex interaction",
    data = data_complex,
    obs_formula = y ~ x1 * x2,
    trend_formula = ~ x1 * x2,
    family = poisson()
  )
)

for (i in seq_along(test_cases)) {
  test_case <- test_cases[[i]]
  cat("=== TEST CASE", i, ":", test_case$name, "===\n")
  
  tryCatch({
    # Setup both models
    obs_setup <- setup_brms_lightweight(test_case$obs_formula, data = test_case$data, family = test_case$family)
    trend_setup <- setup_brms_lightweight(test_case$trend_formula, data = test_case$data, family = gaussian())
    
    cat("Observation model data variables:", names(obs_setup$standata), "\n")
    cat("Trend model data variables:", names(trend_setup$standata), "\n")
    
    # Approach 1: Current extract_and_rename_standata_objects only
    cat("\n--- APPROACH 1: Data values only ---\n")
    data_stanvars_only <- tryCatch({
      extract_and_rename_standata_objects(
        standata = trend_setup$standata,
        suffix = "_trend",
        mapping = list(original_to_renamed = list(), renamed_to_original = list()),
        is_multivariate = FALSE,
        response_names = NULL
      )
    }, error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      NULL
    })
    
    if (!is.null(data_stanvars_only)) {
      cat("Created", length(data_stanvars_only), "data stanvars with values only\n")
      # Test if these create proper declarations
      test_stancode <- "data { }"
      for (j in seq_along(data_stanvars_only)) {
        sv <- data_stanvars_only[[j]]
        if (!is.null(sv$name) && !is.null(sv$sdata)) {
          cat("  ", sv$name, ":", class(sv$sdata), "length =", length(sv$sdata), "\n")
        }
      }
    }
    
    # Approach 2: Extract declarations from trend Stan code
    cat("\n--- APPROACH 2: Extract trend data block ---\n")
    trend_data_block <- extract_stan_block_content(trend_setup$stancode, "data")
    cat("Extracted data block:\n")
    cat(trend_data_block, "\n")
    
    # Parse the data block to understand structure
    data_lines <- strsplit(trend_data_block, "\n")[[1]]
    data_lines <- data_lines[data_lines != "" & !grepl("^\\s*$", data_lines)]
    cat("Data declarations (", length(data_lines), "lines):\n")
    for (line in data_lines) {
      clean_line <- trimws(gsub("//.*$", "", line))
      if (clean_line != "" && grepl(";\\s*$", clean_line)) {
        cat("  ", clean_line, "\n")
      }
    }
    
    # Approach 3: Try combining both (manual test)
    cat("\n--- APPROACH 3: Combined approach test ---\n")
    if (!is.null(data_stanvars_only) && length(data_stanvars_only) > 0) {
      # Take first stanvar and try to add scode
      first_sv <- data_stanvars_only[[1]]
      if (!is.null(first_sv$name)) {
        original_name <- gsub("_trend$", "", first_sv$name)
        # Find corresponding declaration line
        matching_line <- NULL
        for (line in data_lines) {
          if (grepl(paste0("\\b", original_name, "\\b"), line)) {
            matching_line <- gsub(paste0("\\b", original_name, "\\b"), first_sv$name, line)
            break
          }
        }
        
        if (!is.null(matching_line)) {
          cat("Found matching declaration:", matching_line, "\n")
          # Test if we can create combined stanvar
          combined_test <- tryCatch({
            brms::stanvar(
              x = first_sv$sdata,
              name = first_sv$name,
              scode = matching_line,
              block = "data"
            )
          }, error = function(e) {
            cat("Combined stanvar ERROR:", conditionMessage(e), "\n")
            NULL
          })
          
          if (!is.null(combined_test)) {
            cat("✓ Combined stanvar created successfully\n")
            cat("  Name:", combined_test$name, "\n")
            cat("  Has data:", !is.null(combined_test$sdata), "\n")
            cat("  Has scode:", !is.null(combined_test$scode), "\n")
          }
        } else {
          cat("No matching declaration found for", first_sv$name, "\n")
        }
      }
    }
    
  }, error = function(e) {
    cat("OVERALL ERROR for", test_case$name, ":", conditionMessage(e), "\n")
  })
  
  cat("\n" , rep("=", 60), "\n\n")
}

cat("=== SUMMARY ANALYSIS ===\n")
cat("1. Check which approach works for each complexity level\n")
cat("2. Identify where declarations are lost or corrupted\n")  
cat("3. Determine if combined approach is feasible\n")
cat("4. Look for patterns in brms data stanvar requirements\n")