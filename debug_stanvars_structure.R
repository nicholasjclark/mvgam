# Debug script to trace stanvars structure through the pipeline
library(brms)
devtools::load_all()

cat("=== STANVARS STRUCTURE DEBUG ===\n\n")

# Test 1: Basic stanvar combination
cat("TEST 1: Basic stanvar combination\n")
sv1 <- brms::stanvar(name = "sigma_trend", scode = "vector<lower=0>[1] sigma_trend;", block = "parameters")
sv2 <- brms::stanvar(name = "innovations_trend", scode = "matrix[50, 1] innovations_trend;", block = "parameters")
sv3 <- brms::stanvar(name = "innovation_priors", scode = "to_vector(innovations_trend) ~ std_normal();", block = "model")

cat("Individual stanvar classes:\n")
cat("  sv1:", class(sv1), "\n")
cat("  sv2:", class(sv2), "\n")
cat("  sv3:", class(sv3), "\n")

# Test combining with c()
combined_c <- c(sv1, sv2)
cat("\nAfter c(sv1, sv2):\n")
cat("  Class:", class(combined_c), "\n")
cat("  Length:", length(combined_c), "\n")
cat("  Names:", names(combined_c), "\n")

# Add third stanvar
combined_c_3 <- c(combined_c, sv3)
cat("\nAfter c(combined_c, sv3):\n")
cat("  Class:", class(combined_c_3), "\n")
cat("  Length:", length(combined_c_3), "\n")
cat("  Names:", names(combined_c_3), "\n")

# Test 2: What happens when we try to add a field to stanvars
cat("\n\nTEST 2: Adding field to stanvars object (current bug)\n")
test_stanvars <- c(sv1, sv2)
cat("Before assignment - Class:", class(test_stanvars), "Length:", length(test_stanvars), "\n")

# Try the problematic assignment
test_stanvars$priors <- sv3
cat("After test_stanvars$priors <- sv3:\n")
cat("  Class:", class(test_stanvars), "\n")
cat("  Length:", length(test_stanvars), "\n")
cat("  Names:", names(test_stanvars), "\n")
cat("  Has $priors field:", !is.null(test_stanvars$priors), "\n")

# Check if the priors actually got added
if (!is.null(test_stanvars$priors)) {
  cat("  $priors content:", class(test_stanvars$priors), "\n")
}

# Test 3: Proper combination using combine_stanvars
cat("\n\nTEST 3: Using combine_stanvars function\n")
combined_proper <- combine_stanvars(sv1, sv2, sv3)
cat("After combine_stanvars(sv1, sv2, sv3):\n")
cat("  Class:", class(combined_proper), "\n")
cat("  Length:", length(combined_proper), "\n")
cat("  Names:", names(combined_proper), "\n")

# Test 4: Check if stanvars can be iterated
cat("\n\nTEST 4: Iterating over stanvars collection\n")
for (i in seq_along(combined_proper)) {
  sv <- combined_proper[[i]]
  cat("  Element", i, "- Name:", sv$name, "Block:", sv$block, "\n")
}

# Test 5: apply_response_suffix_to_stanvars behavior
cat("\n\nTEST 5: Testing apply_response_suffix_to_stanvars\n")

# Test with single stanvar
single_result <- apply_response_suffix_to_stanvars(sv1, "_y1")
cat("Single stanvar input:\n")
cat("  Result class:", class(single_result), "\n")
cat("  Result name:", single_result$name, "\n")

# Test with stanvars collection
collection_result <- apply_response_suffix_to_stanvars(combined_proper, "_y1")
cat("\nStanvars collection input:\n")
cat("  Result class:", class(collection_result), "\n")
cat("  Result length:", length(collection_result), "\n")
if (inherits(collection_result, "stanvars")) {
  cat("  Result names:", names(collection_result), "\n")
} else if (is.list(collection_result)) {
  cat("  Result is a list with", length(collection_result), "elements\n")
}

# Test 6: Real pipeline simulation
cat("\n\nTEST 6: Simulating real pipeline flow\n")

# Simulate generate_shared_innovation_stanvars returning stanvars
shared_stanvars <- c(sv1, sv2)  # This returns a stanvars object
cat("shared_stanvars from generate_shared_innovation_stanvars:\n")
cat("  Class:", class(shared_stanvars), "Length:", length(shared_stanvars), "\n")

# Generate priors separately
shared_priors <- sv3
cat("\nshared_priors from generate_innovation_model:\n")
cat("  Class:", class(shared_priors), "\n")

# Current buggy approach
cat("\nBuggy approach: shared_stanvars$priors <- shared_priors\n")
buggy_shared <- shared_stanvars
buggy_shared$priors <- shared_priors
cat("  Result class:", class(buggy_shared), "\n")
cat("  Result length:", length(buggy_shared), "\n")
cat("  Has priors field:", !is.null(buggy_shared$priors), "\n")

# Check if model block stanvar is actually included
has_model_block <- FALSE
for (i in seq_along(buggy_shared)) {
  if (!is.null(buggy_shared[[i]]$block) && buggy_shared[[i]]$block == "model") {
    has_model_block <- TRUE
    break
  }
}
cat("  Contains model block stanvar:", has_model_block, "\n")

# Fixed approach
cat("\nFixed approach: combine_stanvars(shared_stanvars, shared_priors)\n")
fixed_shared <- combine_stanvars(shared_stanvars, shared_priors)
cat("  Result class:", class(fixed_shared), "\n")
cat("  Result length:", length(fixed_shared), "\n")

# Check if model block stanvar is included
has_model_block_fixed <- FALSE
for (i in seq_along(fixed_shared)) {
  if (!is.null(fixed_shared[[i]]$block) && fixed_shared[[i]]$block == "model") {
    has_model_block_fixed <- TRUE
    cat("  Found model block stanvar:", fixed_shared[[i]]$name, "\n")
    break
  }
}
cat("  Contains model block stanvar:", has_model_block_fixed, "\n")

# Test applying suffix to fixed version
cat("\nApplying suffix to fixed version:\n")
suffixed_result <- apply_response_suffix_to_stanvars(fixed_shared, "_y1")
cat("  Result class:", class(suffixed_result), "\n")
cat("  Result length:", length(suffixed_result), "\n")

# Verify the suffix was applied
if (length(suffixed_result) > 0) {
  cat("  Checking suffix application:\n")
  for (i in seq_along(suffixed_result)) {
    sv <- suffixed_result[[i]]
    if (inherits(sv, "stanvar")) {
      cat("    ", sv$name, "(block:", sv$block, ")\n")
      # Check if scode contains suffixed parameters
      if (!is.null(sv$scode) && grepl("_trend", sv$scode)) {
        has_suffix <- grepl("_trend_y1", sv$scode)
        cat("      Contains _trend_y1:", has_suffix, "\n")
      }
    }
  }
}

cat("\n=== SUMMARY ===\n")
cat("1. stanvars$field <- value does NOT properly add to collection\n")
cat("2. combine_stanvars() is the correct way to combine stanvars\n")
cat("3. apply_response_suffix_to_stanvars needs to handle stanvars collections\n")