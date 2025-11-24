# Debug script to reproduce hierarchical ZMVN test failure
# Based on "stancode generates correct hierarchical ZMVN(gr = habitat) model with custom prior"

# Load all functions (required by CLAUDE.md guidelines)
devtools::load_all()

# Set up test data exactly as in the test
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

  # Multivariate dataset with balanced design
  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rbinom(n_time * n_series, size = 1, prob = 0.7),
    x = rnorm(n_time * n_series),
    habitat = factor(sample(c("forest", "grassland"), n_time * n_series, replace = TRUE))
  )

  list(
    multivariate = multivariate
  )
}

cat("Setting up test data...\n")
data <- setup_stan_test_data()$multivariate

cat("Creating mvgam formula with hierarchical ZMVN trend...\n")
mf_with_trend <- mvgam_formula(
  biomass ~ 1,
  trend_formula = ~ 1 + x + ZMVN(gr = habitat)
)

cat("Setting up custom prior for alpha_cor_trend...\n")
custom_prior <- brms::prior("beta(5, 5)", class = "alpha_cor_trend")

cat("Generating Stan code...\n")
tryCatch({
  code_with_trend <- stancode(
    mf_with_trend,
    data = data,
    family = lognormal(),
    prior = custom_prior,
    validate = FALSE
  )
  
  cat("SUCCESS: Stan code generated successfully!\n")
  cat("Writing Stan code to file for analysis...\n")
  
  # Write the generated Stan code to a file for examination
  writeLines(code_with_trend, "hierarchical_zmvn_model.stan")
  cat("Stan code written to: hierarchical_zmvn_model.stan\n")
  
  # Check for some key patterns that the test expects
  cat("\n=== Checking for expected patterns ===\n")
  
  # Helper function to check patterns
  stan_pattern <- function(pattern, code) {
    grepl(pattern, code, perl = TRUE)
  }
  
  # Check hierarchical data structures
  cat("✓ n_groups_trend:", stan_pattern("int<lower=1> n_groups_trend;", code_with_trend), "\n")
  
  # Check hierarchical correlation parameters with _trend suffix
  cat("✓ L_Omega_global_trend:", stan_pattern("cholesky_factor_corr\\[N_lv_trend\\] L_Omega_global_trend;", code_with_trend), "\n")
  cat("✓ L_deviation_group_trend:", stan_pattern("array\\[n_groups_trend\\] cholesky_factor_corr\\[N_lv_trend\\] L_deviation_group_trend;", code_with_trend), "\n")
  cat("✓ alpha_cor_trend:", stan_pattern("real<lower=0, upper=1> alpha_cor_trend;", code_with_trend), "\n")
  
  # Check hierarchical correlation functions
  cat("✓ combine_cholesky function:", stan_pattern("matrix combine_cholesky\\(", code_with_trend), "\n")
  
  # Check group-specific correlation computation
  cat("✓ L_Omega_group_trend computation:", stan_pattern("L_Omega_group_trend = combine_cholesky\\(L_Omega_global_trend, L_deviation_group_trend\\[g\\], alpha_cor_trend\\)", code_with_trend), "\n")
  
  # Check custom prior application
  cat("✓ Custom alpha_cor_trend prior:", stan_pattern("alpha_cor_trend ~ beta\\(5, 5\\);", code_with_trend), "\n")
  
  cat("\n=== Analysis complete ===\n")
  cat("Check hierarchical_zmvn_model.stan for detailed Stan code inspection.\n")
  
}, error = function(e) {
  cat("ERROR occurred during Stan code generation:\n")
  cat("Message:", conditionMessage(e), "\n")
  cat("Call:", deparse(conditionCall(e)), "\n")
  
  if (exists("e$trace")) {
    cat("Traceback:\n")
    print(e$trace)
  }
})

cat("Debug script complete.\n")