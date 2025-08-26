# ZMVN Parameter Investigation
# ============================
# Understanding why ZMVN gets L_Omega_trend when monitor_params only shows sigma_trend

devtools::load_all()

cat("=== ZMVN PARAMETER INVESTIGATION ===\n\n")

# Test data with multiple series to trigger multivariate behavior
test_data <- data.frame(
  y = rpois(40, 5),
  x = rnorm(40),
  time = rep(1:20, 2),
  series = factor(rep(c("A", "B"), each = 20))
)

# ZMVN constructor analysis
cat("1. ZMVN Constructor Analysis\n")
cat("----------------------------\n")
zmvn_obj <- ZMVN()
cat("ZMVN trend type:", zmvn_obj$trend, "\n")
cat("ZMVN cor parameter:", zmvn_obj$cor %||% "NULL", "\n")
cat("ZMVN monitor_params:", paste(zmvn_obj$monitor_params, collapse = ", "), "\n")

# Check if cor parameter affects L_Omega_trend generation
cat("\n2. ZMVN with explicit cor=FALSE\n")
cat("-------------------------------\n")
zmvn_nocor <- ZMVN(cor = FALSE)
cat("ZMVN(cor=FALSE) monitor_params:", paste(zmvn_nocor$monitor_params, collapse = ", "), "\n")

cat("\n3. ZMVN with explicit cor=TRUE\n") 
cat("------------------------------\n")
zmvn_cor <- ZMVN(cor = TRUE)
cat("ZMVN(cor=TRUE) monitor_params:", paste(zmvn_cor$monitor_params, collapse = ", "), "\n")

# Test full pipeline for each case
cat("\n4. Full Pipeline Analysis\n")
cat("-------------------------\n")

test_cases <- list(
  "ZMVN()" = ZMVN(),
  "ZMVN(cor=FALSE)" = ZMVN(cor = FALSE), 
  "ZMVN(cor=TRUE)" = ZMVN(cor = TRUE)
)

for (case_name in names(test_cases)) {
  cat(sprintf("\n%s:\n", case_name))
  
  # Create trend formula with this ZMVN object
  # We'll need to do this through a formula since get_prior expects formulas
  
  if (case_name == "ZMVN()") {
    mf <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN())
  } else if (case_name == "ZMVN(cor=FALSE)") {
    mf <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN(cor = FALSE))
  } else if (case_name == "ZMVN(cor=TRUE)") {
    mf <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN(cor = TRUE))
  }
  
  priors <- get_prior(mf, data = test_data, family = poisson())
  trend_priors <- priors[priors$trend_component == "trend", ]
  actual_params <- sort(unique(trend_priors$class))
  
  cat("  Actual parameters:", paste(actual_params, collapse = ", "), "\n")
  
  # Check if L_Omega_trend presence correlates with cor parameter or multivariate data
  has_l_omega <- "L_Omega_trend" %in% actual_params
  cat("  Has L_Omega_trend:", has_l_omega, "\n")
}

# Test with univariate data to see if multivariate context affects results
cat("\n5. Univariate vs Multivariate Context\n")
cat("-------------------------------------\n")

test_data_uv <- data.frame(
  y = rpois(20, 5),
  x = rnorm(20), 
  time = 1:20,
  series = factor("A")
)

mf_uv <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN())
priors_uv <- get_prior(mf_uv, data = test_data_uv, family = poisson())
trend_priors_uv <- priors_uv[priors_uv$trend_component == "trend", ]
actual_uv <- sort(unique(trend_priors_uv$class))

cat("Univariate context:", paste(actual_uv, collapse = ", "), "\n")
cat("Multivariate context:", paste(actual_params, collapse = ", "), "\n")

has_l_omega_uv <- "L_Omega_trend" %in% actual_uv
has_l_omega_mv <- "L_Omega_trend" %in% actual_params

cat("L_Omega_trend in univariate:", has_l_omega_uv, "\n")
cat("L_Omega_trend in multivariate:", has_l_omega_mv, "\n")

cat("\n=== HYPOTHESIS ===\n")
cat("L_Omega_trend might be added automatically when:\n")
cat("1. Multiple series are present (multivariate context), OR\n")
cat("2. The prior generation process assumes correlation is needed\n")
cat("3. There's a default correlation behavior for multivariate trends\n")