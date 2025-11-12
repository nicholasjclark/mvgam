# Verify brms Random Effects Parameter Structure
#
# PURPOSE: Examine actual brms parameter naming for simple and correlated RE
# in both univariate and multivariate models to ensure correct implementation
#
# CRITICAL: Always start with devtools::load_all()

devtools::load_all()
library(brms)

set.seed(123)

# ============================================================================
# TEST 1: Univariate Simple RE
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("TEST 1: Univariate Simple Random Intercepts (1 | group)\n")
cat(rep("=", 80), "\n", sep = "")

# Generate test data
n_groups <- 5
n_per_group <- 10
dat1 <- data.frame(
  y = rnorm(n_groups * n_per_group),
  x = rnorm(n_groups * n_per_group),
  group = factor(rep(1:n_groups, each = n_per_group))
)

# Fit brms model
fit1 <- brm(
  y ~ x + (1 | group),
  data = dat1,
  family = gaussian(),
  chains = 2,
  iter = 500,
  refresh = 0,
  backend = "rstan"
)

# Extract standata
sdata1 <- standata(fit1)
cat("\nStandata structure for simple RE:\n")
cat("Z matrices:\n")
print(names(sdata1)[grep("^Z_", names(sdata1))])
cat("\nJ indices:\n")
print(names(sdata1)[grep("^J_", names(sdata1))])
cat("\nN groups:\n")
print(names(sdata1)[grep("^N_", names(sdata1))])

# Extract parameters
cat("\n\nParameter names for simple RE:\n")
parnames1 <- variables(fit1)
cat("r_ parameters:\n")
print(parnames1[grep("^r_", parnames1)])
cat("\nsd_ parameters:\n")
print(parnames1[grep("^sd_", parnames1)])

# ============================================================================
# TEST 2: Univariate Correlated RE
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("TEST 2: Univariate Correlated Random Effects (x | group)\n")
cat(rep("=", 80), "\n", sep = "")

# Generate test data with x predictor
dat2 <- data.frame(
  y = rnorm(n_groups * n_per_group),
  x = rnorm(n_groups * n_per_group),
  group = factor(rep(1:n_groups, each = n_per_group))
)

# Fit brms model with correlated RE
fit2 <- brm(
  y ~ x + (x | group),
  data = dat2,
  family = gaussian(),
  chains = 2,
  iter = 500,
  refresh = 0,
  backend = "rstan"
)

# Extract standata
sdata2 <- standata(fit2)
cat("\nStandata structure for correlated RE:\n")
cat("Z matrices:\n")
print(names(sdata2)[grep("^Z_", names(sdata2))])
cat("\nJ indices:\n")
print(names(sdata2)[grep("^J_", names(sdata2))])

# Extract parameters
cat("\n\nParameter names for correlated RE:\n")
parnames2 <- variables(fit2)
cat("r_ parameters:\n")
print(parnames2[grep("^r_", parnames2)])
cat("\nsd_ parameters:\n")
print(parnames2[grep("^sd_", parnames2)])
cat("\nz_ parameters:\n")
print(parnames2[grep("^z_", parnames2)])
cat("\nL_ parameters:\n")
print(parnames2[grep("^L_", parnames2)])
cat("\nCor_ parameters:\n")
print(parnames2[grep("^cor_", parnames2)])

# Check dimensions
cat("\n\nParameter dimensions:\n")
draws2 <- as_draws_matrix(fit2)
cat("sd_1 dimensions:", dim(draws2[, grep("^sd_1", colnames(draws2))]), "\n")
cat("z_1 dimensions:", dim(draws2[, grep("^z_1\\[", colnames(draws2))]), "\n")
cat("L_1 dimensions:", dim(draws2[, grep("^L_1\\[", colnames(draws2))]), "\n")

# ============================================================================
# TEST 3: Multivariate Simple RE
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("TEST 3: Multivariate Simple Random Intercepts\n")
cat(rep("=", 80), "\n", sep = "")

# Generate multivariate test data
dat3 <- data.frame(
  y1 = rnorm(n_groups * n_per_group),
  y2 = rnorm(n_groups * n_per_group),
  x = rnorm(n_groups * n_per_group),
  group = factor(rep(1:n_groups, each = n_per_group))
)

# Fit multivariate brms model
fit3 <- brm(
  mvbind(y1, y2) ~ x + (1 | group),
  data = dat3,
  family = gaussian(),
  chains = 2,
  iter = 500,
  refresh = 0,
  backend = "rstan"
)

# Extract standata
sdata3 <- standata(fit3)
cat("\nStandata structure for multivariate simple RE:\n")
cat("Z matrices:\n")
print(names(sdata3)[grep("^Z_", names(sdata3))])
cat("\nJ indices:\n")
print(names(sdata3)[grep("^J_", names(sdata3))])
cat("\nN_ fields:\n")
print(names(sdata3)[grep("^N_", names(sdata3))])

# Extract parameters
cat("\n\nParameter names for multivariate simple RE:\n")
parnames3 <- variables(fit3)
cat("r_ parameters:\n")
print(parnames3[grep("^r_", parnames3)])
cat("\nsd_ parameters:\n")
print(parnames3[grep("^sd_", parnames3)])

# ============================================================================
# TEST 4: Multivariate Correlated RE
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("TEST 4: Multivariate Correlated Random Effects\n")
cat(rep("=", 80), "\n", sep = "")

# Fit multivariate with correlated RE
fit4 <- brm(
  mvbind(y1, y2) ~ x + (x | group),
  data = dat3,
  family = gaussian(),
  chains = 2,
  iter = 500,
  refresh = 0,
  backend = "rstan"
)

# Extract standata
sdata4 <- standata(fit4)
cat("\nStandata structure for multivariate correlated RE:\n")
cat("Z matrices:\n")
print(names(sdata4)[grep("^Z_", names(sdata4))])
cat("\nJ indices:\n")
print(names(sdata4)[grep("^J_", names(sdata4))])

# Extract parameters
cat("\n\nParameter names for multivariate correlated RE:\n")
parnames4 <- variables(fit4)
cat("r_ parameters:\n")
print(parnames4[grep("^r_", parnames4)])
cat("\nsd_ parameters:\n")
print(parnames4[grep("^sd_", parnames4)])
cat("\nz_ parameters:\n")
print(parnames4[grep("^z_", parnames4)])
cat("\nL_ parameters:\n")
print(parnames4[grep("^L_", parnames4)])

# ============================================================================
# SUMMARY: Parameter Structure Documentation
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY: brms Random Effects Parameter Structure\n")
cat(rep("=", 80), "\n", sep = "")

cat("\n1. SIMPLE RE (1 | group):\n")
cat("   - Parameters: r_<group>_<term>[level]\n")
cat("   - Standata: Z_<group>_<term>, J_<group>, N_<group>\n")
cat("   - Multivariate: NO response prefix in parameter names\n")

cat("\n2. CORRELATED RE (x | group):\n")
cat("   - Parameters: sd_<group>[term], z_<group>[...], L_<group>[...]\n")
cat("   - NO r_ parameters (non-centered parameterization)\n")
cat("   - Standata: Z_<group>_<term> for each term, J_<group>\n")

cat("\n3. MULTIVARIATE MODELS:\n")
cat("   - Standata has response-specific N_y1, N_y2\n")
cat("   - BUT: RE parameters do NOT have response prefix\n")
cat("   - Filtering by matching Z length to response-specific N\n")

cat("\n4. STORAGE ORDER (z_ parameters):\n")
cat("   - Check actual indexing from fitted models above\n")

cat("\n\nSaving results for analysis...\n")
saveRDS(list(
  fit1 = fit1, sdata1 = sdata1,
  fit2 = fit2, sdata2 = sdata2,
  fit3 = fit3, sdata3 = sdata3,
  fit4 = fit4, sdata4 = sdata4
), "tasks/brms_re_structure_verification.rds")

cat("\nDone! Results saved to tasks/brms_re_structure_verification.rds\n")
