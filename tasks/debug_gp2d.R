# Debug script for multidimensional GP (2D) prediction issue
# Error: Dimension mismatch: lscale has X columns but slambda has Y dimensions

devtools::load_all()
library(brms)
library(posterior)

FIXTURE_DIR <- "tasks/fixtures"

# Load cached models
cat("=== Loading models ===\n")
brms_9 <- readRDS(file.path(FIXTURE_DIR, "val_brms_ar1_gp2d.rds"))
mvgam_9 <- readRDS(file.path(FIXTURE_DIR, "val_mvgam_ar1_gp2d.rds"))

cat("brms model loaded:", class(brms_9), "\n")
cat("mvgam model loaded:", class(mvgam_9), "\n")

# Create test data (same as validation script)
set.seed(42)
n_time <- 30
z <- seq(-2, 2, length.out = n_time)
test_data_t2 <- data.frame(
 y = rpois(n_time, exp(2)),
 x = rnorm(n_time),
 z = z,
 w = seq(-1, 1, length.out = n_time),
 time = 1:n_time,
 series = factor("s1"),
 group = factor(rep(letters[1:6], each = 5))
)

cat("\n=== Examining brms model structure ===\n")
brms_draws <- as_draws_matrix(brms_9)
brms_params <- colnames(brms_draws)
cat("brms GP-related parameters:\n")
gp_params <- grep("gp|lscale|sdgp|zgp", brms_params, value = TRUE, ignore.case = TRUE)
print(gp_params[1:min(20, length(gp_params))])

cat("\n=== Examining mvgam model structure ===\n")
mvgam_draws <- as_draws_matrix(mvgam_9$fit)
mvgam_params <- colnames(mvgam_draws)
cat("mvgam GP-related parameters:\n")
gp_params_mv <- grep("gp|lscale|sdgp|zgp", mvgam_params, value = TRUE, ignore.case = TRUE)
print(gp_params_mv[1:min(20, length(gp_params_mv))])

cat("\n=== Testing brms posterior_linpred ===\n")
brms_pred <- posterior_linpred(brms_9, newdata = test_data_t2, incl_autocor = FALSE)
cat("brms prediction dimensions:", dim(brms_pred), "\n")

cat("\n=== Testing mvgam extraction ===\n")
cat("Extracting observation parameters...\n")
obs_params <- extract_obs_parameters(mvgam_9)
cat("Found", length(obs_params), "observation parameters\n")
cat("GP-related obs params:\n")
print(grep("gp|lscale|sdgp|zgp", obs_params, value = TRUE, ignore.case = TRUE))

cat("\n=== Creating mock stanfit ===\n")
full_draws <- as_draws_matrix(mvgam_9$fit)
obs_draws <- full_draws[, obs_params, drop = FALSE]
cat("obs_draws dimensions:", dim(obs_draws), "\n")
mock_fit <- create_mock_stanfit(obs_draws)

cat("\n=== Calling prepare_predictions.mock_stanfit ===\n")
prep <- prepare_predictions.mock_stanfit(
 object = mock_fit,
 brmsfit = mvgam_9$obs_model,
 newdata = test_data_t2
)

cat("\n=== Examining prep$sdata for GP structures ===\n")
sdata_names <- names(prep$sdata)
gp_sdata <- grep("gp|Xgp|slambda|NBgp", sdata_names, value = TRUE, ignore.case = TRUE)
cat("GP-related sdata entries:\n")
print(gp_sdata)

for (nm in gp_sdata) {
 obj <- prep$sdata[[nm]]
 cat("\n", nm, ":\n", sep = "")
 cat("  class:", class(obj), "\n")
 if (is.array(obj) || is.matrix(obj)) {
   cat("  dim:", dim(obj), "\n")
   if (length(dim(obj)) <= 2 && prod(dim(obj)) <= 20) {
     cat("  values:\n")
     print(obj)
   }
 } else if (is.vector(obj)) {
   cat("  length:", length(obj), "\n")
   if (length(obj) <= 10) {
     cat("  values:", obj, "\n")
   }
 }
}

cat("\n=== Examining prep$draws for GP parameters ===\n")
draws_names <- colnames(prep$draws)
gp_draws <- grep("gp|lscale|sdgp|zgp", draws_names, value = TRUE, ignore.case = TRUE)
cat("GP-related draw parameters:\n")
print(gp_draws)

# Check lscale structure specifically
lscale_params <- grep("^lscale", draws_names, value = TRUE)
cat("\nlscale parameters found:", lscale_params, "\n")

if (length(lscale_params) > 0) {
 for (lp in lscale_params) {
   cat("\n", lp, ":\n", sep = "")
   vals <- prep$draws[1:3, lp]
   cat("  first 3 draws:", vals, "\n")
 }
}

cat("\n=== Calling detect_gp_terms ===\n")
gp_info <- detect_gp_terms(prep)
cat("GP info:\n")
print(gp_info)

cat("\n=== Attempting extract_linpred_from_prep ===\n")
tryCatch({
 linpred <- extract_linpred_from_prep(prep)
 cat("Success! linpred dimensions:", dim(linpred), "\n")
}, error = function(e) {
 cat("Error:", conditionMessage(e), "\n")
 cat("\nTraceback:\n")
 traceback()
})

cat("\n=== Manual GP computation debug ===\n")
# Try to manually trace the GP computation
if (!is.null(gp_info) && length(gp_info$suffixes) > 0) {
 suffix <- gp_info$suffixes[1]
 cat("Debugging GP suffix:", suffix, "\n")

 # Get slambda
 slambda_name <- paste0("slambda_", suffix)
 if (slambda_name %in% names(prep$sdata)) {
   slambda <- prep$sdata[[slambda_name]]
   cat("\nslambda structure:\n")
   cat("  class:", class(slambda), "\n")
   cat("  dim:", dim(slambda), "\n")

   # Handle 3D array
   if (length(dim(slambda)) == 3) {
     cat("  (3D array, extracting [,,1])\n")
     slambda_2d <- slambda[, , 1]
     cat("  slambda_2d dim:", dim(slambda_2d), "\n")
     cat("  n_basis (rows):", nrow(slambda_2d), "\n")
     cat("  n_dims (cols):", ncol(slambda_2d), "\n")
   } else {
     cat("  n_basis (rows):", nrow(slambda), "\n")
     cat("  n_dims (cols):", ncol(slambda), "\n")
   }
 }

 # Get lscale
 lscale_pattern <- paste0("^lscale_", suffix, "\\[")
 lscale_cols <- grep(lscale_pattern, colnames(prep$draws), value = TRUE)
 cat("\nlscale columns matching pattern '", lscale_pattern, "':\n", sep = "")
 print(lscale_cols)

 if (length(lscale_cols) > 0) {
   lscale_matrix <- prep$draws[, lscale_cols, drop = FALSE]
   cat("\nlscale_matrix structure:\n")
   cat("  dim:", dim(lscale_matrix), "\n")
   cat("  nrow (n_draws):", nrow(lscale_matrix), "\n")
   cat("  ncol (should match n_dims):", ncol(lscale_matrix), "\n")
 }

 # The issue: slambda has n_dims columns, lscale should have n_dims columns
 # For 2D GP: slambda should be [n_basis, 2], lscale should be [n_draws, 2]
}

cat("\n=== Debug complete ===\n")
