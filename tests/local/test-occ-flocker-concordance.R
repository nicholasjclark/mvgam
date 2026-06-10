# Cross-package concordance: same simulated single-season
# occupancy data, fit in mvgam (occ()) and flocker (flock()).
# Compares posterior means + SDs on the shared parameters and
# the per-site posterior P(z = 1 | y).
#
# Fits are cached in tests/local/fixtures/:
#   val_occ_simdata.rds          shared data + truth values
#   val_occ_mvgam.rds            mvgam_fit object
#   val_occ_flocker.rds          flocker_fit object
#
# Build the fixtures via:
#   Rscript -e 'devtools::load_all(); source("tests/local/build_fixtures_occ.R")'
#
# Run the assertions via:
#   Rscript -e 'devtools::load_all();
#               testthat::test_file("tests/local/test-occ-flocker-concordance.R")'

# Source the shared fixture helpers; both this file and the
# builder script live in tests/local so the relative path works
# whether the working directory is the package root or the
# tests/local dir itself.
if (file.exists("tests/local/concordance_helpers.R")) {
  source("tests/local/concordance_helpers.R")
} else if (file.exists("concordance_helpers.R")) {
  source("concordance_helpers.R")
}

testthat::skip_if_not_installed("flocker")
testthat::skip_if_not_installed("posterior")

require_fixtures(
  "val_occ_simdata.rds",
  "val_occ_mvgam.rds",
  "val_occ_flocker.rds"
)

fdir       <- local_fixture_dir()
sim        <- readRDS(file.path(fdir, "val_occ_simdata.rds"))
mvgam_fit  <- readRDS(file.path(fdir, "val_occ_mvgam.rds"))
flocker_fit <- readRDS(file.path(fdir, "val_occ_flocker.rds"))

# ------------------------------------------------------------
# Compare population-level effects
# ------------------------------------------------------------

# `as_draws_matrix(mvgam_fit)` (the mvgam method) applies the
# brms-style aliasing `b[k]` -> `b_<termname>` so the slope
# columns come through with names that match flocker's.
mvgam_draws   <- posterior::as_draws_matrix(mvgam_fit)
flocker_draws <- posterior::as_draws_matrix(flocker_fit)

summarise_one <- function(draws, name) {
  if (!(name %in% colnames(draws))) {
    stop("Parameter '", name, "' not found.")
  }
  c(mean = mean(draws[, name]), sd = stats::sd(draws[, name]))
}

# Name maps: mvgam emits centred `b_Intercept` + `b_elev` for psi
# and `b_p_Intercept` + `b_p_tod_c` for the detection layer.
# flocker emits `b_occ_Intercept` + `b_occ_elev` for the state
# submodel and `b_Intercept` + `b_tod_c` for the detection
# submodel (mu in flocker IS the detection linpred).
mvgam_state_int  <- summarise_one(mvgam_draws,   "b_Intercept")
flocker_state_int <- summarise_one(flocker_draws, "b_occ_Intercept")
mvgam_state_b   <- summarise_one(mvgam_draws,   "b_elev")
flocker_state_b <- summarise_one(flocker_draws, "b_occ_elev")
mvgam_det_int   <- summarise_one(mvgam_draws,   "b_p_Intercept")
flocker_det_int <- summarise_one(flocker_draws, "b_Intercept")
mvgam_det_b     <- summarise_one(mvgam_draws,   "b_p_tod_c")
flocker_det_b   <- summarise_one(flocker_draws, "b_tod_c")

test_that("psi intercept agrees between mvgam and flocker (within 3 SE)", {
  diff <- mvgam_state_int["mean"] - flocker_state_int["mean"]
  joint_se <- sqrt(mvgam_state_int["sd"]^2 + flocker_state_int["sd"]^2)
  expect_lt(abs(diff), 3 * joint_se)
})

test_that("psi elev slope agrees between mvgam and flocker (within 3 SE)", {
  diff <- mvgam_state_b["mean"] - flocker_state_b["mean"]
  joint_se <- sqrt(mvgam_state_b["sd"]^2 + flocker_state_b["sd"]^2)
  expect_lt(abs(diff), 3 * joint_se)
})

test_that("p intercept agrees between mvgam and flocker (within 3 SE)", {
  diff <- mvgam_det_int["mean"] - flocker_det_int["mean"]
  joint_se <- sqrt(mvgam_det_int["sd"]^2 + flocker_det_int["sd"]^2)
  expect_lt(abs(diff), 3 * joint_se)
})

test_that("p tod_c slope agrees between mvgam and flocker (within 3 SE)", {
  diff <- mvgam_det_b["mean"] - flocker_det_b["mean"]
  joint_se <- sqrt(mvgam_det_b["sd"]^2 + flocker_det_b["sd"]^2)
  expect_lt(abs(diff), 3 * joint_se)
})

# ------------------------------------------------------------
# Per-site posterior P(z = 1 | y): mvgam posterior_occupancy
# vs flocker get_Z(history_condition = TRUE)
# ------------------------------------------------------------

mvgam_psi_cond <- posterior_occupancy(
  mvgam_fit, conditional = TRUE, draw = FALSE
)
mvgam_psi_mean <- colMeans(mvgam_psi_cond)

flocker_z <- flocker::get_Z(
  flocker_fit, history_condition = TRUE, sample = FALSE
)
flocker_psi_mean <- if (is.matrix(flocker_z)) {
  rowMeans(flocker_z)
} else {
  flocker_z
}

test_that("per-site P(z=1|y) agrees within 0.05 between mvgam and flocker", {
  expect_length(mvgam_psi_mean, sim$n_sites)
  expect_length(flocker_psi_mean, sim$n_sites)
  max_diff <- max(abs(mvgam_psi_mean - flocker_psi_mean))
  expect_lt(max_diff, 0.05)
})
