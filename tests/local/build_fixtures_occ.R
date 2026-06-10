# Build the cached fits for the flocker / mvgam occ() concordance
# test. Writes three artefacts to tests/local/fixtures:
#   val_occ_simdata.rds   simulated data + truth values
#   val_occ_mvgam.rds     mvgam_fit on the simulated data
#   val_occ_flocker.rds   flocker_fit on the same simulated data
#
# Idempotent: skips any fit that already exists on disk unless
# `force = TRUE`. Run via:
#   Rscript -e 'devtools::load_all(quiet = TRUE);
#               source("tests/local/build_fixtures_occ.R")'

if (!exists("force")) force <- FALSE

stopifnot(requireNamespace("flocker", quietly = TRUE))
stopifnot(requireNamespace("brms",    quietly = TRUE))

source_helpers <- if (file.exists("tests/local/concordance_helpers.R")) {
  "tests/local/concordance_helpers.R"
} else {
  "concordance_helpers.R"
}
source(source_helpers)
fdir <- local_fixture_dir()
if (!dir.exists(fdir)) dir.create(fdir, recursive = TRUE)

paths <- list(
  sim     = file.path(fdir, "val_occ_simdata.rds"),
  mvgam   = file.path(fdir, "val_occ_mvgam.rds"),
  flocker = file.path(fdir, "val_occ_flocker.rds")
)

# ------------------------------------------------------------
# Simulate (shared by both fits)
# ------------------------------------------------------------

if (!file.exists(paths$sim) || force) {
  set.seed(73)
  n_sites <- 60L
  n_visits <- 4L
  elev <- rnorm(n_sites)
  tod_visit <- matrix(stats::runif(n_sites * n_visits, 6, 18),
                      nrow = n_sites, ncol = n_visits)
  b0_state <- -0.4; b1_state <- 0.9
  b0_det   <-  0.3; b1_det   <- 0.15
  psi_true <- plogis(b0_state + b1_state * elev)
  z_true   <- rbinom(n_sites, 1L, psi_true)
  p_visit  <- plogis(b0_det + b1_det * (tod_visit - 12))
  y_mat    <- matrix(0L, nrow = n_sites, ncol = n_visits)
  for (i in seq_len(n_sites)) {
    if (z_true[i] == 1L) {
      y_mat[i, ] <- rbinom(n_visits, 1L, p_visit[i, ])
    }
  }
  sim <- list(
    n_sites = n_sites, n_visits = n_visits,
    elev = elev, tod_visit = tod_visit, y_mat = y_mat,
    z_true = z_true,
    truth = c(b0_state = b0_state, b1_state = b1_state,
              b0_det = b0_det, b1_det = b1_det)
  )
  saveRDS(sim, paths$sim)
  message("wrote ", paths$sim)
} else {
  sim <- readRDS(paths$sim)
  message("reusing ", paths$sim)
}

# ------------------------------------------------------------
# mvgam fit (long-format closure-unit data)
# ------------------------------------------------------------

if (!file.exists(paths$mvgam) || force) {
  mvgam_long <- data.frame(
    series = factor(rep(seq_len(sim$n_sites), each = sim$n_visits)),
    time   = 1L,
    visit  = rep(seq_len(sim$n_visits), sim$n_sites),
    y      = as.integer(t(sim$y_mat)),
    elev   = rep(sim$elev, each = sim$n_visits),
    tod_c  = as.numeric(t(sim$tod_visit)) - 12
  )
  message("fitting mvgam occ() ...")
  mvgam_fit <- mvgam(
    brms::bf(y ~ elev, p ~ tod_c),
    family = occ(),
    data   = mvgam_long,
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2L, refresh = 0L
  )
  saveRDS(mvgam_fit, paths$mvgam)
  message("wrote ", paths$mvgam)
} else {
  message("reusing ", paths$mvgam)
}

# ------------------------------------------------------------
# flocker fit (rep-varying single-season, same simulated y)
# ------------------------------------------------------------

if (!file.exists(paths$flocker) || force) {
  flocker_data <- flocker::make_flocker_data(
    obs        = sim$y_mat,
    unit_covs  = data.frame(elev = sim$elev),
    event_covs = list(tod_c = sim$tod_visit - 12)
  )
  message("fitting flocker flock() ...")
  flocker_fit <- flocker::flock(
    f_occ        = ~ elev,
    f_det        = ~ tod_c,
    flocker_data = flocker_data,
    chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2L, refresh = 0L
  )
  saveRDS(flocker_fit, paths$flocker)
  message("wrote ", paths$flocker)
} else {
  message("reusing ", paths$flocker)
}

invisible(NULL)
