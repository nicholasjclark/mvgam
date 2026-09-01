#' Simulate closure-unit detection data with known truth
#'
#' Generates training data for the closure-unit observation
#' families ([occ()], [nmix()] variants) under a small catalog of
#' fixed covariate recipes. Each recipe carries BOTH a per-site
#' state-level covariate set AND a per-visit detection-level
#' covariate set, mirroring the standard occupancy / N-mixture
#' single-season layout used in textbook examples.
#'
#' One function, switched on `family`. `data_train` is long-form
#' and ready for [mvgam()] / [jsdgam()] directly; `y_array` is
#' the 4D layout consumable by [pivot_detection_array()]. Every
#' generative parameter is exposed on `truth$` for recovery
#' checks. Latent-factor multi-species dynamics (`n_lv > 0`) are
#' supported for both `occ()` and `nmix()`.
#'
#' @param type Integer in `1:2` selecting the covariate recipe.
#'   See *Details* for the catalog.
#' @param family A closure-unit family: [occ()],
#'   [nmix()] (`"poisson_binomial"`, the default),
#'   `nmix("royle_nichols")`, or `nmix("poisson_poisson")`.
#'   Defaults to `occ()`.
#' @param n_species Integer; number of species to simulate.
#'   Defaults to `1L`. With `n_species > 1L` the per-species
#'   intercepts and slopes are sampled independently unless
#'   `n_lv > 0L`, in which case the state-level intercepts get a
#'   shared latent-factor structure (see *Latent factors*).
#' @param n_sites Integer; number of closure units per species.
#'   Defaults to `30L`.
#' @param n_visits Integer; number of visits per closure unit.
#'   Defaults to `4L`. All units share the same visit count
#'   (every unit carries the same number of visits).
#' @param n_lv Integer; number of latent factors for the
#'   multi-species joint species distribution model. Defaults to
#'   `0L` (independent species). When `n_lv > 0L` and
#'   `n_species > 1L`, the species intercepts share a low-rank
#'   loadings matrix `Z` over `n_lv` site-level latent factors,
#'   mirroring the structure [jsdgam()] estimates.
#' @param K_max Integer; per-unit cap on the latent abundance
#'   `N`. Only used when `family` is an `nmix()` variant. Defaults
#'   to `30L`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @details
#' Both recipes carry a per-site (state-level) and a per-visit
#' (detection-level) covariate set. State and detection coefs are
#' drawn from `Normal(0, 0.5)` and exposed on `truth` so users
#' can compare them against `predict(fit, type = "latent_state")`
#' and `predict(fit, type = "detection")`.
#'
#' \describe{
#'   \item{`type = 1`}{**Single covariate each side.** State
#'     linear predictor: `b0 + b1 * env` where `env ~ N(0, 1)`
#'     per site. Detection linear predictor: `c0 + c1 * tod_c`
#'     where `tod_c ~ U(-6, 6)` per visit (centred time of day).
#'     The simplest non-trivial occupancy / N-mixture recipe.}
#'   \item{`type = 2`}{**Two covariates each side.** State linear
#'     predictor: `b0 + b1 * env + b2 * elev` where `env ~ N(0, 1)`
#'     and `elev ~ N(0, 1)` per site. Detection linear
#'     predictor: `c0 + c1 * tod_c + c2 * effort` where
#'     `tod_c ~ U(-6, 6)` and `effort ~ U(0.5, 1.5)` per visit.
#'     Exercises multi-covariate identifiability on both linear
#'     predictors.}
#' }
#'
#' The state linear predictor is on the logit scale for `occ()`
#' and on the log scale for the `nmix()` variants (it determines
#' the Poisson abundance mean `lambda`). The detection linear
#' predictor is always on the logit scale for `occ()` and
#' `nmix("poisson_binomial")`, and on the log scale for the
#' Royle-Nichols (`r`) and Poisson-Poisson (`rho`) variants.
#'
#' @section Latent factors:
#'   When `n_lv > 0L` and `n_species > 1L`, the state linear
#'   predictor for species `k` at site `i` carries an extra
#'   `sum_l Z[k, l] * lv[i, l]` term where `Z ~ N(0, 0.7)` is the
#'   `[n_species x n_lv]` loadings matrix and `lv ~ N(0, 1)` are
#'   the `[n_sites x n_lv]` latent factor scores. Both are
#'   returned on `truth$loadings` / `truth$lv` so users can check
#'   that [jsdgam()] recovers the spectrum and the post-hoc QR
#'   identification (`ordinate()`) lines up with the truth.
#'
#' @return A list with class
#'   `c("mvgam_sim_closure_unit", "mvgam_sim")` carrying:
#'   \describe{
#'     \item{`data_train`}{long-format `data.frame` ready for
#'       `mvgam(family = family, data = ...)` / `jsdgam(...)`.
#'       Columns: `series` (species factor), `site` (1..n_sites),
#'       `time` (= `site` in single-season mode; this is the
#'       closure-unit identifier), `visit` (1..n_visits), `y`,
#'       the state-level covariates (broadcast across visits),
#'       the detection-level covariates (per-visit), and `cap`
#'       (= 1 for occ, = K_max for nmix).}
#'     \item{`data_test`}{`NULL` (closure-unit fits do not split
#'       on timepoints; the slot exists for `sim_mvgam` API
#'       parity).}
#'     \item{`y_array`}{integer array `[n_species, n_sites, 1,
#'       n_visits]` consumable by [pivot_detection_array()].}
#'     \item{`family`}{the closure-unit family used.}
#'     \item{`type`}{the recipe selected.}
#'     \item{`n_species`, `n_sites`, `n_visits`, `n_lv`}{the
#'       requested simulation dimensions.}
#'     \item{`truth`}{named list with all generative parameters
#'       (see *Truth bundle* below).}
#'   }
#'
#' @section Truth bundle:
#'   `truth$` slots are aligned with the post-fit extractors so
#'   recovery checks read naturally:
#'   \describe{
#'     \item{`psi` (occ) / `lambda` (nmix)}{`[n_species x n_sites]`
#'       matrix of true state probability / expected abundance.
#'       Matches `predict(fit, type = "latent_state",
#'       summary = TRUE)` aggregated to the unit grain.}
#'     \item{`z` (occ) / `N` (nmix)}{`[n_species x n_sites]`
#'       matrix of true latent state realisations.}
#'     \item{`p`}{`[n_species x n_sites x n_visits]` array of true
#'       per-visit detection probability (or RN per-individual
#'       `r` / PPM `rho`, depending on the nmix variant).}
#'     \item{`state_coefs`}{named numeric vector
#'       (`n_species == 1L`) or `[n_species x p]` matrix
#'       (`n_species > 1L`) with `intercept` plus per-recipe
#'       state-level slopes.}
#'     \item{`detection_coefs`}{same shape, for the detection
#'       linear predictor.}
#'     \item{`loadings`}{`[n_species x n_lv]` matrix `Z` (only
#'       when `n_lv > 0L`).}
#'     \item{`lv`}{`[n_sites x n_lv]` site-level latent factor
#'       scores (only when `n_lv > 0L`).}
#'   }
#'
#' @author Nicholas J Clark
#'
#' @seealso [sim_mvgam()] for state-space time-series
#'   simulation; [pivot_detection_array()] to re-pivot the
#'   returned `y_array` with custom covariate layouts;
#'   [occ()], [nmix()] for the families this simulator targets;
#'   [`jsdgam()`] for the multi-species joint fitter the
#'   `n_lv > 0L` output feeds into.
#'
#' @examples
#' # Four-species occupancy fixture with state-level env / elev
#' # covariates and visit-level tod_c / effort covariates
#' # (recipe 2L).
#' set.seed(1)
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 4L,
#'   n_sites   = 50L,
#'   n_visits  = 4L,
#'   type      = 2L
#' )
#' head(simdat$data_train)
#' summary(simdat)
#'
#' @export
sim_closure_unit_data <- function(type = 1L,
                                    family = occ(),
                                    n_species = 1L,
                                    n_sites = 30L,
                                    n_visits = 4L,
                                    n_lv = 0L,
                                    K_max = 30L,
                                    seed = NULL) {
  checkmate::assert_int(type, lower = 1L, upper = 2L)
  checkmate::assert_int(n_species, lower = 1L)
  checkmate::assert_int(n_sites, lower = 2L)
  checkmate::assert_int(n_visits, lower = 1L)
  checkmate::assert_int(n_lv, lower = 0L)
  checkmate::assert_int(K_max, lower = 1L)
  if (n_lv > 0L && n_species < 2L) {
    stop(insight::format_error(c(
      "n_lv > 0 requires n_species >= 2.",
      i = "Latent factors share structure across species; with one species the loadings are not identified."
    )))
  }
  fam_name <- resolve_family_name(family) %||% ""
  if (!is_closure_unit_family(family)) {
    stop(insight::format_error(c(
      "'family' must be a closure-unit family.",
      x = paste0("Got family '", fam_name %||% "?", "'."),
      i = "Use family = occ() or family = nmix() / nmix('royle_nichols') / nmix('poisson_poisson')."
    )))
  }

  local_seed(seed)

  recipe <- closure_unit_recipe(type)

  # Per-site state-level covariates (broadcast across visits).
  site_X <- vapply(
    recipe$site_covs,
    function(s) s$sampler(n_sites),
    numeric(n_sites)
  )
  if (n_sites == 1L) site_X <- matrix(site_X, nrow = 1L)
  colnames(site_X) <- names(recipe$site_covs)

  # Per-visit detection-level covariates: sampled independently
  # for each (site, visit) cell so we exercise the per-visit
  # detection sub-formula path. Each cov is stored as a
  # [n_sites x n_visits] matrix and unrolled into a length
  # n_sites * n_visits vector when joined to the long data.
  visit_X <- lapply(
    recipe$visit_covs,
    function(s) matrix(
      s$sampler(n_sites * n_visits),
      nrow = n_sites, ncol = n_visits
    )
  )
  names(visit_X) <- names(recipe$visit_covs)

  # State-level coefs: per species [n_species x (intercept +
  # p_site)]. The state side runs through the family link, so the
  # draw scale is family-aware: occ (logit link) saturates anyway,
  # so wide draws are fine; nmix (log link) explodes lambda when
  # slopes exceed ~1.5, which both saturates the K_max
  # marginalisation ceiling and gives the fit nothing useful to
  # recover. Detection-side draws stay on the wider scale (the
  # detection link is logit for occ / nmix("poisson_binomial")
  # and log for the other nmix variants, but per-visit covariates
  # vary on a tighter empirical range than per-site covariates).
  state_coefs       <- draw_recipe_coefs(
    n_species, names(recipe$site_covs), family = family,
    is_state = TRUE
  )
  detection_coefs   <- draw_recipe_coefs(
    n_species, names(recipe$visit_covs), family = family,
    is_state = FALSE
  )
  p_site  <- length(recipe$site_covs)
  p_visit <- length(recipe$visit_covs)

  # State linear predictor at every (species, site): intercept +
  # site covariate contribution. Optional latent-factor term adds
  # `sum_l Z[k, l] * lv[i, l]` to species k at site i.
  state_lp_obs <- if (p_site > 0L) site_X %*% t(state_coefs[, -1L,
                                                              drop = FALSE]) else
    matrix(0, nrow = n_sites, ncol = n_species)
  state_lp <- t(state_lp_obs) +
                outer(state_coefs[, 1L],
                       rep(1, n_sites))
  loadings <- NULL
  lv_scores <- NULL
  if (n_lv > 0L) {
    loadings <- matrix(
      stats::rnorm(n_species * n_lv, mean = 0, sd = 0.7),
      nrow = n_species, ncol = n_lv
    )
    lv_scores <- matrix(
      stats::rnorm(n_sites * n_lv, mean = 0, sd = 1),
      nrow = n_sites, ncol = n_lv
    )
    state_lp <- state_lp + loadings %*% t(lv_scores)
  }

  # Detection linear predictor at every (species, site, visit).
  # detection_coefs has intercept + p_visit slopes per species.
  # For each species k: det_lp[k, i, j] = c0[k] +
  #   sum_q c_q[k] * visit_X[[q]][i, j].
  det_lp <- array(0, dim = c(n_species, n_sites, n_visits))
  for (k in seq_len(n_species)) {
    cont <- matrix(detection_coefs[k, 1L],
                   nrow = n_sites, ncol = n_visits)
    if (p_visit > 0L) {
      for (q in seq_len(p_visit)) {
        cont <- cont +
          detection_coefs[k, 1L + q] * visit_X[[q]]
      }
    }
    det_lp[k, , ] <- cont
  }

  # Sample the latent state and the observations per family.
  sim_components <- closure_unit_sample(
    family   = family,
    state_lp = state_lp,
    det_lp   = det_lp,
    n_species = n_species, n_sites = n_sites, n_visits = n_visits,
    K_max = K_max
  )

  # Long-format data ready for mvgam() / jsdgam(). The closure
  # unit is (series, time): each species's site index is the
  # `time` slot (single-season). The cap column tracks K_max for
  # nmix variants (occ() carries its default_cap = 1L on the
  # family attr, but writing it explicitly makes the data
  # standalone for users who want to inspect or modify it).
  long <- expand.grid(
    visit = seq_len(n_visits),
    site  = seq_len(n_sites),
    series = factor(paste0("sp_", seq_len(n_species)),
                     levels = paste0("sp_", seq_len(n_species))),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  long$time <- long$site
  long$y    <- sim_components$y_long
  long$cap  <- if (grepl("^nmix", fam_name)) {
    rep(K_max, NROW(long))
  } else {
    rep(1L, NROW(long))
  }
  # Broadcast site covariates by (series, time = site).
  if (p_site > 0L) {
    for (q in seq_len(p_site)) {
      nm <- colnames(site_X)[q]
      long[[nm]] <- site_X[long$site, q]
    }
  }
  # Per-visit covariates index by (site, visit).
  if (p_visit > 0L) {
    for (q in seq_len(p_visit)) {
      nm <- names(visit_X)[q]
      mat <- visit_X[[q]]
      long[[nm]] <- mat[cbind(long$site, long$visit)]
    }
  }
  # Sort to (series, site, visit) order so the canonical
  # build_closure_unit_arrays() grouping (series, time) reads
  # the K visits per unit in order.
  long <- long[order(long$series, long$time, long$visit), ,
                drop = FALSE]
  rownames(long) <- NULL

  # 4D array for pivot_detection_array() round-trip: [N, J, T=1, K].
  y_array <- array(
    sim_components$y_array,
    dim = c(n_species, n_sites, 1L, n_visits),
    dimnames = list(
      paste0("sp_", seq_len(n_species)),
      paste0("site_", seq_len(n_sites)),
      "t1",
      paste0("visit_", seq_len(n_visits))
    )
  )

  truth <- list(
    state_coefs     = if (n_species == 1L) {
      setNames(state_coefs[1L, ], colnames(state_coefs))
    } else state_coefs,
    detection_coefs = if (n_species == 1L) {
      setNames(detection_coefs[1L, ], colnames(detection_coefs))
    } else detection_coefs,
    p               = sim_components$p_array
  )
  truth <- c(truth, sim_components$state_truth)
  if (!is.null(loadings)) {
    truth$loadings <- loadings
    truth$lv       <- lv_scores
  }

  structure(
    list(
      data_train  = long,
      data_test   = NULL,
      y_array     = y_array,
      family      = family,
      type        = type,
      n_species   = n_species,
      n_sites     = n_sites,
      n_visits    = n_visits,
      n_lv        = n_lv,
      truth       = truth
    ),
    class = c("mvgam_sim_closure_unit", "mvgam_sim")
  )
}


# Recipe catalog: each entry is a list of (site_covs, visit_covs)
# where each cov is a list(name, sampler) pair. Sampler returns a
# numeric vector of the requested length. The dispatcher and the
# roxygen catalog read from this same list so they cannot drift.
#'@noRd
closure_unit_recipe <- function(type) {
  recipes <- list(
    list(
      site_covs  = list(env = list(sampler = function(n) {
        stats::rnorm(n, 0, 1)
      })),
      visit_covs = list(tod_c = list(sampler = function(n) {
        stats::runif(n, -6, 6)
      }))
    ),
    list(
      site_covs  = list(
        env  = list(sampler = function(n) stats::rnorm(n, 0, 1)),
        elev = list(sampler = function(n) stats::rnorm(n, 0, 1))
      ),
      visit_covs = list(
        tod_c  = list(sampler = function(n) stats::runif(n, -6, 6)),
        effort = list(sampler = function(n) stats::runif(n, 0.5, 1.5))
      )
    )
  )
  recipes[[type]]
}


# Per-species coef matrix [n_species x (1 + length(cov_names))].
# Per-covariate community mean drawn once from N(0, 1.5), then
# per-species slopes drawn around that mean with sd 0.5. A clear
# community-level direction is recoverable by `conditional_effects`
# even with small fixtures, while species-level departures around
# the mean give the factor model and `residual_cor` something
# non-trivial to recover.
#'@noRd
draw_recipe_coefs <- function(n_species, cov_names,
                                family = NULL, is_state = TRUE) {
  p <- length(cov_names)
  # Tame nmix state-side draws (log link explodes lambda past
  # K_max if slopes drift past ~1.5). Detection-side draws also
  # stay tight because the recipe's per-visit covariates carry
  # wide empirical ranges (`tod_c ~ U(-6, 6)`); wide logit slopes
  # against that range push detection probability to 0 or 1 and
  # leave the fit nothing to recover. Logit-link state for occ()
  # uses the wider draws since the link saturates anyway and a
  # wider draw gives more visible occupancy variation.
  fam_name <- if (is.null(family)) "" else resolve_family_name(family) %||% ""
  is_nmix <- grepl("^nmix", fam_name)
  is_closure <- is_nmix || identical(fam_name, "occ")
  if (is_state && is_nmix) {
    # nmix log-link state: tightest draws.
    intercept_sd      <- 0.5
    community_mean_sd <- 0.4
    slope_sd          <- 0.3
  } else if (!is_state && is_closure) {
    # Detection side (occ logit, nmix logit / log): tame to keep
    # detection probability / encounter rate within recoverable
    # range across the visit-covariate U(-6, 6) span.
    intercept_sd      <- 0.5
    community_mean_sd <- 0.2
    slope_sd          <- 0.15
  } else {
    # Original wider draws (occ logit-state default; non-closure
    # users that may call this helper indirectly).
    intercept_sd      <- 0.75
    community_mean_sd <- 1.5
    slope_sd          <- 0.5
  }
  intercept <- stats::rnorm(n_species, mean = 0, sd = intercept_sd)
  if (p == 0L) {
    out <- matrix(intercept, nrow = n_species, ncol = 1L)
    colnames(out) <- "intercept"
    return(out)
  }
  community_mean <- stats::rnorm(p, mean = 0, sd = community_mean_sd)
  slopes <- matrix(
    stats::rnorm(n_species * p,
                 mean = rep(community_mean, each = n_species),
                 sd = slope_sd),
    nrow = n_species, ncol = p
  )
  out <- cbind(intercept, slopes)
  colnames(out) <- c("intercept", cov_names)
  out
}


# Per-family latent-state + observation kernel. Returns y in two
# layouts (long vector aligned with the long-format data, and 4D
# array aligned with pivot_detection_array()) plus the family-
# specific truth slots (psi / z for occ, lambda / N for nmix).
#'@noRd
closure_unit_sample <- function(family,
                                  state_lp, det_lp,
                                  n_species, n_sites, n_visits,
                                  K_max) {
  fam_name <- resolve_family_name(family) %||% ""
  y_arr <- array(0L, dim = c(n_species, n_sites, n_visits))
  p_arr <- array(0, dim = c(n_species, n_sites, n_visits))
  state_truth <- list()
  switch(
    fam_name,
    "occ" = {
      psi <- plogis(state_lp)
      z   <- matrix(stats::rbinom(n_species * n_sites, 1L,
                                   as.numeric(psi)),
                    nrow = n_species, ncol = n_sites)
      p_arr[] <- plogis(det_lp)
      for (k in seq_len(n_species)) {
        for (i in seq_len(n_sites)) {
          if (z[k, i] == 1L) {
            y_arr[k, i, ] <- stats::rbinom(n_visits, 1L, p_arr[k, i, ])
          }
        }
      }
      state_truth <- list(psi = psi, z = z)
    },
    "nmix" = , "nmix_poisson_binomial" = {
      lambda <- exp(state_lp)
      N      <- matrix(stats::rpois(n_species * n_sites,
                                     as.numeric(lambda)),
                       nrow = n_species, ncol = n_sites)
      N[N > K_max] <- K_max
      p_arr[] <- plogis(det_lp)
      for (k in seq_len(n_species)) {
        for (i in seq_len(n_sites)) {
          if (N[k, i] > 0L) {
            y_arr[k, i, ] <- stats::rbinom(n_visits, N[k, i],
                                            p_arr[k, i, ])
          }
        }
      }
      state_truth <- list(lambda = lambda, N = N)
    },
    "nmix_royle_nichols" = {
      lambda <- exp(state_lp)
      N      <- matrix(stats::rpois(n_species * n_sites,
                                     as.numeric(lambda)),
                       nrow = n_species, ncol = n_sites)
      r_arr <- array(plogis(det_lp),
                     dim = c(n_species, n_sites, n_visits))
      p_arr[] <- 1 - (1 - r_arr) ^ array(N, dim = dim(r_arr))
      for (k in seq_len(n_species)) {
        for (i in seq_len(n_sites)) {
          y_arr[k, i, ] <- stats::rbinom(n_visits, 1L, p_arr[k, i, ])
        }
      }
      state_truth <- list(lambda = lambda, N = N)
    },
    "nmix_poisson_poisson" = {
      lambda <- exp(state_lp)
      N      <- matrix(stats::rpois(n_species * n_sites,
                                     as.numeric(lambda)),
                       nrow = n_species, ncol = n_sites)
      rho_arr <- array(exp(det_lp),
                       dim = c(n_species, n_sites, n_visits))
      p_arr[] <- rho_arr
      for (k in seq_len(n_species)) {
        for (i in seq_len(n_sites)) {
          if (N[k, i] > 0L) {
            mean_y <- N[k, i] * rho_arr[k, i, ]
            y_arr[k, i, ] <- stats::rpois(n_visits, mean_y)
          }
        }
      }
      state_truth <- list(lambda = lambda, N = N)
    },
    stop(insight::format_error(c(
      "Unsupported closure-unit family for sim_closure_unit_data().",
      x = paste0("Got family '", fam_name, "'.")
    )))
  )
  # Long-form y aligned with expand.grid(visit, site, series):
  # the array iterates [species, site, visit] which matches
  # vectorisation when species is the outermost dim.
  y_long <- as.integer(aperm(y_arr, c(3L, 2L, 1L)))
  list(
    y_array    = y_arr,
    p_array    = p_arr,
    y_long     = y_long,
    state_truth = state_truth
  )
}


#' Summary method for sim_closure_unit_data() output
#'
#' Specialised summary that prints the closure-unit dimensions,
#' the recipe, the per-species recovery targets, and a one-line
#' detection-rate diagnostic so users can sanity-check that the
#' simulated data is non-degenerate (e.g. not all-zeros or all-
#' detected).
#'
#' @param object A `mvgam_sim_closure_unit` list returned by
#'   [sim_closure_unit_data()].
#' @param ... Currently ignored.
#'
#' @return An object of class `mvgam_sim_closure_unit_summary`: a list
#'   of per-unit detection and abundance summaries with its own
#'   [print()][print.mvgam_sim_closure_unit_summary] method.
#'
#' @method summary mvgam_sim_closure_unit
#' @export
summary.mvgam_sim_closure_unit <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_sim_closure_unit")
  truth <- object$truth
  fam_name <- resolve_family_name(object$family) %||% "?"
  state_label <- if (!is.null(truth$psi)) "psi" else "lambda"
  state_summary <- if (!is.null(truth$psi)) {
    range(truth$psi)
  } else {
    range(truth$lambda)
  }
  det_summary <- range(truth$p)
  obs_rate <- mean(object$y_array > 0L)
  structure(
    list(
      family            = fam_name,
      type              = object$type,
      n_species         = object$n_species,
      n_sites           = object$n_sites,
      n_visits          = object$n_visits,
      n_lv              = object$n_lv,
      state_label       = state_label,
      state_range       = state_summary,
      detection_range   = det_summary,
      non_zero_fraction = obs_rate,
      state_coefs       = truth$state_coefs,
      detection_coefs   = truth$detection_coefs,
      has_loadings      = !is.null(truth$loadings)
    ),
    class = "mvgam_sim_closure_unit_summary"
  )
}


#' Print method for sim_closure_unit_data() summary output
#'
#' @param x A `mvgam_sim_closure_unit_summary` object.
#' @param digits Integer; significant digits for printed numbers.
#'   Default `3`.
#' @param ... Currently ignored.
#'
#' @return The `mvgam_sim_closure_unit_summary` object `x`, returned
#'   invisibly.
#'
#' @method print mvgam_sim_closure_unit_summary
#' @export
print.mvgam_sim_closure_unit_summary <- function(x, digits = 3L,
                                                    ...) {
  checkmate::assert_class(x, "mvgam_sim_closure_unit_summary")
  checkmate::assert_int(digits, lower = 0L)
  cat("Simulated closure-unit dataset (sim_closure_unit_data type ",
      x$type, ")\n", sep = "")
  cat("  Family       : ", x$family, "\n", sep = "")
  cat("  Species      : ", x$n_species, "\n", sep = "")
  cat("  Sites        : ", x$n_sites, "\n", sep = "")
  cat("  Visits/unit  : ", x$n_visits, "\n", sep = "")
  if (x$n_lv > 0L) {
    cat("  Latent factors: ", x$n_lv, "\n", sep = "")
  }
  cat("\nTrue generative parameters\n")
  cat("  ", x$state_label, " range: [",
      format(round(x$state_range[1L], digits), nsmall = digits),
      ", ",
      format(round(x$state_range[2L], digits), nsmall = digits),
      "]\n", sep = "")
  cat("  Detection range: [",
      format(round(x$detection_range[1L], digits), nsmall = digits),
      ", ",
      format(round(x$detection_range[2L], digits), nsmall = digits),
      "]\n", sep = "")
  cat("  Non-zero observation fraction: ",
      format(round(x$non_zero_fraction, digits), nsmall = digits),
      "\n", sep = "")
  invisible(x)
}
