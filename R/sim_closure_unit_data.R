#' Simulate closure-unit detection data with known truth
#'
#' Simulates single-season data for the closure-unit families, [occ()]
#' and the [nmix()] variants. Each covariate recipe holds per-site
#' covariates for the latent state and per-visit covariates for
#' detection. `data_train` is the long frame [mvgam()] and [jsdgam()]
#' take. `y_array` is the four-dimensional layout of
#' [pivot_detection_array()]. `truth` holds every generative parameter
#' for recovery checks.
#'
#' @param type Integer in `1:2` selecting the covariate recipe.
#'   See *Details*.
#' @param family A closure-unit family: [occ()] (the default),
#'   [nmix()] (`"poisson_binomial"`), `nmix("royle_nichols")` or
#'   `nmix("poisson_poisson")`.
#' @param n_species Integer number of species. Defaults to `1L`.
#' @param n_sites Integer number of closure units per species. Defaults
#'   to `30L`.
#' @param n_visits Integer number of visits to every closure unit.
#'   Defaults to `4L`.
#' @param n_lv Integer number of latent factors shared by the species'
#'   state intercepts. Defaults to `0L`, independent species. Requires
#'   `n_species > 1L`. See *Latent factors*.
#' @param K_max Integer cap on the latent abundance `N` of an `nmix()`
#'   family. Defaults to `30L`.
#' @param seed Optional integer seed. The session's random state is
#'   restored when the call returns.
#'
#' @details
#' \describe{
#'   \item{`type = 1`}{State predictor `b0 + b1 * env`, with
#'     `env ~ N(0, 1)` per site. Detection predictor `c0 + c1 * tod_c`,
#'     with `tod_c ~ U(-6, 6)` per visit, a centred time of day.}
#'   \item{`type = 2`}{State predictor `b0 + b1 * env + b2 * elev`, with
#'     `env` and `elev` each `N(0, 1)` per site. Detection predictor
#'     `c0 + c1 * tod_c + c2 * effort`, with `tod_c ~ U(-6, 6)` and
#'     `effort ~ U(0.5, 1.5)` per visit.}
#' }
#'
#' Each predictor is on the scale of the family's own link. The state
#' predictor takes the logit for `occ()` and the log for every `nmix()`
#' variant, where it sets the Poisson abundance mean `lambda`. The
#' detection predictor takes the logit for `occ()`,
#' `nmix("poisson_binomial")` and `nmix("royle_nichols")`. It takes the
#' log for `nmix("poisson_poisson")`, whose encounter rate `rho` is
#' unbounded.
#'
#' Each species draws an intercept from a normal with mean zero. Each
#' covariate draws one community mean. Each species then draws its
#' slope around that mean. The standard deviations are:
#'
#' | Predictor | Intercept | Community mean | Species slope |
#' |---|---|---|---|
#' | `occ()` state | 0.75 | 1.5 | 0.5 |
#' | `nmix()` state | 0.5 | 0.4 | 0.3 |
#' | Detection | 0.5 | 0.2 | 0.15 |
#'
#' The `nmix()` state scales keep `lambda` well below `K_max`. The
#' detection scales keep detection away from 0 and 1 across the range of
#' `tod_c`.
#'
#' @section Latent factors:
#'   With `n_lv > 0L`, the state predictor of species `k` at site `i`
#'   gains `sum_l Z[k, l] * lv[i, l]`. The `[n_species x n_lv]`
#'   loadings `Z` are normal with standard deviation 0.7. The
#'   `[n_sites x n_lv]` factor scores `lv` are standard normal.
#'   `truth$loadings` and `truth$lv` return both for comparison with
#'   [jsdgam()] estimates and [ordinate()].
#'
#' @return A list of class `c("mvgam_sim_closure_unit", "mvgam_sim")`:
#'   \describe{
#'     \item{`data_train`}{Long `data.frame`. Its columns are `series`
#'       (the species), `site`, `time` (the site, which identifies the
#'       closure unit in a single season), `visit`, `y`, the site and
#'       visit covariates, then `cap`: 1 for `occ()`, `K_max` for
#'       `nmix()`.}
#'     \item{`data_test`}{`NULL`. Closure-unit data have no held-out
#'       times.}
#'     \item{`y_array`}{Integer array
#'       `[n_species, n_sites, 1, n_visits]` for
#'       [pivot_detection_array()].}
#'     \item{`family`, `type`}{The family and recipe.}
#'     \item{`n_species`, `n_sites`, `n_visits`, `n_lv`}{The
#'       simulation dimensions.}
#'     \item{`truth`}{The generative parameters. See *Truth bundle*.}
#'   }
#'
#' @section Truth bundle:
#'   \describe{
#'     \item{`psi` (occ) or `lambda` (nmix)}{`[n_species x n_sites]`
#'       matrix of occupancy probabilities or expected abundances.}
#'     \item{`z` (occ) or `N` (nmix)}{`[n_species x n_sites]` matrix of
#'       the realised latent states that
#'       `predict(fit, type = "latent_state")` estimates.}
#'     \item{`p`}{`[n_species x n_sites x n_visits]` array of the
#'       detection parameter that `predict(fit, type = "detection")`
#'       estimates: the detection probability, the per-individual
#'       probability `r` of `nmix("royle_nichols")` or the encounter
#'       rate `rho` of `nmix("poisson_poisson")`.}
#'     \item{`state_coefs`, `detection_coefs`}{The intercept and slopes
#'       of each predictor. A named vector for one species, an
#'       `[n_species x (1 + p)]` matrix for several.}
#'     \item{`loadings`, `lv`}{`Z` and the factor scores, when
#'       `n_lv > 0L`.}
#'   }
#'
#' @author Nicholas J Clark
#'
#' @seealso [sim_mvgam()] simulates state-space time series.
#'   [sim_jsdgam()] simulates site-by-species data without a detection
#'   process. The
#'   `r mvgam_article("nmix", "N-mixture article")` fits occupancy and
#'   N-mixture models to data of this layout.
#'
#' @examples
#' # Four species with two site and two visit covariates
#' simdat <- sim_closure_unit_data(
#'   family    = occ(),
#'   n_species = 4L,
#'   n_sites   = 50L,
#'   n_visits  = 4L,
#'   type      = 2L,
#'   seed      = 1L
#' )
#' head(simdat$data_train)
#' summary(simdat)
#'
#' # Next: fit these data with jsdgam(), as the shared_variation()
#' # example does
#'
#' @family simulation
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
      i = "With one species the loadings are not identified."
    )), call. = FALSE)
  }
  if (!is_closure_unit_family(family)) {
    stop(insight::format_error(c(
      "'family' must be a closure-unit family.",
      i = paste0("Use occ(), nmix(), nmix('royle_nichols') or ",
                 "nmix('poisson_poisson').")
    )), call. = FALSE)
  }

  local_seed(seed)

  recipe <- closure_unit_recipe(type)
  site_X <- vapply(recipe$site_covs, function(sampler) sampler(n_sites),
                   numeric(n_sites))
  # Each visit covariate is an [n_sites x n_visits] matrix
  visit_X <- lapply(recipe$visit_covs, function(sampler) {
    matrix(sampler(n_sites * n_visits), nrow = n_sites, ncol = n_visits)
  })

  state_sds <- if (models_latent_count(family)) {
    c(0.5, 0.4, 0.3)
  } else {
    c(0.75, 1.5, 0.5)
  }
  state_coefs <- draw_recipe_coefs(n_species, colnames(site_X),
                                   state_sds)
  detection_coefs <- draw_recipe_coefs(n_species, names(visit_X),
                                       c(0.5, 0.2, 0.15))

  # [n_species x n_sites] state predictor
  state_lp <- state_coefs[, 1L] +
    state_coefs[, -1L, drop = FALSE] %*% t(site_X)
  loadings <- NULL
  lv_scores <- NULL
  if (n_lv > 0L) {
    loadings <- matrix(stats::rnorm(n_species * n_lv, sd = 0.7),
                       nrow = n_species, ncol = n_lv)
    lv_scores <- matrix(stats::rnorm(n_sites * n_lv),
                        nrow = n_sites, ncol = n_lv)
    state_lp <- state_lp + loadings %*% t(lv_scores)
  }

  # [n_species x n_sites x n_visits] detection predictor
  det_lp <- array(detection_coefs[, 1L],
                  dim = c(n_species, n_sites, n_visits))
  for (q in seq_along(visit_X)) {
    det_lp <- det_lp + outer(detection_coefs[, 1L + q], visit_X[[q]])
  }

  sim_components <- closure_unit_sample(
    family, state_lp, det_lp, K_max = K_max
  )

  # The closure unit is (series, time). With one season each site is
  # one unit, so `time` is the site index.
  species <- paste0("sp_", seq_len(n_species))
  long <- expand.grid(
    visit = seq_len(n_visits),
    site = seq_len(n_sites),
    series = factor(species, levels = species),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  long$time <- long$site
  long$y <- sim_components$y_long
  long$cap <- if (models_latent_count(family)) K_max else 1L
  for (nm in colnames(site_X)) {
    long[[nm]] <- site_X[long$site, nm]
  }
  for (nm in names(visit_X)) {
    long[[nm]] <- visit_X[[nm]][cbind(long$site, long$visit)]
  }
  long <- long[order(long$series, long$time, long$visit), ,
               drop = FALSE]
  rownames(long) <- NULL

  y_array <- array(
    sim_components$y_array,
    dim = c(n_species, n_sites, 1L, n_visits),
    dimnames = list(
      species,
      paste0("site_", seq_len(n_sites)),
      "t1",
      paste0("visit_", seq_len(n_visits))
    )
  )

  # One species reports its coefficients as a named vector
  as_truth_coefs <- function(coefs) {
    if (n_species == 1L) coefs[1L, ] else coefs
  }
  truth <- c(
    list(
      state_coefs = as_truth_coefs(state_coefs),
      detection_coefs = as_truth_coefs(detection_coefs),
      p = sim_components$p_array
    ),
    sim_components$state_truth
  )
  if (!is.null(loadings)) {
    truth$loadings <- loadings
    truth$lv <- lv_scores
  }

  structure(
    list(
      data_train = long,
      data_test = NULL,
      y_array = y_array,
      family = family,
      type = type,
      n_species = n_species,
      n_sites = n_sites,
      n_visits = n_visits,
      n_lv = n_lv,
      truth = truth
    ),
    class = c("mvgam_sim_closure_unit", "mvgam_sim")
  )
}


#' The covariate samplers of a recipe
#'
#' @param type Integer recipe number
#' @return List of named `site_covs` and `visit_covs` samplers, each a
#'   function of the number of values to draw
#' @noRd
closure_unit_recipe <- function(type) {
  recipes <- list(
    list(
      site_covs = list(env = function(n) stats::rnorm(n)),
      visit_covs = list(tod_c = function(n) stats::runif(n, -6, 6))
    ),
    list(
      site_covs = list(
        env = function(n) stats::rnorm(n),
        elev = function(n) stats::rnorm(n)
      ),
      visit_covs = list(
        tod_c = function(n) stats::runif(n, -6, 6),
        effort = function(n) stats::runif(n, 0.5, 1.5)
      )
    )
  )
  recipes[[type]]
}


#' Per-species intercepts and slopes around community means
#'
#' Each covariate draws one community mean. Each species draws its
#' slope around it.
#'
#' @param n_species Integer number of species
#' @param cov_names Character names of the covariates
#' @param sds Numeric standard deviations of the intercept, the
#'   community mean and the species slope
#' @return `[n_species x (1 + length(cov_names))]` matrix
#' @noRd
draw_recipe_coefs <- function(n_species, cov_names, sds) {
  checkmate::assert_count(n_species, positive = TRUE)
  checkmate::assert_character(cov_names, min.len = 1L)
  checkmate::assert_numeric(sds, len = 3L, lower = 0)
  intercept <- stats::rnorm(n_species, sd = sds[1L])
  community_mean <- stats::rnorm(length(cov_names), sd = sds[2L])
  slopes <- matrix(
    stats::rnorm(n_species * length(cov_names),
                 mean = rep(community_mean, each = n_species),
                 sd = sds[3L]),
    nrow = n_species
  )
  out <- cbind(intercept, slopes)
  colnames(out) <- c("intercept", cov_names)
  out
}


#' Latent states and detections drawn through the family's links
#'
#' The predictors pass through the links the family's likelihood
#' inverts. An `nmix()` abundance is capped at `K_max`, the bound its
#' likelihood marginalises to. Royle-Nichols detects a unit on a visit
#' when any of its `N` individuals is detected, with probability
#' `1 - (1 - r)^N`.
#'
#' @param family A closure-unit family
#' @param state_lp Numeric `[n_species x n_sites]` state predictor
#' @param det_lp Numeric `[n_species x n_sites x n_visits]` detection
#'   predictor
#' @param K_max Integer cap on the abundance
#' @return List of `y_array` and `p_array` (shaped as `det_lp`),
#'   `y_long` (in the order of `expand.grid(visit, site, series)`) and
#'   the family's `state_truth`
#' @noRd
closure_unit_sample <- function(family, state_lp, det_lp, K_max) {
  fam_name <- resolve_family_name(family)
  dims <- dim(det_lp)
  n_species <- dims[1L]
  n_sites <- dims[2L]
  n_visits <- dims[3L]
  y_arr <- array(0L, dim = dims)
  state <- state_lp
  state[] <- stats::make.link(family$link)$linkinv(state_lp)
  detect <- det_lp
  detect[] <- stats::make.link(family$link_p)$linkinv(det_lp)
  if (identical(fam_name, "occ")) {
    z <- matrix(stats::rbinom(n_species * n_sites, 1L, as.numeric(state)),
                nrow = n_species, ncol = n_sites)
    for (k in seq_len(n_species)) {
      for (i in seq_len(n_sites)) {
        if (z[k, i] == 1L) {
          y_arr[k, i, ] <- stats::rbinom(n_visits, 1L, detect[k, i, ])
        }
      }
    }
    state_truth <- list(psi = state, z = z)
  } else {
    N <- matrix(pmin(stats::rpois(n_species * n_sites, as.numeric(state)),
                     K_max),
                nrow = n_species, ncol = n_sites)
    draw_visits <- switch(
      fam_name,
      nmix_royle_nichols = function(N, r) {
        stats::rbinom(n_visits, 1L, 1 - (1 - r)^N)
      },
      nmix_poisson_poisson = function(N, rho) {
        stats::rpois(n_visits, N * rho)
      },
      function(N, p) stats::rbinom(n_visits, N, p)
    )
    for (k in seq_len(n_species)) {
      for (i in seq_len(n_sites)) {
        if (identical(fam_name, "nmix_royle_nichols") || N[k, i] > 0L) {
          y_arr[k, i, ] <- draw_visits(N[k, i], detect[k, i, ])
        }
      }
    }
    state_truth <- list(lambda = state, N = N)
  }
  list(
    y_array = y_arr,
    p_array = detect,
    y_long = as.integer(aperm(y_arr, c(3L, 2L, 1L))),
    state_truth = state_truth
  )
}


#' Summary method for sim_closure_unit_data() output
#'
#' The dimensions, the recipe, the ranges of the true state and
#' detection parameters and the share of non-zero observations. A share
#' of 0 or 1 marks data with no information on detection.
#'
#' @param object A `mvgam_sim_closure_unit` list returned by
#'   [sim_closure_unit_data()].
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class `mvgam_sim_closure_unit_summary`: a list
#'   of per-unit detection and abundance summaries with its own
#'   [print()][print.mvgam_sim_closure_unit_summary] method.
#'
#' @method summary mvgam_sim_closure_unit
#' @export
summary.mvgam_sim_closure_unit <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_sim_closure_unit")
  rlang::check_dots_empty()
  truth <- object$truth
  state_label <- if (!is.null(truth$psi)) "psi" else "lambda"
  structure(
    list(
      family            = resolve_family_name(object$family),
      type              = object$type,
      n_species         = object$n_species,
      n_sites           = object$n_sites,
      n_visits          = object$n_visits,
      n_lv              = object$n_lv,
      state_label       = state_label,
      state_range       = range(truth[[state_label]]),
      detection_range   = range(truth$p),
      non_zero_fraction = mean(object$y_array > 0L),
      state_coefs       = truth$state_coefs,
      detection_coefs   = truth$detection_coefs,
      has_loadings      = !is.null(truth$loadings)
    ),
    class = "mvgam_sim_closure_unit_summary"
  )
}


#' Print method for sim_closure_unit_data() summary output
#'
#' @inheritParams print.mvgam_sim_summary
#' @param x A `mvgam_sim_closure_unit_summary` object.
#' @param ... Unused.
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
