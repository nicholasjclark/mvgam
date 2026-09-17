# Internal: softmax over a vector of linear predictors. The maximum
# is subtracted before exponentiating, which keeps `exp()` finite for
# a predictor of any magnitude and leaves the result unchanged: the
# transform is invariant to a shift shared by every component.
#'@noRd
softmax <- function(x) {
  checkmate::assert_numeric(x, min.len = 1L, any.missing = FALSE)
  e <- exp(x - max(x))
  e / sum(e)
}


# Internal: draw one unit's worth of responses from a `K`-vector of
# linear predictors.
#
# Families whose components are drawn independently go to
# `sim_family_rng()`, which already covers every such family mvgam
# fits. The branches here are the ones a per-row sampler cannot
# express: `mvn()` and `mvt()` add a per-species residual to a shared
# unit, and the composition families draw the K components together
# from one softmax.
#'@noRd
sim_jsdm_draw <- function(eta, family, pars = list()) {
  K <- length(eta)
  switch(
    resolve_family_name(family),
    "mvn" = eta + stats::rnorm(K, sd = pars$Psi %||% 0.5),
    "mvt" = eta + (pars$Psi %||% 0.5) *
      stats::rt(K, df = pars$nu %||% 5),
    "diri" = {
      shape <- softmax(eta) * (pars$phi %||% 30)
      g <- stats::rgamma(K, shape = shape, rate = 1)
      g / sum(g)
    },
    "multi" = as.numeric(stats::rmultinom(
      1L, pars$unit_total %||% 30L, softmax(eta)
    )),
    "categ" = {
      z <- numeric(K)
      z[sample.int(K, 1L, prob = softmax(eta))] <- 1
      z
    },
    sim_family_rng(eta, family, pars)
  )
}


#' Simulate multi-species data for a joint species distribution model
#'
#' @description
#' Draws a site-by-species response matrix from a latent factor model
#' and returns it in the long format [jsdgam()] takes. Each species
#' gets its own intercept and its own slope on a site-level
#' covariate, and the species are tied together by `n_lv` latent
#' factors whose loadings give the residual correlation between them.
#'
#' This is the non-detection counterpart to [sim_closure_unit_data()],
#' which simulates the same latent factor structure for the
#' occupancy and N-mixture families.
#'
#' @details
#' The linear predictor for species `k` at site `i` is
#' `intercept[k] + env_slope[k] * env[i] + Z[k, ] %*% lv[i, ]`, where
#' `Z` holds the loadings and `lv` the site scores. The residual
#' covariance the model targets is `Z Z'`, plus a diagonal of squared
#' per-species scales for [mvn()] and [mvt()].
#'
#' How that predictor becomes a response depends on the family.
#' Families whose species are drawn independently, such as
#' [brms::negbinomial()] or [stats::poisson()], take one draw per
#' species. [mvn()] and [mvt()] add a per-species residual. The
#' composition families draw a site's species together: [diri()]
#' takes gamma variates and normalises them, [multi()] takes counts
#' against a fixed site total, and [categ()] takes one species per
#' site as a one-hot row.
#'
#' A composition family identifies its loadings only up to a shift
#' shared by the species, which is the subspace mvgam's
#' `sum_to_zero_vector` parameterisation covers. The loadings are
#' centred for those families, which puts the simulated truth in the
#' same subspace a fit explores.
#'
#' How much of the factor structure survives into the response
#' depends on the observation noise. [mvn()] and [mvt()] take their
#' per-species scale from `Psi`, which defaults to 0.5 against
#' loadings drawn at 0.7. Every other family takes the scale its own
#' simulator defaults to, and a gaussian's is 1. Measured over eight
#' seeds at 100 sites, the agreement between the loadings-implied
#' residual correlation and the correlation present in the simulated
#' data is 0.96 for [mvn()] and drops as low as -0.44 for a gaussian
#' at that noise level. Pass `family_pars = list(sigma = 0.5)` for a
#' gaussian whose species correlations are recoverable.
#'
#' @param family Observation family. Any family mvgam's response
#'   simulator draws from, plus [mvn()], [mvt()], [diri()],
#'   [multi()] and [categ()]. Defaults to [brms::negbinomial()].
#' @param n_species Number of species. Defaults to `5`.
#' @param n_sites Number of sites. Defaults to `60`.
#' @param n_lv Number of latent factors. Defaults to `2`.
#' @param family_pars Optional named list of family parameters:
#'   `Psi` (per-species scale for [mvn()] / [mvt()]), `nu` (degrees
#'   of freedom for [mvt()]), `phi` (concentration for [diri()]),
#'   `unit_total` (trial total per site for [multi()]), and the
#'   dispersion or scale parameter the remaining families take,
#'   such as `size` for a negative binomial or `sigma` for a
#'   gaussian.
#' @param seed Optional integer seed. The session's random state is
#'   restored when the call returns.
#'
#' @return An object of class `mvgam_sim_jsdm`, inheriting
#'   `mvgam_sim`, with elements:
#'   \describe{
#'     \item{`data_train`}{Long `data.frame` with columns `site`,
#'       `env`, `series` (the species), `y` and `time` (the unit
#'       index [jsdgam()] keys on).}
#'     \item{`data_test`}{`NULL`, kept for consistency with the other
#'       simulators.}
#'     \item{`y_array`}{`[n_sites x n_species]` response matrix.}
#'     \item{`family`}{The family used.}
#'     \item{`n_species`, `n_sites`, `n_lv`}{The design.}
#'     \item{`truth`}{List of the generative parameters: `loadings`,
#'       `lv`, `intercepts`, `env_slopes`, `env`, `residual_cov`,
#'       `residual_cor`, and the family parameters that were used.}
#'   }
#'
#' @seealso [jsdgam()] for the model this simulates from;
#'   [sim_closure_unit_data()] for the occupancy and N-mixture
#'   counterpart; [sim_mvgam()] for time-series simulation.
#'
#' @examples
#' simdat <- sim_jsdm(
#'   family    = brms::negbinomial(),
#'   n_species = 4L,
#'   n_sites   = 30L,
#'   n_lv      = 2L,
#'   seed      = 1L
#' )
#' head(simdat$data_train)
#' summary(simdat)
#'
#' # A composition family draws each site's species together
#' comp <- sim_jsdm(family = diri(), n_species = 4L, n_sites = 20L,
#'                  seed = 2L)
#' rowSums(comp$y_array)[1:3]
#'
#' @export
sim_jsdm <- function(family = brms::negbinomial(),
                     n_species = 5L,
                     n_sites = 60L,
                     n_lv = 2L,
                     family_pars = list(),
                     seed = NULL) {
  checkmate::assert_int(n_species, lower = 2L)
  checkmate::assert_int(n_sites, lower = 2L)
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_list(family_pars, names = "named")
  family <- validate_family(family)
  local_seed(seed)

  # `sim_family_rng()` names a negative binomial's dispersion `size`;
  # the fitted side declares the same quantity as `shape`. Accepting
  # either spelling here keeps one name from leaking into user code.
  if (is.null(family_pars$size) && !is.null(family_pars$shape)) {
    family_pars$size <- family_pars$shape
  }

  species_levels <- paste0("sp_", seq_len(n_species))
  simplex <- is_simplex_response_family(family)

  loadings <- matrix(
    stats::rnorm(n_species * n_lv, sd = 0.7),
    nrow = n_species, ncol = n_lv
  )
  if (simplex) {
    loadings <- loadings -
      rep(colMeans(loadings), each = n_species)
  }

  env <- stats::rnorm(n_sites)
  intercepts <- stats::rnorm(n_species)
  env_slopes <- stats::rnorm(n_species)

  lv <- matrix(0, nrow = n_sites, ncol = n_lv)
  y_array <- matrix(0, nrow = n_sites, ncol = n_species)
  for (i in seq_len(n_sites)) {
    lv[i, ] <- stats::rnorm(n_lv)
    eta_i <- intercepts + env_slopes * env[i] +
      as.numeric(loadings %*% lv[i, ])
    y_array[i, ] <- sim_jsdm_draw(eta_i, family, family_pars)
  }
  colnames(y_array) <- species_levels

  residual_cov <- tcrossprod(loadings)
  if (resolve_family_name(family) %in% c("mvn", "mvt")) {
    psi <- family_pars$Psi %||% 0.5
    scale_sq <- if (identical(resolve_family_name(family), "mvt")) {
      nu <- family_pars$nu %||% 5
      psi^2 * nu / (nu - 2)
    } else {
      psi^2
    }
    residual_cov <- residual_cov + diag(scale_sq, n_species)
  }

  long <- data.frame(
    site = rep(seq_len(n_sites), times = n_species),
    env = rep(env, times = n_species),
    series = factor(
      rep(species_levels, each = n_sites),
      levels = species_levels
    ),
    y = as.numeric(y_array),
    stringsAsFactors = FALSE
  )
  long$time <- long$site
  long <- long[order(long$time, long$series), ]
  rownames(long) <- NULL
  if (family_uses_integers(resolve_family_name(family))) {
    long$y <- as.integer(long$y)
  }

  truth <- c(
    list(
      loadings = loadings,
      lv = lv,
      intercepts = stats::setNames(intercepts, species_levels),
      env_slopes = stats::setNames(env_slopes, species_levels),
      env = env,
      residual_cov = residual_cov,
      # A composition family's loadings-implied covariance is
      # singular by construction, and the jitter keeps `cov2cor()`
      # defined without moving the correlations it reports.
      residual_cor = stats::cov2cor(
        residual_cov + diag(1e-8, n_species)
      )
    ),
    family_pars
  )

  structure(
    list(
      data_train = long,
      data_test  = NULL,
      y_array    = y_array,
      family     = family,
      n_species  = n_species,
      n_sites    = n_sites,
      n_lv       = n_lv,
      truth      = truth
    ),
    class = c("mvgam_sim_jsdm", "mvgam_sim")
  )
}


#' Summary method for sim_jsdm() output
#'
#' Reports the simulated design, the per-species generative
#' parameters and the range of the residual correlations the
#' loadings imply, which is what a fit of these data is asked to
#' recover.
#'
#' @param object A `mvgam_sim_jsdm` list returned by [sim_jsdm()].
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class `mvgam_sim_jsdm_summary`, with its own
#'   [print()][print.mvgam_sim_jsdm_summary] method.
#'
#' @method summary mvgam_sim_jsdm
#' @export
summary.mvgam_sim_jsdm <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_sim_jsdm")
  rlang::check_dots_empty()
  cor_off <- object$truth$residual_cor[
    upper.tri(object$truth$residual_cor)
  ]
  structure(
    list(
      family        = resolve_family_name(object$family),
      n_species     = object$n_species,
      n_sites       = object$n_sites,
      n_lv          = object$n_lv,
      n_rows        = nrow(object$data_train),
      intercepts    = object$truth$intercepts,
      env_slopes    = object$truth$env_slopes,
      cor_range     = range(cor_off),
      response_range = range(object$y_array),
      zero_fraction = mean(object$y_array == 0)
    ),
    class = "mvgam_sim_jsdm_summary"
  )
}


#' Print method for sim_jsdm() summary output
#'
#' @param x A `mvgam_sim_jsdm_summary` object.
#' @param digits Integer; significant digits for printed numbers.
#'   Default `3`.
#' @param ... Currently ignored.
#'
#' @return The `mvgam_sim_jsdm_summary` object `x`, returned
#'   invisibly.
#'
#' @method print mvgam_sim_jsdm_summary
#' @export
print.mvgam_sim_jsdm_summary <- function(x, digits = 3L, ...) {
  checkmate::assert_class(x, "mvgam_sim_jsdm_summary")
  checkmate::assert_int(digits, lower = 0L)
  fmt <- function(v) format(round(v, digits), nsmall = digits)
  cat("Simulated joint species distribution dataset (sim_jsdm)\n")
  cat("  Family        : ", x$family, "\n", sep = "")
  cat("  Species       : ", x$n_species, "\n", sep = "")
  cat("  Sites         : ", x$n_sites, "\n", sep = "")
  cat("  Latent factors: ", x$n_lv, "\n", sep = "")
  cat("  Rows          : ", x$n_rows, "\n", sep = "")
  cat("\nTrue generative parameters\n")
  cat("  Intercepts : ",
      paste(fmt(x$intercepts), collapse = ", "), "\n", sep = "")
  cat("  Env slopes : ",
      paste(fmt(x$env_slopes), collapse = ", "), "\n", sep = "")
  cat("  Residual correlations: [", fmt(x$cor_range[1L]), ", ",
      fmt(x$cor_range[2L]), "]\n", sep = "")
  cat("\nSimulated response\n")
  cat("  Range         : [", fmt(x$response_range[1L]), ", ",
      fmt(x$response_range[2L]), "]\n", sep = "")
  cat("  Zero fraction : ", fmt(x$zero_fraction), "\n", sep = "")
  invisible(x)
}
