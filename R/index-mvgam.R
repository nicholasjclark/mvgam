#' Index \code{mvgam} objects
#'
#' @aliases variables
#'
#' Index variables and their `mgcv` coefficient names
#'
#' @param x A \code{mvgam} object or another \R object for which
#' the methods are defined.
#'
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @name index-mvgam
NULL

#' @rdname index-mvgam
#'
#' @importFrom posterior variables
#'
#' @param x \code{list} object returned from \code{mvgam}. See [mvgam()]
#'
#' @method variables mvgam
#'
#' @return a character vector of parameter names
#'
#' @author Nicholas J Clark
#'
#' @export
#' @export variables
variables.mvgam <- function(x, ...) {
  # Validate input
  checkmate::assert_class(x, "mvgam")

  # The exclusion list, the brms-style renames, the empty-observation
  # placeholder and the rotation-indeterminate factor block are all
  # settled by `mvgam_user_pars()`, which every other user-facing
  # reader of the posterior goes through.
  names(mvgam_user_pars(x))
}


#' Categorize Parameters from mvgam Object
#'
#' Internal helper that categorizes parameters from a fitted mvgam object into
#' observation and trend components. Returns structured list for use by
#' internal functions (tidy, mcmc_plot, pairs, as.data.frame, etc.).
#'
#' @param x A fitted mvgam object
#'
#' @return A list with the following components (each a data.frame with
#'   orig_name and alias columns, or NULL if not present):
#'   \itemize{
#'     \item observation_pars: Family parameters (sigma, shape, nu, phi, zi,
#'       hu)
#'     \item observation_betas: Fixed effect coefficients
#'     \item observation_smoothpars: Smooth parameters (s_, sds_)
#'     \item observation_re_params: Random effect parameters (sd_, r_, cor_)
#'     \item trend_pars: Trend dynamics parameters (AR, innovation SDs, etc.)
#'     \item trend_betas: Trend formula fixed effects
#'     \item trend_smoothpars: Trend formula smooth parameters
#'     \item trend_re_params: Trend formula random effects
#'     \item trends: Computed trend state arrays (trend\[i,j\])
#'   }
#'
#' @details
#' This function distinguishes observation from trend parameters using the
#' _trend suffix naming convention. All trend model parameters include this
#' suffix to avoid naming conflicts with observation model parameters.
#'
#' The alias column is reserved for mapping Stan parameter names to mgcv-style
#' coefficient names (e.g., "s_x_1\[3\]" might alias to "s(x).3"). It holds
#' NA; Stan names are what the draws carry.
#'
#' @noRd
categorize_mvgam_parameters <- function(x) {
  # Validate input
  checkmate::assert_class(x, "mvgam")

  # Pull raw positional names directly from the stanfit. The
  # mvgam-side beta-aliasing (`b[k]` -> `b_<term>`) is a user-facing
  # projection applied inside `extract_mvgam_draws` and
  # `variables.mvgam`; the internal prediction pipeline subsets the
  # raw stanfit draws by positional name and must see them
  # unaliased here.
  all_pars <- variables(posterior::as_draws(x$fit))
  if (!is.null(x$exclude) && length(x$exclude) > 0L) {
    all_pars <- setdiff(all_pars, x$exclude)
  }

  # Helper to create data.frame component or NULL
  create_component <- function(pars) {
    if (length(pars) > 0) {
      data.frame(
        orig_name = pars,
        alias = NA,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }

  # Every bucket is a (side, kind) pair from the one taxonomy.
  # Each used to carry its own regexes, which is how the smooth set
  # here came to differ from the one the `variable =` keyword
  # resolver reports for the same fit.
  kind <- mvgam_par_kind(all_pars)
  side <- mvgam_par_side(all_pars)
  pick <- function(k, sd = NULL) {
    keep <- kind %in% k
    if (!is.null(sd)) keep <- keep & side == sd
    create_component(all_pars[keep])
  }

  observation_pars <- pick("family", "observation")
  observation_betas <- pick(
    c("beta", "basis", "intercept"), "observation"
  )
  observation_smoothpars <- pick(
    c("smooth_sd", "smooth_coef", "gp"), "observation"
  )
  observation_re_params <- pick("ranef", "observation")

  # The latent-dynamics block, plus the loadings that bridge the two
  # sides. The rotation-indeterminate draws are dropped here for the
  # same reason `variables()` drops them: their identified
  # counterparts are in the same posterior and these have arbitrary
  # convergence diagnostics.
  trend_dynamic_pars <- all_pars[
    kind %in% c("dynamics", "loading") & !is_hidden_unrotated(all_pars)
  ]
  trend_pars <- create_component(trend_dynamic_pars)

  # brms reports only the centred intercept.
  trend_beta_pars <- all_pars[
    kind %in% c("beta", "basis", "intercept") & side == "trend" &
      all_pars != "b_Intercept_trend"
  ]
  trend_betas <- create_component(trend_beta_pars)
  trend_smoothpars <- pick(
    c("smooth_sd", "smooth_coef", "gp"), "trend"
  )
  trend_re_params <- pick("ranef", "trend")

  # Every Stan state array lands in exactly one bucket and is
  # reachable via `obj_vars$trends`.
  trends <- pick("state")

  # Return structured list
  list(
    observation_pars = observation_pars,
    observation_betas = observation_betas,
    observation_smoothpars = observation_smoothpars,
    observation_re_params = observation_re_params,
    trend_pars = trend_pars,
    trend_betas = trend_betas,
    trend_smoothpars = trend_smoothpars,
    trend_re_params = trend_re_params,
    trends = trends
  )
}


#' The parameter names of one side of a fit
#'
#' The observation side holds the family's parameters, the
#' population-level coefficients, the smooths and the group-level terms
#' of the observation model. The trend side holds the same of the trend
#' model with its dynamics and loadings. The trend's state arrays are
#' derived quantities and belong to neither.
#'
#' @param mvgam_fit A fitted `mvgam` object
#' @param side `"obs"` or `"trend"`
#' @return Character vector of Stan names, empty when the side has none
#' @noRd
side_parameters <- function(mvgam_fit, side) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_choice(side, c("obs", "trend"))
  prefix <- if (identical(side, "obs")) "observation" else "trend"
  buckets <- categorize_mvgam_parameters(mvgam_fit)[
    paste0(prefix, c("_pars", "_betas", "_smoothpars", "_re_params"))
  ]
  as.character(unlist(lapply(buckets, `[[`, "orig_name"),
                      use.names = FALSE))
}
