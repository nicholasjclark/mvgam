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
#' @examples
#' \donttest{
#' # Simulate data and fit a model
#' simdat <- sim_mvgam(
#'   n_series = 1,
#'   trend_model = AR()
#' )
#'
#' mod <- mvgam(
#'   y ~ s(season, bs = 'cc', k = 6),
#'   trend_model = AR(),
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Extract model variables
#' variables(mod)
#' }
#'
#' @export
#' @export variables
variables.mvgam <- function(x, ...) {
  # Validate input
  checkmate::assert_class(x, "mvgam")

  # Extract parameter names via draws object for backend compatibility
  # Posterior package handles both rstan and cmdstanr stanfit objects
  all_vars <- variables(posterior::as_draws(x$fit), ...)

  # Apply parameter exclusions
  if (!is.null(x$exclude) && length(x$exclude) > 0) {
    all_vars <- setdiff(all_vars, x$exclude)
  }

  all_vars
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
#'     \item trends: Computed trend state arrays (trend[i,j])
#'   }
#'
#' @details
#' This function distinguishes observation from trend parameters using the
#' _trend suffix naming convention. All trend model parameters include this
#' suffix to avoid naming conflicts with observation model parameters.
#'
#' The alias column is reserved for mapping Stan parameter names to mgcv-style
#' coefficient names (e.g., "s_x_1[3]" might alias to "s(x).3"). Currently
#' set to NA as this mapping is not yet implemented.
#'
#' @examples
#' \dontrun{
#' # Internal function called by tidy(), mcmc_plot(), etc.
#' params <- .categorize_parameters(mvgam_fit)
#' names(params)  # observation_pars, trend_pars, etc.
#' head(params$observation_betas)
#' }
#'
#' @noRd
.categorize_parameters <- function(x) {
  # Validate input
  checkmate::assert_class(x, "mvgam")

  # Extract all parameter names as character vector from combined fit
  all_pars <- variables(x)

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

  # Observation family parameters (not from linear predictor)
  obs_family_pattern <- "^(sigma|shape|nu|phi|zi|hu)(_|\\[|$)"
  obs_family_pars <- all_pars[
    grepl(obs_family_pattern, all_pars) &
      !grepl("_trend", all_pars)
  ]
  observation_pars <- create_component(obs_family_pars)

  # Fixed effects from observation formula only
  obs_beta_pars <- all_pars[
    (grepl("^b_", all_pars) |
       grepl("^b\\[", all_pars) |
       all_pars == "Intercept") &
      !grepl("_trend", all_pars)
  ]
  observation_betas <- create_component(obs_beta_pars)

  # Smooth parameters from observation formula only
  obs_smooth_pars <- all_pars[
    grepl("^(sds_|s_)", all_pars) &
      !grepl("_trend", all_pars)
  ]
  observation_smoothpars <- create_component(obs_smooth_pars)

  # Random effect parameters from observation formula only
  obs_re_pars <- all_pars[
    grepl("^(sd_|r_|cor_|L_)", all_pars) &
      !grepl("_trend", all_pars) &
      !grepl("L_Omega_trend", all_pars)
  ]
  observation_re_params <- create_component(obs_re_pars)

  # Trend dynamics parameters (AR coefficients, innovation SDs, correlations)
  # Excludes computed arrays and b_Intercept_trend (uncentered generated
  # quantity already filtered via variables.mvgam)
  trend_dynamic_pars <- all_pars[
    grepl("_trend", all_pars) &
      !grepl("^(trend|lv_trend|innovations_trend|scaled_innovations_trend|mu_trend)\\[", all_pars) &
      all_pars != "b_Intercept_trend"
  ]
  trend_pars <- create_component(trend_dynamic_pars)

  # Fixed effects from trend formula only
  trend_beta_pars <- all_pars[
    (grepl("^b_.*_trend", all_pars) | all_pars == "Intercept_trend") &
      all_pars != "b_Intercept_trend"
  ]
  trend_betas <- create_component(trend_beta_pars)

  # Smooth parameters from trend formula only
  trend_smooth_pars <- all_pars[
    grepl("^(sds_.*_trend|s_.*_trend)", all_pars)
  ]
  trend_smoothpars <- create_component(trend_smooth_pars)

  # Random effect parameters from trend formula only
  trend_re_pars <- all_pars[
    grepl("^(sd_.*_trend|r_.*_trend|cor_.*_trend)", all_pars)
  ]
  trend_re_params <- create_component(trend_re_pars)

  # Computed trend state arrays only
  trend_state_pars <- all_pars[grepl("^trend\\[", all_pars)]
  trends <- create_component(trend_state_pars)

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
