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

  # Extract parameter names via draws object for backend compatibility
  # Posterior package handles both rstan and cmdstanr stanfit objects
  all_vars <- variables(posterior::as_draws(x$fit), ...)

  # Apply parameter exclusions
  if (!is.null(x$exclude) && length(x$exclude) > 0) {
    all_vars <- setdiff(all_vars, x$exclude)
  }

  # Apply the brms-style `b_<term>` / `r_<group>[...]` etc.
  # renames in place of positional Stan slots. Mirrors the
  # rename applied in `extract_mvgam_draws` so character-vector
  # and draws-array consumers see identical names.
  alias_map <- c(mvgam_beta_aliases(x), mvgam_ranef_aliases(x))
  all_vars <- apply_mvgam_beta_aliases(all_vars, alias_map)

  # Drop the empty-obs-formula placeholder coefficient from the
  # user-facing parameter list. The pinned `constant(0)` prior
  # means there is no posterior sample for it; returning the name
  # would leak the workaround. Filter after the alias rename
  # because the alias map renames positional `b[k]` to
  # `b_<colname>`, which would reintroduce the placeholder name.
  ph_prefix <- paste0("b_", MVGAM_EMPTY_OBS_PLACEHOLDER)
  all_vars[!startsWith(all_vars, ph_prefix)]
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
#' coefficient names (e.g., "s_x_1\[3\]" might alias to "s(x).3"). Currently
#' set to NA as this mapping is not yet implemented.
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

  # Observation family parameters (not from linear predictor).
  # `mphi` / `mtheta` are the Tweedie custom-family dispersion
  # and power parameters; they belong here so `tidy()`,
  # `coef()` and the family-extras section of `summary.mvgam()`
  # surface them alongside standard dpars.
  obs_family_pattern <- "^(sigma|shape|nu|phi|zi|hu|mphi|mtheta)(_|\\[|$)"
  obs_family_pars <- all_pars[
    grepl(obs_family_pattern, all_pars) &
      !grepl("_trend", all_pars)
  ]
  observation_pars <- create_component(obs_family_pars)

  # Fixed effects from observation formula only
  # b_ = standard fixed effects, b[ = indexed (multivariate)
  # bs_ = basis spline coefficients (brms), bs[ = basis spline coefficients (mvgam)
  # bsp_ = monotonic coefficients (brms), bsp[ = monotonic coefficients (mvgam)
  # simo_ = simplex parameters for monotonic effects (brms/mvgam)
  # Intercept = intercepts (all variants)
  obs_beta_pattern <- "^(b_|b\\[|bs_|bs\\[|bsp_|bsp\\[|simo_|Intercept)"
  obs_beta_pars <- all_pars[
    grepl(obs_beta_pattern, all_pars) &
      !grepl("_trend", all_pars) &
      all_pars != "Intercept_trend"
  ]
  observation_betas <- create_component(obs_beta_pars)

  # Smooth parameters from observation formula only
  # sds_ = smooth SDs, s_ = smooth coefficients, zs_ = standardized smooths
  # sdgp_ = GP SDs, lscale_ = GP length-scales, zgp_ = GP standardized
  obs_smooth_pattern <- "^(sds_|s_|zs_|sdgp_|lscale_|zgp_)"
  obs_smooth_pars <- all_pars[
    grepl(obs_smooth_pattern, all_pars) &
      !grepl("_trend", all_pars)
  ]
  observation_smoothpars <- create_component(obs_smooth_pars)

  # Random effect parameters from observation formula only
  # sd_ = RE SDs, r_ = RE correlations, cor_ = correlation parameters
  # L_ = Cholesky factors, z_ = standardized RE deviations
  obs_re_pattern <- "^(sd_|r_|cor_|L_|z_)"
  obs_re_pars <- all_pars[
    grepl(obs_re_pattern, all_pars) &
      !grepl("_trend", all_pars) &
      !grepl("L_Omega_trend", all_pars)
  ]
  observation_re_params <- create_component(obs_re_pars)

  # Trend dynamics parameters (AR coefficients, innovation SDs, correlations).
  # Excludes computed arrays, intercepts, fixed effects, and b_Intercept_trend
  # (uncentered generated quantity already filtered via variables.mvgam).
  # The factor-loading matrix bridges observations and latent trends and
  # lacks the `_trend` suffix; the loading regex (`Z_tilde` or `Z`) and the
  # state regex (`lv_trend` plus `lv_trend_tilde`) delegate pattern choice
  # to the shared selectors so other accessors stay in lockstep.
  loading_pattern <- factor_loading_param_pattern(all_pars)
  hide_pattern <- hidden_unrotated_factor_pars(all_pars)
  state_pattern <- paste0(
    "^(trend|lv_trend|lv_trend_tilde|innovations_trend|",
    "scaled_innovations_trend|mu_trend)\\["
  )
  hide_match <- if (is.null(hide_pattern)) {
    rep(FALSE, length(all_pars))
  } else {
    grepl(hide_pattern, all_pars)
  }
  trend_dynamic_pars <- all_pars[
    (grepl("_trend", all_pars) |
       grepl(loading_pattern, all_pars)) &
      !grepl(state_pattern, all_pars) &
      !hide_match &
      all_pars != "b_Intercept_trend" &
      all_pars != "Intercept_trend" &
      !grepl("^b_.*_trend", all_pars) &
      # Exclude smooth params (must mirror the obs-side smooth
      # regex so trend-side GPs and standardised bases land in
      # `trend_smoothpars` instead of leaking into `trend_pars`).
      !grepl(
        paste0(
          "^(sds_|s_|zs_|sdgp_|lscale_|zgp_).*_trend"
        ),
        all_pars
      ) &
      # Exclude RE params (mirror obs-side, which covers
      # `sd_`, `r_`, `cor_`, `L_` and the standardised `z_` raw
      # deviations). Asymmetry here was leaking `L_*_trend` /
      # `z_*_trend` into `trend_pars`.
      !grepl(
        "^(sd_|r_|cor_|L_|z_).*_trend",
        all_pars
      )
  ]
  trend_pars <- create_component(trend_dynamic_pars)

  # Fixed effects from trend formula only
  trend_beta_pars <- all_pars[
    (grepl("^b_.*_trend", all_pars) | all_pars == "Intercept_trend") &
      all_pars != "b_Intercept_trend"
  ]
  trend_betas <- create_component(trend_beta_pars)

  # Smooth parameters from trend formula only. Mirrors the obs-side
  # pattern so trend-side GP marginal SDs (`sdgp_*_trend`), GP
  # length-scales (`lscale_*_trend`), GP standardised draws
  # (`zgp_*_trend`) and standardised smooth-basis coefficients
  # (`zs_*_trend`) all land in `trend_smoothpars` rather than
  # leaking into `trend_pars`.
  trend_smooth_pars <- all_pars[
    grepl(
      paste0(
        "^(sds_|s_|zs_|sdgp_|lscale_|zgp_).*_trend"
      ),
      all_pars
    )
  ]
  trend_smoothpars <- create_component(trend_smooth_pars)

  # Random effect parameters from trend formula only. Mirrors the
  # obs-side regex so trend-side Cholesky factors (`L_*_trend`) and
  # standardised raw deviations (`z_*_trend`) land here instead of
  # leaking into `trend_pars`.
  trend_re_pars <- all_pars[
    grepl("^(sd_|r_|cor_|L_|z_).*_trend", all_pars)
  ]
  trend_re_params <- create_component(trend_re_pars)

  # Computed trend state arrays. Uses the same `state_pattern` the
  # trend-dynamic filter uses to exclude these from `trend_pars`,
  # so every Stan state array (lv_trend, innovations_trend,
  # scaled_innovations_trend, mu_trend, trend) lands in exactly
  # one bucket and is reachable via `obj_vars$trends`.
  trend_state_pars <- all_pars[grepl(state_pattern, all_pars)]
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


#' Extract Parameters by Type from mvgam Object
#'
#' Internal helper that extracts parameter names for a specific model
#' component (observation or trend) from a fitted mvgam object.
#'
#' @param mvgam_fit A fitted mvgam object
#' @param type Character string, either "observation" or "trend"
#'
#' @return Character vector of parameter names. Returns character(0) if
#'   no parameters of the specified type are present.
#'
#' @noRd
extract_parameters_by_type <- function(mvgam_fit,
  type = c("observation", "trend")) {
  # Validate inputs
  checkmate::assert_class(mvgam_fit, "mvgam")
  type <- match.arg(type)

  # Get categorized parameters
  categorized <- categorize_mvgam_parameters(mvgam_fit)

  # Define which components to extract based on type
  if (type == "observation") {
    components <- c(
      "observation_pars", "observation_betas",
      "observation_smoothpars", "observation_re_params"
    )
  } else {
    components <- c(
      "trend_pars", "trend_betas",
      "trend_smoothpars", "trend_re_params"
    )
  }

  # Extract and combine parameter names from all components
  param_names <- unlist(lapply(components, function(comp) {
    if (!is.null(categorized[[comp]])) {
      categorized[[comp]]$orig_name
    } else {
      character(0)
    }
  }))

  # Ensure we return character(0) not NULL
  if (is.null(param_names)) {
    param_names <- character(0)
  }

  param_names
}


#' Extract Observation Parameters from mvgam Object
#'
#' Helper function that extracts all observation model parameter names from
#' a fitted mvgam object. This includes family parameters, fixed effects,
#' smooth parameters, and random effect parameters from the observation
#' formula.
#'
#' @param mvgam_fit A fitted mvgam object
#'
#' @return A character vector of observation parameter names. Returns an
#'   empty character vector (character(0)) if the model has no observation
#'   parameters (which would be unusual for most fitted models).
#'
#' @details
#' Internally calls `categorize_mvgam_parameters()` and combines the
#'   following components:
#' \itemize{
#'   \item observation_pars: Family parameters (sigma, shape, nu, phi, zi,
#'     hu)
#'   \item observation_betas: Fixed effect coefficients
#'   \item observation_smoothpars: Smooth parameters (s_, sds_)
#'   \item observation_re_params: Random effect parameters (sd_, r_, cor_)
#' }
#'
#' @noRd
extract_obs_parameters <- function(mvgam_fit) {
  extract_parameters_by_type(mvgam_fit, type = "observation")
}


#' Extract Trend Parameters from mvgam Object
#'
#' Helper function that extracts all trend model parameter names from a
#' fitted mvgam object. This includes trend dynamics parameters, fixed
#' effects, smooth parameters, and random effect parameters from the trend
#' formula. Excludes computed trend state arrays.
#'
#' @param mvgam_fit A fitted mvgam object
#'
#' @return A character vector of trend parameter names. Returns an empty
#'   character vector (character(0)) if the model has no trend parameters
#'   (e.g., pure brms models with trend_formula = NULL).
#'
#' @details
#' Internally calls `categorize_mvgam_parameters()` and combines the
#'   following components:
#' \itemize{
#'   \item trend_pars: Trend dynamics parameters (AR coefficients,
#'     innovation SDs, correlations, factor loadings)
#'   \item trend_betas: Fixed effect coefficients from trend formula
#'   \item trend_smoothpars: Smooth parameters from trend formula
#'     (s_, sds_)
#'   \item trend_re_params: Random effect parameters from trend formula
#'     (sd_, r_, cor_)
#' }
#'
#' Note: Computed trend state arrays (trend\[i,j\], lv_trend\[i,j\],
#'   innovations_trend\[i,j\]) are excluded as they are derived
#'   quantities, not model parameters.
#'
#' @noRd
extract_trend_parameters <- function(mvgam_fit) {
  extract_parameters_by_type(mvgam_fit, type = "trend")
}
