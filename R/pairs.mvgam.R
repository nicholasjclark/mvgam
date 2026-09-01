#' Create a matrix of output plots from a \code{mvgam} object
#'
#' A \code{\link[graphics:pairs]{pairs}} method for MCMC output.
#' Mirrors the brms convention: when `variable` is left at its
#' default the selection is built from a regex list targeting
#' canonical inferential parameters (intercept, parametric
#' coefficients, family extras, variance components, smoothness
#' penalties, and the mvgam-specific trend dynamics). Per-basis
#' smooth coefficients (`s_*` / `zs_*`) and per-level random-effect
#' deviations (`r_*` / `z_*`) are deliberately excluded because
#' spline / hierarchical fits can carry hundreds of them; supply an
#' explicit `variable` regex when you need them.
#'
#' @param x An object of class \code{mvgam} or \code{jsdgam}.
#' @inheritParams mcmc_plot.mvgam
#' @param ... Further arguments passed to
#'   \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.
#'
#' @return A `bayesplot_grid` object; see
#'   \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Default selection: intercept(s), smoothness penalties, trend
#' # dynamics, family extras.
#' pairs(mod)
#'
#' # Custom regex for a focused pairs panel.
#' pairs(mod, variable = "^(sigma|ar1)_trend", regex = TRUE)
#' }
#'
#' @export
pairs.mvgam <- function(
  x,
  variable = NULL,
  regex = FALSE,
  use_alias = TRUE,
  ...
) {
  if (is.null(variable)) {
    variable <- default_pairs_variables(x)
    regex <- TRUE
  }
  draws <- as.array(
    x,
    variable = variable,
    regex = regex,
    use_alias = use_alias
  )
  with_color_scheme("red", bayesplot::mcmc_pairs(draws, ...))
}


# Internal: regex patterns that drive `pairs.mvgam()`'s default
# variable selection. Mirrors the internal brms default_plot_variables()
# for the observation-side parameters (so users moving between brms and
# mvgam see the same defaults) and adds mvgam-specific patterns for
# trend dynamics and the matching `*_trend` variants of the brms
# patterns. Family-specific distributional parameters (e.g. `sigma`
# for `gaussian()`, `shape` for `Gamma()` / negative binomial) are
# discovered from the fit's family rather than hard-coded.
#' @noRd
default_pairs_variables <- function(x) {
  family_obj <- x$family %||% gaussian()
  # Distributional parameter names are carried on the family object
  # (e.g. c("mu", "sigma") for gaussian()); fall back to "mu" alone.
  dpars <- family_obj$dpars %||% "mu"
  dpars <- setdiff(dpars, "mu")  # `mu` is the linear predictor,
                                  # not a free parameter.
  # The obs-side patterns carry no `$` end-marker, so prefixes
  # like `^b_`, `^sd_`, `^sds_`, `^cor_`, `^lscale_`, `^theta`
  # also catch their `*_trend` siblings on the mvgam side. The
  # trend block below only adds entries that don't share an
  # obs-side prefix (`sigma_trend`, capitalised `Sigma_trend`, the
  # mvgam-specific VAR / PW dynamics, and the centered intercept
  # `Intercept_trend` which lacks the `b_` brms prefix).
  c(
    # Observation-side patterns (brms parity, but the prefixes also
    # match trend-side `*_trend` parameters).
    # brms fixed-effect parameter prefixes (`b_`, `bs_`, `bcs_`,
    # `bsp_`, `bmo_`, `bme_`, `bmi_`, `bm_`).
    "^b(()|(s)|(cs)|(sp)|(mo)|(me)|(mi)|(m))_",
    "^sd_", "^cor_",          # RE variance components
    "^sigma$", "^rescor_",
    if (length(dpars)) paste0("^", dpars, "$"),
    "^delta$", "^theta",
    "^sdb_", "^sdbsp_", "^sdbs_",
    "^sds_", "^sdgp_", "^lscale_",
    # mvgam-specific trend additions (no obs-side prefix overlap).
    "^Intercept_trend$",      # centered trend intercept
    "^sigma_trend",
    "^ar[0-9]+_trend",        # AR coefficients
    "^A_trend",               # VAR coefficient matrices
    "^alpha_cor_trend",       # hierarchical correlation weight
    "^Sigma_trend",
    "^k_trend", "^m_trend", "^delta_trend"     # PW changepoint
  )
}
