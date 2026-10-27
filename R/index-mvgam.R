#' Parameter names of a fitted mvgam model
#'
#' `variables()` lists the name of every quantity in a fitted model's
#' draws. [hypothesis.mvgam()], [as_draws()], [as.data.frame.mvgam()],
#' [mcmc_plot.mvgam()] and [posterior_average.mvgam()] accept these
#' names. [summary.mvgam()] and [posterior_summary()] label their rows
#' with them.
#'
#' @section How parameters are named:
#' Observation parameters take their \pkg{brms} names, such as `b_x`,
#' `sds_sx_1`, `sd_site__Intercept` and `r_site[a,Intercept]`. The
#' trend model's parameters take the same names with a `_trend`
#' suffix, such as `b_x_trend` or `sds_sx_1_trend`.
#'
#' An index that runs over the series takes the series' name, as
#' \pkg{brms} labels a grouping factor's levels. A model with series
#' `DM` and `DO` has `sigma_trend[DM]`, `ar1_trend[DM]` and the
#' innovation covariance `Sigma_trend[DM,DO]`. A VAR's coefficients
#' keep the lag as a number, as in `Phi_trend[1,DM,DO]`, as do the
#' changepoints of `PW()` in `delta_trend[3,DM]`. Factor loadings are
#' `Z_tilde[DM,1]`, with the factor as the second index. In a factor
#' model the dynamics belong to the factors, as in `ar1_trend[1]`.
#' The latent state matrices keep numeric indices, as in
#' `trend[12,1]` for time 12 of the first series.
#'
#' A bare name such as `variable = "sigma_trend"` selects every element
#' of a vector, as in \pkg{posterior}. Because `variables()` includes
#' the latent states, its output runs to hundreds of names on most
#' fits. `grep()` narrows it, as in
#' `grep("^(sigma|ar1)_trend", variables(fit), value = TRUE)`.
#'
#' @param x A fitted `mvgam` object.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A character vector of parameter names.
#'
#' @seealso [mvgam_draws] to extract draws by name,
#'   [hypothesis.mvgam()] to test them, [summary.mvgam()] and
#'   [posterior_summary()] to summarise them.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(n_series = 2L, trend_model = AR())
#' mod <- mvgam(y ~ x,
#'              trend_formula = ~ AR(),
#'              data = simdat$data_train,
#'              chains = 2, silent = 2)
#'
#' # The trend dynamics, indexed by series
#' grep("^(sigma|ar1)_trend", variables(mod), value = TRUE)
#'
#' # Every element of a vector by its bare name
#' as_draws_df(mod, variable = "sigma_trend")
#'
#' # Two series' autoregressive coefficients compared
#' hypothesis(mod, "ar1_trend[series_1] > ar1_trend[series_2]",
#'            class = NULL)
#'
#' # Next, plot chosen parameters with mcmc_plot(mod, variable = ...)
#' }
#'
#' @author Nicholas J Clark
#'
#' @name index-mvgam
#' @aliases variables
#' @importFrom posterior variables
#' @family posterior summaries
#' @method variables mvgam
#' @export
#' @export variables
variables.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  require_fitted_model(x, "variables")
  rlang::check_dots_empty()

  # The exclusion list, the brms-style renames, the empty-observation
  # placeholder and the rotation-indeterminate factor block are all
  # settled by `mvgam_user_pars()`, which every other user-facing
  # reader of the posterior goes through.
  names(mvgam_user_pars(x))
}


#' The parameters a diagnostic plot shows when none is named
#'
#' `mcmc_plot()` and `pairs()` both start here. The set holds what a
#' reader interprets: the population block with the basis
#' coefficients brms writes beside it, the family's parameters, the
#' trend's dynamics and its loadings, the smoothing and
#' Gaussian-process scales, and the group-level scales. Left out are
#' the per-level and per-basis coefficients (`r_*`, `s_*`, `zs_*`,
#' `zgp_*`), which run to hundreds on a spline or hierarchical fit,
#' the trend's states, and brms's centred intercept, which
#' `b_Intercept` reports on the data's scale. Naming any of those in
#' `variable` still selects it.
#'
#' @param x A fitted `mvgam` object
#' @return Character vector of the names a user reads
#' @noRd
default_plot_variables <- function(x) {
  checkmate::assert_class(x, "mvgam")
  pars <- names(mvgam_user_pars(x))
  shown <- c("beta", "basis", "simplex", "family", "dynamics", "loading",
             "loadings_prior", "smooth_sd", "gp", "ranef_sd")
  pars[mvgam_par_kind(pars) %in% shown]
}


#' The parameters of a fit, by side and by kind
#'
#' Sorts the Stan names the fit's draws carry into the buckets
#' `tidy()` and `side_parameters()` read. Each side has the family's
#' parameters or the trend's dynamics, its population block, its
#' smooths and its group-level terms, all decided by
#' `mvgam_par_kind()` and `mvgam_par_side()`. The factor loadings sit
#' with the trend's dynamics. The trend's states are no parameter and
#' belong to no bucket.
#'
#' @param x A fitted `mvgam` object
#' @return A named list of character vectors of Stan names, each
#'   possibly empty: `observation_pars`, `observation_betas`,
#'   `observation_smoothpars`, `observation_re_params`, `trend_pars`,
#'   `trend_betas`, `trend_smoothpars` and `trend_re_params`. A
#'   population block holds brms's centred intercept and the uncentred
#'   `b_Intercept` both.
#' @noRd
categorize_mvgam_parameters <- function(x) {
  checkmate::assert_class(x, "mvgam")

  # The names Stan wrote, which the prediction pipeline subsets the
  # draws by. The names a user reads are `mvgam_user_pars()`'s.
  all_pars <- fit_variables(x$fit)
  kind <- mvgam_par_kind(all_pars)
  side <- by_par_stem(all_pars, mvgam_par_side)
  pick <- function(kinds, on_side) {
    all_pars[kind %in% kinds & side == on_side]
  }
  betas <- c("beta", "basis", "simplex", "intercept")
  smooths <- c("smooth_sd", "smooth_coef", "gp", "gp_coef")
  ranef <- c("ranef_sd", "ranef_coef")

  list(
    observation_pars = pick("family", "observation"),
    observation_betas = pick(betas, "observation"),
    observation_smoothpars = pick(smooths, "observation"),
    observation_re_params = pick(ranef, "observation"),
    # The raw loadings and the broadcast coefficient copies are
    # left out, as `variables()` leaves them out: what reports each
    # quantity is in the same posterior under another name.
    trend_pars = all_pars[kind %in% c("dynamics", "loading",
                                      "loadings_prior") &
                            !is_hidden_par(all_pars)],
    trend_betas = pick(betas, "trend"),
    trend_smoothpars = pick(smooths, "trend"),
    trend_re_params = pick(ranef, "trend")
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
  as.character(unlist(buckets, use.names = FALSE))
}
