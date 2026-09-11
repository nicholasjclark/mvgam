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
  all_pars <- setdiff(variables(posterior::as_draws(x$fit)), x$exclude)
  kind <- mvgam_par_kind(all_pars)
  side <- mvgam_par_side(all_pars)
  pick <- function(kinds, on_side) {
    all_pars[kind %in% kinds & side == on_side]
  }
  betas <- c("beta", "basis", "intercept")
  smooths <- c("smooth_sd", "smooth_coef", "gp", "gp_coef")
  ranef <- c("ranef_sd", "ranef_coef")

  list(
    observation_pars = pick("family", "observation"),
    observation_betas = pick(betas, "observation"),
    observation_smoothpars = pick(smooths, "observation"),
    observation_re_params = pick(ranef, "observation"),
    # The rotation-indeterminate loadings are left out, as
    # `variables()` leaves them out: their identified counterparts
    # are in the same posterior.
    trend_pars = all_pars[kind %in% c("dynamics", "loading") &
                            !is_hidden_unrotated(all_pars)],
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
