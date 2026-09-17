# One taxonomy for parameter names
#
# The `variable =` keyword resolver in `as.data.frame.mvgam.R`, the
# `match_*_pars()` family in `summary.mvgam.R` and the bucket builder
# `categorize_mvgam_parameters()` in `index-mvgam.R` all ask what kind
# of parameter a name is, and all ask here. A regex of their own would
# let one count `innovations_trend` as a parameter while another
# counts it as a latent state.
#
# Their answers differ where they mean to. The `smooth_params` keyword
# reports the smoothing standard deviations alone, which `?mvgam_draws`
# documents, while the parameter buckets group those with the basis
# coefficients and the Gaussian-process hyperparameters. The kinds
# below are fine enough to state that difference, and no caller
# encodes it in a regex of its own.
#
# Every parameter name carries two facts: which side of the model it
# belongs to, and what kind of thing it is. The two functions below
# are the only place either is decided.


#' Which side of the model a parameter belongs to
#'
#' The observation model or the latent trend. mvgam suffixes every
#' trend-side parameter, so the suffix is the whole test; the factor
#' loadings are the exception and are handled by `mvgam_par_kind()`,
#' which classifies them before the side is consulted.
#'
#' @param pars Character vector of parameter names
#' @return Character vector, `"observation"` or `"trend"`
#' @noRd
mvgam_par_side <- function(pars) {
  checkmate::assert_character(pars)
  ifelse(is_trend_parameter(pars), "trend", "observation")
}


#' What kind of thing a parameter is
#'
#' Returns one label per name. The order of the tests is the
#' classification: a name is a latent state before it is anything
#' else, then a factor loading, then a coefficient of a modelled
#' distributional parameter, and so on. Trend-side names reach
#' `"dynamics"` only after the formula-effect kinds have been ruled
#' out, which is what separates the trend's innovation scale
#' `sigma_trend` from the observation family's `sigma`.
#'
#' The kinds are finer than any one caller needs, because the
#' callers want different combinations of them:
#'
#' | kind | names | who asks for it alone |
#' |---|---|---|
#' | `state` | the trend's time-indexed states | nothing; excluded everywhere |
#' | `loading` | `Z`, `Z_tilde` | the factor buckets |
#' | `beta` | `b_*`, `b[k]` | `coef()`, `fixef()`, `vcov()` |
#' | `basis` | `bs_*`, `bsp_*`, `simo_*` | the parameter buckets, with `beta` |
#' | `intercept` | brms's centred `Intercept` | the parameter buckets, with `beta` |
#' | `smooth_sd` | `sds_*` | the `smooth_params` keyword |
#' | `smooth_coef` | `s_*`, `zs_*` | `summary()`, with `smooth_sd` |
#' | `gp` | `sdgp_*`, `lscale_*` | the parameter buckets |
#' | `gp_coef` | `zgp_*` | `tidy()`'s `ran_vals`, with `smooth_coef` |
#' | `ranef_sd` | `sd_*`, `cor_*` | `tidy()`'s `ran_pars` |
#' | `ranef_coef` | `r_*` | `tidy()`'s `ran_vals` |
#' | `family` | `sigma`, `shape`, `nu`, ... | `obs_params` |
#' | `dynamics` | trend-side leftovers | `trend_params` |
#' | `bookkeeping` | `lprior`, `lp__` | nothing; no summary claims them |
#' | `internal` | Stan working arrays | nothing; hidden everywhere |
#'
#' Collapsing any of these would force the caller that wants the
#' narrower set to write a regex of its own, which is the state this
#' file exists to end.
#'
#' @param pars Character vector of parameter names
#' @param dpars Character vector of distributional parameters that
#'   carry their own formula. Their coefficients form their own kind
#'   rather than joining the mean's.
#' @return Character vector of kinds, one per element of `pars`
#' @noRd
mvgam_par_kind <- function(pars, dpars = character()) {
  checkmate::assert_character(pars)
  checkmate::assert_character(dpars)
  if (length(pars) == 0L) {
    return(character(0L))
  }
  side <- mvgam_par_side(pars)
  out <- rep("other", length(pars))
  free <- rep(TRUE, length(pars))

  take <- function(mask, label) {
    hit <- free & mask
    out[hit] <<- label
    free[hit] <<- FALSE
  }

  # Working arrays the generated Stan declares at the top level of
  # transformed parameters, which is where Stan saves everything it
  # sees. They carry no meaning outside the transformation that
  # produced them.
  take(grepl(MVGAM_PAR_INTERNAL_PATTERN, pars), "internal")

  # The trend's own time-indexed states, in every spelling the
  # generated Stan emits. Numerous by construction, so no summary
  # slot claims them.
  take(grepl(MVGAM_PAR_STATE_PATTERN, pars), "state")

  # Factor loadings bridge the two sides and carry no suffix, so
  # they are named before the side is consulted. A rotated fit
  # carries both bases and both are loadings; which one a reader is
  # shown is settled by `is_hidden_unrotated()`, not here, so that
  # the raw block is classified rather than falling through.
  take(grepl("^Z(_tilde)?\\[", pars), "loading")

  # A distributional parameter with its own formula owns its
  # coefficients; without one, `sigma` and friends are family
  # parameters and their names never reach here.
  alt <- dpar_alternation(dpars)
  if (!is.null(alt)) {
    take(grepl(paste0("^b_", alt, "_"), pars), "dpar_beta")
    take(grepl(paste0("^s(ds)?_", alt, "_"), pars), "dpar_smooth")
  }

  take(grepl(MVGAM_PAR_SMOOTH_SD_PATTERN, pars), "smooth_sd")
  take(grepl(MVGAM_PAR_SMOOTH_COEF_PATTERN, pars), "smooth_coef")
  take(grepl(MVGAM_PAR_GP_PATTERN, pars), "gp")
  take(grepl(MVGAM_PAR_GP_COEF_PATTERN, pars), "gp_coef")
  take(grepl(MVGAM_PAR_RANEF_SD_PATTERN, pars), "ranef_sd")
  take(grepl(MVGAM_PAR_RANEF_COEF_PATTERN, pars), "ranef_coef")

  # The population block proper, the basis blocks brms writes beside
  # it, and the centred intercept it writes on its own. They are
  # three kinds rather than one because consumers want different
  # combinations: `coef()` and `fixef()` report the population block
  # alone, while the parameter buckets `tidy()` reads group all
  # three together.
  take(grepl(MVGAM_PAR_BETA_PATTERN, pars), "beta")
  take(grepl(MVGAM_PAR_BASIS_PATTERN, pars), "basis")
  take(grepl(MVGAM_PAR_INTERCEPT_PATTERN, pars), "intercept")

  # `sigma` is the observation family's scale and `sigma_trend` is
  # the trend's innovation scale. Same prefix, different kind, told
  # apart by the side.
  take(grepl(MVGAM_PAR_FAMILY_PATTERN, pars) & side == "observation",
       "family")
  take(grepl(MVGAM_PAR_BOOKKEEPING_PATTERN, pars), "bookkeeping")
  take(side == "trend", "dynamics")

  out
}


#' The order a reader meets a fit's parameters in
#'
#' brms orders a fitted object's parameters by class, and the classes
#' run from the population coefficients through the scales to the
#' quantities Stan keeps for itself. The kinds below are that order,
#' stated once: the bucket a name belongs to decides where it goes and
#' names within a bucket keep the order the program declares them in.
#'
#' @param pars Character vector of parameter names
#' @param dpars Character vector of distributional parameters that
#'   carry their own formula
#' @return Integer vector ordering `pars`
#' @noRd
mvgam_par_order <- function(pars, dpars = character()) {
  checkmate::assert_character(pars)
  # An intercept opens its own class, as brms reports it. Stan's two
  # accumulators have an order of their own, and every other name
  # keeps the order the program declares it in.
  intercept_last <- !grepl("_Intercept(_[0-9]+)?$", pars)
  within_kind <- match(pars, MVGAM_PAR_BOOKKEEPING_ORDER, nomatch = 0L)
  order(match(mvgam_par_kind(pars, dpars), MVGAM_PAR_KIND_ORDER),
        intercept_last, within_kind, seq_along(pars))
}


# The classes brms orders a fitted object by, as the kinds this
# taxonomy names. Stan's own accumulators come last, as they do in
# brms, and a name of no named kind sorts before them.
#'@noRd
MVGAM_PAR_KIND_ORDER <- c(
  "beta", "basis", "dpar_beta", "ranef_sd", "smooth_sd", "dpar_smooth",
  "gp", "family", "intercept", "ranef_coef", "smooth_coef", "gp_coef",
  "loading", "dynamics", "state", "internal", "other", "bookkeeping"
)


# The name patterns, written once. A trend-side name matches the
# same pattern as its observation-side counterpart, and the side is
# what tells the two apart. Writing a second `.*_trend` variant of
# each pattern is what let the two accounts drift.
# The intermediates of the VAR stationarity transformation, and the
# moving-average innovations an `AR(ma = TRUE)` or `RW(ma = TRUE)`
# forms from the scaled ones. Stan saves every variable declared at
# the top level of transformed parameters, and these reach the
# posterior of any VAR, VARMA or ARMA fit without naming a quantity
# a reader interprets. `scaled_innovations_trend` is the innovation
# a forecast seed needs and is classified a state.
#
# The second alternation is brms's own group-level workspace, kept
# in step with `brms:::exclude_pars_re()`: the standardised
# deviates `z_<id>`, the correlation Cholesky `L_<id>` and the
# correlation matrix `Cor_<id>` are what the scaled effects are
# built from, and brms drops all three unless the user asks for
# them with `save_pars(all = TRUE)`. mvgam writes its own Stan, and
# without this pattern `z_1[1,1]` would sit beside the
# `r_grp[a,Intercept]` it produces. The trend side spells the same
# names with `_trend` after the id. The lower-case `cor_<id>`
# vector is a different parameter, which is aliased and kept.
#'@noRd
MVGAM_PAR_INTERNAL_PATTERN <- paste0(
  "^(P_var|result_var|P_ma|result_ma|empty_theta|Q_tilde|",
  "ma_innovations_trend)\\[",
  "|^(z|L|Cor)_[0-9]+(_[0-9]+)*(_trend)?\\["
)

#'@noRd
MVGAM_PAR_STATE_PATTERN <- paste0(
  "^(trend|lv_trend|lv_trend_tilde|innovations_trend|",
  "scaled_innovations_trend|init_trend|mu_trend)\\["
)

# `b[k]` is the positional form the population block takes before
# `mvgam_user_pars()` renames it.
#'@noRd
MVGAM_PAR_BETA_PATTERN <- "^(b_|b\\[)"

# `bs` / `bsp` / `simo` are the smooth basis, monotonic and simplex
# blocks brms writes alongside the population coefficients.
#'@noRd
MVGAM_PAR_BASIS_PATTERN <- "^(bs_|bs\\[|bsp_|bsp\\[|simo_)"

# brms centres the design matrix and writes the intercept as its own
# scalar rather than as a column of `b`.
#'@noRd
MVGAM_PAR_INTERCEPT_PATTERN <- "^Intercept"

# The smoothing penalty standard deviations. `?mvgam_draws`
# documents the `smooth_params` keyword as exactly these, which is
# narrower than the set the parameter buckets group together.
#'@noRd
MVGAM_PAR_SMOOTH_SD_PATTERN <- "^sds_"

# The basis coefficients the penalty applies to, raw and
# standardised.
#'@noRd
MVGAM_PAR_SMOOTH_COEF_PATTERN <- "^(s_|zs_)"

# Gaussian-process marginal deviations and length-scales, and the
# standardised basis coefficients they scale. Grouped with the
# smooths by the parameter buckets and reported apart from them by
# `summary()`.
#'@noRd
MVGAM_PAR_GP_PATTERN <- "^(sdgp_|lscale_)"
#'@noRd
MVGAM_PAR_GP_COEF_PATTERN <- "^zgp_"

# The group-level block as brms spells it after renaming: the
# standard deviations and the correlation vector, and the scaled
# effects they govern. The workspace those are built from carries a
# bare `L_` or `z_` prefix and is claimed by the internal pattern
# above. Matching either prefix here would take the trend's own
# innovation Cholesky (`L_Omega_trend`) for a group-level effect.
#'@noRd
MVGAM_PAR_RANEF_SD_PATTERN <- "^(sd_|cor_)"
#'@noRd
MVGAM_PAR_RANEF_COEF_PATTERN <- "^r_"

# What Stan accumulates for itself: the prior contribution the
# program sums into `lprior` and the log posterior density it reports
# as `lp__`. brms keeps both in the posterior and claims neither in a
# summary table, which is what this kind states.
#'@noRd
MVGAM_PAR_BOOKKEEPING_PATTERN <- "^(lprior|lp__)$"

# brms reports the prior contribution before the log posterior
#'@noRd
MVGAM_PAR_BOOKKEEPING_ORDER <- c("lprior", "lp__")

# `mphi` / `mtheta` / `mtail` are the Tweedie custom family's
# dispersion, power and tail parameters, which follow the same
# convention as the standard distributional parameters.
#'@noRd
# A mixture gives each component's parameters the component's number,
# as `sigma1` and `theta2`. The mixing proportions are a family
# parameter only in that spelling: a bare `theta` is the simplex the
# program is identified by, and `theta_features` is the trend's own
# loading-prior length-scale.
#'@noRd
MVGAM_PAR_FAMILY_PATTERN <- paste0(
  "^(sigma|shape|nu|phi|zi|hu|mphi|mtheta|mtail)[0-9]*(_|\\[|$)",
  "|^theta[0-9]+(\\[|$)"
)
