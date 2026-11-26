# One taxonomy for parameter names
#
# The `variable =` keyword resolver in `as.data.frame.mvgam.R`, the
# summary blocks in `summary.mvgam.R` and the bucket builder
# `categorize_mvgam_parameters()` in `index-mvgam.R` all ask what kind
# of parameter a name is. All three call `mvgam_par_kind()` below,
# which gives each name the same kind in every caller.
#
# The callers take different sets of kinds, by design. The
# `smooth_params` keyword selects the smoothing standard deviations
# alone. The parameter buckets add the basis coefficients and the
# Gaussian-process hyperparameters. The kinds below separate all
# three. No caller needs a regex of its own to split them.
#
# Every parameter name has a side of the model and a kind.
# `mvgam_par_side()` and `mvgam_par_kind()` decide both.


#' Which side of the model a parameter belongs to
#'
#' The observation model or the latent trend. mvgam suffixes every
#' trend-side parameter with `_trend`. The suffix decides the side.
#' The factor loadings and their prior's hyperparameters have no
#' suffix. `mvgam_par_kind()` classifies them before it consults the
#' side.
#'
#' @param pars Character vector of parameter names
#' @return Character vector, `"observation"` or `"trend"`
#' @noRd
mvgam_par_side <- function(pars) {
  checkmate::assert_character(pars)
  ifelse(is_trend_parameter(pars), "trend", "observation")
}


#' Is this parameter from the trend model?
#'
#' The trend side of a model names its parameters with a `_trend`
#' suffix, at the end or before the index: `sigma_trend[1]` and
#' `b_x_trend` are trend parameters. A covariate can contain `_trend`
#' too. The column `pre_trend_score` gives the observation-side
#' coefficient `b_pre_trend_score`. The test matches the suffix only at
#' the end of the name or before its index.
#'
#' A posterior name has an index. `sigma_trend[1]` must match. A prior
#' class has none. The prior tables test the bare suffix themselves.
#'
#' A rotated companion ends in `_trend_tilde`, as `Phi_trend_tilde` does
#' for `Phi_trend`. The pattern matches that ending too, which keeps the
#' companion on the trend side.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_trend_parameter <- function(pars) {
  grepl("_trend(_tilde)?($|\\[)", pars)
}


#' Is this parameter an autoregressive coefficient?
#'
#' An `AR(p)` trend emits one coefficient per lag, `ar1_trend` through
#' `ar<p>_trend`. The hierarchical mean and standard deviation of a
#' coefficient, `mu_ar1_trend` and `sigma_ar1_trend`, are different
#' parameters. The pattern's leading `^` excludes them.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_ar_coefficient <- function(pars) {
  grepl("^ar[0-9]+_trend$", pars)
}


#' Is this parameter an autoregressive partial autocorrelation?
#'
#' A contiguous `AR(p >= 2)` trend samples `ar<k>_pacf_trend` and
#' derives `ar<k>_trend` from it, which keeps every draw stationary.
#' The two names hold different quantities. This pattern matches the
#' partial autocorrelation. `is_ar_coefficient()` matches the
#' coefficient. Each caller collects the one parameter set its own
#' question asks about.
#'
#' @param pars Character vector of parameter names
#' @return Logical vector
#'
#' @noRd
is_ar_partial <- function(pars) {
  grepl("^ar[0-9]+_pacf_trend$", pars)
}


#' The stem of a parameter name
#'
#' The base name with the opening bracket of its index, as `Z[` for
#' `Z[3,1]`. Every name pattern of this file and of
#' `hidden_par_pattern()` tests the stem and ignores the index
#' contents.
#'
#' @param pars Character vector of parameter names
#' @return Character vector of stems
#' @noRd
par_stem <- function(pars) {
  sub("\\[.*$", "[", pars)
}


#' Classify parameter names one distinct stem at a time
#'
#' A trend fit saves thousands of state cells under a handful of
#' stems. A classifier applied to the distinct stems runs each of its
#' patterns over a few dozen names.
#'
#' @param pars Character vector of parameter names
#' @param classify Function of a character vector of distinct stems,
#'   returning one value per stem
#' @return The values of `classify`, one per element of `pars`
#' @noRd
by_par_stem <- function(pars, classify) {
  stem <- par_stem(pars)
  stems <- unique(stem)
  classify(stems)[match(stem, stems)]
}


#' What kind of thing a parameter is
#'
#' Returns one label per name. The first test a name passes decides
#' its kind: a working array, then a latent state, then a factor
#' loading, and so on. A trend-side name reaches `"dynamics"` only once
#' every formula-effect kind has failed. The trend's innovation scale
#' `sigma_trend` and the observation family's `sigma` share a prefix,
#' and the side separates them.
#'
#' Each caller takes its own combination of kinds, listed below. A
#' distributional parameter's coefficients take the kind of their
#' class, as brms groups `b_sigma_x` with `b_x`:
#'
#' | kind | names | who asks for it alone |
#' |---|---|---|
#' | `state` | the trend's time-indexed states | nothing; excluded everywhere |
#' | `loading` | `Z`, `Z_tilde` | the factor buckets |
#' | `loadings_prior` | `theta_features`, `theta_dist_*`, `Psi_diag` | `summary()` |
#' | `beta` | `b_*`, `b[k]` | the parameter buckets |
#' | `basis` | `bs_*`, `bsp_*`, `bcs_*` | `coef()`, `fixef()`, `vcov()`, with `beta` |
#' | `simplex` | `simo_*` | `summary()`; the parameter buckets, with `beta` |
#' | `intercept` | brms's centred `Intercept` | the parameter buckets, with `beta` |
#' | `smooth_sd` | `sds_*` | the `smooth_params` keyword, `summary()` |
#' | `smooth_coef` | `s_*`, `zs_*` | `tidy()`'s `ran_vals`, with `gp_coef` |
#' | `gp` | `sdgp_*`, `lscale_*` | `summary()`; the parameter buckets |
#' | `gp_coef` | `zgp_*` | `tidy()`'s `ran_vals`, with `smooth_coef` |
#' | `ranef_sd` | `sd_*`, `cor_*` | `tidy()`'s `ran_pars` |
#' | `ranef_coef` | `r_*` | `tidy()`'s `ran_vals` |
#' | `measurement` | `meanme_*`, `sdme_*`, `Corme_*` | nothing |
#' | `latent` | `Xme_*`, `Ymi_*`, `Yl_*` | the prediction routes |
#' | `family` | `sigma`, `shape`, `nu`, ... | `obs_params` |
#' | `dynamics` | trend-side leftovers | `trend_params` |
#' | `bookkeeping` | `lprior`, `log_jacobian_trend`, `lp__` | nothing; no summary claims them |
#' | `internal` | Stan working arrays | nothing; hidden everywhere |
#'
#' With any two of these merged, a caller that takes one of them
#' alone would need a regex of its own.
#'
#' @param pars Character vector of parameter names
#' @return Character vector of kinds, one per element of `pars`
#' @noRd
mvgam_par_kind <- function(pars) {
  checkmate::assert_character(pars)
  by_par_stem(pars, par_stem_kind)
}


#' The kind of each distinct parameter stem
#'
#' @param pars Character vector of distinct stems, as `par_stem()` gives
#' @return Character vector of kinds, one per stem
#' @noRd
par_stem_kind <- function(pars) {
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
  # transformed parameters. Stan saves every such variable. Each one
  # has meaning only inside the transformation that produced it.
  take(grepl(MVGAM_PAR_INTERNAL_PATTERN, pars), "internal")

  # The trend's own time-indexed states, in every spelling the
  # generated Stan emits. A fit has one per time point and series. No
  # summary block claims them.
  take(grepl(MVGAM_PAR_STATE_PATTERN, pars), "state")

  # Factor loadings bridge the two sides. Their names lack a side
  # suffix, which puts them here before the side is consulted. A
  # rotated fit has both bases and both are loadings.
  # `is_hidden_par()` settles which one a reader sees. This line
  # classifies the raw block.
  take(grepl("^Z(_tilde)?\\[", pars), "loading")
  take(grepl(MVGAM_PAR_LOADINGS_PRIOR_PATTERN, pars), "loadings_prior")

  take(grepl(MVGAM_PAR_SMOOTH_SD_PATTERN, pars), "smooth_sd")
  take(grepl(MVGAM_PAR_SMOOTH_COEF_PATTERN, pars), "smooth_coef")
  take(grepl(MVGAM_PAR_GP_PATTERN, pars), "gp")
  take(grepl(MVGAM_PAR_GP_COEF_PATTERN, pars), "gp_coef")
  take(grepl(MVGAM_PAR_RANEF_SD_PATTERN, pars), "ranef_sd")
  take(grepl(MVGAM_PAR_RANEF_COEF_PATTERN, pars), "ranef_coef")
  take(grepl(MVGAM_PAR_MEASUREMENT_PATTERN, pars), "measurement")
  take(grepl(MVGAM_PAR_LATENT_PATTERN, pars), "latent")

  # The population block proper, the smooth and special-term
  # coefficients brms reports with it, the monotonic simplexes and the
  # centred intercept. Consumers take different combinations of them.
  # `fixef()` takes the first two. `summary()` prints the simplexes in
  # a block of their own. The parameter buckets `tidy()` builds from
  # group all four.
  take(grepl(MVGAM_PAR_BETA_PATTERN, pars), "beta")
  take(grepl(MVGAM_PAR_BASIS_PATTERN, pars), "basis")
  take(grepl(MVGAM_PAR_SIMPLEX_PATTERN, pars), "simplex")
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
#' `brms:::reorder_pars()` orders a fitted object's parameters by
#' class. The classes run from the population coefficients through the
#' scales to the quantities Stan keeps for itself. The kinds below
#' state that order once. The bucket a name belongs to decides where
#' it goes. Names within a bucket keep the order the program declares
#' them in.
#'
#' @param pars Character vector of parameter names
#' @return Integer vector ordering `pars`
#' @noRd
mvgam_par_order <- function(pars) {
  checkmate::assert_character(pars)
  # An intercept opens its own class, as brms reports it. brms tests
  # the parameter's name, which an ordinal fit's thresholds share as
  # `b_Intercept[k]`. Stan's two accumulators have an order of their
  # own. Every other name keeps the order the program declares it in.
  intercept_last <- !grepl("_Intercept(_[0-9]+)?\\[?$", par_stem(pars))
  within_kind <- match(pars, MVGAM_PAR_BOOKKEEPING_ORDER, nomatch = 0L)
  class_rank <- match(sub("[_[].*$", "", pars), MVGAM_PAR_CLASS_ORDER,
                      nomatch = 0L)
  order(match(mvgam_par_kind(pars), MVGAM_PAR_KIND_ORDER),
        class_rank, intercept_last, within_kind, seq_along(pars))
}


#' The order of parameter kinds in a fitted object
#'
#' The classes brms orders a fitted object by, as the kinds this
#' taxonomy names. `mvgam_par_order()` ranks each name by its kind's
#' position here. Stan's own accumulators come last, as they do in
#' brms. A name of no named kind sorts just before them.
#'
#' @format Character vector of the kinds `mvgam_par_kind()` returns,
#'   in reporting order
#' @noRd
MVGAM_PAR_KIND_ORDER <- c(
  "beta", "basis", "ranef_sd", "smooth_sd", "gp", "family", "intercept",
  "simplex", "ranef_coef", "smooth_coef", "gp_coef", "latent", "measurement",
  "loading", "loadings_prior", "dynamics", "state", "internal", "other",
  "bookkeeping"
)


#' The order of brms classes that share a kind
#'
#' The brms classes that share a kind, in the order
#' `brms:::reorder_pars()` gives them. `mvgam_par_order()` matches each
#' name's class prefix here to rank names within a kind. The
#' unpenalised smooth coefficients precede the special-term ones. Those
#' precede the category-specific ones. The group-level standard
#' deviations precede their correlations. A Gaussian process's marginal
#' deviation precedes its length-scale. A response's imputed values
#' precede its latent values. Those precede the noise-free covariates.
#'
#' @format Character vector of brms class prefixes, in reporting order
#' @noRd
MVGAM_PAR_CLASS_ORDER <- c("bs", "bsp", "bcs", "sd", "cor", "sdgp",
                           "lscale", "Ymi", "Yl", "Xme")


#' Name pattern of the Stan working arrays a reader never sees
#'
#' The name patterns below are written once. A trend-side name matches
#' the same pattern as its observation-side counterpart. The side tells
#' the two apart. `mvgam_par_kind()` gives names this pattern matches
#' the kind `internal`.
#'
#' The first alternation lists Stan working arrays. They are the
#' intermediates of the VAR stationarity transformation, the
#' moving-average innovations an `AR(ma = TRUE)` or `RW(ma = TRUE)`
#' forms from the scaled ones and the Cholesky factors a correlated
#' trend samples. Stan saves every variable declared at the top level
#' of transformed parameters. These reach the posterior of any VAR,
#' VARMA or ARMA fit without naming a quantity a reader interprets. A
#' Cholesky factor carries a unit diagonal and a structurally zero
#' upper triangle. Both print as a row with no posterior width.
#' `A_trend` is the unconstrained matrix the stationarity transform
#' turns into `Phi_trend`. `Z_cols` holds the sum-to-zero columns `Z`
#' is assembled from. Multiplicative gamma process shrinkage samples
#' `Z_raw`, the loadings before their column scales. A structured
#' loadings prior builds its correlation `Phi_loadings` and that
#' matrix's Cholesky factor from the length-scales `summary()`
#' reports. It builds its column scales `Psi_diag` from the increments
#' `varrho_inv`. `residual_cor()` and `shared_variation()` take all of
#' them from `fit_parameter_draws()`. That conversion keeps the Stan
#' names this classification hides. A local linear trend samples its
#' slope and season scales as `sigma_slope_raw_trend` and
#' `sigma_season_raw_trend`, in units of the default priors' scales.
#' It reports `sigma_slope_trend` and `sigma_season_trend`.
#' `scaled_innovations_trend` is the innovation a forecast seed needs.
#' It is classified a state.
#'
#' The second alternation is brms's own group-level workspace, as
#' `brms:::exclude_pars_re()` lists it. brms computes the scaled
#' effects from the standardised deviates `z_<id>`, the correlation
#' Cholesky `L_<id>` and the correlation matrix `Cor_<id>`. It always
#' drops `Cor_<id>`. It drops `z_<id>` and `L_<id>` unless the user
#' asks for them with `save_pars(all = TRUE)`. mvgam writes its own
#' Stan and hides all three here. The trend side spells the same names
#' with `_trend` after the id. The lower-case `cor_<id>` vector is a
#' different parameter. It is aliased and kept. The third alternation
#' matches the measurement-error workspace `zme_<id>` and `Lme_<id>`.
#' The fourth matches the working arrays of a trend `gp()` term. The
#' scaled basis coefficients are `rgp_<k>_trend`. The GP at each
#' distinct input is `gp_pred_<k>_trend`. Both hold one value per
#' time point.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_INTERNAL_PATTERN <- paste0(
  "^(ma_innovations_trend|A_trend|A_group_trend|D_trend|D_group_trend|",
  "L_Omega_trend|L_Sigma_trend|L_Omega_global_trend|",
  "L_Omega_group_trend|L_deviation_group_trend|L_group_trend|",
  "Z_cols|Z_raw|varrho_inv|Phi_loadings|L_Phi_loadings|",
  "sigma_slope_raw_trend|sigma_season_raw_trend)\\[",
  "|^(z|L|Cor)_[0-9]+(_[0-9]+)*(_trend)?\\[",
  "|^(zme|Lme)_[0-9]+(_trend)?\\[",
  "|^(rgp|gp_pred)_[0-9]+(_trend)?\\["
)

#' Name pattern of the measurement-error hyperparameters
#'
#' The hyperparameters of the measurement-error terms. Each latent
#' covariate has a population mean and SD. A group of them has a
#' correlation matrix. `mvgam_par_kind()` gives these names the kind
#' `measurement`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_MEASUREMENT_PATTERN <- "^(meanme|sdme|Corme)_"

#' Name pattern of the latent values of special terms
#'
#' The values a special term takes at each fitted row: the noise-free
#' covariate of `me()`, the imputed values of a `mi()` response and the
#' latent response of one observed with noise. `mvgam_par_kind()` gives
#' these names the kind `latent`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_LATENT_PATTERN <- "^(Xme|Ymi|Yl)_"

#' Base names of the trend's latent states
#'
#' The trend's latent states and the variates they are built from.
#' Their sampling statements are the trend equation and its
#' non-centred reparameterisation. A user sets no prior on them. No
#' summary block reports them. `init_trend` is the vector of states
#' before the first observed time. `init_innovations_trend` is the
#' vector of standard variates an ARMA start scales to its stationary
#' law. `season_state_trend` holds a local linear trend's last
#' seasonal state pairs, the start of a forecast. A centred trend
#' samples `innovations_start_trend` and `lv_dev_trend`, the start
#' rows' innovations and each later state's distance from the first.
#'
#' @format Character vector of Stan variable names without indices
#' @noRd
MVGAM_STATE_PARAMS <- c(
  "trend", "lv_trend", "lv_trend_tilde", "innovations_trend",
  "scaled_innovations_trend", "init_trend", "init_innovations_trend",
  "mu_trend", "slope_trend", "slope_trend_tilde",
  "innovations_slope_trend", "season_trend", "season_trend_tilde",
  "season_state_trend", "innovations_season_trend",
  "innovations_start_trend", "lv_dev_trend"
)

#' Name pattern of each period's season in a local linear trend
#'
#' The season of each period of a local linear trend with several, in
#' both bases. Their number follows the trend's periods. The pattern
#' omits `^` and the index bracket. `MVGAM_PAR_STATE_PATTERN` adds them
#' to match indexed draws. `is_state_param()` adds `^` and `$` to match
#' base names.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PERIOD_SEASON_PATTERN <- "season_[0-9]+_trend(_tilde)?"

#' Name pattern of the trend's latent state draws
#'
#' Matches an indexed draw of any name in `MVGAM_STATE_PARAMS` or any
#' period's season. `mvgam_par_kind()` gives these names the kind
#' `state`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_STATE_PATTERN <- paste0(
  "^(", paste(c(MVGAM_STATE_PARAMS, MVGAM_PERIOD_SEASON_PATTERN),
              collapse = "|"), ")\\["
)


#' Whether each name is one of the trend's latent states
#'
#' Takes base names, as `fit@model_pars` holds them, without indices.
#'
#' @param nm Character vector of parameter names
#' @return Logical vector
#' @noRd
is_state_param <- function(nm) {
  checkmate::assert_character(nm)
  nm %in% MVGAM_STATE_PARAMS |
    grepl(paste0("^", MVGAM_PERIOD_SEASON_PATTERN, "$"), nm)
}

#' Name pattern of the trend's derived matrix cells
#'
#' The per-cell matrix arrays a correlated or hierarchical trend
#' derives. A high-dimensional fit has hundreds of their cells.
#' `summary()` prints them only with `matrices = TRUE`. The Cholesky
#' factors they are built from carry the kind `internal`. Those factors
#' never reach a printed summary.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_MATRIX_PATTERN <- paste0(
  "^(Phi_group_trend|Sigma_group_trend|Phi_trend|Sigma_trend|",
  "Omega_trend|Theta_trend)\\["
)

#' Name pattern of the symmetric matrices among the trend's matrices
#'
#' The symmetric matrices among those `MVGAM_PAR_MATRIX_PATTERN`
#' matches: the innovation covariances and a VAR's stationary
#' covariance. A cell above the diagonal repeats the cell below it. The
#' default view lists the diagonal and the lower triangle, the half
#' `residual_cor()` draws.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_SYMMETRIC_PATTERN <- "^(Sigma_group_trend|Sigma_trend|Omega_trend)\\["

#' Class prefix of the population-level coefficients
#'
#' The class prefix `b<type>_` of each type of coefficient
#' `brms::fixef()` reports, as `brms:::fixef_pars()` writes it.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_FIXEF_PREFIX <- "^b(s|cs|sp|mo|me|mi|m)?_"

#' Coefficient names as brms::fixef() reports them
#'
#' The name `brms::fixef()` gives a population-level coefficient: its
#' class prefix dropped. `fixef()`, `vcov()` and `tidy()` label their
#' coefficients with it.
#'
#' @param pars Character vector of parameter names
#' @return Character vector the length of `pars`, each name without
#'   the prefix `MVGAM_PAR_FIXEF_PREFIX` matches
#' @noRd
fixef_name <- function(pars) {
  sub(MVGAM_PAR_FIXEF_PREFIX, "", pars)
}

#' Name pattern of the population-level coefficients
#'
#' Matches `b_` names and `b[k]`, the positional form the population
#' block takes before `mvgam_user_pars()` renames it.
#' `mvgam_par_kind()` gives these names the kind `beta`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_BETA_PATTERN <- "^(b_|b\\[)"

#' Name pattern of the coefficients brms reports with the population
#' block
#'
#' `bs`, `bsp` and `bcs` are the unpenalised smooth coefficients, the
#' special-term coefficients and the category-specific coefficients.
#' brms reports all three with the population block. `mvgam_par_kind()`
#' gives these names the kind `basis`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_BASIS_PATTERN <- "^(bs_|bs\\[|bsp_|bsp\\[|bcs_|bcs\\[)"

#' Name pattern of the monotonic-effect simplexes
#'
#' The simplex of each monotonic effect. brms reports these in a block
#' of their own. `mvgam_par_kind()` gives them the kind `simplex`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_SIMPLEX_PATTERN <- "^simo_"

#' Name pattern of the centred intercept
#'
#' brms centres the design matrix and writes the intercept as a scalar
#' of its own, outside `b`. `mvgam_par_kind()` gives it the kind
#' `intercept`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_INTERCEPT_PATTERN <- "^Intercept"

#' Name pattern of the smoothing penalty standard deviations
#'
#' `?mvgam_draws` documents the `smooth_params` keyword as exactly
#' these. The parameter buckets group a wider set with them.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_SMOOTH_SD_PATTERN <- "^sds_"

#' Name pattern of the penalised smooth coefficients
#'
#' The basis coefficients the penalty applies to, raw and
#' standardised.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_SMOOTH_COEF_PATTERN <- "^(s_|zs_)"

#' Name pattern of the Gaussian-process hyperparameters
#'
#' Gaussian-process marginal deviations and length-scales. The
#' parameter buckets group these with the smooths. `summary()` reports
#' them apart.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_GP_PATTERN <- "^(sdgp_|lscale_)"
#' Name pattern of the Gaussian-process basis coefficients
#'
#' The standardised basis coefficients the hyperparameters of
#' `MVGAM_PAR_GP_PATTERN` scale. The parameter buckets group these with
#' the smooths. `summary()` reports them apart.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_GP_COEF_PATTERN <- "^zgp_"

#' Name pattern of the group-level standard deviations and
#' correlations
#'
#' The group-level block as brms spells it after renaming. This
#' pattern matches the standard deviations and the correlation vector.
#' The workspace the group-level effects are built from carries a bare
#' `L_` or `z_` prefix. `MVGAM_PAR_INTERNAL_PATTERN` claims it.
#' Matching either prefix here would take the trend's own innovation
#' Cholesky (`L_Omega_trend`) for a group-level effect.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_RANEF_SD_PATTERN <- "^(sd_|cor_)"
#' Name pattern of the scaled group-level effects
#'
#' Matches the scaled effects that the standard deviations and
#' correlations of `MVGAM_PAR_RANEF_SD_PATTERN` govern, as brms spells
#' them after renaming.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_RANEF_COEF_PATTERN <- "^r_"

#' Name pattern of Stan's own accumulators
#'
#' What Stan accumulates for itself: the prior contribution the
#' program sums into `lprior` and the log posterior density it reports
#' as `lp__`. A centred trend adds `log_jacobian_trend`, the log
#' Jacobian of its states' map to the standard innovations. brms keeps
#' the first two in the posterior. No summary table claims any of them.
#' The kind `bookkeeping` records exactly that.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_BOOKKEEPING_PATTERN <- "^(lprior|log_jacobian_trend|lp__)$"

#' The order of Stan's own accumulators
#'
#' brms reports the prior contribution before the log posterior.
#' `mvgam_par_order()` ranks the names by their position here.
#'
#' @format Character vector of the accumulator names
#' @noRd
MVGAM_PAR_BOOKKEEPING_ORDER <- c("lprior", "log_jacobian_trend", "lp__")

#' The distributional parameters of each family
#'
#' The distributional parameters each family declares.
#' `get_family_dpars()` gives them to the prediction paths. The custom
#' families' names follow the same convention. `mphi` and `mtheta` are
#' the Tweedie dispersion and power. `mtail` is the
#' beta-negative-binomial tail. `p` is the closure-unit detection
#' probability. `Psi` is the scale of `mvn()` and `mvt()`.
#'
#' @format Named list with one element per family name, each a
#'   character vector of parameter names, empty for a family with none
#' @noRd
MVGAM_FAMILY_DPARS <- list(
  gaussian = "sigma", student = c("sigma", "nu"),
  skew_normal = c("sigma", "alpha"), lognormal = "sigma",
  shifted_lognormal = c("sigma", "ndt"), gamma = "shape",
  weibull = "shape", frechet = "shape", inverse.gaussian = "shape",
  exgaussian = c("sigma", "beta"), beta = "phi",
  gen_extreme_value = c("sigma", "xi"),
  asym_laplace = c("sigma", "quantile"), wiener = c("bs", "ndt", "bias"),
  von_mises = "kappa", exponential = character(0),
  poisson = character(0), negbinomial = "shape", negbinomial2 = "sigma",
  geometric = character(0), discrete_weibull = "shape",
  com_poisson = "shape",
  binomial = character(0), beta_binomial = "phi",
  bernoulli = character(0),
  zero_inflated_poisson = "zi", zero_inflated_negbinomial = c("zi", "shape"),
  zero_inflated_binomial = "zi",
  zero_inflated_beta_binomial = c("zi", "phi"),
  zero_inflated_beta = c("zi", "phi"),
  zero_one_inflated_beta = c("zoi", "coi", "phi"),
  zero_inflated_asym_laplace = c("zi", "sigma", "quantile"),
  hurdle_poisson = "hu", hurdle_negbinomial = c("hu", "shape"),
  hurdle_gamma = c("hu", "shape"), hurdle_lognormal = c("hu", "sigma"),
  tweedie = c("mphi", "mtheta"), beta_nb = c("shape", "mtail"),
  com_binomial = "nu", nmix = "p", occ = "p", diri = "phi",
  multi = character(0), categ = character(0), mvn = "Psi",
  mvt = c("Psi", "nu")
)

#' Name pattern of the family parameters
#'
#' The family parameters as the posterior names them. These are the
#' parameters of `MVGAM_FAMILY_DPARS` and the discrimination `disc`
#' brms saves for the ordinal families. A response of a model with
#' several takes its key after an underscore, as `sigma_y2`. A mixture
#' gives each component's parameters the component's number, as
#' `sigma1` and `theta2`. The mixing proportions are a family parameter
#' only in that spelling. A bare `theta` is the simplex the program is
#' identified by. `theta_features` is the trend's own loading-prior
#' length-scale. `mvgam_par_kind()` gives an observation-side match the
#' kind `family`.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_FAMILY_PATTERN <- paste0(
  "^(", paste(unique(c(unlist(MVGAM_FAMILY_DPARS), "disc")),
              collapse = "|"),
  ")[0-9]*(_|\\[|$)|^theta[0-9]+(\\[|$)"
)

#' Name pattern of the structured loadings prior's hyperparameters
#'
#' The structured loadings prior's hyperparameters `summary()` reports:
#' the feature and distance length-scales and the column scales of the
#' multiplicative gamma process. They carry no side suffix.
#' `mvgam_par_kind()` classifies them before it consults the side.
#'
#' @format Character string holding a regular expression
#' @noRd
MVGAM_PAR_LOADINGS_PRIOR_PATTERN <-
  "^theta_features(\\[|_)|^theta_dist_|^Psi_diag\\["
