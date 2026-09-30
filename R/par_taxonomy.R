# One taxonomy for parameter names
#
# The `variable =` keyword resolver in `as.data.frame.mvgam.R`, the
# summary blocks in `summary.mvgam.R` and the bucket builder
# `categorize_mvgam_parameters()` in `index-mvgam.R` all ask what kind
# of parameter a name is, and all ask here. With a regex of its own,
# one caller counted `innovations_trend` as a parameter while another
# counted it as a latent state.
#
# The callers take different sets of kinds, by design. `?mvgam_draws`
# documents the `smooth_params` keyword as the smoothing standard
# deviations alone. The parameter buckets add the basis coefficients
# and the Gaussian-process hyperparameters. The kinds below separate
# all three, and no caller writes a regex to split them.
#
# Every parameter name has a side of the model and a kind.
# `mvgam_par_side()` and `mvgam_par_kind()` decide both.


#' Which side of the model a parameter belongs to
#'
#' The observation model or the latent trend. mvgam suffixes every
#' trend-side parameter with `_trend`, and the suffix decides the side.
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
#' A posterior name has an index, and `sigma_trend[1]` must match. A
#' prior class has none, and the prior tables test the bare suffix
#' themselves.
#'
#' A rotated companion ends in `_trend_tilde`, as `Phi_trend_tilde` does
#' for `Phi_trend`. Testing the bare suffix filed those on the
#' observation side, where `mvgam_par_kind()` classified them as
#' `other`.
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
#' parameters, and the pattern's leading `^` excludes them.
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
#' | `basis` | `bs_*`, `bsp_*` | `coef()`, `fixef()`, `vcov()`, with `beta` |
#' | `simplex` | `simo_*` | `summary()`; the parameter buckets, with `beta` |
#' | `intercept` | brms's centred `Intercept` | the parameter buckets, with `beta` |
#' | `smooth_sd` | `sds_*` | the `smooth_params` keyword, `summary()` |
#' | `smooth_coef` | `s_*`, `zs_*` | `tidy()`'s `ran_vals`, with `gp_coef` |
#' | `gp` | `sdgp_*`, `lscale_*` | `summary()`; the parameter buckets |
#' | `gp_coef` | `zgp_*` | `tidy()`'s `ran_vals`, with `smooth_coef` |
#' | `ranef_sd` | `sd_*`, `cor_*` | `tidy()`'s `ran_pars` |
#' | `ranef_coef` | `r_*` | `tidy()`'s `ran_vals` |
#' | `family` | `sigma`, `shape`, `nu`, ... | `obs_params` |
#' | `dynamics` | trend-side leftovers | `trend_params` |
#' | `bookkeeping` | `lprior`, `lp__` | nothing; no summary claims them |
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
  # transformed parameters. Stan saves every such variable, and each
  # one has meaning only inside the transformation that produced it.
  take(grepl(MVGAM_PAR_INTERNAL_PATTERN, pars), "internal")

  # The trend's own time-indexed states, in every spelling the
  # generated Stan emits. A fit has one per time point and series,
  # and no summary block claims them.
  take(grepl(MVGAM_PAR_STATE_PATTERN, pars), "state")

  # Factor loadings bridge the two sides and their names lack a
  # side suffix, which puts them here before the side is consulted.
  # A rotated fit has both bases and both are loadings;
  # `is_hidden_par()` settles which one a reader sees, leaving this
  # line to classify the raw block.
  take(grepl("^Z(_tilde)?\\[", pars), "loading")
  take(grepl(MVGAM_PAR_LOADINGS_PRIOR_PATTERN, pars), "loadings_prior")

  take(grepl(MVGAM_PAR_SMOOTH_SD_PATTERN, pars), "smooth_sd")
  take(grepl(MVGAM_PAR_SMOOTH_COEF_PATTERN, pars), "smooth_coef")
  take(grepl(MVGAM_PAR_GP_PATTERN, pars), "gp")
  take(grepl(MVGAM_PAR_GP_COEF_PATTERN, pars), "gp_coef")
  take(grepl(MVGAM_PAR_RANEF_SD_PATTERN, pars), "ranef_sd")
  take(grepl(MVGAM_PAR_RANEF_COEF_PATTERN, pars), "ranef_coef")

  # The population block proper, the smooth and special-term
  # coefficients brms reports with it, the monotonic simplexes and the
  # centred intercept. Consumers take different combinations of them:
  # `fixef()` takes the first two, `summary()` prints the simplexes
  # in a block of their own, and the parameter buckets `tidy()` builds
  # from group all four.
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
#' class, and the classes run from the population coefficients through
#' the scales to the quantities Stan keeps for itself. The kinds below
#' are that order, stated once: the bucket a name belongs to decides
#' where it goes and names within a bucket keep the order the program
#' declares them in.
#'
#' @param pars Character vector of parameter names
#' @return Integer vector ordering `pars`
#' @noRd
mvgam_par_order <- function(pars) {
  checkmate::assert_character(pars)
  # An intercept opens its own class, as brms reports it. Stan's two
  # accumulators have an order of their own, and every other name
  # keeps the order the program declares it in.
  intercept_last <- !grepl("_Intercept(_[0-9]+)?$", pars)
  within_kind <- match(pars, MVGAM_PAR_BOOKKEEPING_ORDER, nomatch = 0L)
  class_rank <- match(sub("[_[].*$", "", pars), MVGAM_PAR_CLASS_ORDER,
                      nomatch = 0L)
  order(match(mvgam_par_kind(pars), MVGAM_PAR_KIND_ORDER),
        class_rank, intercept_last, within_kind, seq_along(pars))
}


# The classes brms orders a fitted object by, as the kinds this
# taxonomy names. Stan's own accumulators come last, as they do in
# brms, and a name of no named kind sorts before them.
#'@noRd
MVGAM_PAR_KIND_ORDER <- c(
  "beta", "basis", "ranef_sd", "smooth_sd", "gp", "family", "intercept",
  "simplex", "ranef_coef", "smooth_coef", "gp_coef", "loading",
  "loadings_prior", "dynamics", "state", "internal", "other", "bookkeeping"
)


# The brms classes that share a kind, in the order
# `brms:::reorder_pars()` gives them: the unpenalised smooth
# coefficients before the special-term ones, the group-level standard
# deviations before their correlations, and a Gaussian process's
# marginal deviation before its length-scale.
#'@noRd
MVGAM_PAR_CLASS_ORDER <- c("bs", "bsp", "sd", "cor", "sdgp", "lscale")


# The name patterns, written once. A trend-side name matches the
# same pattern as its observation-side counterpart, and the side is
# what tells the two apart. Writing a second `.*_trend` variant of
# each pattern is what let the two accounts drift.
# The intermediates of the VAR stationarity transformation, the
# moving-average innovations an `AR(ma = TRUE)` or `RW(ma = TRUE)`
# forms from the scaled ones, and the Cholesky factors a correlated
# trend samples. Stan saves every variable declared at the top level
# of transformed parameters, and these reach the posterior of any
# VAR, VARMA or ARMA fit without naming a quantity a reader
# interprets. A Cholesky factor carries a unit diagonal and a
# structurally zero upper triangle, both of which print as a row
# with no posterior width. `A_trend` is the unconstrained matrix
# the stationarity transform turns into `Phi_trend`, and `Z_cols` the
# sum-to-zero columns `Z` is assembled from. A structured loadings
# prior builds its correlation `Phi_loadings` and that matrix's
# Cholesky factor from the length-scales `summary()` reports, and its
# column scales `Psi_diag` from the increments `varrho_inv`. `residual_cor()`
# and `shared_variation()` take all of them from
# `posterior::as_draws_matrix(object$fit)`, which reaches the draws
# without this projection. `scaled_innovations_trend` is the
# innovation a forecast seed needs and is classified a state.
#
# The second alternation is brms's own group-level workspace, as
# `brms:::exclude_pars_re()` lists it. brms computes the scaled
# effects from the standardised deviates `z_<id>`, the correlation
# Cholesky `L_<id>` and the correlation matrix `Cor_<id>`, and drops
# all three unless the user asks for them with
# `save_pars(all = TRUE)`. mvgam writes its own Stan, and without this
# pattern `variables()` listed `z_1[1,1]` with the
# `r_grp[a,Intercept]` computed from it. The trend side spells the
# same names with `_trend` after the id. The lower-case `cor_<id>`
# vector is a different parameter, which is aliased and kept.
#'@noRd
MVGAM_PAR_INTERNAL_PATTERN <- paste0(
  "^(P_var|result_var|P_ma|result_ma|empty_theta|Q_tilde|",
  "ma_innovations_trend|A_trend|A_group_trend|D_trend|",
  "L_Omega_trend|L_Sigma_trend|L_Omega_global_trend|",
  "L_Omega_group_trend|L_deviation_group_trend|L_group_trend|",
  "Z_cols|varrho_inv|Phi_loadings|L_Phi_loadings)\\[",
  "|^(z|L|Cor)_[0-9]+(_[0-9]+)*(_trend)?\\["
)

#'@noRd
MVGAM_PAR_STATE_PATTERN <- paste0(
  "^(trend|lv_trend|lv_trend_tilde|innovations_trend|",
  "scaled_innovations_trend|init_trend|mu_trend)\\["
)

# The per-cell matrix arrays a correlated or hierarchical trend
# derives. A high-dimensional fit has hundreds of cells of them, and
# `summary()` prints them only with `matrices = TRUE`. The Cholesky
# factors they are built from carry the kind `internal` and never
# reach a printed summary.
#'@noRd
MVGAM_PAR_MATRIX_PATTERN <- paste0(
  "^(Phi_group_trend|Sigma_group_trend|Phi_trend|Sigma_trend|",
  "Omega_trend|Theta_trend)\\["
)

# The name `brms::fixef()` gives a population-level coefficient: its
# class prefix `b_`, `bs_` or `bsp_` dropped, as `brms:::fixef_pars()`
# drops it.
#'@noRd
fixef_name <- function(pars) {
  sub("^b(s|sp)?_", "", pars)
}

# `b[k]` is the positional form the population block takes before
# `mvgam_user_pars()` renames it.
#'@noRd
MVGAM_PAR_BETA_PATTERN <- "^(b_|b\\[)"

# `bs` / `bsp` are the unpenalised smooth coefficients and the
# special-term coefficients, which brms reports with the population
# block.
#'@noRd
MVGAM_PAR_BASIS_PATTERN <- "^(bs_|bs\\[|bsp_|bsp\\[)"

# The simplex of each monotonic effect, which brms reports in a block
# of its own.
#'@noRd
MVGAM_PAR_SIMPLEX_PATTERN <- "^simo_"

# brms centres the design matrix and writes the intercept as a scalar
# of its own, outside `b`.
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

# The distributional parameters each family declares, which
# `get_family_dpars()` gives to the prediction paths. The custom
# families' names follow the same convention: `mphi`, `mtheta` and
# `mtail` are the Tweedie and beta-negative-binomial dispersion,
# power and tail, `p` the closure-unit detection probability and
# `Psi` the scale of `mvn()` and `mvt()`.
#'@noRd
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

# The family parameters as the posterior names them: any parameter
# of `MVGAM_FAMILY_DPARS`, and the discrimination `disc` brms saves
# for the ordinal families. A response of a model with several takes
# its key after an underscore, as `sigma_y2`. A mixture gives each
# component's parameters the component's number, as `sigma1` and
# `theta2`. The mixing proportions are a family parameter only in
# that spelling: a bare `theta` is the simplex the program is
# identified by, and `theta_features` is the trend's own loading-prior
# length-scale.
#'@noRd
MVGAM_PAR_FAMILY_PATTERN <- paste0(
  "^(", paste(unique(c(unlist(MVGAM_FAMILY_DPARS), "disc")),
              collapse = "|"),
  ")[0-9]*(_|\\[|$)|^theta[0-9]+(\\[|$)"
)

# The structured loadings prior's hyperparameters `summary()` reports:
# the feature and distance length-scales and the column scales of the
# multiplicative gamma process. They carry no side suffix and are
# classified before the side is consulted.
#'@noRd
MVGAM_PAR_LOADINGS_PRIOR_PATTERN <-
  "^theta_features\\[|^theta_dist_|^Psi_diag\\["
