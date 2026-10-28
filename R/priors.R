#' Prior specification and inspection for mvgam
#'
#' @description
#' Prior extraction, combination, and inspection system for mvgam
#' models. This file provides functions for working with priors in both
#' observation and trend components, using the brms brmsprior class
#' throughout.
#'
#' @section Architecture:
#' The prior system uses brmsprior objects throughout:
#' - **Extraction Layer**: Get priors from observation and trend models
#' - **Combination Layer**: Merge observation and trend priors
#' - **Inspection Layer**: User-facing functions for prior specification
#' - **Validation Layer**: Ensure prior specifications are valid
#'
#' @section Design Decisions:
#' - Uses native brmsprior class throughout (no custom mvgamprior class)
#' - Trend parameters distinguished via _trend suffix convention
#' - Optional attributes for mvgam-specific metadata when needed
#' - Direct pass-through to brms for observation model priors

# =============================================================================
# Common trend prior specifications
# =============================================================================
# Prior specifications shared by parameters that appear in more than one
# trend type (sigma_trend is used by RW, AR and CAR). One definition
# keeps every trend on the same default.

#' Common prior specifications for trend parameters
#'
#' @description
#' Default priors for the parameters several trend types or families
#' share, written once for all of them. A comment above each entry
#' names its parameter.
#'
#' @format Named list with one element per parameter, a list holding
#' \describe{
#'   \item{default}{Character string of a Stan distribution, such as
#'     `"exponential(2)"`, or a function of the data's spread on the
#'     link scale returning one}
#'   \item{bounds}{Numeric vector `c(lower, upper)`, `NA` where
#'     unbounded}
#' }
#'
#' @noRd
common_trend_priors <- list(
  # Innovation standard deviation
  sigma_trend = list(
    default = function(unit) paste0("exponential(", signif(2 / unit, 4), ")"),
    bounds = c(0, NA)
  ),

  # The residual scale the mvn() and mvt() families declare. The
  # parameter is named after the quantity and has no `_trend` suffix.
  # It resolves through the same chain as every entry here.
  Psi = list(
    default = "exponential(1)",
    bounds = c(0, NA)
  ),

  # The residual degrees of freedom of the mvt() family. The bound
  # keeps the residual variance finite. The default is brms's prior
  # for a Student-t `nu`.
  nu = list(
    default = "gamma(2, 0.1)",
    bounds = c(2, NA)
  ),

  # AR(1) coefficient
  ar1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1)
  ),

  # Innovation degrees of freedom
  nu_trend = list(
    default = "gamma(4, 0.3)",
    bounds = c(2, NA)
  ),

  # Hierarchical correlation mixing parameter
  alpha_cor_trend = list(
    default = "beta(3, 2)",
    bounds = c(0, 1)
  ),

  # Per-group innovation standard deviation
  sigma_group_trend = list(
    default = function(unit) paste0("exponential(", signif(2 / unit, 4), ")"),
    bounds = c(0, NA)
  ),

  # Trend correlation Cholesky factor
  L_Omega_trend = list(
    default = "lkj_corr_cholesky(2)",
    bounds = c(NA, NA)
  ),

  # Population correlation Cholesky factor, grouped trend
  L_Omega_global_trend = list(
    default = "lkj_corr_cholesky(1)",
    bounds = c(NA, NA)
  ),

  # Per-group deviation Cholesky factor, grouped trend
  L_deviation_group_trend = list(
    default = "lkj_corr_cholesky(6)",
    bounds = c(NA, NA)
  ),

  # Factor loadings matrix
  Z = list(
    default = function(unit) paste0("student_t(3, 0, ", signif(0.5 * unit, 4),
                                    ")"),
    bounds = c(NA, NA)
  ),

  # MA(1) coefficient
  theta1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1)
  ),

  # Mean of the VAR coefficients, diagonal and off
  Amu_trend = list(
    default = "normal(0, sqrt(0.455))",
    bounds = c(NA, NA)
  ),

  # Precision of the VAR coefficients, diagonal and off
  Aomega_trend = list(
    default = "gamma(1.365, 0.071175)",
    bounds = c(0, NA)
  ),

  # Mean of the VARMA moving-average coefficients
  Dmu_trend = list(
    default = "std_normal()",
    bounds = c(NA, NA)
  ),

  # Precision of the VARMA moving-average coefficients
  Domega_trend = list(
    default = "gamma(2, 1)",
    bounds = c(0, NA)
  )
)

#' The trend half of the prior table
#'
#' Built from the prepared specification, the one Stan generation
#' also uses. Its `trend_map`, loadings prior and dimensions decide
#' the rows.
#'
#' @param trend_model The prepared `mvgam_trend` object
#' @param base_formula The trend formula without its constructor, or
#'   `NULL`
#' @param data The trend-grain data frame
#' @param codegen A list from `mvgam_codegen_options()`, or NULL
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors <- function(trend_model, base_formula, data,
                                  codegen = NULL) {
  checkmate::assert_class(trend_model, "mvgam_trend")
  checkmate::assert_class(base_formula, "formula", null.ok = TRUE)
  checkmate::assert_data_frame(data, min.rows = 1)

  prior_list <- list(
    constructor = generate_trend_priors_from_monitor_params(trend_model)
  )

  if (!is.null(base_formula)) {
    # brms has coefficients to report only for a formula with a
    # population term. An intercept counts: `~ 1` gives
    # `Intercept_trend`. Neither `~ 0` nor `~ -1` gives a row.
    if (formula_has_population_terms(base_formula)) {
      # The codegen options the observation side takes, which fix the
      # coefficients the fit estimates
      base_priors <- setup_brms_lightweight(
        formula = base_formula,
        data = data,
        family = gaussian(),
        codegen = codegen
      )$prior
      if (nrow(base_priors) > 0) {
        prior_list$base <- suffix_trend_prior_classes(base_priors)
      }
    }
  }

  bind_brmsprior_rows(prior_list)
}

#' Row-bind `brmsprior` tables whose columns differ
#'
#' brms builds prior tables with different columns. The observation
#' side has newer columns such as `tag`. A bare `prior()` row lacks
#' them. `align_brmsprior_schemas()` pads every table to the union of
#' the columns.
#'
#' @param prior_list List of `brmsprior` tables. `NULL` entries are
#'   dropped.
#' @return One `brmsprior`, or `NULL` when the list holds no table.
#' @noRd
bind_brmsprior_rows <- function(prior_list) {
  checkmate::assert_list(prior_list)
  prior_list <- Filter(Negate(is.null), prior_list)
  if (!length(prior_list)) {
    return(NULL)
  }
  combined <- do.call(rbind, unname(align_brmsprior_schemas(prior_list)))
  rownames(combined) <- NULL
  structure(combined, class = c("brmsprior", "data.frame"))
}

#' Prior rows for the parameters a trend samples
#'
#' @param trend_obj A prepared `mvgam_trend` object
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors_from_monitor_params <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")

  monitor_params <- generate_monitor_params(trend_obj)

  # A single-series trend has no correlation parameters
  if ("L_Omega_trend" %in% monitor_params &&
      !is.null(trend_obj$dimensions) &&
      trend_obj$dimensions$n_series == 1) {
    monitor_params <- setdiff(monitor_params, "L_Omega_trend")
  }

  # Under either sharing mode the per-series coefficient is derived
  # from another sampled quantity. Under `"shared"` it is a broadcast
  # scalar. Under `"hierarchical"` it comes from a population mean and
  # scale. A prior row for the derived vector would promise an override
  # the program discards. The editable rows are the parameters each
  # mode samples. `generate_monitor_params()` keeps the coefficients,
  # because mvgam withholds them from brms.
  sharing <- ar_coef_sharing(trend_obj)
  if (sharing %in% c("shared", "hierarchical")) {
    monitor_params <- monitor_params[
      !(is_ar_coefficient(monitor_params) | is_ar_partial(monitor_params))
    ]
  }

  if (length(monitor_params) == 0) {
    return(brms::empty_prior())
  }

  prior_data <- lapply(monitor_params, function(param) {
    create_trend_parameter_prior(param, trend_obj)
  })
  combined_priors <- do.call(rbind, prior_data)
  class(combined_priors) <- c("brmsprior", "data.frame")
  combined_priors
}

#' Create the prior specification of one trend parameter
#'
#' @param param_name Character string parameter name (e.g., "ar1_trend")
#' @param trend_obj mvgam_trend object for context
#' @return Single-row data frame with prior specification
#' @noRd
create_trend_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")

  prior_info <- get_default_trend_parameter_prior(param_name, trend_obj)

  # `brms::set_prior()` gives the row the full brmsprior schema,
  # including columns such as `tag` that newer brms versions add.
  # mvgam-internal and brms-generated rows then share one schema.
  row <- brms::set_prior(
    prior = prior_info$prior %||% "",
    class = param_name,
    lb = if (nzchar(prior_info$lb %||% "")) prior_info$lb else NA,
    ub = if (nzchar(prior_info$ub %||% "")) prior_info$ub else NA
  )
  # `brms::set_prior()` returns NA for an unbounded parameter and
  # `brms::get_prior()` returns "". The rows mvgam emits use "" to
  # compare equal to observation-side rows.
  if (is.na(row$lb)) row$lb <- ""
  if (is.na(row$ub)) row$ub <- ""
  row$source <- "default"
  row
}

#' Default prior information for a trend parameter
#'
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object for context, or NULL where the
#'   caller has none
#' @return List with prior, lb, ub elements
#' @noRd
get_default_trend_parameter_prior <- function(param_name,
                                             trend_obj = NULL) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend", null.ok = TRUE)

  # `Z` takes one of three branches depending on the loadings prior.
  # Only the unstructured branch matches the shared default. The prior
  # table and the Stan emitter both resolve the default here. Each
  # then describes the prior the model samples.
  if (identical(param_name, "Z")) {
    branch_prior <- loadings_z_prior_string(
      loadings_z_branch(trend_obj$loadings_prior_spec)
    )
    if (!is.null(branch_prior)) {
      return(list(prior = branch_prior, lb = "", ub = ""))
    }
  }

  # A trend-specific resolver may only tighten a bound, as CAR's does
  # for its damping coefficient. The shared default then supplies the
  # distribution. A call without a trend object has no resolver.
  custom_result <- if (!is.null(trend_obj)) {
    trend_method(
      trend_obj$trend, trend_method_optional_templates[["parameter_prior"]],
      default = function(...) NULL
    )(param_name, trend_obj)
  }
  if (!is.null(custom_result) && nzchar(custom_result$prior)) {
    return(custom_result)
  }

  # The shared table covers the parameters every trend can have. The
  # Stan generator resolves its defaults through this same function.
  spec <- common_trend_priors[[param_name]]
  if (!is.null(spec)) {
    bounds <- spec$bounds
    return(list(
      prior = if (is.function(spec$default)) {
        spec$default(trend_prior_unit_of(trend_obj))
      } else {
        spec$default
      },
      lb = custom_result$lb %||%
        (if (!is.na(bounds[1L])) as.character(bounds[1L]) else ""),
      ub = custom_result$ub %||%
        (if (!is.na(bounds[2L])) as.character(bounds[2L]) else "")
    ))
  }
  # A resolver that gives only bounds still needs a distribution. The
  # shared table has no entry for `ar2_trend` and the higher lags,
  # and the name patterns supply one.
  pattern <- get_parameter_type_default_prior(param_name)
  if (!is.null(custom_result)) {
    pattern$lb <- custom_result$lb %||% pattern$lb
    pattern$ub <- custom_result$ub %||% pattern$ub
  }
  pattern
}

#' The factor brms widens its default scale priors by
#'
#' brms sets the scale of its default intercept, `sd` and `sigma`
#' priors to `max(2.5, mad(link(y)))` under the identity, log, inverse,
#' square-root and inverse-square links (`brms:::def_scale_prior()`),
#' which widens its base scale of 2.5 by `max(1, mad(link(y)) / 2.5)`.
#' The trend's scale priors are calibrated in link units and widen by
#' The trend's scale priors are calibrated in link units and widen by
#' the same factor. A count model on the log link rarely spreads past
#' 2.5 and keeps its link-unit defaults. Zeros take 0.1 before a log or
#' inverse link. A family on the log scale, such as `lognormal()`,
#' takes the log link, as in brms. Under any other link the factor is
#' one. Several responses take the widest.
#'
#' @param formula The observation formula
#' @param family The observation family
#' @param data The training frame
#' @return A number of at least 1
#' @noRd
trend_prior_unit <- function(formula, family, data) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_list(formula),
    .var.name = "formula"
  )
  checkmate::assert_list(family, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  forms <- response_formulas(formula)
  families <- formula_families(formula, family)
  columns <- response_columns(formula)
  spreads <- vapply(names(forms), function(key) {
    link_scale_spread(data[[columns[[key]]]], families[[key]])
  }, numeric(1L))
  max(1, spreads / 2.5)
}

#' The spread of one response on its link scale
#'
#' A simplex family's response names categories or shares, which its
#' log-ratio predictor does not measure. A binary detection is no
#' count. Both take zero, as brms's categorical families do. Missing
#' responses are dropped. A transform outside its domain, such as the
#' log of a negative value, gives no finite spread, as in brms.
#'
#' @param y The response column
#' @param family Its family
#' @return The rounded median absolute deviation, or 0
#' @noRd
link_scale_spread <- function(y, family) {
  checkmate::assert_list(family, null.ok = TRUE)
  if (is_simplex_response_family(family) ||
      is_binary_response_family(family)) {
    return(0)
  }
  link <- if ("logscale" %in% family$specials) "log" else family$link
  transform <- switch(
    link %||% "",
    identity = identity, log = log, sqrt = sqrt,
    inverse = function(x) 1 / x, "1/mu^2" = function(x) 1 / x^2,
    NULL
  )
  if (is.null(transform) || !is.numeric(y)) {
    return(0)
  }
  y <- y[!is.na(y)]
  # A log or square root has no value below zero, which leaves no
  # spread to measure
  if (link %in% c("log", "sqrt") && any(y < 0)) {
    return(0)
  }
  if (link %in% c("log", "inverse", "1/mu^2")) {
    y <- ifelse(y == 0, y + 0.1, y)
  }
  spread <- round(stats::mad(transform(y)), 1)
  if (is.finite(spread)) spread else 0
}

#' The spread a trend's default priors are scaled by
#'
#' @param trend_obj An `mvgam_trend`, or NULL where the caller has none
#' @return The spread `trend_prior_unit()` recorded on the trend, or 1
#' @noRd
trend_prior_unit_of <- function(trend_obj) {
  checkmate::assert_list(trend_obj, null.ok = TRUE)
  trend_obj$prior_unit %||% 1
}


#' Is this class one of the loadings-kernel length-scales?
#'
#' `theta_features` and one `theta_dist_<name>` per supplied distance
#' matrix. They are named after the quantity and have no `_trend`
#' suffix. The set is open: a model declares one length-scale per
#' distance source it was given.
#'
#' @param param_name Character vector of class names.
#' @return Logical vector.
#' @noRd
is_loadings_length_scale <- function(param_name) {
  checkmate::assert_character(param_name, any.missing = FALSE)
  param_name == "theta_features" | grepl("^theta_dist_", param_name)
}


#' Default prior for a parameter type
#'
#' @param param_name Character string parameter name
#' @return List with prior, lb, ub elements
#' @noRd
get_parameter_type_default_prior <- function(param_name) {
  checkmate::assert_string(param_name)

  # Pattern matching for common parameter types
  if (identical(param_name, "nu_trend")) {
    # Innovation degrees of freedom. The lower bound of 2 is required:
    # below it the innovations have infinite variance and the
    # stationary initialisation of an autoregressive trend is
    # undefined.
    return(list(
      prior = common_trend_priors$nu_trend$default, lb = "2", ub = ""
    ))
  } else if (any(is_loadings_length_scale(param_name))) {
    # A kernel length-scale is a modelling choice. For a phylogenetic
    # distance it sets how fast covariance decays with relatedness. The
    # table reports it so a user can see and set it.
    return(list(prior = "lognormal(0, 1)", lb = "0", ub = ""))
  } else if (is_ar_coefficient(param_name)) {
    # AR coefficients: typically bounded [-1, 1] for stationarity
    return(list(prior = "normal(0, 0.5)", lb = "-1", ub = "1"))
  } else if (is_ar_partial(param_name)) {
    # Partial autocorrelation of a contiguous AR(p >= 2) trend. The
    # interval is the stationarity condition itself: every vector of
    # partial autocorrelations inside it gives a stationary
    # coefficient vector.
    return(list(prior = "normal(0, 0.5)", lb = "-1", ub = "1"))
  } else if (grepl("^mu_ar[0-9]+_pacf_trend$", param_name)) {
    # Population mean for a pooled partial autocorrelation
    return(list(prior = "normal(0, 0.5)", lb = "-1", ub = "1"))
  } else if (grepl("^sigma_ar[0-9]+_pacf_trend$", param_name)) {
    # Population scale for a pooled partial autocorrelation
    return(list(prior = "exponential(2)", lb = "0", ub = ""))
  } else if (grepl("^mu_ar[0-9]+_trend$", param_name)) {
    # Population mean for hierarchical AR coefficient (lag-specific)
    return(list(prior = "normal(0, 0.5)", lb = "-1", ub = "1"))
  } else if (grepl("^sigma_ar[0-9]+_trend$", param_name)) {
    # Population scale for hierarchical AR coefficient
    return(list(prior = "exponential(2)", lb = "0", ub = ""))
  } else if (grepl("^[AD]omega_trend$", param_name)) {
    # VAR/VARMA hyperprior precisions (Aomega_trend, Domega_trend)
    return(list(prior = "", lb = "0", ub = ""))
  }
  # Every other parameter is unbounded with Stan's default prior. These
  # are the correlation Cholesky factors, the VAR coefficient matrices,
  # their hyperprior means and the loadings `Z`.
  list(prior = "", lb = "", ub = "")
}

# =============================================================================
# Trend-specific prior resolvers
# =============================================================================
# `get_default_trend_parameter_prior()` takes the trend's
# `get_<trend>_parameter_prior()` through `trend_method()`. A resolver
# returns a prior, bounds or both for the parameters whose default
# depends on the trend. It returns NULL for every other parameter.

#' CAR bounds for its damping coefficient
#'
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_car_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")
  # A continuous-time damping coefficient is raised to a real power,
  # and Stan declares it strictly inside the unit interval. The shared
  # default supplies the distribution.
  if (identical(param_name, "ar1_trend")) {
    return(list(prior = "", lb = "0.001", ub = "0.999"))
  }
  NULL
}

#' LLT priors scaled to the number of time points
#'
#' A slope shock at time `t` moves the level at the end of the series
#' by `w_t = 1 + phi + ... + phi^(N - t - 1)`. The slope shocks move it
#' with variance `sigma_slope^2 v`, where `v` sums the squared
#' weights. `student_t(3, 0, 1.3 / sqrt(v))` on `sigma_slope_trend`
#' puts the prior median of that movement's standard deviation at one
#' link unit at every `N`. The movement itself then has prior median
#' 0.55, 90th percentile 2.8 and 99th percentile 8.4 link units. The
#' heavy tail lets an informative series move the posterior.
#'
#' The initial slope moves the level at time `N` by its reach,
#' `1 + phi + ... + phi^(N - 2)` per unit. A prior standard deviation
#' of two over the reach gives it two link units of growth over the
#' series at one standard deviation: `2 / (N - 1)` for an undamped
#' slope. A damped slope takes `v` and the reach at
#' `phi = 0.913`, the median of the `beta(18, 2)` prior on
#' `ar1_slope_trend`.
#'
#' A link unit is the responses' spread on their link scale where that
#' exceeds one, as `trend_prior_unit()` measures it. Factors with
#' sampled loadings have unit level shocks, whose path spreads by
#' `sqrt(N)` over the series. The priors take that spread as their
#' unit. Their loadings carry the data's spread.
#'
#' @param param_name Character string parameter name
#' @param trend_obj The prepared `LLT()` specification, carrying the
#'   data's dimensions
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_llt_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")
  if (identical(param_name, "ar1_slope_trend")) {
    return(list(prior = "beta(18, 2)", lb = "0", ub = "1"))
  }
  scaled <- c("sigma_slope_trend", "init_slope_trend",
              "sigma_season_trend", "init_season_trend")
  if (!param_name %in% scaled) {
    return(NULL)
  }
  n_time <- trend_obj$dimensions$n_time
  if (is.null(n_time)) {
    stop_mvgam_fault(
      "The LLT specification lacks the data's dimensions.",
      "The LLT slope priors scale with the number of time points."
    )
  }
  scales <- llt_prior_scales(n_time, isTRUE(trend_obj$damped),
                             samples_factor_loadings(trend_obj),
                             trend_prior_unit_of(trend_obj),
                             length(llt_season_frequencies(trend_obj)))
  switch(
    param_name,
    sigma_slope_trend = list(
      prior = paste0("student_t(3, 0, ", scales$sigma, ")"), lb = "0",
      ub = ""
    ),
    init_slope_trend = list(
      prior = paste0("normal(0, ", scales$init, ")"), lb = "", ub = ""
    ),
    sigma_season_trend = list(
      prior = paste0("student_t(3, 0, ", scales$season_sigma, ")"),
      lb = "0", ub = ""
    ),
    init_season_trend = list(
      prior = paste0("normal(0, ", scales$season_init, ")"), lb = "",
      ub = ""
    )
  )
}

#' The scales of the default LLT priors
#'
#' The Stan program also samples the slope and season parameters in
#' these units, which keeps a random start within a few prior scales of
#' zero.
#'
#' A seasonal state pair keeps its length under rotation. Each step's
#' shocks add `2 sigma_season^2` to its squared length. Over `N - 1`
#' steps the `K` harmonics move the season by a standard deviation of
#' `sigma_season sqrt(K (N - 1))`. `student_t(3, 0, 1.3 / sqrt(K (N -
#' 1)))` puts the prior median of that drift at one link unit, as the
#' slope prior does for the slope's movement. A starting pair at
#' `normal(0, 1 / sqrt(K))` gives the starting seasonal pattern a mean
#' square of one link unit squared.
#'
#' @param n_time Number of time points
#' @param damped Whether the slope is damped
#' @param unit_factors Whether every factor loading is sampled
#' @param spread The responses' spread on their link scale
#' @param n_harmonics Number of seasonal harmonics, zero without a
#'   season
#' @return List of `sigma` and `init`, each a scalar to four
#'   significant digits. With harmonics it adds `season_sigma` and
#'   `season_init`.
#' @noRd
llt_prior_scales <- function(n_time, damped, unit_factors, spread = 1,
                             n_harmonics = 0L) {
  checkmate::assert_int(n_time, lower = 3L)
  checkmate::assert_flag(damped)
  checkmate::assert_flag(unit_factors)
  checkmate::assert_number(spread, lower = 1)
  checkmate::assert_int(n_harmonics, lower = 0L)
  unit <- if (unit_factors) sqrt(n_time) else spread
  phi <- if (damped) 0.913 else 1
  # How far a unit slope at time `t` moves the level at time `N`
  reach <- function(steps) sum(phi^(seq_len(steps) - 1L))
  shocks <- vapply(seq_len(n_time - 2L), reach, numeric(1L))
  c(
    list(sigma = signif(1.3 * unit / sqrt(sum(shocks^2)), 4),
         init = signif(2 * unit / reach(n_time - 1L), 4)),
    if (n_harmonics > 0L) {
      list(
        season_sigma = signif(
          1.3 * unit / sqrt(n_harmonics * (n_time - 1L)), 4
        ),
        season_init = signif(unit / sqrt(n_harmonics), 4)
      )
    }
  )
}

#' PW priors scaled to the number of time points
#'
#' Time runs in steps. Each prior is set on the series' span:
#'
#' - `k_trend ~ normal(0, 2 / (N - 1))` gives two link units of growth
#'   over the series at one standard deviation, the scale of the
#'   undamped `LLT()` initial slope.
#' - `delta_trend ~ double_exponential(0, changepoint_scale / N)`
#'   makes `changepoint_scale` the typical change in growth over the
#'   series at one changepoint, as Prophet's time scaling does.
#' - `m_trend ~ normal((N + 1) / 2, N / 2)` centres the logistic
#'   inflection on the middle of the series.
#'
#' The linear form's growth is in link units, scaled by the responses'
#' spread on their link scale as `trend_prior_unit()` measures it. The
#' logistic form's growth is in logits of the capacity.
#'
#' @param param_name Character string parameter name
#' @param trend_obj The prepared `PW()` specification, carrying the
#'   data's dimensions
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_pw_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")
  if (!param_name %in% c("k_trend", "delta_trend", "m_trend")) {
    return(NULL)
  }
  n_time <- trend_obj$dimensions$n_time
  if (is.null(n_time)) {
    stop_mvgam_fault(
      "The PW specification lacks the data's dimensions.",
      "The PW priors scale with the number of time points."
    )
  }
  scales <- pw_prior_scales(n_time, trend_obj$changepoint_scale,
                            pw_growth_spread(trend_obj))
  prior <- switch(
    param_name,
    k_trend = paste0("normal(0, ", scales$rate, ")"),
    delta_trend = paste0("double_exponential(0, ", scales$changepoint, ")"),
    m_trend = paste0("normal(", (n_time + 1) / 2, ", ", n_time / 2, ")")
  )
  list(prior = prior, lb = "", ub = "")
}

#' The scales of the default PW growth priors, per step
#'
#' @param n_time Number of time points
#' @param changepoint_scale The `PW()` argument
#' @param spread The unit of growth, from `pw_growth_spread()`
#' @return List of `rate` and `changepoint`, each a scalar to four
#'   significant digits
#' @noRd
pw_prior_scales <- function(n_time, changepoint_scale, spread = 1) {
  checkmate::assert_int(n_time, lower = 2L)
  checkmate::assert_number(changepoint_scale, lower = 0)
  checkmate::assert_number(spread, lower = 1)
  list(rate = signif(2 * spread / (n_time - 1), 4),
       changepoint = signif(changepoint_scale * spread / n_time, 4))
}

#' The unit a PW trend's growth priors are set in
#'
#' @param trend_obj A `PW()` specification
#' @return The responses' spread for linear growth. Logistic growth,
#'   whose rate is in logits of the capacity, takes 1.
#' @noRd
pw_growth_spread <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")
  if (identical(pw_growth(trend_obj), "logistic")) {
    return(1)
  }
  trend_prior_unit_of(trend_obj)
}

# =============================================================================
# Prior combination
# =============================================================================
# One brmsprior holds the observation and trend priors, which the
# `_trend` suffix tells apart.

#' Combine observation and trend priors
#'
#' @param obs_priors A brmsprior object with observation model priors
#' @param trend_priors A brmsprior object with trend model priors
#' @return A combined brmsprior object
#' @noRd
combine_obs_trend_priors <- function(obs_priors, trend_priors) {
  checkmate::assert_class(obs_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_class(trend_priors, "brmsprior", null.ok = TRUE)

  bind_brmsprior_rows(list(obs_priors, trend_priors))
}


#' Align column schemas of one or more `brmsprior` frames
#'
#' brms's prior tables can carry different columns depending on which
#' generator built them. The observation side picks up newer columns
#' such as `tag`. Trend setup output and bare `prior()` rows lack them.
#' Returns a list with every input padded to the union of column names.
#' The tables then bind without losing data when brms adds columns.
#' Padding rule: character columns get `""`; everything else gets `NA`.
#'
#' @param prior_list A list of `brmsprior` data frames.
#' @return A list of the same length with all elements sharing the
#'   same column set in the same order.
#' @noRd
align_brmsprior_schemas <- function(prior_list) {
  checkmate::assert_list(prior_list, min.len = 1L)
  all_cols <- Reduce(union, lapply(prior_list, colnames))
  lapply(prior_list, function(p) {
    missing <- setdiff(all_cols, colnames(p))
    for (col in missing) {
      p[[col]] <- if (col %in% c("lb", "ub")) NA_real_ else ""
    }
    p[, all_cols, drop = FALSE]
  })
}

#' Normalise the `priors` / `prior` argument alias
#'
#' Accepts a `...` list and returns it with any `priors` entry
#' renamed to `prior`. brms uses the singular form. mvgam and jsdgam
#' also accept the plural. Internal code generation takes `prior` and
#' ignores `...$priors`.
#'
#' Errors if both forms are supplied and disagree.
#'
#' @param dots A list captured from `...`.
#' @return The same list with `prior` populated.
#' @noRd
normalise_prior_arg_alias <- function(dots) {
  checkmate::assert_list(dots)
  if (is.null(dots$priors)) {
    return(dots)
  }
  if (!is.null(dots$prior) && !identical(dots$prior, dots$priors)) {
    stop(insight::format_error(c(
      "The arguments 'prior' and 'priors' hold different values.",
      i = "Pass priors once via 'prior =' (brms convention)."
    )), call. = FALSE)
  }
  dots$prior <- dots$priors
  dots$priors <- NULL
  dots
}


#' Split a combined prior object down the middle
#'
#' brms takes the observation-side priors and mvgam keeps the rest. One
#' predicate decides both halves. They are then exact complements. A
#' class on both sides would reach brms, which has no parameter for
#' it.
#'
#' The split is by class. A split on the `_trend` suffix would miss
#' `Z`, which has no suffix and is still mvgam's.
#'
#' @param combined_priors A `brmsprior` holding both sides, or NULL.
#' @param side Which half to return, `"obs"` or `"trend"`.
#' @return A `brmsprior` with that half, or NULL when it is empty.
#' @noRd
filter_priors_by_side <- function(combined_priors, side) {
  checkmate::assert_class(combined_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_choice(side, c("obs", "trend"))
  if (is.null(combined_priors)) {
    return(NULL)
  }
  managed <- is_mvgam_managed_class(combined_priors$class)
  keep <- if (identical(side, "trend")) managed else !managed
  out <- combined_priors[keep, , drop = FALSE]
  if (nrow(out) == 0) {
    return(NULL)
  }
  structure(out, class = c("brmsprior", "data.frame"))
}


#' All mvgam-generated trend parameters
#'
#' @param trend_specs List of trend specifications from mv_spec
#' @return Character vector of all mvgam-generated parameter names
#' @noRd
get_all_mvgam_trend_parameters <- function(trend_specs) {
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  spec <- trend_spec_head(trend_specs)
  if (is.null(spec)) {
    return(character(0))
  }
  ensure_registry_initialized()
  all_mvgam_params <- if (inherits(spec, "mvgam_trend")) {
    generate_monitor_params(spec)
  }

  # These two belong to mvgam whenever they appear, whether or not a
  # given model samples them. This list decides what brms never
  # receives. Dropping one would let it reach brms as `sigma`, an
  # observation-side prior on the wrong scale.
  # `samples_innovation_scale()` reports what a model samples.
  unique(c(all_mvgam_params, "sigma_trend", "nu_trend"))
}

#' The columns that identify a prior row
#'
#' Two rows agreeing on all of them set the same prior.
#'
#' @noRd
brmsprior_key_cols <- c("class", "coef", "group", "resp", "dpar",
                        "nlpar", "lb", "ub")


#' One string per prior row, built from its key columns
#'
#' A missing entry is `NA` from `prior()` and `""` in a fitted table.
#' Both become `""`, which makes the two keys equal.
#'
#' @param tab A `brmsprior` data frame
#' @param cols Key columns to combine, from `brmsprior_key_cols`
#' @return Character vector with one key per row of `tab`
#' @noRd
brmsprior_row_keys <- function(tab, cols) {
  checkmate::assert_data_frame(tab)
  checkmate::assert_subset(cols, brmsprior_key_cols)
  cols <- intersect(cols, names(tab))
  # `c()` on a `brmsprior` adds priors. The columns are taken from a
  # plain list.
  values <- lapply(unclass(tab)[cols], function(x) {
    ifelse(is.na(x), "", as.character(x))
  })
  do.call(paste, c(values, sep = "\r"))
}


#' Merge user-supplied prior overrides onto a default prior table
#'
#' Each row of `user_priors` is matched against `default_priors` on
#' `brmsprior_key_cols`. An empty user-side key field (e.g.
#' `coef = ""`) matches every row of that class. Matched rows take the
#' user's `prior` string and `source`. A user row matching no default
#' is appended.
#'
#' @param default_priors A `brmsprior` data frame; the table to merge
#'   onto. Must include a `source` column.
#' @param user_priors A `brmsprior` data frame of user overrides, or
#'   NULL.
#' @param source String written to the `source` column of every
#'   matched row. Defaults to `"user"`. The family-default overlay
#'   passes `"default"`. `get_prior()` then reports those rows as
#'   defaults.
#' @return The merged `brmsprior` data frame
#' @noRd
merge_user_priors <- function(default_priors, user_priors,
                              source = "user") {
  checkmate::assert_class(default_priors, "brmsprior")
  checkmate::assert_class(user_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_string(source, min.chars = 1L)
  if (is.null(user_priors) || nrow(user_priors) == 0L) {
    return(default_priors)
  }
  key_cols <- intersect(
    brmsprior_key_cols,
    intersect(names(default_priors), names(user_priors))
  )
  is_wildcard <- function(x) is.na(x) | !nzchar(as.character(x))
  unmatched_rows <- list()
  for (i in seq_len(nrow(user_priors))) {
    row <- user_priors[i, , drop = FALSE]
    keep <- rep(TRUE, nrow(default_priors))
    for (col in key_cols) {
      full_val <- default_priors[[col]]
      user_val <- row[[col]]
      keep <- keep & (is_wildcard(user_val) |
                        (full_val == user_val) |
                        (is.na(full_val) & is.na(user_val)))
    }
    if (any(keep, na.rm = TRUE)) {
      default_priors$prior[keep] <- row$prior
      if ("source" %in% names(default_priors)) {
        default_priors$source[keep] <- source
      }
    } else {
      # A user row matching no default is kept. The parameters a trend
      # constructor samples, such as `ar1_trend`, have no brms default
      # row.
      unmatched_rows[[length(unmatched_rows) + 1L]] <- row
    }
  }
  if (length(unmatched_rows) > 0L) {
    # `prior()` returns fewer columns than `validate_prior()`. Each
    # unmatched row takes the schema of `default_priors` before
    # stacking.
    appended_rows <- align_brmsprior_schemas(
      c(list(default_priors), unmatched_rows)
    )[-1L]
    appended <- do.call(rbind, lapply(appended_rows, function(r) {
      if ("source" %in% names(r)) r$source <- source
      r
    }))
    default_priors <- rbind(default_priors, appended)
  }
  structure(default_priors, class = c("brmsprior", "data.frame"))
}


#' Report mvgam's injected family defaults in a prior table
#'
#' Stan code generation replaces the brms fallback prior for a few
#' families (see `family_default_priors()`). Without this overlay
#' `get_prior()` would show the brms fallback while the model sampled
#' under mvgam's. For `com_binomial()` it would show a positive-only
#' `gamma(2, 0.1)` on a `nu` declared with `lb = -5`.
#' The defaults are the ones `response_default_priors()` gives the
#' fit, one set per response.
#'
#' @param obs_priors The `brmsprior` returned by `brms::get_prior()`.
#' @param formula The observation formula.
#' @param family The validated family of the observation formula.
#' @return `obs_priors` with mvgam's defaults substituted in place.
#' @noRd
overlay_family_default_priors <- function(obs_priors, formula, family) {
  checkmate::assert_class(obs_priors, "brmsprior")
  defaults <- response_default_priors(formula, family)
  if (is.null(defaults) || nrow(defaults) == 0L) {
    return(obs_priors)
  }
  merge_user_priors(obs_priors, defaults, source = "default")
}


#' Assemble the post-fit prior table stored on an `mvgam` object
#'
#' Wraps the sequence:
#'   1. Re-suffix the brms-validated trend priors
#'      (`<class>` -> `<class>_trend`).
#'   2. Combine obs + trend rows into a single brmsprior frame.
#'   3. Append mvgam stanvar rows (Z_free_vec, varrho_inv, etc.).
#'   4. Layer the user's full `priors = ` argument over the result,
#'      marking matched rows as `source = "user"`. The mvgam-managed
#'      trend overrides (sigma_trend, ar1_trend, etc.) that the brms
#'      pipeline strips are then recorded on the fit.
#'
#' @param obs_priors The obs-side `brmsprior` from
#'   `setup_brms_lightweight()`.
#' @param trend_priors The trend-side `brmsprior` from
#'   `setup_brms_lightweight()`, or NULL when the fit has no trend
#'   model.
#' @param user_prior The user's original `prior` / `priors` argument
#'   (already aliased via `normalise_prior_arg_alias()`), or NULL.
#' @param combined_stancode The assembled Stan code.
#'   `lift_mvgam_stanvar_priors()` takes the mvgam-injected stanvar
#'   rows from it.
#' @param trend_stancode The trend-side brms program, or NULL.
#' @return A `brmsprior` data frame ready to land on `mvgam_object$prior`.
#' @noRd
assemble_stored_prior_table <- function(obs_priors, trend_priors,
                                          user_prior, combined_stancode,
                                          trend_stancode = NULL) {
  checkmate::assert_class(obs_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_class(trend_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_class(user_prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_character(combined_stancode, null.ok = TRUE)
  checkmate::assert_character(trend_stancode, null.ok = TRUE)
  combined <- combine_obs_trend_priors(
    obs_priors, add_trend_suffix_to_priors(trend_priors)
  )
  brms_trend <- setdiff(
    brms_declared_params(trend_stancode), brms_trend_dropped_params
  )
  lifted <- lift_mvgam_stanvar_priors(
    combined, combined_stancode,
    brms_owned = paste0(brms_trend, "_trend")
  )
  if (!is.null(user_prior) && nrow(user_prior) > 0L) {
    lifted <- merge_user_priors(lifted, user_prior)
  }
  # Hidden on the table the fit stores and `prior_summary()` returns.
  # The code generator uses another table, which keeps the pin.
  hide_obs_placeholder_prior(lifted)
}


#' Hide the empty-obs placeholder's pin from a user-facing prior table
#'
#' The pin holds a column mvgam had to invent at zero. It names no
#' parameter the user wrote. It stays in the table the code generator
#' uses. Without it the placeholder would be free.
#'
#' @param prior A `brmsprior` data frame, or any other object to
#'   return unchanged
#' @return The table without the pin's row and the `b` rows of the
#'   mean that holds the pin
#' @noRd
hide_obs_placeholder_prior <- function(prior) {
  if (!is.data.frame(prior) || !nrow(prior) || !"coef" %in% names(prior)) {
    return(prior)
  }
  # The placeholder is the sole coefficient of its mean. brms writes a
  # class-level `b` row for that mean, which also names nothing the
  # user wrote. That row goes with the pin.
  mean_of <- paste(prior$resp, prior$dpar, prior$nlpar)
  held <- mean_of[prior$coef == MVGAM_EMPTY_OBS_PLACEHOLDER]
  prior[!(prior$class == "b" & mean_of %in% held), , drop = FALSE]
}


#' Does this prior string hold its parameter at a constant?
#'
#' `constant()` names no distribution. brms implements it by moving the
#' parameter out of the `parameters` block and assigning the value. A
#' coefficient with a constant prior has no free parameter. Two
#' consequences follow from this predicate. mvgam's own prior emitters
#' cannot write a constant as a sampling statement and refuse it. The
#' identification check leaves a pinned column out of the design it
#' ranks.
#'
#' @param x Character vector of prior strings.
#' @return Logical vector, `TRUE` where the string is a `constant()`.
#' @noRd
is_constant_prior <- function(x) {
  checkmate::assert_atomic(x)
  if (length(x) == 0L) return(logical(0))
  grepl("^\\s*constant\\s*\\(", as.character(x))
}

#' Re-attach the `_trend` suffix to a brms-validated trend prior table
#'
#' Inverse of `remove_trend_suffix_from_priors`. Takes the prior table
#' returned by the trend-side `setup_brms_lightweight()` call, whose
#' classes have brms's own names like `b`, `sigma`, `sds`, `ar1`.
#' Suffixes every class with `_trend`, the convention of
#' `get_prior.mvgam_formula()`. Rows already suffixed (mvgam-injected
#' stanvars) are left untouched.
#'
#' The trend submodel reaches brms as a gaussian. brms then returns a
#' `sigma` row with its own residual-scale default. The trend's
#' process noise is `sigma_trend`, which the Stan generator samples
#' under `common_trend_priors`. Its row reaches the stored table from
#' the compiled code via `lift_mvgam_stanvar_priors()`. Suffixing
#' brms's row would file its value under mvgam's name. That row is
#' dropped. A row scoped to a coefficient belongs to a distributional
#' sub-formula and is kept.
#'
#' @param trend_priors A brmsprior data frame or NULL.
#' @return The same brmsprior with `class` re-suffixed, or NULL if
#'   `trend_priors` is NULL / empty.
#' @noRd
add_trend_suffix_to_priors <- function(trend_priors) {
  checkmate::assert_class(trend_priors, "brmsprior", null.ok = TRUE)
  if (is.null(trend_priors) || nrow(trend_priors) == 0L) return(NULL)
  out <- suffix_trend_prior_classes(trend_priors)
  structure(out, class = c("brmsprior", "data.frame"))
}


#' One column of a prior table, empty strings where it is absent
#'
#' A table built by hand or by an older brms may lack the `dpar`,
#' `nlpar`, `resp` or `coef` column.
#'
#' @param prior A prior table
#' @param col Character name of the column
#' @return Character vector with one entry per row
#' @noRd
prior_field <- function(prior, col) {
  checkmate::assert_data_frame(prior)
  checkmate::assert_string(col)
  prior[[col]] %||% rep("", nrow(prior))
}


#' Suffix a trend-side prior table's classes with `_trend`
#'
#' Both trend-side tables apply this rule. It drops brms's
#' residual-scale `sigma` row. It leaves an empty class empty and an
#' already-suffixed class unchanged.
#'
#' @param priors A prior table with a `class` column, or NULL.
#' @return The same table, minus brms's bookkeeping `sigma` row, with
#'   every remaining unsuffixed non-empty class suffixed.
#' @noRd
suffix_trend_prior_classes <- function(priors) {
  checkmate::assert_data_frame(priors, null.ok = TRUE)
  if (is.null(priors) || nrow(priors) == 0L) return(priors)
  bookkeeping_sigma <- priors$class %in% brms_trend_dropped_params &
    !nzchar(prior_field(priors, "coef"))
  priors <- priors[!bookkeeping_sigma, , drop = FALSE]
  if (nrow(priors) == 0L) return(priors)
  priors$class <- apply_trend_class_suffix(priors$class)
  priors
}


#' Turn off autoscaling for a trend-side shrinkage prior
#'
#' `horseshoe()` and `R2D2()` scale their global parameter by the
#' family's residual standard deviation, which brms writes as
#' `hs_scale_global * sigma` and `sigma^2 * R2D2_R2 / (1 - R2D2_R2)`.
#' The trend submodel reaches brms as a gaussian whose residual scale
#' mvgam takes back out: the process noise is `sigma_trend`, one scale
#' per latent series. The factor names a parameter the combined program
#' leaves undeclared. Under a gaussian observation family it resolves
#' to the observation model's own `sigma`, a different quantity. brms
#' takes `autoscale = FALSE` for this case.
#'
#' @param priors A `brmsprior` table scoped to the trend formula.
#' @return The same table, with `autoscale = FALSE` on each special
#'   prior that left it unset.
#' @noRd
disable_trend_prior_autoscale <- function(priors) {
  checkmate::assert_data_frame(priors)
  special <- "^\\s*(horseshoe|R2D2)\\s*\\("
  texts <- priors$prior %||% character(0)
  for (i in which(grepl(special, texts))) {
    call <- str2lang(texts[i])
    if ("autoscale" %in% names(call)) {
      if (identical(call$autoscale, FALSE)) {
        next
      }
      stop(insight::format_error(c(
        "A shrinkage prior on the trend must set 'autoscale = FALSE'.",
        x = paste0(
          "'", texts[i], "' was given for class '", priors$class[i], "'."
        ),
        i = paste0(
          "Autoscaling multiplies the prior by the family's residual ",
          "scale."
        ),
        i = "The trend's scale is 'sigma_trend', one per latent series."
      )), call. = FALSE)
    }
    call$autoscale <- FALSE
    priors$prior[i] <- paste(deparse(call), collapse = "")
  }
  priors
}


#' The user's priors on the trend formula's own terms, as brms names them
#'
#' `filter_priors_by_side()` leaves the rows scoped to the trend. They
#' are checked against the table `get_prior()` shows for the model.
#' mvgam then handles the rows for the parameters the trend
#' constructor samples. The rest belong to the brms model of the trend
#' formula, which names its classes with no `_trend` suffix.
#'
#' @param trend_priors `brmsprior` rows scoped to the trend formula.
#' @param trend_specs Trend specifications from `mv_spec`.
#' @param base_formula The trend formula with mvgam constructors removed.
#' @param data The trend-grain data frame.
#' @param codegen A list from `mvgam_codegen_options()`, or NULL.
#' @return `brmsprior` with the `_trend` suffix stripped, or NULL when
#'   no row belongs to brms.
#' @noRd
remove_trend_suffix_from_priors <- function(trend_priors, trend_specs,
                                            base_formula, data,
                                            codegen = NULL) {
  checkmate::assert_class(trend_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_class(base_formula, "formula")
  checkmate::assert_data_frame(data, min.rows = 1)

  if (is.null(trend_priors) || nrow(trend_priors) == 0) {
    return(NULL)
  }
  trend_priors <- translate_lv_axis_labels(
    trend_priors, trend_spec_head(trend_specs)$dimensions$by_lv_target,
    to = "brms"
  )
  trend_priors <- check_trend_priors(
    trend_priors,
    generate_trend_priors(trend_spec_head(trend_specs), base_formula,
                          data, codegen),
    get_all_mvgam_trend_parameters(trend_specs)
  )
  result <- trend_priors[!attr(trend_priors, "constructor"), , drop = FALSE]
  if (nrow(result) == 0L) {
    return(NULL)
  }
  result$class <- gsub("_trend$", "", result$class)
  result <- disable_trend_prior_autoscale(result)

  structure(result, class = c("brmsprior", "data.frame"))
}


#' Check the user's trend priors against the trend's parameters
#'
#' Each row must match a row of the table `get_prior()` shows. A prior
#' on a class the model lacks, such as `sigma_trend` under `PW()`,
#' would otherwise be dropped unseen. The bounds of a parameter the
#' trend constructor samples are fixed by its Stan declaration. A
#' class-wide row is dropped with a warning when every coefficient of
#' the class has a prior of its own, which is how brms sets the
#' length-scale of each `gp()` term.
#'
#' @param priors The user's rows scoped to the trend
#' @param defaults The trend rows of `get_prior()`
#' @param constructor_classes Classes the trend constructor samples
#' @return The rows the model will use, with a logical `constructor`
#'   attribute marking the rows mvgam handles
#' @noRd
check_trend_priors <- function(priors, defaults, constructor_classes) {
  checkmate::assert_data_frame(priors)
  checkmate::assert_data_frame(defaults)
  checkmate::assert_character(constructor_classes)
  cell <- intersect(setdiff(brmsprior_key_cols, c("coef", "lb", "ub")),
                    intersect(names(priors), names(defaults)))
  key <- brmsprior_row_keys
  describe <- function(tab) {
    paste0("class = ", tab$class,
           ifelse(nzchar(tab$coef), paste0(", coef = ", tab$coef), ""),
           ifelse(nzchar(tab$group), paste0(", group = ", tab$group), ""))
  }
  refuse <- function(headline, rows, hint) {
    lines <- describe(priors[rows, , drop = FALSE])
    stop(insight::format_error(c(
      headline, stats::setNames(lines, rep("x", length(lines))), i = hint
    )), call. = FALSE)
  }

  unknown <- !key(priors, c(cell, "coef")) %in% key(defaults, c(cell, "coef"))
  if (any(unknown)) {
    refuse("Each trend prior must match a parameter of the trend.",
           unknown,
           "'get_prior()' lists the classes and coefficients of the model.")
  }
  constructor <- priors$class %in% constructor_classes
  has_bound <- function(x) !is.na(x) & nzchar(as.character(x))
  # A fitted prior table records the declared bounds. A row restating
  # them, as `update()` inherits it, sets the same bounds.
  declared <- key(priors, c(cell, "coef", "lb", "ub")) %in%
    key(defaults, c(cell, "coef", "lb", "ub"))
  bounded <- constructor & !declared &
    (has_bound(priors$lb) | has_bound(priors$ub))
  if (any(bounded)) {
    refuse("The trend sets the bounds of its own parameters.", bounded,
           "Drop 'lb' and 'ub' from these priors.")
  }

  # The prior each coefficient ends with: the user's row, else the
  # default.
  own <- defaults[nzchar(defaults$coef), , drop = FALSE]
  given <- match(key(own, c(cell, "coef")), key(priors, c(cell, "coef")))
  own$prior[!is.na(given)] <- priors$prior[given[!is.na(given)]]
  unused <- vapply(seq_len(nrow(priors)), function(i) {
    if (nzchar(priors$coef[i])) {
      return(FALSE)
    }
    siblings <- own$prior[key(own, cell) == key(priors[i, ], cell)]
    length(siblings) > 0L && all(nzchar(siblings))
  }, logical(1L))
  for (i in which(unused)) {
    coefs <- own$coef[key(own, cell) == key(priors[i, ], cell)]
    insight::format_warning(c(
      paste0("Every coefficient of class '", priors$class[i],
             "' has its own prior."),
      x = paste0("The class-wide prior '", priors$prior[i],
                 "' was dropped."),
      i = paste0("Set it by coefficient: prior(", priors$prior[i],
                 ", class = ", priors$class[i], ", coef = ", coefs[1L], ").")
    ))
  }
  structure(priors[!unused, , drop = FALSE],
            constructor = constructor[!unused])
}



#' The user's prior on a class mvgam samples itself
#'
#' The trend constructor's parameters are set by class alone, as in
#' `prior(exponential(2), class = sigma_trend)`.
#'
#' @param prior_frame A `brmsprior` of user priors
#' @param class_name The class, such as `"sigma_trend"`
#' @return The prior string, or `NULL` when the user gave none
#' @noRd
extract_prior_string <- function(prior_frame, class_name) {
  checkmate::assert_class(prior_frame, "brmsprior")
  checkmate::assert_string(class_name, min.chars = 1)
  rows <- prior_frame$class == class_name &
    (is.na(prior_frame$coef) | !nzchar(prior_frame$coef))
  given <- trimws(prior_frame$prior[rows])
  given <- given[!is.na(given) & nzchar(given)]
  if (!length(given)) {
    return(NULL)
  }
  given[[1L]]
}


#' The prior a trend parameter is sampled under
#'
#' A user prior wins where one is given. The default comes from
#' `get_default_trend_parameter_prior()`, which also builds the table
#' `get_prior()` shows.
#'
#' @param prior A `brmsprior` of user priors, or NULL
#' @param param_name The class, such as `"sigma_trend"`
#' @param trend_obj Optional `mvgam_trend`. Supply it wherever a trend
#'   resolves a prior from its own arguments, as `PW()` does for the
#'   changepoint scale.
#' @return A Stan distribution such as `"exponential(2)"`, or `""`
#'   where Stan's default applies
#' @noRd
get_trend_parameter_prior <- function(prior = NULL, param_name,
                                      trend_obj = NULL) {
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_string(param_name, min.chars = 1)
  checkmate::assert_class(trend_obj, "mvgam_trend", null.ok = TRUE)

  user_prior <- if (!is.null(prior)) extract_prior_string(prior, param_name)
  if (is.null(user_prior)) {
    return(get_default_trend_parameter_prior(param_name, trend_obj)$prior)
  }
  # brms implements `constant()` by moving the parameter out of the
  # parameters block. Written as a sampling statement it reaches Stan
  # as a call to `constant_lpdf`, which does not exist.
  if (is_constant_prior(user_prior)) {
    stop(insight::format_error(c(
      paste0("A 'constant()' prior is not supported for '",
             param_name, "'."),
      x = "Stan samples this parameter from a distribution.",
      i = paste0("Give '", param_name, "' a narrow proper prior or ",
                 "fix it through its trend constructor argument.")
    )), call. = FALSE)
  }
  user_prior
}

# =============================================================================
# Utilities
# =============================================================================

#' Null-coalescing operator
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# =============================================================================
# mvgam_formula interface
# =============================================================================
# `mvgam_formula()` pairs an observation formula with a trend formula
# for `get_prior()`, `stancode()` and `standata()`.

#' Create an mvgam formula object
#'
#' @description
#' Constructs a lightweight mvgam formula object that combines
#' observation and trend model formulas. The inspection functions
#' \code{get_prior()}, \code{stancode()} and \code{standata()} take it.
#'
#' @param formula An object of class \code{formula}, \code{brmsformula}, or
#'   \code{mvbrmsformula} describing the observation model.
#' @param trend_formula An optional formula describing trend dynamics. Default
#'   NULL results in pure brms equivalent model. See Details for trend syntax.
#'
#' @details
#' The mvgam_formula object is a minimal container that pairs an observation
#' formula with an optional trend formula. Data, family, and other model
#' specifications are provided when calling inspection or fitting functions:
#'
#' \code{get_prior(mvgam_formula(y ~ x, ~ AR()), data = dat,
#'   family = poisson())}
#'
#' When \code{trend_formula = NULL}, the model reduces to a pure brms
#' observation model. When specified, the trend_formula can include:
#' \itemize{
#'   \item Trend constructors: \code{RW()}, \code{AR()}, \code{CAR()},
#'     \code{ZMVN()}, \code{VAR()}, \code{LLT()}, \code{PW()}
#'   \item Covariates that affect trend dynamics
#'   \item Smooth terms using mgcv syntax: \code{s()}, \code{t2()}
#'   \item Random effects: \code{(1|group)}, \code{(slope|group)}
#'   \item Gaussian processes: \code{gp()}
#' }
#'
#' @section Trend formula restrictions:
#' The trend_formula has specific restrictions to maintain compatibility with
#' State-Space dynamics and prevent conflicts between observation-level and
#' trend-level modeling:
#'
#' \strong{Forbidden Terms:}
#' \itemize{
#'   \item brms addition-terms: \code{weights()}, \code{se()},
#'     \code{cens()}, \code{trunc()}, \code{trials()}, \code{rate()},
#'     \code{vreal()}, \code{vint()}, \code{subset()}, \code{index()},
#'     \code{dec()}, \code{cat()}, \code{thres()}
#'   \item Missing-value imputation: \code{mi()}
#'   \item brms autocorrelation: \code{ar()}, \code{ma()}, \code{arma()},
#'     \code{cosy()}, \code{unstr()}, \code{autocor()}
#'   \item Offset terms: \code{offset()}
#' }
#'
#' \strong{Rationale:}
#' Addition terms change the observation model, through weights,
#' censoring or exposure. They belong in the observation formula.
#' Autocorrelation terms conflict with the trend's state-space
#' dynamics. brms imputes a \code{mi()} predictor as a response of the
#' model. A trend formula holds no response.
#'
#' @section Multivariate models, single shared trend type:
#' For multivariate observation models (\code{mvbind()} or
#' \code{bf() + bf()}), \strong{a single trend constructor applies to
#' all responses}. Different trend types per response (e.g.
#' \code{RW()} for one response and \code{AR(p = 3)} for another) are
#' \strong{not supported and will not be added}. mvgam stores latent
#' trend states in a single \code{lv_trend} matrix with shared dynamics
#' parameters; mixing trend types per response would require parallel
#' dynamics machinery throughout the codegen, sampling, and prediction
#' pipelines.
#'
#' What multivariate models support:
#' \itemize{
#'   \item Different observation families per response (e.g.
#'     \code{bf(count ~ x, family = poisson()) +
#'     bf(biomass ~ x, family = Gamma())}).
#'   \item Different fixed-effect, smooth, or GP terms per response in
#'     the observation formulas.
#'   \item Hierarchical / grouped trends via the \code{gr} argument on
#'     the (single) trend constructor.
#' }
#'
#' If you need different dynamics per response, fit
#' independent univariate \code{mvgam()} models for each response.
#'
#' \strong{Examples:}
#' \preformatted{
#' # Correct usage
#' mvgam_formula(y ~ x + weights(w), trend_formula = ~ AR(p = 1))
#' mvgam_formula(count ~ offset(log_exposure), trend_formula = ~ RW())
#'
#' # Incorrect usage (will error)
#' mvgam_formula(y ~ x, trend_formula = ~ AR() + weights(w))
#' mvgam_formula(y ~ x, trend_formula = ~ RW() + offset(z))
#' }
#'
#' @return An object of class \code{c("mvgam_formula", base_class)} where
#'   base_class is the class of the input formula, containing:
#' \itemize{
#'   \item \code{formula}: The observation model formula
#'   \item \code{trend_formula}: The trend model formula (or NULL)
#' }
#'
#' @seealso
#' \code{\link{get_prior.mvgam_formula}}, \code{\link[brms]{stancode}},
#' \code{\link[brms]{standata}}, \code{\link{mvgam}}
#'
#' @family model fitting
#' @export
mvgam_formula <- function(formula, trend_formula = NULL) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    .var.name = "formula"
  )

  # Validate observation formula for GP usage and trend constructors
  validate_obs_formula_brms(formula)

  # Validate trend_formula if provided
  if (!is.null(trend_formula)) {
    # A `bf()` trend formula carries distributional or non-linear
    # sub-formulas, which the latent state cannot take. Named here
    # before the generic "Must be a formula" assertion runs.
    if (inherits(trend_formula, c("brmsformula", "bform"))) {
      subs <- names(trend_formula$pforms)
      stop(insight::format_error(c(
        "'trend_formula' must be a one-sided formula such as '~ x + AR()'.",
        x = if (length(subs)) {
          paste0("Found 'bf()' sub-formulas for: ",
                 paste0("'", subs, "'", collapse = ", "), ".")
        } else {
          "Found a 'bf()' formula."
        },
        i = paste0("Write distributional and non-linear formulas with ",
                   "'bf()' in the observation 'formula'.")
      )), call. = FALSE)
    }

    checkmate::assert_formula(trend_formula, .var.name = "trend_formula")

    validate_trend_formula(trend_formula)
  }

  # Formula type, stored as an attribute
  if (inherits(formula, "mvbrmsformula")) {
    formula_class <- "mvbrmsformula"
  } else if (inherits(formula, "brmsformula")) {
    formula_class <- "brmsformula"
  } else {
    formula_class <- "formula"
  }

  out <- list(
    formula = formula,
    trend_formula = trend_formula
  )

  # The S3 class hierarchy keeps the original formula class
  if (formula_class == "mvbrmsformula") {
    class(out) <- c("mvgam_formula", "mvbrmsformula")
  } else if (formula_class == "brmsformula") {
    class(out) <- c("mvgam_formula", "brmsformula")
  } else {
    class(out) <- c("mvgam_formula", "formula")
  }
  attr(out, "formula_class") <- formula_class

  out
}

#' The priors of a model
#'
#' @description
#' Methods:
#' \itemize{
#'   \item [get_prior.mvgam_formula()] lists the priors of a model
#'     before it is fitted.
#'   \item [get_prior.mvgam()] returns the prior table of a model.
#'   \item [get_prior.default()] takes a `formula` or `brmsformula`
#'     with the arguments of [get_prior.mvgam_formula()].
#' }
#' `default_prior()` is the same generic under the newer brms name.
#'
#' @param object An [mvgam_formula()], an `mvgam` model, or a model
#'   formula.
#' @param ... Arguments of the method.
#'
#' @return A `brmsprior` data frame.
#'
#' @seealso [mvgam_formula()], [brms::get_prior()]
#' @export
get_prior <- function(object, ...) {
  UseMethod("get_prior")
}

#' @rdname get_prior
#' @export
default_prior <- function(object, ...) {
  UseMethod("default_prior")
}

#' Whether a formula call names a model only mvgam builds
#'
#' A `trend_formula` names one. So does a family mvgam builds, which
#' carries `mvgam_` attributes and Stan code brms cannot write.
#' The family is the call's `family` argument, its second positional
#' argument after `data`, or one a response's `bf()` holds.
#'
#' @param object A formula or brmsformula
#' @param dots The `...` of the calling method, as a list
#' @return TRUE or FALSE
#' @noRd
names_mvgam_model <- function(object, dots) {
  if (!is.null(dots$trend_formula)) {
    return(TRUE)
  }
  arg_names <- names(dots) %||% rep("", length(dots))
  positional <- dots[!nzchar(arg_names)]
  family <- dots$family %||% if (length(positional) >= 2L) positional[[2L]]
  families <- formula_families(object, validate_family(family %||%
                                                         gaussian()))
  built <- vapply(families, function(f) {
    any(startsWith(names(attributes(validate_family(f))), "mvgam_"))
  }, logical(1L))
  any(built)
}

#' Dispatch a formula call on the `mvgam_formula` it describes
#'
#' The brms path takes the formula and discards a `trend_formula`. It
#' also writes no Stan code for a family mvgam builds. Its result would
#' omit either without any sign. Building the `mvgam_formula` and
#' dispatching on it covers both.
#'
#' `get_prior()` routes every call. `stancode()` and `standata()` route
#' a call `names_mvgam_model()` accepts. Other calls go to brms through
#' `NextMethod()`, which works only when called from the method
#' itself.
#' `brms::make_stancode()` calls the `stancode()` generic, which would
#' dispatch back to the mvgam method and recurse until the C stack
#' fills.
#'
#' @param object A formula or brmsformula
#' @param dots The `...` of the calling method, as a list, carrying
#'   any `trend_formula`
#' @param mvgam_fun The mvgam generic to dispatch on
#' @return Whatever `mvgam_fun` returns
#' @noRd
route_to_mvgam_formula <- function(object, dots, mvgam_fun) {
  checkmate::assert(
    checkmate::check_formula(object),
    checkmate::check_list(object),
    .var.name = "object"
  )
  checkmate::assert_list(dots)
  checkmate::assert_function(mvgam_fun)
  trend_formula <- dots$trend_formula
  dots$trend_formula <- NULL

  do.call(
    mvgam_fun,
    c(list(mvgam_formula(object, trend_formula = trend_formula)), dots)
  )
}

#' The priors of a model formula
#'
#' Covers a `formula`, `brmsformula` or `mvbrmsformula`, which it
#' wraps in the \code{\link{mvgam_formula}} it describes with any
#' `trend_formula` in the call. The table then carries the defaults
#' mvgam gives its own families, with or without a trend.
#'
#' @param object Model specification object
#' @param ... Arguments of [get_prior.mvgam_formula()], such as
#'   `data`, `family` and `trend_formula`
#' @return A \code{brmsprior} data frame
#' @export
get_prior.default <- function(object, ...) {
  route_to_mvgam_formula(object, list(...), get_prior)
}

#' @rdname get_prior.default
#' @export
default_prior.default <- function(object, ...) {
  get_prior.default(object, ...)
}

#' The prior table of an mvgam model
#'
#' `get_prior()`, `default_prior()` and [prior_summary()] return the
#' one prior table a model was built with. Edit it and pass it to
#' `update(fit, prior = ...)` to refit under other priors without
#' recompiling.
#'
#' @param object An `mvgam` model, fitted or built with
#'   `run_model = FALSE`.
#' @param ... Unused. Anything passed here is refused.
#' @return A `brmsprior` data frame of the observation and trend
#'   priors. A row the user set has `source = "user"`. \pkg{brms}
#'   sets the source of the others, mostly `"default"`.
#' @family priors
#' @export
get_prior.mvgam <- function(object, ...) {
  rlang::check_dots_empty()
  prior_summary.mvgam(object)
}

#' @rdname get_prior.mvgam
#' @method default_prior mvgam
#' @export
default_prior.mvgam <- function(object, ...) {
  get_prior.mvgam(object, ...)
}


#' The priors an mvgam model takes, before fitting
#'
#' @description
#' Lists the priors of the observation and trend parts of the model an
#' [mvgam_formula()] describes, with their defaults for the data given.
#' Without a `trend_formula` the table matches that of
#' [brms::get_prior()]. `default_prior()` is the same method under the
#' newer brms name.
#'
#' @inheritParams stancode.mvgam_formula
#' @inheritParams mvgam
#' @param ... Further arguments of [brms::get_prior()], such as
#'   `knots` or `drop_unused_levels`.
#'
#' @return A \code{brmsprior} data frame of the observation and trend
#'   priors, usable with \code{brms::set_prior()} and
#'   \code{brms::prior()}. A trend class carries the \code{_trend}
#'   suffix.
#'
#' @details
#' The observation priors are those \code{brms::get_prior()} gives,
#' with mvgam's defaults for the families it adds. The trend rows list
#' every parameter the trend constructor samples and the coefficients
#' of the trend formula.
#'
#' \strong{Default trend priors.} brms sets the scale of its default
#' intercept, standard deviation and residual priors to
#' \code{max(2.5, mad(link(y)))}, from the spread of the response on
#' the link scale under the identity, log, inverse, square-root and
#' inverse-square links. That widens its base scale of 2.5 by the
#' factor \eqn{s = \max(1, \mathrm{mad}(\mathrm{link}(y)) / 2.5)}.
#' mvgam widens the trend's scale priors by the same factor, taking the
#' widest over several responses and \eqn{s = 1} under any other
#' link:
#' \itemize{
#'   \item \code{sigma_trend} and \code{sigma_group_trend} take
#'     \code{exponential(2 / s)}, with mean \eqn{s / 2}.
#'   \item The factor loadings \code{Z} take
#'     \code{student_t(3, 0, 0.5 s)}.
#'   \item The slope priors of \code{\link{LLT}()} and the growth priors
#'     of a linear \code{\link{PW}()} also scale with the number of
#'     time points. Their help pages give the forms.
#' }
#' A count model on the log link rarely spreads past 2.5. Its
#' defaults keep their link-scale values. Coefficients without
#' units keep fixed priors: autoregressive and damping coefficients,
#' correlations and degrees of freedom. The table this returns holds
#' the resolved values for the data given.
#'
#' A shrinkage prior on a trend-side class is written without
#' autoscaling. \code{horseshoe()} and \code{R2D2()} scale their global
#' parameter by the family's residual standard deviation. The trend's
#' own scale is \code{sigma_trend}, one per latent series. A family's
#' residual scale has no meaning on the trend side. Those rows take
#' \code{autoscale = FALSE}. Writing \code{autoscale = TRUE} on a
#' trend-side class is refused.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' dat <- sim_mvgam(family = poisson(), n_series = 2L,
#'                  n_timepoints = 120L, trend_model = AR())$data_train
#'
#' # Every prior the model takes, before any fitting. Trend classes
#' # carry a `_trend` suffix: `b` belongs to the observation formula
#' # and `b_trend` would belong to the trend formula.
#' mf <- mvgam_formula(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1))
#' get_prior(mf, data = dat, family = poisson())
#'
#' # Replace three defaults. Each `prior()` call gives one row.
#' my_priors <- c(
#'   prior(std_normal(), class = b),
#'   prior(inv_gamma(5, 1), class = sigma_trend),
#'   prior(normal(0.5, 0.25), class = ar1_trend)
#' )
#'
#' # `run_model = FALSE` builds the model without sampling. Its prior
#' # table marks the replaced rows as "user".
#' pre <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'              data = dat, family = poisson(), prior = my_priors,
#'              run_model = FALSE)
#' prior_summary(pre)
#'
#' # Next, drop `run_model = FALSE` to fit the model under these priors
#' }
#'
#' @seealso [mvgam_formula()], [brms::prior()] and
#'   [brms::set_prior()]. The
#'   `r mvgam_article("hierarchical_var", "hierarchical VAR article")`
#'   sets priors on the observation error and the trend correlations.
#' @family priors
#' @export
get_prior.mvgam_formula <- function(object, data, family = gaussian(),
                                    loadings_prior = NULL,
                                    data2 = NULL,
                                    trend_map = NULL, ...) {
  checkmate::assert_class(object, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)

  # `object$formula` would trigger S3 dispatch on the formula class
  formula <- object[[1]]
  trend_formula <- object[[2]]

  # The family a univariate `bf()` names becomes the model's. Each
  # response's family is checked, as it is when the model is built.
  resolved <- resolve_observation_family(formula, family)
  formula <- resolved$formula
  family <- resolved$family

  # A formula declining every coefficient (`y ~ 0` for a pure-trend
  # state-space model) is built with a pinned placeholder. brms has no
  # parameter class to set a default on without one. The table comes
  # from the same formula the model is built from. The pin is hidden
  # here as it is on a fit's stored table.
  injected <- inject_obs_zero_placeholder(formula, data, prior = NULL)
  # brms applies `family` to every response that names none, as the
  # fit does.
  obs_priors <- brms::get_prior(formula = injected$formula,
                                data = injected$data,
                                family = family, ...)
  obs_priors <- overlay_family_default_priors(obs_priors, formula, family)
  obs_priors <- hide_obs_placeholder_prior(obs_priors)

  if (is.null(trend_formula)) {
    return(obs_priors)
  }

  # Each response's column, named by its key, as the fitting path
  # builds them
  response_vars <- response_columns(formula)

  mv_spec <- parse_multivariate_trends(formula, trend_formula)

  mv_spec$trend_specs <- prepare_trend_specs(
    mv_spec$trend_specs, trend_map, loadings_prior, data, data2
  )

  # The spec and trend data Stan generation builds from. The base
  # formula has its `by = lv_axis()` terms rewritten to the columns
  # the trend data holds.
  components <- extract_and_validate_trend_components(
    data, mv_spec, response_vars, family = family
  )
  trend_head <- trend_spec_head(components$enhanced_mv_spec$trend_specs)
  trend_priors <- generate_trend_priors(
    trend_model = trend_head,
    base_formula = components$enhanced_mv_spec$base_formula,
    data = components$trend_data,
    codegen = codegen_from_dots(list(...))
  )
  trend_priors <- translate_lv_axis_labels(
    trend_priors, trend_head$dimensions$by_lv_target, to = "user"
  )

  combine_obs_trend_priors(obs_priors, trend_priors)
}

#' @rdname get_prior.mvgam_formula
#' @method default_prior mvgam_formula
#' @export
default_prior.mvgam_formula <- function(object, ...) {
  get_prior.mvgam_formula(object, ...)
}


