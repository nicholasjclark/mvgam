#' Prior Specification and Inspection System for mvgam
#'
#' @description
#' Prior extraction, combination, and inspection system for mvgam
#' models. This file provides functions for working with priors in both
#' observation and trend components, using the brms brmsprior class
#' throughout.
#'
#' @section Architecture:
#' The prior system uses brmsprior objects directly for maximum compatibility:
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
# SECTION 1: COMMON TREND PRIOR SPECIFICATIONS
# =============================================================================
# Prior specifications shared by parameters that appear in more than one
# trend type (sigma_trend is used by RW, AR and CAR), defined once so the
# trends cannot drift apart on a default.

#' Common Prior Specifications for Trend Parameters
#'
#' @description
#' Default priors for the trend parameters several trend types share,
#' written once for all of them. Each specification holds a Stan
#' distribution string, parameter bounds, a description and a
#' dimension.
#'
#' @format Named list with parameter specifications. Each element is a list with:
#' \describe{
#'   \item{default}{Character string of Stan distribution (e.g., "exponential(2)")}
#'   \item{bounds}{Numeric vector c(lower, upper) with NA for unbounded}
#'   \item{description}{Character string describing the parameter}
#'   \item{dimension}{Character string: "vector", "matrix", or "scalar"}
#' }
#'
#' Available common parameters:
#' \describe{
#'   \item{sigma_trend}{Innovation standard deviation (RW, AR, CAR trends)}
#'   \item{ar1_trend}{AR(1) coefficient (AR, CAR trends)}
#'   \item{nu_trend}{Innovation degrees of freedom when `df = NA`}
#'   \item{Z}{Factor loadings matrix for factor models}
#' }
#'
#' @noRd
common_trend_priors <- list(
  sigma_trend = list(
    default = "exponential(2)",
    bounds = c(0, NA),
    description = "Innovation standard deviation",
    dimension = "vector"
  ),

  # The residual scale the mvn() and mvt() families declare. It
  # carries no `_trend` suffix because the parameter is named after
  # the quantity, and it resolves through the same chain as every
  # entry here.
  Psi = list(
    default = "exponential(1)",
    bounds = c(0, NA),
    description = "Residual standard deviation per response component",
    dimension = "vector"
  ),

  ar1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1),
    description = "AR(1) coefficient",
    dimension = "vector"
  ),

  nu_trend = list(
    default = "gamma(4, 0.3)",
    bounds = c(2, NA),
    description = "Innovation degrees of freedom",
    dimension = "scalar"
  ),

  alpha_cor_trend = list(
    default = "beta(3, 2)",
    bounds = c(0, 1),
    description = "Hierarchical correlation mixing parameter",
    dimension = "scalar"
  ),

  m_trend = list(
    default = "student_t(3, 0, 2.5)",
    bounds = c(NA, NA),
    description = "Piecewise trend offset",
    dimension = "vector"
  ),

  k_trend = list(
    default = "std_normal()",
    bounds = c(NA, NA),
    description = "Piecewise base growth rate",
    dimension = "vector"
  ),

  sigma_group_trend = list(
    default = "exponential(2)",
    bounds = c(0, NA),
    description = "Per-group innovation standard deviation",
    dimension = "vector"
  ),

  L_Omega_trend = list(
    default = "lkj_corr_cholesky(2)",
    bounds = c(NA, NA),
    description = "Trend correlation Cholesky factor",
    dimension = "matrix"
  ),

  L_Omega_global_trend = list(
    default = "lkj_corr_cholesky(1)",
    bounds = c(NA, NA),
    description = "Population correlation Cholesky factor, grouped trend",
    dimension = "matrix"
  ),

  L_deviation_group_trend = list(
    default = "lkj_corr_cholesky(6)",
    bounds = c(NA, NA),
    description = "Per-group deviation Cholesky factor, grouped trend",
    dimension = "matrix"
  ),

  Z = list(
    default = "student_t(3, 0, 0.5)",
    bounds = c(NA, NA),
    description = "Factor loadings matrix",
    dimension = "matrix"
  ),

  theta1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1),
    description = "MA(1) coefficient",
    dimension = "vector"
  ),

  Amu_trend = list(
    default = "normal(0, sqrt(0.455))",
    bounds = c(NA, NA),
    description = "Mean of the VAR coefficients, diagonal and off",
    dimension = "vector"
  ),

  Aomega_trend = list(
    default = "gamma(1.365, 0.071175)",
    bounds = c(0, NA),
    description = "Precision of the VAR coefficients, diagonal and off",
    dimension = "vector"
  ),

  Dmu_trend = list(
    default = "normal(0, 1)",
    bounds = c(NA, NA),
    description = "Mean of the VARMA moving-average coefficients",
    dimension = "vector"
  ),

  Domega_trend = list(
    default = "gamma(2, 1)",
    bounds = c(0, NA),
    description = "Precision of the VARMA moving-average coefficients",
    dimension = "vector"
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
    # `Intercept_trend`, and `~ 0` and `~ -1` give no rows.
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
#' brms builds prior tables with different columns: the observation
#' side has newer columns such as `tag`, and a bare `prior()` row
#' lacks them. Every table is padded
#' to the union of the columns by `align_brmsprior_schemas()`.
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
  combined <- do.call(rbind, align_brmsprior_schemas(prior_list))
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

  # Filter out correlation parameters for single-series trends
  # L_Omega_trend only makes sense with multiple series (n_series > 1)
  if ("L_Omega_trend" %in% monitor_params &&
      !is.null(trend_obj$dimensions) &&
      trend_obj$dimensions$n_series == 1) {
    monitor_params <- setdiff(monitor_params, "L_Omega_trend")
  }

  # Under either sharing mode the per-series coefficient is derived
  # from something else the program samples: a broadcast scalar under
  # `"shared"`, and a population mean and scale under
  # `"hierarchical"`. A prior row for the derived vector promises an
  # override the program would discard. The editable rows are the
  # parameters each mode samples. `generate_monitor_params()` keeps
  # the coefficients, because mvgam withholds them from brms.
  sharing <- ar_coef_sharing(trend_obj)
  if (sharing %in% c("shared", "hierarchical")) {
    monitor_params <- monitor_params[
      !(is_ar_coefficient(monitor_params) | is_ar_partial(monitor_params))
    ]
  }

  if (length(monitor_params) == 0) {
    return(brms::empty_prior())
  }

  # Generate prior specifications for each parameter
  prior_data <- lapply(monitor_params, function(param) {
    create_trend_parameter_prior(param, trend_obj)
  })

  # Combine into data frame
  combined_priors <- do.call(rbind, prior_data)

  # Convert to brmsprior object
  class(combined_priors) <- c("brmsprior", "data.frame")

  return(combined_priors)
}

#' Create Prior Specification for Single Trend Parameter
#'
#' @param param_name Character string parameter name (e.g., "ar1_trend")
#' @param trend_obj mvgam_trend object for context
#' @return Single-row data frame with prior specification
#' @noRd
create_trend_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")

  # Get default prior and bounds for this parameter type
  prior_info <- get_default_trend_parameter_prior(param_name, trend_obj)

  # `Z` is drawn from one of three branches depending on the
  # loadings prior; only the unstructured one matches the registry
  # default. Both the branch and its statement come from the
  # emitter's own helpers, so the table cannot describe a prior the
  # model does not sample.
  if (identical(param_name, "Z")) {
    branch_prior <- loadings_z_prior_string(
      loadings_z_branch(trend_obj$loadings_prior_spec)
    )
    if (!is.null(branch_prior)) prior_info$prior <- branch_prior
  }

  # Delegate to brms::set_prior so the returned row always carries the
  # full canonical brmsprior schema (including columns like `tag` that
  # newer brms versions add). Avoids drift between mvgam-internal and
  # brms-generated prior rows.
  row <- brms::set_prior(
    prior = prior_info$prior %||% "",
    class = param_name,
    lb = if (nzchar(prior_info$lb %||% "")) prior_info$lb else NA,
    ub = if (nzchar(prior_info$ub %||% "")) prior_info$ub else NA
  )
  # brms::set_prior returns NA for unbounded; brms::get_prior returns
  # "". Normalise to "" so prior rows mvgam emits compare equal to
  # rows that came in via get_prior on the obs side.
  if (is.na(row$lb)) row$lb <- ""
  if (is.na(row$ub)) row$ub <- ""
  row$source <- "default"
  row
}

#' Get Default Prior Information for Trend Parameter
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

  # A trend-specific resolver may only tighten a bound, as CAR's does
  # for its damping coefficient. The shared default then supplies the
  # distribution. A call without a trend object has no resolver.
  custom_result <- NULL
  if (!is.null(trend_obj)) {
    resolver <- get0(
      paste0("get_", tolower(trend_obj$trend), "_parameter_prior"),
      envir = asNamespace("mvgam"), mode = "function", inherits = FALSE
    )
    if (!is.null(resolver)) {
      custom_result <- resolver(param_name, trend_obj)
      if (!is.null(custom_result) && nzchar(custom_result$prior)) {
        return(custom_result)
      }
    }
  }

  # The shared table covers the parameters every trend can have. The
  # Stan generator resolves its defaults through this same function.
  spec <- common_trend_priors[[param_name]]
  if (!is.null(spec)) {
    bounds <- spec$bounds
    return(list(
      prior = spec$default,
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

#' Is this class one of the loadings-kernel length-scales?
#'
#' `theta_features` and one `theta_dist_<name>` per supplied
#' distance matrix. They are named after the quantity rather than
#' the submodel, so they carry no `_trend` suffix, and the set is
#' open: a model declares one length-scale per distance source it
#' was given.
#'
#' @param param_name Character vector of class names.
#' @return Logical vector.
#' @noRd
is_loadings_length_scale <- function(param_name) {
  checkmate::assert_character(param_name, any.missing = FALSE)
  param_name == "theta_features" | grepl("^theta_dist_", param_name)
}


#' Get Default Prior Based on Parameter Type
#'
#' @param param_name Character string parameter name
#' @return List with prior, lb, ub elements
#' @noRd
get_parameter_type_default_prior <- function(param_name) {
  checkmate::assert_string(param_name)

  # Pattern matching for common parameter types
  if (identical(param_name, "nu_trend")) {
    # Innovation degrees of freedom. The lower bound of 2 is required
    # rather than conventional: below it the innovations have no finite
    # variance and the stationary initialisation of an autoregressive
    # trend is undefined.
    return(list(
      prior = common_trend_priors$nu_trend$default, lb = "2", ub = ""
    ))
  } else if (any(is_loadings_length_scale(param_name))) {
    # A kernel length-scale is a modelling choice, not a nuisance:
    # for a phylogenetic distance it sets how fast covariance decays
    # with relatedness. Reported here so it can be seen and set,
    # rather than emitted as a literal nothing reads back.
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
  } else if (grepl("sigma.*_trend$", param_name)) {
    # Variance parameters: positive with lower bound
    return(list(prior = "", lb = "0", ub = ""))
  } else if (grepl("theta.*_trend$", param_name)) {
    # Theta parameters (e.g., CAR): typically bounded [0, 1]
    return(list(prior = "", lb = "0", ub = "1"))
  } else if (grepl("alpha_cor.*_trend$", param_name)) {
    # Alpha correlation parameters (hierarchical mixing): bounded [0, 1]
    return(list(prior = "", lb = "0", ub = "1"))
  } else if (grepl("^[AD]omega_trend$", param_name)) {
    # VAR/VARMA hyperprior precisions (Aomega_trend, Domega_trend)
    return(list(prior = "", lb = "0", ub = ""))
  }
  # Every other parameter is unbounded with Stan's default prior:
  # correlation Cholesky factors, VAR coefficient matrices and their
  # hyperprior means, and the loadings `Z`.
  list(prior = "", lb = "", ub = "")
}

# =============================================================================
# SECTION 3: TREND-SPECIFIC PRIOR RESOLVERS
# =============================================================================
# `get_default_trend_parameter_prior()` looks up a function named
# `get_<trend>_parameter_prior()` for the trend being built. A resolver
# returns a prior, bounds or both for the parameters whose default
# depends on the trend, and NULL for every other parameter.

#' CAR bounds for its damping coefficient
#'
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_car_parameter_prior <- function(param_name, trend_obj) {
  # A continuous-time damping coefficient is raised to a real power,
  # and Stan declares it strictly inside the unit interval. The shared
  # default supplies the distribution.
  if (identical(param_name, "ar1_trend")) {
    return(list(prior = "", lb = "0.001", ub = "0.999"))
  }
  NULL
}

#' Get PW-Specific Parameter Prior
#'
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_pw_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)
  checkmate::assert_class(trend_obj, "mvgam_trend")

  # The changepoint scale is a `PW()` argument, so the prior on the
  # rate deviations is only knowable from the trend object. Naming it
  # here rather than at the emission site is what lets the reported
  # prior carry the user's own scale.
  if (identical(param_name, "delta_trend")) {
    scale <- trend_obj$changepoint_scale
    checkmate::assert_number(scale, lower = 0)
    return(list(
      prior = paste0("double_exponential(0, ", scale, ")"),
      lb = "", ub = ""
    ))
  }

  # Return NULL to use default parameter-type handling
  return(NULL)
}

# =============================================================================
# SECTION 4: PRIOR COMBINATION FUNCTIONS
# =============================================================================
# WHY: Combining observation and trend priors into a single brmsprior object
# enables use with brms functions while maintaining clear separation
# via the _trend suffix convention.

#' Combine Observation and Trend Priors
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
#' generator built them (e.g. the obs side picks up newer columns like
#' `tag` from brms; trend setup output and bare `prior()` rows don't).
#' Returns a list with every input padded to the union of column names
#' so they can be safely `rbind`ed without losing data and without
#' breaking when brms adds new columns in future releases.
#'
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
#' renamed to `prior`. The brms convention is the singular form;
#' historic mvgam / jsdgam docs use the plural. Without this
#' normalisation, callers using the plural lose their brms prior
#' table silently because internal codegen takes `prior = NULL`
#' and never inspects `...$priors`.
#'
#' Errors if both forms are supplied and disagree, to surface the
#' ambiguity rather than picking one silently.
#'
#' @param dots A list captured from `...`.
#' @return The same list with `prior` populated.
#' @noRd
normalise_prior_arg_alias <- function(dots) {
  if (is.null(dots$priors)) {
    return(dots)
  }
  if (!is.null(dots$prior) && !identical(dots$prior, dots$priors)) {
    stop(insight::format_error(c(
      "Both 'prior' and 'priors' supplied with different values.",
      i = "Pass priors once via 'prior =' (brms convention)."
    )))
  }
  dots$prior  <- dots$priors
  dots$priors <- NULL
  dots
}
#' Split a combined prior object down the middle
#'
#' brms is handed the observation-side priors and mvgam keeps the
#' rest, so the same partition is read from both ends. Taking each
#' end separately meant the two could stop being complements: a
#' class added to one side and not removed from the other would be
#' passed to brms, which has no parameter for it, and kept by
#' mvgam as well.
#'
#' The split is by class rather than by the `_trend` suffix. `Z`
#' carries no suffix and is still mvgam's.
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


#' Get all mvgam-generated trend parameters using trend system infrastructure
#'
#' @param trend_specs List of trend specifications from mv_spec
#' @return Character vector of all mvgam-generated parameter names
#' @noRd
get_all_mvgam_trend_parameters <- function(trend_specs) {
  spec <- trend_spec_head(trend_specs)
  if (is.null(spec)) {
    return(character(0))
  }
  ensure_registry_initialized()
  all_mvgam_params <- if (inherits(spec, "mvgam_trend")) {
    generate_monitor_params(spec)
  }

  # These two are mvgam's whenever they appear, which is a different
  # question from whether a given model samples them. This list only
  # decides what must never be handed to brms, so naming them
  # unconditionally is the safe direction: dropping one lets it reach
  # brms as `sigma`, an observation-side prior on the wrong scale.
  # What a model actually samples is `samples_innovation_scale()`.
  unique(c(all_mvgam_params, "sigma_trend", "nu_trend"))
}

# The columns that identify a prior row. Two rows agreeing on all of
# them set the same prior.
brmsprior_key_cols <- c("class", "coef", "group", "resp", "dpar",
                        "nlpar", "lb", "ub")


#' Merge user-supplied prior overrides onto a default prior table
#'
#' Match each row of `user_priors` against `default_priors` using the
#' full brms key (`class`, `coef`, `group`, `resp`, `dpar`, `nlpar`,
#' `lb`, `ub`). brms convention: an empty user-side key field (e.g.
#' `coef = ""`) is a wildcard that matches every row of that class.
#' Matched rows have their `prior` string replaced and `source` set
#' to `"user"`. A user row matching no default is appended.
#'
#' @param default_priors A `brmsprior` data frame; the table to merge
#'   onto. Must include a `source` column.
#' @param user_priors A `brmsprior` data frame of user overrides, or
#'   NULL.
#' @param source String written to the `source` column of every
#'   matched row. Defaults to `"user"`; the family-default overlay
#'   passes `"default"` so `get_prior()` still reports those rows as
#'   defaults rather than as something the user asked for.
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
      # Unmatched user row: keep it (brms convention is to preserve
      # user rows even when no default exists, e.g. mvgam-managed
      # `ar1_trend` / `sigma_trend` that the brms pipeline strips).
      unmatched_rows[[length(unmatched_rows) + 1L]] <- row
    }
  }
  if (length(unmatched_rows) > 0L) {
    # `prior()` returns a slimmer frame than `validate_prior()`, so
    # align every unmatched row to `default_priors`'s schema via the
    # shared helper before stacking.
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
#' `get_prior()` would advertise the brms fallback while the model
#' sampled under mvgam's, which for `com_binomial()` means showing a
#' positive-only `gamma(2, 0.1)` on a `nu` declared with `lb = -5`.
#' The defaults are the ones `response_default_priors()` gives the
#' fit, one set per response.
#'
#' @param obs_priors The `brmsprior` returned by `brms::get_prior()`.
#' @param formula The observation formula.
#' @param family The family given beside it, validated.
#' @return `obs_priors` with mvgam's defaults substituted in place.
#' @noRd
overlay_family_default_priors <- function(obs_priors, formula, family) {
  defaults <- response_default_priors(formula, family)
  if (is.null(defaults) || nrow(defaults) == 0L) {
    return(obs_priors)
  }
  merge_user_priors(obs_priors, defaults, source = "default")
}


#' Assemble the post-fit prior table stored on an `mvgam` object
#'
#' Wraps the canonical sequence:
#'   1. Re-suffix the brms-validated trend priors (`<class>` -> `<class>_trend`).
#'   2. Combine obs + trend rows into a single brmsprior frame.
#'   3. Append mvgam stanvar rows (Z_free_vec, varrho_inv, etc.).
#'   4. Layer the user's full `priors = ` argument over the result,
#'      marking matched rows as `source = "user"` so mvgam-managed
#'      trend overrides (sigma_trend, ar1_trend, etc.) that the brms
#'      pipeline strips are still recorded on the fit.
#'
#' @param obs_priors The obs-side `brmsprior` from `setup_brms_lightweight()`.
#' @param trend_priors The trend-side `brmsprior` from `setup_brms_lightweight()`,
#'   or NULL when the fit has no trend model.
#' @param user_prior The user's original `prior` / `priors` argument
#'   (already aliased via `normalise_prior_arg_alias()`), or NULL.
#' @param combined_stancode The assembled Stan code, used by
#'   `lift_mvgam_stanvar_priors()` to detect mvgam-injected stanvar
#'   rows.
#' @param trend_stancode The trend-side brms program, or NULL.
#' @return A `brmsprior` data frame ready to land on `mvgam_object$prior`.
#' @noRd
assemble_stored_prior_table <- function(obs_priors, trend_priors,
                                          user_prior, combined_stancode,
                                          trend_stancode = NULL) {
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
  # The code generator reads another table, which keeps the pin.
  hide_obs_placeholder_prior(lifted)
}


#' Hide the empty-obs placeholder's pin from a prior table a user reads
#'
#' The pin is structural: it holds a column mvgam had to invent at
#' zero, and names no parameter the user wrote. It stays in the table
#' the code generator reads, where dropping it would leave the
#' placeholder free.
#'
#' @param prior A `brmsprior` data frame
#' @return The table without the pin's row, or the `b` rows of the mean
#'   the pin sits in
#' @noRd
hide_obs_placeholder_prior <- function(prior) {
  if (!is.data.frame(prior) || !nrow(prior) || !"coef" %in% names(prior)) {
    return(prior)
  }
  # The placeholder is the only coefficient of the mean it sits in.
  # The class-level `b` row brms writes for that mean describes nothing
  # the user wrote either, and goes with the pin.
  mean_of <- paste(prior$resp, prior$dpar, prior$nlpar)
  held <- mean_of[prior$coef == MVGAM_EMPTY_OBS_PLACEHOLDER]
  prior[!(prior$class == "b" & mean_of %in% held), , drop = FALSE]
}


#' Does this prior string hold its parameter at a constant?
#'
#' `constant()` names no distribution. brms implements it by moving the
#' parameter out of the `parameters` block and assigning the value, so
#' a coefficient carrying one has no free parameter behind it. Two
#' consequences follow, and both are read off this predicate: mvgam's
#' own prior emitters cannot write a constant as a sampling statement
#' and refuse it, and the identification check leaves a pinned column
#' out of the design it ranks.
#'
#' @param x Character vector of prior strings.
#' @return Logical vector, `TRUE` where the string is a `constant()`.
#' @noRd
is_constant_prior <- function(x) {
  if (length(x) == 0L) return(logical(0))
  grepl("^\\s*constant\\s*\\(", as.character(x))
}




#' Re-attach the `_trend` suffix to a brms-validated trend prior table
#'
#' Inverse of `remove_trend_suffix_from_priors`: takes the prior table
#' returned by the trend-side `setup_brms_lightweight()` call (whose
#' classes carry brms's own names like `b`, `sigma`, `sds`, `ar1`) and
#' suffixes every class with `_trend` so the combined obs + trend table
#' uses the same convention as `get_prior.mvgam_formula()`. Rows already
#' suffixed (mvgam-injected stanvars) are left untouched.
#'
#' The trend submodel is handed to brms as a gaussian, so brms returns
#' a `sigma` row carrying its own residual-scale default. The trend's
#' process noise is `sigma_trend`, which the Stan generator samples
#' under `common_trend_priors` and which reaches the stored table from
#' the compiled code via `lift_mvgam_stanvar_priors()`. Suffixing
#' brms's row would file its value under mvgam's name, so it is
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


#' Suffix a trend-side prior table's classes with `_trend`
#'
#' The one implementation of that rule. It was written twice, and the
#' copies drifted: one dropped brms's residual-scale `sigma` row but
#' would suffix an already-suffixed class twice, the other guarded the
#' double suffix but turned an empty class into `"_trend"` and filed
#' brms's `sigma` value under mvgam's `sigma_trend`.
#'
#' @param priors A prior table carrying a `class` column.
#' @return The same table, minus brms's bookkeeping `sigma` row, with
#'   every remaining unsuffixed non-empty class suffixed.
#' @noRd
suffix_trend_prior_classes <- function(priors) {
  if (is.null(priors) || nrow(priors) == 0L) return(priors)
  coefs <- priors$coef %||% rep("", nrow(priors))
  bookkeeping_sigma <- priors$class %in% brms_trend_dropped_params &
    !nzchar(coefs)
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
#' leaves undeclared, and under a gaussian observation family it
#' resolves to the observation model's own `sigma`, a different
#' quantity. brms takes `autoscale = FALSE` for this case.
#'
#' @param priors A `brmsprior` table scoped to the trend formula.
#' @return The same table, with `autoscale = FALSE` on each special
#'   prior that left it unset.
#' @noRd
disable_trend_prior_autoscale <- function(priors) {
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
  cell <- intersect(setdiff(brmsprior_key_cols, c("coef", "lb", "ub")),
                    intersect(names(priors), names(defaults)))
  # `c()` on a `brmsprior` adds priors. The columns are taken from a
  # plain list.
  key <- function(tab, cols) {
    cols <- lapply(unclass(tab)[cols], function(x) {
      ifelse(is.na(x), "", as.character(x))
    })
    do.call(paste, c(cols, sep = "\r"))
  }
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
  bounded <- constructor & (has_bound(priors$lb) | has_bound(priors$ub))
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
# SECTION 5: UTILITY FUNCTIONS
# =============================================================================
# WHY: Utility functions provide common operations needed across the prior
# system.

#' Null-coalescing Operator
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# =============================================================================
# SECTION 6: MVGAM FORMULA INTERFACE
# =============================================================================
# WHY: Provides a consistent interface for model specification that extends
# brms functionality with state-space trend components. The mvgam_formula
# constructor and associated S3 methods enable clean integration with brms
# inspection functions without masking or conflicts.

#' Create an mvgam Formula Object
#'
#' @description
#' Constructs a lightweight mvgam formula object that combines observation and
#' trend model formulas. This provides a clean interface for model specification
#' that can be used with inspection functions like \code{get_prior()},
#' \code{stancode()}, and \code{standata()}.
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
#' \code{get_prior(mvgam_formula(y ~ x, ~ AR()), data = dat, family = poisson())}
#'
#' When \code{trend_formula = NULL}, the model reduces to a pure brms
#' observation model. When specified, the trend_formula can include:
#' \itemize{
#'   \item Trend constructors: \code{RW()}, \code{AR()}, \code{CAR()},
#'     \code{ZMVN()}, \code{VAR()}, \code{PW()}
#'   \item Covariates that affect trend dynamics
#'   \item Smooth terms using mgcv syntax: \code{s()}, \code{t2()}
#'   \item Random effects: \code{(1|group)}, \code{(slope|group)}
#'   \item Gaussian processes: \code{gp()}
#' }
#'
#' @section Trend Formula Restrictions:
#' The trend_formula has specific restrictions to maintain compatibility with
#' State-Space dynamics and prevent conflicts between observation-level and
#' trend-level modeling:
#'
#' \strong{Forbidden Terms:}
#' \itemize{
#'   \item brms addition-terms: \code{weights()}, \code{cens()}, \code{trunc()},
#'     \code{mi()}, \code{trials()}, \code{rate()}, \code{vreal()}, \code{vint()},
#'     \code{subset()}, \code{index()}
#'   \item brms autocorrelation: \code{ar()}, \code{ma()}, \code{arma()},
#'     \code{cosy()}, \code{unstr()}, \code{autocor()}
#'   \item Offset terms: \code{offset()}
#' }
#'
#' \strong{Rationale:}
#' Addition-terms modify observation model behavior (weights, censoring, exposure,
#' etc.) and should be specified in the main observation formula. Autocorrelation
#' terms conflict with mvgam's State-Space dynamics. All forbidden terms remain
#' fully supported in the main observation formula.
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
#' What multivariate models DO support:
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
#' If you need genuinely different dynamics per response, fit
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
#' @export
mvgam_formula <- function(formula, trend_formula = NULL) {

  # Validate formula parameter - must be formula, brmsformula, or mvbrmsformula
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

    # Use trend formula validation from validations.R
    validate_trend_formula(trend_formula)
  }

  # Determine and store formula type for later use
  if (inherits(formula, "mvbrmsformula")) {
    formula_class <- "mvbrmsformula"
  } else if (inherits(formula, "brmsformula")) {
    formula_class <- "brmsformula"
  } else {
    formula_class <- "formula"
  }

  # Create structure with formulas and class metadata
  out <- list(
    formula = formula,
    trend_formula = trend_formula
  )

  # Set S3 class hierarchy preserving original formula class
  if (formula_class == "mvbrmsformula") {
    class(out) <- c("mvgam_formula", "mvbrmsformula")
  } else if (formula_class == "brmsformula") {
    class(out) <- c("mvgam_formula", "brmsformula")
  } else {
    class(out) <- c("mvgam_formula", "formula")
  }
  attr(out, "formula_class") <- formula_class

  return(out)
}

#' Get Prior Specifications for Model Objects
#'
#' @description
#' S3 generic function for extracting prior specifications from various model
#' objects. This function provides a single interface for prior inspection
#' across different model types, with extensions for mvgam
#' State-Space models.
#'
#' @param object Model specification object (formula, brmsformula, mvgam_formula, etc.)
#' @param ... Additional arguments passed to methods
#'
#' @details
#' This generic function dispatches to appropriate methods based on object class:
#' \itemize{
#'   \item \code{formula}: Delegates to \code{brms::get_prior}
#'   \item \code{brmsformula}: Delegates to \code{brms::get_prior}
#'   \item \code{mvgam_formula}: Uses mvgam's extended prior system
#'   \item Default: Falls back to \code{brms::get_prior}
#' }
#'
#' @return A \code{brmsprior} data frame with prior specifications
#'
#' @seealso \code{\link{mvgam_formula}}, \code{\link[brms]{get_prior}}
#' @export
get_prior <- function(object, ...) {
  UseMethod("get_prior")
}

#' Build the `mvgam_formula` that a `trend_formula` names
#'
#' A `trend_formula` in the same call names an mvgam model. The brms
#' path takes the formula and discards the rest, giving a
#' complete-looking result that omits the trend entirely. Building the
#' `mvgam_formula` and dispatching on it covers the trend as well.
#'
#' Each caller handles its own no-trend case. `stancode()` and
#' `standata()` fall through with `NextMethod()`, which works only
#' when called from the method itself. `brms::make_stancode()` looks
#' like a shared exit and is not one: it calls the `stancode()`
#' generic, which dispatches back to the mvgam method and recurses
#' until the C stack fills. `brms::get_prior()` is a plain function
#' and returns directly.
#'
#' @param object A formula or brmsformula
#' @param dots The `...` of the calling method, as a list, carrying a
#'   non-NULL `trend_formula`
#' @param mvgam_fun The mvgam generic to dispatch on
#' @return Whatever `mvgam_fun` returns
#' @noRd
route_trend_formula <- function(object, dots, mvgam_fun) {
  trend_formula <- dots$trend_formula
  dots$trend_formula <- NULL

  do.call(
    mvgam_fun,
    c(list(mvgam_formula(object, trend_formula = trend_formula)), dots)
  )
}

#' Default method for get_prior - delegates to brms
#'
#' @param object Model specification object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.default <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(brms::get_prior(object, ...))
  }
  route_trend_formula(object, dots, get_prior)
}

#' Formula method for get_prior - explicit brms delegation
#'
#' @param object A formula object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.formula <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(brms::get_prior(object, ...))
  }
  route_trend_formula(object, dots, get_prior)
}

#' brmsformula method for get_prior - explicit brms delegation
#'
#' @param object A brmsformula object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.brmsformula <- function(object, ...) {
  dots <- list(...)
  if (is.null(dots$trend_formula)) {
    return(brms::get_prior(object, ...))
  }
  route_trend_formula(object, dots, get_prior)
}

#' Method for fitted mvgam objects - returns the stored prior table
#'
#' Convenience shortcut for the post-fit prior table. In brms the
#' canonical post-fit accessor is `prior_summary()`; mvgam keeps
#' that path and exports `get_prior.mvgam()` as well because users
#' frequently type `get_prior(fit)` by analogy with the formula
#' method. Both return the one table the model was built with. It can
#' be edited and fed back through
#' `update(fit, prior = ..., recompile = FALSE)` for refits that reuse
#' the compiled Stan model.
#'
#' @param object A fitted \code{mvgam} model.
#' @param ... Unused; an argument given here is refused.
#' @return A \code{brmsprior} data frame covering every adjustable
#'   prior row (observation and trend components). Rows the user
#'   overrode at fit time carry `source = "user"`; rows left at
#'   their defaults carry `source = "default"` (or whatever source
#'   string \pkg{brms} assigned).
#' @export
get_prior.mvgam <- function(object, ...) {
  rlang::check_dots_empty()
  prior_summary.mvgam(object)
}

#' Extract Prior Specifications for mvgam Formula Objects
#'
#' @description
#' Extracts and combines prior specifications for both observation and trend
#' components of an mvgam model. This method provides a single interface
#' for prior inspection before model fitting. When
#' \code{trend_formula = NULL}, this function behaves identically to
#' \code{brms::get_prior} for observation-only models.
#'
#' @param object An object of class \code{mvgam_formula} created by
#'   \code{\link{mvgam_formula}}
#' @param data A data frame containing the variables in the model (required)
#' @param family A description of the response distribution and link function.
#'   Default is \code{gaussian()}. Not required if formula contains embedded
#'   families via \code{bf()} specifications.
#' @param loadings_prior Optional structured prior on the factor
#'   loadings, in the form \code{\link{mvgam}} accepts. The prior
#'   reported for \code{Z} depends on it, so pass the same value here
#'   that the fit will use.
#' @param data2 Optional named list holding objects a
#'   \code{loadings_prior} refers to by name, matching the argument
#'   \code{\link{mvgam}} takes.
#' @param trend_map Not supported. Fixed and partial loadings change
#'   which parameters the model has, so the free-loadings table this
#'   returns cannot describe them; supplying it is an error that points
#'   at \code{\link[brms]{stancode}}, which does take it.
#' @param ... Additional arguments passed to \code{brms::get_prior}
#'
#' @return A \code{brmsprior} data frame combining observation and trend priors
#'   with an additional \code{trend_component} column distinguishing:
#'   \itemize{
#'     \item \code{"observation"}: Parameters from the observation model
#'     \item \code{"trend"}: Parameters from the trend model (with _trend suffix)
#'   }
#'   The returned object is fully compatible with \code{brms::set_prior()} and
#'   \code{brms::prior()} functions for customizing priors.
#'
#' @details
#' When a trend formula is specified, the function:
#' \enumerate{
#'   \item Extracts observation model priors using \code{brms::get_prior}
#'   \item Extracts trend model priors using mvgam's trend system
#'   \item Combines them into a single \code{brmsprior} object
#'   \item Adds the \code{trend_component} column for easy filtering
#' }
#'
#' The function handles embedded families automatically when using \code{bf()}
#' specifications and supports all brms family types for observation models
#' while trend components are always modeled as Gaussian State-Space processes.
#'
#' A shrinkage prior on a trend-side class is written without
#' autoscaling. \code{horseshoe()} and \code{R2D2()} scale their global
#' parameter by the family's residual standard deviation, and the
#' trend's own scale is \code{sigma_trend}, one per latent series, so
#' \code{autoscale = FALSE} is set for those rows. Writing
#' \code{autoscale = TRUE} on a trend-side class is refused.
#'
#' @examples
#' \dontrun{
#' # Workflow: discover the adjustable priors, override a subset,
#' # then pass the result back to `mvgam()`.
#' set.seed(1)
#' dat <- sim_mvgam(
#'   family       = poisson(),
#'   n_series     = 2L,
#'   n_timepoints = 120L,
#'   trend_model  = AR()
#' )$data_train
#'
#' # Wrap the formula pair in `mvgam_formula()` so this S3 method
#' # dispatches; the returned `brmsprior` data frame lists every
#' # adjustable row keyed by `class` and `coef`.
#' mf <- mvgam_formula(
#'   formula       = y ~ s(x, k = 5),
#'   trend_formula = ~ AR(p = 1)
#' )
#' get_prior(mf, data = dat, family = poisson())
#'
#' # Override a subset. Each `prior(...)` returns one row; untouched
#' # classes keep their defaults. Trend-side classes carry a
#' # `_trend` suffix so they do not collide with obs-side classes
#' # of the same name (e.g. `b` vs `b_trend`).
#' my_priors <- c(
#'   prior(normal(0, 1),    class = b),
#'   prior(exponential(2),  class = sigma_trend),
#'   prior(normal(0, 0.5),  class = ar1_trend)
#' )
#'
#' mod <- mvgam(
#'   formula       = y ~ s(x, k = 5),
#'   trend_formula = ~ AR(p = 1),
#'   data          = dat,
#'   family        = poisson(),
#'   priors        = my_priors,
#'   chains        = 2,
#'   iter          = 750,
#'   warmup        = 500,
#'   silent        = 2
#' )
#'
#' # Inspect the posterior to confirm the overrides took effect. The
#' # `ar1_trend` and `sigma_trend` rows should sit on the scale set
#' # by their custom priors rather than the flat defaults.
#' summary(mod)
#' }
#'
#' @seealso \code{\link{mvgam_formula}}, \code{\link[brms]{get_prior}},
#'   \code{\link[brms]{set_prior}}, \code{\link[brms]{prior}}
#' @export
get_prior.mvgam_formula <- function(object, data, family = gaussian(),
                                    loadings_prior = NULL,
                                    data2 = NULL,
                                    trend_map = NULL, ...) {
  checkmate::assert_class(object, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)

  # Extract formula components from mvgam_formula object
  formula <- object[[1]]  # object$formula triggers S3 dispatch issues
  trend_formula <- object[[2]]  # object$trend_formula

  # The family a univariate `bf()` names becomes the model's, and each
  # response's family is checked, as they are when the model is built.
  resolved <- resolve_observation_family(formula, family)
  formula <- resolved$formula
  family <- resolved$family

  # A formula declining every coefficient (`y ~ 0` for a pure-trend
  # state-space model) is built with a pinned placeholder, because
  # brms has no parameter class to set a default on without one. The
  # table is read off the same formula the model is built from, and
  # the pin is hidden here as it is on a fit's stored table.
  injected <- inject_obs_zero_placeholder(formula, data, prior = NULL)
  # brms applies `family` to every response that names none, as the
  # fit does.
  obs_priors <- brms::get_prior(formula = injected$formula,
                                data = injected$data,
                                family = family, ...)
  obs_priors <- overlay_family_default_priors(obs_priors, formula, family)
  obs_priors <- hide_obs_placeholder_prior(obs_priors)

  # Handle case where no trend formula is specified
  if (is.null(trend_formula)) {
    # Return standard brms prior object
    return(obs_priors)
  }

  # Each response's column, named by its key, as the fitting path
  # builds them
  response_vars <- response_columns(formula)

  # Parse multivariate trends and validate
  mv_spec <- parse_multivariate_trends(formula, trend_formula)

  mv_spec$trend_specs <- prepare_trend_specs(
    mv_spec$trend_specs, trend_map, loadings_prior, data, data2
  )

  # The spec and trend data Stan generation builds from. The base
  # formula has its `by = lv_axis()` terms rewritten to the columns
  # the trend data holds.
  components <- extract_and_validate_trend_components(
    data, mv_spec, response_vars
  )
  trend_priors <- generate_trend_priors(
    trend_model = trend_spec_head(components$enhanced_mv_spec$trend_specs),
    base_formula = components$enhanced_mv_spec$base_formula,
    data = components$trend_data,
    codegen = codegen_from_dots(list(...))
  )

  combine_obs_trend_priors(obs_priors, trend_priors)
}

