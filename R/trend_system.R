# Trend system: the registry of trend types and their properties,
# validation of trend specifications, trend formula parsing and the
# trend constructors (RW, AR, VAR, CAR, ZMVN, PW).

# =============================================================================
# SECTION 1: TREND REGISTRY SYSTEM
# =============================================================================
# WHY: The registry system is essential for mvgam-brms integration because
# it enables dynamic dispatch to appropriate Stan code generators while
# maintaining type safety and factor model compatibility validation.
# Without centralized registration, the system cannot determine which
# trends support factor models or locate the correct Stan code generators.

# Global trend registry
trend_registry <- new.env(parent = emptyenv())
#' Record a trend type in the registry
#'
#' The package registers each of its trends through this at load, from
#' the trend's `<name>_trend_properties()`.
#'
#' @param name Character string name of the trend type
#' @param supports_factors Logical indicating if trend supports factor
#'   models (n_lv parameter)
#' @param covariance_pattern How the innovations are parameterised:
#'   `"none"` for a deterministic trend with no innovation,
#'   `"diagonal"` for independent innovations scaled by `sigma_trend`,
#'   `"cholesky_scaled"` for `diag(sigma_trend) * L_Omega_trend` and
#'   `"full_covariance"` for a sampled `Sigma_trend`.
#' @param stationary_source How the covariance a marginal prediction
#'   integrates over is obtained: `"lift"` raises the innovation
#'   covariance to the stationary one, `"omega"` takes the stationary
#'   covariance the Stan model already derives, `"none"` keeps the
#'   innovation covariance.
#' @param requires_regular_intervals Logical; does the trend index its
#'   lags by position, which an uneven time grid breaks?
#' @param generator_func Function that generates Stan code for this
#'   trend type
#' @param incompatibility_reason Character string explaining why factor
#'   models are unavailable, required when `supports_factors` is FALSE
#' @return Invisibly returns TRUE on successful registration
#' @noRd
register_trend_type <- function(name, supports_factors, covariance_pattern,
                                stationary_source,
                                requires_regular_intervals,
                                generator_func,
                                incompatibility_reason = NULL) {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_flag(supports_factors)
  checkmate::assert_choice(
    covariance_pattern,
    c("none", "diagonal", "cholesky_scaled", "full_covariance")
  )
  checkmate::assert_choice(stationary_source, c("none", "lift", "omega"))
  checkmate::assert_flag(requires_regular_intervals)
  checkmate::assert_function(generator_func,
                             args = c("trend_specs", "data_info"))
  checkmate::assert_string(incompatibility_reason,
                           null.ok = supports_factors)

  trend_registry[[name]] <- list(
    supports_factors = supports_factors,
    covariance_pattern = covariance_pattern,
    stationary_source = stationary_source,
    requires_regular_intervals = requires_regular_intervals,
    generator = generator_func,
    incompatibility_reason = incompatibility_reason
  )

  invisible(TRUE)
}

#' Get Trend Type Information
#'
#' @description
#' Retrieve information about a registered trend type.
#'
#' @param name Character string name of the trend type
#' @return List containing trend type information
#' @noRd
get_trend_info <- function(name) {
  checkmate::assert_string(name, min.chars = 1)

  if (!exists(name, envir = trend_registry)) {
    available_trends <- ls(trend_registry)
    stop(insight::format_error(c(
      cli::format_inline("Unknown trend type: {.field {name}}"),
      i = cli::format_inline("Available types: {.val {available_trends}}")
    )))
  }

  trend_registry[[name]]
}

#' List Available Trend Types
#'
#' @description
#' List all registered trend types and their factor model support.
#'
#' @return Data frame with trend types and their properties
#' @export
list_trend_types <- function() {
  trend_names <- ls(trend_registry)

  if (length(trend_names) == 0) {
    return(data.frame(
      trend_type = character(0),
      supports_factors = logical(0),
      incompatibility_reason = character(0)
    ))
  }

  trend_info <- lapply(trend_names, function(name) {
    info <- trend_registry[[name]]
    data.frame(
      trend_type = name,
      supports_factors = info$supports_factors,
      incompatibility_reason = info$incompatibility_reason %||% "",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, trend_info)
}

#' Register the package's trend types
#'
#' @description
#' Registers every trend type the package defines, found by naming
#' convention. For trend type "FOO" the package defines
#' `generate_foo_trend_stanvars()`, which emits its Stan code, and
#' `foo_trend_properties()`, which returns the arguments of
#' `register_trend_type()` other than the name and the generator.
#' Called on first use of the registry.
#'
#' @return Invisibly returns TRUE
#' @noRd
register_core_trends <- function() {
  ns <- getNamespace("mvgam")
  generator_pattern <- "^generate_(.+)_trend_stanvars$"
  generator_functions <- grep(generator_pattern, ls(ns), value = TRUE)
  if (length(generator_functions) == 0) {
    stop_mvgam_fault(
      "The mvgam namespace lacks trend generators.",
      "mvgam searched its namespace for 'generate_*_trend_stanvars'."
    )
  }

  for (generator_name in generator_functions) {
    trend_type <- toupper(gsub(generator_pattern, "\\1", generator_name))
    properties_name <- paste0(tolower(trend_type), "_trend_properties")
    properties <- get0(properties_name, envir = ns, mode = "function",
                       inherits = FALSE)
    if (is.null(properties)) {
      stop_mvgam_fault(
        paste0("The '", trend_type, "' trend lacks a properties function."),
        paste0("mvgam looked for '", properties_name, "()'.")
      )
    }
    trend_info <- properties()
    validate_trend_properties(trend_info, properties_name)
    do.call(register_trend_type, c(
      list(name = trend_type,
           generator_func = get(generator_name, envir = ns)),
      trend_info
    ))
  }

  invisible(TRUE)
}

#' Validate Trend Properties Function Output
#'
#' Every property is declared by each trend and none has a default.
#' `register_trend_type()` checks the values.
#'
#' @param trend_info Output from trend properties function
#' @param func_name Properties function name for context
#' @return Invisibly returns TRUE
#' @noRd
validate_trend_properties <- function(trend_info, func_name) {
  checkmate::assert_string(func_name, min.chars = 1)
  what <- paste0("The value of '", func_name, "()'")
  if (!is.list(trend_info)) {
    stop_wrong_class(what, trend_info, "a list")
  }
  missing_fields <- setdiff(
    c("supports_factors", "covariance_pattern", "stationary_source",
      "requires_regular_intervals", "incompatibility_reason"),
    names(trend_info)
  )
  if (length(missing_fields) > 0) {
    stop_missing_fields(what, missing_fields)
  }
  invisible(TRUE)
}

# Core Trend Properties Functions
# These define the capabilities of each built-in trend type. A trend
# that indexes its lags by position requires regular intervals, since
# an uneven grid breaks that indexing.

#' AR Trend Properties
#' @noRd
ar_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    covariance_pattern = "cholesky_scaled",
    # Every lag set settles at the covariance
    # `ar_stationary_factor()` computes from the moving-average weights.
    stationary_source = "lift",
    requires_regular_intervals = TRUE,
    incompatibility_reason = NULL
  )
}

#' RW Trend Properties
#' @noRd
rw_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    covariance_pattern = "cholesky_scaled",
    # A random walk has no stationary distribution.
    stationary_source = "none",
    requires_regular_intervals = TRUE,
    incompatibility_reason = NULL
  )
}

#' VAR Trend Properties
#' @noRd
var_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    covariance_pattern = "full_covariance",
    # The Stan model already derives `Omega_trend` from `Phi_trend` and
    # `Sigma_trend`. Nothing is recomputed here.
    stationary_source = "omega",
    requires_regular_intervals = TRUE,
    incompatibility_reason = NULL
  )
}

#' ZMVN Trend Properties
#' @noRd
zmvn_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    covariance_pattern = "cholesky_scaled",
    # Nothing propagates the state between times, which makes the
    # innovation covariance the state's own covariance.
    stationary_source = "none",
    # A multivariate normal indexed by series is exchangeable in time.
    requires_regular_intervals = FALSE,
    incompatibility_reason = NULL
  )
}

#' CAR Trend Properties
#' @noRd
car_trend_properties <- function() {
  list(
    supports_factors = FALSE,
    covariance_pattern = "cholesky_scaled",
    # Each gap scales the decay and the innovation together. The
    # state then has covariance `Sigma[a, b] / (1 - ar_a * ar_b)` at
    # every occasion of an irregular grid, the factor a lag-1
    # autoregression takes.
    stationary_source = "lift",
    # The kernel carries the elapsed gap between observations.
    requires_regular_intervals = FALSE,
    incompatibility_reason = paste0(
      "Continuous-time AR dynamics follow each series' own irregular ",
      "time gaps."
    )
  )
}

#' PW Trend Properties
#' @noRd
pw_trend_properties <- function() {
  list(
    supports_factors = FALSE,
    # The piecewise path is a deterministic function of its
    # changepoints. It has no innovation to carry a scale.
    covariance_pattern = "none",
    stationary_source = "none",
    requires_regular_intervals = TRUE,
    incompatibility_reason =
      "Piecewise trends model changepoints separately for each series."
  )
}

# Internal: the trend constructors for a message, taken from the
# registry.
#'@noRd
registered_trend_constructors <- function(factors_only = FALSE) {
  checkmate::assert_flag(factors_only)
  ensure_registry_initialized()
  trends <- list_trend_types()
  if (factors_only) {
    trends <- trends[trends$supports_factors, , drop = FALSE]
  }
  paste(paste0(sort(trends$trend_type), "()"), collapse = ", ")
}

#' Check if Registry is Initialized
#'
#' @return Logical indicating if the trends have been registered
#' @noRd
is_registry_initialized <- function() {
  length(ls(trend_registry)) > 0L
}

#' Initialize Registry if Needed
#'
#' @description
#' Registers the trends on first use. Called by every function that
#' queries the registry.
#'
#' @return Invisibly returns TRUE
#' @noRd
ensure_registry_initialized <- function() {
  if (!is_registry_initialized()) {
    register_core_trends()
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Monitor Parameter Generation for Trend Objects
# -----------------------------------------------------------------------------

#' Does this trend sample an innovation standard deviation?
#'
#' Two things remove it. A deterministic trend is registered with
#' covariance pattern `"none"`: `PW()` draws its path from
#' changepoints, and the emitted program declares no `sigma_trend`.
#' Multiplicative gamma process shrinkage derives the scale as
#' `sqrt(Psi_diag)`. A sampled `sigma_trend` would give each column a
#' second magnitude, and the likelihood identifies only the product
#' of the two.
#'
#' The table that reports priors and the generator that emits them
#' both call this function. Every prior the table offers is then one
#' the model declares.
#'
#' @param trend_spec An `mvgam_trend` object.
#' @return `TRUE` when the model samples `sigma_trend`.
#' @noRd
samples_innovation_scale <- function(trend_spec) {
  checkmate::assert_list(trend_spec, min.len = 1)
  ensure_registry_initialized()
  info <- get_trend_info(get_trend_name(trend_spec))
  !identical(info$covariance_pattern, "none") &&
    !loadings_spec_traits(trend_spec$loadings_prior_spec)$mgp &&
    !samples_factor_loadings(trend_spec)
}


#' Does this trend sample its factor loadings?
#'
#' A factor model samples every entry of `Z` unless a `trend_map`
#' fixes some of them. The likelihood sees the factors only through
#' `Z %*% lv`, where any scale or rotation of the factors passes into
#' the loadings. Identification needs the factor innovations at unit
#' scale and zero correlation, and `Z` then sets the covariance among
#' series. Fixed loadings pin the factors' scale, and those models keep
#' a sampled scale and correlation. An all-`NA` mask fixes nothing:
#' `normalise_trend_map_on_specs()` leaves `fixed_Z` unset for it.
#'
#' @param trend_spec A normalised trend specification, or a
#'   per-response list of them.
#' @return `TRUE` when every loading is sampled.
#' @noRd
samples_factor_loadings <- function(trend_spec) {
  spec <- trend_spec_head(trend_spec)
  !is.null(spec$n_lv) && is.null(spec$fixed_Z)
}


#' Does this trend sample any factor loading?
#'
#' A partial `trend_map` samples the entries it leaves `NA` under the
#' `Z` prior, and a fully fixed one samples none. The prior table
#' lists a `Z` row exactly when this holds, matching the program
#' `generate_matrix_z_multiblock_stanvars()` emits.
#'
#' @param trend_spec A normalised trend specification, or a
#'   per-response list of them.
#' @return `TRUE` when at least one entry of `Z` is sampled.
#' @noRd
samples_any_loading <- function(trend_spec) {
  spec <- trend_spec_head(trend_spec)
  !is.null(spec$n_lv) && (is.null(spec$fixed_Z) || anyNA(spec$fixed_Z))
}


#' How a trend's stationary covariance is obtained
#'
#' One registry fact replacing a string comparison against the trend
#' type. A marginal prediction integrates over the covariance the
#' latent state settles at, and each kernel supplies that differently:
#' `"lift"` raises the innovation covariance, `"omega"` takes what the
#' Stan model already derived, `"none"` keeps the innovation
#' covariance. Each trend declares its own in
#' `<name>_trend_properties()`. A fit with no trend, which
#' `get_trend_type()` reports as `"None"`, has no covariance to lift.
#'
#' @param trend_type A registered trend type name, or `"None"`
#' @return One of `"none"`, `"lift"` or `"omega"`
#' @noRd
trend_stationary_source <- function(trend_type) {
  trend_property(trend_type, "stationary_source")
}


#' One registered property of a trend type
#'
#' A fit with no trend, which `get_trend_type()` reports as `"None"`,
#' has neither innovations nor a stationary covariance, and every
#' property asked of it here is `"none"`.
#'
#' @param trend_type A registered trend type name, or `"None"`
#' @param field `"stationary_source"` or `"covariance_pattern"`
#' @return The registered value
#' @noRd
trend_property <- function(trend_type,
                           field = c("stationary_source",
                                     "covariance_pattern")) {
  checkmate::assert_string(trend_type, min.chars = 1)
  field <- match.arg(field)
  if (identical(trend_type, "None")) {
    return("none")
  }
  ensure_registry_initialized()
  get_trend_info(trend_type)[[field]]
}


#' The parameters a trend samples
#'
#' A `trend_map` and a loadings prior both join the specification
#' after the constructor builds it, and each changes which parameters
#' the program samples. The function computes the list from the
#' specification it receives. The prior table and the set of classes
#' mvgam withholds from brms both call it.
#'
#' @param trend_spec An `mvgam_trend` object
#' @return Character vector of parameter names
#' @noRd
generate_monitor_params <- function(trend_spec) {
  checkmate::assert_list(trend_spec, min.len = 1)
  ensure_registry_initialized()
  trend_type <- get_trend_name(trend_spec)

  # A grouped trend scales its innovations through `L_group_trend`,
  # declaring `sigma_group_trend` with a group-scoped correlation
  # block. The ungrouped spellings stay out of its prior surface,
  # which keeps every offered class one the program declares.
  is_grouped <- named_var(trend_spec$gr)

  # The table offers `sigma_trend` only where the program declares
  # it. A user who copies an offered class into a prior must never
  # be refused for it.
  base_params <- if (!is_grouped && samples_innovation_scale(trend_spec)) {
    "sigma_trend"
  } else {
    character(0)
  }

  trend_specific <- switch(trend_type,
    "RW" = generate_rw_monitor_params(trend_spec),
    "AR" = generate_ar_monitor_params(trend_spec),
    "VAR" = generate_var_monitor_params(trend_spec),
    "CAR" = generate_car_monitor_params(trend_spec),
    "ZMVN" = character(0),
    "PW" = generate_pw_monitor_params(trend_spec)
  )

  # Add correlation parameters if enabled
  # Note: Sigma_trend is computed from sigma_trend * L_Omega_trend in Stan,
  # so users should not place priors on it directly
  correlation_params <- if (!is_grouped && (trend_spec$cor %||% FALSE) &&
                            !samples_factor_loadings(trend_spec)) {
    "L_Omega_trend"
  } else {
    character(0)
  }

  # A fully fixed `trend_map` samples no loading. A loadings prior
  # cannot join a fixed `Z`, per `assert_loadings_prior_compatible()`.
  factor_params <- if (!is.null(trend_spec$n_lv)) {
    c(
      if (samples_any_loading(trend_spec)) "Z",
      loadings_monitor_params(trend_spec$loadings_prior_spec)
    )
  }

  # Estimated innovation degrees of freedom. Listing the class here
  # lets `get_prior()` report it with `sigma_trend` and the
  # autoregressive coefficients. A user sets its prior from that table
  # without knowing the class name beforehand. Absent when the
  # innovations are Gaussian or the degrees of freedom are fixed.
  df_params <- if (is.na(trend_spec$df %||% Inf)) {
    "nu_trend"
  } else {
    character(0)
  }

  # Add hierarchical correlation parameters if grouping is specified
  hierarchical_params <- if (is_grouped) {
    c("alpha_cor_trend", "L_Omega_global_trend",
      "L_deviation_group_trend", "sigma_group_trend")
  } else {
    character(0)
  }

  # Combine all parameters
  all_params <- unique(c(
    base_params, trend_specific, correlation_params, factor_params,
    df_params, hierarchical_params
  ))

  return(all_params)
}

#' Parameters an `ma` term adds, by trend type
#'
#' Named once so the trends cannot drift apart on it. `RW()` and
#' `AR()` sample the moving-average coefficient itself; a VARMA
#' samples the hyperparameters its stationary parameterisation is
#' built from. A trend given the term without its parameters listed
#' samples under a prior nothing reports and nothing can override.
#'
#' @noRd
ma_monitor_params <- list(
  RW = "theta1_trend",
  AR = "theta1_trend",
  VAR = c("Dmu_trend", "Domega_trend")
)


#' Monitor parameters an `ma` term contributes to one trend
#'
#' @param trend_spec Trend specification
#' @param trend_type Base trend type, one of `names(ma_monitor_params)`
#' @return Character vector, empty when the term was not asked for
#'
#' @noRd
ma_params_for <- function(trend_spec, trend_type) {
  if (!isTRUE(trend_spec$ma %||% FALSE)) {
    return(character(0))
  }
  ma_monitor_params[[trend_type]] %||% character(0)
}


#' Generate RW-specific monitor parameters
#'
#' @param trend_spec RW trend specification
#' @return Character vector of RW-specific parameters
#'
#' @noRd
generate_rw_monitor_params <- function(trend_spec) {
  ma_params_for(trend_spec, "RW")
}

#' Generate AR-specific monitor parameters
#' @param trend_spec AR trend specification
#' @return Character vector of AR-specific parameters
#' @noRd
generate_ar_monitor_params <- function(trend_spec) {
  # One resolution of the lag set, shared with the Stan generator.
  lag_vec <- resolve_active_lags(trend_spec$p %||% 1)
  # Every AR parameter the sharing mode gives this spec, including
  # the per-series coefficient that summary labels and the brms
  # interception set both need. The prior generator narrows the list
  # to the rows a user can edit.
  sharing <- ar_coef_sharing(trend_spec)
  ar_params <- ar_monitor_coef_names(lag_vec, sharing)

  c(ar_params, ma_params_for(trend_spec, "AR"))
}

#' Generate VAR-specific monitor parameters
#' @param trend_spec VAR trend specification
#' @return Character vector of VAR-specific parameters
#' @noRd
generate_var_monitor_params <- function(trend_spec) {
  # VAR uses hierarchical hyperparameters following Heaps 2023 methodology

  # Base VAR hyperparameters - always present
  var_params <- c("Amu_trend", "Aomega_trend")

  var_params <- c(var_params, ma_params_for(trend_spec, "VAR"))


  # VAR uses variance-correlation decomposition (sigma_trend + L_Omega_trend)
  # These are handled by base_params and correlation_params in the main function

  return(var_params)
}

#' Generate CAR-specific monitor parameters
#' @param trend_spec CAR trend specification
#' @return Character vector of CAR-specific parameters
#' @noRd
generate_car_monitor_params <- function(trend_spec) {
  # CAR uses ar1_trend following standard _trend suffix convention
  return(c("ar1_trend"))
}


#' Generate PW-specific monitor parameters
#' @param trend_spec PW trend specification
#' @return Character vector of PW-specific parameters
#' @noRd
generate_pw_monitor_params <- function(trend_spec) {
  # Piecewise trend parameters. The logistic form adds the `m_trend`
  # offset that positions its curve along time.
  c(
    "k_trend",
    if (pw_is_logistic(trend_spec)) "m_trend",
    "delta_trend"
  )
}

# Internal: the constructor name a trend spec records, the key every
# registry lookup takes. `NA` for no spec.
#'@noRd
get_trend_name <- function(trend_spec) {
  trend_spec_head(trend_spec)$trend %||% NA_character_
}


# Internal: the growth form of a PW model, from either carrier of
# the fact. A `PW()` spec names the field `growth`, validated with
# `match.arg()`. A fitted object's `trend_metadata` persists the same
# value as `pw_growth`, which post-fit callers have in hand where the
# spec is out of reach. Prophet's default form is linear.
#'@noRd
pw_growth <- function(x) {
  if (is.null(x)) return("linear")
  x$growth %||% x$pw_growth %||% "linear"
}


# Internal: whether a PW model uses the logistic form. The logistic
# form needs a carrying capacity, and its `m_trend` offset positions
# the curve along time. For the linear form, the observation formula
# supplies the level.
#'@noRd
pw_is_logistic <- function(x) {
  identical(pw_growth(x), "logistic")
}


# Internal: how an AR trend shares its coefficients across series.
# `AR()` records the mode on every spec it builds. The other trends
# have no coefficients to share, and their specs carry no mode.
#'@noRd
ar_coef_sharing <- function(spec) {
  spec$coef_sharing %||% "none"
}


# Internal: the column that holds a logistic PW model's carrying
# capacity. `create_mvgam_trend()` records it on every spec, as the
# name the user gave or `"cap"`. The Stan data builder and the
# forecast both take the name from here.
#'@noRd
pw_cap_var <- function(spec) {
  cap_var <- spec_field(trend_spec_head(spec), "cap")
  if (!named_var(cap_var)) {
    stop(insight::format_error(c(
      "The PW specification lacks a carrying-capacity column.",
      i = "Build the trend with 'PW(cap = ...)' to record the column."
    )), call. = FALSE)
  }
  cap_var
}

# =============================================================================
# SECTION 3: MVGAM_TREND OBJECT SPECIFICATION
# =============================================================================

#' Fields of an mvgam_trend object
#'
#' @description
#' Every trend constructor, [AR()], [RW()], [VAR()], [CAR()], [PW()] and
#' [ZMVN()], returns an object of class `mvgam_trend`. The object
#' records the arguments of the call. Model building adds the fields
#' that need the data.
#'
#' @section Fields every trend has:
#' \describe{
#'   \item{trend}{The trend type, such as `"AR"`. Stan code generation
#'     and the registry look the type up by this name.}
#'   \item{time, series}{The names of the time and series columns,
#'     `"time"` and `"series"` unless given.}
#'   \item{gr, subgr}{The names of the grouping and subgrouping
#'     columns of a hierarchical trend, or `"NA"`.}
#'   \item{cap}{The name of the carrying-capacity column that a
#'     logistic [PW()] trend uses, `"cap"` unless given.}
#'   \item{ma}{`TRUE` for a moving-average term.}
#'   \item{cor}{`TRUE` for correlated innovations. A grouped trend
#'     always has them.}
#'   \item{validation_rules}{What the trend assumes about the data.
#'     `"requires_regular_intervals"` marks a trend that indexes its
#'     lags by position, and fitting then requires evenly spaced
#'     times. [CAR()] and [ZMVN()] carry no rule.}
#' }
#'
#' @section Fields a constructor adds:
#' \describe{
#'   \item{n_lv}{The number of latent factors, or `NULL` for none.}
#'   \item{trend_map}{The loadings as supplied to the constructor.}
#'   \item{p}{The autoregressive lags of [AR()] and [VAR()].}
#'   \item{coef_sharing}{How [AR()] shares its coefficients across
#'     series.}
#'   \item{df}{The degrees of freedom of the innovations: `Inf` for
#'     Gaussian, `NA` to estimate them.}
#'   \item{growth, n_changepoints, changepoint_range,
#'     changepoint_scale}{The arguments of [PW()].}
#' }
#'
#' @section Fields model building adds:
#' \describe{
#'   \item{fixed_Z}{The loadings matrix a `trend_map` gives, with `NA`
#'     marking a sampled entry. `NULL` when every loading is sampled.}
#'   \item{loadings_prior_spec}{The structured loadings prior built
#'     from the `loadings_prior` argument of [mvgam()].}
#'   \item{dimensions}{The series and time axes the data defines.}
#' }
#'
#' @section Parameters and priors:
#' Which parameters a trend samples depends on its fields at the time
#' the model is built. A fully fixed `trend_map` samples no loadings,
#' and a loadings prior with column shrinkage derives the innovation
#' scale. [get_prior()] lists the parameters for a given model.
#'
#' @seealso [list_trend_types()]
#' @name mvgam_trend_specification
#' @author Nicholas J Clark
NULL

# The one declaration a trend carries, from its registered
# `requires_regular_intervals`. `trend_requires_regular_intervals()`
# scans the specifications for it.
rule_requires_regular_intervals <- "requires_regular_intervals"


# =============================================================================
# SECTION 4: TREND VALIDATION AND PARSING
# =============================================================================
# WHY: Trend validation ensures data integrity and prevents runtime errors
# during Stan model compilation. Formula parsing enables complex multivariate
# trend specifications while maintaining compatibility with brms syntax.
# This layer bridges user-friendly R formulas to internal trend objects.

#' Trend type registry for extensible dispatch
#'
#' Central registry of all available trend types for formula parsing and
#'   validation. New trend types are automatically included when registered.
#'
#' @return Character vector of trend type names
#' @noRd
mvgam_trend_registry <- function() {
  # All trend types from the single registry environment
  ls(trend_registry)
}

#' Split a trend formula into its trend and its regular terms
#'
#' A term is a trend term when its outermost call is a registered
#' trend constructor. A formula with none takes the default `ZMVN()`.
#' `validate_trend_formula()` refuses more than one constructor before
#' `parse_multivariate_trends()` calls this.
#'
#' @param trend_formula A one-sided formula
#' @return A list of `trend_model`, the evaluated constructor,
#'   `base_formula`, the formula without it, and `regular_terms`, the
#'   labels of the other terms
#' @noRd
parse_trend_formula <- function(trend_formula) {
  checkmate::assert_class(trend_formula, "formula")
  ensure_registry_initialized()

  # `terms()` refuses what it cannot expand, such as a `.` with no
  # data, and its message names the problem.
  tf_terms <- stats::terms(trend_formula, keep.order = TRUE)
  refuse_trend_formula_response(trend_formula)
  refuse_trend_formula_offset(tf_terms)

  labels <- attr(tf_terms, "term.labels")
  is_trend <- vapply(labels, is_trend_constructor_call, logical(1L),
                     USE.NAMES = FALSE)
  trend_terms <- labels[is_trend]
  trend_model <- if (length(trend_terms) > 0L) {
    eval_trend_constructor(
      trend_terms[1L],
      formula_env = environment(trend_formula) %||% globalenv()
    )
  } else {
    ZMVN()
  }

  list(
    trend_model = trend_model,
    base_formula = parse_base_formula_safe(trend_formula, trend_terms),
    regular_terms = labels[!is_trend]
  )
}


# Internal: whether a term label is a call to a registered trend
# constructor. A covariate whose name ends in a constructor's name,
# such as `lagAR(x)`, is a regular term.
#'@noRd
is_trend_constructor_call <- function(label) {
  checkmate::assert_string(label)
  expr <- str2lang(label)
  is.call(expr) && is.name(expr[[1L]]) &&
    as.character(expr[[1L]]) %in% mvgam_trend_registry()
}

#' Evaluate a trend constructor call
#'
#' The constructor comes from the mvgam namespace, which a caller of
#' `mvgam::mvgam()` need not attach. The arguments evaluate in the
#' formula's environment, where an inline `trend_map` matrix is found.
#'
#' @param trend_call Character string containing a call to a registered
#'   trend constructor, as `is_trend_constructor_call()` accepts
#' @param formula_env The environment of the originating formula
#' @return A validated mvgam_trend object
#' @noRd
eval_trend_constructor <- function(trend_call, formula_env) {
  checkmate::assert_string(trend_call)
  checkmate::assert_environment(formula_env)
  expr <- str2lang(trend_call)
  name <- as.character(expr[[1L]])
  checkmate::assert_choice(name, mvgam_trend_registry())
  expr[[1L]] <- get(name, envir = asNamespace("mvgam"), mode = "function",
                    inherits = FALSE)
  eval(expr, envir = formula_env)
}

#' Print method for mvgam trend objects
#'
#' Provides informative printing of trend specifications.
#'
#' @param x A mvgam_trend object
#' @param ... Additional arguments (not currently used)
#'
#' @return The `mvgam_trend` object `x`, returned invisibly.
#'
#' @export
print.mvgam_trend <- function(x, ...) {
  cat("mvgam trend specification:\n")
  cat("  Type:", x$trend, "\n")
  cat("  Label:", x$label, "\n")
  cat("  Parameters:", paste(x$tpars, collapse = ", "), "\n")

  if (!is.null(x$n_lv) && x$n_lv > 0) {
    cat("  Dynamic factors:", x$n_lv, "\n")
  }

  if (!is.null(x$cor) && x$cor) {
    cat("  Correlation: enabled\n")
  }

  if (!is.null(x$ma) && x$ma) {
    cat("  Moving average: enabled\n")
  }

  if (named_var(x$gr)) {
    cat("  Hierarchical grouping:", x$gr, "\n")
  }

  invisible(x)
}

# =============================================================================
# SECTION 3: TREND CONSTRUCTOR FUNCTIONS
# =============================================================================
# The trend constructors are the user-facing API for trend
# specifications. Each validates its arguments, sets defaults and
# returns the trend object that Stan generation takes.

#' Trend Model Constructors for \pkg{mvgam}
#'
#' Specify trend models for multivariate State-Space models in \pkg{mvgam}.
#' These constructor functions create trend specifications for various temporal
#' dynamics including random walks (RW), autoregressive models (AR, VAR, CAR),
#' and piecewise trends (PW). These functions do not evaluate their arguments
#' – they exist purely to help set up models with particular trend structures.
#'
#' @param df Degrees of freedom for the latent process innovations.
#'   Defaults to \code{Inf}, which gives Gaussian innovations: a t with
#'   infinite degrees of freedom is a normal. Set \code{df = NA} to
#'   estimate them, or supply a number above \code{2} to fix them.
#'
#'   Finite degrees of freedom let the latent process absorb an
#'   occasional large shock while \code{sigma_trend} stays at the scale
#'   of ordinary variation. That suits boom-and-bust population series
#'   and outbreak dynamics. The innovations follow a multivariate t,
#'   which shares one innovation \emph{scale} across the series at each
#'   time point, and large innovations tend to occur together as a
#'   result. Each series keeps its own direction and magnitude. A shock
#'   in one series says nothing about the sign of the others, and one
#'   series can stay unmoved by an event that moves its neighbours.
#'   \code{cor} sets the correlation between series exactly as it does
#'   for Gaussian innovations, and the degrees of freedom control how
#'   heavy the tails of the innovations are.
#'
#'   When \code{df = NA} the degrees of freedom are estimated as
#'   \code{nu_trend}, with a default \code{gamma(4, 0.3)} prior. The
#'   \code{prior} argument replaces it, for example
#'   \code{prior(gamma(2, 0.1), class = "nu_trend")}.
#'   \code{\link{get_prior}} lists \code{nu_trend} with the other trend
#'   classes.
#'
#'   Three caveats. The bound of \code{2} is required: below it the
#'   innovations have no finite variance, and an autoregressive trend
#'   has no stationary covariance to start from. Above it, the first
#'   states of an autoregressive trend take the stationary mean and
#'   covariance. Their distribution matches the stationary law exactly
#'   under Gaussian innovations alone. A finite mix of t variates has
#'   heavier tails than the infinite sum that defines that law. The
#'   degrees of freedom are informed only through the tail of the
#'   latent process and are weakly identified in short series: below
#'   roughly 200 time points, the posterior stays close to its prior.
#'   Not available for \code{VAR()}, which samples its states directly,
#'   or \code{PW()}, which has no innovations.
#' @param ma \code{Logical}. Include moving average terms of order \code{1}?
#'   Default is \code{FALSE}.
#'
#' @param cor \code{Logical}. Include correlated process errors as part of a
#'   multivariate normal process model? If \code{TRUE} and if
#'   \code{n_series > 1} in the supplied data, a fully structured covariance
#'   matrix will be estimated for the process errors. \code{AR()} and
#'   \code{RW()} default to \code{NULL}, which resolves to \code{FALSE}
#'   for an ungrouped trend and to \code{TRUE} whenever \code{gr} names
#'   a grouping variable: a grouped trend estimates correlations among
#'   the \code{subgr} units within each level of \code{gr}, which is
#'   the model \code{gr} exists to specify. \code{VAR()} and
#'   \code{ZMVN()} always estimate correlations and refuse
#'   \code{cor = FALSE}, as do \code{AR(gr = ...)} and
#'   \code{RW(gr = ...)}. \code{CAR()} defaults to \code{FALSE}.
#'
#' @param p The autoregressive lags. The accepted form depends on the
#'   trend type:
#'   * For `AR()` models: a positive integer or a positive integer
#'     vector. A scalar \code{p = k} is the textbook AR(k) and expands
#'     to the consecutive lags \code{1:k}. The fit then estimates
#'     \code{ar1_trend} through \code{ark_trend}. A vector
#'     \code{p = c(...)} selects a sparse lag set. \code{p = c(1, 12)}
#'     declares only \code{ar1_trend} and \code{ar12_trend}, a seasonal
#'     AR with no intermediate lags. \code{p = c(2, 4)} declares only
#'     \code{ar2_trend} and \code{ar4_trend}.
#'   * For `VAR()` models: a positive integer. \code{p = k} is the
#'     VAR(k), with one coefficient matrix for each of the lags
#'     \code{1:k}. A sparse lag vector is not supported. The stationary
#'     initialisation of Heaps (2023) is defined on the companion form
#'     of consecutive lags. A sparse lag set has no companion form with
#'     the same identified stationary covariance. Use
#'     \code{AR(p = c(...))} for sparse-lag autoregression on a single
#'     series.
#'   * For `CAR()` models: must be \code{1}, a continuous-time AR(1)
#'     process.
#'
#' @param time The unquoted name of the `numeric` or `integer` variable in
#'   `data` that holds each row's time. Defaults to `time`.
#'
#' @param series The unquoted name of the `factor` variable in `data` that
#'   names each row's series. Its levels fix the order of the series. A
#'   trend needs this column even for a single series. Defaults to `series`.
#'
#' @details
#' **Important**: Only ONE trend constructor is allowed per `trend_formula`.
#' For complex temporal dynamics, use the parameter options of a single trend type:
#' \itemize{
#'   \item For seasonal patterns: `AR(p = c(1, 12))`. A sum such as
#'     `RW() + AR(p = 12)` is refused
#'   \item For multiple time scales: `AR(p = c(1, 7, 30))` for daily, weekly, monthly
#'   \item For multivariate dynamics: `VAR(p = 2)` captures cross-series relationships
#' }
#'
#' **Stationarity of `AR(p)`**: a consecutive lag set with
#' \code{p >= 2} is sampled as partial autocorrelations,
#' \code{ar1_pacf_trend} through \code{ark_pacf_trend}, each
#' declared on \code{(-1, 1)}. The Levinson-Durbin recursion derives
#' \code{ar1_trend} through \code{ark_trend} from them, and every
#' summary, forecast and hypothesis test uses those coefficients.
#' Any vector of partial autocorrelations in \code{(-1, 1)} gives a
#' stationary process. Bounding the coefficients directly admits
#' explosive draws: \code{ar1_trend} and \code{ar2_trend} both at
#' 0.9 satisfy the declared bounds, and that process grows without
#' limit. Priors for these models name the \code{ar{k}_pacf_trend}
#' classes.
#'
#' \code{AR(p = 1)} samples \code{ar1_trend} directly. At one lag
#' the declared interval is already the stationary region. A sparse
#' lag set such as \code{AR(p = c(1, 12))} also samples
#' \code{ar{k}_trend} directly and fixes its intermediate
#' coefficients at zero, a constraint the recursion cannot express,
#' and carries no stationarity guarantee. Both take priors on the
#' \code{ar{k}_trend} classes.
#'
#' \code{CAR()} steps a continuous-time AR(1) over the gaps the data
#' records. A gap \code{dt} is measured in units of the median gap
#' between the fitted times. \code{ar1_trend} and \code{sigma_trend}
#' then describe a typical step, and the model does not depend on the
#' unit the time column is recorded in. Across a gap \code{dt} the
#' decay is
#' \code{ar1_trend^dt} and the innovation standard deviation is
#' \code{sigma_trend * sqrt((1 - ar1_trend^(2 dt)) /
#' (1 - ar1_trend^2))}. The decay and the innovation scale together.
#' The marginal variance is then
#' \code{sigma_trend^2 / (1 - ar1_trend^2)} at every occasion of an
#' irregular grid, and the first state comes from that marginal.
#'
#' \code{CAR(cor = TRUE)} correlates the innovations of several
#' series. \code{Sigma_trend} is their covariance over a gap of one.
#' Series \code{a} and \code{b} have stationary covariance
#' \code{Sigma_trend[a, b] / (1 - ar1_trend[a] * ar1_trend[b])}, and
#' the innovations across a gap \code{dt} have that covariance times
#' \code{1 - (ar1_trend[a] * ar1_trend[b])^dt}. This is the exact
#' transition of the continuous-time process. A time point added
#' between two others leaves the model for the remaining times
#' unchanged. \code{L_Omega_trend} is the Cholesky factor of the
#' correlation between the series' shocks over an instant, and its
#' prior is placed there. The correlation of \code{Sigma_trend} is
#' closer to zero than that correlation when the series damp at
#' different rates. On a regular grid the likelihood is that of
#' \code{AR(p = 1, cor = TRUE)} with positive coefficients.
#'
#' @note **VAR fits and `init = 0`**: VAR uses the Heaps-2023
#'   stationary joint-distribution initialisation. Setting
#'   `init = 0` in the call to [mvgam()] starts all parameters
#'   at zero on the unconstrained scale, which collapses the
#'   stationary covariance matrix and prevents the chain from
#'   making any valid first step. Leave the sampler's `init`
#'   argument at its default (random) for VAR fits. AR / RW /
#'   CAR / ZMVN are unaffected.
#'
#' @param gr An optional grouping variable, which must be a `factor` in the
#'   supplied `data`, for setting up hierarchical residual correlation
#'   structures. If specified, this sets up a model where the residual
#'   correlations for a specific level of `gr` are modelled
#'   hierarchically:
#'
#'   \eqn{\Omega_{group} = \alpha_{cor}\Omega_{global} +
#'   (1 - \alpha_{cor})\Omega_{group, local}},
#'
#'   where \eqn{\Omega_{global}} is a *global* correlation matrix,
#'   \eqn{\Omega_{group, local}} is a *local deviation* correlation matrix and
#'   \eqn{\alpha_{cor}} is a weighting parameter controlling how strongly the
#'   local correlation matrix \eqn{\Omega_{group}} is shrunk towards the global
#'   correlation matrix \eqn{\Omega_{global}} (larger values of
#'   \eqn{\alpha_{cor}} indicate a greater degree of shrinkage, i.e. a greater
#'   degree of partial pooling).
#'
#'   A grouped trend estimates correlations among the `subgr` units
#'   within each level of `gr`. `cor` defaults to `TRUE` for such a
#'   trend, and an explicit `cor = FALSE` is refused.
#'
#'   When used within a `VAR()` model, this essentially sets up a hierarchical
#'   panel vector autoregression where both the autoregressive and correlation
#'   matrices are learned hierarchically. If `gr` is supplied then `subgr`
#'   *must* also be supplied.
#'
#'   Two constraints currently apply to hierarchical trend models:
#'   \itemize{
#'     \item `gr` must be constant within each series (each series belongs to
#'       a single group). Models with `gr` varying within a series are
#'       rejected at validation time.
#'     \item Every group must hold the same number of series. The Stan
#'       program sizes each group's correlation block from one shared
#'       count, and validation refuses unbalanced groups before
#'       fitting.
#'   }
#'
#' @param coef_sharing Character string, one of `"none"`,
#'   `"shared"`, or `"hierarchical"`, controlling how AR
#'   coefficients vary across latent series in `AR()`. The
#'   default `"none"` estimates an independent coefficient
#'   vector per series (one `ar{lag}_trend` entry per latent
#'   process per lag). `"shared"` collapses to a single
#'   `shared_ar{lag}_trend` scalar per lag, broadcast across all
#'   series in `transformed parameters`. `"hierarchical"` adds
#'   per-lag population-mean (`mu_ar{lag}_trend`) and scale
#'   (`sigma_ar{lag}_trend`) hyperparameters with a pooled
#'   `ar{lag}_trend[j] ~ normal(mu_ar{lag}_trend,
#'   sigma_ar{lag}_trend)`.
#'
#' @param subgr A subgrouping `factor` variable naming the time series
#'   within each level of `gr`. Defaults to `series`. A model with
#'   hierarchical correlations builds its series from `gr` and `subgr`,
#'   and `data` then needs no `series` column.
#'
#'   For example, to model counts of several species (`species` in
#'   `data`) across three regions (`region`) with residuals correlated
#'   within regions, specify `gr = region` and `subgr = species`.
#'   `mvgam()` then builds the series as
#'
#'   `series = interaction(gr, subgr, drop = TRUE, sep = "_",`
#'   `lex.order = TRUE)`
#'
#'   A region `r1` and a species `sp1` give the series `"r1_sp1"`, and
#'   the levels sort lexically. `summary()`, `plot()` and `forecast()`
#'   all label that series `"r1_sp1"`. A `series` column that disagrees
#'   with the derived series is replaced by it, with a warning.
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}.
#'
#' @section AR coefficient-sharing surface:
#' The `coef_sharing` argument selects how AR coefficients
#' vary across the latent series. The three settings and the
#' Stan parameters each declares are:
#' \tabular{ll}{
#'   \strong{coef_sharing}    \tab \strong{Stan parameters with priors} \cr
#'   `"none"`                 \tab `ar{lag}{stem}_trend` per series \cr
#'   `"shared"`               \tab `shared_ar{lag}{stem}_trend` (one per lag) \cr
#'   `"hierarchical"`         \tab `mu_ar{lag}{stem}_trend`, `sigma_ar{lag}{stem}_trend` \cr
#' }
#' `{stem}` is `_pacf` for a contiguous lag set of two or more
#' lags, where the sampled quantity is a partial autocorrelation,
#' and empty for `p = 1` and for a sparse lag set such as
#' `p = c(1, 3)`. `AR(p = 2)` samples `ar1_pacf_trend` and
#' `AR(p = 1)` samples `ar1_trend`.
#' Every setting defines the same `ar{lag}_trend[j]` in
#' `transformed parameters`, and forecasting, IRF, FEVD and summaries
#' use that quantity whatever the setting. A prior on any sampled
#' parameter is set with `brms::set_prior(class = "<name>")`.
#' `get_prior(mvgam_formula(...))` lists the parameters the chosen
#' `coef_sharing` samples.
#'
#' @param n_lv The number of latent factors to estimate for dynamic
#'   factor models. When `n_lv` is smaller than the number of series,
#'   the latent processes are modelled as `n_lv` factors with estimated
#'   loadings onto the series. Defaults to `NULL`, in which case one
#'   latent process is used per series.
#' @param trend_map Optional specification of which latent process each
#'   series maps onto. It fixes the loadings of a dynamic factor model.
#'   See \code{\link{mvgam}} for the accepted formats. Defaults to
#'   `NULL`.
#'
#' @rdname trend_constructors
#'
#' @details Use `vignette("mvgam_overview")` to see the full details of
#'   available stochastic trend types in \pkg{mvgam}, or view the rendered
#'   version on the package website at:
#'   https://nicholasjclark.github.io/mvgam/articles/mvgam_overview.html
#'
#'   For a worked example that interrogates a fitted `VAR()` trend
#'   with `irf()`, `fevd()` and `stability()`, see
#'   [https://nicholasjclark.github.io/mvgam/articles/var.html](https://nicholasjclark.github.io/mvgam/articles/var.html).
#'   For the `trend_map` shortcut used to fit dynamic factor models
#'   with fewer latent processes than observed series, see
#'   `vignette("dfm", package = "mvgam")`.
#'
#' @section Parameter Naming Convention:
#' Every trend parameter carries a `_trend` suffix, which keeps it
#' distinct from an observation model parameter of the same name. The
#' innovation scale is \code{sigma_trend}, the moving-average
#' coefficient \code{theta1_trend}, the autoregressive coefficients
#' \code{ar1_trend} to \code{ark_trend} and the VAR coefficient
#' matrices \code{Phi_trend}. Priors take these class names, as in
#' \code{prior(normal(0, 1), class = sigma_trend)}, and parameter
#' extraction takes the same names.
#'
#' @section Identification:
#' Factor-model fits (\code{n_lv < n_series}) sample the loadings
#' matrix `Z` unconstrained. Following Heaps & Jermyn (2024), generated
#' quantities identify it by a thin QR decomposition. The fit saves the
#' identified loadings `Z_tilde` with the unrotated `Z`, and the rotated
#' factor paths `lv_trend_tilde` with `lv_trend`. Post-fit methods use
#' the identified versions where they exist. \code{qr_thin_R()} gives `Z_tilde` a non-negative diagonal,
#' which removes the \eqn{2^k} equivalent sign modes. The per-factor
#' coefficients `ar1_trend` and `theta1_trend` stay in the unrotated
#' basis. A VAR factor model also rotates its lag coefficients as
#' `Phi_trend_tilde[lag] = Q_tilde * Phi_trend[lag] * Q_tilde'`.
#'
#' A \code{trend_map} skips the QR step and keeps its fixed entries on
#' `Z` exactly. A `by = lv_axis()` smooth ties each factor to its own
#' covariate effect. Because a rotation would mix those effects, that
#' model skips the QR step as well. For a free factor model, the \code{loadings_prior}
#' argument of \code{mvgam()} replaces the default iid Student-t prior
#' on `Z` with a structured matrix-normal prior. That prior is built
#' from per-series features, pairwise distance matrices or both. See
#' \code{\link{mvgam}} for its arguments.
#'
#' Setting \code{loadings_prior = "mgp"} (or
#' \code{loadings_prior = list(column_shrinkage = "mgp")}) switches
#' the column scaling to the multiplicative gamma process prior of
#' Bhattacharya & Dunson (2011), which shrinks later columns of `Z`
#' toward zero with increasing strength. `n_lv` is then a truncation
#' ceiling on the factor count. \code{\link{active_factors}} reports
#' the posterior distribution of the number of active columns.
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' Heaps, S. E. (2023). Enforcing stationarity through the prior
#' in vector autoregressions. \emph{Journal of Computational and
#' Graphical Statistics}, 32:74-83. (VAR stationarity prior used
#' for \code{VAR()} trends.)
#'
#' Clark, N. J., Ernest, S. K. M., Senyondo, H., Simonis, J.,
#' White, E. P., Yenni, G. M. and Karunarathna, K. A. N. K.
#' (2025). Beyond single-species models: multispecies forecasts
#' for ecological predictability. \emph{PeerJ}, 13:e18929.
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \dontrun{
#' # Simulate three Gaussian series driven by a correlated VAR(1)
#' # process so the cross-series dependencies are recoverable.
#' set.seed(0)
#' simdat <- sim_mvgam(
#'   family       = gaussian(),
#'   n_series     = 3L,
#'   n_timepoints = 120L,
#'   trend_model  = VAR(),
#'   prop_trend   = 0.95
#' )
#'
#' # Fit a VAR(1) state-space model. The trend constructor goes
#' # inside `trend_formula`; the obs side carries only the
#' # intercept.
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ VAR(p = 1),
#'   data          = simdat$data_train,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod)
#'
#' # `variable = "trend_params"` is a keyword shortcut that pulls
#' # every trend-dynamics parameter (Phi_trend, sigma_trend, the
#' # Sigma_trend covariance, etc.) in one call. See ?mvgam_draws
#' # for the full keyword set ("betas", "obs_params",
#' # "smooth_params", "trend_betas", "trend_params",
#' # "trend_smooth_params").
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#'
#' # Post-fit sweep on the same VAR(1). `residual_cor()` pulls
#' # the innovation correlation matrix between the three series.
#' # `irf()` shows how a one-time shock to one series ripples
#' # through the others over h steps; `fevd()` reports the share
#' # of each series' forecast-error variance attributable to each
#' # shock; `stability()` summarises the long-run dynamics
#' # (reactivity = peak amplification of a shock; mean_return_rate
#' # = speed of decay back to the stationary distribution).
#' residual_cor(mod)
#' plot(irf(mod, h = 8L), series = 1)
#' plot(fevd(mod, h = 8L))
#' head(stability(mod)[, c("reactivity", "mean_return_rate")])
#'
#' # Forecast the held-out cells. `forecast()` propagates
#' # uncertainty through the VAR; `plot(fc, series = i)` overlays
#' # the in-sample hindcast and the out-of-sample interval.
#' fc <- forecast(mod, newdata = simdat$data_test)
#' plot(fc, series = 1)
#'
#' # Continuous-time AR(1) on irregularly-spaced data. Recipe 6
#' # of sim_mvgam() defaults to CAR() with U(1, 6) time gaps.
#' set.seed(7)
#' simdat_car <- sim_mvgam(
#'   type         = 6L,
#'   family       = gaussian(),
#'   n_series     = 1L,
#'   n_timepoints = 120L
#' )
#'
#' # Fit the CAR(1) model. ar1_trend[1] is the continuous-time
#' # decay parameter and adapts to the per-step time gap.
#' mod_car <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ CAR(),
#'   data          = simdat_car$data_train,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' mcmc_plot(mod_car, variable = "trend_params", type = "intervals")
#'
#' # Other trend constructors swap in the same place. For example
#' # an AR(1) on a single series:
#' #   trend_formula = ~ AR(p = 1)
#' # or a zero-mean multivariate normal residual prior for cross-
#' # series correlations:
#' #   trend_formula = ~ ZMVN()
#'
#' # Hierarchical VAR with two regions and two outcomes per region.
#' # `gr` names the grouping unit (region) and `subgr` the series
#' # within it (outcome). The fit estimates a population innovation
#' # correlation across outcomes and per-region deviations from it.
#' # `alpha_cor_trend`, with a beta(3, 2) prior by default, sets how
#' # strongly the two are pooled. The simulated outcomes share a
#' # correlation of about 0.6, which gives the population estimate a
#' # signal to recover.
#' set.seed(3)
#' n_t <- 30L; rho <- 0.6
#' Sigma <- matrix(c(1, rho, rho, 1), 2L, 2L)
#' L <- chol(Sigma)
#' regions <- c("A", "B")
#' hdat <- do.call(rbind, lapply(regions, function(r) {
#'   eps <- matrix(rnorm(n_t * 2L), n_t, 2L) %*% L
#'   data.frame(
#'     time    = rep(seq_len(n_t), 2L),
#'     region  = factor(r, levels = regions),
#'     outcome = factor(rep(c("gdp", "cons"), each = n_t),
#'                       levels = c("gdp", "cons")),
#'     y       = c(eps[, 1L], eps[, 2L])
#'   )
#' }))
#'
#' mod_hv <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ VAR(
#'     p = 1, gr = region, subgr = outcome, cor = TRUE
#'   ),
#'   data    = hdat,
#'   family  = gaussian(),
#'   chains  = 1,
#'   iter    = 600,
#'   warmup  = 300,
#'   silent  = 2
#' )
#'
#' # Marginal posterior for the pooling weight. Mass near 1 means
#' # the per-region correlations track the population correlation;
#' # mass near 0 lets the regions diverge.
#' mcmc_plot(mod_hv, variable = "alpha_cor_trend", type = "hist")
#'
#' # `residual_cor()` returns the population (across-outcome)
#' # correlation by default. `by_group = TRUE` also returns the
#' # per-region correlations, for comparing the regions.
#' residual_cor(mod_hv)
#' residual_cor(mod_hv, by_group = TRUE)
#' }
#'
#' @export
RW <- function(
    time = NA,
    series = NA,
    ma = FALSE,
    cor = NULL,
    gr = NA,
    subgr = NA,
    n_lv = NULL,
    trend_map = NULL,
    df = Inf) {

  assert_trend_map_input(trend_map)

  create_mvgam_trend(
    "RW",
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    ma = ma,
    cor = cor,
    n_lv = n_lv,
    trend_map = trend_map
  )
}

#' @rdname trend_constructors
#' @export
AR <- function(time = NA, series = NA, p = 1, ma = FALSE, cor = NULL,
              gr = NA, subgr = NA, n_lv = NULL, trend_map = NULL,
              coef_sharing = c("none", "shared", "hierarchical"),
              df = Inf) {
  # Validate AR order parameter
  if (length(p) == 1) {
    checkmate::assert_int(p, lower = 1)
  } else {
    checkmate::assert_integerish(p, lower = 1, unique = TRUE, sorted = TRUE)
  }

  assert_trend_map_input(trend_map)
  coef_sharing <- match.arg(coef_sharing)

  # The raw `trend_map` joins the spec. `normalise_trend_map()` turns
  # it into loadings once the data is in scope.
  create_mvgam_trend(
    "AR",
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    p = p,
    ma = ma,
    cor = cor,
    n_lv = n_lv,
    trend_map = trend_map,
    coef_sharing = coef_sharing
  )
}

#' @rdname trend_constructors
#' @export
CAR <- function(time = NA, series = NA, cor = FALSE, n_lv = NULL,
               trend_map = NULL, df = Inf) {
  # A continuous-time process evolves per series and has no factor
  # form. The two arguments exist here to refuse a factor request
  # with the reason the registry records, matching the refusal the
  # other three routes to a factor CAR produce.
  refuse_constructor_factor_request(n_lv, trend_map, "CAR")
  checkmate::assert_flag(cor)

  create_mvgam_trend(
    "CAR",  # Base trend type used for ALL dispatch
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    p = 1,        # CAR is always first-order
    ma = FALSE,   # CAR takes no moving-average term
    cor = cor
  )
}

#' @rdname trend_constructors
#' @export
VAR <- function(time = NA, series = NA, p = 1, ma = FALSE, cor = TRUE,
               gr = NA, subgr = NA, n_lv = NULL, trend_map = NULL) {
  # A VAR has correlated innovations by definition. `cor` is accepted
  # for symmetry with AR(), RW() and ZMVN(), and only TRUE is valid.
  if (isFALSE(cor)) {
    stop(insight::format_error(c(
      "VAR(cor = FALSE) is not supported.",
      i = "'AR()' fits independent series."
    )))
  }
  # A scalar order `p` means the consecutive lags 1..p. The `p`
  # documentation in `?trend_constructors` gives the reason a sparse
  # lag set is refused.
  if (length(p) != 1L) {
    stop(insight::format_error(c(
      paste0(
        "Sparse-lag VAR (vector 'p') is not supported."
      ),
      x = paste0("Got 'p' = ", paste(p, collapse = ", "), "."),
      i = "'AR()' takes sparse lags."
    )))
  }
  checkmate::assert_int(p, lower = 1)
  assert_trend_map_input(trend_map)

  create_mvgam_trend(
    "VAR",
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    p = p,
    ma = ma,
    cor = TRUE,
    n_lv = n_lv,
    trend_map = trend_map
  )
}

#' Specify piecewise linear or logistic trends in \pkg{mvgam} models
#'
#' Set up piecewise linear or logistic trend models in \code{mvgam}. These
#' functions do not evaluate their arguments – they exist purely to help set up
#' a model with particular piecewise trend models.
#'
#' @param cap The unquoted name of the variable in `data` that specifies the
#'   carrying capacity for logistic growth models. Required when `growth = 'logistic'`.
#'   This variable should be numeric and can vary by time and series if necessary.
#'   Defaults to `cap` when not specified, with a warning for logistic models.
#'
#' @param n_changepoints A non-negative integer specifying the number of
#'   potential changepoints. Potential changepoints are selected uniformly from
#'   the first `changepoint_range` proportion of timepoints in \code{data}.
#'   Default is `10`.
#'
#' @param changepoint_range Proportion of history in \code{data} in which trend
#'   changepoints will be estimated. Defaults to `0.8` for the first 80%.
#'
#' @param changepoint_scale Parameter modulating the flexibility of the
#'   automatic changepoint selection by altering the scale parameter of a
#'   Laplace distribution. The resulting prior will be
#'   `double_exponential(0, changepoint_scale)`. Large values will allow many
#'   changepoints and a more flexible trend, while small values will allow few
#'   changepoints. Default is `0.05`.
#'
#' @param growth Character string specifying either `'linear'` or `'logistic'`
#'   growth of the trend. If `'logistic'`, the `cap` argument must specify the
#'   variable containing maximum saturation points for the trend (see
#'   details and examples in \code{\link{mvgam}} for more information). Default
#'   is `'linear'`.
#'
#' @inheritParams AR
#'
#' @author Nicholas J Clark
#'
#' @references Taylor, Sean J., and Benjamin Letham. "Forecasting at scale."
#'   The American Statistician 72.1 (2018): 37–45.
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \code{mvgam}.
#'
#' @details
#' *Offsets and intercepts*:
#' A linear piecewise trend takes its level from the observation
#' formula, matching every other trend model in \pkg{mvgam}, which
#' leaves `y ~ 1` as the ordinary spelling for it.
#'
#' A logistic piecewise trend estimates an offset (`m_trend`) that
#' positions its curve along the time axis. It contributes
#' `cap * inv_logit(...)` on the link scale, which saturates the
#' expectation at the `cap` you supply when the observation formula
#' omits its intercept (`y ~ -1` or `y ~ 0 + ...`). An intercept
#' there raises the ceiling above `cap`.
#'
#' *Logistic growth and the cap variable*:
#' Many growing series approach a ceiling, such as a total market size,
#' a population size or an ecological carrying capacity.
#' `growth = 'logistic'` saturates the trend at a capacity that `data`
#' supplies in its `cap` column. The capacity can vary by series and by
#' time, and every observation needs a `cap` value.
#'
#' For observation families that use a non-identity link function, the
#' `cap` value is transformed to the link scale internally: it is
#' log-transformed under a `poisson()` or `negbinomial()` family. Give
#' `cap` on the scale of the outcome. No missing values are allowed in
#' `cap`.
#'
#' @rdname piecewise_trends
#'
#' @examples
#' \dontrun{
#' # Linear PW on a single Poisson series. The observation
#' # intercept supplies the level, matching the other trend
#' # models in mvgam.
#' set.seed(2024)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L)
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ PW(growth = "linear"),
#'   data          = simdat$data_train,
#'   family        = poisson(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod)
#'
#' # A linear PW exposes the base growth rate (k_trend) and the
#' # changepoint rate deltas (delta_trend[changepoint, series]).
#' # A logistic PW adds the offset (m_trend). Visualise the full
#' # set with the trend_params keyword.
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#' }
#'
#' @export
PW <- function(time = NA, series = NA, cap = NA, n_changepoints = 10,
              changepoint_range = 0.8, changepoint_scale = 0.05,
              growth = 'linear', n_lv = NULL, trend_map = NULL) {
  # Validate arguments
  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  checkmate::assert_number(changepoint_range, lower = 0, upper = 1)
  checkmate::assert_int(n_changepoints, lower = 1)
  checkmate::assert_number(changepoint_scale, lower = 0)

  # A factor request written on the constructor is refused here.
  # `CAR()` uses the same helper.
  refuse_constructor_factor_request(n_lv, trend_map, "PW")

  # A logistic PW needs a carrying capacity. The constructor runs
  # while the trend formula is parsed, before the data is in scope.
  # `build_pw_cap_matrix()` checks the data for the `cap` column
  # instead, and its refusal gives the missing column.
  create_mvgam_trend(
    "PW",
    .time = substitute(time),
    .series = substitute(series),
    .cap = substitute(cap),
    n_changepoints = n_changepoints,
    changepoint_range = changepoint_range,
    changepoint_scale = changepoint_scale,
    growth = growth
  )
}

#' Specify correlated residual processes in \pkg{mvgam}
#'
#' Set up latent correlated multivariate Gaussian residual processes in
#' \pkg{mvgam}. This function does not evaluate its arguments – it exists
#' purely to help set up a model with particular error processes
#'
#' @inheritParams AR
#' @param time The unquoted name of the variable that represents the unit of
#'   analysis in `data` over which latent residuals should be correlated. This
#'   variable should be either a `numeric` or `integer` variable in the
#'   supplied `data`. Defaults to `time` to be consistent with other
#'   functionalities in \pkg{mvgam}, though note that the data need not be time
#'   series in this case. See examples below for further details and
#'   explanations.
#'
#'   Unlike \code{\link{AR}} / \code{\link{VAR}} / \code{\link{RW}}, ZMVN
#'   does not require the `unit` values to be regularly spaced. The
#'   likelihood is \eqn{x_{u, :} \sim MVN(0, \Sigma)} with \eqn{\Sigma}
#'   indexed by series only, so \eqn{\Delta u} never enters the math.
#'   Use ZMVN for spatial or otherwise non-temporal grouping factors
#'   where gaps in the `unit` axis are natural (e.g. dropped sites,
#'   stratified k-fold refits, irregular sampling grids).
#'
#' @param gr An optional grouping `factor` in `data` that sets up
#'   hierarchical residual correlations. The correlation matrix of each
#'   level of `gr` is
#'
#'   \eqn{\Omega_{group} = \alpha_{cor}\Omega_{global} +
#'   (1 - \alpha_{cor})\Omega_{group, local}},
#'
#'   where \eqn{\Omega_{global}} is a *global* correlation matrix,
#'   \eqn{\Omega_{group, local}} a *local deviation* correlation matrix
#'   and \eqn{\alpha_{cor}} (`alpha_cor_trend`) the weight that shrinks
#'   each group's matrix toward the global one. Supplying `gr` requires
#'   `subgr`. `cor` defaults to `TRUE` for a grouped trend, and an
#'   explicit `cor = FALSE` is refused.
#'
#' @param subgr A subgrouping `factor` in `data` naming the
#'   observational units within each level of `gr`. Defaults to
#'   `series`, and the data need not be time series. A model with
#'   hierarchical correlations builds its series from `gr` and `subgr`,
#'   and `data` then needs no `series` column.
#'
#'   For example, to model counts of several species (`species` in
#'   `data`) at sampling sites (`site`) in three regions (`region`)
#'   with residuals correlated within regions, specify `unit = site`,
#'   `gr = region` and `subgr = species`. `mvgam()` orders the data by
#'   `unit` and builds the series as
#'
#'   `series = interaction(gr, subgr, drop = TRUE, sep = "_",`
#'   `lex.order = TRUE)`
#'
#'   A region `r1` and a species `sp1` give the series `"r1_sp1"`. A
#'   `series` column that disagrees with the derived series is replaced
#'   by it, with a warning.
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}
#'
#' @inheritSection RW Identification
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @seealso \code{\link{AR}}, \code{\link{VAR}}, \code{\link{RW}},
#'   \code{\link{CAR}}, \code{\link{mvgam}}, \code{\link{jsdgam}},
#'   \code{\link{residual_cor}}, \code{\link{ordinate}}. The
#'   `dfm` vignette walks through dynamic factor models built with
#'   a partial `trend_map`
#'   (\code{vignette("dfm", package = "mvgam")}); for the
#'   integrated species distribution model pattern that shares one
#'   latent process across several observation families, see
#'   \url{https://nicholasjclark.github.io/mvgam/articles/idm.html}.
#'
#' @examples
#' \dontrun{
#' # Simulate four correlated Gaussian series. ZMVN treats each time
#' # point as an independent multivariate normal draw, which leaves
#' # the cross-series covariance as the structure it recovers.
#' set.seed(2)
#' simdat <- sim_mvgam(
#'   family       = gaussian(),
#'   n_series     = 4L,
#'   n_timepoints = 120L,
#'   trend_model  = VAR(),
#'   prop_trend   = 0.9
#' )
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ ZMVN(),
#'   data          = simdat$data_train,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod)
#'
#' # All trend-side latent dynamics in one summary view. For ZMVN
#' # the meaningful entries are sigma_trend (per-series SDs),
#' # L_Omega_trend (Cholesky of the correlation matrix) and
#' # Sigma_trend (the implied covariance).
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#' }
#'
#' @export
ZMVN <- function(time = NA, series = NA, gr = NA, subgr = NA,
                 n_lv = NULL, cor = TRUE, trend_map = NULL,
                 df = Inf) {
  # `cor` is accepted for symmetry with `AR()` and `VAR()`. ZMVN is a
  # multivariate normal whose purpose is the correlation among series.
  checkmate::assert_flag(cor)
  assert_trend_map_input(trend_map)
  if (!cor) {
    stop(insight::format_error(c(
      "'cor = FALSE' is not supported for 'ZMVN()'.",
      x = "ZMVN is a multivariate normal with correlated series.",
      i = "'AR()' and 'RW()' fit independent series."
    )))
  }

  create_mvgam_trend(
    "ZMVN",
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    n_lv = n_lv,
    ma = FALSE,
    cor = TRUE,
    trend_map = trend_map
  )
}

# =============================================================================
# SECTION 6: TREND CONSTRUCTOR HELPER FUNCTIONS
# =============================================================================
# WHY: These helper functions simplify trend constructor development and
# ensure consistency across all trend types. They provide universal defaults,
# automatic validation rule assignment, and standardized creation patterns.

#' Get Universal mvgam Trend Defaults
#'
#' @description
#' Returns universal default values that apply to all trend types.
#' Trend-specific defaults should be handled within individual constructors.
#'
#' @return Named list of universal default values
#' @noRd
get_mvgam_trend_defaults <- function() {
  list(
    # Universal variable defaults
    time = "time",
    series = "series",
    gr = "NA",
    subgr = "NA",

    # Universal behavior defaults
    ma = FALSE,
    cor = FALSE
  )
}

#' Apply mvgam Trend Defaults
#'
#' @description
#' Fills in missing fields in a trend object with the universal
#' defaults, then assigns the validation rules of its trend type.
#'
#' @param trend_obj Partial trend object (list)
#' @return Trend object with all fields filled
#' @noRd
apply_mvgam_trend_defaults <- function(trend_obj) {
  checkmate::assert_list(trend_obj)
  checkmate::assert_character(trend_obj$trend, len = 1, min.chars = 1,
                             null.ok = FALSE, .var.name = "trend_obj$trend")

  # Get universal defaults
  defaults <- get_mvgam_trend_defaults()

  # Apply universal defaults for missing fields
  for (field in names(defaults)) {
    if (!field %in% names(trend_obj)) {
      trend_obj[[field]] <- defaults[[field]]
    }
  }

  trend_obj$validation_rules <- get_default_validation_rules(trend_obj$trend)

  return(trend_obj)
}

#' Validation rules of a trend type
#'
#' @param trend_type Character string, the trend type
#' @return Character vector of validation rule strings, from the
#'   type's registered `requires_regular_intervals`
#' @noRd
get_default_validation_rules <- function(trend_type) {
  checkmate::assert_string(trend_type, min.chars = 1)
  ensure_registry_initialized()
  if (get_trend_info(trend_type)$requires_regular_intervals) {
    return(rule_requires_regular_intervals)
  }
  character(0)
}

#' Create mvgam Trend Object
#'
#' @description
#' Builds the `mvgam_trend` object every trend constructor returns,
#' filling the universal defaults and the trend type's validation
#' rules, then validating it.
#'
#' @param trend_type Registered trend type (e.g., "AR", "RW", "VAR")
#' @param ... Additional trend-specific parameters
#' @param .time Time variable (quoted or unquoted)
#' @param .series Series variable (quoted or unquoted)
#' @param .gr Grouping variable (quoted or unquoted)
#' @param .subgr Subgrouping variable (quoted or unquoted)
#' @param .cap Optional carrying-capacity variable (quoted or unquoted) for
#'   logistic piecewise trends
#' @return mvgam_trend object
#' @noRd
create_mvgam_trend <- function(trend_type, ...,
                               .time = NULL,
                               .series = NULL,
                               .gr = NULL,
                               .subgr = NULL,
                               .cap = NULL) {
  checkmate::assert_string(trend_type, min.chars = 1)

  # Helper to process substituted arguments - handles both quoted and unquoted
  process_arg <- function(x) {
    if (is.null(x)) return("NA")  # Return "NA" for missing arguments
    if (is.character(x)) return(x)
    deparsed <- deparse0(x)
    # Remove surrounding quotes if present (from quoted strings)
    if (grepl('^".*"$', deparsed)) {
      return(substr(deparsed, 2, nchar(deparsed) - 1))
    }
    # `substitute()` returns the argument's own name when the caller
    # left it unset
    if (deparsed %in% c("gr", "subgr", "time", "series", "cap")) {
      return("NA")
    }
    deparsed
  }

  # Process all variables consistently
  time_var <- process_arg(.time)
  series_var <- process_arg(.series)
  gr_var <- process_arg(.gr)
  subgr_var <- process_arg(.subgr)
  cap_var <- process_arg(.cap)

  # Handle default variable names
  if (time_var == "NA") time_var <- "time"
  if (series_var == "NA") series_var <- "series"

  # Handle cap default
  if (cap_var == "NA") cap_var <- "cap"


  # Create base trend object
  trend_obj <- list(
    trend = trend_type,
    time = time_var,
    series = series_var,
    gr = gr_var,
    subgr = subgr_var,
    cap = cap_var,
    ...
  )

  # The arguments the constructors share, checked once for all of
  # them
  checkmate::assert_int(trend_obj$n_lv, lower = 1, null.ok = TRUE,
                        .var.name = "n_lv")
  checkmate::assert_flag(trend_obj$ma, null.ok = TRUE, .var.name = "ma")
  checkmate::assert_flag(trend_obj$cor, null.ok = TRUE, .var.name = "cor")

  # A grouping estimates correlations among the `subgr` units within
  # each level of `gr`. The generated Stan program declares the group
  # correlation parameters whenever `gr` is named, for either value of
  # `cor`. `cor` resolves to TRUE for a grouped trend, and an explicit
  # `cor = FALSE` is refused the way `VAR()` and `ZMVN()` refuse it.
  if (named_var(gr_var)) {
    if (isFALSE(trend_obj$cor)) {
      stop(insight::format_error(c(
        "A grouped trend requires correlated innovations.",
        x = paste0(
          "'gr' models correlations among the '", subgr_var,
          "' units within each level of '", gr_var, "'."
        ),
        i = paste0(
          "Drop 'cor = FALSE' to keep the grouping, or drop 'gr' and ",
          "'subgr' for independent per-series innovations."
        )
      )))
    }
    trend_obj$cor <- TRUE
  } else if (is.null(trend_obj$cor)) {
    trend_obj$cor <- FALSE
  }

  trend_obj <- apply_mvgam_trend_defaults(trend_obj)

  # Set class
  class(trend_obj) <- "mvgam_trend"

  # Validate the assembled object
  validate_mvgam_trend(trend_obj)

  return(trend_obj)
}



# Validate innovation degrees of freedom supplied to a trend
# constructor. `Inf` gives Gaussian innovations: a t with infinite
# degrees of freedom is a normal. `NA` estimates them. A finite value
# fixes them and must exceed 2. The stationary initialisation of an
# autoregressive trend divides by `sqrt(1 - phi^2)`, which presumes
# the innovations have a finite second moment, and a t has one only
# above 2 degrees of freedom.
#' @noRd
assert_trend_df <- function(df) {
  usage <- paste0(
    "Use 'df = Inf' for Gaussian innovations, 'df = NA' to estimate ",
    "the degrees of freedom, or a number above 2 to fix them."
  )
  if (length(df) != 1L || !(is.numeric(df) || is.logical(df))) {
    stop(insight::format_error(c(
      "Argument 'df' must be a single number, 'Inf' or 'NA'.",
      x = paste0("Got an object of class '", class(df)[1], "' of length ",
                 length(df), "."),
      i = usage
    )))
  }
  if (is.na(df)) {
    return(NA_real_)
  }
  df <- as.numeric(df)
  if (is.infinite(df)) {
    if (df < 0) {
      stop(insight::format_error(c(
        "Argument 'df' must be positive.",
        x = "Got '-Inf'.",
        i = usage
      )))
    }
    return(Inf)
  }
  if (df <= 2) {
    stop(insight::format_error(c(
      "Argument 'df' must be greater than 2.",
      x = paste0("Got 'df = ", df, "'."),
      x = paste0("At or below 2 the innovations have infinite variance ",
                 "and the stationary start is undefined.")
    )), call. = FALSE)
  }
  df
}
