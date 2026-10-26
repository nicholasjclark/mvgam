# Trend system: the registry of trend types and their properties,
# validation of trend specifications, trend formula parsing and the
# trend constructors (RW, AR, VAR, CAR, ZMVN, PW).

# =============================================================================
# Trend registry
# =============================================================================
# Each trend type records its properties and its Stan generator here.
# `get_trend_info()` and `trend_property()` return them.

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
#' @param per_factor_coefficients Logical; does each latent factor
#'   take its own dynamic coefficients? Such factors may be reordered
#'   and flipped in sign. `factor_identification()` relabels them.
#'   Factors without their own coefficients may be rotated freely.
#' @param completes_time_grid Logical; does the trend hold a latent
#'   state for a series at a time the data has no row for? Such a
#'   trend takes series observed at their own times.
#' @param shared_innovations Logical; does the trend take its
#'   innovations from the shared system, which emits `sigma_trend`,
#'   `L_Omega_trend`, the grouped correlations and the multiplicative
#'   gamma process column scale? A trend that scales its own
#'   innovations declares them in its generator.
#' @param generator_func Function that generates Stan code for this
#'   trend type
#' @param incompatibility_reason Character string explaining why factor
#'   models are unavailable, required when `supports_factors` is FALSE
#' @return Invisibly returns TRUE on successful registration
#' @noRd
register_trend_type <- function(name, supports_factors, covariance_pattern,
                                stationary_source,
                                requires_regular_intervals,
                                per_factor_coefficients,
                                completes_time_grid,
                                shared_innovations,
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
  checkmate::assert_flag(per_factor_coefficients)
  checkmate::assert_flag(completes_time_grid)
  checkmate::assert_flag(shared_innovations)
  checkmate::assert_function(generator_func,
                             args = c("trend_specs", "data_info"))
  checkmate::assert_string(incompatibility_reason,
                           null.ok = supports_factors)

  trend_registry[[name]] <- list(
    supports_factors = supports_factors,
    covariance_pattern = covariance_pattern,
    stationary_source = stationary_source,
    requires_regular_intervals = requires_regular_intervals,
    per_factor_coefficients = per_factor_coefficients,
    completes_time_grid = completes_time_grid,
    shared_innovations = shared_innovations,
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
      did_you_mean(name, available_trends),
      i = cli::format_inline("Available types: {.val {available_trends}}")
    )), call. = FALSE)
  }

  trend_registry[[name]]
}

#' List Available Trend Types
#'
#' @description
#' List all registered trend types and their factor model support.
#'
#' @return Data frame with trend types and their properties
#' @family trend models
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
#' convention. Trend type "FOO" defines two functions.
#' `generate_foo_trend_stanvars()` emits its Stan code.
#' `foo_trend_properties()` returns the arguments of
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
    # A trend is registered once it defines a function for every task.
    for (template in trend_method_templates) {
      trend_method(trend_type, template)
    }
    do.call(register_trend_type, c(
      list(name = trend_type,
           generator_func = get(generator_name, envir = ns)),
      trend_info
    ))
  }

  invisible(TRUE)
}

#' The tasks each trend performs with its own function
#'
#' `%s` takes the lower-case trend type. `register_core_trends()`
#' requires each trend to define all of them. `trend_method()` finds
#' one.
#' @noRd
trend_method_templates <- c(
  monitor_params = "generate_%s_monitor_params",
  last_state = "extract_%s_state",
  propagate = "propagate_%s",
  latent_rows = "render_latent_%s",
  glossary = "glossary_%s"
)

#' The tasks a trend may perform with its own function
#'
#' A trend without one takes the `default` passed to `trend_method()`.
#' `forecast_context` returns the named arguments of
#' `propagate_trend()` a forecast derives from the fit and the new
#' times. `parameter_prior` returns the prior and bounds of a
#' parameter whose default depends on the trend, or NULL.
#' `validate_data` takes the specification, the data's resolved axes
#' and the model specification. It refuses data the trend cannot
#' model.
#' @noRd
trend_method_optional_templates <- c(
  forecast_context = "forecast_context_%s",
  parameter_prior = "get_%s_parameter_prior",
  validate_data = "validate_data_%s"
)


#' The function a trend type supplies for one task
#'
#' Found by name, as `register_core_trends()` finds a trend's
#' generator. Each task's functions share one signature.
#'
#' @param trend_type A registered trend type, such as `"AR"`
#' @param template One of `trend_method_templates` or
#'   `trend_method_optional_templates`
#' @param default The function an optional task falls back to
#' @return The function
#' @noRd
trend_method <- function(trend_type, template,
                         default = function(...) list()) {
  checkmate::assert_string(trend_type, min.chars = 1L)
  checkmate::assert_choice(
    template, c(trend_method_templates, trend_method_optional_templates)
  )
  name <- sprintf(template, tolower(trend_type))
  fn <- get0(name, envir = asNamespace("mvgam"), mode = "function",
             inherits = FALSE)
  if (is.null(fn) && template %in% trend_method_optional_templates) {
    return(default)
  }
  if (is.null(fn)) {
    stop_mvgam_fault(
      paste0("The '", trend_type, "' trend lacks '", name, "()'."),
      "Each registered trend defines one function per task."
    )
  }
  fn
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
      "requires_regular_intervals", "per_factor_coefficients",
      "completes_time_grid", "shared_innovations",
      "incompatibility_reason"),
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
    per_factor_coefficients = TRUE,
    completes_time_grid = FALSE,
    shared_innovations = TRUE,
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
    per_factor_coefficients = FALSE,
    completes_time_grid = FALSE,
    shared_innovations = TRUE,
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
    per_factor_coefficients = FALSE,
    completes_time_grid = FALSE,
    shared_innovations = FALSE,
    incompatibility_reason = NULL
  )
}

#' LLT Trend Properties
#'
#' The level takes the shared innovations. Under sampled loadings the
#' factors share one slope scale and one damping, which keeps the
#' factors exchangeable under rotation.
#' @noRd
llt_trend_properties <- function() {
  list(
    supports_factors = TRUE,
    covariance_pattern = "cholesky_scaled",
    # An undamped trend has no stationary covariance. The marginal
    # prediction keeps the innovation covariance.
    stationary_source = "none",
    requires_regular_intervals = TRUE,
    per_factor_coefficients = FALSE,
    completes_time_grid = FALSE,
    shared_innovations = TRUE,
    incompatibility_reason = NULL
  )
}

#' Refuse data too short for a local linear trend
#'
#' The slope variance is the variance of the level's second
#' differences, which a short series barely informs. A time term in a
#' formula competes with the slope for the same variation.
#'
#' @param spec The parsed `LLT()` specification
#' @param dimensions The data's resolved axes
#' @param mv_spec The model specification
#' @return `NULL`, invisibly
#' @noRd
validate_data_llt <- function(spec, dimensions, mv_spec) {
  n_time <- dimensions$n_time
  if (n_time < 5L) {
    stop(insight::format_error(c(
      "'LLT()' needs at least 5 time points.",
      x = paste0("The data have ", n_time, "."),
      i = "Use 'RW()' for a shorter series."
    )), call. = FALSE)
  }
  if (n_time < 20L) {
    insight::format_warning(c(
      paste0("'LLT()' on ", n_time, " time points barely informs ",
             "'sigma_slope_trend'."),
      i = paste0("Use 'RW()', or give 'sigma_slope_trend' an ",
                 "informative prior.")
    ))
  }
  time_var <- spec$time
  in_formula <- time_var %in% c(
    all.vars(mv_spec$base_formula),
    if (!is.null(mv_spec$obs_formula)) {
      all.vars(brms::brmsterms(mv_spec$obs_formula)$allvars)
    }
  )
  if (in_formula) {
    insight::format_warning(c(
      paste0("'", time_var, "' appears in a formula with 'LLT()'."),
      x = "A function of time and the latent slope explain the same variation.",
      i = paste0("Drop the '", time_var, "' term or use 'RW()'.")
    ))
  }
  invisible(NULL)
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
    per_factor_coefficients = FALSE,
    completes_time_grid = FALSE,
    shared_innovations = TRUE,
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
    per_factor_coefficients = FALSE,
    # Every series has a state at every time of the union grid. A time
    # added between two others leaves the model unchanged.
    completes_time_grid = TRUE,
    shared_innovations = FALSE,
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
    per_factor_coefficients = FALSE,
    completes_time_grid = FALSE,
    shared_innovations = FALSE,
    incompatibility_reason =
      "Piecewise trends model changepoints separately for each series."
  )
}

#' The trend constructors with given properties, for a message
#'
#' @param require Names of logical registry properties every listed
#'   trend holds, such as `"supports_factors"`
#' @return Character scalar such as `"AR(), RW()"`
#' @noRd
registered_trend_constructors <- function(require = character(0)) {
  checkmate::assert_character(require, any.missing = FALSE)
  ensure_registry_initialized()
  trends <- ls(trend_registry)
  keep <- vapply(trends, function(tt) {
    all(vapply(require, function(field) {
      isTRUE(trend_registry[[tt]][[field]])
    }, logical(1L)))
  }, logical(1L))
  paste(paste0(sort(trends[keep]), "()"), collapse = ", ")
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
#' Three things remove it. A deterministic trend is registered with
#' covariance pattern `"none"`. `PW()` draws its path from
#' changepoints. Its program declares no `sigma_trend`.
#' Multiplicative gamma process shrinkage derives the scale as
#' `sqrt(Psi_diag)`. A sampled `sigma_trend` would give each column a
#' second magnitude. The likelihood identifies only the product of the
#' two. Sampled factor loadings fix the factor scale at one, as
#' `samples_factor_loadings()` describes.
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
#' scale and zero correlation. `Z` then sets the covariance among
#' series. Fixed loadings pin the factors' scale. Those models keep a
#' sampled scale and correlation. An all-`NA` mask fixes nothing.
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
#' `Z` prior. A fully fixed one samples none. The prior table
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
#' A marginal prediction integrates over the covariance the latent
#' state settles at. Each trend obtains it in one of three ways.
#' `"lift"` raises the innovation covariance. `"omega"` takes what the
#' Stan model already derived. `"none"` keeps the innovation
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
#' has neither innovations nor a stationary covariance. A character
#' property asked of it is `"none"` and a logical one `FALSE`.
#'
#' @param trend_type A registered trend type name, or `"None"`
#' @param field The property's name
#' @return The registered value
#' @noRd
trend_property <- function(trend_type,
                           field = c("stationary_source",
                                     "covariance_pattern",
                                     "per_factor_coefficients",
                                     "shared_innovations",
                                     "supports_factors")) {
  checkmate::assert_string(trend_type, min.chars = 1)
  field <- match.arg(field)
  if (identical(trend_type, "None")) {
    return(if (field %in% c("stationary_source", "covariance_pattern")) {
      "none"
    } else {
      FALSE
    })
  }
  ensure_registry_initialized()
  get_trend_info(trend_type)[[field]]
}


#' The parameters a trend samples
#'
#' A `trend_map` and a loadings prior both join the specification
#' after the constructor builds it. Each changes which parameters the
#' program samples. The function computes the list from the
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

  trend_specific <- trend_method(
    trend_type, trend_method_templates[["monitor_params"]]
  )(trend_spec)

  # The correlation factor takes a prior. `Sigma_trend` is derived
  # from it and takes none.
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

  unique(c(
    base_params, trend_specific, correlation_params, factor_params,
    df_params, hierarchical_params
  ))
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
  # The hyperparameters of the stationary prior of Heaps (2023).
  # `sigma_trend` and `L_Omega_trend` come from the main list.
  c("Amu_trend", "Aomega_trend", ma_params_for(trend_spec, "VAR"))
}

#' Generate CAR-specific monitor parameters
#' @param trend_spec CAR trend specification
#' @return The damping `ar1_trend`
#' @noRd
generate_car_monitor_params <- function(trend_spec) {
  "ar1_trend"
}


#' LLT monitor parameters
#'
#' The slope scale, any damping and the free initial slope.
#' @param trend_spec LLT trend specification
#' @return Character vector of LLT-specific parameters
#' @noRd
generate_llt_monitor_params <- function(trend_spec) {
  c("sigma_slope_trend",
    if (isTRUE(trend_spec$damped)) "ar1_slope_trend", "init_slope_trend")
}


#' ZMVN monitor parameters
#'
#' The shared innovation system declares every ZMVN parameter.
#' @param trend_spec ZMVN trend specification
#' @return An empty character vector
#' @noRd
generate_zmvn_monitor_params <- function(trend_spec) {
  character(0)
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

#' The constructor name a trend specification records
#'
#' @param trend_spec A trend specification, a list of them, or NULL
#' @return The name every registry lookup takes, or `NA` for no
#'   specification
#' @noRd
get_trend_name <- function(trend_spec) {
  trend_spec_head(trend_spec)$trend %||% NA_character_
}

#' The growth form of a PW model
#'
#' A `PW()` specification names the field `growth`. A fit's
#' `trend_metadata` stores it as `pw_growth`, which post-fit callers
#' hold where the specification is out of reach. Prophet's default form
#' is linear.
#'
#' @param x A `PW()` specification, a fit's `trend_metadata`, or NULL
#' @return "linear" or "logistic"
#' @noRd
pw_growth <- function(x) {
  checkmate::assert_list(x, null.ok = TRUE)
  x$growth %||% x$pw_growth %||% "linear"
}

#' Does a PW model take the logistic form?
#'
#' The logistic form needs a carrying capacity. Its `m_trend` offset
#' places the curve along time. The observation formula supplies the
#' level of the linear form.
#'
#' @inheritParams pw_growth
#' @return Logical scalar
#' @noRd
pw_is_logistic <- function(x) {
  identical(pw_growth(x), "logistic")
}

#' How an AR trend shares its coefficients across series
#'
#' `AR()` records the mode on every specification it builds. The other
#' trends have no coefficients to share.
#'
#' @param spec A trend specification
#' @return "none", "shared" or "hierarchical"
#' @noRd
ar_coef_sharing <- function(spec) {
  checkmate::assert_list(spec)
  spec$coef_sharing %||% "none"
}

#' The carrying-capacity column of a logistic PW model
#'
#' `create_mvgam_trend()` records it on a `PW()` specification: the
#' name the user gave, or `"cap"`. The Stan data builder and the
#' forecast both take the name from here.
#'
#' @param spec A trend specification, or a list of them
#' @return Character scalar
#' @noRd
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
# The mvgam_trend object
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
#'   \item{ma}{`TRUE` for a moving-average term.}
#'   \item{cor}{`TRUE` for correlated innovations. A grouped trend
#'     always has them.}
#'   \item{validation_rules}{What the trend assumes about the data.
#'     `"requires_regular_intervals"` marks a trend that indexes its
#'     lags by position. Fitting it requires evenly spaced times.
#'     [CAR()] and [ZMVN()] carry no rule.}
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
#'   \item{damped}{`TRUE` for an [LLT()] whose slope is damped.}
#'   \item{cap}{The name of the carrying-capacity column that a
#'     logistic [PW()] trend uses, `"cap"` unless given.}
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
#' the model is built. A fully fixed `trend_map` samples no loadings.
#' A loadings prior with column shrinkage derives the innovation
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
# Trend formula parsing
# =============================================================================
# Turns a `trend_formula` into its trend constructor and the brms terms
# that build the trend's linear predictor.

#' Trend type registry for extensible dispatch
#'
#' The names of the registered trend types, which formula parsing and
#' validation recognise as constructors.
#'
#' @return Character vector of trend type names
#' @noRd
mvgam_trend_registry <- function() {
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
#' @return A list with three elements: `trend_model` (the evaluated
#'   constructor), `base_formula` (the formula without it) and
#'   `regular_terms` (the labels of the other terms)
#' @noRd
parse_trend_formula <- function(trend_formula) {
  checkmate::assert_class(trend_formula, "formula")
  ensure_registry_initialized()

  # `terms()` refuses what it cannot expand, such as a `.` with no
  # data. Its message names the problem.
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


#' Is a term label a call to a registered trend constructor?
#'
#' A covariate whose name ends in a constructor's name, such as
#' `lagAR(x)`, is a regular term.
#'
#' @param label One term label of a trend formula
#' @return Logical scalar
#' @noRd
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
#' Shows the trend label a fit would print, such as `AR({1, 12})`, with
#' the columns and settings the constructor holds. Settings left at
#' their defaults are omitted.
#'
#' @param x A mvgam_trend object
#' @param ... Ignored.
#'
#' @return The `mvgam_trend` object `x`, returned invisibly.
#'
#' @export
print.mvgam_trend <- function(x, ...) {
  label <- trend_metadata_label(list(
    trend_type = x$trend,
    ar_lags = derive_ar_lags(x),
    ma_lags = derive_ma_lags(x),
    growth = x$growth
  ))
  lines <- c(
    paste0("Type: ", label),
    paste0("Time variable: ", x$time),
    paste0("Series variable: ", x$series)
  )
  if (isTRUE(x$cor)) {
    lines <- c(lines, "Correlated innovations")
  }
  if (!is.null(x$df) && !identical(x$df, Inf)) {
    df <- if (is.na(x$df)) "estimated" else x$df
    lines <- c(lines, paste0("Student-t innovations, df: ", df))
  }
  if (!is.null(x$n_lv)) {
    lines <- c(lines, paste0("Dynamic factors: ", x$n_lv))
  }
  if (!is.null(x$trend_map)) {
    lines <- c(lines, "Loadings fixed by 'trend_map'")
  }
  if (named_var(x$gr)) {
    lines <- c(lines, paste0("Hierarchical grouping: ", x$gr))
  }
  if (named_var(x$subgr)) {
    lines <- c(lines, paste0("Subgroups: ", x$subgr))
  }
  if (!identical(ar_coef_sharing(x), "none")) {
    lines <- c(lines, paste0("AR coefficients: ", x$coef_sharing))
  }
  if (isTRUE(x$damped)) {
    lines <- c(lines, "Damped slope")
  }
  if (identical(x$trend, "PW")) {
    lines <- c(lines, paste0(
      "Changepoints: ", x$n_changepoints, " in the first ",
      100 * x$changepoint_range, "% of times, scale ",
      x$changepoint_scale
    ))
    if (pw_is_logistic(x)) {
      lines <- c(lines, paste0("Carrying capacity variable: ", x$cap))
    }
  }
  cat("mvgam trend specification:\n")
  cat(paste0("  ", lines), sep = "\n")
  invisible(x)
}

# =============================================================================
# Trend constructors
# =============================================================================
# The trend constructors are the user-facing API for trend
# specifications. Each validates its arguments, sets defaults and
# returns the trend object that Stan generation takes.

#' Trend Model Constructors for \pkg{mvgam}
#'
#' Specify trend models for multivariate State-Space models in \pkg{mvgam}.
#' These constructor functions create trend specifications for various temporal
#' dynamics including random walks (RW), autoregressive models (AR, VAR, CAR),
#' local linear trends (LLT) and piecewise trends (PW). A constructor
#' records its arguments. The model evaluates them against the data.
#'
#' @param df Degrees of freedom for the latent process innovations.
#'   Defaults to \code{Inf}, which gives Gaussian innovations: a t with
#'   infinite degrees of freedom is a normal. Set \code{df = NA} to
#'   estimate them, or supply a number above \code{2} to fix them.
#'
#'   Finite degrees of freedom let the latent process absorb an
#'   occasional large shock while \code{sigma_trend} stays at the scale
#'   of ordinary variation. That suits boom-and-bust population series
#'   and outbreak dynamics. The innovations follow a multivariate t.
#'   Because it shares one innovation \emph{scale} across the series at
#'   each time point, large innovations tend to occur together. Each
#'   series keeps its own direction and magnitude. A shock in one series
#'   says nothing about the sign of the others. One series can also stay
#'   unmoved by an event that moves its neighbours. \code{cor} sets the
#'   correlation between series exactly as it does for Gaussian
#'   innovations. The degrees of freedom set how heavy the tails of the
#'   innovations are.
#'
#'   When \code{df = NA} the degrees of freedom are estimated as
#'   \code{nu_trend}, with a default \code{gamma(4, 0.3)} prior. The
#'   \code{prior} argument replaces it, for example
#'   \code{prior(gamma(2, 0.1), class = "nu_trend")}.
#'   \code{\link{get_prior}} lists \code{nu_trend} with the other trend
#'   classes.
#'
#'   Three caveats apply. The bound of \code{2} is required. Below it
#'   the innovations have no finite variance. An autoregressive trend
#'   then has no stationary covariance to start from. Above it, the
#'   first states of an autoregressive trend take the stationary mean
#'   and covariance. Their distribution matches the stationary law
#'   exactly under Gaussian innovations alone. A finite mix of t
#'   variates has heavier tails than the infinite sum that defines that
#'   law. The degrees of freedom are informed only through the tail of
#'   the latent process. They are weakly identified in short series:
#'   below roughly 200 time points, the posterior stays close to its
#'   prior. \code{VAR()} samples its states directly and \code{PW()}
#'   has no innovations. Neither takes \code{df}.
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
#'
#' @param time The unquoted name of the `numeric` or `integer` variable in
#'   `data` that holds each row's time. Defaults to `time`.
#'
#' @param series The unquoted name of the `factor` variable in `data` that
#'   names each row's series. Its levels fix the order of the series. A
#'   trend needs this column even for a single series. Defaults to `series`.
#'
#' @details
#' A `trend_formula` takes one trend constructor. The arguments of that
#' constructor express richer dynamics:
#' \itemize{
#'   \item For seasonal patterns: `AR(p = c(1, 12))`. A sum such as
#'     `RW() + AR(p = 12)` is refused
#'   \item For several time scales: `AR(p = c(1, 7, 30))` for daily,
#'     weekly and monthly lags
#'   \item For cross-series dynamics: `VAR(p = 2)`
#' }
#'
#' **Stationarity of `AR(p)`**: a consecutive lag set with
#' \code{p >= 2} is sampled as partial autocorrelations,
#' \code{ar1_pacf_trend} through \code{ark_pacf_trend}, each
#' declared on \code{(-1, 1)}. The Levinson-Durbin recursion derives
#' \code{ar1_trend} through \code{ark_trend} from them. Every
#' summary, forecast and hypothesis test uses those coefficients.
#' Any vector of partial autocorrelations in \code{(-1, 1)} gives a
#' stationary process. Bounding the coefficients directly admits
#' explosive draws. With \code{ar1_trend} and \code{ar2_trend} both
#' at 0.9 the declared bounds hold, yet that process grows without
#' limit. Priors for these models name the \code{ar{k}_pacf_trend}
#' classes.
#'
#' \code{AR(p = 1)} samples \code{ar1_trend} directly. At one lag
#' the declared interval is already the stationary region. A sparse
#' lag set such as \code{AR(p = c(1, 12))} also samples
#' \code{ar{k}_trend} directly. It fixes its intermediate
#' coefficients at zero, a constraint the recursion cannot express. It
#' carries no stationarity guarantee. Both take priors on the
#' \code{ar{k}_trend} classes.
#'
#' \code{CAR()} steps a continuous-time AR(1) over the gaps the data
#' records. A gap \code{dt} is measured in units of the median gap
#' between two consecutive observations of one series.
#' \code{ar1_trend} and \code{sigma_trend} then describe a typical
#' step. The model is the same whatever unit the time column records.
#' Across a gap \code{dt} the decay is
#' \code{ar1_trend^dt} and the innovation standard deviation is
#' \code{sigma_trend * sqrt((1 - ar1_trend^(2 dt)) /
#' (1 - ar1_trend^2))}. The decay and the innovation scale together.
#' The marginal variance is then
#' \code{sigma_trend^2 / (1 - ar1_trend^2)} at every occasion of an
#' irregular grid. The first state comes from that marginal.
#'
#' \code{CAR(cor = TRUE)} correlates the innovations of several
#' series. \code{Sigma_trend} is their covariance over a gap of one.
#' Series \code{a} and \code{b} have stationary covariance
#' \code{Sigma_trend[a, b] / (1 - ar1_trend[a] * ar1_trend[b])}. The
#' innovations across a gap \code{dt} have that covariance times
#' \code{1 - (ar1_trend[a] * ar1_trend[b])^dt}. This is the exact
#' transition of the continuous-time process. A time point added
#' between two others leaves the model for the remaining times
#' unchanged. \code{L_Omega_trend} is the Cholesky factor of the
#' correlation between the series' shocks over an instant. Its prior
#' is placed there. The correlation of \code{Sigma_trend} is
#' closer to zero than that correlation when the series damp at
#' different rates. On a regular grid the likelihood is that of
#' \code{AR(p = 1, cor = TRUE)} with positive coefficients.
#'
#' The series of a \code{CAR()} trend may be observed at their own
#' times. The trend has a state for every series at each time any
#' series was observed. A series contributes to the likelihood at the
#' times it has a row. This is the model a frame gives when every
#' unobserved time of a series has a row with a missing response.
#' \code{forecast()} takes each series at its own future times.
#' \code{df} needs the series on shared times. With one mixing scale
#' per time, a multivariate t would let the times of one series change
#' the trend of another.
#'
#' @note **VAR fits and `init = 0`**: the stationary map of Heaps
#'   (2023) takes matrix square roots by eigendecomposition. The
#'   gradient of an eigendecomposition divides by differences between
#'   eigenvalues. `init = 0` in the call to [mvgam()] sets every raw
#'   coefficient to zero and every innovation scale to one. Each matrix
#'   the map decomposes is then the identity, with repeated eigenvalues,
#'   and the first gradient is not finite. Leave `init` at its default
#'   random draws for `VAR()` fits. `AR()`, `RW()`, `CAR()` and `ZMVN()`
#'   are unaffected.
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
#'   trend. An explicit `cor = FALSE` is refused.
#'
#'   Within a `VAR()` it sets up a hierarchical panel vector
#'   autoregression. Each group learns its autoregressive, moving-average
#'   and correlation matrices from shared hyperparameters. The groups do
#'   not interact. Without `subgr`, the series column names the units
#'   within each group.
#'
#'   Two constraints apply to hierarchical trend models:
#'   \itemize{
#'     \item `gr` must be constant within each series (each series belongs to
#'       a single group). Models with `gr` varying within a series are
#'       rejected at validation time.
#'     \item Every group must hold the same number of series. The Stan
#'       program sizes each group's correlation block from one shared
#'       count. Validation refuses unbalanced groups before fitting.
#'   }
#'
#' @param coef_sharing How the coefficients of `AR()` vary across the
#'   latent series. `"none"` (the default) estimates each series'
#'   coefficients independently. `"shared"` estimates one coefficient
#'   per lag for every series. `"hierarchical"` pools each series'
#'   coefficient toward a mean for its lag, as
#'   `ar{lag}_trend[j] ~ normal(mu_ar{lag}_trend, sigma_ar{lag}_trend)`.
#'   The section on AR coefficient sharing lists the parameters each
#'   setting samples.
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
#'   A region `r1` and a species `sp1` give the series `"r1_sp1"`. The
#'   levels sort lexically. `summary()`, `plot()` and `forecast()`
#'   all label that series `"r1_sp1"`. A `series` column that disagrees
#'   with the derived series is replaced by it, with a warning.
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}.
#'
#' @section AR coefficient sharing:
#' Each `coef_sharing` setting samples its own Stan parameters:
#' \tabular{ll}{
#'   \strong{coef_sharing} \tab \strong{Stan parameters with priors} \cr
#'   `"none"`         \tab `ar{lag}{stem}_trend` per series \cr
#'   `"shared"`       \tab `shared_ar{lag}{stem}_trend` (one per lag) \cr
#'   `"hierarchical"` \tab `mu_ar{lag}{stem}_trend`,
#'     `sigma_ar{lag}{stem}_trend` \cr
#' }
#' `{stem}` is `_pacf` for a contiguous lag set of two or more
#' lags, where the sampled quantity is a partial autocorrelation,
#' and empty for `p = 1` and for a sparse lag set such as
#' `p = c(1, 3)`. `AR(p = 2)` samples `ar1_pacf_trend` and
#' `AR(p = 1)` samples `ar1_trend`.
#' Every setting defines the same `ar{lag}_trend[j]` in
#' `transformed parameters`. Forecasting, IRF, FEVD and summaries use
#' that quantity whatever the setting. A prior on any sampled
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
#' @aliases trend_constructors
#'
#' @seealso [get_prior()] lists the parameters a trend adds and their
#'   default priors. [residual_cor()], [irf()], [fevd()] and
#'   [stability()] interpret fitted latent dynamics. [forecast()]
#'   propagates them forward. The
#'   `r mvgam_article("mvgam_overview", "overview vignette")` compares
#'   the trend types. The `r mvgam_article("var", "VAR article")`
#'   interprets a fitted `VAR()` with `irf()`, `fevd()` and
#'   `stability()`. The
#'   `r mvgam_article("hierarchical_var", "hierarchical VAR article")`
#'   fits `gr` and `subgr`. The
#'   `r mvgam_article("dfm", "dynamic factor vignette")` fits `n_lv` and
#'   `trend_map`.
#'
#' @section Parameter Naming Convention:
#' Every trend parameter carries a `_trend` suffix, which keeps it
#' distinct from an observation model parameter of the same name. The
#' innovation scale is \code{sigma_trend}, the moving-average
#' coefficient \code{theta1_trend}, the autoregressive coefficients
#' \code{ar1_trend} to \code{ark_trend} and the VAR coefficient
#' matrices \code{Phi_trend}. Priors take these class names, as in
#' \code{prior(normal(0, 1), class = sigma_trend)}. Parameter
#' extraction takes the same names.
#'
#' @section Identification:
#' A factor model with sampled loadings fits equally well under any
#' reordering or sign change of its factors. Some models also fit
#' equally well under any rotation. mvgam reports one version, chosen
#' by the dynamics of the factors:
#' \itemize{
#'   \item \code{AR()} factors take their own coefficients, as do
#'     \code{RW()} and \code{AR()} factors with \code{ma = TRUE}. A
#'     rotation would mix those coefficients. After sampling, the
#'     factors of every draw are reordered and signed to agree with one
#'     reference. The loadings `Z`, the factor paths `lv_trend` and the
#'     coefficients `ar1_trend` or `theta1_trend` move together.
#'     Factors are numbered by the variance they explain, largest
#'     first.
#'   \item \code{RW()}, \code{VAR()} and \code{ZMVN()} factors
#'     without a moving-average term are unchanged by rotation. So are
#'     \code{AR()} factors with \code{coef_sharing = "shared"} and no
#'     moving-average term.
#'     Following Heaps & Jermyn (2024), generated quantities rotate each
#'     draw to lower-triangular loadings `Z_tilde` with a non-negative
#'     diagonal. The factor paths rotate to `lv_trend_tilde`. A VAR
#'     factor model rotates its coefficients to `Phi_trend_tilde`.
#' }
#' Post-fit methods use the identified loadings and factor paths.
#' `shared_variation()` and `residual_cor()` are the same under every
#' version.
#'
#' A \code{trend_map} keeps its fixed entries on `Z` exactly. A
#' `by = lv_axis()` smooth ties each factor to its own covariate
#' effect. Neither takes a rotation. For a model with every loading
#' sampled, the \code{loadings_prior}
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
#' # A constructor records a trend before any data are seen. Printing
#' # one shows the settings the model will use
#' AR(p = c(1, 12))
#' VAR(p = 2, ma = TRUE)
#' RW(gr = region, subgr = species)
#'
#' # Three Poisson series, each surveyed at its own irregular times.
#' # Every series keeps about 60 percent of the occasions
#' set.seed(11)
#' simdat <- sim_mvgam(type = 6L, family = poisson(), n_series = 3L,
#'                     n_timepoints = 80L)
#' own_times <- simdat$data_train
#' own_times <- own_times[runif(nrow(own_times)) < 0.6, ]
#'
#' # The parameters a trend adds, with their default priors, before
#' # any fitting
#' get_prior(
#'   mvgam_formula(y ~ s(season, bs = "cc", k = 6),
#'                 trend_formula = ~ CAR(cor = TRUE)),
#'   data   = own_times,
#'   family = poisson()
#' )
#'
#' # A continuous-time AR(1) steps each series over its own gaps
#' mod <- mvgam(
#'   y ~ s(season, bs = "cc", k = 6),
#'   trend_formula = ~ CAR(cor = TRUE),
#'   data          = own_times,
#'   family        = poisson(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod)
#'
#' # Correlations among the series' latent states
#' residual_cor(mod)
#'
#' # Each series is forecast at its own future times
#' plot(forecast(mod, newdata = simdat$data_test), series = 2)
#'
#' # Next: hindcast() returns the fitted latent trends. The articles
#' # under 'See Also' fit VAR, hierarchical and factor trends
#' }
#'
#' @family trend models
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
    "CAR",
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
      "Sparse-lag VAR (vector 'p') is not supported.",
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

#' Specify local linear trends in \pkg{mvgam} models
#'
#' A latent level that moves with a latent growth rate, the local
#' linear trend of Harvey (1989). Each latent process carries a level
#' and a slope:
#'
#' \deqn{\mu_t = \mu_{t-1} + \beta_{t-1} + \eta_t}
#' \deqn{\beta_t = \phi \beta_{t-1} + \zeta_t}
#'
#' The level shocks \eqn{\eta_t} have scale `sigma_trend` and the slope
#' shocks \eqn{\zeta_t} scale `sigma_slope_trend`. A small
#' `sigma_slope_trend` gives a trend that keeps its direction. A zero
#' scale gives a random walk with a fixed drift. `damped = TRUE`
#' estimates \eqn{\phi} on (0, 1) as `ar1_slope_trend`, which pulls
#' the growth rate back toward zero over the horizon (Gardner and
#' McKenzie 1985). The undamped trend has \eqn{\phi = 1}. Its
#' forecasts extend the current growth rate.
#'
#' @inheritParams RW
#' @param damped Logical. Estimate a damping coefficient for the
#'   slope? Default `FALSE`.
#'
#' @details
#' The level shocks take the innovation structure of `RW()`: `cor`
#' correlates them across series, `gr` and `subgr` pool their
#' correlations hierarchically and `df` gives them a multivariate t.
#' The slope shocks are Gaussian and take the level's correlation
#' matrix with scales of their own.
#'
#' The slope starts at a free value, `init_slope_trend`, damped or not,
#' as in the damped trends of exponential smoothing. Damping serves a
#' series that starts with fast growth and slows. The initial slope of
#' such a series can lie far outside the slope's stationary
#' distribution. The level starts at its first shock. The observation
#' intercept carries its mean.
#'
#' Default priors scale with the number of time points \eqn{N} and
#' widen by the factor \eqn{s} that brms applies to its own default
#' scales, as [get_prior()] describes. `sigma_slope_trend` takes a
#' half-Student-t with 3 degrees of freedom. Under its scale, the
#' trend's movement over the series from the slope shocks has a
#' standard deviation with prior median \eqn{s}. The movement itself
#' has a 90th percentile near \eqn{2.8 s}. The scale accounts for the damping at
#' the median of its prior. `init_slope_trend ~ normal(0, 2 / r)`,
#' where \eqn{r = 1 + \phi + \dots + \phi^{N - 2}} is how far a unit
#' initial slope moves the level by the end of the series, gives the
#' initial slope \eqn{2 s} of growth over the series at one prior
#' standard deviation. An undamped
#' slope has \eqn{r = N - 1}. A factor model with every loading sampled
#' measures both in units of its level's spread over the series,
#' \eqn{\sqrt{N}}. Its loadings carry \eqn{s}. `ar1_slope_trend` takes
#' `beta(18, 2)`, with median 0.91. `get_prior()` prints the resolved
#' values.
#'
#' A smooth or linear function of time in either formula competes with
#' the slope for the same variation. `mvgam()` warns when the time
#' variable appears in one. The slope variance is weakly identified on
#' short series: `LLT()` warns below 20 time points and refuses fewer
#' than 5. On informative data, such as large counts, a chain started
#' at random can stall far from the posterior. `init = "pathfinder"`
#' in [mvgam()] starts every chain from Stan's Pathfinder
#' approximation.
#'
#' In a factor model with every loading sampled, the loadings set
#' each factor's scale and the level shocks take unit scale. The
#' factors share one `sigma_slope_trend` and one `ar1_slope_trend`.
#' The model is then unchanged under a rotation of the factors. The QR
#' rotation that `RW()` factors take identifies the loadings. A trend
#' with no level noise would need an infinite slope scale. A factor
#' model always keeps some level noise. A
#' `trend_map` that fixes loadings keeps one slope scale per factor.
#'
#' @return An object of class `mvgam_trend`.
#'
#' @references
#' Harvey, A. C. (1989). \emph{Forecasting, Structural Time Series
#' Models and the Kalman Filter}. Cambridge University Press.
#'
#' Gardner, E. S. and McKenzie, E. (1985). Forecasting trends in time
#' series. \emph{Management Science}, 31:1237-1246.
#'
#' @seealso [RW()], [PW()], [trend_constructors], [get_prior()] for the
#'   scale of the default priors, [forecast.mvgam()]
#'
#' @examples
#' \dontrun{
#' # A series that grows, then levels off: its slope shrinks by a
#' # factor of 0.9 at each step
#' set.seed(4)
#' n_t <- 80
#' slope <- 0.1 * 0.9^(0:(n_t - 1))
#' dat <- data.frame(
#'   time = 1:n_t,
#'   series = factor("s1"),
#'   y = rpois(n_t, exp(1 + cumsum(slope) + cumsum(rnorm(n_t, 0, 0.03))))
#' )
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ LLT(damped = TRUE),
#'   data = dat[1:60, ],
#'   family = poisson(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # The posterior for `ar1_slope_trend` recovers the damping of 0.9
#' summary(mod)
#'
#' # The damped slope levels the forecast off
#' fc <- forecast(mod, newdata = dat[61:80, ])
#' plot(fc)
#' }
#'
#' @family trend models
#' @export
LLT <- function(time = NA, series = NA, damped = FALSE, cor = NULL,
                gr = NA, subgr = NA, n_lv = NULL, trend_map = NULL,
                df = Inf) {
  checkmate::assert_flag(damped)
  assert_trend_map_input(trend_map)

  create_mvgam_trend(
    "LLT",
    df = assert_trend_df(df),
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    damped = damped,
    cor = cor,
    n_lv = n_lv,
    trend_map = trend_map
  )
}

#' Specify piecewise linear or logistic trends in \pkg{mvgam} models
#'
#' Sets up a piecewise linear or logistic trend in \code{mvgam}. The
#' constructor records its arguments. The model evaluates them against
#' the data.
#'
#' @param cap The unquoted name of the variable in `data` that holds
#'   the carrying capacity of a logistic model, on the response scale.
#'   It is numeric and can vary by time and series. Defaults to `cap`. A
#'   logistic model whose data lack the column is refused.
#'
#' @param n_changepoints A non-negative integer specifying the number of
#'   potential changepoints. Potential changepoints are selected uniformly from
#'   the first `changepoint_range` proportion of timepoints in \code{data}.
#'   Default is `10`.
#'
#' @param changepoint_range Proportion of history in \code{data} in which trend
#'   changepoints will be estimated. Defaults to `0.8` for the first 80%.
#'
#' @param changepoint_scale A positive number: the typical change in
#'   growth at one changepoint over the whole series, as in Prophet, in
#'   link units for linear growth and in logits for logistic growth.
#'   For a series of `N` time points the prior on each per-step rate
#'   change is `double_exponential(0, s * changepoint_scale / N)`,
#'   where `s` is the factor [get_prior()] describes for linear growth
#'   and 1 for logistic growth. Larger values give a more flexible
#'   trend. The default of `0.3` gives the ten default changepoints a
#'   prior median movement of the level of about half a unit over the
#'   series, the movement the
#'   [LLT()] slope prior allows. Prophet's own default of `0.05` holds a
#'   link-scale trend close to a straight line.
#'
#' @param growth Character string, `'linear'` (the default) or
#'   `'logistic'`. Logistic growth saturates the trend at the carrying
#'   capacity in the `cap` column.
#'
#' @inheritParams AR
#'
#' @author Nicholas J Clark
#'
#' @references
#' Taylor, S. J. and Letham, B. (2018). Forecasting at scale.
#' \emph{The American Statistician}, 72:37-45.
#'
#' @seealso [LLT()] for a trend whose growth changes continuously,
#'   [get_prior()] for the scale of the default priors
#'
#' @return An object of class `mvgam_trend`.
#'
#' @details
#' *Offsets and intercepts*:
#' A linear piecewise trend takes its level from the observation
#' formula, as every other trend model in \pkg{mvgam} does. `y ~ 1` is
#' the ordinary observation formula for it.
#'
#' A logistic piecewise trend estimates an offset (`m_trend`) that
#' positions its curve along the time axis. It contributes
#' `cap * inv_logit(...)` on the link scale, which saturates the
#' expectation at the `cap` you supply when the observation formula
#' omits its intercept (`y ~ -1` or `y ~ 0 + ...`). An intercept
#' there raises the ceiling above `cap`.
#'
#' *Priors*:
#' Time runs in steps `1, ..., N`. The default priors are set on the
#' span of the series. The base growth rate takes
#' `k_trend ~ normal(0, 2 s / (N - 1))`, two units of growth over the
#' series at one standard deviation, where \eqn{s} is the factor
#' [get_prior()] describes for linear growth and 1 for logistic
#' growth. The logistic inflection takes
#' `m_trend ~ normal((N + 1) / 2, N / 2)`, centred on the middle of the
#' series. [get_prior()] shows the values for a given model.
#'
#' *Logistic growth and the cap variable*:
#' Many growing series approach a ceiling, such as a total market size,
#' a population size or an ecological carrying capacity.
#' `growth = 'logistic'` saturates the trend at a capacity that `data`
#' supplies in its `cap` column. The capacity can vary by series and by
#' time. Every observation needs a `cap` value.
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
#' @family trend models
#' @export
PW <- function(time = NA, series = NA, cap = NA, n_changepoints = 10,
              changepoint_range = 0.8, changepoint_scale = 0.3,
              growth = 'linear', n_lv = NULL, trend_map = NULL) {
  # Validate arguments
  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  checkmate::assert_number(changepoint_range, lower = 0, upper = 1)
  checkmate::assert_int(n_changepoints, lower = 1)
  if (!checkmate::test_number(changepoint_scale, finite = TRUE) ||
      changepoint_scale <= 0) {
    stop(insight::format_error(
      "'changepoint_scale' must be a positive number."
    ), call. = FALSE)
  }

  # A factor request written on the constructor is refused here.
  # `CAR()` uses the same helper.
  refuse_constructor_factor_request(n_lv, trend_map, "PW")

  # A logistic PW needs a carrying capacity. The constructor runs
  # while the trend formula is parsed, before the data is in scope.
  # `build_pw_cap_matrix()` checks the data for the `cap` column
  # instead. Its refusal names the missing column.
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
#' \pkg{mvgam}. The constructor records its arguments. The model
#' evaluates them against the data.
#'
#' @inheritParams AR
#' @param time The unquoted name of the `numeric` or `integer` variable
#'   in `data` that indexes the units whose latent residuals are
#'   correlated. Defaults to `time`, as for the other trends. The units
#'   can be sites, plots or any other replicate.
#'
#'   ZMVN takes these values at any spacing. The likelihood is
#'   \eqn{x_{u, :} \sim MVN(0, \Sigma)} with \eqn{\Sigma} indexed by
#'   series alone. The gap between two units enters nowhere. Use ZMVN
#'   for spatial or otherwise non-temporal units, where gaps are
#'   natural: dropped sites, stratified k-fold refits or irregular
#'   sampling grids.
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
#'   `subgr`. `cor` defaults to `TRUE` for a grouped trend. An explicit
#'   `cor = FALSE` is refused.
#'
#' @param subgr A subgrouping `factor` in `data` naming the
#'   observational units within each level of `gr`. Defaults to
#'   `series`. A model with hierarchical correlations builds its series
#'   from `gr` and `subgr`. Its `data` then needs no `series` column.
#'
#'   For example, to model counts of several species (`species` in
#'   `data`) at sampling sites (`site`) in three regions (`region`)
#'   with residuals correlated within regions, specify `time = site`,
#'   `gr = region` and `subgr = species`. `mvgam()` orders the data by
#'   `site` and builds the series as
#'
#'   `series = interaction(gr, subgr, drop = TRUE, sep = "_",`
#'   `lex.order = TRUE)`
#'
#'   A region `r1` and a species `sp1` give the series `"r1_sp1"`. A
#'   `series` column that disagrees with the derived series is replaced
#'   by it, with a warning.
#'
#' @return An object of class `mvgam_trend`.
#'
#' @inheritSection RW Identification
#'
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' @seealso [jsdgam()] fits `ZMVN()` factors across species.
#'   [residual_cor()] and [ordinate()] interpret the fitted
#'   correlations. The
#'   `r mvgam_article("dfm", "dynamic factor vignette")` builds factor
#'   models with a partial `trend_map`. The
#'   `r mvgam_article("idm", "integrated distribution model article")`
#'   shares one latent process across several observation families.
#'
#' @examples
#' \dontrun{
#' # Simulate counts of four series whose latent effects are correlated
#' # at each time point but independent over time.
#' set.seed(2)
#' simdat <- sim_mvgam(
#'   family       = poisson(),
#'   n_series     = 4L,
#'   n_timepoints = 120L,
#'   trend_model  = ZMVN(),
#'   prop_trend   = 0.9
#' )
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ ZMVN(),
#'   data          = simdat$data_train,
#'   family        = poisson(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod)
#'
#' # The per-series latent SDs
#' mcmc_plot(mod, variable = "sigma_trend", regex = TRUE,
#'           type = "intervals")
#'
#' # The correlations among the series that ZMVN() estimates
#' residual_cor(mod)
#' }
#'
#' @family trend models
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
# Trend constructor helpers
# =============================================================================
# The defaults and checks every trend constructor shares.

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
    # An unset constructor argument substitutes to its default, `NA`.
    # A column the user names `gr` or `subgr` is a real column.
    if (is.null(x)) return("NA")
    if (is.character(x)) return(x)
    deparsed <- deparse0(x)
    # Remove surrounding quotes if present (from quoted strings)
    if (grepl('^".*"$', deparsed)) {
      return(substr(deparsed, 2, nchar(deparsed) - 1))
    }
    deparsed
  }

  time_var <- process_arg(.time)
  series_var <- process_arg(.series)
  gr_var <- process_arg(.gr)
  subgr_var <- process_arg(.subgr)
  if (time_var == "NA") time_var <- "time"
  if (series_var == "NA") series_var <- "series"

  trend_obj <- list(
    trend = trend_type,
    time = time_var,
    series = series_var,
    gr = gr_var,
    subgr = subgr_var,
    ...
  )
  # A carrying capacity belongs to the constructor that names one
  if (!is.null(.cap)) {
    cap_var <- process_arg(.cap)
    trend_obj$cap <- if (cap_var == "NA") "cap" else cap_var
  }

  # The arguments the constructors share, checked once for all of
  # them
  checkmate::assert_int(trend_obj$n_lv, lower = 1, null.ok = TRUE,
                        .var.name = "n_lv")
  checkmate::assert_flag(trend_obj$ma, null.ok = TRUE, .var.name = "ma")
  checkmate::assert_flag(trend_obj$cor, null.ok = TRUE, .var.name = "cor")

  # A grouping estimates correlations among the `subgr` units within
  # each level of `gr`. The generated Stan program declares the group
  # correlation parameters whenever `gr` is named, for either value of
  # `cor`. `cor` resolves to TRUE for a grouped trend. An explicit
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
# the innovations have a finite second moment. A t has one only above
# 2 degrees of freedom.
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
