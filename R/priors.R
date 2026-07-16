#' Prior Specification and Inspection System for mvgam
#'
#' @description
#' Complete prior extraction, combination, and inspection system for mvgam
#' models. This file provides functions for working with priors in both
#' observation and trend components, leveraging the native brms brmsprior
#' class throughout.
#'
#' @section Architecture:
#' The prior system uses brmsprior objects directly for maximum compatibility:
#' - **Extraction Layer**: Get priors from observation and trend models
#' - **Combination Layer**: Merge observation and trend priors seamlessly
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
# WHY: Shared prior specifications enable DRY principle for parameters used
# across multiple trend types (e.g., sigma_trend used by RW, AR, CAR trends).
# Centralized definitions ensure consistency and easier maintenance.

#' Common Prior Specifications for Trend Parameters
#'
#' @description
#' Shared prior specifications for trend parameters that are used across
#' multiple trend types. This ensures consistent defaults and reduces
#' duplication in trend registrations. Each specification contains default
#' Stan distribution strings, parameter bounds, descriptions, and dimension
#' information.
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
#'   \item{LV}{Latent variables (all trends with state-space structure)}
#'   \item{ar1_trend}{AR(1) coefficient (AR, CAR trends)}
#'   \item{Z}{Factor loadings matrix for factor models}
#' }
#'
#' @seealso \code{\link{register_trend_type}} for using prior specifications
#' @noRd
common_trend_priors <- list(
  sigma_trend = list(
    default = "exponential(2)",
    bounds = c(0, NA),
    description = "Innovation standard deviation",
    dimension = "vector"
  ),

  LV = list(
    default = "std_normal()",
    bounds = c(NA, NA),
    description = "Latent variables",
    dimension = "matrix"
  ),

  ar1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1),
    description = "AR(1) coefficient",
    dimension = "vector"
  ),

  alpha_cor_trend = list(
    default = "beta(3, 2)",
    bounds = c(0, 1),
    description = "Hierarchical correlation mixing parameter",
    dimension = "scalar"
  ),

  Z = list(
    default = "std_normal()",
    bounds = c(NA, NA),
    description = "Factor loadings matrix",
    dimension = "matrix"
  ),

  theta1_trend = list(
    default = "normal(0, 0.5)",
    bounds = c(-1, 1),
    description = "MA(1) coefficient",
    dimension = "vector"
  )
)

#' Extract Trend Model Priors
#'
#' @param trend_formula Trend formula specification
#' @param data Data frame
#' @param response_names Character vector of response variable names
#' @return A brmsprior object with trend model priors
#' @noRd
extract_trend_priors <- function(trend_formula, data, response_names = NULL, .precomputed_dimensions = NULL) {
  if (!is.null(trend_formula)) {
    checkmate::assert_formula(trend_formula)
  }
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(response_names)) {
    checkmate::assert_character(response_names, min.len = 1)
  }
  if (!is.null(.precomputed_dimensions)) {
    checkmate::assert_list(.precomputed_dimensions, names = "named")
  }

  if (is.null(trend_formula)) {
    # No trend model - return empty brmsprior with canonical schema
    empty_prior <- create_empty_brmsprior()
    return(empty_prior)
  }

  # Parse trend formula to determine trend type
  trend_spec <- parse_trend_formula(trend_formula, data,
                                   response_vars = response_names,
                                   .precomputed_dimensions = .precomputed_dimensions)

  # Generate priors based on trend type using convention-based dispatch
  # Pass data through for base formula prior extraction
  trend_priors <- generate_trend_priors(trend_spec, data, response_names)

  return(trend_priors)
}

# NOTE: parse_trend_formula() has been moved to R/trend_system.R as part of the
# complete mvgam trend parsing system. The function provides ZMVN defaults and
# complete mvgam_trend objects with monitor_params metadata.

#' Generate Trend Priors from Monitor Parameters
#'
#' @param trend_spec Trend specification from parse_trend_formula
#' @param response_names Character vector of response names for
#'   multivariate models
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors <- function(trend_spec, data, response_names = NULL) {
  # Comprehensive parameter validation per code reviewer requirements
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_data_frame(data, min.rows = 1)
  if (!is.null(response_names)) {
    checkmate::assert_character(response_names, min.len = 1)
  }

  # Validate trend_spec structure before accessing components
  if (!"trend_model" %in% names(trend_spec)) {
    stop(insight::format_error(c(
      cli::format_inline("Invalid {.field trend_spec} structure."),
      x = cli::format_inline(
        "Expected component {.field trend_model} not found."
      )
    )))
  }

  if (!"base_formula" %in% names(trend_spec)) {
    stop(insight::format_error(c(
      cli::format_inline("Invalid {.field trend_spec} structure."),
      x = cli::format_inline(
        "Expected component {.field base_formula} not found."
      )
    )))
  }

  # Validate component types
  if (!is.null(trend_spec$trend_model)) {
    checkmate::assert_class(trend_spec$trend_model, "mvgam_trend")
  }
  if (!is.null(trend_spec$base_formula)) {
    checkmate::assert_class(trend_spec$base_formula, "formula")
  }

  # Initialize list to collect different prior sources
  prior_list <- list()

  # Extract components from validated trend_spec
  trend_model <- trend_spec$trend_model
  base_formula <- trend_spec$base_formula

  # 1. Get trend constructor priors (AR, RW, etc.)
  if (inherits(trend_model, "mvgam_trend")) {
    prior_list$constructor <- generate_trend_priors_from_monitor_params(trend_model)
  }

  # 2. Get base formula priors using existing mvgam infrastructure
  # Use setup_brms_lightweight which handles fake response variables automatically
  if (!is.null(base_formula) && inherits(base_formula, "formula")) {
    # Extract priors for all formulas except no-intercept (~0)
    # This includes intercept-only (~1) which should generate Intercept_trend
    if (!all.equal(base_formula, ~ 0, check.attributes = FALSE) == TRUE) {

      # Call verified setup_brms_lightweight function
      trend_setup <- setup_brms_lightweight(
        formula = base_formula,
        data = data,
        family = gaussian()
      )

      # Validate that setup returned expected structure
      if (!is.list(trend_setup)) {
        stop(insight::format_error(c(
          "setup_brms_lightweight returned unexpected structure.",
          x = "Expected list object with prior component."
        )))
      }

      if (!"prior" %in% names(trend_setup)) {
        stop(insight::format_error(c(
          cli::format_inline(
            "setup_brms_lightweight missing expected {.field prior} component."
          ),
          x = "Cannot extract base formula priors."
        )))
      }

      # Extract and validate prior structure
      base_priors <- trend_setup$prior
      if (!inherits(base_priors, c("brmsprior", "data.frame"))) {
        stop(insight::format_error(c(
          "Invalid prior structure from setup_brms_lightweight.",
          x = "Expected brmsprior data frame."
        )))
      }

      # Add _trend suffix to distinguish from observation priors
      if (nrow(base_priors) > 0) {
        # Validate expected columns exist
        if (!"class" %in% names(base_priors)) {
          stop(insight::format_error(c(
            "Invalid base_priors structure.",
            x = cli::format_inline(
              "Missing required {.field class} column."
            )
          )))
        }

        # Add _trend suffix to all classes except sigma (which conflicts with trend constructor)
        # and exclude sigma entirely to prevent conflicts
        mask <- base_priors$class != "" & base_priors$class != "sigma"
        base_priors$class[mask] <- paste0(base_priors$class[mask], "_trend")

        # Remove sigma rows to prevent conflicts with trend constructor sigma_trend
        base_priors <- base_priors[base_priors$class != "sigma", , drop = FALSE]

        prior_list$base <- base_priors
      }
    }
  }

  # Combine all priors
  if (length(prior_list) == 0) {
    return(create_empty_brmsprior())
  }
  return(bind_brmsprior_rows(prior_list))
}

#' Row-bind brmsprior data frames with column-schema union
#'
#' brms's prior data frames carry slightly different columns depending
#' on which generator built them (the obs side picks up newer columns
#' like `tag` from brms; the trend side and mvgam-internal generators
#' may not). dplyr::bind_rows fills missing columns with NA, which
#' lets this helper accept future brms schema additions without
#' tracking each column individually.
#'
#' @param prior_list List of brmsprior / data.frame objects.
#' @return brmsprior object with all rows row-bound and columns unioned.
#' @noRd
bind_brmsprior_rows <- function(prior_list) {
  checkmate::assert_list(prior_list, min.len = 1)
  combined <- dplyr::bind_rows(prior_list)
  class(combined) <- c("brmsprior", "data.frame")
  combined
}

#' Generate Trend Priors from Monitor Parameters
#'
#' @description
#' Generate priors for trend parameters using the monitor_params metadata
#' from the trend object. This integrates with the existing trend dispatcher
#' system and automatically works for all trend types.
#'
#' @param trend_obj A mvgam_trend object with monitor_params metadata
#' @return A brmsprior object with trend priors
#' @noRd
generate_trend_priors_from_monitor_params <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")

  # Get monitor parameters that need priors
  monitor_params <- trend_obj$monitor_params

  # Filter out correlation parameters for single-series trends
  # L_Omega_trend only makes sense with multiple series (n_series > 1)
  if ("L_Omega_trend" %in% monitor_params &&
      !is.null(trend_obj$dimensions) &&
      trend_obj$dimensions$n_series == 1) {
    monitor_params <- setdiff(monitor_params, "L_Omega_trend")
  }

  if (length(monitor_params) == 0) {
    return(create_empty_brmsprior())
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
#' @param trend_obj mvgam_trend object for context
#' @return List with prior, lb, ub elements
#' @noRd
get_default_trend_parameter_prior <- function(param_name, trend_obj) {
  checkmate::assert_string(param_name)

  # Check for trend-specific customization first
  trend_type <- trend_obj$trend
  custom_function <- paste0("get_", tolower(trend_type), "_parameter_prior")

  if (exists(custom_function, mode = "function")) {
    custom_prior <- get(custom_function, mode = "function")
    result <- custom_prior(param_name, trend_obj)
    if (!is.null(result)) {
      return(result)
    }
  }

  # Use parameter-type-based defaults
  get_parameter_type_default_prior(param_name)
}

#' Get Default Prior Based on Parameter Type
#'
#' @param param_name Character string parameter name
#' @return List with prior, lb, ub elements
#' @noRd
get_parameter_type_default_prior <- function(param_name) {
  checkmate::assert_string(param_name)

  # Pattern matching for common parameter types
  if (grepl("^ar[0-9]+_trend$", param_name)) {
    # AR coefficients: typically bounded [-1, 1] for stationarity
    return(list(prior = "normal(0, 0.5)", lb = "-1", ub = "1"))
  } else if (grepl("^mu_ar[0-9]+_trend$", param_name)) {
    # Population mean for hierarchical AR coefficient (lag-specific)
    return(list(prior = "normal(0, 0.5)", lb = "-1", ub = "1"))
  } else if (grepl("^sigma_ar[0-9]+_trend$", param_name)) {
    # Population scale for hierarchical AR coefficient
    return(list(prior = "exponential(2)", lb = "0", ub = ""))
  } else if (grepl("sigma.*_trend$", param_name)) {
    # Variance parameters: positive with lower bound
    return(list(prior = "", lb = "0", ub = ""))
  } else if (grepl("L_Omega.*_trend$", param_name)) {
    # Correlation matrix Cholesky factors
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl("theta.*_trend$", param_name)) {
    # Theta parameters (e.g., CAR): typically bounded [0, 1]
    return(list(prior = "", lb = "0", ub = "1"))
  } else if (grepl("alpha_cor.*_trend$", param_name)) {
    # Alpha correlation parameters (hierarchical mixing): bounded [0, 1]
    return(list(prior = "", lb = "0", ub = "1"))
  } else if (grepl("A[0-9]+_trend$", param_name)) {
    # VAR coefficient matrices
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl("^[AD]mu_trend$", param_name)) {
    # VAR/VARMA hyperprior means (Amu_trend, Dmu_trend)
    return(list(prior = "", lb = "", ub = ""))
  } else if (grepl("^[AD]omega_trend$", param_name)) {
    # VAR/VARMA hyperprior precisions (Aomega_trend, Domega_trend) - positive
    return(list(prior = "", lb = "0", ub = ""))
  } else if (grepl(".*_trend$", param_name)) {
    # Generic trend parameter
    return(list(prior = "", lb = "", ub = ""))
  } else if (param_name == "Z") {
    # Factor loading matrix (factor models)
    return(list(prior = "", lb = "", ub = ""))
  } else {
    # Other parameters (non-trend parameters in mixed contexts)
    return(list(prior = "", lb = "", ub = ""))
  }
}

#' Create Empty brmsprior Object
#'
#' @return Empty brmsprior data frame
#' @noRd
create_empty_brmsprior <- function() {
  # Delegate to brms so the empty schema always tracks the current
  # brmsprior columns (e.g. `tag` added in recent brms versions). Any
  # mvgam-internal prior rows that get appended downstream will inherit
  # the canonical column set instead of drifting away from brms.
  brms::empty_prior()
}

# =============================================================================
# SECTION 3: TREND-SPECIFIC PRIOR CUSTOMIZATION (OPTIONAL)
# =============================================================================
# WHY: While the integrated system handles most cases automatically via
# monitor_params, some trends may need custom prior logic. These functions
# provide trend-specific customization when the default parameter-type-based
# approach isn't sufficient.

# Note: These functions are optional. If they don't exist, the system falls
# back to parameter-type-based defaults. This provides flexibility while
# maintaining the convention-based approach.

#' Get AR-Specific Parameter Prior (Optional Customization)
#'
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_ar_parameter_prior <- function(param_name, trend_obj) {
  # AR trends can have custom logic for stationarity constraints
  if (grepl("^ar[0-9]+_trend$", param_name)) {
    # For AR coefficients, we might want tighter bounds for stability
    return(list(prior = "", lb = "-0.99", ub = "0.99"))
  }

  # Return NULL to use default parameter-type handling
  return(NULL)
}

#' Get CAR-Specific Parameter Prior (Optional Customization)
#'
#' @param param_name Character string parameter name
#' @param trend_obj mvgam_trend object
#' @return List with prior, lb, ub elements, or NULL for default handling
#' @noRd
get_car_parameter_prior <- function(param_name, trend_obj) {
  # CAR has some special parameter handling
  if (param_name == "ar1") {
    # Legacy CAR parameter without _trend suffix
    return(list(prior = "", lb = "0", ub = "1"))
  }

  # Return NULL to use default parameter-type handling
  return(NULL)
}

# =============================================================================
# SECTION 4: PRIOR COMBINATION FUNCTIONS
# =============================================================================
# WHY: Combining observation and trend priors into a single brmsprior object
# enables seamless use with brms functions while maintaining clear separation
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

  # Handle null cases
  if (is.null(obs_priors) && is.null(trend_priors)) {
    return(NULL)
  }
  if (is.null(obs_priors)) {
    return(trend_priors)
  }
  if (is.null(trend_priors)) {
    return(obs_priors)
  }

  aligned <- align_brmsprior_schemas(list(obs_priors, trend_priors))
  combined <- rbind(aligned[[1L]], aligned[[2L]])

  # Return standard brms prior object
  structure(combined, class = c("brmsprior", "data.frame"))
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

#' Filter Observation Priors from Combined Prior Object
#'
#' @description
#' Extracts only observation model priors from a combined brmsprior object by
#' filtering out trend parameters (those with _trend suffix). This is the
#' complement of filter_trend_priors() and uses the established _trend suffix
#' convention for parameter separation.
#'
#' @param combined_priors A brmsprior object containing both observation and trend priors
#' @return A brmsprior object with only observation model priors
#' @noRd
filter_obs_priors <- function(combined_priors) {
  checkmate::assert_class(combined_priors, "brmsprior", null.ok = TRUE)
  
  if (is.null(combined_priors)) {
    return(NULL)
  }
  
  # Filter out trend parameters (those with _trend suffix)
  obs_mask <- !grepl("_trend$", combined_priors$class)
  obs_priors <- combined_priors[obs_mask, , drop = FALSE]
  
  if (nrow(obs_priors) == 0) {
    return(NULL)
  }
  
  # Return standard brms prior object
  structure(obs_priors, class = c("brmsprior", "data.frame"))
}

#' Filter Trend Priors from Combined Prior Object
#'
#' @description
#' Extracts only trend model priors from a combined brmsprior object by
#' filtering for trend parameters (those with _trend suffix). This is the
#' complement of filter_obs_priors() and uses the established _trend suffix
#' convention for parameter separation.
#'
#' @param combined_priors A brmsprior object containing both observation and trend priors
#' @return A brmsprior object with only trend model priors
#' @noRd
filter_trend_priors <- function(combined_priors) {
  checkmate::assert_class(combined_priors, "brmsprior", null.ok = TRUE)
  
  if (is.null(combined_priors)) {
    return(NULL)
  }
  
  # Filter for trend parameters (those with _trend suffix)
  trend_mask <- grepl("_trend$", combined_priors$class)
  trend_priors <- combined_priors[trend_mask, , drop = FALSE]
  
  if (nrow(trend_priors) == 0) {
    return(NULL)
  }
  
  # Return standard brms prior object
  structure(trend_priors, class = c("brmsprior", "data.frame"))
}

#' Get all mvgam-generated trend parameters using trend system infrastructure
#'
#' @param trend_specs List of trend specifications from mv_spec
#' @return Character vector of all mvgam-generated parameter names
#' @noRd
get_all_mvgam_trend_parameters <- function(trend_specs) {
  if (is.null(trend_specs)) {
    return(character(0))
  }
  
  # Ensure trend registry is initialized
  ensure_registry_initialized()
  
  all_mvgam_params <- character(0)
  
  # Handle both single trend specs and multivariate trend specs
  if (is_multivariate_trend_specs(trend_specs)) {
    # Multivariate: extract trend models from each response
    for (response_name in names(trend_specs)) {
      trend_spec <- trend_specs[[response_name]]
      if (inherits(trend_spec, "mvgam_trend")) {
        monitor_params <- generate_monitor_params(trend_spec)
        all_mvgam_params <- c(all_mvgam_params, monitor_params)
      }
    }
  } else {
    # Single trend spec
    if (inherits(trend_specs, "mvgam_trend")) {
      monitor_params <- generate_monitor_params(trend_specs)
      all_mvgam_params <- c(all_mvgam_params, monitor_params)
    }
  }
  
  # Add base parameters that all trends have
  all_mvgam_params <- c(all_mvgam_params, "sigma_trend")
  
  unique(all_mvgam_params)
}

#' Merge user-supplied prior overrides onto a default prior table
#'
#' Match each row of `user_priors` against `default_priors` using the
#' full brms key (`class`, `coef`, `group`, `resp`, `dpar`, `nlpar`,
#' `lb`, `ub`). brms convention: an empty user-side key field (e.g.
#' `coef = ""`) is a wildcard that matches every row of that class.
#' Matched rows have their `prior` string replaced and `source` set
#' to `"user"`. Unmatched user rows are reported back so the caller
#' can warn.
#'
#' @param default_priors A `brmsprior` data frame; the table to merge
#'   onto. Must include a `source` column.
#' @param user_priors A `brmsprior` data frame of user overrides, or
#'   NULL.
#' @return A list with two elements:
#'   * `priors` -- the merged `brmsprior` data frame
#'   * `unmatched` -- a character vector of `class` strings for user
#'     rows that did not match any default row
#' @noRd
merge_user_priors <- function(default_priors, user_priors) {
  checkmate::assert_class(default_priors, "brmsprior")
  checkmate::assert_class(user_priors, "brmsprior", null.ok = TRUE)
  if (is.null(user_priors) || nrow(user_priors) == 0L) {
    return(list(priors = default_priors, unmatched = character(0L)))
  }
  key_cols <- intersect(
    c("class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub"),
    intersect(names(default_priors), names(user_priors))
  )
  is_wildcard <- function(x) is.na(x) | !nzchar(as.character(x))
  unmatched_classes <- character(0L)
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
        default_priors$source[keep] <- "user"
      }
    } else {
      # Unmatched user row: keep it (brms convention is to preserve
      # user rows even when no default exists, e.g. mvgam-managed
      # `ar1_trend` / `sigma_trend` that the brms pipeline strips).
      # Tagged for the caller's warning, then appended below.
      unmatched_classes <- c(unmatched_classes, row$class)
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
      if ("source" %in% names(r)) r$source <- "user"
      r
    }))
    default_priors <- rbind(default_priors, appended)
  }
  list(
    priors = structure(default_priors,
                         class = c("brmsprior", "data.frame")),
    unmatched = unmatched_classes
  )
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
#'      pipeline strips are still recorded on the fit. Unmatched user
#'      rows are silently retained (the warning path lives in
#'      `get_prior.mvgam()` so fitting itself stays quiet).
#'
#' @param obs_priors The obs-side `brmsprior` from `setup_brms_lightweight()`.
#' @param trend_priors The trend-side `brmsprior` from `setup_brms_lightweight()`,
#'   or NULL when the fit has no trend model.
#' @param user_prior The user's original `prior` / `priors` argument
#'   (already aliased via `normalise_prior_arg_alias()`), or NULL.
#' @param combined_stancode The assembled Stan code, used by
#'   `lift_mvgam_stanvar_priors()` to detect mvgam-injected stanvar
#'   rows.
#' @return A `brmsprior` data frame ready to land on `mvgam_object$prior`.
#' @noRd
assemble_stored_prior_table <- function(obs_priors, trend_priors,
                                          user_prior, combined_stancode) {
  combined <- combine_obs_trend_priors(
    obs_priors, add_trend_suffix_to_priors(trend_priors)
  )
  lifted <- lift_mvgam_stanvar_priors(combined, combined_stancode)
  if (!is.null(user_prior) && nrow(user_prior) > 0L) {
    lifted <- merge_user_priors(lifted, user_prior)$priors
  }
  lifted
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
#' @param trend_priors A brmsprior data frame or NULL.
#' @return The same brmsprior with `class` re-suffixed, or NULL if
#'   `trend_priors` is NULL / empty.
#' @noRd
add_trend_suffix_to_priors <- function(trend_priors) {
  checkmate::assert_class(trend_priors, "brmsprior", null.ok = TRUE)
  if (is.null(trend_priors) || nrow(trend_priors) == 0L) return(NULL)
  needs_suffix <- !grepl("_trend$", trend_priors$class)
  trend_priors$class[needs_suffix] <-
    paste0(trend_priors$class[needs_suffix], "_trend")
  structure(trend_priors, class = c("brmsprior", "data.frame"))
}


#' Strip the `_trend` suffix from the keys of a `brmsprior` table so
#' the remaining rows can be merged back into a brms-only prior set.
#' Used by the trend-side prior pipeline to hand off
#' brms-managed parameters once mvgam's dynamics rows have been
#' factored out via `filter_trend_priors()`.
#'
#' @param trend_priors `brmsprior` rows scoped to the trend formula.
#' @param trend_specs Trend specifications from `mv_spec`.
#' @param base_formula The trend formula with mvgam constructors removed.
#' @param data Data frame; passed through to brms for prior validation.
#' @return `brmsprior` with the `_trend` suffix stripped, or NULL when
#'   the input is empty.
#' @noRd
remove_trend_suffix_from_priors <- function(trend_priors, trend_specs, base_formula, data) {
  checkmate::assert_class(trend_priors, "brmsprior", null.ok = TRUE)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_class(base_formula, "formula")
  checkmate::assert_data_frame(data, min.rows = 1)
  
  if (is.null(trend_priors) || nrow(trend_priors) == 0) {
    return(NULL)
  }
  
  # Get all mvgam-generated parameters from trend system
  mvgam_generated_params <- get_all_mvgam_trend_parameters(trend_specs)
  
  # Filter out mvgam-generated parameters - they belong to mvgam, not brms
  is_brms_compatible <- !trend_priors$class %in% mvgam_generated_params
  
  if (!any(is_brms_compatible)) {
    return(NULL)
  }
  
  # Keep only brms-compatible parameters and remove _trend suffix
  result <- trend_priors[is_brms_compatible, , drop = FALSE]
  result$class <- gsub("_trend$", "", result$class)
  
  structure(result, class = c("brmsprior", "data.frame"))
}

#' Get Complete Prior Specification for a Trend Type
#'
#' @description
#' Retrieves the complete prior specification for a given trend type by
#' merging trend-specific priors from the registry with shared defaults
#' from common_trend_priors. This enables trends to override common defaults
#' where needed while inheriting shared specifications.
#'
#' @param trend_type Character string specifying the trend type (e.g., "AR", "RW")
#' @return Named list of prior specifications, or NULL if trend type not found
#'
#' @details
#' The function works by:
#' 1. Retrieving trend-specific prior_spec from the trend registry
#' 2. For each parameter, using trend-specific specification if available
#' 3. Falling back to common_trend_priors for parameters not specified
#' 4. Returning NULL if the trend type is not registered
#'
#' @seealso \code{\link{register_trend_type}}, \code{common_trend_priors}
#' @noRd
get_trend_prior_spec <- function(trend_type) {
  checkmate::assert_character(trend_type, len = 1, min.chars = 1,
                             any.missing = FALSE)

  # Get trend info from registry
  if (!exists(trend_type, envir = trend_registry)) {
    return(NULL)
  }

  trend_info <- get(trend_type, envir = trend_registry)
  trend_specific_priors <- trend_info$prior_spec

  # Start with empty result
  result <- list()

  # If trend has specific prior specifications, use them
  if (!is.null(trend_specific_priors)) {
    result <- trend_specific_priors
  }

  # For any parameters not specified by trend, check if they exist in common priors
  # This allows trends to inherit common specifications they don't override
  if (length(result) > 0) {
    param_names <- names(result)
    for (param_name in param_names) {
      # If parameter references common_trend_priors, resolve it
      if (is.character(result[[param_name]]) &&
          length(result[[param_name]]) == 1 &&
          startsWith(result[[param_name]], "common_trend_priors.")) {
        common_param <- sub("common_trend_priors\\.", "", result[[param_name]])
        if (common_param %in% names(common_trend_priors)) {
          result[[param_name]] <- common_trend_priors[[common_param]]
        }
      }
    }
  }

  return(result)
}

#' Build Dynamic AR Prior Specification for Non-Continuous Lags
#'
#' @description
#' Generates prior specifications for AR models with arbitrary lag structures,
#' including non-continuous lags like AR(p = c(1, 12, 24)). Creates individual
#' ar{lag}_trend specifications for each lag while sharing common parameters.
#'
#' @param lags Numeric vector of lag values (e.g., c(1, 12, 24))
#' @param ar_prior_base Named list with AR coefficient prior specification
#'   (defaults to common_trend_priors$ar1_trend)
#' @param include_sigma Logical indicating whether to include sigma_trend
#'   (defaults to TRUE)
#' @param include_common Logical indicating whether to include common parameters
#'   like LV (defaults to TRUE)
#'
#' @return Named list of prior specifications for all AR parameters
#'
#' @details
#' For AR(p = c(1, 12, 24)), this creates:
#' - ar1_trend: AR coefficient for lag 1
#' - ar12_trend: AR coefficient for lag 12
#' - ar24_trend: AR coefficient for lag 24
#' - sigma_trend: Innovation standard deviation (if include_sigma = TRUE)
#' - LV, LV_raw: Latent variable specifications (if include_common = TRUE)
#'
#' @seealso \code{\link{get_trend_prior_spec}}, \code{common_trend_priors}
#' @noRd
build_ar_prior_spec <- function(lags, ar_prior_base = NULL,
                               include_sigma = TRUE,
                               include_common = TRUE) {
  checkmate::assert_numeric(lags, min.len = 1, any.missing = FALSE,
                           finite = TRUE)
  checkmate::assert_list(ar_prior_base, null.ok = TRUE, names = "named")
  checkmate::assert_logical(include_sigma, len = 1, any.missing = FALSE)
  checkmate::assert_logical(include_common, len = 1, any.missing = FALSE)

  # Validate lags are positive integers
  if (any(lags <= 0) || any(lags != as.integer(lags))) {
    stop(insight::format_error(c(
      "Invalid lag specification.",
      x = "All lags must be positive integers."
    )))
  }

  # Use default AR prior if not specified
  if (is.null(ar_prior_base)) {
    ar_prior_base <- common_trend_priors$ar1_trend
  }

  # Validate ar_prior_base structure
  required_fields <- c("default", "bounds", "description")
  missing_fields <- setdiff(required_fields, names(ar_prior_base))
  if (length(missing_fields) > 0) {
    stop(insight::format_error(c(
      "Invalid ar_prior_base specification.",
      x = cli::format_inline(
        "Missing required fields: {.val {missing_fields}}"
      )
    )))
  }

  result <- list()

  # Generate ar{lag}_trend specifications for each lag
  for (lag in lags) {
    param_name <- paste0("ar", lag, "_trend")
    result[[param_name]] <- list(
      default = ar_prior_base$default,
      bounds = ar_prior_base$bounds,
      description = paste0("AR(", lag, ") coefficient"),
      dimension = ar_prior_base$dimension %||% "vector"
    )
  }

  # Add sigma_trend if requested
  if (include_sigma) {
    result$sigma_trend <- common_trend_priors$sigma_trend
  }

  # Add common trend parameters if requested
  if (include_common) {
    result$LV <- common_trend_priors$LV
  }

  return(result)
}

#' Convert brmsprior row to Stan distribution string
#'
#' Takes a single row from a brmsprior data frame and extracts the prior
#' specification as a clean Stan distribution string suitable for use in
#' Stan model code. All brms prior functions include prior strings, so no
#' fallback is needed.
#'
#' @param prior_row Data frame with exactly one row containing brmsprior
#'   specification. Must have a 'prior' column with valid prior string.
#'
#' @return Character string containing Stan distribution syntax like
#'   "normal(0, 1)" or "exponential(2)".
#'
#' @noRd
map_prior_to_stan_string <- function(prior_row) {
  # Input validation
  checkmate::assert_data_frame(prior_row, nrows = 1)

  # Validate required column exists
  if (!"prior" %in% names(prior_row)) {
    stop(insight::format_error(
      cli::format_inline(
        "Input {.field prior_row} must contain a 'prior' column"
      )
    ))
  }

  # Extract prior string
  extracted_prior <- prior_row$prior

  # Validate prior string exists and is not empty
  if (is.null(extracted_prior) || is.na(extracted_prior) ||
      nchar(trimws(extracted_prior)) == 0) {
    stop(insight::format_error(
      "Prior string cannot be empty or missing. All brms priors must specify a distribution."
    ))
  }

  # Clean prior string
  extracted_prior <- trimws(extracted_prior)

  # Enhanced Stan distribution syntax validation
  # Check for distribution name followed by parentheses with parameters
  stan_pattern <- "^[a-zA-Z_][a-zA-Z0-9_]*\\s*\\([^\\(\\)]*\\)$"
  if (!grepl(stan_pattern, extracted_prior)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        paste("Prior string", shQuote(extracted_prior),
              "may not be valid Stan syntax.",
              "Expected format: distribution_name(parameters)"),
        .frequency = "once",
        .frequency_id = "mvgam_stan_syntax"
      )
    }
  }

  return(extracted_prior)
}

#' Extract Prior String from brmsprior Object by Class and Coefficient
#'
#' Finds a matching prior in a brmsprior object based on class and coefficient
#' names, with special handling for the _trend suffix convention used in mvgam.
#' Implements hierarchical matching: exact match -> class default -> pattern
#' match -> fallback.
#'
#' @param prior_frame A brmsprior object containing prior specifications
#' @param class_name Character string specifying the parameter class to match
#'   (e.g., "sigma_trend", "ar1_trend", "b")
#' @param coef_name Character string specifying the coefficient name to match.
#'   If NULL, matches class-level defaults. Default is NULL.
#' @param handle_suffix Logical indicating whether to handle _trend suffix
#'   matching. If TRUE, will attempt to match both with and without suffix.
#'   Default is TRUE.
#'
#' @return Character string containing the matched prior specification, or
#'   NULL if no match is found.
#'
#' @noRd
extract_prior_string <- function(prior_frame, class_name, coef_name = NULL,
                                 handle_suffix = TRUE) {
  # Input validation
  checkmate::assert_class(prior_frame, "brmsprior")
  checkmate::assert_string(class_name, min.chars = 1)
  checkmate::assert_string(coef_name, null.ok = TRUE)
  checkmate::assert_logical(handle_suffix, len = 1)

  # Validate required columns exist
  required_cols <- c("prior", "class", "coef")
  missing_cols <- setdiff(required_cols, names(prior_frame))
  if (length(missing_cols) > 0) {
    stop(insight::format_error(
      cli::format_inline(
        "brmsprior object missing required columns: {.field {missing_cols}}"
      )
    ))
  }

  # Strategy 1: Exact class and coef match
  if (!is.null(coef_name)) {
    exact_match <- subset(prior_frame,
                         class == class_name & coef == coef_name)
    if (nrow(exact_match) > 0) {
      return(get_best_prior_match(exact_match))
    }
  }

  # Strategy 2: Class match with empty coef (class-level default)
  class_default <- subset(prior_frame,
                         class == class_name & (coef == "" | is.na(coef)))
  if (nrow(class_default) > 0) {
    return(get_best_prior_match(class_default))
  }

  # Strategy 3: Handle suffix matching if enabled
  if (handle_suffix && !is.null(coef_name)) {
    # Try matching with _trend suffix added
    if (!grepl("_trend$", coef_name)) {
      trend_coef <- paste0(coef_name, "_trend")
      trend_match <- subset(prior_frame,
                           class == class_name & coef == trend_coef)
      if (nrow(trend_match) > 0) {
        return(get_best_prior_match(trend_match))
      }
    }

    # Try matching with _trend suffix removed
    if (grepl("_trend$", coef_name)) {
      base_coef <- gsub("_trend$", "", coef_name)
      base_match <- subset(prior_frame,
                          class == class_name & coef == base_coef)
      if (nrow(base_match) > 0) {
        return(get_best_prior_match(base_match))
      }
    }
  }

  # Strategy 4: Pattern matching for complex coefficient names
  if (!is.null(coef_name)) {
    # Create safe regex pattern from coef_name
    safe_pattern <- gsub("([.()^${}+*?|\\\\\\[\\]])", "\\\\\\1", coef_name)
    pattern_match <- subset(prior_frame,
                           class == class_name & grepl(safe_pattern, coef))
    if (nrow(pattern_match) > 0) {
      return(get_best_prior_match(pattern_match))
    }
  }

  # No match found
  return(NULL)
}

#' Get Best Prior Match from Multiple Candidates
#'
#' When multiple rows match the search criteria, prioritize user-specified
#' priors over defaults and non-empty priors over empty ones.
#'
#' @param matches Data frame subset of brmsprior with matching rows
#' @return Character string of best prior, or NULL if no valid prior found
#' @noRd
get_best_prior_match <- function(matches) {
  if (nrow(matches) == 0) {
    return(NULL)
  }

  # Priority 1: User-specified priors (source != "default")
  if ("source" %in% names(matches)) {
    user_priors <- subset(matches, source != "default")
    if (nrow(user_priors) > 0) {
      matches <- user_priors
    }
  }

  # Priority 2: Non-empty prior strings
  non_empty <- subset(matches,
                     !is.na(prior) & nchar(trimws(prior)) > 0)
  if (nrow(non_empty) > 0) {
    matches <- non_empty
  }

  # Return first (best) match
  prior_string <- matches$prior[1]

  # Handle empty/missing priors
  if (is.na(prior_string) || nchar(trimws(prior_string)) == 0) {
    return(NULL)
  }

  return(trimws(prior_string))
}

#' Map Trend Priors from brmsprior Object
#'
#' @description
#' Extracts relevant priors from a brmsprior object for a specific trend type,
#' handling ar{lag}_trend patterns and other trend-specific parameters. Uses
#' the trend registry system for extensibility and the extract_prior_string()
#' helper for parameter matching.
#'
#' @param prior A brmsprior object containing prior specifications
#' @param trend_type Character string specifying the trend type (e.g., "AR", "RW")
#' @return Named list of Stan distribution strings ready for stanvar generators.
#'   Parameter names match those expected by the trend's Stan generator function.
#'   Returns empty list if trend_type is not registered or no matching priors found.
#'
#' @details
#' This function leverages the existing trend registry system for extensibility:
#' 1. Gets prior specification from \code{get_trend_prior_spec(trend_type)}
#' 2. For each parameter in the specification, uses \code{extract_prior_string()}
#'    to find matching prior in the brmsprior object
#' 3. Handles special patterns like ar{lag}_trend for AR models with multiple lags
#' 4. Returns Stan distribution strings ready for immediate use in stanvar generators
#'
#' The function is designed to be extensible - no hardcoded trend types. New trends
#' automatically work if properly registered with \code{register_trend_type()}.
#'
#' @seealso \code{\link{get_trend_prior_spec}}, \code{\link{extract_prior_string}},
#'   \code{\link{register_trend_type}}, \code{\link[brms]{prior}}, \code{\link[brms]{set_prior}}
#' @noRd
map_trend_priors <- function(prior, trend_type) {
  # Input validation
  checkmate::assert_class(prior, "brmsprior")
  checkmate::assert_character(trend_type, len = 1, min.chars = 1,
                             any.missing = FALSE)

  # Get prior specification for this trend type from registry
  trend_prior_spec <- get_trend_prior_spec(trend_type)

  # Return empty list if trend type not registered
  if (is.null(trend_prior_spec)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(c(
          cli::format_inline(
            "Trend type {.field {trend_type}} not found in registry."
          ),
          i = "No priors will be mapped."
        )),
        .frequency = "once",
        .frequency_id = "mvgam_trend_registry"
      )
    }
    return(list())
  }

  # Initialize result list
  mapped_priors <- list()

  # Extract priors for each parameter in the trend specification
  for (param_name in names(trend_prior_spec)) {

    # Handle special AR lag patterns (ar1_trend, ar2_trend, ar12_trend, etc.)
    if (grepl("^ar\\d+_trend$", param_name)) {
      # Extract specific AR lag parameter
      prior_string <- extract_prior_string(prior, param_name,
                                          handle_suffix = TRUE)
    } else {
      # Standard parameter extraction
      prior_string <- extract_prior_string(prior, param_name,
                                          handle_suffix = TRUE)
    }

    # Add to result if prior was found
    if (!is.null(prior_string)) {
      # Convert to Stan string and store
      temp_prior_row <- data.frame(prior = prior_string,
                                   stringsAsFactors = FALSE)
      mapped_priors[[param_name]] <- map_prior_to_stan_string(temp_prior_row)
    }
  }

  return(mapped_priors)
}

#' Get Trend Parameter Prior with Fallback to Common Default
#'
#' @description
#' Centralized helper for any trend generator to access user-defined priors
#' with automatic fallback to common defaults. This function provides the
#' foundation for simple, DRY prior resolution across all trend types.
#'
#' @param prior A brmsprior object containing custom prior specifications, or NULL
#' @param param_name Character string parameter name (e.g., "sigma_trend", "ar1_trend")
#' @return Character string containing Stan prior distribution (e.g., "exponential(2)")
#'   or empty string if no prior is specified (Stan will use its defaults)
#'
#' @details
#' Resolution strategy:
#' 1. **User specification first**: Checks the provided brmsprior object for the parameter
#' 2. **Common default fallback**: Uses `common_trend_priors` if parameter is available
#' 3. **Empty string fallback**: Returns "" if no specification found (Stan defaults)
#'
#' This design ensures maximum extensibility - any new trend type can call this
#' function for any parameter and get consistent behavior. New parameters can
#' be added to `common_trend_priors` and automatically work across all trends.
#'
#' @seealso \code{\link{extract_prior_string}}, \code{common_trend_priors}
#' @noRd
get_trend_parameter_prior <- function(prior = NULL, param_name) {
  # Input validation
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE, .var.name = "prior")
  checkmate::assert_character(param_name, len = 1, min.chars = 1,
                             any.missing = FALSE, .var.name = "param_name")

  # Strategy 1: Try user specification first
  if (!is.null(prior)) {
    user_prior <- extract_prior_string(prior, param_name, handle_suffix = TRUE)
    if (!is.null(user_prior)) {
      # Defensive check for helper function return
      if (!is.character(user_prior)) {
        stop(insight::format_error(
          cli::format_inline(
            "extract_prior_string returned non-character value for parameter {.field {param_name}}"
          )
        ))
      }

      # Convert to clean Stan string
      temp_prior_row <- data.frame(prior = user_prior, stringsAsFactors = FALSE)
      stan_string <- map_prior_to_stan_string(temp_prior_row)

      # Validate the result
      if (!is.character(stan_string) || length(stan_string) != 1) {
        stop(insight::format_error(
          cli::format_inline(
            "map_prior_to_stan_string returned invalid result for parameter {.field {param_name}}"
          )
        ))
      }

      return(stan_string)
    }
  }

  # Strategy 2: Fallback to common default if available
  if (!is.list(common_trend_priors)) {
    stop(insight::format_error(
      "common_trend_priors must be a list structure"
    ))
  }

  if (param_name %in% names(common_trend_priors)) {
    default_spec <- common_trend_priors[[param_name]]
    if (!is.list(default_spec) || !"default" %in% names(default_spec)) {
      stop(insight::format_error(
        cli::format_inline(
          "Invalid structure for common_trend_priors parameter {.field {param_name}}"
        )
      ))
    }
    return(default_spec$default)
  }

  # Strategy 3: Pattern-based defaults for parameter types
  pattern_default <- get_parameter_type_default_prior(param_name)
  if (pattern_default$prior != "") {
    return(pattern_default$prior)
  }

  # Strategy 4: Empty string fallback (Stan will use its defaults)
  return("")
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
#'   \item Smooth terms using mgcv syntax: \code{s()}, \code{te()}, \code{ti()}, \code{t2()}
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
#' \code{\link{get_prior.mvgam_formula}}, \code{\link{stancode}},
#' \code{\link{standata}}, \code{\link{mvgam}}
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
    # Detect bf() / brmsformula with distributional-parameter formulas
    # (pforms) before the assert_formula guard fires. The downstream
    # validator returns a generic "Must be a formula, not brmsformula"
    # message; trap the dpar case here so users get a targeted hint.
    if (inherits(trend_formula, c("brmsformula", "bform")) &&
        !is.null(trend_formula$pforms) &&
        length(trend_formula$pforms) > 0L) {
      dpar_names <- paste(
        paste0("'", names(trend_formula$pforms), "'"),
        collapse = ", "
      )
      stop(insight::format_error(c(
        paste0(
          "Distributional-parameter formulas (e.g. 'sigma ~ z') ",
          "inside 'trend_formula' are not currently supported."
        ),
        x = paste0(
          "Found dpar formula(s): ", dpar_names, "."
        ),
        i = paste0(
          "Use bf(...) in 'formula' (the observation model) for ",
          "distributional parameters, or open an issue if you need ",
          "trend-side dpar support."
        )
      )))
    }

    checkmate::assert_formula(trend_formula, .var.name = "trend_formula")

    # Use comprehensive trend formula validation from validations.R
    validate_single_trend_formula(trend_formula, context = "trend_formula")
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
#' objects. This function provides a unified interface for prior inspection
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

#' Default method for get_prior - delegates to brms
#'
#' @param object Model specification object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.default <- function(object, ...) {
  brms::get_prior(object, ...)
}

#' Formula method for get_prior - explicit brms delegation
#'
#' @param object A formula object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.formula <- function(object, ...) {
  brms::get_prior(object, ...)
}

#' brmsformula method for get_prior - explicit brms delegation
#'
#' @param object A brmsformula object
#' @param ... Additional arguments passed to \code{brms::get_prior}
#' @return A \code{brmsprior} data frame
#' @export
get_prior.brmsformula <- function(object, ...) {
  brms::get_prior(object, ...)
}

#' Method for fitted mvgam objects - returns the stored prior table
#'
#' Convenience shortcut for the post-fit prior table. In brms the
#' canonical post-fit accessor is `prior_summary()`; mvgam keeps
#' that path (it forwards to `object$prior` too) but exports
#' `get_prior.mvgam()` as well because users frequently type
#' `get_prior(fit)` by analogy with the formula method. The
#' returned `brmsprior` is the literal prior table mvgam handed
#' to brms at fit time (with all adaptive constants baked in by
#' brms), so it can be edited and fed back through
#' `update(fit, prior = ..., recompile = FALSE)` for refits that
#' reuse the compiled Stan model.
#'
#' @param object A fitted \code{mvgam} model.
#' @param ... Currently unused; present for S3 generic dispatch.
#' @return A \code{brmsprior} data frame covering every adjustable
#'   prior row (observation and trend components). Rows the user
#'   overrode at fit time carry `source = "user"`; rows left at
#'   their defaults carry `source = "default"` (or whatever source
#'   string \pkg{brms} assigned).
#' @export
get_prior.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")

  # Re-derive the full obs + trend prior table from the formula
  # slots on the fit; `object$prior` alone carries only the subset
  # the user supplied at fit time and therefore hides every
  # trend-side row that Stan also used.
  formula_obs <- object$formula
  trend_call <- object$trend_call
  data <- object$data %||% object$obs_data
  family <- object$family %||% gaussian()

  rederive_failed <- is.null(formula_obs) || is.null(data)
  if (!rederive_failed) {
    full <- tryCatch(
      get_prior.mvgam_formula(
        mvgam_formula(formula_obs, trend_call),
        data = data, family = family
      ),
      error = function(e) NULL
    )
    rederive_failed <- is.null(full)
  }

  # If re-derivation is not possible (e.g. the fit pre-dates the
  # formula-slot enrichments), fall back to whatever is on
  # `object$prior` so callers still get something usable. The user
  # also sees only the legacy view in that case, so warn once.
  if (rederive_failed) {
    if (is.null(object$prior)) {
      stop(insight::format_error(c(
        "Fit was not stored with a prior table.",
        i = "Refit with the current package version to enable get_prior() on fitted objects."
      )))
    }
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        paste0(
          "Returning the user-supplied prior overrides only; ",
          "the full prior table could not be re-derived from the ",
          "stored formula slots."
        ),
        class = "mvgam_get_prior_fallback",
        .frequency = "once",
        .frequency_id = "mvgam_get_prior_fallback"
      )
    }
    return(object$prior)
  }

  # `object$prior` (combined obs + trend at fit time) carries BOTH
  # the user-supplied rows AND brms's auto-defaults; only the
  # `source = "user"` subset is treated as overrides here.
  user <- object$prior
  if (!is.null(user) && "source" %in% names(user)) {
    user <- user[user$source == "user", , drop = FALSE]
  }
  merged <- merge_user_priors(full, user)
  if (length(merged$unmatched) > 0L &&
      !identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      paste0(
        length(merged$unmatched),
        " user-supplied prior row(s) did not match any row in the ",
        "default table: ",
        paste(unique(merged$unmatched), collapse = ", "),
        ". Check the `class` / `coef` spelling."
      ),
      class = "mvgam_get_prior_unmatched",
      .frequency = "once",
      .frequency_id = "mvgam_get_prior_unmatched"
    )
  }
  merged$priors
}

#' Detect Embedded Families in Formula Objects
#'
#' Checks if a formula object contains embedded family specifications
#' (e.g., bf(y1 ~ x, family = poisson()) + bf(y2 ~ x, family = gaussian()))
#'
#' @param formula A formula, brmsformula, or mvbrmsformula object
#' @return Logical indicating whether embedded families are present
#' @noRd
has_embedded_families <- function(formula) {
  checkmate::assert_multi_class(formula, c("formula", "brmsformula", "mvbrmsformula"))

  if (inherits(formula, "mvbrmsformula") && !is.null(formula$forms)) {
    # Multivariate case: check if any bf() component has embedded family
    return(any(sapply(formula$forms, function(x) !is.null(x$family))))
  } else if (inherits(formula, "brmsformula")) {
    # Single brmsformula with potential embedded family
    return(!is.null(formula$family))
  }

  # Regular formula objects cannot have embedded families
  return(FALSE)
}

#' Extract Prior Specifications for mvgam Formula Objects
#'
#' @description
#' Extracts and combines prior specifications for both observation and trend
#' components of an mvgam model. This method provides a unified interface
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
#'   \item Combines them into a unified \code{brmsprior} object
#'   \item Adds the \code{trend_component} column for easy filtering
#' }
#'
#' The function handles embedded families automatically when using \code{bf()}
#' specifications and supports all brms family types for observation models
#' while trend components are always modeled as Gaussian State-Space processes.
#'
#' @examples
#' \donttest{
#' # Workflow: discover the adjustable priors, override a subset,
#' # then pass the result back to `mvgam()`.
#' set.seed(1)
#' dat <- sim_mvgam(
#'   family       = poisson(),
#'   n_series     = 2L,
#'   n_timepoints = 50L,
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
#' summary(mod, include_betas = FALSE)
#' }
#'
#' @seealso \code{\link{mvgam_formula}}, \code{\link[brms]{get_prior}},
#'   \code{\link[brms]{set_prior}}, \code{\link[brms]{prior}}
#' @export
get_prior.mvgam_formula <- function(object, data, family = gaussian(), ...) {

  # Input validation (required by CLAUDE.md standards)
  checkmate::assert_class(object, "mvgam_formula")
  checkmate::assert_data_frame(data, min.rows = 1)

  # Extract formula components from mvgam_formula object
  formula <- object[[1]]  # object$formula triggers S3 dispatch issues
  trend_formula <- object[[2]]  # object$trend_formula

  # Validate family parameter conditionally based on formula type
  if (!has_embedded_families(formula)) {
    checkmate::assert_class(family, "family")
  }

  # Validate formula structure before proceeding
  if (length(formula) < 3) {
    stop(insight::format_error(c(
      "Formula missing response variable.",
      i = "Ensure formula has form: y ~ predictors"
    )))
  }

  # Extract observation priors with embedded family support.
  # `safe_brms_prior_call` falls back to `brms::empty_prior()` on
  # the known empty-frame crash that occurs when the obs formula
  # has no coefficient classes (e.g. `y ~ 0` for a pure-trend
  # state-space model).
  if (has_embedded_families(formula)) {
    # Let brms handle embedded families - don't pass family parameter
    obs_priors <- safe_brms_prior_call(
      brms::get_prior(formula = formula, data = data, ...)
    )
  } else {
    # Pass family parameter for non-embedded cases
    obs_priors <- safe_brms_prior_call(
      brms::get_prior(formula = formula, data = data,
                      family = family, ...)
    )
  }

  # Handle case where no trend formula is specified
  if (is.null(trend_formula)) {
    # Return standard brms prior object
    return(obs_priors)
  }

  # Extract response variable names for trend prior generation
  response_names <- extract_response_names(formula)

  # Parse multivariate trends and validate
  mv_spec <- parse_multivariate_trends(formula, trend_formula)
  
  # Extract and validate trend components. This call also runs the
  # `by = lv_axis()` AST rewrite (factor-active rewrites to
  # `by = .trend`; non-factor rewrites to `by = series`), so the
  # rewritten formula must be threaded into the downstream prior
  # extraction; passing the raw `trend_formula` here would surface
  # `lv_axis()` as an unresolved variable inside brms's
  # `validate_data()`.
  components <- extract_and_validate_trend_components(
    data, mv_spec, response_names, "time", "series", trend_formula
  )

  # Extract dimensions from the validated spec
  dimensions <- if (is_multivariate_trend_specs(components$enhanced_mv_spec$trend_specs)) {
    first_spec <- components$enhanced_mv_spec$trend_specs[[1]]
    first_spec$dimensions
  } else {
    components$enhanced_mv_spec$trend_specs$dimensions
  }

  # The validator rewrites `by = lv_axis()` markers in the trend
  # formula to either `by = series` (non-factor path) or
  # `by = .trend` (factor path); without this rewrite,
  # `extract_trend_priors()` would surface the literal `lv_axis()`
  # call as an unresolved variable inside brms's `validate_data()`.
  # For all other trend formulas, the validator's `base_formula`
  # carries only the trend-predictor side (the constructor stripped),
  # so we must keep the original `trend_formula` and only swap in
  # the rewritten form when the marker was actually present.
  uses_lv_axis_marker <- inherits(trend_formula, "formula") &&
    any(grepl("lv_axis\\(\\)|by = trend\\b",
               deparse(trend_formula)))
  trend_formula_for_priors <- if (uses_lv_axis_marker &&
      inherits(components$enhanced_mv_spec$base_formula, "formula")) {
    components$enhanced_mv_spec$base_formula
  } else {
    trend_formula
  }

  # Extract trend model priors using validated components
  trend_priors <- extract_trend_priors(
    trend_formula = trend_formula_for_priors,
    data = components$trend_data,
    response_names = response_names,
    .precomputed_dimensions = dimensions
  )

  # Combine observation and trend priors using existing helper function
  combined_priors <- combine_obs_trend_priors(obs_priors, trend_priors)

  return(combined_priors)
}

