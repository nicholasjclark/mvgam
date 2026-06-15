#' Cache Formula Latent Parameters
#'
#' @description
#' Extract and cache latent parameters from brmsformula objects to avoid
#' repetitive parsing during validation. Caches parameters from pforms
#' (distributional parameters) and nlpars (nonlinear parameters).
#'
#' @param formula A formula object, potentially with brmsformula structure
#' @return Same formula object with cached latent parameters as attributes
#' @noRd
#'
#' @examples
#' # Internal usage only
#' formula <- brms::bf(y ~ x, sigma ~ z, nl = TRUE)
#' cached_formula <- cache_formula_latent_params(formula)
cache_formula_latent_params <- function(formula) {
  # Input validation
  if (is.null(formula)) {
    return(formula)
  }

  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    combine = "or"
  )

  # Skip if already cached (avoid redundant processing)
  existing_cache <- attr(formula, "mvgam_latent_params")
  if (!is.null(existing_cache) && !is.null(attr(formula, "mvgam_cache_version"))) {
    return(formula)
  }

  # Initialize latent parameter collection
  latent_params <- character()

  # Extract latent parameters based on formula type
  if (inherits(formula, "brmsformula")) {
    # Extract distributional parameters (pforms: sigma, nu, etc.)
    if (!is.null(formula$pforms) && length(formula$pforms) > 0) {
      pform_params <- names(formula$pforms)
      if (!is.null(pform_params)) {
        latent_params <- c(latent_params, pform_params)
      }
    }

    # Extract nonlinear parameters (nlpars) if present
    if (!is.null(formula$nlpars) && length(formula$nlpars) > 0) {
      nlpar_params <- names(formula$nlpars)
      if (!is.null(nlpar_params)) {
        latent_params <- c(latent_params, nlpar_params)
      }
    }
  }

  # Clean and deduplicate parameters
  latent_params <- unique(latent_params[nzchar(latent_params)])

  # Cache results as attributes
  attr(formula, "mvgam_latent_params") <- latent_params
  attr(formula, "mvgam_cache_version") <- "1.0"
  attr(formula, "mvgam_cache_timestamp") <- Sys.time()

  return(formula)
}

#' Filter Required Variables Using Formula Metadata
#'
#' @description
#' Remove latent parameters from variable requirements using cached formula
#' metadata. Prevents validation errors for model-defined parameters that
#' don't exist in user data.
#'
#' @param required_vars Character vector of required variable names
#' @param formula Formula object with potential cached metadata
#' @return Character vector with latent parameters filtered out
#' @noRd
#'
#' @examples
#' # Internal usage only
#' required_vars <- c("x", "y", "sigma", "nu")
#' formula <- brms::bf(y ~ x, sigma ~ z)
#' cached_formula <- cache_formula_latent_params(formula)
#' filtered_vars <- filter_required_variables(required_vars, cached_formula)
#' # Returns: c("x", "y") - sigma filtered out as latent parameter
filter_required_variables <- function(required_vars, formula = NULL) {
  # Input validation
  checkmate::assert_character(required_vars, any.missing = FALSE)

  # Early return for edge cases
  if (length(required_vars) == 0) {
    return(character())
  }

  if (is.null(formula)) {
    return(required_vars)
  }

  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    combine = "or"
  )

  # Extract latent parameters for filtering
  latent_params <- character()

  # Primary: Use cached metadata if available
  cached_latent <- attr(formula, "mvgam_latent_params")
  cache_version <- attr(formula, "mvgam_cache_version")

  if (!is.null(cached_latent) && !is.null(cache_version)) {
    latent_params <- cached_latent
  } else {
    # Fallback: Direct extraction for uncached formulas
    if (inherits(formula, "brmsformula")) {
      fallback_params <- character()

      if (!is.null(formula$pforms) && length(formula$pforms) > 0) {
        pform_names <- names(formula$pforms)
        if (!is.null(pform_names)) {
          fallback_params <- c(fallback_params, pform_names)
        }
      }

      if (!is.null(formula$nlpars) && length(formula$nlpars) > 0) {
        nlpar_names <- names(formula$nlpars)
        if (!is.null(nlpar_names)) {
          fallback_params <- c(fallback_params, nlpar_names)
        }
      }

      latent_params <- unique(fallback_params[nzchar(fallback_params)])
    }
  }

  # Filter out latent parameters from requirements
  if (length(latent_params) > 0) {
    filtered_vars <- setdiff(required_vars, latent_params)
    return(filtered_vars)
  }

  return(required_vars)
}

#' Validate and Standardize Family Argument
#'
#' @description
#' Checks and corrects validity of the model family. Converts functions,
#' family objects, or character strings to a standardized brmsfamily object.
#'
#' @param family Either a function, an object of class 'family' or
#'   'brmsfamily', or a character string of length one or two
#' @param link An optional character string naming the link function.
#'   Ignored if family is a function or a family object.
#'
#' @return A brmsfamily object
#'
#' @noRd
validate_family <- function(family, link = NULL) {
 # Input validation
  checkmate::assert(
    checkmate::check_function(family),
    checkmate::check_class(family, "family"),
    checkmate::check_class(family, "brmsfamily"),
    checkmate::check_character(family, min.len = 1, max.len = 2),
    combine = "or"
  )
  checkmate::assert_string(link, null.ok = TRUE)

  # Handle function input (e.g., gaussian, poisson)
  if (is.function(family)) {
    family <- family()
  }

  # Already a brmsfamily - return as is
  if (inherits(family, "brmsfamily")) {
    return(family)
  }

  # Handle standard R family objects
  if (inherits(family, "family")) {
    link <- family$link
    family <- family$family
  }

  # Handle character input
  if (is.character(family)) {
    if (is.null(link)) {
      link <- family[2]
    }
    family_name <- family[1]

    # Use brms family conversion with error handling
    family_result <- try(
      brms::brmsfamily(family_name, link = link),
      silent = TRUE
    )

    if (inherits(family_result, "try-error")) {
      stop("family not recognized", call. = FALSE)
    }

    return(family_result)
  }

  # Fallback for unexpected input
  stop("family not recognized", call. = FALSE)
}


#' Validate Family is Supported by mvgam
#'
#' @description
#' Checks that the family is supported by mvgam. Multi-category families
#' (categorical, multinomial, dirichlet, etc.) are not supported because
#' they require 3D linear predictors that cannot be combined with
#' State-Space trend components.
#'
#' @param family A family or brmsfamily object
#'
#' @return Invisible TRUE if valid, stops with error if unsupported
#'
#' @noRd
validate_supported_family <- function(family) {
  checkmate::assert(
    checkmate::check_class(family, "family"),
    checkmate::check_class(family, "brmsfamily"),
    checkmate::check_class(family, "customfamily"),
    combine = "or"
  )
  # mvgam-side multi-response wrappers (diri / multi / categ / mvn /
  # mvt) carry the `mvgam_multi_response` attribute and route a
  # long-format K-row response through the closure-unit pipeline.
  # The naked brms families that wrap K-1 per-category linear
  # predictors with a reference category cannot compose with the
  # mvgam factor-model trend; emit a single-line error pointing the
  # user at the correct mvgam wrapper instead.
  if (is_multi_response_family(family)) {
    return(invisible(TRUE))
  }
  pointer <- switch(
    family$family %||% "",
    dirichlet       = "diri()",
    multinomial     = "multi()",
    categorical     = "categ()",
    logistic_normal = "mvn()",
    NULL
  )
  if (!is.null(pointer)) {
    stop(insight::format_error(paste0(
      "Family '", family$family, "' is not supported by mvgam directly. ",
      "Use ", pointer, " instead (long-format multi-response wrapper)."
    )))
  }
  invisible(TRUE)
}


#' Validate observation data shape for a closure-unit family
#'
#' Closure-unit families (`nmix()`, `occ()`, future
#' `royle_nichols()`, etc.) accept long-format data with one row
#' per visit. Multiple rows per `(series, time)` pair encode
#' replicate visits to one closure unit. This validator checks
#' the data conforms to that shape, validates the `cap`
#' (upper-truncation) column when required, and warns when the
#' visit structure is at risk of leaving the state and detection
#' parameters separately unidentified.
#'
#' Identifiability rules implemented:
#'   - error if `N_unit == 1` (single closure unit gives no
#'     information about the state parameter).
#'   - warn (once per session) if every closure unit has a single
#'     visit AND neither the state nor the detection formula
#'     carries a covariate (a fully intercept-only single-visit
#'     model identifies only the product of state and detection;
#'     the individual parameters are prior-dominated, MacKenzie
#'     et al. 2002, Royle and Dorazio 2008 ch. 3.5).
#'   - warn (once per session) if more than 30% of closure units
#'     have a single visit.
#'   - error if any closure unit has `cap < max(y)` (impossible
#'     latent abundance support; the likelihood evaluates to
#'     `-Inf`).
#'
#' @param data Long-format observation data frame.
#' @param response_var Name of the response column.
#' @param series_var Series factor column name (default
#'   `"series"`).
#' @param time_var Time column name (default `"time"`).
#' @param cap_var Per-row upper-truncation column name (default
#'   `"cap"`). For binary-response families this column is
#'   optional; when missing a constant of 1 is used.
#' @param has_obs_covariates Logical; TRUE when the state formula
#'   contains at least one covariate.
#' @param has_det_covariates Logical; TRUE when a detection
#'   sub-formula (e.g. `p ~ tod`) is supplied.
#' @param binary_y_check Logical; TRUE when the family restricts
#'   the response to {0, 1} (e.g. `occ()`,
#'   `nmix("royle_nichols")`). Triggers the y-range check.
#' @param cap_required Logical; TRUE when the family requires the
#'   `cap` data column to be present. FALSE for families that
#'   default the per-unit upper truncation (e.g. `occ()` defaults
#'   to `cap = 1`). Royle-Nichols carries binary response but
#'   keeps `cap_required = TRUE` because its latent abundance can
#'   exceed one.
#' @return Invisible `TRUE` on success; stops on hard
#'   identifiability failure.
#' @noRd
validate_closure_unit_data <- function(data,
                                        response_var,
                                        series_var          = "series",
                                        time_var            = "time",
                                        cap_var             = "cap",
                                        has_obs_covariates  = FALSE,
                                        has_det_covariates  = FALSE,
                                        binary_y_check      = FALSE,
                                        cap_required        = TRUE) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(response_var)
  checkmate::assert_string(series_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(cap_var)
  checkmate::assert_flag(has_obs_covariates)
  checkmate::assert_flag(has_det_covariates)
  checkmate::assert_flag(binary_y_check)
  checkmate::assert_flag(cap_required)

  # Required columns: response + grouping. The cap column is
  # optional only for families that default the per-unit upper
  # truncation (e.g. occ() defaults to 1); count-latent families
  # such as nmix() and nmix("royle_nichols") always require it.
  required_cols <- c(response_var, series_var, time_var)
  if (cap_required) {
    required_cols <- c(required_cols, cap_var)
  }
  for (col in required_cols) {
    if (!col %in% colnames(data)) {
      stop(insight::format_error(c(
        paste0(
          "Closure-unit families require column '", col,
          "' to be present in 'data'."
        ),
        i = paste0(
          "Each row of 'data' is one visit; the (",
          series_var, ", ", time_var, ") pair identifies a ",
          "closure unit and '", cap_var, "' bounds the latent ",
          "state per unit."
        )
      )))
    }
  }

  y_vals   <- data[[response_var]]
  # Binary families default `cap` to 1 when the column is absent;
  # users may still supply `cap` explicitly (any positive integer
  # >= y), in which case it flows through the same checks as
  # count families.
  cap_vals <- if (cap_var %in% colnames(data)) {
    data[[cap_var]]
  } else {
    rep(1L, nrow(data))
  }

  if (any(!is.finite(suppressWarnings(as.numeric(y_vals))))) {
    stop(insight::format_error(
      paste0(
        "Non-finite or non-numeric values found in '",
        response_var, "'."
      )
    ))
  }
  y_int <- as.integer(y_vals)
  if (any(y_int < 0L)) {
    stop(insight::format_error(
      paste0("Negative counts found in '", response_var, "'.")
    ))
  }
  if (any(abs(as.numeric(y_vals) - y_int) > 1e-8)) {
    stop(insight::format_error(c(
      paste0(
        "Non-integer values found in response '",
        response_var, "'."
      ),
      i = paste0(
        "Closure-unit families model integer counts; round or ",
        "cast '", response_var, "' to integer before fitting."
      )
    )))
  }
  if (binary_y_check && any(y_int > 1L)) {
    bad <- which(y_int > 1L)[1L]
    stop(insight::format_error(c(
      paste0(
        "Binary-response closure-unit family requires '",
        response_var, "' in {0, 1}."
      ),
      x = paste0(
        "Row ", bad, ": ", response_var, " = ", y_int[bad], "."
      ),
      i = paste0(
        "For count detections use family = nmix() instead; ",
        "occ() and nmix(\"royle_nichols\") model detection / ",
        "non-detection only."
      )
    )))
  }

  if (any(!is.finite(suppressWarnings(as.numeric(cap_vals))))) {
    stop(insight::format_error(
      paste0(
        "Non-finite or non-numeric values found in '",
        cap_var, "'."
      )
    ))
  }
  cap_int <- as.integer(cap_vals)
  if (any(cap_int < 1L)) {
    stop(insight::format_error(
      paste0("'", cap_var, "' must be a positive integer.")
    ))
  }
  if (any(cap_int < y_int)) {
    bad <- which(cap_int < y_int)[1L]
    stop(insight::format_error(c(
      paste0(
        "Some '", cap_var, "' values are below the observed counts."
      ),
      x = paste0(
        "Row ", bad, ": ", cap_var, " = ", cap_int[bad],
        ", ", response_var, " = ", y_int[bad], "."
      ),
      i = paste0(
        "Each closure unit's '", cap_var, "' must be at least ",
        "the largest observed count in that unit; raise '",
        cap_var, "' or drop the offending row."
      )
    )))
  }

  # Closure-unit grouping. Run once and reuse for both the
  # cap-constant-within-unit check and the identifiability
  # heuristics so users see the friendly error at validation
  # time rather than mid-array-build.
  series_vals <- as.factor(data[[series_var]])
  time_vals   <- data[[time_var]]
  unit_label  <- paste(as.integer(series_vals), as.integer(time_vals),
                       sep = "_")
  unit_int    <- match(unit_label, unique(unit_label))
  rep_counts  <- tabulate(unit_int)

  n_unit <- length(rep_counts)
  for (g in seq_len(n_unit)) {
    rows_g <- which(unit_int == g)
    cap_g  <- cap_int[rows_g]
    if (length(unique(cap_g)) > 1L) {
      bad_row <- rows_g[1L]
      stop(insight::format_error(c(
        paste0(
          "'", cap_var, "' must be constant within a closure unit."
        ),
        x = paste0(
          "Closure unit (", series_var, "=",
          as.character(series_vals[bad_row]), ", ",
          time_var, "=", time_vals[bad_row],
          ") has differing '", cap_var, "' values: ",
          paste(unique(cap_g), collapse = ", "), "."
        ),
        i = paste0(
          "Each closure unit has one latent abundance, so its ",
          "upper truncation '", cap_var, "' must be a single value."
        )
      )))
    }
  }

  # Structurally degenerate input: a single closure unit is one
  # draw from the state distribution; its parameters (mean,
  # variance) are not identified by a single realisation even
  # with arbitrarily many visits to that one unit.
  if (n_unit < 2L) {
    stop(insight::format_error(c(
      "Closure-unit family requires at least two closure units.",
      x = paste0(
        "Only ", n_unit, " unique (", series_var, ", ", time_var,
        ") combination found."
      ),
      i = paste0(
        "Each closure unit is one draw from the state ",
        "distribution; a single draw cannot identify the ",
        "distribution parameters regardless of the visit count."
      )
    )))
  }

  any_covariates <- has_obs_covariates || has_det_covariates
  # All-single-visit + no-covariates handling diverges by family
  # support. For occ (bounded psi in [0, 1]) the inference is
  # prior-dominated but proper (Royle and Dorazio 2008 ch. 3.5);
  # warn and allow the fit. For nmix (unbounded lambda > 0) the
  # literature treats the same configuration as an identifiability
  # failure: lambda and p sit on the lambda * p = y / n_visits
  # isocurve with no data signal to break the symmetry (Solymos
  # et al. 2012, Dennis et al. 2015, Kery 2018). Refuse the fit.
  if (all(rep_counts == 1L) && !any_covariates) {
    if (binary_y_check) {
      if (!identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          insight::format_warning(c(
            "Every closure unit has a single visit and no covariates.",
            i = paste0(
              "Only the product of state and detection probability ",
              "is identified by data; the individual parameters ",
              "are prior-dominated (Royle and Dorazio 2008, ch. 3.5)."
            )
          )),
          .frequency = "once",
          .frequency_id = "closure_unit_all_single_visit"
        )
      }
    } else {
      stop(insight::format_error(c(
        "Closure-unit count family is non-identified.",
        x = paste0(
          "Every unit has a single visit and neither the state ",
          "nor the detection formula carries a covariate."
        ),
        i = paste0(
          "With unbounded state support, lambda and detection p ",
          "lie on the lambda * p = observed isocurve with no data ",
          "signal to separate them (Solymos et al. 2012). Add a ",
          "covariate to a formula or supply additional visits per ",
          "closure unit."
        )
      )))
    }
  }
  single_visit_share <- mean(rep_counts == 1L)
  if (single_visit_share > 0.3 && !all(rep_counts == 1L)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(c(
          paste0(
            "More than 30% of closure units have a single visit ",
            "(", round(100 * single_visit_share),
            "% single-visit units)."
          ),
          i = paste0(
            "State and detection probability share information ",
            "only via the formulae; with this proportion of ",
            "single-visit units, posterior identifiability ",
            "depends largely on the covariate structure."
          )
        )),
        .frequency = "once",
        .frequency_id = "closure_unit_single_visit"
      )
    }
  }

  invisible(TRUE)
}


#' Validate Nonlinear Trend Compatibility
#'
#' @description
#' Validates that trend specifications are compatible with nonlinear model structure.
#'
#' @param nl_components List of nonlinear components
#' @param trend_specs Trend specification
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_nonlinear_trend_compatibility <- function(nl_components, trend_specs) {
  checkmate::assert_list(nl_components)
  checkmate::assert_list(trend_specs, null.ok = TRUE)

  if (is.null(trend_specs)) {
    return(invisible(TRUE))
  }

  # Check that trend type is compatible with nonlinear models
  incompatible_trends <- c()  # Currently all trends should work

  if (trend_specs$type %in% incompatible_trends) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Trend type {.val {trend_specs$type}} is not compatible with nonlinear models."
      ),
      i = "Consider using different trend specification."
    )))
  }

  invisible(TRUE)
}

#' Validate required variables exist in data
#'
#' @description
#' Checks that all required variables are present in the provided data.
#' Automatically filters out latent parameters from nonlinear formulas
#' to avoid validation errors for model-defined parameters.
#'
#' @param data Data frame to check
#' @param required_vars Character vector of required variable names
#' @param context Context description for error messages
#' @param formula Optional formula object for nonlinear parameter filtering
#' @return Invisible TRUE if validation passes, throws error otherwise
#' @noRd
validate_required_variables <- function(data, required_vars, context = "data", formula = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(required_vars, min.len = 1)
  checkmate::assert_character(context, len = 1)
  checkmate::assert(
    is.null(formula) || inherits(formula, "brmsformula"),
    .var.name = "formula"
  )

  # Remove latent parameters from nonlinear formulas as they are model-defined, not data variables
  if (!is.null(formula) && inherits(formula, "brmsformula") && !is.null(formula$pforms)) {
    latent_params <- names(formula$pforms)
    required_vars <- setdiff(required_vars, latent_params)

    # If no variables remain after filtering, validation passes
    if (length(required_vars) == 0) {
      return(invisible(TRUE))
    }
  }

  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    # Pre-compute strings for proper interpolation
    missing_str <- paste(missing_vars, collapse = ", ")
    available_str <- paste(names(data), collapse = ", ")

    stop(insight::format_error(
      c(
        paste0("Required variables not found in ", context, ":"),
        "x" = paste0("Missing: ", missing_str),
        "i" = paste0("Available: ", available_str)
      )
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Normalise the user-facing `trend_map` argument
#'
#' Single entry point that converts any of the accepted
#' `trend_map` input shapes to a canonical numeric loading
#' matrix `Z` of dimension `n_series × n_lv`. Every consumer of
#' fixed loadings (the `mvgam()` arg, the trend constructors,
#' the future `nmix`/`jsdgam` ports) calls this function — there
#' is no parallel parsing anywhere else.
#'
#' Accepted shapes:
#' \itemize{
#'   \item Numeric `matrix` of dimension `n_series × n_lv`.
#'     Finite entries are treated as fixed and preserved exactly
#'     on `Z` in the posterior (including fractional or
#'     negative values). `NA` entries mark loadings to sample
#'     (partial Z); the free entries are sampled jointly while
#'     the fixed entries stay pinned. `n_lv` is inferred from
#'     `ncol(Z)`.
#'   \item `data.frame(series, trend)` — one row per series
#'     assigning each to a single trend integer. The resulting
#'     Z is binary `0/1` with `Z[s, k] = 1` iff
#'     `trend_map$trend[trend_map$series == s] == k`.
#'   \item Character scalar `"identity"` (each series its own
#'     trend; `Z = diag(n_series)`) or `"shared"` (single shared
#'     latent factor; `Z = matrix(1, n_series, 1)`).
#' }
#'
#' Any non-NULL `trend_map` bypasses the post-hoc QR
#' identification used by default factor models (Heaps & Jermyn
#' 2024). The user-supplied loadings are saved as `Z[i, j]`
#' directly; no `Z_tilde` is emitted because the encoded
#' structure already anchors the basis.
#'
#' Rejects:
#' \itemize{
#'   \item Integer vector form (silent ordering bug when series
#'     factor levels change).
#'   \item Z matrices with `Inf` or `NaN` entries (use `NA` to
#'     mark sampled entries).
#'   \item Rows of fixed zeros with no free entries (series
#'     silently unmodelled).
#'   \item data.frame mappings with missing series, gaps in the
#'     trend integer sequence, or `max(trend) > n_series`.
#' }
#'
#' @param input The user-supplied `trend_map` value.
#' @param data Training `data.frame` carrying a `series` factor
#'   so the normaliser can resolve dimensions and validate
#'   labels.
#'
#' @return `list(Z = <num matrix>, n_lv = <int>)`.
#'
#' Shape-only assertion for `trend_map` (constructor fail-fast).
#'
#' Called from every trend constructor that accepts `trend_map`
#' so malformed input errors immediately, not at fit time. Defers
#' all data-dependent validation (dimension match, series-label
#' coverage, contiguity etc.) to `normalise_trend_map()`.
#'
#' @noRd
assert_trend_map_input <- function(input) {
  if (is.null(input)) return(invisible(NULL))
  checkmate::assert(
    checkmate::check_character(input, len = 1L),
    checkmate::check_matrix(input, mode = "numeric"),
    checkmate::check_data_frame(input),
    .var.name = "trend_map"
  )
  invisible(NULL)
}


#' @noRd
normalise_trend_map <- function(input, data) {
  if (is.null(input)) return(NULL)
  checkmate::assert_data_frame(data)
  series_levels <- if (is.factor(data$series)) {
    levels(data$series)
  } else if (!is.null(data$series)) {
    sort(unique(as.character(data$series)))
  } else {
    stop(insight::format_error(c(
      "trend_map requires a 'series' column on 'data'.",
      i = "Add a 'series' factor / character column to 'data'."
    )))
  }
  n_series <- length(series_levels)
  Z <- if (is.character(input)) {
    trend_map_from_character(input, n_series)
  } else if (is.data.frame(input)) {
    trend_map_from_dataframe(input, series_levels)
  } else if (is.matrix(input) && is.numeric(input)) {
    trend_map_from_matrix(input, n_series)
  } else {
    stop(insight::format_error(c(
      "'trend_map' must be a matrix, data.frame, or character code.",
      x = paste0("Got: ", class(input)[1L], "."),
      i = "See ?mvgam for accepted shapes."
    )))
  }
  rownames(Z) <- series_levels
  colnames(Z) <- paste0("trend_", seq_len(ncol(Z)))
  list(Z = Z, n_lv = ncol(Z))
}


# Character-code branch. Two recognised codes.
#'@noRd
trend_map_from_character <- function(input, n_series) {
  if (length(input) != 1L) {
    stop(insight::format_error(c(
      "Character 'trend_map' must be a single string.",
      x = paste0("Got length-", length(input), " vector.")
    )))
  }
  switch(
    input,
    identity = diag(1, nrow = n_series, ncol = n_series),
    shared = matrix(1, nrow = n_series, ncol = 1L),
    stop(insight::format_error(c(
      paste0("Unknown 'trend_map' code: '", input, "'."),
      i = "Accepted codes: 'identity', 'shared'."
    )))
  )
}


# Numeric-matrix branch. Validates shape + finite + no all-zero
# rows.
#'@noRd
trend_map_from_matrix <- function(input, n_series) {
  if (nrow(input) != n_series) {
    stop(insight::format_error(c(
      "'trend_map' matrix has the wrong number of rows.",
      x = paste0(
        "Expected ", n_series, " (one per series), got ",
        nrow(input), "."
      ),
      i = "Rows correspond to series; columns to latent factors."
    )))
  }
  # NA entries mark free (sampled) loadings under the partial-Z
  # surface; only Inf / NaN are now rejected as malformed.
  finite_or_na <- is.na(input) | is.finite(input)
  if (any(!finite_or_na)) {
    stop(insight::format_error(c(
      "'trend_map' matrix has Inf or NaN entries.",
      i = paste0(
        "Use NA to mark free (sampled) entries, or supply a ",
        "finite numeric loading to fix the entry."
      )
    )))
  }
  # Zero-loading rows mean a series is unmodelled by every
  # factor. A row of all NA is fine (every entry is free), but a
  # row of finite zeros (no NAs) with rowSums == 0 leaves the
  # series unmodelled.
  fixed_mask <- !is.na(input)
  fixed_only <- input
  fixed_only[!fixed_mask] <- 0
  row_has_any_free <- rowSums(!fixed_mask) > 0L
  zero_rows <- which(rowSums(abs(fixed_only)) == 0 & !row_has_any_free)
  if (length(zero_rows) > 0L) {
    stop(insight::format_error(c(
      "'trend_map' has zero-loading rows; those series are unmodelled.",
      x = paste0(
        "Rows with zero loadings: ",
        paste(zero_rows, collapse = ", "), "."
      ),
      i = paste0(
        "Every series must load on at least one factor (set a ",
        "finite non-zero loading or NA to sample the entry)."
      )
    )))
  }
  # Fully-free columns (every entry NA) are identified up to
  # sign by the Heaps post-hoc QR rotation emitted in
  # `generate_factor_model()`, which decomposes Z = Q_tilde'
  # Z_tilde and saves Z_tilde with a non-negative diagonal.
  # Post-fit accessors prefer Z_tilde over the raw Z, so no
  # warning about unfixed columns is needed here.
  input
}


# data.frame branch. Validates cols + factor-level coverage +
# contiguous trend integers; lifts to a binary 0/1 Z matrix.
#'@noRd
trend_map_from_dataframe <- function(input, series_levels) {
  validate_required_variables(
    input, c("series", "trend"), "trend_map"
  )
  s <- as.character(input$series)
  if (!all(series_levels %in% s)) {
    missing_series <- setdiff(series_levels, s)
    stop(insight::format_error(c(
      "'trend_map' must list every training series exactly once.",
      x = paste0(
        "Missing from trend_map: ",
        paste0("'", missing_series, "'", collapse = ", "), "."
      )
    )))
  }
  if (anyDuplicated(s)) {
    stop(insight::format_error(c(
      "'trend_map' contains duplicate series labels.",
      x = paste0(
        "Duplicated: ",
        paste0("'", unique(s[duplicated(s)]), "'", collapse = ", "),
        "."
      ),
      i = "Each series must map to exactly one trend."
    )))
  }
  unknown <- setdiff(s, series_levels)
  if (length(unknown) > 0L) {
    stop(insight::format_error(c(
      "'trend_map' references series not present in the training data.",
      x = paste0(
        "Unknown: ",
        paste0("'", unknown, "'", collapse = ", "), "."
      )
    )))
  }
  if (!is.numeric(input$trend) && !is.integer(input$trend)) {
    stop(insight::format_error(c(
      "'trend_map$trend' must be a numeric or integer column.",
      x = paste0("Got column of class ", class(input$trend)[1L], "."),
      i = "Use integer factor indices 1..K to assign series."
    )))
  }
  t <- as.integer(input$trend)
  if (anyNA(t) || any(t < 1L)) {
    stop(insight::format_error(c(
      "'trend_map$trend' must be positive integers.",
      x = paste0(
        "Got: ",
        paste0(input$trend, collapse = ", "), "."
      )
    )))
  }
  max_t <- max(t)
  if (!setequal(unique(t), seq_len(max_t))) {
    stop(insight::format_error(c(
      "'trend_map$trend' must be a contiguous integer sequence 1..K.",
      x = paste0(
        "Got: ",
        paste0(sort(unique(t)), collapse = ", "), "."
      ),
      i = paste0(
        "Latent factors are indexed 1..K with no gaps; ",
        "renumber the mapping if needed."
      )
    )))
  }
  if (max_t > length(series_levels)) {
    stop(insight::format_error(c(
      paste0(
        "'trend_map$trend' has more factors than series (",
        max_t, " > ", length(series_levels), ")."
      ),
      i = "max(trend) must not exceed the number of series."
    )))
  }
  # Reorder rows to match series_levels so Z[s, ] aligns with
  # the canonical series order.
  ord <- match(series_levels, s)
  Z <- matrix(0, nrow = length(series_levels), ncol = max_t)
  Z[cbind(seq_along(series_levels), t[ord])] <- 1
  Z
}


#' Apply Rule-Based Validation Dispatch
#'
#' @description
#' Applies validation rules specified in trend objects to automatically
#' dispatch to appropriate validation functions. Makes adding new trends
#' trivial - just specify validation rules in constructor.
#'
#' @param trend_specs Trend specification(s)
#' @param data Data frame with time series data
#' @return Enhanced trend specs with processed validation
#' @noRd
apply_validation_rules <- function(trend_specs, data) {

  # Handle univariate vs multivariate specs
  if (is_multivariate_trend_specs(trend_specs)) {
    # Process each response specification
    for (response_name in names(trend_specs)) {
      trend_specs[[response_name]] <- process_trend_validation_rules(
        trend_specs[[response_name]], data
      )
    }
  } else {
    # Process single trend specification
    trend_specs <- process_trend_validation_rules(trend_specs, data)
  }

  return(trend_specs)
}

#' Process Validation Rules for Single Trend
#'
#' @description
#' Processes validation rules for a single trend specification using
#' rule-based dispatch.
#'
#' @param trend_spec Single trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
process_trend_validation_rules <- function(trend_spec, data) {

  # Get validation rules from trend object
  validation_rules <- trend_spec$validation_rules %||% character(0)

  # Apply each validation rule
  for (rule in validation_rules) {
    trend_spec <- dispatch_validation_rule(rule, trend_spec, data)
  }

  return(trend_spec)
}

#' Dispatch Single Validation Rule
#'
#' @description
#' Dispatches a single validation rule to the appropriate validation function.
#' Uses rule-to-function mapping for clean, extensible architecture.
#'
#' @param rule Validation rule name
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
dispatch_validation_rule <- function(rule, trend_spec, data) {

  # Rule-to-function mapping
  rule_functions <- get_validation_rule_dispatch_table()

  if (!rule %in% names(rule_functions)) {
    warning(paste("Unknown validation rule:", rule), call. = FALSE)
    return(trend_spec)
  }

  # Call the appropriate validation function
  validation_function <- rule_functions[[rule]]
  result <- validation_function(trend_spec, data)

  return(result)
}

#' Get Validation Rule Dispatch Table
#'
#' @description
#' Returns mapping from validation rule names to validation functions.
#' Central dispatch table for rule-based validation.
#'
#' @return Named list mapping rules to functions
#' @noRd
get_validation_rule_dispatch_table <- function() {
  list(
    "requires_grouping_validation" = validate_trend_grouping,
    "requires_regular_intervals" = validate_trend_time_intervals,
    "supports_factors" = validate_trend_factor_compatibility,
    "requires_parameter_processing" = validate_and_process_trend_parameters
  )
}

#' Validate Trend Grouping
#'
#' @description
#' Validates and processes grouping arguments for trends that support them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_grouping <- function(trend_spec, data, cached_formulas = NULL) {

  # Process grouping arguments if present
  if (!is.null(trend_spec$gr) && trend_spec$gr != 'NA') {
    groupings <- validate_grouping_arguments(trend_spec$gr, trend_spec$subgr)
    trend_spec$gr <- groupings$gr
    trend_spec$subgr <- groupings$subgr

    # Validate grouping variables exist in data
    required_vars <- trend_spec$gr
    if (!is.null(trend_spec$subgr) && trend_spec$subgr != 'NA') {
      required_vars <- c(required_vars, trend_spec$subgr)
    }

    # Filter variables using cached formula metadata
    formula_to_use <- if (!is.null(cached_formulas)) cached_formulas$formula else NULL
    filtered_vars <- filter_required_variables(required_vars, formula_to_use)
    validate_required_variables(data, filtered_vars, "grouping data")

    # Each series must belong to a single group. If gr varies within a
    # series, mvgam silently picks the first row's value and Stan fails
    # at init with non-finite gradients; better to flag it here.
    validate_gr_constant_per_series(trend_spec, data)
  }

  return(trend_spec)
}


#' Validate Hierarchical Groups Are Balanced
#'
#' Hierarchical trend codegen currently sizes per-group cholesky and
#' sigma blocks by `max(series-per-group)` and fills only the first
#' `k` entries of a fixed-size innovation vector per group. Tail
#' entries stay uninitialised and produce NaN at Stan init for
#' unbalanced designs. Fail-fast with a clear message until ragged
#' array support is implemented.
#'
#' @param trend_spec Trend specification with `gr`, `subgr` and
#'   (optionally) `series` fields.
#' @param data Data frame containing `gr` and series columns.
#' @return Invisibly NULL; called for its side-effect.
#' @noRd
validate_gr_balanced_groups <- function(trend_spec, data) {
  gr_var <- trend_spec$gr
  series_var <- trend_spec$series %||% "series"

  # Factor-model subgr (e.g. subgr = "site") drives a path where
  # N_subgroups is set explicitly and series-per-group balance is not
  # derived. The auto-filled subgr = "series" (matching the default
  # series variable) is the canonical series-level path and must still
  # be checked.
  subgr <- trend_spec$subgr
  user_supplied_subgr <- !is.null(subgr) &&
    !identical(subgr, "NA") &&
    !identical(subgr, series_var)
  if (user_supplied_subgr) {
    return(invisible(NULL))
  }

  if (!series_var %in% colnames(data) || !gr_var %in% colnames(data)) {
    return(invisible(NULL))
  }

  unique_series_rows <- data[!duplicated(data[[series_var]]), , drop = FALSE]
  series_group_table <- table(unique_series_rows[[gr_var]])
  group_counts <- as.integer(series_group_table)

  if (length(unique(group_counts)) <= 1L) {
    return(invisible(NULL))
  }

  counts_str <- paste(
    paste0(names(series_group_table), "=", group_counts),
    collapse = ", "
  )
  stop(insight::format_error(c(
    paste0(
      "Hierarchical trend models currently require equal ",
      "series-per-group counts."
    ),
    x = paste0(
      "Grouping variable '", gr_var,
      "' has unbalanced groups: ", counts_str, "."
    ),
    i = paste0(
      "Subset the data to a balanced design, or combine small ",
      "groups. Support for unbalanced groups is planned."
    )
  )))
}


#' Validate Grouping Variable is Constant Within Each Series
#'
#' Series-level hierarchical models map each series to a single group.
#' If `gr` varies across rows of the same series, the model is incoherent.
#'
#' @param trend_spec Trend specification with `gr` and (optionally)
#'   `series` fields.
#' @param data Data frame containing `gr` and series columns.
#' @return Invisibly NULL; called for its side-effect (errors on
#'   inconsistent series).
#' @noRd
validate_gr_constant_per_series <- function(trend_spec, data) {
  gr_var <- trend_spec$gr
  series_var <- trend_spec$series %||% "series"

  if (!series_var %in% colnames(data)) {
    return(invisible(NULL))
  }
  if (!gr_var %in% colnames(data)) {
    return(invisible(NULL))
  }

  series_vec <- data[[series_var]]
  gr_vec <- data[[gr_var]]
  counts <- vapply(
    split(gr_vec, series_vec),
    function(x) length(unique(x[!is.na(x)])),
    integer(1)
  )
  bad_series <- names(counts)[counts > 1]
  if (length(bad_series) > 0) {
    shown <- utils::head(bad_series, 5)
    tail_msg <- if (length(bad_series) > 5) {
      paste0(" (and ", length(bad_series) - 5, " more)")
    } else {
      ""
    }
    stop(insight::format_error(c(
      paste0("Grouping variable '", gr_var,
             "' is not constant within each series."),
      x = paste0("Inconsistent series: ",
                 paste(shown, collapse = ", "), tail_msg, "."),
      i = "Each series must belong to a single group."
    )))
  }

  invisible(NULL)
}

#' Validate Trend Time Intervals
#'
#' @description
#' Validates regular time intervals for trends that require them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_time_intervals <- function(trend_spec, data) {

  # Extract time variable
  time_var <- trend_spec$time %||% "time"

  if (time_var %in% colnames(data)) {
    # Use existing validation function
    validate_regular_time_intervals(data[[time_var]], time_var)
  }

  return(trend_spec)
}

#' Validate Trend Factor Compatibility
#'
#' @description
#' Validates factor model requirements for trends that support them.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification
#' @noRd
validate_trend_factor_compatibility <- function(trend_spec, data) {

  # Validate factor model requirements
  if (!is.null(trend_spec$n_lv)) {
    # Get series count from data
    series_var <- trend_spec$series %||% "series"
    if (series_var %in% colnames(data)) {
      n_series <- length(unique(data[[series_var]]))

      if (trend_spec$n_lv >= n_series) {
        stop(insight::format_error(c(
          cli::format_inline("Factor model requires {.field n_lv < n_series}."),
          x = cli::format_inline(
            "You specified {.field n_lv = {trend_spec$n_lv}} but data has {n_series} series."
          ),
          i = "Reduce n_lv or increase number of series."
        )))
      }
    }
  }

  return(trend_spec)
}

#' Validate Factor Compatibility
#'
#' @description
#' Validates that a trend specification is compatible with factor models.
#' Uses the trend registry to check factor support.
#'
#' @param trend_spec List with trend specification including trend_model name
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_factor_compatibility <- function(trend_spec) {
  checkmate::assert_list(trend_spec)

  if (is.null(trend_spec$n_lv) || trend_spec$n_lv == 0) {
    return(invisible(TRUE))  # No factor model requested
  }

  trend_name <- trend_spec$trend_model %||% "Unknown"

  # Check if trend type is registered
  if (!exists(trend_name, envir = trend_registry)) {
    stop(insight::format_error(c(
      cli::format_inline("Unknown trend type: {.val {trend_name}}"),
      i = cli::format_inline(
        "Available types: {paste(ls(trend_registry), collapse = ', ')}"
      )
    )))
  }

  # Get trend info from registry
  trend_info <- get(trend_name, envir = trend_registry)

  # Check factor support
  if (!trend_info$supports_factors) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Factor models (n_lv > 0) not supported for {trend_name} trends."
      ),
      x = trend_info$incompatibility_reason
    )))
  }

  invisible(TRUE)
}

#' Validate Grouping Arguments
#'
#' @description
#' Validates hierarchical grouping structure and returns processed arguments.
#'
#' @param gr Grouping variable name or NULL/'NA'
#' @param subgr Subgrouping variable name or NULL/'NA'
#' @return List with processed gr and subgr values
#' @noRd
validate_grouping_arguments <- function(gr, subgr) {
  # Process gr argument
  if (is.null(gr) || is.na(gr) || gr == "NA") {
    gr <- NULL
  } else {
    checkmate::assert_string(gr, min.chars = 1)
  }

  # Process subgr argument
  if (is.null(subgr) || is.na(subgr) || subgr == "NA") {
    subgr <- NULL
  } else {
    checkmate::assert_string(subgr, min.chars = 1)
  }

  # Auto-fill subgr to the default 'series' when only gr is supplied:
  # the hierarchical codegen path derives subgroups from the existing
  # series column (see generate_hierarchical_correlation_data() in
  # stan_assembly.R), so a bare ZMVN(gr = X) is well-defined as long as
  # the data carries a series variable. Users who want a different
  # within-group identifier still pass subgr explicitly.
  if (!is.null(gr) && is.null(subgr)) {
    subgr <- "series"
  }

  if (!is.null(subgr) && is.null(gr)) {
    stop(insight::format_error(c(
      "Subgrouping requires main grouping variable.",
      x = cli::format_inline(
        "Cannot specify {.field subgr = {subgr}} without {.field gr}."
      )
    )))
  }

  return(list(gr = gr, subgr = subgr))
}


#' Validate Regular Time Intervals
#'
#' @description
#' Validates that time series has regular intervals for trends that require it.
#'
#' @param time_values Vector of time values
#' @param time_var Name of time variable (for error messages)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_regular_time_intervals <- function(time_values, time_var = "time") {
  checkmate::assert_numeric(time_values, min.len = 2)

  # Calculate intervals between consecutive time points
  intervals <- diff(sort(unique(time_values)))

  # Check for regular intervals (allowing small numerical tolerance)
  tolerance <- 1e-10
  interval_range <- range(intervals)
  is_regular <- abs(interval_range[2] - interval_range[1]) < tolerance

  if (!is_regular) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Irregular time intervals detected in {.field {time_var}}."
      ),
      x = "Some trends require regular time spacing.",
      x = cli::format_inline(
        "Interval range: {min(intervals)} to {max(intervals)}"
      ),
      i = "Consider using CAR() for irregular intervals or interpolate data."
    )))
  }


  invisible(TRUE)
}

#' Utility function equivalent to base::deparse0
#'
#' @description
#' Provides deparse0 functionality for compatibility with older R versions.
#'
#' @param expr Expression to deparse
#' @param ... Additional arguments passed to deparse
#' @return Character string representation of the expression
#' @noRd
deparse0 <- function(expr, ...) {
  paste(deparse(expr, ...), collapse = "")
}

#' Check if Formula is Nonlinear
#'
#' @description
#' Determines if a brms formula specifies a nonlinear model.
#'
#' @param formula brms formula object
#' @return Logical indicating if formula is nonlinear
#' @noRd
is_nonlinear_formula <- function(formula) {
  checkmate::assert(inherits(formula, c("formula", "brmsformula", "brmsterms")))

  # Check for brms bf() structure with nl = TRUE
  if (inherits(formula, "brmsterms")) {
    return(formula$nl)
  }

  if (inherits(formula, "brmsformula")) {
    return(attr(formula$formula, "nl") %||% FALSE)
  }

  # Check formula structure for nonlinear indicators
  formula_str <- deparse(formula)

  # Look for bf() with nl = TRUE
  has_nl_true <- grepl("nl\\s*=\\s*TRUE", formula_str, ignore.case = TRUE)

  # Look for nonlinear parameter specifications
  has_nl_params <- grepl("\\b[a-zA-Z]+\\s*~", formula_str) &&
                   grepl("\\bnl\\s*=", formula_str)

  return(has_nl_true || has_nl_params)
}

#' @noRd
validate_brms_formula <- function(formula) {
  issues <- character(0)

  # Check if formula is NULL
  if (is.null(formula)) {
    return(list(valid = FALSE, issues = "Formula cannot be NULL"))
  }

  # Check formula class
  valid_classes <- c("formula", "brmsformula", "bform")
  if (!any(sapply(valid_classes, function(cls) inherits(formula, cls)))) {
    issues <- c(issues, paste(
      "Formula must be of class:", paste(valid_classes, collapse = ", "),
      "but got:", class(formula)[1]
    ))
  }

  # Try to validate with existing brms validation function
  validation_result <- try({
    validate_obs_formula_brms(formula)
    NULL  # No issues if validation succeeds
  }, silent = TRUE)

  if (inherits(validation_result, "try-error")) {
    error_msg <- attr(validation_result, "condition")$message
    issues <- c(issues, paste("brms validation failed:", error_msg))
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues
  ))
}

# Utility function to extract formula string using brms pattern
formula2str_mvgam <- function(formula, space = "trim") {
  if (is.null(formula)) {
    return(NULL)
  }

  # Handle complex brms formula objects (bf, distributional, nonlinear)
  if (inherits(formula, c("brmsformula", "bform"))) {
    # Extract all formula components for comprehensive string representation
    formula_strings <- character(0)

    # Main formula
    if (!is.null(formula$formula)) {
      main_str <- deparse(formula$formula)
      formula_strings <- c(formula_strings, main_str)
    }

    # Distributional parameter formulas (sigma, nu, phi, etc.)
    if (!is.null(formula$pforms)) {
      pform_strings <- sapply(formula$pforms, function(pf) {
        if (!is.null(pf$formula)) {
          deparse(pf$formula)
        } else {
          deparse(pf)
        }
      })
      formula_strings <- c(formula_strings, pform_strings)
    }

    # Nonlinear parameter formulas
    if (!is.null(formula$nlpars)) {
      nlpar_strings <- sapply(formula$nlpars, function(nlp) {
        if (!is.null(nlp$formula)) {
          deparse(nlp$formula)
        } else {
          deparse(nlp)
        }
      })
      formula_strings <- c(formula_strings, nlpar_strings)
    }

    # If we couldn't extract components, use the whole object
    if (length(formula_strings) == 0) {
      formula_strings <- deparse(formula)
    }

    # Combine all components
    x <- paste(formula_strings, collapse = " ")
  } else {
    # Standard formula handling
    formula <- as.formula(formula)
    x <- Reduce(paste, deparse(formula))
  }

  # Clean up whitespace
  x <- gsub("[\t\r\n]+", " ", x, perl = TRUE)

  if (space == "trim") {
    x <- trimws(x)  # Use base R trimws for simplicity
  }

  return(x)
}

#' Collect function-call symbol names from an unevaluated R expression
#'
#' Recursively walks a `call`/`language` tree and returns the function
#' names at each call site. Used by formula validators that need to
#' detect specific function calls without false-positives from
#' substring matches in deparsed text (e.g. distinguishing the brms
#' `se()` addition term from a user variable named `se_x` or
#' `defense`).
#'
#' Handles namespace-qualified calls (`pkg::fun(x)` returns `fun`).
#' Returns `character(0)` for symbols, literals, and `NULL`.
#'
#' @param expr An unevaluated R expression (the kind you get from
#'   indexing a formula, e.g. `formula[[length(formula)]]`).
#' @return A character vector of call-site function names, in
#'   left-to-right depth-first order. May contain duplicates.
#' @noRd
collect_call_names <- function(expr) {
  if (!is.call(expr)) {
    return(character(0L))
  }
  head <- expr[[1L]]
  head_name <- if (is.symbol(head)) {
    as.character(head)
  } else if (is.call(head) && identical(head[[1L]], as.name("::"))) {
    as.character(head[[3L]])
  } else {
    character(0L)
  }
  arg_names <- unlist(
    lapply(as.list(expr)[-1L], collect_call_names),
    use.names = FALSE
  )
  c(head_name, arg_names)
}

#' Function-call names in the right-hand side of a formula
#'
#' Dispatches on the formula class so plain `formula`, `brmsformula`,
#' `bform`, and `mvbrmsformula` inputs all return the function calls
#' appearing in their predictor expressions. For `brmsformula` /
#' `bform` we walk the main RHS plus every distributional-parameter
#' (`pforms`) and non-linear-parameter (`nlpars`) sub-formula. For
#' `mvbrmsformula` we recurse over each response sub-formula.
#'
#' The LHS of a formula (response, brms addition-terms like
#' `y | cens(c)`) is deliberately not walked: validators that care
#' about LHS specials handle them separately via the parsed
#' `brmsterms()` `$adforms` slot.
#'
#' @param x A formula, brmsformula, bform, or mvbrmsformula.
#' @return Unique character vector of function-call names appearing on
#'   the RHS. Empty character vector if `x` is `NULL` or no calls.
#' @noRd
formula_rhs_function_names <- function(x) {
  if (is.null(x)) {
    return(character(0L))
  }
  if (inherits(x, "mvbrmsformula")) {
    return(unique(unlist(
      lapply(x$forms, formula_rhs_function_names),
      use.names = FALSE
    )))
  }
  if (inherits(x, c("brmsformula", "bform"))) {
    parts <- list(
      if (!is.null(x$formula)) formula_rhs_function_names(x$formula),
      if (!is.null(x$pforms)) {
        unlist(lapply(x$pforms, formula_rhs_function_names),
               use.names = FALSE)
      },
      if (!is.null(x$nlpars)) {
        unlist(lapply(x$nlpars, formula_rhs_function_names),
               use.names = FALSE)
      }
    )
    return(unique(unlist(parts, use.names = FALSE)))
  }
  if (inherits(x, "formula")) {
    rhs <- x[[length(x)]]
    return(unique(collect_call_names(rhs)))
  }
  character(0L)
}

#' Get Dynamic Trend Validation Patterns
#'
#' @description
#' Dynamically generates regex patterns for detecting trend constructors
#' based on the current trend registry. This ensures validation works
#' with custom trend types without hard-coding patterns.
#'
#' @return Named character vector of regex patterns
#' @noRd
get_trend_validation_patterns <- function() {

  # Access trend registry from mvgam namespace
  trend_registry <- get("trend_registry", envir = asNamespace("mvgam"))
  trend_types <- ls(trend_registry)

  # Generate patterns dynamically
  patterns <- character(length(trend_types))
  names(patterns) <- paste0("\\b", trend_types, "\\s*\\(")

  # Create the values (display names)
  for (i in seq_along(trend_types)) {
    patterns[i] <- paste0(trend_types[i], "()")
  }

  return(patterns)
}

#' Validate observation formula excludes mvgam trend constructors
#'
#' Validate that all gp() terms use approximate GP (have k parameter)
#' 
#' @param formula A formula object
#' @noRd
validate_exact_gp_usage <- function(formula) {
  # Input validation (required by CLAUDE.md standards)
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_null(formula),
    .var.name = "formula"
  )
  
  if (is.null(formula)) return(invisible(NULL))
  
  # Handle different formula types
  if (inherits(formula, "brmsformula")) {
    # For brmsformula, extract the main formula component
    main_formula <- formula$formula
  } else if (inherits(formula, "mvbrmsformula")) {
    # For multivariate brmsformula, check all response formulas
    all_formulas <- formula$forms
    for (form in all_formulas) {
      if (!is.null(form$formula)) {
        validate_exact_gp_usage(form$formula)
      }
    }
    return(invisible(NULL))
  } else {
    # Regular formula object
    main_formula <- formula
  }
  
  # Get term labels from formula
  termlabs <- attr(terms.formula(main_formula, keep.order = TRUE), 
                   "term.labels")
  which_gp <- grep("gp(", termlabs, fixed = TRUE)
  
  if (length(which_gp) == 0) return(invisible(NULL))
  
  # Check each gp() term with error handling
  for (j in seq_along(which_gp)) {
    gp_term <- termlabs[which_gp[j]]
    
    # Safe evaluation with error handling
    gp_obj <- try({
      eval(parse(text = gp_term))
    }, silent = TRUE)
    
    if (inherits(gp_obj, "try-error")) {
      stop(insight::format_error(c(
        cli::format_inline("Invalid GP term syntax: {.field {gp_term}}"),
        i = "GP terms must be valid function calls"
      )))
    }

    if (is.na(gp_obj$k)) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Exact GP terms (without {.field k} parameter) are not supported."
        ),
        x = paste("Found:", gp_term),
        i = cli::format_inline(
          "Add {.field k} to specify number of basis functions."
        ),
        i = paste("Example:", gsub("\\)$", ", k=20)", gp_term))
      )))
    }
  }
  
  return(invisible(NULL))
}

#' Main validation for observation formulas - ensures they're clean for brms
#' processing by checking they don't contain mvgam trend constructors.
#'
#' @param formula Observation formula (formula, brmsformula, or bform)
#' @return The original formula unchanged (brms handles processing)
#' @noRd
validate_obs_formula_brms <- function(formula) {
  if (is.null(formula)) return(NULL)

  # Accept any brms-compatible formula class
  checkmate::assert(
    inherits(formula, "formula") ||
    inherits(formula, "brmsformula") ||
    inherits(formula, "bform"),
    .var.name = "formula"
  )

  # Extract string representation for pattern matching
  formula_str <- formula2str_mvgam(formula)

  # Check for mvgam trend constructors using dynamic registry lookup
  trend_patterns <- get_trend_validation_patterns()

  detected_trends <- character(0)
  for (pattern in names(trend_patterns)) {
    if (grepl(pattern, formula_str, perl = TRUE)) {
      detected_trends <- c(detected_trends, trend_patterns[[pattern]])
    }
  }

  if (length(detected_trends) > 0) {
    stop(insight::format_error(c(
      cli::format_inline(
        "mvgam trend constructors found in observation {.field formula}:"
      ),
      x = paste("Found:", paste(unique(detected_trends), collapse = ", ")),
      i = cli::format_inline(
        "Trend constructors belong in {.field trend_formula}, not {.field formula}."
      ),
      i = cli::format_inline(
        "Use: {.code mvgam(y ~ x, trend_formula = ~ RW())}"
      )
    )))
  }

  # Check for exact GP usage (gp() without k parameter)
  validate_exact_gp_usage(formula)

  # Return original formula unchanged - brms handles all other validation
  return(formula)
}

#' Validate trend formula for State-Space compatibility
#'
#' Validates trend formulas supporting bf() objects, named lists, and single
#' formulas. Ensures compatibility with mvgam State-Space dynamics.
#'
#' @param trend_formula Trend specification (formula, bf object, or named list)
#' @return The validated trend formula
#' @noRd
validate_trend_formula_brms <- function(trend_formula) {
  if (is.null(trend_formula)) return(NULL)

  # Handle bf() objects for multivariate trend specifications
  if (inherits(trend_formula, c("brmsformula", "bform"))) {
    return(validate_bf_trend_formula(trend_formula))
  }

  # Handle named list for multivariate (alternative to bf())
  if (is.list(trend_formula) && !inherits(trend_formula, "formula")) {
    return(validate_list_trend_formula(trend_formula))
  }

  # Single trend formula validation
  if (inherits(trend_formula, "formula")) {
    return(validate_single_trend_formula(trend_formula))
  }

  # Invalid type
  stop(insight::format_error(c(
    cli::format_inline(
      "Invalid {.field trend_formula} type: {class(trend_formula)}"
    ),
    i = "Must be formula, bf() object, or named list."
  )))
}

#' Validate bf() trend formula objects
#'
#' @param bf_obj A brmsformula or bform object
#' @noRd
validate_bf_trend_formula <- function(bf_obj) {
  checkmate::assert_class(bf_obj, c("brmsformula", "bform"))

  # For bf() objects in trend context, response variables are allowed
  # because they identify which trend belongs to which response

  # Extract and validate all formula components from bf() object
  all_formulas <- extract_all_bf_formulas(bf_obj)

  # Validate each formula component
  for (i in seq_along(all_formulas)) {
    formula_component <- all_formulas[[i]]
    context_name <- names(all_formulas)[i] %||% paste("bf() component", i)

    if (inherits(formula_component, "formula")) {
      validate_single_trend_formula(
        formula_component,
        context = context_name,
        allow_response = TRUE
      )
    }
  }

  return(bf_obj)
}

#' Extract all formula components from a bf() object
#'
#' Helper function to comprehensively extract all formulas from brmsformula objects
#' for validation purposes.
#'
#' @param bf_obj A brmsformula or bform object
#' @return Named list of all formula components
#' @noRd
extract_all_bf_formulas <- function(bf_obj) {
  formulas <- list()

  # Main formula
  if (!is.null(bf_obj$formula)) {
    formulas[["main"]] <- bf_obj$formula
  }

  # Distributional parameter formulas (pforms)
  if (!is.null(bf_obj$pforms)) {
    for (i in seq_along(bf_obj$pforms)) {
      pform <- bf_obj$pforms[[i]]
      param_name <- names(bf_obj$pforms)[i] %||% paste("pform", i)

      if (!is.null(pform$formula)) {
        formulas[[paste("pform", param_name)]] <- pform$formula
      } else if (inherits(pform, "formula")) {
        formulas[[paste("pform", param_name)]] <- pform
      }
    }
  }

  # Nonlinear parameter formulas (nlpars)
  if (!is.null(bf_obj$nlpars)) {
    for (i in seq_along(bf_obj$nlpars)) {
      nlpar <- bf_obj$nlpars[[i]]
      param_name <- names(bf_obj$nlpars)[i] %||% paste("nlpar", i)

      if (!is.null(nlpar$formula)) {
        formulas[[paste("nlpar", param_name)]] <- nlpar$formula
      } else if (inherits(nlpar, "formula")) {
        formulas[[paste("nlpar", param_name)]] <- nlpar
      }
    }
  }

  # Additional formulas (if any other slots exist)
  # This is a fallback for any other formula-containing slots
  other_slots <- setdiff(names(bf_obj), c("formula", "pforms", "nlpars", "family", "autocor", "loop"))
  for (slot_name in other_slots) {
    slot_content <- bf_obj[[slot_name]]
    if (inherits(slot_content, "formula")) {
      formulas[[paste("other", slot_name)]] <- slot_content
    }
  }

  return(formulas)
}

#' Validate named list trend formula
#'
#' @param formula_list Named list of formulas
#' @noRd
validate_list_trend_formula <- function(formula_list) {
  if (is.null(names(formula_list))) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Multivariate {.field trend_formula} must be a named list."
      ),
      i = cli::format_inline(
        "Use: {.code trend_formula = list(resp1 = ~ AR(), resp2 = ~ RW())}"
      )
    )))
  }

  # Validate each component formula
  validated_list <- lapply(names(formula_list), function(name) {
    if (is.null(formula_list[[name]])) {
      return(NULL)  # Allow NULL for responses without trends
    }
    validate_single_trend_formula(formula_list[[name]], context = paste("response", name))
  })
  names(validated_list) <- names(formula_list)

  return(validated_list)
}

#' Validate a single trend formula
#'
#' @param formula Single trend formula
#' @param context Optional context for error messages
#' @param allow_response Logical; whether to allow response variables (TRUE for multivariate identification)
#' @noRd
validate_single_trend_formula <- function(formula, context = NULL, allow_response = FALSE) {
  if (is.null(formula)) return(NULL)

  checkmate::assert_class(formula, "formula")

  # Check for response variable handling
  if (length(formula) == 3) {
    if (!allow_response) {
      context_msg <- if (!is.null(context)) paste("in", context) else ""
      stop(insight::format_error(c(
        cli::format_inline(
          "Trend formula {context_msg} should not have a response variable."
        ),
        i = cli::format_inline(
          "Use: {.code trend_formula = ~ RW()}, not {.code trend_formula = y ~ RW()}"
        ),
        i = cli::format_inline(
          "For multivariate models, use: {.code trend_formula = bf(y1 ~ AR(), y2 ~ RW())}"
        )
      )))
    }
    # If response variables are allowed, continue with validation but note it's for multivariate
  }

  # Extract string for validation
  formula_str <- formula2str_mvgam(formula)

  # Validate trend formula restrictions
  validate_trend_formula_restrictions(formula_str,
                                     c("offsets", "brms_autocor", "addition_terms", "multiple_constructors"),
                                     formula)

  # Check for exact GP usage (gp() without k parameter)
  validate_exact_gp_usage(formula)

  return(formula)
}


#' Validate trend formula restrictions
#'
#' Validates that trend formulas do not contain prohibited patterns that would
#' conflict with State-Space dynamics or observation model behavior.
#'
#' @param formula_str String representation of trend formula
#' @param restrictions Character vector of restriction types to check
#' @param formula Original formula object (for offset checking)
#' @noRd
validate_trend_formula_restrictions <- function(formula_str,
                                               restrictions = c("brms_autocor", "addition_terms", "multiple_constructors", "offsets"),
                                               formula = NULL) {
  checkmate::assert_string(formula_str)
  restrictions <- match.arg(restrictions, several.ok = TRUE)

  # Define restriction patterns and their error messages
  restriction_configs <- list(
    "brms_autocor" = list(
      patterns = c(
        "\\bar\\s*\\(" = "ar()",
        "\\bma\\s*\\(" = "ma()",
        "\\barma\\s*\\(" = "arma()",
        "\\bcosy\\s*\\(" = "cosy()",
        "\\bunstr\\s*\\(" = "unstr()",
        "\\bautocor\\s*\\(" = "autocor()"
      ),
      error_header = "brms autocorrelation terms not allowed in {.field trend_formula}:",
      error_reason = "These conflict with mvgam State-Space dynamics.",
      error_suggestion = "Use mvgam trend types instead: {.code ar(p = 1)} → {.code AR(p = 1)}"
    ),

    "addition_terms" = list(
      # brms `formula_ad` specials that modify the *observation model*
      # and therefore have no defined meaning on a latent State-Space
      # trend. `mi` is deliberately absent: per the design intent of
      # GH issue #109 item 12, missing-predictor imputation is allowed
      # on the latent scale; the obs-side rejection of `mi()` as a
      # predictor lives in `validate_obs_formula_brms`. Detection walks
      # the formula AST (see `formula_rhs_function_names`) rather than
      # grepping the deparsed string so variable names like `defense`
      # or `se_x` cannot false-positive.
      patterns = function(formula_str, formula = NULL) {
        if (is.null(formula)) return(character(0L))
        addition_terms <- c(
          "weights", "se", "cens", "trunc", "trials", "rate",
          "vreal", "vint", "subset", "index", "dec", "cat",
          "thres", "cov_ranef"
        )
        rhs_calls <- formula_rhs_function_names(formula)
        hit <- intersect(rhs_calls, addition_terms)
        if (length(hit) > 0L) {
          structure(paste0(hit, "()"), names = rep("detected", length(hit)))
        } else character(0L)
      },
      error_header = "brms addition-terms not allowed in {.field trend_formula}:",
      error_reason = "These terms modify observation model behavior, not State-Space dynamics.",
      error_suggestion = "Include these terms in the observation {.field formula} instead."
    ),

    "multiple_constructors" = list(
      patterns = function(formula_str) {
        trend_patterns <- get_trend_validation_patterns()
        detected_trends <- character(0)
        for (pattern in names(trend_patterns)) {
          if (grepl(pattern, formula_str, perl = TRUE)) {
            detected_trends <- c(detected_trends, trend_patterns[[pattern]])
          }
        }
        if (length(detected_trends) > 1) {
          structure(detected_trends, names = rep("detected", length(detected_trends)))
        } else character(0)
      },
      error_header = "Multiple trend constructors found in single {.field trend_formula}:",
      error_reason = "Each trend formula must contain exactly one trend constructor.",
      error_suggestion = c("For multiple trends, use response-specific formulas:",
                          "{.code trend_formula = list(y1 = ~ AR(), y2 = ~ RW())}")
    ),

    "offsets" = list(
      patterns = function(formula_str, formula = NULL) {
        has_offset_function <- grepl("\\boffset\\s*\\(", formula_str, perl = TRUE)
        has_offset_attr <- FALSE
        if (!is.null(formula) && inherits(formula, "formula")) {
          terms_obj <- terms(formula)
          has_offset_attr <- !is.null(attr(terms_obj, 'offset'))
        }
        if (has_offset_function || has_offset_attr) {
          structure("offset()", names = "detected")
        } else character(0)
      },
      error_header = "Offset terms not allowed in {.field trend_formula}.",
      error_reason = "Offsets interfere with State-Space dynamics.",
      error_suggestion = c("Include offsets in the main observation {.field formula} instead:",
                          "Use {.code formula = y ~ x + offset(log_exposure)} not {.code trend_formula = ~ RW() + offset(z)}")
    )
  )

  # Check each restriction
  for (restriction in restrictions) {
    config <- restriction_configs[[restriction]]

    # Detect violations. `offsets` and `addition_terms` both need the
    # original formula object: `offsets` uses `terms(formula)` to find
    # the offset attribute, `addition_terms` walks the AST via
    # `formula_rhs_function_names()` to avoid false-positives from
    # variable names that share a substring with an addition-term name.
    if (is.function(config$patterns)) {
      if (restriction %in% c("offsets", "addition_terms")) {
        detected <- config$patterns(formula_str, formula)
      } else {
        detected <- config$patterns(formula_str)
      }
    } else {
      detected <- character(0)
      for (pattern in names(config$patterns)) {
        if (grepl(pattern, formula_str, perl = TRUE)) {
          detected <- c(detected, config$patterns[[pattern]])
        }
      }
    }

    # Generate error if violations found
    if (length(detected) > 0) {
      if (restriction == "multiple_constructors") {
        found_text <- paste("Found:", paste(unique(detected), collapse = " + "))
      } else {
        found_text <- paste("Found:", paste(unique(detected), collapse = ", "))
      }

      stop(insight::format_error(c(
        cli::format_inline(config$error_header),
        x = found_text,
        x = config$error_reason,
        i = cli::format_inline(config$error_suggestion)
      )))
    }
  }

  return(invisible(NULL))
}

#'
#' Ensures that multivariate models with separate trends per response only use
#' basic temporal dynamics without advanced features (factors, correlations, groupings).
#'
#' @param trend_formula Formula for a single response trend
#' @param response_name Name of the response variable
#' @noRd
validate_multivariate_trend_constraints <- function(trend_formula, response_name) {
  checkmate::assert_formula(trend_formula)
  checkmate::assert_string(response_name)

  # Parse the trend formula to extract trend constructors
  parsed <- try(mvgam:::parse_trend_formula(trend_formula), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    return(invisible(NULL))  # Let parse_trend_formula handle the error
  }

  # Check each trend component for advanced features
  for (trend_component in parsed$trend_components) {
    # Check for factor models (n_lv parameter)
    if (!is.null(trend_component$n_lv) && trend_component$n_lv > 0) {
      stop(insight::format_error(c(
        "Factor models not allowed in multivariate response trends.",
        x = cli::format_inline(
          "Response {.val {response_name}} has n_lv = {trend_component$n_lv}."
        ),
        i = "Remove n_lv parameter for basic temporal dynamics only."
      )))
    }

    # Check for correlations (cor parameter)
    if (!is.null(trend_component$cor) && trend_component$cor) {
      stop(insight::format_error(c(
        "Correlation structures not allowed in multivariate response trends.",
        x = cli::format_inline(
          "Response {.val {response_name}} has cor = TRUE."
        ),
        i = "Remove cor parameter for basic temporal dynamics only."
      )))
    }

    # Check for hierarchical grouping (gr, subgr parameters)
    if (!is.null(trend_component$gr)) {
      stop(insight::format_error(c(
        "Hierarchical grouping not allowed in multivariate response trends.",
        x = cli::format_inline(
          "Response {.val {response_name}} has gr = {.val {trend_component$gr}}."
        ),
        i = "Remove gr parameter for basic temporal dynamics only."
      )))
    }

    if (!is.null(trend_component$subgr)) {
      stop(insight::format_error(c(
        "Hierarchical grouping not allowed in multivariate response trends.",
        x = cli::format_inline(
          "Response {.val {response_name}} has subgr = {.val {trend_component$subgr}}."
        ),
        i = "Remove subgr parameter for basic temporal dynamics only."
      )))
    }
  }

  invisible(NULL)
}

#' Main validation function for autocorrelation separation
#'
#' Validates proper separation between observation-level (brms) and trend-level
#' (mvgam) autocorrelation handling.
#'
#' @param obs_formula Observation formula (any brms-compatible type)
#' @param trend_formula Trend specification (formula, bf(), or named list)
#' @return List with validated formulas
#' @noRd
validate_autocor_separation <- function(obs_formula, trend_formula = NULL) {
  # Validate observation formula (minimal - let brms handle most validation)
  validated_obs <- validate_obs_formula_brms(obs_formula)

  # Validate trend formula (comprehensive - mvgam State-Space requirements)
  validated_trend <- validate_trend_formula_brms(trend_formula)

  return(list(
    obs_formula = validated_obs,
    trend_formula = validated_trend
  ))
}

#' Validate Setup Components
#' @param components List of setup components
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_setup_components <- function(components) {
  checkmate::assert_list(components, names = "named")

  required_components <- c("formula", "data", "family", "stancode", "standata")
  missing_components <- setdiff(required_components, names(components))

  if (length(missing_components) > 0) {
    stop(insight::format_error(c(
      "Missing required setup components:",
      x = paste(missing_components, collapse = ", ")
    )))
  }

  # Validate Stan code is not empty
  if (is.null(components$stancode) ||
      (is.character(components$stancode) && nchar(components$stancode) == 0)) {
    stop(insight::format_error(c(
      "Stan code extraction failed.",
      x = "Could not obtain valid Stan model code from brms setup."
    )))
  }

  # Validate Stan data is not empty
  if (is.null(components$standata) || length(components$standata) == 0) {
    stop(insight::format_error(c(
      "Stan data extraction failed.",
      x = "Could not obtain valid Stan data from brms setup."
    )))
  }

  invisible(TRUE)
}

#' Validate Time Series Structure for Trends
#'
#' @description
#' Ensures time series data is compatible with specified trend models.
#' Leverages existing mvgam validation functions.
#'
#' @param data Data to validate (data.frame or list)
#' @param trend_specs Trend specification containing trend model info
#' @param silent Verbosity level
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_time_series_for_trends <- function(data, trend_specs, silent = 1, response_vars = NULL, cached_formulas = NULL, .precomputed_dimensions = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(trend_specs)
  # Validate response_vars parameter if provided
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE, null.ok = TRUE)
  }
  # Validate cached_formulas parameter if provided
  if (!is.null(cached_formulas)) {
    checkmate::assert_list(cached_formulas)
  }
  # Validate precomputed dimensions parameter if provided
  if (!is.null(.precomputed_dimensions)) {
    checkmate::assert_list(.precomputed_dimensions, names = "named")
  }

  # Extract variable names from trend specification
  # Standardized structure: univariate=direct, multivariate=named list
  if (is_multivariate_trend_specs(trend_specs)) {
    # Multivariate: extract from first response spec
    first_spec <- trend_specs[[1]]
    time_var <- first_spec$time_var %||% first_spec$time %||% "time"
    series_var <- first_spec$series_var %||% first_spec$series %||% "series"
    trend_type <- first_spec$trend_type %||% first_spec$trend_model %||% first_spec$trend
  } else {
    # Univariate: extract directly from trend object
    time_var <- trend_specs$time_var %||% trend_specs$time %||% "time"
    series_var <- trend_specs$series_var %||% trend_specs$series %||% "series"
    trend_type <- trend_specs$trend_type %||% trend_specs$trend_model %||% trend_specs$trend
  }

  # Create time and series attributes for consistent grouping operations
  checkmate::assert_names(names(data), must.include = time_var)
  parsed_trend <- if (is_multivariate_trend_specs(trend_specs)) trend_specs[[1]] else trend_specs
  data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var, response_vars)


  # Use precomputed dimensions - no fallback in ultra-DRY architecture
  if (is.null(.precomputed_dimensions)) {
    stop(insight::format_error(c(
      "Missing precomputed dimensions in ultra-DRY architecture.",
      x = "This function should only be called with precomputed dimensions.",
      i = "Check that extract_and_validate_trend_components() is passing dimensions correctly."
    )), call. = FALSE)
  }

  dimensions <- .precomputed_dimensions

  # ARCHITECTURAL FIX: Three-phase validation replacing circular validation chain
  # Phase 1: Input validation (already done by extract_time_series_dimensions above)

  # Phase 2: Verify attribute creation succeeded
  if (!has_mvgam_variables(data)) {
    stop(insight::format_error(c(
      "Attribute creation failed during time series validation.",
      x = "mvgam time and series attributes are missing from data."
    )), call. = FALSE)
  }

  # Phase 3: Context-specific validations (only what's essential)
  if (trend_type != "CAR") {
    # Only validate regular time intervals for non-CAR trends using original time values
    original_times <- attr(data, "mvgam_original_time")
    if (!is.null(original_times)) {
      validate_regular_time_intervals(original_times, time_var)
    }
  }


  # Return data with preserved attributes and dimensions
  invisible(list(
    data = data,  # Keep attribute-enhanced data
    dimensions = dimensions
  ))
}

#' Check if object is a mvgam trend
#'
#' Tests whether an object is a valid mvgam trend specification.
#'
#' @param x Object to test
#' @return Logical indicating if x is a mvgam trend
#' @export
is.mvgam_trend <- function(x) {
  inherits(x, "mvgam_trend")
}

#' Validate trend components for conflicts
#'
#' Checks for conflicting trend specifications like multiple dynamic factor models
#' or incompatible correlation structures using brms-inspired validation patterns.
#'
#' @param trend_components List of trend components to validate
#'
#' @noRd
validate_trend_components <- function(trend_components) {

  # Check for multiple trend types - only one trend type allowed per formula
  if (length(trend_components) > 1) {
    trend_types <- sapply(trend_components, function(x) x$trend_type)
    stop(insight::format_error(c(
      "Multiple trend types detected in single formula.",
      x = paste("Found:", paste(trend_types, collapse = ", ")),
      x = "Only one trend constructor is allowed per trend_formula.",
      i = "Use separate models or combine into a single trend type."
    )))
  }

  # Check for multiple dynamic factor models
  n_lv_models <- sum(sapply(trend_components, function(x) !is.null(x$n_lv) && x$n_lv > 0))
  if (n_lv_models > 1) {
    stop(insight::format_error(c(
      "Multiple dynamic factor models specified.",
      x = cli::format_inline(
        "Only one trend component can have {.field n_lv > 0}."
      ),
      i = "Consider combining factor structures or removing one factor model."
    )))
  }

  # Check for conflicting correlation structures
  cor_settings <- sapply(trend_components, function(x) x$cor %||% FALSE)
  if (any(cor_settings) && !all(cor_settings)) {
    stop(insight::format_error(c(
      "Mixed correlation settings detected.",
      x = "Some trend components have correlation enabled while others don't."
    )))
  }

  invisible(NULL)
}

#' Extract Time Series Dimensions from Data
#'
#' @description
#' Extracts core time series dimensions directly from data and validates
#' structure based on trend type. This is the single source of truth for
#' all time series dimensions used throughout the system.
#'
#' @param data Data frame containing time series
#' @param time_var Name of time variable (default: "time")
#' @param series_var Name of series variable (default: "series")
#' @param trend_type Type of trend model ("CAR" allows irregular intervals)
#' @param trend_specs Optional trend specification list for enhanced metadata
#' @return List with time series dimensions and optional enhanced metadata
#' @noRd
extract_time_series_dimensions <- function(data, time_var = "time", series_var = "series", trend_type = NULL, trend_specs = NULL, response_vars = NULL, cached_formulas = NULL) {

  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  if (!is.null(trend_specs)) {
    checkmate::assert_list(trend_specs)
  }
  # Validate response_vars parameter if provided
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE)
  }


  # Validate required variables exist - only require time_var since series can be created via attributes
  formula_to_use <- if (!is.null(cached_formulas)) cached_formulas$formula else NULL
  filtered_vars <- filter_required_variables(time_var, formula_to_use)  # Only require time_var
  validate_required_variables(data, filtered_vars, "time series data")

  # Calculate core dimensions from data using attribute-based accessors
  time_vals <- get_time_for_grouping(data)
  series_vals <- get_series_for_grouping(data)

  unique_times <- unique(time_vals)
  unique_series <- unique(series_vals)
  min_time <- min(time_vals, na.rm = TRUE)
  max_time <- max(time_vals, na.rm = TRUE)

  # Create data ordering mappings for Stan-required sorted order
  ordering_df <- data.frame(
    original_row = seq_len(nrow(data)),
    time_val = time_vals,
    series_val = series_vals,
    stringsAsFactors = FALSE
  )

  # Sort for Stan: first by time, then by series (matches times_trend matrix structure)
  sorted_ordering <- ordering_df[order(ordering_df$time_val, ordering_df$series_val), ]

  # Create bidirectional mappings
  stan_to_original <- sorted_ordering$original_row  # Maps Stan row index -> Original row index
  original_to_stan <- integer(nrow(data))
  original_to_stan[stan_to_original] <- seq_len(nrow(data))  # Maps Original row index -> Stan row index

  # Create time/series index mappings (for times_trend matrix interpretation)
  sorted_unique_times <- sort(unique_times)
  sorted_unique_series <- sort(unique_series)
  time_indices <- match(sorted_ordering$time_val, sorted_unique_times)
  series_indices <- match(sorted_ordering$series_val, sorted_unique_series)

  # Calculate per-series time information for forecasting
  if (requireNamespace("dplyr", quietly = TRUE)) {
    # Use attribute-based accessors for series and time data
    time_vals <- get_time_for_grouping(data)
    series_vals <- get_series_for_grouping(data)

    series_time_info <- data.frame(
      series = series_vals,
      time = time_vals,
      stringsAsFactors = FALSE
    ) %>%
      dplyr::group_by(.data$series) %>%
      dplyr::summarise(
        first_time = min(.data$time, na.rm = TRUE),
        last_time = max(.data$time, na.rm = TRUE),
        n_obs_series = dplyr::n(),
        time_span = max(.data$time, na.rm = TRUE) - min(.data$time, na.rm = TRUE),
        .groups = "drop"
      )

    # Create named vectors for quick access (ordered by sorted unique_series)
    last_times <- setNames(
      series_time_info$last_time[match(sorted_unique_series, series_time_info[[1]])],
      sorted_unique_series
    )
    first_times <- setNames(
      series_time_info$first_time[match(sorted_unique_series, series_time_info[[1]])],
      sorted_unique_series
    )
    series_lengths <- setNames(
      series_time_info$n_obs_series[match(sorted_unique_series, series_time_info[[1]])],
      sorted_unique_series
    )
  } else {
    # Fallback without dplyr
    series_time_info <- NULL
    last_times <- NULL
    first_times <- NULL
    series_lengths <- NULL
  }

  # Backward compatible structure: Original fields maintained
  dimensions <- list(
    n_time = length(unique_times),          # Number of unique time points
    n_series = length(unique_series),       # Number of series
    n_obs = nrow(data),                    # Total observations
    time_range = c(min_time, max_time),    # Time range
    time_var = time_var,                   # Variable names for downstream use
    series_var = series_var,
    unique_times = sorted_unique_times,    # Sorted unique time points
    unique_series = sorted_unique_series   # Sorted unique series
  )

  # Generate observation-to-trend mappings if response variables provided
  # This centralizes mapping generation with dimension calculation for consistency
  if (!is.null(response_vars)) {
    dimensions$mappings <- list()

    for (resp_var in response_vars) {
      # Validate response variable exists in data
      formula_to_use <- if (!is.null(cached_formulas)) cached_formulas$formula else NULL
      filtered_vars <- filter_required_variables(resp_var, formula_to_use)
      validate_required_variables(data, filtered_vars, "response mapping data")

      # Generate mapping for this response variable using existing function
      mapping <- generate_obs_trend_mapping(
        data = data,
        response_var = resp_var,
        time_var = time_var,
        series_var = series_var,
        dimensions = dimensions,  # Pass already-calculated dimensions
        cached_formulas = cached_formulas,
        response_vars = response_vars  # Pass full response_vars for multivariate series creation
      )

      # Store mapping with response variable name as key
      dimensions$mappings[[resp_var]] <- mapping
    }
  }

  # Enhanced metadata: Comprehensive information for post-processing
  if (!is.null(trend_specs)) {
    dimensions$metadata <- list(
      # Variable identification
      variables = list(
        time_var = time_var,
        series_var = series_var,
        gr_var = trend_specs$gr %||% 'NA',
        subgr_var = trend_specs$subgr %||% 'NA',
        has_grouping = !is.null(trend_specs$gr) && trend_specs$gr != 'NA'
      ),

      # Data dimensions
      dimensions = list(
        n_obs = nrow(data),
        n_time = length(unique_times),
        n_series = length(unique_series),
        n_groups = trend_specs$n_groups %||% 1,
        n_subgroups = trend_specs$n_subgroups %||% 1,
        n_lv = trend_specs$n_lv %||% NULL,
        time_range = c(min_time, max_time)
      ),

      # Unique values (sorted for Stan)
      levels = list(
        unique_times = sorted_unique_times,
        unique_series = sorted_unique_series,
        unique_groups = if (!is.null(trend_specs$gr) && trend_specs$gr != 'NA')
                          sort(unique(data[[trend_specs$gr]])) else NULL,
        unique_subgroups = if (!is.null(trend_specs$subgr) && trend_specs$subgr != 'NA')
                             sort(unique(data[[trend_specs$subgr]])) else NULL
      ),

      # Per-series time information (key for forecasting)
      time_info = list(
        # Quick access vectors (ordered by sorted unique_series)
        last_times = last_times,
        first_times = first_times,
        series_lengths = series_lengths,

        # Global time information
        global_last_time = max_time,
        global_first_time = min_time,
        max_forecast_horizon = if (!is.null(last_times)) max_time - min(last_times, na.rm = TRUE) else 0,

        # Panel structure information
        is_balanced = if (!is.null(series_lengths)) length(unique(series_lengths)) == 1 else FALSE,
        series_time_info = series_time_info  # Full details by series
      ),

      # Data ordering mappings
      ordering = list(
        # Core mappings between original data and Stan-required order
        stan_to_original = stan_to_original,
        original_to_stan = original_to_stan,

        # Index mappings for times_trend matrix interpretation
        time_indices = time_indices,
        series_indices = series_indices,
        group_indices = if (!is.null(trend_specs$gr) && trend_specs$gr != 'NA')
                          match(data[[trend_specs$gr]], sort(unique(data[[trend_specs$gr]]))) else NULL,
        subgroup_indices = if (!is.null(trend_specs$subgr) && trend_specs$subgr != 'NA')
                             match(data[[trend_specs$subgr]], sort(unique(data[[trend_specs$subgr]]))) else NULL,

        # Efficiency flags
        requires_reordering = !identical(seq_len(nrow(data)), stan_to_original)
      ),

      # Trend model metadata
      trend = if (!is.null(trend_specs)) list(
        trend_type = trend_specs$trend %||% trend_specs$trend_model %||% NULL,
        has_trend = TRUE,
        is_factor_model = !is.null(trend_specs$n_lv) && trend_specs$n_lv < length(unique_series),
        correlation_structure = list(
          cor = trend_specs$cor %||% FALSE,
          ma = trend_specs$ma %||% FALSE,
          lags = trend_specs$lags %||% 1
        ),
        trend_specs = trend_specs
      ) else list(has_trend = FALSE),

      # Validation flags
      validation = list(
        data_complete = !any(is.na(c(get_time_for_grouping(data), get_series_for_grouping(data)))),
        validation_timestamp = Sys.time()
      )
    )
  }

  # Validate regular intervals for non-CAR trends
  if (!is.null(trend_type) && trend_type != "CAR") {
    validate_regular_time_intervals(attr(data, "mvgam_original_time"), time_var)
  }

  return(dimensions)
}

#' Generate Observation to Trend Mapping
#'
#' @description
#' Creates mapping arrays that align each observation in brms-ordered data
#' to its corresponding position in the trend matrix. This solves the problem
#' where brms excludes NA observations but doesn't provide an obs_ind array.
#'
#' @param data Data frame with observations (may include NAs)
#' @param response_var Name of response variable to check for missing values
#' @param time_var Name of time variable
#' @param series_var Name of series variable
#' @param dimensions List from extract_time_series_dimensions with time series structure
#' @param response_vars Character vector of response variable names for multivariate series creation
#' @return List containing obs_trend_time and obs_trend_series arrays for Stan
#' @noRd
generate_obs_trend_mapping <- function(data, response_var, time_var = "time",
                                      series_var = "series", dimensions = NULL, cached_formulas = NULL, response_vars = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(response_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE)
  }

  # Validate dimensions parameter when provided
  if (!is.null(dimensions)) {
    checkmate::assert_list(dimensions)
    required_fields <- c("unique_times", "unique_series", "n_time", "n_series")
    missing_fields <- setdiff(required_fields, names(dimensions))
    if (length(missing_fields) > 0) {
      stop(insight::format_error(
        cli::format_inline(
          "Dimensions list missing required fields: {paste(missing_fields, collapse = ', ')}"
        )
      ), call. = FALSE)
    }
  }

  # Validate required columns exist - only require variables that can't be created via attributes
  formula_to_use <- if (!is.null(cached_formulas)) cached_formulas$formula else NULL
  filtered_vars <- filter_required_variables(c(response_var, time_var), formula_to_use)  # series_var can be created via attributes
  validate_required_variables(data, filtered_vars, "mapping data")

  # Extract dimensions if not provided
  if (is.null(dimensions)) {
    dimensions <- extract_time_series_dimensions(data, time_var, series_var, cached_formulas = cached_formulas, response_vars = response_vars)
  }

  # Identify non-missing observations (brms will only use these)
  non_missing_idx <- which(!is.na(data[[response_var]]))

  # Handle edge case where all observations are missing
  if (length(non_missing_idx) == 0) {
    stop(insight::format_error(c(
      cli::format_inline(
        "All observations are missing for response variable {.field {response_var}}."
      ),
      x = "Cannot create observation mappings without any valid data."
    )), call. = FALSE)
  }

  # Extract time and series indices for non-missing observations
  obs_data <- data[non_missing_idx, , drop = FALSE]

  # Preserve mvgam attributes after subsetting with correct lengths
  # Vector attributes must be subsetted to match obs_data length
  vector_attrs <- c("mvgam_time", "mvgam_series", "mvgam_original_time")
  for (attr_name in vector_attrs) {
    if (!is.null(attr(data, attr_name))) {
      attr(obs_data, attr_name) <- attr(data, attr_name)[non_missing_idx]
    }
  }

  # Scalar attributes can be copied as-is
  scalar_attrs <- c("mvgam_time_source", "mvgam_series_source")
  for (attr_name in scalar_attrs) {
    if (!is.null(attr(data, attr_name))) {
      attr(obs_data, attr_name) <- attr(data, attr_name)
    }
  }

  # Get sorted unique values from dimensions
  sorted_unique_times <- dimensions$unique_times
  sorted_unique_series <- dimensions$unique_series

  # Create time and series index mappings using attribute-based system
  # These map each observation to its position in the trend matrix
  # For time: use processed values from attributes (matches dimension calculation)
  # For series: use processed values from attributes (handles all creation strategies)
  time_values <- get_time_for_grouping(obs_data)
  series_values <- get_series_for_grouping(obs_data)

  obs_trend_time <- match(time_values, sorted_unique_times)
  obs_trend_series <- match(series_values, sorted_unique_series)

  # Validate the mappings
  if (any(is.na(obs_trend_time))) {
    stop(insight::format_error(c(
      "Failed to map some observations to time indices.",
      x = "This indicates a data structure problem."
    )), call. = FALSE)
  }

  if (any(is.na(obs_trend_series))) {
    stop(insight::format_error(c(
      "Failed to map some observations to series indices.",
      x = "This indicates a data structure problem."
    )), call. = FALSE)
  }

  # Validate bounds
  if (any(obs_trend_time < 1 | obs_trend_time > dimensions$n_time)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Time indices out of bounds: must be in [1, {dimensions$n_time}]."
      ),
      x = cli::format_inline(
        "Found indices: [{min(obs_trend_time)}, {max(obs_trend_time)}]"
      )
    )), call. = FALSE)
  }

  if (any(obs_trend_series < 1 | obs_trend_series > dimensions$n_series)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Series indices out of bounds: must be in [1, {dimensions$n_series}]."
      ),
      x = cli::format_inline(
        "Found indices: [{min(obs_trend_series)}, {max(obs_trend_series)}]"
      )
    )), call. = FALSE)
  }

  return(list(
    obs_trend_time = as.integer(obs_trend_time),
    obs_trend_series = as.integer(obs_trend_series),
    n_obs_non_missing = length(non_missing_idx),
    has_missing = length(non_missing_idx) < nrow(data)
  ))
}


#' Validate mvgam_trend object structure
#' @param trend_obj mvgam_trend object to validate
#' @return Logical TRUE if valid, stops with error if not
#' @noRd
validate_mvgam_trend <- function(trend_obj) {
  checkmate::assert_class(trend_obj, "mvgam_trend")
  checkmate::assert_list(trend_obj, min.len = 1)
  checkmate::assert_string(trend_obj$trend, min.chars = 1)

  # Validate required fields exist
  required_fields <- c("trend", "time", "series")
  missing_fields <- setdiff(required_fields, names(trend_obj))
  if (length(missing_fields) > 0) {
    stop(insight::format_error(
      cli::format_inline(
        "Missing required fields in mvgam_trend object: {.field {missing_fields}}"
      )
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate proportional values (0-1 range)
#' @param x Numeric value to validate
#' @param name Parameter name for error messages
#' @return Logical TRUE if valid, stops with error if not
#' @noRd
validate_proportional <- function(x, name = deparse(substitute(x))) {
  checkmate::assert_number(x, lower = 0, upper = 1, .var.name = name)
  invisible(TRUE)
}

#' Validate positive integers
#' @param x Integer value to validate
#' @param name Parameter name for error messages
#' @return Logical TRUE if valid, stops with error if not
#' @noRd
validate_pos_integer <- function(x, name = deparse(substitute(x))) {
  checkmate::assert_int(x, lower = 1, .var.name = name)
  invisible(TRUE)
}

#' Evaluate an expression without printing output or messages
#' @param expr expression to be evaluated
#' @param type type of output to be suppressed (see ?sink)
#' @param try wrap evaluation of expr in 'try' and
#'   not suppress outputs if evaluation fails?
#' @param silent actually evaluate silently?
#' @noRd
eval_silent <- function(
    expr,
    type = "output",
    try = FALSE,
    silent = TRUE,
    ...
) {
  try <- as_one_logical(try)
  silent <- as_one_logical(silent)
  type <- match.arg(type, c("output", "message"))
  expr <- substitute(expr)
  envir <- parent.frame()
  if (silent) {
    if (try && type == "message") {
      try_out <- try(utils::capture.output(
        out <- eval(expr, envir),
        type = type,
        ...
      ))
      if (is_try_error(try_out)) {
        # try again without suppressing error messages
        out <- eval(expr, envir)
      }
    } else {
      utils::capture.output(out <- eval(expr, envir), type = type, ...)
    }
  } else {
    out <- eval(expr, envir)
  }
  out
}

#' Check if x is a try-error resulting from try()
is_try_error <- function(x) {
  inherits(x, "try-error")
}

#' Check if trend_specs represents multivariate trends
#'
#' @description
#' Determines whether trend specifications represent a multivariate model
#' (named list of trend specifications) or a univariate model (single trend).
#'
#' @param trend_specs Trend specifications to check
#' @return Logical indicating if multivariate
#' @noRd
is_multivariate_trend_specs <- function(trend_specs) {
  if (is.null(trend_specs)) {
    return(FALSE)
  }

  # Standardized structure check:
  # Multivariate: named list without trend fields (response names as keys)
  # Univariate: direct trend object with trend field
  if (is.list(trend_specs) && !is.null(names(trend_specs))) {
    # Check if this is a direct trend object (has trend field) or multivariate (response names)
    has_trend_field <- any(c("trend", "trend_type", "trend_model") %in% names(trend_specs))
    return(!has_trend_field)  # Multivariate if no trend field at top level
  }

  # Univariate case: direct trend specification object
  return(FALSE)
}

#' Apply the top-level `trend_map` alias to parsed trend specs
#'
#' Single point that lets users supply `trend_map` either at the
#' trend constructor (`AR(trend_map = ...)`) or at `mvgam()`.
#' Errors if both are populated (collision); otherwise sets the
#' top-level alias on each spec when the constructor-level value
#' is absent.
#'
#' @param trend_specs Parsed trend specs (single spec or named
#'   list of specs for multi-response models).
#' @param mvgam_trend_map The top-level `trend_map` value (may
#'   be NULL).
#'
#' @return `trend_specs` with `$trend_map` populated when the
#'   top-level alias was supplied; unchanged otherwise.
#'
#' @noRd
apply_trend_map_alias <- function(trend_specs, mvgam_trend_map) {
  if (is.null(trend_specs) || is.null(mvgam_trend_map)) {
    return(trend_specs)
  }
  is_multivar <- is_multivariate_trend_specs(trend_specs)
  specs <- if (is_multivar) trend_specs else list(trend_specs)
  for (i in seq_along(specs)) {
    if (!is.null(specs[[i]]$trend_map)) {
      stop(insight::format_error(c(
        paste0(
          "'trend_map' supplied at both trend constructor and ",
          "'mvgam()' (collision)."
        ),
        x = paste0(
          "Trend spec '", names(specs)[i] %||% i,
          "' has 'trend_map' on the constructor; mvgam() ",
          "also supplies it at the top level."
        ),
        i = paste0(
          "Drop the mvgam()-level 'trend_map' argument ",
          "(constructor-level takes precedence)."
        )
      )))
    }
    specs[[i]]$trend_map <- mvgam_trend_map
  }
  if (is_multivar) specs else specs[[1L]]
}


#' Normalise raw `trend_map` input on every spec to a fixed-Z
#' matrix and reconcile with `n_lv`.
#'
#' Called once in the Stan-code pipeline right after
#' `apply_trend_map_alias()`. Walks each trend spec, calls
#' `normalise_trend_map()` for any non-NULL `trend_map`, stashes
#' the canonical numeric Z on `spec$fixed_Z`, and updates
#' `spec$n_lv` to match `ncol(Z)`. If the user also set `n_lv`
#' explicitly on the constructor, the two values must agree.
#'
#' @param trend_specs Parsed trend specs (single spec or named
#'   list).
#' @param data Training `data.frame` (used to resolve series
#'   levels for the normaliser).
#'
#' @return `trend_specs` with `$fixed_Z` and reconciled `$n_lv`
#'   set on any spec whose `$trend_map` was supplied.
#'
#' @noRd
# Attach a normalised loadings-prior spec onto each trend spec.
# Mirrors `normalise_trend_map_on_specs` for the multivariate
# unwrap/rewrap so all call sites see one consistent shape. Also
# asserts compatibility with any fixed_Z already attached: a
# structured prior cannot coexist with either a partial-Z or
# fully-fixed Z (see `assert_loadings_prior_compatible()` for
# the rationale and error messages).
#'@noRd
attach_loadings_prior_spec <- function(trend_specs, spec) {
  if (is.null(trend_specs) || is.null(spec)) return(trend_specs)
  is_multivar <- is_multivariate_trend_specs(trend_specs)
  specs <- if (is_multivar) trend_specs else list(trend_specs)
  for (i in seq_along(specs)) {
    assert_loadings_prior_compatible(spec, specs[[i]]$fixed_Z)
    specs[[i]]$loadings_prior_spec <- spec
  }
  if (is_multivar) specs else specs[[1L]]
}


normalise_trend_map_on_specs <- function(trend_specs, data) {
  if (is.null(trend_specs)) return(trend_specs)
  is_multivar <- is_multivariate_trend_specs(trend_specs)
  specs <- if (is_multivar) trend_specs else list(trend_specs)
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    if (is.null(spec$trend_map)) next
    normalised <- normalise_trend_map(spec$trend_map, data)
    if (is.null(normalised)) next
    if (!is.null(spec$n_lv) && spec$n_lv != normalised$n_lv) {
      stop(insight::format_error(c(
        paste0(
          "'trend_map' shape conflicts with constructor ",
          "'n_lv' on spec '", names(specs)[i] %||% i, "'."
        ),
        x = paste0(
          "trend_map implies n_lv = ", normalised$n_lv,
          " but n_lv was set to ", spec$n_lv, "."
        ),
        i = "Drop the redundant 'n_lv' or update trend_map shape."
      )))
    }
    # An all-NA mask is just the canonical factor-model trigger
    # (every loading free). Treat it as NULL so the downstream
    # 'matrix-Z' code path emits a free parameter Z that
    # loadings_prior can wire onto column-wise; keep n_lv so the
    # factor-model gate (n_lv < n_series) still fires.
    spec$fixed_Z <- if (all(is.na(normalised$Z))) NULL else normalised$Z
    spec$n_lv <- normalised$n_lv
    specs[[i]] <- spec
  }
  # Multivariate fits use ONE shared trend component across all
  # responses (see `enrich_trend_metadata()` which already keeps
  # only the first spec's trend metadata). A fixed Z must
  # therefore agree across responses; otherwise the downstream
  # resolver would silently apply the first response's Z to
  # every series.
  if (is_multivar) {
    fixed_Zs <- lapply(specs, function(s) s$fixed_Z)
    populated <- which(!vapply(fixed_Zs, is.null, logical(1L)))
    if (length(populated) >= 2L) {
      ref <- fixed_Zs[[populated[1L]]]
      mismatch <- populated[-1L][
        !vapply(populated[-1L], function(j) {
          identical(unname(fixed_Zs[[j]]), unname(ref))
        }, logical(1L))
      ]
      if (length(mismatch) > 0L) {
        offending <- names(specs)[mismatch] %||% as.character(mismatch)
        stop(insight::format_error(c(
          paste0(
            "'trend_map' differs across multivariate trend specs."
          ),
          x = paste0(
            "Mismatched spec(s): ",
            paste(offending, collapse = ", "), "."
          ),
          i = paste0(
            "mvgam uses one shared trend component across ",
            "responses; supply the same 'trend_map' for every ",
            "spec (or set it once at the top level via ",
            "'mvgam(trend_map = ...)')."
          )
        )))
      }
    }
  }
  if (is_multivar) specs else specs[[1L]]
}


#' Validate Factor Levels
#'
#' @description
#' Checks for unused factor levels that could cause Stan indexing issues.
#' Issues warnings for validation phase, allows auto-dropping in preparation phase.
#'
#' @param data Data frame containing the factor variable
#' @param var_name Name of the factor variable to check
#' @param data_name Name of the data object (for error messages)
#' @param auto_drop Whether to automatically drop unused levels
#' @return Modified data if auto_drop=TRUE, otherwise original data
#' @noRd
validate_factor_levels <- function(data, var_name, data_name = "data", auto_drop = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(var_name)
  checkmate::assert_flag(auto_drop)

  if (!var_name %in% names(data)) {
    return(data)  # Variable doesn't exist, will be caught elsewhere
  }

  if (!is.factor(data[[var_name]])) {
    return(data)  # Not a factor, will be caught elsewhere
  }

  var_data <- data[[var_name]]
  used_levels <- unique(var_data)
  all_levels <- levels(var_data)
  unused_levels <- setdiff(all_levels, used_levels)

  if (length(unused_levels) > 0) {
    if (auto_drop) {
      # Auto-drop unused levels
      data[[var_name]] <- droplevels(var_data)
    } else {
      # Warn about unused levels
      rlang::warn(
        message = insight::format_warning(c(
          cli::format_inline(
            "Factor variable {.field {var_name}} in {data_name} has unused levels: {paste(unused_levels, collapse = ', ')}."
          ),
          i = "Consider using droplevels() to remove unused factor levels.",
          x = "This may cause indexing issues in Stan model compilation."
        )),
        .frequency = "once"
      )
    }
  }

  return(data)
}


#' Extract Factor Levels from Data Column
#'
#' Extracts unique levels from a factor or character column. Handles
#' NULL/NA variable names, missing columns, empty columns, and both
#' factor and character data types.
#'
#' @param data Data frame to extract levels from
#' @param var_name Name of variable to extract levels from. Can be
#'   NULL, NA, or "NA" (all return NULL).
#'
#' @return Character vector of unique levels (excluding NA values), or
#'   NULL if variable is missing, NULL, NA, "NA", or column is empty.
#'
#' @details
#' Level extraction behavior:
#' - For factors: returns `levels()` (preserves level ordering)
#' - For characters: returns `unique()` sorted alphabetically
#' - For other types: coerces to character first
#' - NA values are always excluded from returned levels
#'
#' Used by `extract_trend_data()` to store training factor levels in
#' `trend_metadata$levels` for prediction validation.
#'
#' @noRd
extract_factor_levels <- function(data, var_name) {
  checkmate::assert_data_frame(data)

  # Handle NULL/NA/missing variable name
  if (is.null(var_name) ||
      length(var_name) == 0 ||
      is.na(var_name) ||
      identical(var_name, "NA")) {
    return(NULL)
  }

  checkmate::assert_string(var_name, min.chars = 1)

  # Handle missing column
  if (!var_name %in% names(data)) {
    return(NULL)
  }

  col_data <- data[[var_name]]

  # Handle empty or all-NA columns
  if (length(col_data) == 0 || all(is.na(col_data))) {
    return(NULL)
  }

  # Remove NA values before extraction
  col_data <- col_data[!is.na(col_data)]

  if (is.factor(col_data)) {
    # Preserve all factor levels (including unused) for training metadata
    return(levels(col_data))
  } else {
    # Character or other: unique sorted values
    return(sort(unique(as.character(col_data))))
  }
}


#' Validate Prediction Data Factor Levels
#'
#' Validates that factor levels in prediction data are a subset of training
#' data levels. Called by `ensure_mvgam_variables()` when metadata with
#' stored levels is provided.
#'
#' @param data Data frame of prediction data to validate
#' @param metadata List containing `levels` (with series/gr/subgr) and
#'   `variables` (with series_var/gr_var/subgr_var)
#'
#' @return Invisible TRUE if valid. Stops with informative error if
#'   prediction data contains factor levels not in training data.
#'
#' @noRd
validate_prediction_factor_levels <- function(data, metadata) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(metadata, names = "named")

  # Validate required metadata structure
  if (is.null(metadata$levels)) {
    return(invisible(TRUE))
  }
  checkmate::assert_list(metadata$levels, names = "named")

  if (is.null(metadata$variables)) {
    return(invisible(TRUE))
  }
  checkmate::assert_list(metadata$variables, names = "named")

  # Validate series levels
  if (!is.null(metadata$levels$series)) {
    series_var <- metadata$variables$series_var
    if (!is.null(series_var) && series_var %in% names(data)) {
      newdata_levels <- extract_factor_levels(data, series_var)
      if (!is.null(newdata_levels)) {
        invalid <- setdiff(newdata_levels, metadata$levels$series)
        if (length(invalid) > 0) {
          stop(insight::format_error(c(
            "Series levels in newdata not found in training data.",
            x = cli::format_inline("Invalid: {.val {invalid}}."),
            i = cli::format_inline(
              "Training data has levels: {.val {metadata$levels$series}}."
            )
          )), call. = FALSE)
        }
      }
    }
  }

  # Validate gr levels (if hierarchical model)
  if (!is.null(metadata$levels$gr)) {
    gr_var <- metadata$variables$gr_var
    if (!is.null(gr_var) && gr_var %in% names(data)) {
      newdata_levels <- extract_factor_levels(data, gr_var)
      if (!is.null(newdata_levels)) {
        invalid <- setdiff(newdata_levels, metadata$levels$gr)
        if (length(invalid) > 0) {
          stop(insight::format_error(c(
            cli::format_inline(
              "Grouping variable {.field {gr_var}} has levels not in training data."
            ),
            x = cli::format_inline("Invalid: {.val {invalid}}."),
            i = cli::format_inline(
              "Training data has levels: {.val {metadata$levels$gr}}."
            )
          )), call. = FALSE)
        }
      }
    }
  }

  # Validate subgr levels (if hierarchical model)
  if (!is.null(metadata$levels$subgr)) {
    subgr_var <- metadata$variables$subgr_var
    if (!is.null(subgr_var) && subgr_var %in% names(data)) {
      newdata_levels <- extract_factor_levels(data, subgr_var)
      if (!is.null(newdata_levels)) {
        invalid <- setdiff(newdata_levels, metadata$levels$subgr)
        if (length(invalid) > 0) {
          stop(insight::format_error(c(
            cli::format_inline(
              "Sub-grouping variable {.field {subgr_var}} has levels not in training data."
            ),
            x = cli::format_inline("Invalid: {.val {invalid}}."),
            i = cli::format_inline(
              "Training data has levels: {.val {metadata$levels$subgr}}."
            )
          )), call. = FALSE)
        }
      }
    }
  }

  invisible(TRUE)
}


#' Validate Stan Code Structure
#'
#' @description
#' Validates that Stan code contains required blocks (data, parameters, model).
#'
#' @param stan_code Character string containing Stan model code
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_stan_code_structure <- function(stan_code) {
  checkmate::assert_string(stan_code, min.chars = 1)

  # Required Stan blocks
  required_blocks <- c("data", "parameters", "model")

  # Check for each required block
  missing_blocks <- character(0)

  for (block in required_blocks) {
    # Pattern to match block declaration
    block_pattern <- paste0("\\b", block, "\\s*\\{")

    if (!grepl(block_pattern, stan_code, ignore.case = FALSE)) {
      missing_blocks <- c(missing_blocks, block)
    }
  }

  if (length(missing_blocks) > 0) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Missing required Stan block{?s}: {.field {missing_blocks}}"
      ),
      i = "Stan models must contain data, parameters, and model blocks."
    )))
  }

  invisible(TRUE)
}

#' Check if Braces are Balanced
#'
#' @description
#' Checks if opening and closing braces are properly balanced in Stan code.
#'
#' @param stan_code Character string containing Stan model code
#' @return Logical indicating whether braces are balanced
#' @noRd
are_braces_balanced <- function(stan_code) {
  checkmate::assert_string(stan_code)

  # Split into individual characters
  chars <- unlist(strsplit(stan_code, "", fixed = TRUE))

  # Track brace depth
  depth <- 0

  for (char in chars) {
    if (char == "{") {
      depth <- depth + 1
    } else if (char == "}") {
      depth <- depth - 1

      # If depth goes negative, we have unmatched closing brace
      if (depth < 0) {
        return(FALSE)
      }
    }
  }

  # Return TRUE only if depth is exactly 0 (all braces matched)
  return(depth == 0)
}

#' Parse Data Declarations from Stan Data Block
#'
#' @description
#' Extracts variable names from Stan data block declarations.
#' This is a simplified parser for basic variable declarations.
#'
#' @param data_block Character string containing Stan data block content
#' @return Character vector of declared variable names
#' @noRd
parse_data_declarations <- function(data_block) {
  checkmate::assert_string(data_block)

  if (nchar(data_block) == 0) {
    return(character(0))
  }

  # Split into lines and clean up
  lines <- strsplit(data_block, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]  # Remove empty lines
  lines <- lines[!grepl("^//", lines)]  # Remove comment lines

  var_names <- character(0)

  for (line in lines) {
    # Look for variable declarations (simplified pattern)
    # Pattern: type<constraints> variable_name;
    # Examples: "int N;", "vector[N] y;", "real<lower=0> sigma;"

    # Remove inline comments
    line <- sub("//.*$", "", line)
    line <- trimws(line)

    if (nchar(line) == 0) next

    # Basic pattern for variable declarations
    # This is a simplified approach - a full parser would be more robust
    if (grepl(";\\s*$", line)) {  # Line ends with semicolon
      # Extract variable name (last word before semicolon)
      clean_line <- gsub(";\\s*$", "", line)  # Remove semicolon
      tokens <- strsplit(clean_line, "\\s+")[[1]]

      if (length(tokens) >= 2) {
        # Variable name is typically the last token
        var_name <- tokens[length(tokens)]

        # Remove array subscripts if present: variable[N] -> variable
        var_name <- gsub("\\[.*\\]", "", var_name)

        # Remove any remaining special characters
        var_name <- gsub("[^a-zA-Z0-9_]", "", var_name)

        if (nchar(var_name) > 0) {
          var_names <- c(var_names, var_name)
        }
      }
    }
  }

  return(unique(var_names))
}

#' @noRd
validate_stan_code <- function(stan_code, backend = "rstan", silent = TRUE, ...) {
  checkmate::assert_string(stan_code)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))

  # Handle empty string case - always error for empty code
  if (nchar(stan_code) == 0) {
    stop(insight::format_error(c(
      "Empty Stan code provided.",
      i = "Stan code must contain at least one character."
    )))
  }

  # Primary validation using rstan::stanc() (most comprehensive and up-to-date)
  if (backend == "rstan") {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Package {.pkg rstan} is required for Stan code validation."
        ),
        i = "Install rstan or use cmdstanr backend."
      )))
    }

    # rstan::stanc() doesn't accept silent parameter, so filter it out
    # Always let rstan::stanc() errors show directly - no masking
    rstan::stanc(model_code = stan_code, verbose = FALSE, ...)
    invisible(TRUE)
  } else {
    # cmdstanr backend (fallback) - pass silent through
    return(parse_model_cmdstanr(stan_code, silent = silent, ...))
  }
}

#' Parse Stan Model Code with cmdstanr
#'
#' @description
#' Validates Stan model code using cmdstanr::cmdstan_model without compilation.
#' Based on existing mvgam patterns in backends.R.
#'
#' @param model Stan model code
#' @param silent Numeric indicating verbosity level
#' @param ... Additional arguments passed to cmdstanr functions
#' @return Validated Stan model code
#' @noRd
parse_model_cmdstanr <- function(model, silent = 1, ...) {
  checkmate::assert_string(model, min.chars = 1)

  # Check if cmdstanr is available
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Package {.pkg cmdstanr} is required for Stan code validation."
      ),
      i = "Install cmdstanr or use rstan backend."
    )))
  }

  # Write Stan model to temporary file - let errors bubble up
  temp_file <- cmdstanr::write_stan_file(model)

  # Validate using cmdstan_model without compilation, using existing eval_silent
  out <- eval_silent(
    cmdstanr::cmdstan_model(temp_file, compile = FALSE, ...),
    type = "message",
    try = TRUE,
    silent = silent > 0L
  )

  if (inherits(out, "try-error")) {
    stop(insight::format_error(c(
      "Stan code validation failed with cmdstanr backend.",
      x = "Check Stan syntax and model structure.",
      i = cli::format_inline(
        "Error details: {attr(out, 'condition')$message}"
      )
    )))
  }

  # Check syntax and return code - let errors bubble up
  out$check_syntax(quiet = TRUE)
  return(paste(out$code(), collapse = "\n"))
}

#'Argument validation functions
#'@param data Data to be validated (list or data.frame)
#'@noRd
validate_series_time = function(
    data,
    name = 'data',
    time_var,
    series_var = NULL,
    check_levels = TRUE,
    check_times = TRUE,
    is_multivariate = FALSE
) {
  # Input validation (NON-NEGOTIABLE per CLAUDE.md)
  checkmate::assert_data_frame(data, .var.name = name)
  checkmate::assert_string(name)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var, null.ok = is_multivariate)  # Allow NULL for multivariate
  checkmate::assert_logical(check_levels, len = 1)
  checkmate::assert_logical(check_times, len = 1)
  checkmate::assert_logical(is_multivariate, len = 1)

  # Preserve mvgam attributes if they exist
  mvgam_attrs <- list(
    mvgam_time = attr(data, "mvgam_time"),
    mvgam_time_source = attr(data, "mvgam_time_source"),
    mvgam_original_time = attr(data, "mvgam_original_time"),
    mvgam_series = attr(data, "mvgam_series"),
    mvgam_series_source = attr(data, "mvgam_series_source")
  )

  # Ungroup any grouped data
  data %>%
    dplyr::ungroup() -> data

  # Common validation: time variable must exist
  if (!time_var %in% colnames(data)) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field {name}} does not contain a {.val {time_var}} variable."
      )
    ), call. = FALSE)
  }

  # Dispatch to appropriate validation strategy
  if (is_multivariate) {
    data <- validate_multivariate_series_time(data, name, time_var, check_times)
  } else {
    data <- validate_univariate_series_time(data, name, time_var, series_var, check_levels, check_times)
  }

  # Restore mvgam attributes if they existed
  for (attr_name in names(mvgam_attrs)) {
    if (!is.null(mvgam_attrs[[attr_name]])) {
      attr(data, attr_name) <- mvgam_attrs[[attr_name]]
    }
  }

  return(data)
}

#' Validate multivariate series time structure
#' @param data Data frame to validate
#' @param name Name for error messages
#' @param time_var Time variable name
#' @param check_times Whether to check time completeness
#' @return Validated data frame
#' @noRd
validate_multivariate_series_time <- function(data, name, time_var, check_times) {
  # Input validation
  checkmate::assert_data_frame(data)
  checkmate::assert_string(name)
  checkmate::assert_string(time_var)
  checkmate::assert_logical(check_times, len = 1)

  # For multivariate models, check time completeness at global level
  if (check_times) {
    min_time <- as.numeric(min(data[[time_var]]))
    max_time <- as.numeric(max(data[[time_var]]))

    unique_times_in_data <- sort(unique(data[[time_var]]))
    expected_times <- seq.int(from = min_time, to = max_time)

    if (!identical(as.numeric(unique_times_in_data), as.numeric(expected_times))) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Time series in {.field {name}} is missing observations for one or more timepoints."
        ),
        i = "Multivariate models require complete time sampling for all responses."
      )), call. = FALSE)
    }
  }

  return(data)
}

#' Validate univariate series time structure
#' @param data Data frame to validate
#' @param name Name for error messages
#' @param time_var Time variable name
#' @param series_var Series variable name
#' @param check_levels Whether to check factor levels
#' @param check_times Whether to check time completeness
#' @return Validated data frame
#' @noRd
validate_univariate_series_time <- function(data, name, time_var, series_var, check_levels, check_times) {
  # Input validation
  checkmate::assert_data_frame(data)
  checkmate::assert_string(name)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_logical(check_levels, len = 1)
  checkmate::assert_logical(check_times, len = 1)

  # Check that series variable exists and is a factor
  if (!series_var %in% colnames(data)) {
    stop(insight::format_error(
      cli::format_inline(
        "{.field {name}} does not contain a {.val {series_var}} variable."
      )
    ), call. = FALSE)
  }

  if (!is.factor(data[[series_var]])) {
    stop(insight::format_error(c(
      cli::format_inline("Variable {.field {series_var}} must be a factor."),
      i = cli::format_inline(
        "Convert to factor using: data${series_var} <- factor(data${series_var})"
      )
    )), call. = FALSE)
  }

  # Check for unused factor levels in series variable
  data <- validate_factor_levels(data, series_var, name, auto_drop = FALSE)

  # Series factor must have all unique levels present if this is a forecast check
  if (check_levels) {
    if (!all(levels(data[[series_var]]) %in% unique(data[[series_var]]))) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Mismatch between factor levels of {.field {series_var}} and unique values."
        ),
        i = cli::format_inline(
          "Use setdiff(levels(data${series_var}), unique(data${series_var})) for guidance."
        )
      )), call. = FALSE)
    }
  }

  # Check time completeness for each series
  if (check_times) {
    all_times_avail = function(time, min_time, max_time) {
      identical(
        as.numeric(sort(time)),
        as.numeric(seq.int(from = min_time, to = max_time))
      )
    }

    # Use attribute-based accessors for series and time data
    time_vals <- get_time_for_grouping(data)
    series_vals <- get_series_for_grouping(data)

    min_time <- as.numeric(min(time_vals))
    max_time <- as.numeric(max(time_vals))

    data.frame(series = series_vals, time = time_vals) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        all_there = all_times_avail(time, min_time, max_time),
        .groups = 'drop'
      ) -> checked_times

    if (any(checked_times$all_there == FALSE)) {
      stop(insight::format_error(
        cli::format_inline(
          "One or more series in {.field {name}} is missing observations for one or more timepoints."
        )
      ), call. = FALSE)
    }
  }

  return(data)
}

#'@noRd
as_one_logical = function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single logical value.", call. = FALSE)
  }
  x
}

#' Validate Grouping Structure
#'
#' @description
#' Validates that grouping variables (gr, subgr) exist, are factors, and have
#' proper hierarchical structure for trend models that use grouping.
#'
#' @param data Data frame to validate
#' @param trend_model mvgam_trend object with grouping specifications
#' @param name Name of data object for error messages
#' @return Original data (validation only, no transformation)
#' @noRd
validate_grouping_structure = function(data, trend_model, name = 'data') {
  checkmate::assert_data_frame(data)

  # Extract variable names from trend_model
  gr_var <- if (!is.null(trend_model$gr) && trend_model$gr != 'NA') trend_model$gr else NULL
  subgr_var <- if (!is.null(trend_model$subgr) && trend_model$subgr != 'NA') trend_model$subgr else NULL

  # If no grouping is used, return early
  if (is.null(gr_var) && is.null(subgr_var)) {
    return(data)
  }

  # Validate grouping variables if they exist
  if (!is.null(gr_var)) {
    # Check gr variable exists and is factor
    if (!gr_var %in% names(data)) {
      stop(insight::format_error(c(
        cli::format_inline(
          "{name} does not contain grouping variable {.val {gr_var}}."
        ),
        x = cli::format_inline(
          "The grouping variable {.val {gr_var}} was specified in the trend constructor but is missing from the data."
        )
      )), call. = FALSE)
    }

    if (!is.factor(data[[gr_var]])) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Grouping variable {.val {gr_var}} must be a factor."
        ),
        i = cli::format_inline(
          "Convert to factor using: data${gr_var} <- factor(data${gr_var})"
        )
      )), call. = FALSE)
    }

    # Check for unused factor levels in gr variable
    data <- validate_factor_levels(data, gr_var, name, auto_drop = FALSE)
  }

  if (!is.null(subgr_var)) {
    # Check subgr variable exists and is factor
    if (!subgr_var %in% names(data)) {
      stop(insight::format_error(c(
        cli::format_inline(
          "{name} does not contain subgrouping variable {.val {subgr_var}}."
        ),
        x = cli::format_inline(
          "The subgrouping variable {.val {subgr_var}} was specified in the trend constructor but is missing from the data."
        )
      )), call. = FALSE)
    }

    if (!is.factor(data[[subgr_var]])) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Subgrouping variable {.val {subgr_var}} must be a factor."
        ),
        i = cli::format_inline(
          "Convert to factor using: data${subgr_var} <- factor(data${subgr_var})"
        )
      )), call. = FALSE)
    }

    # Check for unused factor levels in subgr variable
    data <- validate_factor_levels(data, subgr_var, name, auto_drop = FALSE)
  }

  # If both gr and subgr are specified, validate hierarchical structure
  if (!is.null(gr_var) && !is.null(subgr_var)) {
    validate_complete_grouping(data, gr_var, subgr_var, name)
  }

  return(data)
}

#' Validate and Process Trend Parameters
#'
#' @description
#' Validates and processes complex trend parameters that require data context.
#' This function handles parameter processing that was moved from constructors
#' to provide better data context for validation.
#'
#' @param trend_spec Trend specification
#' @param data Data frame with time series data
#' @return Enhanced trend specification with processed parameters
#' @noRd
validate_and_process_trend_parameters <- function(trend_spec, data) {
  # Input validation with checkmate
  checkmate::assert_list(trend_spec, min.len = 1)
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(trend_spec$trend, min.chars = 1)

  # Process lag parameters for AR/VAR models that have complex lag logic
  if (!is.null(trend_spec$p) && trend_spec$trend %in% c("AR", "VAR")) {
    trend_spec$p <- process_lag_parameters(trend_spec$p, trend_spec$trend)
  }

  # Process capacity parameter for PW models with data context
  if (trend_spec$trend == "PW" && !is.null(trend_spec$cap)) {
    trend_spec$cap <- process_capacity_parameter(trend_spec$cap, data)
  }

  # Add dimension information for downstream processing
  if (!is.null(trend_spec$dimensions)) {
    # Dimensions already extracted, validate consistency with parameters
    if (!is.null(trend_spec$n_lv) && trend_spec$n_lv >= trend_spec$dimensions$n_series) {
      stop(insight::format_error(c(
        cli::format_inline("Factor model requires {.field n_lv < n_series}."),
        x = cli::format_inline(
          "You specified {.field n_lv = {trend_spec$n_lv}} with {trend_spec$dimensions$n_series} series."
        )
      )))
    }
  }

  return(trend_spec)
}

#' Process Lag Parameters
#'
#' @description
#' Processes lag parameters for AR/VAR models, handling complex lag structures.
#'
#' @param p Lag parameter(s)
#' @param trend_type Trend type for context
#' @return Processed lag parameter(s)
#' @noRd
process_lag_parameters <- function(p, trend_type) {
  # Input validation with checkmate
  checkmate::assert_string(trend_type, min.chars = 1)

  if (is.null(p)) {
    return(1L)  # Default lag
  }

  # Validate parameter type and range
  checkmate::assert_integerish(p, lower = 1, min.len = 1, any.missing = FALSE)

  # Convert to integer
  p <- as.integer(p)

  # Additional validation for edge cases
  if (any(p <= 0) || any(!is.finite(p))) {
    stop(insight::format_error(c(
      "Lag parameters must be positive integers.",
      x = cli::format_inline(
        "You specified {.field p = {paste(p, collapse = ', ')}} for {.field {trend_type}} model."
      )
    )))
  }

  # Sort and remove duplicates for consistent processing
  p <- sort(unique(p))

  return(p)
}

#' Process Capacity Parameter
#'
#' @description
#' Processes capacity parameter for piecewise (PW) models with data context validation.
#'
#' @param cap Capacity parameter
#' @param data Data frame for context
#' @return Processed capacity parameter
#' @noRd
process_capacity_parameter <- function(cap, data) {
  # Input validation with checkmate
  checkmate::assert_data_frame(data, min.rows = 1)

  if (is.null(cap)) {
    return(NULL)
  }

  # If character, validate it's a column in data
  if (is.character(cap)) {
    checkmate::assert_string(cap, min.chars = 1)
    validate_required_variables(data, cap, "capacity data")
    return(cap)
  }

  # If numeric, validate it's positive
  if (is.numeric(cap)) {
    checkmate::assert_number(cap, lower = 0, finite = TRUE)
    if (cap <= 0) {
      stop(insight::format_error(c(
        "Capacity must be a positive finite number.",
        x = cli::format_inline("You specified {.field cap = {cap}}.")
      )))
    }
    return(cap)
  }

  stop(insight::format_error(c(
    "Capacity parameter must be either a positive number or a column name.",
    x = cli::format_inline(
      "You specified {.field cap = {cap}} of type {.field {class(cap)}}."
    )
  )))
}

#' Validate Factor + Hierarchical Restriction
#'
#' @description
#' Validates that factor models and hierarchical grouping are not used together.
#' This restriction applies to all trends that support both features.
#'
#' @param trend_specs Trend specification list containing n_lv, gr, subgr parameters
#' @param n_series Number of series from data_info
#' @param trend_name Name of the trend type for error messages (e.g., "RW", "AR", "VAR")
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_no_factor_hierarchical <- function(trend_specs, n_series, trend_name) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_string(trend_name)

  # Check if this is a factor model
  n_lv <- trend_specs$n_lv
  is_factor_model <- !is.null(n_lv) && n_lv < n_series

  # Check if hierarchical grouping is requested
  use_grouping <- !is.null(trend_specs$gr) && trend_specs$gr != 'NA'

  # Factor models are incompatible with hierarchical grouping
  if (use_grouping && is_factor_model) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Hierarchical {trend_name} models cannot use factor models."
      ),
      i = cli::format_inline(
        "Use {.field n_lv = n_series} or remove {.field gr}/{.field subgr} parameters."
      )
    )))
  }

  return(invisible(TRUE))
}

# Formula Parsing Helpers
# =======================
# These functions provide structure-preserving formula manipulation using rlang
# to avoid reformulate() issues with complex formula structures like (1|series)

#' Parse base formula by removing trend constructor terms (structure-preserving)
#'
#' @description
#' Safely removes trend constructor terms from a formula while preserving
#' complex structures like (1|group) random effects. Uses rlang AST manipulation
#' to avoid reformulate() issues.
#'
#' @param trend_formula A formula object containing potential trend terms
#' @param trend_terms Character vector of trend constructor patterns to remove
#' @return A formula object with trend terms removed, preserving structure
#' @noRd
parse_base_formula_safe <- function(trend_formula, trend_terms) {
  # Input validation - non-negotiable per CLAUDE.md
  checkmate::assert_class(trend_formula, "formula")
  checkmate::assert_character(trend_terms, min.len = 0)

  if (length(trend_terms) == 0) {
    return(trend_formula)
  }

  # Extract and clean right-hand side expression using rlang
  rhs_expr <- rlang::f_rhs(trend_formula)
  cleaned_expr <- remove_trend_expressions(rhs_expr, trend_terms, depth = 0)

  # Handle case where all terms were removed
  if (is.null(cleaned_expr)) {
    cleaned_expr <- quote(0)  # Default to no intercept for trend models
  }

  # Reconstruct formula preserving environment (no lhs for trend formulas)
  rlang::new_formula(lhs = NULL, rhs = cleaned_expr, env = rlang::f_env(trend_formula))
}

#' Recursively remove trend expressions from AST
#'
#' @description
#' Walks the expression tree to remove trend constructor calls while preserving
#' the overall formula structure. Handles nested expressions safely.
#'
#' @param expr Expression to process
#' @param trend_patterns Character vector of trend patterns to match
#' @param depth Current recursion depth for protection
#' @return Cleaned expression or NULL if expression should be removed
#' @noRd
remove_trend_expressions <- function(expr, trend_patterns, depth = 0) {
  # Input validation
  checkmate::assert_character(trend_patterns)
  checkmate::assert_int(depth, lower = 0)

  # Recursion depth protection
  if (depth > 50) {
    stop(insight::format_error(c(
      "Formula nesting too deep (>50 levels).",
      i = "Simplify the trend formula structure."
    )))
  }

  # Handle minus operations first to check for unary case
  if (rlang::is_call(expr, "-")) {
    args <- rlang::call_args(expr)
    
    if (length(args) == 1) {
      # Unary minus: preserve operator with processed argument
      arg <- remove_trend_expressions(args[[1]], trend_patterns, depth + 1)
      return(if (is.null(arg)) NULL else rlang::call2("-", arg))
    }
    # Binary minus continues to shared binary logic below
  }
  
  # Handle binary operations (both + and binary -)
  if (rlang::is_call(expr, "+") || rlang::is_call(expr, "-")) {
    op <- rlang::call_name(expr)
    args <- rlang::call_args(expr)
    
    # Validate binary operation structure
    if (length(args) != 2) {
      stop(insight::format_error(c(
        paste0("Invalid ", op, " operation in formula."),
        x = "Expected binary operation with two arguments."
      )))
    }
    
    # Process both operands recursively
    lhs <- remove_trend_expressions(args[[1]], trend_patterns, depth + 1)
    rhs <- remove_trend_expressions(args[[2]], trend_patterns, depth + 1)
    
    # Reconstruct based on remaining operands
    if (is.null(lhs) && is.null(rhs)) {
      return(NULL)
    } else if (is.null(lhs)) {
      # For subtraction with no left operand, create unary minus
      return(if (op == "-") rlang::call2("-", rhs) else rhs)
    } else if (is.null(rhs)) {
      return(lhs)
    } else {
      return(rlang::call2(op, lhs, rhs))
    }
  } else {
    # Check if this expression is a trend term
    if (is_trend_term(expr, trend_patterns)) {
      return(NULL)
    } else {
      return(expr)
    }
  }
}

#' Check if brmsfit is Multivariate
#' @param brmsfit brms model fit object
#' @return Logical indicating if model is multivariate
#' @noRd
is_multivariate_brmsfit <- function(brmsfit) {
  checkmate::assert_class(brmsfit, "brmsfit")

  # Check if formula has multivariate structure
  if (!is.null(brmsfit$formula) && inherits(brmsfit$formula, "mvbrmsformula")) {
    return(TRUE)
  }

  # Check brmsterms for multivariate structure
  if (!is.null(brmsfit$formula)) {
    terms_obj <- try(brms::brmsterms(brmsfit$formula), silent = TRUE)
    if (!inherits(terms_obj, "try-error") && inherits(terms_obj, "mvbrmsterms")) {
      return(TRUE)
    }
  }

  return(FALSE)
}

#' Check if expression matches trend term patterns
#'
#' @description
#' Determines if an expression represents a trend constructor term that
#' should be removed from the base formula.
#'
#' @param expr Expression to check
#' @param trend_patterns Character vector of trend patterns to match against
#' @return Logical indicating if expression is a trend term
#' @noRd
is_trend_term <- function(expr, trend_patterns) {
  # Input validation
  checkmate::assert_character(trend_patterns)

  # Match by the function being CALLED, not by full deparsed text.
  # Reason: full-text fixed-string match fails on argument-literal
  # variants (e.g. pattern "AR(p = 1)" vs expr "AR(p = 1L)" do not
  # match even though both refer to the same AR constructor). The
  # function name is the only stable identifier across literal forms.
  if (!rlang::is_call(expr)) return(FALSE)
  fn_name <- rlang::call_name(expr)
  if (is.null(fn_name)) return(FALSE)
  trend_fn_names <- sub("\\(.*$", "", trend_patterns)
  fn_name %in% trend_fn_names
}

#' Create universal time and series attributes for mvgam grouping
#'
#' Sets up attribute-based time and series variables for consistent (time, series)
#' grouping across all mvgam trend processing. Creates implicit time mapping and
#' handles three series creation strategies. Works in both fitting and prediction contexts.
#'
#' @param data Data frame containing trend data
#' @param parsed_trend Parsed trend formula object with trend_model component (fitting context)
#' @param time_var Character name of time variable column
#' @param series_var Character name of series variable column
#' @param response_vars Character vector of response variable names (for multivariate series creation)
#' @param metadata Trend metadata object for prediction context series recreation
#' @return Data frame with mvgam time and series attributes added
#' @noRd
ensure_mvgam_variables <- function(data, parsed_trend = NULL, time_var = "time", series_var = "series", response_vars = NULL, metadata = NULL) {

  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1, any.missing = FALSE)
  }
  if (!is.null(metadata)) {
    checkmate::assert_list(metadata, names = "named")
    if (!is.null(metadata$variables)) {
      checkmate::assert_list(metadata$variables, names = "named")
    }

    # Validate factor levels in prediction context
    if (!is.null(metadata$levels)) {
      validate_prediction_factor_levels(data, metadata)
    }
  }

  # Always create implicit time mapping for consistency
  checkmate::assert_names(names(data), must.include = time_var)
  unique_times <- unique(data[[time_var]])
  time_mapping <- setNames(seq_along(unique_times), unique_times)
  attr(data, "mvgam_time") <- time_mapping[as.character(data[[time_var]])]
  attr(data, "mvgam_time_source") <- "implicit"
  attr(data, "mvgam_original_time") <- data[[time_var]]  # Store original for distance calculations

  # Helper function to eliminate duplication between strategies
  create_multivariate_series <- function(response_vars, n_obs,
                                         trend_specs = NULL) {
    checkmate::assert_character(response_vars, min.len = 1,
                                any.missing = FALSE, min.chars = 1)
    checkmate::assert_integerish(n_obs, len = 1, lower = 1)
    checkmate::assert_list(trend_specs, null.ok = TRUE)

    # Check if observations can be evenly divided across responses
    if (n_obs %% length(response_vars) != 0) {
      stop(insight::format_error(c(
        "Cannot create series from multivariate structure.",
        x = cli::format_inline(
          "Data has {n_obs} observations but {length(response_vars)} responses."
        ),
        i = "Expected equal observations per response for series creation."
      )), call. = FALSE)
    }

    n_obs_per_response <- n_obs / length(response_vars)

    # Detect shared trends using existing pattern from stan_assembly.R
    is_shared_trend <- FALSE
    if (!is.null(trend_specs) && is.list(trend_specs)) {
      # Apply existing detect_shared_trends logic
      non_null_specs <- trend_specs[!sapply(trend_specs, is.null)]
      if (length(non_null_specs) > 1) {
        first_spec <- non_null_specs[[1]]
        is_shared_trend <- all(sapply(non_null_specs[-1], function(x) {
          identical(x, first_spec, ignore.environment = TRUE)
        }))
      }
    }

    if (is_shared_trend) {
      # Shared trend: create single series for all responses
      list(
        series_values = factor(rep("shared", n_obs)),
        series_source = "multivariate_shared"
      )
    } else {
      # Response-specific trend: create separate series per response
      list(
        series_values = factor(rep(response_vars, each = n_obs_per_response)),
        series_source = "multivariate"
      )
    }
  }

  # Four series creation strategies (including prediction context)
  series_values <- NULL
  series_source <- NULL

  # Strategy 1: Prediction context - use stored metadata to recreate series
  if (!is.null(metadata)) {
    stored_source <- metadata$series_source %||% "explicit"

    if (stored_source == "hierarchical" && !is.null(metadata$variables)) {
      gr_var <- metadata$variables$gr_var
      subgr_var <- metadata$variables$subgr_var

      if (!is.null(gr_var) && !is.null(subgr_var) &&
          !is.na(gr_var) && !is.na(subgr_var) &&
          gr_var != "NA" && subgr_var != "NA") {
        checkmate::assert_names(names(data), must.include = c(gr_var, subgr_var))
        series_values <- interaction(data[[gr_var]], data[[subgr_var]], drop = TRUE, sep = '_', lex.order = TRUE)
        series_source <- "hierarchical"
      }
    } else if (stored_source == "multivariate" && !is.null(metadata$response_vars)) {
      # Use helper function for trend-aware series creation
      result <- create_multivariate_series(metadata$response_vars, nrow(data), metadata$trend_specs)
      series_values <- result$series_values
      series_source <- result$series_source
    }
    # If prediction context but explicit series, fall through to Strategy 3
  }

  # Strategy 2: Hierarchical series (gr + subgr present in fitting context)
  if (is.null(series_values) && !is.null(parsed_trend$trend_model)) {
    gr_var <- if (!is.null(parsed_trend$trend_model$gr) && parsed_trend$trend_model$gr != "NA") parsed_trend$trend_model$gr else NULL
    subgr_var <- if (!is.null(parsed_trend$trend_model$subgr) && parsed_trend$trend_model$subgr != "NA") parsed_trend$trend_model$subgr else NULL

    if (!is.null(gr_var) && !is.null(subgr_var) && subgr_var != series_var) {
      # When subgr is a separate variable, build series from
      # interaction(gr, subgr). When subgr defaults to the existing
      # series column, fall through to Strategy 3 so the original
      # series values are preserved (the codegen path reads the series
      # column directly).
      checkmate::assert_names(names(data), must.include = c(gr_var, subgr_var))
      series_values <- interaction(data[[gr_var]], data[[subgr_var]], drop = TRUE, sep = '_', lex.order = TRUE)
      series_source <- "hierarchical"
    }
  }

  # Strategy 3: Explicit series (column exists)
  if (is.null(series_values) && series_var %in% names(data)) {
    series_values <- data[[series_var]]
    series_source <- "explicit"
  }

  # Strategy 4: Missing series (create from multivariate structure in fitting context)
  if (is.null(series_values)) {
    if (!is.null(response_vars) && length(response_vars) > 1) {
      # Use helper function for trend-aware series creation
      trend_specs_param <- if (!is.null(metadata)) metadata$trend_specs else NULL
      result <- create_multivariate_series(response_vars, nrow(data), trend_specs_param)
      series_values <- result$series_values
      series_source <- result$series_source
    } else {
      stop(insight::format_error(c(
        "No series variable found in data.",
        i = cli::format_inline(
          "Either provide {.field {series_var}} column, hierarchical grouping variables (gr and subgr), or specify response_vars for multivariate series creation."
        )
      )), call. = FALSE)
    }
  }

  # Store series as attribute
  attr(data, "mvgam_series") <- series_values
  attr(data, "mvgam_series_source") <- series_source


  return(data)
}

#' Get time variable for grouping operations
#'
#' Retrieves implicit time indices from data attributes for consistent grouping
#'
#' @param data Data frame with mvgam time attributes
#' @return Numeric vector of sequential time indices (1, 2, 3, ...)
#' @noRd
get_time_for_grouping <- function(data) {
  checkmate::assert_data_frame(data, min.rows = 1)

  time_values <- attr(data, "mvgam_time")
  if (is.null(time_values)) {
    stop(insight::format_error(c(
      "No time variable attribute found.",
      i = "Call ensure_mvgam_variables() first to create time attributes."
    )), call. = FALSE)
  }

  return(time_values)
}

#' Get series variable for grouping operations
#'
#' Retrieves series values from data attributes for consistent grouping
#'
#' @param data Data frame with mvgam series attributes
#' @return Factor or character vector of series identifiers
#' @noRd
get_series_for_grouping <- function(data) {
  checkmate::assert_data_frame(data, min.rows = 1)

  series_values <- attr(data, "mvgam_series")
  if (is.null(series_values)) {
    stop(insight::format_error(c(
      "No series variable attribute found.",
      i = "Call ensure_mvgam_variables() first to create series attributes."
    )), call. = FALSE)
  }

  return(series_values)
}

#' Check if mvgam variables are ready
#'
#' Verifies that both time and series attributes exist on data object
#'
#' @param data Data frame to check for mvgam attributes
#' @return Logical indicating if both time and series attributes exist
#' @noRd
has_mvgam_variables <- function(data) {
  checkmate::assert_data_frame(data)
  !is.null(attr(data, "mvgam_time")) && !is.null(attr(data, "mvgam_series"))
}

#' Remove mvgam variable attributes
#'
#' Cleans up all mvgam-related attributes from data object
#'
#' @param data Data frame with mvgam attributes to remove
#' @return Data frame with mvgam attributes removed
#' @noRd
remove_mvgam_variables <- function(data) {
  checkmate::assert_data_frame(data)
  attr(data, "mvgam_time") <- NULL
  attr(data, "mvgam_time_source") <- NULL
  attr(data, "mvgam_original_time") <- NULL
  attr(data, "mvgam_series") <- NULL
  attr(data, "mvgam_series_source") <- NULL
  return(data)
}

#' Extract and Validate Trend Components (Consolidated)
#'
#' @description
#' Consolidates dual path trend processing by combining data extraction,
#' validation, and dimension injection into a single comprehensive operation.
#' Replaces separate calls to extract_trend_data() and
#' validate_time_series_for_trends() to eliminate redundant
#' extract_time_series_dimensions() computation.
#'
#' @param data Data frame containing time series data
#' @param mv_spec Multivariate specification object with base_formula and
#'   trend_specs
#' @param response_vars Character vector of response variable names
#' @param time_var Name of time variable column (default: "time")
#' @param series_var Name of series variable column (default: "series")
#' @return List with trend_data, enhanced_mv_spec, metadata, and
#'   validation_passed
#' @noRd
extract_and_validate_trend_components <- function(data, mv_spec,
                                                  response_vars,
                                                  time_var = "time",
                                                  series_var = "series",
                                                  trend_formula = NULL) {
  # Parameter validation per CLAUDE.md standards
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(mv_spec)
  required_fields <- c("base_formula", "trend_specs", "has_trends")
  if (!all(required_fields %in% names(mv_spec))) {
    stop(insight::format_error(c(
      cli::format_inline("Invalid {.field mv_spec} structure."),
      i = cli::format_inline(
        "Must contain {.field base_formula}, {.field trend_specs}, and {.field has_trends} fields."
      )
    )), call. = FALSE)
  }
  checkmate::assert_character(response_vars, min.len = 1, null.ok = TRUE)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)

  if (!mv_spec$has_trends) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Cannot process trend components when {.field mv_spec$has_trends} is FALSE."
      ),
      i = "This function should only be called for models with trend specifications."
    )), call. = FALSE)
  }

  # Create attributes early to fix root cause
  parsed_trend <- if (is_multivariate_trend_specs(mv_spec$trend_specs)) {
    mv_spec$trend_specs[[1]]
  } else {
    mv_spec$trend_specs
  }

  # by = lv_axis() machinery: detect per-factor smooth markers in
  # mv_spec$base_formula, rewrite each to `by = .trend`, and inject a
  # `.trend` factor column into trend_data so the existing single brms
  # compile sees a regular factor by-variable. The grain switches from
  # (time, series) to (time, .trend) only when has_by_lv is TRUE; the
  # standard path is unchanged. has_by_lv + n_lv are threaded through
  # to extract_trend_data and downstream stanvar emission.
  has_by_lv <- FALSE
  n_lv_for_grain <- NULL
  if (!is.null(mv_spec$base_formula) &&
      inherits(mv_spec$base_formula, "formula")) {
    by_lv_res <- detect_and_rewrite_by_lv(mv_spec$base_formula)
    if (by_lv_res$has_by_lv) {
      n_lv_for_grain <- parsed_trend$n_lv
      if (is.null(n_lv_for_grain) || n_lv_for_grain < 1L) {
        stop(insight::format_error(c(
          paste0(
            "'by = lv_axis()' requires a factor model ",
            "(n_lv < n_series)."
          ),
          x = paste0(
            "No 'n_lv' is set on the trend spec; the formula uses ",
            by_lv_res$n_by_lv, " per-factor smooth term(s) but no ",
            "factor model is configured."
          ),
          i = paste0(
            "Set 'trend_map = matrix(NA, n_species, n_lv)' (or supply ",
            "a partial-Z matrix) so a factor model is triggered, or ",
            "remove 'by = lv_axis()' from the trend formula."
          )
        )), call. = FALSE)
      }
      has_by_lv <- TRUE
      mv_spec$base_formula <- by_lv_res$formula
      if (by_lv_res$deprecated_trend_seen) {
        warn_legacy_trend_by()
      }

      # When the loadings matrix Z is user-pinned (fully or partially),
      # the rotation concern that `by = lv_axis()` was designed to
      # address is moot: the env constraint and the data-side constraint
      # both pin factor identification, so the QR-skip auto-resolve
      # is a no-op (the standard factor-model emission path is not
      # entered anyway). Surface this as a one-time warning so users
      # know their rotation setting will not influence the fit.
      has_fixed_Z <- !is.null(parsed_trend$fixed_Z) ||
        !is.null(parsed_trend$Z)
      if (has_fixed_Z &&
          !identical(Sys.getenv("TESTTHAT"), "true")) {
        rlang::warn(
          paste0(
            "'by = lv_axis()' was supplied with a user-pinned ",
            "'trend_map' (numeric entries on Z). The per-factor ",
            "smooths still fit, but factor identification is ",
            "already pinned by the user-supplied loadings; the ",
            "rotation auto-skip behaviour does not apply."
          ),
          class = "mvgam_by_lv_with_pinned_Z",
          .frequency = "once",
          .frequency_id = "mvgam_by_lv_with_pinned_Z"
        )
      }
    }
  }

  # Enforce gr/subgr coherence on every trend spec that carries them.
  # validate_trend_grouping is only dispatched via a validation rule that
  # is currently dead, so the gr-requires-subgr check and the
  # gr-constant-per-series check at validate_gr_constant_per_series never
  # fire from the standata path. Call them directly here so both rules
  # apply uniformly.
  groupings <- validate_grouping_arguments(parsed_trend$gr, parsed_trend$subgr)
  parsed_trend$gr <- groupings$gr %||% "NA"
  parsed_trend$subgr <- groupings$subgr %||% "NA"
  if (!is.null(groupings$gr)) {
    validate_gr_constant_per_series(parsed_trend, data)
    # Reason: current Stan template sizes per-group cholesky/sigma
    # blocks by max(series-per-group) and produces NaN at init for
    # unbalanced designs; fail-fast here until ragged-array support
    # lands. Mirrors the gr-constant-per-series check above.
    validate_gr_balanced_groups(parsed_trend, data)
  }

  data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var,
                                response_vars)

  # Compute dimensions once to eliminate redundant calls
  dimensions <- extract_time_series_dimensions(
    data,
    time_var,
    series_var,
    trend_specs = mv_spec$trend_specs,
    response_vars = response_vars,
    cached_formulas = mv_spec$cached_formulas
  )

  # Persist the by_lv grain flags on dimensions so downstream stanvar
  # emission (extract_and_rename_trend_parameters → times_trend) sees
  # the matching axis. The standard (time, series) path stays unchanged
  # when has_by_lv is FALSE.
  dimensions$has_by_lv <- has_by_lv
  dimensions$n_lv_for_grain <- n_lv_for_grain

  # Cross-grain check now that n_series is known. has_by_lv requires
  # n_lv < n_series so a factor model is actually triggered downstream
  # (the existing gate is is_factor_model <- n_lv < n_series).
  if (has_by_lv) {
    n_series_for_check <- dimensions$n_series %||%
      length(dimensions$unique_series %||% character(0))
    if (n_series_for_check < 1L ||
        n_lv_for_grain >= n_series_for_check) {
      stop(insight::format_error(c(
        paste0(
          "'by = lv_axis()' requires a factor model ",
          "(n_lv < n_series)."
        ),
        x = paste0(
          "Configured n_lv = ", n_lv_for_grain,
          " but the data has n_series = ", n_series_for_check, "."
        ),
        i = paste0(
          "Reduce 'n_lv' below the number of series, or remove ",
          "'by = lv_axis()' from the trend formula."
        )
      )), call. = FALSE)
    }
  }

  # Extract trend variables using existing safe functionality
  trend_variables <- character(0)
  all_formula_vars <- character(0)
  if (!is.null(trend_formula)) {
    # Use existing parse_trend_formula with precomputed dimensions to avoid redundant computation
    parsed_trend_result <- parse_trend_formula(trend_formula, data, response_vars,
                                             .precomputed_dimensions = dimensions)
    regular_terms <- parsed_trend_result$regular_terms %||% character(0)

    # Extract all variables using brms formula parsing
    all_formula_vars <- character(0)
    for (term in regular_terms) {
      # Add dummy response for brms parsing
      term_formula <- as.formula(paste("y ~", term))
      bterms <- brms::brmsterms(term_formula)
      
      # Extract predictor variables from fixed effects (excluding intercept)
      term_vars <- character(0)
      if (!is.null(bterms$dpars$mu$fe) && 
          !identical(bterms$dpars$mu$fe, ~ 1)) {
        term_vars <- c(term_vars, all.vars(bterms$dpars$mu$fe))
      }
      
      # Extract variables from smooth terms  
      if (!is.null(bterms$dpars$mu$sm)) {
        sm_allvars <- attr(bterms$dpars$mu$sm, "allvars")
        if (!is.null(sm_allvars)) {
          sm_vars <- all.vars(sm_allvars)
          sm_vars <- sm_vars[sm_vars != "1"]  # Remove intercept
          term_vars <- c(term_vars, sm_vars)
        }
      }
      
      # Extract variables from GP terms
      if (!is.null(bterms$dpars$mu$gp)) {
        gp_allvars <- attr(bterms$dpars$mu$gp, "allvars")
        if (!is.null(gp_allvars)) {
          gp_vars <- all.vars(gp_allvars)
          term_vars <- c(term_vars, gp_vars)
        }
      }
      
      # Extract variables from special predictors (monotonic effects, etc.)
      if (!is.null(bterms$dpars$mu$sp)) {
        sp_allvars <- attr(bterms$dpars$mu$sp, "allvars")
        if (!is.null(sp_allvars)) {
          sp_vars <- all.vars(sp_allvars)
          term_vars <- c(term_vars, sp_vars)
        }
      }
      
      # Extract grouping variables from random effects (needed for data subsetting)
      grouping_vars <- character(0)
      if (!is.null(bterms$dpars$mu$re) && nrow(bterms$dpars$mu$re) > 0) {
        for (i in seq_len(nrow(bterms$dpars$mu$re))) {
          group_factor <- bterms$dpars$mu$re$group[i]
          # Parse nested grouping like "series:habitat" -> c("series", "habitat")
          group_components <- unlist(strsplit(group_factor, ":", fixed = TRUE))
          # Exclude standard mvgam variables that are handled separately
          group_components <- group_components[!group_components %in% c("series", "time")]
          grouping_vars <- c(grouping_vars, group_components)
        }
      }
      
      trend_variables <- c(trend_variables, term_vars)
      all_formula_vars <- c(all_formula_vars, term_vars, grouping_vars)
    }
    trend_variables <- unique(trend_variables)
    all_formula_vars <- unique(all_formula_vars)
  }

  # Add extracted variables to dimensions metadata for extract_trend_data
  # Use all_formula_vars (including grouping variables) for data subsetting
  if (is.null(dimensions$metadata)) {
    dimensions$metadata <- list()
  }
  dimensions$metadata$covariates <- all_formula_vars

  # Data extraction using precomputed dimensions and existing tested functionality
  trend_data <- data
  trend_metadata <- NULL

  if (!is.null(response_vars) && length(response_vars) > 0) {
    # Call existing extract_trend_data with precomputed dimensions to avoid redundant computation
    # This reuses tested logic for proper data reduction to unique (time, series) combinations
    result <- extract_trend_data(
      data, trend_formula, time_var, series_var,
      response_vars = response_vars, .return_metadata = TRUE,
      .precomputed_dimensions = dimensions, trend_specs = mv_spec$trend_specs,
      has_by_lv = has_by_lv, n_lv_for_grain = n_lv_for_grain
    )

    if (!is.list(result) || !all(c("trend_data", "metadata") %in% names(result))) {
      stop(insight::format_error(c(
        "Invalid result from trend data extraction.",
        i = cli::format_inline(
          "Expected list with {.field trend_data} and {.field metadata} fields."
        )
      )), call. = FALSE)
    }

    trend_data <- result$trend_data
    trend_metadata <- result$metadata
  }

  # Validation using precomputed dimensions
  validation_result <- validate_time_series_for_trends(
    data,
    mv_spec$trend_specs,
    response_vars = response_vars,
    cached_formulas = mv_spec$cached_formulas,
    .precomputed_dimensions = dimensions
  )

  if (is.null(validation_result) || is.null(validation_result$dimensions)) {
    stop(insight::format_error(c(
      "Validation returned invalid results.",
      i = cli::format_inline(
        "Expected {.field validation_result} with {.field dimensions} field."
      )
    )), call. = FALSE)
  }

  # Inject dimensions into mv_spec
  enhanced_mv_spec <- mv_spec
  if (is_multivariate_trend_specs(mv_spec$trend_specs)) {
    for (response_name in names(mv_spec$trend_specs)) {
      enhanced_mv_spec$trend_specs[[response_name]]$dimensions <- dimensions
    }
  } else {
    enhanced_mv_spec$trend_specs$dimensions <- dimensions
  }

  return(list(
    trend_data = trend_data,
    enhanced_mv_spec = enhanced_mv_spec,
    metadata = trend_metadata,
    validation_passed = TRUE
  ))
}

#' Collapse a (time, series)-grained data.frame to one row per unique
#' time for the listed time-level covariates.
#'
#' The validator (`extract_trend_data()`) enforces that every entry in
#' `trend_variables` is constant within each (time, series) cell, so
#' "promoting" the values from (time, series) grain to time grain is
#' lossless: we take `dplyr::first()` within each (time, series) and
#' then again within each time. The result is sorted by time and
#' carries one row per unique time value with the listed covariates
#' attached. Used by `extract_trend_data()` when building the
#' (time, .trend) grain for `has_by_lv = TRUE` fits, and by
#' `compose_by_lv_trend_linpred()` when building the matching
#' prediction grid on newdata.
#'
#' @param data Data frame containing the covariates.
#' @param time_vals Time accessor values (parallel to `nrow(data)`).
#' @param series_vals Series accessor values (parallel to `nrow(data)`).
#' @param trend_variables Character vector of column names to collapse.
#'   When empty the function returns a one-row-per-time frame with no
#'   covariates attached.
#' @return Data frame with columns `time, <trend_variables>`, sorted by
#'   time.
#' @noRd
collapse_to_time_level <- function(data, time_vals, series_vals,
                                    trend_variables) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_character(trend_variables)

  if (length(trend_variables) == 0L) {
    return(data.frame(time = sort(unique(time_vals))))
  }

  data %>%
    dplyr::mutate(time = time_vals, series = series_vals) %>%
    dplyr::group_by(.data$time, .data$series) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(trend_variables), dplyr::first),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(trend_variables), dplyr::first),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$time)
}

extract_trend_data <- function(data, trend_formula = NULL, time_var = "time", series_var = "series",
                              mvgam_object = NULL, newdata = NULL, response_vars = NULL,
                              .return_metadata = FALSE, .precomputed_dimensions = NULL, trend_specs = NULL,
                              has_by_lv = FALSE, n_lv_for_grain = NULL) {

  # Input validation for new parameters - non-negotiable per CLAUDE.md
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1)
  }
  checkmate::assert_logical(.return_metadata, len = 1)
  if (!is.null(.precomputed_dimensions)) {
    checkmate::assert_list(.precomputed_dimensions, names = "named")
  }
  checkmate::assert_flag(has_by_lv)
  checkmate::assert_integerish(n_lv_for_grain, lower = 1L, len = 1L,
                                null.ok = TRUE)
  if (has_by_lv && is.null(n_lv_for_grain)) {
    stop(insight::format_error(c(
      "'has_by_lv = TRUE' requires 'n_lv_for_grain' to be set."
    )))
  }

  # In prediction context the grain mode follows the fitted object's
  # trend_metadata, which is restored downstream from
  # mvgam_object$trend_metadata; the caller does not pass these flags.
  # Pull them out of metadata when present so newdata reshaping uses
  # the same grain that was used during fitting.
  if (!is.null(mvgam_object)) {
    md_has_by_lv <- mvgam_object$trend_metadata$has_by_lv %||% FALSE
    if (isTRUE(md_has_by_lv)) {
      has_by_lv <- TRUE
      n_lv_for_grain <- mvgam_object$trend_metadata$n_lv_for_grain
    }
  }

  # Dual-context dispatch: fitting vs prediction
  if (!is.null(mvgam_object)) {
    # PREDICTION CONTEXT: mvgam_object + newdata provided
    checkmate::assert_class(mvgam_object, "mvgam")
    checkmate::assert_data_frame(newdata, min.rows = 1)

    # Get stored metadata from fitted object
    if (is.null(mvgam_object$trend_metadata)) {
      stop(insight::format_error(c(
        cli::format_inline(
          "No trend metadata found in fitted {.cls mvgam} object."
        ),
        x = "This model may have been fitted without trend components.",
        x = "Or it was fitted with an older version that didn't store metadata."
      )), call. = FALSE)
    }

    metadata <- mvgam_object$trend_metadata
    data <- newdata  # Use newdata as data for extraction
    time_var <- metadata$variables$time_var %||% "time"
    series_var <- metadata$variables$series_var %||% "series"

    # Get covariates from stored metadata instead of parsing formula
    trend_variables <- character(0)
    if (!is.null(metadata$covariates) && length(metadata$covariates) > 0) {
      trend_variables <- unique(metadata$covariates)
    }

    # Validate newdata has required variables
    required_vars <- unique(c(time_var, series_var, trend_variables))
    missing_vars <- setdiff(required_vars, names(newdata))

    if (length(missing_vars) > 0) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Missing required variables in {.arg newdata}."
        ),
        x = cli::format_inline(
          "Required variables: {.field {required_vars}}"
        ),
        x = cli::format_inline("Missing: {.field {missing_vars}}"),
        i = "Ensure newdata contains all variables used during model fitting."
      )), call. = FALSE)
    }

    # Get grouping variables from metadata
    grouping_vars <- character(0)
    if (!is.null(metadata$variables$gr_var) && metadata$variables$gr_var != "NA") {
      grouping_vars <- c(grouping_vars, metadata$variables$gr_var)
    }
    if (!is.null(metadata$variables$subgr_var) && metadata$variables$subgr_var != "NA") {
      grouping_vars <- c(grouping_vars, metadata$variables$subgr_var)
    }

    # Create attribute-based time and series variables for prediction context using metadata
    data <- ensure_mvgam_variables(data, NULL, time_var, series_var, NULL, metadata)

  } else {
    # FITTING CONTEXT: data + trend_formula provided
    checkmate::assert_data_frame(data, min.rows = 1)
    checkmate::assert_class(trend_formula, "formula")
    checkmate::assert_string(time_var)
    checkmate::assert_string(series_var)
    # Only require time_var - series_var can be created via attributes if missing
    checkmate::assert_names(names(data), must.include = time_var)

    # Use precomputed dimensions - no fallback in ultra-DRY architecture
    if (is.null(.precomputed_dimensions)) {
      stop(insight::format_error(c(
        "Missing precomputed dimensions in ultra-DRY architecture.",
        x = "This function should only be called with precomputed dimensions.",
        i = "Check that extract_and_validate_trend_components() is passing dimensions correctly."
      )), call. = FALSE)
    }

    # Extract everything from precomputed dimensions - skip parse_trend_formula entirely
    trend_variables <- .precomputed_dimensions$metadata$covariates %||% character(0)
    # Pull the trend model out of trend_specs so downstream metadata
    # builders see gr/subgr/trend (otherwise the top-level
    # trend_metadata$variables$gr_var falls through to NA).
    # Safety note: ensure_mvgam_variables() only constructs the
    # interaction-based hierarchical series when BOTH gr and subgr are
    # present (see line ~3138). With only gr set, the hierarchical
    # series-creation path is not triggered, so populating trend_model
    # with a real spec here is non-invasive for non-hierarchical models.
    single_spec <- if (!is.null(trend_specs)) {
      if (is_multivariate_trend_specs(trend_specs)) trend_specs[[1]] else trend_specs
    } else {
      NULL
    }
    parsed_trend <- list(trend_model = single_spec)

    # Create attribute-based time and series variables for fitting context
    data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var, response_vars,
                                   metadata = list(trend_specs = trend_specs))

    # Validate trend covariates don't include response variables
    if (!is.null(response_vars) && length(trend_variables) > 0) {
      offending_vars <- intersect(trend_variables, response_vars)
      if (length(offending_vars) > 0) {
        stop(insight::format_error(c(
          "Response variables cannot be used as trend predictors:",
          x = cli::format_inline(
            "Offending variables: {.field {offending_vars}}"
          ),
          i = "Trend models require exogenous covariates only.",
          i = "Consider using these variables in the observation formula instead.",
          i = "See ?mvgam_formulas for guidance on proper covariate specification."
        )), call. = FALSE)
      }
    }

    # Validate trend covariate invariance within grouping structure
    if (length(trend_variables) > 0) {
      # Skip validation for CAR models (no shared latent states)
      skip_invariance <- !is.null(parsed_trend$trend_model) &&
                        identical(parsed_trend$trend_model$trend, "CAR")

      if (!skip_invariance) {
        # Extract accessor values once to avoid duplication
        time_vals <- get_time_for_grouping(data)
        series_vals <- get_series_for_grouping(data)

        # Create temporary columns with unique names to avoid collisions
        validation_data <- data %>%
          dplyr::mutate(
            .validation_time_temp = time_vals,
            .validation_series_temp = series_vals
          )

        # Determine grouping structure from trend metadata
        validation_grouping_vars <- character(0)

        if (!is.null(parsed_trend$trend_model)) {
          has_gr <- !is.null(parsed_trend$trend_model$gr) &&
                    parsed_trend$trend_model$gr != "NA"
          has_subgr <- !is.null(parsed_trend$trend_model$subgr) &&
                       parsed_trend$trend_model$subgr != "NA"

          if (has_gr && has_subgr) {
            validation_grouping_vars <- c(".validation_time_temp",
                                         parsed_trend$trend_model$gr,
                                         parsed_trend$trend_model$subgr)
            grouping_desc <- paste0("(time, ",
                                   parsed_trend$trend_model$gr, ", ",
                                   parsed_trend$trend_model$subgr, ")")
          } else if (has_gr) {
            validation_grouping_vars <- c(".validation_time_temp",
                                         parsed_trend$trend_model$gr)
            grouping_desc <- paste0("(time, ",
                                   parsed_trend$trend_model$gr, ")")
          } else {
            # Always use time and series from attributes
            validation_grouping_vars <- c(".validation_time_temp",
                                         ".validation_series_temp")
            grouping_desc <- "(time, series)"
          }
        } else {
          # Always use time and series from attributes
          validation_grouping_vars <- c(".validation_time_temp",
                                       ".validation_series_temp")
          grouping_desc <- "(time, series)"
        }

        # Validate trend variables exist (grouping vars are temporary)
        missing_trend_vars <- setdiff(trend_variables, names(validation_data))
        if (length(missing_trend_vars) > 0) {
          stop(insight::format_error(c(
            "Required trend variables not found in data:",
            x = cli::format_inline("Missing: {.field {missing_trend_vars}}"),
            i = paste("Available:", paste(names(data), collapse = ", "))
          )), call. = FALSE)
        }

        # A grouping variable is trivially constant within its own
        # groups, so exclude it from the across() to avoid a tidyselect
        # error when gr_var also appears in trend_variables (e.g.
        # ~ x + (x | habitat) + ZMVN(gr = habitat)).
        trend_vars_to_check <- setdiff(trend_variables,
                                        validation_grouping_vars)

        varying_covariates <- if (length(trend_vars_to_check) > 0) {
          validation_data %>%
            dplyr::group_by(
              dplyr::across(dplyr::all_of(validation_grouping_vars))
            ) %>%
            dplyr::summarise(
              dplyr::across(dplyr::all_of(trend_vars_to_check),
                           ~ length(unique(.x)) > 1),
              .groups = "drop"
            ) %>%
            dplyr::select(-dplyr::all_of(validation_grouping_vars)) %>%
            dplyr::summarise(dplyr::across(dplyr::everything(), any)) %>%
            dplyr::select(dplyr::where(isTRUE)) %>%
            names()
        } else {
          character(0)
        }

        if (length(varying_covariates) > 0) {
          stop(insight::format_error(c(
            paste0("Trend covariates must be constant within ",
                   grouping_desc, " groups:"),
            x = cli::format_inline(
              "Varying covariates: {.field {varying_covariates}}"
            ),
            i = paste0("Each ", grouping_desc,
                       " combination must have identical covariate values."),
            i = "Consider aggregating data or using observation-level effects instead.",
            i = "See ?mvgam_data_structure for data preparation guidance."
          )), call. = FALSE)
        }
      }
    }

    # Extract grouping variables from parsed trend
    grouping_vars <- character(0)
    if (!is.null(parsed_trend$trend_model)) {
      if (!is.null(parsed_trend$trend_model$gr) && parsed_trend$trend_model$gr != "NA") {
        grouping_vars <- c(grouping_vars, parsed_trend$trend_model$gr)
      }
      if (!is.null(parsed_trend$trend_model$subgr) && parsed_trend$trend_model$subgr != "NA") {
        grouping_vars <- c(grouping_vars, parsed_trend$trend_model$subgr)
      }
    }
  }

  # Universal (time, series) grouping using attribute-based accessors
  # Following dplyr best practices: mutate first, then group_by with .data pronouns

  # Extract attribute values with validation
  time_vals <- get_time_for_grouping(data)
  series_vals <- get_series_for_grouping(data)

  # When `by = lv_axis()` is present, switch trend_data from the
  # default (time, series) grain to a (time, .trend) grain: one row
  # per (unique_time, latent_factor) combination, with the injected
  # .trend factor column carrying levels 1:n_lv. The trend covariates
  # are first reduced to one value per (time, series) cell (taking
  # `dplyr::first()` to mirror the standard path) and then promoted to
  # one value per time (assumed time-level: the existing covariate
  # invariance check above enforces constant-within-(time, series),
  # which together with the n_lv < n_series gate means time-level
  # values are unambiguous).
  if (has_by_lv) {
    time_level <- collapse_to_time_level(
      data, time_vals, series_vals, trend_variables
    )
    lv_grid <- tidyr::expand_grid(
      time = time_level$time,
      .trend = factor(seq_len(n_lv_for_grain))
    )
    trend_data <- if (length(trend_variables) > 0L) {
      dplyr::arrange(
        dplyr::left_join(lv_grid, time_level, by = "time"),
        .data$time, .data$.trend
      )
    } else {
      lv_grid
    }
    trend_data <- remove_mvgam_variables(trend_data)
  } else if (length(trend_variables) > 0) {
    trend_data <- data %>%
      dplyr::mutate(
        time = time_vals,
        series = series_vals
      ) %>%
      dplyr::group_by(.data$time, .data$series) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(trend_variables), dplyr::first),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$time, .data$series)

    # Clean up attributes after processing
    trend_data <- remove_mvgam_variables(trend_data)
  } else {
    # Handle no covariates case with same attribute approach
    trend_data <- data %>%
      dplyr::mutate(
        time = time_vals,
        series = series_vals
      ) %>%
      dplyr::group_by(.data$time, .data$series) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(.data$time, .data$series) %>%
      dplyr::select("time", "series")

    trend_data <- remove_mvgam_variables(trend_data)
  }

  # Create metadata for fitted object storage
  if (.return_metadata && !is.null(mvgam_object)) {
    # Prediction context - metadata already available
    metadata <- mvgam_object$trend_metadata
  } else if (.return_metadata) {
    # Fitting context - create metadata from parsed trend
    metadata <- list(
      trend_type = parsed_trend$trend_model$trend %||% NA_character_,
      covariates = trend_variables,
      variables = list(
        time_var = time_var,
        series_var = series_var,
        gr_var = if (!is.null(parsed_trend$trend_model$gr) &&
                     parsed_trend$trend_model$gr != "NA") {
                   parsed_trend$trend_model$gr
                 } else NA_character_,
        subgr_var = if (!is.null(parsed_trend$trend_model$subgr) &&
                        parsed_trend$trend_model$subgr != "NA") {
                      parsed_trend$trend_model$subgr
                    } else NA_character_
      ),
      is_car = !is.null(parsed_trend$trend_model) &&
               identical(parsed_trend$trend_model$trend, "CAR"),
      # Context-guarded fields for prediction context
      time_source = if (has_mvgam_variables(data)) {
        attr(data, "mvgam_time_source")
      } else {
        NULL
      },
      series_source = if (has_mvgam_variables(data)) {
        attr(data, "mvgam_series_source")
      } else {
        NULL
      },
      response_vars = if (is.null(mvgam_object)) response_vars else NULL,
      # by = lv_axis() grain switch: persist so prediction rebuilds
      # newdata at the matching (time, .trend) grid via the same
      # extract_trend_data code path under the prediction context.
      has_by_lv = has_by_lv,
      n_lv_for_grain = n_lv_for_grain,
      # Store factor levels for prediction validation
      levels = list(
        series = if (is.factor(series_vals)) {
          levels(series_vals)
        } else {
          sort(unique(as.character(series_vals)))
        },
        gr = extract_factor_levels(
          data,
          if (!is.null(parsed_trend$trend_model$gr) &&
              parsed_trend$trend_model$gr != "NA") {
            parsed_trend$trend_model$gr
          } else {
            NA_character_
          }
        ),
        subgr = extract_factor_levels(
          data,
          if (!is.null(parsed_trend$trend_model$subgr) &&
              parsed_trend$trend_model$subgr != "NA") {
            parsed_trend$trend_model$subgr
          } else {
            NA_character_
          }
        )
      )
    )
  }

  # Backward compatible return
  if (.return_metadata) {
    return(list(trend_data = trend_data, metadata = metadata))
  } else {
    return(trend_data)
  }
}

#' Format Pipeline Error with Context
#'
#' @description
#' Creates detailed error message with context information for debugging
#' pipeline failures. Follows established validation.R error patterns.
#'
#' @param message Character string with base error message
#' @param context List with optional debugging context (default: NULL)
#'
#' @return Stops execution with formatted error
#'
#' @noRd
format_pipeline_error <- function(message, context = NULL) {
  checkmate::assert_character(message, len = 1, min.chars = 1)
  checkmate::assert_list(context, null.ok = TRUE)
  
  # Build error message components
  error_components <- c(message)
  
  # Add context information if provided
  if (!is.null(context) && length(context) > 0) {
    for (name in names(context)) {
      if (!is.null(context[[name]])) {
        context_info <- sprintf("%s: {.field %s}", name, 
                               as.character(context[[name]])[1])
        error_components <- c(error_components, context_info)
      }
    }
  }
  
  # Format and stop with error
  full_message <- paste(error_components, collapse = ". ")
  stop(insight::format_error(full_message), call. = FALSE)
}


# Normalise the user-supplied `loadings_prior` specification into
# a structured list consumed by `make_loadings_prior_stanvars()`.
# Resolves names against `data2`, validates dimensions against
# `n_series`, encodes feature columns via
# `encode_loadings_features()` and validates each pairwise
# distance via `validate_pairwise_distance()`.
#
# Accepted shapes for the user-facing list:
#   loadings_prior = list(
#     features         = "<name>" or a matrix / data.frame,
#     distances        = "<name>", character vector of names,
#                        a single matrix, named list of matrices,
#                        or unnamed list (auto-named "dist_1",
#                        "dist_2", ...),
#     column_shrinkage = "iid" (default) or "mgp",
#     mgp_a1, mgp_a2   = numeric MGP hyperparameters (only used
#                        when column_shrinkage == "mgp"; defaults
#                        2 and 3 following Heaps & Jermyn 2024
#                        Sect. 6.3.1).
#   )
#
# Returns NULL when input is NULL. Otherwise returns a list with
# elements:
#   features_mat       p x c numeric matrix or NULL
#   distance_mats      named list of p x p numeric matrices
#                      (possibly empty)
#   column_shrinkage   "iid" or "mgp"
#   mgp_a1, mgp_a2     numeric (NA when shrinkage = "iid")
#   n_series           int p
#   n_features         int c (0 when features is NULL)
#   n_distances        int K (length of distance_mats)
#'@noRd
normalise_loadings_prior <- function(input, data2, data,
                                     n_series = NULL) {
  if (is.null(input)) return(NULL)
  # String shorthand: `loadings_prior = "mgp"` is sugar for
  # `loadings_prior = list(column_shrinkage = "mgp")` with default
  # MGP hyperparameters (a1 = 2, a2 = 4). Pure MGP (no features /
  # distances) is the Bhattacharya & Dunson (2011) parameterisation
  # and is mathematically defined for any positive integer n_lv.
  if (is.character(input) && length(input) == 1L) {
    checkmate::assert_choice(input, "mgp")
    input <- list(column_shrinkage = input)
  }
  checkmate::assert_list(input, names = "named")
  allowed <- c(
    "features", "distances", "column_shrinkage",
    "mgp_a1", "mgp_a2"
  )
  unknown <- setdiff(names(input), allowed)
  if (length(unknown) > 0L) {
    stop(insight::format_error(c(
      "Unknown 'loadings_prior' fields.",
      x = paste0(
        "Unrecognised: ",
        paste0("'", unknown, "'", collapse = ", "), "."
      ),
      i = paste0(
        "Accepted fields: ",
        paste0("'", allowed, "'", collapse = ", "), "."
      )
    )))
  }
  uses_mgp_shorthand <- identical(input$column_shrinkage, "mgp")
  if (is.null(input$features) && is.null(input$distances) &&
      !uses_mgp_shorthand) {
    stop(insight::format_error(c(
      paste0(
        "'loadings_prior' must supply at least one of ",
        "'features', 'distances', or 'column_shrinkage = \"mgp\"'."
      ),
      i = paste0(
        "An empty spec collapses to the default iid prior; ",
        "drop the argument instead."
      )
    )))
  }
  series_levels <- if (is.factor(data$series)) {
    levels(data$series)
  } else if (!is.null(data$series)) {
    sort(unique(as.character(data$series)))
  } else {
    stop(insight::format_error(c(
      "'loadings_prior' requires a 'series' column on 'data'.",
      i = "Add a 'series' factor / character column to 'data'."
    )))
  }
  n_series_actual <- length(series_levels)
  if (!is.null(n_series) && n_series != n_series_actual) {
    stop(insight::format_error(c(
      "Mismatch between supplied n_series and series levels.",
      x = paste0(
        "Got n_series = ", n_series, ", levels(data$series) = ",
        n_series_actual, "."
      )
    )))
  }
  features_mat <- resolve_features_input(input$features, data2)
  distance_mats <- resolve_distances_input(input$distances, data2)
  if (!is.null(features_mat)) {
    features_mat <- encode_loadings_features(
      features_mat, series_levels
    )
  }
  if (length(distance_mats) > 0L) {
    distance_mats <- mapply(
      function(mat, nm) {
        validate_pairwise_distance(
          mat, n_series_actual, nm,
          series_levels = series_levels
        )
      },
      distance_mats, names(distance_mats),
      SIMPLIFY = FALSE
    )
  }
  shrinkage <- input$column_shrinkage %||% "iid"
  checkmate::assert_choice(shrinkage, c("iid", "mgp"))
  mgp_a1 <- if (shrinkage == "mgp") input$mgp_a1 %||% 2 else NA_real_
  # `mgp_a2 = 4` sits in the [3, 5] range Schiavon, Canale and
  # Dunson (2022, Biometrics 78:995) and Legramanti, Durante and
  # Dunson (2020, JRSS-B) recommend for the moderate-n ecology /
  # community regime (n in the tens to low hundreds). The original
  # Bhattacharya-Dunson (2011) `a2 = 2.1` was calibrated for
  # `p >> n` and is too weak to produce visible truncation in the
  # mvgam target setting; the package surfaces `a2` via
  # `loadings_prior = list(mgp_a2 = ...)` for sensitivity work.
  mgp_a2 <- if (shrinkage == "mgp") input$mgp_a2 %||% 4 else NA_real_
  if (shrinkage == "mgp") {
    checkmate::assert_number(mgp_a1, lower = .Machine$double.eps)
    checkmate::assert_number(mgp_a2, lower = .Machine$double.eps)
  } else if (!is.null(input$mgp_a1) || !is.null(input$mgp_a2)) {
    stop(insight::format_error(c(
      paste0(
        "'mgp_a1' / 'mgp_a2' supplied but ",
        "'column_shrinkage' is not 'mgp'."
      ),
      i = "Set column_shrinkage = 'mgp' to use these hyperparameters."
    )))
  }
  length_scale_collinearity_warning(features_mat, distance_mats)
  imbalance_warning(features_mat)
  list(
    features_mat = features_mat,
    distance_mats = distance_mats,
    column_shrinkage = shrinkage,
    mgp_a1 = mgp_a1,
    mgp_a2 = mgp_a2,
    n_series = n_series_actual,
    n_features = if (is.null(features_mat)) 0L else ncol(features_mat),
    n_distances = length(distance_mats)
  )
}


# Resolve the `features` field of `loadings_prior` against
# `data2`. Accepts a single string lookup, a matrix, or a
# data.frame. Returns the unencoded object (encoding happens in
# the normaliser via `encode_loadings_features()`).
#'@noRd
resolve_features_input <- function(features, data2) {
  if (is.null(features)) return(NULL)
  if (is.character(features) && length(features) == 1L) {
    if (is.null(data2) || !features %in% names(data2)) {
      stop(insight::format_error(c(
        paste0(
          "'loadings_prior$features' = '", features,
          "' not found in 'data2'."
        ),
        i = paste0(
          "Supply 'data2 = list(", features,
          " = <matrix or data.frame>)' or pass the object inline."
        )
      )))
    }
    return(data2[[features]])
  }
  if (is.matrix(features) || is.data.frame(features)) {
    return(features)
  }
  stop(insight::format_error(c(
    "'loadings_prior$features' has an unsupported type.",
    x = paste0("Got: ", class(features)[1L], "."),
    i = paste0(
      "Supply a single 'data2' lookup string, a numeric matrix, ",
      "or a data.frame."
    )
  )))
}


# Resolve the `distances` field of `loadings_prior` against
# `data2`. Returns a named list of pairwise distance matrices
# (possibly empty). Auto-names unnamed list entries
# "dist_1", "dist_2", ... and inline matrices "dist_1".
#'@noRd
resolve_distances_input <- function(distances, data2) {
  if (is.null(distances)) return(list())
  if (is.character(distances)) {
    if (length(distances) == 0L) return(list())
    missing_names <- setdiff(
      distances, if (is.null(data2)) character(0) else names(data2)
    )
    if (length(missing_names) > 0L) {
      stop(insight::format_error(c(
        "'loadings_prior$distances' references missing names.",
        x = paste0(
          "Not in 'data2': ",
          paste0("'", missing_names, "'", collapse = ", "), "."
        ),
        i = paste0(
          "Add the missing matrix / matrices to 'data2' or pass ",
          "them inline as a named list."
        )
      )))
    }
    assert_distance_names_unreserved(distances)
    return(stats::setNames(
      lapply(distances, function(nm) data2[[nm]]),
      distances
    ))
  }
  if (is.matrix(distances)) {
    return(list(dist_1 = distances))
  }
  if (is.list(distances)) {
    if (length(distances) == 0L) return(list())
    nms <- names(distances)
    if (is.null(nms) || any(nms == "")) {
      nms <- paste0("dist_", seq_along(distances))
      names(distances) <- nms
    }
    assert_distance_names_unreserved(names(distances))
    bad <- vapply(distances, function(d) {
      !(is.matrix(d) || is.data.frame(d))
    }, logical(1))
    if (any(bad)) {
      stop(insight::format_error(c(
        paste0(
          "'loadings_prior$distances' list entries must be ",
          "matrices."
        ),
        x = paste0(
          "Bad entries: ",
          paste(nms[bad], collapse = ", "), "."
        )
      )))
    }
    return(distances)
  }
  stop(insight::format_error(c(
    "'loadings_prior$distances' has an unsupported type.",
    x = paste0("Got: ", class(distances)[1L], "."),
    i = paste0(
      "Supply a single name string, a character vector of ",
      "'data2' names, a single matrix, or a named list of ",
      "matrices."
    )
  )))
}


# Check that `loadings_prior` is coherent with `trend_map`:
# - cannot combine with fully-fixed Z (no parameters left to put
#   a prior on)
# - cannot combine with partial Z (NAs in trend_map mark free
#   entries, but Heaps' framework treats all entries jointly)
# Both error. Called from make_stan after both arguments have
# been normalised.
#'@noRd
# Defensive consistency check on a normalised loadings-prior
# spec, called from `make_loadings_prior_stanvars()` before
# stanvar emission. Validates that the required fields are
# present and that the count fields agree with the actual
# matrix dimensions, catching silent corruption between the
# normaliser and the emitter.
#'@noRd
assert_loadings_prior_spec_consistent <- function(spec) {
  checkmate::assert_list(spec)
  required <- c(
    "features_mat", "distance_mats", "column_shrinkage",
    "mgp_a1", "mgp_a2", "n_series", "n_features", "n_distances"
  )
  missing <- setdiff(required, names(spec))
  if (length(missing) > 0L) {
    stop(insight::format_error(c(
      "Loadings-prior spec is missing required fields.",
      x = paste0(
        "Missing: ",
        paste0("'", missing, "'", collapse = ", "), "."
      ),
      i = "Build the spec via `normalise_loadings_prior()`."
    )))
  }
  if (!is.null(spec$features_mat) &&
      spec$n_features != ncol(spec$features_mat)) {
    stop(insight::format_error(c(
      "Loadings-prior spec has inconsistent feature dimensions.",
      x = paste0(
        "spec$n_features = ", spec$n_features,
        " but ncol(features_mat) = ",
        ncol(spec$features_mat), "."
      ),
      i = "Rebuild the spec via `normalise_loadings_prior()`."
    )))
  }
  if (spec$n_distances != length(spec$distance_mats)) {
    stop(insight::format_error(c(
      "Loadings-prior spec has inconsistent distance counts.",
      x = paste0(
        "spec$n_distances = ", spec$n_distances,
        " but length(distance_mats) = ",
        length(spec$distance_mats), "."
      ),
      i = "Rebuild the spec via `normalise_loadings_prior()`."
    )))
  }
  invisible(NULL)
}


# Guard against user-supplied distance names that would collide
# with the auto-naming scheme (`dist_1`, `dist_2`, ...) used for
# unnamed inline list entries. Also reserves any name starting
# with `dist_` to avoid silent Stan-variable shadowing further
# down the emission. Reserved names error early with a clear
# fix message rather than later as a Stan compile failure.
#'@noRd
assert_distance_names_unreserved <- function(nms) {
  bad <- nms[grepl("^dist_", nms)]
  if (length(bad) > 0L) {
    stop(insight::format_error(c(
      paste0(
        "'loadings_prior$distances' names cannot start with ",
        "'dist_'."
      ),
      x = paste0(
        "Reserved: ",
        paste0("'", bad, "'", collapse = ", "), "."
      ),
      i = paste0(
        "Stan emission uses 'dist_<name>' / 'theta_dist_<name>' ",
        "internally; rename to avoid collisions."
      )
    )))
  }
  invisible(NULL)
}


assert_loadings_prior_compatible <- function(loadings_prior_spec,
                                             trend_map_Z) {
  # Compatibility contract (matching the user-facing roxygen on
  # `mvgam()` / `jsdgam()` `trend_map` + `loadings_prior` args):
  # - trend_map_Z = NULL (no fixed entries; canonical free-Z trigger
  #   via `n_lv`, or the all-NA jsdgam mask which
  #   `normalise_trend_map_on_specs()` collapses to NULL): COMPATIBLE.
  # - trend_map_Z mixed (some fixed values, some NAs): INCOMPATIBLE.
  #   Partial-Z parameterises Z element-wise via a vector parameter
  #   under an iid student_t prior; the matrix-normal loadings_prior
  #   wires onto the assembled Z matrix in tparameters and would
  #   silently overlap with the per-element prior.
  # - trend_map_Z fully fixed (no NAs): INCOMPATIBLE. No free
  #   parameters for the structured prior to act on.
  if (is.null(loadings_prior_spec) || is.null(trend_map_Z)) {
    return(invisible(NULL))
  }
  any_free <- anyNA(trend_map_Z)
  if (any_free) {
    stop(insight::format_error(c(
      paste0(
        "'loadings_prior' cannot combine with a partial 'trend_map' ",
        "(mixed fixed entries and NAs)."
      ),
      i = paste0(
        "Drop 'trend_map' to apply the structured prior to a free ",
        "loadings matrix (the n_lv argument alone triggers the ",
        "factor model), or drop 'loadings_prior' to keep the ",
        "user-supplied partial pattern. An all-NA mask is treated ",
        "as 'no fixed entries' upstream and does combine cleanly ",
        "with 'loadings_prior'."
      )
    )))
  }
  stop(insight::format_error(c(
    paste0(
      "'loadings_prior' cannot combine with a fully-fixed ",
      "'trend_map'."
    ),
    i = paste0(
      "Fixed loadings have no free parameters to put a prior on; ",
      "drop one of the two arguments."
    )
  )))
}
