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
#' @param default_cap Integer scalar, or `NULL`. The fixed bound the
#'   family supplies when the frame carries no `cap` column. `NULL`
#'   means the family either requires the column or derives the
#'   bound per unit from the data, and in both cases there is no
#'   fabricated cap to check.
#' @param cap_required Logical; TRUE when the family requires the
#'   `cap` data column to be present. FALSE for families that
#'   default the per-unit upper truncation (e.g. `occ()` defaults
#'   to `cap = 1`). Royle-Nichols carries binary response but
#'   keeps `cap_required = TRUE` because its latent abundance can
#'   exceed one.
#' @param unit_grouping_vars Optional character vector of column
#'   names identifying one closure unit. Defaults to
#'   `c(series_var, time_var)` so existing 2-axis grouping is
#'   unchanged. Multi-season families (`occ(multi_season = TRUE)`,
#'   `nmix(multi_season = TRUE)`) pass a 3-axis grouping such as
#'   `c("series", "site", "time")` so each (species, site, season)
#'   forms one closure unit with replicate visits inside. The cap-
#'   constancy and single-visit identifiability checks all generalise
#'   over the supplied grouping cardinality.
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
                                        cap_required        = TRUE,
                                        default_cap         = NULL,
                                        unit_grouping_vars  = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(response_var)
  checkmate::assert_string(series_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(cap_var)
  checkmate::assert_flag(has_obs_covariates)
  checkmate::assert_flag(has_det_covariates)
  checkmate::assert_flag(binary_y_check)
  checkmate::assert_flag(cap_required)
  checkmate::assert_int(default_cap, lower = 1L, null.ok = TRUE)
  if (is.null(unit_grouping_vars)) {
    unit_grouping_vars <- closure_unit_key_vars(
      NULL, series_var = series_var, time_var = time_var
    )
  }
  checkmate::assert_character(unit_grouping_vars, min.len = 1L,
                               any.missing = FALSE, unique = TRUE)
  # Pretty-print of the grouping tuple for messages, e.g.
  # "(species, site, season)".
  grouping_label <- paste0("(", paste(unit_grouping_vars, collapse = ", "),
                            ")")

  # Required columns: response + grouping. The cap column is
  # optional only for families that default the per-unit upper
  # truncation (e.g. occ() defaults to 1); count-latent families
  # such as nmix() and nmix("royle_nichols") always require it.
  required_cols <- c(response_var, unit_grouping_vars)
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
          "Each row of 'data' is one visit; the ", grouping_label,
          " tuple identifies a closure unit and '", cap_var,
          "' bounds the latent state per unit."
        )
      )))
    }
  }

  y_vals   <- data[[response_var]]
  # A `cap` column the user supplied is checked below whatever the
  # family. Where there is none, the family supplies the bound
  # instead: a fixed default -- `occ()`'s one, the Royle-Nichols
  # `nmix()`'s static ceiling -- is checked the same way, while a
  # data-driven buffer is by construction at least the largest count
  # in its unit and leaves nothing to check.
  #
  # Fabricating a cap of one here instead refused every count family
  # that declared a buffer, and did it on a message naming a column
  # the frame never had.
  cap_vals <- if (cap_var %in% colnames(data)) {
    data[[cap_var]]
  } else if (!is.null(default_cap)) {
    rep(as.integer(default_cap), nrow(data))
  } else {
    NULL
  }

  # A missing response is a visit that did not happen, which is
  # routine in repeat-visit designs. Those rows carry no count to
  # check, so every response check below runs over the observed
  # visits and the unit arrays skip the rest.
  observed <- !is.na(y_vals)
  if (!any(observed)) {
    stop(insight::format_error(c(
      paste0("All values of '", response_var, "' are missing."),
      i = paste0(
        "Closure-unit families need at least one observed visit to ",
        "estimate detection."
      )
    )))
  }
  y_obs <- suppressWarnings(as.numeric(y_vals[observed]))
  if (any(!is.finite(y_obs))) {
    stop(insight::format_error(
      paste0(
        "Non-finite or non-numeric values found in '",
        response_var, "'."
      )
    ))
  }
  y_int <- rep(NA_integer_, length(y_vals))
  y_int[observed] <- as.integer(y_vals[observed])
  if (any(y_int[observed] < 0L)) {
    stop(insight::format_error(
      paste0("Negative counts found in '", response_var, "'.")
    ))
  }
  if (any(abs(y_obs - y_int[observed]) > 1e-8)) {
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
  if (binary_y_check && any(y_int[observed] > 1L, na.rm = TRUE)) {
    bad <- which(!is.na(y_int) & y_int > 1L)[1L]
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

  if (!is.null(cap_vals) &&
        any(!is.finite(suppressWarnings(as.numeric(cap_vals))))) {
    stop(insight::format_error(
      paste0(
        "Non-finite or non-numeric values found in '",
        cap_var, "'."
      )
    ))
  }
  # Without a cap there is nothing to check; a zero-length vector
  # would make each test below pass by arithmetic rather than by
  # decision, which reads the same and means something else.
  cap_int <- if (is.null(cap_vals)) NULL else as.integer(cap_vals)
  if (!is.null(cap_int) && any(cap_int < 1L)) {
    stop(insight::format_error(
      paste0("'", cap_var, "' must be a positive integer.")
    ))
  }
  if (!is.null(cap_int) && any(cap_int < y_int, na.rm = TRUE)) {
    bad <- which(!is.na(y_int) & cap_int < y_int)[1L]
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
  # time rather than mid-array-build. The grouping is polymorphic
  # over the cardinality of `unit_grouping_vars`: each column is
  # coerced to a factor-integer code and concatenated.
  # Read the unit layout from the builder's own derivation rather
  # than repeating it, and read it over the visits that happened.
  # Counting every row instead let both guards below pass on frames
  # whose fitted model is the case they exist to refuse: a unit
  # whose rows are all missing was still counted as a unit, and a
  # unit left with one observed visit still counted as two.
  idx        <- closure_unit_index(data, unit_grouping_vars,
                                   response_var)
  unit_int   <- idx$unit
  # Every unit the frame names, which is the axis the cap check
  # below walks; a unit that lost its response still has a cap.
  n_unit_named <- length(idx$levels)
  # And the units the likelihood will actually hold, with their
  # observed visit counts, which is what the guards read.
  rep_counts <- idx$n_rep[idx$n_rep > 0L]
  n_unit <- length(rep_counts)
  for (g in seq_len(n_unit_named)) {
    rows_g <- which(unit_int == g)
    cap_g  <- cap_int[rows_g]
    if (!is.null(cap_int) && length(unique(cap_g)) > 1L) {
      bad_row <- rows_g[1L]
      # Build a (col=value, ...) tuple description of the bad unit
      # for the diagnostic.
      bad_tuple <- paste(
        vapply(unit_grouping_vars, function(col) {
          paste0(col, "=", as.character(data[[col]][bad_row]))
        }, character(1L)),
        collapse = ", "
      )
      stop(insight::format_error(c(
        paste0(
          "'", cap_var, "' must be constant within a closure unit."
        ),
        x = paste0(
          "Closure unit (", bad_tuple,
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
        "Only ", n_unit, " unique ", grouping_label,
        " combination found."
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
          insight::format_message(c(
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
        insight::format_message(c(
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


#' Validate required variables exist in data
#'
#' @description
#' Checks that all required variables are present in the provided data.
#' Automatically filters out latent parameters from nonlinear formulas
#' to avoid validation errors for model-defined parameters.
#'
#' Extract response (LHS) variable names from a formula-like object
#'
#' Handles plain `formula`, `brmsformula` (uses the
#' `$formula` slot), and `mvbrmsformula` (iterates `$forms`).
#' Returns an empty character vector for one-sided formulas,
#' NULL input, or non-formula objects so callers can rely on a
#' character vector return shape.
#'
#' `rlang::f_lhs()` cannot be used directly because it errors on
#' `brmsformula` objects (they are lists internally, not true
#' `formula`s).
#'
#' @noRd
extract_response_vars <- function(formula) {
  if (is.null(formula)) return(character(0L))
  if (inherits(formula, "mvbrmsformula") && !is.null(formula$forms)) {
    return(unique(unlist(lapply(formula$forms, function(x) {
      extract_response_vars(x$formula %||% x)
    }))))
  }
  if (inherits(formula, "brmsformula")) {
    formula <- formula$formula
  }
  if (!inherits(formula, "formula") || length(formula) < 3L) {
    return(character(0L))
  }
  all.vars(formula[[2L]])
}


#' Extract predictor variable names from one or more formulas
#'
#' Returns the unique names of variables that appear on the
#' right-hand side of the supplied formula(s), suitable for use
#' with `data[[var]]` lookups. Handles plain `formula`,
#' `brmsformula` (including multi-arm fits via `pforms`), and
#' `mvbrmsformula` (multivariate response via `forms`). Skips
#' `NULL` entries silently so callers can pass a list of optional
#' formulas without prior filtering.
#'
#' The brmsterms-based predictor walk inside
#' `extract_and_validate_trend_components()` (also in this file)
#' is a more elaborate variant that yields metadata for the
#' dimension-computation pipeline. This helper is intentionally a
#' thinner pure-R wrapper aimed at the pre-fit NA check, which
#' only needs the column names. Keeping the two parallel avoids
#' reaching into the dimensions pipeline for a much simpler use
#' case.
#'
#' @param formulas A single formula, brmsformula, mvbrmsformula,
#'   or a list mixing any of those.
#' @return Character vector of unique variable names (RHS only,
#'   response variables excluded). Empty when nothing useful is
#'   found.
#' @noRd
extract_predictor_vars <- function(formulas) {
  if (is.null(formulas)) return(character(0L))
  # brmsformula / mvbrmsformula are lists internally; treat them
  # as single inputs (not iterables) at the entry point so the
  # collector recurses into $formula / $pforms / $forms correctly.
  if (inherits(formulas, c("formula", "brmsformula", "mvbrmsformula")) ||
      !is.list(formulas)) {
    formulas <- list(formulas)
  }

  collect <- function(f) {
    if (is.null(f)) return(character(0L))
    if (inherits(f, c("brmsformula", "mvbrmsformula"))) {
      forms <- list()
      if (!is.null(f$formula)) forms <- c(forms, list(f$formula))
      if (!is.null(f$pforms))  forms <- c(forms, unname(f$pforms))
      if (!is.null(f$forms))   forms <- c(forms, unname(lapply(
        f$forms, function(x) x$formula %||% x
      )))
      unlist(lapply(forms, collect))
    } else if (inherits(f, "formula")) {
      # RHS only when response is on LHS; full formula otherwise.
      rhs <- if (length(f) == 3L) f[[3L]] else f
      lhs_vars <- if (length(f) == 3L) all.vars(f[[2L]]) else character(0L)
      setdiff(all.vars(rhs), lhs_vars)
    } else {
      character(0L)
    }
  }

  unique(unlist(lapply(formulas, collect)))
}


#' Validate that no formula-referenced covariate contains NAs
#'
#' brms's default `na_action = na_omit` silently drops rows with
#' `NA` in any model-frame column. That is harmless for the
#' response (mvgam preserves the trend time grid separately and
#' only skips the dropped rows in the likelihood) but it is
#' fatal for covariates: the trend pipeline expects a row at
#' every timepoint, and a missing-covariate row breaks the
#' dimension alignment downstream in Stan with an opaque
#' chain-failure error. This pre-fit check raises an error at
#' the validator layer naming the offending columns.
#'
#' @param data A data frame or list of vectors / matrices.
#' @param formulas A single formula, brmsformula, mvbrmsformula,
#'   or list of any of those. NULL elements are skipped.
#' @param response_vars Character vector of response column
#'   names to exclude from the check (NAs in the response are
#'   allowed and preserved by mvgam).
#' @param context String used in the error message to identify
#'   the offending data object (e.g. `"data"` or `"newdata"`).
#' @return Invisible NULL on success; an informative error on
#'   failure listing which columns carry how many NAs.
#' @noRd
validate_no_covariate_nas <- function(data, formulas,
                                        response_vars = character(0L),
                                        context = "data") {
  if (is.null(data) || is.null(formulas)) {
    return(invisible(NULL))
  }
  predictor_vars <- extract_predictor_vars(formulas)
  predictor_vars <- setdiff(predictor_vars, response_vars)

  # Drop names that aren't data columns. These are typically NSE
  # bare names from trend constructors (e.g. AR(time = week)) or
  # bs / k literals brms has already absorbed -- not covariates.
  # `data` may be a data frame or a list (for matrix predictors).
  available <- names(data)
  predictor_vars <- intersect(predictor_vars, available)
  if (length(predictor_vars) == 0L) {
    return(invisible(NULL))
  }

  na_counts <- vapply(predictor_vars, function(v) {
    col <- data[[v]]
    # `is.na()` handles vectors AND matrices uniformly; a matrix
    # column (e.g. distributed-lag predictor) returns a logical
    # matrix and `sum()` counts every NA cell.
    if (is.null(col)) 0L else as.integer(sum(is.na(col)))
  }, integer(1L))

  bad <- na_counts[na_counts > 0L]
  if (length(bad) == 0L) {
    return(invisible(NULL))
  }

  bad_lines <- vapply(seq_along(bad), function(i) {
    paste0("'", names(bad)[i], "': ", bad[i], " NA",
           if (bad[i] > 1L) "s" else "")
  }, character(1L))

  stop(insight::format_error(c(
    paste0(
      "Covariates referenced by the formula contain ",
      "missing values in '", context, "'."
    ),
    x = paste(bad_lines, collapse = "; "),
    i = paste0(
      "mvgam preserves NAs in the response to maintain the ",
      "time grid (the likelihood simply skips those rows), but ",
      "covariates that appear in the formula must be complete ",
      "for the trend pipeline to align across timepoints. Drop ",
      "the NA rows, impute the covariate, or remove that column ",
      "from the formula before fitting."
    )
  )))
}


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
#' the future `nmix`/`jsdgam` ports) calls this function, so there
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
#'   \item `data.frame(series, trend)`: one row per series
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
#' The series a `trend_map` or `loadings_prior` row belongs to
#'
#' Both arguments are matrices with one row per series, handed in
#' before any axis has been resolved, so both have to name the
#' series for themselves. They asked the same question in the same
#' ten lines and differed only in which of them the error message
#' named.
#'
#' The levels are the ones something is observed at, not the ones a
#' factor happens to declare. A column carrying a level nothing
#' reaches gave a matrix a row for a series the trend does not
#' have, and the argument was then refused for having the wrong
#' number of rows.
#'
#' @param data The user's data frame.
#' @param argument Name of the argument being normalised, for the
#'   message a frame naming no series receives.
#' @return Character vector of series levels, in axis order.
#' @noRd
argument_series_levels <- function(data, argument) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(argument)
  if (is.null(data$series)) {
    stop(insight::format_error(c(
      paste0("'", argument, "' requires a 'series' column on 'data'."),
      i = "Add a 'series' factor / character column to 'data'."
    )))
  }
  as.character(observed_series_levels(data$series))
}




#' @noRd
normalise_trend_map <- function(input, data) {
  if (is.null(input)) return(NULL)
  checkmate::assert_data_frame(data)
  series_levels <- argument_series_levels(data, "trend_map")
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


#' Does any trend in a `trend_specs` payload require regular time
#' intervals?
#'
#' @description
#' Returns `TRUE` when at least one trend's `validation_rules`
#' contains `"requires_regular_intervals"`. Handles three shapes:
#' `NULL` (returns `FALSE`), a single `mvgam_trend` object, or a
#' named list of trend specs. Used by the data validators
#' upstream so the gate matches each trend's declared rule rather
#' than a hardcoded trend-type predicate.
#'
#' @param trend_specs `NULL`, an `mvgam_trend`, or a list of
#'   `mvgam_trend` objects.
#' @return Logical scalar.
#' @noRd
any_trend_requires_regular_intervals <- function(trend_specs) {
  if (is.null(trend_specs)) return(FALSE)
  rule <- "requires_regular_intervals"
  specs <- if (inherits(trend_specs, "mvgam_trend")) {
    list(trend_specs)
  } else if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs
  } else {
    list(trend_specs)
  }
  for (spec in specs) {
    if (rule %in% (spec$validation_rules %||% character(0))) {
      return(TRUE)
    }
  }
  FALSE
}
#' The series a spec will give a frame, before any axis exists
#'
#' The two grouping validators run before the axis is resolved and
#' both need the same answer: which series each row belongs to under
#' this specification. They asked for it differently, and each way
#' was wrong in its own direction. One read the specification flat
#' and vanished silently on the nested spelling; both returned
#' without checking whenever the frame carried no `series` column,
#' which is exactly the frame a grouping names its series for.
#'
#' A `gr` that names no column of the frame is refused here rather
#' than passed on. Left alone it reached Stan assembly, after brms
#' setup, and was reported there against a variable the user never
#' wrote.
#'
#' @param trend_spec A trend specification, in either spelling.
#' @param data The frame being validated.
#' @return A list with `gr_var` and the per-row `series`, or `NULL`
#'   when the spec names no grouping.
#' @noRd
spec_series_values <- function(trend_spec, data) {
  groupings <- spec_groupings(trend_spec)
  gr_var <- groupings$gr
  if (!named_var(gr_var)) {
    return(NULL)
  }
  if (!gr_var %in% colnames(data)) {
    stop(insight::format_error(c(
      paste0("Grouping variable '", gr_var, "' is not in the data."),
      x = paste0(
        "Columns present: ", paste(colnames(data), collapse = ", "), "."
      ),
      i = "'gr' must name a column that says which group a series is in."
    )), call. = FALSE)
  }
  subgr <- groupings$subgr
  series_var <- trend_spec$series %||%
    trend_spec$trend_model$series %||% "series"
  series <- if (named_var(subgr) && !identical(subgr, series_var) &&
                  subgr %in% colnames(data)) {
    hierarchical_series_values(data, gr_var, subgr)
  } else if (series_var %in% colnames(data)) {
    data[[series_var]]
  } else {
    return(NULL)
  }
  list(gr_var = gr_var, series = series, series_var = series_var)
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
  resolved <- spec_series_values(trend_spec, data)
  if (is.null(resolved)) {
    return(invisible(NULL))
  }
  gr_var <- resolved$gr_var
  series_var <- resolved$series_var

  # The series counted are the series the trend will have, which is
  # what lets an unbalanced design be caught: sized by the largest
  # group, the smaller groups take a slice of a correlation matrix
  # they never asked for, and every index stays in range.
  series_vals <- resolved$series

  # One group value per series, taken by the helper that owns that
  # question. It reads the prepared axis when the frame carries one
  # and the labels it is handed otherwise, which is this case: the
  # balance is checked before any axis has been resolved.
  series_group_table <- table(series_group_values(
    data, series_var, gr_var, labels = series_vals
  ))
  # A factor column keeps levels nothing observes, and a group with
  # no series is not an unbalanced group.
  series_group_table <- series_group_table[series_group_table > 0L]
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
      "Every group must hold the same number of series, because the ",
      "per-group blocks share one size. Subset the data to a ",
      "balanced design, or combine small groups."
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
  resolved <- spec_series_values(trend_spec, data)
  if (is.null(resolved)) {
    return(invisible(NULL))
  }
  gr_var <- resolved$gr_var
  series_vec <- resolved$series
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


#' Detect whether a trend spec describes a factor model
#'
#' Single helper for the `is_factor_model` gate. Returns TRUE iff
#' `n_lv` is set and `n_lv <= n_series`. The `<=` admits the MGP
#' truncation-ceiling case `n_lv = n_series`; the wrapper-layer
#' `validate_n_lv_ceiling()` refuses more factors than series so
#' this predicate never spuriously promotes a default-prior fit to
#' a degenerate full-rank factor model.
#'
#' @param n_lv Integer or NULL (the `n_lv` slot from a trend spec).
#' @param n_series Positive integer.
#' @return Logical scalar.
#' @noRd
is_factor_model_spec <- function(n_lv, n_series) {
  if (is.null(n_lv)) return(FALSE)
  n_lv <= n_series
}

#' The latent dimension a fitted trend runs in
#'
#' The post-fit face of `is_factor_model_spec()`, and the one place
#' that answers the question for a fitted object. Reading the Stan
#' dimension instead cannot answer it: a non-factor fit carries
#' `N_lv_trend == N_series_trend` with an identity `Z` in transformed
#' data, and that is the same dimension a factor fit takes at its
#' ceiling, which `n_lv = n_series` reaches under an MGP loadings
#' prior or a `by = lv_axis()` term. What separates the two is
#' whether the fit asked for factors, which the trend spec records
#' and `N_lv_trend` has already forgotten.
#'
#' Asking through the shared predicate gives every post-fit surface
#' the answer codegen used when it decided whether to sample `Z`.
#'
#' For multivariate trend specs (one per response in an mvbind fit)
#' the first spec is used, which is the whole story unless the
#' responses were given different `n_lv`.
#'
#' A caller that knows the series count passes it. Otherwise the
#' fit's own `N_series_trend` answers, and where even that is
#' missing the ceiling test is skipped rather than guessed at: a
#' requested `n_lv` is taken at face value, which is what this
#' returned before it consulted the predicate at all, and which
#' `validate_n_lv_ceiling()` has already made safe by refusing
#' `n_lv > n_series` at fit time. Guessing a series count instead
#' would let a wrong guess silently withdraw factor handling from
#' a fit that has it.
#'
#' @param object A fitted `mvgam` object.
#' @param n_series Number of observed series. Defaults to the count
#'   the fit was built with.
#' @return Integer latent dimension, or NULL when the trend is not a
#'   factor model.
#' @noRd
detect_factor_n_lv <- function(object, n_series = NULL) {
  # Whether the user asked for latent factors, which is a different
  # question from how many latent columns the trend has. The record
  # answers the second: a model with no factor constructor still
  # gets one column per series, so `axes$factor$n_lv` is populated
  # for every fit and reading it here reported an ordinary trend as
  # a factor model with as many factors as series, sending every
  # caller looking for a `Z` that was never sampled.
  n_lv <- spec_n_lv(first_trend_spec(object))
  if (is.null(n_lv) || !is.numeric(n_lv) || n_lv < 1L) {
    return(NULL)
  }
  # The series count has one owner, and it is the record.
  n_series <- n_series %||% mvgam_axes(object)$series$n %||%
    object$standata$N_series_trend
  # A model with as many factors as series is not a factor model:
  # every series loads on its own state and there is nothing to
  # plot as a loading.
  if (!is.null(n_series) && !is_factor_model_spec(n_lv, n_series)) {
    return(NULL)
  }
  as.integer(n_lv)
}

#' Validate the `n_lv` ceiling
#'
#' Shared entry-point gate for `jsdgam()` and `mvgam()`. More
#' factors than series is refused whatever prior the loadings
#' carry, because the marginal `Z Z'` has rank at most the number
#' of series and the extra columns add no expressive capacity.
#' `n_lv = n_series` is allowed: the loadings prior is what decides
#' whether that boundary samples well, and saying so here would
#' refuse a model the prior makes admissible.
#'
#' @noRd
validate_n_lv_ceiling <- function(n_lv, n_species,
                                  fit_function = "mvgam") {
  checkmate::assert_int(n_lv, lower = 1)
  checkmate::assert_int(n_species, lower = 2)
  checkmate::assert_choice(fit_function, c("mvgam", "jsdgam"))
  # `jsdgam()` users think in species; mvgam-direct users think in
  # series. The check is the same; only the noun changes.
  noun <- if (identical(fit_function, "jsdgam")) "species" else "series"
  n_lv_int <- as.integer(n_lv)
  if (n_lv_int > n_species) {
    stop(insight::format_error(c(
      paste0(
        "'n_lv' cannot exceed the number of ", noun, "."
      ),
      x = paste0(
        "Got n_lv = ", n_lv_int, ", n_", noun, " = ", n_species, "."
      ),
      i = paste0(
        "The marginal residual covariance has rank at most n_",
        noun, ", so additional columns of 'Z' add no expressive ",
        "capacity. With `loadings_prior = \"mgp\"`, increase ",
        "'mgp_a2' (e.g. to 5) for stronger column shrinkage instead."
      )
    )))
  }
  # n_lv = n_species is allowed; n_lv > n_species is caught above.
  invisible(TRUE)
}

#' Refuse an `n_lv` written where nothing reads it
#'
#' The factor count belongs to the trend that carries the factors,
#' so it is written on the constructor inside `trend_formula`, as
#' `AR(p = 1, n_lv = 2)`. Passed to `mvgam()` itself it lands in
#' `...`, which forwards to brms and Stan, and nothing there reads
#' it: a two-series frame asked for one factor came back with two
#' and nothing recorded that the request had been raised.
#'
#' @param requested_n_lv Whatever arrived as a top-level `n_lv`.
#' @return `invisible(TRUE)`.
#' @noRd
refuse_top_level_n_lv <- function(requested_n_lv) {
  if (is.null(requested_n_lv)) return(invisible(TRUE))
  stop(insight::format_error(c(
    "Argument 'n_lv' is not read by 'mvgam()'.",
    x = paste0(
      "It reached the arguments forwarded to brms and Stan, where ",
      "no factor count is read, so the trend would have been given ",
      "one latent state per series."
    ),
    i = paste0(
      "Write it on the trend instead, as ",
      "'trend_formula = ~ AR(p = 1, n_lv = ", requested_n_lv,
      ")', or use 'jsdgam(n_lv = ", requested_n_lv, ")'."
    )
  )), call. = FALSE)
}


#' Refuse a factor model on one named trend
#'
#' The registry records whether a trend decomposes into latent
#' factors, and the reason it does not, so the refusal is composed
#' from what is registered rather than restated wherever a factor
#' request can arrive. Both remedies are named whichever argument
#' raised the request, so the four routes to a factor trend give a
#' user one sentence to act on.
#'
#' @param trend_name Registered trend name, such as `"PW"`.
#' @return `invisible(TRUE)` when the trend takes factors.
#' @noRd
refuse_factor_request_for_trend <- function(trend_name) {
  ensure_registry_initialized()
  info <- get_trend_info(trend_name)
  if (isTRUE(info$supports_factors)) return(invisible(TRUE))
  stop(insight::format_error(c(
    paste0("Factor models are not supported for ", trend_name,
           " trends."),
    x = info$incompatibility_reason %||%
      paste0(trend_name, " has no latent-factor form."),
    i = paste0(
      "Drop 'n_lv' and 'trend_map', or use a trend that ",
      "decomposes into factors: AR, RW, VAR, ZMVN."
    )
  )), call. = FALSE)
}


#' Refuse a factor model on a trend that has no factor form
#'
#' Whether a trend decomposes into latent factors is recorded on the
#' registry, as `supports_factors` alongside the reason it does not.
#' Nothing read it: each constructor refused on its own, so the rule
#' held only for the one route that goes through the constructor.
#' Asked four ways of a piecewise trend, `PW(n_lv = 1)` was refused
#' while `trend_map = matrix(NA, 2, 1)` and a `jsdgam()`
#' `factor_formula` each built a one-factor model, and a top-level
#' `n_lv` was dropped so silently that `N_lv_trend` came back at the
#' series count.
#'
#' This is the one place a factor request meets the trend it was
#' asked of, so it reads the registry rather than restating the
#' rule. A request reaches a spec two ways and both are counted:
#' `n_lv` on the spec, and a `trend_map` normalised to a fixed `Z`.
#' The third spelling, `n_lv` passed to `mvgam()` itself, reaches no
#' spec at all and is refused by `refuse_top_level_n_lv()` before
#' this runs.
#'
#' @param trend_specs Parsed trend specifications, one spec or a
#'   per-response list.
#' @return `invisible(TRUE)`.
#' @noRd
enforce_factor_support_against_specs <- function(trend_specs) {
  if (is.null(trend_specs)) return(invisible(TRUE))
  ensure_registry_initialized()
  specs <- if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs
  } else {
    list(trend_specs)
  }
  for (spec in specs) {
    if (is.null(spec$n_lv) && is.null(spec$fixed_Z)) next
    trend_name <- get_trend_name(spec)
    if (is.null(trend_name) || !nzchar(trend_name)) next
    refuse_factor_request_for_trend(trend_name)
  }
  invisible(TRUE)
}


#' Walk trend specs to find `n_lv` and apply the ceiling gate
#'
#' Wrapper-layer hook called once from
#' `generate_stan_components()` after the loadings prior is
#' attached. Reads `n_lv` from the first factor-model trend spec
#' it finds (multivariate trend lists may be a single spec or a
#' per-response list, but mvgam currently fits one shared trend
#' so the first hit is the authoritative one), counts unique
#' series from `data`, and dispatches to
#' `validate_n_lv_ceiling()`. No-ops when no spec carries `n_lv`.
#'
#' @noRd
enforce_n_lv_ceiling_against_data <- function(trend_specs, data,
                                              fit_function = "mvgam") {
  if (is.null(trend_specs)) return(invisible(TRUE))
  specs <- if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs
  } else {
    list(trend_specs)
  }
  for (spec in specs) {
    n_lv <- spec$n_lv
    if (is.null(n_lv) || !is.numeric(n_lv) || n_lv < 1L) next
    # `fixed_Z` (set by `trend_map = matrix(...)` or `"identity"`)
    # supplies Z directly; no Z prior is sampled, so the iid funnel
    # at `n_lv = n_series` does not apply. The downstream
    # invariant gates still reject `n_lv > n_series`.
    if (!is.null(spec$fixed_Z)) next
    series_var <- spec$series_var %||% spec$series %||% "series"
    if (!series_var %in% colnames(data)) next
    n_series <- length(unique(data[[series_var]]))
    if (n_series < 2L) next
    validate_n_lv_ceiling(
      n_lv         = as.integer(n_lv),
      n_species    = as.integer(n_series),
      fit_function = fit_function
    )
    break
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


# Response support per family: the interval a response must lie
# in, whether each endpoint is included, and whether values must
# be whole numbers. Families brms also defines carry brms's own
# values, read from `family_info(family, "ybounds")`, `"closed"`
# and `use_int()`. `test-mvgam-data.R` drives brms through its
# public interface and asserts it refuses exactly what this table
# refuses, so the two cannot drift apart. mvgam's native families
# are not in brms and carry their own entries.
#
# Ordinal, categorical and multi-response families are absent on
# purpose: their responses are levels rather than points on an
# interval, and brms checks them against the category count it
# derives from the data. Closure-unit families go through
# `validate_closure_unit_data()`.
#'@noRd
mvgam_response_support <- local({
  count <- list(bounds = c(0, Inf), closed = c(TRUE, NA),
                integer = TRUE)
  positive <- list(bounds = c(0, Inf), closed = c(FALSE, NA),
                   integer = FALSE)
  unit_open <- list(bounds = c(0, 1), closed = c(FALSE, FALSE),
                    integer = FALSE)
  counts <- c("poisson", "negbinomial", "negbinomial2",
              "geometric", "binomial",
              "beta_binomial", "com_poisson", "discrete_weibull",
              "hurdle_poisson", "hurdle_negbinomial",
              "zero_inflated_poisson", "zero_inflated_negbinomial",
              "zero_inflated_binomial",
              "zero_inflated_beta_binomial",
              "com_binomial", "beta_nb")
  positives <- c("gamma", "lognormal", "weibull", "exponential",
                 "frechet", "inverse.gaussian",
                 "shifted_lognormal", "wiener")
  out <- c(
    stats::setNames(rep(list(count), length(counts)), counts),
    stats::setNames(rep(list(positive), length(positives)),
                    positives)
  )
  out$bernoulli <- list(bounds = c(0, 1), closed = c(TRUE, TRUE),
                        integer = TRUE)
  out$beta <- unit_open
  out$betar <- unit_open
  out$dirichlet <- unit_open
  out$logistic_normal <- unit_open
  out$zero_inflated_beta <- list(bounds = c(0, 1),
                                 closed = c(TRUE, FALSE),
                                 integer = FALSE)
  out$zero_one_inflated_beta <- list(bounds = c(0, 1),
                                     closed = c(TRUE, TRUE),
                                     integer = FALSE)
  out$xbeta <- list(bounds = c(0, 1), closed = c(TRUE, TRUE),
                    integer = FALSE)
  out$von_mises <- list(bounds = c(-pi, pi), closed = c(TRUE, TRUE),
                        integer = FALSE)
  # The hurdle continuous families place a point mass at zero, so
  # they admit it where their base family does not.
  out$hurdle_gamma <- list(bounds = c(0, Inf), closed = c(TRUE, NA),
                           integer = FALSE)
  out$hurdle_lognormal <- out$hurdle_gamma
  out$tweedie <- list(bounds = c(0, Inf), closed = c(TRUE, NA),
                      integer = FALSE)
  out
})


# Render a family's support as the inequality a reader can check
# their own column against, e.g. "0 < y < 1" or "y >= 0".
#'@noRd
describe_response_support <- function(spec, y_name) {
  lo <- spec$bounds[1L]
  hi <- spec$bounds[2L]
  parts <- character()
  if (is.finite(lo)) {
    parts <- c(parts, paste0(
      format(lo, digits = 4L), if (isTRUE(spec$closed[1L])) " <= " else " < "
    ))
  }
  body <- paste0(paste(parts, collapse = ""), "'", y_name, "'")
  if (is.finite(hi)) {
    body <- paste0(
      body, if (isTRUE(spec$closed[2L])) " <= " else " < ",
      format(hi, digits = 4L)
    )
  }
  if (isTRUE(spec$integer)) {
    body <- paste0(body, " (integers only)")
  }
  body
}


# Internal: response-vs-family shape check for non-closure-unit
# families. Closure-unit families (`occ()`, `nmix()` variants)
# go through `validate_closure_unit_data()` instead, which does
# its own integer / binary / cap / non-negative checks. Driven by
# `mvgam_response_support`, so a family is covered by naming it
# there rather than by another branch here.
#'@noRd
validate_response_for_family <- function(y, family, y_name = "y") {
  # A factor response carries no numeric order to range-check, and
  # the families that accept one (bernoulli, the ordinal and
  # categorical families) are validated by brms against their own
  # level requirements. Comparing a factor to floor() errors.
  if (!is.numeric(y) && !is.logical(y)) return(invisible(TRUE))
  y_nz <- y[!is.na(y)]
  if (!length(y_nz)) return(invisible(TRUE))
  fam_name <- resolve_family_name(family)
  spec <- mvgam_response_support[[tolower(fam_name)]]
  if (is.null(spec)) return(invisible(TRUE))

  observed <- paste0("Observed range: [", format(min(y_nz), digits = 4L),
                     ", ", format(max(y_nz), digits = 4L), "].")
  present <- !is.na(y)
  lo <- spec$bounds[1L]
  hi <- spec$bounds[2L]
  below <- if (!is.finite(lo)) rep(FALSE, length(y)) else if (
    isTRUE(spec$closed[1L])) present & y < lo else present & y <= lo
  above <- if (!is.finite(hi)) rep(FALSE, length(y)) else if (
    isTRUE(spec$closed[2L])) present & y > hi else present & y >= hi
  outside <- below | above

  if (any(outside)) {
    stop(insight::format_error(c(
      paste0("'", y_name, "' has values outside the support of ",
             "family '", fam_name, "'."),
      x = paste0("Requires: ",
                 describe_response_support(spec, y_name), "."),
      x = paste0(observed, " ", sum(outside), " of ", sum(present),
                 " values fall outside, the first at row ",
                 which(outside)[1L], "."),
      i = response_support_hint(tolower(fam_name))
    )))
  }

  if (isTRUE(spec$integer) && any(present & y != floor(y))) {
    frac <- present & y != floor(y)
    stop(insight::format_error(c(
      paste0("'", y_name, "' has non-integer values but family '",
             fam_name, "' counts events."),
      x = paste0(sum(frac), " of ", sum(present),
                 " values are not integers, the first at row ",
                 which(frac)[1L], "."),
      i = paste0("Round the column if the fractions are a storage ",
                 "artefact, or model it with a continuous family.")
    )))
  }

  invisible(TRUE)
}


# The one line of advice that is specific to a family, rather than
# a restatement of the bound the caller has just been shown.
#'@noRd
response_support_hint <- function(fam) {
  if (fam == "bernoulli") {
    return(paste0("Bernoulli takes 0/1 values, or a factor with two ",
                  "levels."))
  }
  if (fam %in% c("beta", "betar")) {
    return(paste0("Beta excludes both endpoints; use ",
                  "zero_inflated_beta() for exact zeros or ",
                  "zero_one_inflated_beta() for both."))
  }
  if (fam == "zero_inflated_beta") {
    return(paste0("This family admits exact zeros but not ones; ",
                  "use zero_one_inflated_beta() for both."))
  }
  if (fam == "tweedie") {
    return("Tweedie models a zero-inflated continuous response.")
  }
  if (fam == "von_mises") {
    return("Angles are measured in radians on [-pi, pi].")
  }
  if (fam %in% c("gamma", "lognormal", "weibull", "exponential",
                 "frechet", "inverse.gaussian",
                 "shifted_lognormal")) {
    return(paste0("This family excludes zero; use its hurdle ",
                  "counterpart if exact zeros are real observations."))
  }
  if (isTRUE(mvgam_response_support[[fam]]$integer)) {
    return(paste0("Counts cannot be negative; check the column for ",
                  "missing-data sentinels such as -1 or -999."))
  }
  "Check the response column, or choose a family whose support covers it."
}


# Internal: run `validate_response_for_family()` over each
# response `mvgam()` has already resolved, so a fit rejects a response
# its family cannot take with a message naming the column and the
# values observed, rather than letting brms report the constraint
# from a function the user did not call. Multi-response families
# take a matrix response and closure-unit families are checked by
# `validate_closure_unit_data()`, so both are left to their own
# validators.
#'@noRd
validate_response_shapes <- function(data, resp_vars, family) {
  if (is.null(family) || !length(resp_vars)) return(invisible(TRUE))
  # A list of frames is the multiple-imputation path; each frame
  # carries the same response and any one of them can be wrong.
  frames <- if (is.data.frame(data)) {
    list(data)
  } else if (is.list(data)) {
    Filter(is.data.frame, data)
  } else {
    list()
  }
  if (!length(frames)) return(invisible(TRUE))

  # A multivariate call may pair one family per response; anything
  # else applies the single family to each.
  fams <- if (is.list(family) && !inherits(family, "family") &&
                length(family) == length(resp_vars)) {
    family
  } else {
    rep(list(family), length(resp_vars))
  }

  for (i in seq_along(resp_vars)) {
    # `validate_family()` normalises a character, family or
    # customfamily into one object and refuses anything else, which
    # is a fault in the user's own argument and belongs here.
    fam <- validate_family(fams[[i]])
    if (is_multi_response_family(fam) || is_closure_unit_family(fam)) {
      next
    }
    for (frame in frames) {
      if (!resp_vars[i] %in% names(frame)) next
      validate_response_for_family(
        frame[[resp_vars[i]]], fam, y_name = resp_vars[i]
      )
    }
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
  checkmate::assert(inherits(formula, c("formula", "brmsformula",
                                          "mvbrmsformula", "brmsterms")))

  # mvbrmsformula is the multi-response wrapper; nl is a per-response
  # property recorded on each form in `$forms`. Return TRUE if any
  # form carries `nl = TRUE` so callers that branch on "is this an
  # nl model anywhere" route correctly.
  if (inherits(formula, "mvbrmsformula")) {
    return(any(vapply(formula$forms, is_nonlinear_formula,
                       logical(1L))))
  }

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
    # Extract all formula components for a full string representation
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

#' Per-session memo of exact-GP terms that have already warned.
#' Keys are the literal `gp_term` strings (e.g. `"gp(x)"`); the
#' value is `TRUE`. Reset between R sessions automatically; not
#' user-facing.
#' @noRd
.exact_gp_warned <- new.env(parent = emptyenv())

#' Fire the "exact GP, no newdata prediction" warning at most once
#' per term per session. Silent under `TESTTHAT=true` so the test
#' suite does not surface the noise.
#' @noRd
maybe_warn_exact_gp <- function(gp_term) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) return(invisible())
  # Honour `silent >= 2` (mirrors brms / mvgam silent semantics:
  # 0 = chatty, 1 = default, 2 = also suppress mvgam notices).
  # mvgam() / jsdgam() stash the entry-time `silent` on this
  # option so deep validators can read it without threading.
  if (isTRUE(getOption("mvgam.silent", 0L) >= 2L)) {
    return(invisible())
  }
  if (isTRUE(.exact_gp_warned[[gp_term]])) return(invisible())
  assign(gp_term, TRUE, envir = .exact_gp_warned)
  # NOTE: do not wrap the message body in `insight::format_warning()`
  # -- that function emits a warning as a side effect on top of
  # returning the formatted string, so combining it with message()
  # produces two user-facing notices for one logical event.
  # Build the multi-line body by hand and emit via message() so the
  # user sees exactly one prefixed notice per (term, session).
  example <- gsub("\\)$", ", k = 20)", gp_term)
  body <- paste(
    cli::format_inline(
      "Exact GP term in {.field {gp_term}} (no {.field k} given)."
    ),
    cli::format_inline(
      "i Fit + in-sample inference work; ",
      "prediction at newdata is not wired up for exact GPs."
    ),
    cli::format_inline(
      "i Pass {.field k} (e.g. {.code {example}}) to use the ",
      "Hilbert-space approximate form, which supports prediction ",
      "at newdata."
    ),
    sep = "\n"
  )
  message(body)
}

#' Warn (once) on exact GP terms
#'
#' Scans a formula for `gp()` terms that omit `k`. Exact GPs fit
#' through brms cleanly but mvgam's prediction surface cannot yet
#' reconstruct their basis at newdata, so the warn flags that
#' specific gap and lets users opt into the approximate form when
#' they need newdata prediction.
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
      # Exact GPs fit fine through brms (full covariance kernel),
      # but mvgam's prediction surface cannot yet reconstruct the
      # basis at newdata. Warn rather than hard-fail so users
      # can still fit / interpret in-sample; predict-on-newdata
      # currently relies on the approximate form.
      #
      # Dedupe per (process, term) so a single mvgam() call doesn't
      # fire the same warn from each validator entry station (the
      # obs validator, the trend validator, and setup_brms_lightweight
      # all hit this code path during one fit). rlang's `.frequency`
      # was not honouring the dedupe across re-entries; an explicit
      # package env makes the "once per term per session" semantics
      # bullet-proof.
      maybe_warn_exact_gp(gp_term)
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
#' Helper function to extract all formulas from brmsformula objects
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
      error_suggestion = "Use mvgam trend types instead: {.code ar(p = 1)} \u2192 {.code AR(p = 1)}"
    ),

    "addition_terms" = list(
      # brms `formula_ad` specials that modify the *observation model*
      # and therefore have no defined meaning on a latent State-Space
      # trend. `mi` is deliberately absent: missing-predictor
      # imputation is allowed on the latent scale; the obs-side
      # rejection of `mi()` as a predictor lives in
      # `validate_obs_formula_brms`. Detection walks
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
#' basic temporal dynamics without extra features (factors, correlations, groupings).
#'
#' @param trend_formula Formula for a single response trend
#' @param response_name Name of the response variable
#' @noRd
validate_multivariate_trend_constraints <- function(trend_formula, response_name) {
  checkmate::assert_formula(trend_formula)
  checkmate::assert_string(response_name)

  # Parse the trend formula to extract trend constructors
  parsed <- try(parse_trend_formula(trend_formula), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    return(invisible(NULL))  # Let parse_trend_formula handle the error
  }

  # Check each trend component for extra features
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
#' structure based on trend type. Every time series dimension the model
#' needs is read here.
#'
#' @param data Data frame containing time series
#' @param time_var Name of time variable (default: "time")
#' @param series_var Name of series variable (default: "series")
#' @param trend_type Type of trend model ("CAR" allows irregular intervals)
#' @param trend_specs Optional trend specification list for added metadata
#' @return List with time series dimensions and optional added metadata
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
  # A response-keyed frame states its axis rather than implying it
  # through the row values, and states it in formula order, so it is
  # taken as given below rather than sorted back to alphabetical.
  # One axis, held once and in one order: the responses as the formula
  # names them where the frame is response-keyed, otherwise the
  # frame's own series sorted. Keeping a sorted and an unsorted
  # spelling of the same axis is what let two readers disagree.
  response_axis <- mvgam_response_axis(data)
  series_axis <- response_axis %||% sort(unique(series_vals))

  # The group each axis entry belongs to, read from the rows that
  # define the axis. `group_inds_trend[s]` is subscripted by the
  # trend's own series index, so it has to follow `series_axis`;
  # deriving it further downstream reads a frame whose series
  # attribute was rebuilt without the trend's grouping, and the two
  # spellings then match to nothing.
  series_groups <- axis_group_values(data, trend_specs, series_vals,
                                     series_axis)
  original_time <- attr(data, "mvgam_original_time")

  min_time <- min(time_vals, na.rm = TRUE)
  max_time <- max(time_vals, na.rm = TRUE)

  sorted_unique_times <- sort(unique_times)

  dimensions <- list(
    n_time = length(unique_times),          # Number of unique time points
    n_series = length(series_axis),         # Number of series
    n_obs = nrow(data),                    # Total observations
    time_range = c(min_time, max_time),    # Time range
    time_var = time_var,                   # Variable names for downstream use
    series_var = series_var,
    unique_times = sorted_unique_times,    # Sorted unique time points
    unique_series = series_axis,           # The series axis, in order
    series_groups = series_groups          # Their groups, same order
  )

  # Both axes as one record, assembled where they are decided. It is
  # carried onto the fit so post-fit reads the axes the model was
  # given rather than rebuilding them from the frame and answering
  # with a permutation that stays in range.
  #
  # Time is held as its ordered original values. The integer index is
  # then `match()` and the gaps `CAR()` and the Gaussian processes
  # need are `diff()`, which is one representation where the package
  # kept three.
  time_values <- if (is.null(original_time)) {
    sorted_unique_times
  } else {
    sort(unique(original_time))
  }
  # What `forecast()` extends the grid by. A regular grid has one gap and
  # an irregular one has no single step, which is stated as `NA` rather
  # than guessed at from the first pair.
  time_gaps <- if (length(time_values) > 1L) diff(time_values) else numeric(0)
  time_step <- if (length(time_gaps) &&
                     isTRUE(all.equal(max(time_gaps), min(time_gaps)))) {
    time_gaps[1L]
  } else {
    NA_real_
  }

  groupings <- spec_groupings(trend_specs)
  n_lv <- spec_n_lv(trend_specs)

  # The last time each series was seen at, in axis order. `CAR()`
  # forecasts from it and it is a fact about the fit, so recording it
  # here spares the forecast surface a walk of the training frame,
  # which is where it was picking series out by a column the grouping
  # may have superseded.
  series_last_time <- axis_last_times(
    data, series_vals, series_axis, original_time %||% time_vals,
    response_axis, response_vars
  )

  dimensions$axes <- list(
    series = list(
      levels = as.character(series_axis),
      source = attr(data, "mvgam_series_source") %||% "explicit",
      n = length(series_axis),
      groups = series_groups,
      last_time = series_last_time
    ),
    # One representation of time, not three. The integer index a
    # trend steps along is `match()` into these values and the gaps
    # `CAR()` and the Gaussian processes measure are `diff()` of
    # them, so recording either alongside would be a second account
    # of the same fact, free to disagree with it.
    time = list(
      values = time_values,
      n = length(unique_times),
      step = time_step
    ),
    # The columns of `Z` and of `lv_trend`. A model with no factor
    # constructor loads each series on its own state, so the factor
    # axis is the series axis and `n_lv` says so rather than staying
    # silent.
    factor = list(
      n_lv = as.integer(n_lv %||% length(series_axis))
    ),
    # What the second dimension of `times_trend` indexes. The series,
    # except under `by = lv_axis()`, where Stan declares the map
    # `[N_time_trend, N_lv_trend]` and folds `mu_factor` into
    # `lv_trend`. Whether the trend takes that grain is not known
    # until the trend formula has been walked, so this is left unset
    # here and named by `extract_trend_data()`. Unset rather than
    # defaulted to the common answer: a reader that arrives early
    # then finds nothing instead of finding "series" and believing
    # it.
    grain = NULL,
    # The columns that identify a row, so a frame the model has never
    # seen can be placed on these axes without a second source.
    vars = list(
      time_var = time_var,
      series_var = series_var,
      gr_var = groupings$gr,
      subgr_var = groupings$subgr,
      response_vars = response_vars
    )
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
        dimensions = dimensions
      )

      # Store mapping with response variable name as key
      dimensions$mappings[[resp_var]] <- mapping
    }
  }

  # Gate on the trend's own rule rather than a hardcoded
  # `!= "CAR"` predicate; see the matching call upstream.
  if (any_trend_requires_regular_intervals(trend_specs)) {
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
#' @return List containing obs_trend_time and obs_trend_series arrays for Stan
#' @noRd
generate_obs_trend_mapping <- function(data, response_var,
                                       time_var = "time",
                                       series_var = "series",
                                       dimensions) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(response_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_list(dimensions)

  # The response column is checked by the caller and the time column
  # where the time index is built, so neither is asked for again.

  # Identify non-missing observations. mvgam keeps each response
  # on its own valid-row set so the shared latent state is informed
  # at every time point at which this response was observed. The
  # multi-response standata is expanded per-arm to match (see
  # `expand_per_response_standata` in R/stan_assembly.R).
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
  # The response axis is one entry per series, not per row, so it is
  # carried across the subset whole rather than indexed by it.
  scalar_attrs <- c("mvgam_time_source", "mvgam_series_source",
                    "mvgam_series_levels")
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
  # Every row of a response-keyed frame carries every response, so the
  # series is fixed by which response is being mapped rather than by
  # where the row sits. Reading the row values here instead is what
  # gave one response the first half of the timeline and another the
  # rest.
  response_axis <- mvgam_response_axis(obs_data)
  obs_trend_series <- if (is.null(response_axis)) {
    match(series_values, sorted_unique_series)
  } else {
    rep(response_series_index(response_axis, response_var),
        length(non_missing_idx))
  }

  # Neither index can be missing or out of range. `obs_data` is a
  # subset of the frame the axes were resolved on, so every value
  # it holds is in the list being matched against, and a `match()`
  # into a list of length n answers in 1..n or not at all. A frame
  # whose rows name no cell is refused where the two indices are
  # built. Checking again here would be noise standing where a real
  # check should be.

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
#' @noRd
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
    assert_column_shrinkage_compatible(spec, specs[[i]]$trend)
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
# Internal: tell the user once when a `series` column they supplied is
# superseded by the one `gr` and `subgr` imply.
#
# `gr` and `subgr` together identify a series, so mvgam builds the
# column itself and ignores whatever `series` the data carried. Staying
# silent leaves a user whose own column disagreed believing the model
# was grouped their way, and every post-fit label then reads back in
# mvgam's spelling rather than theirs.
#'@noRd
warn_series_superseded <- function(data, series_var, series_values,
                                   gr_var, subgr_var) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(invisible(NULL))
  }
  # `silent >= 2` suppresses mvgam's own notices. mvgam() and jsdgam()
  # stash the entry-time value on this option so validators this deep
  # can read it without it being threaded through every call.
  if (isTRUE(getOption("mvgam.silent", 0L) >= 2L)) {
    return(invisible(NULL))
  }
  if (is.null(series_var) || !series_var %in% names(data)) {
    return(invisible(NULL))
  }
  supplied <- as.character(data[[series_var]])
  derived <- as.character(series_values)
  if (identical(supplied, derived)) {
    return(invisible(NULL))
  }
  rlang::warn(
    insight::format_message(c(
      paste0(
        "The '", series_var, "' column was replaced by the series that '",
        gr_var, "' and '", subgr_var, "' define."
      ),
      x = paste0(
        "Supplied: '", supplied[1L], "'. Used: '", derived[1L], "'."
      ),
      i = paste0(
        "Hierarchical trends name a series by its grouping variables, ",
        "so supplying '", series_var, "' is not needed."
      ),
      i = paste0(
        "Every post-fit summary, plot and forecast labels this series ",
        "'", derived[1L], "'."
      )
    )),
    .frequency = "once",
    .frequency_id = "mvgam_series_superseded"
  )
  invisible(NULL)
}
#' A grouping variable's name, or `NA` where the trend names none
#'
#' The metadata fields spell an absent grouping `NA_character_`
#' rather than `NULL`, and the same ternary decided that four
#' times over. `named_var()` owns whether a name is a name; this
#' owns what to record when it is not.
#'
#' @param var A grouping variable name, or a sentinel for none.
#' @return The name, or `NA_character_`.
#' @noRd
named_var_or_na <- function(var) {
  if (named_var(var)) as.character(var) else NA_character_
}




# Internal: TRUE when a metadata variable name points at a usable
# column. Trend metadata stores an absent variable as NULL, NA or the
# literal string "NA" depending on how it was recorded.
#'@noRd
named_var <- function(var) {
  !is.null(var) && length(var) == 1L && !is.na(var) &&
    nzchar(var) && !identical(var, "NA")
}


# Internal: TRUE when a metadata variable names a column the data
# actually carries.
#'@noRd
usable_var <- function(var, data) {
  named_var(var) && var %in% names(data)
}


# Internal: require the grouping columns a hierarchical trend needs.
#
# Reason: a bare `checkmate::assert_names()` reports the missing names
# without saying why they are wanted, which reads as an internal
# assertion to someone who simply passed a newdata built from the
# covariates they model over.
#'@noRd
assert_grouping_columns <- function(data, gr_var, subgr_var) {
  wanted <- c(gr_var, subgr_var)
  missing <- wanted[!vapply(wanted, usable_var, logical(1L), data = data)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "Columns needed to identify each series are missing from 'newdata'.",
    x = cli::format_inline("Missing: {.field {missing}}."),
    i = cli::format_inline(paste0(
      "This model groups its trend by {.field {gr_var}} and ",
      "{.field {subgr_var}}, which together name a series, so ",
      "'newdata' must carry both."
    ))
  )), call. = FALSE)
}


# Internal: the series identifier a hierarchical trend uses.
#
# `gr` and `subgr` together name a series, so mvgam derives the column
# rather than reading one the user supplied. Every site that needs the
# value builds it here, so the fitting path, the prediction path and
# the level validator cannot drift apart on separator or ordering.
#'@noRd
hierarchical_series_values <- function(data, gr_var, subgr_var) {
  interaction(
    data[[gr_var]], data[[subgr_var]],
    drop = TRUE, sep = "_", lex.order = TRUE
  )
}


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

  # Validate series levels.
  #
  # A hierarchical trend derives its series identifier from `gr` and
  # `subgr`, so any `series` column sitting in the data is not the one
  # the model was fitted on and must not be compared against the stored
  # levels. Rebuild the derived value instead: that still catches a
  # `gr` / `subgr` combination the training data never contained, even
  # though each level on its own is known.
  # The axis, read through the one accessor that understands both
  # the record and the spelling a fit saved before it used. Reading
  # `levels$series` and `series_source` here was a second account of
  # the axis, free to disagree with the record on the same object.
  axes <- axes_from_metadata(metadata)
  fitted_levels <- axes$series$levels
  if (!is.null(fitted_levels)) {
    gr_var <- metadata$variables$gr_var
    subgr_var <- metadata$variables$subgr_var
    # A pair of grouping variables says the series was derived, and
    # so does the recorded source; either is enough, because a fit
    # missing one still carries the other.
    derived_hier <- identical(axes$series$source, "hierarchical") ||
      (named_var(gr_var) && named_var(subgr_var))
    newdata_levels <- NULL
    if (derived_hier) {
      assert_grouping_columns(data, gr_var, subgr_var)
      newdata_levels <- levels(droplevels(
        hierarchical_series_values(data, gr_var, subgr_var)
      ))
    } else {
      series_var <- metadata$variables$series_var
      if (!is.null(series_var) && series_var %in% names(data)) {
        # The series the frame holds rows for, not the levels its
        # factor happens to declare. Subsetting a data frame keeps
        # every level, so reading the declaration refuses a frame
        # that names no unknown series and merely carries a dead
        # level. The grouping branch above already drops them.
        newdata_levels <- observed_series_levels(data[[series_var]])
      }
    }
    if (!is.null(newdata_levels)) {
      invalid <- setdiff(newdata_levels, fitted_levels)
      if (length(invalid) > 0) {
        stop(insight::format_error(c(
          "Series levels in newdata not found in training data.",
          x = cli::format_inline("Invalid: {.val {invalid}}."),
          i = cli::format_inline(
            "Training data has levels: {.val {fitted_levels}}."
          )
        )), call. = FALSE)
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

  # Primary validation using rstan::stanc() (most thorough and up-to-date)
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

  n_lv <- trend_specs$n_lv
  is_factor_model <- is_factor_model_spec(n_lv, n_series)

  # Check if hierarchical grouping is requested
  use_grouping <- named_var(trend_specs$gr)

  # Factor models are incompatible with hierarchical grouping
  if (use_grouping && is_factor_model) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Hierarchical {trend_name} models cannot use factor models."
      ),
      i = cli::format_inline(
        "Drop {.field n_lv} (one trend per series) or remove {.field gr}/{.field subgr} to keep the factor model."
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

  # Always create implicit time mapping for consistency.
  #
  # The index has to run in time order, not in the order the rows
  # happen to arrive in. It is what the trend steps along, so numbering
  # the times by first appearance makes an AR recursion advance
  # through whatever sequence the frame was assembled in: a frame
  # grouped by series rather than by date gives its first series'
  # earliest time index 1 and leaves the real first time somewhere in
  # the middle. The mapping stays a bijection either way, so nothing
  # downstream can notice. Sorted input, which is the usual shape and
  # the one every fixture carries, is unaffected.
  checkmate::assert_names(names(data), must.include = time_var)
  unique_times <- sort(unique(data[[time_var]]))
  time_mapping <- setNames(seq_along(unique_times), unique_times)
  attr(data, "mvgam_time") <- time_mapping[as.character(data[[time_var]])]
  attr(data, "mvgam_time_source") <- "implicit"

  # Every row has to name a cell of the trend matrix. A missing time
  # or series maps to an NA index, which stays an NA index: the
  # trend is read at `trend[NA, s]`, the observation takes a missing
  # linear predictor, and nothing between here and the answer says
  # so. Refusing here is the one layer that owns the question,
  # because this is where the two indices are built.
  missing_time <- which(is.na(attr(data, "mvgam_time")))
  if (length(missing_time) > 0L) {
    stop(insight::format_error(c(
      paste0("Time variable '", time_var, "' has missing values."),
      x = paste0(
        length(missing_time), " row(s) carry no time, first at row ",
        missing_time[1L], "."
      ),
      i = paste0(
        "Every row must name an occasion for the latent trend to be ",
        "read at. Drop these rows or supply their times."
      )
    )), call. = FALSE)
  }
  attr(data, "mvgam_original_time") <- data[[time_var]]  # Store original for distance calculations

  # The series axis of a frame whose responses are its series
  #
  # A frame written with `brms::mvbf()` carries one row per time and
  # one column per response, so the series an observation sits on is a
  # property of the (row, response) pair and not of the row. A vector
  # with one entry per row cannot say that, and cutting the rows into
  # a block per response says something false: it reads as a stacked
  # frame, gives the first stretch of the timeline to one response and
  # the rest to another, and raises nothing.
  #
  # So the axis is carried as the level set instead. The levels are
  # the responses in the order the formula names them, which makes
  # series `k` the `k`th response and the `k`th row of the loadings.
  # The per-row values are one constant level, which is the grain the
  # trend design needs: a covariate column of a wide frame holds one
  # value per time, so the design has one row per time and every
  # series reads the same one.
  create_multivariate_series <- function(response_vars, n_obs) {
    checkmate::assert_character(response_vars, min.len = 1,
                                any.missing = FALSE, min.chars = 1)
    checkmate::assert_integerish(n_obs, len = 1, lower = 1)

    list(
      series_values = factor(
        rep(response_vars[1L], n_obs), levels = response_vars
      ),
      series_levels = response_vars,
      series_source = "multivariate"
    )
  }

  # Four series creation strategies (including prediction context)
  series_values <- NULL
  series_source <- NULL
  series_levels <- NULL

  # Strategy 1: Prediction context - use stored metadata to recreate series
  if (!is.null(metadata)) {
    stored_source <- metadata$series_source %||% "explicit"

    if (stored_source == "hierarchical" && !is.null(metadata$variables)) {
      gr_var <- metadata$variables$gr_var
      subgr_var <- metadata$variables$subgr_var

      if (named_var(gr_var) && named_var(subgr_var)) {
        assert_grouping_columns(data, gr_var, subgr_var)
        series_values <- hierarchical_series_values(
          data, gr_var, subgr_var
        )
        series_source <- "hierarchical"
      }
    } else if (stored_source == "multivariate" && !is.null(metadata$response_vars)) {
      # Use helper function for trend-aware series creation
      result <- create_multivariate_series(
        metadata$response_vars, nrow(data)
      )
      series_values <- result$series_values
      series_levels <- result$series_levels
      series_source <- result$series_source
    }
    # If prediction context but explicit series, fall through to Strategy 3
  }

  # Strategy 2: Hierarchical series (gr + subgr present in fitting context)
  #
  # The specification arrives flat from some callers and nested under
  # `$trend_model` from others, so the grouping is read through
  # `spec_groupings()` rather than from one spelling. Reading only the
  # nested one left two of the three calls in a single `standata()`
  # build believing the model was ungrouped, and they took the axis
  # from the superseded series column while the third took it from the
  # grouping.
  if (is.null(series_values)) {
    groupings <- spec_groupings(parsed_trend)
    gr_var <- groupings$gr
    subgr_var <- groupings$subgr

    if (!is.null(gr_var) && !is.null(subgr_var) && subgr_var != series_var) {
      # When subgr is a separate variable, build series from
      # interaction(gr, subgr). When subgr defaults to the existing
      # series column, fall through to Strategy 3 so the original
      # series values are preserved (the codegen path reads the series
      # column directly).
      assert_grouping_columns(data, gr_var, subgr_var)
      series_values <- hierarchical_series_values(
        data, gr_var, subgr_var
      )
      series_source <- "hierarchical"
      warn_series_superseded(data, series_var, series_values,
                             gr_var, subgr_var)
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
      result <- create_multivariate_series(response_vars, nrow(data))
      series_values <- result$series_values
      series_levels <- result$series_levels
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

  # Store series as attribute. `mvgam_series_levels` is set only when
  # the series axis is the response axis, where the per-row values
  # cannot carry it; everywhere else the values are the axis and the
  # attribute stays absent.
  attr(data, "mvgam_series") <- series_values
  attr(data, "mvgam_series_source") <- series_source
  attr(data, "mvgam_series_levels") <- series_levels

  # The other half of the same requirement, checked where the series
  # is settled rather than where the time is.
  missing_series <- which(is.na(series_values))
  if (length(missing_series) > 0L) {
    stop(insight::format_error(c(
      "Series identity has missing values.",
      x = paste0(
        length(missing_series), " row(s) name no series, first at row ",
        missing_series[1L], "."
      ),
      i = paste0(
        "Every row must name a series so its latent state can be ",
        "found. Check '", series_var, "' and any grouping variables ",
        "for missing values."
      )
    )), call. = FALSE)
  }

  return(data)
}
#' The prepared time or series index a frame carries
#'
#' A frame read for a model carries its trend indices as attributes,
#' and both are fetched the same way. Fetching them through two
#' functions with one body each meant the two could answer
#' differently the day either changed, on a question that has one
#' answer.
#'
#' @param data Data frame carrying the prepared attributes.
#' @param what Which index to fetch, `"time"` or `"series"`.
#' @return The stored vector.
#' @noRd
mvgam_prepared_index <- function(data, what) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_choice(what, c("time", "series"))
  values <- attr(data, paste0("mvgam_", what))
  if (is.null(values)) {
    stop(insight::format_error(c(
      paste0("This data frame carries no ", what, " index."),
      i = paste0(
        "The ", what, " index is built when a frame is read for a ",
        "model, so this frame has not been through that reading."
      )
    )), call. = FALSE)
  }
  values
}


#' Get time variable for grouping operations
#'
#' @param data Data frame with mvgam time attributes
#' @return Numeric vector of sequential time indices (1, 2, 3, ...)
#' @noRd
get_time_for_grouping <- function(data) {
  mvgam_prepared_index(data, "time")
}

#' The group each series belongs to, one entry per series
#'
#' Three callers want this one answer: the balance check counts it,
#' the subgroup dimension takes the largest count, and
#' `group_inds_trend` is the vector itself. Only the last is sensitive
#' to order, and that is the one that matters: Stan reads
#' `group_inds_trend[s]` against `s`, the trend matrix's own series
#' index, so it must be given the series order the trend was built
#' with. Reading the data in row order answers a different question
#' whenever the rows are not sorted by series, and the two answers are
#' a permutation of one another, so nothing raises and the model
#' correlates the wrong series together.
#'
#' The series identity is read the way the fit reads it, through the
#' `mvgam_series` attribute when the frame carries one, so a derived
#' series is not silently compared against the column it superseded.
#'
#' @param data Data frame holding one or more rows per series
#' @param series_var Name of the series column, used when the frame
#'   carries no prepared series attribute
#' @param gr_var Name of the grouping column
#' @param order_by Series identifiers in the order the answer must
#'   follow, or NULL to keep the order the data presents
#' @param labels Series identity per row, for a caller that has
#'   derived it and reaches this before any axis exists. `NULL`
#'   reads the prepared attribute, then the series column.
#' @return Vector of group values, one per series
#'
#' @noRd
series_group_values <- function(data, series_var, gr_var,
                                order_by = NULL, labels = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(gr_var)
  labels <- labels %||% attr(data, "mvgam_series") %||% data[[series_var]]
  if (is.null(labels)) {
    stop(insight::format_error(
      paste0("Series variable '", series_var, "' not found in data.")
    ))
  }
  labels <- as.character(labels)
  first <- !duplicated(labels)
  groups <- data[[gr_var]][first]
  if (is.null(order_by)) {
    return(groups)
  }
  groups[match(as.character(order_by), labels[first])]
}
#' Get series variable for grouping operations
#'
#' @param data Data frame with mvgam series attributes
#' @return Factor or character vector of series identifiers
#' @noRd
get_series_for_grouping <- function(data) {
  mvgam_prepared_index(data, "series")
}

#' The series a frame observes, in the order its axis runs
#'
#' A factor keeps levels the data never uses, and those levels have
#' no latent state, so they are not series. Dropping them leaves the
#' same answer a character column gives, in the order the levels
#' declare rather than alphabetically.
#'
#' A frame whose responses are its series answers elsewhere, through
#' `mvgam_response_axis()`: its per-row values are one constant and
#' the axis is the level set, so reading the values would find a
#' single series where the trend has one per response.
#'
#' @param series_vals Series values, factor or otherwise
#' @return Character vector of observed series labels, in axis order
#' @noRd
observed_series_levels <- function(series_vals) {
  if (is.factor(series_vals)) {
    levs <- levels(series_vals)
    return(levs[levs %in% as.character(series_vals)])
  }
  sort(unique(as.character(series_vals)))
}

#' The series axis of a frame whose responses are its series
#'
#' Returns the response names, in the order the formula gives them,
#' for a frame written one row per time and one column per response.
#' Every other frame answers `NULL`, because there the per-row series
#' values are the axis and reading them is right.
#'
#' Ask this before reading `get_series_for_grouping()` for anything
#' other than a grouping grain. On a response-keyed frame those values
#' are a single constant, and the series an observation sits on
#' depends on which response is being asked about, which the values
#' cannot express.
#'
#' @param data Data frame carrying mvgam attributes
#' @return Character vector of response names, or `NULL`
#' @noRd
mvgam_response_axis <- function(data) {
  checkmate::assert_data_frame(data)
  attr(data, "mvgam_series_levels")
}

#' Which series a named response sits on
#'
#' @param axis Response axis from `mvgam_response_axis()`
#' @param response_var Name of the response being asked about
#' @return Integer index into the trend matrix's series dimension
#' @noRd
response_series_index <- function(axis, response_var) {
  checkmate::assert_character(axis, min.len = 1, any.missing = FALSE,
                              unique = TRUE)
  checkmate::assert_string(response_var)
  idx <- match(response_var, axis)
  if (is.na(idx)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Response {.field {response_var}} is not on the series axis."
      ),
      i = cli::format_inline("Axis holds {.val {axis}}.")
    )), call. = FALSE)
  }
  idx
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
  attr(data, "mvgam_series_levels") <- NULL
  return(data)
}

#' Extract and Validate Trend Components
#'
#' @description
#' Resolves the axes once, extracts the trend data on them and
#' injects the dimensions into the specification, so a single pass
#' answers what three separate ones used to answer differently.
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
  # mv_spec$base_formula, rewrite each `by` argument so the single brms
  # compile sees a regular factor by-variable. The rewrite target and
  # downstream codepath depend on whether the trend spec carries n_lv:
  #
  #   factor model (n_lv set): rewrite to `by = .trend`, switch grain
  #     to (time, .trend), emit factor-model Stan with loadings Z.
  #   non-factor (n_lv not set): rewrite to `by = series`, keep the
  #     standard (time, series) grain. Each series gets its own smooth
  #     basis on the trend side, exactly as `by = series` would on the
  #     obs side, but with the contribution living in the latent state.
  #
  # has_by_lv + n_lv_for_grain are threaded through to extract_trend_data
  # and the downstream stanvar emission only for the factor-model path.
  has_by_lv <- FALSE
  n_lv_for_grain <- NULL
  # Reason: `has_by_lv` selects the (time, .trend)-grain codepath and is
  # FALSE on the non-factor rewrite. `had_by_lv` records that the user
  # wrote `by = lv_axis()` regardless of which path took it, so display
  # code (conditional_effects list names, summary tables) can strip the
  # internal `series` rewrite token and present per-latent-axis
  # semantics back to the user.
  had_by_lv <- FALSE
  if (!is.null(mv_spec$base_formula) &&
      inherits(mv_spec$base_formula, "formula")) {
    factor_active <- !is.null(parsed_trend$n_lv) &&
      isTRUE(as.integer(parsed_trend$n_lv) >= 1L)
    by_lv_res <- detect_and_rewrite_by_lv(
      mv_spec$base_formula,
      factor_active = factor_active
    )
    if (by_lv_res$has_by_lv) {
      had_by_lv <- TRUE
      mv_spec$base_formula <- by_lv_res$formula
      if (by_lv_res$deprecated_trend_seen) {
        warn_legacy_trend_by()
      }

      if (factor_active) {
        # Factor-model path: switch grain, emit factor-model codegen.
        has_by_lv <- TRUE
        n_lv_for_grain <- parsed_trend$n_lv

        # When Z is user-pinned (fully or partially), the rotation
        # concern that by = lv_axis() was designed to address is moot;
        # the env constraint and the data-side constraint both pin
        # factor identification. Emit a one-time warning so users
        # know the rotation auto-skip is a no-op for their fit.
        has_fixed_Z <- !is.null(parsed_trend$fixed_Z) ||
          !is.null(parsed_trend$Z)
        if (has_fixed_Z) {
          mvgam_warn_once_user(
            message = paste0(
              "'by = lv_axis()' was supplied with a user-pinned ",
              "'trend_map' (numeric entries on Z). The per-factor ",
              "smooths still fit, but factor identification is ",
              "already pinned by the user-supplied loadings; the ",
              "rotation auto-skip behaviour does not apply."
            ),
            class = "mvgam_by_lv_with_pinned_Z"
          )
        }
      }
      # Non-factor path: has_by_lv stays FALSE; the formula was already
      # rewritten to use `by = series`. The standard (time, series)
      # codepath handles everything downstream, including conditional
      # effects via brms native predict.
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

  # Persist the by_lv grain markers on dimensions so downstream stanvar
  # emission (extract_and_rename_trend_parameters → times_trend) sees
  # the matching axis. The standard (time, series) path stays unchanged
  # when has_by_lv is FALSE. `had_by_lv` is a display-only marker (no
  # effect on codegen) that records whether the AST detector found a
  # `by = lv_axis()` term, used by conditional_effects.mvgam to hide
  # the internal `series` / `.trend` rewrite tokens from the
  # user-visible plot list names.
  dimensions$has_by_lv <- has_by_lv
  dimensions$had_by_lv <- had_by_lv
  dimensions$n_lv_for_grain <- n_lv_for_grain

  # has_by_lv requires 1 <= n_lv <= n_series.
  if (has_by_lv) {
    n_series_for_check <- dimensions$n_series %||%
      length(dimensions$unique_series %||% character(0))
    if (n_series_for_check < 1L ||
        n_lv_for_grain > n_series_for_check) {
      stop(insight::format_error(c(
        paste0(
          "'by = lv_axis()' requires a factor model ",
          "(1 <= n_lv <= n_series)."
        ),
        x = paste0(
          "Configured n_lv = ", n_lv_for_grain,
          " but the data has n_series = ", n_series_for_check, "."
        ),
        i = paste0(
          "Lower 'n_lv' to at most n_series, or remove ",
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
      has_by_lv = has_by_lv, had_by_lv = had_by_lv,
      n_lv_for_grain = n_lv_for_grain
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

# Covariates to collapse, excluding the grouping columns. `time` and
# `series` are added by the caller and grouped on, and dplyr omits
# grouping columns from `across()`, so naming them in the selection
# errors with "Element `time` doesn't exist". A `trend_formula` that
# refers to the time or series variable, for example `~ s(time)` or a
# plain `~ time`, puts them in `trend_variables`. Grouping already
# carries `time` through to the result, so drop both here.
#'@noRd
trend_covariate_names <- function(trend_variables) {
  checkmate::assert_character(trend_variables, any.missing = FALSE)
  setdiff(trend_variables, c("time", "series"))
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

  if (length(trend_covariate_names(trend_variables)) == 0L) {
    return(data.frame(time = sort(unique(time_vals))))
  }

  data %>%
    dplyr::mutate(time = time_vals, series = series_vals) %>%
    dplyr::group_by(.data$time, .data$series) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(trend_covariate_names(trend_variables)),
        dplyr::first
      ),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(trend_covariate_names(trend_variables)),
        dplyr::first
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$time)
}

extract_trend_data <- function(data, trend_formula = NULL, time_var = "time", series_var = "series",
                              mvgam_object = NULL, newdata = NULL, response_vars = NULL,
                              .return_metadata = FALSE, .precomputed_dimensions = NULL, trend_specs = NULL,
                              has_by_lv = FALSE, had_by_lv = FALSE,
                              n_lv_for_grain = NULL) {

  # Input validation for new parameters - non-negotiable per CLAUDE.md
  if (!is.null(response_vars)) {
    checkmate::assert_character(response_vars, min.len = 1)
  }
  checkmate::assert_logical(.return_metadata, len = 1)
  if (!is.null(.precomputed_dimensions)) {
    checkmate::assert_list(.precomputed_dimensions, names = "named")
  }
  checkmate::assert_flag(has_by_lv)
  checkmate::assert_flag(had_by_lv)
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
    # had_by_lv is display-only but mirrored through prediction so
    # downstream calls see the same metadata shape regardless of
    # context. No effect on codegen or newdata reshaping.
    if (isTRUE(mvgam_object$trend_metadata$had_by_lv %||% FALSE)) {
      had_by_lv <- TRUE
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

    # The columns a frame needs are the time, which is read here,
    # and the trend's own covariates. Which columns name the series
    # is not settled at this layer: a hierarchical fit reads `gr`
    # and `subgr`, a response-keyed one reads nothing, and only the
    # explicit case wants the column `series_var` names. Demanding
    # that column here refused a hierarchical fit its own prediction
    # frame, three lines before `ensure_mvgam_variables()` would
    # have derived the axis from the grouping it does carry. That
    # function owns the refusal, and states which of the three
    # things is missing.
    required_vars <- unique(c(time_var, trend_variables))
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

    # Pre-computed dimensions are the fast path. When a caller
    # comes through that has not threaded them in (e.g. the
    # `is_trend_setup = TRUE` branch in `setup_brms_lightweight()`,
    # which receives already-reduced trend data and just needs
    # metadata), synthesise a minimal `.precomputed_dimensions`
    # shell carrying just the predictor names. Only
    # `$metadata$covariates` is read downstream in this code path,
    # so a heavier brmsterms walk is unnecessary here. Unblocks
    # the brms-special surface (trials / se / cens / me / mm / cs
    # / car) that all reach this point via `stancode()`.
    if (is.null(.precomputed_dimensions)) {
      .precomputed_dimensions <- list(
        metadata = list(
          covariates = setdiff(
            extract_predictor_vars(trend_formula),
            response_vars %||% character(0L)
          )
        )
      )
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
          has_gr <- named_var(parsed_trend$trend_model$gr)
          has_subgr <- named_var(parsed_trend$trend_model$subgr)

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
        dplyr::across(
        dplyr::all_of(trend_covariate_names(trend_variables)),
        dplyr::first
      ),
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
        gr_var = named_var_or_na(parsed_trend$trend_model$gr),
        subgr_var = named_var_or_na(parsed_trend$trend_model$subgr)
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
      # `had_by_lv` is the display-only twin (see comment near
      # `dimensions$had_by_lv <- had_by_lv` above) consumed by
      # `conditional_effects.mvgam` via `mvgam_had_by_lv()`.
      has_by_lv = has_by_lv,
      had_by_lv = had_by_lv,
      n_lv_for_grain = n_lv_for_grain,
      # Both axes as they were resolved when the model was built, so
      # post-fit reads the axes Stan was given instead of rebuilding
      # them from the frame. Read, never rebuilt here: a second
      # construction is the thing this record exists to end.
      # The record, completed with the one field that is not knowable
      # where the rest of it is built. Read, never rebuilt: a second
      # construction is the thing this record exists to end.
      axes = complete_axes_grain(.precomputed_dimensions$axes,
                                 has_by_lv, had_by_lv),
      # Store factor levels for prediction validation
      levels = list(
        # The series the trend actually has, in axis order. Taking a
        # factor's declared levels instead counts any the data never
        # observes, so the stored levels outnumber the trend columns
        # and whatever labels those columns from this list runs off
        # the end of it. A character column was already answering the
        # observed question, so the two spellings of this one field
        # disagreed with each other as well.
        series = mvgam_response_axis(data) %||%
          observed_series_levels(series_vals),
        gr = extract_factor_levels(
          data, named_var_or_na(parsed_trend$trend_model$gr)
        ),
        subgr = extract_factor_levels(
          data, named_var_or_na(parsed_trend$trend_model$subgr)
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
#   N_features_trend         int c (0 when features is NULL)
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
  series_levels <- argument_series_levels(data, "loadings_prior")
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
    N_features_trend = if (is.null(features_mat)) 0L else ncol(features_mat),
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
    "mgp_a1", "mgp_a2", "n_series", "N_features_trend", "n_distances"
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
      spec$N_features_trend != ncol(spec$features_mat)) {
    stop(insight::format_error(c(
      "Loadings-prior spec has inconsistent feature dimensions.",
      x = paste0(
        "spec$N_features_trend = ", spec$N_features_trend,
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
  # A spec with neither a kernel nor column shrinkage carries no
  # structure to emit, and the emitter's kernel branch would then
  # write `Z ~ multi_normal_cholesky(..., L_Phi_loadings)` against
  # an `L_Phi_loadings` nothing declares, which fails at `stanc`
  # rather than here. `normalise_loadings_prior()` already refuses
  # to build one; this is what stops a second builder from doing so.
  traits <- loadings_spec_traits(spec)
  if (!traits$kernel && !traits$mgp) {
    stop(insight::format_error(c(
      "Loadings-prior spec carries no structure to emit.",
      x = paste0(
        "It names no features, no distance matrices and ",
        "'column_shrinkage = \"", spec$column_shrinkage %||% "iid",
        "\"'."
      ),
      i = paste0(
        "An empty spec collapses to the default iid prior; drop ",
        "'loadings_prior' instead of passing one."
      )
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


# Trends that carry the multiplicative gamma process column scale.
# The scale reaches the model through `sigma_trend`, which only the
# shared-innovation path builds, so a trend that opts out of shared
# innovations has nowhere to put it. `PW()` and `CAR()` refuse factor
# models outright and never reach this.
#'@noRd
mgp_capable_trends <- c("AR", "RW", "ZMVN")


#' Refuse MGP column shrinkage on a trend that cannot apply it.
#'
#' @param loadings_prior_spec Normalised loadings-prior spec, or NULL.
#' @param trend Character trend name from the trend spec.
#' @return Invisibly `NULL`; raises otherwise.
#' @noRd
assert_column_shrinkage_compatible <- function(loadings_prior_spec, trend) {
  if (is.null(loadings_prior_spec) || is.null(trend)) {
    return(invisible(NULL))
  }
  if (!loadings_spec_traits(loadings_prior_spec)$mgp) {
    return(invisible(NULL))
  }
  trend_nm <- toupper(as.character(trend)[1L])
  if (trend_nm %in% mgp_capable_trends) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    paste0(
      "Multiplicative gamma process shrinkage is not available for ",
      "'", trend_nm, "()' trends."
    ),
    x = paste0(
      "The column scale is applied through the shared innovation ",
      "scale, which '", trend_nm, "()' does not use."
    ),
    i = paste0(
      "Use column_shrinkage = 'iid', or a trend that carries it: ",
      paste0(mgp_capable_trends, "()", collapse = ", "), "."
    )
  )))
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


#' Require a `trials()` addition term for `com_binomial()`
#'
#' `com_binomial()` reads its per-row binomial denominator from the
#' `trials` array, which brms emits only when the response carries
#' a `trials()` addition term. Routing the denominator through brms
#' keeps it aligned with the response when rows are dropped, but
#' omitting the term leaves `trials` undefined and stanc fails on a
#' name the user never wrote. Name the omission here instead.
#'
#' @param formula The observation formula, possibly a `brmsformula`.
#' @return Invisible NULL on success; an informative error if the
#'   `trials()` term is absent.
#' @noRd
assert_com_binomial_trials <- function(formula) {
  if (!is.null(find_aterm_call(formula, "trials"))) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    "'com_binomial()' needs its number of trials in a 'trials()' term.",
    x = "No 'trials()' term found in the observation formula.",
    i = paste0(
      "Supply the binomial denominator as an addition term, e.g. ",
      "'bf(y | trials(n) ~ x)' for a column or 'bf(y | trials(10) ~ x)' ",
      "for a constant."
    )
  )))
}


#' Locate an addition term in a model formula
#'
#' Addition terms live on the response side, so only the left-hand
#' side is searched: a covariate that happens to share the term's
#' name must not read as the term. Walking the parse tree matches
#' the call rather than a bare symbol.
#'
#' @param formula The observation formula, possibly a `brmsformula`
#'   or `mvgam_formula`.
#' @param aterm Name of the addition term to find, e.g. `"trials"`.
#' @return The matched call, or NULL when there is none.
#' @noRd
find_aterm_call <- function(formula, aterm) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvgam_formula"),
    .var.name = "formula"
  )
  checkmate::assert_string(aterm, min.chars = 1L)
  inner <- if (!is.null(formula$formula)) formula$formula else formula
  lhs <- if (length(inner) >= 3L) inner[[2L]] else inner
  found <- NULL
  walk <- function(expr) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }
    if (identical(as.character(expr[[1L]])[1L], aterm)) {
      found <<- expr
    }
    for (part in as.list(expr)[-1L]) {
      walk(part)
    }
    invisible(NULL)
  }
  walk(lhs)
  found
}


#' Resolve a model's per-row binomial denominator for a data frame
#'
#' A model has two row spaces: the likelihood covers the observed
#' responses only, while predictions cover every row of the target
#' data including those whose response was missing. The denominator
#' brms places in `standata` belongs to the first, so reusing it for
#' the second misaligns as soon as any response is `NA`. Evaluating
#' the `trials()` addition term against the data being predicted
#' keeps them aligned, and covers a constant denominator such as
#' `trials(30)`, which has no column to read.
#'
#' @param formula The model's observation formula.
#' @param data The data frame predictions are being made for.
#' @return A vector with one element per row of `data`, or NULL when
#'   the formula carries no `trials()` term or the term names a
#'   column absent from `data`.
#' @noRd
resolve_trials_denominator <- function(formula, data) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  if (!inherits(formula, c("formula", "brmsformula", "mvgam_formula"))) {
    return(NULL)
  }
  trials_call <- find_aterm_call(formula, "trials")
  if (is.null(trials_call) || length(trials_call) < 2L) {
    return(NULL)
  }
  denominator <- trials_call[[2L]]
  needed <- all.vars(denominator)
  if (length(needed) && !all(needed %in% names(data))) {
    return(NULL)
  }
  values <- eval(denominator, data)
  if (length(values) == 1L) {
    values <- rep(values, nrow(data))
  }
  values
}


#' Report each distinct warning raised under an expression once
#'
#' Several mvgam entry points reach their answer by running an inner
#' routine more than once: the code generator makes three passes
#' through brms, and the residual panel calls `pp_check()` once per
#' panel. Each pass re-raises whatever the last one did, so a single
#' user-facing call can repeat one warning several times over. This
#' keeps the first occurrence of each distinct message and drops the
#' repeats, so a second, different warning is never hidden behind the
#' first.
#'
#' @param expr Expression to evaluate
#' @return The value of `expr`
#'
#' @noRd
warn_once_per_call <- function(expr) {
  seen <- character()
  withCallingHandlers(expr, warning = function(w) {
    msg <- conditionMessage(w)
    if (msg %in% seen) {
      invokeRestart("muffleWarning")
    }
    seen <<- c(seen, msg)
  })
}


#' Are these forecast times the ones the trend can step to?
#'
#' A discrete-time trend advances one step per time point, so its
#' forecast horizon is the number of steps taken from the last
#' observed state. That is only the number of rows supplied when
#' the forecast times continue the training series without a gap
#' and at its own spacing. Handed `t = 41:42` after training ends
#' at `t = 30`, such a trend takes two steps rather than twelve
#' and reports a two-step-ahead spread for a twelve-step-ahead
#' question, understating the uncertainty with nothing said.
#'
#' `CAR()` and `ZMVN()` are exempt and answer for themselves:
#' `CAR()` carries the elapsed gap into its kernel, and `ZMVN()`
#' has no temporal structure to step through. The registry says
#' which is which, through the same `requires_regular_intervals`
#' rule that gates the fit-time and `mvgam_data()` checks.
#'
#' @param fc_times Named list of forecast times per series.
#' @param training The training arms, carrying `times` per series.
#' @param trend_spec The fit's trend specification.
#' @param step The spacing the fit's own time grid runs on,
#'   from `axes$time$step`. `NA` where it is irregular.
#' @return Invisibly `TRUE`; raises otherwise.
#' @noRd
assert_forecast_times_steppable <- function(fc_times, training,
                                            trend_spec, step) {
  checkmate::assert_list(fc_times, null.ok = TRUE)
  checkmate::assert_list(training)
  if (!any_trend_requires_regular_intervals(trend_spec)) {
    return(invisible(TRUE))
  }
  # The latent state lives on one time grid shared by every series,
  # so that grid is what the trend steps along. Reading each
  # series' own observed times instead refuses a legitimate
  # forecast whenever a series ends on unobserved responses: brms
  # drops those rows, the series looks short, and the check sees a
  # gap that the latent state does not have. mvgam asks users to
  # pad exactly that way, so it is a common shape rather than an
  # odd one.
  #
  # The horizon is resolved from the last *observed* occasion, which
  # is a different question and rightly answered differently: a
  # padded series must not be forecast from an occasion it was never
  # seen at. So on a padded frame the two disagree about the
  # occasions between the last response and the end of the grid, and
  # the message below has to name the grid rather than call its last
  # position an observation.
  past <- sort(unique(as.numeric(unlist(
    training$times %||% list(), use.names = FALSE
  ))))
  if (length(past) < 2L) return(invisible(TRUE))
  # The spacing the fit recorded, not a second reading of it.
  # Deriving the step here as well gave the same fact two
  # answers, free to disagree the day either changed. `NA` is
  # an irregular grid, which has no single step to continue,
  # and `NULL` is a fit that recorded none: neither can say
  # what the next occasion should be, so neither refuses one.
  if (is.null(step) || is.na(step)) return(invisible(TRUE))
  # Left as it was recorded. Coercing to integer turned a grid
  # spaced by half a unit into a step of zero, which no forecast
  # can continue.
  step <- as.numeric(step)
  for (lv in names(fc_times)) {
    # Compared as the occasions the user supplied. Truncating both
    # sides to an integer first made every grid spaced by less than
    # one unit compare equal to itself shifted, and a grid spaced by
    # more than one compare unequal when it was right. The
    # comparison is a tolerance rather than an identity because the
    # expected occasions are arithmetic on a recorded step, so they
    # land within floating-point noise of the values the frame
    # holds rather than on them exactly.
    fut <- sort(as.numeric(fc_times[[lv]]))
    if (!length(fut)) next
    expected <- past[length(past)] + step * seq_along(fut)
    if (!isTRUE(all.equal(fut, expected))) {
      stop(insight::format_error(c(
        paste0(
          "'newdata' must continue the training series for a '",
          get_trend_name(trend_spec), "' trend."
        ),
        x = paste0(
          "The training grid runs to time ", past[length(past)],
          ", so the next ", length(fut), " times for series '", lv,
          "' are ", expected[1L], " to ",
          expected[length(expected)], "; got ", fut[1L], " to ",
          fut[length(fut)], "."
        ),
        i = paste0(
          "This trend advances one step per time point, so a gap ",
          "would be forecast as though it were not there. Supply ",
          "every intervening time, or use 'CAR()', which carries ",
          "the elapsed gap."
        ),
        i = paste0(
          "The grid is where the latent state runs, which on a frame ",
          "padded with unobserved rows reaches past the last ",
          "response. Occasions inside it already carry a state, so ",
          "'hindcast()' is what reads them."
        )
      )))
    }
  }
  invisible(TRUE)
}


#' Which observation designs a fitted model holds
#'
#' A univariate model has one, keyed `X`; a model written with
#' `brms::mvbf()` has one per response, keyed `X_<resp>` and paired
#' with its own `obs_trend_time_<resp>` and `obs_trend_series_<resp>`.
#' The index arrays are mvgam's own, so they are what the set is read
#' from rather than the design names, which a covariate could collide
#' with.
#'
#' @param standata The assembled Stan data list.
#' @return Character vector of response suffixes, `""` for the
#'   univariate case.
#' @noRd
obs_design_responses <- function(standata) {
  nm <- grep("^obs_trend_time", names(standata), value = TRUE)
  sub("^obs_trend_time_?", "", nm)
}


#' Map each observation row onto its row of the trend design
#'
#' The generated program reads a row's latent state as
#' `trend[obs_trend_time[n], obs_trend_series[n]]`, and that cell's own
#' mean is `mu_trend[times_trend[t, s]]`. Composing the two is what
#' says which row of `X_trend` enters observation row `n`, and it is
#' the only place that composition is written in R.
#'
#' @param standata The assembled Stan data list.
#' @param resp Response suffix, `""` on a univariate model.
#' @return Integer vector, one trend-design row per observation row, or
#'   `NULL` where the arrays needed to answer are not present.
#' @noRd
obs_rows_to_trend_rows <- function(standata, resp = "") {
  sfx <- if (nzchar(resp)) paste0("_", resp) else ""
  needed <- c(
    "times_trend", paste0("obs_trend_time", sfx),
    paste0("obs_trend_series", sfx)
  )
  if (!all(needed %in% names(standata))) {
    return(NULL)
  }
  tt <- standata$times_trend
  ot <- as.integer(standata[[needed[2L]]])
  os <- as.integer(standata[[needed[3L]]])
  if (!is.matrix(tt) || length(ot) != length(os) || length(ot) == 0L) {
    return(NULL)
  }
  in_range <- ot >= 1L & ot <= nrow(tt) & os >= 1L & os <= ncol(tt)
  if (!all(in_range)) {
    return(NULL)
  }
  as.integer(tt[cbind(ot, os)])
}


#' The design the likelihood actually sees
#'
#' `mu[n]` is `X[n, ] * b` plus the trend at that row's cell, whose own
#' mean is `X_trend * b_trend` read through
#' `obs_rows_to_trend_rows()`. Whether the two sides are separately
#' identified is therefore a question about the pair stacked side by
#' side, and it is settled before any sampling.
#'
#' @param standata The assembled Stan data list.
#' @param pinned_coefs Observation coefficients held at a constant,
#'   whose columns carry no free parameter and are dropped.
#' @param resp Response suffix, `""` on a univariate model.
#' @return A numeric matrix with named columns, or `NULL` where the
#'   stack cannot be formed.
#' @noRd
stacked_obs_trend_design <- function(standata, pinned_coefs = character(),
                                     resp = "") {
  x_obs <- standata[[if (nzchar(resp)) paste0("X_", resp) else "X"]]
  x_trend <- standata$X_trend
  if (is.null(x_obs) || is.null(x_trend) ||
        !is.matrix(x_obs) || !is.matrix(x_trend)) {
    return(NULL)
  }
  idx <- obs_rows_to_trend_rows(standata, resp)
  if (is.null(idx) || length(idx) != nrow(x_obs) ||
        any(idx < 1L) || any(idx > nrow(x_trend))) {
    return(NULL)
  }
  obs_names <- colnames(x_obs)
  if (is.null(obs_names)) {
    obs_names <- paste0("obs_", seq_len(ncol(x_obs)))
    colnames(x_obs) <- obs_names
  }
  x_obs <- x_obs[, !obs_names %in% pinned_coefs, drop = FALSE]
  mapped <- x_trend[idx, , drop = FALSE]
  trend_names <- colnames(x_trend)
  if (is.null(trend_names)) {
    trend_names <- paste0("trend_", seq_len(ncol(x_trend)))
  }
  colnames(mapped) <- paste0(trend_names, "_trend")
  cbind(x_obs, mapped)
}


#' Warn where the observation and trend designs span a direction twice
#'
#' The two sides are fitted together, so a column of one that lies in
#' the span of the other leaves a flat direction in the likelihood.
#'
#' A notice rather than a refusal, settled by fitting the pairing three
#' ways on one frame. `y ~ 1` against `~ series + AR(p = 1)` under the
#' default priors returns each part at a posterior SD of 4.26 while
#' their sums hold at 0.11 to 0.36; under `std_normal()` on the trend
#' coefficients the same fit is proper at R-hat 1.02 and the parts are
#' still displaced by the intercept the data cannot separate. The
#' identified spelling, `y ~ -1`, recovers the levels. So what a
#' confounded pairing costs is the decomposition, not the fit: the
#' sums, the fitted values and the forecasts are all identified, which
#' is why refusing would reject a model that samples and predicts.
#'
#' @param standata The assembled Stan data list.
#' @param prior The observation-side prior table, read for the
#'   coefficients a constant pins.
#' @return `invisible(TRUE)`.
#' @noRd
warn_confounded_obs_trend_design <- function(standata, prior = NULL) {
  for (resp in obs_design_responses(standata)) {
    design <- stacked_obs_trend_design(
      standata, pinned_prior_coefs(prior, resp), resp
    )
    if (is.null(design) || ncol(design) == 0L || nrow(design) == 0L) {
      next
    }
    decomp <- qr(design)
    if (decomp$rank >= ncol(design)) {
      next
    }
    dependent <- colnames(design)[
      decomp$pivot[seq(decomp$rank + 1L, ncol(design))]
    ]
    rlang::warn(insight::format_warning(c(
      paste0(
        "The observation and trend designs are not separately ",
        "identified", if (nzchar(resp)) paste0(" for '", resp, "'"), "."
      ),
      x = paste0(
        "Stacked they hold ", ncol(design), " columns of rank ",
        decomp$rank, ", so one direction is flat in the likelihood."
      ),
      x = paste0(
        "'", paste(dependent, collapse = "', '"),
        "' adds nothing the other columns do not already span."
      ),
      i = paste0(
        "Sums of the confounded coefficients are identified, so fitted ",
        "values and forecasts are unaffected; the individual values are ",
        "not, and report whatever the prior allowed."
      ),
      i = paste0(
        "Drop the observation-side term, or move the shared term to one ",
        "side only."
      )
    )))
  }
  invisible(TRUE)
}
