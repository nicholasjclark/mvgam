#' Validate and Standardize Family Argument
#'
#' @description
#' The one reader of a family argument. Turns every spelling a model
#' accepts into one family object: a family function, a call to one,
#' a brms family name, or the name of one of mvgam's own families. A
#' stats family becomes the `brmsfamily` brms builds from it, which
#' carries its distributional parameters beside its links.
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
    constructor <- mvgam_family_constructors()[[family[1]]]
    if (!is.null(constructor)) {
      if (!is.na(link)) {
        stop(insight::format_error(c(
          paste0("A link cannot be given with the family name '",
                 family[1], "'."),
          i = paste0("Give the link inside the call: '", family[1],
                     "(link = ...)'.")
        )), call. = FALSE)
      }
      return(constructor())
    }
    # brms's own refusal names the family it was given and the ones
    # it supports, which says more than a caught error could.
    return(brms::brmsfamily(family[1], link = link))
  }

  # A function that returned something other than a family
  stop(insight::format_error(
    "'family' must be a family object, a family function or a family name."
  ), call. = FALSE)
}


#' Validate Family is Supported by mvgam
#'
#' @description
#' Checks that the family is supported by mvgam. A brms `mixture()`
#' is refused because mvgam's post-fit methods (summaries, predictions,
#' log-likelihoods and residuals) have no kernel for a mixture of
#' families. brms's multi-category families model a matrix response
#' through one linear predictor per category, while mvgam's trend adds
#' to a single predictor per row, and the two cannot be composed. brms
#' marks every such family with a special, `"categorical"`,
#' `"multinomial"` or `"simplex"`, and each is pointed at the mvgam
#' family that takes the same response in long format.
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
  if (inherits(family, "mixfamily")) {
    stop(insight::format_error(c(
      "Mixture families are not supported by mvgam.",
      x = paste0("mvgam's summaries, predictions and log-likelihoods ",
                 "each take a single family per response."),
      i = paste0("Request support at ",
                 "https://github.com/nicholasjclark/mvgam/issues.")
    )), call. = FALSE)
  }
  # mvgam's own multi-response families (diri / multi / categ / mvn /
  # mvt) take a long-format K-row response through the closure-unit
  # pipeline.
  if (is_multi_response_family(family)) {
    return(invisible(TRUE))
  }
  # `logistic_normal()` is also marked "simplex", so it is matched
  # first.
  wrappers <- c(
    logistic_normal = "mvn()",
    categorical = "categ()",
    multinomial = "multi()",
    simplex = "diri()"
  )
  pointer <- wrappers[intersect(names(wrappers), family$specials)][1L]
  if (!is.na(pointer)) {
    stop(insight::format_error(c(
      paste0("Family '", resolve_family_name(family),
             "' is not supported by mvgam directly."),
      i = paste0("Use ", pointer, ", the mvgam wrapper for a response ",
                 "in long format.")
    )), call. = FALSE)
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
#' @param identifiability Logical. Whether to run the checks that ask
#'   whether the design can separate state from detection: two units
#'   at minimum, and the single-visit tests below it. Those are
#'   questions about a design being fitted. A prediction frame is not
#'   being fitted, so the predict-time caller passes `FALSE`. Asked of
#'   one, they refused a forecast grid, which carries no response by
#'   construction, and a caller predicting at a single site.
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
                                        unit_grouping_vars  = NULL,
                                        identifiability     = TRUE) {
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
  checkmate::assert_flag(identifiability)
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
  assert_axis_column(data, series_var, "series")
  assert_axis_column(data, time_var, "time")
  assert_closure_unit_columns(
    data, setdiff(required_cols, c(series_var, time_var))
  )

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
  #
  # A frame with nothing observed anywhere is refused where the units
  # are formed, which is the layer that owns the condition and the
  # only one a fit-time call cannot bypass. Refusing it a second time
  # here also refused a forecast grid, where no row carries a response
  # by construction and every check below is vacuous.
  observed <- !is.na(y_vals)
  y_obs <- suppressWarnings(as.numeric(y_vals[observed]))
  if (any(!is.finite(y_obs))) {
    stop(insight::format_error(
      paste0(
        "'", response_var, "' contains values that are not ",
        "finite numbers."
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
    stop(insight::format_error(
      paste0("Non-integer values found in response '", response_var, "'.")
    ))
  }
  if (binary_y_check && any(y_int[observed] > 1L, na.rm = TRUE)) {
    bad <- which(!is.na(y_int) & y_int > 1L)[1L]
    stop(insight::format_error(c(
      paste0(
        "A closure-unit family with a binary response requires '",
        response_var, "' in {0, 1}."
      ),
      x = paste0(
        "Row ", bad, ": ", response_var, " = ", y_int[bad], "."
      ),
      i = paste0(
        "For count detections use family = nmix(). ",
        "occ() and nmix(\"royle_nichols\") model detection / ",
        "non-detection only."
      )
    )))
  }

  if (!is.null(cap_vals) &&
        any(!is.finite(suppressWarnings(as.numeric(cap_vals))))) {
    stop(insight::format_error(
      paste0(
        "'", cap_var, "' contains values that are not ",
        "finite numbers."
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
        i = paste0("Give '", cap_var, "' one value per closure unit.")
      )))
    }
  }

  if (!identifiability) {
    return(invisible(TRUE))
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
        "A single unit is one draw from the state distribution, ",
        "however many visits it has."
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
      warn_once(
        c(
          "Every closure unit has a single visit and zero covariates.",
          i = paste0(
            "The data identify only the product of state and ",
            "detection probability."
          ),
          i = paste0(
            "The priors set each probability on its own ",
            "(Royle and Dorazio 2008, ch. 3.5)."
          )
        ),
        "closure_unit_all_single_visit"
      )
    } else {
      stop(insight::format_error(c(
        "The closure-unit count family is not identified.",
        x = paste0(
          "Every unit has a single visit and neither the state ",
          "nor the detection formula contains a covariate."
        ),
        x = paste0("With unbounded state support, lambda and detection ",
                   "p lie on the isocurve lambda * p = observed ",
                   "(Solymos et al. 2012)."),
        i = paste0("Add a covariate to a formula or supply more visits ",
                   "per closure unit.")
      )), call. = FALSE)
    }
  }
  single_visit_share <- mean(rep_counts == 1L)
  if (single_visit_share > 0.3 && !all(rep_counts == 1L)) {
    warn_once(
      c(
        paste0(
          "More than 30% of closure units have a single visit ",
          "(", round(100 * single_visit_share),
          "% single-visit units)."
        ),
        i = paste0(
          "A single-visit unit separates state from detection only ",
          "through the covariates in the two formulas."
        )
      ),
      "closure_unit_single_visit"
    )
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
#' Every column a formula's left-hand side reads
#'
#' The response and its addition terms together, so `y | trials(n)`
#' answers `y` and `n`. That is the set a model frame carries and the
#' set a covariate check has to leave alone. Which of them is the
#' response is `response_columns()`'s question, not this one.
#'
#' Handles plain `formula`, `brmsformula` (uses the `$formula` slot)
#' and `mvbrmsformula` (iterates `$forms`). Returns an empty character
#' vector for one-sided formulas, NULL input, or non-formula objects
#' so callers can rely on a character vector return shape.
#'
#' `rlang::f_lhs()` cannot be used directly because it errors on
#' `brmsformula` objects (they are lists internally, not true
#' `formula`s).
#'
#' @noRd
lhs_columns <- function(formula) {
  if (is.null(formula)) return(character(0L))
  if (inherits(formula, "mvbrmsformula") && !is.null(formula$forms)) {
    return(unique(unlist(lapply(formula$forms, function(x) {
      lhs_columns(x$formula %||% x)
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


#' The data columns each formula of a model uses
#'
#' Each side is parsed with `brms::brmsterms()`, as brms parses it
#' when it builds the model frame. The parse lists the responses, the
#' covariates, the columns of addition terms such as `trials()` and
#' `weights()`, offsets, and the slopes and grouping factors of random
#' effects. The trend side leaves out its constructor, whose axis and
#' grouping columns are checked where the axis is resolved.
#'
#' @param obs_formula The observation formula
#' @param family The observation family
#' @param trend_formula The trend formula, or `NULL`
#' @return A list with the `formula` and `trend_formula` columns
#' @noRd
model_columns <- function(obs_formula, family, trend_formula = NULL) {
  list(
    formula = mvgam_side_terms(obs_formula, family)$all,
    trend_formula = if (!is.null(trend_formula)) {
      trend_formula_covariates(
        parse_base_formula_safe(trend_formula, mvgam_trend_registry())
      )
    }
  )
}


#' Refuse absent or incomplete model columns
#'
#' brms reports an absent column only when it builds the model frame,
#' after the trend is parsed, and its message names neither the formula
#' that uses the column nor the argument that lacks it. As in
#' `brms:::validate_data()`, a name `data2` holds counts as present.
#'
#' brms also drops a row with `NA` in any model-frame column. mvgam
#' keeps the rows of a missing response on the trend's time grid and
#' skips them in the likelihood. A missing covariate leaves a row that
#' the trend expects and the linear predictor cannot fill. An addition
#' term is checked only on the rows whose response was observed,
#' because a row with a missing response leaves the likelihood.
#'
#' @param data A data frame, or a list of vectors and matrices
#' @param obs_formula The observation formula, with its family resolved
#' @param family The observation family
#' @param trend_formula The trend formula, or `NULL`
#' @param data2 The list of further model data, or `NULL`
#' @param optional Columns `data` may omit, such as the responses of a
#'   prediction frame
#' @param context The argument that supplied `data`
#' @return `TRUE`, invisibly
#' @noRd
validate_model_columns <- function(data, obs_formula, family,
                                   trend_formula = NULL, data2 = NULL,
                                   optional = character(0L),
                                   context = "data") {
  columns <- model_columns(obs_formula, family, trend_formula)
  used <- unique(unlist(columns, use.names = FALSE))
  absent <- setdiff(used, c(names(data), names(data2), optional))
  covariates <- intersect(setdiff(used, lhs_columns(obs_formula)),
                          names(data))
  refuse_column_gaps(
    absent,
    c(na_rows(data, covariates), addition_term_na_rows(data, obs_formula)),
    context
  )
}


# Internal: the rows of each column that hold a missing value. A
# matrix column, such as a distributed-lag predictor, counts a row
# once however many of its cells are missing.
#'@noRd
na_rows <- function(data, columns) {
  rows <- lapply(columns, function(v) {
    x <- data[[v]]
    which(if (is.matrix(x)) rowSums(is.na(x)) > 0L else is.na(x))
  })
  stats::setNames(rows, columns)
}


# Internal: refuse model columns a frame omits or leaves incomplete.
# The fit and prediction paths share this message, and a frame meets
# the same wording wherever it enters.
#'@noRd
refuse_column_gaps <- function(absent, gaps, context) {
  gaps <- Filter(length, gaps)
  if (!length(absent) && !length(gaps)) {
    return(invisible(TRUE))
  }
  headline <- if (length(absent)) {
    paste0("Columns the model uses are missing from '", context, "'.")
  } else {
    paste0("Columns the model uses hold missing values in '", context,
           "'.")
  }
  lines <- c(
    if (length(absent)) {
      paste0("Absent: ", paste0("'", absent, "'", collapse = ", "), ".")
    },
    vapply(names(gaps), function(v) {
      n <- length(gaps[[v]])
      paste0("'", v, "': ", n, if (n > 1L) " NAs" else " NA",
             ", first at row ", gaps[[v]][1L], ".")
    }, character(1L))
  )
  hint <- if (identical(context, "newdata")) {
    "A missing response is allowed and marks a row to predict."
  } else {
    "Impute the missing values before fitting."
  }
  stop(insight::format_error(c(
    headline,
    stats::setNames(lines, rep("x", length(lines))),
    if (length(gaps)) c(i = hint)
  )), call. = FALSE)
}


#' Missing values in the addition terms of observed rows
#'
#' brms evaluates an addition term such as `weights(w)` or `trials(n)`
#' on every row whose response was observed, and drops a row whose
#' addition term is missing as it drops one with a missing covariate.
#' The trend mapping keeps every row with an observed response, and
#' the two would then describe different rows. A row with a missing
#' response leaves the likelihood whatever its addition terms hold.
#'
#' @param data A data frame
#' @param obs_formula The observation formula
#' @return Named list of row indices, one per addition-term column
#' @noRd
addition_term_na_rows <- function(data, obs_formula) {
  if (is.null(obs_formula) || !length(lhs_columns(obs_formula))) {
    return(list())
  }
  rows <- lapply(unname(response_formulas(obs_formula)), function(form) {
    y <- response_columns(form)[[1L]]
    extra <- intersect(setdiff(lhs_columns(form), y), names(data))
    if (!length(extra) || is.null(data[[y]])) {
      return(list())
    }
    seen <- !is.na(data[[y]])
    lapply(na_rows(data, extra), function(r) r[seen[r]])
  })
  do.call(c, rows)
}


#' Refuse a frame missing named columns
#'
#' @param data Data frame to check
#' @param required_vars Character vector of required column names
#' @param context The argument that supplied `data`
#' @return `TRUE`, invisibly
#' @noRd
validate_required_variables <- function(data, required_vars,
                                        context = "data") {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(required_vars, min.len = 1)
  checkmate::assert_string(context)
  missing_vars <- setdiff(required_vars, names(data))
  if (!length(missing_vars)) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    paste0("Required columns are missing from '", context, "'."),
    x = paste0("Absent: ", paste0("'", missing_vars, "'", collapse = ", "),
               "."),
    i = paste0("Columns present: ",
               paste0("'", names(data), "'", collapse = ", "), ".")
  )), call. = FALSE)
}


#' Shape-only assertion for `trend_map` (constructor fail-fast).
#'
#' Every trend constructor that accepts `trend_map` calls this, so
#' malformed input fails when the constructor runs. The checks that
#' need data (dimension match, series-label coverage, contiguity) run
#' in `normalise_trend_map()`.
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
#' Both arguments hold one row per series and arrive before the axis
#' is resolved. The levels returned are those observed in the data.
#' A declared level with no rows has no trend series and gets no row.
#'
#' @param data The user's data frame.
#' @param series_var The series column the trend constructor names.
#' @return Character vector of series levels, in axis order.
#' @noRd
argument_series_levels <- function(data, series_var) {
  checkmate::assert_data_frame(data)
  assert_axis_column(data, series_var, "series")
  as.character(observed_levels(data[[series_var]]))
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
#' @param data Training `data.frame` whose series column resolves
#'   the dimensions and labels.
#' @param series_var The series column the trend constructor names.
#'
#' @return `list(Z = <num matrix>, n_lv = <int>)`.
#' @noRd
normalise_trend_map <- function(input, data, series_var = "series") {
  if (is.null(input)) return(NULL)
  checkmate::assert_data_frame(data)
  series_levels <- argument_series_levels(data, series_var)
  n_series <- length(series_levels)
  Z <- if (is.character(input)) {
    trend_map_from_character(input, n_series)
  } else if (is.data.frame(input)) {
    trend_map_from_dataframe(input, series_levels)
  } else if (is.matrix(input) && is.numeric(input)) {
    trend_map_from_matrix(input, series_levels)
  } else {
    stop(insight::format_error(c(
      "'trend_map' must be a matrix, data.frame or character code.",
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


# Internal: does a table name every training series exactly once?
#
# Three shapes ask this. A `trend_map` data frame names its series in
# a column, a `trend_map` matrix names them in its rownames, and a
# `loadings_prior` feature frame does either. Every fault is named in
# one message, so a caller who wrote a stranger in place of a real
# series is told both halves at once instead of learning the second
# after fixing the first.
#
# @param supplied The series labels the caller wrote.
# @param series_levels The training series, in their canonical order.
# @param subject The argument to name in the message.
#'@noRd
assert_series_coverage <- function(supplied, series_levels, subject) {
  supplied <- as.character(supplied)
  absent <- setdiff(series_levels, supplied)
  unknown <- setdiff(supplied, series_levels)
  dups <- unique(supplied[duplicated(supplied)])
  if (!length(absent) && !length(unknown) && !length(dups)) {
    return(invisible(TRUE))
  }
  quoted <- function(v) paste0("'", v, "'", collapse = ", ")
  stop(insight::format_error(c(
    paste0("'", subject,
           "' must list every training series exactly once."),
    x = if (length(absent)) {
      paste0("Missing: ", quoted(absent), ".")
    },
    x = if (length(unknown)) {
      paste0("Unknown to the training data: ", quoted(unknown), ".")
    },
    x = if (length(dups)) {
      paste0("Duplicated: ", quoted(dups), ".")
    },
    i = paste0("The training series are: ", quoted(series_levels), ".")
  )), call. = FALSE)
}


# Internal: the row order that puts a caller's series into the axis
# order, refusing any labelling that cannot produce one.
#
# Reordering rows without first checking the labels is the fault this
# exists to prevent, so the two steps are one operation and a caller
# cannot reach the second without the first.
#
# @inheritParams assert_series_coverage
# @return An integer index, one position per training series.
#'@noRd
series_row_order <- function(supplied, series_levels, subject) {
  assert_series_coverage(supplied, series_levels, subject)
  match(series_levels, as.character(supplied))
}

#' Refuse a method that needs draws an unfitted stub does not carry
#'
#' `run_model = FALSE` returns an object classed `mvgam_prefit` whose
#' `fit` slot is `NULL`. Every method needing a posterior otherwise
#' reached `posterior::as_draws_matrix()` and failed on the empty
#' slot with "Don't know how to transform an object of class 'NULL'",
#' which names neither the state nor the argument that produced it.
#' Reading the program and its data is why the mode exists, and
#' `stancode()` and `standata()` do not call this.
#'
#' The class is what the stub is asked for. An absent `fit` slot is
#' the broader condition and also describes the classed stubs the
#' tests build to reach one validator without a posterior, which a
#' user cannot construct and which this refusal would mis-describe.
#'
#' @param object An `mvgam` object.
#' @param fn The method's own name, which the message reports. A
#'   shared accessor several methods reach leaves this `NULL`, since
#'   naming the accessor would describe code the caller never wrote.
#' @return Invisibly `TRUE`, or an error.
#' @noRd
require_fitted_model <- function(object, fn = NULL) {
  if (!inherits(object, "mvgam_prefit")) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    if (is.null(fn)) {
      "A fitted model is required."
    } else {
      paste0(fn, "() requires a fitted model.")
    },
    x = "The object was built with `run_model = FALSE`.",
    i = "`stancode()` and `standata()` work on it."
  )), call. = FALSE)
}


#' Refuse a set of models where any one of them lacks a posterior.
#'
#' Averaging weighs several fits against each other, so a single
#' unfitted stub among them stops the whole call. The message is the
#' one every other method gives a prefit, raised from one place.
#'
#' @param models A list of `mvgam` objects.
#' @param fn The calling method's name, which the message reports.
#' @return Invisibly `TRUE`, or an error.
#' @noRd
require_fitted_models <- function(models, fn) {
  for (model in models) {
    require_fitted_model(model, fn)
  }
  invisible(TRUE)
}


#' Refuse dot arguments absent from the method's and receiver's
#' formals.
#'
#' A method that hands `...` to another package splits the names in
#' its own formals from the names in the receiver's. A name in neither
#' set reached the receiver, which discards what it does not
#' recognise, and the caller got a result computed from the default
#' they meant to override. bayesplot emits a warning for this and loo
#' emits nothing, so the signal depended on which package was
#' downstream.
#'
#' `rlang::check_dots_empty()` covers the methods with no use for
#' dots. This covers the ones that legitimately forward them.
#'
#' @param dots The method's `list(...)`.
#' @param allowed Names in the method's or the receiver's formals.
#' @param fn The method's own name, which the message reports.
#' @return Invisibly `TRUE`, or an error.
#' @noRd
refuse_unread_dots <- function(dots, allowed, fn) {
  unread <- setdiff(names(dots), allowed)
  if (!length(unread)) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "Unrecognised arguments were supplied.",
    x = paste0(
      "Unknown to ", fn, "() and to what it hands the work to: ",
      paste0("'", unread, "'", collapse = ", "), "."
    ),
    i = paste0("Check the spelling against '?", fn, "'.")
  )), call. = FALSE)
}


# Numeric-matrix branch. Validates shape + finite + no all-zero
# rows. Rownames, where the user supplies them, say which series each
# row of loadings belongs to, and the rows are read in that order.
#'@noRd
trend_map_from_matrix <- function(input, series_levels) {
  n_series <- length(series_levels)
  if (nrow(input) != n_series) {
    stop(insight::format_error(c(
      "'trend_map' matrix has the wrong number of rows.",
      x = paste0(
        "Expected ", n_series, " (one per series), got ",
        nrow(input), "."
      )
    )))
  }
  # A matrix carries rownames, and a user who writes them is saying
  # which series each row of loadings belongs to. Taking the rows
  # positionally instead hands each series another's loadings, and
  # the declared names are then written back over the result.
  rn <- rownames(input)
  if (!is.null(rn)) {
    input <- input[
      series_row_order(rn, series_levels, "trend_map"), , drop = FALSE
    ]
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
      "Every series must load on at least one factor in 'trend_map'.",
      x = paste0(
        "Series with no loadings: ",
        paste0("'", series_levels[zero_rows], "'", collapse = ", "), "."
      ),
      i = "NA marks an entry to be sampled."
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
  ord <- series_row_order(s, series_levels, "trend_map")
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
  # `ord` puts the user's rows into the canonical series order, so
  # `Z[s, ]` aligns with the axis the sampler indexes.
  Z <- matrix(0, nrow = length(series_levels), ncol = max_t)
  Z[cbind(seq_along(series_levels), t[ord])] <- 1
  Z
}


#' Does the trend require regular time intervals?
#'
#' @description
#' The data validators ask the trend's declared `validation_rules`,
#' so a new trend type sets the gate by its own declaration.
#'
#' @param trend_specs `NULL`, an `mvgam_trend`, or a per-response
#'   list of them.
#' @return Logical scalar.
#' @noRd
trend_requires_regular_intervals <- function(trend_specs) {
  rule_requires_regular_intervals %in%
    trend_spec_head(trend_specs)$validation_rules
}
#' The series a spec will give a frame, before any axis exists
#'
#' The two grouping validators run before the axis is resolved, and
#' both need the series each row belongs to under this specification.
#' The grouping is taken through `spec_groupings()`, which accepts the
#' flat and the nested spelling of a spec. A frame with no `series`
#' column still has series here, because `gr` and `subgr` name them.
#'
#' `assert_grouping_columns()` refuses a grouping column the frame
#' lacks or leaves incomplete. Stan assembly would otherwise report it
#' after brms setup, against a variable the user never wrote.
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
  subgr <- groupings$subgr
  series_var <- spec_axis_vars(trend_spec)$series_var
  # A `subgr` naming the series column is checked with the axis.
  own_subgr <- named_var(subgr) && !identical(subgr, series_var)
  assert_grouping_columns(data, gr_var, if (own_subgr) subgr)
  series <- if (own_subgr) {
    hierarchical_series_values(data, gr_var, subgr)
  } else if (series_var %in% colnames(data)) {
    data[[series_var]]
  } else {
    return(NULL)
  }
  list(gr_var = gr_var, series = series, series_var = series_var)
}


# Internal: the trend's grouping arguments, checked against the frame.
# `gr` needs `subgr`, and each series belongs to one group. The Stan
# program sizes every group's Cholesky and scale blocks by one subgroup
# count, and an unbalanced design gives NaN at initialisation. The
# returned spec holds "NA" for a grouping it does not use.
#'@noRd
resolve_trend_groupings <- function(parsed_trend, data, series_var) {
  groupings <- validate_grouping_arguments(
    parsed_trend$gr, parsed_trend$subgr, series_var
  )
  parsed_trend$gr <- groupings$gr %||% "NA"
  parsed_trend$subgr <- groupings$subgr %||% "NA"
  if (!is.null(groupings$gr)) {
    validate_gr_constant_per_series(parsed_trend, data)
    validate_gr_balanced_groups(parsed_trend, data)
  }
  parsed_trend
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

  # The series counted are the series the trend will have, which is
  # what lets an unbalanced design be caught: sized by the largest
  # group, the smaller groups take a slice of a correlation matrix
  # they never asked for, and every index stays in range.
  series_vals <- resolved$series

  # One group value per series, from the first row naming it. The
  # balance is checked before any axis is resolved, and the count
  # needs no order.
  first <- !duplicated(as.character(series_vals))
  series_group_table <- table(data[[gr_var]][first])
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
      "Hierarchical trend models require the same number of ",
      "series in every group."
    ),
    x = paste0(
      "Grouping variable '", gr_var,
      "' has unbalanced groups: ", counts_str, "."
    ),
    i = paste0("Subset the data to a balanced design or combine small ",
               "groups.")
  )), call. = FALSE)
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
  # `mvgam_axes()` falls back to the Stan data for a fit that
  # predates it.
  n_series <- n_series %||% mvgam_axes(object)$series$n
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
      paste0("'n_lv' must be at most the number of ", noun, "."),
      x = paste0(
        "Got n_lv = ", n_lv_int, ", n_", noun, " = ", n_species, "."
      ),
      i = paste0(
        "The residual covariance has rank at most n_", noun, "."
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
    "'mvgam()' takes 'n_lv' on the trend constructor.",
    x = "brms and Stan ignore a top-level 'n_lv'.",
    i = paste0(
      "Write it on the trend as ",
      "'trend_formula = ~ AR(p = 1, n_lv = ", requested_n_lv,
      ")' or use 'jsdgam(n_lv = ", requested_n_lv, ")'."
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
    x = info$incompatibility_reason,
    i = paste0(
      "Drop 'n_lv' and 'trend_map', or use a trend with a factor form: ",
      registered_trend_constructors(factors_only = TRUE), "."
    )
  )), call. = FALSE)
}


#' Refuse a factor request written on a trend constructor
#'
#' `CAR()` and `PW()` take `n_lv` and `trend_map` only to refuse
#' them. One helper composes that refusal from the registry entry,
#' giving the same sentence as the routes that reach a spec without
#' a constructor call.
#'
#' @param n_lv Whatever arrived as the constructor's `n_lv`.
#' @param trend_map Whatever arrived as the constructor's `trend_map`.
#' @param trend_name Registered trend name, such as `"PW"`.
#' @return `invisible(TRUE)` when neither argument was written.
#' @noRd
refuse_constructor_factor_request <- function(n_lv, trend_map,
                                              trend_name) {
  if (is.null(n_lv) && is.null(trend_map)) return(invisible(TRUE))
  refuse_factor_request_for_trend(trend_name)
}


#' Refuse a factor model on a trend that has no factor form
#'
#' The registry's `supports_factors` entry records whether a trend
#' decomposes into latent factors, and `incompatibility_reason` gives
#' the reason for a trend that cannot. `prepare_trend_specs()` calls
#' this once every route to a factor model has set `n_lv` on the
#' spec: the constructor's `n_lv`, a `trend_map`, and the all-`NA`
#' `trend_map` that `jsdgam()` builds. A top-level `n_lv` passed to
#' `mvgam()` reaches no spec, and `refuse_top_level_n_lv()` refuses
#' it.
#'
#' @param trend_specs Normalised trend specifications, one spec or a
#'   per-response list.
#' @return `invisible(TRUE)`.
#' @noRd
enforce_factor_support_against_specs <- function(trend_specs) {
  spec <- trend_spec_head(trend_specs)
  if (!is.null(spec$n_lv)) {
    refuse_factor_request_for_trend(get_trend_name(spec))
  }
  invisible(TRUE)
}


#' Apply the `n_lv` ceiling to the resolved series axis
#'
#' Called once, where `extract_and_validate_trend_components()` has
#' resolved the series axis. The axis also counts series that a
#' grouping or the responses define with no series column, which a
#' count of the column missed. mvgam fits one shared trend, whose
#' spec gives `n_lv`. The function applies
#' `validate_n_lv_ceiling()` for the capacity ceiling and
#' `warn_unidentified_component_scale()` for the identification
#' bound of the mv-response families. A trend with no `n_lv` passes
#' unchecked.
#'
#' @param trend_specs One trend spec or a per-response list of them.
#' @param n_series Length of the resolved series axis.
#' @param fit_function Name of the calling fit function, for the
#'   refusal message.
#' @param family The observation family, or NULL.
#' @return `TRUE`, invisibly.
#' @noRd
enforce_n_lv_ceiling_against_data <- function(trend_specs, n_series,
                                              fit_function = "mvgam",
                                              family = NULL) {
  checkmate::assert_int(n_series, lower = 1L)
  spec <- trend_spec_head(trend_specs)
  n_lv <- spec_n_lv(spec)
  if (is.null(n_lv) || n_lv < 1L || n_series < 2L) {
    return(invisible(TRUE))
  }
  validate_n_lv_ceiling(
    n_lv         = n_lv,
    n_species    = as.integer(n_series),
    fit_function = fit_function
  )
  # The ceiling above is about capacity: more factors than series
  # buys nothing. This is about identification, and it bites well
  # below that ceiling for a family carrying a residual scale per
  # component. A `trend_map` fixing the loadings samples no `Z`,
  # which leaves no split to identify.
  if (is.null(spec$fixed_Z)) {
    warn_unidentified_component_scale(
      n_lv = n_lv, n_species = as.integer(n_series), family = family
    )
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
#' @param series_var The series column, which names the subgroups when
#'   `gr` is given without `subgr`
#' @return List with processed gr and subgr values
#' @noRd
validate_grouping_arguments <- function(gr, subgr, series_var) {
  checkmate::assert_string(series_var, min.chars = 1)
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

  # The hierarchical Stan data derives the subgroups from the series
  # column, and a bare `ZMVN(gr = X)` names that column as `subgr`.
  if (!is.null(gr) && is.null(subgr)) {
    subgr <- series_var
  }

  if (!is.null(subgr) && is.null(gr)) {
    stop(insight::format_error(
      paste0("'subgr = ", subgr, "' requires a main grouping 'gr'.")
    ))
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
  # Spacing is a property of the sorted distinct times, whatever order
  # the time index takes them in.
  intervals <- diff(sort(unique(time_values)))

  # Check for regular intervals (allowing small numerical tolerance)
  tolerance <- 1e-10
  interval_range <- range(intervals)
  is_regular <- abs(interval_range[2] - interval_range[1]) < tolerance

  if (!is_regular) {
    stop(insight::format_error(c(
      paste0("The trend needs regularly spaced times in '", time_var, "'."),
      x = paste0(
        "Gaps between times range from ", min(intervals), " to ",
        max(intervals), "."
      ),
      i = "Add the missing times as rows with an NA response.",
      i = "'CAR()' models a trend over irregular times."
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


# Internal: families whose mean parameter must be positive. brms
# defaults each of these to a link that keeps it positive, and
# defaults `lognormal` and `shifted_lognormal` to identity because
# their `mu` is a log-scale location that takes any sign.
#'@noRd
mvgam_positive_mean_families <- c(
  "gamma", "weibull", "exponential", "frechet", "inverse.gaussian"
)

# Internal: links leaving the linear predictor free to reach zero or
# below. Each needs `eta > 0` for the mean to stay positive.
#'@noRd
mvgam_links_needing_positive_eta <- c("identity", "inverse", "1/mu^2")


#' Warn when a latent trend can drive a positive mean to zero
#'
#' A family in `mvgam_positive_mean_families` needs `mu > 0`. Under
#' `identity`, `inverse` or `1/mu^2` that holds only where the linear
#' predictor stays positive on every row. A latent trend is unbounded
#' and centred near zero, and the predictor reaches zero during
#' sampling, where the likelihood is undefined and Stan rejects the
#' draw. Without an intercept the initial draws never reach the
#' region at all and every chain stops before warmup.
#'
#' The link the caller asked for is kept. `Gamma()` and
#' `Gamma(link = "inverse")` build the same object, so substituting a
#' link here would override a deliberate choice as readily as an
#' inherited default.
#'
#' The notice is raised once per call, with no session-level
#' frequency. The condition belongs to the model the caller supplied,
#' and a second model pairing the two earns a second notice.
#' `mvgam()` and `mvgam_multiple()` each raise it once, which keeps a
#' list of imputations to one notice for the whole list.
#'
#' Every response is checked under its own family: one written inside
#' its `bf()`, or `family` otherwise. `validate_family()` turns a
#' family given by name or as a function into the object checked here.
#'
#' @param formula The observation formula
#' @param family The observation family, or `NULL` when every response
#'   names its own
#' @param trend_formula The trend formula, or `NULL`
#' @return `invisible(TRUE)`
#' @noRd
warn_positive_mean_link_with_trend <- function(formula, family,
                                               trend_formula) {
  if (is.null(trend_formula)) {
    return(invisible(TRUE))
  }
  default <- if (!is.null(family)) validate_family(family)
  families <- Filter(Negate(is.null), formula_families(formula, default))
  pairs <- unique(lapply(families, function(fam) {
    c(resolve_family_name(fam), fam$link)
  }))
  for (pair in pairs) {
    if (!pair[1L] %in% mvgam_positive_mean_families ||
        !pair[2L] %in% mvgam_links_needing_positive_eta) {
      next
    }
    insight::format_warning(c(
      paste0("Family '", pair[1L], "' with 'link = \"", pair[2L],
             "\"' needs a positive linear predictor."),
      x = paste0("A latent trend takes negative values, where the ",
                 "likelihood is undefined."),
      x = paste0("An intercept-free observation formula stops every ",
                 "chain before warmup."),
      i = paste0("Set 'link = \"log\"' to keep the mean positive for ",
                 "any predictor."),
      i = paste0("Keeping this link needs an intercept well above zero ",
                 "and a small trend scale.")
    ))
  }
  invisible(TRUE)
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
  spec <- mvgam_response_support[[fam_name]]
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
      i = response_support_hint(fam_name)
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
      i = paste0("Round the column if the fractions come from storage, ",
                 "or model it with a continuous family.")
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
    return(paste0("Use zero_inflated_beta() for exact zeros or ",
                  "zero_one_inflated_beta() for zeros and ones."))
  }
  if (fam == "zero_inflated_beta") {
    return("Use zero_one_inflated_beta() for exact ones.")
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
    return(paste0("Use the family's hurdle counterpart if exact zeros ",
                  "are real observations."))
  }
  if (isTRUE(mvgam_response_support[[fam]]$integer)) {
    return(paste0("Check the column for ",
                  "missing-data sentinels such as -1 or -999."))
  }
  "Check the response column, or choose a family whose support covers it."
}


# Internal: run `validate_response_for_family()` over each response
# the observation formula names, so a fit rejects a response its
# family cannot take with a message naming the column and the values
# observed, rather than letting brms report the constraint from a
# function the user did not call. Multi-response families take a
# matrix response and closure-unit families are checked by
# `validate_closure_unit_data()`, so both are left to their own
# validators.
#
# Each response is held to its own family: the one named inside its
# `bf()` where it has one, `family` otherwise. Only the response is
# held to it. An addition term such as `weights(w)` or `trials(n)` is
# a column the model reads, but its values are not observations of the
# family, and checking them as though they were refused a poisson fit
# for carrying fractional weights.
#'@noRd
validate_response_shapes <- function(data, formula, family) {
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

  columns <- response_columns(formula)
  # `validate_family()` normalises a character, family or customfamily
  # into one object and refuses anything else, which is a fault in the
  # user's own argument and belongs here.
  families <- formula_families(formula, validate_family(family))
  for (key in names(columns)) {
    fam <- families[[key]]
    if (is_multi_response_family(fam) || uses_closure_unit_layout(fam)) {
      next
    }
    column <- columns[[key]]
    trials <- trials_expression(response_formulas(formula)[[key]], fam)
    for (frame in frames) {
      if (!column %in% names(frame)) next
      validate_response_for_family(frame[[column]], fam, y_name = column)
      refuse_counts_above_trials(frame, column, trials)
    }
  }
  invisible(TRUE)
}


# Internal: the expression inside a response's `trials()` term, as a
# quosure in the formula's environment, or `NULL` when the response
# has none.
#'@noRd
trials_expression <- function(form, family) {
  form$family <- family
  ad <- brms::brmsterms(form)$adforms$trials
  if (is.null(ad)) {
    return(NULL)
  }
  # brms stores the term as `~ resp_trials(<expr>)`.
  rlang::new_quosure(rlang::f_rhs(ad)[[2L]],
                     environment(form$formula) %||% globalenv())
}


# Internal: refuse an observed count above its `trials()` denominator.
# brms reports this from its data preparation without naming the
# column or the row. A frame without the denominator's columns is left
# to `validate_model_columns()`.
#'@noRd
refuse_counts_above_trials <- function(frame, column, trials) {
  if (is.null(trials)) {
    return(invisible(TRUE))
  }
  # A name the frame lacks may be a constant the formula environment holds.
  unbound <- setdiff(all.vars(rlang::quo_get_expr(trials)), names(frame))
  env <- rlang::quo_get_env(trials)
  if (!all(vapply(unbound, exists, logical(1L), envir = env))) {
    return(invisible(TRUE))
  }
  y <- frame[[column]]
  n <- rep_len(rlang::eval_tidy(trials, frame), length(y))
  over <- !is.na(y) & !is.na(n) & y > n
  if (!any(over)) {
    return(invisible(TRUE))
  }
  first <- which(over)[1L]
  stop(insight::format_error(c(
    paste0("'", column, "' has counts above its 'trials()' denominator."),
    x = paste0(sum(over), " of ", sum(!is.na(y)),
               " values exceed it, the first at row ", first, " (",
               column, " = ", y[first], ", trials = ", n[first], ").")
  )), call. = FALSE)
}


#' Deparse an expression to one string
#'
#' @description
#' `base::deparse()` returns one element per line. This joins them and
#' truncates the result where a caller asks for a bounded length, which
#' is what a message naming the user's own expression needs.
#'
#' @param expr Expression to deparse
#' @param max_char Keep at most this many characters, marking a
#'   truncation with an ellipsis. `NULL` keeps the whole string.
#' @param ... Additional arguments passed to deparse
#' @return Character string representation of the expression
#' @noRd
deparse0 <- function(expr, max_char = NULL, ...) {
  out <- paste(deparse(expr, ...), collapse = "")
  if (!is.null(max_char) && nchar(out) > max_char) {
    out <- paste0(substr(out, 1L, max_char), "...")
  }
  out
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

  # brms takes non-linearity from `bf(..., nl = TRUE)` alone, and a
  # plain formula has no such flag.
  FALSE
}

# Internal: the function a call names, with `pkg::fun` giving `fun`.
# `character(0)` for a call whose head is itself computed.
#'@noRd
call_head_name <- function(expr) {
  head <- expr[[1L]]
  if (is.symbol(head)) {
    as.character(head)
  } else if (is.call(head) && identical(head[[1L]], as.name("::"))) {
    as.character(head[[3L]])
  } else {
    character(0L)
  }
}

#' Collect function-call symbol names from an unevaluated R expression
#'
#' Recursively walks a `call`/`language` tree and returns the function
#' names at each call site. Formula validators use it to detect
#' specific function calls. A user variable named `se_x` or `defense`
#' is no call, and the walk never reports it as the brms `se()` term.
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
  c(
    call_head_name(expr),
    unlist(lapply(as.list(expr)[-1L], collect_call_names),
           use.names = FALSE)
  )
}

# Internal: each call in `expr` to a function in `fns`, deparsed, in
# the order written. A call to one of `fns` nested in another counts.
#'@noRd
find_calls <- function(expr, fns) {
  if (!is.call(expr)) {
    return(character(0L))
  }
  c(
    if (any(call_head_name(expr) %in% fns)) deparse1(expr),
    unlist(lapply(as.list(expr)[-1L], find_calls, fns = fns),
           use.names = FALSE)
  )
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

#' Tell the user once per term and session that an exact GP term
#' predicts in-sample alone. `inform_once()` keeps the count and
#' honours `silent >= 2`.
#' @noRd
maybe_warn_exact_gp <- function(gp_term) {
  example <- gsub("\\)$", ", k = 20)", gp_term)
  inform_once(
    c(
      cli::format_inline(
        "Exact GP term in {.field {gp_term}} (no {.field k} given)."
      ),
      i = "Prediction at 'newdata' requires the approximate form.",
      i = cli::format_inline(
        "Pass {.field k}, as in {.code {example}}, to use it."
      )
    ),
    paste0("mvgam_exact_gp_", gp_term)
  )
}

#' Warn (once) on exact GP terms
#'
#' Scans a formula for `gp()` terms that omit `k`. Exact GPs fit
#' through brms with their full covariance kernel. mvgam's prediction
#' surface does not reconstruct their basis at newdata, and the warning
#' names that gap and the approximate form that avoids it.
#'
#' @param formula A formula, `brmsformula` or `mvbrmsformula`
#' @noRd
validate_exact_gp_usage <- function(formula) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_null(formula),
    .var.name = "formula"
  )
  if (is.null(formula)) return(invisible(NULL))

  forms <- if (inherits(formula, "mvbrmsformula")) formula$forms else {
    list(formula)
  }
  for (form in forms) {
    main <- obs_arm_main_formula(form)
    # `k` is read off each `gp()` call as written. Evaluating the call
    # needed every object its arguments name to exist here, and a
    # `k = kk` was refused as invalid syntax.
    for (gp_call in formula_calls(main[[length(main)]], "gp")) {
      k <- gp_call_to_spec(gp_call)$k
      if (is.na(k) || identical(k, "NA")) {
        # One mvgam() call reaches this from the observation validator,
        # the trend validator and `setup_brms_lightweight()`. The
        # notice's id carries the term, and it shows once per term
        # per session.
        maybe_warn_exact_gp(paste(deparse(gp_call), collapse = ""))
      }
    }
  }
  invisible(NULL)
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

  ensure_registry_initialized()
  detected_trends <- intersect(formula_rhs_function_names(formula),
                               mvgam_trend_registry())
  if (length(detected_trends) > 0) {
    stop(insight::format_error(c(
      "Trend constructors are not supported in the observation 'formula'.",
      x = paste0("Found: ", paste0("'", detected_trends, "()'",
                                   collapse = ", "), "."),
      i = "Write the trend in 'trend_formula', as in 'trend_formula = ~ RW()'."
    )), call. = FALSE)
  }

  # A `.` stands for every column of the frame, and an mvgam frame
  # holds its time and series columns beside the covariates.
  forms <- if (inherits(formula, "mvbrmsformula")) formula$forms else {
    list(formula)
  }
  has_dot <- vapply(forms, function(f) {
    main <- obs_arm_main_formula(f)
    "." %in% all.vars(main[[length(main)]])
  }, logical(1L))
  if (any(has_dot)) {
    stop(insight::format_error(c(
      "A '.' is not supported in the observation 'formula'.",
      x = paste0("'.' enters every column of 'data' as a predictor, ",
                 "the time and series columns among them."),
      i = "List the covariates the model should use."
    )))
  }

  # Check for exact GP usage (gp() without k parameter)
  validate_exact_gp_usage(formula)

  # Return original formula unchanged - brms handles all other validation
  return(formula)
}

# Two refusals, each written once here: a trend formula naming a
# response, and a trend formula with an offset among its terms. The
# user's entry point raises them through
# `validate_trend_formula()`, and `parse_trend_formula()` raises
# them for the callers that reach it directly.
#' @noRd
refuse_trend_formula_response <- function(formula) {
  if (length(formula) != 3L) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "A trend formula takes predictors only.",
    x = paste0("Found the response '", deparse(formula[[2L]]), "'."),
    i = "Write the response in the observation 'formula'."
  )), call. = FALSE)
}

# The caller passes the `terms` object it already built. Recomputing it
# here would drop the `data` that expanded a `.`.
#' @noRd
refuse_trend_formula_offset <- function(terms_obj) {
  if (is.null(attr(terms_obj, "offset"))) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "Offsets apply to the observation model.",
    x = "Found an offset term in 'trend_formula'.",
    i = "Write it as 'formula = y ~ x + offset(log_exposure)'."
  )), call. = FALSE)
}

#' Validate a trend formula
#'
#' Refuses a response, an offset, brms autocorrelation and addition
#' terms, more than one trend constructor, and an exact `gp()`. The
#' checks walk the formula's calls. A variable such as `se_x` or
#' `weights` is no call, and none of them refuses it.
#'
#' @param formula A one-sided trend formula, or `NULL`
#' @return The formula, unchanged
#' @noRd
validate_trend_formula <- function(formula) {
  if (is.null(formula)) return(NULL)
  checkmate::assert_class(formula, "formula")
  refuse_trend_formula_response(formula)
  refuse_trend_formula_offset(stats::terms(formula))

  rhs <- formula[[length(formula)]]
  found <- function(calls) {
    paste0("Found: ", paste0("'", calls, "'", collapse = ", "), ".")
  }

  autocor <- find_calls(rhs, brms_autocor_terms)
  if (length(autocor) > 0L) {
    stop(insight::format_error(c(
      "brms autocorrelation terms are not supported in 'trend_formula'.",
      x = found(autocor),
      x = "Autocorrelation terms conflict with the trend dynamics.",
      i = "Use a trend constructor, as in 'AR(p = 1)' for 'ar(p = 1)'."
    )), call. = FALSE)
  }

  # `mi()` is absent: missing-predictor imputation is allowed on the
  # latent scale.
  addition <- find_calls(rhs, brms_addition_terms)
  if (length(addition) > 0L) {
    stop(insight::format_error(c(
      "brms addition terms are not supported in 'trend_formula'.",
      x = found(addition),
      x = "Addition terms change the observation model.",
      i = "Write them in the observation 'formula'."
    )), call. = FALSE)
  }

  ensure_registry_initialized()
  constructors <- find_calls(rhs, mvgam_trend_registry())
  if (length(constructors) > 1L) {
    stop(insight::format_error(c(
      "Multiple trend constructors found in 'trend_formula'.",
      x = found(constructors),
      x = "Every response of a model shares one latent trend.",
      i = "Keep one constructor, as in 'trend_formula = ~ AR()'."
    )), call. = FALSE)
  }

  validate_exact_gp_usage(formula)
  formula
}

# brms autocorrelation terms, which model temporal dependence the
# trend already models
brms_autocor_terms <- c("ar", "ma", "arma", "cosy", "unstr", "autocor")

# brms `formula_ad` specials that modify the observation model and
# have no meaning on a latent trend
brms_addition_terms <- c(
  "weights", "se", "cens", "trunc", "trials", "rate", "vreal", "vint",
  "subset", "index", "dec", "cat", "thres", "cov_ranef"
)

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
#' @param response_vars Response columns named by response key, from
#'   `response_columns()`
#' @return List with time series dimensions and optional added metadata
#' @noRd
extract_time_series_dimensions <- function(data, time_var = "time",
                                           series_var = "series",
                                           trend_type = NULL,
                                           trend_specs = NULL,
                                           response_vars = NULL) {

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


  # Calculate core dimensions from data using attribute-based accessors
  time_vals <- get_time_for_grouping(data)
  series_vals <- get_series_for_grouping(data)
  # `ensure_mvgam_variables()` records how the series were named
  # with the series themselves. A response-keyed or hierarchical
  # axis taken as explicit is placed on the wrong rows after the fit.
  series_source <- attr(data, "mvgam_series_source")
  checkmate::assert_choice(
    series_source, c("explicit", "hierarchical", "multivariate"),
    .var.name = "attr(data, 'mvgam_series_source')"
  )

  # A response-keyed frame states its axis rather than implying it
  # through the row values, and states it in formula order, so it is
  # taken as given below rather than sorted back to alphabetical.
  # One axis, held once and in one order: the responses as the formula
  # names them where the frame is response-keyed, otherwise the
  # frame's own series sorted. Keeping a sorted and an unsorted
  # spelling of the same axis is what let two readers disagree.
  response_axis <- mvgam_response_axis(data)
  series_axis <- response_axis %||% series_axis_values(series_vals)

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

  sorted_unique_times <- time_axis_values(time_vals)

  dimensions <- list(
    n_time = length(sorted_unique_times),   # Number of unique time points
    n_series = length(series_axis),         # Number of series
    n_obs = nrow(data),                    # Total observations
    time_range = c(min_time, max_time),    # Time range
    time_var = time_var,                   # Variable names for downstream use
    series_var = series_var,
    unique_times = sorted_unique_times,    # Sorted unique time points
    unique_series = series_axis,           # The series axis, in order
    series_groups = series_groups,         # Their groups, same order
    # Each row's cell on the two axes. A response-keyed frame gives
    # every row every response, and the response a caller maps
    # decides the series.
    row_time = match(time_vals, sorted_unique_times),
    row_series = if (is.null(response_axis)) {
      match(series_vals, series_axis)
    }
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
    time_axis_values(original_time)
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
      source = series_source,
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
      n = length(sorted_unique_times),
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
    # until the trend formula has been walked. It is left unset here
    # and named by `extract_and_validate_trend_components()`. A default
    # of "series" would give an early reader a wrong value it would
    # believe.
    grain = NULL,
    # The columns that identify a row. Prediction places a frame the
    # model has never seen on these axes by them.
    vars = list(
      time_var = time_var,
      series_var = series_var,
      gr_var = groupings$gr,
      subgr_var = groupings$subgr
    ),
    # The levels each grouping column held in training, in the order
    # Stan numbers the groups. Prediction refuses a frame naming a
    # group the model never saw.
    group_levels = lapply(
      list(gr = groupings$gr, subgr = groupings$subgr),
      function(column) {
        if (named_var(column) && column %in% names(data)) {
          observed_levels(data[[column]])
        }
      }
    )
  )

  # Generate observation-to-trend mappings if response variables provided
  # This centralizes mapping generation with dimension calculation for consistency
  if (!is.null(response_vars)) {
    dimensions$mappings <- list()

    # Keyed by the name brms suffixes each response's data with, and
    # read from the response's own column.
    for (key in names(response_vars)) {
      column <- response_vars[[key]]
      dimensions$mappings[[key]] <- generate_obs_trend_mapping(
        data = data,
        response_var = column,
        response_key = key,
        time_var = time_var,
        series_var = series_var,
        dimensions = dimensions
      )
    }
  }

  # Gate on the trend's own rule rather than a hardcoded
  # `!= "CAR"` predicate; see the matching call upstream.
  if (trend_requires_regular_intervals(trend_specs)) {
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
#' @param response_var Name of response column to check for missing values
#' @param response_key The response's key, as `response_columns()` names
#'   it, which is what a response-keyed series axis holds
#' @param time_var Name of time variable
#' @param series_var Name of series variable
#' @param dimensions List from extract_time_series_dimensions with time series structure
#' @return List containing obs_trend_time and obs_trend_series arrays for Stan
#' @noRd
generate_obs_trend_mapping <- function(data, response_var,
                                       response_key = response_var,
                                       time_var = "time",
                                       series_var = "series",
                                       dimensions) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(response_var)
  checkmate::assert_string(response_key)
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
  scalar_attrs <- c("mvgam_series_source", "mvgam_series_levels")
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
    rep(response_series_index(response_axis, response_key),
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

  missing_fields <- setdiff(c("trend", "time", "series"), names(trend_obj))
  if (length(missing_fields) > 0) {
    stop_missing_fields("The trend object", missing_fields)
  }

  invisible(TRUE)
}

#' Evaluate an expression without printing output or messages
#'
#' With `type = "message"` the messages are held and replayed only when
#' the call fails. A compiler reports what went wrong in its messages
#' and then raises an error that points at them, and those messages are
#' what a user needs from a failed compile. The call runs once.
#'
#' @param expr Expression to evaluate.
#' @param type Which stream to suppress: `"output"` or `"message"`.
#' @param silent Suppress at all?
#' @param ... Passed to `utils::capture.output()` for `type = "output"`.
#' @noRd
eval_silent <- function(expr, type = "output", silent = TRUE, ...) {
  silent <- as_one_logical(silent)
  type <- match.arg(type, c("output", "message"))
  expr <- substitute(expr)
  envir <- parent.frame()
  if (!silent) {
    return(eval(expr, envir))
  }
  if (identical(type, "message")) {
    held <- character(0L)
    return(withCallingHandlers(
      eval(expr, envir),
      message = function(m) {
        held <<- c(held, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      error = function(e) {
        if (length(held)) rlang::inform(paste(held, collapse = ""))
      }
    ))
  }
  utils::capture.output(out <- eval(expr, envir), type = type, ...)
  out
}

#' Prepare parsed trend specs for the model build
#'
#' `mvgam()`, `stancode()`, `standata()` and `get_prior()` all build
#' from specs prepared here. A top-level `trend_map` joins the spec
#' and becomes the fixed loadings. A factor request meets the
#' registry's record of the trend. The loadings prior attaches last.
#'
#' @param trend_specs Parsed trend specs, or `NULL` for no trend.
#' @param trend_map The top-level `trend_map` argument, or `NULL`.
#' @param loadings_prior The `loadings_prior` argument, or `NULL`.
#' @param data,data2 The training data and `data2`.
#' @return The prepared specs.
#' @noRd
prepare_trend_specs <- function(trend_specs, trend_map, loadings_prior,
                                data, data2) {
  specs <- apply_trend_map_alias(trend_specs, trend_map)
  specs <- normalise_trend_map_on_specs(specs, data)
  enforce_factor_support_against_specs(specs)
  attach_loadings_prior_spec(specs, loadings_prior, data, data2)
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
#'   top-level alias was supplied. A mapping supplied with no trend
#'   spec to hold it is refused here, the last point that still sees
#'   it.
#'
#' @noRd
apply_trend_map_alias <- function(trend_specs, mvgam_trend_map) {
  if (is.null(mvgam_trend_map)) {
    return(trend_specs)
  }
  if (is.null(trend_specs)) {
    stop(insight::format_error(
      "Argument 'trend_map' requires a 'trend_formula'."
    ), call. = FALSE)
  }
  if (!is.null(trend_spec_head(trend_specs)$trend_map)) {
    stop(insight::format_error(c(
      "'trend_map' is given both to 'mvgam()' and to the trend constructor.",
      i = "Supply 'trend_map' in one place."
    )), call. = FALSE)
  }
  map_trend_specs(trend_specs, function(spec) {
    spec$trend_map <- mvgam_trend_map
    spec
  })
}


# Normalise the user's `loadings_prior` and attach it to the trend
# spec. `mvgam()` and `get_prior()` both call this. A structured
# prior cannot coexist with a partial or fully fixed Z (see
# `assert_loadings_prior_compatible()`).
#'@noRd
attach_loadings_prior_spec <- function(trend_specs, loadings_prior,
                                       data, data2) {
  if (is.null(loadings_prior)) return(trend_specs)
  if (is.null(trend_specs)) {
    stop(insight::format_error(c(
      "Argument 'loadings_prior' requires a 'trend_formula'.",
      x = "The prior applies to the loadings of a factor trend."
    )), call. = FALSE)
  }
  if (is.null(trend_spec_head(trend_specs)$n_lv)) {
    stop(insight::format_error(c(
      "Argument 'loadings_prior' requires a factor trend.",
      x = "The loadings belong to the latent factors that 'n_lv' sets.",
      i = "Set 'n_lv' on the trend constructor, as in 'AR(n_lv = 2)'."
    )), call. = FALSE)
  }
  spec <- normalise_loadings_prior(
    loadings_prior, data2 = data2, data = data,
    series_var = spec_axis_vars(trend_specs)$series_var
  )
  head <- trend_spec_head(trend_specs)
  assert_loadings_prior_compatible(spec, head$fixed_Z)
  assert_column_shrinkage_compatible(spec, head$trend)
  map_trend_specs(trend_specs, function(trend_spec) {
    trend_spec$loadings_prior_spec <- spec
    trend_spec
  })
}


#' Normalise a raw `trend_map` to a fixed-Z matrix and reconcile
#' it with `n_lv`.
#'
#' Called once in the Stan-code pipeline right after
#' `apply_trend_map_alias()`. `normalise_trend_map()` converts the
#' spec's `trend_map` to the numeric Z stored on `spec$fixed_Z`, and
#' `spec$n_lv` takes `ncol(Z)`. An `n_lv` also set on the
#' constructor must agree.
#'
#' @param trend_specs One trend spec or a per-response list of them.
#' @param data Training `data.frame`, whose series column gives the
#'   rows of Z.
#'
#' @return `trend_specs` with `$fixed_Z` and `$n_lv` set when a
#'   `trend_map` was supplied.
#'
#' @noRd
normalise_trend_map_on_specs <- function(trend_specs, data) {
  spec <- trend_spec_head(trend_specs)
  if (is.null(spec$trend_map)) return(trend_specs)
  if (named_var(spec_groupings(spec)$gr)) {
    refuse_factor_hierarchical(spec$trend)
  }
  normalised <- normalise_trend_map(
    spec$trend_map, data, spec_axis_vars(spec)$series_var
  )
  if (!is.null(spec$n_lv) && spec$n_lv != normalised$n_lv) {
    stop(insight::format_error(c(
      "'trend_map' and 'n_lv' give different numbers of factors.",
      x = paste0("'trend_map' has ", normalised$n_lv,
                 " columns and 'n_lv' is ", spec$n_lv, "."),
      i = "Drop 'n_lv' to let the columns of 'trend_map' set it."
    )), call. = FALSE)
  }
  # An all-NA mask frees every loading, which is the default factor
  # model. It keeps `n_lv` and samples a free Z that a loadings prior
  # can act on.
  fixed_Z <- if (all(is.na(normalised$Z))) NULL else normalised$Z
  map_trend_specs(trend_specs, function(trend_spec) {
    trend_spec$fixed_Z <- fixed_Z
    trend_spec$n_lv <- normalised$n_lv
    trend_spec
  })
}


# Internal: tell the user once when a `series` column they supplied is
# superseded by the one `gr` and `subgr` imply.
#
# `gr` and `subgr` together identify a series. mvgam builds the series
# from them and ignores the `series` column the data carried. A user
# whose column disagreed would believe the model grouped their way, and
# every post-fit label uses mvgam's names for the series.
#'@noRd
warn_series_superseded <- function(data, series_var, series_values,
                                   gr_var, subgr_var) {
  if (is.null(series_var) || !series_var %in% names(data)) {
    return(invisible(NULL))
  }
  supplied <- as.character(data[[series_var]])
  derived <- as.character(series_values)
  if (identical(supplied, derived)) {
    return(invisible(NULL))
  }
  warn_once(
    c(
      paste0(
        "The '", series_var, "' column was replaced by the series that '",
        gr_var, "' and '", subgr_var, "' define."
      ),
      x = paste0(
        "Supplied: '", supplied[1L], "'. Used: '", derived[1L], "'."
      ),
      i = "A hierarchical trend names each series by its grouping columns.",
      i = paste0(
        "Every post-fit summary, plot and forecast labels this series ",
        "'", derived[1L], "'."
      )
    ),
    "mvgam_series_superseded"
  )
  invisible(NULL)
}

# Internal: refuse a time column with missing values. A row without a
# time maps to no occasion of the trend, and its trend index stays
# `NA` through to the linear predictor.
#'@noRd
refuse_missing_times <- function(values, time_var) {
  rows <- which(is.na(values))
  if (!length(rows)) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    paste0("Time variable '", time_var, "' has missing values."),
    x = paste0("'", time_var, "' is NA on ", count_rows(rows),
               ", first at row ", rows[1L], "."),
    i = "Drop these rows or supply their times."
  )), call. = FALSE)
}


# Internal: "1 row" or "n rows".
#'@noRd
count_rows <- function(rows) {
  paste0(length(rows), if (length(rows) == 1L) " row" else " rows")
}


# Internal: TRUE when a variable name points at a column. A trend
# constructor spells an absent grouping NA and the normalised spec
# spells it "NA".
#'@noRd
named_var <- function(var) {
  !is.null(var) && length(var) == 1L && !is.na(var) &&
    nzchar(var) && !identical(var, "NA")
}


# Internal: TRUE when a variable names a column the data carries.
#'@noRd
usable_var <- function(var, data) {
  named_var(var) && var %in% names(data)
}


# Internal: require the grouping columns a hierarchical trend needs.
#
# Reason: `checkmate::assert_names()` reports the missing names as an
# internal assertion and gives no reason for them. This message names
# the role of the columns. Training and prediction frames share it.
#'@noRd
assert_grouping_columns <- function(data, gr_var, subgr_var = NULL) {
  wanted <- unlist(Filter(named_var, list(gr_var, subgr_var)))
  missing <- setdiff(wanted, names(data))
  if (length(missing)) {
    stop(insight::format_error(c(
      "Grouping columns of the trend are missing from the data.",
      x = paste0("Absent: ", paste0("'", missing, "'", collapse = ", "),
                 "."),
      i = paste0("A hierarchical trend identifies each series by its ",
                 "'gr' and 'subgr' columns.")
    )), call. = FALSE)
  }
  # A row without its group belongs to no series.
  gaps <- wanted[vapply(wanted, function(v) anyNA(data[[v]]), logical(1L))]
  if (!length(gaps)) {
    return(invisible(TRUE))
  }
  lines <- vapply(gaps, function(v) {
    paste0("'", v, "': ", sum(is.na(data[[v]])), " NA, first at row ",
           which(is.na(data[[v]]))[1L], ".")
  }, character(1L))
  stop(insight::format_error(c(
    "Grouping columns of the trend hold missing values.",
    stats::setNames(lines, rep("x", length(lines))),
    i = "Drop these rows or supply their groups."
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


# Internal: check the column that places a frame's rows.
#
# A trend model indexes every row by one occasion and one series. mvgam
# takes them from the `time` and `series` columns, or from the columns
# the trend constructor names, and checks them here before using
# either. mvgam requires the series column even for a single series,
# which the column records by holding one level. Training data need a
# factor series column, whose levels fix the order of the series in
# the model and every result. Prediction data take the fitted order,
# and a character column there is accepted. Time must be numeric. The
# trend orders and spaces its steps by the value of each time.
#' @noRd
assert_axis_column <- function(data, column, axis = c("time", "series"),
                               require_factor = TRUE) {
  checkmate::assert_string(column)
  checkmate::assert_flag(require_factor)
  axis <- match.arg(axis)
  if (column %in% names(data)) {
    if (axis == "series" && require_factor &&
        !is.factor(data[[column]])) {
      stop(insight::format_error(c(
        paste0("Column '", column, "' must be a factor."),
        x = paste0("Got class '", class(data[[column]])[1L], "'."),
        i = "A factor's levels fix the order of the series.",
        i = paste0("Convert it with data$", column, " <- factor(data$",
                   column, ").")
      )), call. = FALSE)
    }
    if (axis == "time" && !is.numeric(data[[column]])) {
      stop(insight::format_error(c(
        paste0("Column '", column, "' must be numeric."),
        x = paste0("Got class '", class(data[[column]])[1L], "'."),
        i = "mvgam orders and spaces the times by their values.",
        i = paste0(
          "Convert it with data$", column, " <- ",
          if (is.factor(data[[column]])) {
            paste0("as.numeric(as.character(data$", column, ")).")
          } else {
            paste0("as.numeric(data$", column, ").")
          }
        )
      )), call. = FALSE)
    }
    return(invisible(TRUE))
  }
  place <-c(time = "at one occasion", series = "on one series")[[axis]]
  hints <- c(
    i = paste0("A latent trend places each row ", place, "."),
    i = paste0(
      "Add '", column, "' to the data or name its column with the ",
      "trend constructor's '", axis, "' argument."
    ),
    if (axis == "series") {
      c(i = paste0("For a single time series: data$", column,
                   " <- factor(\"series_1\")"))
    }
  )
  stop(insight::format_error(c(
    paste0("Column '", column, "' is absent from the data."),
    x = paste0(
      "Supplied: ", paste0("'", names(data), "'", collapse = ", "), "."
    ),
    hints
  )), call. = FALSE)
}


# Internal: refuse a prediction frame with gaps in the model's columns.
#
# A prediction uses the columns the formulas name, the groupings, the
# aterms, the offset and the axis. A gap in any of them reaches the
# linear predictor unchanged: a numeric column contributes `NA` cells,
# and a factor column takes its reference level with nothing to mark
# it. The check excludes the response. A missing response marks the
# occasions a forecast frame asks mvgam to predict.
#' @noRd
validate_newdata_complete <- function(newdata, object) {
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_class(object, "mvgam")
  terms <- mvgam_term_list(object)
  vars <- axis_vars(object)
  axes <- mvgam_axes(object)
  # The term list keeps a meta variable only where it varies in the
  # training data, and a fit on one occasion drops its time column
  # from that list. The axis names the column whatever the training
  # values did. The time comes from there.
  time_var <- if (named_var(vars$time_var)) {
    vars$time_var
  } else {
    character(0L)
  }
  read <- setdiff(
    unique(c(terms$conditional, terms$random, terms$aterms,
             terms$offset, terms$index, time_var)),
    terms$response
  )
  # A column the frame omits altogether is the widest gap of all, and
  # narrowing to the columns present would pass it through. The
  # series column is held out of that test. The axis record places a
  # row the frame never keyed, and a grouping names the series with
  # no such column present. The time column is held to the test: an
  # occasion has no substitute, and a frame omitting it was given
  # predictions at whatever position the observation structure fell
  # back to.
  exempt <- setdiff(terms$index %||% character(0L), vars$time_var)
  # Where the column itself names several series and no grouping can
  # derive them, a row's series comes from that column alone. Every
  # row of a single-series fit belongs to the same series, and the
  # column adds nothing there.
  if (identical(axes$series$source, "explicit") &&
        length(axes$series$levels) > 1L &&
        !(named_var(vars$gr_var) && named_var(vars$subgr_var))) {
    exempt <- setdiff(exempt, vars$series_var)
  }
  absent <- setdiff(setdiff(read, exempt), names(newdata))
  present <- intersect(read, names(newdata))
  refuse_column_gaps(absent, na_rows(newdata, present), "newdata")
}


#' Refuse levels absent from the training data
#'
#' @param label What the levels belong to, e.g. "Series"
#' @param seen The levels in the new data
#' @param fitted The training levels
#' @return `TRUE`, invisibly
#' @noRd
refuse_unseen_levels <- function(label, seen, fitted) {
  invalid <- setdiff(seen, fitted)
  if (length(invalid) > 0L) {
    stop(insight::format_error(c(
      paste0(label, " in 'newdata' has levels absent from the ",
             "training data."),
      x = cli::format_inline("Unseen: {.val {invalid}}."),
      i = cli::format_inline("Fitted levels: {.val {fitted}}.")
    )), call. = FALSE)
  }
  invisible(TRUE)
}


#' Refuse a frame with a series or group the model never saw
#'
#' @param data Frame to predict on
#' @param metadata A fit's `trend_metadata`, or `NULL` for a model
#'   whose frame names no axis
#' @return `TRUE`, invisibly
#' @noRd
validate_prediction_factor_levels <- function(data, metadata) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(metadata, names = "named", null.ok = TRUE)
  axes <- metadata$axes
  if (is.null(axes)) {
    return(invisible(TRUE))
  }
  vars <- axes$vars

  # A hierarchical trend derives its series from `gr` and `subgr`, and
  # the model ignored any `series` column. Deriving the value here also
  # catches a pair the training data never held, though each level
  # alone is known. Every check compares only the levels the frame
  # holds rows for. Subsetting a frame keeps every declared factor
  # level, and a level with no rows names nothing unseen.
  hierarchical <- identical(axes$series$source, "hierarchical")
  seen <- if (hierarchical) {
    assert_grouping_columns(data, vars$gr_var, vars$subgr_var)
    levels(droplevels(
      hierarchical_series_values(data, vars$gr_var, vars$subgr_var)
    ))
  } else if (vars$series_var %in% names(data)) {
    observed_levels(data[[vars$series_var]])
  }
  refuse_unseen_levels("Series", seen, axes$series$levels)

  for (role in c("gr", "subgr")) {
    column <- vars[[paste0(role, "_var")]]
    fitted <- axes$group_levels[[role]]
    if (!is.null(fitted) && column %in% names(data)) {
      refuse_unseen_levels(paste0("Column '", column, "'"),
                           observed_levels(data[[column]]), fitted)
    }
  }
  invisible(TRUE)
}

#' Parse a Stan program with the backend's own parser
#'
#' The parse is the one `parse_model()` runs before a fit, so a program
#' that passes here passes there.
#'
#' @param stan_code The Stan program as one string.
#' @param backend `"rstan"` or `"cmdstanr"`.
#' @param silent Suppress the parser's messages unless it fails.
#' @param ... Passed to the backend's parser.
#' @return `invisible(TRUE)`; the parser's error when the code is invalid.
#' @noRd
validate_stan_code <- function(stan_code, backend = "rstan", silent = TRUE,
                               ...) {
  checkmate::assert_string(stan_code, min.chars = 1L)
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  parse_model(stan_code, backend = backend, silent = silent, ...)
  invisible(TRUE)
}


#'@noRd
as_one_logical <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop(insight::format_error(
      paste0("'", s, "' must be a single logical value.")
    ))
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

  if (use_grouping && is_factor_model) {
    refuse_factor_hierarchical(trend_name)
  }

  return(invisible(TRUE))
}


# Internal: refuse a trend that is both grouped and a factor model.
#
# A grouping and the loadings each define the series axis. Every site
# that finds the pair raises this one refusal.
#'@noRd
refuse_factor_hierarchical <- function(trend_name) {
  stop(insight::format_error(c(
    paste0("Factor models are not supported for hierarchical ",
           trend_name, " trends."),
    x = "A grouping and the loadings both define the series axis.",
    i = "Remove 'gr' and 'subgr', or drop 'n_lv' and 'trend_map'."
  )), call. = FALSE)
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
  if (rlang::is_call(expr, "+") || rlang::is_call(expr, "-")) {
    op <- rlang::call_name(expr)
    args <- rlang::call_args(expr)

    # A unary operator keeps its sign on the processed operand
    if (length(args) == 1) {
      arg <- remove_trend_expressions(args[[1]], trend_patterns, depth + 1)
      return(if (is.null(arg)) NULL else rlang::call2(op, arg))
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
#' @param response_vars Response columns named by response key, from
#'   `response_columns()`, for multivariate series creation
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
  # A fit's metadata marks the prediction context, where a frame
  # naming an unseen series or group is refused.
  validate_prediction_factor_levels(data, metadata)

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
  assert_axis_column(data, time_var, "time")
  unique_times <- time_axis_values(data[[time_var]])
  time_mapping <- setNames(seq_along(unique_times), unique_times)
  attr(data, "mvgam_time") <- time_mapping[as.character(data[[time_var]])]

  # Every row has to name a cell of the trend matrix. A missing time
  # or series maps to an NA index, which stays an NA index: the
  # trend is read at `trend[NA, s]`, the observation takes a missing
  # linear predictor, and nothing between here and the answer says
  # so. Refusing here is the one layer that owns the question,
  # because this is where the two indices are built.
  refuse_missing_times(data[[time_var]], time_var)
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

  # Strategy 1: in the prediction context the series are rebuilt the
  # way the fit built them. An explicit axis falls through to the
  # series column.
  stored <- metadata$axes
  if (identical(stored$series$source, "hierarchical")) {
    gr_var <- stored$vars$gr_var
    subgr_var <- stored$vars$subgr_var
    assert_grouping_columns(data, gr_var, subgr_var)
    series_values <- hierarchical_series_values(data, gr_var, subgr_var)
    series_source <- "hierarchical"
  } else if (identical(stored$series$source, "multivariate")) {
    # The response axis the fit recorded, keyed as brms keys the
    # responses.
    result <- create_multivariate_series(stored$series$levels, nrow(data))
    series_values <- result$series_values
    series_levels <- result$series_levels
    series_source <- result$series_source
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

  # Strategy 3: the responses of a wide multivariate frame are its
  # series, unless the frame names them in a series column.
  if (is.null(series_values) && !series_var %in% names(data) &&
        length(response_vars) > 1) {
    result <- create_multivariate_series(names(response_vars), nrow(data))
    series_values <- result$series_values
    series_levels <- result$series_levels
    series_source <- result$series_source
  }

  # Strategy 4: the series column
  if (is.null(series_values)) {
    assert_axis_column(data, series_var, "series",
                       require_factor = is.null(metadata))
    series_values <- data[[series_var]]
    series_source <- "explicit"
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
        "The series is NA on ", count_rows(missing_series),
        ", first at row ", missing_series[1L], "."
      ),
      i = paste0("Fill the missing values in '", series_var,
                 "' and any grouping variables.")
    )), call. = FALSE)
  }

  return(data)
}

#' Whether a frame can name the axes a record needs
#'
#' `ensure_mvgam_variables()` accepts a frame carrying its time column
#' with no missing values, naming its series either with a column or
#' with several responses. It refuses anything else, and a trendless
#' model reaches post-fit today with no record at all. This names the
#' set where building one succeeds, so a frame that works now keeps
#' working.
#'
#' @param data Data frame the model was given
#' @param time_var Time column name
#' @param series_var Series column name
#' @param response_vars Response columns named by key
#' @return A single logical
#' @noRd
frame_names_an_axis <- function(data, time_var, series_var,
                                response_vars = NULL) {
  if (!time_var %in% names(data) || anyNA(data[[time_var]])) {
    return(FALSE)
  }
  if (series_var %in% names(data)) {
    return(!anyNA(data[[series_var]]))
  }
  length(response_vars) > 1L
}

#' The axis record a model without a trend carries
#'
#' The trend branch builds this record while resolving the trend's own
#' dimensions. A model written without a `trend_formula` skips that
#' branch, and every post-fit surface then derives an axis of its own:
#' a padded series takes the date of its last row instead of its last
#' observation, and a wide frame loses the one field naming its
#' responses as the series. `trend_type` stays unset, which marks a
#' model stepping no latent state.
#'
#' @param data Data frame the model was given
#' @param time_var Time column name
#' @param series_var Series column name
#' @param response_vars Response columns named by key
#' @return A metadata list, or `NULL` where the frame names no axis
#' @noRd
trendless_trend_metadata <- function(data, time_var, series_var,
                                     response_vars = NULL) {
  if (!frame_names_an_axis(data, time_var, series_var, response_vars)) {
    return(NULL)
  }
  prepared <- ensure_mvgam_variables(
    data = data, parsed_trend = NULL, time_var = time_var,
    series_var = series_var, response_vars = response_vars
  )
  axis_record(extract_time_series_dimensions(
    prepared, time_var, series_var, response_vars = response_vars
  ))
}


#' The record of a model's axes that a fit stores
#'
#' Post-fit methods take the axes Stan was given from this record, and
#' a second construction from the frame is the thing it exists to end.
#' `enrich_trend_metadata()` adds the trend's own fields to it.
#'
#' @param dimensions The dimensions `extract_time_series_dimensions()`
#'   built from the prepared frame.
#' @param covariates The trend formula's covariates, which prediction
#'   carries onto the factor grid of a `by = lv_axis()` term.
#' @param has_by_lv,had_by_lv,n_lv_for_grain The `by = lv_axis()`
#'   grain, which prediction rebuilds newdata on. `had_by_lv` is its
#'   display-only twin, used by `conditional_effects.mvgam` through
#'   `mvgam_had_by_lv()`.
#' @return A metadata list
#' @noRd
axis_record <- function(dimensions, covariates = character(0),
                        has_by_lv = FALSE, had_by_lv = FALSE,
                        n_lv_for_grain = NULL) {
  list(
    covariates = covariates,
    has_by_lv = has_by_lv,
    had_by_lv = had_by_lv,
    n_lv_for_grain = n_lv_for_grain,
    axes = complete_axes_grain(dimensions$axes, has_by_lv, had_by_lv)
  )
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
    stop_mvgam_fault(
      paste0("The data frame lacks its ", what, " index."),
      "mvgam builds the index when it prepares a frame for a model."
    )
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

#' Get series variable for grouping operations
#'
#' @param data Data frame with mvgam series attributes
#' @return Factor or character vector of series identifiers
#' @noRd
get_series_for_grouping <- function(data) {
  mvgam_prepared_index(data, "series")
}

#' The levels a column observes, in the order they are declared
#'
#' A factor keeps levels the data never uses. Those levels have no
#' rows, no latent state and no group. Dropping them leaves the labels
#' a character column holds, in the order the factor declares. The
#' series axis and the grouping columns both take their levels here.
#'
#' A frame whose responses are its series takes its axis from
#' `mvgam_response_axis()`. Its per-row values are one constant and
#' the axis is the level set.
#'
#' @param values Column values, factor or otherwise
#' @return Character vector of observed labels
#' @noRd
observed_levels <- function(values) {
  if (is.factor(values)) {
    levs <- levels(values)
    return(levs[levs %in% as.character(values)])
  }
  sort(unique(as.character(values)))
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
#' Asked while the model is built, of the axis being built, and after
#' fitting, of the axis the fit records, so both read one answer.
#'
#' @param axis A response-keyed series axis, holding response keys
#' @param response_key The key of the response being asked about
#' @return Integer index into the trend matrix's series dimension
#' @noRd
response_series_index <- function(axis, response_key) {
  checkmate::assert_character(axis, min.len = 1, any.missing = FALSE,
                              unique = TRUE)
  checkmate::assert_string(response_key)
  idx <- match(response_key, axis)
  if (is.na(idx)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "Response {.field {response_key}} is not on the series axis."
      ),
      i = cli::format_inline("The axis has {.val {axis}}.")
    )), call. = FALSE)
  }
  idx
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
#' records the dimensions on the specification. Stan assembly and
#' every post-fit method take their axes from this one pass.
#'
#' @param data Data frame containing time series data
#' @param mv_spec Multivariate specification object with base_formula and
#'   trend_specs
#' @param response_vars Character vector of response variable names
#' @param family The observation family, or NULL
#' @return List with trend_data, enhanced_mv_spec and metadata
#' @noRd
extract_and_validate_trend_components <- function(data, mv_spec,
                                                  response_vars,
                                                  family = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(mv_spec)
  missing_fields <- setdiff(
    c("base_formula", "trend_specs", "has_trends", "regular_terms"),
    names(mv_spec)
  )
  if (length(missing_fields) > 0L) {
    stop_missing_fields("The parsed model specification", missing_fields)
  }
  checkmate::assert_character(response_vars, min.len = 1, null.ok = TRUE)
  if (!isTRUE(mv_spec$has_trends)) {
    stop_mvgam_fault(
      "Trend components were requested for a trendless model.",
      "The parsed specification has 'has_trends = FALSE'."
    )
  }

  parsed_trend <- trend_spec_head(mv_spec$trend_specs)
  axis_names <- spec_axis_vars(parsed_trend)
  time_var <- axis_names$time_var
  series_var <- axis_names$series_var

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
  # `trend_cell_frame()` and the stanvar emission use has_by_lv and
  # n_lv_for_grain on the factor-model path alone.
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
        n_lv_for_grain <- spec_n_lv(parsed_trend)
      }
      # Non-factor path: has_by_lv stays FALSE; the formula was already
      # rewritten to use `by = series`. The standard (time, series)
      # codepath handles everything downstream, including conditional
      # effects via brms native predict.
    }
  }

  parsed_trend <- resolve_trend_groupings(parsed_trend, data, series_var)

  data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var,
                                response_vars)

  # Compute dimensions once to eliminate redundant calls
  dimensions <- extract_time_series_dimensions(
    data,
    time_var,
    series_var,
    trend_specs = mv_spec$trend_specs,
    response_vars = response_vars
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

  # The `n_lv` ceiling, on the series axis just resolved. It also
  # holds `by = lv_axis()` to at most one factor per series.
  enforce_n_lv_ceiling_against_data(
    mv_spec$trend_specs, dimensions$n_series, family = family
  )
  if (!is.null(family)) {
    warn_zmvn_single_series(mv_spec, family, dimensions$n_series)
  }

  # A CAR trend with covariates is refused on more than one series.
  # The count is the resolved series axis, which also holds series
  # that a grouping or the responses define with no series column.
  if (identical(parsed_trend$trend, "CAR") &&
      length(mv_spec$regular_terms) > 0L && dimensions$n_series > 1L) {
    stop(insight::format_error(c(
      "'CAR()' takes 'trend_formula' covariates on a single series only.",
      x = paste0("The trend has ", dimensions$n_series, " series."),
      i = "Move the covariates to the observation formula."
    )), call. = FALSE)
  }

  # The trend formula's covariates. A response may not be one, and
  # each has to be constant within a trend cell. A CAR trend shares no
  # latent state across rows, and its covariates may vary.
  covariates <- trend_formula_covariates(mv_spec$base_formula)
  offending_vars <- intersect(covariates, response_vars)
  if (length(offending_vars) > 0) {
    stop(insight::format_error(c(
      "Responses are not supported as trend covariates.",
      x = paste0("Found in 'trend_formula': ",
                 paste0("'", offending_vars, "'", collapse = ", "), "."),
      i = "Move the term to the observation formula."
    )), call. = FALSE)
  }
  time_vals <- get_time_for_grouping(data)
  series_vals <- get_series_for_grouping(data)
  if (length(covariates) > 0 && !identical(parsed_trend$trend, "CAR")) {
    assert_trend_covariates_constant(data, covariates, parsed_trend,
                                     time_vals, series_vals)
  }

  if (!has_by_lv) {
    refuse_ragged_trend_grid(time_vals, series_vals, parsed_trend$trend)
  }
  trend_data <- trend_cell_frame(data, covariates, time_vals, series_vals,
                                 has_by_lv, n_lv_for_grain)
  trend_metadata <- axis_record(
    dimensions, covariates = covariates, has_by_lv = has_by_lv,
    had_by_lv = had_by_lv, n_lv_for_grain = n_lv_for_grain
  )

  enhanced_mv_spec <- mv_spec
  enhanced_mv_spec$trend_specs <- map_trend_specs(
    mv_spec$trend_specs, function(spec) {
      spec$dimensions <- dimensions
      spec
    }
  )

  list(
    trend_data = trend_data,
    enhanced_mv_spec = enhanced_mv_spec,
    metadata = trend_metadata
  )
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


#' The covariate columns of a trend formula
#'
#' Parses the formula `parse_trend_formula()` leaves once the trend
#' constructor is removed. A bare `all.vars()` of the full right-hand
#' side would list the columns named inside `AR(gr = region)` as
#' covariates. `newdata` must hold these columns, and the collapse to
#' trend grain selects them.
#'
#' @param base_formula The one-sided trend formula without its
#'   constructor
#' @return Character vector of column names, with the slopes and
#'   grouping factors of any random effect included.
#' @noRd
trend_formula_covariates <- function(base_formula) {
  checkmate::assert_formula(base_formula)
  # `allvars` names every column a term uses: a slope inside a random
  # effect, a grouping factor, a smooth's `by` and an offset alike.
  # The axis columns and the `by = lv_axis()` rewrite are carried
  # separately.
  bterms <- brms::brmsterms(rlang::new_formula(
    quote(.mvgam_lhs), rlang::f_rhs(base_formula)
  ))
  setdiff(trend_covariate_names(all.vars(bterms$allvars)),
          c(".mvgam_lhs", by_lv_rewrite_tokens()))
}

# Internal: the series observed at fewer times than the frame holds.
# A trend matrix and a rolling origin both need every series on one
# shared time grid.
#'@noRd
ragged_series <- function(time_vals, series_vals) {
  n_time <- length(unique(time_vals))
  per_series <- split(time_vals, series_vals, drop = TRUE)
  short <- vapply(per_series, function(t) length(unique(t)) < n_time,
                  logical(1L))
  names(per_series)[short]
}


#' Refuse series that do not share one time grid
#'
#' The trend is a matrix over every time and series, and a row of
#' `data` locates each cell. A series missing a time the others have
#' leaves a cell with no row. The refusal names the short series and
#' the remedy, and for `CAR()` where uneven spacing is allowed.
#'
#' @param time_vals,series_vals The resolved axes, one per row.
#' @param trend The trend type.
#' @return `TRUE`, invisibly.
#' @noRd
refuse_ragged_trend_grid <- function(time_vals, series_vals, trend) {
  short <- ragged_series(time_vals, series_vals)
  if (length(short) == 0L) {
    return(invisible(TRUE))
  }
  n_time <- length(unique(time_vals))
  n_series <- length(unique(series_vals))
  n_cells <- nrow(unique(data.frame(time_vals, series_vals)))
  stop(insight::format_error(c(
    "Every series in 'data' must share one time grid.",
    x = paste0("Series missing a time the others have: ",
               paste0("'", short, "'", collapse = ", "), "."),
    x = paste0(
      "Got ", n_cells, " time and series cells over ", n_time,
      " times and ", n_series, " series, expected ", n_time * n_series, "."
    ),
    i = "Give each unobserved time of a series a row with response 'NA'.",
    if (identical(trend, "CAR")) {
      c(i = paste0("'CAR()' allows uneven spacing between times, and ",
                   "every series shares those times."))
    }
  )), call. = FALSE)
}


#' Collapse a frame to one row per trend cell
#'
#' A trend cell is one occasion of one series. Under `by = lv_axis()`
#' it is one occasion of one latent factor. The covariates there are
#' time-level: `assert_trend_covariates_constant()` holds them constant
#' within each (time, series) cell, and the first value at each time
#' is joined onto every factor.
#'
#' @param data The training frame, carrying its axis attributes.
#' @param covariates The trend covariates.
#' @param time_vals,series_vals The resolved axes.
#' @param has_by_lv,n_lv_for_grain The `by = lv_axis()` grain.
#' @return A data frame sorted by time, then by series or factor.
#' @noRd
trend_cell_frame <- function(data, covariates, time_vals, series_vals,
                             has_by_lv, n_lv_for_grain) {
  columns <- trend_covariate_names(covariates)
  cells <- data %>%
    dplyr::mutate(time = time_vals, series = series_vals) %>%
    dplyr::group_by(.data$time, .data$series) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(columns), dplyr::first),
                     .groups = "drop") %>%
    dplyr::arrange(.data$time, .data$series)
  if (has_by_lv) {
    time_level <- cells %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(columns), dplyr::first),
                       .groups = "drop")
    cells <- tidyr::expand_grid(
      time = time_level$time,
      .trend = factor(seq_len(n_lv_for_grain))
    ) %>%
      dplyr::left_join(time_level, by = "time") %>%
      dplyr::arrange(.data$time, .data$.trend)
  }
  remove_mvgam_variables(cells)
}


#' Refuse trend covariates that vary inside one trend cell
#'
#' A trend cell is one occasion of one series, or of one group under a
#' grouped trend. The trend's design matrix has one row per cell.
#'
#' @param data The training frame, carrying its axis attributes.
#' @param trend_variables The trend covariates.
#' @param trend_model The trend spec.
#' @param time_vals,series_vals The resolved axes.
#' @return `TRUE`, invisibly.
#' @noRd
assert_trend_covariates_constant <- function(data, trend_variables,
                                             trend_model, time_vals,
                                             series_vals) {
  missing_vars <- setdiff(trend_variables, names(data))
  if (length(missing_vars) > 0) {
    stop(insight::format_error(c(
      "A trend covariate is absent from the data.",
      x = paste0("Missing: ",
                 paste0("'", missing_vars, "'", collapse = ", "), "."),
      i = paste0("Supplied: ",
                 paste0("'", names(data), "'", collapse = ", "), ".")
    )), call. = FALSE)
  }
  groupings <- spec_groupings(trend_model)
  cell <- if (is.null(groupings$gr)) {
    ".cell_series"
  } else {
    c(groupings$gr, groupings$subgr)
  }
  frame <- data
  frame$.cell_time <- time_vals
  frame$.cell_series <- series_vals
  cell_vars <- c(".cell_time", cell)
  # A grouping variable is constant within its own groups, and is
  # left out of the check
  checked <- setdiff(trend_variables, cell_vars)
  if (length(checked) == 0L) {
    return(invisible(TRUE))
  }
  varying <- frame %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(cell_vars))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(checked), ~ length(unique(.x)) > 1),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(checked)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), any)) %>%
    dplyr::select(dplyr::where(isTRUE)) %>%
    names()
  if (length(varying) > 0) {
    cell_label <- paste0(
      "(time, ", if (identical(cell, ".cell_series")) "series" else
        paste(cell, collapse = ", "), ")"
    )
    stop(insight::format_error(c(
      paste0("A trend covariate varies within one ", cell_label, " cell."),
      x = paste0("Varying: ",
                 paste0("'", varying, "'", collapse = ", "), "."),
      i = "The trend has one design row per cell.",
      i = "Aggregate the covariate or move it to the observation formula."
    )), call. = FALSE)
  }
  invisible(TRUE)
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
                                     n_series = NULL,
                                     series_var = "series") {
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
        "'features', 'distances' or 'column_shrinkage = \"mgp\"'."
      ),
      i = "Leave 'loadings_prior' out to use the default iid prior."
    )))
  }
  series_levels <- argument_series_levels(data, series_var)
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
      "Supply a single 'data2' lookup string, a numeric matrix ",
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
      "'data2' names, a single matrix or a named list of ",
      "matrices."
    )
  )))
}


# Consistency check on a normalised loadings-prior spec, called
# from `make_loadings_prior_stanvars()` before stanvar emission. The
# count fields must agree with the matrices they count.
#'@noRd
assert_loadings_prior_spec_consistent <- function(spec) {
  checkmate::assert_list(spec)
  missing_fields <- setdiff(
    c("features_mat", "distance_mats", "column_shrinkage", "mgp_a1",
      "mgp_a2", "n_series", "N_features_trend", "n_distances"),
    names(spec)
  )
  if (length(missing_fields) > 0L) {
    stop_missing_fields("The loadings-prior spec", missing_fields)
  }
  if (!is.null(spec$features_mat) &&
      spec$N_features_trend != ncol(spec$features_mat)) {
    stop_shape_fault("The loadings-prior feature matrix",
                     ncol(spec$features_mat), spec$N_features_trend)
  }
  if (spec$n_distances != length(spec$distance_mats)) {
    stop_shape_fault("The loadings-prior distance list",
                     length(spec$distance_mats), spec$n_distances)
  }
  # A spec with neither a kernel nor column shrinkage carries no
  # structure to emit, and the emitter's kernel branch would then
  # write `Z ~ multi_normal_cholesky(..., L_Phi_loadings)` against
  # an `L_Phi_loadings` nothing declares, which fails at `stanc`
  # rather than here. `normalise_loadings_prior()` already refuses
  # to build one; this is what stops a second builder from doing so.
  traits <- loadings_spec_traits(spec)
  if (!traits$kernel && !traits$mgp) {
    stop_mvgam_fault(
      "The loadings-prior spec needs features, distances or MGP shrinkage.",
      paste0("Got column_shrinkage = '",
             spec$column_shrinkage %||% "iid",
             "', with the features and distances empty.")
    )
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
      x = paste0("Got: ", paste0("'", bad, "'", collapse = ", "), ".")
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
      "Multiplicative gamma process shrinkage applies to ",
      paste0("'", mgp_capable_trends, "()'", collapse = ", "),
      " trends."
    ),
    x = paste0("Got '", trend_nm, "()'.")
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
  stop(insight::format_error(c(
    "'loadings_prior' requires every 'trend_map' entry to be NA.",
    x = paste0(
      "The 'trend_map' fixes ", sum(!is.na(trend_map_Z)), " of ",
      length(trend_map_Z), " entries."
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
    i = paste0("Write 'bf(y | trials(n) ~ x)' for a column or ",
               "'bf(y | trials(10) ~ x)' for a constant.")
  )), call. = FALSE)
}


#' Every call to one function in an expression
#'
#' Walks the parse tree. A bare symbol sharing the function's name is
#' not a match, and a matched call's own arguments are not searched.
#'
#' @param expr A language object, such as one side of a formula.
#' @param fname The function's name, e.g. `"gp"`.
#' @return A list of the matched calls, in the order they are written.
#' @noRd
formula_calls <- function(expr, fname) {
  checkmate::assert_string(fname, min.chars = 1L)
  if (!is.call(expr)) return(list())
  if (is.name(expr[[1L]]) && identical(as.character(expr[[1L]]), fname)) {
    return(list(expr))
  }
  # An empty argument, as in `x[, 1]`, is the missing symbol, which
  # cannot be passed on as a value.
  args <- as.list(expr)[-1L]
  args <- args[!vapply(args, rlang::is_missing, logical(1L))]
  unlist(lapply(args, formula_calls, fname = fname), recursive = FALSE)
}


#' The text a formula argument was written as
#'
#' A string literal gives its value and anything else its deparsed
#' expression. A description of a model reads the formula as the user
#' wrote it: evaluating an argument such as `k = kk` there needed every
#' object it names to exist in the describing session, and a failure
#' was reported as the default the user had overridden.
#'
#' @param arg One argument of a call in a formula.
#' @return A character string.
#' @noRd
formula_arg_text <- function(arg) {
  if (is.character(arg) && length(arg) == 1L) return(arg)
  paste(deparse(arg), collapse = "")
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
  calls <- formula_calls(lhs, aterm)
  if (length(calls)) calls[[1L]] else NULL
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


#' Compute an effective sample size without the capping notice
#'
#' `posterior` caps an ESS estimate at `S * log10(S)` whenever a
#' parameter's autocorrelation sum falls below `1 / log10(S)`, and
#' warns each time it caps one. Anticorrelated draws are what put an
#' estimate above that bound, and the capped figure is both the
#' conservative one and the figure mvgam reports, which leaves the
#' reader nothing to act on. Every mvgam surface reporting an ESS
#' runs its computation through here, which keeps the decision about
#' showing the notice in one place. The match is on the message,
#' which keeps every other warning the computation raises visible.
#'
#' @param expr Expression computing one or more ESS values
#' @return The value of `expr`
#'
#' @noRd
without_ess_cap_notice <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("ESS has been capped", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
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
#' @param object The fitted `mvgam` object. Its axis record gives the
#'   grid and the spacing, and its trend specification says whether
#'   the trend steps.
#' @return Invisibly `TRUE`; raises otherwise.
#' @noRd
assert_forecast_times_steppable <- function(fc_times, object) {
  checkmate::assert_list(fc_times, null.ok = TRUE)
  checkmate::assert_class(object, "mvgam")
  trend_spec <- first_trend_spec(object)
  if (!trend_requires_regular_intervals(trend_spec)) {
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
  # The grid is the fit's recorded axis. On a frame whose responses
  # are its series, each response's training rows hold its observed
  # occasions alone. Gathering the grid from them made a wide frame padded at
  # the end look shorter than its latent state. A forecast starting
  # inside the grid then passed, and the trend stepped on from the
  # end of the grid under times it had not reached.
  #
  # The horizon is resolved from the last *observed* occasion, which
  # is a different question and rightly answered differently: a
  # padded series must not be forecast from an occasion it was never
  # seen at. So on a padded frame the two disagree about the
  # occasions between the last response and the end of the grid, and
  # the message below has to name the grid rather than call its last
  # position an observation.
  axis_time <- mvgam_axes(object)$time
  past <- as.numeric(axis_time$values)
  step <- axis_time$step
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
        x = paste0("The training grid runs to time ", past[length(past)],
                   "."),
        x = paste0("Series '", lv, "' needs times ", expected[1L], " to ",
                   expected[length(expected)], " and got ", fut[1L],
                   " to ", fut[length(fut)], "."),
        i = "'CAR()' models a trend over irregular times.",
        i = "'hindcast()' covers times inside the training grid."
      )), call. = FALSE)
    }
  }
  invisible(TRUE)
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
