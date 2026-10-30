#' Validate and Standardize Family Argument
#'
#' @description
#' The one reader of a family argument. Turns every spelling a model
#' accepts into one family object: a family function, a call to one,
#' a brms family name, or the name of one of mvgam's own families. A
#' stats family becomes the `brmsfamily` brms builds from it. That
#' object lists the family's distributional parameters with their
#' links.
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
  checkmate::assert(
    checkmate::check_function(family),
    checkmate::check_class(family, "family"),
    checkmate::check_class(family, "brmsfamily"),
    checkmate::check_character(family, min.len = 1, max.len = 2),
    combine = "or"
  )
  checkmate::assert_string(link, null.ok = TRUE)

  if (is.function(family)) {
    family <- family()
  }
  if (inherits(family, "brmsfamily")) {
    return(family)
  }
  if (inherits(family, "family")) {
    link <- family$link
    family <- family$family
  }

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
    # brms refuses an unknown name itself. Its message names the
    # family and lists the ones brms supports.
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
#' through one linear predictor per category. mvgam's trend adds to a
#' single predictor per row. brms marks each such family with a
#' special, `"categorical"`, `"multinomial"` or `"simplex"`. The
#' refusal names the mvgam family that takes the same response in
#' long format. Family `com_poisson` needs brms 2.23.2 or later, the
#' first version whose Stan likelihood takes `mu` as the mode.
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
      i = paste0("Request support at ", mvgam_issues_url(), ".")
    )), call. = FALSE)
  }
  # mvgam's own multi-response families (diri / multi / categ / mvn /
  # mvt) take a long-format K-row response through the closure-unit
  # pipeline.
  if (is_multi_response_family(family)) {
    return(invisible(TRUE))
  }
  # `logistic_normal()` is also marked "simplex". Its entry comes
  # first. A logistic-normal composition has multivariate normal
  # additive log-ratios, which `mvn()` fits once transformed.
  long_format <- function(wrapper) {
    paste0("Use ", wrapper, ", the mvgam wrapper for a response in ",
           "long format.")
  }
  hints <- c(
    logistic_normal = paste0("Fit mvn() to the additive log-ratios of ",
                             "the proportions, in long format."),
    categorical = long_format("categ()"),
    multinomial = long_format("multi()"),
    simplex = long_format("diri()")
  )
  hint <- hints[intersect(names(hints), family$specials)][1L]
  if (!is.na(hint)) {
    stop(insight::format_error(c(
      paste0("Family '", resolve_family_name(family),
             "' is not supported by mvgam directly."),
      i = unname(hint)
    )), call. = FALSE)
  }
  # Reason: brms before 2.23.2 writes the COM-Poisson likelihood with
  # weights mu^k / (k!)^shape. The brms R functions take mu as the
  # mode, with weights (mu^k / k!)^shape, as mvgam's do. A fit would
  # sample one distribution and predict from another.
  if (identical(family$family, "com_poisson") &&
      utils::packageVersion("brms") < "2.23.2") {
    stop(insight::format_error(c(
      "Family 'com_poisson' requires brms 2.23.2 or later.",
      x = paste0("Installed brms: ", utils::packageVersion("brms"), "."),
      i = paste0("Earlier brms versions parameterise the Stan ",
                 "likelihood differently from their predictions."),
      i = paste0("Install brms from GitHub with ",
                 "'remotes::install_github(\"paul-buerkner/brms\")'.")
    )), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate observation data shape for a closure-unit family
#'
#' Closure-unit families (`nmix()` and `occ()`) take long-format data
#' with one row per visit. Rows sharing a `(series, time)` pair are
#' replicate visits to one closure unit. The checks:
#'   - the response holds non-negative integers, in {0, 1} for a
#'     binary family;
#'   - `cap` holds positive integers, constant within a unit and at
#'     least each observed count, where the likelihood would
#'     otherwise be `-Inf`;
#'   - at least two units, since one unit is a single draw of the
#'     state;
#'   - every unit visited once with no covariate in either formula
#'     identifies only the product of state and detection. A binary
#'     family warns once per session, its bounded state leaving the
#'     posterior prior-dominated but proper (MacKenzie et al. 2002;
#'     Royle and Dorazio 2008, ch. 3.5). A count family is refused,
#'     its unbounded state leaving lambda and p on one isocurve
#'     (Solymos et al. 2012);
#'   - more than 30% of units visited once warns once per session.
#'
#' @param data Long-format observation data frame
#' @param family The detection family, which sets the binary check, the
#'   default cap and the columns that key a unit
#' @param response_var Name of the response column
#' @param series_var,time_var Names of the series and time columns
#' @param cap_var Name of the column bounding the latent state
#' @param has_obs_covariates Logical; TRUE when the state formula
#'   contains at least one covariate
#' @param has_det_covariates Logical; TRUE when the detection formula
#'   contains one
#' @param identifiability Logical. Whether to run the checks that the
#'   design separates state from detection: two units at minimum, then
#'   the single-visit tests. They concern a design being fitted. A
#'   forecast grid holds no response. A prediction may cover a single
#'   site. The predict-time caller passes `FALSE`.
#' @return Invisible `TRUE` on success; stops on hard
#'   identifiability failure.
#' @noRd
validate_closure_unit_data <- function(data,
                                       family,
                                       response_var,
                                       series_var = "series",
                                       time_var = "time",
                                       cap_var = "cap",
                                       has_obs_covariates = FALSE,
                                       has_det_covariates = FALSE,
                                       identifiability = TRUE) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_true(is_closure_unit_family(family))
  checkmate::assert_flag(has_obs_covariates)
  checkmate::assert_flag(has_det_covariates)
  checkmate::assert_flag(identifiability)
  binary_y_check <- is_binary_response_family(family)
  default_cap <- closure_unit_default_cap(family)
  # A family with a fixed cap or a data-driven buffer needs no column
  cap_required <- is.null(default_cap) &&
    is.null(closure_unit_default_cap(family, buffer = TRUE))
  unit_grouping_vars <- closure_unit_key_vars(family, series_var, time_var)
  # Pretty-print of the grouping tuple for messages, e.g.
  # "(species, site, season)".
  grouping_label <- paste0("(", paste(unit_grouping_vars, collapse = ", "),
                            ")")

  required_cols <- c(response_var, unit_grouping_vars,
                     if (cap_required) cap_var)
  assert_axis_column(data, series_var, "series")
  assert_axis_column(data, time_var, "time")
  assert_closure_unit_columns(
    data, setdiff(required_cols, c(series_var, time_var))
  )

  y_vals <- data[[response_var]]
  # A user's `cap` column is checked whatever the family. Without one,
  # a family's fixed default, such as `occ()`'s one, is checked the
  # same way. A bound derived from the data is at least the largest
  # count of its unit by construction.
  cap_vals <- if (cap_var %in% colnames(data)) {
    data[[cap_var]]
  } else if (!is.null(default_cap)) {
    rep(as.integer(default_cap), nrow(data))
  }

  # A missing response is a visit that did not happen. The checks
  # below cover the observed visits. A frame with no response at all
  # is refused where the units are formed.
  observed <- !is.na(y_vals)
  y_obs <- y_vals[observed]
  if (!(is.numeric(y_obs) || is.logical(y_obs)) ||
        any(!is.finite(as.numeric(y_obs)))) {
    stop(insight::format_error(paste0(
      "'", response_var, "' must hold finite numbers."
    )), call. = FALSE)
  }
  y_int <- rep(NA_integer_, length(y_vals))
  y_int[observed] <- as.integer(y_obs)
  if (any(y_int[observed] < 0L)) {
    stop(insight::format_error(
      paste0("Negative counts found in '", response_var, "'.")
    ), call. = FALSE)
  }
  if (any(abs(as.numeric(y_obs) - y_int[observed]) > 1e-8)) {
    stop(insight::format_error(
      paste0("Non-integer values found in response '", response_var, "'.")
    ), call. = FALSE)
  }
  if (binary_y_check && any(y_int[observed] > 1L)) {
    bad <- which(!is.na(y_int) & y_int > 1L)[1L]
    stop(insight::format_error(c(
      paste0(
        "A closure-unit family with a binary response requires '",
        response_var, "' in {0, 1}."
      ),
      x = paste0("Row ", bad, ": ", response_var, " = ", y_int[bad], "."),
      i = paste0("occ() and nmix(\"royle_nichols\") model detection and ",
                 "non-detection."),
      i = "For count detections use family = nmix()."
    )), call. = FALSE)
  }

  if (!is.null(cap_vals) &&
        (!is.numeric(cap_vals) || any(!is.finite(cap_vals)))) {
    stop(insight::format_error(paste0(
      "'", cap_var, "' must hold finite numbers."
    )), call. = FALSE)
  }
  cap_int <- if (!is.null(cap_vals)) as.integer(cap_vals)
  if (!is.null(cap_int) && any(cap_int < 1L)) {
    stop(insight::format_error(
      paste0("'", cap_var, "' must be a positive integer.")
    ), call. = FALSE)
  }
  if (!is.null(cap_int) && any(cap_int < y_int, na.rm = TRUE)) {
    bad <- which(!is.na(y_int) & cap_int < y_int)[1L]
    stop(insight::format_error(c(
      paste0("Some '", cap_var, "' values are below the observed counts."),
      x = paste0(
        "Row ", bad, ": ", cap_var, " = ", cap_int[bad],
        ", ", response_var, " = ", y_int[bad], "."
      )
    )), call. = FALSE)
  }

  # The unit layout comes from `closure_unit_index()`, which the data
  # builder uses too. A unit whose responses are all missing holds no
  # visit. The cap check covers every unit the frame names.
  idx <- closure_unit_index(data, unit_grouping_vars, response_var)
  rep_counts <- idx$n_rep[idx$n_rep > 0L]
  n_unit <- length(rep_counts)
  if (!is.null(cap_int)) {
    refuse_unit_varying(
      cap_int, list(row_unit = idx$unit, N_unit = length(idx$levels)),
      cap_var, "The cap bounds the latent state of the unit."
    )
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
    )), call. = FALSE)
  }

  any_covariates <- has_obs_covariates || has_det_covariates
  # Single visits without covariates. A bounded state (occ, psi in
  # [0, 1]) gives a prior-dominated but proper posterior (Royle and
  # Dorazio 2008, ch. 3.5). That fit proceeds with a warning. An
  # unbounded state (nmix, lambda > 0) leaves lambda and p anywhere
  # on the isocurve lambda * p = y / n_visits (Solymos et al. 2012;
  # Dennis et al. 2015; Kery 2018). That fit is refused.
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


#' Every column a formula's left-hand side uses
#'
#' The response and its addition terms together: `y | trials(n)` gives
#' `y` and `n`. A model frame holds these columns. A covariate check
#' leaves them out. `response_columns()` tells which is the
#' response. `rlang::f_lhs()` errors on a `brmsformula`, a list.
#'
#' @param formula A `formula`, `brmsformula` or `mvbrmsformula`, or
#'   NULL
#' @return Character vector, empty for a one-sided formula or NULL
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
  checkmate::assert_multi_class(
    obs_formula, c("formula", "brmsformula", "mvbrmsformula")
  )
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
#' after the trend is parsed. Its message names neither the formula
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
  checkmate::assert_character(optional)
  checkmate::assert_string(context)
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


#' The rows of each column that hold a missing value
#'
#' A matrix column, such as a distributed-lag predictor, counts a row
#' once however many of its cells are missing.
#'
#' @param data A data frame, or a list of vectors and matrices
#' @param columns Names of the columns to check
#' @return Named list of row indices, one per column
#' @noRd
na_rows <- function(data, columns) {
  checkmate::assert_character(columns)
  rows <- lapply(columns, function(v) {
    x <- data[[v]]
    which(if (is.matrix(x)) rowSums(is.na(x)) > 0L else is.na(x))
  })
  stats::setNames(rows, columns)
}


#' Refuse model columns a frame omits or leaves incomplete
#'
#' The fit and prediction paths share this message.
#'
#' @param absent Names of the absent columns
#' @param gaps Named list of the rows holding missing values, one per
#'   column
#' @param context The argument that supplied the frame
#' @return `TRUE`, invisibly
#' @noRd
refuse_column_gaps <- function(absent, gaps, context) {
  checkmate::assert_character(absent)
  checkmate::assert_list(gaps)
  checkmate::assert_string(context)
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
#' on every row whose response was observed. It drops a row whose
#' addition term is missing, as it drops one with a missing covariate.
#' The trend mapping keeps every row with an observed response. The
#' two would then describe different rows. A row with a missing
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


#' Refuse a `trend_map` of the wrong shape
#'
#' Every trend constructor that accepts `trend_map` calls this.
#' `normalise_trend_map()` checks it against the data.
#'
#' @param input The user's `trend_map`, or NULL
#' @return NULL, invisibly
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


#' The fixed loadings a `trend_map` gives
#'
#' Turns each accepted shape into a numeric loading matrix `Z` of
#' dimension `n_series x n_lv`. The `mvgam()` argument and the trend
#' constructors both call it.
#'
#' Accepted shapes:
#' \itemize{
#'   \item Numeric `matrix` of dimension `n_series x n_lv`. Finite
#'     entries are fixed and keep their values in the posterior,
#'     fractional and negative ones included. `NA` entries are
#'     sampled. `n_lv` is `ncol(Z)`.
#'   \item `data.frame(series, trend)`: one row per series assigning
#'     each to a single trend integer. `Z` is binary, with
#'     `Z[s, k] = 1` where `trend_map$trend[trend_map$series == s]` is
#'     `k`.
#'   \item Character scalar `"identity"` (each series its own trend,
#'     `Z = diag(n_series)`) or `"shared"` (one factor,
#'     `Z = matrix(1, n_series, 1)`).
#' }
#'
#' A `trend_map` with a fixed entry takes no QR rotation. Its loadings
#' are saved as `Z[i, j]` with the fixed entries exact.
#'
#' Refused: an integer vector, which depends on the order of the
#' series levels; `Inf` or `NaN` entries; rows of fixed zeros with no
#' free entry, which leave a series out of the model; and data frame
#' mappings with missing series, gaps in the trend integers or
#' `max(trend) > n_series`.
#'
#' @param input The user-supplied `trend_map` value.
#' @param data Training `data.frame` whose series column resolves
#'   the dimensions and labels.
#' @param series_var The series column the trend constructor names.
#' @return `list(Z = <num matrix>, n_lv = <int>)`, or NULL
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
    )), call. = FALSE)
  }
  rownames(Z) <- series_levels
  colnames(Z) <- paste0("trend_", seq_len(ncol(Z)))
  list(Z = Z, n_lv = ncol(Z))
}


#' The loadings of a `trend_map` code
#'
#' @param input `"identity"` or `"shared"`
#' @param n_series Number of series
#' @return Numeric loading matrix
#' @noRd
trend_map_from_character <- function(input, n_series) {
  checkmate::assert_int(n_series, lower = 1L)
  if (length(input) != 1L) {
    stop(insight::format_error(c(
      "Character 'trend_map' must be a single string.",
      x = paste0("Got length-", length(input), " vector.")
    )), call. = FALSE)
  }
  switch(
    input,
    identity = diag(1, nrow = n_series, ncol = n_series),
    shared = matrix(1, nrow = n_series, ncol = 1L),
    stop(insight::format_error(c(
      paste0("Unknown 'trend_map' code: '", input, "'."),
      i = "Accepted codes: 'identity', 'shared'."
    )), call. = FALSE)
  )
}


#' The loadings of a `trend_map` matrix
#'
#' Rownames, where given, name the series of each row. The rows are
#' then reordered to the series axis.
#'
#' @param input Numeric matrix, `NA` marking a sampled entry
#' @param series_levels The training series, in axis order
#' @return Numeric loading matrix in axis order
#' @noRd
trend_map_from_matrix <- function(input, series_levels) {
  checkmate::assert_matrix(input, mode = "numeric")
  checkmate::assert_character(series_levels, min.len = 1L)
  n_series <- length(series_levels)
  if (nrow(input) != n_series) {
    stop(insight::format_error(c(
      "'trend_map' matrix has the wrong number of rows.",
      x = paste0(
        "Expected ", n_series, " (one per series), got ",
        nrow(input), "."
      )
    )), call. = FALSE)
  }
  if (!is.null(rownames(input))) {
    input <- input[
      series_row_order(rownames(input), series_levels, "trend_map"), ,
      drop = FALSE
    ]
  }
  if (any(is.infinite(input) | is.nan(input))) {
    stop(insight::format_error(c(
      "'trend_map' matrix has Inf or NaN entries.",
      i = paste0(
        "Use NA to mark free (sampled) entries, or supply a ",
        "finite numeric loading to fix the entry."
      )
    )), call. = FALSE)
  }
  # A row of all NA has every entry free. A row of fixed zeros leaves
  # the series out of the model.
  fixed_zero_row <- rowSums(is.na(input)) == 0L &
    rowSums(abs(input)) == 0
  if (any(fixed_zero_row)) {
    stop(insight::format_error(c(
      "Every series must load on at least one factor in 'trend_map'.",
      x = paste0(
        "Series whose loadings are all fixed at zero: ",
        paste0("'", series_levels[fixed_zero_row], "'", collapse = ", "),
        "."
      ),
      i = "NA marks an entry to be sampled."
    )), call. = FALSE)
  }
  # A column with no fixed entry has no fixed sign or order. Where each
  # factor takes its own coefficients, `relabel_factors()` aligns such
  # columns after sampling.
  input
}


#' The loadings of a `trend_map` data frame
#'
#' @param input Data frame with `series` and `trend` columns
#' @param series_levels The training series, in axis order
#' @return Binary loading matrix in axis order
#' @noRd
trend_map_from_dataframe <- function(input, series_levels) {
  checkmate::assert_character(series_levels, min.len = 1L)
  validate_required_variables(
    input, c("series", "trend"), "trend_map"
  )
  ord <- series_row_order(as.character(input$series), series_levels,
                          "trend_map")
  if (!is.numeric(input$trend)) {
    stop(insight::format_error(c(
      "'trend_map$trend' must be a numeric or integer column.",
      x = paste0("Got column of class ", class(input$trend)[1L], "."),
      i = "Use integer factor indices 1..K to assign series."
    )), call. = FALSE)
  }
  t <- as.integer(input$trend)
  if (anyNA(t) || any(t < 1L)) {
    stop(insight::format_error(c(
      "'trend_map$trend' must be positive integers.",
      x = paste0("Got: ", paste0(input$trend, collapse = ", "), ".")
    )), call. = FALSE)
  }
  max_t <- max(t)
  if (!setequal(unique(t), seq_len(max_t))) {
    stop(insight::format_error(c(
      "'trend_map$trend' must be a contiguous integer sequence 1..K.",
      x = paste0("Got: ", paste0(sort(unique(t)), collapse = ", "), ".")
    )), call. = FALSE)
  }
  if (max_t > length(series_levels)) {
    stop(insight::format_error(c(
      paste0(
        "'trend_map$trend' has more factors than series (",
        max_t, " > ", length(series_levels), ")."
      ),
      i = "max(trend) must be at most the number of series."
    )), call. = FALSE)
  }
  # `ord` puts the user's rows in axis order. `Z[s, ]` then matches the
  # series the sampler indexes.
  Z <- matrix(0, nrow = length(series_levels), ncol = max_t)
  Z[cbind(seq_along(series_levels), t[ord])] <- 1
  Z
}


#' Refuse a series table that misses, invents or repeats a series
#'
#' A `trend_map` data frame names its series in a column. A
#' `trend_map` matrix names them in its rownames. A `loadings_prior`
#' feature frame uses either. One message names every fault.
#'
#' @param supplied The series labels the caller wrote
#' @param series_levels The training series, in axis order
#' @param subject The argument to name in the message
#' @return `TRUE`, invisibly
#' @noRd
assert_series_coverage <- function(supplied, series_levels, subject) {
  checkmate::assert_character(series_levels, min.len = 1L)
  checkmate::assert_string(subject)
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


#' The row order that puts a caller's series in axis order
#'
#' Checks the labels first with `assert_series_coverage()`.
#'
#' @inheritParams assert_series_coverage
#' @return Integer index, one position per training series
#' @noRd
series_row_order <- function(supplied, series_levels, subject) {
  assert_series_coverage(supplied, series_levels, subject)
  match(series_levels, as.character(supplied))
}


#' Refuse a method that needs draws an unfitted stub lacks
#'
#' `run_model = FALSE` returns an `mvgam_prefit` whose `fit` slot is
#' `NULL`. `stancode()` and `standata()` work on it and do not call
#' this. The class marks the stub. Test stubs that lack a `fit` slot
#' without the class pass.
#'
#' @param object An `mvgam` object.
#' @param fn The method's own name, which the message reports. A
#'   shared accessor several methods reach leaves this `NULL`.
#' @return Invisibly `TRUE`, or an error.
#' @noRd
require_fitted_model <- function(object, fn = NULL) {
  checkmate::assert_string(fn, null.ok = TRUE)
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


#' Refuse a set of models where any one lacks a posterior
#'
#' @param models A list of `mvgam` objects.
#' @param fn The calling method's name, which the message reports.
#' @return Invisibly `TRUE`, or an error.
#' @noRd
require_fitted_models <- function(models, fn) {
  checkmate::assert_list(models)
  for (model in models) {
    require_fitted_model(model, fn)
  }
  invisible(TRUE)
}


#' Refuse dot arguments that neither a method nor its receiver takes
#'
#' A method that hands `...` to another package splits the names in
#' its own formals from the names in the receiver's. The receiver
#' discards a name in neither set. The caller then gets the default
#' they meant to override. bayesplot warns about such a name and loo
#' stays silent. `rlang::check_dots_empty()` covers the methods with
#' no use for dots. A name close to an allowed one draws a suggestion.
#'
#' @param dots The method's `list(...)`.
#' @param allowed Names in the method's or the receiver's formals.
#' @param fn The method's own name, which the message reports.
#' @return Invisibly `TRUE`, or an error.
#' @noRd
refuse_unread_dots <- function(dots, allowed, fn) {
  checkmate::assert_list(dots)
  checkmate::assert_character(allowed)
  checkmate::assert_string(fn)
  unread <- setdiff(names(dots), c(allowed, ""))
  if (!length(unread)) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "Unrecognised arguments were supplied.",
    x = paste0(
      "Unknown to ", fn, "() and to what it hands the work to: ",
      paste0("'", unread, "'", collapse = ", "), "."
    ),
    did_you_mean(unread[[1L]], setdiff(allowed, "...")) %||%
      c(i = paste0("Check the spelling against '?", fn, "'."))
  )), call. = FALSE)
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
#' The two grouping validators run before the axis is resolved. Both
#' need the series each row belongs to under this specification.
#' The grouping is taken through `spec_groupings()`, which accepts the
#' flat and the nested spelling of a spec. A frame with no `series`
#' column still has series here, because `gr` and `subgr` name them.
#'
#' `assert_grouping_columns()` refuses a grouping column the frame
#' lacks or leaves incomplete, before brms setup.
#'
#' @param trend_spec A trend specification, in either spelling.
#' @param data The frame being validated.
#' @return A list with `gr_var` and the per-row `series`, or `NULL`
#'   when the spec names no grouping.
#' @noRd
spec_series_values <- function(trend_spec, data) {
  checkmate::assert_data_frame(data)
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


#' The trend's grouping arguments, checked against the frame
#'
#' `gr` needs `subgr`. Each series belongs to one group. The Stan
#' program sizes every group's Cholesky and scale blocks by one
#' subgroup count. An unbalanced design gives NaN at initialisation.
#'
#' @param parsed_trend A parsed trend specification
#' @param data The frame being validated
#' @param series_var The series column
#' @return `parsed_trend`, holding "NA" for a grouping it does not use
#' @noRd
resolve_trend_groupings <- function(parsed_trend, data, series_var) {
  checkmate::assert_list(parsed_trend)
  checkmate::assert_data_frame(data)
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


#' Refuse hierarchical groups of unequal size
#'
#' The hierarchical trend program sizes each group's Cholesky factor
#' and scales by the largest group. A smaller group then fills part of
#' a fixed-size innovation vector. The rest stays uninitialised and
#' gives NaN at Stan initialisation.
#'
#' @param trend_spec Trend specification with `gr`, `subgr` and
#'   (optionally) `series` fields.
#' @param data Data frame containing `gr` and series columns.
#' @return NULL, invisibly
#' @noRd
validate_gr_balanced_groups <- function(trend_spec, data) {
  resolved <- spec_series_values(trend_spec, data)
  if (is.null(resolved)) {
    return(invisible(NULL))
  }
  gr_var <- resolved$gr_var
  # One group value per series of the trend, from the first row naming
  # it. A factor column keeps unobserved levels, which hold no series.
  first <- !duplicated(as.character(resolved$series))
  series_group_table <- table(data[[gr_var]][first])
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


#' Refuse a grouping that varies within a series
#'
#' A hierarchical trend maps each series to a single group.
#'
#' @param trend_spec Trend specification with `gr` and (optionally)
#'   `series` fields.
#' @param data Data frame containing `gr` and series columns.
#' @return NULL, invisibly
#' @noRd
validate_gr_constant_per_series <- function(trend_spec, data) {
  resolved <- spec_series_values(trend_spec, data)
  if (is.null(resolved)) {
    return(invisible(NULL))
  }
  gr_var <- resolved$gr_var
  counts <- vapply(
    split(data[[gr_var]], resolved$series),
    function(x) length(unique(x[!is.na(x)])),
    integer(1L)
  )
  bad_series <- names(counts)[counts > 1L]
  if (length(bad_series) > 0L) {
    more <- length(bad_series) - 5L
    stop(insight::format_error(c(
      paste0("Grouping variable '", gr_var,
             "' must be constant within each series."),
      x = paste0("Inconsistent series: ",
                 paste(utils::head(bad_series, 5L), collapse = ", "),
                 if (more > 0L) paste0(" (and ", more, " more)"), "."),
      i = "Each series must belong to a single group."
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' Whether a trend spec describes a factor model
#'
#' An `n_lv` of at most `n_series` is a factor model.
#' `n_lv = n_series` is the MGP truncation ceiling.
#' `validate_n_lv_ceiling()` refuses more factors than series.
#'
#' @param n_lv Integer or NULL (the `n_lv` slot from a trend spec).
#' @param n_series Positive integer.
#' @return Logical scalar.
#' @noRd
is_factor_model_spec <- function(n_lv, n_series) {
  checkmate::assert_number(n_lv, null.ok = TRUE)
  checkmate::assert_number(n_series, lower = 1)
  if (is.null(n_lv)) return(FALSE)
  n_lv <= n_series
}


#' The number of latent factors of a fitted trend
#'
#' The fitted counterpart of `is_factor_model_spec()`. The trend spec
#' records whether the user asked for factors. `N_lv_trend` equals
#' `N_series_trend` on a fit without factors, whose `Z` is the
#' identity. A factor fit reaches the same dimension at its ceiling,
#' under an MGP loadings prior or a `by = lv_axis()` term. The spec
#' separates the two.
#'
#' An mvbind fit takes the first response's spec. Without a series
#' count from the caller or the fit, the function returns the
#' requested `n_lv`. `validate_n_lv_ceiling()` refused
#' `n_lv > n_series` at fit time.
#'
#' @param object A fitted `mvgam` object.
#' @param n_series Number of observed series. Defaults to the count
#'   the fit was built with.
#' @return Integer number of factors, or NULL for a trend without
#'   factors.
#' @noRd
detect_factor_n_lv <- function(object, n_series = NULL) {
  checkmate::assert_number(n_series, lower = 1, null.ok = TRUE)
  n_lv <- spec_n_lv(first_trend_spec(object))
  if (is.null(n_lv) || !is.numeric(n_lv) || n_lv < 1L) {
    return(NULL)
  }
  n_series <- n_series %||% mvgam_axes(object)$series$n
  if (!is.null(n_series) && !is_factor_model_spec(n_lv, n_series)) {
    return(NULL)
  }
  as.integer(n_lv)
}


#' The number of latent factors, or an error naming the caller
#'
#' @param object A fitted `mvgam` object
#' @param caller The function that needs a factor model, for the message
#' @return Integer number of factors
#' @noRd
require_factor_n_lv <- function(object, caller) {
  checkmate::assert_string(caller)
  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv)) {
    stop(insight::format_error(c(
      paste0("'", caller, "' requires a latent factor model."),
      i = paste0("Fit with 'n_lv' in 'jsdgam()' or in the trend ",
                 "constructor, e.g. '~ AR(n_lv = 2)'.")
    )), call. = FALSE)
  }
  n_lv
}


#' Refuse a time-series view of a fit whose data name no series
#'
#' A trend-free model fits on any frame. A frame naming no time column
#' or no series column leaves the fit with no axis record. A hindcast
#' or forecast then has no series to step along.
#'
#' @param object A fitted `mvgam` object
#' @param caller The function that needs the axes, for the message
#' @return `TRUE`, invisibly
#' @noRd
require_series_axis <- function(object, caller) {
  checkmate::assert_string(caller)
  if (!is.null(fitted_series_levels(object))) {
    return(invisible(TRUE))
  }
  vars <- axis_vars(object)
  cols <- c(vars$time_var, vars$series_var)
  absent <- setdiff(cols, names(object$data))
  stop(insight::format_error(c(
    paste0("'", caller, "()' requires data that name a time and a ",
           "series."),
    x = if (length(absent)) {
      paste0("Absent from the fitted data: ",
             paste0("'", absent, "'", collapse = ", "), ".")
    } else {
      paste0("The fitted data's ", paste0("'", cols, "'", collapse = " or "),
             " column holds missing values.")
    },
    i = paste0("For a single time series, add data$", vars$series_var,
               " <- factor(\"series_1\") and refit."),
    i = "'predict()' and 'posterior_predict()' work on this fit."
  )), call. = FALSE)
}


#' Refuse more factors than series
#'
#' `jsdgam()` and `mvgam()` both call this. The marginal `Z Z'` has
#' rank at most the number of series. Extra factors add nothing,
#' whatever the loadings prior. `n_lv = n_series` passes. The loadings
#' prior decides how well that boundary samples.
#'
#' @param n_lv The requested number of factors
#' @param n_species The number of series or species
#' @param fit_function `"mvgam"` or `"jsdgam"`, which sets the noun
#'   in the message
#' @return `TRUE`, invisibly
#' @noRd
validate_n_lv_ceiling <- function(n_lv, n_species,
                                  fit_function = "mvgam") {
  checkmate::assert_int(n_lv, lower = 1)
  checkmate::assert_int(n_species, lower = 2)
  checkmate::assert_choice(fit_function, c("mvgam", "jsdgam"))
  noun <- if (identical(fit_function, "jsdgam")) "species" else "series"
  if (n_lv > n_species) {
    stop(insight::format_error(c(
      paste0("'n_lv' must be at most the number of ", noun, "."),
      x = paste0(
        "Got n_lv = ", n_lv, ", n_", noun, " = ", n_species, "."
      ),
      i = paste0(
        "The residual covariance has rank at most n_", noun, "."
      )
    )), call. = FALSE)
  }
  invisible(TRUE)
}


#' Refuse an `n_lv` passed to `mvgam()` itself
#'
#' The factor count belongs to the trend with the factors. It is
#' written on the constructor inside `trend_formula`, as
#' `AR(p = 1, n_lv = 2)`. Passed to `mvgam()` itself it lands in
#' `...`, which forwards to brms and Stan. Neither uses it. A
#' two-series frame asked for one factor would fit two without a
#' message.
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
#' factors. It also gives the reason for a trend without a factor
#' form. The refusal is composed from the registry entry. Each route
#' to a factor request then gives the same refusal. Both remedies are
#' named whichever argument raised the request. The four routes to a
#' factor trend give a user one sentence to act on.
#'
#' @param trend_name Registered trend name, such as `"PW"`.
#' @return `invisible(TRUE)` when the trend takes factors.
#' @noRd
refuse_factor_request_for_trend <- function(trend_name) {
  checkmate::assert_string(trend_name)
  ensure_registry_initialized()
  info <- get_trend_info(trend_name)
  if (isTRUE(info$supports_factors)) return(invisible(TRUE))
  stop(insight::format_error(c(
    paste0("Factor models are not supported for ", trend_name,
           " trends."),
    x = info$incompatibility_reason,
    i = paste0(
      "Drop 'n_lv' and 'trend_map', or use a trend with a factor form: ",
      registered_trend_constructors("supports_factors"), "."
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
  checkmate::assert_string(trend_name)
  if (is.null(n_lv) && is.null(trend_map)) return(invisible(TRUE))
  refuse_factor_request_for_trend(trend_name)
}


#' Refuse a factor model on a trend that has no factor form
#'
#' The registry's `supports_factors` entry records whether a trend
#' decomposes into latent factors. `incompatibility_reason` gives the
#' reason for a trend that cannot. `prepare_trend_specs()` calls this
#' once every route to a factor model has set `n_lv` on the spec: the
#' constructor's `n_lv`, a `trend_map` and the all-`NA` `trend_map`
#' that `jsdgam()` builds. A top-level `n_lv` passed to `mvgam()`
#' reaches no spec. `refuse_top_level_n_lv()` refuses it.
#'
#' @param trend_specs Normalised trend specifications, one spec or a
#'   per-response list.
#' @return `invisible(TRUE)`.
#' @noRd
enforce_factor_support_against_specs <- function(trend_specs) {
  checkmate::assert_list(trend_specs, null.ok = TRUE)
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
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_string(fit_function)
  checkmate::assert_list(family, null.ok = TRUE)
  spec <- trend_spec_head(trend_specs)
  n_lv <- spec_n_lv(spec)
  if (is.null(n_lv) || n_lv < 1L || n_series < 2L) {
    return(invisible(TRUE))
  }
  validate_n_lv_ceiling(
    n_lv = n_lv,
    n_species = as.integer(n_series),
    fit_function = fit_function
  )
  # The ceiling above is about capacity: more factors than series
  # buys nothing. This check is about identification. It applies well
  # below that ceiling for a family with a residual scale per
  # component. A `trend_map` fixing the loadings samples no `Z` and
  # leaves no split to identify.
  if (is.null(spec$fixed_Z)) {
    warn_unidentified_component_scale(
      n_lv = n_lv, n_species = as.integer(n_series), family = family
    )
  }
  invisible(TRUE)
}

#' Validate grouping arguments
#'
#' @description
#' Validates hierarchical grouping structure and returns processed
#' arguments.
#'
#' @param gr Grouping variable name or NULL/'NA'
#' @param subgr Subgrouping variable name or NULL/'NA'
#' @param series_var The series column, which names the subgroups when
#'   `gr` is given without `subgr`
#' @return List with processed gr and subgr values
#' @noRd
validate_grouping_arguments <- function(gr, subgr, series_var) {
  checkmate::assert_string(series_var, min.chars = 1)
  if (is.null(gr) || is.na(gr) || gr == "NA") {
    gr <- NULL
  } else {
    checkmate::assert_string(gr, min.chars = 1)
  }

  if (is.null(subgr) || is.na(subgr) || subgr == "NA") {
    subgr <- NULL
  } else {
    checkmate::assert_string(subgr, min.chars = 1)
  }

  # The hierarchical Stan data derives the subgroups from the series
  # column. A bare `ZMVN(gr = X)` names that column as `subgr`.
  if (!is.null(gr) && is.null(subgr)) {
    subgr <- series_var
  }

  # `subgr` naming the series column without `gr` is the ungrouped
  # default
  if (identical(subgr, series_var) && is.null(gr)) {
    subgr <- NULL
  }
  if (!is.null(subgr) && is.null(gr)) {
    stop(insight::format_error(
      paste0("'subgr = ", subgr, "' requires a main grouping 'gr'.")
    ), call. = FALSE)
  }

  list(gr = gr, subgr = subgr)
}


#' Validate regular time intervals
#'
#' @description
#' Validates that time series has regular intervals for trends that
#' require it.
#'
#' @param time_values Vector of time values
#' @param time_var Name of time variable (for error messages)
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_regular_time_intervals <- function(time_values, time_var = "time") {
  checkmate::assert_numeric(time_values, min.len = 2)
  checkmate::assert_string(time_var)

  # Spacing is a property of the sorted distinct times, whatever order
  # the time index takes them in.
  intervals <- diff(sort(unique(time_values)))

  # Regular intervals, allowing a small numerical tolerance
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
    )), call. = FALSE)
  }

  invisible(TRUE)
}


#' Response support per family
#'
#' The interval a response must lie in, whether each endpoint is
#' included and whether values must be whole numbers. Families brms
#' also defines take brms's own values from
#' `family_info(family, "ybounds")`, `"closed"` and `use_int()`.
#' `test-mvgam-data.R` drives brms through its public interface. It
#' asserts that brms refuses exactly what this table refuses. mvgam's
#' native families are absent from brms and have their own entries.
#'
#' Ordinal, categorical and multi-response families are absent on
#' purpose. Their responses are levels. brms checks them against the
#' category count it derives from the data. Closure-unit families
#' go through `validate_closure_unit_data()`.
#'
#' @noRd
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


#' A family's support as an inequality, such as "0 < y < 1" or "y >= 0"
#'
#' @param spec One entry of `mvgam_response_support`
#' @param y_name Character name of the response column
#' @return A single string
#' @noRd
describe_response_support <- function(spec, y_name) {
  checkmate::assert_list(spec)
  checkmate::assert_string(y_name)
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


#' Families whose mean parameter must be positive
#'
#' brms gives each of these a default link that keeps it positive.
#' `lognormal` and `shifted_lognormal` default to identity. Their `mu`
#' is a log-scale location that takes any sign.
#'
#' @noRd
mvgam_positive_mean_families <- c(
  "gamma", "weibull", "exponential", "frechet", "inverse.gaussian"
)

#' Links leaving the linear predictor free to reach zero or below
#'
#' Each needs `eta > 0` for the mean to stay positive.
#'
#' @noRd
mvgam_links_needing_positive_eta <- c("identity", "inverse", "1/mu^2")


#' Warn when a latent trend can drive a positive mean to zero
#'
#' A family in `mvgam_positive_mean_families` needs `mu > 0`. Under
#' `identity`, `inverse` or `1/mu^2` that holds only where the linear
#' predictor stays positive on every row. A latent trend is unbounded
#' and centred near zero. The predictor reaches zero during sampling,
#' where the likelihood is undefined and Stan rejects the draw. Without
#' an intercept the initial draws never reach the region at all. Every
#' chain then stops before warmup.
#'
#' The link the caller asked for is kept. `Gamma()` and
#' `Gamma(link = "inverse")` build the same object. A substituted link
#' would override a deliberate choice as readily as an inherited
#' default.
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
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_list(formula),
    .var.name = "formula"
  )
  checkmate::assert_formula(trend_formula, null.ok = TRUE)
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


#' Check a response against the support of its family
#'
#' Serves the families that are not closure-unit families.
#' Closure-unit families (`occ()`, `nmix()` variants) go through
#' `validate_closure_unit_data()`, which runs its own integer, binary,
#' cap and non-negative checks. `mvgam_response_support` drives the
#' check. Naming a family there covers it.
#'
#' @param y The response column
#' @param family The response's family
#' @param y_name Character name of the response column
#' @return `TRUE`, invisibly, or an error
#' @noRd
validate_response_for_family <- function(y, family, y_name = "y") {
  checkmate::assert_list(family)
  checkmate::assert_string(y_name)
  # A factor response has no numeric order to range-check. brms
  # validates the families that accept one (bernoulli, the ordinal and
  # categorical families) against their own level requirements.
  # Comparing a factor to floor() errors.
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
    )), call. = FALSE)
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
    )), call. = FALSE)
  }

  invisible(TRUE)
}


#' The advice specific to a family about its support
#'
#' The hint adds to the bound the caller has just been shown.
#'
#' @param fam Character family name
#' @return A single string
#' @noRd
response_support_hint <- function(fam) {
  checkmate::assert_string(fam)
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


#' Check each response against its family's support
#'
#' Runs `validate_response_for_family()` over each response the
#' observation formula names. A response its family cannot take is
#' refused with a message naming the column and the observed values.
#' brms would report the constraint from a function the user did not
#' call. Multi-response families take a matrix response.
#' `validate_closure_unit_data()` checks closure-unit families. Both
#' are left to their own validators.
#'
#' Each response is held to its own family: the one named inside its
#' `bf()` where it has one, `family` otherwise. Only the response is
#' held to it. An addition term such as `weights(w)` or `trials(n)` is
#' a column of the model, but its values are not observations of the
#' family. A poisson fit with fractional weights is valid.
#'
#' @param data Data frame, a list of imputed frames, or any other
#'   object, which passes unchecked
#' @param formula The observation formula
#' @param family The observation family
#' @return `TRUE`, invisibly, or an error
#' @noRd
validate_response_shapes <- function(data, formula, family) {
  # A list of frames is the multiple-imputation path. Each frame has
  # the same response and any one of them can be wrong.
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
  # into one object. It refuses anything else, which is a fault in the
  # user's own argument and belongs here.
  families <- formula_families(formula, validate_family(family))
  for (key in names(columns)) {
    fam <- families[[key]]
    if (is_multi_response_family(fam) || uses_closure_unit_layout(fam)) {
      next
    }
    column <- columns[[key]]
    form <- response_formulas(formula)[[key]]
    for (frame in frames) {
      if (!column %in% names(frame)) next
      validate_response_for_family(frame[[column]], fam, y_name = column)
      refuse_counts_above_trials(
        frame, column, addition_term_values(form, fam, frame)$trials
      )
    }
  }
  invisible(TRUE)
}


#' Refuse an observed count above its `trials()` denominator
#'
#' brms reports this from its data preparation without naming the
#' column or the row. A frame without the denominator's columns is left
#' to `validate_model_columns()`.
#'
#' @param frame Data frame holding the response
#' @param column Character name of the response column
#' @param n Trial counts, one per row of `frame`, or `NULL`
#' @return `TRUE`, invisibly, or an error
#' @noRd
refuse_counts_above_trials <- function(frame, column, n) {
  checkmate::assert_data_frame(frame)
  checkmate::assert_string(column)
  checkmate::assert_numeric(n, null.ok = TRUE)
  if (is.null(n)) {
    return(invisible(TRUE))
  }
  y <- frame[[column]]
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
#' `base::deparse()` returns one element per line. This joins them. A
#' caller naming the user's own expression in a message can bound the
#' length. The result is then truncated.
#'
#' @param expr Expression to deparse
#' @param max_char Keep at most this many characters, marking a
#'   truncation with an ellipsis. `NULL` keeps the whole string.
#' @param ... Additional arguments passed to deparse
#' @return Character string representation of the expression
#' @noRd
deparse0 <- function(expr, max_char = NULL, ...) {
  checkmate::assert_count(max_char, null.ok = TRUE)
  out <- paste(deparse(expr, ...), collapse = "")
  if (!is.null(max_char) && nchar(out) > max_char) {
    out <- paste0(substr(out, 1L, max_char), "...")
  }
  out
}

#' Whether a formula is nonlinear
#'
#' @description
#' Determines if a brms formula specifies a nonlinear model.
#'
#' @param formula brms formula object
#' @return Logical indicating if formula is nonlinear
#' @noRd
is_nonlinear_formula <- function(formula) {
  checkmate::assert(
    checkmate::check_class(formula, "formula"),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "brmsterms"),
    .var.name = "formula"
  )

  # mvbrmsformula is the multi-response wrapper. Non-linearity is a
  # per-response property recorded on each form in `$forms`. Any form
  # with `nl = TRUE` makes the model nonlinear.
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

  # brms takes non-linearity from `bf(..., nl = TRUE)` alone. A plain
  # formula has no such setting.
  FALSE
}

#' The function a call names
#'
#' @param expr A call. `pkg::fun` gives `fun`.
#' @return The function name, or `character(0)` for a call whose head
#'   is itself computed
#' @noRd
call_head_name <- function(expr) {
  checkmate::assert_true(is.call(expr))
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
#' is no call. The walk never reports it as the brms `se()` term.
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

#' Each call to one of a set of functions, deparsed
#'
#' Calls are listed in the order written. A call to one of `fns`
#' nested in another counts.
#'
#' @param expr An unevaluated R expression
#' @param fns Character vector of function names
#' @return Character vector of deparsed calls
#' @noRd
find_calls <- function(expr, fns) {
  checkmate::assert_character(fns)
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
#' Dispatches on the formula class. Plain `formula`, `brmsformula`,
#' `bform` and `mvbrmsformula` inputs all return the function calls in
#' their predictor expressions. For `brmsformula` / `bform` the walk
#' covers the main RHS plus every distributional-parameter (`pforms`)
#' and non-linear-parameter (`nlpars`) sub-formula. For
#' `mvbrmsformula` it recurses over each response sub-formula.
#'
#' The LHS of a formula (response, brms addition-terms like
#' `y | cens(c)`) is excluded. Validators that check LHS specials use
#' the parsed `brmsterms()` `$adforms` slot.
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

#' Tell the user once per term that an exact GP term predicts in-sample
#'
#' `inform_once()` keeps the count and honours `silent >= 2`.
#'
#' @param gp_term Character deparse of the `gp()` call
#' @return `NULL`, invisibly
#' @noRd
maybe_warn_exact_gp <- function(gp_term) {
  checkmate::assert_string(gp_term)
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

#' Inform once per term on exact GP terms
#'
#' Scans a formula for `gp()` terms that omit `k`. Exact GPs fit
#' through brms with their full covariance kernel. mvgam's prediction
#' code does not reconstruct their basis at newdata. The notice names
#' that gap and the approximate form that avoids it.
#'
#' @param formula A formula, `brmsformula` or `mvbrmsformula`
#' @return `NULL`, invisibly
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
    # `k` is taken from each `gp()` call as written. An unevaluated
    # `k = kk` needs no object named `kk` to exist here.
    for (gp_call in formula_calls(main[[length(main)]], "gp")) {
      k <- gp_call_to_spec(gp_call)$k
      if (is.na(k) || identical(k, "NA")) {
        # One mvgam() call reaches this from the observation validator,
        # the trend validator and `setup_brms_lightweight()`. The
        # notice's id carries the term. It shows once per term per
        # session.
        maybe_warn_exact_gp(paste(deparse(gp_call), collapse = ""))
      }
    }
  }
  invisible(NULL)
}

#' Validate an observation formula for brms
#'
#' Refuses mvgam trend constructors and a `.` term in the observation
#' formula.
#'
#' @param formula Observation formula (formula, brmsformula, or bform)
#' @return The original formula unchanged (brms handles processing)
#' @noRd
validate_obs_formula_brms <- function(formula) {
  if (is.null(formula)) return(NULL)

  checkmate::assert(
    checkmate::check_class(formula, "formula"),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "bform"),
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

  # A `.` expands to every column of the frame. An mvgam frame also
  # holds its time and series columns.
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
    )), call. = FALSE)
  }

  # The Stan likelihood includes a `rate()` denominator and an `se()`
  # standard error. Predictions, residuals and log-likelihoods are
  # computed in R from the predictor and the family's parameters. Those
  # computations would leave both terms out.
  for (aterm in c("rate", "se")) {
    if (any(vapply(forms, function(f) {
      !is.null(find_aterm_call(f, aterm))
    }, logical(1L)))) {
      stop(insight::format_error(c(
        paste0("'", aterm, "()' is not supported in the observation ",
               "'formula'."),
        i = if (aterm == "rate") {
          "Write the exposure as an offset, as in '+ offset(log(area))'."
        }
      )), call. = FALSE)
    }
  }

  validate_exact_gp_usage(formula)

  # brms handles all other validation
  formula
}

#' Refuse a trend formula that names a response
#'
#' `validate_trend_formula()` raises this at the user's entry point.
#' `parse_trend_formula()` raises it for the callers that reach it
#' directly.
#'
#' @param formula The trend formula
#' @return `TRUE`, invisibly, or an error
#' @noRd
refuse_trend_formula_response <- function(formula) {
  checkmate::assert_formula(formula)
  if (length(formula) != 3L) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "A trend formula takes predictors only.",
    x = paste0("Found the response '", deparse(formula[[2L]]), "'."),
    i = "Write the response in the observation 'formula'."
  )), call. = FALSE)
}

#' Refuse a trend formula with an offset among its terms
#'
#' The caller passes the `terms` object it already built. A recomputed
#' one would lose the `data` that expanded a `.`.
#'
#' @param terms_obj The `terms` object of the trend formula
#' @return `TRUE`, invisibly, or an error
#' @noRd
refuse_trend_formula_offset <- function(terms_obj) {
  checkmate::assert_class(terms_obj, "terms")
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
#' terms, `mi()`, `cs()`, more than one trend constructor and an exact
#' `gp()`.
#' The checks walk the formula's calls. A variable such as `se_x` or
#' `weights` is no call. None of the checks refuses it.
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

  if (length(find_calls(rhs, "mi")) > 0L) {
    stop(insight::format_error(c(
      "'mi()' is not supported in 'trend_formula'.",
      x = "brms imputes a 'mi()' predictor as a response of the model.",
      i = "Impute missing trend covariates before fitting."
    )), call. = FALSE)
  }

  if (length(find_calls(rhs, "cs")) > 0L) {
    stop(insight::format_error(c(
      "'cs()' is not supported in 'trend_formula'.",
      x = "A category-specific effect shifts an ordinal response's thresholds.",
      x = "The latent trend has no thresholds.",
      i = "Write 'cs()' in the observation 'formula' of an ordinal family."
    )), call. = FALSE)
  }

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
  # A call to no function that lies close to a trend constructor is a
  # misspelt trend. brms would report only that the function is absent.
  # The brms and mvgam namespaces resolve 's()', 'gp()' and 'lv_axis()'
  # for a caller who attached neither package.
  envs <- list(environment(formula) %||% globalenv(), asNamespace("brms"),
               asNamespace("mvgam"))
  for (nm in setdiff(formula_rhs_function_names(formula),
                     mvgam_trend_registry())) {
    resolved <- any(vapply(envs, function(env) {
      exists(nm, envir = env, mode = "function")
    }, logical(1L)))
    if (!resolved && !is.null(did_you_mean(nm, mvgam_trend_registry()))) {
      get_trend_info(nm)
    }
  }
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

#' brms autocorrelation terms
#'
#' They model temporal dependence the trend already models.
#'
#' @noRd
brms_autocor_terms <- c("ar", "ma", "arma", "cosy", "unstr", "autocor")

#' brms addition terms
#'
#' The `formula_ad` specials that modify the observation model. They
#' have no meaning on a latent trend.
#'
#' @noRd
brms_addition_terms <- c(
  "weights", "se", "cens", "trunc", "trials", "rate", "vreal", "vint",
  "subset", "index", "dec", "cat", "thres", "cov_ranef"
)

#' Whether an object is an mvgam trend
#'
#' Tests whether an object is a valid mvgam trend specification.
#'
#' @param x Object to test
#' @return Logical indicating if x is a mvgam trend
#' @export
is.mvgam_trend <- function(x) {
  inherits(x, "mvgam_trend")
}

#' Extract time series dimensions from data
#'
#' @description
#' Extracts core time series dimensions directly from data and validates
#' structure based on trend type. Every time series dimension the model
#' needs is derived here.
#'
#' @param data Data frame containing time series
#' @param time_var Name of time variable (default: "time")
#' @param series_var Name of series variable (default: "series")
#' @param trend_specs Optional trend specification list for added metadata
#' @param obs_formula The observation formula, which names the
#'   responses and the rows each covers, or `NULL` for a frame alone
#' @return List with time series dimensions and optional added metadata
#' @noRd
extract_time_series_dimensions <- function(data, time_var = "time",
                                           series_var = "series",
                                           trend_specs = NULL,
                                           obs_formula = NULL) {

  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  response_vars <- if (!is.null(obs_formula)) response_columns(obs_formula)

  # Core dimensions from the attribute-based accessors
  time_vals <- get_time_for_grouping(data)
  series_vals <- get_series_for_grouping(data)
  # `ensure_mvgam_variables()` records how the series were named with
  # the series themselves. A response-keyed or hierarchical axis taken
  # as explicit would be placed on the wrong rows after the fit.
  series_source <- attr(data, "mvgam_series_source")
  checkmate::assert_choice(
    series_source, c("explicit", "hierarchical", "multivariate"),
    .var.name = "attr(data, 'mvgam_series_source')"
  )

  # A response-keyed frame states its axis directly, in formula order.
  # That order is kept. The axis is held once and in one order: the
  # responses as the formula names them where the frame is
  # response-keyed, otherwise the frame's own series sorted.
  response_axis <- mvgam_response_axis(data)
  series_axis <- response_axis %||% series_axis_values(series_vals)

  # The group of each axis entry, from the rows that define the axis.
  # `group_inds_trend[s]` is subscripted by the trend's own series
  # index and follows `series_axis`. A frame whose series attribute
  # was rebuilt without the trend's grouping would match no group.
  series_groups <- axis_group_values(data, trend_specs, series_vals,
                                     series_axis)
  original_time <- attr(data, "mvgam_original_time")

  min_time <- min(time_vals, na.rm = TRUE)
  max_time <- max(time_vals, na.rm = TRUE)

  sorted_unique_times <- time_axis_values(time_vals)

  dimensions <- list(
    n_time = length(sorted_unique_times),
    n_series = length(series_axis),
    n_obs = nrow(data),
    time_range = c(min_time, max_time),
    time_var = time_var,
    series_var = series_var,
    unique_times = sorted_unique_times,
    # The series axis, in order, and the group of each series
    unique_series = series_axis,
    series_groups = series_groups,
    # Each row's cell on the two axes. A response-keyed frame gives
    # every row every response. The response a caller maps decides the
    # series.
    row_time = match(time_vals, sorted_unique_times),
    row_series = if (is.null(response_axis)) {
      match(series_vals, series_axis)
    }
  )

  # Both axes as one record, assembled where they are decided. The fit
  # stores it. Post-fit code takes the axes the model was given. Axes
  # rebuilt from the frame could come back permuted and still in
  # range.
  #
  # Time is held as its ordered original values. The integer index is
  # then `match()`. The gaps `CAR()` and the Gaussian processes need
  # are `diff()`.
  time_values <- if (is.null(original_time)) {
    sorted_unique_times
  } else {
    time_axis_values(original_time)
  }
  # The step `forecast()` extends the grid by. A regular grid has one
  # gap. An irregular one has no single step and records `NA`.
  time_gaps <- if (length(time_values) > 1L) diff(time_values) else numeric(0)
  time_step <- if (length(time_gaps) &&
                     isTRUE(all.equal(max(time_gaps), min(time_gaps)))) {
    time_gaps[1L]
  } else {
    NA_real_
  }

  groupings <- spec_groupings(trend_specs)
  n_lv <- spec_n_lv(trend_specs)

  # The last time each series was observed, in axis order. `CAR()`
  # forecasts from it. The fit records it. The forecast code then needs
  # no walk of the training frame.
  observed_times <- axis_observed_times(
    data, series_vals, series_axis, original_time %||% time_vals,
    response_axis, response_vars
  )
  series_last_time <- axis_last_times(observed_times)

  dimensions$axes <- list(
    series = list(
      levels = as.character(series_axis),
      source = series_source,
      n = length(series_axis),
      groups = series_groups,
      last_time = series_last_time
    ),
    # One representation of time. The integer index a trend steps
    # along is `match()` into these values. The gaps `CAR()` and the
    # Gaussian processes measure are `diff()` of them. Recording either
    # as well would give a second account of the same fact.
    time = list(
      values = time_values,
      n = length(sorted_unique_times),
      step = time_step,
      # The typical gap between two observations of one series, the
      # unit `CAR()` measures its gaps in
      observation_gap = axis_observation_gap(observed_times)
    ),
    # The columns of `Z` and of `lv_trend`. A model with no factor
    # constructor loads each series on its own state. The factor axis
    # is then the series axis. `n_lv` records the series count.
    factor = list(
      n_lv = as.integer(n_lv %||% length(series_axis))
    ),
    # What the second dimension of `times_trend` indexes. The series,
    # except under `by = lv_axis()`, where Stan declares the map
    # `[N_time_trend, N_lv_trend]` and folds `mu_factor` into
    # `lv_trend`. The grain is known only once the trend formula has
    # been walked. It is left unset here and named by
    # `extract_and_validate_trend_components()`. A default of "series"
    # would be wrong for a `by = lv_axis()` trend.
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

  # Observation-to-trend mappings when response variables are given.
  # They are built here, with the dimensions they index.
  if (!is.null(obs_formula)) {
    dimensions$mappings <- list()
    rows <- response_rows(obs_formula, data)

    # Keyed by the name brms suffixes each response's data with. Each
    # mapping takes the rows the response's likelihood covers.
    for (key in names(response_vars)) {
      dimensions$mappings[[key]] <- generate_obs_trend_mapping(
        data = data,
        rows = rows[[key]],
        response_key = key,
        time_var = time_var,
        series_var = series_var,
        dimensions = dimensions
      )
    }
  }

  # The trend's registry entry states whether it needs regular
  # intervals. See the matching call upstream.
  if (trend_requires_regular_intervals(trend_specs)) {
    validate_regular_time_intervals(attr(data, "mvgam_original_time"), time_var)
  }

  dimensions
}

#' Generate the observation-to-trend mapping
#'
#' @description
#' Creates mapping arrays that align each row of a response's
#' likelihood to its position in the trend matrix. brms keeps the rows
#' `response_rows()` gives and provides no obs_ind array.
#'
#' @param data Data frame with observations (may include NAs)
#' @param rows Logical vector along the rows of `data`: the rows the
#'   response's likelihood covers, from `response_rows()`
#' @param response_key The response's key, as `response_columns()` names
#'   it, which is what a response-keyed series axis holds
#' @param time_var Name of time variable
#' @param series_var Name of series variable
#' @param dimensions List from extract_time_series_dimensions with time
#'   series structure
#' @return List containing obs_trend_time and obs_trend_series arrays for Stan
#' @noRd
generate_obs_trend_mapping <- function(data, rows, response_key,
                                       time_var = "time",
                                       series_var = "series",
                                       dimensions) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_logical(rows, any.missing = FALSE, len = nrow(data))
  checkmate::assert_string(response_key)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_list(dimensions)

  # The time column is checked where the time index is built. mvgam
  # keeps each response on its own rows. The shared latent state then
  # takes information at every time point this response covers. The
  # multi-response standata is expanded per response to match.
  non_missing_idx <- which(rows)
  if (length(non_missing_idx) == 0L) {
    stop_unobserved_response(response_key)
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

  # Scalar attributes are copied as-is. The response axis has one
  # entry per series and is copied whole.
  scalar_attrs <- c("mvgam_series_source", "mvgam_series_levels")
  for (attr_name in scalar_attrs) {
    if (!is.null(attr(data, attr_name))) {
      attr(obs_data, attr_name) <- attr(data, attr_name)
    }
  }

  # Get sorted unique values from dimensions
  sorted_unique_times <- dimensions$unique_times
  sorted_unique_series <- dimensions$unique_series

  # Time and series index mappings from the attribute-based system.
  # They map each observation to its position in the trend matrix.
  # Both use the processed attribute values, which match the dimension
  # calculation and every series creation strategy.
  time_values <- get_time_for_grouping(obs_data)
  series_values <- get_series_for_grouping(obs_data)

  obs_trend_time <- match(time_values, sorted_unique_times)
  # Every row of a response-keyed frame has every response. The
  # response being mapped fixes the series. The row position does not
  # enter.
  response_axis <- mvgam_response_axis(obs_data)
  obs_trend_series <- if (is.null(response_axis)) {
    match(series_values, sorted_unique_series)
  } else {
    rep(response_series_index(response_axis, response_key),
        length(non_missing_idx))
  }

  # Neither index can be missing or out of range. `obs_data` is a
  # subset of the frame the axes were resolved on. Every value it holds
  # is in the list being matched against. A `match()` into a list of
  # length n gives 1..n. A frame whose rows name no cell is refused
  # where the two indices are built.

  list(
    obs_trend_time = as.integer(obs_trend_time),
    obs_trend_series = as.integer(obs_trend_series),
    n_obs_non_missing = length(non_missing_idx),
    has_missing = length(non_missing_idx) < nrow(data)
  )
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
#' the call fails. A compiler reports what went wrong in its messages.
#' It then raises an error that points at them. Those messages are what
#' a user needs from a failed compile. The call runs once.
#'
#' @param expr Expression to evaluate.
#' @param type Which stream to suppress: `"output"` or `"message"`.
#' @param silent Suppress at all?
#' @param ... Passed to `utils::capture.output()` for `type = "output"`.
#' @return The value of `expr`
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
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  checkmate::assert_list(data2, null.ok = TRUE)
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
  checkmate::assert_list(trend_specs, null.ok = TRUE)
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


#' Attach a normalised `loadings_prior` to the trend spec
#'
#' `mvgam()` and `get_prior()` both call this. A structured prior
#' cannot coexist with a partial or fully fixed Z (see
#' `assert_loadings_prior_compatible()`).
#'
#' @param trend_specs One trend spec, a per-response list, or `NULL`
#' @param loadings_prior The `loadings_prior` argument, or `NULL`
#' @param data Training data frame
#' @param data2 Named list of objects the prior names, or `NULL`
#' @return `trend_specs` with `loadings_prior_spec` set
#' @noRd
attach_loadings_prior_spec <- function(trend_specs, loadings_prior,
                                       data, data2) {
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  checkmate::assert_list(data2, null.ok = TRUE)
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


#' Normalise a raw `trend_map` and reconcile it with `n_lv`
#'
#' Called once in the Stan-code pipeline right after
#' `apply_trend_map_alias()`. `normalise_trend_map()` converts the
#' spec's `trend_map` to the numeric Z stored on `spec$fixed_Z`.
#' `spec$n_lv` takes `ncol(Z)`. An `n_lv` also set on the constructor
#' must agree.
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
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_data_frame(data)
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
  # model. It keeps `n_lv` and samples a free Z for a loadings prior.
  fixed_Z <- if (all(is.na(normalised$Z))) NULL else normalised$Z
  map_trend_specs(trend_specs, function(trend_spec) {
    trend_spec$fixed_Z <- fixed_Z
    trend_spec$n_lv <- normalised$n_lv
    trend_spec
  })
}


#' Tell the user once when the hierarchical series replace theirs
#'
#' `gr` and `subgr` together identify a series. mvgam builds the series
#' from them and ignores the `series` column of the data. A user whose
#' column disagreed would believe the model grouped their way. Every
#' post-fit label uses mvgam's names for the series.
#'
#' @param data Training data frame
#' @param series_var Character name of the series column, or `NULL`
#' @param series_values The series values mvgam derived
#' @param gr_var Character name of the grouping column
#' @param subgr_var Character name of the subgrouping column
#' @return `NULL`, invisibly
#' @noRd
warn_series_superseded <- function(data, series_var, series_values,
                                   gr_var, subgr_var) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(series_var, null.ok = TRUE)
  checkmate::assert_string(gr_var)
  checkmate::assert_string(subgr_var)
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

#' Refuse a time column with missing values
#'
#' A row without a time maps to no occasion of the trend. Its trend
#' index would stay `NA` through to the linear predictor.
#'
#' @param values The time column
#' @param time_var Character name of the time column
#' @return `TRUE`, invisibly, or an error
#' @noRd
refuse_missing_times <- function(values, time_var) {
  checkmate::assert_string(time_var)
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


#' "1 row" or "n rows"
#'
#' @param rows Integer vector of row indices
#' @return A single string
#' @noRd
count_rows <- function(rows) {
  checkmate::assert_integerish(rows)
  paste0(length(rows), if (length(rows) == 1L) " row" else " rows")
}


#' Whether a variable name refers to a column
#'
#' A trend constructor spells an absent grouping NA. The normalised
#' spec spells it "NA".
#'
#' @param var A variable name, `NA`, `"NA"` or `NULL`
#' @return A single logical
#' @noRd
named_var <- function(var) {
  !is.null(var) && length(var) == 1L && !is.na(var) &&
    nzchar(var) && !identical(var, "NA")
}


#' Require the grouping columns a hierarchical trend needs
#'
#' Reason: `checkmate::assert_names()` reports the missing names as an
#' internal assertion and gives no reason for them. This message names
#' the role of the columns. Training and prediction frames share it.
#'
#' @param data Data frame
#' @param gr_var Name of the grouping column
#' @param subgr_var Name of the subgrouping column, or `NULL`
#' @return `TRUE`, invisibly, or an error
#' @noRd
assert_grouping_columns <- function(data, gr_var, subgr_var = NULL) {
  checkmate::assert_data_frame(data)
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


#' The series identifier a hierarchical trend uses
#'
#' `gr` and `subgr` together name a series. mvgam derives the column
#' itself. Every site that needs the value builds it here. The fitting
#' path, the prediction path and the level validator then share one
#' separator and ordering.
#'
#' @param data Data frame
#' @param gr_var Character name of the grouping column
#' @param subgr_var Character name of the subgrouping column
#' @return Factor of series identifiers, one per row
#' @noRd
hierarchical_series_values <- function(data, gr_var, subgr_var) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(gr_var)
  checkmate::assert_string(subgr_var)
  interaction(
    data[[gr_var]], data[[subgr_var]],
    drop = TRUE, sep = "_", lex.order = TRUE
  )
}


#' Check the column that places a frame's rows
#'
#' A trend model indexes every row by one occasion and one series.
#' mvgam takes them from the `time` and `series` columns, or from the
#' columns the trend constructor names. This function checks both. mvgam
#' requires the series column even for a single series. The column
#' then holds one level. Training data need a factor series column.
#' Its levels fix the order of the series in the model and every
#' result. Prediction data take the fitted order and accept a
#' character column. Time must be numeric. The trend orders and spaces
#' its steps by the value of each time.
#'
#' @param data Data frame
#' @param column Character name of the column
#' @param axis Either `"time"` or `"series"`
#' @param require_factor Logical. `TRUE` requires a factor series
#'   column.
#' @param frame Character name of the frame in the user's call, such
#'   as `"newdata"`. The messages name it and use it in their remedies.
#' @return `TRUE`, invisibly, or an error
#' @noRd
assert_axis_column <- function(data, column, axis = c("time", "series"),
                               require_factor = TRUE, frame = "data") {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(column)
  checkmate::assert_flag(require_factor)
  checkmate::assert_string(frame, min.chars = 1L)
  axis <- match.arg(axis)
  where <- if (frame == "data") "the data" else paste0("'", frame, "'")
  cell <- paste0(frame, "$", column)
  if (column %in% names(data)) {
    if (axis == "series" && require_factor &&
        !is.factor(data[[column]])) {
      stop(insight::format_error(c(
        paste0("Column '", column, "' must be a factor."),
        x = paste0("Got class '", class(data[[column]])[1L], "'."),
        i = "A factor's levels fix the order of the series.",
        i = paste0("Convert it with ", cell, " <- factor(", cell, ").")
      )), call. = FALSE)
    }
    if (axis == "time" && !is.numeric(data[[column]])) {
      stop(insight::format_error(c(
        paste0("Column '", column, "' must be numeric."),
        x = paste0("Got class '", class(data[[column]])[1L], "'."),
        i = "mvgam orders and spaces the times by their values.",
        i = paste0(
          "Convert it with ", cell, " <- ",
          if (is.factor(data[[column]])) {
            paste0("as.numeric(as.character(", cell, ")).")
          } else {
            paste0("as.numeric(", cell, ").")
          }
        )
      )), call. = FALSE)
    }
    return(invisible(TRUE))
  }
  place <- c(time = "at one occasion", series = "on one series")[[axis]]
  hints <- c(
    i = paste0("A latent trend places each row ", place, "."),
    i = paste0(
      "Add '", column, "' to ", where, " or name its column with the ",
      "trend constructor's '", axis, "' argument."
    ),
    if (axis == "series") {
      c(i = paste0("For a single time series: ", cell,
                   " <- factor(\"series_1\")"))
    }
  )
  stop(insight::format_error(c(
    paste0("Column '", column, "' is absent from ", where, "."),
    x = paste0(
      "Supplied: ", paste0("'", names(data), "'", collapse = ", "), "."
    ),
    hints
  )), call. = FALSE)
}


#' Refuse a prediction frame with gaps in the model's columns
#'
#' A prediction uses the columns the formulas name, the groupings, the
#' aterms, the offset and the axis. A gap in any of them reaches the
#' linear predictor unchanged. A numeric column contributes `NA` cells.
#' A factor column takes its reference level with nothing to mark it.
#' The check excludes the response. A missing response marks the
#' occasions a forecast frame asks mvgam to predict.
#'
#' @param newdata Prediction data frame
#' @param object A fitted `mvgam` object
#' @return `TRUE`, invisibly, or an error
#' @noRd
validate_newdata_complete <- function(newdata, object) {
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_class(object, "mvgam")
  terms <- mvgam_term_list(object)
  vars <- axis_vars(object)
  axes <- mvgam_axes(object)
  # The term list keeps a meta variable only where it varies in the
  # training data. A fit on one occasion drops its time column from
  # that list. The axis names the column whatever the training values
  # did. The time comes from there. A trend-free fit places no row in
  # time. Its formulas name any time column it uses.
  time_var <- if (named_var(vars$time_var) &&
                    get_trend_type(object) != "None") {
    vars$time_var
  } else {
    character(0L)
  }
  read <- setdiff(
    unique(c(terms$conditional, terms$random, terms$slopes,
             terms$aterms, terms$offset, terms$index, time_var)),
    terms$response
  )
  # A column the frame omits altogether is the widest gap of all.
  # Narrowing to the columns present would pass it through. The series
  # column is held out of that test. The axis record places a row the
  # frame never keyed. A grouping names the series with no such column
  # present. The time column is held to the test. An occasion has no
  # substitute. A frame omitting it would be given predictions at
  # whatever position the observation structure fell back to.
  exempt <- setdiff(terms$index %||% character(0L), vars$time_var)
  # Where the column itself names several series and no grouping can
  # derive them, a row's series comes from that column alone. Every
  # row of a single-series fit belongs to the same series. The column
  # adds nothing there.
  if (identical(axes$series$source, "explicit") &&
        length(axes$series$levels) > 1L &&
        !(named_var(vars$gr_var) && named_var(vars$subgr_var))) {
    exempt <- setdiff(exempt, vars$series_var)
  }
  absent <- setdiff(setdiff(read, exempt), names(newdata))
  present <- user_axis_names(object, intersect(read, names(newdata)))
  refuse_column_gaps(
    user_axis_names(object, absent),
    na_rows(newdata, intersect(present, names(newdata))), "newdata"
  )
}


#' Refuse levels absent from the training data
#'
#' @param label What the levels belong to, e.g. "Series"
#' @param seen The levels in the new data
#' @param fitted The training levels
#' @return `TRUE`, invisibly
#' @noRd
refuse_unseen_levels <- function(label, seen, fitted) {
  checkmate::assert_string(label)
  checkmate::assert_atomic(seen)
  checkmate::assert_atomic(fitted)
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
#' @param series_label What the message calls the series
#' @return `TRUE`, invisibly
#' @noRd
validate_prediction_factor_levels <- function(data, metadata,
                                              series_label = "Series") {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(metadata, names = "named", null.ok = TRUE)
  checkmate::assert_string(series_label)
  axes <- metadata$axes
  if (is.null(axes)) {
    return(invisible(TRUE))
  }
  vars <- axes$vars

  # A hierarchical trend derives its series from `gr` and `subgr`. The
  # model ignored any `series` column. Deriving the value here also
  # catches a pair the training data never held, though each level
  # alone is known. Every check compares only the levels the frame
  # holds rows for. Subsetting a frame keeps every declared factor
  # level. A level with no rows names nothing unseen.
  hierarchical <- identical(axes$series$source, "hierarchical")
  seen <- if (hierarchical) {
    assert_grouping_columns(data, vars$gr_var, vars$subgr_var)
    levels(droplevels(
      hierarchical_series_values(data, vars$gr_var, vars$subgr_var)
    ))
  } else if (vars$series_var %in% names(data)) {
    observed_levels(data[[vars$series_var]])
  }
  refuse_unseen_levels(series_label, seen, axes$series$levels)

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
#' `parse_model()` runs the same parse before a fit. A program that
#' passes here passes there.
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


#' Coerce an argument to a single logical value
#'
#' @param x The argument. Its expression names it in the error.
#' @param allow_na Logical. `TRUE` accepts `NA`.
#' @return A single logical
#' @noRd
as_one_logical <- function(x, allow_na = FALSE) {
  checkmate::assert_flag(allow_na)
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop(insight::format_error(
      paste0("'", s, "' must be a single logical value.")
    ), call. = FALSE)
  }
  x
}

#' Refuse a grouped factor trend
#'
#' @description
#' Factor models and hierarchical grouping cannot be combined. The
#' restriction applies to every trend that supports both.
#'
#' @param trend_specs Trend specification list holding `n_lv`, `gr` and
#'   `subgr`
#' @param n_series Number of series in the data
#' @param trend_name Name of the trend for error messages, such as
#'   "RW", "AR" or "VAR"
#' @return `TRUE`, invisibly, or an error
#' @noRd
validate_no_factor_hierarchical <- function(trend_specs, n_series,
                                            trend_name) {
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


#' Refuse a trend that is both grouped and a factor model
#'
#' A grouping and the loadings each define the series axis. Every site
#' that finds the pair raises this one refusal.
#'
#' @param trend_name Name of the trend for the message
#' @return Does not return. Raises an error.
#' @noRd
refuse_factor_hierarchical <- function(trend_name) {
  checkmate::assert_string(trend_name)
  stop(insight::format_error(c(
    paste0("Factor models are not supported for hierarchical ",
           trend_name, " trends."),
    x = "A grouping and the loadings both define the series axis.",
    i = "Remove 'gr' and 'subgr', or drop 'n_lv' and 'trend_map'."
  )), call. = FALSE)
}

#' Remove trend constructor terms from a formula
#'
#' @description
#' Removes trend constructor calls from a formula and keeps every other
#' term, including random effects such as `(1 | group)`. The function
#' edits the right-hand side expression directly with rlang.
#' `reformulate()` would rebuild the formula from deparsed term labels
#' and can mangle such terms.
#'
#' @param trend_formula A formula object containing potential trend terms
#' @param trend_terms Character vector of trend constructor patterns to
#'   remove
#' @return A formula object with trend terms removed
#' @noRd
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
  rlang::new_formula(
    lhs = NULL, rhs = cleaned_expr, env = rlang::f_env(trend_formula)
  )
}

#' Recursively remove trend expressions from a formula expression
#'
#' @description
#' Walks the expression tree and removes trend constructor calls. Every
#' other part of the formula structure is kept.
#'
#' @param expr Expression to process
#' @param trend_patterns Character vector of trend patterns to match
#' @param depth Current recursion depth, bounded at 50
#' @return Cleaned expression, or `NULL` when the whole expression is a
#'   trend term
#' @noRd
remove_trend_expressions <- function(expr, trend_patterns, depth = 0) {
  checkmate::assert_character(trend_patterns)
  checkmate::assert_int(depth, lower = 0)

  # Recursion depth protection
  if (depth > 50) {
    stop(insight::format_error(c(
      "Trend formula nesting exceeds 50 levels.",
      i = "Write the trend formula with fewer nested terms."
    )), call. = FALSE)
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
  checkmate::assert_character(trend_patterns)

  # Match by the name of the function called. A match on the full
  # deparsed text fails on argument-literal variants. Pattern
  # "AR(p = 1)" and expression "AR(p = 1L)" name the same AR
  # constructor and differ as text. The function name stays the same
  # across literal forms.
  if (!rlang::is_call(expr)) return(FALSE)
  fn_name <- rlang::call_name(expr)
  if (is.null(fn_name)) return(FALSE)
  trend_fn_names <- sub("\\(.*$", "", trend_patterns)
  fn_name %in% trend_fn_names
}

#' Attach the time and series indices mvgam groups rows by
#'
#' Sets the time and series attributes every trend computation groups
#' rows by. The time index numbers the sorted distinct times. The
#' series come from the fit's record in the prediction context, from
#' `gr` and `subgr`, from the responses of a wide frame or from the
#' series column, in that order of precedence.
#'
#' @param data Data frame containing trend data
#' @param parsed_trend Parsed trend specification, or `NULL`. Used in
#'   the fitting context.
#' @param time_var Character name of time variable column
#' @param series_var Character name of series variable column
#' @param response_vars Response columns named by response key, from
#'   `response_columns()`, for multivariate series creation
#' @param metadata A fit's trend metadata, or `NULL`. Used in the
#'   prediction context.
#' @param frame Character name of the frame in the user's call, which
#'   a missing axis column's error names
#' @return Data frame with mvgam time and series attributes added
#' @noRd
ensure_mvgam_variables <- function(data, parsed_trend = NULL,
                                   time_var = "time",
                                   series_var = "series",
                                   response_vars = NULL,
                                   metadata = NULL,
                                   frame = "data") {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_list(parsed_trend, null.ok = TRUE)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  checkmate::assert_character(response_vars, min.len = 1,
                              any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_list(metadata, null.ok = TRUE)
  # A fit's metadata marks the prediction context, where a frame
  # naming an unseen series or group is refused.
  validate_prediction_factor_levels(data, metadata)

  # The time index runs in time order whatever order the rows arrive
  # in. The trend steps along it. Numbering the times by first
  # appearance would make an AR recursion advance through whatever
  # sequence the frame was assembled in. A frame grouped by series
  # would give its first series' earliest time index 1 and leave the
  # real first time somewhere in the middle. The mapping would stay a
  # bijection and no downstream check could detect the error. Sorted
  # input is unaffected.
  assert_axis_column(data, time_var, "time", frame = frame)
  unique_times <- time_axis_values(data[[time_var]])
  time_mapping <- setNames(seq_along(unique_times), unique_times)
  attr(data, "mvgam_time") <- time_mapping[as.character(data[[time_var]])]

  # Every row must name a cell of the trend matrix. A missing time or
  # series gives an NA index. Stan would index the trend at
  # `trend[NA, s]` and the observation would take a missing linear
  # predictor. No later step reports it. The two indices are built
  # here and the refusal belongs here.
  refuse_missing_times(data[[time_var]], time_var)
  # The original times give the distances `CAR()` and GPs use
  attr(data, "mvgam_original_time") <- data[[time_var]]

  # The series axis of a frame whose responses are its series
  #
  # A frame written with `brms::mvbf()` has one row per time and one
  # column per response. The series of an observation is a property of
  # the (row, response) pair. A vector with one entry per row cannot
  # hold that. Cutting the rows into a block per response would treat
  # the frame as stacked. The first stretch of the timeline would go to
  # one response and the rest to another, with no error.
  #
  # The level set holds the axis. The levels are the responses in the
  # order the formula names them. Series `k` is then the `k`th response
  # and the `k`th row of the loadings. The per-row values are one
  # constant level, the grain the trend design needs. A covariate
  # column of a wide frame holds one value per time. The design has one
  # row per time and every series uses the same one.
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
  # Some callers pass the specification flat and others nest it under
  # `$trend_model`. `spec_groupings()` takes the grouping from either
  # spelling. Every call within one `standata()` build then resolves
  # the same axis.
  if (is.null(series_values)) {
    groupings <- spec_groupings(parsed_trend)
    gr_var <- groupings$gr
    subgr_var <- groupings$subgr

    if (!is.null(gr_var) && !is.null(subgr_var) &&
          subgr_var != series_var) {
      # A separate subgr variable builds the series from
      # interaction(gr, subgr). A subgr that defaults to the existing
      # series column falls through to Strategy 4. The original series
      # values are then kept. The codegen path takes the series column
      # directly.
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
                       require_factor = is.null(metadata), frame = frame)
    series_values <- data[[series_var]]
    series_source <- "explicit"
  }

  # Store series as attribute. `mvgam_series_levels` is set only when
  # the series axis is the response axis. The per-row values cannot
  # hold it there. Everywhere else the values are the axis and the
  # attribute stays absent.
  attr(data, "mvgam_series") <- series_values
  attr(data, "mvgam_series_source") <- series_source
  attr(data, "mvgam_series_levels") <- series_levels

  # A missing series is the other half of the missing-time refusal. It
  # is checked here, where the series is settled.
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
#' `ensure_mvgam_variables()` accepts a frame with its time column free
#' of missing values that names its series either with a column or
#' with several responses. It refuses anything else. A trendless model
#' would otherwise reach post-fit with no record at all. This function
#' names the set where building one succeeds. Every frame accepted
#' before keeps working.
#'
#' @param data Data frame the model was given
#' @param time_var Time column name
#' @param series_var Series column name
#' @param response_vars Response columns named by key
#' @return A single logical
#' @noRd
frame_names_an_axis <- function(data, time_var, series_var,
                                response_vars = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
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
#' branch. Without this record every post-fit method would derive an
#' axis of its own. A padded series would take the date of its last row
#' in place of its last observation. A wide frame would lose the one
#' field naming its responses as the series. `trend_type` stays unset,
#' which marks a model stepping no latent state.
#'
#' @param data Data frame the model was given
#' @param time_var Time column name
#' @param series_var Series column name
#' @param obs_formula The observation formula, which names the
#'   responses and the rows each covers, or `NULL` for a frame alone
#' @return A metadata list, or `NULL` where the frame names no axis
#' @noRd
trendless_trend_metadata <- function(data, time_var, series_var,
                                     obs_formula = NULL) {
  response_vars <- if (!is.null(obs_formula)) response_columns(obs_formula)
  if (!frame_names_an_axis(data, time_var, series_var, response_vars)) {
    return(NULL)
  }
  prepared <- ensure_mvgam_variables(
    data = data, parsed_trend = NULL, time_var = time_var,
    series_var = series_var, response_vars = response_vars
  )
  axis_record(extract_time_series_dimensions(
    prepared, time_var, series_var, obs_formula = obs_formula
  ))
}


#' The record of a model's axes that a fit stores
#'
#' Post-fit methods take the axes Stan was given from this record. A
#' second construction from the frame could disagree with it.
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
  checkmate::assert_list(dimensions)
  checkmate::assert_character(covariates)
  checkmate::assert_flag(has_by_lv)
  checkmate::assert_flag(had_by_lv)
  list(
    covariates = covariates,
    has_by_lv = has_by_lv,
    had_by_lv = had_by_lv,
    n_lv_for_grain = n_lv_for_grain,
    axes = complete_axes_grain(dimensions$axes, has_by_lv)
  )
}

#' The prepared time or series index a frame carries
#'
#' mvgam stores a prepared frame's trend indices as attributes. Both
#' are fetched here. One function keeps the two accessors in step.
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
  checkmate::assert_atomic(values)
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
#' Every other frame gives `NULL`. There the per-row series values are
#' the axis.
#'
#' Call this before using `get_series_for_grouping()` for anything
#' other than a grouping grain. On a response-keyed frame those values
#' are a single constant. The series of an observation depends on the
#' response in question. The per-row values cannot express it.
#'
#' @param data Data frame carrying mvgam attributes
#' @return Character vector of response names, or `NULL`
#' @noRd
mvgam_response_axis <- function(data) {
  checkmate::assert_data_frame(data)
  attr(data, "mvgam_series_levels")
}

#' The series index of a named response
#'
#' Model building calls this on the axis being built. Post-fit code
#' calls it on the axis the fit records. Both get the same index.
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
#' Removes every mvgam attribute from a data frame.
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

#' Extract and validate trend components
#'
#' @description
#' Resolves the axes once, extracts the trend data on them and
#' records the dimensions on the specification. Stan assembly and
#' every post-fit method take their axes from this one pass.
#'
#' @param data Data frame containing time series data
#' @param mv_spec Multivariate specification object with base_formula and
#'   trend_specs
#' @param obs_formula The observation formula, which names the
#'   responses and the rows each covers
#' @param family The observation family, or `NULL`
#' @return List with `trend_data`, `enhanced_mv_spec` and `metadata`
#' @noRd
extract_and_validate_trend_components <- function(data, mv_spec,
                                                  obs_formula,
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
  response_vars <- response_columns(obs_formula)
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

  parsed_trend <- resolve_trend_groupings(parsed_trend, data, series_var)

  data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var,
                                response_vars)

  # `lv_axis()` in the trend formula is replaced by the trend-data
  # column of the trend's axis:
  #
  #   factor model (n_lv set): `.trend`. The trend data then runs over
  #     (time, factor). Stan passes the whole trend predictor through
  #     the loadings Z.
  #   any other model: the series column, on the usual (time, series)
  #     grain. A trend whose series come from a grouping or from the
  #     responses has no such column. It takes the `series` column
  #     `trend_cell_frame()` builds.
  #
  # `has_by_lv` selects the (time, factor) grain. `had_by_lv` records
  # that the user wrote `lv_axis()` on either path. Display code uses
  # it to name effects without the replacement column.
  has_by_lv <- FALSE
  had_by_lv <- FALSE
  by_lv_target <- NULL
  n_lv_for_grain <- NULL
  if (inherits(mv_spec$base_formula, "formula")) {
    factor_active <- !is.null(parsed_trend$n_lv) &&
      isTRUE(as.integer(parsed_trend$n_lv) >= 1L)
    explicit <- identical(attr(data, "mvgam_series_source"), "explicit")
    target <- if (factor_active) {
      factor_axis_column
    } else if (explicit) {
      series_var
    } else {
      "series"
    }
    by_lv_res <- detect_and_rewrite_by_lv(
      mv_spec$base_formula, target = target, series_var = series_var
    )
    if (by_lv_res$has_by_lv) {
      if (!factor_active && identical(attr(data, "mvgam_series_source"),
                                      "multivariate")) {
        stop(insight::format_error(c(
          paste0("'lv_axis()' requires latent factors in a model whose ",
                 "responses are its series."),
          x = "The trend design of this model has one row per time.",
          i = paste0("Set 'n_lv' on the trend constructor, or give each ",
                     "response its own effect in 'formula'.")
        )), call. = FALSE)
      }
      if (factor_active && spec_n_lv(parsed_trend) < 2L) {
        stop(insight::format_error(c(
          "'lv_axis()' requires at least two latent factors.",
          x = "The trend has one factor, which takes every effect.",
          i = "Drop 'lv_axis()' from the term, or raise 'n_lv'."
        )), call. = FALSE)
      }
      had_by_lv <- TRUE
      by_lv_target <- target
      mv_spec$base_formula <- by_lv_res$formula
      if (by_lv_res$deprecated_trend_seen) {
        warn_legacy_trend_by()
      }
      if (factor_active) {
        has_by_lv <- TRUE
        n_lv_for_grain <- spec_n_lv(parsed_trend)
      }
    }
  }

  # The series and time dimensions every later step uses
  dimensions <- extract_time_series_dimensions(
    data,
    time_var,
    series_var,
    trend_specs = mv_spec$trend_specs,
    obs_formula = obs_formula
  )

  # The grain travels on the dimensions to the stanvar emission, which
  # sizes `times_trend` by it.
  dimensions$has_by_lv <- has_by_lv
  dimensions$had_by_lv <- had_by_lv
  dimensions$by_lv_target <- by_lv_target
  dimensions$n_lv_for_grain <- n_lv_for_grain

  # The `n_lv` ceiling, on the series axis just resolved. It also
  # holds `by = lv_axis()` to at most one factor per series.
  enforce_n_lv_ceiling_against_data(
    mv_spec$trend_specs, dimensions$n_series, family = family
  )
  if (!is.null(family)) {
    warn_zmvn_single_series(mv_spec, family, dimensions$n_series)
  }

  # What a trend requires of the data, on the resolved axes
  if (!is.null(parsed_trend$trend)) {
    trend_method(
      parsed_trend$trend,
      trend_method_optional_templates[["validate_data"]],
      default = function(...) invisible(NULL)
    )(parsed_trend, dimensions, mv_spec)
  }
  # The trend formula's covariates. A response may not be one. Each has
  # to be constant within a trend cell.
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
  if (length(covariates) > 0) {
    assert_trend_covariates_constant(data, covariates, parsed_trend,
                                     time_vals, series_vals,
                                     by_time = has_by_lv)
  }

  # A trend that completes its grid holds a state for a series at a
  # time the frame has no row for. Every other trend takes one row per
  # time and series.
  completes_grid <- trend_completes_time_grid(parsed_trend)
  if (completes_grid) {
    refuse_heavy_tails_on_ragged_grid(time_vals, series_vals, parsed_trend)
    refuse_covariates_on_ragged_grid(time_vals, series_vals, covariates)
  } else if (!has_by_lv) {
    refuse_ragged_trend_grid(time_vals, series_vals)
  }
  trend_data <- trend_cell_frame(data, covariates, time_vals, series_vals,
                                 has_by_lv, n_lv_for_grain,
                                 fill_grid = completes_grid)
  trend_metadata <- axis_record(
    dimensions, covariates = covariates, has_by_lv = has_by_lv,
    had_by_lv = had_by_lv, n_lv_for_grain = n_lv_for_grain
  )

  # The trend's default scale priors take the responses' spread on
  # their link scale, as brms's defaults do
  prior_unit <- if (!is.null(family) && !is.null(mv_spec$obs_formula)) {
    trend_prior_unit(mv_spec$obs_formula, family, data)
  } else {
    1
  }
  enhanced_mv_spec <- mv_spec
  enhanced_mv_spec$trend_specs <- map_trend_specs(
    mv_spec$trend_specs, function(spec) {
      spec$dimensions <- dimensions
      spec$prior_unit <- prior_unit
      spec
    }
  )

  list(
    trend_data = trend_data,
    enhanced_mv_spec = enhanced_mv_spec,
    metadata = trend_metadata
  )
}

#' Trend covariates without the axis columns
#'
#' The caller adds `time` and `series` and groups on them. dplyr omits
#' grouping columns from `across()`. Naming them in the selection
#' errors with "Element `time` doesn't exist". A `trend_formula` that
#' refers to the time or series variable, as in `~ s(time)` or a plain
#' `~ time`, puts them in `trend_variables`. Grouping carries `time`
#' through to the result. Both are dropped here.
#'
#' @param trend_variables Character vector of trend formula variables
#' @return Character vector without `time` and `series`
#' @noRd
trend_covariate_names <- function(trend_variables) {
  checkmate::assert_character(trend_variables, any.missing = FALSE)
  setdiff(trend_variables, c("time", "series"))
}


#' The covariate columns of a trend formula
#'
#' Parses the formula `parse_trend_formula()` leaves once the trend
#' constructor is removed. A bare `all.vars()` of the full right-hand
#' side would list the columns named inside `AR(gr = region)` as
#' covariates. `newdata` must hold these columns. The collapse to trend
#' grain selects them.
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
  # brms refuses a call as a grouping factor. The parse runs after each
  # `lv_axis()` is replaced. The axis columns are carried separately.
  base_formula <- detect_and_rewrite_by_lv(base_formula)$formula
  bterms <- brms_terms(rlang::new_formula(
    quote(.mvgam_lhs), rlang::f_rhs(base_formula)
  ))
  setdiff(trend_covariate_names(all.vars(bterms$allvars)),
          c(".mvgam_lhs", by_lv_rewrite_tokens()))
}

#' The series observed at fewer times than the frame holds
#'
#' A trend matrix and a rolling origin both need every series on one
#' shared time grid.
#'
#' @param time_vals Time of each row
#' @param series_vals Series of each row
#' @return Character vector of the short series' names
#' @noRd
ragged_series <- function(time_vals, series_vals) {
  checkmate::assert_atomic(time_vals, min.len = 1L)
  checkmate::assert_atomic(series_vals, len = length(time_vals))
  n_time <- length(unique(time_vals))
  per_series <- split(time_vals, series_vals, drop = TRUE)
  short <- vapply(per_series, function(t) length(unique(t)) < n_time,
                  logical(1L))
  names(per_series)[short]
}


#' Refuse series that do not share one time grid
#'
#' The trend is a matrix over every time and series. A row of `data`
#' locates each cell. A series missing a time the others have leaves a
#' cell with no row. The refusal names the short series, the remedy
#' and the trend that takes series at their own times.
#'
#' @param time_vals,series_vals The resolved axes, one per row.
#' @param headline Character headline of the refusal
#' @param remedy Character hint naming what takes ragged series
#' @return `TRUE`, invisibly.
#' @noRd
refuse_ragged_trend_grid <- function(
    time_vals, series_vals,
    headline = "Every series in 'data' must share one time grid.",
    remedy = "'CAR()' takes series observed at their own times.") {
  checkmate::assert_string(headline)
  checkmate::assert_string(remedy)
  short <- ragged_series(time_vals, series_vals)
  if (length(short) == 0L) {
    return(invisible(TRUE))
  }
  n_time <- length(unique(time_vals))
  n_series <- length(unique(series_vals))
  n_cells <- nrow(unique(data.frame(time_vals, series_vals)))
  stop(insight::format_error(c(
    headline,
    x = paste0("Series missing a time the others have: ",
               paste0("'", short, "'", collapse = ", "), "."),
    x = paste0(
      "Got ", n_cells, " time and series cells over ", n_time,
      " times and ", n_series, " series, expected ", n_time * n_series, "."
    ),
    i = "Add a row with an NA response for each time a series missed.",
    i = remedy
  )), call. = FALSE)
}


#' Does the trend hold a state at a time a series has no row for?
#'
#' @param trend_spec An `mvgam_trend`
#' @return The trend type's registered `completes_time_grid`
#' @noRd
trend_completes_time_grid <- function(trend_spec) {
  # A model with no trend has no time grid to fill
  if (is.null(trend_spec$trend)) {
    return(FALSE)
  }
  isTRUE(get_trend_info(trend_spec$trend)$completes_time_grid)
}


#' Refuse t innovations for series observed at their own times
#'
#' A multivariate t draws one mixing scale per occasion. On the union
#' of the series' times, the occasions one series is observed at then
#' change the law of another series' trend. Gaussian innovations give
#' the same law on any grid.
#'
#' @param time_vals,series_vals The resolved axes, one per row.
#' @param trend_spec The trend spec.
#' @return `TRUE`, invisibly.
#' @noRd
refuse_heavy_tails_on_ragged_grid <- function(time_vals, series_vals,
                                              trend_spec) {
  checkmate::assert_list(trend_spec)
  df <- trend_spec$df %||% Inf
  if (!is.na(df) && is_gaussian_df(df)) {
    return(invisible(TRUE))
  }
  short <- ragged_series(time_vals, series_vals)
  if (length(short) == 0L) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "'df' is not supported for series observed at their own times.",
    x = paste0("Series missing a time the others have: ",
               paste0("'", short, "'", collapse = ", "), "."),
    x = "The times of one series would change the trend of another.",
    i = "Add a row with an NA response for each time a series missed.",
    i = "Gaussian innovations take the series as they are."
  )), call. = FALSE)
}


#' Refuse trend covariates for series observed at their own times
#'
#' The trend holds a state for each series at every time of the union
#' grid. The trend's design matrix needs a covariate value in each
#' cell added for a time a series was not observed at.
#'
#' @param time_vals,series_vals The resolved axes, one per row.
#' @param covariates The trend covariates.
#' @return `TRUE`, invisibly.
#' @noRd
refuse_covariates_on_ragged_grid <- function(time_vals, series_vals,
                                             covariates) {
  short <- ragged_series(time_vals, series_vals)
  if (length(covariates) == 0L || length(short) == 0L) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    "Trend covariates need every series observed at every time.",
    x = paste0("Series missing a time the others have: ",
               paste0("'", short, "'", collapse = ", "), "."),
    x = "The trend's design matrix needs a covariate value at those times.",
    i = "Add a row with an NA response for each time a series missed.",
    i = "Or move the covariates to the observation formula."
  )), call. = FALSE)
}


#' Collapse a frame to one row per trend cell
#'
#' A trend cell is one occasion of one series. Under `by = lv_axis()`
#' it is one occasion of one latent factor. The covariates there are
#' time-level. `assert_trend_covariates_constant()` holds them constant
#' within each (time, series) cell. The first value at each time is
#' joined onto every factor.
#'
#' @param data The training frame, carrying its axis attributes.
#' @param covariates The trend covariates.
#' @param time_vals,series_vals The resolved axes.
#' @param has_by_lv,n_lv_for_grain The `by = lv_axis()` grain.
#' @param fill_grid Whether to add a cell for each time a series has no
#'   row at. Trend covariates are refused on such a grid. The added
#'   cells need none.
#' @return A data frame sorted by time, then by series or factor.
#' @noRd
trend_cell_frame <- function(data, covariates, time_vals, series_vals,
                             has_by_lv, n_lv_for_grain,
                             fill_grid = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(covariates)
  checkmate::assert_flag(has_by_lv)
  checkmate::assert_flag(fill_grid)
  columns <- trend_covariate_names(covariates)
  cells <- data %>%
    dplyr::mutate(time = time_vals, series = series_vals) %>%
    dplyr::group_by(.data$time, .data$series) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(columns), dplyr::first),
                     .groups = "drop")
  if (fill_grid) {
    cells <- dplyr::full_join(
      tidyr::expand_grid(time = unique(cells$time),
                         series = unique(cells$series)),
      cells, by = c("time", "series")
    )
  }
  cells <- dplyr::arrange(cells, .data$time, .data$series)
  if (has_by_lv) {
    time_level <- cells %>%
      dplyr::group_by(.data$time) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(columns), dplyr::first),
                       .groups = "drop")
    cells <- expand_factor_axis(n_lv_for_grain, time = time_level$time) %>%
      dplyr::left_join(time_level, by = "time") %>%
      dplyr::arrange(.data$time, .data[[factor_axis_column]])
  }
  remove_mvgam_variables(cells)
}


#' Refuse trend covariates that vary inside one trend cell
#'
#' A trend cell is one occasion of one series, or of one group under a
#' grouped trend. The trend's design matrix has one row per cell.
#' With `lv_axis()` in a factor model the design has one row per time
#' and factor. A cell is then one occasion.
#'
#' @param data The training frame, carrying its axis attributes.
#' @param trend_variables The trend covariates.
#' @param trend_model The trend spec.
#' @param time_vals,series_vals The resolved axes.
#' @param by_time Does the trend design run over (time, factor)?
#' @return `TRUE`, invisibly.
#' @noRd
assert_trend_covariates_constant <- function(data, trend_variables,
                                             trend_model, time_vals,
                                             series_vals,
                                             by_time = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(trend_variables)
  checkmate::assert_list(trend_model)
  checkmate::assert_flag(by_time)
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
  cell <- if (by_time) {
    character(0L)
  } else if (is.null(groupings$gr)) {
    ".cell_series"
  } else {
    c(groupings$gr, groupings$subgr)
  }
  frame <- data
  frame$.cell_time <- time_vals
  frame$.cell_series <- series_vals
  cell_vars <- c(".cell_time", cell)
  # A grouping variable is constant within its own groups. The check
  # leaves it out.
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
    cell_label <- if (by_time) {
      "time"
    } else {
      paste0("(time, ", if (identical(cell, ".cell_series")) "series" else
        paste(cell, collapse = ", "), ") cell")
    }
    stop(insight::format_error(c(
      paste0("A trend covariate varies within one ", cell_label, "."),
      x = paste0("Varying: ",
                 paste0("'", varying, "'", collapse = ", "), "."),
      i = if (by_time) {
        paste0("With 'lv_axis()' in a factor model, the trend has one ",
               "design row per time and factor.")
      } else {
        "The trend has one design row per cell."
      },
      i = "Aggregate the covariate or move it to the observation formula."
    )), call. = FALSE)
  }
  invisible(TRUE)
}


#' Normalise a user `loadings_prior` specification
#'
#' Converts the user-supplied `loadings_prior` into the structured list
#' `make_loadings_prior_stanvars()` consumes. Resolves names against
#' `data2`, checks dimensions against `n_series`, encodes feature
#' columns via `encode_loadings_features()` and checks each pairwise
#' distance via `validate_pairwise_distance()`.
#'
#' The user-facing list accepts these fields:
#' \itemize{
#'   \item `features`: a `data2` name, a matrix or a data frame.
#'   \item `distances`: a `data2` name, a character vector of names, a
#'     single matrix, a named list of matrices or an unnamed list. An
#'     unnamed list is named "dist_1", "dist_2" and so on.
#'   \item `column_shrinkage`: `"iid"` (default) or `"mgp"`.
#'   \item `mgp_a1`, `mgp_a2`: numeric MGP hyperparameters, used only
#'     when `column_shrinkage = "mgp"`. They default to 2 and 4.
#' }
#'
#' @param input The `loadings_prior` argument: `NULL`, `"mgp"` or a
#'   named list
#' @param data2 Named list of objects the prior names, or `NULL`
#' @param data Training data frame
#' @param n_series Expected series count, or `NULL` to skip the check
#' @param series_var Character name of the series column
#' @return `NULL` for `NULL` input. Otherwise a list with
#'   `features_mat` (p x c numeric matrix or `NULL`), `distance_mats`
#'   (named list of p x p matrices, possibly empty), `column_shrinkage`,
#'   `mgp_a1` and `mgp_a2` (`NA` under iid shrinkage), `n_series`,
#'   `N_features_trend` (0 without features) and `n_distances`.
#' @noRd
normalise_loadings_prior <- function(input, data2, data,
                                     n_series = NULL,
                                     series_var = "series") {
  if (is.null(input)) return(NULL)
  checkmate::assert_list(data2, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  checkmate::assert_count(n_series, null.ok = TRUE)
  checkmate::assert_string(series_var)
  # `loadings_prior = "mgp"` is shorthand for
  # `loadings_prior = list(column_shrinkage = "mgp")` with default
  # MGP hyperparameters (a1 = 2, a2 = 4). Pure MGP (no features or
  # distances) is the Bhattacharya & Dunson (2011) parameterisation.
  # It is defined for any positive integer n_lv.
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
    )), call. = FALSE)
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
    )), call. = FALSE)
  }
  series_levels <- argument_series_levels(data, series_var)
  n_series_actual <- length(series_levels)
  if (!is.null(n_series) && n_series != n_series_actual) {
    stop(insight::format_error(c(
      "Mismatch between supplied n_series and series levels.",
      x = paste0(
        "Got n_series = ", n_series, ", levels of '", series_var,
        "' = ", n_series_actual, "."
      )
    )), call. = FALSE)
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
  # `mgp_a2 = 4` shrinks later factors hard enough to truncate them
  # at the tens to low hundreds of series of an ecological community.
  # Weaker settings leave the extra factors active at those sizes.
  # `loadings_prior = list(mgp_a2 = ...)` sets `a2` for sensitivity
  # work.
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
    )), call. = FALSE)
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


#' Resolve the `features` field of `loadings_prior`
#'
#' Accepts a single `data2` name, a matrix or a data frame.
#' `normalise_loadings_prior()` encodes the result with
#' `encode_loadings_features()`.
#'
#' @param features The `features` field, or `NULL`
#' @param data2 Named list of objects the prior names, or `NULL`
#' @return The unencoded matrix or data frame, or `NULL`
#' @noRd
resolve_features_input <- function(features, data2) {
  checkmate::assert_list(data2, null.ok = TRUE)
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
      )), call. = FALSE)
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
  )), call. = FALSE)
}


#' Resolve the `distances` field of `loadings_prior`
#'
#' Unnamed list entries are named "dist_1", "dist_2" and so on. A
#' single inline matrix is named "dist_1".
#'
#' @param distances The `distances` field, or `NULL`
#' @param data2 Named list of objects the prior names, or `NULL`
#' @return Named list of pairwise distance matrices, possibly empty
#' @noRd
resolve_distances_input <- function(distances, data2) {
  checkmate::assert_list(data2, null.ok = TRUE)
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
      )), call. = FALSE)
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
      )), call. = FALSE)
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
  )), call. = FALSE)
}


#' Check a normalised loadings-prior spec for consistency
#'
#' `make_loadings_prior_stanvars()` calls this before emitting
#' stanvars. The count fields must agree with the matrices they count.
#'
#' @param spec A spec from `normalise_loadings_prior()`
#' @return `NULL`, invisibly, or an error
#' @noRd
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
  # A spec with neither a kernel nor column shrinkage has no structure
  # to emit. The emitter's kernel branch would write
  # `Z ~ multi_normal_cholesky(..., L_Phi_loadings)` against an
  # undeclared `L_Phi_loadings`. `stanc` would fail on it.
  # `normalise_loadings_prior()` refuses to build such a spec. This
  # check covers any other builder.
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


#' Refuse distance names that start with `dist_`
#'
#' Unnamed inline list entries take the names `dist_1`, `dist_2` and so
#' on. A user name starting with `dist_` could collide with them and
#' shadow a Stan variable. The refusal comes here, before any Stan
#' compile failure.
#'
#' @param nms Character vector of distance names
#' @return `NULL`, invisibly, or an error
#' @noRd
assert_distance_names_unreserved <- function(nms) {
  checkmate::assert_character(nms)
  bad <- nms[grepl("^dist_", nms)]
  if (length(bad) > 0L) {
    stop(insight::format_error(c(
      paste0(
        "'loadings_prior$distances' names cannot start with ",
        "'dist_'."
      ),
      x = paste0("Got: ", paste0("'", bad, "'", collapse = ", "), ".")
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' Refuse MGP column shrinkage on a trend that cannot apply it
#'
#' The column scale reaches the model through `sigma_trend`, which the
#' shared innovation system builds. A trend needs that system and
#' factor support to take it.
#'
#' @param loadings_prior_spec Normalised loadings-prior spec, or NULL.
#' @param trend Character trend name from the trend spec.
#' @return `NULL`, invisibly, or an error
#' @noRd
assert_column_shrinkage_compatible <- function(loadings_prior_spec, trend) {
  checkmate::assert_list(loadings_prior_spec, null.ok = TRUE)
  if (is.null(loadings_prior_spec) || is.null(trend)) {
    return(invisible(NULL))
  }
  if (!loadings_spec_traits(loadings_prior_spec)$mgp) {
    return(invisible(NULL))
  }
  trend_nm <- toupper(as.character(trend)[1L])
  if (trend_property(trend_nm, "shared_innovations") &&
      trend_property(trend_nm, "supports_factors")) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    paste0(
      "Multiplicative gamma process shrinkage applies to ",
      registered_trend_constructors(
        c("shared_innovations", "supports_factors")
      ),
      " trends."
    ),
    x = paste0("Got '", trend_nm, "()'.")
  )), call. = FALSE)
}


#' Refuse a structured loadings prior on fixed loadings
#'
#' A `trend_map_Z` of `NULL` means every loading is free. Both the
#' `n_lv` route and the all-NA `jsdgam()` mask give it, since
#' `normalise_trend_map_on_specs()` collapses that mask to `NULL`. A
#' structured prior applies there. A partly fixed Z samples its free
#' entries element-wise under an iid student_t prior. The matrix-normal
#' `loadings_prior` acts on the assembled Z in transformed parameters.
#' The two priors would overlap. A fully fixed Z has no free entries
#' for the prior to act on.
#'
#' @param loadings_prior_spec Normalised loadings-prior spec, or `NULL`
#' @param trend_map_Z The fixed loadings matrix, or `NULL`
#' @return `NULL`, invisibly, or an error
#' @noRd
assert_loadings_prior_compatible <- function(loadings_prior_spec,
                                             trend_map_Z) {
  checkmate::assert_list(loadings_prior_spec, null.ok = TRUE)
  checkmate::assert_matrix(trend_map_Z, null.ok = TRUE)
  if (is.null(loadings_prior_spec) || is.null(trend_map_Z)) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    "'loadings_prior' requires every 'trend_map' entry to be NA.",
    x = paste0(
      "The 'trend_map' fixes ", sum(!is.na(trend_map_Z)), " of ",
      length(trend_map_Z), " entries."
    )
  )), call. = FALSE)
}


#' Require a `trials()` addition term for `com_binomial()`
#'
#' `com_binomial()` takes its per-row binomial denominator from the
#' `trials` array. brms emits that array only when the response has a
#' `trials()` addition term. Routing the denominator through brms
#' keeps it aligned with the response when rows are dropped. Without
#' the term `trials` is undefined. stanc would then fail on a name the
#' user never wrote. The refusal here names the omission.
#'
#' @param formula The observation formula, possibly a `brmsformula`.
#' @return `NULL`, invisibly, or an error naming the absent `trials()`
#'   term
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


#' Refuse a sub-formula on a dpar whose link cannot respect its bounds
#'
#' A custom family bounds a scalar dpar through its declaration. A
#' sub-formula replaces the scalar with a linear predictor, which only
#' a link maps into the bounds. An identity link on a dpar bounded on
#' both sides leaves the predictor free to cross them. The likelihood
#' then rejects most of the parameter space.
#'
#' @param formula One response's observation formula
#' @param family That response's family object
#' @return `NULL`, invisibly. Raises an error for an unusable dpar.
#' @noRd
assert_modelled_dpar_links <- function(formula, family) {
  for (dp in modelled_dpars(formula, family)) {
    lb <- as.numeric(family$lb[[dp]] %||% NA)
    ub <- as.numeric(family$ub[[dp]] %||% NA)
    link <- family[[paste0("link_", dp)]] %||% "identity"
    if (identical(link, "identity") && is.finite(lb) && is.finite(ub)) {
      stop(insight::format_error(c(
        paste0("A sub-formula on '", dp, "' is not supported for family '",
               resolve_family_name(family), "'."),
        x = paste0("'", dp, "' must stay in [", lb, ", ", ub, "]."),
        x = paste0("An identity link on '", dp, "' lets a linear ",
                   "predictor leave that range."),
        i = paste0("Remove '", dp, " ~ ...' to estimate one '", dp,
                   "' per response.")
      )), call. = FALSE)
    }
  }
  invisible(NULL)
}


#' Every call to one function in an expression
#'
#' Walks the parse tree. A bare symbol sharing the function's name is
#' not a match. The search does not descend into a matched call's own
#' arguments.
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
#' expression. A model description shows the formula as the user
#' wrote it. Evaluating an argument such as `k = kk` would need every
#' object it names to exist in the describing session. A failed
#' evaluation would be reported as the default the user had
#' overridden.
#'
#' @param arg One argument of a call in a formula.
#' @return A character string.
#' @noRd
formula_arg_text <- function(arg) {
  if (is.character(arg) && length(arg) == 1L) return(arg)
  paste(deparse(arg), collapse = "")
}


#' The arguments of a term's call, as the user wrote them
#'
#' @param call A call such as `s(x, k = 5)` or `gp(x, by = g)`
#' @return List of two elements. `positional` holds the text of each
#'   unnamed argument. `named(name, default)` returns the text of a
#'   named argument, or `default` when the call omits it.
#' @noRd
call_args_as_written <- function(call) {
  checkmate::assert_class(call, "call")
  args <- as.list(call)[-1L]
  arg_names <- names(args) %||% rep("", length(args))
  list(
    positional = unname(vapply(args[arg_names == ""], formula_arg_text,
                               character(1L))),
    named = function(name, default) {
      if (name %in% arg_names) formula_arg_text(args[[name]]) else default
    }
  )
}


#' The settings of a `gp()` term, as written
#'
#' A setting left out takes brms's default.
#'
#' @param call A `gp()` call
#' @return List holding `vars`, `k`, `by`, `cov` and the brms
#'   coefficient stem `coef`
#' @noRd
gp_call_to_spec <- function(call) {
  args <- call_args_as_written(call)
  vars <- args$positional
  if (length(vars) == 0L) {
    stop(insight::format_error(c(
      "A 'gp()' term needs at least one variable.",
      x = paste0("Got '", deparse1(call), "'.")
    )), call. = FALSE)
  }
  list(
    vars = vars,
    k = args$named("k", NA_character_),
    by = args$named("by", NA_character_),
    cov = args$named("cov", "exp_quad"),
    coef = paste0("gp", paste(vars, collapse = ""))
  )
}


#' Locate an addition term in a model formula
#'
#' Addition terms belong to the response side. Only the left-hand side
#' is searched. A covariate sharing the term's name must never match.
#' Walking the parse tree matches the call and skips a bare symbol.
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


#' Are these forecast times the ones the trend can step to?
#'
#' A discrete-time trend advances one step per time point. Its forecast
#' horizon is the number of steps taken from the last observed state.
#' That equals the number of rows supplied only when the forecast times
#' continue the training series without a gap and at its own spacing.
#' Given `t = 41:42` after training ends at `t = 30`, such a trend
#' would take two steps where twelve are needed. Its forecast spread
#' would match a horizon of two for a horizon of twelve, with no
#' message about the understated uncertainty.
#'
#' `CAR()` and `ZMVN()` are exempt. `CAR()` passes the elapsed gap to
#' its kernel. `ZMVN()` has no temporal structure to step through. The
#' registry's `requires_regular_intervals` entry marks which trends
#' step. The fit-time and `mvgam_data()` checks use the same entry.
#'
#' @param fc_times Named list of forecast times per series.
#' @param object The fitted `mvgam` object. Its axis record gives the
#'   grid and the spacing. Its trend specification states whether the
#'   trend steps.
#' @return `TRUE`, invisibly, or an error
#' @noRd
assert_forecast_times_steppable <- function(fc_times, object) {
  checkmate::assert_list(fc_times, null.ok = TRUE)
  checkmate::assert_class(object, "mvgam")
  trend_spec <- first_trend_spec(object)
  if (!trend_requires_regular_intervals(trend_spec)) {
    return(invisible(TRUE))
  }
  # Every series shares one latent time grid. The trend steps along
  # that grid. A check on each series' own observed times would refuse
  # a valid forecast whenever a series ends on unobserved responses.
  # brms drops those rows and the series looks short. The check would
  # find a gap the latent state does not have. mvgam asks users to pad
  # series that way, which makes the shape common.
  #
  # The grid is the fit's recorded axis. On a frame whose responses
  # are its series, each response's training rows hold its observed
  # occasions alone. A grid gathered from them would make a wide frame
  # padded at the end look shorter than its latent state. A forecast
  # starting inside the grid would pass. The trend would then step on
  # from the end of the grid under times it had not reached.
  #
  # The horizon is resolved from the last *observed* occasion. A padded
  # series must not be forecast from an occasion it was never seen at.
  # On a padded frame the grid and the horizon disagree about the
  # occasions between the last response and the end of the grid. The
  # message below names the grid. It does not call the grid's last
  # position an observation.
  axis_time <- mvgam_axes(object)$time
  past <- as.numeric(axis_time$values)
  step <- axis_time$step
  if (length(past) < 2L) return(invisible(TRUE))
  # The spacing comes from the fit's record. A step derived here as
  # well could disagree with it. `NA` marks an irregular grid with no
  # single step to continue. `NULL` marks a fit that recorded no step.
  # Neither defines the next occasion and neither refuses one.
  if (is.null(step) || is.na(step)) return(invisible(TRUE))
  # The step keeps its recorded value. Coercion to integer would turn
  # a grid spaced by half a unit into a step of zero.
  step <- as.numeric(step)
  end <- past[length(past)]
  for (lv in names(fc_times)) {
    # The comparison uses the occasions the user supplied. Integer
    # truncation would make a grid spaced by less than one unit equal
    # itself shifted. It would make a correct grid spaced by more than
    # one unequal. The expected occasions are arithmetic on a recorded
    # step. They land within floating-point noise of the frame's
    # values. The tolerance allows for that noise.
    fut <- sort(unique(as.numeric(fc_times[[lv]])))
    if (!length(fut)) next
    steps <- (fut - end) / step
    inside <- fut[steps < 1 - 1e-8]
    off_step <- setdiff(fut[abs(steps - round(steps)) > 1e-8], inside)
    expected <- end + step * seq_len(max(0L, round(max(steps))))
    skipped <- expected[!vapply(expected, function(t) {
      any(abs(fut - t) <= 1e-8 * max(1, abs(t)))
    }, logical(1L))]
    if (!length(inside) && !length(off_step) && !length(skipped)) next
    stop(insight::format_error(c(
      paste0(
        "'newdata' must continue the training series for a '",
        get_trend_name(trend_spec), "' trend."
      ),
      x = paste0("The training grid runs to time ", end, " in steps of ",
                 step, "."),
      if (length(inside)) {
        c(x = cli::format_inline(
          "Series '{lv}' has {cli::qty(length(inside))}time{?s} {inside} ",
          "inside the training grid."
        ))
      },
      if (length(skipped)) {
        c(x = cli::format_inline(
          "Series '{lv}' skips {cli::qty(length(skipped))}time{?s} {skipped}."
        ))
      },
      if (length(off_step)) {
        c(x = cli::format_inline(
          "Series '{lv}' has {cli::qty(length(off_step))}time{?s} ",
          "{off_step} off the step of {step}."
        ))
      },
      i = "Supply one row per step, through the last time to forecast.",
      i = "'CAR()' models a trend over irregular times.",
      if (length(inside)) {
        c(i = "'hindcast()' covers times inside the training grid.")
      }
    )), call. = FALSE)
  }
  invisible(TRUE)
}



#' Map each observation row onto its row of the trend design
#'
#' The generated program indexes a row's latent state as
#' `trend[obs_trend_time[n], obs_trend_series[n]]`. That cell's own
#' mean is `mu_trend[times_trend[t, s]]`. Composing the two gives the
#' row of `X_trend` that enters observation row `n`. R code that needs
#' this mapping calls this function.
#'
#' @param standata The assembled Stan data list.
#' @param resp Response suffix, `""` on a univariate model.
#' @return Integer vector, one trend-design row per observation row, or
#'   `NULL` where the arrays it needs are absent.
#' @noRd
obs_rows_to_trend_rows <- function(standata, resp = "") {
  checkmate::assert_list(standata)
  checkmate::assert_string(resp)
  sfx <- predictor_suffix(resp)
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
