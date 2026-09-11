#' brms Ecosystem Integration for mvgam
#'
#' @description
#' brms integration and validation for mvgam models.
#' This file consolidates brms setup, formula validation, multivariate
#' parsing, and nonlinear model support.
#'
#' @section Architecture:
#' The brms integration system provides compatibility across layers:
#' - **Setup Layer**: Lightweight brms model setup for rapid prototyping
#' - **Validation Layer**: Formula validation and syntax checking
#' - **Parsing Layer**: Multivariate and nonlinear formula interpretation
#' - **Extension Layer**: Functionality beyond base brms capabilities

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Decide if a trend formula should include an intercept (mvgam convention)
#' 
#' @description
#' Analyzes trend formulas to determine intercept inclusion following mvgam 
#' conventions rather than R's default formula behavior. Trend constructors
#' like RW() and AR() default to no intercept unless explicitly specified.
#' 
#' @param formula A formula object for trend specification
#' @return Logical indicating whether intercept should be included
#' @noRd
should_trend_formula_have_intercept <- function(formula) {
  # Input validation following mvgam standards
  checkmate::assert_formula(formula)
  
  # Get formula terms for analysis
  formula_terms <- terms(formula)
  
  # Check R's built-in intercept attribute
  has_r_intercept <- attr(formula_terms, 'intercept') == 1
  
  if (!has_r_intercept) {
    # Explicitly excluded via ~ -1 or ~ 0 
    return(FALSE)
  }
  
  # For formulas with R intercept = 1, check if intercept was explicit
  # Parse right-hand side of formula to detect explicit "+1" vs default behavior
  rhs_terms <- attr(formula_terms, "term.labels")
  
  # If no terms on RHS beyond intercept, this is ~ 1 (explicit intercept only)
  if (length(rhs_terms) == 0) {
    return(TRUE)
  }
  
  # Check if "1" is explicitly listed as a term (~ 1 + RW())
  if ("1" %in% rhs_terms) {
    return(TRUE)
  }
  
  # Pure trend constructor formulas (~ RW(), ~ AR()) follow the mvgam
  # convention of no intercept for trend innovations
  return(FALSE)
}

#' Collect the brms code-generation options into one list
#'
#' @param knots Named list of knot positions for smooth terms, or NULL.
#' @param sample_prior One of "no", "yes" or "only".
#' @param drop_unused_levels Whether to drop unused factor levels.
#' @param normalize Whether brms normalises its sampling statements.
#' @return A named list carrying all four values.
#' @noRd
mvgam_codegen_options <- function(knots = NULL, sample_prior = "no",
                                  drop_unused_levels = TRUE,
                                  normalize = TRUE) {
  checkmate::assert_list(knots, names = "named", null.ok = TRUE)
  checkmate::assert_choice(sample_prior, c("no", "yes", "only"))
  checkmate::assert_flag(drop_unused_levels)
  checkmate::assert_flag(normalize)
  list(
    knots = knots,
    sample_prior = sample_prior,
    drop_unused_levels = drop_unused_levels,
    normalize = normalize
  )
}

# Which options each brms generator declares. Both sets are derived
# from the one options list rather than written out again, so an
# option added to `mvgam_codegen_options()` is routed without a second
# edit. `brms::standata()` takes no `normalize`: whether the sampling
# statements carry their normalising constants is a property of the
# program alone.
mvgam_codegen_stancode_options <- names(formals(mvgam_codegen_options))
mvgam_codegen_standata_options <- setdiff(
  mvgam_codegen_stancode_options, "normalize"
)

#' Build the options list from a call's dots
#'
#' `get_prior()` hands its dots to brms for the observation side. The
#' trend side is built by mvgam rather than brms, so it has to be given
#' the same options explicitly or the two halves of one prior table
#' describe different models.
#'
#' @param dots A list of named arguments, usually `list(...)`.
#' @return A list from `mvgam_codegen_options()`.
#' @noRd
codegen_from_dots <- function(dots) {
  checkmate::assert_list(dots)
  named <- intersect(names(dots), names(formals(mvgam_codegen_options)))
  do.call(mvgam_codegen_options, dots[named])
}

#' Take the options one brms generator declares
#'
#' @param codegen A list from `mvgam_codegen_options()`, or NULL.
#' @param accepted Character vector of option names the generator takes.
#' @return A named list ready to splice into the brms call.
#' @noRd
codegen_args_for <- function(codegen, accepted) {
  checkmate::assert_list(codegen, names = "named", null.ok = TRUE)
  checkmate::assert_character(accepted, min.len = 1, any.missing = FALSE)
  if (is.null(codegen)) {
    return(list())
  }
  codegen[intersect(names(codegen), accepted)]
}

# =============================================================================
# SECTION 1: BRMS LIGHTWEIGHT SETUP SYSTEM
# =============================================================================
# WHY: Lightweight brms setup enables rapid model prototyping and validation
# without full compilation overhead. This is essential for mvgam's two-stage
# assembly system where brms provides the foundation and mvgam adds trends
# through stanvars injection without modifying brms internals.
#' Lightweight brms Setup with Prior Support
#'
#' @param formula Formula or brms formula object for model specification
#' @param data Data frame containing model variables
#' @param family Response distribution family (default: gaussian())
#' @param trend_formula Optional trend formula specification (default: NULL)
#' @param stanvars Optional brms stanvars object (default: NULL)
#' @param prior A brmsprior object or NULL. Prior specifications
#'   for model parameters. Defaults to NULL.
#' @param is_trend_setup Logical. If TRUE, validates trend covariates and
#'   reduces data to one row per (time, series) combination. Default: FALSE.
#' @param response_vars Character vector of response variable names for 
#'   trend validation. Required when is_trend_setup = TRUE.
#' @param time_var Character name of time variable. Default: "time".
#' @param series_var Character name of series variable. Default: "series".
#' @param codegen A list from `mvgam_codegen_options()` holding the brms
#'   code-generation options, or NULL to take brms's own defaults.
#' @param ... Additional arguments passed to brms functions
#' @noRd
setup_brms_lightweight <- function(formula, data, family = gaussian(),
                                   trend_formula = NULL, stanvars = NULL,
                                   prior = NULL,
                                   data2 = NULL,
                                   is_trend_setup = FALSE,
                                   response_vars = NULL,
                                   time_var = "time",
                                   series_var = "series",
                                   codegen = NULL,
                                   ...) {
  # Accept both regular formulas and brms formula objects
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    checkmate::check_class(formula, "brmsformula"),
    combine = "or"
  )
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert(
    checkmate::check_null(prior), 
    checkmate::check_class(prior, "brmsprior"),
    combine = "or"
  )
  # Validation for new parameters
  checkmate::assert_logical(is_trend_setup, len = 1)
  checkmate::assert_list(codegen, names = "named", null.ok = TRUE)
  checkmate::assert_character(response_vars, null.ok = TRUE)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  if (!is.null(trend_formula)) {
    checkmate::assert(
      inherits(trend_formula, "formula") ||
      inherits(trend_formula, "brmsformula") ||
      inherits(trend_formula, "bform") ||
      is.list(trend_formula),
      .var.name = "trend_formula"
    )
  }

  # Trend context handling - validate and reduce data if this is trend setup
  if (is_trend_setup && !is.null(trend_formula)) {
    # Use consolidated validation and data extraction with metadata capture
    result <- extract_trend_data(
      data, trend_formula, time_var, series_var,
      response_vars = response_vars, .return_metadata = TRUE
    )
    data <- result$trend_data
    trend_metadata <- result$metadata

    # brms cannot evaluate mvgam trend constructors (AR, RW, VAR,
    # PW, CAR, ZMVN) as R functions in its model frame -- they
    # are mvgam DSL, not formula terms. The fitting pipeline
    # normally strips them via `extract_and_validate_trend_components()`
    # before calling here, but the stancode() / standata() entry
    # points reach this branch with the raw trend_formula. Run
    # `parse_trend_formula()` in parsing-only mode (no data) to
    # obtain the constructor-free `base_formula` and hand THAT to
    # brms.
    formula <- parse_trend_formula(trend_formula)$base_formula
  }

  # Handle trend formulas without response variables
  # Only apply this logic to regular formula objects, not brms formula objects
  if (inherits(formula, "formula") &&
      !inherits(formula, c("brmsformula", "mvbrmsformula", "bform"))) {
    # Check if formula lacks response variable (e.g., ~ 1, ~ x + y).
    # `deparse()` on a long multi-term formula returns multiple
    # lines, so collapse to a single string before the scalar
    # grepl checks.
    formula_chr <- paste(deparse(formula), collapse = " ")
    if (!grepl("~.*~", formula_chr) && grepl("^\\s*~", formula_chr)) {
      # This is a trend formula without response variable
      # Add fake trend_y response variable following mvgam pattern
      data <- data
      data$trend_y <- rnorm(nrow(data))

      # Update formula to include trend_y response
      # Use mvgam-aware intercept detection for trend formulas
      has_intercept_check <- should_trend_formula_have_intercept(formula)
      
      # Check formula structure. Collapse multi-line deparse so
      # the scalar `||` comparison below sees one string.
      rhs_str <- paste(
        deparse(rlang::f_rhs(formula)), collapse = " "
      )

      if (rhs_str == "0" || rhs_str == "-1") {
        # Direct assignment for clean no-intercept formula
        formula <- as.formula("trend_y ~ 0")
      } else if (has_intercept_check) {
        # Has intercept: keep intercept to get Intercept_trend prior
        formula <- update(formula, trend_y ~ .)
      } else {
        # No intercept: explicitly remove intercept
        formula <- update(formula, trend_y ~ 0 + .)
      }
    }
  }

  # Validate brms formula compatibility. Skip the trend-constructor
  # rule when we are inside the trend setup branch (formula was
  # rewritten to `trend_formula` above and is legitimately allowed
  # to contain AR()/VAR()/RW() etc.).
  if (!is_trend_setup) {
    validate_obs_formula_brms(formula)
  }

  # Parse and validate trend formula if provided
  trend_specs <- NULL
  if (!is.null(trend_formula)) {
    trend_specs <- parse_multivariate_trends(formula, trend_formula)
  }

  # An observation formula with no coefficient (`y ~ 0`) is given a
  # pinned zero column, as `inject_obs_zero_placeholder()` explains.
  # Only applied to the obs side; the trend side goes through mvgam's
  # own stancode rewriter, which strips brms's parameters declarations
  # and would leave the placeholder's pin assignment orphaned.
  if (!isTRUE(is_trend_setup)) {
    injected <- inject_obs_zero_placeholder(formula, data, prior)
    formula <- injected$formula
    data    <- injected$data
    prior   <- injected$prior
  }

  # Use mock backend for rapid setup (creates brmsfit object
  # needed for prediction).
  #
  # `data2` is forwarded explicitly so brms specials that reference
  # objects living outside `data` (e.g. `car()` adjacency matrices,
  # `cov_ranef()` covariance matrices) can resolve their lookups.
  #
  # `threads` is forwarded so brms emits its
  # `partial_log_lik_lpmf` + `reduce_sum` instrumentation for
  # brms-native families. Without this, the mock fit produces
  # unthreaded stancode and the user-requested threading is
  # silently dropped on every brms-native fit. Closure-unit
  # families thread via their own `partial_sum_*_lpmf`
  # stanvars and do not depend on this path.
  dots <- list(...)
  # Forward threads only when the user actually asked for > 1
  # thread. Default `threads = getOption("mc.cores", 1)` upstream
  # would otherwise emit brms's `partial_log_lik_lpmf` wrapper on
  # every fit (correctness-preserving but unnecessary compile-time
  # overhead and a behaviour change vs prior releases).
  raw_threads <- dots$threads
  if (inherits(raw_threads, "brmsthreads")) {
    raw_threads <- raw_threads$threads
  }
  brm_threads <- if (is.numeric(raw_threads) &&
                       isTRUE(raw_threads > 1)) {
    as.integer(raw_threads)
  } else {
    NULL
  }
  # The code-generation options ride along here and again in every
  # downstream regeneration, so the mock fit, the assembled Stan
  # program and the assembled Stan data are all built under the same
  # basis expansions, factor levels and prior-only setting.
  mock_setup <- do.call(brms::brm, c(
    list(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars,
      prior = prior,
      data2 = data2,
      threads = brm_threads,
      backend = "mock",
      mock_fit = 1,
      rename = FALSE
    ),
    codegen_args_for(codegen, mvgam_codegen_stancode_options)
  ))

  # Add version metadata to prevent restructure() from calling update()
  # standata() and prepare_predictions() call restructure() which checks version
  # and attempts update() if version is NULL or < "1.0". Multivariate models
  # update() throws an error. Adding current brms version prevents this.
  mock_setup$version <- list(brms = utils::packageVersion("brms"))

  # NB: the placeholder column is retained in `data` (and in
  # `mock_setup$data`) because mvgam's stancode regenerator,
  # brms's prediction helpers and any post-fit code that re-runs
  # `validate_data()` against the rewritten formula need the
  # column to satisfy the formula. Hiding the placeholder from the
  # user happens downstream, where a reader meets it: the
  # stored prior table (`assemble_stored_prior_table()`), the
  # variables list (`variables.mvgam()`), the term list
  # (`insight::find_predictors()`) and the displayed formula in
  # `summary.mvgam()`. None of those filters may run on a table the
  # code generator reads, or the pin holding the placeholder at
  # zero leaves the program with them.

  # Extract key components for mvgam integration. `data2` is retained
  # on the setup so downstream Stan-code regenerators
  # (`generate_base_stancode_with_stanvars`) can forward it to brms
  # for specials whose lookups live outside `data` (e.g. `car()`,
  # `cov_ranef()`).
  setup_components <- list(
    formula = formula,
    trend_formula = trend_formula,
    data = data,
    data2 = data2,
    family = family,
    stanvars = stanvars,
    threads = brm_threads,  # honoured by downstream make_stancode
    codegen = codegen,      # read again by the downstream generators
    stancode = brms::stancode(mock_setup),
    standata = brms::standata(mock_setup),
    prior = extract_prior_from_setup(mock_setup, codegen),
    brmsterms = extract_brmsterms_from_setup(mock_setup),
    brmsfit = mock_setup,  # Keep the mock brmsfit for prediction
    trend_specs = trend_specs,  # Include parsed trend specifications
    # Pass through any trend metadata captured upstream so
    # downstream stancode generation can reuse it.
    trend_metadata = if (exists("trend_metadata")) trend_metadata else NULL,
    setup_time = system.time({})[["elapsed"]]
  )

  # Validate extracted components
  validate_setup_components(setup_components)

  return(setup_components)
}


#' Extract Prior Information from brms Setup
#' @param setup_object brms setup object
#' @param codegen A list from `mvgam_codegen_options()`, or NULL. The
#'   prior table is keyed by the coefficients the design matrix holds,
#'   so it has to be merged under the same knots and factor levels the
#'   Stan data is built under.
#' @return Data frame of prior specifications
#' @noRd
extract_prior_from_setup <- function(setup_object, codegen = NULL) {
  # Always return the full merged prior table: brms defaults for
  # every parameter class, with user-supplied rows overlaid on top
  # and tagged `source = "user"`. brms::validate_prior() is the
  # canonical merge engine; calling it here makes prior_summary()
  # on an mvgam fit match the brmsfit convention exactly.
  # The user prior may be NULL (no overrides), in which case
  # validate_prior just returns the default table.
  prior <- do.call(brms::validate_prior, c(
    list(
      prior   = setup_object$prior,
      formula = setup_object$formula,
      data    = setup_object$data,
      family  = setup_object$family,
      data2   = setup_object$data2
    ),
    codegen_args_for(codegen, mvgam_codegen_standata_options)
  ))
  # The placeholder's `constant(0)` row stays in this table. It is
  # read again by `generate_base_stancode_with_stanvars()`, which
  # regenerates the program mvgam actually compiles, so dropping it
  # here dropped the pin from the model rather than from the
  # display: a formula that declined an intercept was fitted with a
  # free one, lying on an exact ridge against any trend intercept.
  # `assemble_stored_prior_table()` hides the row where the user
  # reads it.
  prior
}


# Reserved column name for the empty-obs-formula placeholder,
# referenced by `inject_obs_zero_placeholder()` (the injection
# site) and by any post-fit code that needs to filter the
# placeholder out of user-visible output.
#'@noRd
MVGAM_EMPTY_OBS_PLACEHOLDER <- ".mvgam_empty_obs"


# Internal: the responses whose observation formula names the
# placeholder, as brms keys them, with `""` standing for the one
# response of a univariate formula. The rewritten formula decides
# whether a frame or a prior table needs the column and its pin, and
# the stored frame need not carry the column.
#'@noRd
obs_placeholder_resps <- function(formula) {
  if (inherits(formula, "mvgam_formula")) formula <- formula$formula
  mv <- inherits(formula, "mvbrmsformula")
  forms <- if (mv) formula$forms else list(formula)
  names_it <- vapply(forms, function(f) {
    main <- obs_arm_main_formula(f)
    inherits(main, "formula") &&
      MVGAM_EMPTY_OBS_PLACEHOLDER %in% all.vars(main)
  }, logical(1L))
  if (mv) {
    return(vapply(forms[names_it], function(f) f$resp, character(1L)))
  }
  if (names_it) "" else character(0L)
}


# Internal: does this observation formula name the placeholder?
#'@noRd
obs_formula_needs_placeholder <- function(formula) {
  length(obs_placeholder_resps(formula)) > 0L
}


# Internal: the main formula of one response's observation model. A
# `brmsformula` keeps it in `$formula`, beside its distributional and
# non-linear sub-formulas; a plain formula is its own main formula.
#'@noRd
obs_arm_main_formula <- function(f) {
  if (inherits(f, "brmsformula")) f$formula else f
}


# Internal: stamp the placeholder column on a frame that is about
# to reach brms, when the fit's rewritten formula names it.
# Without this, brms's `validate_data()` rejects predict / forecast
# / posterior_predict / pp_check frames, whether the user assembled
# them or mvgam resolved them from its own training data.
#'@noRd
ensure_obs_placeholder <- function(data, object) {
  if (is.null(data)) return(data)
  if (MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(data)) return(data)
  if (!obs_formula_needs_placeholder(object$formula)) return(data)
  data[[MVGAM_EMPTY_OBS_PLACEHOLDER]] <- 0
  data
}


# Internal: take the placeholder back out of a frame the user
# reads. `mvgam()` writes it into the frame brms is given, and the
# fit stores that frame, so the column would otherwise sit beside
# the user's own columns in `fit$data` and everything drawn from
# it.
#'@noRd
drop_obs_placeholder <- function(data) {
  if (!is.data.frame(data)) return(data)
  keep <- setdiff(names(data), MVGAM_EMPTY_OBS_PLACEHOLDER)
  data[, keep, drop = FALSE]
}


# Internal: strip the ` + .mvgam_empty_obs` placeholder term
# from a formatted formula string before it goes to the user.
# Used by `summary.mvgam()` so the displayed obs formula matches
# what the user actually passed (`y ~ 0`), not the rewritten
# pinned-coefficient form that brms received. `format()` on a
# canonical formula always emits ` + <term>` with single spaces;
# matching that literal with `fixed = TRUE` avoids any regex
# metacharacter ambiguity around the leading `.` in the
# placeholder name.
#'@noRd
strip_empty_obs_placeholder <- function(formula_str) {
  checkmate::assert_character(formula_str, any.missing = FALSE)
  pat <- paste0(" + ", MVGAM_EMPTY_OBS_PLACEHOLDER)
  sub(pat, "", formula_str, fixed = TRUE)
}


# Internal: deparse a model formula for display. `format()` on a
# `brmsformula` returns one string per list element (`formula`,
# `pforms`, `family`, `resp`, ...), so printing with `sep = ""`
# glues the trailing `NULL`s onto the formula text. Reaching the
# plain formula first keeps an addition-term model displaying as
# `y | trials(n) ~ 1`. Stripping per formula rather than on a
# joined string matters for multivariate models, where
# `strip_empty_obs_placeholder()` would otherwise only clear the
# placeholder from the first response.
#' @noRd
format_model_formula <- function(formula) {
  # A distributional model carries one extra formula per parameter in
  # `pforms`. brms prints each on its own line under the response
  # formula, and a reader who wrote `sigma ~ x` needs to see it in the
  # summary, so they are collected here before `formula` is narrowed
  # to the response formula alone.
  dpar_lines <- unlist(lapply(formula$pforms, format), use.names = FALSE)
  if (!is.null(formula$formula)) {
    formula <- formula$formula
  }
  c(strip_empty_obs_placeholder(format(formula)), dpar_lines)
}


#' Format the link of every distributional parameter a family carries
#'
#' brms names a link per distributional parameter, keeping the mean's on
#' `family$link` and the rest on `family$link_<dpar>`. A header that
#' reports only the mean leaves a reader of a `sigma ~ x` or `nu ~ x`
#' block with no way to tell which scale those coefficients are on.
#' Families that declare no distributional parameters, such as Poisson,
#' still report the single mean link.
#'
#' @param family A family object
#' @return A single string of the form `"mu = logit; nu = identity"`
#'
#' @noRd
format_family_links <- function(family) {
  dpars <- family$dpars
  if (length(dpars) == 0) {
    return(paste0("mu = ", family$link))
  }
  links <- vapply(dpars, function(dpar) {
    family[[paste0("link_", dpar)]] %||% family$link
  }, character(1))
  paste0(dpars, " = ", links, collapse = "; ")
}


# Internal: `formula = y ~ 0` (or `~ -1`) is a legitimate
# state-space pattern where the entire linear predictor flows
# through the trend formula. brms cannot build it under a family
# with no distributional parameter of its own, such as poisson or
# bernoulli: the formula then leaves no parameter class at all, and
# brms's prior pipeline stamps `source = "default"` onto a 0-row
# brmsprior and stops with `replacement has 1 row, data has 0`.
#
# The injection adds a column of zeros (`MVGAM_EMPTY_OBS_PLACEHOLDER`)
# to `data`, rewrites the formula to `y ~ 0 + <placeholder>` and holds
# the coefficient at zero with `prior(constant(0), class = "b",
# coef = <placeholder>)`. brms then declares `b` in `transformed
# parameters` as `b[1] = 0`, the `parameters` block keeps only the
# family's own scale, and the linear predictor is the user's `y ~ 0`.
# The rewrite runs under every family, and brms meets one formula
# shape whichever family is chosen.
#
# The pin only holds if it survives to the table
# `generate_base_stancode_with_stanvars()` regenerates the
# compiled program from. Filtering the row anywhere upstream of
# that leaves the placeholder free, and against a trend intercept
# the two lie on an exact ridge.
#
# Detection is strict: only when the formula's RHS has zero term
# labels and zero intercept (empty model matrix). Formulas with
# even one term (e.g. `y ~ 1`, `y ~ x`) pass through unchanged. A
# `brmsformula` is rewritten in its main formula, and a multivariate
# formula in each response's main formula that declines every term,
# with a pin per rewritten response.
#'@noRd
inject_obs_zero_placeholder <- function(formula, data, prior) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    combine = "or",
    .var.name = "formula"
  )
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert(
    checkmate::check_null(prior),
    checkmate::check_class(prior, "brmsprior"),
    combine = "or",
    .var.name = "prior"
  )
  mv <- inherits(formula, "mvbrmsformula")
  forms <- if (mv) formula$forms else list(formula)
  empty <- vapply(forms, function(f) {
    main <- obs_arm_main_formula(f)
    inherits(main, "formula") && length(main) == 3L &&
      !formula_has_population_terms(main)
  }, logical(1L))
  if (!any(empty)) {
    return(list(formula = formula, data = data, prior = prior))
  }

  forms[empty] <- lapply(forms[empty], rewrite_empty_obs_arm)
  resps <- if (mv) {
    vapply(forms[empty], function(f) f$resp, character(1L))
  } else {
    ""
  }
  if (mv) {
    formula$forms <- forms
  } else {
    formula <- forms[[1L]]
  }
  # Zero, not one. The pin already holds the coefficient at zero, so
  # the contribution is nil either way, but a column of ones is an
  # intercept a reader of `standata()$X` would take at face value on a
  # formula that declined one.
  data[[MVGAM_EMPTY_OBS_PLACEHOLDER]] <- 0
  pin <- obs_placeholder_pin(resps)
  list(
    formula = formula,
    data = data,
    prior = if (is.null(prior)) pin else rbind(prior, pin)
  )
}


# Internal: rewrite one response's main formula from `y ~ 0` to
# `y ~ 0 + <placeholder>`, keeping the response and environment.
#'@noRd
rewrite_empty_obs_arm <- function(f) {
  main <- obs_arm_main_formula(f)
  new_main <- stats::reformulate(
    termlabels = c("0", MVGAM_EMPTY_OBS_PLACEHOLDER),
    response   = main[[2L]],
    env        = environment(main)
  )
  if (!inherits(f, "brmsformula")) return(new_main)
  # brms stores settings such as `nl = TRUE` as attributes of the
  # main formula. The rewritten formula carries them over.
  for (a in setdiff(names(attributes(main)), c("class", ".Environment"))) {
    attr(new_main, a) <- attr(main, a)
  }
  f$formula <- new_main
  f
}


# Internal: the priors that hold the injected placeholder's
# coefficient at zero, one per response it was written into. The
# column then contributes `0 * 0 = 0` and the formula means what the
# user wrote. A refit needs the same rows and cannot get them from
# the injection: the stored formula already names the placeholder,
# and the injection does not fire a second time.
#'@noRd
obs_placeholder_pin <- function(resps) {
  checkmate::assert_character(resps, min.len = 1L, any.missing = FALSE)
  pins <- lapply(resps, function(r) {
    brms::set_prior("constant(0)", class = "b",
                    coef = MVGAM_EMPTY_OBS_PLACEHOLDER, resp = r)
  })
  do.call(rbind, pins)
}


# Internal: put the placeholder's pins back into a prior table that
# lost them. `mvgam()` filters the rows out of the table it stores,
# and the user never reads a prior for a column they did not write.
# A refit inheriting that table would sample the placeholder free,
# and against a trend intercept the two lie on an exact ridge. The
# sibling of `ensure_obs_placeholder()`, which restores the column.
#'@noRd
ensure_obs_placeholder_pin <- function(prior, object) {
  resps <- obs_placeholder_resps(object$formula)
  if (!is.null(prior) && nrow(prior) > 0L) {
    held <- prior$resp[prior$coef == MVGAM_EMPTY_OBS_PLACEHOLDER]
    resps <- setdiff(resps, held)
  }
  if (!length(resps)) return(prior)
  pin <- obs_placeholder_pin(resps)
  if (is.null(prior) || nrow(prior) == 0L) pin else rbind(prior, pin)
}


# Parameters mvgam injects that do not carry the `_trend` suffix.
# Everything else it emits is suffixed, so the two rules together name
# the whole mvgam-side surface without naming any brms parameter.
mvgam_unsuffixed_params <- c(
  "Z", "Z_free_vec", "Psi", "varrho_inv", "theta_features"
)


#' Does this prior class name a parameter mvgam manages?
#'
#' @description
#' Most of what mvgam emits carries the `_trend` suffix, but not all
#' of it: the loadings matrix and the loadings-prior parameters are
#' named after the quantity rather than the submodel. Splitting a
#' prior table on the suffix alone therefore files `Z` as
#' observation-side, hands it to brms, and brms refuses a class it
#' has no parameter for, while `default_prior()` was the source of
#' the class name. One predicate so the split cannot disagree with
#' the emitter about whose parameter a class names.
#'
#' @param class Character vector of prior class names.
#' @return Logical vector, `TRUE` where mvgam owns the class.
#' @noRd
is_mvgam_managed_class <- function(class) {
  checkmate::assert_character(class, any.missing = FALSE)
  grepl("_trend$", class) |
    class %in% mvgam_unsuffixed_params |
    grepl("^theta_dist_", class)
}


#' Name the trend-side rows of a prior table after their Stan parameters
#'
#' A prior table built from the trend prefit carries brms's own class
#' names, so `sigma` there is the trend's innovation scale and has to
#' become `sigma_trend` before a reader can match it to the program.
#' The classes that must not move are exactly the ones that already
#' name an mvgam parameter: `Z` is `Z` in the Stan, and `Z_trend`
#' names nothing. That makes the suffix rule the complement of the
#' class predicate rather than a second list to keep in step with it.
#'
#' @param class Character vector of prior class names.
#' @return The same vector, suffixed where a suffix was owed.
#' @noRd
apply_trend_class_suffix <- function(class) {
  checkmate::assert_character(class, any.missing = FALSE)
  owed <- nzchar(class) & !is_mvgam_managed_class(class)
  class[owed] <- paste0(class[owed], "_trend")
  class
}


# The latent states. Their sampling statements are the trend equation
# and the non-centred reparameterisation it is written under, not
# priors a user set or could change. `init_trend` holds the states
# before the first observed time, drawn from the stationary
# distribution the autoregression implies, so its statement is a
# function of `A_trend` and `Sigma_trend` rather than a prior of its
# own.
mvgam_state_params <- c(
  "trend", "lv_trend", "lv_trend_tilde",
  "innovations_trend", "scaled_innovations_trend", "init_trend"
)


#' The bounds each parameter is declared with, keyed by name
#'
#' A Stan declaration carries the parameter's support in the same
#' line that names it, as `vector<lower=-1, upper=1>[N] ar1_trend;`.
#' Reading it here keeps the prior table describing the program that
#' was compiled rather than a second opinion about it.
#'
#' @param sc The generated Stan code, as one string.
#' @return A named list of `list(lb, ub)`, both character and empty
#'   where the declaration names no bound.
#' @noRd
stancode_declared_bounds <- function(sc) {
  decl_re <- paste0(
    "(?:real|int|vector|row_vector|matrix|simplex|ordered|",
    "positive_ordered|unit_vector|cholesky_factor_corr|",
    "cholesky_factor_cov|corr_matrix|cov_matrix)",
    "[[:space:]]*(<[^>]*>)?",
    "[[:space:]]*(?:\\[[^]]*\\])?",
    "[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*;"
  )
  out <- list()
  for (hit in regmatches(sc, gregexpr(decl_re, sc))[[1L]]) {
    m <- regmatches(hit, regexec(decl_re, hit))[[1L]]
    if (length(m) < 3L) next
    spec <- m[2L]
    one <- function(which) {
      pat <- paste0(which, "[[:space:]]*=[[:space:]]*")
      if (!grepl(pat, spec)) return("")
      trimws(sub(paste0(".*", pat, "([^,>]+).*"), "\\1", spec))
    }
    out[[m[3L]]] <- list(lb = one("lower"), ub = one("upper"))
  }
  out
}


#' Read every mvgam prior statement out of an assembled Stan model
#'
#' The compiled code is what the sampler ran, so a prior table read
#' from it cannot disagree with what the model sampled. Two emission
#' forms appear: `x ~ dist(args);` and the `target += dist_lpdf(x |
#' args)` an mvgam stanvar writes when it also adds a Jacobian or a
#' truncation term. A container call on the left, as in
#' `to_vector(Z) ~ student_t(3, 0, 0.5)`, is unwrapped to the
#' parameter it holds.
#'
#' The latent states are excluded. `innovations_trend` carries a
#' `std_normal()` because the trend is written non-centred, so that
#' statement is a reparameterisation rather than a prior anyone set,
#' and the state paths are governed by the trend equation rather than
#' by a prior of their own.
#'
#' @param sc Character scalar holding the assembled Stan model.
#' @return A list of single-row `brmsprior` objects, possibly empty.
#' @noRd
mvgam_stancode_prior_rows <- function(sc) {
  checkmate::assert_string(sc)
  # States are excluded on top of the shared predicate: their
  # sampling statements are the trend equation, not priors.
  is_mvgam_param <- function(nm) {
    is_mvgam_managed_class(nm) & !nm %in% mvgam_state_params
  }
  # The support a parameter is sampled on is declared in the same
  # program as its prior, a line or two above it. Reading the prior
  # and leaving the bounds behind reported `normal(0, 0.5)` for an
  # autoregressive coefficient held inside (-1, 1), and for a
  # continuous-time one held inside (0.001, 0.999), as though half
  # the mass of each sat outside the model's own support.
  declared <- stancode_declared_bounds(sc)

  rows <- list()
  seen <- character(0L)
  add <- function(class, coef, prior) {
    key <- paste0(class, "|", coef)
    if (key %in% seen) return(invisible(NULL))
    seen[[length(seen) + 1L]] <<- key
    bound <- declared[[class]] %||% list(lb = "", ub = "")
    row <- brms::prior_string(
      prior, class = class, coef = coef,
      lb = if (nzchar(bound$lb)) bound$lb else NA,
      ub = if (nzchar(bound$ub)) bound$ub else NA
    )
    # brms returns NA for an unbounded parameter and `get_prior()`
    # returns "", so the two sides of the table compare equal.
    if (is.na(row$lb)) row$lb <- ""
    if (is.na(row$ub)) row$ub <- ""
    rows[[length(rows) + 1L]] <<- row
    invisible(NULL)
  }

  # `x ~ dist(args);`, with an optional index becoming the coef.
  tilde_re <- paste0(
    "(?:^|[[:space:];{}])",
    "(?:(?:to_vector|to_matrix|to_array_1d)[[:space:]]*\\([[:space:]]*)?",
    "([A-Za-z_][A-Za-z0-9_]*)",
    "(\\[[^]]*\\])?[[:space:]]*\\)?[[:space:]]*~[[:space:]]*",
    "([A-Za-z_][A-Za-z0-9_]*[[:space:]]*\\([^;]*\\))[[:space:]]*;"
  )
  for (hit in regmatches(sc, gregexpr(tilde_re, sc))[[1L]]) {
    m <- regmatches(hit, regexec(tilde_re, hit))[[1L]]
    if (length(m) < 4L || !is_mvgam_param(m[2L])) next
    coef <- gsub("^\\[|\\]$", "", m[3L])
    # A multi-dimensional slice is a loop body rather than a prior on
    # a named coefficient: `Z[ : , i_z] ~ ...` sits inside a `for` and
    # names a Stan local. `varrho_inv[1]` and `varrho_inv[2:N]` carry
    # a single index and are the rows a reader wants.
    if (grepl(",", coef, fixed = TRUE)) next
    add(m[2L], coef, gsub("[[:space:]]+", " ", trimws(m[4L])))
  }

  # `target += dist_lpdf(x | args);`, the form a statement carries
  # when it keeps its normalising constant. The left side takes the
  # same shapes the tilde branch allows, a container call or a single
  # index, so both readers name the same parameter. The bar is
  # optional because a parameter-free density is written without one,
  # as `std_normal_lpdf(to_vector(Z))`. Anchoring on the accumulator
  # keeps density calls inside a function body out of the table, and
  # the likelihood stays out because its response is not an mvgam
  # parameter.
  lpdf_re <- paste0(
    "(?:target|lprior)[[:space:]]*\\+=[[:space:]]*",
    "([A-Za-z_][A-Za-z0-9_]*)_", stan_density_call_rx,
    "[[:space:]]*\\([[:space:]]*",
    "(?:(?:to_vector|to_matrix|to_array_1d)[[:space:]]*\\([[:space:]]*)?",
    "([A-Za-z_][A-Za-z0-9_]*)",
    "(\\[[^]]*\\])?[[:space:]]*\\)?[[:space:]]*",
    "(?:\\|[[:space:]]*([^;]*))?\\)[[:space:]]*;"
  )
  for (hit in regmatches(sc, gregexpr(lpdf_re, sc))[[1L]]) {
    m <- regmatches(hit, regexec(lpdf_re, hit))[[1L]]
    if (length(m) < 5L || !is_mvgam_param(m[3L])) next
    coef <- gsub("^\\[|\\]$", "", m[4L])
    # A multi-index names a cell rather than a coefficient, which the
    # prior table has no row shape for.
    if (grepl(",", coef, fixed = TRUE)) next
    args <- gsub("[[:space:]]+", " ", trimws(m[5L]))
    add(m[3L], coef, paste0(m[2L], "(", args, ")"))
  }
  rows
}


#' Name the mvgam parameters carrying a `~` sampling statement
#'
#' Stan drops the normalising constant of every `x ~ dist(args);`, so
#' each one offsets a program's `lp__` by a constant. Reading whole
#' left-hand sides rather than one spelling of them resolves a
#' container call, a slice, a transpose and a loop body alike:
#' `diagonal(A_raw_trend[lag])` and `lv_trend[t, : ]'` are statements
#' a parameter-shaped pattern does not see.
#'
#' Ownership comes from `is_mvgam_managed_class()`, the predicate the
#' prior table is built from, so the two readers cannot disagree
#' about whose parameter a statement names. Latent states count here
#' and not there: their statements are the trend equation rather than
#' priors, but they drop a constant all the same, and the
#' innovations are the largest such term in a factor program.
#'
#' @param sc Character scalar holding the assembled Stan model.
#' @return Character vector of parameter names, possibly empty.
#' @noRd
mvgam_tilde_statement_params <- function(sc) {
  checkmate::assert_string(sc)
  stmts <- strsplit(strip_stan_comments(sc), ";", fixed = TRUE)[[1L]]
  stmts <- stmts[grepl("~", stmts, fixed = TRUE)]
  if (length(stmts) == 0L) {
    return(character(0L))
  }
  # `stan_statement_lhs()` drops a block a generator opened on the
  # same line, so a loop bound is not read as a sampled parameter.
  lhs <- vapply(stmts, stan_statement_lhs, character(1L),
                  USE.NAMES = FALSE)
  lhs <- lhs[!is.na(lhs)]
  tokens <- unlist(regmatches(
    lhs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", lhs)
  ))
  if (length(tokens) == 0L) {
    return(character(0L))
  }
  unique(tokens[is_mvgam_managed_class(tokens)])
}


#' Which densities in a Stan program drop a normalising constant
#'
#' brms signals the choice by calling `_lupdf` or `_lupmf`; mvgam
#' signals it by writing `~`. A program can carry either, so both
#' have to be read for the answer to cover the whole model.
#'
#' @param stancode Character vector or scalar holding the program.
#' @return Character vector naming what was found, empty when the
#'   program is normalised throughout.
#' @noRd
mvgam_unnormalized_terms <- function(stancode) {
  checkmate::assert_character(stancode, null.ok = TRUE)
  sc <- paste(as.character(stancode), collapse = "\n")
  if (!nzchar(sc)) {
    return(character(0L))
  }
  lu <- unique(unlist(regmatches(
    sc, gregexpr("_lup[dm]f", sc)
  )))
  c(lu, mvgam_tilde_statement_params(sc))
}


#' Does this formula contribute any population-level term?
#'
#' `~ 0` and `~ -1` describe the same empty linear predictor, so a
#' formula compared against `~ 0` answers for one spelling and not
#' the other. Reading the terms object answers for every spelling at
#' once, and does not depend on `all.equal()` happening to return a
#' length-one result, which is what kept the comparison from erroring
#' on a vector condition.
#'
#' An intercept counts: `~ 1` has a term to report even though it
#' names no predictor.
#'
#' @param formula A formula.
#' @return `TRUE` when the formula has predictors or an intercept.
#' @noRd
formula_has_population_terms <- function(formula) {
  if (!inherits(formula, "formula")) return(FALSE)
  trms <- stats::terms(formula)
  length(attr(trms, "term.labels")) > 0L ||
    attr(trms, "intercept") != 0L
}


#' Lift mvgam-emitted Stan priors into the brmsprior table
#'
#' brms's `validate_prior()` only sees priors that flow through its
#' own prior pipeline. mvgam emits several additional priors via
#' stanvars that bypass that pipeline:
#'   - partial-Z loadings (`Z_free_vec ~ student_t(3, 0, 1)`)
#'   - loadings_prior features kernel
#'     (`theta_features ~ lognormal(0, 1)`)
#'   - loadings_prior distance kernels (one `theta_dist_<NAME>` per
#'     supplied distance matrix)
#'   - MGP column shrinkage (`varrho_inv[1]` + `varrho_inv[2:]`)
#'   - closure-unit family scale (`Psi ~ exponential(1)` for mvn /
#'     mvt families)
#'
#' This helper scans the fully assembled Stan model for those known
#' emission sites and appends matching `brmsprior` rows tagged
#' `source = "mvgam"`. The scan is pattern-driven, not flag-driven:
#' adding a new mvgam-side prior is a single regex entry here.
#'
#' @param prior A `brmsprior` returned by `validate_prior()`.
#' @param stancode Character string (or vector of lines) holding the
#'   full assembled Stan model. NULL / empty returns `prior` as-is.
#'
#' @return The merged `brmsprior` with mvgam-side rows appended.
#' @noRd
lift_mvgam_stanvar_priors <- function(prior, stancode) {
  checkmate::assert_class(prior, "brmsprior")
  checkmate::assert(
    checkmate::check_null(stancode),
    checkmate::check_character(stancode),
    .var.name = "stancode"
  )
  if (is.null(stancode)) return(prior)
  sc <- paste(as.character(stancode), collapse = "\n")
  if (!nzchar(sc)) return(prior)

  rows <- mvgam_stancode_prior_rows(sc)

  if (length(rows) == 0L) return(prior)

  mvgam_block <- Reduce(`+`, rows)
  # validate_prior() tags rows with `source`; prior_string() does
  # not. Set explicitly so prior_summary readers can filter on origin.
  mvgam_block$source <- "mvgam"

  # Align columns then rbind. brmsprior is a data.frame subclass.
  missing_cols <- setdiff(names(prior), names(mvgam_block))
  for (col in missing_cols) {
    mvgam_block[[col]] <- if (is.character(prior[[col]])) "" else NA
  }
  missing_in_prior <- setdiff(names(mvgam_block), names(prior))
  for (col in missing_in_prior) {
    prior[[col]] <- if (is.character(mvgam_block[[col]])) "" else NA
  }
  mvgam_block <- mvgam_block[, names(prior), drop = FALSE]

  out <- rbind(prior, mvgam_block)
  class(out) <- class(prior)
  out
}

#' Extract brms Terms from Setup
#' @param setup_object brms setup object
#' @return brmsterms object
#' @noRd
extract_brmsterms_from_setup <- function(setup_object) {
  # Extract brms terms - let errors bubble up
  if (!is.null(setup_object$formula)) {
    terms_info <- brms::brmsterms(setup_object$formula)
  } else {
    terms_info <- NULL
  }

  return(terms_info)
}

# =============================================================================
# SECTION 2: MULTIVARIATE TRENDS PARSING
# =============================================================================
# WHY: Multivariate models require careful formula parsing to handle
# response-specific trends and cross-series dependencies. This system enables
# per-response trend specifications while maintaining brms compatibility for
# multivariate response families and distributional modeling.
#' @noRd
parse_multivariate_trends <- function(formula, trend_formula = NULL) {
  # Validate inputs - accept either regular formula or brms formula objects
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    checkmate::check_class(formula, "brmsformula"),
    combine = "or"
  )

  # Cache formula metadata so latent_params lookups in downstream
  # validators read from an attribute instead of reparsing the AST.
  formula <- cache_formula_latent_params(formula)
  if (!is.null(trend_formula)) {
    trend_formula <- cache_formula_latent_params(trend_formula)
  }

  # Handle missing trend formula
  if (is.null(trend_formula)) {
    return(list(
      has_trends = FALSE,
      is_multivariate = FALSE,
      trend_specs = NULL,
      base_formula = NULL,
      cached_formulas = list(
        formula = formula,
        trend_formula = NULL
      )
    ))
  }

  # Validate trend formula structure
  trend_validation <- validate_trend_formula_brms(trend_formula)

  # Check if main formula is multivariate
  is_mv_main <- is_multivariate_formula(formula)

  # The keys brms gives the responses, which are what a per-response
  # trend specification is named by.
  response_names <- names(response_columns(formula))


  # Handle response-specific trend formulas
  if (inherits(trend_formula, "brmsformula") ||
      inherits(trend_formula, "brmsterms") ||
      inherits(trend_formula, "mvbrmsterms")) {

    # Extract response-specific trend specifications
    trend_specs <- extract_response_trends(trend_formula, response_names)

    # Create base formula for brms setup
    base_formula <- create_trend_base_formula(trend_specs)

  } else if (is.list(trend_formula) && !is.null(names(trend_formula))) {
    # Handle response-specific trends as validated lists
    if (!is_mv_main) {
      stop(insight::format_error(c(
        cli::format_inline(
          "List {.field trend_formula} requires multivariate main formula."
        ),
        i = "Use mvbind() or bf() for multiple responses."
      )))
    }

    # Validate response names match
    missing_responses <- setdiff(names(trend_formula), response_names)
    if (length(missing_responses) > 0) {
      stop(insight::format_error(c(
        cli::format_inline(paste0(
          "Unknown responses in {.field trend_formula}: ",
          "{paste(missing_responses, collapse = ', ')}"
        )),
        i = paste(
          "Available responses:",
          paste(response_names, collapse = ", ")
        )
      )))
    }

    # Parse each trend formula
    trend_specs <- lapply(names(trend_formula), function(resp) {
      if (is.null(trend_formula[[resp]])) return(NULL)
      parse_trend_formula(trend_formula[[resp]])$trend_model
    })
    names(trend_specs) <- names(trend_formula)

    # Create base formula from first non-NULL trend
    non_null_trends <- which(!sapply(trend_formula, is.null))
    if (length(non_null_trends) > 0) {
      base_formula <- trend_formula[[non_null_trends[1]]]
    } else {
      base_formula <- ~ 1  # Fallback if all trends are NULL
    }

  } else {
    # Single trend formula applied to all responses. Parse it first
    # so we store the trend objects, not the raw formula.
    parsed_trend <- parse_trend_formula(trend_formula)

    trend_specs <- if (is_mv_main && !is.null(response_names)) {
      # Apply same parsed trend to all responses
      setNames(
        replicate(
          length(response_names), parsed_trend$trend_model,
          simplify = FALSE
        ),
        response_names
      )
    } else {
      # Univariate case: return trend_model directly (no wrapper)
      parsed_trend$trend_model
    }

    base_formula <- parsed_trend$base_formula
  }

  return(list(
    has_trends = TRUE,
    is_multivariate = is_mv_main,
    trend_specs = trend_specs,
    base_formula = base_formula,
    validation = trend_validation,
    cached_formulas = list(
      formula = formula,
      trend_formula = trend_formula
    )
  ))
}

# Names that a `pforms` entry can carry while still describing one
# response. Anything else there names a second response, so a
# missing entry reads a distributional parameter as a whole extra
# response: the formula is taken for multivariate and the trend
# never reaches `mu`, which compiles and samples and is wrong.
# `tests/testthat/test-brms-integration.R` asserts every mvgam
# family's own dpars appear here, so adding a family with a new one
# fails a test rather than producing a model with no trend.
# `Psi` is not a dpar; mvn() and mvt() declare it as a parameter
# through stanvars, and it is named here for the same reason.
mvgam_distributional_params <- c(
  "sigma", "sigma2", "shape", "nu", "phi", "kappa", "theta",
  "zi", "hu", "disc", "bs", "ndt", "bias", "xi", "coi", "zoi",
  "beta", "hurdle", "alpha", "sigma_error",
  "p", "Psi", "mphi", "mtheta", "mtail"
)


#' Check if Formula Object is Multivariate
#'
#' @description
#' Detects multivariate formula specifications across all brms patterns:
#' mvbind(), bf() with multiple responses, mvbf(), and combined bf() objects.
#' Uses brms-compatible structural validation to detect them.
#'
#' Note: cbind() is NOT considered multivariate per brms standards - it creates
#' binomial trial specifications (univariate models with trials structure).
#'
#' @param formula Formula object to check. Can be formula, brmsformula,
#'   mvbrmsformula, or bform class
#' @return Logical indicating if formula contains multiple response variables
#'
#' @details
#' Supports all major brms multivariate patterns:
#' \itemize{
#'   \item mvbrmsformula class objects (combined bf() formulas)
#'   \item brmsformula objects with additional responses (bf(y1~x, y2~z))
#'   \item formula objects with mvbind() response binding
#'   \item All patterns validated using brms-compatible structure checks
#' }
#'
#' @seealso \code{\link{parse_multivariate_trends}}
#' @noRd
is_multivariate_formula <- function(formula) {
  # Parameter validation - support all brms formula types
  checkmate::assert(
    inherits(formula, c("formula", "brmsformula", "mvbrmsformula", "bform")),
    .var.name = "formula"
  )

  # Case 1: mvbrmsformula class - always multivariate (brms standard)
  # This covers: bf() + bf() combinations and mvbf() objects
  if (inherits(formula, "mvbrmsformula")) {
    return(TRUE)
  }

  # Case 2: brmsformula with distributional parameters or multiple responses  
  if (inherits(formula, "brmsformula")) {
    # Nonlinear formulas already handled correctly
    if (is_nonlinear_formula(formula)) {
      return(FALSE)
    }
    
    # Check pforms content
    if (!is.null(formula$pforms) && length(formula$pforms) > 0) {
      pform_names <- names(formula$pforms)
      # Distributional parameters are univariate, non-dpar
      # responses are multivariate.
      if (all(pform_names %in% mvgam_distributional_params)) {
        return(FALSE)
      }
      return(TRUE)
    }
    
    return(FALSE)
  }

  # Case 3: Standard formula with mvbind binding ONLY (corrected)
  # This covers: mvbind(y1, y2) ~ x (cbind REMOVED - not multivariate per brms)
  if (inherits(formula, "formula")) {
    return(has_mvbind_response(formula))
  }

  # Default case: univariate
  return(FALSE)
}

#' Check for mvbind Response in Formula
#'
#' @description
#' Helper function that checks formula response side for mvbind() binding
#' using expression parsing. cbind() is explicitly excluded as it
#' creates binomial trial specifications, not multivariate models.
#'
#' @param formula Formula object to check
#' @return Logical indicating presence of mvbind response binding
#'
#' @details
#' Uses safe expression parsing rather than fragile regex patterns.
#' Validates formula structure and ensures mvbind() is well-formed.
#'
#' @noRd
# Name of the function a response call invokes, tolerating namespace
# qualification. `as.character()` on `brms::mvbind(y1, y2)` returns
# c("::", "brms", "mvbind"), so comparing it directly to a single name
# errors with "the condition has length > 1" rather than reporting an
# unrecognised response.
#'@noRd
response_call_name <- function(response_expr) {
  if (!is.call(response_expr)) {
    return("")
  }
  target <- response_expr[[1]]
  if (is.name(target)) {
    return(as.character(target))
  }
  if (is.call(target) && identical(target[[1]], quote(`::`))) {
    return(as.character(target[[3]]))
  }
  ""
}

has_mvbind_response <- function(formula) {
  checkmate::assert_formula(formula)

  # Validate formula has response side
  if (length(formula) < 3) {
    return(FALSE)
  }

  # Get response expression (left side of ~)
  response_expr <- formula[[2]]

  # Check if response expression is a call to mvbind
  if (!is.call(response_expr)) {
    return(FALSE)
  }

  # Extract function name from call
  call_name <- response_call_name(response_expr)

  # Check if call is to mvbind (not cbind)
  if (call_name != "mvbind") {
    return(FALSE)
  }

  # Validate mvbind has arguments (at least 2 responses for multivariate)
  if (length(response_expr) < 3) {
    stop(insight::format_error(c(
      "Invalid mvbind() specification in formula.",
      x = paste0(
        "mvbind() requires at least 2 response variables for ",
        "multivariate models."
      ),
      i = "Ensure syntax: mvbind(y1, y2, ...) ~ predictors"
    )), call. = FALSE)
  }

  return(TRUE)
}

#' Drop the addition terms from a formula's left-hand side
#'
#' `y | trials(n) ~ x` carries the response and the terms that qualify
#' it in one expression. Reading variable names off the whole
#' left-hand side counts the qualifiers as responses, so they are
#' removed first.
#'
#' @param formula A two-sided formula
#' @return The same formula with the left-hand side reduced to the
#'   response expression
#'
#' @noRd
strip_addition_terms <- function(formula) {
  lhs <- formula[[2L]]
  if (!is.call(lhs) || !identical(as.character(lhs[[1L]]), "|")) {
    return(formula)
  }
  formula[[2L]] <- lhs[[2L]]
  formula
}


#' Extract Response-Specific Trend Specifications
#' @param trend_formula brms formula object with response-specific trends
#' @param response_names Character vector of response names
#' @return Named list of trend specifications per response
#' @noRd
extract_response_trends <- function(trend_formula, response_names) {
  checkmate::assert_character(response_names, null.ok = TRUE)

  # Check if trend_formula is already processed brms terms
  if (inherits(trend_formula, c("brmsterms", "mvbrmsterms"))) {
    trend_terms <- trend_formula
  } else {
    # brms parses the structure, and a malformed formula is refused
    # with brms's own account of what is wrong with it.
    trend_terms <- brms::brmsterms(trend_formula)
  }

  # Extract terms for each response
  trend_specs <- list()

  if (inherits(trend_terms, "mvbrmsterms")) {
    # Multivariate terms - extract each response
    for (i in seq_along(trend_terms$terms)) {
      resp_name <- names(trend_terms$terms)[i]
      if (is.null(resp_name) && i <= length(response_names)) {
        resp_name <- response_names[i]
      }

      if (!is.null(resp_name)) {
        trend_specs[[resp_name]] <- trend_terms$terms[[i]]$formula
      }
    }
  } else {
    # Single response trend - apply to main or first response
    resp_name <- if (!is.null(response_names)) response_names[1] else "main"
    trend_specs[[resp_name]] <- trend_terms$formula
  }

  return(trend_specs)
}

#' Create Base Formula for Trend Setup
#' @param trend_specs Named list of trend specifications
#' @return Formula object suitable for brms setup
#' @noRd
create_trend_base_formula <- function(trend_specs) {
  checkmate::assert_list(trend_specs, min.len = 1)

  # Use the first trend specification as base
  base_spec <- trend_specs[[1]]

  if (inherits(base_spec, "formula")) {
    return(base_spec)
  } else if (inherits(base_spec, "brmsterms")) {
    return(base_spec$formula)
  } else {
    # Fallback: create minimal trend formula
    return(~ 1)
  }
}
