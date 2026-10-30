#' brms integration for mvgam
#'
#' @description
#' brms integration and validation for mvgam models.
#' This file holds brms setup, formula validation, multivariate parsing
#' and nonlinear model support.
#'
#' @section Architecture:
#' The brms integration system provides compatibility across layers:
#' - **Setup Layer**: Lightweight brms model setup for rapid prototyping
#' - **Validation Layer**: Formula validation and syntax checking
#' - **Parsing Layer**: Multivariate and nonlinear formula interpretation
#' - **Extension Layer**: Functionality beyond base brms capabilities

# =============================================================================
# Helper functions
# =============================================================================

#' Does a trend formula keep its intercept?
#'
#' Only a formula holding nothing but an intercept, as `~ 1 + RW()`
#' leaves once the constructor is removed, keeps one. Every other
#' trend formula is given to brms without its implicit intercept, which
#' leaves the level to the observation formula. The `1` of `~ 1 + x`
#' reaches brms as a column of the trend design.
#'
#' @param formula The trend formula without its constructor
#' @return `TRUE` when the formula is `~ 1`
#' @noRd
should_trend_formula_have_intercept <- function(formula) {
  checkmate::assert_formula(formula)
  formula_terms <- stats::terms(formula)
  attr(formula_terms, "intercept") == 1L &&
    length(attr(formula_terms, "term.labels")) == 0L
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

#' The options each brms code generator declares
#'
#' Both sets derive from the formals of `mvgam_codegen_options()`. An
#' option added there reaches both generators. `brms::standata()` takes
#' no `normalize`: whether the sampling statements carry their
#' normalising constants is a property of the program alone.
#'
#' @noRd
mvgam_codegen_stancode_options <- names(formals(mvgam_codegen_options))

#' The options `brms::standata()` declares
#'
#' @noRd
mvgam_codegen_standata_options <- setdiff(
  mvgam_codegen_stancode_options, "normalize"
)

#' Build the options list from a call's dots
#'
#' `get_prior()` hands its dots to brms for the observation side. mvgam
#' builds the trend side itself. That side takes the same options
#' explicitly. Without them the two halves of one prior table would
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
# brms lightweight setup
# =============================================================================

#' Lightweight brms setup with prior support
#'
#' Builds a mock brms fit without compiling. brms provides the
#' observation model. mvgam adds the trends through stanvars without
#' modifying brms internals.
#'
#' @param formula Formula or brms formula object for model specification
#' @param data Data frame containing model variables
#' @param family Response distribution family (default: gaussian())
#' @param stanvars Optional brms stanvars object (default: NULL)
#' @param prior A brmsprior object or NULL. Prior specifications
#'   for model parameters. Defaults to NULL.
#' @param data2 Named list of objects brms specials look up outside
#'   `data`, such as `car()` adjacency matrices, or NULL
#' @param is_trend_setup Logical. `TRUE` for the trend submodel, whose
#'   data `extract_and_validate_trend_components()` has already reduced
#'   to one row per trend cell. Default: FALSE.
#' @param codegen A list from `mvgam_codegen_options()` holding the brms
#'   code-generation options, or NULL to take brms's own defaults.
#' @param ... Additional arguments passed to brms functions
#' @return List holding the formula, data, data2, family, stanvars,
#'   threads, codegen, Stan code, Stan data, prior table and mock
#'   `brmsfit`
#' @noRd
setup_brms_lightweight <- function(formula, data, family = gaussian(),
                                   stanvars = NULL,
                                   prior = NULL,
                                   data2 = NULL,
                                   is_trend_setup = FALSE,
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
  checkmate::assert_flag(is_trend_setup)
  checkmate::assert_list(codegen, names = "named", null.ok = TRUE)
  checkmate::assert_list(family)
  checkmate::assert_class(stanvars, "stanvars", null.ok = TRUE)
  checkmate::assert_list(data2, null.ok = TRUE)

  # A plain one-sided formula is a trend formula, as `~ 1` or `~ x`. Its
  # brms model takes the placeholder response `trend_y`, which no
  # likelihood scores. brms centres the default intercept prior on the
  # response's median. A zero placeholder centres it on zero, where the
  # trend is centred. A random placeholder drew from the user's random
  # numbers and moved that prior with the seed.
  if (inherits(formula, "formula") &&
      !inherits(formula, c("brmsformula", "mvbrmsformula", "bform")) &&
      length(formula) == 2L) {
    data$trend_y <- 0
    rhs <- rlang::f_rhs(formula)
    formula <- if (identical(rhs, 0) || identical(rhs, quote(-1))) {
      # `as.formula()` with no `env` takes this function's frame. A
      # refit resolves the constructor's names against the environment
      # the user wrote the formula in.
      stats::as.formula("trend_y ~ 0", env = environment(formula))
    } else if (should_trend_formula_have_intercept(formula)) {
      # The intercept keeps its `Intercept_trend` prior
      update(formula, trend_y ~ .)
    } else {
      update(formula, trend_y ~ 0 + .)
    }
  }

  # The trend submodel's formula is the trend formula with its
  # constructor removed. The observation rules do not apply to it.
  if (!is_trend_setup) {
    validate_obs_formula_brms(formula)
  }

  # An observation formula with no coefficient (`y ~ 0`) is given a
  # pinned zero column, as `inject_obs_zero_placeholder()` explains.
  # Only applied to the obs side; the trend side goes through mvgam's
  # own stancode rewriter, which strips brms's parameters declarations
  # and would leave the placeholder's pin assignment orphaned.
  if (!isTRUE(is_trend_setup)) {
    injected <- inject_obs_zero_placeholder(formula, data, prior)
    formula <- injected$formula
    data <- injected$data
    prior <- injected$prior
  }

  refuse_gp_level_sd_priors(prior, formula, data,
                            if (is_trend_setup) "_trend" else "")

  # A mock-backend brmsfit, which prediction needs. `data2` holds the
  # objects brms specials look up outside `data`, such as a `car()`
  # adjacency matrix or a `cov_ranef()` covariance matrix.
  #
  # A brms family is threaded by brms's `partial_log_lik_lpmf` and
  # `reduce_sum`, which brms writes only when given more than one
  # thread. A single thread would add the wrapper and its compile
  # time for nothing. Closure-unit families are threaded by their own
  # `partial_sum_*_lpmf` stanvars.
  dots <- list(...)
  n_threads <- thread_count(dots$threads)
  brm_threads <- if (n_threads > 1L) n_threads
  # The code-generation options travel with the mock fit and with every
  # later regeneration. The mock fit, the assembled Stan program and
  # the assembled Stan data then share their basis expansions, factor
  # levels and prior-only setting.
  #
  # brms announces the defaults it settles, such as `rescor` on a
  # multivariate formula, with `message()`. `silent = 2` turns
  # messages off, as it does for `brms::brm()`.
  mock_setup <- eval_silent(
    do.call(brms::brm, c(
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
    )),
    type = "message",
    silent = mvgam_quiet()
  )

  # Add version metadata to prevent restructure() from calling update()
  # standata() and prepare_predictions() call restructure() which checks version
  # and attempts update() if version is NULL or < "1.0". Multivariate models
  # update() throws an error. Adding current brms version prevents this.
  mock_setup$version <- list(brms = utils::packageVersion("brms"))

  # The program and its data are generated again from `formula`, once
  # per response for the data. brms settles an unset `rescor` on a
  # multivariate formula and announces it on every pass. The mock fit
  # settled it once. The formula carries that value from here. A bare
  # `mvbind(y1, y2) ~ x` becomes the `mvbrmsformula` that `bf()` makes
  # of it.
  if (inherits(mock_setup$formula, "mvbrmsformula")) {
    if (!inherits(formula, "mvbrmsformula")) {
      formula <- brms::bf(formula)
    }
    if (is.null(formula$rescor)) {
      formula$rescor <- isTRUE(mock_setup$formula$rescor)
    }
  }

  # The placeholder column stays in `data` and in `mock_setup$data`.
  # mvgam's stancode regenerator, brms's prediction helpers and any
  # post-fit `validate_data()` against the rewritten formula need the
  # column. The user-facing outputs hide it: the stored prior table
  # (`assemble_stored_prior_table()`), the variables list
  # (`variables.mvgam()`), the term list (`insight::find_predictors()`)
  # and the displayed formula in `summary.mvgam()`. A table the code
  # generator uses must keep the placeholder and its zero pin.

  # Key components for mvgam integration. `data2` stays on the setup.
  # The downstream Stan-code regenerators
  # (`generate_base_stancode_with_stanvars`) forward it to brms for
  # specials whose lookups live outside `data` (e.g. `car()`,
  # `cov_ranef()`). They also take `threads` and `codegen` again. The
  # mock `brmsfit` serves prediction.
  list(
    formula = formula,
    data = data,
    data2 = data2,
    family = family,
    stanvars = stanvars,
    threads = brm_threads,
    codegen = codegen,
    stancode = brms::stancode(mock_setup),
    standata = brms::standata(mock_setup),
    prior = extract_prior_from_setup(mock_setup, codegen),
    brmsfit = mock_setup
  )
}


#' Extract prior information from a brms setup
#'
#' @param setup_object Mock `brmsfit` from the brms setup, or a list
#'   holding its `formula`, `data`, `family`, `data2` and `prior`
#' @param codegen A list from `mvgam_codegen_options()`, or NULL. The
#'   prior table is keyed by the coefficients the design matrix holds.
#'   It is merged under the knots and factor levels the Stan data is
#'   built under.
#' @return Data frame of prior specifications
#' @noRd
extract_prior_from_setup <- function(setup_object, codegen = NULL) {
  checkmate::assert_list(setup_object)
  checkmate::assert_list(codegen, names = "named", null.ok = TRUE)
  # The full merged prior table: brms defaults for every parameter
  # class, with user-supplied rows overlaid and tagged
  # `source = "user"`. `brms::validate_prior()` performs the merge.
  # `prior_summary()` on an mvgam fit then matches the brmsfit
  # convention. A NULL user prior gives the default table.
  prior <- do.call(brms::validate_prior, c(
    list(
      prior = setup_object$prior,
      formula = setup_object$formula,
      data = setup_object$data,
      family = setup_object$family,
      data2 = setup_object$data2
    ),
    codegen_args_for(codegen, mvgam_codegen_standata_options)
  ))
  # The placeholder's `constant(0)` row stays in this table.
  # `generate_base_stancode_with_stanvars()` regenerates the compiled
  # program from it. Without the row a formula that declined an
  # intercept would fit a free one, on an exact ridge with any trend
  # intercept. `assemble_stored_prior_table()` hides the row from the
  # user.
  prior
}


#' Reserved column name of the empty-observation-formula placeholder
#'
#' `inject_obs_zero_placeholder()` writes it. Post-fit code filters it
#' out of user-visible output.
#'
#' @noRd
MVGAM_EMPTY_OBS_PLACEHOLDER <- ".mvgam_empty_obs"


#' The responses whose observation formula names the placeholder
#'
#' Responses are keyed as brms keys them, with `""` for the single
#' response of a univariate formula. The rewritten formula decides
#' whether a frame or a prior table needs the column and its pin. The
#' stored frame need not hold the column.
#'
#' @param formula Formula, `brmsformula`, `mvbrmsformula`,
#'   `mvgam_formula` or `NULL`
#' @return Character vector of response names
#' @noRd
obs_placeholder_resps <- function(formula) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_list(formula),
    checkmate::check_null(formula)
  )
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


#' Whether an observation formula names the placeholder
#'
#' @param formula Formula, `brmsformula`, `mvbrmsformula`,
#'   `mvgam_formula` or `NULL`
#' @return Logical scalar
#' @noRd
obs_formula_needs_placeholder <- function(formula) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_list(formula),
    checkmate::check_null(formula)
  )
  length(obs_placeholder_resps(formula)) > 0L
}


#' The main formula of one response's observation model
#'
#' A `brmsformula` keeps it in `formula`, together with its
#' distributional and non-linear sub-formulas. A plain formula is its
#' own main formula.
#'
#' @param f Formula or `brmsformula`
#' @return The main formula
#' @noRd
obs_arm_main_formula <- function(f) {
  checkmate::assert(
    checkmate::check_formula(f),
    checkmate::check_list(f)
  )
  if (inherits(f, "brmsformula")) f$formula else f
}


#' A frame given the placeholder column the rewritten formula names
#'
#' The brms data check refuses a prediction frame without the column.
#' Frames the user built and frames mvgam resolved from its training
#' data both need it.
#'
#' @param data Data frame or list of new data, or `NULL`
#' @param object Fitted `mvgam` object
#' @return `data` with the placeholder column added when needed
#' @noRd
ensure_obs_placeholder <- function(data, object) {
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data),
    checkmate::check_null(data)
  )
  checkmate::assert_list(object)
  if (is.null(data)) return(data)
  if (MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(data)) return(data)
  if (!obs_formula_needs_placeholder(object$formula)) return(data)
  data[[MVGAM_EMPTY_OBS_PLACEHOLDER]] <- 0
  data
}


#' A frame without the placeholder column
#'
#' `mvgam()` writes the column into the frame brms receives. The fit
#' stores that frame. Removing the column keeps it out of `fit$data`
#' and everything drawn from it.
#'
#' @param data Data frame, or any other object to return unchanged
#' @return `data` without the placeholder column
#' @noRd
drop_obs_placeholder <- function(data) {
  if (!is.data.frame(data)) return(data)
  keep <- setdiff(names(data), MVGAM_EMPTY_OBS_PLACEHOLDER)
  data[, keep, drop = FALSE]
}


#' A formatted formula without the placeholder term
#'
#' The displayed formula then matches what the user wrote, such as
#' `y ~ 0`. `format()` writes the term as ` + <term>` with single
#' spaces. A fixed match on that literal avoids treating the leading
#' `.` of the placeholder as a regular expression.
#'
#' @param formula_str Character vector of formatted formulas
#' @return Character vector with the placeholder term removed
#' @noRd
strip_empty_obs_placeholder <- function(formula_str) {
  checkmate::assert_character(formula_str, any.missing = FALSE)
  pat <- paste0(" + ", MVGAM_EMPTY_OBS_PLACEHOLDER)
  sub(pat, "", formula_str, fixed = TRUE)
}


#' A model formula formatted for display
#'
#' `format()` on a `brmsformula` returns one string per list element.
#' Formatting the main formula keeps an addition-term model displaying
#' as `y | trials(n) ~ 1`. Each distributional formula gets its own
#' line. A multivariate formula gives each response its own lines, with
#' the placeholder stripped from every response.
#'
#' @param formula Formula, `brmsformula` or `mvbrmsformula`
#' @return Character vector with one line per formula
#' @noRd
format_model_formula <- function(formula) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_list(formula)
  )
  # A multivariate formula gives each response its own lines, in the
  # order brms prints them.
  if (brms::is.mvbrmsformula(formula)) {
    return(unlist(lapply(formula$forms, format_model_formula),
                  use.names = FALSE))
  }
  if (!brms::is.brmsformula(formula)) {
    return(strip_empty_obs_placeholder(format(formula)))
  }
  # brms stores one formula per distributional parameter in `pforms`
  # and prints each on its own line under the response formula. A
  # reader who wrote `sigma ~ x` needs to see it. The lines are
  # collected before `formula` is narrowed to the response formula.
  dpar_lines <- unlist(lapply(formula$pforms, format), use.names = FALSE)
  c(strip_empty_obs_placeholder(format(formula$formula)), dpar_lines)
}


#' Format the link of every distributional parameter a family carries
#'
#' brms names a link per distributional parameter. The mean's link is
#' `family$link` and every other is `family$link_<dpar>`. A header that
#' reports only the mean gives a reader of a `sigma ~ x` or `nu ~ x`
#' block no way to tell the scale of those coefficients. Families that
#' declare no distributional parameters, such as Poisson, still report
#' the single mean link.
#'
#' @param family A family object
#' @return A single string of the form `"mu = logit; nu = identity"`
#'
#' @noRd
format_family_links <- function(family) {
  checkmate::assert_class(family, "family")
  dpars <- family$dpars
  if (length(dpars) == 0) {
    return(paste0("mu = ", family$link))
  }
  links <- vapply(dpars, function(dpar) {
    family[[paste0("link_", dpar)]] %||% family$link
  }, character(1))
  paste0(dpars, " = ", links, collapse = "; ")
}


#' A formula without population terms given a pinned placeholder
#'
#' `formula = y ~ 0` is a state-space model whose linear predictor comes
#' entirely from the trend. brms cannot build it under a family without
#' its own distributional parameter, such as poisson. Its prior pipeline
#' then fails on an empty prior table. The rewrite adds a column of
#' zeros to `data` and writes the formula as `y ~ 0 + <placeholder>`. A
#' `constant(0)` prior pins the coefficient. The rewrite runs under
#' every family, giving brms one formula shape. The pin must reach the
#' prior table that `generate_base_stancode_with_stanvars()` rebuilds
#' the program from. A free placeholder and a trend intercept lie on an
#' exact ridge. Only a formula with no term labels and no intercept is
#' rewritten. Each response of a multivariate formula is rewritten on
#' its own, with its own pin.
#'
#' @param formula Formula, `brmsformula` or `mvbrmsformula`
#' @param data Data frame of training data
#' @param prior `brmsprior` table, or `NULL`
#' @return List holding the rewritten `formula`, `data` and `prior`
#' @noRd
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
  # Zero. The pin already holds the coefficient at zero. A column of
  # ones would still appear as an intercept in `standata()$X` on a
  # formula that declined one.
  data[[MVGAM_EMPTY_OBS_PLACEHOLDER]] <- 0
  pin <- obs_placeholder_pin(resps)
  list(
    formula = formula,
    data = data,
    prior = if (is.null(prior)) pin else rbind(prior, pin)
  )
}


#' One response's main formula with the placeholder appended
#'
#' The placeholder joins the right-hand side as written. An `offset()`
#' has no population coefficient and stays in place. Editing the call
#' keeps the formula's class, environment and the attributes brms
#' stores on it, such as `nl = TRUE`.
#'
#' @param f Formula or `brmsformula` of one response
#' @return `f` with the placeholder term appended
#' @noRd
rewrite_empty_obs_arm <- function(f) {
  checkmate::assert(
    checkmate::check_formula(f),
    checkmate::check_list(f)
  )
  main <- obs_arm_main_formula(f)
  main[[3L]] <- call("+", main[[3L]], as.name(MVGAM_EMPTY_OBS_PLACEHOLDER))
  if (!inherits(f, "brmsformula")) return(main)
  f$formula <- main
  f
}


#' The priors that pin the placeholder coefficient at zero
#'
#' One prior per response the placeholder was written into. A refit
#' needs the same rows. Its stored formula already names the
#' placeholder. The injection then skips it.
#'
#' @param resps Character vector of response names, `""` for a
#'   univariate formula
#' @return A `brmsprior` table
#' @noRd
obs_placeholder_pin <- function(resps) {
  checkmate::assert_character(resps, min.len = 1L, any.missing = FALSE)
  pins <- lapply(resps, function(r) {
    brms::set_prior("constant(0)", class = "b",
                    coef = MVGAM_EMPTY_OBS_PLACEHOLDER, resp = r)
  })
  do.call(rbind, pins)
}


#' A prior table given back the placeholder pins it lost
#'
#' `mvgam()` removes the pins from the table it stores, hiding a prior
#' for a column the user never wrote. A refit inheriting that table
#' would leave the placeholder free, on an exact ridge with a trend
#' intercept. `ensure_obs_placeholder()` restores the column.
#'
#' @param prior `brmsprior` table, or `NULL`
#' @param object Fitted `mvgam` object
#' @return `prior` with a pin for every response that needs one
#' @noRd
ensure_obs_placeholder_pin <- function(prior, object) {
  checkmate::assert_data_frame(prior, null.ok = TRUE)
  checkmate::assert_list(object)
  resps <- obs_placeholder_resps(object$formula)
  if (NROW(prior) > 0L) {
    held <- prior$resp[prior$coef == MVGAM_EMPTY_OBS_PLACEHOLDER]
    resps <- setdiff(resps, held)
  }
  if (!length(resps)) return(prior)
  pin <- obs_placeholder_pin(resps)
  if (NROW(prior) == 0L) pin else rbind(prior, pin)
}


#' Parameters mvgam injects without the `_trend` suffix
#'
#' Every other parameter mvgam emits ends in the suffix. The two rules
#' together name the whole mvgam-side surface and no brms parameter.
#'
#' @noRd
mvgam_unsuffixed_params <- c(
  "Z", "Z_free_vec", "Psi", "varrho_inv", "theta_features"
)


#' The brms trend parameters the assembled program drops
#'
#' The trend reaches brms as a gaussian model. The assembled program
#' drops that model's residual `sigma`. The process noise is the
#' `sigma_trend` mvgam declares, one per latent series.
#'
#' @noRd
brms_trend_dropped_params <- "sigma"


#' Does this prior class name a parameter mvgam manages?
#'
#' @description
#' Most of what mvgam emits carries the `_trend` suffix. The loadings
#' matrix and the loadings-prior parameters are named after the
#' quantity. A split on the suffix alone would file `Z` as
#' observation-side. brms would then refuse a class it has no
#' parameter for. The prior table split and the emitter both use this
#' predicate.
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
#' names. Its `sigma` is the trend's innovation scale and becomes
#' `sigma_trend` to match the program. The classes that keep their
#' names are those that already name an mvgam parameter. `Z` is `Z` in
#' the Stan. `Z_trend` names nothing. The suffix rule is the
#' complement of `is_mvgam_managed_class()`.
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


#' The declaration of each Stan variable, keyed by name
#'
#' A Stan declaration carries the variable's type, support and size in
#' the line that names it, as `vector<lower=-1, upper=1>[N] ar1_trend;`.
#' A declaration may also define the variable, as
#' `vector<lower=0>[N] sigma_slope_trend = scale * raw;` does.
#' The prior table takes the bounds from the compiled program. The
#' parameter names take the size, from which they label a vector's
#' entries.
#'
#' @param sc The generated Stan code, as one string.
#' @return A named list of `list(type, lb, ub, size, array)`, all
#'   character. `array` holds the sizes of an `array[...]` prefix. Each
#'   field is empty where the declaration has none.
#' @noRd
stancode_declarations <- function(sc) {
  checkmate::assert_string(sc)
  # A size may nest one index, as `[knots_1[1]]` does
  size_re <- "\\[((?:[^][]|\\[[^]]*\\])*)\\]"
  decl_re <- paste0(
    "(?:array[[:space:]]*", size_re, "[[:space:]]*)?",
    "(real|int|vector|row_vector|matrix|simplex|ordered|",
    "positive_ordered|unit_vector|cholesky_factor_corr|",
    "cholesky_factor_cov|corr_matrix|cov_matrix)",
    "[[:space:]]*(<[^>]*>)?",
    "[[:space:]]*(?:", size_re, ")?",
    "[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*(?:;|=)"
  )
  out <- list()
  for (hit in regmatches(sc, gregexpr(decl_re, sc))[[1L]]) {
    m <- regmatches(hit, regexec(decl_re, hit))[[1L]]
    if (length(m) < 6L) next
    spec <- m[4L]
    one <- function(which) {
      pat <- paste0(which, "[[:space:]]*=[[:space:]]*")
      if (!grepl(pat, spec)) return("")
      trimws(sub(paste0(".*", pat, "([^,>]+).*"), "\\1", spec))
    }
    out[[m[6L]]] <- list(type = m[3L], lb = one("lower"),
                         ub = one("upper"), size = trimws(m[5L]),
                         array = trimws(m[2L]))
  }
  out
}


#' The mvgam prior statements of an assembled Stan model
#'
#' The sampler ran the compiled code. A prior table built from it
#' matches what the model sampled. Two emission forms appear:
#' `x ~ dist(args);` and the `target += dist_lpdf(x | args)` an mvgam
#' stanvar writes when it also adds a Jacobian or a truncation term. A
#' container call on the left, as in
#' `to_vector(Z) ~ student_t(3, 0, 0.5)`, is unwrapped to the
#' parameter it holds.
#'
#' The latent states are excluded. `innovations_trend` carries a
#' `std_normal()` because the trend is written non-centred. That
#' statement is a reparameterisation. The trend equation governs the
#' state paths.
#'
#' @param sc Character scalar holding the assembled Stan model.
#' @return A list of single-row `brmsprior` objects, possibly empty.
#' @noRd
mvgam_stancode_prior_rows <- function(sc) {
  checkmate::assert_string(sc)
  # The shared predicate, less the latent states. Their sampling
  # statements are the trend equation.
  is_mvgam_param <- function(nm) {
    is_mvgam_managed_class(nm) & !is_state_param(nm)
  }
  # Each parameter's support is declared in the same program as its
  # prior. Without the bounds an autoregressive coefficient held inside
  # (-1, 1) would report `normal(0, 0.5)` with half its mass outside
  # the support.
  declared <- stancode_declarations(sc)

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
    # returns "". Both sides of the table use "".
    if (is.na(row$lb)) row$lb <- ""
    if (is.na(row$ub)) row$ub <- ""
    rows[[length(rows) + 1L]] <<- row
    invisible(NULL)
  }

  # An index names a coefficient. A bare identifier there is a loop
  # variable, as in `Amu_trend[k]`. The statement then covers the whole
  # parameter.
  statement_coef <- function(index) {
    coef <- gsub("^\\[|\\]$", "", index)
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", coef)) "" else coef
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
    coef <- statement_coef(m[3L])
    # A multi-dimensional slice is a loop body. `Z[ : , i_z] ~ ...` is
    # inside a `for` and names a Stan local. `varrho_inv[1]` and
    # `varrho_inv[2:N]` carry a single index and are the rows a reader
    # wants.
    if (grepl(",", coef, fixed = TRUE)) next
    add(m[2L], coef, gsub("[[:space:]]+", " ", trimws(m[4L])))
  }

  # `target += dist_lpdf(x | args);`, the form a statement takes when
  # it keeps its normalising constant. The left side takes the shapes
  # of the tilde branch, a container call or a single index. Both
  # branches then name the same parameter. The bar is optional because
  # a parameter-free density is written without one, as
  # `std_normal_lpdf(to_vector(Z))`. The pattern requires the
  # accumulator. Density calls inside a function body stay out of the
  # table. The likelihood stays out because its response is outside the
  # mvgam parameters.
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
    coef <- statement_coef(m[4L])
    # A multi-index names a cell. The prior table has no row shape for
    # a cell.
    if (grepl(",", coef, fixed = TRUE)) next
    args <- gsub("[[:space:]]+", " ", trimws(m[5L]))
    add(m[3L], coef, paste0(m[2L], "(", args, ")"))
  }
  rows
}


#' Name the mvgam parameters carrying a `~` sampling statement
#'
#' Stan drops the normalising constant of every `x ~ dist(args);`.
#' Each one offsets a program's `lp__` by a constant. The scan takes
#' whole left-hand sides. It resolves a container call, a slice, a
#' transpose and a loop body alike: `diagonal(A_trend[lag])` and
#' `lv_trend[t, : ]'` are statements a parameter-shaped pattern misses.
#'
#' Ownership comes from `is_mvgam_managed_class()`, the predicate the
#' prior table is built from. Both scans then agree about whose
#' parameter a statement names. Latent states count here and not
#' there. Their statements are the trend equation. They drop a
#' constant all the same. The innovations are the largest such term in
#' a factor program.
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
  # same line. A loop bound is then excluded from the sampled
  # parameters.
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
#' brms marks the choice by calling `_lupdf` or `_lupmf`. mvgam marks
#' it by writing `~`. A program can carry either. The scan covers both.
#'
#' @param stancode Character vector or scalar holding the program.
#' @return Character vector naming what was found, empty when the
#'   program is normalised throughout.
#' @noRd
mvgam_unnormalized_terms <- function(stancode) {
  checkmate::assert_character(stancode, null.ok = TRUE)
  sc <- strip_stan_comments(paste(as.character(stancode), collapse = "\n"))
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
#' `~ 0` and `~ -1` describe the same empty linear predictor. The terms
#' object covers every spelling. A comparison with `~ 0` would cover
#' one.
#'
#' An intercept counts: `~ 1` has a term to report even though it
#' names no predictor.
#'
#' @param formula A formula. Any other object gives `FALSE`.
#' @return `TRUE` when the formula has predictors or an intercept.
#' @noRd
formula_has_population_terms <- function(formula) {
  if (!inherits(formula, "formula")) return(FALSE)
  trms <- stats::terms(formula)
  length(attr(trms, "term.labels")) > 0L ||
    attr(trms, "intercept") != 0L
}


#' The parameters a brms program declares
#'
#' @param stancode A brms program, as one string or its lines.
#' @return Character vector of parameter names, empty for NULL.
#' @noRd
brms_declared_params <- function(stancode) {
  checkmate::assert_character(stancode, null.ok = TRUE)
  if (is.null(stancode)) return(character(0L))
  lines <- strsplit(paste(stancode, collapse = "\n"), "\n", fixed = TRUE)[[1L]]
  body <- stan_block_body(lines, "parameters")
  names(stancode_declarations(paste(body, collapse = "\n")))
}


#' Lift mvgam-emitted Stan priors into the brmsprior table
#'
#' brms's `validate_prior()` only sees priors that flow through its
#' own prior pipeline. mvgam emits several additional priors via
#' stanvars that bypass that pipeline:
#'   - partial-Z loadings (`Z_free_vec` under the class `Z` prior)
#'   - loadings_prior features kernel
#'     (`theta_features ~ lognormal(0, 1)`)
#'   - loadings_prior distance kernels (one `theta_dist_<NAME>` per
#'     supplied distance matrix)
#'   - MGP column shrinkage (`varrho_inv[1]` + `varrho_inv[2:]`)
#'   - closure-unit family scale (`Psi ~ exponential(1)` for mvn /
#'     mvt families)
#'
#' This helper scans the fully assembled Stan model for mvgam prior
#' statements and appends matching `brmsprior` rows tagged
#' `source = "mvgam"`. The scan matches statement patterns. A new
#' mvgam-side prior appears without a new entry here.
#'
#' @param prior A `brmsprior` returned by `validate_prior()`.
#' @param stancode Character string (or vector of lines) holding the
#'   full assembled Stan model. NULL / empty returns `prior` as-is.
#' @param brms_owned Names the trend's brms program declares, as the
#'   assembled model spells them. brms files their priors under its
#'   classes (`sds_1_trend` under `sds_trend`). A statement on one of
#'   them adds no row.
#'
#' @return The merged `brmsprior` with mvgam-side rows appended.
#' @noRd
lift_mvgam_stanvar_priors <- function(prior, stancode,
                                      brms_owned = character(0L)) {
  checkmate::assert_class(prior, "brmsprior")
  checkmate::assert(
    checkmate::check_null(stancode),
    checkmate::check_character(stancode),
    .var.name = "stancode"
  )
  if (is.null(stancode)) return(prior)
  # A comment can carry a statement's shape, as the AR(1) start's
  # `lv_trend[1, j] ~ Normal(...)` does. The program keeps its comments.
  sc <- strip_stan_comments(paste(as.character(stancode), collapse = "\n"))
  if (!nzchar(sc)) return(prior)

  rows <- mvgam_stancode_prior_rows(sc)
  # A row brms already validated is in the table. The trend's brms
  # model writes its own priors into the program under the `_trend`
  # suffix. The scan above finds `b_trend` there a second time. The
  # scanned prior fills a row brms left flat. Every other held row is
  # already stated.
  held <- paste0(prior$class, "|", prior$coef)
  for (r in rows) {
    at <- match(paste0(r$class, "|", r$coef), held)
    if (!is.na(at) && !nzchar(prior$prior[at])) {
      prior$prior[at] <- r$prior
      if ("source" %in% names(prior)) prior$source[at] <- "mvgam"
    }
  }
  rows <- Filter(function(r) {
    !paste0(r$class, "|", r$coef) %in% held && !r$class %in% brms_owned
  }, rows)

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

# =============================================================================
# Multivariate trend parsing
# =============================================================================
#' Parse the trend formula for each response
#'
#' One trend formula serves every response of a multivariate model.
#' Each response gets a copy of the same parsed specification, keyed by
#' the name brms gives that response.
#'
#' @param formula Observation formula, `brmsformula` or
#'   `mvbrmsformula`
#' @param trend_formula Trend formula, or `NULL`
#' @return List holding `has_trends`, `is_multivariate`, `trend_specs`
#'   and `base_formula`. A model with a trend also holds
#'   `regular_terms` and `obs_formula`.
#' @noRd
parse_multivariate_trends <- function(formula, trend_formula = NULL) {
  checkmate::assert(
    checkmate::check_formula(formula),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    checkmate::check_class(formula, "brmsformula"),
    combine = "or"
  )

  checkmate::assert_formula(trend_formula, null.ok = TRUE)

  # Handle missing trend formula
  if (is.null(trend_formula)) {
    return(list(
      has_trends = FALSE,
      is_multivariate = FALSE,
      trend_specs = NULL,
      base_formula = NULL
    ))
  }

  validate_trend_formula(trend_formula)

  # Check if main formula is multivariate
  is_mv_main <- is_multivariate_formula(formula)

  # The keys brms gives the responses, which name each copy.
  response_names <- names(response_columns(formula))

  # The trend formula is parsed once and every response of a
  # multivariate model takes a copy of the specification.
  parsed_trend <- parse_trend_formula(trend_formula)
  trend_specs <- if (is_mv_main && !is.null(response_names)) {
    setNames(
      replicate(
        length(response_names), parsed_trend$trend_model,
        simplify = FALSE
      ),
      response_names
    )
  } else {
    parsed_trend$trend_model
  }
  return(list(
    has_trends = TRUE,
    is_multivariate = is_mv_main,
    trend_specs = trend_specs,
    base_formula = parsed_trend$base_formula,
    regular_terms = parsed_trend$regular_terms,
    obs_formula = formula
  ))
}

#' Names a `pforms` entry can carry while describing one response
#'
#' Any other name there marks a second response. A dpar missing from
#' this list would make a univariate formula look multivariate. The
#' trend would then miss `mu`. The model would still compile and
#' sample. `tests/testthat/test-brms-observation-specials.R` asserts
#' that every mvgam family's dpars appear here. A family with a new dpar
#' then fails a test. `Psi` is a parameter that mvn() and mvt() declare
#' through stanvars. It is listed for the same reason.
#'
#' @noRd
mvgam_distributional_params <- c(
  "sigma", "sigma2", "shape", "nu", "phi", "kappa", "theta",
  "zi", "hu", "disc", "bs", "ndt", "bias", "xi", "coi", "zoi",
  "beta", "hurdle", "alpha", "sigma_error",
  "p", "Psi", "mphi", "mtheta", "mtail"
)


#' Whether a formula has several responses
#'
#' @description
#' Detects multivariate formula specifications across all brms patterns:
#' mvbind(), bf() with multiple responses, mvbf(), and combined bf()
#' objects. Uses brms-compatible structural validation to detect them.
#'
#' brms treats a cbind() response as a binomial trials specification,
#' a univariate model.
#'
#' @param formula Formula object to check. Can be formula, brmsformula,
#'   mvbrmsformula, or bform class
#' @return Logical indicating if formula contains multiple response
#'   variables
#'
#' @details
#' Supports all major brms multivariate patterns:
#' \itemize{
#'   \item mvbrmsformula class objects (combined bf() formulas)
#'   \item brmsformula objects with additional responses (bf(y1~x, y2~z))
#'   \item formula objects with mvbind() response binding
#' }
#'
#' @seealso \code{\link{parse_multivariate_trends}}
#' @noRd
is_multivariate_formula <- function(formula) {
  checkmate::assert(
    checkmate::check_class(formula, "formula"),
    checkmate::check_class(formula, "brmsformula"),
    checkmate::check_class(formula, "mvbrmsformula"),
    checkmate::check_class(formula, "bform"),
    combine = "or",
    .var.name = "formula"
  )

  # Case 1: mvbrmsformula class - always multivariate (brms standard)
  # This covers: bf() + bf() combinations and mvbf() objects
  if (inherits(formula, "mvbrmsformula")) {
    return(TRUE)
  }

  # Case 2: brmsformula with distributional parameters or several
  # responses
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

  # Case 3: a plain formula is multivariate only with an mvbind()
  # response. brms treats a cbind() response as binomial trials.
  if (inherits(formula, "formula")) {
    return(has_mvbind_response(formula))
  }

  # Default case: univariate
  return(FALSE)
}

#' The function a response call invokes
#'
#' Tolerates namespace qualification. `as.character()` on
#' `brms::mvbind(y1, y2)` returns `c("::", "brms", "mvbind")`. A direct
#' comparison with one name would fail on a length-three condition.
#'
#' @param response_expr The left-hand side of a formula
#' @return The function name, or `""` for an expression without a call
#' @noRd
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

#' Whether a formula has an mvbind() response
#'
#' @description
#' Checks the response side of a formula for an mvbind() call by
#' expression parsing. A cbind() response is a binomial trials
#' specification and gives `FALSE`.
#'
#' @param formula Formula object to check
#' @return Logical indicating presence of mvbind response binding
#'
#' @details
#' Refuses an mvbind() call with fewer than two responses.
#'
#' @noRd
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
      "'mvbind()' needs at least two responses.",
      i = "Write a single response as 'y ~ x'."
    )), call. = FALSE)
  }

  return(TRUE)
}

#' Drop the addition terms from a formula's left-hand side
#'
#' `y | trials(n) ~ x` carries the response and the terms that qualify
#' it in one expression. Variable names taken from the whole left-hand
#' side would count the qualifiers as responses. They are removed
#' first.
#'
#' @param formula A two-sided formula
#' @return The same formula with the left-hand side reduced to the
#'   response expression
#'
#' @noRd
strip_addition_terms <- function(formula) {
  checkmate::assert_formula(formula)
  lhs <- formula[[2L]]
  if (!is.call(lhs) || !identical(as.character(lhs[[1L]]), "|")) {
    return(formula)
  }
  formula[[2L]] <- lhs[[2L]]
  formula
}


