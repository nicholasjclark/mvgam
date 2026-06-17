#' Trait-mediated environmental slopes for `jsdgam()`
#'
#' Internal helpers that rewrite `(obs_formula, trait_slopes)` into the
#' brms non-linear formula that parameterises the fourth-corner
#' regression
#'
#' \deqn{\beta_{s,k} = \sum_l T_{s, l}\, \gamma_{l, k} + \epsilon_{s, k}}
#'
#' where every fixed slope on an environmental covariate becomes a
#' trait-regressed nlpar carrying its own species-level random
#' deviation. The rewrite expands
#'
#'   `obs_formula  = y ~ env1 + env2`
#'   `trait_slopes = ~ trait1`
#'
#' into
#'
#'   `bf(y ~ a + b1 * env1 + b2 * env2,
#'       a  ~ trait1 + (1 | sp | species),
#'       b1 ~ trait1 + (1 | sp | species),
#'       b2 ~ trait1 + (1 | sp | species),
#'       nl = TRUE)`
#'
#' The `|sp|` correlated-random-effect tag jointly parameterises the
#' species-level intercept and slope deviations under an LKJ prior so
#' the trait wrapper composes with the rest of the jsdgam factor
#' model. The actual nl plumbing (priors, parameter aliasing,
#' discovery for marginaleffects) is in `R/insight.mvgam.R`,
#' `R/as.data.frame.mvgam.R` and `R/conditional_effects.mvgam.R`.
#'
#' @name trait_slopes_helpers
#' @keywords internal
NULL


# Identify the fixed-effect terms of a one-sided or two-sided formula
# we will rewrite into nl sub-formulas. Excludes specials (smooths,
# gp, random-effect bars) since the top-level nl formula cannot host
# them; the validator rejects those upstream.
trait_slopes_fixed_terms <- function(obs_formula) {
  f <- if (inherits(obs_formula, "brmsformula")) {
    obs_formula$formula
  } else {
    obs_formula
  }
  attr(stats::terms(f, keep.order = TRUE), "term.labels")
}


# One place that decides the nlpar layout of the rewrite. Given the
# original obs formula, returns the list both
# build_trait_slopes_formula() and default_trait_slopes_priors()
# consume:
#   $has_intercept  -- whether the wrapper emits the intercept nlpar
#                      `a`. Driven by the formula's `attr(terms,
#                      "intercept")` so `y ~ 0 + x` does not get one.
#   $slope_terms    -- top-level fixed terms (one nlpar per term).
#   $slope_nlpars   -- machine names for the slope nlpars (b1, b2...).
#   $nlpars         -- full ordered nlpar list (a + slopes).
trait_slopes_nlpar_spec <- function(obs_formula) {
  f <- if (inherits(obs_formula, "brmsformula")) {
    obs_formula$formula
  } else {
    obs_formula
  }
  has_intercept <- attr(
    stats::terms(f, keep.order = TRUE), "intercept"
  ) == 1L
  slope_terms <- trait_slopes_fixed_terms(obs_formula)
  slope_nlpars <- if (length(slope_terms) > 0L) {
    paste0("b", seq_along(slope_terms))
  } else {
    character(0L)
  }
  nlpars <- c(
    if (has_intercept) "a" else NULL,
    slope_nlpars
  )
  list(
    has_intercept = has_intercept,
    slope_terms   = slope_terms,
    slope_nlpars  = slope_nlpars,
    nlpars        = nlpars
  )
}


# Validate the trait_slopes spec. Errors are user-facing.
validate_trait_slopes <- function(trait_slopes, obs_formula, data,
                                  species_chr) {
  if (is.null(trait_slopes)) {
    return(invisible(NULL))
  }
  if (!inherits(trait_slopes, "formula") || length(trait_slopes) != 2L) {
    stop(insight::format_error(c(
      "'trait_slopes' must be a one-sided formula.",
      i = "Example: trait_slopes = ~ trait1 + trait2."
    )))
  }
  trait_vars <- all.vars(trait_slopes)
  if (length(trait_vars) == 0L) {
    stop(insight::format_error(
      "'trait_slopes' must reference at least one trait column."
    ))
  }
  missing <- setdiff(trait_vars, names(data))
  if (length(missing) > 0L) {
    stop(insight::format_error(c(
      "Trait columns not found in 'data'.",
      x = paste0(
        "Missing: ",
        paste(shQuote(missing), collapse = ", "), "."
      ),
      i = paste0(
        "Add them to 'data' (left-join the per-species trait table",
        " before calling jsdgam)."
      )
    )))
  }
  # Trait values must vary only across species, not within. A trait
  # that varies within a species is a per-observation covariate and
  # belongs in obs_formula directly.
  for (v in trait_vars) {
    by_species <- vapply(
      split(data[[v]], data[[species_chr]]),
      function(x) length(unique(x[!is.na(x)])),
      FUN.VALUE = integer(1L)
    )
    if (any(by_species > 1L)) {
      bad <- names(by_species[by_species > 1L])
      stop(insight::format_error(c(
        paste0(
          "Trait '", v, "' varies within species in 'data'."
        ),
        x = paste0(
          "Offending species: ",
          paste(utils::head(shQuote(bad), 6L), collapse = ", "),
          if (length(bad) > 6L) " ..." else "",
          "."
        ),
        i = paste0(
          "Traits must be constant per species; per-observation",
          " covariates belong in 'formula'."
        )
      )))
    }
  }
  # Specials inside the top-level obs formula would block the nl
  # rewrite because brms's nl=TRUE forbids smooths / gp() at the top
  # level. They are allowed inside the trait_slopes formula itself
  # (each sub-formula is a regular linear predictor where brms
  # accepts smooths).
  trms <- trait_slopes_fixed_terms(obs_formula)
  smooth_pat <- "^(s|te|t2|ti|gp)\\("
  hits <- grep(smooth_pat, trms, value = TRUE)
  if (length(hits) > 0L) {
    stop(insight::format_error(c(
      paste0(
        "Smooth terms in 'formula' are not compatible with",
        " 'trait_slopes'."
      ),
      x = paste0(
        "Offending term(s): ",
        paste(shQuote(hits), collapse = ", "), "."
      ),
      i = paste0(
        "Move smooth specials into the trait_slopes sub-formula",
        " (e.g. trait_slopes = ~ s(trait1)) or fit without",
        " trait_slopes."
      )
    )))
  }
  invisible(NULL)
}


# Rewrite (obs_formula, trait_slopes, species_var) into the nl
# brmsformula described above. The species_var is read from the
# jsdgam wrapper after data_train has been aligned to use 'series'
# as the species index, so the RE grouping factor on every nlpar
# is consistently '(1 | sp | series)'.
build_trait_slopes_formula <- function(obs_formula, trait_slopes,
                                       species_var = "series") {
  spec <- trait_slopes_nlpar_spec(obs_formula)
  if (length(spec$nlpars) == 0L) {
    stop(insight::format_error(c(
      "Cannot apply 'trait_slopes' to a formula with no fixed terms.",
      i = "Add an intercept or covariate to 'formula' first."
    )))
  }
  response <- deparse(
    if (inherits(obs_formula, "brmsformula"))
      obs_formula$formula[[2L]] else obs_formula[[2L]]
  )
  # Build the top-level non-linear expression: a + b1*env1 + b2*env2.
  rhs_parts <- c(
    if (spec$has_intercept) "a" else NULL,
    if (length(spec$slope_terms) > 0L)
      paste0(spec$slope_nlpars, " * ", spec$slope_terms) else NULL
  )
  top_form <- stats::as.formula(
    paste(response, "~", paste(rhs_parts, collapse = " + ")),
    env = environment(obs_formula)
  )
  # Each sub-formula regresses the nlpar on the user trait spec and
  # adds a species-level random deviation. The shared '|sp|' tag
  # jointly correlates the intercept and slope species deviations
  # under an LKJ prior so the trait wrapper composes with jsdgam's
  # factor model without double-counting between-species variation.
  trait_rhs <- paste(deparse(trait_slopes[[2L]]), collapse = " ")
  sub_rhs <- paste0(trait_rhs, " + (1 | sp | ", species_var, ")")
  pforms <- lapply(spec$nlpars, function(nm) {
    stats::as.formula(
      paste(nm, "~", sub_rhs),
      env = environment(obs_formula)
    )
  })
  names(pforms) <- spec$nlpars
  do.call(brms::bf, c(list(top_form, nl = TRUE), pforms))
}


# Emit weakly-informative default priors over the nl parameters
# the rewrite introduces: normal(0, 1) on each gamma coefficient
# block and student_t(3, 0, 2.5) on each species-level SD. User-
# supplied priors merge on top via the existing brms prior
# pipeline (last-wins on matching class/coef/nlpar tuples).
default_trait_slopes_priors <- function(obs_formula) {
  spec <- trait_slopes_nlpar_spec(obs_formula)
  if (length(spec$nlpars) == 0L) {
    return(NULL)
  }
  pri <- lapply(spec$nlpars, function(nm) {
    brms::prior_string("normal(0, 1)", nlpar = nm) +
      brms::prior_string(
        "student_t(3, 0, 2.5)",
        class = "sd", nlpar = nm
      )
  })
  Reduce(`+`, pri)
}
