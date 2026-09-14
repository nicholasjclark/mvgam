#' insight S3 methods for mvgam objects
#'
#' Minimum surface that `marginaleffects` and `insight` need to build
#' prediction grids and walk formula structure. mvgam keeps its
#' observation submodel formula in `x$formula` (a `brmsformula`) and
#' its trend submodel formula in `x$trend_formula`; these methods
#' stitch both into the structures insight expects rather than
#' relying on the default formula walker, which chokes on the
#' combined obs + trend representation.
#'
#' @name mvgam_insight
#' @keywords internal
NULL


# Pull the observation submodel's two-sided formula out of `$formula`,
# which may be either a brmsformula or a bare formula. Used by several
# of the methods below.
mvgam_obs_formula <- function(x) {
  obs_arm_main_formula(x$formula)
}


# Drop tokens that are not actual columns of the fit's data frame.
# brms RE syntax carries correlation IDs (`(1 | sp | series)`) and
# nested-group separators as if they were variables; this filter
# enforces "predictor names are addressable in the model data".
# Skips the filter when data is unavailable (mock objects in unit
# tests) so the helper composes with stub fixtures.
#
# The empty-obs placeholder is dropped here as well. It is absent
# from `x$data`, so the filter below removes it on a fit, but the
# rewritten formula still names it and mock objects carry no frame
# for the filter to work from.
mvgam_keep_data_columns <- function(vars, x) {
  vars <- setdiff(vars, MVGAM_EMPTY_OBS_PLACEHOLDER)
  if (is.null(x$data)) {
    return(vars)
  }
  intersect(vars, colnames(x$data))
}


#' @importFrom insight find_formula
#' @export
find_formula.mvgam <- function(x, verbose = TRUE, ...) {
  f <- list(conditional = mvgam_obs_formula(x))
  if (!is.null(x$trend_formula)) {
    f$trend <- x$trend_formula
  }
  class(f) <- c("insight_formula", "list")
  f
}


#' @importFrom insight find_response
#' @export
find_response.mvgam <- function(x, combine = TRUE, ...) {
  # The columns, not brms's keys for them: insight's callers read the
  # answer off the data, and a multivariate fit returns every response
  # so marginaleffects finds each one.
  unname(response_columns(x))
}


# The variables one parsed formula contributes, by the part of the
# model they belong to. brms has already separated them: `fe` holds
# the parametric terms, `sm` the smooths, `gp` the Gaussian
# processes, `sp` the special terms such as `mo()`, `offset` the
# offset and `re` a row per group-level term. Reading each slot is
# what keeps a covariate that reaches the linear predictor only
# through a smooth or a Gaussian process from being dropped.
#'@noRd
mvgam_brmsterms_parts <- function(bt) {
  # A non-linear formula keeps its sub-formulas under `nlpars`, and
  # their covariates appear in no `dpars` entry: the predictor above
  # them enumerates parameter names instead. Both are walked, and the
  # parameter names are dropped, since `a` and `b` name no column.
  nlpars <- bt$nlpars %||% list()
  dpars <- c(bt$dpars %||% list(), nlpars)
  from_slot <- function(slot) {
    unique(unlist(
      lapply(dpars, function(d) all.vars(d[[slot]])),
      use.names = FALSE
    ))
  }
  # An offset is a known quantity the prediction carries, not a term
  # a reader takes a slope over, so it is kept apart from the rest.
  offset <- from_slot("offset")
  # A non-linear predictor has no `fe`: its own terms enumerate
  # parameter names, and the covariates they are built from are
  # recorded under `covars`.
  conditional <- setdiff(
    unique(c(from_slot("fe"), from_slot("sm"), from_slot("gp"),
             from_slot("sp"), from_slot("cs"), from_slot("covars"))),
    c(offset, names(nlpars))
  )
  # Indexed exactly. A distributional parameter with no group-level
  # term has no `re` entry, and `$` would partial-match it to `resp`,
  # handing back a character vector that answers as though the
  # parameter had one.
  groups <- unique(unlist(
    lapply(dpars, function(d) {
      re <- d[["re"]]
      if (NROW(re) == 0L) NULL else as.character(re$group)
    }),
    use.names = FALSE
  ))
  # The terms as written, which is what a drawn effect is keyed by: an
  # interaction is one effect over two columns, and a smooth of two
  # covariates is one effect over both. `stats::terms()` files an
  # offset under its own attribute, so an offset never appears here.
  labels <- unique(unlist(
    lapply(dpars, function(d) {
      unlist(lapply(c("fe", "sm", "gp", "sp", "covars"), function(s) {
        if (is.null(d[[s]])) {
          return(NULL)
        }
        attr(stats::terms(d[[s]], keep.order = TRUE), "term.labels")
      }), use.names = FALSE)
    }),
    use.names = FALSE
  ))
  list(
    conditional = conditional,
    random = groups,
    # A `trials()` denominator or a truncation bound is a column the
    # likelihood reads. It is no one's predictor, and a grid built
    # without it is refused by brms.
    aterms = unique(unlist(lapply(bt$adforms, all.vars),
                           use.names = FALSE)),
    offset = offset,
    labels = setdiff(labels, names(nlpars)),
    all = all.vars(bt$allvars)
  )
}


# One side of the model, parsed once.
#
# The stored `brmsformula` carries no family. `brmsterms()` then
# defaults to gaussian and refuses `trials()` as unsupported for it,
# which is why the fit's own family is attached before parsing. A
# multivariate formula keeps no top-level `dpars`: each response has
# its own parse under `$terms`, and the parts are unioned.
#'@noRd
mvgam_side_terms <- function(f, family = NULL) {
  empty <- list(conditional = character(0L), random = character(0L),
                aterms = character(0L), offset = character(0L),
                labels = character(0L), all = character(0L))
  if (is.null(f)) {
    return(empty)
  }
  if (!is.null(family)) {
    if (inherits(f, "mvbrmsformula")) {
      # Each arm of a multivariate formula carries its own family,
      # and mvgam records one on the fit. An arm left without one
      # reads as neither gaussian nor student, which is what brms
      # requires of every arm before it will estimate `rescor`.
      f$forms <- lapply(f$forms, function(arm) {
        if (is.null(arm$family)) arm$family <- family
        arm
      })
    } else if (inherits(f, "brmsformula")) {
      f$family <- family
    }
  }
  # A trend submodel is written without a response. brms parses a
  # two-sided formula, and the name given here reaches no column, so
  # it is filtered out with every other name the data does not carry.
  if (inherits(f, "formula") && length(f) == 2L) {
    f <- stats::reformulate(deparse(f[[2L]]), response = ".mvgam_lhs")
  }
  bt <- brms::brmsterms(f)
  if (is.null(bt$terms)) {
    return(mvgam_brmsterms_parts(bt))
  }
  per_resp <- lapply(bt$terms, mvgam_brmsterms_parts)
  fold <- function(nm) {
    unique(unlist(lapply(per_resp, `[[`, nm), use.names = FALSE))
  }
  list(
    conditional = fold("conditional"), random = fold("random"),
    aterms = fold("aterms"), offset = fold("offset"),
    labels = fold("labels"),
    all = unique(c(fold("all"), all.vars(bt$allvars)))
  )
}


#' Every term the model has, split the way its readers ask for it
#'
#' One parse, read by `find_predictors()`, `find_random()`,
#' `find_variables()`, `terms()`, `model.frame()` and the effect
#' groupings `conditional_effects()` draws. Asking twice is how a
#' grouping factor came to be offered as a population slope while
#' `find_random()` reported nothing at all.
#'
#' The axis columns are the distinction the split turns on. Which
#' occasion a row belongs to is addressable in a prediction grid, so
#' `time` and `series` stay in the variable list. Neither supports a
#' slope or a contrast, so neither is a predictor. A column holding
#' one value supports neither either, which is why the axis entries
#' are kept only while they vary.
#'
#' @param x A fitted `mvgam` object
#' @return A list of character vectors: `response`, `conditional`,
#'   `random`, `aterms`, `offset` and `index`, each filtered to
#'   columns the model's data carries
#' @noRd
mvgam_term_list <- function(x) {
  obs <- mvgam_side_terms(x$formula, x$family)
  trend <- mvgam_side_terms(x$trend_formula)
  keep <- function(v) mvgam_keep_data_columns(unique(v), x)

  # The axis and the groupings it is built on. jsdgam aliases the
  # user's species column to `series`, and the original name is kept
  # so a grid can still be addressed in the user's own terms.
  # Both records are read defensively: a fit saved before either was
  # stored carries something other than a list there, and a term list
  # is not the place to refuse one.
  as_list <- function(v) if (is.list(v)) v else list()
  meta <- as_list(as_list(x$trend_metadata)$variables)
  jsdgam_meta <- as_list(attr(x$model_data, "prepped_trend_model"))
  index <- varying_meta_vars(
    c(meta$time_var, meta$series_var, meta$gr_var, meta$subgr_var,
      unlist(jsdgam_meta[c("unit", "species")], use.names = FALSE)),
    x
  )
  index <- keep(index)

  # A column a formula names is a term of the model, whatever else it
  # also is. `series` names the axis, and a model written
  # `y ~ series` asks for a per-series effect as well: the axis entry
  # is what a grid addresses, and the formula is what makes it a term.
  # Only the axis columns no formula mentions are grid-only.
  conditional <- keep(c(obs$conditional, trend$conditional))

  # The same terms, grouped as they are drawn. `conditional_effects()`
  # plots one panel per grouping, so an interaction is one entry over
  # two columns and a three-way smooth is its three pairwise margins.
  groupings <- unlist(
    lapply(unique(c(obs$labels, trend$labels)), split_term_labels),
    recursive = FALSE
  )
  groupings <- lapply(groupings, function(g) keep(g))
  groupings <- groupings[lengths(groupings) > 0L]
  groupings <- groupings[!duplicated(vapply(
    groupings, paste, character(1L), collapse = ":"
  ))]

  list(
    response = keep(unname(response_columns(x))),
    conditional = conditional,
    random = setdiff(keep(c(obs$random, trend$random)), conditional),
    aterms = setdiff(keep(c(obs$aterms, trend$aterms)), conditional),
    offset = keep(c(obs$offset, trend$offset)),
    index = setdiff(index, conditional),
    groupings = groupings
  )
}


#' @importFrom insight find_predictors
#' @export
find_predictors.mvgam <- function(x, effects = "fixed",
                                  component = "conditional",
                                  flatten = FALSE, verbose = TRUE, ...) {
  effects <- match.arg(effects, c("fixed", "random", "all"))
  terms_list <- mvgam_term_list(x)
  out <- list()
  # An element is left out when it is empty, which is how insight
  # answers for a model with no predictor: `$conditional` is absent
  # and reads as NULL.
  if (effects %in% c("fixed", "all") && length(terms_list$conditional)) {
    out$conditional <- terms_list$conditional
  }
  if (effects %in% c("random", "all") && length(terms_list$random)) {
    out$random <- terms_list$random
  }
  # `insight::find_variables()` is not a generic. It composes
  # `find_response()` with this list, and `marginaleffects::datagrid()`
  # builds its grid from that composition, so a column reaching the
  # grid has no other route. The axis a per-series facet is drawn on
  # and the denominator of a `trials()` term are both read by the
  # model without anyone taking a slope over them, and they ride
  # here under a name of their own:
  # `marginaleffects::get_predictors()` keeps only the components it
  # knows, so nothing filed here is offered as a term to contrast.
  grid_only <- unique(c(terms_list$index, terms_list$aterms,
                        terms_list$offset))
  if (length(grid_only)) {
    out$grid <- grid_only
  }
  if (flatten) unique(unlist(out, use.names = FALSE)) else out
}


#' @importFrom insight find_random
#' @export
find_random.mvgam <- function(x, split_nested = FALSE, flatten = FALSE,
                              ...) {
  groups <- mvgam_term_list(x)$random
  if (!length(groups)) {
    return(NULL)
  }
  if (isTRUE(split_nested)) {
    groups <- unique(unlist(strsplit(groups, ":", fixed = TRUE),
                            use.names = FALSE))
  }
  if (flatten) groups else list(random = groups)
}


#' Model terms for a fitted mvgam object
#'
#' `terms()` is how a caller discovers a model's structure without
#' knowing its class, and it is the accessor `model.frame()` is
#' normally paired with. The object is built from the terms the model
#' has: its responses on the left, the covariates of both submodels on
#' the right. Group-level terms are not among them, since the bar
#' syntax has no meaning to `stats::terms()`; read them with
#' [insight::find_random()].
#'
#' @param x A fitted `mvgam` object.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class `terms`.
#' @export
#' @method terms mvgam
terms.mvgam <- function(x, ...) {
  checkmate::assert_class(x, "mvgam")
  rlang::check_dots_empty()
  terms_list <- mvgam_term_list(x)
  labels <- terms_list$conditional
  response <- terms_list$response
  lhs <- if (length(response) > 1L) {
    str2lang(paste0("cbind(", paste(response, collapse = ", "), ")"))
  } else if (length(response) == 1L) {
    str2lang(response)
  } else {
    NULL
  }
  stats::terms(stats::reformulate(
    if (length(labels)) labels else "1",
    response = lhs
  ))
}




# Internal: keep the meta variables that take more than one value in
# the training data.
#
# Reason: time, series and the grouping columns are surfaced so
# `marginaleffects::datagrid()` can address them, but the same list
# is what `slopes()` and `comparisons()` iterate over when the caller
# names no variables. A column holding a single value supports
# neither a slope nor a contrast, and asking for one aborts the whole
# call, so a single-series fit could not be handed to `avg_slopes()`
# at all. A meta variable that genuinely varies is still reported.
#'@noRd
varying_meta_vars <- function(vars, x) {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (!length(vars)) return(character(0))
  dat <- mvgam_training_data(x)
  if (is.null(dat)) return(vars)
  keep <- vapply(vars, function(v) {
    if (!v %in% names(dat)) return(TRUE)
    length(unique(dat[[v]][!is.na(dat[[v]])])) > 1L
  }, logical(1L))
  vars[keep]
}


#' @importFrom insight get_data
#' @export
get_data.mvgam <- function(x, effects = "all", component = "all",
                           source = "environment", verbose = TRUE, ...) {
  if (is.null(x$data)) {
    stop(insight::format_error(c(
      "No data found in mvgam object.",
      i = "Refit the model and ensure the data is stored on the object."
    )))
  }
  x$data
}


#' Extract a model frame from a fitted mvgam object
#'
#' Returns the model's training data restricted to the response plus
#' the variables `find_predictors()` reports. Used by upstream
#' consumers (`insight`, `marginaleffects::datagrid`) when they walk
#' the model frame to build prediction grids.
#'
#' @param formula A fitted `mvgam` object.
#' @param trend_effects Logical; if `TRUE` and the model has a trend
#'   formula, return the trend submodel's covariates only.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A data frame.
#' @export
model.frame.mvgam <- function(formula, trend_effects = FALSE, ...) {
  checkmate::assert_class(formula, "mvgam")
  checkmate::assert_logical(trend_effects, len = 1L)
  rlang::check_dots_empty()
  if (trend_effects && is.null(formula$trend_formula)) {
    return(NULL)
  }
  if (is.null(formula$data)) {
    stop(insight::format_error(c(
      "No data found in mvgam object.",
      i = "Refit the model and ensure the data is stored on the object."
    )))
  }
  vars <- if (trend_effects) {
    mvgam_side_terms(formula$trend_formula)$all
  } else {
    # Every variable the model reads: the responses, the covariates
    # of both submodels, the groupings, the addition terms beside the
    # response and the axis the rows are indexed by.
    unique(unlist(mvgam_term_list(formula), use.names = FALSE))
  }
  vars <- intersect(vars, colnames(formula$data))
  formula$data[, vars, drop = FALSE]
}


# Families mvgam classifies for `insight::model_info()`, keyed on the
# name `resolve_family_name()` reports.
#
# Reason: every family mvgam defines itself carries the literal
# `family$family == "custom"`, so reading that field classified all ten
# of them as nothing at all. `model_info()` is what marginaleffects and
# the easystats packages read to decide how to treat a fit, so a blank
# answer there silently changes their behaviour.
#'@noRd
mvgam_model_info_families <- function() {
  list(
    binomial = c(
      "binomial", "bernoulli", "beta_binomial", "com_binomial",
      "zero_inflated_binomial", "zero_inflated_beta_binomial", "occ"
    ),
    count = c(
      "poisson", "negbinomial", "negbinomial2", "negative_binomial",
      "geometric", "zero_inflated_poisson", "zero_inflated_negbinomial",
      "hurdle_poisson", "hurdle_negbinomial", "com_poisson",
      "discrete_weibull", "beta_nb", "nmix", "nmix_royle_nichols",
      "nmix_poisson_poisson"
    ),
    continuous = c(
      "gaussian", "student", "skew_normal", "gamma", "lognormal",
      "shifted_lognormal", "exponential", "weibull", "frechet",
      "inverse.gaussian", "exgaussian", "asym_laplace", "von_mises",
      "tweedie", "mvn", "mvt", "hurdle_gamma", "hurdle_lognormal"
    ),
    categorical = c("categorical", "categ"),
    proportion = c(
      "beta", "zero_inflated_beta", "zero_one_inflated_beta"
    ),
    multinomial = c("multinomial", "dirichlet_multinomial", "multi"),
    simplex = c("dirichlet", "dirichlet2", "diri")
  )
}


#' @importFrom insight model_info
#' @export
model_info.mvgam <- function(x, response = NULL, ...) {
  # A multivariate fit gives each response its own family, and
  # insight describes a brms fit once per response. Reading `$family`
  # alone described every arm as the last.
  resolve_resp(x, response)
  keys <- names(response_columns(x))
  if (is.null(response) && length(keys) > 1L) {
    return(lapply(stats::setNames(keys, keys), function(r) {
      model_info.mvgam(x, response = r, ...)
    }))
  }
  fam <- model_families(x, response)
  fam_name <- resolve_family_name(fam)
  link <- fam$link %||% NA_character_
  cls <- mvgam_model_info_families()

  # Group-level terms show up as `sd_` scales and `r_` deviations. A
  # prefit has no draws to show them.
  pars <- if (is.null(x$fit)) character(0) else variables(x)

  list(
    is_binomial = fam_name %in% cls$binomial,
    is_count = fam_name %in% cls$count,
    is_continuous = fam_name %in% cls$continuous,
    is_ordinal = is_ordinal_family(fam),
    is_categorical = fam_name %in% cls$categorical,
    is_multinomial = fam_name %in% cls$multinomial,
    is_dirichlet = fam_name %in% cls$simplex,
    is_zero_inflated = grepl("zero_inflated|zero_one_inflated|hurdle",
                             fam_name),
    is_proportion = fam_name %in% cls$proportion,
    is_linear = identical(link, "identity"),
    is_logit = identical(link, "logit"),
    is_probit = identical(link, "probit"),
    is_log = identical(link, "log"),
    is_mixed = any(grepl("^sd_|^r_", pars)),
    is_multivariate = isTRUE(brms::is.mvbrmsformula(x$formula)),
    is_bayesian = TRUE,
    family = fam_name,
    link_function = link,
    n_obs = nrow(x$data %||% data.frame())
  )
}
