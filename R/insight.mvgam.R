#' insight S3 methods for mvgam objects
#'
#' Minimum surface that `marginaleffects` and `insight` need to build
#' prediction grids and walk formula structure. mvgam keeps its
#' observation submodel formula in `x$formula` (a `brmsformula`) and
#' its trend submodel formula in `x$trend_formula`. These methods
#' combine both into the structures insight expects. The default
#' formula walker fails on the combined observation and trend
#' representation.
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
# brms group-level syntax holds correlation IDs (`(1 | sp | series)`)
# and nested-group separators in the place of variables. The filter
# keeps the names the model data holds as columns. Mock objects in
# unit tests hold no data. The filter then returns the names as they
# are.
#
# The empty-observation placeholder is dropped as well. It is absent
# from `x$data`, which removes it on a fit. A mock object has no frame
# to remove it with.
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
  # insight's callers index the data with the result. The function
  # returns column names in place of brms's response keys. A
  # multivariate fit returns every response column, one for each
  # response marginaleffects looks up.
  unname(response_columns(x))
}


# The variables one parsed formula contributes, by the part of the
# model they belong to. brms has already separated them: `fe` holds
# the parametric terms, `sm` the smooths, `gp` the Gaussian
# processes, `sp` the special terms such as `mo()`, `offset` the
# offset and `re` a row per group-level term. Every slot is collected.
# A covariate that reaches the linear predictor only through a smooth
# or a Gaussian process then stays in the list.
#'@noRd
mvgam_brmsterms_parts <- function(bt) {
  # A non-linear formula keeps its sub-formulas under `nlpars`. Their
  # covariates appear in no `dpars` entry, whose predictor lists
  # parameter names. Both are collected. The parameter names `a` and
  # `b` name no column and are dropped.
  nlpars <- bt$nlpars %||% list()
  dpars <- c(bt$dpars %||% list(), nlpars)
  from_slot <- function(slot) {
    unique(unlist(
      lapply(dpars, function(d) all.vars(d[[slot]])),
      use.names = FALSE
    ))
  }
  # An offset is a known quantity the prediction carries. No slope is
  # taken over it. It is kept apart from the terms.
  offset <- from_slot("offset")
  # A non-linear predictor has no `fe`. Its own terms list parameter
  # names. brms records the covariates those parameters use under
  # `covars`.
  conditional <- setdiff(
    unique(c(from_slot("fe"), from_slot("sm"), from_slot("gp"),
             from_slot("sp"), from_slot("cs"), from_slot("covars"))),
    c(offset, names(nlpars))
  )
  # Indexed exactly. A distributional parameter with no group-level
  # term has no `re` entry. `$` would partial-match `re` to `resp` and
  # return a character vector, as though the parameter had one.
  groups <- unique(unlist(
    lapply(dpars, function(d) {
      re <- d[["re"]]
      if (NROW(re) == 0L) NULL else as.character(re$group)
    }),
    use.names = FALSE
  ))
  # A covariate whose slope varies by group may appear in no other
  # term. The design of the group-level term still needs its column.
  slopes <- unique(unlist(
    lapply(dpars, function(d) {
      re <- d[["re"]]
      if (NROW(re) == 0L) NULL else lapply(re$form, all.vars)
    }),
    use.names = FALSE
  ))
  # The terms as written key the drawn effects. An interaction is one
  # effect over two columns. A smooth of two covariates is one effect
  # over both. `stats::terms()` files an offset under its own
  # attribute, outside these labels.
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
    slopes = setdiff(slopes, c(conditional, offset)),
    # A `trials()` denominator or a truncation bound is a column the
    # likelihood uses. It is no predictor. brms refuses a grid that
    # lacks it.
    aterms = unique(unlist(lapply(bt$adforms, all.vars),
                           use.names = FALSE)),
    offset = offset,
    labels = setdiff(labels, names(nlpars)),
    all = all.vars(bt$allvars)
  )
}


# Parses of one side of a model, keyed by `side_terms_key()`. A
# prediction checks its frame against the terms of both formulas once
# per component. Each check would parse them again.
#'@noRd
.side_terms_cache <- new.env(parent = emptyenv())


# One side of the model, parsed once per session.
#
# The stored `brmsformula` holds no family. `brmsterms()` then
# defaults to gaussian and refuses `trials()` as unsupported for it.
# The fit's own family is attached before parsing. A multivariate
# formula keeps no top-level `dpars`. Each response has its own parse
# under `$terms`. The function returns one response's parts when
# `resp` names it, or the union over responses.
#'@noRd
mvgam_side_terms <- function(f, family = NULL, resp = NULL) {
  checkmate::assert_string(resp, null.ok = TRUE)
  if (is.null(f)) {
    return(list(conditional = character(0L), random = character(0L),
                slopes = character(0L),
                aterms = character(0L), offset = character(0L),
                labels = character(0L), all = character(0L)))
  }
  key <- side_terms_key(f, family, resp)
  .side_terms_cache[[key]] %||%
    (.side_terms_cache[[key]] <- parse_side_terms(f, family, resp))
}


#' The cache key of one side's parse
#'
#' A hash of the formula's structure, its attributes included. A
#' nonlinear `bf()` differs from a linear one only in an attribute.
#' Each formula's environment is dropped first. `brmsterms()` takes
#' the variable names from the symbols. An environment holding the
#' caller's data would make the hash slow.
#'
#' @inheritParams mvgam_side_terms
#' @return A string
#' @noRd
side_terms_key <- function(f, family, resp) {
  bare <- rapply(
    list(f),
    function(x) {
      attr(x, ".Environment") <- NULL
      x
    },
    classes = "formula", how = "replace"
  )
  rlang::hash(list(bare, family$family, family$name, family$link, resp))
}


#' The parse `mvgam_side_terms()` caches
#'
#' @inheritParams mvgam_side_terms
#' @return The list `mvgam_side_terms()` returns
#' @noRd
parse_side_terms <- function(f, family, resp) {
  if (!is.null(family)) {
    if (inherits(f, "mvbrmsformula")) {
      # Each response of a multivariate formula holds its own family.
      # mvgam records one on the fit. A response formula without one
      # counts as neither gaussian nor student. brms requires one of
      # those two of every response before it estimates `rescor`.
      f$forms <- lapply(f$forms, function(arm) {
        if (is.null(arm$family)) arm$family <- family
        arm
      })
    } else if (inherits(f, "brmsformula")) {
      f$family <- family
    } else if (inherits(f, "formula") && length(f) == 3L) {
      f <- brms::bf(f, family = family)
    }
  }
  # A trend submodel is written without a response. brms parses a
  # two-sided formula. The placeholder response names no column. It is
  # filtered out with every other name absent from the data.
  if (inherits(f, "formula") && length(f) == 2L) {
    f <- stats::reformulate(deparse(f[[2L]]), response = ".mvgam_lhs")
  }
  # brms announces its default for an unset `rescor` on every parse.
  # `silent = 2` has no effect here. The residual correlation adds no
  # column. Fixing it on this copy leaves the terms unchanged.
  if (inherits(f, "mvbrmsformula") && is.null(f$rescor)) {
    f$rescor <- FALSE
  }
  bt <- brms_terms(f)
  if (is.null(bt$terms)) {
    return(mvgam_brmsterms_parts(bt))
  }
  # One response of a multivariate formula takes its own parse
  if (!is.null(resp)) {
    return(mvgam_brmsterms_parts(bt$terms[[resp]]))
  }
  per_resp <- lapply(bt$terms, mvgam_brmsterms_parts)
  fold <- function(nm) {
    unique(unlist(lapply(per_resp, `[[`, nm), use.names = FALSE))
  }
  list(
    conditional = fold("conditional"), random = fold("random"),
    slopes = fold("slopes"),
    aterms = fold("aterms"), offset = fold("offset"),
    labels = fold("labels"),
    all = unique(c(fold("all"), all.vars(bt$allvars)))
  )
}


#' Every term of the model, split for the functions that use them
#'
#' One parse serves `find_predictors()`, `find_random()`,
#' `find_variables()`, `terms()`, `model.frame()` and the effect
#' groupings `conditional_effects()` draws. Two separate parses once
#' offered a grouping factor as a population slope while
#' `find_random()` reported nothing at all.
#'
#' The split turns on the axis columns. A prediction grid addresses the
#' occasion of each row. `time` and `series` stay in the variable list
#' for that grid. Neither supports a slope or a contrast. Neither is a
#' predictor. A column holding one value supports neither. The axis
#' entries are kept only while they vary.
#'
#' @param x A fitted `mvgam` object
#' @param resp Optional response of a multivariate model. Its own
#'   observation terms replace those of every response. The trend terms
#'   are shared by all responses.
#' @return A list of character vectors: `response`, `conditional`,
#'   `random`, `aterms`, `offset`, `index` and `groupings`, each
#'   filtered to columns the model's data carries
#' @noRd
mvgam_term_list <- function(x, resp = NULL) {
  obs <- mvgam_side_terms(x$formula, x$family, resp = resp)
  trend <- mvgam_side_terms(x$trend_formula)
  keep <- function(v) mvgam_keep_data_columns(unique(v), x)

  # The axis columns and the grouping columns that define them.
  # jsdgam aliases the user's species column to `series`. The original
  # name stays in the list. A grid can then name species the way the
  # user does.
  jsdgam_meta <- jsdgam_axis_names(x)
  index <- varying_meta_vars(
    c(unlist(mvgam_axes(x)$vars, use.names = FALSE),
      unlist(jsdgam_meta[c("unit", "species")], use.names = FALSE)),
    x
  )
  index <- keep(index)

  # A column a formula names is a term of the model, whatever else it
  # also is. `series` names the axis. A model written `y ~ series`
  # also asks for a per-series effect. The axis entry makes the column
  # addressable in a grid. The formula makes it a term. Only the axis
  # columns no formula mentions are grid-only.
  conditional <- keep(c(obs$conditional, trend$conditional))

  # The same terms, grouped as they are drawn. `conditional_effects()`
  # plots one panel per grouping. An interaction is one entry over two
  # columns. A three-way smooth is its three pairwise margins.
  groupings <- unlist(
    lapply(unique(c(obs$labels, trend$labels)), split_term_labels),
    recursive = FALSE
  )
  # A grouping's first variable is the x axis of its panel. Its second
  # splits the panel. `terms()` orders an interaction by first
  # appearance. `~ lv_axis() + x:lv_axis()` then gave `series:x`, a
  # panel over the series with `x` as colours. Placing the series
  # second draws a slope per series, as a smooth per series is drawn.
  series_var <- axis_vars(x)$series_var
  groupings <- lapply(groupings, function(g) {
    g <- keep(g)
    if (length(g) > 1L && series_var %in% g) {
      rest <- setdiff(g, series_var)
      g <- c(rest[1L], series_var, rest[-1L])
    }
    g
  })
  groupings <- groupings[lengths(groupings) > 0L]
  groupings <- groupings[!duplicated(vapply(
    groupings, paste, character(1L), collapse = ":"
  ))]

  list(
    response = keep(unname(response_columns(x))),
    conditional = conditional,
    random = setdiff(keep(c(obs$random, trend$random)), conditional),
    slopes = setdiff(keep(c(obs$slopes, trend$slopes)), conditional),
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
  # An empty element is left out, as insight does for a model with no
  # predictor. `$conditional` is then absent and returns NULL.
  if (effects %in% c("fixed", "all") && length(terms_list$conditional)) {
    out$conditional <- terms_list$conditional
  }
  if (effects %in% c("random", "all") && length(terms_list$random)) {
    out$random <- terms_list$random
  }
  # `insight::find_variables()` dispatches on no class. It combines
  # `find_response()` with this list. `marginaleffects::datagrid()`
  # builds its grid from that combination, the one route a column has
  # into the grid. The axis a per-series facet is drawn on and the
  # denominator of a `trials()` term enter the model with no slope
  # taken over them. They are filed here under `grid`.
  # `marginaleffects::get_predictors()` keeps only the components it
  # knows. A column filed under `grid` is never offered as a term to
  # contrast.
  grid_only <- unique(c(terms_list$index, terms_list$slopes,
                        terms_list$aterms, terms_list$offset))
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
#' `terms()` reports a model's structure to a caller that does not know
#' its class. `model.frame()` is the accessor usually paired with it.
#' The object holds the model's responses on the left and the
#' covariates of both submodels on the right. `stats::terms()` has no
#' meaning for the bar syntax of group-level terms.
#' [insight::find_random()] returns those.
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


#' The meta variables that vary in the training data
#'
#' Time, series and the grouping columns are reported for
#' `marginaleffects::datagrid()` to address. `slopes()` and
#' `comparisons()` iterate over the same list when the caller names no
#' variables. A column with a single value supports neither a slope nor
#' a contrast. Requesting one aborts the call. Dropping such columns
#' lets a single-series fit reach `avg_slopes()`. A column absent from
#' the training data is kept.
#'
#' @param vars Character vector of candidate meta variable names, or
#'   `NULL`
#' @param x Fitted `mvgam` object
#' @return Character vector of the names that vary
#' @noRd
varying_meta_vars <- function(vars, x) {
  checkmate::assert_character(vars, null.ok = TRUE)
  checkmate::assert_class(x, "mvgam")
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (!length(vars)) return(character(0))
  dat <- x$data
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
    # Every variable the model uses: the responses, the covariates of
    # both submodels, the groupings, the addition terms of the response
    # and the axis that indexes the rows.
    unique(unlist(mvgam_term_list(formula), use.names = FALSE))
  }
  vars <- intersect(vars, colnames(formula$data))
  formula$data[, vars, drop = FALSE]
}


# Families mvgam classifies for `insight::model_info()`, keyed on the
# name `resolve_family_name()` reports.
#
# Reason: every family mvgam defines itself has the literal
# `family$family == "custom"`. Classifying by that field left all ten
# of them unclassified. marginaleffects and the easystats packages use
# `model_info()` to decide how to treat a fit. A blank classification
# changes their behaviour without a message.
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
  # A multivariate fit gives each response its own family. insight
  # describes a brms fit once per response. `$family` alone described
  # every response by the family of the last.
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
    is_mixed = any(mvgam_par_kind(pars) %in% c("ranef_sd", "ranef_coef")),
    is_multivariate = isTRUE(brms::is.mvbrmsformula(x$formula)),
    is_bayesian = TRUE,
    family = fam_name,
    link_function = link,
    n_obs = nrow(x$data %||% data.frame())
  )
}
