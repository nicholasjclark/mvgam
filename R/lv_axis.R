# The axis a trend's effects vary along.
#
# `lv_axis()` marks a `trend_formula` term as varying along the
# trend's own axis. That axis is each latent factor of a factor model
# and each series of any other model. `detect_and_rewrite_by_lv()`
# finds every call by name. It replaces each with the trend data's
# column for that axis. brms then builds the term as it builds any
# term on a factor. In a factor model the column is `.trend`. The
# trend data then runs over (time, factor). The leading dot keeps
# `.trend` clear of any column a user supplies.

# The trend data's factor column in a factor model
factor_axis_column <- ".trend"

#' One row per combination of the given columns and latent factor
#'
#' The factor column varies fastest. Each consecutive block of `n_lv`
#' rows then shares one value of the other columns.
#'
#' @param n_lv Integer number of latent factors
#' @param ... Named columns to cross with the factors
#' @return A tibble with the columns and `factor_axis_column`
#' @noRd
expand_factor_axis <- function(n_lv, ...) {
  checkmate::assert_count(n_lv, positive = TRUE)
  factor_col <- stats::setNames(
    list(factor(seq_len(n_lv))), factor_axis_column
  )
  tidyr::expand_grid(..., !!!factor_col)
}

#' Vary a trend effect along the trend's axis
#'
#' Use `lv_axis()` in a `trend_formula` in any position that takes a
#' factor variable, to give each latent factor its own effect. In a
#' model with no latent factors it gives each series its own effect.
#' Use it when a covariate should act differently on different series,
#' such as temperature raising one species and lowering another.
#'
#' For example:
#' \preformatted{
#' trend_formula = ~ s(elev, by = lv_axis()) + AR(n_lv = 2)
#' trend_formula = ~ temp:lv_axis() + AR(n_lv = 2)
#' trend_formula = ~ (1 + temp | lv_axis()) + AR()
#' trend_formula = ~ s(temp, lv_axis(), bs = "fs") + AR()
#' }
#'
#' With `n_lv` set on the trend constructor, each factor
#' `k = 1, ..., n_lv` takes its own effect. The effect of a covariate
#' on series `s` is the sum over factors of the loading `Z[s, k]` times
#' the effect on factor `k`. This is constrained ordination, the model
#' [jsdgam()] fits. Every term of such a `trend_formula` is then
#' evaluated once per factor and time. Each covariate must take one
#' value per time.
#'
#' With no `n_lv`, each series takes its own effect on the latent
#' state.
#'
#' The series column itself is not supported in a `trend_formula`
#' term. Write `lv_axis()` for an effect that differs along the
#' trend's axis. An effect of the series on the observations belongs
#' in `formula`. `by = trend` is deprecated and is treated as
#' `by = lv_axis()`.
#'
#' @return `NULL` (invisibly). The function only marks a term in a
#'   formula.
#'
#' @section What each form estimates:
#' With no latent factors, `lv_axis()` is each series:
#' \itemize{
#'   \item `~ x:lv_axis()` gives a slope of `x` per series.
#'   \item `~ lv_axis()` gives a level per series.
#'   \item `~ (1 + x | lv_axis())` gives levels and slopes that are
#'     partially pooled across series.
#'   \item `~ s(x, by = lv_axis())` gives a smooth of `x` per series,
#'     and `~ s(x, lv_axis(), bs = "fs")` gives smooths that share a
#'     smoothness penalty.
#' }
#' With `n_lv` latent factors, each of these is estimated once per
#' factor. The coefficient of one factor has no scale of its own. The
#' loadings and the factor effects trade scale. The data identify
#' their product. [conditional_effects.mvgam()] and [predict.mvgam()]
#' report that product, the effect of the covariate on each series.
#' [conditional_effects.mvgam()] draws a panel per series. When every
#' `lv_axis()` term is a slope or a level, [summary.mvgam()] reports the
#' per-factor coefficients in the basis of the identified loadings
#' `Z_tilde`.
#'
#' @seealso [jsdgam()] fits per-factor effects across species.
#'   [conditional_effects.mvgam()] plots the effect on each series. The
#'   `r mvgam_article("dfm", "dynamic factor vignette")` fits factor
#'   models. The `r mvgam_article("jsdgam", "JSDM article")` fits
#'   per-factor trait and covariate effects.
#'
#' @examples
#' # `lv_axis()` marks a term in a formula. Called by itself it
#' # returns NULL.
#' lv_axis()
#'
#' \dontrun{
#' # Three series whose seasonal effect differs: 1, 0.5 and -1
#' set.seed(1)
#' dat <- sim_mvgam(family = gaussian(), n_series = 3L,
#'                  n_timepoints = 60L)$data_train
#' dat$season <- sin(2 * pi * dat$time / 12)
#' dat$y <- dat$y + c(1, 0.5, -1)[as.integer(dat$series)] * dat$season
#'
#' # One seasonal slope per series
#' mod <- mvgam(
#'   formula       = y ~ 1,
#'   trend_formula = ~ season:lv_axis() + AR(p = 1),
#'   data          = dat,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod)
#' conditional_effects(mod)
#'
#' # With two latent factors, each factor takes a seasonal slope. The
#' # loadings carry the two slopes to the three series. The prior
#' # table lists one coefficient per factor. A smooth takes the same
#' # marker: `s(season, by = lv_axis())`
#' get_prior(
#'   mvgam_formula(y ~ 1,
#'                 trend_formula = ~ season:lv_axis() + AR(p = 1, n_lv = 2)),
#'   data   = dat,
#'   family = gaussian()
#' )
#' }
#'
#' @family trend models
#' @export
lv_axis <- function() {
  invisible(NULL)
}


# The smooth and Gaussian-process constructors brms fits. Each takes
# its covariates as unnamed arguments and a grouping as `by`.
#'@noRd
MVGAM_SMOOTH_CALLS <- c("s", "t2", "gp")

#' Is an expression a call to `lv_axis()`?
#'
#' Matched on the function called, as `is_trend_term()` matches a
#' trend constructor, which takes `mvgam::lv_axis()` as well.
#'
#' @param expr Unevaluated R expression.
#' @return A single logical.
#' @noRd
is_lv_axis_call <- function(expr) {
  rlang::is_call(expr) && identical(rlang::call_name(expr), "lv_axis")
}

#' Is a `by` argument the deprecated symbol `trend`?
#'
#' @param expr Unevaluated R expression (the `by` argument).
#' @return A single logical.
#' @noRd
is_legacy_trend_symbol <- function(expr) {
  is.symbol(expr) && identical(as.character(expr), "trend")
}

#' Replace each `lv_axis()` of a trend formula with its data column
#'
#' Every `lv_axis()` call becomes the symbol `target`, whatever its
#' position: a smooth's `by`, a parametric interaction, a grouping
#' factor or a factor-smooth margin. The deprecated `by = trend` of
#' a smooth is replaced the same way. A term naming the series column
#' is refused, as `lv_axis()` is how a trend term varies along that
#' axis.
#'
#' @param formula A trend formula with its constructor removed.
#' @param target The column holding the axis: `".trend"` in a factor
#'   model, the series column otherwise.
#' @param series_var The trend's series column, refused in a term.
#'   `NULL` refuses nothing, for a caller that needs the columns of a
#'   formula `extract_and_validate_trend_components()` checks later.
#' @return A list of `has_by_lv`, `n_by_lv`, the number of
#'   replacements, `deprecated_trend_seen` and the rewritten
#'   `formula`.
#' @noRd
detect_and_rewrite_by_lv <- function(formula, target = factor_axis_column,
                                     series_var = NULL) {
  checkmate::assert_class(formula, "formula")
  checkmate::assert_string(target, min.chars = 1L)
  checkmate::assert_string(series_var, null.ok = TRUE)

  rhs <- rlang::f_rhs(formula)
  refuse_series_terms(rhs, series_var)

  state <- new.env(parent = emptyenv())
  state$n_by_lv <- 0L
  state$deprecated_trend_seen <- FALSE
  state$target <- as.symbol(target)
  new_rhs <- walk_by_lv(rhs, state, depth = 0L)

  list(
    has_by_lv = state$n_by_lv > 0L,
    n_by_lv = state$n_by_lv,
    deprecated_trend_seen = state$deprecated_trend_seen,
    formula = rlang::new_formula(
      lhs = rlang::f_lhs(formula),
      rhs = new_rhs,
      env = rlang::f_env(formula)
    )
  )
}

#' Refuse a trend formula whose terms name the series column
#'
#' The trend data stores the resolved axis as `series` whatever the
#' user's column is called. Both names are refused.
#'
#' @param rhs Right-hand side of the trend formula, as a language object
#' @param series_var Character name of the trend's series column, or
#'   `NULL` to refuse nothing
#' @return `TRUE`, invisibly, or an error
#' @noRd
refuse_series_terms <- function(rhs, series_var) {
  checkmate::assert_string(series_var, null.ok = TRUE)
  if (is.null(series_var)) {
    return(invisible(TRUE))
  }
  found <- intersect(all.vars(rhs), c("series", series_var))
  if (length(found) == 0L) {
    return(invisible(TRUE))
  }
  stop(insight::format_error(c(
    paste0("The series column '", found[1L],
           "' is not supported in a 'trend_formula' term."),
    x = "A trend term varies along the trend's own axis through 'lv_axis()'.",
    i = paste0("'lv_axis()' is each latent factor of a factor model ",
               "and each series of any other model."),
    i = paste0("Write 'lv_axis()' in place of '", found[1L],
               "', or move the term to 'formula'.")
  )), call. = FALSE)
}

#' An expression with each `lv_axis()` replaced by its data column
#'
#' The recursion of `detect_and_rewrite_by_lv()`. A smooth's
#' `by = trend` is replaced the same way.
#'
#' @param expr Language object to rewrite
#' @param state Environment holding the `target` symbol and the counters
#'   `n_by_lv` and `deprecated_trend_seen`, updated in place
#' @param depth Integer recursion depth, capped at 50
#' @return The rewritten language object
#' @noRd
walk_by_lv <- function(expr, state, depth = 0L) {
  checkmate::assert_environment(state)
  checkmate::assert_int(depth, lower = 0L)
  if (depth > 50L) {
    stop(insight::format_error(c(
      "Formula nesting too deep (>50 levels).",
      i = "Simplify the trend formula structure."
    )))
  }
  if (is_lv_axis_call(expr)) {
    state$n_by_lv <- state$n_by_lv + 1L
    return(state$target)
  }
  if (!rlang::is_call(expr)) {
    return(expr)
  }
  args <- as.list(expr)
  if (any(call_head_name(expr) %in% MVGAM_SMOOTH_CALLS) &&
        is_legacy_trend_symbol(args[["by"]])) {
    state$n_by_lv <- state$n_by_lv + 1L
    state$deprecated_trend_seen <- TRUE
    args[["by"]] <- state$target
  }
  tail <- lapply(args[-1L], walk_by_lv, state = state, depth = depth + 1L)
  rlang::call2(args[[1L]], !!!tail)
}

#' Name the trend's axis in prior labels as the user wrote it
#'
#' brms labels the trend formula's coefficients and groups with the
#' column that replaced `lv_axis()`, such as
#' `s(month, by = series)`. A user writes `lv_axis()` in the formula
#' and is refused the series column there. The prior table names the
#' term the same way. `to = "user"` writes `lv_axis()` in place of the
#' column. `to = "brms"` writes the column in place of `lv_axis()`.
#' A label holding the column only as part of a longer name, such as
#' the factor level `seriesGreens`, is left alone.
#'
#' @param priors A `brmsprior` of trend rows.
#' @param target The column that replaced `lv_axis()`, or `NULL`
#'   when the formula has none.
#' @param to `"user"` or `"brms"`.
#' @return `priors` with its `coef` and `group` labels translated.
#' @noRd
translate_lv_axis_labels <- function(priors, target, to = c("user", "brms")) {
  to <- match.arg(to)
  if (is.null(target) || is.null(priors) || nrow(priors) == 0L) {
    return(priors)
  }
  checkmate::assert_string(target, min.chars = 1L)
  name_chars <- "[:alnum:]._"
  token <- if (to == "user") {
    gsub("([.])", "\\\\\\1", target)
  } else {
    "lv_axis\\(\\)"
  }
  pattern <- paste0("(?<![", name_chars, "])", token,
                    "(?![", name_chars, "(])")
  replacement <- if (to == "user") "lv_axis()" else target
  for (col in intersect(c("coef", "group"), names(priors))) {
    priors[[col]] <- gsub(pattern, replacement, priors[[col]], perl = TRUE)
  }
  priors
}

#' The columns `detect_and_rewrite_by_lv()` writes for `lv_axis()`
#'
#' `.trend` is the factor axis. `series` is the axis a trend derives
#' from a grouping or from the responses. A fit whose data names its
#' series passes that column too.
#'
#' @param series_var Character name of the fit's series column, or
#'   `NULL`
#' @return Character vector of column names
#' @noRd
by_lv_rewrite_tokens <- function(series_var = NULL) {
  checkmate::assert_string(series_var, null.ok = TRUE)
  unique(c(factor_axis_column, "series", series_var))
}


#' Did the user write `lv_axis()` in the `trend_formula`?
#'
#' `extract_and_validate_trend_components()` records this on
#' `trend_metadata`, for a factor model and for any other.
#' `conditional_effects.mvgam()` uses it to name its panels without
#' the column that replaced `lv_axis()`.
#'
#' @param object A fitted `mvgam` object.
#' @return A single logical.
#' @noRd
mvgam_had_by_lv <- function(object) {
  isTRUE(object$trend_metadata$had_by_lv)
}


#' Drop the columns that replaced `lv_axis()` from effect groupings
#'
#' `conditional_effects.mvgam()` names each panel by its grouping. It
#' passes the full grouping to `marginaleffects::plot_predictions()`,
#' which keeps the facet for each series. A grouping holding only such
#' a column is returned as it is. Its panel then keeps a name.
#'
#' @param cond_labs List of character vectors (each one a
#'   conditional-effects grouping).
#' @param had_by_lv Logical scalar. When `FALSE`, returns
#'   `cond_labs` unchanged so non-by-lv fits keep their full labels.
#' @param series_var The fit's series column, or `NULL`.
#' @return List of character vectors, same length as `cond_labs`.
#' @noRd
strip_by_lv_rewrite_tokens <- function(cond_labs, had_by_lv,
                                       series_var = NULL) {
  if (!isTRUE(had_by_lv)) {
    return(cond_labs)
  }
  rewrites <- by_lv_rewrite_tokens(series_var)
  lapply(cond_labs, function(g) {
    stripped <- setdiff(g, rewrites)
    if (length(stripped) == 0L) g else stripped
  })
}


#' Warn that `by = trend` is deprecated
#'
#' Raised once per session through `warn_once()`. The validator calls
#' it when `detect_and_rewrite_by_lv()` reports `deprecated_trend_seen`.
#'
#' @noRd
warn_legacy_trend_by <- function() {
  warn_once(
    c(
      "'by = trend' in 'trend_formula' is deprecated.",
      i = "Use 'by = lv_axis()', which fits the same model."
    ),
    "mvgam_by_trend_deprecated"
  )
}
