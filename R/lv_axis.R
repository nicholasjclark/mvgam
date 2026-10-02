# The axis a trend's effects vary along.
#
# `lv_axis()` marks a `trend_formula` term as varying along the
# trend's own axis: each latent factor of a factor model, and each
# series of any other model. `detect_and_rewrite_by_lv()` finds every
# call by name and replaces it with the column the trend data holds
# for that axis, and brms then builds the term as it builds any term
# on a factor. In a factor model the column is `.trend`, and the
# trend data runs over (time, factor). The leading dot keeps `.trend`
# clear of any column a user supplies.

#' Vary a trend effect along the trend's axis
#'
#' Use `lv_axis()` in a `trend_formula` in any position that takes a
#' factor variable, to give each latent factor its own effect. In a
#' model with no latent factors it gives each series its own effect.
#'
#' \preformatted{
#' trend_formula = ~ s(elev, by = lv_axis()) + AR(n_lv = 2)
#' trend_formula = ~ temp:lv_axis() + AR(n_lv = 2)
#' trend_formula = ~ (1 + temp | lv_axis()) + AR()
#' trend_formula = ~ s(temp, lv_axis(), bs = "fs") + AR()
#' }
#'
#' With `n_lv` set on the trend constructor, each factor
#' `k = 1, ..., n_lv` takes its own effect, and the effect of a
#' covariate on series `s` is the sum over factors of the loading
#' `Z[s, k]` times the effect on factor `k`. This is constrained
#' ordination, the model [jsdgam()] fits. Every term of such a
#' `trend_formula` is then evaluated once per factor and time, and
#' each covariate must take one value per time.
#'
#' With no `n_lv`, each series takes its own effect on the latent
#' state.
#'
#' The series column itself is not supported in a `trend_formula`
#' term. Write `lv_axis()` for an effect that differs along the
#' trend's axis, and put an effect of the series on the observations
#' in `formula`. `by = trend` is deprecated and is treated as
#' `by = lv_axis()`.
#'
#' @return `NULL` (invisibly). The function exists solely as a
#'   formula sentinel.
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
#' factor. The coefficient of one factor has no scale of its own: the
#' loadings and the factor effects trade scale, and their product is
#' what the data identify. [conditional_effects()] and [predict()]
#' report that product, the effect of the covariate on each series,
#' and [conditional_effects()] draws a panel per series.
#'
#' @seealso [mvgam()], [jsdgam()], [conditional_effects.mvgam()]
#'
#' @examples
#' # `lv_axis()` marks a term in a formula. Called by itself it
#' # returns NULL.
#' lv_axis()
#'
#' \dontrun{
#' set.seed(1)
#' dat <- sim_mvgam(
#'   family       = gaussian(),
#'   n_series     = 3L,
#'   n_timepoints = 60L
#' )$data_train
#' # `x` differs among the series at each time. `season` takes one
#' # value per time, and its effect here differs among the series.
#' dat$season <- sin(2 * pi * dat$time / 12)
#' dat$y <- dat$y + c(1, 0.5, -1)[as.integer(dat$series)] * dat$season
#'
#' # ---- No latent factors ----
#' # A slope of `x` for each series
#' mod_slopes <- mvgam(
#'   formula       = y ~ 1,
#'   trend_formula = ~ x:lv_axis() + AR(p = 1),
#'   data          = dat,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod_slopes)
#' conditional_effects(mod_slopes)
#'
#' # A smooth of `x` for each series
#' mod_smooths <- mvgam(
#'   formula       = y ~ 1,
#'   trend_formula = ~ s(x, k = 5, by = lv_axis()) + AR(p = 1),
#'   data          = dat,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' conditional_effects(mod_smooths)
#'
#' # ---- Two latent factors ----
#' # Each factor takes its own seasonal slope, and the loadings carry
#' # the two slopes to the three series. The covariate takes one value
#' # per time, as every term of this trend formula must.
#' mod_factors <- mvgam(
#'   formula       = y ~ 1,
#'   trend_formula = ~ season:lv_axis() + AR(p = 1, n_lv = 2),
#'   data          = dat,
#'   family        = gaussian(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' # The seasonal effect on each series
#' conditional_effects(mod_factors)
#'
#' # The series column is refused in a trend term
#' try(mvgam(
#'   formula       = y ~ 1,
#'   trend_formula = ~ x:series + AR(p = 1),
#'   data          = dat,
#'   family        = gaussian(),
#'   run_model     = FALSE
#' ))
#' }
#'
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
detect_and_rewrite_by_lv <- function(formula, target = ".trend",
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

# Internal: refuse a trend formula whose terms name the series
# column. The trend data holds the resolved axis as `series` whatever
# the user's column is called, and both names are refused.
#'@noRd
refuse_series_terms <- function(rhs, series_var) {
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

# Internal: the recursion of `detect_and_rewrite_by_lv()`. `state`
# counts the replacements.
#'@noRd
walk_by_lv <- function(expr, state, depth = 0L) {
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

# Internal: the columns `detect_and_rewrite_by_lv()` writes in place
# of `lv_axis()`. `.trend` is the factor axis and `series` the axis a
# trend derives from a grouping or from the responses. A fit whose
# data names its series passes that column as well.
#'@noRd
by_lv_rewrite_tokens <- function(series_var = NULL) {
  unique(c(".trend", "series", series_var))
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
#' `conditional_effects.mvgam()` names each panel by its grouping and
#' passes the full grouping to `marginaleffects::plot_predictions()`,
#' which keeps the facet for each series. A grouping holding only such
#' a column is returned as it is, and its panel keeps a name.
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


#' Emit the deprecation warning for `by = trend` (legacy jsdgam
#' syntax). Raised once per session through `warn_once()`, quiet
#' under testthat.
#'
#' Called once per validator pass when
#' `detect_and_rewrite_by_lv()` reports `deprecated_trend_seen`.
#'
#' @noRd
warn_legacy_trend_by <- function() {
  warn_once(
    paste0(
      "'by = trend' in 'trend_formula' is deprecated. Use ",
      "'by = lv_axis()' instead. mvgam will continue to accept ",
      "'by = trend' but the legacy spelling may be removed in a ",
      "future release."
    ),
    "mvgam_by_trend_deprecated"
  )
}
