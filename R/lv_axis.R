# Per-factor smooth sentinel for trend_formula.
#
# `lv_axis()` is a no-op function whose only purpose is to mark
# `by = lv_axis()` smooth terms in a trend_formula as per-latent-factor
# smooths. mvgam's AST detector recognises it by name, switches the
# trend-side data grain from (time, series) to (time, factor), and
# rewrites the formula so the literal `lv_axis()` call becomes
# `by = .trend` before it reaches brms. brms then emits per-level
# smooth coefficients using its native `by = factor` machinery on the
# internal `.trend` factor column. The leading dot in `.trend` keeps
# the injected column collision-safe vs any user-supplied column.

#' Per-latent-factor smooth sentinel
#'
#' Use inside the `by =` argument of a smooth or GP term in a
#' `trend_formula` to indicate that the smooth should be fit
#' independently for each latent factor of a factor model
#' (`n_lv < n_series`). For example:
#'
#' \preformatted{
#' factor_formula = ~ s(elev, by = lv_axis()) +
#'                   gp(lon, lat, by = lv_axis()) - 1
#' }
#'
#' This is the canonical syntax for constrained ordination in mvgam.
#' Each factor `k = 1, ..., n_lv` receives its own smooth basis; the
#' species-specific responses arise from the factor loadings `Z[s, k]`
#' multiplied by the per-factor smooth contributions.
#'
#' `lv_axis()` is a sentinel: it has no side effects and returns
#' `NULL` invisibly. Calling it outside the `by =` position of a
#' smooth/GP term is silently no-op, but the trend-formula validator
#' will surface a clear error in that case. The legacy syntax
#' `by = trend` is supported via deprecation warning and auto-translates
#' to `by = lv_axis()`.
#'
#' @details The sentinel is recognised purely by name in the
#'   formula's abstract syntax tree, so passing it through `eval()`
#'   or `quote()` outside a mvgam call yields `NULL`. The companion
#'   validator (in `R/validations.R`) rejects `by = series` in
#'   `trend_formula`. That pattern is semantically confusing on the
#'   trend side, and the helper directs users to either
#'   `by = lv_axis()` (for per-factor smooths) or to moving the
#'   per-series effect into `obs_formula`.
#'
#' @return `NULL` (invisibly). The function exists solely as a
#'   formula sentinel.
#' @export
#' @examples
#' \donttest{
#' # Inside a factor-model jsdgam fit (illustration, not a fit):
#' formula_obj <- ~ s(elev, by = lv_axis()) - 1
#' formula_obj
#' }
lv_axis <- function() {
  invisible(NULL)
}


# Internal AST detection + rewrite helpers.
#
# These mirror the existing `is_trend_term()` / `remove_trend_expressions()`
# pattern in R/validations.R so the formula walk reuses the same rlang
# idioms (`rlang::is_call`, `rlang::call_args`, `rlang::call_name`,
# `rlang::call2`). The walk is structural: it tracks whether the
# current node is in a smooth or GP call (`s`, `te`, `ti`, `t2`, `gp`)
# and, if so, inspects only the `by` argument of that call.

# Smooth/GP constructor names that accept a `by` argument and whose
# `by` position is where `lv_axis()` may appear.
.smooth_with_by_calls <- c("s", "te", "ti", "t2", "gp")

#' Check if an expression is a literal call to `lv_axis()`.
#'
#' Mirrors `is_trend_term()`'s call-by-name strategy so that
#' argument-literal variants (`lv_axis()` vs `mvgam::lv_axis()`)
#' match identically.
#'
#' @param expr Unevaluated R expression.
#' @return `TRUE` if `expr` is a `lv_axis()` call (bare or
#'   namespace-qualified), else `FALSE`.
#' @noRd
is_lv_axis_call <- function(expr) {
  if (!rlang::is_call(expr)) return(FALSE)
  fn_name <- rlang::call_name(expr)
  identical(fn_name, "lv_axis")
}

#' Check if a `by =` argument node is the literal symbol `trend`.
#'
#' The legacy jsdgam API used `by = trend` as the per-factor
#' sentinel. mvgam supports it via a deprecation warning that
#' auto-translates the term to `by = lv_axis()`.
#'
#' @param expr Unevaluated R expression (the `by` arg value).
#' @return `TRUE` if `expr` is the bare symbol `trend`.
#' @noRd
is_legacy_trend_symbol <- function(expr) {
  is.symbol(expr) && identical(as.character(expr), "trend")
}

#' Check if a `by =` argument node is the literal symbol `series`.
#'
#' `by = series` inside `trend_formula` is semantically confusing
#' (the trend side is for shared/factor effects, not per-series obs
#' effects). The validator hard rejects it.
#'
#' @param expr Unevaluated R expression (the `by` arg value).
#' @return `TRUE` if `expr` is the bare symbol `series`.
#' @noRd
is_series_symbol <- function(expr) {
  is.symbol(expr) && identical(as.character(expr), "series")
}

#' Walk a trend_formula AST detecting `by = lv_axis()` (and legacy
#' equivalents), and rewrite each such `by` arg to the internal
#' `.trend` symbol so the downstream brms compile sees a normal
#' factor by-variable on the injected `.trend` column.
#'
#' Returns a list with `has_by_lv` (logical), `n_by_lv` (count of
#' rewritten by-positions), `deprecated_trend_seen` (logical, used
#' to fire a one-time deprecation warning at the call site), and
#' `formula` (the rewritten formula).
#'
#' Throws an `insight::format_error` on `by = series` (hard reject).
#'
#' Reuses the rlang idioms already used by `remove_trend_expressions()`
#' (`R/validations.R:3952`): structural recursion via `rlang::is_call`,
#' `rlang::call_args`, `rlang::call_name`, `rlang::call2`.
#'
#' @param formula A trend-side formula (one-sided or two-sided).
#' @return Named list with elements `has_by_lv`, `n_by_lv`,
#'   `deprecated_trend_seen`, `formula`.
#' @noRd
detect_and_rewrite_by_lv <- function(formula) {
  checkmate::assert_class(formula, "formula")

  state <- new.env(parent = emptyenv())
  state$n_by_lv <- 0L
  state$deprecated_trend_seen <- FALSE

  rhs <- rlang::f_rhs(formula)
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

#' Recursive companion to `detect_and_rewrite_by_lv()`.
#'
#' Walks the expression tree. At each smooth/GP call (`s`, `te`,
#' `ti`, `t2`, `gp`), inspects the `by` argument. If `by` is a call
#' to `lv_axis()` (or the legacy symbol `trend`), increments
#' `state$n_by_lv` and rewrites the argument to the bare symbol
#' `.trend`. If `by` is the symbol `series`, raises a hard error.
#' Otherwise leaves the call untouched but recurses into children
#' so that arbitrarily nested operators (`+`, `-`, etc.) are
#' traversed.
#'
#' @param expr Current expression node.
#' @param state Environment holding mutable counters.
#' @param depth Recursion depth guard (matches the limit used by
#'   `remove_trend_expressions()`).
#' @return Possibly-rewritten expression.
#' @noRd
walk_by_lv <- function(expr, state, depth = 0L) {
  if (depth > 50L) {
    stop(insight::format_error(c(
      "Formula nesting too deep (>50 levels).",
      i = "Simplify the trend formula structure."
    )))
  }

  if (!rlang::is_call(expr)) {
    return(expr)
  }

  fn_name <- rlang::call_name(expr)

  if (!is.null(fn_name) && fn_name %in% .smooth_with_by_calls) {
    args <- rlang::call_args(expr)
    by_arg <- args[["by"]]
    if (!is.null(by_arg)) {
      if (is_lv_axis_call(by_arg)) {
        state$n_by_lv <- state$n_by_lv + 1L
        args[["by"]] <- as.symbol(".trend")
        return(rlang::call2(fn_name, !!!args))
      }
      if (is_legacy_trend_symbol(by_arg)) {
        state$n_by_lv <- state$n_by_lv + 1L
        state$deprecated_trend_seen <- TRUE
        args[["by"]] <- as.symbol(".trend")
        return(rlang::call2(fn_name, !!!args))
      }
      if (is_series_symbol(by_arg)) {
        stop(insight::format_error(c(
          "'by = series' is not allowed inside 'trend_formula'.",
          x = paste0(
            "The trend side models shared dynamics or per-latent-",
            "factor effects; per-series obs effects belong in ",
            "'formula', not 'trend_formula'."
          ),
          i = paste0(
            "Use 'by = lv_axis()' if you want a per-factor smooth ",
            "(requires a factor model with 'n_lv < n_series'), or ",
            "move the smooth to 'formula' for a per-series effect."
          )
        )))
      }
    }
    return(expr)
  }

  # For non-smooth calls, walk arguments recursively so nested
  # `+`/`-` operators (and any other call) are visited. The call
  # itself is reconstructed only if at least one child changes.
  args <- as.list(expr)
  head <- args[[1L]]
  tail <- lapply(args[-1L], walk_by_lv, state = state, depth = depth + 1L)
  rlang::call2(head, !!!tail)
}

#' Surface the deprecation warning for `by = trend` (legacy jsdgam
#' syntax). Gated by `Sys.getenv("TESTTHAT")` so the testthat run is
#' quiet by default; the warning fires once per session for users.
#'
#' Called once per validator pass when
#' `detect_and_rewrite_by_lv()` reports `deprecated_trend_seen`.
#'
#' @noRd
warn_legacy_trend_by <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) return(invisible(NULL))
  rlang::warn(
    paste0(
      "'by = trend' in 'trend_formula' is deprecated. Use ",
      "'by = lv_axis()' instead. mvgam will continue to accept ",
      "'by = trend' but the legacy spelling may be removed in a ",
      "future release."
    ),
    class = "mvgam_by_trend_deprecated",
    .frequency = "once",
    .frequency_id = "mvgam_by_trend_deprecated"
  )
}
