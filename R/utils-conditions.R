#' Raise a warning once per session
#'
#' `id` names the warning for the rlang once-per-session counter and
#' gives the condition its class, letting a caller catch it by name. The
#' warning stays quiet under `testthat`. The suite allows no warnings,
#' and a warning spent in one test would never reach the test asserting
#' it. A test asserting one clears the variable with
#' `withr::local_envvar(TESTTHAT = "")`.
#'
#' @param message Character vector holding the main line, then any named
#'   `i` or `x` bullets for rlang to format
#' @param id Identifier for the counter and the condition class
#' @return `NULL`, invisibly
#' @noRd
warn_once <- function(message, id) {
  checkmate::assert_character(message, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(id, min.chars = 1L)
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(invisible(NULL))
  }
  rlang::warn(message, class = id, .frequency = "once",
              .frequency_id = id)
  invisible(NULL)
}


#' Record `silent` for the length of the calling function
#'
#' `mvgam()`, `jsdgam()` and the Stan code generator store `silent` in
#' the `mvgam.silent` option. A validator deep in the call then takes
#' it from `mvgam_quiet()`, without the argument passed down to it. The
#' option is restored when the caller returns.
#'
#' @param silent Verbosity, as `silent` of [mvgam()]
#' @param frame Frame whose exit restores the option
#' @return `NULL`, invisibly
#' @noRd
local_mvgam_silent <- function(silent, frame = parent.frame()) {
  checkmate::assert_number(silent)
  old <- options(mvgam.silent = silent)
  do.call(
    base::on.exit,
    list(substitute(options(old), list(old = old)), add = TRUE),
    envir = frame
  )
  invisible(NULL)
}


#' Whether the running call asked for no messages
#'
#' @return `TRUE` when the recorded `silent` is 2 or more
#' @noRd
mvgam_quiet <- function() {
  isTRUE(getOption("mvgam.silent", 1L) >= 2L)
}


#' Show an informational message once per session
#'
#' A message informs and a warning reports a problem. `mvgam()` and
#' `jsdgam()` record `silent` in the `mvgam.silent` option for the
#' length of the call. A value of 2 or more turns messages off and
#' leaves warnings on. The message stays quiet under `testthat` for the
#' reason `warn_once()` gives.
#'
#' @param message Character vector holding the main line, then any named
#'   `i` or `x` bullets for rlang to format
#' @param id Identifier for the counter and the condition class
#' @return `NULL`, invisibly
#' @noRd
inform_once <- function(message, id) {
  checkmate::assert_character(message, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(id, min.chars = 1L)
  if (identical(Sys.getenv("TESTTHAT"), "true") || mvgam_quiet()) {
    return(invisible(NULL))
  }
  rlang::inform(message, class = id, .frequency = "once",
                .frequency_id = id)
  invisible(NULL)
}


#' Evaluate a PSIS call whose caller acts on the Pareto k values
#'
#' `loo` warns when a Pareto k is high or the tail holds too few draws
#' to fit. `lfo_cv()` and `kfold()` refit wherever k crosses their
#' threshold, which makes both warnings the decision itself. Every other
#' warning reaches the user.
#'
#' @param expr A call to `loo::psis()` or `loo::loo()`
#' @return The value of `expr`
#' @noRd
with_pareto_k_handled <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("Pareto k diagnostic|Not enough tail samples",
              conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}


#' A "Did you mean" bullet for a name that matched nothing
#'
#' Follows the rule of `rlang::arg_match()`. A unique partial match is
#' the candidate. A closer candidate replaces it where its edit distance
#' is at most half its length. Case is ignored only when neither
#' finds one.
#'
#' @param name The name that matched nothing
#' @param candidates The names it could have meant
#' @return A named `c(i = ...)` bullet, or `NULL` with no candidate
#' @noRd
did_you_mean <- function(name, candidates) {
  checkmate::assert_string(name)
  checkmate::assert_character(candidates)
  if (!length(candidates)) {
    return(NULL)
  }
  candidate <- NULL
  partial <- pmatch(name, candidates)
  if (!is.na(partial)) {
    candidate <- candidates[[partial]]
  }
  close <- utils::adist(name, candidates)[1L, ] / nchar(candidates)
  if (any(close <= 0.5)) {
    candidate <- candidates[[which.min(close)]]
  }
  if (is.null(candidate)) {
    close <- utils::adist(name, candidates, ignore.case = TRUE)[1L, ] /
      nchar(candidates)
    if (any(close <= 0.5)) {
      candidate <- candidates[[which.min(close)]]
    }
  }
  if (is.null(candidate)) {
    return(NULL)
  }
  c(i = paste0("Did you mean '", candidate, "'?"))
}
