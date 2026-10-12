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
  if (identical(Sys.getenv("TESTTHAT"), "true") ||
      isTRUE(getOption("mvgam.silent", 1L) >= 2L)) {
    return(invisible(NULL))
  }
  rlang::inform(message, class = id, .frequency = "once",
                .frequency_id = id)
  invisible(NULL)
}
