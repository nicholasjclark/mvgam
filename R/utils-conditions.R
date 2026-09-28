# Internal: raise a warning once per session.
#
# `id` names the warning for rlang's once-per-session counter and
# gives the condition its class, which lets a caller catch it by name.
# Under `testthat` the warning stays quiet: the suite allows no
# warnings, and a once-per-session warning raised in one test would be
# spent before the test asserting it runs. A test asserting one clears
# the variable with `withr::local_envvar(TESTTHAT = "")`.
#
# @param message Character vector: the main line, then any named
#   `i`/`x` bullets rlang formats.
# @param id Identifier for the counter and the condition class.
# @return `NULL`, invisibly.
#'@noRd
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


# Internal: show an informational message once per session.
#
# A message informs and a warning reports a problem. `silent >= 2`, which
# `mvgam()` and `jsdgam()` record in the `mvgam.silent` option for the
# length of the call, turns messages off and leaves warnings alone.
# Quiet under `testthat` for the reason `warn_once()` gives.
#
# @inheritParams warn_once
# @return `NULL`, invisibly.
#'@noRd
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
