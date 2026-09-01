#' Seed the RNG for the caller, and leave the session as it was
#'
#' @description
#' `set.seed()` writes to the global random-number state, so a
#' function that seeds for its own reproducibility decides what its
#' caller draws next. Putting the previous state back covers most of
#' it, but not the case where there was no previous state: a session
#' that has never drawn has no `.Random.seed`, and seeding creates
#' one, so the variable has to be removed rather than reassigned.
#' Restoring only when something was there leaves the caller with a
#' stream it never started.
#'
#' Call this at the top of a function that seeds. The cleanup is
#' registered in the calling frame, so it runs when that function
#' returns, whether it returns normally or by error.
#'
#' @param seed Integer seed, or `NULL` to leave the RNG alone.
#' @param frame Frame to register the cleanup in. Defaults to the
#'   caller, which is what you want.
#' @return Invisibly `NULL`, called for its effect on the RNG.
#' @noRd
local_seed <- function(seed, frame = parent.frame()) {
  if (is.null(seed)) {
    return(invisible(NULL))
  }
  checkmate::assert_int(seed)
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    # `bquote` puts the state itself into the expression, so the
    # handler does not depend on this frame surviving.
    restore <- bquote(
      assign(".Random.seed", .(get(".Random.seed", envir = .GlobalEnv)),
             envir = .GlobalEnv)
    )
  } else {
    restore <- quote(
      suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
    )
  }
  do.call(on.exit, list(restore, add = TRUE), envir = frame)
  set.seed(seed)
  invisible(NULL)
}
