#' Seed the RNG for the caller, then restore the session's state
#'
#' @description
#' `set.seed()` writes to the global random-number state. A function
#' that seeds for its own reproducibility would otherwise decide what
#' its caller draws next. Putting the previous state back covers most
#' cases. A session that has never drawn has no `.Random.seed`.
#' Seeding creates one, which the cleanup must remove. Restoring only
#' when a state existed would leave the caller with a stream it never
#' started.
#'
#' Call this at the top of a function that seeds. Because the cleanup
#' is registered in the calling frame, it runs when that function
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
    # `bquote` puts the state itself into the expression. The handler
    # then works after this frame is gone.
    restore <- bquote(
      assign(".Random.seed", .(get(".Random.seed", envir = .GlobalEnv)),
             envir = .GlobalEnv)
    )
  } else {
    restore <- quote(
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    )
  }
  do.call(on.exit, list(restore, add = TRUE), envir = frame)
  set.seed(seed)
  invisible(NULL)
}
