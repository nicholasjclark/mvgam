#' Every combination of the values given, the first varying slowest
#'
#' `expand.grid()` varies its first argument fastest. The arguments
#' are crossed in reverse, then the columns restored to their order.
#'
#' @param ... Named vectors
#' @return A tibble with one column per argument and one row per
#'   combination
#' @noRd
cross_grid <- function(...) {
  cols <- list(...)
  checkmate::assert_list(cols, names = "unique", min.len = 1L)
  grid <- expand.grid(rev(cols), KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
  tibble::as_tibble(grid[names(cols)])
}


#' Arrays bound along their third dimension
#'
#' R stores an array with its last dimension varying slowest. Binding
#' along that dimension then concatenates the values. A matrix enters
#' as one slice, named by its argument name. An array's slices keep
#' their names.
#'
#' @param ... Matrices or three-dimensional arrays sharing their first
#'   two dimensions
#' @return A three-dimensional array, its slices in argument order.
#'   The third dimension is named when any slice is.
#' @noRd
bind_slices <- function(...) {
  parts <- list(...)
  checkmate::assert_list(parts, min.len = 1L)
  lead <- dim(parts[[1L]])[1:2]
  arg_names <- names(parts) %||% character(length(parts))
  slice_names <- unlist(Map(function(p, nm) {
    checkmate::assert_true(length(dim(p)) %in% 2:3)
    checkmate::assert_true(identical(dim(p)[1:2], lead))
    if (length(dim(p)) == 2L) return(nm)
    dimnames(p)[[3L]] %||% character(dim(p)[3L])
  }, parts, arg_names), use.names = FALSE)
  out <- array(unlist(parts, use.names = FALSE),
               c(lead, length(slice_names)))
  if (any(nzchar(slice_names))) {
    dimnames(out) <- list(NULL, NULL, slice_names)
  }
  out
}
