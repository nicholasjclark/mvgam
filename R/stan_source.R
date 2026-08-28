# Reading the Stan source brms generated.
#
# mvgam splices its trend machinery into a program brms wrote, so it
# has to locate blocks and statements in that text. The rules for
# doing so live here rather than beside each caller, because they were
# written more than once and the copies were free to disagree.


# The blocks of a Stan program, in the order one declares them.
stan_block_names <- c(
  "functions", "data", "transformed data", "parameters",
  "transformed parameters", "model", "generated quantities"
)


#' Header pattern for a named Stan block
#'
#' @param block One of `stan_block_names`.
#' @param own_line Require the brace to end the line. brms writes each
#'   header that way, so the stricter form is safe where a caller
#'   rewrites the surrounding source and wants no doubt about which
#'   line it matched.
#' @return A regular expression matching that block's opening line.
#' @noRd
stan_block_header <- function(block, own_line = FALSE) {
  checkmate::assert_choice(block, stan_block_names)
  checkmate::assert_flag(own_line)
  paste0(
    "^\\s*", gsub(" ", "\\\\s+", block), "\\s*\\{",
    if (own_line) "\\s*$" else ""
  )
}


#' Pattern matching the header of any Stan block
#'
#' @return A regular expression alternating over `stan_block_names`.
#' @noRd
stan_any_block_header <- function() {
  paste0(
    "^(", paste(gsub(" ", "\\\\s+", stan_block_names), collapse = "|"),
    ")\\s*\\{"
  )
}


#' First line below `after` at which some other block opens
#'
#' Used where a block's end is taken as the start of the next one.
#' Brace counting is the stricter test, but these callers have always
#' scanned for the next header and the two disagree only on code that
#' opens a block inside another.
#'
#' @param lines Character vector of Stan source lines.
#' @param after Line number to search below.
#' @param exclude Block whose own header should not count.
#' @return An integer line number, or `NA_integer_` when none follows.
#' @noRd
stan_next_block_line <- function(lines, after, exclude = character(0)) {
  checkmate::assert_character(lines)
  checkmate::assert_int(after, lower = 0L)
  hits <- integer(0)
  for (block in setdiff(stan_block_names, exclude)) {
    matches <- grep(stan_block_header(block), lines, ignore.case = TRUE)
    hits <- c(hits, matches[matches > after])
  }
  if (length(hits) == 0L) return(NA_integer_)
  min(hits)
}


#' The lines a named Stan block spans
#'
#' Delegates to `find_matching_closing_brace()`, so a nested `{` in a
#' loop or a local scope does not end the block early.
#'
#' @param lines Character vector of Stan source lines.
#' @param block One of `stan_block_names`.
#' @return A list of `start` and `end` line numbers, or NULL when the
#'   block is absent. `end` is the line holding its closing brace.
#' @noRd
stan_block_bounds <- function(lines, block) {
  checkmate::assert_character(lines)
  start <- grep(stan_block_header(block), lines, ignore.case = TRUE)
  if (length(start) == 0L) return(NULL)
  start <- start[1L]
  if (start >= length(lines)) {
    return(list(start = start, end = length(lines)))
  }
  end <- find_matching_closing_brace(lines, start)
  if (is.na(end)) end <- length(lines)
  list(start = start, end = end)
}


#' The body of a named Stan block, without its braces
#'
#' @param lines Character vector of Stan source lines.
#' @param block One of `stan_block_names`.
#' @return Character vector of the block's inner lines, or NULL when
#'   the block is absent.
#' @noRd
stan_block_body <- function(lines, block) {
  bounds <- stan_block_bounds(lines, block)
  if (is.null(bounds) || bounds$end <= bounds$start + 1L) return(NULL)
  lines[seq.int(bounds$start + 1L, bounds$end - 1L)]
}
