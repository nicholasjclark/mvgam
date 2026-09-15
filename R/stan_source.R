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


#' Line where a brace opened on `start_line` closes
#'
#' @param lines Character vector of Stan source lines.
#' @param start_line Line number holding the opening brace.
#' @return Integer line number of the matching closing brace, or NA
#'   when the braces do not balance.
#' @noRd
find_matching_closing_brace <- function(lines, start_line) {
  checkmate::assert_character(lines, min.len = 1)
  checkmate::assert_int(start_line, lower = 1, upper = length(lines))

  # The opening brace is on `start_line`. A brace in a string or a line
  # comment opens nothing.
  depth <- 1L
  later <- seq.int(start_line + 1L, length.out = length(lines) - start_line)
  for (i in later) {
    depth <- depth + count_stan_braces(lines[i])
    if (depth == 0L) return(i)
    if (depth < 0L) return(NA)
  }
  NA
}


#' The code on Stan source lines
#'
#' @param lines Character vector of Stan source lines.
#' @return `lines` without string literals or line comments. A brace
#'   or a `;` in either is not code.
#' @noRd
stan_line_code <- function(lines) {
  checkmate::assert_character(lines)
  # Strings first: a comment marker can be part of one.
  sub("//.*$", "", gsub('"[^"]*"', "", lines))
}


#' Net brace depth a Stan line opens
#'
#' @param line One Stan source line.
#' @return Integer count of `{` less `}` in the line's code.
#' @noRd
count_stan_braces <- function(line) {
  checkmate::assert_string(line)
  chars <- strsplit(stan_line_code(line), "", fixed = TRUE)[[1L]]
  sum(chars == "{") - sum(chars == "}")
}


#' The statements of a Stan block
#'
#' Splits a block body into statements. A statement ends on a line
#' whose code closes with `;`, on a line that opens or closes a brace
#' and on a `for`, `if`, `else` or `while` header. Any other line
#' continues onto the next one, which is how brms writes a density with
#' the normalising constant of a bounded parameter:
#'
#' ```
#' target += student_t_lpdf(hs_local | hs_df, 0, 1)
#'   - rows(hs_local) * log(0.5);
#' ```
#'
#' A statement is top level when every brace opened before it has
#' closed and no header written without a brace governs it. A nested
#' statement can reference a loop variable or belong to its block, and
#' moving it breaks the program.
#'
#' @param lines Character vector of Stan source lines.
#' @param bounds A list of the block's `start` and `end` lines, as
#'   `stan_block_bounds()` returns.
#' @return A data frame with one row per statement: its `start` and
#'   `end` line numbers in `lines`, whether it is at the `top` level
#'   and its `head`, the code on its first line with whitespace
#'   trimmed. A blank or comment line is a statement with an empty head.
#' @noRd
stan_statements <- function(lines, bounds) {
  checkmate::assert_character(lines)
  checkmate::assert_list(bounds)
  checkmate::assert_int(bounds$start, lower = 1L)
  checkmate::assert_int(bounds$end, lower = bounds$start,
                        upper = length(lines))

  body <- seq.int(bounds$start + 1L,
                  length.out = bounds$end - bounds$start - 1L)
  start <- end <- integer(0)
  top <- logical(0)
  depth <- 0L
  governed <- FALSE
  i <- 1L
  while (i <= length(body)) {
    first <- i
    at_top <- depth == 0L && !governed
    repeat {
      code <- trimws(stan_line_code(lines[body[i]]))
      depth <- depth + count_stan_braces(code)
      delimited <- grepl("[;{}]", code)
      header <- !delimited && grepl("^(for|if|else|while)\\b", code)
      if (!nzchar(code) || delimited || header || i == length(body)) break
      i <- i + 1L
    }
    start <- c(start, body[first])
    end <- c(end, body[i])
    top <- c(top, at_top)
    # A blank line under a header leaves the header governing
    if (nzchar(code) || first < i) governed <- header
    i <- i + 1L
  }
  data.frame(start = start, end = end, top = top,
             head = trimws(stan_line_code(lines[start])))
}


#' Line numbers of statements
#'
#' @param statements Rows of `stan_statements()`.
#' @return Integer vector of every line the statements span, in order.
#' @noRd
statement_lines <- function(statements) {
  as.integer(unlist(Map(seq.int, statements$start, statements$end),
                    use.names = FALSE))
}
