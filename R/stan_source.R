# The Stan source brms generated.
#
# mvgam splices its trend machinery into a program brms wrote. That
# means locating blocks and statements in the text. The rules for
# doing so are collected here, because they were written more than
# once and the copies were free to disagree.


# The blocks of a Stan program, in the order one declares them.
stan_block_names <- c(
  "functions", "data", "transformed data", "parameters",
  "transformed parameters", "model", "generated quantities"
)


#' Header pattern for a named Stan block
#'
#' @param block One of `stan_block_names`.
#' @param own_line Require the brace to end the line. brms writes each
#'   header that way. The stricter form suits a caller that rewrites
#'   the surrounding source and needs certainty about which line it
#'   matched.
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
#' Delegates to `find_matching_closing_brace()`. A nested `{` in a
#' loop or a local scope leaves the block open.
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


#' One Stan line with its line comment removed
#'
#' The string literals are kept, which is what separates this from
#' `stan_line_code()`. A `//` inside a literal is part of the string
#' and starts no comment. Code that will be emitted is stripped here;
#' `stan_line_code()` suits analysis, where a literal's contents
#' matter to nothing.
#'
#' @param line One Stan source line.
#' @return `line` up to its first line comment, that comment removed.
#' @noRd
stan_drop_line_comment <- function(line) {
  checkmate::assert_string(line)
  chars <- strsplit(line, "", fixed = TRUE)[[1L]]
  in_string <- FALSE
  for (i in seq_along(chars)) {
    if (identical(chars[i], '"')) {
      in_string <- !in_string
    } else if (!in_string && identical(chars[i], "/") &&
               i < length(chars) && identical(chars[i + 1L], "/")) {
      if (i == 1L) return("")
      return(paste(chars[seq_len(i - 1L)], collapse = ""))
    }
  }
  line
}


#' Lines of the prior_only guard inside a block body
#'
#' `prior_only_bounds()` locates the guard in a whole program, taking
#' the model block's bounds. A block body carries no header of its
#' own. One is supplied here, and the line numbers are shifted back
#' onto the body's own numbering.
#'
#' @param lines Character vector of a block body's lines.
#' @return Integer line numbers of the guard's header and of the brace
#'   matching it, or `integer(0)` when the body holds no guard.
#' @noRd
prior_only_guard_indices <- function(lines) {
  checkmate::assert_character(lines)
  if (length(lines) == 0L) return(integer(0))
  wrapped <- c("model {", lines, "}")
  guard <- prior_only_bounds(
    wrapped, list(start = 1L, end = length(wrapped))
  )
  if (is.null(guard)) return(integer(0))
  c(guard$start, guard$end) - 1L
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


#' Headers left governing nothing
#'
#' A `for` or `if` whose every statement goes takes its own header with
#' it, and the brace closing it where the source kept one. Headers are
#' examined last one first. A loop emptied by an inner loop leaving is
#' then emptied itself.
#'
#' @param lines The lines a block spans, its braces included.
#' @param statements Rows of `stan_statements()` over `lines`.
#' @param removed One logical per statement, TRUE where it goes.
#' @return `removed`, with emptied headers and their braces added.
#' @noRd
emptied_blocks <- function(lines, statements, removed) {
  opens <- grepl("^(for|if|while|else)\\b.*\\{\\s*$", statements$head)
  for (i in rev(which(opens))) {
    closes_at <- find_matching_closing_brace(lines, statements$end[i])
    last <- if (is.na(closes_at)) length(lines) + 1L else closes_at
    inner <- which(statements$start > statements$end[i] &
                     statements$start < last)
    # A blank line and a comment have an empty head. Neither computes
    # anything. Neither keeps a block alive, and both leave with the
    # statements they described.
    code <- inner[nzchar(statements$head[inner])]
    if (length(code) == 0L || !all(removed[code])) next
    removed[inner] <- TRUE
    removed[i] <- TRUE
    if (!is.na(closes_at)) removed[statements$start == closes_at] <- TRUE
  }
  removed
}


#' A block body without the statements another block already writes
#'
#' brms declares a group-level effect or a smooth coefficient in the
#' transformed parameters block and assigns it there. `mu_trend`'s
#' construction repeats that pair. Both then precede the predictor
#' using them, and the block written second drops what the first wrote.
#' Comparison is by statement, since brms splits a density and its
#' normalising constant over two lines.
#'
#' @param code One block body, without its braces.
#' @param written Stan code whose statements appear elsewhere.
#' @return `code` without any statement that `written` also contains.
#' @noRd
drop_repeated_statements <- function(code, written) {
  checkmate::assert_string(code)
  checkmate::assert_string(written)

  as_block <- function(text) {
    c("model {", strsplit(text, "\n", fixed = TRUE)[[1L]], "}")
  }
  # One statement per element. Indentation and the line a statement was
  # split across then cannot make two spellings of one code differ.
  one_line_each <- function(lines, statements) {
    spans <- Map(seq.int, statements$start, statements$end)
    vapply(spans, function(span) {
      gsub("\\s+", " ", paste(trimws(lines[span]), collapse = " "))
    }, character(1L))
  }

  body <- as_block(code)
  statements <- stan_statements(body, list(start = 1L, end = length(body)))
  elsewhere <- as_block(written)
  taken <- one_line_each(elsewhere, stan_statements(
    elsewhere, list(start = 1L, end = length(elsewhere))
  ))

  text <- one_line_each(body, statements)
  repeated <- nzchar(text) & text %in% taken[nzchar(taken)]
  repeated <- emptied_blocks(body, statements, repeated)
  if (!any(repeated)) return(code)

  dropped <- statement_lines(statements[repeated, , drop = FALSE])
  wrappers <- c(1L, length(body))
  paste(body[setdiff(seq_along(body), c(wrappers, dropped))],
        collapse = "\n")
}
