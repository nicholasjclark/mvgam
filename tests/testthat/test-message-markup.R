# `insight::format_error()` and its siblings do not interpolate cli
# markup: a literal "{.field resp}" handed to them reaches the user
# with its braces intact, and "{.val {family_name}}" prints the
# variable's name rather than its value. Only `cli::format_inline()`
# renders it, so a marked-up string has to pass through that first.
#
# The check walks the namespace rather than the R/ sources, so it
# runs against the installed package the same way it runs here.

# The macros cli renders. A brace pair carrying one of these is
# markup rather than ordinary text.
cli_markup <- "\\{\\.(val|field|fn|code|arg|cls|file|url|pkg|emph|strong)[ }]"

format_calls <- c("format_error", "format_warning", "format_alert")
renders <- c("format_inline", "cli_abort", "cli_warn", "cli_inform", "glue")

call_name <- function(e) {
  if (!is.call(e)) return("")
  head <- e[[1L]]
  if (is.name(head)) return(as.character(head))
  # `insight::format_error` and friends.
  if (is.call(head) && length(head) == 3L) return(as.character(head[[3L]]))
  ""
}

# Whether an expression is a string this message will print as
# written: a literal, or a `c()` / `paste0()` built only from
# literals. Anything else -- a variable, a `format_inline()` call --
# is either rendered elsewhere or not knowable here, and is left
# alone.
literal_strings <- function(e) {
  if (is.character(e)) return(e)
  if (!is.call(e)) return(character(0))
  nm <- call_name(e)
  if (nm %in% renders) return(character(0))
  if (!nm %in% c("c", "paste0", "paste")) return(character(0))
  unlist(lapply(as.list(e)[-1L], literal_strings))
}

# Literal strings handed straight to a `format_*()` call, which is
# where markup goes unrendered.
unrendered_markup <- function(fn) {
  bad <- character(0)
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    if (call_name(e) %in% format_calls) {
      lits <- unlist(lapply(as.list(e)[-1L], literal_strings))
      bad <<- c(bad, lits[grepl(cli_markup, lits)])
    }
    for (p in as.list(e)[-1L]) {
      if (missing(p)) next
      walk(p)
    }
    invisible(NULL)
  }
  walk(body(fn))
  unique(bad)
}


test_that("no user-facing message ships unrendered cli markup", {
  ns <- asNamespace("mvgam")
  fns <- Filter(function(nm) is.function(get(nm, envir = ns)),
                ls(ns, all.names = TRUE))
  # An empty set would satisfy the loop without checking anything.
  expect_gt(length(fns), 200L)

  offenders <- character(0)
  for (nm in fns) {
    f <- get(nm, envir = ns)
    if (is.null(body(f))) next
    hits <- unrendered_markup(f)
    if (length(hits)) {
      offenders <- c(offenders, paste0(nm, ": ", hits[1L]))
    }
  }
  expect_identical(offenders, character(0))
})


test_that("the markup check finds markup a format call would print", {
  # The check is only worth having if it fails on the shape it
  # exists to catch, so both spellings of the real defect are put to
  # it here alongside the wrapped form that is correct.
  bare <- function() {
    stop(insight::format_error("Needs {.field resp}."))
  }
  in_c <- function() {
    stop(insight::format_error(c(
      "Header.", i = "Pass {.val {nm}}."
    )))
  }
  wrapped <- function() {
    stop(insight::format_error(
      cli::format_inline("Needs {.field resp}.")
    ))
  }
  plain <- function() {
    stop(insight::format_error("Needs a response name."))
  }
  expect_length(unrendered_markup(bare), 1L)
  expect_length(unrendered_markup(in_c), 1L)
  expect_length(unrendered_markup(wrapped), 0L)
  expect_length(unrendered_markup(plain), 0L)
})
