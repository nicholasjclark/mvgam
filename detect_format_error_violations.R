# Detect insight::format_error / format_warning sites that violate
# the project standard: multi-arg calls must wrap their message in
# c(main, x = "...", i = "..."). Compliant: <= 1 argument, or first
# argument is a call to c().
#
# Run from the package root:
#   Rscript detect_format_error_violations.R
# Emits a TSV of file/line/fn/n_args/compliant to stdout, plus a
# per-file summary to stderr.

target_fns <- c("format_error", "format_warning")

`%||%` <- function(a, b) if (is.null(a)) b else a

is_target_call <- function(x) {
  if (!is.call(x)) return(FALSE)
  head <- x[[1L]]
  if (is.symbol(head)) {
    return(as.character(head) %in% target_fns)
  }
  if (is.call(head) && length(head) == 3L &&
      identical(head[[1L]], as.symbol("::")) &&
      identical(as.character(head[[2L]]), "insight") &&
      as.character(head[[3L]]) %in% target_fns) {
    return(TRUE)
  }
  FALSE
}

fn_label <- function(x) {
  head <- x[[1L]]
  if (is.symbol(head)) {
    return(as.character(head))
  }
  paste0(as.character(head[[2L]]), "::", as.character(head[[3L]]))
}

is_c_call <- function(x) {
  is.call(x) && length(x) >= 1L &&
    is.symbol(x[[1L]]) && as.character(x[[1L]]) == "c"
}

# A string contains a cli or glue token if it has {...} patterns. The
# c()-form refactor handles structure; cli::format_inline handles inline
# rendering and glue interpolation. Strings with {...} that are not
# wrapped in cli::format_inline are violations even when n_args == 1.
string_needs_format_inline <- function(x) {
  if (!is.character(x)) return(FALSE)
  any(grepl("\\{", x, fixed = FALSE))
}

is_format_inline_call <- function(x) {
  if (!is.call(x)) return(FALSE)
  head <- x[[1L]]
  if (is.symbol(head) && as.character(head) == "format_inline") return(TRUE)
  if (is.call(head) && length(head) == 3L &&
      identical(head[[1L]], as.symbol("::")) &&
      identical(as.character(head[[2L]]), "cli") &&
      identical(as.character(head[[3L]]), "format_inline")) {
    return(TRUE)
  }
  FALSE
}

# Check whether a single arg (string, c(...) element, paste(...) call,
# etc.) already routes its {...} markup through cli::format_inline. Returns
# TRUE if no violation; FALSE if the arg has {...} patterns but no wrapper.
arg_handles_braces <- function(x) {
  if (is_format_inline_call(x)) return(TRUE)
  if (is.character(x)) {
    return(!string_needs_format_inline(x))
  }
  if (is.call(x)) {
    # Recurse: a paste()/paste0() call is OK if no inner string holds
    # un-wrapped {...}; a c(...) is OK if every element is OK.
    for (i in seq_along(x)[-1L]) {
      if (!arg_handles_braces(x[[i]])) return(FALSE)
    }
    return(TRUE)
  }
  TRUE
}

srcref_line <- function(sr) {
  if (is.null(sr)) return(NA_integer_)
  if (is.list(sr)) sr <- sr[[1L]]
  if (length(sr) == 0L) return(NA_integer_)
  as.integer(sr[1L])
}

is_stop_call <- function(x) {
  is.call(x) && is.symbol(x[[1L]]) && as.character(x[[1L]]) == "stop"
}

walk <- function(x, outer_line, inside_stop, sites) {
  if (is_target_call(x)) {
    line <- srcref_line(attr(x, "srcref"))
    if (is.na(line)) line <- outer_line
    n_args <- length(x) - 1L
    first_arg <- if (n_args >= 1L) x[[2L]] else NULL
    structural_ok <- n_args <= 1L || is_c_call(first_arg)
    # Inline-rendering check: every payload-bearing arg (either the lone
    # arg, or the elements of the leading c(...)) must not leave {...}
    # markup unwrapped.
    inline_ok <- TRUE
    if (n_args == 1L) {
      inline_ok <- arg_handles_braces(first_arg)
    } else if (is_c_call(first_arg)) {
      for (i in seq_along(first_arg)[-1L]) {
        if (!arg_handles_braces(first_arg[[i]])) {
          inline_ok <- FALSE
          break
        }
      }
    }
    # Missing-stop check: only applies to format_error. format_warning is
    # typically passed to rlang::warn() or used bare, so it's exempt.
    fn_str <- fn_label(x)
    is_format_error <- endsWith(fn_str, "format_error")
    stop_ok <- !is_format_error || inside_stop
    compliant <- structural_ok && inline_ok && stop_ok
    reason <- if (!structural_ok) {
      "positional"
    } else if (!inline_ok) {
      "unwrapped-braces"
    } else if (!stop_ok) {
      "missing-stop"
    } else {
      "ok"
    }
    sites[[length(sites) + 1L]] <- data.frame(
      line = line,
      fn = fn_str,
      n_args = n_args,
      compliant = compliant,
      reason = reason,
      stringsAsFactors = FALSE
    )
  }
  if (is.call(x)) {
    own_sr_line <- srcref_line(attr(x, "srcref"))
    own_line <- if (is.na(own_sr_line)) outer_line else own_sr_line
    # If this call IS stop(), every descendant counts as inside_stop.
    descend_inside_stop <- inside_stop || is_stop_call(x)
    for (i in seq_along(x)) {
      sites <- walk(x[[i]], own_line, descend_inside_stop, sites)
    }
  }
  sites
}

scan_file <- function(path) {
  parsed <- parse(file = path, keep.source = TRUE)
  srcrefs <- attr(parsed, "srcref")
  rows <- list()
  for (i in seq_along(parsed)) {
    top_line <- if (is.null(srcrefs)) {
      NA_integer_
    } else {
      as.integer(srcrefs[[i]][1L])
    }
    sites <- walk(parsed[[i]], top_line, FALSE, list())
    for (s in sites) {
      s$file <- path
      rows[[length(rows) + 1L]] <- s
    }
  }
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

main <- function() {
  files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  results <- lapply(files, scan_file)
  out <- do.call(rbind, Filter(Negate(is.null), results))
  out <- out[, c("file", "line", "fn", "n_args", "compliant", "reason")]
  out <- out[order(out$compliant, out$file, out$line), ]
  write.table(out, "", sep = "\t", quote = FALSE, row.names = FALSE)
  message("")
  message("# Per-file summary (violations / total):")
  by_file <- split(out, out$file)
  for (f in names(by_file)) {
    sub <- by_file[[f]]
    message(sprintf("  %-44s  %4d / %4d",
                    basename(f), sum(!sub$compliant), nrow(sub)))
  }
  message("")
  message("# Total: ", sum(!out$compliant),
          " violations across ", nrow(out), " sites in ",
          length(unique(out$file)), " files")
  invisible(out)
}

if (sys.nframe() == 0L) main()
