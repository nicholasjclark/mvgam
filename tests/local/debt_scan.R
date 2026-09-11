# Count the shapes tech debt takes in R/, as FINDINGS.md entry 89
# describes them.
#
# Each shape leaves a mark in the source that a scan can find, and a
# count can be driven down and checked. The scan reads parse data, not
# text: a comment or a string that happens to contain a pattern is not
# counted.
#
# Run from the package root:
#   Rscript tests/local/debt_scan.R           # a count per shape
#   Rscript tests/local/debt_scan.R try       # every site of one shape
#
# Shapes: try, default, narrow, raw_axis, rebuild, mv_class, suppress,
# double, dead_param. `dead_param` loads the package and lists
# parameters no body reads outside an assertion. It also counts
# dispatch kernels that share a signature and S3 generics, and each
# hit is read before anything is removed.

tokens_of <- function(path) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE))
  pd <- pd[pd$terminal & pd$token != "COMMENT", ]
  pd[order(pd$line1, pd$col1), c("line1", "token", "text")]
}

# The sites of one shape in one file's tokens, as line numbers.
shape_sites <- list(
  # An error turned into a default value.
  try = function(tk) {
    tk$line1[tk$token == "SYMBOL_FUNCTION_CALL" &
               tk$text %in% c("try", "tryCatch")]
  },
  # A literal standing in for a missing value: `x %||% "y"`.
  default = function(tk) {
    i <- which(tk$token == "SPECIAL" & tk$text == "%||%")
    i <- i[i < nrow(tk) & tk$token[i + 1L] == "STR_CONST"]
    tk$line1[i]
  },
  # A column the frame lacks, skipped rather than refused.
  narrow = function(tk) {
    i <- which(tk$token == "SYMBOL_FUNCTION_CALL" & tk$text == "intersect")
    hits <- vapply(i, function(k) {
      window <- tk$text[k:min(nrow(tk), k + 12L)]
      "names" %in% window || "colnames" %in% window
    }, logical(1L))
    tk$line1[i[hits]]
  },
  # The series or time read off a frame's own column.
  raw_axis = function(tk) {
    i <- which(tk$token == "LBB")
    hits <- vapply(i, function(k) {
      window <- tk$text[(k + 1L):min(nrow(tk), k + 3L)]
      any(window %in% c("series_var", "time_var"))
    }, logical(1L))
    tk$line1[i[hits]]
  },
  # An axis rebuilt by sorting the values a frame holds.
  rebuild = function(tk) {
    i <- which(tk$token == "SYMBOL_FUNCTION_CALL" & tk$text == "sort")
    i <- i[i + 2L <= nrow(tk) & tk$text[i + 2L] == "unique"]
    tk$line1[i]
  },
  # The formula's class asked in place of the question meant.
  mv_class = function(tk) {
    a <- which(tk$token == "SYMBOL_FUNCTION_CALL" &
                 tk$text == "is.mvbrmsformula")
    b <- which(tk$token == "STR_CONST" &
                 tk$text %in% c("\"mvbrmsformula\"", "'mvbrmsformula'"))
    tk$line1[c(a, b)]
  },
  # A warning silenced, not traced.
  suppress = function(tk) {
    tk$line1[tk$token == "SYMBOL_FUNCTION_CALL" &
               tk$text %in% c("suppressWarnings", "suppressMessages")]
  },
  # One condition raised twice: `insight::format_warning()` raises its
  # own warning, and a `warning()` or `rlang::warn()` around it raises
  # a second one.
  double = function(tk) {
    i <- which(tk$token == "SYMBOL_FUNCTION_CALL" &
                 tk$text %in% c("warning", "warn"))
    i <- i[i + 4L <= nrow(tk)]
    hit <- tk$text[i + 2L] == "insight" &
      tk$text[i + 4L] %in% c("format_warning", "format_alert")
    tk$line1[i[hit]]
  }
)

scan_files <- function(files) {
  rows <- lapply(files, function(path) {
    tk <- tokens_of(path)
    do.call(rbind, lapply(names(shape_sites), function(shape) {
      lines <- shape_sites[[shape]](tk)
      if (!length(lines)) return(NULL)
      data.frame(shape = shape, file = path, line = lines)
    }))
  })
  do.call(rbind, rows)
}

# Parameters no body reads outside an assertion.
dead_params <- function() {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
  ns <- asNamespace("mvgam")
  read <- function(e) {
    if (is.call(e)) {
      head <- paste(deparse(e[[1L]]), collapse = "")
      if (grepl("^checkmate::assert|^assert_", head)) return(character(0))
      return(unique(c(all.names(e[[1L]]), unlist(lapply(
        as.list(e)[-1L], function(a) if (missing(a)) character(0) else read(a)
      )))))
    }
    if (is.name(e)) return(as.character(e))
    character(0)
  }
  out <- list()
  for (nm in ls(ns, all.names = TRUE)) {
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) next
    formal <- setdiff(names(formals(f)), "...")
    unused <- setdiff(formal, read(body(f)))
    if (length(unused)) out[[nm]] <- unused
  }
  data.frame(
    shape = "dead_param",
    fn = names(out),
    params = vapply(out, paste, character(1L), collapse = ", "),
    row.names = NULL
  )
}

# The text of every message, warning and error a user can meet, one
# section per call site, written as markdown for the prose linter.
# The string constants inside each outermost condition call are joined
# in order, which is how `paste0()` and `c()` assemble them.
condition_heads <- c(
  "stop", "warning", "message", "warn", "abort", "inform",
  "format_error", "format_warning", "format_message", "format_alert"
)

message_sites <- function(path) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE))
  heads <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
                pd$text %in% condition_heads, ]
  if (!nrow(heads)) return(NULL)
  # A call's expression is the parent of the expression that holds
  # its name.
  call_ids <- pd$parent[match(heads$parent, pd$id)]
  calls <- pd[match(unique(call_ids), pd$id), ]
  starts_after <- function(l1, c1, l2, c2) l1 > l2 | l1 == l2 & c1 >= c2
  inside <- function(i, j) {
    starts_after(calls$line1[i], calls$col1[i],
                 calls$line1[j], calls$col1[j]) &&
      starts_after(calls$line2[j], calls$col2[j],
                   calls$line2[i], calls$col2[i])
  }
  outer <- vapply(seq_len(nrow(calls)), function(i) {
    !any(vapply(seq_len(nrow(calls))[-i], function(j) inside(i, j),
                logical(1L)))
  }, logical(1L))
  calls <- calls[outer, ]
  strs <- pd[pd$token == "STR_CONST", ]
  out <- lapply(seq_len(nrow(calls)), function(i) {
    within <- starts_after(strs$line1, strs$col1,
                           calls$line1[i], calls$col1[i]) &
      starts_after(calls$line2[i], calls$col2[i],
                   strs$line2, strs$col2)
    text <- vapply(strs$text[within], function(t) {
      eval(parse(text = t))
    }, character(1L), USE.NAMES = FALSE)
    text <- text[nzchar(trimws(text))]
    if (!length(text)) return(NULL)
    data.frame(file = path, line = calls$line1[i],
               text = paste(text, collapse = " "))
  })
  do.call(rbind, out)
}

args <- commandArgs(trailingOnly = TRUE)
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

if (length(args) && identical(args[[1L]], "dead_param")) {
  print(dead_params(), right = FALSE)
} else if (length(args) && identical(args[[1L]], "messages")) {
  sites <- do.call(rbind, lapply(files, message_sites))
  writeLines(paste0("## ", sites$file, ":", sites$line, "\n\n",
                    sites$text, "\n"))
} else {
  sites <- scan_files(files)
  if (length(args)) {
    hit <- sites[sites$shape == args[[1L]], c("file", "line")]
    print(hit[order(hit$file, hit$line), ], row.names = FALSE)
  } else {
    counts <- table(factor(sites$shape, levels = names(shape_sites)))
    print(data.frame(shape = names(counts), sites = as.integer(counts)),
          row.names = FALSE)
  }
}
