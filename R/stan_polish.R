#' Polish generated Stan code
#'
#' Regroups the statements of a generated program and formats it with
#' the Stan formatter of the stanc.js that StanHeaders bundles.
#'
#' @param stan_code Character vector of Stan code lines, or one string
#'   holding newlines
#' @param silent Logical. `FALSE` reports when the formatter is
#'   unavailable. Defaults to `TRUE`.
#'
#' @return The polished Stan program as one string
#'
#' @details
#' Every comment the generators and brms write is kept. A comment
#' written directly above a statement moves with it when the statement
#' is regrouped.
#'
#' The preprocessing steps run once. Their result is formatted with the
#' stanc.js bundled in StanHeaders. It is returned unformatted when V8
#' or stanc.js is unavailable or stanc rejects the program. Formatting
#' is cosmetic. A program stanc rejects is still returned. The parse
#' `build_stan_components()` runs on the polished program then reports
#' the fault.
#'
#' @noRd
polish_generated_stan_code <- function(stan_code, silent = TRUE) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_logical(silent, len = 1)

  lines <- strsplit(paste(stan_code, collapse = "\n"), "\n",
                    fixed = TRUE)[[1]]
  lines <- update_stan_header(lines)
  lines <- fix_blank_lines(lines)
  lines <- reorganize_lprior_statements(lines)
  lines <- reorganize_target_statements(lines)
  lines <- reorganize_model_block_statements(lines)
  preprocessed_code <- paste(lines, collapse = "\n")

  stanc_format(preprocessed_code, silent) %||% preprocessed_code
}

#' A Stan program formatted by stanc.js
#'
#' Runs the stanc.js of StanHeaders in V8 with its `auto-format`
#' option. The output matches that of `stanc --auto-format`.
#'
#' @param stan_code Character string of Stan code
#' @param silent Logical. `FALSE` reports when V8 or stanc.js is missing.
#' @param line_length Integer maximum line length. Defaults to 80.
#'
#' @return The formatted program, or `NULL` when the code is empty, the
#'   formatter is unavailable or stanc rejects the program
#'
#' @noRd
stanc_format <- function(stan_code, silent = TRUE, line_length = 80) {
  if (!nzchar(trimws(stan_code))) {
    return(NULL)
  }

  # Acquire the cached V8 context with stanc.js already sourced. A
  # fresh V8 isolate carries ~1-2 GB of native heap that R's GC
  # cannot see; spawning one per polish call accumulates across the
  # test sweep until V8 itself OOMs. stanc.js is a pure function so
  # one shared isolate suffices.
  ctx <- get_stanc_v8_context(silent = silent)
  if (is.null(ctx)) {
    return(NULL)
  }

  result <- ctx$call("stanc", "model", stan_code,
                     c("auto-format", as.character(line_length)))

  # stanc returns `result` on success and `errors` on a rejection
  if (is.list(result) && "result" %in% names(result)) {
    formatted_code <- result$result

    # Without `auto-format` stanc writes C++. Formatted Stan is kept.
    if (!grepl('#include.*stan.*model|namespace.*model', formatted_code) &&
        grepl('(data|parameters|model).*\\{', formatted_code)) {
      return(formatted_code)
    }
  }

  return(NULL)
}

# Package-private cache for the V8 isolate holding stanc.js. stanc
# itself is a pure function (input string in, formatted string out)
# so one isolate can serve every polish call for the session.
.stanc_v8_env <- new.env(parent = emptyenv())

#' The V8 context holding stanc.js, built once per session
#'
#' The first call starts a V8 isolate and sources the stanc.js of
#' StanHeaders into it. Later calls return the same isolate. Without V8
#' or stanc.js it returns `NULL`. The program then stays unformatted.
#'
#' @param silent Logical; suppress availability messages.
#'
#' @return A V8 context object, or NULL when the inputs are missing.
#'
#' @noRd
get_stanc_v8_context <- function(silent = TRUE) {
  if (!is.null(.stanc_v8_env$ctx)) {
    return(.stanc_v8_env$ctx)
  }
  if (!requireNamespace("V8", quietly = TRUE)) {
    if (!silent) {
      rlang::inform("Stan formatting requires the V8 package.")
    }
    return(NULL)
  }
  stanc_js_path <- system.file("stanc.js", package = "StanHeaders")
  if (!file.exists(stanc_js_path)) {
    if (!silent) {
      rlang::inform(
        "Stan formatting requires stanc.js from StanHeaders."
      )
    }
    return(NULL)
  }
  ctx <- V8::v8()
  ctx$source(stanc_js_path)
  .stanc_v8_env$ctx <- ctx
  ctx
}



#' Fix Blank Lines with Comment Block Logic
#'
#' Removes excessive blank lines while ensuring proper comment block spacing.
#' Comment blocks (consecutive comment lines) get a blank line above them,
#' but no blank lines within the block.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with cleaned blank line usage
#'
#' @noRd
fix_blank_lines <- function(lines) {
  if (length(lines) == 0) return(lines)

  result <- character(0)
  i <- 1

  while (i <= length(lines)) {
    line <- lines[i]
    line_trimmed <- trimws(line)
    is_empty <- (line_trimmed == "")
    is_comment <- grepl("^//", line_trimmed)

    if (is_comment) {
      # Check if we need a blank line above this comment
      if (length(result) > 0) {
        last_line_trimmed <- trimws(result[length(result)])
        last_is_empty <- (last_line_trimmed == "")
        last_is_comment <- grepl("^//", last_line_trimmed)

        # A blank line separates the comment from code above it
        if (!last_is_empty && !last_is_comment) {
          result <- c(result, "", line)
        } else {
          result <- c(result, line)
        }
      } else {
        result <- c(result, line)
      }

      # Blank lines between comments of one block are dropped
      j <- i + 1
      while (j <= length(lines)) {
        next_line_trimmed <- trimws(lines[j])
        next_is_empty <- (next_line_trimmed == "")
        next_is_comment <- grepl("^//", next_line_trimmed)

        if (next_is_empty) {
          # Skip this empty line if followed by comment
          k <- j + 1
          while (k <= length(lines) && trimws(lines[k]) == "") {
            k <- k + 1  # Skip multiple consecutive empty lines
          }
          if (k <= length(lines) && grepl("^//", trimws(lines[k]))) {
            # Empty line(s) followed by comment - skip the empty lines
            j <- k
          } else {
            # Empty line not followed by comment - keep it
            break
          }
        } else if (next_is_comment) {
          # Another comment - add it directly (no blank line)
          result <- c(result, lines[j])
          j <- j + 1
        } else {
          # Code ends the comment block
          break
        }
      }
      i <- j

    } else if (is_empty) {
      # Add empty line if not consecutive with previous empty line
      if (length(result) == 0 || trimws(result[length(result)]) != "") {
        result <- c(result, line)
      }
      i <- i + 1

    } else {
      # Code line - add as-is
      result <- c(result, line)
      i <- i + 1
    }
  }

  # Remove trailing empty lines
  while (length(result) > 0 && trimws(result[length(result)]) == "") {
    result <- result[-length(result)]
  }

  return(result)
}

#' Update Stan Header with Package Versions
#'
#' Replaces the generic brms header with one that includes both mvgam
#' and brms versions.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with updated header
#'
#' @noRd
update_stan_header <- function(lines) {
  if (length(lines) == 0) return(lines)

  # Check if first line is the brms generated header
  if (grepl("^//\\s*generated with brms", lines[1])) {
    # Create new header with both package versions
    mvgam_version <- utils::packageVersion("mvgam")
    brms_version <- utils::packageVersion("brms")
    lines[1] <- paste0("// Generated with mvgam ", mvgam_version,
                       " using brms ", brms_version)
  }

  return(lines)
}

#' Gather the prior accumulations in transformed parameters
#'
#' Moves every top-level `lprior +=` statement to the line after
#' `real lprior = 0;`. brms writes these statements at the end of the
#' block. Each takes a parameter, data or a quantity declared with its
#' value in the block's declarations. None uses a quantity the block
#' assigns later.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with the statements moved
#'
#' @noRd
reorganize_lprior_statements <- function(lines) {
  checkmate::assert_character(lines)

  tparams <- stan_block_bounds(lines, "transformed parameters")
  if (is.null(tparams)) return(lines)

  st <- stan_commented_statements(lines, tparams)
  init <- which(st$top & grepl("^real\\s+lprior\\s*=\\s*0\\s*;", st$head))
  moving <- st$top & grepl("^lprior\\s*\\+=", st$head)
  if (length(init) == 0L || !any(moving)) return(lines)

  relocate_lines(lines, statement_lines(st[moving, ]), st$end[init[1L]] + 1L)
}

#' Gather the likelihood statements in the model block
#'
#' Moves every top-level `target +=` statement in the `if (!prior_only)`
#' block to the end of that block. The move is cosmetic.
#'
#' Only a whole top-level statement moves. A nested statement can use a
#' variable its loop declares, such as `ps` in a mixture likelihood.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with the statements moved
#'
#' @noRd
reorganize_target_statements <- function(lines) {
  checkmate::assert_character(lines)

  model <- stan_block_bounds(lines, "model")
  if (is.null(model)) return(lines)
  guard <- prior_only_bounds(lines, model)
  if (is.null(guard)) return(lines)

  st <- stan_commented_statements(lines, guard)
  moving <- st$top & grepl("^target\\s*\\+=", st$head)
  if (!any(moving)) return(lines)

  relocate_lines(lines, statement_lines(st[moving, ]), guard$end)
}

#' The lines of the likelihood guard in a model block
#'
#' @param lines Character vector of Stan code lines
#' @param model The model block's bounds, from `stan_block_bounds()`
#'
#' @return A list of `start` and `end` line numbers for the top-level
#'   `if (!prior_only) {` block, or NULL when there is none
#'
#' @noRd
prior_only_bounds <- function(lines, model) {
  st <- stan_statements(lines, model)
  hit <- which(
    st$top & grepl("^if\\s*\\(\\s*!prior_only\\s*\\)\\s*\\{$", st$head)
  )
  if (length(hit) == 0L) return(NULL)
  start <- st$start[hit[1L]]
  end <- find_matching_closing_brace(lines, start)
  if (is.na(end)) return(NULL)
  list(start = start, end = end)
}

#' Reorder lines of Stan code
#'
#' @param lines Character vector of Stan code lines
#' @param moved Line numbers to move, in the order they are placed
#' @param before The line number the moved lines precede once moved,
#'   counted in `lines`
#'
#' @return Character vector with the lines moved
#'
#' @noRd
relocate_lines <- function(lines, moved, before) {
  checkmate::assert_character(lines)
  checkmate::assert_integerish(moved, lower = 1, upper = length(lines),
                               any.missing = FALSE)
  checkmate::assert_int(before, lower = 1L, upper = length(lines) + 1L)
  kept <- setdiff(seq_along(lines), moved)
  c(lines[kept[kept < before]], lines[moved], lines[kept[kept >= before]])
}

#' Reorganize Model Block into Three Sections
#'
#' Reorganizes model block statements into clean sections: priors first,
#' if (!prior_only) block unchanged, then the top-level target +=
#' statements with `target += lprior;` first. Statements move whole,
#' each with the comment written directly above it. A comment with no
#' statement below it is dropped.
#'
#' @param lines Character vector of Stan code lines
#' @return Character vector with reorganized model block
#' @noRd
reorganize_model_block_statements <- function(lines) {
  checkmate::assert_character(lines)

  model <- stan_block_bounds(lines, "model")
  if (is.null(model)) return(lines)

  st <- stan_commented_statements(lines, model)
  guard <- which(
    st$top & grepl("^if\\s*\\(\\s*!prior_only\\s*\\)\\s*\\{$", st$head)
  )[1L]
  guard_lines <- integer(0)
  in_guard <- rep(FALSE, nrow(st))
  if (!is.na(guard)) {
    guard_end <- find_matching_closing_brace(lines, st$end[guard])
    if (!is.na(guard_end)) {
      guard_lines <- seq.int(st$start[guard], guard_end)
      in_guard <- st$start >= st$start[guard] & st$start <= guard_end
    }
  }
  target <- st$top & !in_guard & grepl("^target\\s*\\+=", st$head)
  lprior <- target & grepl("^target\\s*\\+=\\s*lprior\\s*;", st$head)
  other <- !in_guard & !target & nzchar(st$head)

  new_model <- c(
    lines[statement_lines(st[other, ])],
    if (length(guard_lines)) c("", lines[guard_lines]),
    if (any(target)) {
      c("", lines[statement_lines(st[lprior, ])],
        lines[statement_lines(st[target & !lprior, ])])
    }
  )

  c(lines[seq_len(model$start)], new_model,
    lines[model$end:length(lines)])
}

