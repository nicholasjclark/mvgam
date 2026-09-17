#' Polish Generated Stan Code
#'
#' Applies Stan code formatting using StanHeaders stanc.js with auto-format, with
#' minimal fallback fixes for edge cases. Uses official Stan formatter to
#' eliminate += operator and word-breaking issues.
#'
#' @param stan_code Character vector of Stan code lines or single string with newlines
#' @param silent Logical; should Stan formatting warnings be suppressed? Default TRUE
#'
#' @return The polished Stan program as one string
#'
#' @details
#' The preprocessing steps run once. Their result is formatted with the
#' stanc.js bundled in StanHeaders, and returned unformatted when V8 or
#' stanc.js is unavailable or stanc rejects the program. Formatting is
#' cosmetic: a program stanc rejects is still returned, and the parse
#' `build_stan_components()` runs on the polished program reports the
#' fault.
#'
#' @noRd
polish_generated_stan_code <- function(stan_code, silent = TRUE) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_logical(silent, len = 1)

  lines <- strsplit(paste(stan_code, collapse = "\n"), "\n",
                    fixed = TRUE)[[1]]
  lines <- update_stan_header(lines)
  lines <- clean_stan_comments(lines)
  lines <- fix_blank_lines(lines)
  lines <- reorganize_lprior_statements(lines)
  lines <- reorganize_target_statements(lines)
  lines <- reorganize_model_block_statements(lines)
  lines <- add_targeted_comments(lines)
  preprocessed_code <- paste(lines, collapse = "\n")

  try_stanheaders_formatting(preprocessed_code, silent) %||%
    preprocessed_code
}

#' Try StanHeaders Stan Code Formatting
#'
#' Attempts to format Stan code using StanHeaders stanc.js with V8.
#' Uses the official Stan formatter so output matches stanc.
#'
#' @param stan_code Character string containing Stan code
#' @param silent Logical; should warnings be suppressed?
#' @param line_length Integer; maximum line length for formatting (default 80)
#'
#' @return Character string with formatted Stan code or NULL if invalid code
#'
#' @noRd
try_stanheaders_formatting <- function(stan_code, silent = TRUE, line_length = 80) {
  # Validate input first
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

  # Call stanc with auto-format using the working web demo approach
  result <- ctx$call('stanc', 'model', stan_code, c('auto-format', as.character(line_length)))

  # Extract formatted code if successful
  if (is.list(result) && 'result' %in% names(result)) {
    formatted_code <- result$result

    # Verify this is formatted Stan code (not C++)
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

#' Get the cached V8 context with stanc.js sourced
#'
#' First call initialises the V8 isolate and sources StanHeaders'
#' stanc.js into it. Subsequent calls return the same isolate.
#' Returns NULL (with optional message) if V8 or stanc.js are
#' unavailable, so the polish step degrades gracefully.
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

        # Add blank line if previous line was code (not empty, not comment)
        if (!last_is_empty && !last_is_comment) {
          result <- c(result, "", line)
        } else {
          result <- c(result, line)
        }
      } else {
        result <- c(result, line)
      }

      # Skip ahead past any blank lines followed by more comments
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
          # Not empty, not comment - stop processing comment block
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
#' Replaces the generic brms header with one that includes both mvgam and brms versions.
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

#' Add Targeted Comments to Stan Code
#'
#' Adds specific, useful comments to explain key data structures and
#' computational steps that are not obvious from variable names alone.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with targeted comments added
#'
#' @noRd
add_targeted_comments <- function(lines) {
  if (length(lines) == 0) return(lines)

  # Comment 1: Z matrix - factor loadings matrix
  z_pattern <- "matrix\\[N_series_trend,\\s*N_lv_trend\\]\\s*Z\\s*="
  z_lines <- grep(z_pattern, lines)
  if (length(z_lines) > 0) {
    lines <- insert_comment_before_line(lines, z_lines[1],
                                        "  // Factor loadings matrix: maps latent variables to observed series")
  }

  # Comment 2: lv_trend matrix - latent variable trajectories
  lv_pattern <- "matrix\\[N_time_trend,\\s*N_lv_trend\\]\\s*lv_trend\\s*;"
  lv_lines <- grep(lv_pattern, lines)
  if (length(lv_lines) > 0) {
    lines <- insert_comment_before_line(lines, lv_lines[1],
                                        "  // Latent variable trajectories over time")
  }

  # Comment 3: trend matrix - final trend values
  trend_pattern <- "matrix\\[N_time_trend,\\s*N_series_trend\\]\\s*trend\\s*;"
  trend_lines <- grep(trend_pattern, lines)
  if (length(trend_lines) > 0) {
    lines <- insert_comment_before_line(lines, trend_lines[1],
                                        "  // Final trend values for each time point and series")
  }

  # Comment 4: Trend mapping computation
  trend_loop_pattern <- "for\\s*\\(\\s*i\\s*in\\s*1\\s*:\\s*N_time_trend\\)\\s*\\{"
  trend_loop_lines <- grep(trend_loop_pattern, lines)
  if (length(trend_loop_lines) > 0) {
    # Check if this is the trend mapping loop (contains dot_product and trend assignment)
    for (line_idx in trend_loop_lines) {
      # Look ahead a few lines to see if this contains trend mapping
      check_range <- line_idx:(min(line_idx + 10, length(lines)))
      if (any(grepl("trend\\[.*\\].*=.*dot_product", lines[check_range]))) {
        lines <- insert_comment_before_line(lines, line_idx,
                                            "  // Map latent variables to trend values via factor loadings")
        break
      }
    }
  }

  # Comment 5: Observation linear predictors and likelihoods
  likelihood_pattern <- "if\\s*\\(\\s*!prior_only\\s*\\)\\s*\\{"
  likelihood_lines <- grep(likelihood_pattern, lines)
  if (length(likelihood_lines) > 0) {
    lines <- insert_comment_before_line(
      lines, likelihood_lines[1],
      "  // Observation linear predictors and likelihoods (skipped when sampling from prior only)"
    )
  }

  # Comment 6: First target += statement for likelihood calculations
  # Find the if (!prior_only) block
  prior_only_pattern <- "if\\s*\\(\\s*!prior_only\\s*\\)\\s*\\{"
  prior_only_lines <- grep(prior_only_pattern, lines)

  if (length(prior_only_lines) > 0) {
    prior_only_start <- prior_only_lines[1]
    prior_only_end <- find_matching_closing_brace(lines, prior_only_start)

    if (!is.na(prior_only_end)) {
      # Find first target += within this block
      target_pattern <- "target\\s*\\+="
      for (i in (prior_only_start + 1):(prior_only_end - 1)) {
        if (grepl(target_pattern, lines[i])) {
          # Check if there's already a "Likelihood calculations" comment above it
          comment_above <- if (i > 1) trimws(lines[i - 1]) else ""
          if (!grepl("Likelihood calculations", comment_above)) {
            lines <- insert_comment_before_line(lines, i,
                                                "    // Likelihood calculations")
          }
          break  # Only comment the first target += statement
        }
      }
    }
  }

  # Comment 7: Prior log-probability accumulator
  lprior_init_pattern <- "real\\s+lprior\\s*=\\s*0\\s*;"
  lprior_init_lines <- grep(lprior_init_pattern, lines)
  if (length(lprior_init_lines) > 0) {
    lines <- insert_comment_before_line(lines, lprior_init_lines[1],
                                        "  // Prior log-probability accumulator")
  }

  # Comment 8: Prior contributions (target += lprior should be first in final section)
  # Find target += lprior in model block
  model_pattern <- stan_block_header("model", own_line = TRUE)
  model_lines <- grep(model_pattern, lines)

  if (length(model_lines) > 0) {
    model_start <- model_lines[1]
    model_end <- find_matching_closing_brace(lines, model_start)

    if (!is.na(model_end)) {
      # Find target += lprior line in model block
      for (i in (model_start + 1):(model_end - 1)) {
        if (grepl("^\\s*target\\s*\\+=\\s*lprior", lines[i])) {
          # Check if there's already a "Prior contributions" comment above it
          comment_above <- if (i > 1) trimws(lines[i - 1]) else ""
          if (!grepl("Prior contributions", comment_above)) {
            lines <- insert_comment_before_line(lines, i,
                                                "  // Prior contributions")
          }
          break  # Only comment the first target += lprior
        }
      }
    }
  }

  return(lines)
}

#' Insert Comment Before Line
#' @param lines Character vector of Stan code lines
#' @param line_num Line number to insert before
#' @param comment Comment text to insert
#' @return Character vector with comment inserted
#' @noRd
insert_comment_before_line <- function(lines, line_num, comment) {
  if (line_num < 1 || line_num > length(lines)) return(lines)

  before <- if (line_num == 1) character(0) else lines[1:(line_num - 1)]
  after <- lines[line_num:length(lines)]

  c(before, comment, after)
}

#' Gather the prior accumulations in transformed parameters
#'
#' Moves every top-level `lprior +=` statement to the line after
#' `real lprior = 0;`. brms writes these statements at the end of the
#' block. Each takes a parameter, data or a quantity declared with its
#' value in the block's declarations, and none uses a quantity the
#' block assigns later.
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

  st <- stan_statements(lines, tparams)
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

  st <- stan_statements(lines, guard)
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

#' Clean Stan Comments
#'
#' Removes ALL comments and empty lines from all blocks except functions block.
#' Preserves all comments and empty lines within functions block for documentation.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with cleaned comments and empty lines
#'
#' @noRd
clean_stan_comments <- function(lines) {
  if (length(lines) == 0) return(lines)

  result <- character(0)
  in_functions_block <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]
    line_trimmed <- trimws(line)

    # Track if we're in functions block
    if (grepl(stan_block_header("functions"), line_trimmed)) {
      in_functions_block <- TRUE
    } else if (grepl(stan_any_block_header(), line_trimmed)) {
      in_functions_block <- FALSE
    }

    # Check if this is the header (first line with "Generated with")
    is_header <- (i == 1 && grepl("^//\\s*Generated with", line_trimmed))

    # Handle comments
    if (in_functions_block || is_header) {
      # Functions block or header: preserve everything
      result <- c(result, line)

    } else {
      # All other blocks: remove ALL comments and empty lines
      if (grepl("//", line)) {
        code_part <- stan_drop_line_comment(line)
        code_trimmed <- trimws(code_part, which = "right")
        # Only keep line if there's actual code
        if (nzchar(code_trimmed)) {
          result <- c(result, code_trimmed)
        }
        # Skip comment-only lines entirely
      } else {
        # No comment - only keep if not empty
        line_trimmed <- trimws(line)
        if (nzchar(line_trimmed)) {
          result <- c(result, line)
        }
        # Skip completely empty lines
      }
    }

    # Handle closing braces (they reset block tracking)
    if (grepl("^\\}\\s*$", line_trimmed)) {
      if (in_functions_block) {
        # Functions block ends when we see another block start
        in_functions_block <- FALSE
      }
    }
  }

  return(result)
}

#' Reorganize Model Block into Three Sections
#'
#' Reorganizes model block statements into clean sections: priors first,
#' if (!prior_only) block unchanged, then the top-level target +=
#' statements with `target += lprior;` first. Statements move whole.
#'
#' @param lines Character vector of Stan code lines
#' @return Character vector with reorganized model block
#' @noRd
reorganize_model_block_statements <- function(lines) {
  checkmate::assert_character(lines)

  model <- stan_block_bounds(lines, "model")
  if (is.null(model)) return(lines)
  guard <- prior_only_bounds(lines, model)

  st <- stan_statements(lines, model)
  in_guard <- if (is.null(guard)) {
    rep(FALSE, nrow(st))
  } else {
    st$start >= guard$start & st$start <= guard$end
  }
  target <- st$top & !in_guard & grepl("^target\\s*\\+=", st$head)
  lprior <- target & grepl("^target\\s*\\+=\\s*lprior\\s*;", st$head)
  other <- !in_guard & !target & nzchar(st$head)

  new_model <- c(
    lines[statement_lines(st[other, ])],
    if (!is.null(guard)) c("", lines[guard$start:guard$end]),
    if (any(target)) {
      c("", lines[statement_lines(st[lprior, ])],
        lines[statement_lines(st[target & !lprior, ])])
    }
  )

  c(lines[seq_len(model$start)], new_model,
    lines[model$end:length(lines)])
}

