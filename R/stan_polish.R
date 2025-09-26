#' Polish Generated Stan Code
#'
#' Applies Stan code formatting using StanHeaders stanc.js with auto-format, with
#' minimal fallback fixes for edge cases. Uses official Stan formatter to
#' eliminate += operator and word-breaking issues.
#'
#' @param stan_code Character vector of Stan code lines or single string with newlines
#' @param silent Logical; should Stan formatting warnings be suppressed? Default TRUE
#'
#' @return Character vector of polished Stan code lines
#'
#' @details
#' This function uses a two-stage approach:
#' 1. Primary: Fix comment spacing, then official Stan formatting via StanHeaders stanc.js with V8
#' 2. Fallback: Only comment spacing fixes if Stan formatting fails
#'
#' @examples
#' \dontrun{
#' # Polish generated Stan code
#' cleaned_code <- polish_generated_stan_code(stan_code_lines)
#' }
#'
#' @noRd
polish_generated_stan_code <- function(stan_code, silent = TRUE) {
  checkmate::assert_character(stan_code, min.len = 1)
  checkmate::assert_logical(silent, len = 1)

  # Convert to single string for rstan::stanc
  stan_string <- if (length(stan_code) == 1 && grepl("\n", stan_code)) {
    stan_code
  } else if (length(stan_code) == 1) {
    stan_code
  } else {
    paste(stan_code, collapse = "\n")
  }

  # Apply comment cleaning and spacing fixes BEFORE calling stanc
  lines <- strsplit(stan_string, "\n", fixed = TRUE)[[1]]
  lines <- update_stan_header(lines)
  lines <- clean_stan_comments(lines)
  lines <- fix_blank_lines(lines)
  lines <- reorganize_lprior_statements(lines)
  lines <- reorganize_target_statements(lines)
  lines <- reorganize_model_block_statements(lines)
  lines <- add_targeted_comments(lines)
  preprocessed_code <- paste(lines, collapse = "\n")

  # Try StanHeaders formatting on preprocessed code
  formatted_code <- try_stanheaders_formatting(preprocessed_code, silent)

  # Return StanHeaders formatted code if successful
  if (!is.null(formatted_code)) {
    return(formatted_code)
  }

  # Fallback: apply all preprocessing steps
  lines <- strsplit(stan_string, "\n", fixed = TRUE)[[1]]
  lines <- update_stan_header(lines)
  lines <- clean_stan_comments(lines)
  lines <- fix_blank_lines(lines)
  lines <- reorganize_lprior_statements(lines)
  lines <- reorganize_target_statements(lines)
  lines <- reorganize_model_block_statements(lines)
  lines <- add_targeted_comments(lines)

  # Return as single string with embedded newlines
  return(paste(lines, collapse = "\n"))
}

#' Try StanHeaders Stan Code Formatting
#'
#' Attempts to format Stan code using StanHeaders stanc.js with V8.
#' Uses official Stan formatter for robust results.
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

  # Check if required packages are available
  if (!requireNamespace("V8", quietly = TRUE)) {
    if (!silent) {
      message("V8 package not available for Stan formatting")
    }
    return(NULL)
  }

  # Check if StanHeaders stanc.js exists
  stanc_js_path <- system.file('stanc.js', package = 'StanHeaders')
  if (!file.exists(stanc_js_path)) {
    if (!silent) {
      message("StanHeaders stanc.js not found")
    }
    return(NULL)
  }

  # Initialize V8 context with StanHeaders stanc.js
  ctx <- V8::v8()
  ctx$source(stanc_js_path)

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
  lv_pattern <- "matrix\\[N_trend,\\s*N_lv_trend\\]\\s*lv_trend\\s*;"
  lv_lines <- grep(lv_pattern, lines)
  if (length(lv_lines) > 0) {
    lines <- insert_comment_before_line(lines, lv_lines[1],
                                        "  // Latent variable trajectories over time")
  }

  # Comment 3: trend matrix - final trend values
  trend_pattern <- "matrix\\[N_trend,\\s*N_series_trend\\]\\s*trend\\s*;"
  trend_lines <- grep(trend_pattern, lines)
  if (length(trend_lines) > 0) {
    lines <- insert_comment_before_line(lines, trend_lines[1],
                                        "  // Final trend values for each time point and series")
  }

  # Comment 4: Trend mapping computation
  trend_loop_pattern <- "for\\s*\\(\\s*i\\s*in\\s*1\\s*:\\s*N_trend\\)\\s*\\{"
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
  model_pattern <- "^\\s*model\\s*\\{\\s*$"
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

#' Reorganize lprior Statements in Transformed Parameters Block
#'
#' Moves all lprior += statements to just after the lprior = 0 declaration
#' in the transformed parameters block. Handles multi-line statements correctly.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with lprior statements reorganized
#'
#' @noRd
reorganize_lprior_statements <- function(lines) {
  if (length(lines) == 0) return(lines)

  # Find parameters block (transformed parameters always follows)
  params_pattern <- "^\\s*parameters\\s*\\{\\s*$"
  params_line <- grep(params_pattern, lines)
  if (length(params_line) == 0) return(lines)

  # Find transformed parameters block (always after parameters)
  tparams_pattern <- "^\\s*transformed parameters\\s*\\{\\s*$"
  tparams_line <- grep(tparams_pattern, lines)
  if (length(tparams_line) == 0) return(lines)

  # Find model block (always after transformed parameters)
  model_pattern <- "^\\s*model\\s*\\{\\s*$"
  model_line <- grep(model_pattern, lines)
  if (length(model_line) == 0) return(lines)

  tparams_start <- tparams_line[1]
  model_start <- model_line[1]

  # Find the lprior = 0 line within the transformed parameters block
  lprior_init_pattern <- "real\\s+lprior\\s*=\\s*0\\s*;"
  lprior_init_line <- NA
  for (i in (tparams_start + 1):(model_start - 1)) {
    if (grepl(lprior_init_pattern, lines[i])) {
      lprior_init_line <- i
      break
    }
  }

  if (is.na(lprior_init_line)) return(lines)

  # Find all lprior += statements and their continuation lines
  lprior_statements <- character(0)
  lines_to_remove <- integer(0)
  i <- tparams_start + 1

  while (i < model_start) {
    line <- trimws(lines[i])

    if (grepl("^lprior\\s*\\+=", line)) {
      # Always collect lprior statements - we'll move ALL of them to right after lprior = 0
      statement_lines <- c(i)

      # Look for continuation lines (multi-line statements)
      j <- i + 1
      while (j < model_start && !grepl(";\\s*$", lines[j-1])) {
        next_line <- lines[j]
        next_trimmed <- trimws(next_line)

        # Continue if line is indented OR starts with mathematical operators (for lprior continuations)
        is_indented_continuation <- grepl("^\\s+", next_line) &&
            !grepl("^\\s*(real|int|vector|matrix|array|for|if|while|lprior)",
                   next_trimmed) &&
            !grepl("^\\}", next_trimmed)

        # Also accept unindented lines that start with mathematical operators (common in lprior statements)
        is_math_continuation <- grepl("^\\s*[-+*/]", next_trimmed) &&
            !grepl("^\\s*(real|int|vector|matrix|array|for|if|while|lprior)", next_trimmed)

        if (is_indented_continuation || is_math_continuation) {
          statement_lines <- c(statement_lines, j)
          j <- j + 1
        } else {
          break
        }
      }

      # Collect the statement text
      for (idx in statement_lines) {
        lprior_statements <- c(lprior_statements, lines[idx])
        lines_to_remove <- c(lines_to_remove, idx)
      }

      i <- max(statement_lines) + 1
    } else {
      i <- i + 1
    }
  }

  if (length(lines_to_remove) == 0) return(lines)

  # Insert lprior statements immediately after lprior = 0 line BEFORE removing them
  # This avoids line number adjustment issues
  insert_point <- lprior_init_line + 1

  # Insert the collected lprior statements
  before <- lines[1:lprior_init_line]
  after <- lines[insert_point:length(lines)]
  lines_with_inserted <- c(before, lprior_statements, after)

  # Now remove the original lprior statements (adjust indices for inserted lines)
  # Need to adjust line numbers because we inserted lines
  adjusted_remove_indices <- lines_to_remove + length(lprior_statements)
  # But lines that were before the insertion point don't need adjustment
  adjusted_remove_indices[lines_to_remove <= lprior_init_line] <-
    lines_to_remove[lines_to_remove <= lprior_init_line]

  # Remove the original lprior statements
  lines_final <- lines_with_inserted[-adjusted_remove_indices]

  return(lines_final)
}

#' Reorganize target Statements in Model Block
#'
#' Moves all target += statements to just before the closing brace of the
#' if (!prior_only) block in the model block. Handles multi-line statements
#' correctly.
#' This is purely cosmetic to improve Stan code readability.
#'
#' @param lines Character vector of Stan code lines
#'
#' @return Character vector with target statements reorganized
#'
#' @details
#' This function reorganizes target += statements for cosmetic purposes, moving them
#' to the end of the if (!prior_only) block. It follows the same pattern as
#' reorganize_lprior_statements() but operates on the model block instead of
#' transformed parameters block.
#'
#' @noRd
reorganize_target_statements <- function(lines) {
  checkmate::assert_character(lines, min.len = 0)

  if (length(lines) == 0) return(lines)

  # Find model block
  model_pattern <- "^\\s*model\\s*\\{\\s*$"
  model_line <- grep(model_pattern, lines)
  if (length(model_line) == 0) return(lines)

  # Find if (!prior_only) block within model block
  model_start <- model_line[1]

  # Find the if (!prior_only) line
  prior_only_pattern <- "if\\s*\\(\\s*!prior_only\\s*\\)\\s*\\{"
  prior_only_line <- NA
  for (i in (model_start + 1):length(lines)) {
    if (grepl(prior_only_pattern, lines[i])) {
      prior_only_line <- i
      break
    }
  }

  if (is.na(prior_only_line)) return(lines)

  # Find closing brace of if (!prior_only) block using helper function
  prior_only_end <- find_matching_closing_brace(lines, prior_only_line)

  if (is.na(prior_only_end)) return(lines)

  # Find all target += statements within the if (!prior_only) block
  target_statements <- character(0)
  lines_to_remove <- integer(0)
  i <- prior_only_line + 1

  while (i < prior_only_end) {
    line <- trimws(lines[i])

    if (grepl("^target\\s*\\+=", line)) {
      statement_lines <- c(i)

      # Look for continuation lines (multi-line statements)
      j <- i + 1
      while (j < prior_only_end && !grepl(";\\s*$", lines[j-1])) {
        next_line <- lines[j]
        next_trimmed <- trimws(next_line)

        # Continue if line is indented OR starts with mathematical operators
        is_indented_continuation <- grepl("^\\s+", next_line) &&
            !grepl("^\\s*(real|int|vector|matrix|array|for|if|while|target)",
                   next_trimmed) &&
            !grepl("^\\}", next_trimmed)

        is_math_continuation <- grepl("^\\s*[-+*/]", next_trimmed) &&
            !grepl("^\\s*(real|int|vector|matrix|array|for|if|while|target)",
                   next_trimmed)

        if (is_indented_continuation || is_math_continuation) {
          statement_lines <- c(statement_lines, j)
          j <- j + 1
        } else {
          break
        }
      }

      # Collect the statement text
      for (idx in statement_lines) {
        target_statements <- c(target_statements, lines[idx])
        lines_to_remove <- c(lines_to_remove, idx)
      }

      i <- max(statement_lines) + 1
    } else {
      i <- i + 1
    }
  }

  if (length(lines_to_remove) == 0) return(lines)

  # Insert target statements just before closing brace of if (!prior_only)
  insert_point <- prior_only_end

  # Insert the collected target statements
  before <- lines[1:(insert_point - 1)]
  after <- lines[insert_point:length(lines)]
  lines_with_inserted <- c(before, target_statements, after)

  # Remove the original target statements (adjust indices for inserted lines)
  adjusted_remove_indices <- lines_to_remove + length(target_statements)
  # Lines before insertion point don't need adjustment
  adjusted_remove_indices[lines_to_remove < insert_point] <-
    lines_to_remove[lines_to_remove < insert_point]

  # Remove the original target statements
  lines_final <- lines_with_inserted[-adjusted_remove_indices]

  return(lines_final)
}

#' Find Closing Brace Using Brace Counting
#'
#' Finds the line number of the closing brace that matches an opening brace
#' at the specified starting line. Uses brace counting to handle nested blocks.
#'
#' @param lines Character vector of Stan code lines
#' @param start_line Line number where the opening brace is located
#'
#' @return Integer line number of the matching closing brace, or NA if not found
#'
#' @noRd
find_matching_closing_brace <- function(lines, start_line) {
  checkmate::assert_character(lines, min.len = 1)
  checkmate::assert_int(start_line, lower = 1, upper = length(lines))

  brace_count <- 1  # Start with 1 for the opening brace

  for (i in (start_line + 1):length(lines)) {
    line <- lines[i]
    # Count opening and closing braces on this line
    open_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
    close_braces <- lengths(regmatches(line, gregexpr("\\}", line)))
    brace_count <- brace_count + open_braces - close_braces

    if (brace_count == 0) {
      return(i)
    }

    # Safety check: if brace count goes negative, something is wrong
    if (brace_count < 0) {
      return(NA)
    }
  }

  # No matching closing brace found
  return(NA)
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
    if (grepl("^functions\\s*\\{", line_trimmed)) {
      in_functions_block <- TRUE
    } else if (grepl("^(data|transformed data|parameters|transformed parameters|model|generated quantities)\\s*\\{", line_trimmed)) {
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
        code_part <- strsplit(line, "//", fixed = TRUE)[[1]][1]
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
#' if (!prior_only) block unchanged, then other target += statements.
#'
#' @param lines Character vector of Stan code lines
#' @return Character vector with reorganized model block
#' @noRd
reorganize_model_block_statements <- function(lines) {
  checkmate::assert_character(lines, min.len = 0)

  if (length(lines) == 0) return(lines)

  # Find model block
  model_pattern <- "^\\s*model\\s*\\{\\s*$"
  model_start <- grep(model_pattern, lines)[1]
  if (is.na(model_start)) return(lines)

  model_end <- find_matching_closing_brace(lines, model_start)
  if (is.na(model_end)) return(lines)

  # Find if (!prior_only) block
  prior_only_pattern <- "if\\s*\\(\\s*!prior_only\\s*\\)\\s*\\{"
  prior_only_start <- NA
  prior_only_end <- NA

  for (i in (model_start + 1):(model_end - 1)) {
    if (grepl(prior_only_pattern, lines[i])) {
      prior_only_start <- i
      prior_only_end <- find_matching_closing_brace(lines, i)
      break
    }
  }

  # Collect statements by category
  prior_statements <- character(0)
  prior_only_block <- character(0)
  target_statements <- character(0)

  # Preserve if (!prior_only) block unchanged
  if (!is.na(prior_only_start) && !is.na(prior_only_end)) {
    prior_only_block <- lines[prior_only_start:prior_only_end]
  }

  # Classify other statements
  lprior_statement <- character(0)
  other_target_statements <- character(0)

  i <- model_start + 1
  while (i < model_end) {
    # Skip if (!prior_only) block
    if (!is.na(prior_only_start) && i >= prior_only_start && i <= prior_only_end) {
      i <- prior_only_end + 1
      next
    }

    line <- trimws(lines[i])

    if (grepl("^target\\s*\\+=", line)) {
      # Prioritize target += lprior; as first in target section
      if (grepl("target\\s*\\+=\\s*lprior", line)) {
        lprior_statement <- lines[i]
      } else {
        other_target_statements <- c(other_target_statements, lines[i])
      }
    } else if (nzchar(line) && !grepl("^\\s*//", line)) {
      prior_statements <- c(prior_statements, lines[i])
    }

    i <- i + 1
  }

  # Combine target statements with lprior first
  target_statements <- c(lprior_statement, other_target_statements)

  # Reconstruct model block
  before_model <- lines[1:model_start]
  after_model <- lines[model_end:length(lines)]

  new_model <- character(0)
  if (length(prior_statements) > 0) {
    new_model <- c(new_model, prior_statements)
  }
  if (length(prior_only_block) > 0) {
    new_model <- c(new_model, "", prior_only_block)
  }
  if (length(target_statements) > 0) {
    new_model <- c(new_model, "", target_statements)
  }

  return(c(before_model, new_model, after_model))
}

