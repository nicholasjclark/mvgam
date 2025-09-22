#' Stan Code Polishing and Formatting
#'
#' Functions for cleaning up and formatting generated Stan code to follow
#' Stan style guide conventions and best practices. Implements stanc --auto-format
#' functionality with zero dependencies.
#'
#' @name stan_polish
NULL

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

