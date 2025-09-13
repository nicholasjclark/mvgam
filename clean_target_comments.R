# Script to remove inline comments from target Stan code files
# Preserves essential header comments but removes all // inline comments
# Also cleans up formatting: removes excessive blank lines and applies consistent indenting

#' Clean Stan code formatting and comments
#' @param stan_code Character vector of Stan code lines
#' @return Character vector with comments removed and formatting cleaned
clean_stan_code <- function(stan_code) {
  # Preserve header comments (first 15 lines that start with //)
  header_lines <- character(0)
  code_start <- 1

  for (i in seq_along(stan_code)) {
    line <- stan_code[i]
    # Check if this is a header comment line (starts with // and is in first 15 lines)
    if (i <= 15 && grepl("^\\s*//", line)) {
      header_lines <- c(header_lines, line)
      code_start <- i + 1
    } else if (grepl("^\\s*//", line)) {
      # Skip standalone comment lines after header
      next
    } else {
      # This is where actual code starts
      break
    }
  }

  # Process the rest of the code
  if (code_start <= length(stan_code)) {
    code_lines <- stan_code[code_start:length(stan_code)]

    # Remove inline comments (// and everything after on same line)
    cleaned_code <- gsub("//.*$", "", code_lines)

    # Remove trailing whitespace
    cleaned_code <- trimws(cleaned_code, which = "right")

    # Apply consistent indenting and remove excessive blank lines
    formatted_code <- format_stan_code(cleaned_code)

    # Combine header and cleaned code
    result <- c(header_lines, "", formatted_code)
  } else {
    result <- header_lines
  }

  # Remove empty lines at the end
  while (length(result) > 0 && result[length(result)] == "") {
    result <- result[-length(result)]
  }

  return(result)
}

#' Format Stan code with consistent indenting and spacing
#' @param code_lines Character vector of Stan code lines
#' @return Character vector with improved formatting
format_stan_code <- function(code_lines) {
  formatted_lines <- character(0)
  indent_level <- 0
  prev_line_empty <- FALSE

  for (line in code_lines) {
    # Skip empty lines but track them
    if (trimws(line) == "") {
      # Only add one blank line if previous line wasn't empty
      if (!prev_line_empty && length(formatted_lines) > 0) {
        formatted_lines <- c(formatted_lines, "")
        prev_line_empty <- TRUE
      }
      next
    }

    prev_line_empty <- FALSE

    # Determine indentation changes
    line_trimmed <- trimws(line)

    # Decrease indent for closing braces/blocks
    if (grepl("^\\}", line_trimmed)) {
      indent_level <- max(0, indent_level - 1)
    }

    # Apply current indentation (2 spaces per level)
    indented_line <- paste0(strrep("  ", indent_level), line_trimmed)
    formatted_lines <- c(formatted_lines, indented_line)

    # Increase indent for opening braces/blocks
    if (grepl("\\{\\s*$", line_trimmed)) {
      indent_level <- indent_level + 1
    }

    # Special handling for Stan blocks
    if (grepl("^(functions|data|transformed data|parameters|transformed parameters|model|generated quantities)\\s*\\{", line_trimmed)) {
      indent_level <- 1  # Reset to 1 for Stan block content
    }
  }

  return(formatted_lines)
}

# Process each target file
target_files <- paste0("tasks/target_stancode_", 1:8, ".stan")

for (file_path in target_files) {
  if (file.exists(file_path)) {
    cat("Processing", file_path, "...\n")

    # Read the file
    stan_code <- readLines(file_path)

    # Clean comments and format code
    cleaned_code <- clean_stan_code(stan_code)

    # Write back to the same file
    writeLines(cleaned_code, file_path)

    cat("✓ Cleaned and formatted", file_path, "\n")
  } else {
    cat("⚠ File not found:", file_path, "\n")
  }
}

cat("\n🎉 All target files cleaned and formatted!\n")
cat("- Removed all inline comments while preserving essential headers\n")
cat("- Applied consistent 2-space indenting\n")
cat("- Removed excessive blank lines (max 1 consecutive)\n")
