# R Package Debugging Guide for LLM Agents

## Purpose
This guide provides comprehensive debugging strategies for R package development. Follow these techniques when debugging issues in R packages, particularly when working with complex function chains, environments, and error tracking.

## Core Debugging Tools

### 1. browser() - Interactive Debugging

**Usage**: Insert `browser()` in code to pause execution and enter interactive debugging mode.

```r
my_function <- function(x, y) {
  result <- x * 2
  browser()  # Execution pauses here
  final <- result + y
  return(final)
}
```

**Interactive Commands in browser() mode**:
- `n` (next): Execute next statement
- `s` (step into): Step into function calls
- `f` (finish): Finish current loop/function
- `c` (continue): Exit debugger
- `Q` (quit): Terminate execution
- `where`: Print call stack
- `ls.str()`: List and describe all objects in current environment

**Best Practice - Conditional browser()**:
```r
# Only activate in interactive sessions, not during testing
debug_enabled <- function() {
  interactive() && 
  !identical(Sys.getenv("TESTTHAT"), "true") &&
  !identical(getOption("testthat.disable_browser"), TRUE)
}

my_function <- function(x) {
  if (debug_enabled() && x > 100) {
    browser()  # Only stops when conditions are met
  }
  # Function logic continues
}
```

### 2. debug() and debugonce() - Function-level Debugging

**Usage**: Mark functions for debugging without modifying source code.

```r
# Debug every call to the function
debug(my_function)
my_function(10, 20)  # Enters debugger
undebug(my_function)  # Turn off debugging

# Debug only the next call
debugonce(my_function)
my_function(10, 20)  # Enters debugger once
my_function(5, 10)   # Normal execution
```

**Package Function Debugging**:
```r
# Debug functions from loaded packages
debugonce(dplyr::mutate)
debugonce(stats::lm)

# Debug S3 methods
debugonce(print.data.frame)
```

### 3. trace() - Advanced Function Modification

**Usage**: Insert code at any point in a function without modifying source.

```r
# Basic tracing - prints when function is called
trace(my_function)

# Insert browser at function start (like debug())
trace(my_function, tracer = browser)

# Conditional debugging with trace
trace(my_function, 
      tracer = quote(if (x > 10) browser()),
      at = 3)  # Insert at line 3

# Insert code at specific location
trace(my_function,
      tracer = quote({
        message("x = ", x, ", y = ", y)
        print(ls.str())
      }),
      at = 2)

# Remove tracing
untrace(my_function)
```

### 4. recover() - Post-mortem Debugging

**Usage**: Debug after an error occurs by examining the call stack.

```r
# Set as global error handler
options(error = recover)

# Use in specific context
options(error = recover)
source("problematic_script.R")
options(error = NULL)  # Reset

# Custom recover for package development
safe_recover <- function() {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    recover()
  }
}
options(error = safe_recover)
```

## Environment and State Inspection

### 1. Environment Tracking

```r
# Inspect current environment
environment()
ls.str()  # List all objects with structure
parent.env(environment())  # Parent environment

# Track function environments
track_env <- function(env_name = "current") {
  env <- parent.frame()
  cat("Environment:", env_name, "\n")
  cat("Objects:\n")
  print(ls.str(envir = env))
  cat("Parent:", environmentName(parent.env(env)), "\n\n")
}

# Usage in function
my_function <- function(x) {
  y <- x * 2
  track_env("inside my_function")
  return(y)
}
```

### 2. Call Stack Inspection

```r
# Get call stack during execution
show_call_stack <- function() {
  calls <- sys.calls()
  cat("Call Stack:\n")
  for (i in seq_along(calls)) {
    cat(i, ": ", deparse(calls[[i]])[1], "\n")
  }
}

# Get detailed traceback after error
my_traceback <- function() {
  tb <- traceback()
  for (i in seq_along(tb)) {
    cat("\n", i, ":\n")
    cat(tb[[i]], sep = "\n")
  }
}
```

### 3. Object Internal Inspection

```r
# Deep inspection of object internals
inspect_object <- function(x) {
  cat("Object class:", class(x), "\n")
  cat("Object type:", typeof(x), "\n")
  cat("Object mode:", mode(x), "\n")
  cat("Object length:", length(x), "\n")
  cat("\nAttributes:\n")
  str(attributes(x))
  
  # For environments
  if (is.environment(x)) {
    cat("\nEnvironment contents:\n")
    ls.str(envir = x)
  }
  
  # Use .Internal(inspect()) for deep internals (use cautiously)
  if (interactive()) {
    cat("\nInternal structure:\n")
    .Internal(inspect(x))
  }
}
```

## Advanced Debugging Patterns

### 1. Debug Pipe Operator for Pipelines

```r
# Define debug pipe operator
`%d>%` <- function(lhs, rhs) {
  rhs_call <- substitute(rhs)
  if (is.call(rhs_call)) {
    func_name <- as.character(rhs_call[[1]])
    if (exists(func_name, mode = "function")) {
      debugonce(get(func_name, mode = "function"))
    }
  }
  eval(substitute(lhs %>% rhs), parent.frame())
}

# Usage in pipeline
library(dplyr)
mtcars %>%
  filter(cyl == 6) %d>%  # Debug the next function (group_by)
  group_by(gear) %>%
  summarise(mean_mpg = mean(mpg))
```

### 2. Enhanced Error Context with rlang

```r
# Use rlang for better error tracking
library(rlang)

# Enhanced abort with context
my_function <- function(x, y) {
  if (!is.numeric(x)) {
    abort(
      "x must be numeric",
      class = "type_error",
      x = x,
      expected_type = "numeric",
      actual_type = typeof(x),
      call = caller_env()
    )
  }
  x + y
}

# Get detailed error information
tryCatch(
  my_function("a", 2),
  error = function(e) {
    # Use rlang::last_error() and last_trace() after error
    print(e)
  }
)

# After error occurs:
# rlang::last_error()  # Detailed error info
# rlang::last_trace()  # Full call stack
```

### 3. Test-Aware Debugging

```r
# Create test-aware debugging utilities
debug_if_not_testing <- function(func) {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    debug(func)
  }
}

# Conditional message printing
debug_message <- function(...) {
  if (interactive() && 
      !identical(Sys.getenv("TESTTHAT"), "true") &&
      getOption("package.debug", FALSE)) {
    message("[DEBUG] ", ...)
  }
}

# Usage
debug_message("Processing step 1: x =", x)
```

### 4. Package Development Debugging

```r
# Load package with debugging support
devtools::load_all(".", recompile = TRUE)
options(keep.source = TRUE)  # Preserve source references

# Debug package function
debugonce(package::function_name)

# Trace package internals
trace(
  what = "function_name",
  where = asNamespace("package_name"),
  tracer = browser
)

# Set breakpoints in package code
utils::setBreakpoint("path/to/file.R#42")  # Line 42
```

## Debugging Complex Scenarios

### 1. Debugging Non-Interactive Code

```r
# For R Markdown/scripts that run non-interactively
options(
  error = function() {
    # Save workspace for later inspection
    save.image("debug_workspace.RData")
    
    # Print traceback
    cat("\n--- ERROR OCCURRED ---\n")
    traceback()
    
    # Print session info
    cat("\n--- SESSION INFO ---\n")
    print(sessionInfo())
  }
)

# Later inspect saved workspace
load("debug_workspace.RData")
# Now examine variables that existed at error time
```

### 2. Debugging with withCallingHandlers

```r
# Capture full context while allowing execution to continue
debug_with_context <- function(expr) {
  withCallingHandlers(
    expr,
    error = function(e) {
      cat("\n=== ERROR CONTEXT ===\n")
      cat("Error message:", conditionMessage(e), "\n")
      cat("Call stack:\n")
      print(sys.calls())
      cat("Current environment:\n")
      print(ls.str())
      
      # Re-signal the error
      stop(e)
    },
    warning = function(w) {
      cat("\n=== WARNING CONTEXT ===\n")
      cat("Warning:", conditionMessage(w), "\n")
      cat("Called from:\n")
      print(sys.call(-1))
      
      # Continue execution
      invokeRestart("muffleWarning")
    }
  )
}

# Usage
debug_with_context({
  x <- 10
  y <- "not a number"
  x + y  # Will show full context before error
})
```

### 3. Memory and Performance Debugging

```r
# Track memory usage during execution
debug_memory <- function(expr, label = "Operation") {
  gc_before <- gc(reset = TRUE)
  mem_before <- memory.size()
  
  time_taken <- system.time({
    result <- force(expr)
  })
  
  gc_after <- gc()
  mem_after <- memory.size()
  
  cat("\n=== Performance Report:", label, "===\n")
  cat("Time taken:", time_taken["elapsed"], "seconds\n")
  cat("Memory before:", mem_before, "MB\n")
  cat("Memory after:", mem_after, "MB\n")
  cat("Memory increase:", mem_after - mem_before, "MB\n")
  print(gc_after - gc_before)
  
  invisible(result)
}

# Usage
debug_memory({
  large_matrix <- matrix(rnorm(1e7), ncol = 1000)
  result <- colMeans(large_matrix)
}, label = "Matrix calculation")
```

## Quick Reference Commands

### During Interactive Debugging Session

```r
# Essential inspection commands to use in browser()
ls()              # List objects
ls.str()          # List with details
str(object)       # Structure of specific object
environment()     # Current environment
sys.calls()       # Call stack
sys.frames()      # Frame stack
where             # Show call stack (in browser)
get("x")          # Get object even if masked
exists("x")       # Check if object exists
find("function")  # Find where function is defined

# Modify execution flow
n                 # Next line
s                 # Step into
f                 # Finish current function
c                 # Continue (exit browser)
Q                 # Quit entire execution
```

### Error Recovery Options

```r
# Different error handling strategies
options(error = browser)     # Enter debugger on error
options(error = recover)     # Choose frame to debug
options(error = NULL)        # Default behavior
options(warn = 2)            # Convert warnings to errors

# Restore defaults
options(error = NULL, warn = 0)
```

## Best Practices for Package Development

1. **Always use conditional debugging** that respects test environments
2. **Track environment state** when debugging complex function chains
3. **Use rlang for enhanced error messages** with full context preservation
4. **Implement debug modes** that can be toggled via options
5. **Clean up debug code** before committing (search for browser() calls)
6. **Document debugging requirements** in function comments
7. **Use trace() for temporary debugging** without modifying source
8. **Save workspace on errors** in non-interactive contexts for later analysis

## Common Debugging Patterns

### Pattern 1: Debug Specific Iteration in Loop
```r
for (i in seq_along(items)) {
  if (i == problematic_index) browser()
  # Process items[i]
}
```

### Pattern 2: Debug on Condition
```r
process_data <- function(data) {
  if (any(is.na(data))) {
    save(data, file = "debug_data.RData")
    browser()
  }
  # Continue processing
}
```

### Pattern 3: Progressive Debugging
```r
# Start with high-level function
debugonce(high_level_function)
# Step through until you find the problem area
# Then debug the specific sub-function
debugonce(problematic_subfunction)
```

### Pattern 4: Comparison Debugging
```r
# Compare working vs failing cases
debug_comparison <- function(working_input, failing_input) {
  cat("Working case:\n")
  result1 <- my_function(working_input)
  
  cat("\nFailing case:\n")
  debugonce(my_function)
  result2 <- my_function(failing_input)
  
  # Compare environments, results, etc.
}
```

## Notes for LLM Agents

When debugging R package issues:

1. **First, understand the error context** - use `traceback()` or `rlang::last_trace()`
2. **Identify the error location** - which function and line
3. **Choose appropriate debugging tool**:
   - Use `browser()` for exploring state at specific point
   - Use `debug()` for systematic function debugging
   - Use `trace()` for non-invasive debugging
   - Use `recover()` for post-mortem analysis
4. **Always check test environment** - ensure debugging doesn't interfere with automated tests
5. **Document your debugging process** - leave comments about what was investigated
6. **Clean up after debugging** - remove browser() calls and debug settings
7. **Use defensive programming** - add validation and informative error messages

Remember: The goal is not just to fix the immediate error, but to understand why it occurred and prevent similar issues in the future.