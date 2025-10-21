#' Stan Backend Management for mvgam
#'
#' @description
#' This file contains Stan backend abstraction functions adapted from the brms package.
#' These functions provide unified interfaces for Stan model parsing, compilation, and fitting
#' across different Stan backends (rstan, cmdstanr, mock).
#'
#' @section Attribution:
#' The functions in this file are adapted from the brms package (https://paul-buerkner.github.io/brms/)
#' developed by Paul-Christian Bürkner and the brms development team. Original implementations
#' are used with gratitude and appropriate attribution under the GPL-2 license.
#'
#' Key contributors to the original brms backend system:
#' - Paul-Christian Bürkner (maintainer and primary developer)
#' - The brms development team and community contributors
#'
#' Original source: https://github.com/paul-buerkner/brms/blob/master/R/backends.R
#'
#' @section Modifications for mvgam:
#' Functions have been adapted for integration with mvgam's architecture while maintaining
#' compatibility with the original brms design patterns. Key adaptations include:
#' - Integration with mvgam's validation systems
#' - Compatibility with mvgam object structures
#' - Preservation of all original functionality for seamless backend switching
#'
#' @author Paul-Christian Bürkner (original brms implementation)
#' @author mvgam development team (adaptations for mvgam)

# =============================================================================
# MINIMAL UTILITY FUNCTIONS
# =============================================================================
# Essential functions for Stan backend compatibility following mvgam standards

#' Object type checkers for mvgam compatibility
#' @noRd
is.mvgam <- function(x) {
  inherits(x, "mvgam")
}

#' @noRd
is.stanfit <- function(x) {
  inherits(x, "stanfit")
}

#' Named list creation utility
#' @noRd
nlist <- function(...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}

#' Concatenation replacement function
#' @noRd
`c<-` <- function(x, value) {
  c(x, value)
}

# =============================================================================
# STAN MODEL PARSING
# =============================================================================

#' Parse Stan Model Code
#'
#' @description
#' Validates and parses Stan model code using the specified backend.
#' Adapted from brms backend system.
#'
#' @param model Character string containing Stan model code
#' @param backend Character string specifying backend ("rstan", "cmdstanr", "mock")
#' @param ... Additional arguments passed to backend-specific parsers
#' @return Character string with validated Stan model code
#' @noRd
parse_model <- function(model, backend, ...) {
  checkmate::assert_string(backend)
  .parse_model <- get(paste0(".parse_model_", backend), mode = "function")
  .parse_model(model, ...)
}

#' Parse Stan Model Code with rstan Backend
#' @description
#' Internal function to parse Stan model code using the rstan backend.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Character string containing Stan model code
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to rstan::stanc
#' @return Character string with validated Stan model code
#' @noRd
.parse_model_rstan <- function(model, silent = 1, ...) {
  out <- eval_silent(
    rstan::stanc(model_code = model, ...),
    type = "message", try = TRUE, silent = silent
  )
  out$model_code
}

#' Parse Stan Model Code with cmdstanr Backend
#' @description
#' Internal function to parse Stan model code using the cmdstanr backend.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Character string containing Stan model code
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to cmdstanr functions
#' @return Character string with validated Stan model code
#' @noRd
.parse_model_cmdstanr <- function(model, silent = 1, ...) {
  require_package("cmdstanr")
  temp_file <- cmdstanr::write_stan_file(model)
  # if (cmdstanr::cmdstan_version() >= "2.29.0") {
  #   .canonicalize_stan_model(temp_file, overwrite_file = TRUE)
  # }
  out <- eval_silent(
    cmdstanr::cmdstan_model(temp_file, compile = FALSE, ...),
    type = "message", try = TRUE, silent = silent
  )
  out$check_syntax(quiet = TRUE)
  paste(out$code(), collapse = "\n")
}

#' Parse Stan Model Code with Mock Backend
#' @description
#' Internal function to parse Stan model code using a mock backend for testing.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Character string containing Stan model code
#' @param silent Logical controlling verbosity
#' @param parse_error Optional error message to simulate parsing failure
#' @param parse_check Backend to use for actual parsing ("rstan", "cmdstanr", or NULL)
#' @param ... Additional arguments passed to parsing functions
#' @return Character string with mock or validated Stan model code
#' @noRd
.parse_model_mock <- function(model, silent = TRUE, parse_error = NULL,
                              parse_check = "rstan", ...) {
  if (!is.null(parse_error)) {
    stop(insight::format_error(parse_error), call. = FALSE)
  } else if (parse_check == "rstan") {
    out <- .parse_model_rstan(model, silent = silent, ...)
  } else if (parse_check == "cmdstanr") {
    out <- .parse_model_cmdstanr(model, silent = silent, ...)
  } else if (is.null(parse_check)) {
    out <- "mock_code"
  } else {
    stop(insight::format_error("Unknown 'parse_check' value."), call. = FALSE)
  }
  out
}

# =============================================================================
# STAN MODEL COMPILATION
# =============================================================================

#' Compile Stan Model
#'
#' @description
#' Compiles Stan model using the specified backend.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#'
#' @param model Character string containing Stan model code
#' @param backend Character string specifying backend ("rstan", "cmdstanr", "mock")
#' @param ... Additional arguments passed to backend-specific compilers
#' @return Compiled Stan model object (backend-specific type)
#' @noRd
compile_model <- function(model, backend, ...) {
  checkmate::assert_string(backend)
  .compile_model <- get(paste0(".compile_model_", backend), mode = "function")
  .compile_model(model, ...)
}

#' Compile Stan Model with rstan Backend
#' @description
#' Internal function to compile Stan model using the rstan backend.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Character string containing Stan model code
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to rstan::stan_model
#' @return Compiled rstan model object
#' @noRd
.compile_model_rstan <- function(model, threads, opencl, silent = 1, ...) {
  args <- list(...)
  args$model_code <- model
  if (silent < 2) {
    message("Compiling Stan program...")
  }
  if (use_threading(threads, force = TRUE)) {
    if (utils::packageVersion("rstan") >= "2.26") {
      threads_per_chain_def <- rstan::rstan_options("threads_per_chain")
      on.exit(rstan::rstan_options(threads_per_chain = threads_per_chain_def))
      rstan::rstan_options(threads_per_chain = threads$threads)
    } else {
      stop(insight::format_error(
        "Threading is not supported by backend 'rstan' version {utils::packageVersion('rstan')}."
      ), call. = FALSE)
    }
  }
  if (use_opencl(opencl)) {
    stop(insight::format_error(
      "OpenCL is not supported by backend 'rstan' version {utils::packageVersion('rstan')}."
    ), call. = FALSE)
  }
  eval_silent(
    brms::do_call(rstan::stan_model, args),
    type = "message", try = TRUE, silent = silent >= 2
  )
}

#' Compile Stan Model with cmdstanr Backend
#' @description
#' Internal function to compile Stan model using the cmdstanr backend.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Character string containing Stan model code
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to cmdstanr::cmdstan_model
#' @return Compiled cmdstanr model object
#' @noRd
.compile_model_cmdstanr <- function(model, threads, opencl, silent = 1, ...) {
  require_package("cmdstanr")
  args <- list(...)
  args$stan_file <- cmdstanr::write_stan_file(model)
  # if (cmdstanr::cmdstan_version() >= "2.29.0") {
  #   .canonicalize_stan_model(args$stan_file, overwrite_file = TRUE)
  # }
  if (use_threading(threads, force = TRUE)) {
    args$cpp_options$stan_threads <- TRUE
  }
  if (use_opencl(opencl)) {
    args$cpp_options$stan_opencl <- TRUE
  }
  eval_silent(
    brms::do_call(cmdstanr::cmdstan_model, args),
    type = "message", try = TRUE, silent = silent >= 2
  )
}

#' Compile Stan Model with Mock Backend
#' @description
#' Internal function to compile Stan model using a mock backend for testing.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Character string containing Stan model code
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param compile_check Backend to use for actual compilation ("rstan", "cmdstanr", or NULL)
#' @param compile_error Optional error message to simulate compilation failure
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to compilation functions
#' @return Mock compiled model object or parsed code
#' @noRd
.compile_model_mock <- function(model, threads, opencl, compile_check = "rstan",
                                compile_error = NULL, silent = 1, ...) {
  if (!is.null(compile_error)) {
    stop(insight::format_error(compile_error), call. = FALSE)
  } else if (compile_check == "rstan") {
    out <- .parse_model_rstan(model, silent = silent, ...)
  } else if (compile_check == "cmdstanr") {
    out <- .parse_model_cmdstanr(model, silent = silent, ...)
  } else if (is.null(compile_check)) {
    out <- list()
  } else {
    stop(insight::format_error("Unknown 'compile_check' value."), call. = FALSE)
  }
  out
}

# =============================================================================
# STAN MODEL FITTING
# =============================================================================

#' Fit Stan Model
#'
#' @description
#' Fits Stan model using the specified backend with comprehensive parameter handling.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#'
#' @param model Compiled Stan model object
#' @param backend Character string specifying backend ("rstan", "cmdstanr", "mock")
#' @param ... Additional arguments passed to backend-specific fitting functions
#' @return Fitted Stan model object (stanfit for rstan, CmdStanMCMC for cmdstanr)
#' @noRd
fit_model <- function(model, backend, ...) {
  checkmate::assert_string(backend)
  .fit_model <- get(paste0(".fit_model_", backend), mode = "function")
  .fit_model(model, ...)
}

#' Fit Stan Model with rstan Backend
#' @description
#' Internal function to fit Stan model using the rstan backend with comprehensive
#' parameter handling for MCMC, variational inference, and future processing.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Compiled Stan model object from rstan
#' @param sdata Named list of Stan data
#' @param algorithm Sampling algorithm ("sampling", "meanfield", "fullrank", "fixed_param")
#' @param iter Number of iterations per chain
#' @param warmup Number of warmup iterations
#' @param thin Thinning interval
#' @param chains Number of MCMC chains
#' @param cores Number of CPU cores for parallel processing
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param init Initialization method or values
#' @param exclude Parameters to exclude from output
#' @param seed Random seed for reproducibility
#' @param control Stan control parameters list
#' @param silent Integer controlling verbosity level
#' @param future Logical; use future processing for parallel chains?
#' @param ... Additional arguments passed to Stan sampling functions
#' @return Fitted stanfit object
#' @noRd
.fit_model_rstan <- function(model, sdata, algorithm, iter, warmup, thin,
                             chains, cores, threads, opencl, init, exclude,
                             seed, control, silent, future, ...) {

  # some input checks and housekeeping
  if (use_threading(threads, force = TRUE)) {
    if (utils::packageVersion("rstan") >= "2.26") {
      threads_per_chain_def <- rstan::rstan_options("threads_per_chain")
      on.exit(rstan::rstan_options(threads_per_chain = threads_per_chain_def))
      rstan::rstan_options(threads_per_chain = threads$threads)
    } else {
      stop(insight::format_error(
        "Threading is not supported by backend 'rstan' version {utils::packageVersion('rstan')}."
      ), call. = FALSE)
    }
  }
  if (use_opencl(opencl)) {
    stop(insight::format_error(
      "OpenCL is not supported by backend 'rstan' version {utils::packageVersion('rstan')}."
    ), call. = FALSE)
  }
  if (is.null(init)) {
    init <- "random"
  } else if (is.character(init) && !init %in% c("random", "0")) {
    init <- get(init, mode = "function", envir = parent.frame())
  }
  future <- future && algorithm %in% "sampling"
  args <- nlist(
    object = model, data = sdata, iter, seed,
    init = init, pars = exclude, include = FALSE
  )
  dots <- list(...)
  args[names(dots)] <- dots

  # do the actual sampling
  if (silent < 2) {
    message("Start sampling")
  }
  if (algorithm %in% c("sampling", "fixed_param")) {
    c(args) <- nlist(warmup, thin, control, show_messages = !silent)
    if (algorithm == "fixed_param") {
      args$algorithm <- "Fixed_param"
    }
    if (future) {
      if (!requireNamespace("future", quietly = TRUE)) {
        rlang::inform(
          message = paste0(
            'Package "future" is required for parallel chain processing.\n',
            'Please install it with: install.packages("future")\n',
            'Falling back to standard parallel processing with cores argument.'
          ),
          .frequency = "once",
          .frequency_id = "future_rstan_fitting"
        )
        # Fall back to standard parallel processing
        c(args) <- nlist(chains, cores)
        out <- brms::do_call(rstan::sampling, args)
      } else {
        if (cores > 1L) {
          rlang::warn(insight::format_warning("Argument 'cores' is ignored when using 'future'."))
        }
        args$chains <- 1L
        out <- futures <- vector("list", chains)
        for (i in seq_len(chains)) {
          args$chain_id <- i
          if (is.list(init)) {
            args$init <- init[i]
          }
          futures[[i]] <- future::future(
            brms::do_call(rstan::sampling, args),
            packages = "rstan",
            seed = TRUE
          )
        }
        for (i in seq_len(chains)) {
          out[[i]] <- future::value(futures[[i]])
        }
        out <- rstan::sflist2stanfit(out)
        rm(futures)
      }
    } else {
      c(args) <- nlist(chains, cores)
      out <- brms::do_call(rstan::sampling, args)
    }
  } else if (algorithm %in% c("fullrank", "meanfield")) {
    # vb does not support parallel execution
    c(args) <- nlist(algorithm)
    out <- brms::do_call(rstan::vb, args)
  } else {
    stop(insight::format_error("Algorithm '{algorithm}' is not supported."), call. = FALSE)
  }
  # TODO: add support for pathfinder and laplace
  out <- repair_stanfit(out)
  out
}

#' Fit Stan Model with cmdstanr Backend
#' @description
#' Internal function to fit Stan model using the cmdstanr backend with comprehensive
#' parameter handling for MCMC, variational inference, pathfinder, and laplace.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Compiled Stan model object from cmdstanr
#' @param sdata Named list of Stan data
#' @param algorithm Sampling algorithm ("sampling", "meanfield", "fullrank", "pathfinder", "laplace", "fixed_param")
#' @param iter Number of iterations per chain
#' @param warmup Number of warmup iterations
#' @param thin Thinning interval
#' @param chains Number of MCMC chains
#' @param cores Number of CPU cores for parallel processing
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param init Initialization method or values
#' @param exclude Parameters to exclude from output
#' @param seed Random seed for reproducibility
#' @param control Stan control parameters list
#' @param silent Integer controlling verbosity level
#' @param future Logical; use future processing for parallel chains?
#' @param ... Additional arguments passed to Stan sampling functions
#' @return Fitted stanfit object (converted from cmdstanr output)
#' @noRd
.fit_model_cmdstanr <- function(model, sdata, algorithm, iter, warmup, thin,
                                chains, cores, threads, opencl, init, exclude,
                                seed, control, silent, future, ...) {

  require_package("cmdstanr")
  # some input checks and housekeeping
  class(sdata) <- "list"
  if (is_NA(seed)) {
    seed <- NULL
  }
  if (is_equal(init, "random")) {
    init <- NULL
  } else if (is_equal(init, "0")) {
    init <- 0
  }
  future <- future && algorithm %in% "sampling"
  args <- nlist(data = sdata, seed, init)
  if (use_opencl(opencl)) {
    args$opencl_ids <- opencl$ids
  }
  dots <- list(...)
  args[names(dots)] <- dots
  args[names(control)] <- control

  checkmate::assert_number(chains)
  empty_model <- chains <= 0
  if (empty_model) {
    # fit the model with minimal amount of draws
    # TODO: replace with a better solution
    chains <- 1
    iter <- 2
    warmup <- 1
    thin <- 1
    cores <- 1
  }

  # do the actual sampling
  if (silent < 2) {
    message("Start sampling")
  }
  use_threading <- use_threading(threads, force = TRUE)
  if (algorithm %in% c("sampling", "fixed_param")) {
    c(args) <- list(
      iter_sampling = iter - warmup,
      iter_warmup = warmup,
      chains = chains,
      thin = thin,
      parallel_chains = cores,
      show_messages = silent < 2,
      show_exceptions = silent == 0,
      fixed_param = algorithm == "fixed_param"
    )
    if (use_threading) {
      args$threads_per_chain <- threads$threads
    }
    if (future) {
      if (!requireNamespace("future", quietly = TRUE)) {
        rlang::inform(
          message = paste0(
            'Package "future" is required for parallel chain processing.\n',
            'Please install it with: install.packages("future")\n',
            'Falling back to standard parallel processing with cores argument.'
          ),
          .frequency = "once",
          .frequency_id = "future_cmdstanr_fitting"
        )
        # Fall back to standard parallel processing
        out <- brms::do_call(model$sample, args)
      } else {
        if (cores > 1L) {
          rlang::warn(insight::format_warning("Argument 'cores' is ignored when using 'future'."))
        }
        args$chains <- 1L
        out <- futures <- vector("list", chains)
        for (i in seq_len(chains)) {
          args$chain_ids <- i
          if (is.list(init)) {
            args$init <- init[i]
          }
          futures[[i]] <- future::future(
            brms::do_call(model$sample, args),
            packages = "cmdstanr",
            seed = TRUE
          )
        }
        for (i in seq_len(chains)) {
          out[[i]] <- future::value(futures[[i]])
        }
        rm(futures)
      }
    } else {
      out <- brms::do_call(model$sample, args)
    }
  } else if (algorithm %in% c("fullrank", "meanfield")) {
    c(args) <- nlist(iter, algorithm)
    if (use_threading) {
      args$threads <- threads$threads
    }
    out <- brms::do_call(model$variational, args)
  } else if (algorithm %in% c("pathfinder")) {
    c(args) <- list(num_paths = chains)
    if (use_threading) {
      args$num_threads <- threads$threads
    }
    out <- brms::do_call(model$pathfinder, args)
  } else if (algorithm %in% c("laplace")) {
    if (use_threading) {
      args$threads <- threads$threads
    }
    out <- brms::do_call(model$laplace, args)
  } else {
    stop(insight::format_error("Algorithm '{algorithm}' is not supported."), call. = FALSE)
  }

  if (future) {
    # 'out' is a list of fitted models
    output_files <- ulapply(out, function(x) x$output_files())
    stan_variables <- out[[1]]$metadata()$stan_variables
  } else {
    # 'out' is a single fitted model
    output_files <- out$output_files()
    stan_variables <- out$metadata()$stan_variables
  }

  out <- brms::read_csv_as_stanfit(
    output_files, variables = stan_variables,
    model = model, exclude = exclude, algorithm = algorithm
  )

  if (empty_model) {
    # allow correct updating of an 'empty' model
    out@sim <- list()
  }
  out
}

#' Fit Stan Model with Mock Backend
#' @description
#' Internal function to fit Stan model using a mock backend for testing.
#' Returns mock fitted objects for testing purposes without actual computation.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param model Mock compiled Stan model object
#' @param sdata Named list of Stan data (ignored in mock)
#' @param algorithm Sampling algorithm (ignored in mock)
#' @param iter Number of iterations per chain (ignored in mock)
#' @param warmup Number of warmup iterations (ignored in mock)
#' @param thin Thinning interval (ignored in mock)
#' @param chains Number of MCMC chains (ignored in mock)
#' @param cores Number of CPU cores (ignored in mock)
#' @param threads Threading configuration (ignored in mock)
#' @param opencl OpenCL configuration (ignored in mock)
#' @param init Initialization method (ignored in mock)
#' @param exclude Parameters to exclude (ignored in mock)
#' @param seed Random seed (ignored in mock)
#' @param control Stan control parameters (ignored in mock)
#' @param silent Verbosity level (ignored in mock)
#' @param future Future processing flag (ignored in mock)
#' @param mock_fit Mock fitted object or function returning one
#' @param ... Additional arguments (ignored in mock)
#' @return Mock fitted stanfit object
#' @noRd
.fit_model_mock <- function(model, sdata, algorithm, iter, warmup, thin,
                            chains, cores, threads, opencl, init, exclude,
                            seed, control, silent, future, mock_fit, ...) {
  if (is.function(mock_fit)) {
    out <- mock_fit()
  } else {
    out <- mock_fit
  }
  out
}

# =============================================================================
# BACKEND UTILITY FUNCTIONS
# =============================================================================

#' Extract Compiled Stan Model
#'
#' @description
#' Extracts the compiled Stan model from a fitted object.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#'
#' @param x Fitted model object (brmsfit or mvgam)
#' @return Compiled Stan model object
#' @noRd
compiled_model <- function(x) {
  stopifnot(is.mvgam(x))
  backend <- x$backend %||% "rstan"
  if (backend == "rstan") {
    out <- rstan::get_stanmodel(x$fit)
  } else if (backend == "cmdstanr") {
    out <- attributes(x$fit)$CmdStanModel
  } else if (backend == "mock") {
    stop(insight::format_error("'compiled_model' is not supported in the mock backend."), call. = FALSE)
  }
  out
}

# Does the model need recompilation before being able to sample again?
needs_recompilation <- function(x) {
  stopifnot(is.mvgam(x))
  backend <- x$backend %||% "rstan"
  if (backend == "rstan") {
    # TODO: figure out when rstan requires recompilation
    out <- FALSE
  } else if (backend == "cmdstanr") {
    exe_file <- attributes(x$fit)$CmdStanModel$exe_file()
    out <- !is.character(exe_file) || !file.exists(exe_file)
  } else if (backend == "mock") {
    out <- FALSE
  }
  out
}

# extract the elapsed time during model fitting
# @param x brmsfit object
elapsed_time <- function(x) {
  stopifnot(is.mvgam(x))
  backend <- x$backend %||% "rstan"
  if (backend == "rstan") {
    out <- rstan::get_elapsed_time(x$fit)
    out <- data.frame(
      chain_id = seq_len(nrow(out)),
      warmup = out[, "warmup"],
      sampling = out[, "sample"]
    )
    out$total <- out$warmup + out$sampling
    rownames(out) <- NULL
  } else if (backend == "cmdstanr") {
    out <- attributes(x$fit)$metadata$time$chains
  } else if (backend == "mock") {
    stop(insight::format_error("'elapsed_time' not supported in the mock backend."), call. = FALSE)
  }
  out
}

#' Supported Stan Backends
#'
#' @description
#' Returns vector of supported Stan backends.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#'
#' @return Character vector of backend names
#' @noRd
backend_choices <- function() {
  c("rstan", "cmdstanr", "mock")
}

#' Supported Stan Algorithms
#'
#' @description
#' Returns vector of supported Stan sampling algorithms.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#'
#' @return Character vector of algorithm names
#' @noRd
algorithm_choices <- function() {
  c("sampling", "meanfield", "fullrank", "pathfinder", "laplace", "fixed_param")
}

#' Require Specific Backend
#'
#' @description
#' Validates that fitted model uses required backend.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#'
#' @param backend Character string with required backend
#' @param x Fitted model object
#' @return Logical TRUE if validation passes (stops with error otherwise)
#' @noRd
require_backend <- function(backend, x) {
  stopifnot(is.mvgam(x))
  backend <- match.arg(backend, backend_choices())
  if (isTRUE(x$backend != backend)) {
    stop(insight::format_error("Backend '{backend}' is required for this method."), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check if Object is brmsthreads
#' @description
#' Tests if an object has class brmsthreads.
#' Adapted from brms backend system.
#' @param x Object to test
#' @return Logical indicating if x is a brmsthreads object
#' @noRd
is.brmsthreads <- function(x) {
  inherits(x, "brmsthreads")
}

#' Validate Threading Argument
#' @description
#' Validates and normalizes the threads argument for Stan threading.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param threads NULL, numeric, or brmsthreads object
#' @return Validated brmsthreads object
#' @noRd
validate_threads <- function(threads) {
  if (is.null(threads)) {
    threads <- brms::threading()
  } else if (is.numeric(threads)) {
    checkmate::assert_number(threads)
    threads <- brms::threading(threads)
  } else if (!is.brmsthreads(threads)) {
    stop(insight::format_error(
      "Argument 'threads' needs to be numeric or specified via the 'threading' function."
    ), call. = FALSE)
  }
  threads
}

#' Check if Threading is Activated
#' @description
#' Determines whether Stan threading should be used based on configuration.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param threads Threading configuration object
#' @param force Logical; force threading even if force flag is set?
#' @return Logical indicating if threading should be used
#' @noRd
use_threading <- function(threads, force = FALSE) {
  threads <- validate_threads(threads)
  out <- isTRUE(threads$threads > 0)
  if (!force) {
    # Stan code will only be altered in non-forced mode
    out <- out && !isTRUE(threads$force)
  }
  out
}

#' Check if Object is brmsopencl
#' @description
#' Tests if an object has class brmsopencl.
#' Adapted from brms backend system.
#' @param x Object to test
#' @return Logical indicating if x is a brmsopencl object
#' @noRd
is.brmsopencl <- function(x) {
  inherits(x, "brmsopencl")
}

#' Validate OpenCL Argument
#' @description
#' Validates and normalizes the opencl argument for Stan OpenCL support.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param opencl NULL, numeric, or brmsopencl object
#' @return Validated brmsopencl object
#' @noRd
validate_opencl <- function(opencl) {
  if (is.null(opencl)) {
    opencl <- brms::opencl()
  } else if (is.numeric(opencl)) {
    opencl <- brms::opencl(opencl)
  } else if (!is.brmsopencl(opencl)) {
    stop(insight::format_error(
      "Argument 'opencl' needs to an integer vector or specified via the 'opencl' function."
    ), call. = FALSE)
  }
  opencl
}

#' Check if OpenCL is Activated
#' @description
#' Determines whether Stan OpenCL should be used based on configuration.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param opencl OpenCL configuration object
#' @return Logical indicating if OpenCL should be used
#' @noRd
use_opencl <- function(opencl) {
  !is.null(validate_opencl(opencl)$ids)
}

#' Validate Silent Argument
#' @description
#' Validates the silent argument for controlling output verbosity.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param silent Integer from 0 to 2 controlling verbosity level
#' @return Validated silent value
#' @noRd
validate_silent <- function(silent) {
  checkmate::assert_int(silent, lower = 0, upper = 2)
  silent
}

#' Repair Variable Names for Stan Compatibility
#' @description
#' Converts variable names to proper Stan array syntax (e.g., b.1.1 to b[1,1]).
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param x Character vector of variable names to repair
#' @return Character vector with corrected Stan array syntax
#' @noRd
repair_variable_names <- function(x) {
  x <- sub("\\.", "[", x)
  x <- gsub("\\.", ",", x)
  x[grep("\\[", x)] <- paste0(x[grep("\\[", x)], "]")
  x
}

#' Repair Parameter Names in stanfit Objects
#' @description
#' Ensures parameter names in stanfit objects are unique and properly formatted
#' for compatibility with the posterior package.
#' Adapted from brms backend system by Paul-Christian Bürkner.
#' @param x stanfit object to repair
#' @return stanfit object with repaired parameter names
#' @noRd
repair_stanfit <- function(x) {
  stopifnot(is.stanfit(x))
  if (!length(x@sim$fnames_oi)) {
    # nothing to rename
    return(x)
  }
  # the posterior package cannot deal with non-unique parameter names
  # this case happens rarely but might happen when sample_prior = "yes"
  x@sim$fnames_oi <- make.unique(as.character(x@sim$fnames_oi), "__")
  for (i in seq_along(x@sim$samples)) {
    # stanfit may have renamed dimension suffixes (#1218)
    if (length(x@sim$samples[[i]]) == length(x@sim$fnames_oi)) {
      names(x@sim$samples[[i]]) <- x@sim$fnames_oi
    }
  }
  x
}

#' Possible Options for file_refit Argument
#' @description
#' Returns valid options for file refit control.
#' Adapted from brms backend system.
#' @return Character vector of valid options
#' @noRd
file_refit_options <- function() {
  c("never", "always", "on_change")
}

#' Unlist lapply Output
#' @description
#' Convenience function to unlist lapply results.
#' Adapted from brms backend system.
#' @param X A vector (atomic or list)
#' @param FUN Function to apply to each element
#' @param ... Additional arguments to FUN
#' @param recursive Logical; should unlisting be applied recursively?
#' @param use.names Logical; should names be preserved?
#' @return Unlisted result of lapply
#' @noRd
ulapply <- function(X, FUN, ..., recursive = TRUE, use.names = TRUE) {
  unlist(lapply(X, FUN, ...), recursive, use.names)
}

#' Check if Package is Installed
#' @description
#' Validates that a required package is installed with optional version check.
#' Adapted from brms backend system for mvgam compatibility.
#' @param package Character string with package name
#' @param version Optional minimum version requirement
#' @return Invisible TRUE if package requirements met
#' @noRd
require_package <- function(package, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(insight::format_error("Please install the '{package}' package."), call. = FALSE)
  }
  if (!is.null(version)) {
    version <- as.package_version(version)
    if (utils::packageVersion(package) < version) {
      stop(insight::format_error(
        "Please install package '{package}' version {version} or higher."
      ), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Check Equality with Tolerance
#' @description
#' Tests for equality using all.equal with default tolerance.
#' Adapted from brms backend system.
#' @param x First object to compare
#' @param y Second object to compare
#' @param check.attributes Logical; should attributes be checked?
#' @param ... Additional arguments passed to all.equal
#' @return Logical indicating if objects are equal
#' @noRd
is_equal <- function(x, y, check.attributes = FALSE, ...) {
  isTRUE(all.equal(x, y, check.attributes = check.attributes, ...))
}

#' Check for Single NA Value
#' @description
#' Tests if an object is a single NA value.
#' Adapted from brms backend system.
#' @param x Object to test
#' @return Logical indicating if x is a single NA
#' @noRd
is_NA <- function(x) {
  length(x) == 1L && is.na(x)
}
