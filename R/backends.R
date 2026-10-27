# Stan backend functions for parsing, compiling and fitting with the
# rstan, cmdstanr and mock backends. They are adapted from
# `R/backends.R` of brms (https://github.com/paul-buerkner/brms), by
# Paul-Christian Burkner and the brms contributors, under the GPL-2
# license.

# =============================================================================
# Utility functions
# =============================================================================

#' Whether an object is a `stanfit`
#'
#' @param x Object to test
#' @return A single logical
#' @noRd
is.stanfit <- function(x) {
  inherits(x, "stanfit")
}

#' A list named by its arguments
#'
#' Unnamed arguments take the deparsed argument expression as name.
#'
#' @param ... Objects to collect
#' @return A named list
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

#' Append to an object in place
#'
#' @param x Object to extend
#' @param value Elements to append
#' @return `c(x, value)`
#' @noRd
`c<-` <- function(x, value) {
  c(x, value)
}

# =============================================================================
# Stan model parsing
# =============================================================================

#' Parse Stan model code
#'
#' @description
#' Validates and parses Stan model code using the specified backend.
#'
#' @param model Character string containing Stan model code
#' @param backend Character string specifying backend ("rstan",
#'   "cmdstanr", "mock")
#' @param ... Additional arguments passed to backend-specific parsers
#' @return Character string with validated Stan model code
#' @noRd
parse_model <- function(model, backend, ...) {
  checkmate::assert_string(model)
  checkmate::assert_string(backend)
  .parse_model <- get(paste0(".parse_model_", backend), mode = "function")
  .parse_model(model, ...)
}

#' Parse Stan model code with the rstan backend
#' @description
#' Internal function to parse Stan model code using the rstan backend.
#' @param model Character string containing Stan model code
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to rstan::stanc
#' @return Character string with validated Stan model code
#' @noRd
.parse_model_rstan <- function(model, silent = 1, ...) {
  checkmate::assert_string(model)
  out <- eval_silent(
    rstan::stanc(model_code = model, ...),
    type = "message", silent = silent
  )
  out$model_code
}

#' Parse Stan model code with the cmdstanr backend
#' @description
#' Internal function to parse Stan model code using the cmdstanr backend.
#' @param model Character string containing Stan model code
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to cmdstanr functions
#' @return Character string with validated Stan model code
#' @noRd
.parse_model_cmdstanr <- function(model, silent = 1, ...) {
  checkmate::assert_string(model)
  insight::check_if_installed("cmdstanr")
  temp_file <- cmdstanr::write_stan_file(model)
  out <- eval_silent(
    cmdstanr::cmdstan_model(temp_file, compile = FALSE, ...),
    type = "message", silent = silent
  )
  out$check_syntax(quiet = TRUE)
  paste(out$code(), collapse = "\n")
}

#' Parse Stan model code with the mock backend
#' @description
#' Internal function to parse Stan model code using a mock backend for testing.
#' @param model Character string containing Stan model code
#' @param silent Logical controlling verbosity
#' @param parse_error Optional error message to simulate parsing failure
#' @param parse_check Backend to use for actual parsing ("rstan",
#'   "cmdstanr", or NULL)
#' @param ... Additional arguments passed to parsing functions
#' @return Character string with mock or validated Stan model code
#' @noRd
.parse_model_mock <- function(model, silent = TRUE, parse_error = NULL,
                              parse_check = "rstan", ...) {
  checkmate::assert_string(model)
  checkmate::assert_string(parse_error, null.ok = TRUE)
  checkmate::assert_string(parse_check, null.ok = TRUE)
  if (!is.null(parse_error)) {
    stop(insight::format_error(parse_error), call. = FALSE)
  } else if (is.null(parse_check)) {
    out <- "mock_code"
  } else if (parse_check == "rstan") {
    out <- .parse_model_rstan(model, silent = silent, ...)
  } else if (parse_check == "cmdstanr") {
    out <- .parse_model_cmdstanr(model, silent = silent, ...)
  } else {
    stop(insight::format_error("Unknown 'parse_check' value."), call. = FALSE)
  }
  out
}

# =============================================================================
# Stan model compilation
# =============================================================================

#' Compile a Stan model
#'
#' @description
#' Compiles Stan model using the specified backend.
#'
#' @param model Character string containing Stan model code
#' @param backend Character string specifying backend ("rstan",
#'   "cmdstanr", "mock")
#' @param ... Additional arguments passed to backend-specific compilers
#' @return Compiled Stan model object (backend-specific type)
#' @noRd
compile_model <- function(model, backend, ...) {
  checkmate::assert_string(model)
  checkmate::assert_string(backend)
  .compile_model <- get(paste0(".compile_model_", backend), mode = "function")
  .compile_model(model, ...)
}

#' Compile a Stan model with the rstan backend
#' @description
#' Internal function to compile Stan model using the rstan backend.
#' @param model Character string containing Stan model code
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to rstan::stan_model
#' @return Compiled rstan model object
#' @noRd
.compile_model_rstan <- function(model, threads, opencl, silent = 1, ...) {
  checkmate::assert_string(model)
  checkmate::assert_number(silent)
  args <- list(...)
  args$model_code <- model
  if (silent < 2) {
    rlang::inform("Compiling Stan program...")
  }
  old_threads <- set_rstan_threads(threads, opencl)
  if (!is.null(old_threads)) {
    on.exit(rstan::rstan_options(threads_per_chain = old_threads),
            add = TRUE)
  }
  eval_silent(
    brms::do_call(rstan::stan_model, args),
    type = "message", silent = silent >= 2
  )
}

#' Set rstan's threads per chain, refusing what rstan cannot run
#'
#' Threading needs rstan 2.26 or later. rstan runs no OpenCL.
#'
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @return The previous `threads_per_chain` option, for the caller to
#'   restore, or NULL when threading is off
#' @noRd
set_rstan_threads <- function(threads, opencl) {
  if (use_opencl(opencl)) {
    stop(insight::format_error(
      cli::format_inline(
        "OpenCL is not supported by backend 'rstan' version ",
        "{utils::packageVersion('rstan')}."
      )
    ), call. = FALSE)
  }
  if (!use_threading(threads, force = TRUE)) {
    return(NULL)
  }
  if (utils::packageVersion("rstan") < "2.26") {
    stop(insight::format_error(
      cli::format_inline(
        "Threading is not supported by backend 'rstan' version ",
        "{utils::packageVersion('rstan')}."
      )
    ), call. = FALSE)
  }
  old <- rstan::rstan_options("threads_per_chain")
  rstan::rstan_options(threads_per_chain = threads$threads)
  old
}


#' Compile a Stan model with the cmdstanr backend
#' @description
#' Internal function to compile Stan model using the cmdstanr backend.
#' @param model Character string containing Stan model code
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to cmdstanr::cmdstan_model
#' @return Compiled cmdstanr model object
#' @noRd
.compile_model_cmdstanr <- function(model, threads, opencl, silent = 1, ...) {
  checkmate::assert_string(model)
  checkmate::assert_number(silent)
  insight::check_if_installed("cmdstanr")
  args <- list(...)
  args$stan_file <- cmdstanr::write_stan_file(model)
  if (use_threading(threads, force = TRUE)) {
    args$cpp_options$stan_threads <- TRUE
  }
  if (use_opencl(opencl)) {
    args$cpp_options$stan_opencl <- TRUE
  }
  eval_silent(
    brms::do_call(cmdstanr::cmdstan_model, args),
    type = "message", silent = silent >= 2
  )
}

#' Compile a Stan model with the mock backend
#' @description
#' Internal function to compile Stan model using a mock backend for testing.
#' @param model Character string containing Stan model code
#' @param threads Threading configuration object
#' @param opencl OpenCL configuration object
#' @param compile_check Backend to use for actual compilation ("rstan",
#'   "cmdstanr", or NULL)
#' @param compile_error Optional error message to simulate compilation failure
#' @param silent Integer controlling verbosity (0=verbose, 1=moderate, 2=silent)
#' @param ... Additional arguments passed to compilation functions
#' @return Mock compiled model object or parsed code
#' @noRd
.compile_model_mock <- function(model, threads, opencl,
                                compile_check = "rstan",
                                compile_error = NULL, silent = 1, ...) {
  checkmate::assert_string(model)
  checkmate::assert_string(compile_check, null.ok = TRUE)
  checkmate::assert_string(compile_error, null.ok = TRUE)
  if (!is.null(compile_error)) {
    stop(insight::format_error(compile_error), call. = FALSE)
  } else if (is.null(compile_check)) {
    out <- list()
  } else if (compile_check == "rstan") {
    out <- .parse_model_rstan(model, silent = silent, ...)
  } else if (compile_check == "cmdstanr") {
    out <- .parse_model_cmdstanr(model, silent = silent, ...)
  } else {
    stop(insight::format_error("Unknown 'compile_check' value."), call. = FALSE)
  }
  out
}

# =============================================================================
# Stan model fitting
# =============================================================================

#' Fit a Stan model
#'
#' @description
#' Fits Stan model using the specified backend, dispatching to
#' backend-specific handlers.
#'
#' @param model Compiled Stan model object
#' @param backend Character string specifying backend ("rstan",
#'   "cmdstanr", "mock")
#' @param ... Additional arguments passed to backend-specific fitting
#'   functions
#' @return Fitted Stan model object (stanfit for rstan, CmdStanMCMC for
#'   cmdstanr)
#' @noRd
fit_model <- function(model, backend, ...) {
  checkmate::assert_string(backend)
  .fit_model <- get(paste0(".fit_model_", backend), mode = "function")
  .fit_model(model, ...)
}

#' Fit a Stan model with the rstan backend
#' @description
#' Internal function to fit Stan model using the rstan backend, handling
#' parameters for MCMC, variational inference and parallel chains
#' through the future package.
#' @param model Compiled Stan model object from rstan
#' @param sdata Named list of Stan data
#' @param algorithm Sampling algorithm ("sampling", "meanfield",
#'   "fullrank", "fixed_param")
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
#' @param future Logical; run chains in parallel with the future
#'   package?
#' @param ... Additional arguments passed to Stan sampling functions
#' @return Fitted stanfit object
#' @noRd
.fit_model_rstan <- function(model, sdata, algorithm, iter, warmup, thin,
                             chains, cores, threads, opencl, init, exclude,
                             seed, control, silent, future, ...) {

  checkmate::assert_list(sdata)
  checkmate::assert_string(algorithm)
  checkmate::assert_count(iter, positive = TRUE)
  checkmate::assert_count(warmup)
  checkmate::assert_count(thin, positive = TRUE)
  checkmate::assert_count(chains, positive = TRUE)
  checkmate::assert_count(cores, positive = TRUE)
  checkmate::assert_list(control, null.ok = TRUE)
  checkmate::assert_number(silent)
  checkmate::assert_flag(future)
  # Input checks and housekeeping
  old_threads <- set_rstan_threads(threads, opencl)
  if (!is.null(old_threads)) {
    on.exit(rstan::rstan_options(threads_per_chain = old_threads),
            add = TRUE)
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
  # rstan prints its progress lines unless `refresh = 0`. `silent = 2`
  # prints nothing, as it does under cmdstanr. A `refresh` the caller
  # gives is kept.
  if (silent >= 2 && is.null(args$refresh)) {
    args$refresh <- 0
  }

  if (silent < 2) {
    rlang::inform("Start sampling")
  }
  if (algorithm %in% c("sampling", "fixed_param")) {
    c(args) <- nlist(warmup, thin, control, show_messages = !silent)
    if (algorithm == "fixed_param") {
      args$algorithm <- "Fixed_param"
    }
    if (future) {
      if (!future_available()) {
        c(args) <- nlist(chains, cores)
        out <- brms::do_call(rstan::sampling, args)
      } else {
        if (cores > 1L) {
          insight::format_warning(
            "Argument 'cores' is ignored when using 'future'."
          )
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
    # rstan::vb() runs a single chain
    c(args) <- nlist(algorithm)
    out <- brms::do_call(rstan::vb, args)
  } else {
    stop(insight::format_error(
      cli::format_inline("Algorithm '{algorithm}' is not supported.")
    ), call. = FALSE)
  }
  # 'pathfinder' and 'laplace' are CmdStan algorithms with no rstan
  # equivalent. Only the cmdstanr backend reaches them.
  out <- repair_stanfit(out)
  out
}

#' Fit a Stan model with the cmdstanr backend
#' @description
#' Internal function to fit Stan model using the cmdstanr backend, handling
#' parameters for MCMC, variational inference, pathfinder, and laplace.
#' @param model Compiled Stan model object from cmdstanr
#' @param sdata Named list of Stan data
#' @param algorithm Sampling algorithm ("sampling", "meanfield",
#'   "fullrank", "pathfinder", "laplace", "fixed_param")
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
#' @param future Logical; run chains in parallel with the future
#'   package?
#' @param ... Additional arguments passed to Stan sampling functions
#' @return Fitted stanfit object (converted from cmdstanr output)
#' @noRd
.fit_model_cmdstanr <- function(model, sdata, algorithm, iter, warmup, thin,
                                chains, cores, threads, opencl, init, exclude,
                                seed, control, silent, future, ...) {

  checkmate::assert_list(sdata)
  checkmate::assert_string(algorithm)
  checkmate::assert_count(iter, positive = TRUE)
  checkmate::assert_count(warmup)
  checkmate::assert_count(thin, positive = TRUE)
  checkmate::assert_count(chains, positive = TRUE)
  checkmate::assert_count(cores, positive = TRUE)
  checkmate::assert_list(control, null.ok = TRUE)
  checkmate::assert_number(silent)
  checkmate::assert_flag(future)
  insight::check_if_installed("cmdstanr")
  # mvgam's own starting values set only the parameters that need one.
  # cmdstanr reports every partial init list, which here says nothing
  # the user asked about.
  if (isTRUE(attr(init, "mvgam_partial"))) {
    old_warn <- options(cmdstanr_warn_inits = FALSE)
    on.exit(options(old_warn), add = TRUE)
  }
  # Input checks and housekeeping
  class(sdata) <- "list"
  if (is_NA(seed)) {
    seed <- NULL
  }
  # The "pathfinder" keyword names a strategy. Its value exists only
  # once the model is compiled. Stan never receives the keyword: `init`
  # is blanked here and `pathfinder_init` records the request.
  pathfinder_init <- is_equal(init, "pathfinder")
  if (is_equal(init, "random") || pathfinder_init) {
    init <- NULL
  } else if (is_equal(init, "0")) {
    init <- 0
  }
  future <- future && algorithm %in% "sampling"
  # Every cmdstanr method takes the two output settings. `silent`
  # governs them for each algorithm alike.
  args <- nlist(data = sdata, seed, init,
                show_messages = silent < 2, show_exceptions = silent == 0)
  if (use_opencl(opencl)) {
    args$opencl_ids <- opencl$ids
  }
  dots <- list(...)
  refuse_unknown_cmdstanr_args(model, algorithm, names(dots))
  args[names(dots)] <- dots
  args[names(control)] <- control


  # Run the sampler
  if (silent < 2) {
    rlang::inform("Start sampling")
  }
  threading_on <- use_threading(threads, force = TRUE)

  # Starting values from Pathfinder. Passing the approximation back as
  # `init` lets cmdstanr draw one starting value per chain from the
  # approximate posterior. The default draws from U(-2, 2) on the
  # unconstrained scale. Reason: a state-space model declares one
  # innovation per time point and latent variable. Random starts place
  # hundreds of correlated parameters far from the typical set.
  if (pathfinder_init) {
    insight::check_if_installed("cmdstanr", minimum_version = "0.8.1")
    if (silent < 2) {
      rlang::inform("Running Pathfinder to obtain initial values")
    }
    args$init <- run_pathfinder(model, args, chains, threading_on,
                                threads, starts = TRUE)
  }
  if (algorithm %in% c("sampling", "fixed_param")) {
    c(args) <- list(
      iter_sampling = iter - warmup,
      iter_warmup = warmup,
      chains = chains,
      thin = thin,
      parallel_chains = cores,
      fixed_param = algorithm == "fixed_param"
    )
    if (threading_on) {
      args$threads_per_chain <- threads$threads
    }
    if (future) {
      if (!future_available()) {
        out <- brms::do_call(model$sample, args)
      } else {
        if (cores > 1L) {
          insight::format_warning(
            "Argument 'cores' is ignored when using 'future'."
          )
        }
        args$chains <- 1L
        out <- futures <- vector("list", chains)
        for (i in seq_len(chains)) {
          args$chain_ids <- i
          # Only list inits are split per chain. A Pathfinder init is
          # an R6 fit object. cmdstanr resamples one starting value per
          # chain from it internally. Every chain receives it whole.
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
    if (threading_on) {
      args$threads <- threads$threads
    }
    out <- brms::do_call(model$variational, args)
  } else if (algorithm %in% c("pathfinder")) {
    out <- run_pathfinder(model, args, chains, threading_on, threads)
  } else if (algorithm %in% c("laplace")) {
    if (threading_on) {
      args$threads <- threads$threads
    }
    # `$laplace()` finds the mode with `$optimize()`, which receives
    # the output settings through `opt_args` alone.
    args$opt_args <- utils::modifyList(
      args[c("show_messages", "show_exceptions")],
      as.list(args$opt_args)
    )
    out <- brms::do_call(model$laplace, args)
  } else {
    stop(insight::format_error(
      cli::format_inline("Algorithm '{algorithm}' is not supported.")
    ), call. = FALSE)
  }

  # `$metadata()` and `$output_files()` below assume a chain produced
  # output. A run where none did fails inside cmdstanr with "Unable to
  # retrieve the metadata", which names neither the model nor the
  # cause. The pathfinder branch checks its return codes already. The
  # sampler takes the same check here. A run that kept some chains
  # continues. `check_chains_finished()` names the shortfall after the
  # fit.
  if (algorithm %in% c("sampling", "fixed_param")) {
    codes <- if (future) {
      ulapply(out, function(x) x$return_codes())
    } else {
      out$return_codes()
    }
    if (length(codes) > 0L && all(codes != 0)) {
      stop(insight::format_error(c(
        "Every Stan chain exited with an error.",
        x = paste0(
          "All ", length(codes), " chains returned a non-zero code."
        ),
        i = "Refit with 'silent = 0' to see Stan's own messages.",
        i = "Try a different 'seed' or 'init', or a higher 'adapt_delta'."
      )), call. = FALSE)
    }
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

  out <- read_cmdstan_stanfit(
    output_files, variables = stan_variables,
    model = model, exclude = exclude, algorithm = algorithm
  )

  out
}

#' Fit a Stan model with the mock backend
#' @description
#' Internal function to fit Stan model using a mock backend for testing.
#' Returns mock fitted objects for testing purposes without actual computation.
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
#' @param future Parallel chains setting (ignored in mock)
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
# Backend utility functions
# =============================================================================

#' Extract the compiled Stan model of a fit
#'
#' @description
#' Extracts the compiled Stan model from a fitted object.
#'
#' @param x Fitted model object (brmsfit or mvgam)
#' @return Compiled Stan model object
#' @noRd
compiled_model <- function(x) {
  checkmate::assert_class(x, "mvgam")
  backend <- x$backend
  checkmate::assert_choice(backend, backend_choices(),
                           .var.name = "x$backend")
  if (backend == "rstan") {
    out <- rstan::get_stanmodel(x$fit)
  } else if (backend == "cmdstanr") {
    out <- attributes(x$fit)$CmdStanModel
  } else if (backend == "mock") {
    stop(insight::format_error(
      "'compiled_model' is not supported in the mock backend."
    ), call. = FALSE)
  }
  out
}

#' Supported Stan backends
#'
#' @description
#' Returns vector of supported Stan backends.
#'
#' @return Character vector of backend names
#' @noRd
backend_choices <- function() {
  c("rstan", "cmdstanr", "mock")
}

#' The Stan version a backend compiles with in this session
#'
#' The version of Stan itself. An R package version can differ: rstan
#' 2.32.7 ships Stan 2.32.2. A fit records this version when it is
#' built. The session that later describes it may have another.
#'
#' @param backend One of `backend_choices()`.
#' @return A character string, or `NA` for the `"mock"` backend and for
#'   cmdstanr with no CmdStan installed. The mock backend runs no
#'   compiler.
#' @noRd
live_stan_version <- function(backend) {
  checkmate::assert_choice(backend, backend_choices())
  if (identical(backend, "cmdstanr")) {
    insight::check_if_installed("cmdstanr")
    v <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
    return(if (is.null(v)) NA_character_ else as.character(v))
  }
  if (identical(backend, "rstan")) {
    return(as.character(rstan::stan_version()))
  }
  NA_character_
}

#' Assert the active Stan backend ships at least a given Stan version
#'
#' Returns the underlying engine version reported by the live backend
#' (`rstan::stan_version()` for `"rstan"`,
#' `cmdstanr::cmdstan_version()` for `"cmdstanr"`). Errors with an
#' upgrade hint when it falls below `min_version`. Guards features that
#' need recent Stan compiler additions, such as `sum_to_zero_vector[K]`
#' for the simplex families.
#'
#' @param backend One of `"rstan"` or `"cmdstanr"`. The `"mock"`
#'   backend is rejected because it does not run a real Stan compiler.
#' @param min_version Minimum Stan engine version required (e.g.
#'   `"2.36.0"`). Compared via `numeric_version()`.
#' @param feature Optional short description of the feature that needs
#'   the version. The error message names it.
#' @return Invisible NULL; errors if the engine version is too old.
#' @noRd
assert_stan_version <- function(backend, min_version, feature = NULL) {
  checkmate::assert_choice(backend, c("rstan", "cmdstanr"))
  checkmate::assert_string(min_version, min.chars = 1L)
  checkmate::assert_string(feature, null.ok = TRUE)
  required <- numeric_version(min_version)
  live <- live_stan_version(backend)
  if (is.na(live)) {
    stop(insight::format_error(c(
      "CmdStan is not installed.",
      i = "Install it with `cmdstanr::install_cmdstan()`."
    )), call. = FALSE)
  }
  stan_v <- numeric_version(live)
  if (stan_v >= required) return(invisible(NULL))
  upgrade <- if (identical(backend, "cmdstanr")) {
    paste0(
      "Upgrade with `cmdstanr::install_cmdstan(version = '",
      required, "')` or newer."
    )
  } else {
    paste0(
      "Upgrade with `install.packages('rstan', ",
      "repos = c('https://stan-dev.r-universe.dev', getOption('repos')))` ",
      "to a build that ships Stan >= ", required, "."
    )
  }
  feature_clause <- if (is.null(feature)) {
    ""
  } else {
    paste0(" (", feature, " requires this version)")
  }
  stop(insight::format_error(c(
    paste0(
      "Stan >= ", required, " is required", feature_clause, "."
    ),
    x = paste0("Detected Stan ", stan_v, " via backend '", backend, "'."),
    i = upgrade
  )), call. = FALSE)
}

#' Supported Stan algorithms
#'
#' @description
#' Returns vector of supported Stan sampling algorithms.
#'
#' @return Character vector of algorithm names
#' @noRd
algorithm_choices <- function() {
  c("sampling", "meanfield", "fullrank", "pathfinder", "laplace",
    "fixed_param")
}

#' Algorithms implemented by a backend
#'
#' @description
#' Records which entries of `algorithm_choices()` a backend can run.
#' `validate_algorithm()` and `validate_init()` both use it. `rstan`
#' has no Pathfinder or Laplace method. Setup refuses a request for
#' either, before the model is parsed and compiled.
#'
#' @param backend One of `backend_choices()`
#' @return Character vector of algorithm names
#' @noRd
backend_algorithms <- function(backend) {
  checkmate::assert_choice(backend, backend_choices())
  if (identical(backend, "rstan")) {
    return(setdiff(algorithm_choices(), c("pathfinder", "laplace")))
  }
  algorithm_choices()
}

#' Validate the requested sampling algorithm
#'
#' @param algorithm Requested algorithm name
#' @param backend One of `backend_choices()`
#' @return The validated `algorithm`, unchanged
#' @noRd
validate_algorithm <- function(algorithm, backend) {
  checkmate::assert_string(algorithm)
  supported <- backend_algorithms(backend)
  if (algorithm %in% supported) {
    return(algorithm)
  }
  stop(insight::format_error(c(
    paste0("Argument 'algorithm = \"", algorithm, "\"' is not available."),
    x = paste0(
      "Backend '", backend, "' supports: ",
      paste(supported, collapse = ", "), "."
    ),
    i = if (algorithm %in% algorithm_choices()) {
      "Set 'backend = \"cmdstanr\"' to use this algorithm."
    } else {
      "See '?mvgam' for the algorithms mvgam can pass to Stan."
    }
  )), call. = FALSE)
}

#' Whether an object is a brmsthreads object
#' @description
#' Tests if an object has class brmsthreads.
#' @param x Object to test
#' @return Logical indicating if x is a brmsthreads object
#' @noRd
is.brmsthreads <- function(x) {
  inherits(x, "brmsthreads")
}

#' Number of chains to sample at once
#'
#' `getOption("mc.cores")` when set, as in brms. Otherwise one fewer
#' than the machine's cores, which keeps one free. The count never
#' exceeds the number of chains.
#'
#' @param chains Number of chains
#' @return Positive integer
#' @noRd
default_cores <- function(chains) {
  checkmate::assert_int(chains, lower = 1L)
  set <- getOption("mc.cores")
  if (!is.null(set)) return(as.integer(set))
  available <- parallel::detectCores()
  if (is.na(available)) available <- 1L
  as.integer(max(1L, min(chains, available - 1L)))
}

#' Validate the threading argument
#' @description
#' Validates and normalizes the threads argument for Stan threading.
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
      paste0("Argument 'threads' needs to be numeric or specified via ",
             "the 'threading' function.")
    ), call. = FALSE)
  }
  threads
}

#' The number of threads per chain a `threads` argument asks for
#'
#' @inheritParams validate_threads
#' @return Integer, 1 for `NULL` or a `brms::threading()` object without
#'   a count
#' @noRd
thread_count <- function(threads) {
  as.integer(validate_threads(threads)$threads %||% 1L)
}

#' Whether Stan threading is active
#' @description
#' Determines whether Stan threading should be used based on configuration.
#' @param threads Threading configuration object
#' @param force Logical; use threading even when the threads object
#'   sets `force`?
#' @return Logical indicating if threading should be used
#' @noRd
use_threading <- function(threads, force = FALSE) {
  checkmate::assert_flag(force)
  threads <- validate_threads(threads)
  out <- isTRUE(threads$threads > 0)
  if (!force) {
    # Stan code changes only in non-forced mode
    out <- out && !isTRUE(threads$force)
  }
  out
}

#' Whether an object is a brmsopencl object
#' @description
#' Tests if an object has class brmsopencl.
#' @param x Object to test
#' @return Logical indicating if x is a brmsopencl object
#' @noRd
is.brmsopencl <- function(x) {
  inherits(x, "brmsopencl")
}

#' Validate the OpenCL argument
#' @description
#' Validates and normalizes the opencl argument for Stan OpenCL support.
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
      paste0("Argument 'opencl' needs to be an integer vector or ",
             "specified via the 'opencl' function.")
    ), call. = FALSE)
  }
  opencl
}

#' Whether Stan OpenCL is active
#' @description
#' Determines whether Stan OpenCL should be used based on configuration.
#' @param opencl OpenCL configuration object
#' @return Logical indicating if OpenCL should be used
#' @noRd
use_opencl <- function(opencl) {
  !is.null(validate_opencl(opencl)$ids)
}

#' Validate the silent argument
#' @description
#' Validates the silent argument for controlling output verbosity.
#' @param silent Integer from 0 to 2 controlling verbosity level
#' @return Validated silent value
#' @noRd
validate_silent <- function(silent) {
  checkmate::assert_int(silent, lower = 0, upper = 2)
  silent
}

#' A stanfit from CmdStan output, without Pathfinder's `path__`
#'
#' cmdstanr 0.9.0 writes a `path__` column to Pathfinder output and
#' lists it among `$metadata()$stan_variables`. brms 2.23.0 knows the
#' `lp__` and `lp_approx__` columns of each algorithm. It lacks this
#' one. Passed through as a variable, `path__` makes
#' `read_cmdstan_csv()` stop. Left out, cmdstanr still returns the
#' column in the draws. brms then names it in `fnames_oi` with no
#' dimension entry, which shifts every name after it. The column
#' records which path a draw came from. It is removed at both points.
#'
#' @inheritParams brms::read_csv_as_stanfit
#' @return A `stanfit` object.
#' @noRd
read_cmdstan_stanfit <- function(files, variables, model, exclude,
                                 algorithm) {
  checkmate::assert_character(files, min.len = 1L)
  checkmate::assert_character(variables)
  checkmate::assert_string(algorithm)
  out <- brms::read_csv_as_stanfit(
    files, variables = setdiff(variables, "path__"),
    model = model, exclude = exclude, algorithm = algorithm
  )
  if (!"path__" %in% out@sim$fnames_oi) {
    return(out)
  }
  out@sim$samples <- lapply(out@sim$samples, function(chain) {
    chain$path__ <- NULL
    means <- attr(chain, "mean_pars")
    attr(chain, "mean_pars") <- means[names(means) != "path__"]
    chain
  })
  out@sim$fnames_oi <- setdiff(out@sim$fnames_oi, "path__")
  out@sim$n_flatnames <- length(out@sim$fnames_oi)
  out
}


#' Refuse sampler arguments the cmdstanr method lacks
#'
#' Called with argument names `mvgam()` forwards to the sampler. rstan
#' names an unknown argument itself. cmdstanr reports only an R
#' `unused argument` error against a renamed placeholder.
#'
#' @param model A compiled `CmdStanModel`.
#' @param algorithm The validated algorithm.
#' @param arg_names Names of the forwarded arguments.
#' @return `NULL`, invisibly.
#' @noRd
refuse_unknown_cmdstanr_args <- function(model, algorithm, arg_names) {
  checkmate::assert_string(algorithm)
  checkmate::assert_character(arg_names, null.ok = TRUE)
  method <- switch(algorithm,
    sampling = , fixed_param = "sample",
    meanfield = , fullrank = "variational",
    algorithm
  )
  refuse_unread_dots(
    stats::setNames(vector("list", length(arg_names)), arg_names),
    names(formals(model[[method]])), "mvgam"
  )
  invisible(NULL)
}


#' The arguments the samplers of both backends take
#'
#' cmdstanr's `$sample()` declares its arguments as formals of the
#' `CmdStanModel` class, taken from the class when cmdstanr is
#' installed. rstan's `sampling()` method declares only `...`. Its
#' documented arguments are listed here.
#'
#' @return Character vector of argument names
#' @noRd
sampler_arg_names <- function() {
  rstan_args <- c(
    "pars", "include", "chains", "iter", "warmup", "thin", "seed",
    "init", "check_data", "sample_file", "diagnostic_file", "verbose",
    "algorithm", "control", "cores", "open_progress", "show_messages",
    "chain_id", "init_r", "test_grad", "append_samples", "refresh",
    "save_warmup", "enable_random_init"
  )
  cmdstanr_args <- if (requireNamespace("cmdstanr", quietly = TRUE)) {
    model_class <- utils::getFromNamespace("CmdStanModel", "cmdstanr")
    names(formals(model_class$public_methods$sample))
  }
  setdiff(unique(c(rstan_args, cmdstanr_args)), "...")
}


#' Run Stan's Pathfinder on a compiled cmdstanr model
#' @description
#' Shared by the `algorithm = "pathfinder"` fitting branch and the
#' `init = "pathfinder"` starting-value branch. Both reach Stan with
#' the same argument set.
#' @param model Compiled `CmdStanModel` object
#' @param args Argument list already assembled for the sampler; entries
#'   here take precedence over the defaults added below
#' @param chains Number of chains, used as the number of paths
#' @param threading_on Logical; is within-chain threading active?
#' @param threads Validated `brmsthreads` object, or `NULL`
#' @param starts Whether the draws start the sampler's chains
#' @return A `CmdStanPathfinder` object
#' @noRd
run_pathfinder <- function(model, args, chains, threading_on, threads,
                           starts = FALSE) {
  checkmate::assert_list(args)
  checkmate::assert_count(chains, positive = TRUE)
  checkmate::assert_flag(threading_on)
  checkmate::assert_flag(starts)
  defaults <- list(num_paths = chains)
  # Starting values take draws from the approximation itself. An
  # importance resample of it can collapse onto too few distinct
  # draws to start every chain, which cmdstanr refuses.
  if (starts) {
    defaults$psis_resample <- FALSE
  }
  if (threading_on) {
    defaults$num_threads <- threads$threads
  }
  # Fill in only the names the caller did not already supply. A value
  # passed through `...` then wins. `do_call()` rejects a name that
  # appears twice in `args`.
  missing_names <- setdiff(names(defaults), names(args))
  args[missing_names] <- defaults[missing_names]
  # A Pathfinder start receives the arguments assembled for
  # `$sample()`. Those Pathfinder lacks, such as `save_warmup`, stay
  # with the sampler. cmdstanr's methods name every argument they take.
  accepted <- names(formals(model$pathfinder))
  if (!"..." %in% accepted) {
    args <- args[names(args) %in% accepted]
  }
  # Start the quasi-Newton search in a tight ball around the
  # unconstrained origin unless the caller asked for something else.
  # Reason: from Stan's default U(-2, 2), a few hundred non-centred
  # innovations put the initial log density near -1e11. The line
  # search then fails before it can move. From exactly zero, the VAR
  # coefficient matrix and its correlation factor start on a flat
  # surface and fail the same way. Across four trend types and three
  # seeds each, U(-2, 2) and 0 both converged 9 times in 12. Every
  # jitter from 0.1 to 1 converged 12 in 12.
  if (is.null(args$init)) {
    args$init <- 0.2
  }
  out <- brms::do_call(model$pathfinder, args)
  if (any(out$return_codes() != 0)) {
    stop(insight::format_error(c(
      "Stan's Pathfinder algorithm did not converge.",
      x = "No path finished: the run returned no draws.",
      i = paste0(
        "Try a different 'seed', supply 'init' explicitly or fit with ",
        "'init = \"random\"'."
      )
    )), call. = FALSE)
  }
  out
}

#' Validate the requested iteration counts
#' @description
#' `iter` counts warmup and sampling together. A `warmup` at or above
#' it leaves nothing to sample. The backend would compute a negative
#' sampling count and fail inside cmdstanr with a message that names
#' neither argument.
#' @param iter Total iterations per chain
#' @param warmup Warmup iterations per chain, or `NULL` to accept the
#'   `iter %/% 2` default
#' @return Invisible `NULL`
#' @noRd
validate_sampler_iterations <- function(iter, warmup = NULL) {
  checkmate::assert_int(iter, lower = 1L)
  if (is.null(warmup)) {
    return(invisible(NULL))
  }
  checkmate::assert_int(warmup, lower = 0L)
  if (warmup < iter) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    "Argument 'warmup' must be smaller than 'iter'.",
    x = paste0("Got 'warmup' = ", warmup, " and 'iter' = ", iter, "."),
    x = "'iter' counts warmup and sampling together.",
    i = paste0("Raise 'iter' above ", warmup, " or lower 'warmup'.")
  )), call. = FALSE)
}

#' Starting values for the coefficients of a sparse-lag AR trend
#'
#' A contiguous lag set is sampled as partial autocorrelations, which
#' keeps every draw stationary. A sparse set such as `p = c(1, 12)` is
#' bounded one coefficient at a time. A random start can place a chain
#' on an explosive recursion. Its latent path then grows without
#' limit. The chain stays at that start. The coefficients and their
#' hierarchical means start near zero here. Stan draws every other
#' parameter as it would.
#'
#' @param stancode The assembled Stan program
#' @param standata The Stan data, which sizes the coefficient vectors
#' @param chains Number of chains
#' @param seed Seed the starting values are drawn under
#' @return A list of per-chain init lists marked `mvgam_partial`, or
#'   `NULL` when the model has no sparse lag set
#' @noRd
sparse_ar_inits <- function(stancode, standata, chains, seed) {
  checkmate::assert_string(stancode)
  checkmate::assert_list(standata)
  checkmate::assert_count(chains, positive = TRUE)
  pars <- grep("^(shared_|mu_)?ar[0-9]+_trend$",
               brms_declared_params(stancode), value = TRUE)
  lags <- sort(unique(as.integer(sub("^\\D*ar([0-9]+)_trend$", "\\1",
                                     pars))))
  if (length(lags) < 2L || ar_lags_contiguous(lags)) return(NULL)
  # A hierarchical mean is one value and a shared coefficient a
  # length-one vector. Every other coefficient has one value per
  # process.
  sizes <- ifelse(startsWith(pars, "mu_"), NA_integer_,
                  ifelse(startsWith(pars, "shared_"), 1L,
                         as.integer(standata$N_lv_trend)))
  local_seed(seed)
  out <- lapply(seq_len(chains), function(chain) {
    stats::setNames(lapply(sizes, function(n) {
      # `array()` keeps a length-one vector a vector in Stan's JSON.
      if (is.na(n)) stats::runif(1L, -0.1, 0.1) else
        array(stats::runif(n, -0.1, 0.1))
    }), pars)
  })
  attr(out, "mvgam_partial") <- TRUE
  out
}

#' Validate the initial value specification
#' @description
#' Checks the `init` argument against the capabilities of the chosen
#' backend. Only the `"pathfinder"` keyword is backend-specific. Every
#' other value passes through untouched. The numeric, list and function
#' forms inherited from brms then keep working.
#' @param init Initial value specification supplied by the user
#' @param backend One of `"rstan"`, `"cmdstanr"` or `"mock"`
#' @return The validated `init`, unchanged
#' @noRd
validate_init <- function(init, backend) {
  checkmate::assert_choice(backend, backend_choices())
  if (!is_equal(init, "pathfinder")) {
    return(init)
  }
  if (!"pathfinder" %in% backend_algorithms(backend)) {
    stop(insight::format_error(c(
      "Argument 'init = \"pathfinder\"' is not available.",
      x = paste0("Backend '", backend, "' does not implement Pathfinder."),
      i = paste0(
        "Set 'backend = \"cmdstanr\"', or supply 'init' as \"random\", ",
        "\"0\", a numeric value, a list or a function."
      )
    )), call. = FALSE)
  }
  init
}

#' Repair parameter names in stanfit objects
#' @description
#' Ensures parameter names in stanfit objects are unique and properly formatted
#' for compatibility with the posterior package.
#' @param x stanfit object to repair
#' @return stanfit object with repaired parameter names
#' @noRd
repair_stanfit <- function(x) {
  checkmate::assert_class(x, "stanfit")
  if (!length(x@sim$fnames_oi)) {
    # Nothing to rename
    return(x)
  }
  # The posterior package requires unique parameter names. Duplicates
  # are rare. `sample_prior = "yes"` can produce them.
  x@sim$fnames_oi <- make.unique(as.character(x@sim$fnames_oi), "__")
  for (i in seq_along(x@sim$samples)) {
    # stanfit may rename dimension suffixes
    if (length(x@sim$samples[[i]]) == length(x@sim$fnames_oi)) {
      names(x@sim$samples[[i]]) <- x@sim$fnames_oi
    }
  }
  x
}

#' The sampler arguments each chain recorded
#'
#' `brms::read_csv_as_stanfit()` records an argument the method never
#' takes as a zero-length value. A variational fit holds
#' `iter = integer(0)`, for one. Those entries are dropped here.
#'
#' @param fit A `stanfit`, or any other fit object
#' @return List with one named list of recorded arguments per chain,
#'   or `NULL` when the fit has no `stan_args` slot or the slot is
#'   empty
#' @noRd
recorded_stan_args <- function(fit) {
  if (!isS4(fit) || !"stan_args" %in% methods::slotNames(fit)) {
    return(NULL)
  }
  args <- methods::slot(fit, "stan_args")
  if (length(args) == 0L) {
    return(NULL)
  }
  lapply(args, function(chain) chain[lengths(chain) > 0L])
}



#' How many chains does a fitted Stan model hold?
#'
#' Both backends hand back a `stanfit`: the rstan path through
#' `repair_stanfit()` and the cmdstanr path through
#' `brms::read_csv_as_stanfit()`. Its `stan_args` slot holds one entry
#' per chain that finished, which is the count every later summary is
#' computed from. The draws are the fallback for a fit carrying no
#' `stan_args`, such as a stub built from a draws array.
#'
#' `stan_args` has precedence because it costs constant time. The draws
#' route converts `lp__` alone. Both count the chains that finished.
#' They agree on every cached fixture.
#'
#' @param fit A fitted Stan model, or a draws object
#' @return Integer count of chains, or `NA_integer_` when neither
#'   route yields one
#' @noRd
realised_chain_count <- function(fit) {
  n <- length(recorded_stan_args(fit))
  if (n > 0L) {
    return(n)
  }
  if (is.stanfit(fit) || inherits(fit, "draws")) {
    return(as.integer(posterior::nchains(fit_draws_shape(fit))))
  }
  NA_integer_
}


#' Say when fewer chains finished than were requested
#'
#' Stan hands back the chains that finished. The fit records no
#' requested count. A chain lost to a failed start or an exception
#' leaves a smaller posterior without a message. Every later summary
#' uses the smaller number. The notice is raised once per call.
#' `silent` leaves it in place: a quiet fit is a request about progress
#' output. A lost part of the posterior is a different matter.
#'
#' @param fit The fitted Stan model the backend returned
#' @param requested Chain count the caller asked for
#' @param algorithm The algorithm that ran
#' @return `invisible(TRUE)`
#' @noRd
check_chains_finished <- function(fit, requested, algorithm) {
  checkmate::assert_count(requested, positive = TRUE)
  checkmate::assert_string(algorithm)
  # `fixed_param` runs chains in the ordinary way and holds only the
  # parameters still. Its chains count here. The approximations have
  # no chains. "pathfinder" spends the same argument on its number of
  # paths.
  if (!algorithm %in% c("sampling", "fixed_param")) {
    return(invisible(TRUE))
  }
  finished <- realised_chain_count(fit)
  if (is.na(finished) || finished >= requested) {
    return(invisible(TRUE))
  }
  # `insight::format_warning()` raises the warning while formatting
  # it. The returned string is discarded here.
  insight::format_warning(c(
    "Fewer chains finished than were requested.",
    x = paste0(
      "Asked for ", requested, " chains and ", finished, " finished."
    ),
    x = paste0("Every summary of this model uses the ", finished,
               " chains that finished."),
    i = "Refit to recover the full posterior."
  ))
  invisible(TRUE)
}


#' Unlisted `lapply()` output
#' @description
#' Convenience function to unlist lapply results.
#' @param X A vector (atomic or list)
#' @param FUN Function to apply to each element
#' @param ... Additional arguments to FUN
#' @param recursive Logical; should unlisting be applied recursively?
#' @param use.names Logical; should names be preserved?
#' @return Unlisted result of lapply
#' @noRd
ulapply <- function(X, FUN, ..., recursive = TRUE, use.names = TRUE) {
  checkmate::assert_function(FUN)
  unlist(lapply(X, FUN, ...), recursive, use.names)
}

#' Equality with tolerance
#' @description
#' Tests for equality using all.equal with default tolerance.
#' @param x First object to compare
#' @param y Second object to compare
#' @param check.attributes Logical; should attributes be checked?
#' @param ... Additional arguments passed to all.equal
#' @return Logical indicating if objects are equal
#' @noRd
is_equal <- function(x, y, check.attributes = FALSE, ...) {
  isTRUE(all.equal(x, y, check.attributes = check.attributes, ...))
}

#' Whether an object is a single NA value
#' @description
#' Tests if an object is a single NA value.
#' @param x Object to test
#' @return Logical indicating if x is a single NA
#' @noRd
is_NA <- function(x) {
  length(x) == 1L && is.na(x)
}


#' Whether `future = TRUE` can run through the future package
#'
#' Without the package the caller runs serially. The samplers run their
#' chains through `cores`. The other callers use `lapply()`. The user
#' is told once per session.
#'
#' @return A single logical
#' @noRd
future_available <- function() {
  ok <- requireNamespace("future", quietly = TRUE)
  if (!ok) {
    inform_once(
      c("'future = TRUE' requires the 'future' package.",
        i = "The computation runs serially."),
      "mvgam_future_missing"
    )
  }
  ok
}
