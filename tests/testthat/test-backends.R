# CI tests for the Stan backend layer in R/backends.R. Everything here
# runs without a Stan installation: the algorithm / init validators and
# the Stan-version gate are pure functions of their arguments, and the
# reporting helpers operate on plain lists. Fitting behaviour that needs
# a compiled model lives in `tests/local/test-pathfinder-init.R`.


# ---------------------------------------------------------------
# Backend capability table
# ---------------------------------------------------------------

test_that("backend_algorithms() withholds cmdstanr-only algorithms from rstan", {
  cmdstanr_algs <- mvgam:::backend_algorithms("cmdstanr")
  rstan_algs <- mvgam:::backend_algorithms("rstan")

  expect_identical(cmdstanr_algs, mvgam:::algorithm_choices())
  expect_true(all(c("pathfinder", "laplace") %in% cmdstanr_algs))
  expect_false(any(c("pathfinder", "laplace") %in% rstan_algs))
  # Everything rstan does support is still offered
  expect_identical(rstan_algs, c("sampling", "meanfield", "fullrank",
                                 "fixed_param"))
})

test_that("backend_algorithms() rejects an unknown backend", {
  expect_error(mvgam:::backend_algorithms("jags"), "backend")
})


# ---------------------------------------------------------------
# validate_algorithm()
# ---------------------------------------------------------------

test_that("validate_algorithm() passes supported algorithms through unchanged", {
  for (alg in mvgam:::backend_algorithms("cmdstanr")) {
    expect_identical(mvgam:::validate_algorithm(alg, "cmdstanr"), alg)
  }
  for (alg in mvgam:::backend_algorithms("rstan")) {
    expect_identical(mvgam:::validate_algorithm(alg, "rstan"), alg)
  }
})

test_that("validate_algorithm() rejects cmdstanr-only algorithms under rstan", {
  # The message must name the backend and point at the fix, because the
  # alternative is a failure after parsing and compiling the model
  expect_error(
    mvgam:::validate_algorithm("pathfinder", "rstan"),
    "not available"
  )
  expect_error(
    mvgam:::validate_algorithm("laplace", "rstan"),
    "cmdstanr"
  )
})

test_that("validate_algorithm() rejects an algorithm Stan does not have", {
  expect_error(
    mvgam:::validate_algorithm("nonsense", "cmdstanr"),
    "not available"
  )
})


# ---------------------------------------------------------------
# validate_init()
# ---------------------------------------------------------------

test_that("validate_init() passes every non-keyword specification through", {
  fn <- function() list(Intercept = 0)
  lst <- list(list(Intercept = 0), list(Intercept = 1))

  expect_identical(mvgam:::validate_init("random", "cmdstanr"), "random")
  expect_identical(mvgam:::validate_init("0", "cmdstanr"), "0")
  expect_identical(mvgam:::validate_init(0.5, "cmdstanr"), 0.5)
  expect_identical(mvgam:::validate_init(lst, "cmdstanr"), lst)
  expect_identical(mvgam:::validate_init(fn, "cmdstanr"), fn)
  # rstan resolves a bare character init as a function name, so that
  # form must survive the validator untouched
  expect_identical(mvgam:::validate_init("my_inits", "rstan"), "my_inits")
})

test_that("validate_init() accepts the pathfinder keyword only for cmdstanr", {
  expect_identical(
    mvgam:::validate_init("pathfinder", "cmdstanr"), "pathfinder"
  )
  expect_error(
    mvgam:::validate_init("pathfinder", "rstan"),
    "not available"
  )
  # The error should offer the alternatives rather than just refusing
  expect_error(mvgam:::validate_init("pathfinder", "rstan"), "cmdstanr")
})

test_that("validate_init() and validate_algorithm() agree on Pathfinder", {
  # Both read the same capability table, so a backend can never accept
  # one spelling of the request and refuse the other
  expect_identical(
    mvgam:::validate_algorithm("pathfinder", "cmdstanr"), "pathfinder"
  )
  expect_identical(
    mvgam:::validate_init("pathfinder", "cmdstanr"), "pathfinder"
  )
  expect_error(mvgam:::validate_algorithm("pathfinder", "rstan"))
  expect_error(mvgam:::validate_init("pathfinder", "rstan"))
})


# ---------------------------------------------------------------
# run_pathfinder()
# ---------------------------------------------------------------

# A stand-in for the compiled CmdStanModel: `$pathfinder()` records
# whatever argument list it was handed so the test can inspect it
# without a Stan installation.
fake_pathfinder_model <- function(codes = 0L) {
  list(pathfinder = function(...) {
    out <- list(...)
    out$return_codes <- function() codes
    out
  })
}

test_that("run_pathfinder() supplies one path per chain", {
  got <- mvgam:::run_pathfinder(
    fake_pathfinder_model(),
    args = list(data = list(N = 1), seed = 2, init = NULL),
    chains = 4, threading_on = FALSE, threads = NULL, silent = 2
  )
  expect_identical(got$num_paths, 4)
  expect_false(got$show_messages)
  expect_identical(got$data, list(N = 1))
  expect_identical(got$seed, 2)
})

test_that("run_pathfinder() lets caller-supplied arguments win", {
  # Duplicated names are not a matter of precedence: do_call() rejects
  # them outright with "matched by multiple actual arguments", so the
  # defaults must be filled in rather than appended.
  got <- mvgam:::run_pathfinder(
    fake_pathfinder_model(),
    args = list(data = list(N = 1), seed = 2, init = NULL,
                num_paths = 10, show_messages = TRUE),
    chains = 4, threading_on = FALSE, threads = NULL, silent = 2
  )
  expect_identical(got$num_paths, 10)
  expect_true(got$show_messages)
  expect_equal(anyDuplicated(names(got)), 0L)
})

test_that("run_pathfinder() starts the search in a tight ball near zero", {
  # Neither extreme is safe. Stan's default U(-2, 2) start fails on
  # models carrying many non-centred innovations, and exactly zero
  # fails on VAR trends whose coefficient matrix starts flat there.
  # Both converged 9 times in 12 across four trend types; every jitter
  # between 0.1 and 1 converged 12 in 12.
  got <- mvgam:::run_pathfinder(
    fake_pathfinder_model(),
    args = list(data = list(N = 1), seed = 2, init = NULL),
    chains = 2, threading_on = FALSE, threads = NULL, silent = 2
  )
  expect_true(got$init > 0 && got$init < 1)

  # An explicit request still wins
  explicit <- mvgam:::run_pathfinder(
    fake_pathfinder_model(),
    args = list(data = list(N = 1), seed = 2, init = 0.5),
    chains = 2, threading_on = FALSE, threads = NULL, silent = 2
  )
  expect_identical(explicit$init, 0.5)
})

test_that("run_pathfinder() reports a failed approximation rather than passing it on", {
  # cmdstanr returns an object whose metadata cannot be read when no
  # path converges. Handing that to the sampler produces "Unable to
  # retrieve the metadata", which tells the user nothing.
  expect_error(
    mvgam:::run_pathfinder(
      fake_pathfinder_model(codes = 1L),
      args = list(data = list(N = 1), seed = 2, init = NULL),
      chains = 2, threading_on = FALSE, threads = NULL, silent = 2
    ),
    "did not converge"
  )
  expect_error(
    mvgam:::run_pathfinder(
      fake_pathfinder_model(codes = c(0L, 1L)),
      args = list(data = list(N = 1), seed = 2, init = NULL),
      chains = 2, threading_on = FALSE, threads = NULL, silent = 2
    ),
    "seed"
  )
})

test_that("run_pathfinder() forwards threads only when threading is on", {
  args <- list(data = list(N = 1), seed = 2, init = NULL)
  off <- mvgam:::run_pathfinder(
    fake_pathfinder_model(), args, chains = 2,
    threading_on = FALSE, threads = NULL, silent = 2
  )
  expect_false("num_threads" %in% names(off))

  on <- mvgam:::run_pathfinder(
    fake_pathfinder_model(), args, chains = 2,
    threading_on = TRUE, threads = list(threads = 3L), silent = 2
  )
  expect_identical(on$num_threads, 3L)
})


# ---------------------------------------------------------------
# validate_sampler_iterations()
# ---------------------------------------------------------------

test_that("validate_sampler_iterations() accepts workable counts", {
  expect_null(mvgam:::validate_sampler_iterations(1000L, 500L))
  expect_null(mvgam:::validate_sampler_iterations(10L, 0L))
  # NULL warmup means the caller will take the iter %/% 2 default,
  # which can never exceed iter
  expect_null(mvgam:::validate_sampler_iterations(2000L, NULL))
  expect_null(mvgam:::validate_sampler_iterations(1L))
})

test_that("validate_sampler_iterations() refuses a warmup that leaves nothing to sample", {
  # `iter` counts warmup and sampling together. Unchecked, this reaches
  # cmdstanr as a negative sampling count and fails with a message that
  # names neither argument.
  expect_error(mvgam:::validate_sampler_iterations(400L, 750L),
               "smaller than", fixed = TRUE)
  expect_error(mvgam:::validate_sampler_iterations(400L, 750L),
               "750", fixed = TRUE)
  expect_error(mvgam:::validate_sampler_iterations(400L, 750L),
               "400", fixed = TRUE)
  # equality leaves zero sampling iterations, which is also useless
  expect_error(mvgam:::validate_sampler_iterations(500L, 500L),
               "smaller than", fixed = TRUE)
})


# ---------------------------------------------------------------
# printable_init()
# ---------------------------------------------------------------

test_that("printable_init() keeps keywords and drops unprintable starts", {
  expect_identical(mvgam:::printable_init("pathfinder"), "pathfinder")
  expect_identical(mvgam:::printable_init("0"), "0")
  # "random" is the default, so echoing it back adds nothing
  expect_identical(mvgam:::printable_init("random"), NA_character_)
  expect_identical(mvgam:::printable_init(NULL), NA_character_)
  expect_identical(mvgam:::printable_init(""), NA_character_)
  expect_identical(mvgam:::printable_init(0), NA_character_)
  expect_identical(mvgam:::printable_init(list(list(a = 1))), NA_character_)
  expect_identical(
    mvgam:::printable_init(list(list(a = 1), list(a = 2))), NA_character_
  )
})

test_that("extract_implementation_info() prefers the stored init request", {
  # Stan records a temporary file path when starting values are written
  # to disk, which is what a Pathfinder or list init produces. That path
  # is machine-local, so the reproduction call must show what the user
  # asked for instead.
  stub <- structure(
    list(
      fit = NULL,
      backend = "cmdstanr",
      algorithm = "sampling",
      init = "pathfinder"
    ),
    class = "mvgam"
  )
  info <- mvgam:::extract_implementation_info(stub)
  expect_identical(info$init, "pathfinder")

  stub$init <- list(list(Intercept = 0))
  expect_identical(mvgam:::extract_implementation_info(stub)$init,
                   NA_character_)

  stub$init <- "random"
  expect_identical(mvgam:::extract_implementation_info(stub)$init,
                   NA_character_)
})


# ---------------------------------------------------------------
# assert_stan_version()
# ---------------------------------------------------------------

test_that("assert_stan_version() passes when the engine is new enough", {
  # 2.19 predates every Stan build either backend can ship
  expect_silent(mvgam:::assert_stan_version("rstan", "2.19.0"))
  expect_null(mvgam:::assert_stan_version("rstan", "2.19.0"))
})

test_that("assert_stan_version() reports version, backend and remedy", {
  # A user hitting this needs to know what they have, what they need and
  # how to close the gap, so assert on all three
  expect_error(
    mvgam:::assert_stan_version("rstan", "99.0.0", feature = "'widget_lpdf'"),
    "99.0.0", fixed = TRUE
  )
  expect_error(
    mvgam:::assert_stan_version("rstan", "99.0.0", feature = "'widget_lpdf'"),
    "widget_lpdf", fixed = TRUE
  )
  expect_error(
    mvgam:::assert_stan_version("rstan", "99.0.0"),
    "Detected Stan", fixed = TRUE
  )
  expect_error(
    mvgam:::assert_stan_version("rstan", "99.0.0"), "install.packages",
    fixed = TRUE
  )
})

test_that("live_stan_version() reports Stan, not the package calling it", {
  expect_identical(mvgam:::live_stan_version("rstan"),
                   as.character(rstan::stan_version()))
  # The mock backend runs no compiler and has no version to report.
  expect_identical(mvgam:::live_stan_version("mock"), NA_character_)
})

test_that("assert_stan_version() rejects the mock backend", {
  # The mock backend runs no Stan compiler, so a version claim about it
  # would be meaningless
  expect_error(mvgam:::assert_stan_version("mock", "2.26.0"), "backend")
})
