# Setup models for tests
library("testthat")
library("mvgam")

expect_character <- function(object, ...) {
  testthat::expect_true(is(object, "character"), ...)
}

expect_list <- function(object, ...) {
  testthat::expect_true(is(object, "list"), ...)
}

expect_ggplot <- function(object, ...) {
  testthat::expect_true(is(object, "ggplot"), ...)
}

expect_loo <- function(object, ...) {
  testthat::expect_true(is(object, "psis_loo"), ...)
}

expect_range <- function(object, lower = -Inf, upper = Inf, ...) {
  testthat::expect_true(all(object >= lower & object <= upper), ...)
}

SM <- suppressMessages
SW <- suppressWarnings

# A warning raised with `rlang::warn(.frequency = "once")` is shown
# once per R session. A second `devtools::test()` in one session
# meets a cache already set. An `expect_warning()` on such a warning
# then fails where the first run passed. A failing expectation still
# counts one, leaving the suite total unchanged. Under `"verbose"`
# rlang ignores the frequency cache. The scope is the calling test
# alone. Every other test keeps the cache.
local_verbose_warnings <- function(.env = parent.frame()) {
  withr::local_options(
    rlib_warning_verbosity = "verbose",
    .local_envir = .env
  )
}

# The condition an expression raises, or NULL for one that raises
# none. `expect_error()` asserts a raise. These callers compare one
# behaviour against another and need it as a value.
caught_error <- function(expr) {
  rlang::catch_cnd(expr, classes = "error")
}

# bayesplot's colour scheme is one global. A test that sets it and
# stops there gives every later test that scheme. Restoring a
# literal name assumes the session began with it. The package's own
# `restore_color_scheme()` puts back whatever was there. One
# definition covers the tests and the figures.
local_color_scheme <- function(scheme, .env = parent.frame()) {
  prior <- bayesplot::color_scheme_get()
  bayesplot::color_scheme_set(scheme)
  withr::defer(mvgam:::restore_color_scheme(prior), envir = .env)
  invisible(prior)
}

# How many lines of a Stan program open a named block. The programs
# are one string with embedded newlines. On a miss,
# `gregexpr(pattern, code)[[1]]` is `-1L`. Its length is 1, matching
# the length a single hit gives. `^` on that one string reaches only
# the first line. A count of 1 passed on a program missing the block
# entirely.
stan_block_count <- function(code, block) {
  lines <- strsplit(
    paste(as.character(code), collapse = "\n"), "\n", fixed = TRUE
  )[[1]]
  sum(grepl(paste0("^\\s*", block, "\\s*\\{"), lines))
}


# The six blocks every assembled mvgam program declares once.
STAN_BLOCKS <- c(
  "data", "transformed data", "parameters",
  "transformed parameters", "model", "generated quantities"
)


# How many times a pattern matches the whole program. `gregexpr()`
# gives `-1L` for no match, whose length is 1, matching the length
# one hit gives. A count taken from that length reported a missing
# declaration as a single one.
stan_match_count <- function(code, pattern) {
  m <- gregexpr(pattern, paste(as.character(code), collapse = "\n"))[[1]]
  if (identical(as.integer(m), -1L)) 0L else length(m)
}


# Stan code and Stan data from one
# `generate_stan_components_mvgam_formula()` call. `stancode()` and
# `standata()` each run the whole pipeline. Each starts a V8 isolate
# to polish the code. A test needing both calls this once.
mvgam_stan_setup <- function(formula, data, family = gaussian(), ...) {
  cc <- mvgam:::generate_stan_components_mvgam_formula(
    formula = formula, data = data, family = family, ...
  )
  code <- cc$combined_components$stancode
  class(code) <- c("mvgamstancode", "stancode", "character")
  list(code = code, data = cc$combined_components$standata)
}


# The statement an emitter writes for one prior, built by the
# package's own writer. Assertions then match the form the program
# holds. `test-stancode-standata.R` pins the writer's output
# directly. `stanc` compiles the programs it writes.
stan_prior_line <- function(param, dist, normalize = TRUE) {
  mvgam:::stan_prior_statement(param, dist, normalize = normalize)
}


# The prior an assembled program places on one parameter, parsed
# from the program. A normalised program writes
# `target += dist_lpdf(x | args);` where an emitter wrote
# `x ~ dist(args);`. A negative assertion matching the tilde would
# pass whether or not the prior is there. Returns an empty vector for
# a parameter without a prior, such as a brms coefficient left at its
# flat default.
stan_prior_on <- function(code, param) {
  rows <- mvgam:::mvgam_stancode_prior_rows(
    paste(as.character(code), collapse = "\n")
  )
  keep <- vapply(
    rows, function(r) identical(as.character(r$class), param),
    logical(1L)
  )
  vapply(rows[keep], function(r) as.character(r$prior), character(1L))
}


# The axes record `mvgam()` stores for a frame with `time` and
# `series` columns, built by the package's own code. A mock fit takes
# its `trend_metadata` from here and adds the trend's fields.
mock_axis_record <- function(data) {
  mvgam:::trendless_trend_metadata(data, "time", "series")
}

# One factor-fit mock, shared by every test of the loadings.
# `residual_cor()` and `shared_variation()` are two views of one
# `Z Sigma Z'`. The mock holds an `n_lv` on the trend spec and the
# series levels on the axes record. Its fit slot gives the `Z[i,j]`
# and `sigma_trend` draws through `posterior::as_draws_matrix()`.
mk_factor_obj <- function(n_series = 3L, n_lv = 2L, ndraws = 50L,
                          Z_target = NULL, sigma_target = NULL) {
  if (is.null(Z_target)) {
    Z_target <- matrix(c(0.8, 0.1, -0.3,
                         0.2, 0.5, 0.7), nrow = n_series, ncol = n_lv)
  }
  # Unequal latent scales. A formula that drops them then disagrees
  # with one that keeps them.
  if (is.null(sigma_target)) {
    sigma_target <- c(2.0, 0.25)[seq_len(n_lv)]
  }
  # Build a fake draws matrix with Z[i,j] columns named in Stan order,
  # plus the latent scales the trend samples.
  col_names <- as.vector(outer(seq_len(n_series), seq_len(n_lv),
                                FUN = function(i, j) sprintf("Z[%d,%d]", i, j)))
  sig_names <- sprintf("sigma_trend[%d]", seq_len(n_lv))
  draws_mat <- matrix(NA_real_, nrow = ndraws,
                      ncol = length(col_names) + length(sig_names))
  colnames(draws_mat) <- c(col_names, sig_names)
  for (i in seq_len(n_series)) {
    for (j in seq_len(n_lv)) {
      nm <- sprintf("Z[%d,%d]", i, j)
      # Constant across draws so per-draw cov is deterministic and
      # comparable to a known target.
      draws_mat[, nm] <- Z_target[i, j]
    }
  }
  for (j in seq_len(n_lv)) {
    draws_mat[, sig_names[j]] <- sigma_target[j]
  }
  list(
    obj = structure(
      list(
        mv_spec = list(trend_specs = structure(
          list(n_lv = n_lv), class = "mvgam_trend"
        )),
        trend_metadata = list(
          trend_type = "ZMVN",
          n_lv = n_lv,
          axes = list(series = list(
            levels = paste0("s", seq_len(n_series)), n = n_series
          ))
        ),
        fit = draws_mat
      ),
      class = "mvgam"
    ),
    series_levels = paste0("s", seq_len(n_series)),
    Z_target = Z_target,
    sigma_target = sigma_target
  )
}


#' Match a pattern against generated Stan code
#'
#' Whitespace is stripped from the pattern and the program. An
#' assertion then names the tokens a generator emits, whatever layout
#' the polish step gives them. Either spelling of a sampling statement
#' matches.
#'
#' @param pattern Regular expression, or a literal when the caller
#'   passes `fixed = TRUE`. Must escape its own `(`, `[` and `|`
#'   otherwise.
#' @param x Character vector where matches are sought
#' @param ignore.case Logical. If TRUE, case is ignored (default FALSE)
#' @param ... Additional arguments passed to grepl(), `fixed = TRUE`
#'   among them
#'
#' @return Logical vector indicating matches
#' @noRd
stan_pattern <- function(pattern, x, ignore.case = FALSE, ...) {
  remove_whitespace <- function(text) {
    gsub("\\s+", "", text)
  }

  # With whitespace gone from both sides, `\\s*` between two tokens
  # is redundant and `\\s+` never matches
  x_no_space <- remove_whitespace(x)

  # A regular expression naming Stan syntax escapes its own
  # metacharacters. A literal needs `fixed = TRUE`.
  pattern_final <- remove_whitespace(pattern)

  # An emitter writes `lhs ~ dist(args);`. A normalised program holds
  # `target += dist_lpdf(lhs | args);`. A pattern naming one spelling
  # matches the other. The alternation is a regular expression and is
  # built for regular-expression patterns alone.
  dots <- list(...)
  if (!isTRUE(dots$fixed)) {
    alt <- tilde_pattern_as_lpdf(pattern_final)
    if (!is.na(alt)) {
      pattern_final <- paste0("(", pattern_final, ")|(", alt, ")")
    }
  }

  grepl(pattern_final, x_no_space, ignore.case = ignore.case, ...)
}


# The density-call spelling of a whitespace-free `lhs~dist(args)`
# pattern, or NA for a pattern naming no sampling statement
tilde_pattern_as_lpdf <- function(pattern) {
  if (!grepl("~", pattern, fixed = TRUE)) return(NA_character_)
  # `strsplit()` drops a trailing empty field. The two sides are taken
  # with `sub()`.
  lhs <- sub("~.*$", "", pattern)
  rhs <- sub("^[^~]*~", "", pattern)
  if (!nzchar(lhs)) return(NA_character_)
  # A pattern naming no distribution asserts only that the
  # parameter is sampled. The density call and any container
  # wrapping the parameter, such as `diagonal(...)`, are left open.
  if (!nzchar(rhs)) {
    # The parameter must be the density's first operand. The trend
    # equation names it among the arguments. A looser pattern would
    # match there with the prior deleted.
    return(paste0("_lpdf\\((?:[a-z_]*\\()?", lhs, "[|)]"))
  }
  open <- regexpr("\\\\?\\(", rhs)
  if (open > 0L) {
    dist <- substr(rhs, 1L, open - 1L)
    args <- substr(rhs, open, nchar(rhs))
    args <- sub("^\\\\?\\(", "", args)
    args <- sub("\\\\?\\);?$", "", args)
    if (!nzchar(args)) {
      return(paste0("target\\+=", dist, "_lpdf\\(", lhs, "\\)"))
    }
    paste0("target\\+=", dist, "_lpdf\\(", lhs, "\\|", args, "\\)")
  } else {
    paste0("target\\+=", rhs, "_lpdf\\(", lhs, "\\|")
  }
}
