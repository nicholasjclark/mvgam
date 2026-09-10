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

# Build both Stan code and Stan data from one
# `generate_stan_components_mvgam_formula()` call. Tests that need
# both surfaces should use this instead of calling `stancode()` and
# `standata()` separately, since each public dispatcher re-runs the
# full pipeline (and triggers a fresh V8 isolate for the Stan code
# polish step) on its own.
mvgam_stan_setup <- function(formula, data, family = gaussian(), ...) {
  cc <- mvgam:::generate_stan_components_mvgam_formula(
    formula = formula, data = data, family = family, ...
  )
  code <- cc$combined_components$stancode
  class(code) <- c("mvgamstancode", "stancode", "character")
  list(code = code, data = cc$combined_components$standata)
}


# The statement an emitter writes for one prior. Assertions build
# their expected text through the package's own writer, so a test
# cannot drift from the form the program actually carries. The
# writer itself is pinned directly in `test-stancode-standata.R`,
# and by the generated programs `stanc` compiles, so the agreement
# is not circular.
stan_prior_line <- function(param, dist, normalize = TRUE) {
  mvgam:::stan_prior_statement(param, dist, normalize = normalize)
}


# The prior an assembled program places on one parameter, read out
# of the program rather than matched against a spelling of it. A
# normalised program writes `target += dist_lpdf(x | args);` where
# an emitter wrote `x ~ dist(args);`, so a negative assertion built
# on the tilde passes whether or not the prior is there. Returns an
# empty vector when the parameter carries no prior, which is the
# answer for a brms-owned coefficient left at its flat default.
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


# One factor-fit mock, shared by every test that reads the loadings.
# `residual_cor()` and `shared_variation()` are two views of the same
# `Z Sigma Z'`, so a mock defined in one file and copied into the
# other is two accounts of the quantity the assertions exist to pin.
# Helper to build a fake mvgam fit carrying just enough state for the
# factor-loadings branch: an n_lv on the trend spec, plus a fit slot
# that returns the requested Z[i,j] columns when handed to
# posterior::as_draws_matrix(). resolve_series_info() is consulted
# via local_mocked_bindings so the helper doesn't need a real fit.
mk_factor_obj <- function(n_series = 3L, n_lv = 2L, ndraws = 50L,
                          Z_target = NULL, sigma_target = NULL) {
  if (is.null(Z_target)) {
    Z_target <- matrix(c(0.8, 0.1, -0.3,
                         0.2, 0.5, 0.7), nrow = n_series, ncol = n_lv)
  }
  # Unequal latent scales, so a formula that drops them cannot
  # accidentally agree with one that keeps them.
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
          n_lv = n_lv,
          trend = list(trend_type = "ZMVN")
        ),
        trend_components = list(types = "ZMVN"),
        fit = draws_mat
      ),
      class = "mvgam"
    ),
    series_levels = paste0("s", seq_len(n_series)),
    Z_target = Z_target,
    sigma_target = sigma_target
  )
}
