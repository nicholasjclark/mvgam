# Unit tests for sign_canonicalise_factors. The function mutates
# `@sim$samples` per chain so every saved draw has Z[k,k] >= 0
# without changing the Z * lv_trend product (likelihood-invariant).

# ---- helpers -------------------------------------------------

# Build a minimal fake mvgam fit with a small per-chain samples
# data.frame containing the Z[i,j] and lv_trend[t,k] columns the
# function expects. Avoids a Stan compile; tests the mutation in
# isolation.
mk_fake_factor_fit <- function(n_series = 4L, n_lv = 2L,
                                 n_time = 5L, n_chains = 2L,
                                 n_iter = 10L, seed = 1L,
                                 fixed_Z = NULL) {
  set.seed(seed)
  samples_list <- vector("list", n_chains)
  for (chain in seq_len(n_chains)) {
    s <- list()
    # Z[i, j] lower triangular: upper triangle (i < j) is zero
    for (j in seq_len(n_lv)) {
      for (i in seq_len(n_series)) {
        key <- sprintf("Z[%d,%d]", i, j)
        if (i < j) {
          s[[key]] <- rep(0, n_iter)
        } else {
          s[[key]] <- stats::rnorm(n_iter)
        }
      }
    }
    # lv_trend[t, k]
    for (k in seq_len(n_lv)) {
      for (t in seq_len(n_time)) {
        key <- sprintf("lv_trend[%d,%d]", t, k)
        s[[key]] <- stats::rnorm(n_iter)
      }
    }
    samples_list[[chain]] <- as.data.frame(s, check.names = FALSE)
  }
  sim <- list(samples = samples_list, n_save = rep(n_iter, n_chains),
               chains = n_chains, iter = n_iter, warmup = 0L,
               thin = 1L)
  stanfit <- new("stanfit")
  stanfit@sim <- sim

  trend_metadata <- list(n_lv = n_lv, fixed_Z = fixed_Z)
  trend_spec <- structure(
    list(trend = "AR", n_lv = n_lv, trend_type = "AR"),
    class = "mvgam_trend"
  )
  obj <- list(
    fit = stanfit,
    mv_spec = list(trend_specs = trend_spec),
    series_info = list(n_series = n_series),
    trend_metadata = trend_metadata
  )
  class(obj) <- c("mvgam", "brmsfit")
  obj
}

# Compute the saved Z * lv_trend product (per draw, summed across
# factors) for a given chain. Used to assert invariance.
chain_z_lv_product <- function(samples, n_series, n_lv, n_time) {
  prod <- array(0, dim = c(nrow(samples), n_time, n_series))
  for (t in seq_len(n_time)) {
    for (s in seq_len(n_series)) {
      acc <- rep(0, nrow(samples))
      for (k in seq_len(n_lv)) {
        z_key <- sprintf("Z[%d,%d]", s, k)
        lv_key <- sprintf("lv_trend[%d,%d]", t, k)
        acc <- acc + samples[[z_key]] * samples[[lv_key]]
      }
      prod[, t, s] <- acc
    }
  }
  prod
}


# ---- core behaviour ------------------------------------------

test_that("Z[k,k] is non-negative for every saved draw after fix", {
  fit <- mk_fake_factor_fit(seed = 7L)
  fixed <- mvgam:::sign_canonicalise_factors(fit)
  for (chain in seq_along(fixed$fit@sim$samples)) {
    s <- fixed$fit@sim$samples[[chain]]
    expect_true(all(s[["Z[1,1]"]] >= 0))
    expect_true(all(s[["Z[2,2]"]] >= 0))
  }
})

test_that("Z * lv_trend product is invariant per draw", {
  fit <- mk_fake_factor_fit(seed = 13L)
  raw_prods <- lapply(fit$fit@sim$samples, chain_z_lv_product,
                       n_series = 4L, n_lv = 2L, n_time = 5L)
  fixed <- mvgam:::sign_canonicalise_factors(fit)
  fix_prods <- lapply(fixed$fit@sim$samples, chain_z_lv_product,
                       n_series = 4L, n_lv = 2L, n_time = 5L)
  for (chain in seq_along(raw_prods)) {
    expect_equal(raw_prods[[chain]], fix_prods[[chain]],
                 tolerance = 1e-12)
  }
})

test_that("sign_canonicalised flag is set after a successful pass", {
  fit <- mk_fake_factor_fit()
  expect_null(fit$trend_metadata$sign_canonicalised)
  out <- mvgam:::sign_canonicalise_factors(fit)
  expect_true(isTRUE(out$trend_metadata$sign_canonicalised))
})

test_that("function is idempotent (second pass is a no-op)", {
  fit <- mk_fake_factor_fit(seed = 21L)
  once <- mvgam:::sign_canonicalise_factors(fit)
  twice <- mvgam:::sign_canonicalise_factors(once)
  for (chain in seq_along(once$fit@sim$samples)) {
    expect_identical(once$fit@sim$samples[[chain]],
                      twice$fit@sim$samples[[chain]])
  }
})


# ---- skip conditions -----------------------------------------

test_that("non-factor fits pass through unchanged", {
  fit <- mk_fake_factor_fit()
  # Wipe n_lv: simulate a non-factor model
  fit$mv_spec$trend_specs$n_lv <- NULL
  fit$trend_metadata$n_lv <- NULL
  out <- mvgam:::sign_canonicalise_factors(fit)
  for (chain in seq_along(fit$fit@sim$samples)) {
    expect_identical(out$fit@sim$samples[[chain]],
                      fit$fit@sim$samples[[chain]])
  }
  expect_null(out$trend_metadata$sign_canonicalised)
})

test_that("fixed-Z fits (trend_map) pass through unchanged", {
  fixed_Z <- matrix(c(1, 0, 0.5, 0.5, 0, 1, 0.3, 0.7),
                     nrow = 4L, ncol = 2L)
  fit <- mk_fake_factor_fit(fixed_Z = fixed_Z, seed = 41L)
  out <- mvgam:::sign_canonicalise_factors(fit)
  for (chain in seq_along(fit$fit@sim$samples)) {
    expect_identical(out$fit@sim$samples[[chain]],
                      fit$fit@sim$samples[[chain]])
  }
  expect_null(out$trend_metadata$sign_canonicalised)
})

test_that("empty per-chain samples are skipped without error", {
  fit <- mk_fake_factor_fit(n_iter = 0L)
  out <- mvgam:::sign_canonicalise_factors(fit)
  # Function should not crash; flag may still get set since
  # n_lv-based eligibility check passes, but no samples mutate.
  for (chain in seq_along(out$fit@sim$samples)) {
    expect_equal(nrow(out$fit@sim$samples[[chain]]), 0L)
  }
})

test_that("input validation rejects non-mvgam objects", {
  expect_error(
    mvgam:::sign_canonicalise_factors(list()),
    "Must inherit from class 'mvgam'"
  )
})
