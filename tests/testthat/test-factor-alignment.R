# Relabelling of factors that take their own coefficients, and the
# choice between it and the QR rotation.

# Draws of a three-factor model whose factors are reordered and
# flipped at random in every draw, as label switching leaves them.
switched_draws <- function(n_draws = 40L, n_series = 6L, n_time = 5L) {
  set.seed(1)
  truth <- cbind(c(2, 2, 2, 0, 0, 0), c(0, 0, 0, 1, 1, 0),
                 c(0, 0, 0, 0, 0, -0.5))
  ar_truth <- c(0.8, 0.1, 0.5)
  n_lv <- ncol(truth)
  chain <- list()
  for (s in seq_len(n_draws)) {
    order_s <- sample(n_lv)
    sign_s <- sample(c(-1, 1), n_lv, replace = TRUE)
    Z <- (truth + rnorm(length(truth), 0, 0.02))[, order_s] %*% diag(sign_s)
    lv <- matrix(rnorm(n_time * n_lv), n_time)[, order_s] %*% diag(sign_s)
    chain[[s]] <- c(
      stats::setNames(Z, sprintf("Z[%d,%d]", row(Z), col(Z))),
      stats::setNames(lv, sprintf("lv_trend[%d,%d]", row(lv), col(lv))),
      stats::setNames(ar_truth[order_s], sprintf("ar1_trend[%d]", 1:n_lv)),
      "lp__" = -s
    )
  }
  as.data.frame(do.call(rbind, chain), check.names = FALSE)
}

draw_arrays <- function(chain, n_series = 6L, n_time = 5L, n_lv = 3L) {
  pick <- function(stem, n) {
    cols <- sprintf("%s[%d,%d]", stem, rep(seq_len(n), n_lv),
                    rep(seq_len(n_lv), each = n))
    array(as.matrix(chain[cols]), c(nrow(chain), n, n_lv))
  }
  list(Z = pick("Z", n_series), lv = pick("lv_trend", n_time),
       ar = as.matrix(chain[sprintf("ar1_trend[%d]", seq_len(n_lv))]))
}

test_that("relabelling gives every draw one ordering and orientation", {
  chain <- switched_draws()
  before <- draw_arrays(chain)
  alignment <- align_factor_draws(before$Z, lp = chain$lp__)
  after <- draw_arrays(relabel_chain(chain, alignment$perm, alignment$sign))

  # Factors are ordered by the size of their loadings, each with its
  # largest loading positive, and its coefficient travels with it.
  mean_Z <- apply(after$Z, c(2, 3), mean)
  expect_equal(mean_Z, cbind(c(2, 2, 2, 0, 0, 0), c(0, 0, 0, 1, 1, 0),
                             c(0, 0, 0, 0, 0, 0.5)), tolerance = 0.02)
  expect_equal(apply(after$Z, c(2, 3), sd) < 0.05, matrix(TRUE, 6, 3))
  expect_equal(unname(after$ar),
               matrix(c(0.8, 0.1, 0.5), 40, 3, byrow = TRUE))
  # The trend each draw implies is untouched.
  trend <- function(x, s) x$lv[s, , ] %*% t(x$Z[s, , ])
  expect_equal(trend(after, 7L), trend(before, 7L))
})

test_that("a trend_map limits the moves to those that keep it intact", {
  chain <- switched_draws()
  Z <- draw_arrays(chain)$Z
  # Column 1 holds a fixed non-zero entry. Columns 2 and 3 share one
  # template and may be exchanged and flipped.
  template <- matrix(NA_real_, 6, 3)
  template[1, ] <- c(1, 0, 0)
  alignment <- align_factor_draws(Z, lp = chain$lp__, template = template)
  expect_true(all(alignment$perm[, 1] == 1L & alignment$sign[, 1] == 1))
  expect_setequal(unique(alignment$perm[, 2]), c(2L, 3L))
  # The vector copy of the sampled loadings follows the matrix.
  chain[sprintf("Z_free_vec[%d]", 1:15)] <- 0
  relabelled <- relabel_chain(chain, alignment$perm, alignment$sign, template)
  expect_identical(relabelled[["Z_free_vec[6]"]], relabelled[["Z[2,2]"]])
})

test_that("the greedy assignment for many factors keeps one column each", {
  set.seed(2)
  score <- array(runif(5 * 8 * 8), c(5, 8, 8))
  pick <- best_assignment(score)
  expect_true(all(apply(pick, 1, function(p) setequal(p, 1:8))))
})

test_that("identification follows the dynamics of the factors", {
  spec <- function(trend, ...) {
    structure(list(trend = trend, n_lv = 2L, ...), class = "mvgam_trend")
  }
  partial <- matrix(c(1, NA, NA, NA), 2, 2)
  expect_identical(factor_identification(spec("AR")), "relabel")
  expect_identical(
    factor_identification(spec("AR", coef_sharing = "shared")), "rotation"
  )
  expect_identical(factor_identification(spec("RW")), "rotation")
  expect_identical(factor_identification(spec("VAR")), "rotation")
  expect_identical(factor_identification(spec("ZMVN")), "rotation")
  expect_identical(factor_identification(spec("AR", fixed_Z = partial)),
                   "relabel")
  expect_identical(factor_identification(spec("RW", fixed_Z = partial)),
                   "none")
  expect_identical(
    factor_identification(spec("AR", fixed_Z = partial, cor = TRUE)), "none"
  )
  expect_identical(factor_identification(spec("AR"), has_by_lv = TRUE),
                   "none")
})

test_that("every per-factor parameter of an AR factor model is relabelled", {
  # A parameter sized by the number of factors that `relabel_factors()`
  # does not move would keep the sampled labelling while its factor
  # moved. Each declaration of an AR factor program is either moved or
  # known to hold one value under every relabelling.
  set.seed(1)
  dat <- sim_mvgam(family = gaussian(), n_series = 4L,
                   n_timepoints = 20L)$data_train
  unmoved <- c("L_Omega_trend", "Sigma_trend",
               factor_indexed_pars$recomputed)
  declared <- function(trend_formula, ...) {
    code <- stancode(
      mvgam_formula(y ~ 1, trend_formula = trend_formula),
      data = dat, family = gaussian(), ...
    )
    lines <- grep("N_lv_trend\\]", strsplit(code, "\n")[[1]], value = TRUE)
    lines <- grep("^\\s*(vector|matrix|array|cholesky|cov_matrix)", lines,
                  value = TRUE)
    unique(sub("^.*\\]\\s+([A-Za-z0-9_]+).*$", "\\1", lines))
  }
  names_found <- unique(c(
    declared(~ AR(p = 2, n_lv = 2)),
    declared(~ AR(p = 1, ma = TRUE, n_lv = 2, cor = TRUE)),
    declared(~ AR(p = 1, n_lv = 2), loadings_prior = "mgp")
  ))
  moved <- names_found %in% factor_indexed_pars$signed |
    grepl(factor_indexed_pars$unsigned, names_found)
  expect_identical(setdiff(names_found[!moved], unmoved), character(0))
})
