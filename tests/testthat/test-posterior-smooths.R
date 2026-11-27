# CI-safe tests for posterior_smooths.mvgam, conditional_smooths.mvgam
# and the smooth record they read. A stub carries brms models built
# with `brms::brm(empty = TRUE)`, which writes the Stan data without
# compiling anything, and a hand-made draws matrix standing in for the
# posterior. Concordance with brms on fitted models lives in
# tests/local/test-linpred-parity.R.

# ---- Dispatch + signature parity -----------------------------------

test_that("posterior_smooths.mvgam matches brms signature", {
  expected <- names(formals(getS3method("posterior_smooths", "brmsfit")))
  actual <- names(formals(getS3method("posterior_smooths", "mvgam")))
  # brms passes everything else through ..., so we require at
  # minimum the same named formals up to (and including) `...`.
  expect_true(all(expected %in% actual))
})

test_that("conditional_smooths.mvgam matches brms-parity arg set", {
  brms_args <- names(formals(getS3method("conditional_smooths", "brmsfit")))
  mvgam_args <- names(formals(getS3method("conditional_smooths", "mvgam")))
  # All current brms named args (excluding deprecated nsamples /
  # subset and the legacy `probs` alias brms keeps for back-compat)
  # must be accepted by the mvgam method.
  required <- setdiff(brms_args, c("nsamples", "subset", "probs"))
  expect_true(all(required %in% mvgam_args))
})

# ---- Error paths ---------------------------------------------------

test_that("posterior_smooths.mvgam takes one of 'dpar' and 'nlpar'", {
  stub <- smooth_stub(brms::bf(y ~ 1 + s(z)))
  expect_error(posterior_smooths(stub, "s(z)", dpar = "mu", nlpar = "a"),
               "Supply only one of")
})

# ---- subset_draws_rows ---------------------------------------------

test_that("subset_draws_rows validates ndraws and draw_ids", {
  mat <- matrix(rnorm(20L), nrow = 10L)
  expect_identical(nrow(subset_draws_rows(mat, NULL, NULL)), 10L)
  expect_identical(nrow(subset_draws_rows(mat, 5L, NULL)), 5L)
  # `draw_ids` names the rows it keeps, and comparing them is
  # stronger than counting them. `ndraws` samples its rows instead,
  # and the line above asserts its count.
  expect_identical(subset_draws_rows(mat, NULL, c(1L, 3L)),
                   mat[c(1L, 3L), ])
  # A count covering every row keeps them in the order they were
  # sampled rather than shuffling them.
  expect_identical(subset_draws_rows(mat, 10L, NULL), mat)
  expect_error(subset_draws_rows(mat, 99L, NULL), "more draws")
  expect_error(subset_draws_rows(mat, NULL, c(1L, 99L)), "exceed")
})
