# The tests of test-factor-alignment.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

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
    declared(~ AR(p = 1, ma = TRUE, n_lv = 2)),
    declared(~ AR(p = 1, n_lv = 2), loadings_prior = "mgp"),
    # Centred, the start innovations and the distances move with lv_trend
    declared(~ AR(p = 2, ma = TRUE, n_lv = 2, centred = TRUE))
  ))
  moved <- names_found %in% factor_indexed_pars$signed |
    grepl(factor_indexed_pars$unsigned, names_found)
  expect_identical(setdiff(names_found[!moved], unmoved), character(0))
})
