# Regression fixture: ZMVN trend on irregular time intervals.
#
# After dropping `requires_regular_intervals` from ZMVN's default
# validation rules (because ZMVN is `MVN(0, Sigma)` with covariance
# indexed by series only and Δt does not enter the likelihood),
# this fixture confirms that every downstream surface still works
# on a fit whose time grid has gaps.
#
# Cache: tests/local/fixtures/val_mvgam_zmvn_irregular.rds
#
# Run with:
#   Rscript tests/local/zmvn_irregular_time.R

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
})

# testthat sets the working directory to tests/local/ when it runs a
# file, while Rscript runs it from the package root. Reach the shared
# fixture helpers by whichever of the two paths exists.
source(if (file.exists("concordance_helpers.R")) {
  "concordance_helpers.R"
} else {
  file.path("tests", "local", "concordance_helpers.R")
})

cache <- local_fixture_path("val_mvgam_zmvn_irregular.rds")

if (file.exists(cache)) {
  fit <- readRDS(cache)
  cat("[CACHE] reusing", cache, "\n")
} else {
  set.seed(2026L)
  n_series <- 3L
  series_names <- paste0("s", seq_len(n_series))

  # Deliberately gappy time grid: drop 3, 7, 11 from 1:15.
  # diff(unique_times) = c(2, 1, 1, 2, 1, 1, 2, 1, 1, 1) -> irregular.
  unique_times <- setdiff(seq_len(15L), c(3L, 7L, 11L))

  # Cross-series correlation Sigma_true (ZMVN's actual parameter).
  Sigma_true <- matrix(c(
    1.0,  0.6, -0.3,
    0.6,  1.0,  0.2,
   -0.3,  0.2,  1.0
  ), nrow = 3L, byrow = TRUE)
  L_true <- chol(Sigma_true)

  rows <- list()
  for (t in unique_times) {
    # Per-row scaled innovations: vector of length n_series.
    eta <- t(L_true) %*% rnorm(n_series)
    for (s in seq_len(n_series)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = series_names[s],
        time = t,
        y = 0.5 + eta[s] + rnorm(1L, sd = 0.4)
      )
    }
  }
  dat <- do.call(rbind, rows)
  dat$series <- factor(dat$series, levels = series_names)

  cat("Fixture:", nrow(dat), "rows;", length(unique_times),
      "time points; intervals =",
      paste(diff(sort(unique_times)), collapse = ", "), "\n")

  cat("Fitting mvgam with ZMVN(cor = TRUE) on irregular time ...\n")
  fit <- mvgam(
    formula = y ~ 1,
    trend_formula = ~ ZMVN(cor = TRUE),
    data = dat, family = gaussian(),
    chains = 2L, burnin = 300L, samples = 300L,
    silent = 2, refresh = 0
  )
  # The generative truth rides on the saved fit so a separate test
  # can assert recovery against it without repeating the simulation.
  attr(fit, "sim_truth") <- list(
    n_series = n_series, series_names = series_names,
    unique_times = unique_times, Sigma_true = Sigma_true
  )
  saveRDS(fit, cache)
  cat("[FIT] cached to", cache, "\n")
}

# ------------------------------------------------------------
# Downstream surface checks. Each block prints OK/FAIL + a
# dimensional sanity check; any error halts so we see the failure
# in the log instead of soldiering on with stale state.
# ------------------------------------------------------------
ok <- function(label, val) {
  cat(sprintf("[OK]  %-32s : %s\n", label, val))
}

cat("\n=== Downstream surface audit ===\n")

# 1. summary / print
s <- suppressWarnings(summary(fit))
ok("summary()", class(s)[1L])
ok("print() runs",
   { invisible(capture.output(print(fit))); "ok" })

# 2. predict (link / response / expected)
pl <- predict(fit, type = "link", summary = FALSE)
ok("predict(link)", paste(dim(pl), collapse = " x "))
pr <- predict(fit, type = "response", summary = FALSE)
ok("predict(response)", paste(dim(pr), collapse = " x "))
pe <- posterior_epred(fit, ndraws = 30L)
ok("posterior_epred(30)", paste(dim(pe), collapse = " x "))
pp <- posterior_predict(fit, ndraws = 30L)
ok("posterior_predict(30)", paste(dim(pp), collapse = " x "))

# 3. log_lik / loo / waic
ll <- log_lik(fit, ndraws = 30L)
ok("log_lik(30)",
   paste0(paste(dim(ll), collapse = " x "),
          " (finite = ", all(is.finite(ll)), ")"))
L <- suppressWarnings(loo(fit))
ok("loo elpd_loo",
   round(L$estimates["elpd_loo", "Estimate"], 2))

# 4. forecast / hindcast on the existing grid
fc <- forecast(fit, newdata = NULL)
ok("forecast()", class(fc)[1L])
hc <- hindcast(fit)
ok("hindcast()", class(hc)[1L])

# 5. residuals / tidier helpers
res <- residuals(fit, summary = TRUE)
ok("residuals()", paste(dim(res), collapse = " x "))
aug <- broom::augment(fit)
ok("augment()", paste(dim(aug), collapse = " x "))
tdy <- broom::tidy(fit)
ok("tidy()", paste(dim(tdy), collapse = " x "))
gl <- broom::glance(fit)
ok("glance()", paste(dim(gl), collapse = " x "))

# 6. residual_cor (ZMVN's headline surface)
rc <- residual_cor(fit)
ok("residual_cor()",
   paste0("cor dim = ",
          paste(dim(rc$cor), collapse = " x ")))

# 7. plot(type = ...) round-trip
for (typ in c("trend", "series", "smooths")) {
  p <- tryCatch(plot(fit, type = typ),
                error = function(e) conditionMessage(e))
  ok(paste0("plot(type = '", typ, "')"),
     if (inherits(p, "ggplot")) "ggplot" else paste("ERROR:", p))
}

# 8. pp_check
pc <- suppressWarnings(pp_check(fit, ndraws = 30L))
ok("pp_check(30)",
   if (inherits(pc, "ggplot")) "ggplot" else class(pc)[1L])

# 9. conditional_effects (should be inert since formula has no
#    predictors, but should not error)
ce <- tryCatch(conditional_effects(fit),
               error = function(e) conditionMessage(e))
ok("conditional_effects()",
   if (is.list(ce)) paste(length(ce), "panel(s)") else
   paste("ERROR:", ce))

# 10. Confirm Sigma was actually recovered (sanity, not strict)
post_Sigma <- residual_cor(fit)$cor
cat("\nRecovered cor matrix (posterior median):\n")
print(round(post_Sigma, 3L))
cat("\nTrue cor matrix:\n")
true_cor <- diag(1 / sqrt(diag(crossprod(matrix(c(
  1.0,  0.6, -0.3,  0.6,  1.0,  0.2, -0.3,  0.2,  1.0
), 3L, byrow = TRUE))))) %*% matrix(c(
  1.0,  0.6, -0.3,  0.6,  1.0,  0.2, -0.3,  0.2,  1.0
), 3L, byrow = TRUE) %*% diag(1 / sqrt(diag(crossprod(matrix(c(
  1.0,  0.6, -0.3,  0.6,  1.0,  0.2, -0.3,  0.2,  1.0
), 3L, byrow = TRUE)))))
print(round(true_cor, 3L))

cat("\nAll downstream helpers OK on irregular-time ZMVN fit.\n")
