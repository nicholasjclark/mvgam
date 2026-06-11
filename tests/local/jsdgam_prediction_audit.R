# Local fixture: prediction surface audit for a fitted jsdgam.
#
# Fits one small jsdgam, caches the result, then smoke-tests each
# inherited surface (residual_cor, predict, posterior_epred,
# posterior_predict, log_lik, loo, print, summary, ordinate,
# shared_variation, conditional_effects, pp_check). The audit
# matrix in the v2.0 plan §chunk-4 lives here so testthat stays
# fast and pure-compile.
#
# Run with:
#   Rscript tests/local/jsdgam_prediction_audit.R
#
# The cached fit is written to /tmp/chunk4_jsdgam_fit.rds and
# reloaded on subsequent runs. Delete it to force a refit.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
})

set.seed(11L)

cache_path <- "/tmp/chunk4_jsdgam_fit.rds"

if (file.exists(cache_path)) {
  cat("Loading cached jsdgam fit from", cache_path, "\n")
  fit <- readRDS(cache_path)
} else {
  cat("Fitting jsdgam from scratch\n")
  n_time <- 20L
  n_species <- 4L
  dat <- expand.grid(
    time = seq_len(n_time),
    species = paste0("sp", seq_len(n_species))
  )
  dat$species <- factor(dat$species)
  dat$elev <- rep(rnorm(n_time), times = n_species)
  dat$y <- rpois(nrow(dat), lambda = 2)
  fit <- jsdgam(
    formula = y ~ 1 + elev,
    factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = poisson(), n_lv = 2L,
    chains = 2L, parallel = TRUE,
    burnin = 200L, samples = 200L,
    silent = 2
  )
  saveRDS(fit, cache_path)
  cat("Saved fit to", cache_path, "\n")
}

# Audit runner: each surface gets one smoke check, results
# accumulated into a single result table at the end.

audit_results <- list()

record <- function(name, success, detail = "") {
  audit_results[[name]] <<- list(
    surface = name, ok = success, detail = detail
  )
}

safe_check <- function(name, expr) {
  out <- tryCatch(
    {
      val <- expr()
      list(ok = TRUE, val = val)
    },
    error = function(e) list(ok = FALSE, msg = conditionMessage(e))
  )
  if (out$ok) {
    record(name, TRUE, detail = "ok")
  } else {
    record(name, FALSE, detail = out$msg)
  }
}

safe_check("residual_cor", function() {
  rcor <- residual_cor(fit)
  stopifnot(inherits(rcor, "mvgam_residcor"))
  stopifnot(all(dim(rcor$cor) == c(4L, 4L)))
  stopifnot(all(diag(rcor$cor) == 1))
  TRUE
})

safe_check("predict_link", function() {
  lin <- predict(fit, type = "link")
  cat("  predict(link) class:", class(lin)[1L],
      "dim:", paste(dim(lin), collapse = " x "), "\n")
  TRUE
})

safe_check("predict_response", function() {
  resp <- predict(fit, type = "response")
  cat("  predict(response) class:", class(resp)[1L], "\n")
  TRUE
})

safe_check("posterior_epred", function() {
  ep <- posterior_epred(fit, ndraws = 30L)
  cat("  posterior_epred dim:",
      paste(dim(ep), collapse = " x "), "\n")
  TRUE
})

safe_check("posterior_predict", function() {
  pp <- posterior_predict(fit, ndraws = 30L)
  cat("  posterior_predict dim:",
      paste(dim(pp), collapse = " x "), "\n")
  TRUE
})

safe_check("log_lik", function() {
  ll <- log_lik(fit, ndraws = 30L)
  stopifnot(is.matrix(ll))
  stopifnot(all(is.finite(ll)))
  cat("  log_lik dim:", paste(dim(ll), collapse = " x "), "\n")
  TRUE
})

safe_check("loo", function() {
  L <- suppressWarnings(loo(fit))
  stopifnot(inherits(L, "loo"))
  cat("  loo elpd_loo:",
      round(L$estimates["elpd_loo", "Estimate"], 2), "\n")
  TRUE
})

safe_check("print", function() {
  invisible(capture.output(print(fit)))
  TRUE
})

safe_check("summary", function() {
  s <- suppressWarnings(summary(fit))
  TRUE
})

safe_check("ordinate", function() {
  p <- ordinate(fit, alpha = 0.7)
  stopifnot(inherits(p, "ggplot"))
  TRUE
})

safe_check("shared_variation", function() {
  sv <- shared_variation(fit)
  cat("  shared_variation class:", class(sv)[1L], "\n")
  TRUE
})

safe_check("conditional_effects", function() {
  ce <- suppressWarnings(conditional_effects(fit))
  cat("  conditional_effects class:", class(ce)[1L], "\n")
  TRUE
})

safe_check("pp_check", function() {
  p <- suppressWarnings(pp_check(fit, ndraws = 30L))
  stopifnot(inherits(p, "ggplot"))
  TRUE
})

audit_df <- do.call(rbind, lapply(audit_results, function(x) {
  data.frame(
    surface = x$surface, ok = x$ok, detail = x$detail,
    stringsAsFactors = FALSE
  )
}))
rownames(audit_df) <- NULL

cat("\n=== Audit matrix ===\n")
print(audit_df)

n_pass <- sum(audit_df$ok)
n_total <- nrow(audit_df)
cat("\nSummary:", n_pass, "/", n_total, "surfaces passed.\n")

invisible(audit_df)
