# Smoke check: run loo / loo_compare / waic / pp_check / log_lik on
# cached fixtures and compare structure / values to brms.
# Not a test file — invoke manually with `Rscript`.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(brms)
  library(loo)
})

load_fix <- function(name, kind) {
  path <- file.path("tests/local/fixtures",
                    paste0("val_", kind, "_", name, ".rds"))
  if (!file.exists(path)) stop("missing fixture: ", path)
  readRDS(path)
}

cat_h <- function(s) cat("\n", strrep("=", 70), "\n", s, "\n",
                         strrep("=", 70), "\n", sep = "")

for (fix in c("ar1_fx", "beta_ar1")) {
  cat_h(paste("Fixture:", fix))
  bf <- load_fix(fix, "brms")
  mv <- load_fix(fix, "mvgam")
  cat("brms family: ", bf$family$family, "\n",
      "mvgam family: ", mv$family$family, "\n", sep = "")

  # 1. log_lik shape + finite
  cat("\n-- log_lik shape + finite --\n")
  ll_b <- brms::log_lik(bf)
  ll_m <- log_lik(mv)
  cat("brms dim: ", paste(dim(ll_b), collapse = " x "), "  ",
      "all finite: ", all(is.finite(ll_b)), "\n",
      "mvgam dim: ", paste(dim(ll_m), collapse = " x "), "  ",
      "all finite: ", all(is.finite(ll_m)), "\n", sep = "")

  cat("\n-- log_lik per-obs mean cor --\n")
  cor_ll <- stats::cor(colMeans(ll_b), colMeans(ll_m))
  cat("cor(brms mean log_lik, mvgam mean log_lik) = ",
      round(cor_ll, 3), "\n", sep = "")

  # 2. loo
  cat("\n-- loo --\n")
  loo_b <- suppressWarnings(brms::loo(bf))
  loo_m <- suppressWarnings(loo(mv))
  cat("brms  elpd_loo = ", round(loo_b$estimates["elpd_loo", "Estimate"], 3),
      " (SE ", round(loo_b$estimates["elpd_loo", "SE"], 3), ")\n",
      "mvgam elpd_loo = ", round(loo_m$estimates["elpd_loo", "Estimate"], 3),
      " (SE ", round(loo_m$estimates["elpd_loo", "SE"], 3), ")\n",
      "diff abs = ",
      round(abs(loo_b$estimates["elpd_loo", "Estimate"] -
                loo_m$estimates["elpd_loo", "Estimate"]), 3),
      " (bar = 3 * (SE_b + SE_m) = ",
      round(3 * (loo_b$estimates["elpd_loo", "SE"] +
                 loo_m$estimates["elpd_loo", "SE"]), 3),
      ")\n", sep = "")

  # 3. waic
  cat("\n-- waic --\n")
  waic_b <- suppressWarnings(brms::waic(bf))
  waic_m <- suppressWarnings(waic(mv))
  cat("brms  waic = ", round(waic_b$estimates["waic", "Estimate"], 3), "\n",
      "mvgam waic = ", round(waic_m$estimates["waic", "Estimate"], 3), "\n",
      "mvgam class: ", paste(class(waic_m), collapse = "/"), "\n",
      sep = "")

  # 4. pp_check: several types, confirm ggplot returns
  cat("\n-- pp_check types --\n")
  ppc_types <- c("dens_overlay", "ecdf_overlay", "intervals",
                 "stat", "loo_pit_overlay")
  for (t in ppc_types) {
    plt <- tryCatch(
      suppressWarnings(suppressMessages(
        pp_check(mv, type = t, ndraws = 50)
      )),
      error = function(e) e
    )
    if (inherits(plt, "error")) {
      cat("  ", t, ": ERROR — ", conditionMessage(plt), "\n", sep = "")
    } else {
      cat("  ", t, ": OK (", paste(class(plt), collapse = "/"), ")\n",
          sep = "")
    }
  }
}

# 5. loo_compare on two mvgam models
cat_h("loo_compare across two mvgam fits")
mv_fx <- load_fix("ar1_fx", "mvgam")
mv_int <- load_fix("ar1_int", "mvgam")
cmp <- suppressWarnings(loo_compare(mv_fx, mv_int))
cat("class: ", paste(class(cmp), collapse = "/"), "\n",
    "rows: ", nrow(cmp), "\n", sep = "")
print(cmp)

# 6. loo_compare with criterion = "waic"
cat_h("loo_compare with criterion = 'waic'")
cmp_w <- suppressWarnings(loo_compare(mv_fx, mv_int, criterion = "waic"))
print(cmp_w)

# 7. brms-parity arg surface — make sure the new signature accepts each
cat_h("loo brms-parity arg surface")
# These should not error (all are no-ops or valid pass-through)
ok <- list()
for (test in list(
  list(compare = FALSE),
  list(resp = NULL),
  list(model_names = NULL),
  list(k_threshold = 0.5),
  list(save_psis = TRUE),
  list(incl_dynamics = FALSE),
  list(incl_dynamics = TRUE)
)) {
  result <- tryCatch(
    suppressWarnings(do.call(loo, c(list(mv_fx), test))),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cat("  ", names(test)[1], "=", as.character(test[[1]]),
        ": ERROR — ", conditionMessage(result), "\n", sep = "")
  } else {
    cat("  ", names(test)[1], "=", as.character(test[[1]]),
        ": OK\n", sep = "")
  }
}

# And the args we should reject
cat("\n-- expected-error args --\n")
for (test in list(
  list(pointwise = TRUE),
  list(moment_match = TRUE),
  list(reloo = TRUE)
)) {
  result <- tryCatch(
    suppressWarnings(do.call(loo, c(list(mv_fx), test))),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cat("  ", names(test)[1], "=", as.character(test[[1]]),
        ": correctly errored\n", sep = "")
  } else {
    cat("  ", names(test)[1], "=", as.character(test[[1]]),
        ": should have errored but did not!\n", sep = "")
  }
}

cat("\nDone.\n")
