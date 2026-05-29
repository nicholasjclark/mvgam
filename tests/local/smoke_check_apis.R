# Smoke check: run log_lik / loo / loo_compare / waic / pp_check on a
# broad slice of cached fixtures and report any failures or surprising
# divergences. Not a test file — invoke manually with `Rscript`.

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(brms)
  library(loo)
})

load_fix <- function(name, kind) {
  path <- file.path(
    "tests/local/fixtures",
    paste0("val_", kind, "_", name, ".rds")
  )
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

# Fixture pairs covering distinct families and architectures.
fixtures <- list(
  list(name = "ar1_int",   note = "Poisson AR(1) intercept-only"),
  list(name = "ar1_fx",    note = "Poisson AR(1) + fixed effect"),
  list(name = "ar1_re",    note = "Poisson AR(1) + random intercept"),
  list(name = "ar1_re_smooth",
       note = "Poisson AR(1) + RE + smooth"),
  list(name = "ar1_cor_re",
       note = "Poisson AR(1) + correlated RE"),
  list(name = "ar1_mo",    note = "Poisson AR(1) + monotonic mo()"),
  list(name = "ar1_gp",    note = "Poisson AR(1) + 1D Gaussian process"),
  list(name = "ar1_gp2_by",
       note = "Poisson AR(1) + GP by-variable"),
  list(name = "ar1_gp2d",  note = "Poisson AR(1) + 2D GP"),
  list(name = "ar1_hs",    note = "Poisson AR(1) + horseshoe prior"),
  list(name = "ar1_t2_noint",
       note = "Poisson AR(1) + t2 tensor, no intercept"),
  list(name = "mv_gauss",  note = "Multivariate Gaussian"),
  list(name = "beta_ar1",  note = "Beta AR(1)"),
  list(name = "binom_ar1", note = "Binomial AR(1)"),
  list(name = "cumulative_fx",
       note = "Cumulative ordinal + fixed effect"),
  list(name = "hurdle_poisson_ar1",
       note = "Hurdle Poisson AR(1)"),
  list(name = "hurdle_negbinomial_ar1",
       note = "Hurdle NegBin AR(1)"),
  list(name = "zero_inflated_poisson_ar1",
       note = "Zero-inflated Poisson AR(1)")
)

results <- vector("list", length(fixtures))
names(results) <- vapply(fixtures, `[[`, "", "name")

run_call <- function(expr) {
  tryCatch(
    suppressWarnings(suppressMessages(expr)),
    error = function(e) structure(e, class = c("smoke_err", class(e)))
  )
}

fmt_err <- function(x) {
  if (inherits(x, "smoke_err")) paste0("ERR: ", conditionMessage(x))
  else "OK"
}

for (i in seq_along(fixtures)) {
  fx <- fixtures[[i]]
  cat("\n----------------------------------------------------------\n",
      fx$name, " — ", fx$note, "\n",
      "----------------------------------------------------------\n",
      sep = "")
  bf <- load_fix(fx$name, "brms")
  mv <- load_fix(fx$name, "mvgam")
  if (is.null(bf) || is.null(mv)) {
    cat("  SKIP (missing fixture pair)\n")
    results[[i]] <- list(status = "skip")
    next
  }
  cat("  brms family : ", bf$family$family, "\n",
      "  mvgam family: ", mv$family$family, "\n",
      sep = "")

  r <- list(family = mv$family$family)

  # 1. log_lik shape + per-obs mean cor vs brms (where comparable)
  ll <- run_call(log_lik(mv))
  r$log_lik <- if (inherits(ll, "smoke_err")) {
    fmt_err(ll)
  } else {
    finite <- all(is.finite(ll))
    dim_str <- paste(dim(ll), collapse = "x")
    paste0(dim_str, ", finite=", finite)
  }
  cat("  log_lik   : ", r$log_lik, "\n", sep = "")
  if (!inherits(ll, "smoke_err")) {
    # Try brms log_lik for the comparable cases (univariate, both fits
    # share newdata = mv$data)
    if (!inherits(bf$family, "mvbrmsfamily")) {
      ll_b <- run_call(brms::log_lik(bf, newdata = mv$data))
      if (!inherits(ll_b, "smoke_err") &&
          identical(dim(ll), dim(ll_b))) {
        cor_ll <- stats::cor(colMeans(ll), colMeans(ll_b))
        r$log_lik_cor <- round(cor_ll, 3)
        cat("    cor vs brms: ", r$log_lik_cor, "\n", sep = "")
      }
    }
  }

  # 2. loo (default incl_dynamics = FALSE)
  lo <- run_call(loo(mv))
  r$loo <- if (inherits(lo, "smoke_err")) {
    fmt_err(lo)
  } else {
    paste0("elpd=", round(lo$estimates["elpd_loo", "Estimate"], 1),
           " SE=", round(lo$estimates["elpd_loo", "SE"], 1))
  }
  cat("  loo       : ", r$loo, "\n", sep = "")

  # 3. waic
  w <- run_call(waic(mv))
  r$waic <- if (inherits(w, "smoke_err")) {
    fmt_err(w)
  } else {
    paste0("waic=", round(w$estimates["waic", "Estimate"], 1))
  }
  cat("  waic      : ", r$waic, "\n", sep = "")

  # 4. pp_check: one general type + the PSIS-LOO branch
  # Multivariate fits require a single resp argument (brms parity).
  is_mv <- brms::is.mvbrmsformula(mv$formula)
  pp_resp <- if (is_mv) mv$formula$responses[1L] else NULL
  for (pt in c("dens_overlay", "loo_pit_overlay")) {
    plt <- run_call(
      if (is_mv) {
        pp_check(mv, type = pt, ndraws = 50, resp = pp_resp)
      } else {
        pp_check(mv, type = pt, ndraws = 50)
      }
    )
    r[[paste0("pp_check_", pt)]] <- if (inherits(plt, "smoke_err")) {
      fmt_err(plt)
    } else {
      paste(class(plt)[1])
    }
    cat("  pp_check ", sprintf("%-18s", paste0(pt, ":")),
        r[[paste0("pp_check_", pt)]], "\n", sep = "")
  }

  results[[i]] <- r
}


# -------------------------------------------------------------------------
# Edge-case rounds. Each is its own header so failures stand out.
# -------------------------------------------------------------------------

cat("\n\n",
    "==========================================================\n",
    "EDGE CASE A: log_lik out-of-sample\n",
    "==========================================================\n",
    sep = "")
mv_fx <- load_fix("ar1_fx", "mvgam")
# Split the data in half: fit was on all 30 obs, score on the last 15.
holdout <- mv_fx$data[16:30, , drop = FALSE]
ll_in <- run_call(log_lik(mv_fx))
ll_out <- run_call(log_lik(mv_fx, newdata = holdout))
cat("  in-sample log_lik dim : ",
    paste(dim(ll_in), collapse = "x"), "\n", sep = "")
cat("  out-of-sample dim     : ",
    paste(dim(ll_out), collapse = "x"), "\n",
    "  (expected ndraws x 15)\n", sep = "")
cat("  out-of-sample finite  : ",
    if (!inherits(ll_out, "smoke_err")) all(is.finite(ll_out)) else "ERR",
    "\n", sep = "")


cat("\n",
    "==========================================================\n",
    "EDGE CASE B: loo_compare with 3 models\n",
    "==========================================================\n",
    sep = "")
mv_int <- load_fix("ar1_int", "mvgam")
mv_re <- load_fix("ar1_re", "mvgam")
cmp3 <- run_call(loo_compare(mv_int, mv_fx, mv_re))
if (inherits(cmp3, "smoke_err")) {
  cat("  ERR: ", conditionMessage(cmp3), "\n", sep = "")
} else {
  cat("  class : ", paste(class(cmp3), collapse = "/"), "\n",
      "  nrow  : ", nrow(cmp3), "  (expected 3)\n", sep = "")
  print(cmp3)
}


cat("\n",
    "==========================================================\n",
    "EDGE CASE C: pp_check grouped / x= / prefix=ppd / rootogram\n",
    "==========================================================\n",
    sep = "")
# Use ar1_re_smooth because it has 'series' and 'season' covariates.
mv_g <- load_fix("ar1_re_smooth", "mvgam")
cat("  data names: ", paste(names(mv_g$data), collapse = ", "), "\n",
    sep = "")
pp_edge_types <- list(
  c(type = "dens_overlay_grouped", arg = "group"),
  c(type = "stat_grouped",         arg = "group"),
  c(type = "intervals",            arg = "x"),
  c(type = "ribbon",               arg = "x"),
  c(type = "rootogram",            arg = "none"),
  c(type = "dens_overlay",         arg = "ppd")
)
for (cfg in pp_edge_types) {
  pt <- cfg[["type"]]
  arg <- cfg[["arg"]]
  plt <- run_call(switch(arg,
    group = pp_check(mv_g, type = pt, ndraws = 30, group = "series"),
    x = pp_check(mv_g, type = pt, ndraws = 30, x = "x"),
    none = pp_check(mv_g, type = pt, ndraws = 30),
    ppd = pp_check(mv_g, type = pt, ndraws = 30, prefix = "ppd")
  ))
  cat("  ", sprintf("%-26s", paste0(pt, "(", arg, "):")),
      if (inherits(plt, "smoke_err")) fmt_err(plt)
      else paste0("OK (", class(plt)[1L], ")"),
      "\n", sep = "")
}


cat("\n",
    "==========================================================\n",
    "EDGE CASE D: log_lik / loo on a multivariate fit\n",
    "==========================================================\n",
    sep = "")
mv_mv <- load_fix("mv_gauss", "mvgam")
ll_joint <- run_call(log_lik(mv_mv))
ll_y1 <- run_call(log_lik(mv_mv, resp = "y1"))
cat("  joint   dim : ",
    if (!inherits(ll_joint, "smoke_err"))
      paste(dim(ll_joint), collapse = "x")
    else fmt_err(ll_joint), "\n", sep = "")
cat("  resp=y1 dim : ",
    if (!inherits(ll_y1, "smoke_err"))
      paste(dim(ll_y1), collapse = "x")
    else fmt_err(ll_y1), "\n", sep = "")
# brms parity: joint log_lik for mv fits is the per-obs sum across
# responses, so it has the same column count as a single response.
if (!inherits(ll_joint, "smoke_err") && !inherits(ll_y1, "smoke_err")) {
  cat("  joint width == per-resp width ? : ",
      ncol(ll_joint) == ncol(ll_y1), "\n", sep = "")
}
loo_mv <- run_call(loo(mv_mv))
cat("  loo(joint) elpd_loo : ",
    if (!inherits(loo_mv, "smoke_err"))
      round(loo_mv$estimates["elpd_loo", "Estimate"], 1)
    else fmt_err(loo_mv), "\n", sep = "")


cat("\n",
    "==========================================================\n",
    "EDGE CASE E: process_error TRUE vs FALSE — log_lik differs\n",
    "==========================================================\n",
    sep = "")
# Pick a Gaussian fit where the trend uncertainty actually moves the
# linear predictor; ar1_fx is Poisson and small. mv_gauss is the only
# Gaussian fixture currently in cache.
mv_pe <- load_fix("ar1_re_smooth", "mvgam")
ll_pe_FALSE <- run_call(log_lik(mv_pe, process_error = FALSE))
ll_pe_TRUE  <- run_call(log_lik(mv_pe, process_error = TRUE))
if (!inherits(ll_pe_FALSE, "smoke_err") && !inherits(ll_pe_TRUE, "smoke_err")) {
  diff_mean <- mean(abs(colMeans(ll_pe_FALSE) - colMeans(ll_pe_TRUE)))
  cat("  mean |colMean diff| (FALSE vs TRUE) : ",
      round(diff_mean, 3), "\n", sep = "")
  cat("  > 0 (toggling has a measurable effect)? : ",
      diff_mean > 1e-6, "\n", sep = "")
} else {
  cat("  ERR computing log_lik for the toggle test\n")
}


cat("\n",
    "==========================================================\n",
    "EDGE CASE F: invalid resp on a multivariate fit errors\n",
    "==========================================================\n",
    sep = "")
err_no_resp <- run_call(pp_check(mv_mv, type = "dens_overlay", ndraws = 5))
err_bad_resp <- run_call(
  pp_check(mv_mv, type = "dens_overlay", ndraws = 5, resp = "not_a_response")
)
cat("  pp_check(mv, no resp)        : ",
    if (inherits(err_no_resp, "smoke_err"))
      "correctly errored"
    else "should have errored but did not!",
    "\n", sep = "")
cat("  pp_check(mv, resp='bogus')   : ",
    if (inherits(err_bad_resp, "smoke_err"))
      "correctly errored"
    else "should have errored but did not!",
    "\n", sep = "")


# Final compact summary
cat("\n\n==========================================================\n",
    "SUMMARY: ", length(fixtures), " fixtures\n",
    "==========================================================\n",
    sep = "")
ok <- function(s) !startsWith(s %||% "", "ERR:")
for (i in seq_along(fixtures)) {
  fx <- fixtures[[i]]
  r <- results[[i]]
  if (identical(r$status, "skip")) {
    cat(sprintf("  %-32s  SKIP\n", fx$name))
    next
  }
  flags <- c(
    log_lik       = ok(r$log_lik),
    loo           = ok(r$loo),
    waic          = ok(r$waic),
    pp_dens       = ok(r$pp_check_dens_overlay),
    pp_loo_pit    = ok(r$pp_check_loo_pit_overlay)
  )
  marks <- ifelse(flags, "OK", "FAIL")
  cat(sprintf("  %-32s  %s\n",
              fx$name,
              paste0(names(flags), "=", marks, collapse = "  ")))
}
cat("\nDone.\n")
