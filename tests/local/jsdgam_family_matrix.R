# Local fixture: family pass-through matrix for jsdgam.
#
# Fits a small jsdgam under each observation family that the
# univariate-ready checklist names and audits the inherited
# method surface plus residual_cor on each. Confirms the wrapper
# composes cleanly with the brms family pipeline across continuous,
# count, binary, proportion, and positive-skewed responses.
#
# Families exercised:
#   gaussian (continuous)
#   poisson  (counts)             [also in jsdgam_prediction_audit]
#   nb       (over-dispersed counts)
#   bernoulli (binary)            [also in heaps_birds_replica]
#   Beta     (proportions in (0,1))
#   gamma    (positive continuous)
#
# Run with:
#   Rscript tests/local/jsdgam_family_matrix.R
#
# Cached fits at /tmp/jsdgam_family_<name>.rds, delete to refit.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
})

set.seed(2024L)

n_time <- 18L
n_species <- 4L

build_panel <- function(rfun) {
  set.seed(2024L)
  dat <- expand.grid(
    time = seq_len(n_time),
    species = paste0("sp", seq_len(n_species))
  )
  dat$species <- factor(dat$species)
  dat$elev <- rep(rnorm(n_time), times = n_species)
  dat$y <- rfun(nrow(dat))
  dat
}

fit_one <- function(name, family, rfun) {
  cache <- paste0("/tmp/jsdgam_family_", name, ".rds")
  if (file.exists(cache)) {
    cat("  loading cached", name, "fit\n")
    return(readRDS(cache))
  }
  dat <- build_panel(rfun)
  fit <- jsdgam(
    formula = y ~ 1 + elev,
    factor_formula = ~ -1,
    data = dat, unit = time, species = species,
    family = family, n_lv = 2L,
    chains = 2L, parallel = TRUE,
    burnin = 200L, samples = 200L,
    silent = 2
  )
  saveRDS(fit, cache)
  fit
}

audit <- function(name, fit) {
  cat("\n[", name, "]\n", sep = "")
  ok_count <- 0L
  total <- 0L
  step <- function(label, expr) {
    total <<- total + 1L
    out <- tryCatch(expr, error = function(e) {
      cat("  [FAIL]", label, ":",
          strsplit(conditionMessage(e), "\n")[[1]][1], "\n")
      return(NULL)
    })
    if (!is.null(out)) {
      cat("  [ok]", label, "\n")
      ok_count <<- ok_count + 1L
    }
    invisible(out)
  }
  step("residual_cor", {
    rc <- residual_cor(fit)
    stopifnot(all(dim(rc$cor) == c(n_species, n_species)))
    rc
  })
  step("predict(link)", {
    p <- predict(fit, type = "link")
    stopifnot(NROW(p) == n_time * n_species)
    p
  })
  step("predict(response)", {
    p <- predict(fit, type = "response")
    stopifnot(NROW(p) == n_time * n_species)
    p
  })
  step("posterior_epred", {
    ep <- posterior_epred(fit, ndraws = 25L)
    stopifnot(NROW(ep) == 25L)
    ep
  })
  step("posterior_predict", {
    pp <- posterior_predict(fit, ndraws = 25L)
    stopifnot(NROW(pp) == 25L)
    pp
  })
  step("log_lik finite", {
    ll <- log_lik(fit, ndraws = 25L)
    stopifnot(all(is.finite(ll)))
    ll
  })
  step("loo finite", {
    L <- suppressWarnings(loo(fit))
    stopifnot(is.finite(L$estimates["elpd_loo", "Estimate"]))
    L
  })
  step("summary()", {
    invisible(capture.output(suppressWarnings(summary(fit))))
    TRUE
  })
  step("print()", {
    invisible(capture.output(print(fit)))
    TRUE
  })
  step("pp_check()", {
    suppressWarnings(pp_check(fit, ndraws = 25L))
    TRUE
  })
  step("ordinate()", {
    p <- ordinate(fit, alpha = 0.7)
    stopifnot(inherits(p, "ggplot"))
    TRUE
  })
  cat("  -- ", ok_count, "/", total, " surfaces ok\n", sep = "")
  list(ok = ok_count, total = total)
}

family_defs <- list(
  gaussian = list(
    family = gaussian(),
    rfun = function(n) rnorm(n)
  ),
  poisson = list(
    family = poisson(),
    rfun = function(n) rpois(n, 2)
  ),
  nb = list(
    family = brms::negbinomial(),
    rfun = function(n) rnbinom(n, mu = 3, size = 1)
  ),
  bernoulli = list(
    family = bernoulli(),
    rfun = function(n) rbinom(n, 1L, 0.4)
  ),
  Beta = list(
    family = Beta(),
    rfun = function(n) {
      pmin(pmax(rbeta(n, 2, 5), 1e-4), 1 - 1e-4)
    }
  ),
  gamma = list(
    family = Gamma(link = "log"),
    rfun = function(n) rgamma(n, shape = 2, rate = 1)
  )
)

results <- list()
for (nm in names(family_defs)) {
  cat("\n=== Fitting", nm, "===\n")
  defn <- family_defs[[nm]]
  fit <- tryCatch(
    fit_one(nm, defn$family, defn$rfun),
    error = function(e) {
      cat("  [FAIL] fit error:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (!is.null(fit)) {
    results[[nm]] <- audit(nm, fit)
  }
}

cat("\n=== Summary ===\n")
for (nm in names(results)) {
  cat(format(nm, width = 12), ": ",
      results[[nm]]$ok, "/", results[[nm]]$total, " surfaces ok\n",
      sep = "")
}
