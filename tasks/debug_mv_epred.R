# Debug script for multivariate posterior_epred family extraction
# Run: Rscript tasks/debug_mv_epred.R

library(mvgam)
library(brms)

cat("=== Debug: Multivariate posterior_epred ===\n\n")

# Check if cached model exists
mv_path <- "tasks/fixtures/val_mvgam_mv_gauss.rds"
if (!file.exists(mv_path)) {
  cat("Multivariate model not found. Fitting fresh model...\n")

  set.seed(123)
  n_mv <- 30
  latent_mv <- numeric(n_mv)
  latent_mv[1] <- rnorm(1, 0, 0.5)
  for (t in 2:n_mv) {
    latent_mv[t] <- 0.6 * latent_mv[t - 1] + rnorm(1, 0, 0.4)
  }

  test_data_mv <- data.frame(
    y1 = rnorm(n_mv, 2 + 0.5 * (1:n_mv / n_mv) + latent_mv, 0.3),
    y2 = rnorm(n_mv, 1 - 0.3 * (1:n_mv / n_mv) + latent_mv, 0.4),
    x = (1:n_mv) / n_mv,
    time = 1:n_mv,
    series = factor("s1")
  )

  mvgam_11 <- mvgam(
    formula = bf(y1 ~ x) + bf(y2 ~ x),
    trend_formula = ~ AR(p = 1),
    data = test_data_mv,
    family = gaussian(),
    chains = 2, iter = 500, backend = "cmdstanr"
  )

  saveRDS(mvgam_11, mv_path)
} else {
  cat("Loading cached multivariate model...\n")
  mvgam_11 <- readRDS(mv_path)
}

cat("\n=== Step 1: Basic object structure ===\n")
cat("Class of mvgam_11:", class(mvgam_11), "\n")

cat("\n=== Step 2: Top-level family ===\n")
cat("mvgam_11$family exists:", !is.null(mvgam_11$family), "\n")
if (!is.null(mvgam_11$family)) {
  cat("Class:", class(mvgam_11$family), "\n")
  cat("Is list:", is.list(mvgam_11$family), "\n")
  if (is.list(mvgam_11$family)) {
    cat("Names:", names(mvgam_11$family), "\n")
    if (!is.null(mvgam_11$family$family)) {
      cat("$family:", mvgam_11$family$family, "\n")
    }
    if (!is.null(mvgam_11$family$link)) {
      cat("$link:", mvgam_11$family$link, "\n")
    }
    cat("Has $linkinv:", !is.null(mvgam_11$family$linkinv), "\n")
  }
}

cat("\n=== Step 3: Formula structure ===\n")
cat("mvgam_11$formula exists:", !is.null(mvgam_11$formula), "\n")
if (!is.null(mvgam_11$formula)) {
  cat("Class:", class(mvgam_11$formula), "\n")
  cat("Inherits mvbrmsformula:", inherits(mvgam_11$formula, "mvbrmsformula"), "\n")
  cat("Has $forms:", !is.null(mvgam_11$formula$forms), "\n")
  cat("Has $responses:", !is.null(mvgam_11$formula$responses), "\n")

  if (!is.null(mvgam_11$formula$responses)) {
    cat("Responses:", mvgam_11$formula$responses, "\n")
  }

  if (!is.null(mvgam_11$formula$forms)) {
    cat("Forms names:", names(mvgam_11$formula$forms), "\n")
    cat("Length of forms:", length(mvgam_11$formula$forms), "\n")
  }
}

cat("\n=== Step 4: Per-response family extraction ===\n")
if (!is.null(mvgam_11$formula$forms)) {
  for (nm in names(mvgam_11$formula$forms)) {
    cat("\n--- Response:", nm, "---\n")
    form <- mvgam_11$formula$forms[[nm]]
    cat("  Form class:", class(form), "\n")
    cat("  Form has $family:", !is.null(form$family), "\n")

    if (!is.null(form$family)) {
      fam <- form$family
      cat("  Family class:", class(fam), "\n")
      cat("  Family is list:", is.list(fam), "\n")
      if (is.list(fam)) {
        cat("  Family names:", names(fam), "\n")
        cat("  Family$family:", fam$family, "\n")
        cat("  Family$link:", fam$link, "\n")
        cat("  Has $linkinv:", !is.null(fam$linkinv), "\n")
      }
    } else {
      cat("  Family is NULL - checking alternatives...\n")
      cat("  Form names:", names(form), "\n")
    }
  }
}

cat("\n=== Step 5: Test multivariate detection logic ===\n")
is_multivariate <- inherits(mvgam_11$formula, "mvbrmsformula") &&
  !is.null(mvgam_11$formula$forms) &&
  length(mvgam_11$formula$forms) > 1
cat("Is multivariate:", is_multivariate, "\n")

cat("\n=== Step 6: Attempt family extraction (as in posterior_epred.mvgam) ===\n")
if (is_multivariate) {
  cat("Extracting per-response families...\n")
  family <- tryCatch({
    lapply(mvgam_11$formula$forms, function(f) f$family)
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(family)) {
    cat("Extraction succeeded\n")
    cat("Family names:", names(family), "\n")
    for (nm in names(family)) {
      cat("\n  Response", nm, ":\n")
      fam <- family[[nm]]
      cat("    Is NULL:", is.null(fam), "\n")
      if (!is.null(fam)) {
        cat("    Has $family:", !is.null(fam$family), "\n")
        cat("    Has $linkinv:", !is.null(fam$linkinv), "\n")
        if (!is.null(fam$family)) cat("    $family:", fam$family, "\n")
      }
    }
  }
} else {
  cat("Using univariate family: mvgam_11$family\n")
}

cat("\n=== Step 7: Test get_combined_linpred ===\n")
test_data_mv <- mvgam_11$data
linpred <- tryCatch({
  get_combined_linpred(
    mvgam_fit = mvgam_11,
    newdata = test_data_mv,
    process_error = TRUE,
    ndraws = 10
  )
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(linpred)) {
  cat("Linpred extraction succeeded\n")
  cat("Is list:", is.list(linpred) && !is.matrix(linpred), "\n")
  if (is.list(linpred) && !is.matrix(linpred)) {
    cat("Names:", names(linpred), "\n")
    for (nm in names(linpred)) {
      cat("  ", nm, "dims:", dim(linpred[[nm]]), "\n")
    }
  }
}

cat("\n=== Step 8: Test posterior_epred.mvgam ===\n")
epred <- tryCatch({
  posterior_epred(mvgam_11, newdata = test_data_mv, ndraws = 10)
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(epred)) {
  cat("posterior_epred succeeded!\n")
  if (is.list(epred) && !is.matrix(epred)) {
    cat("Names:", names(epred), "\n")
    for (nm in names(epred)) {
      cat("  ", nm, "dims:", dim(epred[[nm]]), "\n")
    }
  } else {
    cat("Dims:", dim(epred), "\n")
  }
}

cat("\n=== Debug complete ===\n")
