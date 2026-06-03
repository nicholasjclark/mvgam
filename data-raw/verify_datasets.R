# Fit the recommended model for each new stock dataset and verify
# that the headline generative patterns are recoverable. Produces:
#
#   data-raw/verify_outputs/<dataset>_summary.txt      model summary
#   data-raw/verify_outputs/<dataset>_ppcheck.png      pp_check
#   data-raw/verify_outputs/<dataset>_ceffs.png        conditional_effects
#   data-raw/verify_outputs/<dataset>_predictions.png  plot_predictions
#   data-raw/verify_outputs/<dataset>_traces.png       MCMC trace
#   data-raw/verify_outputs/<dataset>_diagnostics.txt  Rhat/ESS sanity
#   data-raw/verify_outputs/<dataset>_recovery.txt     truth-vs-posterior
#
# Run from the package root:
#   Rscript data-raw/verify_datasets.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(bayesplot)
})
devtools::load_all()

# Match the mvgam house style: red bayesplot scheme + clean ggplot
# theme. (mvgam's internal plotting methods already override these,
# but we set them explicitly so any ad-hoc plots inherit the same
# look.)
bayesplot::color_scheme_set("red")
ggplot2::theme_set(ggplot2::theme_classic())

out_dir <- "data-raw/verify_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_block <- function(path, header, body) {
  writeLines(
    c(paste0("===== ", header, " ====="), "", body, ""),
    con = path
  )
}

save_plot <- function(p, path, width = 7, height = 5) {
  ggplot2::ggsave(
    path, plot = p, width = width, height = height, dpi = 110
  )
}

# Save a trace plot for the headline parameters of a fit, using
# mvgam::mcmc_plot which already overrides the bayesplot scheme
# to red.
save_traces <- function(fit, pars, path, width = 9, height = 6) {
  p <- mcmc_plot(fit, variable = pars, regex = TRUE,
                  type = "trace")
  save_plot(p, path, width = width, height = height)
}

# Quick Rhat / ESS sanity check on the headline parameters. Writes
# a one-line PASS or a multi-line WARN to `path`.
check_diagnostics <- function(fit, pars, path) {
  smry <- posterior::summarise_draws(
    as_draws_df(fit, variable = pars, regex = TRUE),
    "rhat", "ess_bulk", "ess_tail"
  )
  bad_rhat <- smry$rhat > 1.05 | is.na(smry$rhat)
  bad_ess <- smry$ess_bulk < 100 | is.na(smry$ess_bulk)
  lines <- if (!any(bad_rhat) && !any(bad_ess)) {
    paste0("PASS: ", nrow(smry),
           " headline parameter rows all have ",
           "rhat <= 1.05 and ess_bulk >= 100.")
  } else {
    c("WARN: some headline parameters have weak diagnostics.",
      capture.output(print(smry[bad_rhat | bad_ess, ])))
  }
  writeLines(lines, con = path)
}


# ----- 1. birdsong (hierarchical AR(1) with cor = TRUE) -----

message("\n=== birdsong fit ===")
data("birdsong")
fit_bs <- mvgam(
  formula = y ~ s(week_in_year, bs = "cc", k = 8) +
    s(species, bs = "re"),
  trend_formula = ~ AR(p = 1, cor = TRUE),
  data = birdsong,
  family = poisson(),
  chains = 1L,
  iter = 800L,
  warmup = 400L,
  refresh = 0L,
  silent = 2L,
  backend = "cmdstanr"
)

write_block(
  file.path(out_dir, "birdsong_summary.txt"),
  "birdsong: mvgam summary",
  capture.output(summary(fit_bs))
)

pp_bs <- pp_check(fit_bs, type = "dens_overlay", ndraws = 30L)
save_plot(pp_bs, file.path(out_dir, "birdsong_ppcheck.png"))

ce_bs <- marginaleffects::plot_predictions(
  fit_bs, condition = "week_in_year", type = "response"
) + ggplot2::ggtitle("birdsong: shared seasonal cycle")
save_plot(ce_bs, file.path(out_dir, "birdsong_ceffs.png"))

pred_bs <- marginaleffects::plot_predictions(
  fit_bs,
  condition = list("week_in_year", "species"),
  type = "response"
) + ggplot2::ggtitle("birdsong: predicted weekly counts by species")
save_plot(pred_bs, file.path(out_dir, "birdsong_predictions.png"),
          width = 9, height = 5)

save_traces(
  fit_bs,
  pars = c("ar1", "sigma_trend", "Sigma_trend"),
  path = file.path(out_dir, "birdsong_traces.png")
)
check_diagnostics(
  fit_bs,
  pars = c("ar1", "sigma_trend", "Sigma_trend", "b_"),
  path = file.path(out_dir, "birdsong_diagnostics.txt")
)


# Recovery: extract the estimated AR(1) cross-series correlation
# matrix and compare to the truth used in build_birdsong.R.
true_cor <- matrix(
  c( 1,    0.7, -0.7, -0.7,
     0.7,  1,   -0.7, -0.7,
    -0.7, -0.7,  1,    0.7,
    -0.7, -0.7,  0.7,  1),
  nrow = 4, byrow = TRUE
)
post_corr <- posterior::summarise_draws(
  as_draws_df(fit_bs, variable = "^Sigma_trend", regex = TRUE)
)
write_block(
  file.path(out_dir, "birdsong_recovery.txt"),
  "birdsong: AR(1) cross-species correlation recovery",
  c(
    "Truth (innovation correlation matrix):",
    capture.output(print(round(true_cor, 2))),
    "",
    "Posterior summary of Sigma_trend (if present):",
    if (is.null(post_corr)) "<could not extract>" else
      capture.output(print(post_corr))
  )
)


# ----- 2. lake_chemistry (state-space, RW(cor = TRUE)) -----

message("\n=== lake_chemistry fit ===")
data("lake_chemistry")
fit_lc <- mvgam(
  formula = y ~ treated + (treated || lake),
  trend_formula = ~ RW(cor = TRUE),
  data = lake_chemistry,
  family = gaussian(),
  chains = 1L,
  iter = 800L,
  warmup = 400L,
  refresh = 0L,
  silent = 2L,
  backend = "cmdstanr"
)

write_block(
  file.path(out_dir, "lake_chemistry_summary.txt"),
  "lake_chemistry: mvgam summary",
  capture.output(summary(fit_lc))
)

pp_lc <- pp_check(fit_lc, type = "dens_overlay", ndraws = 30L)
save_plot(pp_lc, file.path(out_dir, "lake_chemistry_ppcheck.png"))

ce_lc <- marginaleffects::plot_predictions(
  fit_lc, condition = list("treated", "lake"), type = "response"
) + ggplot2::ggtitle(
  "lake_chemistry: treatment effect by lake (heterogeneous slopes)"
)
save_plot(ce_lc, file.path(out_dir, "lake_chemistry_ceffs.png"),
          width = 9, height = 5)

pred_lc <- marginaleffects::plot_predictions(
  fit_lc,
  condition = list("month", "lake"),
  type = "response"
) + ggplot2::ggtitle(
  "lake_chemistry: predicted chemistry by lake and month"
)
save_plot(pred_lc,
          file.path(out_dir, "lake_chemistry_predictions.png"),
          width = 11, height = 6)

save_traces(
  fit_lc,
  pars = c("b_treated", "sd_lake", "sigma_trend", "sigma_obs"),
  path = file.path(out_dir, "lake_chemistry_traces.png")
)
check_diagnostics(
  fit_lc,
  pars = c("b_treated", "sd_lake", "r_lake",
            "sigma_trend", "sigma_obs"),
  path = file.path(out_dir, "lake_chemistry_diagnostics.txt")
)

# Recovery: the truth has heterogeneous treatment slopes
# (alpine = +0.8, boreal = +0.6, coastal = -0.5, delta = -0.7,
# estuary = -0.6). The recommended fit estimates a single
# aggregate `treated` effect, expected to be near the mean
# (around -0.16) with a wide CI because the latent state absorbs
# the lake-level variation. Report the posterior summary of the
# `treated` coefficient.
write_block(
  file.path(out_dir, "lake_chemistry_recovery.txt"),
  "lake_chemistry: treatment effect recovery",
  c(
    "Truth (per-lake slopes):",
    "  alpine  = +0.8",
    "  boreal  = +0.6",
    "  coastal = -0.5",
    "  delta   = -0.7",
    "  estuary = -0.6",
    "  mean    = -0.16",
    "",
    "Fixed treatment coefficient (population mean slope):",
    capture.output(print(
      posterior::summarise_draws(
        as_draws_df(fit_lc, variable = "b_treated", regex = TRUE)
      )
    )),
    "",
    "Per-lake random treatment deviations (treated | lake):",
    capture.output(print(
      posterior::summarise_draws(
        as_draws_df(fit_lc, variable = "r_lake", regex = TRUE)
      )
    ))
  )
)


# ----- 3. coral_surveys (state-space, CAR(1)) -----

message("\n=== coral_surveys fit ===")
data("coral_surveys")
fit_cs <- mvgam(
  formula = y ~ s(sst, k = 8),
  trend_formula = ~ CAR(),
  data = coral_surveys,
  family = gaussian(),
  chains = 1L,
  iter = 800L,
  warmup = 400L,
  refresh = 0L,
  silent = 2L,
  backend = "cmdstanr"
)

write_block(
  file.path(out_dir, "coral_surveys_summary.txt"),
  "coral_surveys: mvgam summary",
  capture.output(summary(fit_cs))
)

pp_cs <- pp_check(fit_cs, type = "dens_overlay", ndraws = 30L)
save_plot(pp_cs, file.path(out_dir, "coral_surveys_ppcheck.png"))

ce_cs <- marginaleffects::plot_predictions(
  fit_cs, condition = "sst", type = "response"
) + ggplot2::ggtitle("coral_surveys: SST -> bleaching smooth")
save_plot(ce_cs, file.path(out_dir, "coral_surveys_ceffs.png"),
          width = 8, height = 5)

pred_cs <- marginaleffects::plot_predictions(
  fit_cs,
  condition = list("sst", "reef"),
  type = "response"
) + ggplot2::ggtitle(
  "coral_surveys: predicted bleaching vs SST by reef"
)
save_plot(pred_cs,
          file.path(out_dir, "coral_surveys_predictions.png"),
          width = 10, height = 6)

save_traces(
  fit_cs,
  pars = c("ar1", "sigma_trend", "sigma_obs", "bs_sst"),
  path = file.path(out_dir, "coral_surveys_traces.png")
)
check_diagnostics(
  fit_cs,
  pars = c("ar1", "sigma_trend", "sigma_obs", "bs_sst"),
  path = file.path(out_dir, "coral_surveys_diagnostics.txt")
)

# Recovery: true phi = 0.7. The CAR fit estimates a per-reef
# `ar1` parameter; report the posterior for each.
write_block(
  file.path(out_dir, "coral_surveys_recovery.txt"),
  "coral_surveys: CAR(1) phi recovery",
  c(
    "Truth: phi = 0.7, sigma = 0.5 (shared across reefs).",
    "",
    "Posterior summary of ar1 (per-reef):",
    capture.output(print(
      posterior::summarise_draws(
        as_draws_df(fit_cs, variable = "ar1", regex = TRUE)
      )
    )),
    "",
    "Posterior summary of sigma:",
    capture.output(print(
      posterior::summarise_draws(
        as_draws_df(fit_cs, variable = "sigma", regex = TRUE)
      )
    ))
  )
)

message("\nAll three fits done. Outputs in ", out_dir)
