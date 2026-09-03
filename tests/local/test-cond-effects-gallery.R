# Structural checks on `conditional_effects.mvgam()`, and the PNG
# gallery that used to stand here on its own.
#
# The gallery's premise was that a plot can only be judged by eye:
# that a grep over the CI tests catches shape, and that an inverted
# axis, a flat curve or a missing facet needs a person. Two of those
# three are numbers. An interval drawn the wrong way round is
# `conf.high < conf.low`; a facet cut by the wrong series is a `series`
# column that is not the model's axis. Reading them here leaves the
# renderer for the judgements that really are visual.
#
# The PNGs are still written, to /tmp/cond_effects_audit/, so the
# hand-audit is unchanged. What has gone is `try(silent = TRUE)` around
# every call: a total failure of `conditional_effects()` on every
# fixture used to print three lines and exit clean.
#
# Run with:
#   testthat::test_file("tests/local/test-cond-effects-gallery.R")

source("setup_tests_local.R")
source("concordance_helpers.R")

suppressMessages(library(ggplot2))

OUT_DIR <- "/tmp/cond_effects_audit"

# Maintained fixtures only. Two of the three this file used to drive
# were `/tmp` paths written by smoke scripts, so they were absent on
# any machine that had rebooted and the file reported them as skips.
CE_FIXTURES <- c("closure_labels_occ", "closure_labels_nmix",
                 "lv_factor", "normalize_on", "var_cor")


# One effect panel, held to what the gallery was looking for by eye.
expect_effect_sane <- function(p, fit, label) {
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_true(is.data.frame(d))
  expect_gt(nrow(d), 0L)
  expect_true(all(c("estimate", "conf.low", "conf.high") %in% names(d)))
  expect_true(all(is.finite(d$estimate)))

  # An interval drawn the wrong way round renders as a ribbon of the
  # right shape around the right line, and reads as correct until
  # someone checks which edge is which.
  expect_true(all(d$conf.low <= d$estimate))
  expect_true(all(d$estimate <= d$conf.high))

  # Where the panel is cut by series it is cut by the axis the model
  # was fitted on, in that order. A frame whose `series` column was
  # superseded by a grouping spells it another way, and a plot that
  # takes the column prints one series' name over another's estimates
  # while every value on the panel stays correct.
  if ("series" %in% names(d)) {
    levs <- as.character(mvgam_axes(fit)$series$levels)
    if (length(levs)) {
      expect_identical(as.character(unique(d$series)), levs)
    }
  }
}


# Render for the hand-audit. Not a check: failures here are the
# renderer's, and the assertions above have already run.
render_gallery <- function(ce, fname, title) {
  dir.create(OUT_DIR, showWarnings = FALSE)
  for (nm in names(ce)) {
    ggsave(
      file.path(OUT_DIR, sprintf("%s_%s.png", fname, nm)),
      plot = ce[[nm]] + ggtitle(paste0(title, " : ", nm)),
      width = 7, height = 4.5, dpi = 130
    )
  }
}


for (fx in CE_FIXTURES) {
  local({
    nm <- fx
    test_that(paste0("conditional_effects is drawable on ", nm), {
      require_fixtures(paste0("val_mvgam_", nm, ".rds"))
      fit <- load_mvgam(nm)
      ce <- conditional_effects(fit)
      expect_s3_class(ce, "mvgam_conditional_effects")

      # A fit whose observation formula carries no predictor has
      # nothing to condition on, and answers with no effects rather
      # than with an empty panel. That is the whole contract for such
      # a fit, so it is stated rather than passed over.
      preds <- tryCatch(unlist(insight::find_predictors(fit)),
                        error = function(e) character(0))
      if (!length(preds)) {
        expect_length(ce, 0L)
        return(invisible(NULL))
      }

      expect_gt(length(ce), 0L)
      for (eff in names(ce)) {
        expect_effect_sane(ce[[eff]], fit, label = paste(nm, eff))
      }
      render_gallery(ce, nm, nm)
    })
  })
}


test_that("a series effect separates the series it names", {
  # The panel a collapsed axis draws is the one every series shares,
  # and it is the panel this gallery was made to catch by eye: every
  # label present, every interval finite, one curve under all of them.
  require_fixtures("val_mvgam_closure_labels_occ.rds")
  fit <- load_mvgam("closure_labels_occ")
  ce <- conditional_effects(fit)
  expect_true("series" %in% names(ce))

  d <- ce[["series"]]$data
  levs <- as.character(mvgam_axes(fit)$series$levels)
  expect_identical(as.character(d$series), levs)
  expect_gt(length(unique(d$estimate)), 1L)
})
