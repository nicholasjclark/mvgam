# Local PNG gallery for conditional_effects.mvgam across closure-
# unit and multi-response custom families. Renders each cached
# fixture's per-effect ggplot to PNG and writes them to
# /tmp/cond_effects_audit/ so a hand-audit can confirm the response
# scale, ribbon shape and per-series facets look right.
#
# String-grep assertions in tests/testthat/test-conditional-effects.R
# catch shape but cannot tell when the plot is conceptually wrong
# (e.g. an inverted axis, a degenerate flat curve, or a missing
# facet). This script is the visual gate.
#
# Local-only because the cached fixtures live in /tmp/ and
# tests/local/fixtures/, neither of which CI builds.
#
# Run with:
#   Rscript tests/local/test-cond-effects-gallery.R

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
})

out_dir <- "/tmp/cond_effects_audit"
dir.create(out_dir, showWarnings = FALSE)

render <- function(fit, fname, title) {
  cat("---", title, "---\n")
  ce <- try(conditional_effects(fit), silent = TRUE)
  if (inherits(ce, "try-error")) {
    cat("  ERROR:", attr(ce, "condition")$message, "\n\n")
    return(invisible(NULL))
  }
  cat("  effects produced:", paste(names(ce), collapse = ", "), "\n")
  p <- try(plot(ce, ask = FALSE), silent = TRUE)
  if (inherits(p, "try-error")) {
    cat("  PLOT ERROR:", attr(p, "condition")$message, "\n\n")
    return(invisible(NULL))
  }
  if (is.list(p)) {
    for (nm in names(p)) {
      ggsave(
        file.path(out_dir, sprintf("%s_%s.png", fname, nm)),
        plot = p[[nm]] + ggtitle(paste0(title, " : ", nm)),
        width = 7, height = 4.5, dpi = 130
      )
    }
  } else {
    ggsave(
      file.path(out_dir, sprintf("%s.png", fname)),
      plot = p + ggtitle(title), width = 7, height = 4.5, dpi = 130
    )
  }
  cat("  saved.\n\n")
}

fixtures <- list(
  list(
    path  = "tests/local/fixtures/val_occ_mvgam.rds",
    fname = "01_occ",
    title = "occ() : elev + tod_c"
  ),
  list(
    path  = "/tmp/diri_smoke_fit.rds",
    fname = "02_diri",
    title = "diri() : env * series"
  )
)

for (f in fixtures) {
  if (!file.exists(f$path)) {
    cat("SKIP (no fixture):", f$path, "\n\n")
    next
  }
  fit <- readRDS(f$path)
  render(fit, f$fname, f$title)
}

cat("Files written to", out_dir, ":\n")
print(list.files(out_dir, full.names = TRUE))
