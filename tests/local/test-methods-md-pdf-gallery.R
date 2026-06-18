# Local PDF render check for methods_md(fit) output.
#
# String-grep assertions in tests/testthat/test-methods-md.R catch
# math content but cannot tell when a LaTeX construct is malformed
# (an upstream pandoc + xelatex pass is the only thing that does).
# This script renders the full effect gallery to PDF and fails
# loudly if pandoc returns non-zero or the PDF is empty.
#
# Local-only because pandoc / xelatex are not available in CI.
#
# Run with:
#   Rscript tests/local/test-methods-md-pdf-gallery.R
#
# Add a fixture below whenever a new renderer (effect type,
# family, trend variant) lands so the PDF gate catches LaTeX
# regressions that the grep assertions miss.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
})

# Skip cleanly when the required tools are not on PATH; this is a
# local-only test and not all developer boxes have pandoc.
need <- c("pandoc", "xelatex")
missing_tools <- need[Sys.which(need) == ""]
if (length(missing_tools) > 0L) {
  cat("SKIP: missing required tools:",
      paste(missing_tools, collapse = ", "), "\n")
  quit(status = 0L)
}

set.seed(1L)
n <- 120L
dat <- data.frame(
  time = rep(1:30, 4L),
  series = factor(rep(paste0("s", 1:4), each = 30L)),
  x = rnorm(n), z = rnorm(n),
  sdx = abs(rnorm(n, 0.2, 0.05)),
  ord = factor(sample(1:5, n, replace = TRUE), ordered = TRUE),
  grp = factor(rep(c("a", "b", "c", "d"), each = 30L)),
  y = rpois(n, 3)
)

fixtures <- list(
  list(name = "01_basic",      f = y ~ x,                            t = ~ AR(p = 1)),
  list(name = "02_smooth_re",  f = y ~ s(x, k = 5) + (1 | grp),      t = ~ AR(p = 1)),
  list(name = "03_tensor_t2",  f = y ~ t2(x, z, k = 4),              t = NULL),
  list(name = "04_gp_1d",      f = y ~ gp(x, k = 5),                 t = NULL),
  list(name = "05_gp_2d",      f = y ~ gp(x, z, k = 5),              t = NULL),
  list(name = "06_gp_by",      f = y ~ gp(x, by = grp, k = 5),       t = NULL),
  list(name = "07_gp_exact",   f = y ~ gp(x),                        t = NULL),
  list(name = "08_vslope",     f = y ~ x + (x | grp),                t = NULL),
  list(name = "09_mo",         f = y ~ mo(ord),                      t = NULL),
  list(name = "10_me",         f = y ~ me(x, sdx),                   t = NULL),
  list(name = "11_kitchen",
       f = y ~ x + s(z, k = 4) + gp(x, k = 5) + mo(ord) + (1 | grp),
       t = ~ AR(p = 2)),
  list(name = "12_dpar_sigma",
       f = brms::bf(y ~ x, sigma ~ x),
       t = NULL,
       family = gaussian())
)

# Render each fixture through methods_md(), concatenate into one
# document, hand to pandoc + xelatex.
render_one <- function(spec) {
  fam <- spec$family %||% poisson()
  mod <- suppressWarnings(suppressMessages(mvgam(
    formula = spec$f, trend_formula = spec$t,
    data = dat, family = fam,
    run_model = FALSE, silent = 2
  )))
  methods_md(mod)
}

blocks <- vapply(fixtures, function(s) {
  cat("rendering fixture", s$name, "...\n")
  body <- render_one(s)
  paste0("\n# Fixture: ", s$name, " -- `",
         paste(deparse(s$f), collapse = " "), "`\n\n",
         body, "\n\n")
}, character(1L))

md_path <- tempfile(fileext = ".md")
pdf_path <- tempfile(fileext = ".pdf")
writeLines(c(
  "---",
  "title: methods_md effect gallery",
  "geometry: margin=1in",
  "header-includes: |",
  "  \\usepackage{amsmath}",
  "---",
  "",
  blocks
), md_path)

cat("\nrunning pandoc -> xelatex ...\n")
log_path <- tempfile(fileext = ".log")
status <- system2(
  "pandoc",
  c(shQuote(md_path), "-o", shQuote(pdf_path),
    "--pdf-engine=xelatex"),
  stdout = log_path, stderr = log_path
)
if (status != 0L) {
  cat("\nPANDOC OUTPUT (tail):\n")
  cat(tail(readLines(log_path, warn = FALSE), 40), sep = "\n")
  stop("pandoc failed with status ", status,
       "; see log at ", log_path)
}
info <- file.info(pdf_path)
if (is.na(info$size) || info$size < 1024) {
  stop("pandoc produced an empty / truncated PDF at ", pdf_path)
}

cat("\nOK -- gallery rendered cleanly:\n")
cat("  markdown:", md_path, "\n")
cat("  pdf:     ", pdf_path, "(", info$size, " bytes)\n")
