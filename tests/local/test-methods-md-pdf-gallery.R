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
  region = factor(rep(c("r1", "r1", "r2", "r2"), each = 30L)),
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
       family = gaussian()),
  list(name = "13_mvbind",
       f = brms::bf(brms::mvbind(yA, yB) ~ x) + brms::set_rescor(FALSE),
       t = NULL,
       family = gaussian(),
       extra_data = list(
         yA = rnorm(120), yB = rnorm(120)
       )),
  list(name = "14_mvbind_rescor",
       f = brms::bf(brms::mvbind(yA, yB) ~ x) + brms::set_rescor(TRUE),
       t = NULL,
       family = gaussian(),
       extra_data = list(
         yA = rnorm(120), yB = rnorm(120)
       )),
  list(name = "15_nl_trait_slopes",
       f = brms::bf(yC ~ a + b * x,
                     a + b ~ trait1,
                     nl = TRUE),
       t = NULL,
       family = gaussian(),
       extra_data = list(
         yC = rnorm(120),
         trait1 = rep(rnorm(4), each = 30)
       )),
  list(name = "16_factor_iid",
       f = y ~ x,
       t = ~ AR(p = 1, n_lv = 2)),
  list(name = "17_factor_mgp",
       f = y ~ x,
       t = ~ AR(p = 1, n_lv = 2),
       loadings_prior = list(column_shrinkage = "mgp")),
  list(name = "18_ar_hier_cor",
       f = y ~ x,
       t = ~ AR(p = 1, gr = region, subgr = series)),
  list(name = "19_occ_single_season",
       f = brms::bf(y_occ ~ x, p ~ z),
       t = NULL,
       family = occ(),
       data_overrides = list(
         # Each (series, time) is one closure unit; we have
         # 4 units x 30 visits each.
         y_occ = rbinom(120, 1L, 0.4)
       )),
  list(name = "20_nmix_pb",
       f = brms::bf(y_nmix ~ x, p ~ z),
       t = NULL,
       family = nmix("poisson_binomial"),
       data_overrides = list(
         y_nmix = rpois(120, 3),
         cap = rep(10L, 120)
       )),
  list(name = "21_diri",
       use_jsdgam = TRUE,
       f = y_diri ~ x * series,
       factor_formula = ~ -1,
       family = diri(),
       n_lv = 2L,
       data_overrides = list(
         y_diri = local({
           y <- runif(120)
           sums <- tapply(y, rep(1:30, 4), sum)
           y / sums[match(rep(1:30, 4), names(sums))]
         })
       )),
  list(name = "22_mvn",
       use_jsdgam = TRUE,
       f = y_mvn ~ x,
       factor_formula = ~ -1,
       family = mvn(),
       n_lv = 1L,
       data_overrides = list(
         y_mvn = rnorm(120)
       ))
)

# Render each fixture through methods_md(), concatenate into one
# document, hand to pandoc + xelatex.
render_one <- function(spec) {
  fam <- spec$family %||% poisson()
  # Per-fixture column additions (e.g. mvbind needs yA / yB
  # alongside the shared base columns). Merged into the shared
  # dat so spec$f compiles without rewriting the gallery data.
  fixture_data <- dat
  for (nm in names(spec$extra_data %||% list())) {
    fixture_data[[nm]] <- spec$extra_data[[nm]]
  }
  for (nm in names(spec$data_overrides %||% list())) {
    fixture_data[[nm]] <- spec$data_overrides[[nm]]
  }
  # jsdgam fixtures (mvn / mvt / diri / multi / categ) need
  # `unit` and `species` instead of mvgam's series / time keys.
  if (isTRUE(spec$use_jsdgam)) {
    jsdgam_args <- list(
      formula = spec$f,
      factor_formula = spec$factor_formula %||% ~ -1,
      data = fixture_data,
      unit = quote(time), species = quote(series),
      family = fam,
      n_lv = spec$n_lv %||% 1L,
      run_model = FALSE, silent = 2, backend = "cmdstanr"
    )
    mod <- suppressWarnings(suppressMessages(
      do.call(jsdgam, jsdgam_args)
    ))
    return(methods_md(mod))
  }
  mvgam_args <- list(
    formula = spec$f, trend_formula = spec$t,
    data = fixture_data, family = fam,
    run_model = FALSE, silent = 2
  )
  if (!is.null(spec$loadings_prior)) {
    mvgam_args$loadings_prior <- spec$loadings_prior
  }
  mod <- suppressWarnings(suppressMessages(
    do.call(mvgam, mvgam_args)
  ))
  methods_md(mod)
}

blocks <- vapply(fixtures, function(s) {
  cat("rendering fixture", s$name, "...\n")
  body <- render_one(s)
  # `formula_text()` reconstructs brmsformula / mvbrmsformula
  # back to a `brms::bf(...) [+ brms::set_rescor(...)]` string;
  # raw `deparse()` on a brms object would spit the `structure(
  # list(formula = ..., pforms = ...))` internal form into the
  # header.
  title <- mvgam:::formula_text(s$f)
  paste0("\n# Fixture: ", s$name, " -- `", title, "`\n\n",
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
