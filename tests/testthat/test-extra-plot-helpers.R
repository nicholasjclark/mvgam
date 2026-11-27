# The tests of test-plot-helpers.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("no plot writes a scheme colour as a literal", {
  # Every hex the bayesplot schemes carry. A colour comes from the
  # scheme by its role.
  schemes <- c("red", "blue", "green", "purple", "teal", "gray",
               "yellow", "orange")
  scheme_hex <- unique(toupper(unlist(lapply(
    schemes, function(s) unname(unlist(bayesplot::color_scheme_get(s)))
  ))))
  r_files <- list.files(
    test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE
  )
  if (!length(r_files)) {
    r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  }
  offenders <- character(0)
  for (f in r_files) {
    if (basename(f) == "plot_helpers.R") next
    lines <- readLines(f, warn = FALSE)
    code <- grep("^\\s*#", lines, value = TRUE, invert = TRUE)
    for (hx in scheme_hex) {
      if (any(grepl(hx, toupper(code), fixed = TRUE))) {
        offenders <- c(offenders, paste0(basename(f), ": ", hx))
      }
    }
  }
  expect_equal(offenders, character(0))
})
