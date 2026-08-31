# The class pages describe the shape of what mvgam hands back, so
# they go stale the moment a slot is added or renamed and nothing
# says so. `?mvgam-class` described the 1.x object for the whole of
# the rebuild: seventeen of its twenty-two slots were gone and
# twenty-three that a fit carries were unmentioned.
#
# These read the roxygen bullets straight out of `R/*-class.R` and
# check them against objects built from cached fits, so the doc and
# the object cannot drift apart unnoticed.

source(test_path("concordance_helpers.R"))


# Slot names a class page documents. The pages use three bullet
# styles between them (`- \`name\``, `\item \`name\``, and
# `\item{name}{}`), plus prose that names a slot inline, so rather
# than matching one bullet form this reads every backticked bare
# identifier plus every `\item{}` label. That over-collects a
# little, which is the safe
# direction: the check below asks whether a slot the object carries
# is mentioned at all, and a slot nobody has written about will not
# appear under any style.
documented_slots <- function(file) {
  path <- file.path("..", "..", "R", file)
  if (!file.exists(path)) path <- file.path("R", file)
  lines <- grep("^#'", readLines(path, warn = FALSE), value = TRUE)
  ticked <- gsub("`", "", unlist(regmatches(
    lines, gregexpr("`[^`]+`", lines)
  )))
  # `\item{a, b}{...}` names its slots without backticks.
  items <- unlist(regmatches(
    lines, gregexpr("(?<=\\\\item\\{)[^}]+(?=\\})", lines, perl = TRUE)
  ))
  # Split the bullets that name several slots at once.
  named <- trimws(unlist(strsplit(c(ticked, items), ",[[:space:]]*")))
  unique(named[grepl("^[A-Za-z._][A-Za-z0-9._]*$", named)])
}


test_that("every slot a fitted mvgam carries is documented", {
  require_fixtures("val_mvgam_var_cor.rds", "val_mvgam_ar1_fx.rds")
  documented <- documented_slots("mvgam-class.R")
  expect_gt(length(documented), 20L)
  for (nm in c("var_cor", "ar1_fx")) {
    fit <- load_mvgam(nm)
    expect_setequal(intersect(names(fit), documented), names(fit))
  }
})


test_that("every slot a jsdgam adds on top is documented", {
  require_fixtures("val_jsdgam_trait.rds")
  documented <- documented_slots("mvgam-class.R")
  fit <- readRDS(
    file.path(local_fixture_dir(), "val_jsdgam_trait.rds")
  )
  expect_setequal(intersect(names(fit), documented), names(fit))
})


test_that("every element of a residual correlation is documented", {
  require_fixtures("val_mvgam_var_cor.rds")
  documented <- documented_slots("mvgam_residcor-class.R")
  fit <- load_mvgam("var_cor")
  # Both surfaces, since the partial one is only built on request.
  for (partial in c(FALSE, TRUE)) {
    rc <- residual_cor(fit, partial = partial)
    expect_setequal(intersect(names(rc), documented), names(rc))
  }
})


test_that("a forecast reports the elements its class page lists", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  documented <- documented_slots("mvgam_forecast-class.R")
  hc <- hindcast(load_mvgam("ar1_fx"))
  expect_setequal(intersect(names(hc), documented), names(hc))
})


test_that("irf and fevd return the classes their pages name", {
  require_fixtures("val_mvgam_var_cor.rds")
  fit <- load_mvgam("var_cor")
  # The summary is the default and the draws sit behind
  # `summary = FALSE`; the pages described only the draws.
  expect_s3_class(irf(fit, h = 3, ndraws = 10), "mvgam_irf_summary")
  expect_s3_class(irf(fit, h = 3, ndraws = 10, summary = FALSE), "mvgam_irf")
  expect_s3_class(fevd(fit, h = 3, ndraws = 10), "mvgam_fevd_summary")
  expect_s3_class(fevd(fit, h = 3, ndraws = 10, summary = FALSE), "mvgam_fevd")

  irf_cols <- names(irf(fit, h = 3, ndraws = 10))
  expect_true(all(c("shock", "horizon", "irfQ50") %in% irf_cols))
  fevd_cols <- names(fevd(fit, h = 3, ndraws = 10))
  expect_true(all(c("shock", "horizon", "fevdQ50") %in% fevd_cols))
})
