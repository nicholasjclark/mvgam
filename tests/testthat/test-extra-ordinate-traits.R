# The tests of test-ordinate-traits.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("the sites are labelled by the fit's own time axis", {
  # Labels came from a column named `time` on the frame. A fit whose
  # time column is named otherwise had its sites numbered 1 to n.
  set.seed(9L)
  years <- c(2004L, 2001L, 2003L, 2002L, 2005L, 2000L)
  d <- expand.grid(year = years, sp = factor(c("a", "b", "c")))
  d$y <- rnorm(nrow(d))
  prefit <- mvgam(
    y ~ 1,
    trend_formula = ~ AR(p = 1, n_lv = 2, time = year, series = sp),
    data = d, family = gaussian(), run_model = FALSE, silent = 2,
    validate = FALSE
  )
  # The draws are the one thing a prefit lacks. The rotation is
  # replaced with scores of the right size, and the plot builder
  # hands back the labels it was given.
  local_mocked_bindings(
    ordinate_factor_components = function(...) {
      list(scores = matrix(0, length(years), 2L),
           loadings = matrix(0, 3L, 2L), n_lv = 2L)
    },
    ordinate_build_plot = function(comp, which_lvs, biplot,
                                   label_sites, site_names, ...) {
      site_names
    }
  )
  expect_identical(
    as.numeric(mvgam:::ordinate.mvgam(prefit)), as.numeric(sort(years))
  )
})



test_that("label_species labels the longest arrows, every arrow or none", {
  skip_if_not_installed("ggrepel")
  # Arrow s12 is the longest and s1 the shortest
  loadings <- cbind(seq(0.1, 1.2, by = 0.1), 0)
  comp <- list(scores = matrix(0, 3L, 2L), loadings = loadings)
  sp <- paste0("s", 1:12)
  labelled <- function(label_species) {
    p <- mvgam:::ordinate_build_plot(
      comp, 1:2, biplot = TRUE, label_sites = FALSE,
      site_names = 1:3, species_names = sp,
      label_species = label_species
    )
    repel <- Filter(function(l) "label" %in% names(l$data), p$layers)
    repel[[1L]]$data$label
  }
  expect_setequal(labelled("longest"), paste0("s", 3:12))
  expect_identical(labelled("all"), sp)
  expect_length(labelled("none"), 0L)
})
