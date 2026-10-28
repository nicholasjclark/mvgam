# The trait-overlay helpers of ordinate() are linear algebra on the
# rotated loadings. A synthetic Z and a trait frame exercise them
# without a Stan fit.

test_that("ordinate_trait_arrows() returns NULL when no numeric traits remain", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(
    diet = factor(c("herb", "carn", "herb", "omni", "carn"))
  )
  out <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  expect_null(out)
})

test_that("ordinate_trait_arrows() returns one row per numeric trait", {
  set.seed(7L)
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(
    body_size = c(1.0, 2.5, 4.0, 3.0, 1.5),
    fecundity = c(50, 30, 10, 25, 60)
  )
  out <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  expect_named(out, c("x", "y", "trait_name"))
  # Row order follows the trait columns. `expect_setequal()` passed
  # on a swap, which puts each arrow on the other trait.
  expect_identical(out$trait_name, c("body_size", "fecundity"))
})

test_that("ordinate_trait_arrows() aligns rows via rownames when set", {
  set.seed(11L)
  sp <- c("rare", "common", "intermediate", "edge", "core")
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  rownames(Z) <- sp
  # Traits supplied in a deliberately scrambled order.
  traits <- data.frame(
    body_size = c(5, 2, 4, 3, 1)
  )
  rownames(traits) <- c("core", "rare", "edge", "intermediate", "common")
  out <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = sp, arrow_scale = 1
  )
  expect_identical(out$trait_name, "body_size")
  # The same traits, already in `sp` order, give the same arrow.
  aligned <- data.frame(body_size = c(2, 1, 3, 4, 5))
  rownames(aligned) <- sp
  expect_equal(
    out[, c("x", "y")],
    mvgam:::ordinate_trait_arrows(
      traits = aligned, loadings_2d = Z,
      species_names = sp, arrow_scale = 1
    )[, c("x", "y")]
  )
  # Taking the scrambled values in order gives a different arrow,
  # which is what a helper skipping the rownames would return.
  verbatim <- data.frame(body_size = c(5, 2, 4, 3, 1))
  rownames(verbatim) <- sp
  expect_false(isTRUE(all.equal(
    out[, c("x", "y")],
    mvgam:::ordinate_trait_arrows(
      traits = verbatim, loadings_2d = Z,
      species_names = sp, arrow_scale = 1
    )[, c("x", "y")]
  )))
})

test_that("ordinate_trait_arrows() errors when row count mismatches", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(body_size = c(1, 2, 3))  # 3 != 5
  expect_error(
    mvgam:::ordinate_trait_arrows(
      traits = traits, loadings_2d = Z,
      species_names = paste0("sp", 1:5),
      arrow_scale = 1
    ),
    "'traits' needs one row per training series"
  )
})

test_that("ordinate_trait_arrows() errors when some species are missing", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(body_size = c(1, 2, 3, 4, 5))
  rownames(traits) <- paste0("sp", 1:5)
  expect_error(
    mvgam:::ordinate_trait_arrows(
      traits = traits, loadings_2d = Z,
      species_names = c("sp1", "sp2", "sp3", "missing", "sp5"),
      arrow_scale = 1
    ),
    "must list every training series exactly once"
  )
})

test_that("ordinate_trait_arrows() errors on non-data input", {
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  expect_error(
    mvgam:::ordinate_trait_arrows(
      traits = "not a data frame", loadings_2d = Z,
      species_names = paste0("sp", 1:5),
      arrow_scale = 1
    ),
    "must be a data.frame or matrix"
  )
})

test_that("ordinate_trait_arrows() respects trait_arrow_scale", {
  set.seed(3L)
  Z <- matrix(rnorm(10L), nrow = 5L, ncol = 2L)
  traits <- data.frame(body_size = c(1, 5, 2, 4, 3))
  out_one <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 1
  )
  out_two <- mvgam:::ordinate_trait_arrows(
    traits = traits, loadings_2d = Z,
    species_names = paste0("sp", 1:5),
    arrow_scale = 2
  )
  r_one <- sqrt(out_one$x^2 + out_one$y^2)
  r_two <- sqrt(out_two$x^2 + out_two$y^2)
  expect_equal(r_two / r_one, rep(2, length(r_one)), tolerance = 1e-8)
})

test_that("resolve_auto_traits() passes a trait frame through", {
  traits <- data.frame(body_size = c(1, 2, 3))
  expect_identical(mvgam:::resolve_auto_traits(traits, list()), traits)
})

test_that("resolve_auto_traits('auto') pulls from a trait-informed fit", {
  features <- data.frame(body_size = c(1, 2), row.names = c("sp1", "sp2"))
  spec <- structure(
    list(trend = "ZMVN", loadings_prior = list(features = features)),
    class = "mvgam_trend"
  )
  stub_fit <- list(mv_spec = list(trend_specs = spec))
  out <- mvgam:::resolve_auto_traits("auto", stub_fit)
  expect_equal(out, features)
})

test_that("resolve_auto_traits('auto') notices a fit with no traits", {
  spec <- structure(list(trend = "ZMVN"), class = "mvgam_trend")
  stub_fit <- list(mv_spec = list(trend_specs = spec))
  # The overlay the caller asked for is absent from the returned
  # plot, so every call earns the notice and not just the first.
  expect_warning(
    out <- mvgam:::resolve_auto_traits("auto", stub_fit),
    "requires a fit built with traits"
  )
  expect_null(out)
})

test_that("resolve_auto_traits() errors on unknown string", {
  stub_fit <- list()
  expect_error(
    mvgam:::resolve_auto_traits("nonsense", stub_fit),
    "string value must be 'auto'"
  )
})

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
