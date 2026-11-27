# The tests of test-posterior-smooths.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# Draws with a value for every name, two draws, as a draws matrix.
stub_draws <- function(names) {
  set.seed(9L)
  posterior::as_draws_matrix(matrix(
    rnorm(2L * length(names)), nrow = 2L,
    dimnames = list(NULL, names)
  ))
}

# The coefficient names of one smooth object of a predictor, as brms
# writes them, and the basis times the coefficients by hand.
smooth_by_hand <- function(sdata, draws, sfx, object) {
  cols <- attr(sdata[[paste0("Xs", sfx)]], "smcols")[[object]]
  eta <- as.matrix(draws[, paste0("bs", sfx, "[", cols, "]")]) %*%
    t(sdata[[paste0("Xs", sfx)]][, cols, drop = FALSE])
  for (j in seq_len(sdata[[paste0("nb", sfx, "_", object)]])) {
    Zs <- sdata[[paste0("Zs", sfx, "_", object, "_", j)]]
    eta <- eta + as.matrix(draws[, paste0("s", sfx, "_", object, "_", j,
                                          "[", seq_len(ncol(Zs)), "]")]) %*%
      t(Zs)
  }
  unname(eta)
}

# ---- The smooth record ----------------------------------------------

test_that("each predictor's smooths are listed, the trend's last", {
  # A distributional parameter's smooths are a predictor of their own,
  # and the trend's follow the observation model's.
  stub <- smooth_stub(brms::bf(y ~ s(z), sigma ~ s(x)),
                      trend = brms::bf(trend_y ~ s(z) - 1))
  expect_identical(smooths(stub), c("s(z)", "s(x)", "s(z)"))
  expect_identical(
    vapply(mvgam_smooth_terms(stub), smooth_panel_name, character(1L)),
    c("mu: s(z)", "sigma: s(x)", "mu: s(z) (trend)")
  )
})

test_that("a term's smooth objects are numbered as brms numbers them", {
  # brms gives a `by` factor one smooth object per level and numbers
  # the objects across the predictor's terms: here 1, 2 to 4 and 5.
  stub <- smooth_stub(brms::bf(y ~ s(x) + s(z, by = grp) + t2(x, w)))
  hits <- mvgam_smooth_terms(stub)
  expect_identical(lapply(hits, `[[`, "objects"), list(1L, 2:4, 5L))

  expect_true(is.na(hits[[1L]]$by_var))
  expect_identical(hits[[2L]]$by_var, "grp")
  expect_identical(hits[[3L]]$covars, c("x", "w"))
})

test_that("a numeric by variable is one smooth object", {
  hits <- mvgam_smooth_terms(smooth_stub(brms::bf(y ~ s(x, by = w))))
  expect_length(hits, 1L)
  expect_identical(hits[[1L]]$objects, 1L)
  expect_identical(hits[[1L]]$by_var, "w")
})

test_that("each response of a model with several keeps its own smooths", {
  stub <- smooth_stub(
    brms::bf(y ~ s(x)) + brms::bf(trend_y ~ s(w)) + brms::set_rescor(FALSE)
  )
  hits <- mvgam_smooth_terms(stub)
  expect_identical(vapply(hits, `[[`, character(1L), "resp"),
                   c("y", "trendy"))
  expect_identical(lapply(hits, `[[`, "objects"), list(1L, 1L))
  expect_identical(vapply(hits, smooth_panel_name, character(1L)),
                   c("y_mu: s(x)", "trendy_mu: s(w)"))
})

test_that("a non-linear parameter's smooths belong to it", {
  stub <- smooth_stub(
    brms::bf(y ~ a * exp(b * x), a ~ s(z), b ~ 1, nl = TRUE),
    prior = c(brms::prior(normal(0, 1), nlpar = "a"),
              brms::prior(normal(0, 1), nlpar = "b"))
  )
  hits <- mvgam_smooth_terms(stub)
  expect_length(hits, 1L)
  expect_identical(hits[[1L]]$nlpar, "a")
  expect_null(hits[[1L]]$dpar)
})

test_that("a smooth is found under a family carrying an addition term", {
  # `count | trials(size) ~ s(x)` only parses when brms knows the
  # family, which the fitted model does.
  stub <- smooth_stub(brms::bf(count | trials(size) ~ s(x)),
                      family = binomial())
  expect_identical(smooths(stub), "s(x)")
})

test_that("a label matches whatever its spacing", {
  stub <- smooth_stub(brms::bf(y ~ 1 + s(z, by = grp)))
  hit1 <- resolve_mvgam_smooth(stub, "s(z,by=grp)")
  hit2 <- resolve_mvgam_smooth(stub, "s(z,  by =  grp)")
  expect_identical(hit1$term, "s(z, by = grp)")
  expect_identical(hit2$term, "s(z, by = grp)")
})

# ---- Evaluating one smooth ------------------------------------------

test_that("a smooth reads only its own objects", {
  # `s(z)` is object 2 of `y ~ s(x) + s(z)`. Its value is its own basis
  # times its own coefficients, and none of `s(x)`'s.
  obs <- brms::bf(y ~ s(x) + s(z))
  stub <- smooth_stub(obs)
  sdata <- brms::standata(stub$obs_model, internal = TRUE)
  stub$fit <- stub_draws(c(
    "b_Intercept", "Intercept", "sigma",
    paste0("bs[", seq_len(ncol(sdata$Xs)), "]"),
    paste0("s_1_1[", seq_len(ncol(sdata$Zs_1_1)), "]"),
    paste0("s_2_1[", seq_len(ncol(sdata$Zs_2_1)), "]")
  ))
  expect_equal(
    unname(posterior_smooths(stub, "s(z)")),
    smooth_by_hand(sdata, stub$fit, "", 2L)
  )
})

test_that("a smooth is evaluated on a frame holding its own variables", {
  # brms fills every other variable from the fitted frame, `trials()`
  # included, with a value it accepts.
  stub <- smooth_stub(brms::bf(count | trials(size) ~ s(x)),
                      family = binomial())
  grid <- data.frame(x = c(-1, 0, 1))
  sdata <- brms::standata(stub$obs_model,
                          newdata = transform(grid, count = 0L, size = 10L),
                          internal = TRUE)
  stub$fit <- stub_draws(c(
    "b_Intercept", "Intercept",
    paste0("bs[", seq_len(ncol(sdata$Xs)), "]"),
    paste0("s_1_1[", seq_len(ncol(sdata$Zs_1_1)), "]")
  ))
  expect_equal(
    unname(posterior_smooths(stub, "s(x)", newdata = grid)),
    smooth_by_hand(sdata, stub$fit, "", 1L)
  )
})

test_that("a distributional parameter's smooth reads its own names", {
  stub <- smooth_stub(brms::bf(y ~ 1, sigma ~ s(x)))
  sdata <- brms::standata(stub$obs_model, internal = TRUE)
  stub$fit <- stub_draws(c(
    "b_Intercept", "Intercept", "b_sigma_Intercept", "Intercept_sigma",
    paste0("bs_sigma[", seq_len(ncol(sdata$Xs_sigma)), "]"),
    paste0("s_sigma_1_1[", seq_len(ncol(sdata$Zs_sigma_1_1)), "]")
  ))
  expect_equal(
    unname(posterior_smooths(stub, "s(x)", dpar = "sigma")),
    smooth_by_hand(sdata, stub$fit, "_sigma", 1L)
  )
  # The mean has no smooth of that name.
  expect_error(posterior_smooths(stub, "s(x)"), "Available smooth terms")
})

test_that("both smooth summaries refuse a fit without smooths", {
  stub <- smooth_stub(brms::bf(y ~ x))
  expect_error(conditional_smooths(stub), "require a smooth term")
  expect_error(posterior_smooths(stub, "s(x)"), "require a smooth term")
})

# ---- Conditional_smooths grid builder edge cases -------------------

test_that("a 2D smooth's grid is a surface or focal slices", {
  stub <- smooth_stub(brms::bf(y ~ s(x, z)))
  hit <- mvgam_smooth_terms(stub)[[1L]]
  grid <- function(surface, facets, resolution) {
    build_smooth_grid(stub, hit, surface = surface, facets = facets,
                      resolution = resolution, int_conditions = NULL,
                      too_far = 0)
  }
  # A surface crosses the resolution with itself
  g <- grid(TRUE, 3L, 10L)
  expect_identical(nrow(g$newdata), 100L)
  expect_true(g$surface)
  # Slices cross the focal resolution with one level per facet
  g <- grid(FALSE, 5L, 20L)
  expect_identical(nrow(g$newdata), 100L)
  expect_false(g$surface)
})

test_that("int_conditions sets the focal covariate's grid values", {
  stub <- smooth_stub(brms::bf(y ~ s(x)))
  hit <- mvgam_smooth_terms(stub)[[1L]]
  grid <- function(cond) {
    build_smooth_grid(stub, hit, surface = TRUE, facets = 3L,
                      resolution = 20L, int_conditions = cond, too_far = 0)
  }
  g <- grid(list(x = c(-1, 0, 1)))
  expect_identical(g$newdata$x, c(-1, 0, 1))
  # The grid holds the term's own variable alone.
  expect_identical(names(g$newdata), "x")
  # A function is applied to the data: quantile() gives five values
  expect_identical(unname(grid(list(x = stats::quantile))$newdata$x),
                   unname(stats::quantile(stub$data$x)))
})

# ---- Drawing -------------------------------------------------------

test_that("spaghetti draws one line per draw from its own columns", {
  # The overlay is built by `build_spaghetti_data()` and drawn by
  # the 1D panel, and the two have to name the same columns.
  grid <- data.frame(x = seq(-1, 1, length.out = 5L))
  eta <- matrix(rnorm(3L * 5L), nrow = 3L)
  df <- data.frame(
    x = grid$x, effect1__ = grid$x, cond__ = factor(1L),
    estimate__ = colMeans(eta), lower__ = -1, upper__ = 1
  )
  attr(df, "effects") <- "x"
  attr(df, "surface") <- FALSE
  attr(df, "spaghetti") <- build_spaghetti_data(eta, df, "x")
  built <- ggplot2::ggplot_build(build_mvgam_smooth_plot(df, "mu: s(x)"))
  lines <- built$data[[2L]]
  expect_identical(length(unique(lines$group)), 3L)
  # Each group is one draw, in its own order. Comparing the sorted
  # values matched the same multiset for a transposed eta.
  for (g in seq_len(nrow(eta))) {
    expect_equal(lines$y[lines$group == g], eta[g, ])
  }
})

test_that("a factor smooth draws one interval per level", {
  # `s(series, bs = "re")` puts a single grid point on each level.
  # A ribbon and a line are both drawn from at least two points, so a
  # panel built from them is empty whatever the fit contains, and the
  # emptiness is silent: the plot object is well formed and every
  # layer reports zero rows only once it is built.
  df <- data.frame(
    series = factor(c("a", "b", "c")),
    estimate__ = c(0.2, -0.1, 0.4),
    lower__ = c(-0.1, -0.5, 0.0),
    upper__ = c(0.5, 0.3, 0.8)
  )
  attr(df, "effects") <- "series"
  attr(df, "surface") <- FALSE

  p <- mvgam:::build_mvgam_smooth_plot(df, "s(series)")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1L], character(1))
  expect_true("GeomPointrange" %in% geoms)
  # The geoms that cannot draw a single point per group are the ones
  # that made the panel empty.
  expect_false(any(c("GeomRibbon", "GeomLine") %in% geoms))
  # Every level reaches the panel.
  built <- ggplot2::ggplot_build(p)
  expect_identical(nrow(built$data[[1L]]), 3L)

  # A numeric smooth keeps the ribbon and line it is drawn with.
  dfn <- data.frame(
    x = seq(0, 1, length.out = 10),
    estimate__ = seq(0, 1, length.out = 10),
    lower__ = seq(-1, 0, length.out = 10),
    upper__ = seq(1, 2, length.out = 10)
  )
  attr(dfn, "effects") <- "x"
  attr(dfn, "surface") <- FALSE
  pn <- mvgam:::build_mvgam_smooth_plot(dfn, "s(x)")
  geoms_n <- vapply(pn$layers, function(l) class(l$geom)[1L], character(1))
  expect_true("GeomRibbon" %in% geoms_n)
  expect_true("GeomLine" %in% geoms_n)
})
