# CI-safe tests for posterior_smooths.mvgam, conditional_smooths.mvgam,
# and the supporting smooth-table helpers. Numerical concordance
# against brms lives in tests/local. Here we lock in S3 dispatch,
# signature parity with brms, the smooth-index helper (single
# smooth, by-factor expansion, multi-smooth, trend-side), and the
# error paths for unknown terms / no-smooth fits.


make_smooth_stub <- function(by_factor = FALSE,
                              trend_smooth = FALSE) {
  set.seed(5L)
  n <- 40L
  df <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    z = rnorm(n),
    grp = factor(sample(letters[1:3], n, replace = TRUE),
                 levels = letters[1:3])
  )
  obs_form <- if (by_factor) {
    brms::bf(y ~ 1 + s(z, by = grp))
  } else {
    brms::bf(y ~ 1 + s(z))
  }
  trend_form <- if (trend_smooth) {
    brms::bf(trend_y ~ s(z) - 1)
  } else {
    NULL
  }
  # Build a minimal mvgam-shaped stub. extract_mvgam_draws +
  # mvgam_smooth_eta both call brms::standata on this formula, so
  # the standata wiring must be valid.
  sd_ <- brms::standata(
    obs_form, data = df, family = brms::brmsfamily("gaussian")
  )
  structure(
    list(
      formula = obs_form,
      trend_formula = trend_form,
      data = df,
      family = brms::brmsfamily("gaussian"),
      standata = as.list(sd_),
      stancode = "// stub"
    ),
    class = "mvgam"
  )
}


# ---- Dispatch + signature parity -----------------------------------

test_that("posterior_smooths.mvgam matches brms signature", {
  expected <- names(formals(getS3method("posterior_smooths", "brmsfit")))
  actual <- names(formals(getS3method("posterior_smooths", "mvgam")))
  # brms passes everything else through ..., so we require at
  # minimum the same named formals up to (and including) `...`.
  expect_true(all(expected %in% actual))
})


test_that("conditional_smooths.mvgam matches brms-parity arg set", {
  brms_args <- names(formals(getS3method("conditional_smooths", "brmsfit")))
  mvgam_args <- names(formals(getS3method("conditional_smooths", "mvgam")))
  # All current brms named args (excluding deprecated nsamples /
  # subset and the legacy `probs` alias brms keeps for back-compat)
  # must be accepted by the mvgam method.
  required <- setdiff(brms_args, c("nsamples", "subset", "probs"))
  expect_true(all(required %in% mvgam_args))
  # The mvgam-only `facets` knob must also be present.
  expect_true("facets" %in% mvgam_args)
})


# ---- smooths() ------------------------------------------------------

test_that("smooths.mvgam returns canonical labels for s() smooths", {
  stub <- make_smooth_stub()
  expect_identical(smooths(stub), "s(z)")
})


test_that("smooths.mvgam expands by-factor to one smframe row per level", {
  stub <- make_smooth_stub(by_factor = TRUE)
  expect_identical(smooths(stub), "s(z, by = grp)")
  idx <- mvgam_smooth_index(stub$formula, stub$data)
  # 3 grp levels = 3 smframe rows for one user-facing term.
  expect_identical(nrow(idx), 3L)
  expect_identical(idx$by_level, c("a", "b", "c"))
})


test_that("smooths.mvgam enumerates obs and trend sides", {
  stub <- make_smooth_stub(trend_smooth = TRUE)
  out <- smooths(stub)
  expect_identical(length(out), 2L)
  expect_true("s(z)" %in% out)
})


# ---- mvgam_smooth_index --------------------------------------------

test_that("mvgam_smooth_index returns NULL on a no-smooth formula", {
  set.seed(11L)
  df <- data.frame(y = rnorm(20L), x = rnorm(20L))
  expect_null(mvgam_smooth_index(brms::bf(y ~ x), df))
})


test_that("mvgam_smooth_index assigns sequential smframe rows", {
  set.seed(17L); n <- 40L
  df <- data.frame(
    y = rnorm(n), x = rnorm(n), z = rnorm(n),
    grp = factor(sample(letters[1:2], n, replace = TRUE))
  )
  form <- brms::bf(y ~ s(x) + s(z, by = grp))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 3L)  # 1 (s(x)) + 2 (s(z, by=grp))
  expect_identical(idx$row, 1:3)
  expect_identical(idx$term_idx, c(1L, 2L, 2L))
})


# ---- mvgam_side_formula / suffix ----------------------------------

test_that("mvgam_side_formula returns bf object or NULL", {
  stub <- make_smooth_stub(trend_smooth = TRUE)
  expect_s3_class(mvgam_side_formula(stub, "obs"), "brmsformula")
  expect_s3_class(mvgam_side_formula(stub, "trend"), "brmsformula")
  stub$trend_formula <- NULL
  expect_null(mvgam_side_formula(stub, "trend"))
})


test_that("mvgam_side_suffix returns the right brms convention", {
  expect_identical(mvgam_side_suffix("obs"), "")
  expect_identical(mvgam_side_suffix("trend"), "_trend")
})


# ---- Error paths ---------------------------------------------------

test_that("posterior_smooths.mvgam errors on unknown smooth term", {
  stub <- make_smooth_stub()
  expect_error(
    posterior_smooths(stub, smooth = "s(nope)"),
    "Available smooth terms"
  )
})


test_that("posterior_smooths.mvgam rejects resp/dpar/nlpar", {
  stub <- make_smooth_stub()
  expect_error(posterior_smooths(stub, "s(z)", resp = "y"),
               "brms-parity")
  expect_error(posterior_smooths(stub, "s(z)", dpar = "mu"),
               "brms-parity")
  expect_error(posterior_smooths(stub, "s(z)", nlpar = "a"),
               "brms-parity")
})


test_that("conditional_smooths.mvgam errors on no-smooth fits", {
  set.seed(23L)
  df <- data.frame(y = rnorm(15L), x = rnorm(15L))
  sd_ <- brms::standata(
    brms::bf(y ~ x), data = df,
    family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms::bf(y ~ x), data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  expect_error(conditional_smooths(stub), "no smooth terms")
})


# ---- subset_draws_for_smooth ---------------------------------------

test_that("subset_draws_for_smooth validates ndraws and draw_ids", {
  mat <- matrix(rnorm(20L), nrow = 10L)
  expect_identical(nrow(subset_draws_for_smooth(mat, NULL, NULL)), 10L)
  expect_identical(nrow(subset_draws_for_smooth(mat, 5L, NULL)), 5L)
  expect_identical(nrow(subset_draws_for_smooth(mat, NULL, c(1L, 3L))),
                   2L)
  expect_error(subset_draws_for_smooth(mat, 99L, NULL), "exceeds")
  expect_error(subset_draws_for_smooth(mat, NULL, c(1L, 99L)),
               "beyond")
})


# ---- Edge cases: different basis families --------------------------

test_that("mvgam_smooth_index handles cyclic cubic basis (bs = 'cc')", {
  set.seed(31L); n <- 60L
  df <- data.frame(y = rnorm(n), season = runif(n, 1, 12))
  form <- brms::bf(y ~ s(season, bs = "cc"))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 1L)
  expect_identical(idx$term, "s(season, bs = \"cc\")")
})


test_that("mvgam_smooth_index handles thin-plate basis explicitly (bs = 'tp')", {
  set.seed(33L); n <- 50L
  df <- data.frame(y = rnorm(n), x = rnorm(n))
  form <- brms::bf(y ~ s(x, bs = "tp", k = 8))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 1L)
  expect_identical(idx$term_idx, 1L)
})


# ---- Edge cases: multidimensional smooths --------------------------

test_that("mvgam_smooth_index handles 2D smooths (s(x, y))", {
  set.seed(35L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n), z = rnorm(n))
  form <- brms::bf(y ~ s(x, z))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 1L)
  expect_identical(idx$term, "s(x, z)")
})


test_that("mvgam_smooth_index handles tensor product t2(x, y)", {
  set.seed(37L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n), z = rnorm(n))
  form <- brms::bf(y ~ t2(x, z))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 1L)
  expect_identical(idx$term, "t2(x, z)")
})


test_that("mvgam_smooth_index handles mixed s() and t2() in one formula", {
  # brms supports `s()` and `t2()` (not `te()`/`ti()`), so the
  # mixed-formula coverage tracks that surface.
  set.seed(39L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n), z = rnorm(n))
  form <- brms::bf(y ~ s(x) + t2(x, z))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 2L)
  expect_identical(idx$term, c("s(x)", "t2(x, z)"))
  expect_identical(idx$term_idx, c(1L, 2L))
})


# ---- Edge cases: by-factor multi-level expansion -------------------

test_that("by-factor smooths produce one smframe row per level", {
  set.seed(41L); n <- 60L
  df <- data.frame(
    y = rnorm(n), z = rnorm(n),
    grp = factor(sample(letters[1:4], n, replace = TRUE),
                 levels = letters[1:4])
  )
  form <- brms::bf(y ~ s(z, by = grp))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 4L)
  expect_identical(idx$by_level, c("a", "b", "c", "d"))
  expect_identical(unique(idx$term), "s(z, by = grp)")
  expect_identical(idx$row, 1:4)
})


test_that("by-factor on a 2D smooth expands per by-level", {
  set.seed(43L); n <- 60L
  df <- data.frame(
    y = rnorm(n), x = rnorm(n), z = rnorm(n),
    grp = factor(sample(letters[1:3], n, replace = TRUE),
                 levels = letters[1:3])
  )
  form <- brms::bf(y ~ t2(x, z, by = grp))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 3L)
  expect_identical(unique(idx$term), "t2(x, z, by = grp)")
})


test_that("numeric by= produces a single smframe row (no expansion)", {
  set.seed(45L); n <- 50L
  df <- data.frame(y = rnorm(n), x = rnorm(n), w = rnorm(n))
  form <- brms::bf(y ~ s(x, by = w))
  idx <- mvgam_smooth_index(form, df)
  expect_identical(nrow(idx), 1L)
  expect_identical(idx$term, "s(x, by = w)")
  expect_identical(idx$by_var, "w")
  expect_true(is.na(idx$by_level))
})


# ---- Edge cases: mixed multi-smooth formulas -----------------------

test_that("multiple smooths with mixed types and by-factor get sequential rows", {
  set.seed(47L); n <- 80L
  df <- data.frame(
    y = rnorm(n), x = rnorm(n), z = rnorm(n), w = rnorm(n),
    grp = factor(sample(letters[1:3], n, replace = TRUE))
  )
  form <- brms::bf(y ~ s(x) + s(z, by = grp) + t2(x, w))
  idx <- mvgam_smooth_index(form, df)
  # 1 (s(x)) + 3 (s(z,by=grp)) + 1 (t2(x,w)) = 5
  expect_identical(nrow(idx), 5L)
  expect_identical(idx$term_idx, c(1L, 2L, 2L, 2L, 3L))
  expect_identical(idx$row, 1:5)
  expect_identical(
    idx$term,
    c("s(x)", rep("s(z, by = grp)", 3L), "t2(x, w)")
  )
})


# ---- Conditional_smooths grid builder edge cases -------------------

test_that("build_smooth_grid surface=TRUE uses resolution^2 for 2D", {
  set.seed(51L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n), z = rnorm(n))
  sd_ <- brms::standata(
    brms::bf(y ~ s(x, z)), data = df,
    family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms::bf(y ~ s(x, z)), data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  hit <- mvgam_smooth_terms(stub)[[1L]]
  g <- build_smooth_grid(stub, hit, surface = TRUE, facets = 3L,
                          resolution = 10L, int_conditions = NULL,
                          too_far = 0)
  expect_identical(nrow(g$newdata), 100L)
  expect_true(g$surface)
})


test_that("build_smooth_grid surface=FALSE for 2D uses focal x facets", {
  set.seed(53L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n), z = rnorm(n))
  sd_ <- brms::standata(
    brms::bf(y ~ s(x, z)), data = df,
    family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms::bf(y ~ s(x, z)), data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  hit <- mvgam_smooth_terms(stub)[[1L]]
  # facets = 5 should produce 5 levels of the second covariate
  g <- build_smooth_grid(stub, hit, surface = FALSE, facets = 5L,
                          resolution = 20L, int_conditions = NULL,
                          too_far = 0)
  expect_identical(nrow(g$newdata), 100L)  # 20 x 5
  expect_false(g$surface)
})


test_that("int_conditions overrides the focal-covariate grid values", {
  set.seed(55L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n))
  sd_ <- brms::standata(
    brms::bf(y ~ s(x)), data = df,
    family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms::bf(y ~ s(x)), data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  hit <- mvgam_smooth_terms(stub)[[1L]]
  g <- build_smooth_grid(stub, hit, surface = TRUE, facets = 3L,
                          resolution = 20L,
                          int_conditions = list(x = c(-1, 0, 1)),
                          too_far = 0)
  expect_identical(g$newdata$x, c(-1, 0, 1))
})


test_that("int_conditions accepts a function applied to the data", {
  set.seed(57L); n <- 60L
  df <- data.frame(y = rnorm(n), x = rnorm(n))
  sd_ <- brms::standata(
    brms::bf(y ~ s(x)), data = df,
    family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms::bf(y ~ s(x)), data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  hit <- mvgam_smooth_terms(stub)[[1L]]
  g <- build_smooth_grid(stub, hit, surface = TRUE, facets = 3L,
                          resolution = 20L,
                          int_conditions = list(x = stats::quantile),
                          too_far = 0)
  # quantile(x) returns 5 quantiles by default
  expect_identical(length(g$newdata$x), 5L)
})


# ---- resolve_mvgam_smooth tolerates whitespace differences ---------

test_that("resolve_mvgam_smooth normalises whitespace in user-supplied label", {
  stub <- make_smooth_stub(by_factor = TRUE)
  # brms canonical is "s(z, by = grp)"; user might pass
  # "s(z,by=grp)" or "s(z,  by =  grp)".
  hit1 <- resolve_mvgam_smooth(stub, "s(z,by=grp)")
  hit2 <- resolve_mvgam_smooth(stub, "s(z,  by =  grp)")
  expect_identical(hit1$term, "s(z, by = grp)")
  expect_identical(hit2$term, "s(z, by = grp)")
})


test_that("plot.mvgam_conditional_smooths dispatches on the mvgam class", {
  # Mock a 1D smooth's summary data.frame with the columns
  # `conditional_smooths.mvgam` produces. Verifies:
  #   - class is `mvgam_conditional_smooths` (not just brms's)
  #   - plot() returns a list of ggplots
  #   - the ribbon is not the flat-line-at-zero bug (#384)
  grid <- seq(-2, 2, length.out = 25L)
  df <- data.frame(
    env = grid,
    effect1__ = grid,
    cond__ = 1L,
    estimate__ = 1.5 * exp(-0.5 * (grid - 0.4)^2) - 0.4,
    se__ = rep(0.25, length(grid)),
    lower__ = 1.5 * exp(-0.5 * (grid - 0.4)^2) - 0.4 - 0.5,
    upper__ = 1.5 * exp(-0.5 * (grid - 0.4)^2) - 0.4 + 0.5
  )
  attr(df, "response") <- "mu: s(env, k = 8) (trend)"
  attr(df, "effects") <- "env"
  attr(df, "surface") <- FALSE
  attr(df, "spaghetti") <- NULL
  attr(df, "points") <- NULL
  cs <- structure(
    list(df),
    class = c("mvgam_conditional_smooths",
              "brms_conditional_effects", "list"),
    smooths_only = TRUE
  )
  names(cs) <- attr(df, "response")
  ggs <- plot(cs, plot = FALSE)
  expect_type(ggs, "list")
  expect_length(ggs, 1L)
  expect_s3_class(ggs[[1L]], "ggplot")
  # Ribbon should not be a flat line at zero: the estimate at the
  # peak (env ~ 0.4) should be clearly above the estimate at the
  # tail (env ~ 2). Guards against the #384 regression.
  peak_idx <- which.min(abs(grid - 0.4))
  tail_idx <- which.min(abs(grid - 2))
  expect_gt(df$estimate__[peak_idx] - df$estimate__[tail_idx], 0.5)
})
