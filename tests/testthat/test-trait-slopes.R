# The expansion of `formula` and `trait_slopes` into the fourth-corner
# formula. Fits that sample it are driven in tests/local.

# Fixed values: a point constraint takes the mean of a covariate
trait_frame <- data.frame(
  y = 1:6, env = c(-1, 0, 1, 2, -2, 0), x = c(0, 1, 2, 3, 4, 2),
  z = c(1, 2, 1, 2, 1, 2),
  hab = factor(rep(c("a", "b"), 3)),
  series = factor(rep(c("s1", "s2", "s3"), each = 2)),
  size = rep(c(1, 2, 3), each = 2)
)

expand <- function(formula, traits, ...) {
  out <- mvgam:::build_trait_slopes_formula(formula, traits, trait_frame,
                                            ...)$formula
  paste(deparse(mvgam:::obs_arm_main_formula(out), width.cutoff = 500L),
        collapse = " ")
}


test_that("terms with design columns cross the trait and vary by species", {
  expect_identical(
    expand(y ~ env + hab + I(env^2) + env:x, ~ size),
    paste0("y ~ env + hab + I(env^2) + env:x + size + env:size + ",
           "hab:size + I(env^2):size + env:x:size + ",
           "(1 + env + hab + I(env^2) + env:x | sp | series)")
  )
  # Without an intercept the traits enter through the covariates alone
  expect_identical(
    expand(y ~ 0 + env, ~ size),
    "y ~ 0 + env + env:size + (0 + env | sp | series)"
  )
})


test_that("smooths and processes scale by the trait and vary by species", {
  expect_identical(
    expand(y ~ s(x, k = 5) + gp(z, k = 5), ~ size),
    paste0("y ~ s(x, k = 5) + gp(z, k = 5) + size + ",
           "s(x, k = 5, by = size, pc = list(x = 2)) + ",
           "gp(z, k = 5, by = size) + ",
           "(1 | sp | series) + s(x, k = 5, by = series, m = 1) + ",
           "gp(z, k = 5, by = series)")
  )
})


test_that("a smooth trait scales by a covariate or spans a surface", {
  expect_identical(
    expand(y ~ env + s(x), ~ s(size, k = 3)),
    paste0("y ~ env + s(x) + s(size, k = 3) + ",
           "s(size, k = 3, by = env, pc = list(size = 2)) + ",
           "t2(x, size, bs = \"tp\", m = 1, k = c(-1, 3)) + ",
           "(1 + env | sp | series) + s(x, by = series, m = 1)")
  )
  # A gp() surface keeps the covariate's k, which keeps it approximate.
  # A t2() surface takes each margin's k.
  expect_match(
    expand(y ~ s(x, k = 4) + gp(z, k = 5), ~ s(size, k = 3)),
    "t2(x, size, bs = \"tp\", m = 1, k = c(4, 3)) + gp(z, size, k = 5)",
    fixed = TRUE
  )
  # An s() of two covariates is one isotropic margin. A t2() keeps its
  # own margins. A cyclic margin stays cyclic.
  expect_match(
    expand(y ~ s(x, z, k = 8), ~ s(size, k = 3)),
    paste0("t2(x, z, size, d = c(2, 1), bs = c(\"ds\", \"tp\"), ",
           "m = list(c(1, 0.5), 1), k = c(8, 3))"),
    fixed = TRUE
  )
  expect_match(
    expand(y ~ t2(x, z, k = 4), ~ s(size, k = 3)),
    "t2(x, z, size, bs = \"tp\", m = 1, k = c(4, 4, 3))",
    fixed = TRUE
  )
  expect_match(
    expand(y ~ s(x, bs = "cc"), ~ s(size)),
    "t2(x, size, bs = c(\"cc\", \"tp\"), m = 1)",
    fixed = TRUE
  )
})


test_that("group terms and sub-formulas stay as written", {
  # A random-effect smooth is a group term written as a smooth
  expect_identical(
    expand(y ~ env + s(hab, bs = "re"), ~ size),
    paste0("y ~ env + s(hab, bs = \"re\") + size + env:size + ",
           "(1 + env | sp | series)")
  )
  out <- mvgam:::build_trait_slopes_formula(
    brms::bf(y ~ env + (1 | hab), p ~ z), ~ size, trait_frame
  )$formula
  expect_identical(
    paste(deparse(out$formula, width.cutoff = 500L), collapse = " "),
    paste0("y ~ env + (1 | hab) + size + env:size + ",
           "(1 + env | sp | series)")
  )
  expect_equal(out$pforms$p, p ~ z, ignore_formula_env = TRUE)
})


test_that("a term without one numeric column scales through derived ones", {
  # A factor scales the trait's smooth by each of its design columns
  expect_identical(
    expand(y ~ hab, ~ s(size, k = 3)),
    paste0("y ~ hab + s(size, k = 3) + ",
           "s(size, k = 3, by = habb, pc = list(size = 2)) + ",
           "(1 + hab | sp | series)")
  )
  # A factor trait scales the covariate's smooth the same way
  expect_identical(
    expand(y ~ s(x), ~ hab),
    paste0("y ~ s(x) + hab + s(x, by = habb, pc = list(x = 2)) + ",
           "(1 | sp | series) + s(x, by = series, m = 1)")
  )
  # A numeric `by` leaves the smooth uncentred. Its crossing keeps the
  # constant, the trait's effect on the env slope. Each species scales
  # its own deviation by env.
  expect_identical(
    expand(y ~ s(x, by = env), ~ size),
    paste0("y ~ s(x, by = env) + size + s(x, by = env_size) + ",
           "(1 | sp | series) + s(x, by = env_seriess1, m = 1) + ",
           "s(x, by = env_seriess2, m = 1) + ",
           "s(x, by = env_seriess3, m = 1)")
  )
  # A factor `by` centres each level. Its species deviations cross the
  # levels with the species.
  expect_identical(
    expand(y ~ s(x, by = hab), ~ size),
    paste0("y ~ s(x, by = hab) + size + ",
           "s(x, by = haba_size, pc = list(x = 2)) + ",
           "s(x, by = habb_size, pc = list(x = 2)) + (1 | sp | series) + ",
           "s(x, by = hab_series, m = 1)")
  )
  # Each column of one design comes from its own term. A grid moving
  # 'env' leaves the column of 'hab' alone.
  sources <- mvgam:::derived_column_sources(mvgam:::build_trait_slopes_formula(
    y ~ env + hab + I(env^2), ~ s(size, k = 3), trait_frame
  )$columns)
  expect_identical(sources, list(habb = "hab", IenvE2 = "env"))
})


test_that("derived columns are rebuilt from each frame's own variables", {
  out <- mvgam:::build_trait_slopes_formula(
    y ~ poly(x, 2) + s(x, by = hab), ~ s(size, k = 3), trait_frame
  )
  fitted <- mvgam:::derive_trait_columns(out$columns, trait_frame)
  # The fitted basis of poly(), set by its stored coefficients
  expect_equal(fitted$polyx21,
               unname(stats::poly(trait_frame$x, 2)[, 1]))
  # Two rows hold their fitted values. A fresh poly() basis on two
  # rows would differ.
  two <- mvgam:::derive_trait_columns(out$columns, trait_frame[c(2, 5), ])
  expect_equal(two$polyx22, fitted$polyx22[c(2, 5)])
  expect_identical(as.character(two$hab_series), c("b_s1", "a_s3"))
  # A frame changing a source variable changes its column
  moved <- transform(trait_frame, x = x + 1)
  expect_false(isTRUE(all.equal(
    mvgam:::derive_trait_columns(out$columns, moved)$polyx21,
    fitted$polyx21
  )))
  expect_error(
    mvgam:::derive_trait_columns(out$columns,
                                 trait_frame[names(trait_frame) != "x"]),
    "Missing: 'x'"
  )
})


test_that("the expansion refuses what brms has no term for", {
  # A species term repeats the species deviation
  expect_error(expand(y ~ env * series, ~ size),
               "Species term in 'formula': 'series'")
  # The user's own species column counts too
  expect_error(expand(y ~ env:taxon, ~ size, species_names = "taxon"),
               "Species term in 'formula': 'env:taxon'")
  # A monotonic effect has no design column to scale a smooth by
  expect_error(expand(y ~ mo(z), ~ s(size, k = 3)),
               "Scaling 's\\(size, k = 3\\)' by 'mo\\(z\\)'")
  # mgcv's t2() errors on a point constraint over two margins. One
  # margin takes it.
  expect_error(expand(y ~ t2(x, z, k = 4), ~ size),
               "Scaling 't2\\(x, z, k = 4\\)' by 'size'")
  expect_match(expand(y ~ t2(x, k = 4), ~ size),
               "t2(x, k = 4, by = size, pc = list(x = ", fixed = TRUE)
  expect_error(expand(y ~ -1, ~ size), "an intercept or a covariate")
})


test_that("each coefficient's default prior scales by its column's SD", {
  out <- mvgam:::build_trait_slopes_formula(y ~ env + hab, ~ size,
                                            trait_frame)
  frame <- mvgam:::derive_trait_columns(out$columns, trait_frame)
  sds <- mvgam:::trait_slopes_column_sds(out$formula, frame)
  # One SD per coefficient brms names, the intercept left out
  expect_equal(sds[["env:size"]], stats::sd(trait_frame$env * trait_frame$size))
  expect_equal(sds[["habb"]], stats::sd(trait_frame$hab == "b"))
  defaults <- mvgam:::default_trait_slopes_priors(
    "series", c(env = 0.5, habb = 1)
  )
  expect_identical(
    paste(defaults$class, defaults$coef, defaults$group, defaults$prior),
    c("b env  normal(0, 2)", "b habb  std_normal()",
      "sd  series student_t(3, 0, 2.5)")
  )
  # An identity link scales every default by the response's SD, as
  # rstanarm's autoscaled priors do. A constant column has no SD and
  # takes the class default.
  scaled <- mvgam:::default_trait_slopes_priors(
    "series", c(env = 0.5, flat = 0), response_scale = 4
  )
  expect_identical(
    paste(scaled$class, scaled$coef, scaled$prior),
    c("b  normal(0, 4)", "b env normal(0, 8)",
      "sd  student_t(3, 0, 10)")
  )
  y_frame <- transform(trait_frame, y = y * 10)
  expect_equal(
    mvgam:::trait_slopes_response_scale(y ~ env, gaussian(), y_frame),
    stats::sd(y_frame$y)
  )
  expect_identical(
    mvgam:::trait_slopes_response_scale(y ~ env, poisson(), y_frame), 1
  )
  # A user prior on the class replaces every coefficient's default. A
  # coefficient's own default would otherwise outrank it.
  user <- brms::prior(normal(0, 5), class = b)
  merged <- mvgam:::merge_user_priors(
    mvgam:::default_trait_slopes_priors("series", c(env = 0.5), user), user
  )
  expect_identical(
    paste(merged$class, merged$coef, merged$prior, merged$source),
    c("sd  student_t(3, 0, 2.5) default", "b  normal(0, 5) user")
  )
  # A user prior on one coefficient replaces that default alone
  one <- brms::prior(normal(0, 9), class = b, coef = env)
  merged <- mvgam:::merge_user_priors(
    mvgam:::default_trait_slopes_priors("series", c(env = 0.5, habb = 1),
                                        one),
    one
  )
  expect_identical(merged$prior[merged$coef == "env"], "normal(0, 9)")
  expect_identical(merged$prior[merged$coef == "habb"], "std_normal()")
  # mvgam() labels every row it is given as the user's. The untouched
  # defaults take their label back.
  fitted <- merged
  fitted$source <- "user"
  expect_identical(
    mvgam:::label_trait_slopes_defaults(fitted, merged)$source,
    ifelse(merged$coef == "env", "user", "default")
  )
})


test_that("validate_trait_slopes refuses traits it cannot regress on", {
  bad <- trait_frame
  bad$varying <- rnorm(6)
  check <- function(traits) {
    mvgam:::validate_trait_slopes(traits, y ~ env, bad, "series")
  }
  expect_error(check("size"), "must be a one-sided formula")
  expect_error(check(~ absent), "Each trait must be a column")
  expect_error(check(~ varying), "varies within species")
  expect_error(check(~ size + (1 | hab)), "Group-level term: '1 \\| hab'")
})
