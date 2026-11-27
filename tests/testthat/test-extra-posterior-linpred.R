# The tests of test-posterior-linpred.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


test_that("a group-level term is read by its block id and prefix", {
  # `bf(y ~ (1|g), sigma ~ (1|g))`: one grouping factor, two blocks.
  # brms numbers the design and coefficients by the block `id`: the
  # sigma term is `Z_2_sigma_1` against `r_2_sigma_1`, indexed by
  # `J_2`. Keyed by the grouping factor's number, it would not be
  # found.
  J <- c(1L, 3L, 2L, 1L)
  sdata <- list(
    X = intercept_design(4L), Z_1_1 = rep(1, 4), J_1 = J,
    X_sigma = intercept_design(4L), Z_2_sigma_1 = rep(1, 4),
    J_2 = J
  )
  draws <- one_draw(
    b_Intercept = 0, b_sigma_Intercept = 0,
    `r_1_1[1]` = 10, `r_1_1[2]` = 20, `r_1_1[3]` = 30,
    `r_2_sigma_1[1]` = 1, `r_2_sigma_1[2]` = 2, `r_2_sigma_1[3]` = 3
  )
  ranef <- mk_ranef(
    list(id = c(1L, 2L), group = c("g", "g"), dpar = c("", "sigma")),
    list(g = c("a", "b", "c"))
  )
  prep <- mk_prep(sdata, draws, brms::bf(y ~ (1 | g), sigma ~ (1 | g)),
                  ranef = ranef)
  expect_equal(unname(extract_linpred_from_prep(prep)),
               matrix(c(10, 30, 20, 10), 1L))
  expect_equal(unname(extract_linpred_from_prep(prep, dpar = "sigma")),
               matrix(c(1, 3, 2, 1), 1L))

  # With every term kept, a design brms did not write is a fault, where
  # skipping it in silence is what lost the sigma term.
  sdata$Z_2_sigma_1 <- NULL
  expect_error(
    extract_linpred_from_prep(mk_prep(
      sdata, draws, brms::bf(y ~ (1 | g), sigma ~ (1 | g)), ranef = ranef
    ), dpar = "sigma"),
    "Please report this mvgam fault"
  )
  # Under `re_formula = NA` brms writes no group-level data, and no
  # term is read.
  population_only <- sdata[c("X", "X_sigma")]
  for (dpar in list(NULL, "sigma")) {
    expect_equal(
      unname(extract_linpred_from_prep(mk_prep(
        population_only, draws, brms::bf(y ~ (1 | g), sigma ~ (1 | g)),
        ranef = ranef, re_formula = NA
      ), dpar = dpar)),
      matrix(0, 1L, 4L)
    )
  }
})


test_that("each formula term's part sums to the linear predictor", {
  # Stan data and the group-level table from brms itself, with a draw
  # for every parameter the formula declares. `(1 | g) + (0 + x | g)`
  # are two blocks of one grouping factor. The column `x:fb` belongs to
  # the interaction alone.
  set.seed(3)
  n <- 12L
  dat <- data.frame(
    y = 0, x = rnorm(n), z = rnorm(n), w = rnorm(n), v = rnorm(n),
    f = gl(2L, 1L, n), g = gl(3L, 4L), h = gl(2L, 6L),
    m = factor(rep(1:3, 4L), ordered = TRUE), o = runif(n)
  )
  form <- brms::bf(
    y ~ x * f + s(z, k = 4) + (1 | g) + (0 + x | g) + (1 + x || h) +
      gp(w, k = 5, cov = "matern32") + gp(v, k = 5) + mo(m) + offset(o)
  )
  fit <- brms::brm(form, dat, empty = TRUE)
  vec <- function(stem, k) paste0(stem, "[", seq_len(k), "]")
  pars <- c(
    "b_Intercept", vec("b", 3L), "bs[1]", vec("s_1_1", 2L),
    vec("r_1_1", 3L), vec("r_2_1", 3L), vec("r_3_1", 2L), vec("r_3_2", 2L),
    "sdgp_1[1]", "lscale_1[1,1]", vec("zgp_1", 5L),
    "sdgp_2[1]", "lscale_2[1,1]", vec("zgp_2", 5L),
    "bsp[1]", vec("simo_1", 2L)
  )
  draws <- posterior::as_draws_matrix(
    matrix(runif(3L * length(pars), 0.2, 1.5), 3L,
           dimnames = list(NULL, pars))
  )
  plain <- as_plain_matrix(draws)
  prep <- prepare_linpred_data(draws, fit, dat)
  predictor <- prep$predictors[[1L]]

  parts <- linear_terms_parts(prep, plain, predictor, n)
  expect_identical(names(parts), c(
    "Intercept", "x", "f", "x:f", "s(z, k = 4)", "(1 | g)", "(0 + x | g)",
    "(1 + x || h)", 'gp(w, k = 5, cov = "matern32")', "gp(v, k = 5)",
    "mo(m)", "offset"
  ))
  expect_equal(parts[["(0 + x | g)"]],
               unname(plain[, vec("r_2_1", 3L)])[, as.integer(dat$g)] *
                 rep(dat$x, each = 3L))
  expect_equal(Reduce(`+`, parts), extract_linpred_from_prep(prep))

  # Under `re_formula = NA` brms writes no group-level data
  prep <- prepare_linpred_data(draws, fit, dat, re_formula = NA)
  parts <- linear_terms_parts(prep, plain, predictor, n)
  expect_false(any(startsWith(names(parts), "(")))
  expect_equal(Reduce(`+`, parts), extract_linpred_from_prep(prep))

  # Each `gp()` label takes the draws brms numbers in formula order
  plain[, vec("zgp_2", 5L)] <- 0
  parts <- linear_terms_parts(prep, plain, predictor, n)
  expect_true(all(parts[["gp(v, k = 5)"]] == 0))
  expect_true(all(parts[['gp(w, k = 5, cov = "matern32")']] != 0))

  # Without a sampled `b_Intercept`, as under `bf(center = FALSE)`, the
  # intercept is the first slope column
  fit <- brms::brm(brms::bf(y ~ x * f, center = FALSE), dat, empty = TRUE)
  draws <- posterior::as_draws_matrix(one_draw(
    `b[1]` = 0.3, `b[2]` = 2, `b[3]` = -1, `b[4]` = 5
  ))
  parts <- linear_terms_parts(prepare_linpred_data(draws, fit, dat),
                              as_plain_matrix(draws),
                              model_predictors(fit$formula)[[1L]], n)
  expect_identical(names(parts), c("Intercept", "x", "f", "x:f"))
  expect_equal(drop(parts[["Intercept"]]), rep(0.3, n))
})


test_that("re_formula keeps the terms it names", {
  # brms numbers the kept slope as block 1 in the data. Its draws keep
  # block 2.
  dat <- data.frame(y = 0, x = c(1, 2, 3, 4), g = c("a", "b", "a", "b"))
  fit <- brms::brm(y ~ 0 + (1 | g) + (0 + x || g), dat, empty = TRUE)
  pars <- c("r_1_1[1]", "r_1_1[2]", "r_2_1[1]", "r_2_1[2]")
  draws <- posterior::as_draws_matrix(
    matrix(c(1, 2, 3, 5), 1L, dimnames = list(NULL, pars))
  )
  prep <- prepare_linpred_data(draws, fit, dat, re_formula = ~ (0 + x || g))
  expect_equal(as.numeric(extract_linpred_from_prep(prep)),
               c(3, 5, 3, 5) * dat$x)
  # brms changes how a new level is sampled only when it is allowed one.
  expect_error(
    validate_group_level_args(NULL, FALSE, "gaussian"),
    "'allow_new_levels' is TRUE"
  )
})


test_that("a multi-membership term sums over its memberships", {
  # Each row weighs its two levels by its own scaled weights. Its slope
  # takes a covariate per membership.
  set.seed(6)
  n <- 8L
  dat <- data.frame(y = 0, g1 = gl(3L, 1L, n), g2 = gl(3L, 2L, n),
                    w1 = runif(n), w2 = runif(n), x1 = rnorm(n),
                    x2 = rnorm(n))
  fit <- brms::brm(
    y ~ 1 + (1 + mmc(x1, x2) | mm(g1, g2, weights = cbind(w1, w2))), dat,
    empty = TRUE
  )
  pars <- c("b_Intercept", paste0("r_1_1[", 1:3, "]"),
            paste0("r_1_2[", 1:3, "]"))
  draws <- posterior::as_draws_matrix(matrix(
    runif(2L * length(pars)), 2L, dimnames = list(NULL, pars)
  ))
  plain <- as_plain_matrix(draws)
  prep <- prepare_linpred_data(draws, fit, dat)
  label <- "(1 + mmc(x1, x2) | mm(g1, g2, weights = cbind(w1, w2)))"
  part <- linear_terms_parts(prep, plain, prep$predictors[[1L]], n)[[label]]
  share <- dat$w1 / (dat$w1 + dat$w2)
  u <- unname(plain[, paste0("r_1_1[", 1:3, "]")])
  v <- unname(plain[, paste0("r_1_2[", 1:3, "]")])
  i1 <- as.integer(dat$g1)
  i2 <- as.integer(dat$g2)
  expect_equal(
    part[2L, ],
    share * (u[2L, i1] + v[2L, i1] * dat$x1) +
      (1 - share) * (u[2L, i2] + v[2L, i2] * dat$x2)
  )
})


test_that("a new grouping level takes coefficients as brms draws them", {
  set.seed(7)
  dat <- data.frame(y = 0, x = c(1, 2, 3, 2), g = c("a", "b", "c", "a"))
  fit <- brms::brm(y ~ 0 + (1 + x | g), dat, empty = TRUE)
  pars <- c(paste0("r_1_1[", 1:3, "]"), paste0("r_1_2[", 1:3, "]"),
            "sd_1[1]", "sd_1[2]", "cor_1[1]")
  draws <- posterior::as_draws_matrix(matrix(
    runif(40L * length(pars), 0.2, 0.9), 40L, dimnames = list(NULL, pars)
  ))
  plain <- as_plain_matrix(draws)
  new <- data.frame(y = 0, x = 1, g = "z")
  prep_for <- function(rule) {
    prepare_linpred_data(draws, fit, new, allow_new_levels = TRUE,
                         sample_new_levels = rule)
  }
  coefs <- function(prep) unname(cbind(prep$new_levels[["1"]],
                                       prep$new_levels[["2"]]))
  level_of <- function(row, draw) {
    which(abs(plain[draw, 1:3] - row[1L]) + abs(plain[draw, 4:6] - row[2L]) <
            1e-12)
  }

  # Each draw takes both coefficients from one fitted level
  picked <- coefs(prep_for("uncertainty"))
  chosen <- vapply(1:40, function(d) level_of(picked[d, ], d), integer(1L))
  expect_setequal(chosen, 1:3)

  # One fitted level serves every draw
  picked <- coefs(prep_for("old_levels"))
  chosen <- vapply(1:40, function(d) level_of(picked[d, ], d), integer(1L))
  expect_length(unique(chosen), 1L)

  # Each draw takes the group's own covariance
  set.seed(8)
  picked <- coefs(prep_for("gaussian"))
  set.seed(8)
  z <- matrix(rnorm(80L), 2L)
  rho <- unname(plain[, "cor_1[1]"])
  expect_equal(picked[, 1L], unname(plain[, "sd_1[1]"]) * z[1L, ])
  expect_equal(picked[, 2L],
               unname(plain[, "sd_1[2]"]) * (rho * z[1L, ] +
                                       sqrt(1 - rho^2) * z[2L, ]))
})


test_that("special terms compose as brms writes them", {
  # Draws for every parameter of brms's own Stan data. Each special
  # coefficient multiplies its `mo()`, `me()` and `mi()` values and any
  # covariate it interacts with. A varying slope of a special term adds
  # its level's deviation to the coefficient.
  set.seed(5)
  n <- 12L
  dat <- data.frame(y = 0, z = rnorm(n), x = rnorm(n), sdx = 1e-12,
                    m = rep(1:3, 4L), f = gl(2L, 1L, n), g = gl(3L, 4L))
  # Draws of one parameter, as unnamed as the parts are
  take <- function(cols) unname(plain[, cols])
  draws_for <- function(pars) {
    posterior::as_draws_matrix(matrix(
      runif(2L * length(pars), 0.2, 1.5), 2L, dimnames = list(NULL, pars)
    ))
  }
  vec <- function(stem, k) paste0(stem, "[", seq_len(k), "]")
  mo_of <- function(simo, x) {
    cbind(0, t(apply(simo, 1L, cumsum)))[, x + 1L] * ncol(simo)
  }

  fit <- brms::brm(y ~ mo(m) + mo(m):z + mo(m):f + (mo(m) | g), dat,
                   empty = TRUE)
  draws <- draws_for(c("b_Intercept", vec("bsp", 3L), vec("simo_1", 2L),
                       vec("simo_2", 2L), vec("simo_3", 2L),
                       vec("r_1_1", 3L), vec("r_1_2", 3L)))
  plain <- as_plain_matrix(draws)
  prep <- prepare_linpred_data(draws, fit, dat)
  parts <- linear_terms_parts(prep, plain, prep$predictors[[1L]], n)
  expect_identical(names(parts), c("Intercept", "(1 + mo(m) | g)", "mo(m)",
                                   "mo(m):z", "mo(m):f"))
  sd <- prep$sdata
  J <- as.integer(dat$g)
  expect_equal(parts[["(1 + mo(m) | g)"]], take(vec("r_1_1", 3L))[, J])
  expect_equal(
    parts[["mo(m)"]],
    (take("bsp[1]") + take(vec("r_1_2", 3L))[, J]) *
      mo_of(take(vec("simo_1", 2L)), sd$Xmo_1)
  )
  expect_equal(parts[["mo(m):z"]],
               take("bsp[2]") * mo_of(take(vec("simo_2", 2L)), sd$Xmo_2) *
                 rep(dat$z, each = 2L))
  # With `mo(m)` in the formula, `mo(m):f` is the contrast of the second
  # level of `f`
  expect_equal(parts[["mo(m):f"]],
               take("bsp[3]") * mo_of(take(vec("simo_3", 2L)), sd$Xmo_3) *
                 rep(as.numeric(dat$f == "2"), each = 2L))
  expect_equal(Reduce(`+`, parts), extract_linpred_from_prep(prep))

  # The fit's noise-free values at the rows it was given. Other rows
  # draw them from the observed values, here measured without error.
  fit <- brms::brm(y ~ me(x, sdx):mo(m), dat, empty = TRUE)
  draws <- draws_for(c("b_Intercept", "bsp[1]", vec("simo_1", 2L),
                       vec("Xme_1", n)))
  plain <- as_plain_matrix(draws)
  prep <- prepare_linpred_data(draws, fit, dat)
  mo <- mo_of(take(vec("simo_1", 2L)), prep$sdata$Xmo_1)
  expect_equal(linear_terms_parts(prep, plain, prep$predictors[[1L]],
                                  n)[["me(x, sdx):mo(m)"]],
               take("bsp[1]") * take(vec("Xme_1", n)) * mo)
  # Reordered rows keep their own draws. A new row draws its value from
  # the observed one, here measured without error.
  new <- dat[c(n:2, 1L), ]
  new$x[n] <- 5
  prep <- prepare_linpred_data(draws, fit, new)
  xme <- cbind(take(vec("Xme_1", n))[, n:2], 5)
  expect_equal(linear_terms_parts(prep, plain, prep$predictors[[1L]],
                                  n)[["me(x, sdx):mo(m)"]],
               take("bsp[1]") * xme *
                 mo_of(take(vec("simo_1", 2L)), prep$sdata$Xmo_1))

  # A missing value of an `mi()` variable takes its imputed draw at the
  # rows the fit was given
  dat$x[c(2L, 7L)] <- NA
  fit <- brms::brm(brms::bf(y ~ mi(x)) + brms::bf(x | mi() ~ z) +
                     brms::set_rescor(FALSE), dat, empty = TRUE)
  draws <- draws_for(c("b_y_Intercept", "bsp_y[1]", vec("Ymi_x", 2L)))
  plain <- as_plain_matrix(draws)
  prep <- prepare_linpred_data(draws, fit, dat)
  x <- matrix(dat$x, 2L, n, byrow = TRUE)
  x[, c(2L, 7L)] <- take(vec("Ymi_x", 2L))
  expect_equal(
    linear_terms_parts(prep, plain, find_predictor(prep, "y"), n)[["mi(x)"]],
    take("bsp_y[1]") * x
  )
  # Reordered rows keep their own draws.
  prep <- prepare_linpred_data(draws, fit, dat[c(7L, 1L, 2L), ])
  expect_equal(
    linear_terms_parts(prep, plain, find_predictor(prep, "y"), 3L)[["mi(x)"]],
    take("bsp_y[1]") * x[, c(7L, 1L, 2L)]
  )
  # A missing value at a new row comes from its own predictor
  new <- dat[2L, ]
  new$z <- 9
  draws <- draws_for(c("b_y_Intercept", "bsp_y[1]", vec("Ymi_x", 2L),
                       "b_x_Intercept", "b_x[1]"))
  plain <- as_plain_matrix(draws)
  prep <- prepare_linpred_data(
    draws, fit, new, impute = function(var, mu, rows) mu + 1
  )
  expect_equal(
    linear_terms_parts(prep, plain, find_predictor(prep, "y"), 1L)[["mi(x)"]],
    take("bsp_y[1]") * matrix(take("b_x_Intercept") + 9 * take("b_x[1]") + 1)
  )
})
