#' Unit tests for the combination logic in get_combined_linpred()
#'
#' get_combined_linpred() takes the obs and trend linear predictors and
#' adds them on the link scale. It must handle three shapes that the
#' upstream extract_component_linpred() can return:
#'
#'   - both obs and trend as single matrix (univariate model)
#'   - obs as named list, trend as single matrix (multivariate obs +
#'     shared trend across responses; the most common multivariate
#'     case in mvgam, exercised by tests/local fit2)
#'   - obs as named list, trend as named list (multivariate obs with
#'     one trend per response, which is what a conditional read of a
#'     response-keyed fit gives: there each response is its own series
#'     and holds its own `trend[t, s]` column)
#'
#' Tests use testthat::local_mocked_bindings() to feed deterministic
#' inputs through the combination, avoiding the need for fitted Stan
#' models in fast unit tests.

stub_obj <- function(has_trend = TRUE, n_draws = 0L) {
  out <- structure(list(formula = brms::bf(y ~ 1)), class = "mvgam")
  if (has_trend) {
    out$trend_model <- list(formula = ~ 1)
  }
  # A conditional read reaches for the posterior before it asks for
  # the latent state, so those tests need draws to subset.
  if (n_draws > 0L) {
    out$fit <- posterior::draws_matrix(par = seq_len(n_draws))
  }
  out
}


test_that("get_combined_linpred adds obs and trend in univariate case", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(1, nrow = 4, ncol = 3)
      } else {
        matrix(0.5, nrow = 4, ncol = 3)
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = TRUE)
  expect_equal(dim(out), c(4L, 3L))
  expect_equal(out, matrix(1.5, 4, 3))
})


test_that("get_combined_linpred handles list obs + matrix shared trend", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        list(
          y1 = matrix(1, nrow = 5, ncol = 4),
          y2 = matrix(10, nrow = 5, ncol = 4)
        )
      } else {
        # Shared trend: single matrix applies to both responses
        matrix(0.25, nrow = 5, ncol = 4)
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = TRUE)
  expect_named(out, c("y1", "y2"))
  expect_equal(out$y1, matrix(1.25, 5, 4))
  expect_equal(out$y2, matrix(10.25, 5, 4))
})


test_that("get_combined_linpred handles list obs + list per-response trend", {
  # A conditional read of a response-keyed fit takes this shape: the
  # series axis there is the responses, so each one has its own
  # latent state and they are composed against their own arm.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        list(
          y1 = matrix(1, nrow = 5, ncol = 4),
          y2 = matrix(10, nrow = 5, ncol = 4)
        )
      } else {
        list(
          y1 = matrix(0.1, nrow = 5, ncol = 4),
          y2 = matrix(2.0, nrow = 5, ncol = 4)
        )
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = TRUE)
  expect_named(out, c("y1", "y2"))
  expect_equal(out$y1, matrix(1.1, 5, 4))
  expect_equal(out$y2, matrix(12.0, 5, 4))
})


test_that("get_combined_linpred process_error=FALSE preserves per-draw trend", {
  # process_error = FALSE must not collapse the trend to its
  # column-mean. The deterministic-submodel draws (X %*% b_trend) are
  # legitimate per-draw coefficient uncertainty and must ride through
  # unchanged. Only the marginal latent-state noise contribution is
  # toggled by `process_error`.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(0, 3, 4)
      } else {
        # Three draws with distinct values per row; the result must
        # preserve each draw's row, not broadcast a single posterior
        # mean across rows.
        rbind(c(0, 0, 0, 0),
              c(1, 1, 1, 1),
              c(2, 2, 2, 2))
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(), newdata = NULL,
                              process_error = FALSE)
  expect_equal(out, rbind(c(0, 0, 0, 0),
                           c(1, 1, 1, 1),
                           c(2, 2, 2, 2)))
})


test_that("get_combined_linpred errors on dimension mismatch (univariate)", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(0, 3, 4)
      } else {
        matrix(0, 3, 5)  # mismatched ncols
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  expect_error(
    get_combined_linpred(stub_obj(), newdata = NULL,
                         process_error = TRUE),
    "has the wrong shape"
  )
})


test_that("get_combined_linpred errors on dim mismatch (per-response)", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        list(y1 = matrix(0, 3, 4), y2 = matrix(0, 3, 4))
      } else {
        list(y1 = matrix(0, 3, 4), y2 = matrix(0, 3, 5))  # y2 mismatch
      }
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  expect_error(
    get_combined_linpred(stub_obj(), newdata = NULL,
                         process_error = TRUE),
    "has the wrong shape"
  )
})


test_that("get_combined_linpred returns obs only when no trend model", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      stopifnot(component == "obs")
      matrix(7, 3, 2)
    },
    .package = "mvgam"
  )
  out <- get_combined_linpred(stub_obj(has_trend = FALSE),
                              newdata = NULL, process_error = TRUE)
  expect_equal(out, matrix(7, 3, 2))
})


# transform = TRUE answers for `mu` on its own scale, which is the
# inverse of that parameter's link and nothing more. brms does the same
# by setting `dpar = "mu"`; `posterior_epred()` is what answers for
# `E[Y]`, and the two differ wherever a family moves mass to zero,
# multiplies by trials or carries a Jensen correction.
test_that("posterior_linpred(transform = TRUE) applies mu's inverse link", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") matrix(1, 4, 3) else matrix(0.5, 4, 3)
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  obj <- stub_obj()
  obj$data <- data.frame(x = 1:3)
  obj$family <- poisson()

  linear <- posterior_linpred.mvgam(obj, transform = FALSE,
                                    process_error = TRUE)
  response <- posterior_linpred.mvgam(obj, transform = TRUE,
                                      process_error = TRUE)

  expect_equal(linear, matrix(1.5, 4, 3))
  expect_equal(response, exp(linear))
})

test_that("posterior_linpred(transform = TRUE) does not answer for E[Y]", {
  # A binomial `mu` is a probability, so the transformed predictor must
  # not carry the trial counts that `posterior_epred()` multiplies in.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") matrix(0, 4, 3) else matrix(0, 4, 3)
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  obj <- stub_obj()
  obj$data <- data.frame(x = 1:3)
  obj$family <- binomial()

  out <- posterior_linpred.mvgam(obj, transform = TRUE,
                                 process_error = TRUE)
  expect_equal(out, matrix(0.5, 4, 3))
  expect_true(all(out <= 1))
})


test_that("posterior_linpred(transform = FALSE) keeps link-scale path", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") matrix(1, 4, 3) else matrix(0.5, 4, 3)
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  obj <- stub_obj()
  obj$data <- data.frame(x = 1:3)
  out <- posterior_linpred.mvgam(obj, transform = FALSE,
                                 process_error = TRUE)
  expect_equal(out, matrix(1.5, 4, 3))
})


test_that("posterior_linpred(transform = ...) validated as flag", {
  expect_error(
    posterior_linpred.mvgam(stub_obj(), transform = "yes"),
    "transform"
  )
  expect_error(
    posterior_linpred.mvgam(stub_obj(), transform = NA),
    "transform"
  )
})


# draw_ids plumbing: posterior_linpred -> get_combined_linpred ->
# extract_component_linpred(twice) must thread the same draw_ids so mu
# / sigma extractions stay aligned to a single posterior subset.

test_that("posterior_linpred(draw_ids = ...) forwards through stack", {
  captured <- list()
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component,
                                         ...) {
      args <- list(...)
      captured[[component]] <<- args$draw_ids
      matrix(1, nrow = if (is.null(args$draw_ids)) 4L else
        length(args$draw_ids), ncol = 3L)
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  obj <- stub_obj()
  obj$data <- data.frame(x = 1:3)
  posterior_linpred.mvgam(obj, transform = FALSE,
                          draw_ids = c(2L, 5L, 11L))
  expect_identical(captured$obs, c(2L, 5L, 11L))
  expect_identical(captured$trend, c(2L, 5L, 11L))
})


test_that("posterior_linpred(draw_ids = TRUE) rejects non-integer", {
  expect_error(
    posterior_linpred.mvgam(stub_obj(), draw_ids = c(1.5, 2.5)),
    "draw_ids"
  )
})


# ---- The linear-predictor composer --------------------------------
#
# A prep as `prepare_linpred_data()` builds it: the draws,
# the Stan data brms writes, the formula, the group-level table and
# the `re_formula` the data was built under. The end-to-end check
# against brms's own predictor lives in tests/local, which needs fits.

mk_prep <- function(sdata, draws, formula, ranef = NULL, re_formula = NULL,
                    nobs = NULL) {
  structure(list(
    draws = posterior::as_draws_matrix(draws),
    sdata = sdata,
    nobs = nobs %||% nrow(sdata$X %||% sdata[[1L]]),
    formula = formula,
    predictors = model_predictors(formula),
    ranef = ranef,
    re_formula = re_formula
  ), class = "mvgam_prep")
}

one_draw <- function(...) {
  values <- c(...)
  matrix(values, nrow = 1L, dimnames = list(NULL, names(values)))
}

# A design as brms writes it, each column numbered by its formula term
# in `assign`, the intercept as 0
design <- function(X, assign) {
  structure(X, assign = as.integer(assign))
}

# An intercept-only design of `n` rows
intercept_design <- function(n) {
  design(cbind(Intercept = rep(1, n)), 0L)
}


test_that("predictor_suffix() writes brms's order of parts", {
  expect_identical(predictor_suffix(), "")
  # A univariate program passes its response key as ""
  expect_identical(predictor_suffix(resp = ""), "")
  expect_identical(predictor_suffix(dpar = "mu"), "")
  expect_identical(predictor_suffix(resp = "y1"), "_y1")
  expect_identical(predictor_suffix(dpar = "sigma"), "_sigma")
  expect_identical(predictor_suffix(resp = "y1", dpar = "sigma"),
                   "_sigma_y1")
  expect_identical(predictor_suffix(resp = "y1", nlpar = "a"), "_y1_a")
})


test_that("the intercept column goes only when brms drew an intercept", {
  # `y ~ 0 + ones`: the column of ones is a regressor with its own b[1].
  X <- design(cbind(ones = rep(1, 5)), 1L)
  lp <- extract_linpred_from_prep(mk_prep(
    list(X = X), one_draw(`b[1]` = 0.788), brms::bf(y ~ 0 + ones)
  ))
  expect_equal(unname(lp), matrix(0.788, 1L, 5L))

  # `y ~ 1 + x`: brms samples the slope against the design without its
  # "Intercept" column and writes the intercept as b_Intercept.
  X <- design(cbind(Intercept = 1, x = c(0, 0.5, 1, 1.5, 2)), 0:1)
  lp <- extract_linpred_from_prep(mk_prep(
    list(X = X), one_draw(b_Intercept = 0.5, `b[1]` = 2),
    brms::bf(y ~ 1 + x)
  ))
  expect_equal(unname(lp), matrix(0.5 + 2 * X[, "x"], 1L))
})


test_that("each predictor reads only the terms carrying its own suffix", {
  # `bf(y ~ 1, sigma ~ s(x) + offset(o))` writes the mean's design bare
  # and sigma's with `_sigma`. Reading every smooth for the mean put
  # sigma's smooth into it, and keeping the bare `offsets` for sigma put
  # the mean's offset into sigma.
  Zs <- matrix(c(1, 0, 2, 1, 1, 0), nrow = 3L)
  sdata <- list(
    X = intercept_design(3L), offsets = c(10, 20, 30),
    X_sigma = intercept_design(3L),
    # One smooth object, with no unpenalised columns
    Xs_sigma = structure(matrix(0, 3L, 0L), smcols = list(integer(0L)),
                         bylevels = list(`s(x)` = NULL)),
    nb_sigma_1 = 1L, Zs_sigma_1_1 = Zs, offsets_sigma = c(1, 1, 1)
  )
  draws <- one_draw(b_Intercept = 2, b_sigma_Intercept = -1,
                    `s_sigma_1_1[1]` = 0.5, `s_sigma_1_1[2]` = 3)
  prep <- mk_prep(sdata, draws, brms::bf(y ~ 1, sigma ~ s(x)))
  expect_equal(unname(extract_linpred_from_prep(prep)),
               matrix(2 + c(10, 20, 30), 1L))
  expect_equal(unname(extract_linpred_from_prep(prep, dpar = "sigma")),
               matrix(-1 + 1 + drop(Zs %*% c(0.5, 3)), 1L))
})


# The group-level table brms keeps on a fit, one row per term.
mk_ranef <- function(rows, levels) {
  ranef <- data.frame(
    id = rows$id, group = rows$group, gn = rows$gn %||% 1L, gtype = "",
    coef = "Intercept", cn = rows$cn %||% 1L, resp = rows$resp %||% "",
    dpar = rows$dpar %||% "", nlpar = "", type = "", cor = TRUE,
    stringsAsFactors = FALSE
  )
  ranef$form <- rep(list(~1), nrow(ranef))
  attr(ranef, "levels") <- levels
  ranef
}


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


test_that("re_formula takes NULL or NA and refuses a formula", {
  # brms numbers the group-level terms a formula keeps afresh, and the
  # fitted draws keep the numbers the whole model gave them. On
  # `(1 | g) + (0 + x || g)`, `re_formula = ~ (0 + x || g)` writes the
  # slope's design as block 1, and reading it against the fitted block
  # 1 pairs the slope's design with the intercept's coefficients.
  expect_null(validate_group_level_args(NULL, FALSE, "uncertainty"))
  expect_null(validate_group_level_args(NA, FALSE, "uncertainty"))
  expect_error(
    validate_group_level_args(~ (0 + x || g), FALSE, "uncertainty"),
    "takes NULL or NA"
  )
  # brms changes how a new level is sampled only when it is allowed one.
  expect_error(
    validate_group_level_args(NULL, FALSE, "gaussian"),
    "'allow_new_levels' is TRUE"
  )
  expect_null(validate_group_level_args(NULL, TRUE, "gaussian"))
})


test_that("a draw count and a set of draw indices are not both read", {
  # `draw_ids` names the draws to read and `ndraws` takes that many at
  # random, so the resolver honours the indices and the count goes
  # unread. Either alone passes.
  expect_null(validate_draw_selectors(NULL, NULL))
  expect_null(validate_draw_selectors(10L, NULL))
  expect_null(validate_draw_selectors(NULL, 1:3))
  err <- expect_error(
    validate_draw_selectors(10L, 1:3),
    "Specify only one"
  )
  # The refusal names both quantities, which is what tells a caller
  # which of the two to drop.
  expect_match(conditionMessage(err), "10")
  expect_match(conditionMessage(err), "3")
})


test_that("a response of a model with several reads its own terms", {
  # brms suffixes a response's names with its key, and indexes its
  # grouping by `J_<id>_<resp>`. The fixed smooth `Xs_y2` and the
  # random effect `r_1_y1_1` were both dropped.
  # One smooth object, its unpenalised column alone
  Xs <- structure(cbind(c(1, 2, 3)), smcols = list(1L),
                  bylevels = list(`s(x)` = NULL))
  sdata <- list(
    N_y1 = 3L, N_y2 = 3L,
    X_y1 = intercept_design(3L), Z_1_y1_1 = rep(1, 3),
    J_1_y1 = c(2L, 1L, 2L),
    X_y2 = intercept_design(3L), Xs_y2 = Xs, nb_y2_1 = 0L,
    offsets_y2 = c(0.5, 0.5, 0.5)
  )
  draws <- one_draw(
    b_y1_Intercept = 1, `r_1_y1_1[1]` = -1, `r_1_y1_1[2]` = 4,
    b_y2_Intercept = 2, `bs_y2[1]` = 10
  )
  ranef <- mk_ranef(list(id = 1L, group = "g", resp = "y1"),
                    list(g = c("a", "b")))
  prep <- mk_prep(sdata, draws,
                  brms::bf(y1 ~ (1 | g)) + brms::bf(y2 ~ s(x)),
                  ranef = ranef, nobs = 3L)
  lp <- extract_linpred_from_prep(prep)
  expect_identical(names(lp), c("y1", "y2"))
  expect_equal(unname(lp$y1), matrix(1 + c(4, -1, 4), 1L))
  expect_equal(unname(lp$y2), matrix(2 + 10 * c(1, 2, 3) + 0.5, 1L))
  expect_equal(unname(extract_linpred_from_prep(prep, resp = "y2")),
               unname(lp$y2))
})


test_that("a monotonic term reads each level as brms codes it", {
  # Three levels coded 0..2. A frame holding only the top level is
  # coded 2 and takes the whole effect, D * (simo1 + simo2).
  sdata <- list(Jmo = 2L, Xmo_1 = c(2L, 2L))
  draws <- one_draw(`simo_1[1]` = 0.25, `simo_1[2]` = 0.75)
  expect_equal(
    unname(mo_value(as_plain_matrix(posterior::as_draws_matrix(draws)),
                    sdata, "", 1L, 2L)),
    matrix(2 * (0.25 + 0.75), 1L, 2L)
  )
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
  # Reordered rows keep their own draws. A new row needs the value.
  prep <- prepare_linpred_data(draws, fit, dat[c(7L, 1L, 2L), ])
  expect_equal(
    linear_terms_parts(prep, plain, find_predictor(prep, "y"), 3L)[["mi(x)"]],
    take("bsp_y[1]") * x[, c(7L, 1L, 2L)]
  )
  new <- dat[2L, ]
  new$z <- 9
  prep <- prepare_linpred_data(draws, fit, new)
  expect_error(
    linear_terms_parts(prep, plain, find_predictor(prep, "y"), 1L),
    "needs a value of 'x'"
  )
})


test_that("a non-linear mean is its expression at its parameters", {
  # Each non-linear parameter is a predictor of its own, suffixed by its
  # name, and the covariate the expression reads is `C_1`.
  sdata <- list(
    X_a = intercept_design(3L), X_b = intercept_design(3L),
    C_1 = c(0, 1, 2)
  )
  draws <- one_draw(`b_a[1]` = 2, `b_b[1]` = 0.5)
  form <- brms::bf(y ~ a * exp(b * x), a ~ 1, b ~ 1, nl = TRUE)
  lp <- extract_linpred_from_prep(mk_prep(sdata, draws, form, nobs = 3L))
  expect_equal(unname(lp), matrix(2 * exp(0.5 * c(0, 1, 2)), 1L))
})


test_that("each Gaussian process takes the kernel its own gp() names", {
  expect_identical(
    gp_kernels(~ gp(x, cov = "matern32", k = 5) + gp(z, k = 4)),
    c(`gp(x, cov = "matern32", k = 5)` = "matern32",
      `gp(z, k = 4)` = "exp_quad")
  )
  expect_identical(gp_kernels(NULL), character(0))
})


# ---- Distributional parameters -----------------------------------

test_that("predicted_dpar_names() names only what carries a formula", {
  obj <- structure(
    list(formula = brms::bf(y ~ x, sigma ~ x)),
    class = c("mvgam", "brmsfit")
  )
  expect_equal(predicted_dpar_names(obj, c("sigma", "nu")), "sigma")
  # Non-linear parameters live in `pforms` too, and only the family's
  # own parameters are asked for.
  expect_equal(predicted_dpar_names(obj, "nu"), character())

  scalar_only <- structure(
    list(formula = brms::bf(y ~ x)),
    class = c("mvgam", "brmsfit")
  )
  expect_equal(predicted_dpar_names(scalar_only, "sigma"), character())

  mv <- structure(
    list(formula = brms::bf(y1 ~ x, sigma ~ x) + brms::bf(y2 ~ x)),
    class = c("mvgam", "brmsfit")
  )
  # Scoping to a response asks only that arm.
  expect_equal(predicted_dpar_names(mv, "sigma", resp = "y1"), "sigma")
  expect_equal(predicted_dpar_names(mv, "sigma", resp = "y2"), character())
})


test_that("dpar_link() reads the parameter's own link", {
  # brms keeps the mean's link apart from the rest.
  expect_equal(dpar_link(brms::Beta(), "phi"), "log")
  expect_equal(dpar_link(com_binomial(), "nu"), "identity")
  expect_equal(dpar_link(occ(), "p"), "logit")
  # Poisson-Poisson's `p` is an encounter rate rather than a
  # probability, so it is log-linked where its siblings are logit.
  expect_equal(dpar_link(nmix("poisson_poisson"), "p"), "log")

  # A family built by stats carries no per-parameter links, so the
  # default brms would have applied is what counts. Reading these as
  # identity would return the parameter on the link scale.
  expect_equal(dpar_link(gaussian(), "sigma"), "log")
  expect_equal(dpar_link(stats::Gamma(), "shape"), "log")

  # A parameter the family has no link for anywhere is an error, not
  # a silent identity.
  expect_error(
    dpar_link(poisson(), "sigma"),
    "Family 'poisson' lacks a link for parameter 'sigma'",
    fixed = TRUE
  )
})


test_that("resolve_draw_indices() is the one rule for choosing draws", {
  # Indices the caller already holds are passed through untouched, so
  # a count can never override an explicit choice.
  expect_equal(resolve_draw_indices(100L, ndraws = 10L,
                                    draw_ids = c(2L, 5L)),
               c(2L, 5L))
  expect_error(
    resolve_draw_indices(100L, ndraws = NULL, draw_ids = c(1L, 101L)),
    "exceed available draws"
  )
  # An index that names no draw is refused. Zero and a negative index
  # would drop rows under R's subsetting, a fraction would truncate and
  # a missing index would pass an `NA` row through.
  # The check runs once, where a user's arguments enter.
  for (bad in list(c(0L, 3L), c(-1L, 3L), c(1.5, 3), c(NA, 3L))) {
    expect_error(validate_draw_selectors(NULL, bad), "draw_ids")
  }
  expect_error(validate_draw_selectors(0L, NULL), "ndraws")
  expect_error(validate_draw_selectors(5L, 1:5),
               "only one of 'ndraws' or 'draw_ids'")
  # No count and no indices means every draw, in the order sampled.
  expect_equal(resolve_draw_indices(10L, NULL, NULL), seq_len(10L))
  # A count covering the posterior also keeps that order. Returning
  # nothing here, or a permutation, is what let two extractions
  # disagree while both claiming to use every draw.
  expect_equal(resolve_draw_indices(10L, ndraws = 10L, draw_ids = NULL),
               seq_len(10L))
  # Asking for more than exists is refused rather than truncated.
  expect_error(
    resolve_draw_indices(10L, ndraws = 11L, draw_ids = NULL),
    "more draws than the posterior has"
  )
  # A smaller count gives that many distinct indices, in order.
  set.seed(1L)
  ids <- resolve_draw_indices(100L, ndraws = 10L, draw_ids = NULL)
  expect_length(ids, 10L)
  expect_equal(ids, sort(ids))
  expect_equal(anyDuplicated(ids), 0L)
  expect_true(all(ids >= 1L & ids <= 100L))
})


test_that("resolve_draw_ids() materialises a count as indices", {
  stub <- structure(
    list(fit = posterior::as_draws_matrix(
      posterior::draws_matrix(a = rnorm(100))
    )),
    class = c("mvgam", "brmsfit")
  )
  # Indices the caller already has are passed straight through, so a
  # count is never allowed to override them.
  expect_equal(resolve_draw_ids(stub, ndraws = 10, draw_ids = c(2L, 5L)),
               c(2L, 5L))
  # Only the absence of a count leaves nothing to choose.
  expect_null(resolve_draw_ids(stub, ndraws = NULL, draw_ids = NULL))
  # A count covering the whole posterior still resolves, because the
  # extractions subsample at random: handed the bare count they would
  # each return every draw in a different order.
  expect_equal(resolve_draw_ids(stub, ndraws = 100, draw_ids = NULL),
               seq_len(100))
  # Asking for more than exists is an error, not a silent truncation.
  expect_error(resolve_draw_ids(stub, ndraws = 150, draw_ids = NULL),
               "more draws than the posterior has")
  # A count below the total becomes that many sorted, distinct indices.
  set.seed(1L)
  ids <- resolve_draw_ids(stub, ndraws = 10, draw_ids = NULL)
  expect_length(ids, 10L)
  expect_equal(ids, sort(ids))
  expect_equal(anyDuplicated(ids), 0L)
  expect_true(all(ids >= 1L & ids <= 100L))
})


test_that("trend_state = 'conditional' reads the fitted state", {
  # The conditional surface takes `trend[t, s]` and must not add the
  # deterministic trend submodel on top of it: the Stan kernel is
  # written on the centred convention, so that contribution is already
  # inside the state and adding it again would count it twice.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") {
        matrix(1, nrow = 4, ncol = 3)
      } else {
        matrix(100, nrow = 4, ncol = 3)
      }
    },
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                           full_draws, ...) {
      matrix(0.25, nrow = 4, ncol = 3)
    },
    has_stochastic_trend = function(object) TRUE,
    sample_process_errors = function(...) matrix(999, nrow = 4, ncol = 3),
    .package = "mvgam"
  )
  out <- get_combined_linpred(
    stub_obj(n_draws = 4L), newdata = NULL, process_error = TRUE,
    trend_state = "conditional", draw_ids = 1:4
  )
  expect_equal(out, matrix(1.25, 4, 3))
})


test_that("trend_state = 'marginal' samples the innovations once", {
  # The innovations are composed here and nowhere else. Adding a
  # second, independent sample downstream put twice the process
  # variance into every marginal prediction.
  n_calls <- 0L
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      matrix(0, nrow = 4, ncol = 3)
    },
    has_stochastic_trend = function(object) TRUE,
    sample_process_errors = function(...) {
      n_calls <<- n_calls + 1L
      matrix(2, nrow = 4, ncol = 3)
    },
    .package = "mvgam"
  )
  out <- get_combined_linpred(
    stub_obj(), newdata = NULL, process_error = TRUE,
    trend_state = "marginal"
  )
  expect_equal(out, matrix(2, 4, 3))
  expect_identical(n_calls, 1L)
})


test_that("a conditional read ignores process_error", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      matrix(0, nrow = 4, ncol = 3)
    },
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                           full_draws, ...) {
      matrix(3, nrow = 4, ncol = 3)
    },
    has_stochastic_trend = function(object) TRUE,
    sample_process_errors = function(...) matrix(999, nrow = 4, ncol = 3),
    .package = "mvgam"
  )
  for (pe in c(TRUE, FALSE)) {
    out <- get_combined_linpred(
      stub_obj(n_draws = 4L), newdata = NULL, process_error = pe,
      trend_state = "conditional", draw_ids = 1:4
    )
    expect_equal(out, matrix(3, 4, 3))
  }
})


test_that("a fit missing its trend draws is a fault", {
  # Every trend writes `trend[t, s]`, PW and ZMVN included. Every fit
  # keeps it. The conditional surface is built from it.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(...) matrix(1, 4, 3),
    extract_trend_latent_states = function(...) NULL,
    .package = "mvgam"
  )
  expect_error(
    get_combined_linpred(stub_obj(n_draws = 4L), newdata = NULL,
                         trend_state = "conditional", draw_ids = 1:4),
    "Please report this mvgam fault"
  )
})


test_that("trend_state rejects an unknown surface", {
  expect_error(
    get_combined_linpred(stub_obj(), newdata = NULL,
                          trend_state = "nonsense"),
    "should be one of"
  )
})


test_that("autocor_to_trend_state maps the surface selector", {
  expect_identical(autocor_to_trend_state(TRUE), "conditional")
  expect_identical(autocor_to_trend_state(FALSE), "marginal")
  expect_error(autocor_to_trend_state("conditional"), "logical")
  expect_error(autocor_to_trend_state(c(TRUE, FALSE)), "length 1")
  expect_error(autocor_to_trend_state(NA), "missing")
})


test_that("every prediction entry point spells the surface the same way", {
  # One idea, one name: `incl_autocor` picks the surface and
  # `process_error` decides whether the marginal surface samples
  # innovations. A method offering either under a second spelling is
  # how the two came to disagree.
  entry_points <- list(
    posterior_epred = posterior_epred.mvgam,
    posterior_predict = posterior_predict.mvgam,
    posterior_linpred = posterior_linpred.mvgam,
    predict = predict.mvgam,
    fitted = fitted.mvgam
  )
  for (nm in names(entry_points)) {
    args <- formals(entry_points[[nm]])
    expect_true("incl_autocor" %in% names(args))
    expect_true("process_error" %in% names(args))
    expect_false("trend_state" %in% names(args))
  }
})


test_that("only a caller of get_combined_linpred names trend_state", {
  # `trend_state` is the name `get_combined_linpred()` reads.
  # Every other prediction function takes the user-facing
  # `incl_autocor`, so naming `trend_state` across that boundary
  # puts it in `...`, where it is dropped and the surface falls back
  # to the default without saying so. A method may therefore mention
  # `trend_state` only if it calls `get_combined_linpred()` itself.
  methods <- list(
    posterior_epred = posterior_epred.mvgam,
    posterior_predict = posterior_predict.mvgam,
    posterior_linpred = posterior_linpred.mvgam,
    predict = predict.mvgam,
    fitted = fitted.mvgam,
    log_lik = log_lik.mvgam
  )
  for (nm in names(methods)) {
    src <- paste(deparse(body(methods[[nm]])), collapse = " ")
    reads_internal <- grepl("get_combined_linpred", src, fixed = TRUE)
    if (!reads_internal) {
      expect_false(grepl("trend_state", src, fixed = TRUE))
    }
  }
})


test_that("posterior_predict forwards the surface it was given", {
  # The delegation this pins is to a sibling public method rather
  # than to `get_combined_linpred()`, which is where the argument
  # changes name.
  src <- paste(deparse(body(posterior_predict.mvgam)), collapse = " ")
  expect_true(grepl("incl_autocor = incl_autocor", src, fixed = TRUE))
})


test_that("the prediction entry points share one set of defaults", {
  # `predict()` forwards to `posterior_predict()`, so a default that
  # differs between them answers two ways on one fit with no argument
  # given.
  entry_points <- list(
    posterior_epred.mvgam, posterior_predict.mvgam,
    posterior_linpred.mvgam, predict.mvgam
  )
  for (fn in entry_points) {
    args <- formals(fn)
    expect_identical(eval(args$process_error), FALSE)
    expect_identical(eval(args$incl_autocor), FALSE)
  }
  # `fitted()` includes the fitted state for the training data.
  expect_identical(formals(fitted.mvgam)$incl_autocor,
                   quote(is.null(newdata)))
})


test_that("the ELPD surfaces condition on the fitted state", {
  # Decision 22 assigns model comparison to the conditional surface,
  # so `log_lik()` and everything built on it default the other way
  # from the prediction methods.
  expect_identical(eval(formals(log_lik.mvgam)$incl_autocor), TRUE)
  expect_identical(eval(formals(loo.mvgam)$incl_autocor), TRUE)
  expect_identical(eval(formals(waic.mvgam)$incl_autocor), TRUE)
})


test_that("diagnostic_surface_args names the surface as incl_autocor", {
  # In sample the conditional state, out of sample the marginal, and
  # the conditional state whenever importance weights are involved.
  expect_identical(
    diagnostic_surface_args(list(), in_sample = TRUE)$incl_autocor, TRUE
  )
  expect_null(
    diagnostic_surface_args(list(), in_sample = FALSE)$incl_autocor
  )
  expect_identical(
    diagnostic_surface_args(
      list(), in_sample = FALSE, weighted = TRUE
    )$incl_autocor,
    TRUE
  )
  # A caller that named the surface keeps it.
  expect_identical(
    diagnostic_surface_args(
      list(incl_autocor = FALSE), in_sample = TRUE
    )$incl_autocor,
    FALSE
  )
  # The caller states the fact rather than handing over a frame, so a
  # frame cannot be mistaken for an answer to it.
  expect_error(
    diagnostic_surface_args(list(), data.frame(x = 1)),
    "in_sample"
  )
})


test_that("a grouping level the model never saw is named, not indexed", {
  # brms extends the grouping index under `allow_new_levels = TRUE`,
  # but no coefficient was ever drawn for the new level. Indexing
  # past the end of the draws would give `subscript out of bounds`,
  # an internal message even though `allow_new_levels` is brms's own
  # documented way to handle this case.
  J <- c(1L, 2L, 4L)
  prep <- mk_prep(
    list(Z_1_1 = rep(1, 3), J_1 = J),
    one_draw(`r_1_1[1]` = 1, `r_1_1[2]` = 1, `r_1_1[3]` = 1),
    brms::bf(y ~ 0 + (1 | grp)),
    ranef = mk_ranef(list(id = 1L, group = "grp"),
                     list(grp = c("a", "b", "c"))),
    nobs = 3L
  )
  # The message names the factor and the way out.
  expect_error(extract_linpred_from_prep(prep),
               "grouping level the model never saw")
  expect_error(extract_linpred_from_prep(prep), "'grp'")
  expect_error(extract_linpred_from_prep(prep), "re_formula = NA",
               fixed = TRUE)
})


test_that("the prediction entry points share one new-level default", {
  # brms's own `snl_options` is uncertainty / gaussian / old_levels,
  # and each entry point defaults to the first, as brms does.
  for (fn in list(predict.mvgam, fitted.mvgam, posterior_predict.mvgam,
                  posterior_epred.mvgam, posterior_linpred.mvgam)) {
    expect_identical(eval(formals(fn)$sample_new_levels), "uncertainty")
  }
})
