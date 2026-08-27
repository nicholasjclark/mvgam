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
#'     per-response trend output; not produced by any current mvgam
#'     codegen path, but the combination branch must still be correct
#'     because the prediction-system contract supports it)
#'
#' Tests use testthat::local_mocked_bindings() to feed deterministic
#' inputs through the combination, avoiding the need for fitted Stan
#' models in fast unit tests.

stub_obj <- function(has_trend = TRUE, n_draws = 0L) {
  out <- structure(list(), class = "mvgam")
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
  expect_true(is.matrix(out))
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
  expect_type(out, "list")
  expect_named(out, c("y1", "y2"))
  expect_equal(out$y1, matrix(1.25, 5, 4))
  expect_equal(out$y2, matrix(10.25, 5, 4))
})


test_that("get_combined_linpred handles list obs + list per-response trend", {
  # The per-response trend list shape isn't produced by current mvgam
  # codegen (single trend type per fit; see architecture decisions),
  # but the combination logic supports it for completeness.
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
  expect_type(out, "list")
  expect_named(out, c("y1", "y2"))
  expect_equal(out$y1, matrix(1.1, 5, 4))
  expect_equal(out$y2, matrix(12.0, 5, 4))
})


test_that("get_combined_linpred process_error=FALSE preserves per-draw trend", {
  # process_error = FALSE no longer collapses the trend to its
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
    "Dimension mismatch"
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
    "Dimension mismatch"
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


# extract_linpred_univariate must only drop X[, 1] when it is the
# brms-reserved intercept column (i.e., `b_Intercept` is present in
# draws). Formulas written as `y ~ 0 + <regressor>` (including the
# `.mvgam_empty_obs` placeholder injected for empty obs sub-formulas)
# have no `b_Intercept`; their first column IS a real regressor with
# its own b[k] and must NOT be dropped.

mk_linpred_prep <- function(X, draws, formula_str = "y ~ 1") {
  structure(
    list(draws = draws, sdata = list(X = X), nobs = nrow(X),
         formula = brms::brmsformula(stats::as.formula(formula_str))),
    class = "brmsprep"
  )
}

test_that("extract_linpred_univariate keeps X[,1] when no b_Intercept", {
  # `y ~ 0 + ones`: X is a column of 1s, b[1] is the lone coef.
  # Pre-fix bug: code unconditionally dropped X[,1] if all-1s, so
  # the linpred came back as 0, losing the b[1] contribution.
  draws <- posterior::as_draws_matrix(matrix(
    rep(0.788, 4L), nrow = 4L, dimnames = list(NULL, "b[1]")
  ))
  X <- cbind(ones = rep(1, 5))
  lp <- extract_linpred_univariate(
    mk_linpred_prep(X, draws, "y ~ 0 + ones")
  )
  expect_identical(dim(lp), c(4L, 5L))
  expect_true(all(abs(lp - 0.788) < 1e-9))
})

test_that("extract_linpred_univariate drops intercept col when b_Intercept present", {
  # Standard `y ~ 1 + x`: b_Intercept handled separately, X[,1] is the
  # all-1s intercept column and should be dropped before b[k] %*% t(X).
  draws <- posterior::as_draws_matrix(matrix(
    c(rep(0.5, 4L), rep(2.0, 4L)), nrow = 4L,
    dimnames = list(NULL, c("b_Intercept", "b[1]"))
  ))
  X <- cbind(intercept = rep(1, 5), x = c(0, 0.5, 1, 1.5, 2))
  lp <- extract_linpred_univariate(
    mk_linpred_prep(X, draws, "y ~ 1 + x")
  )
  expected <- matrix(rep(0.5 + 2.0 * X[, "x"], each = 4L),
                       nrow = 4L, ncol = 5L)
  expect_identical(dim(lp), c(4L, 5L))
  expect_true(all(abs(lp - expected) < 1e-9))
})

test_that("extract_linpred_univariate keeps cell-means factor without intercept", {
  # `y ~ 0 + factor`: per-level indicators; col 1 is NOT all-1s
  # (only rows with the reference level are 1). Old code's all-1s
  # check returns FALSE; new code reaches the same branch. This
  # test guards the path for users writing cell-means formulas.
  X <- model.matrix(~ 0 + factor(c("a", "b", "c", "a", "b", "c")))
  draws <- posterior::as_draws_matrix(matrix(
    c(rep(1, 4L), rep(2, 4L), rep(3, 4L)), nrow = 4L,
    dimnames = list(NULL, c("b[1]", "b[2]", "b[3]"))
  ))
  lp <- extract_linpred_univariate(
    mk_linpred_prep(X, draws, "y ~ 0 + grp")
  )
  expected <- matrix(rep(c(1, 2, 3, 1, 2, 3), each = 4L),
                       nrow = 4L, ncol = 6L)
  expect_true(all(abs(lp - expected) < 1e-9))
})


# ---- Distributional parameters -----------------------------------

test_that("predicted_dpar_names() names only what carries a formula", {
  obj <- structure(
    list(formula = brms::bf(y ~ x, sigma ~ x)),
    class = c("mvgam", "brmsfit")
  )
  expect_equal(predicted_dpar_names(obj, c("sigma", "nu")), "sigma")
  # Non-linear parameters live in `pforms` too, so the family's own
  # parameters are the ones asked for.
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
  expect_error(dpar_link(poisson(), "sigma"), "No link is recorded")
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
    "more draws than the posterior holds"
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
               "more draws than the posterior holds")
  # A count below the total becomes that many sorted, distinct indices.
  set.seed(1L)
  ids <- resolve_draw_ids(stub, ndraws = 10, draw_ids = NULL)
  expect_length(ids, 10L)
  expect_equal(ids, sort(ids))
  expect_equal(anyDuplicated(ids), 0L)
  expect_true(all(ids >= 1L & ids <= 100L))
})


test_that("latent_state = 'conditional' reads the fitted state", {
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
    extract_trend_latent_states = function(mvgam_fit, newdata, full_draws) {
      matrix(0.25, nrow = 4, ncol = 3)
    },
    has_stochastic_trend = function(object) TRUE,
    sample_process_errors = function(...) matrix(999, nrow = 4, ncol = 3),
    .package = "mvgam"
  )
  out <- get_combined_linpred(
    stub_obj(n_draws = 4L), newdata = NULL, process_error = TRUE,
    latent_state = "conditional", draw_ids = 1:4
  )
  expect_equal(out, matrix(1.25, 4, 3))
})


test_that("latent_state = 'marginal' samples the innovations once", {
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
    latent_state = "marginal"
  )
  expect_equal(out, matrix(2, 4, 3))
  expect_identical(n_calls, 1L)
})


test_that("a conditional read ignores process_error", {
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      matrix(0, nrow = 4, ncol = 3)
    },
    extract_trend_latent_states = function(mvgam_fit, newdata, full_draws) {
      matrix(3, nrow = 4, ncol = 3)
    },
    has_stochastic_trend = function(object) TRUE,
    sample_process_errors = function(...) matrix(999, nrow = 4, ncol = 3),
    .package = "mvgam"
  )
  for (pe in c(TRUE, FALSE)) {
    out <- get_combined_linpred(
      stub_obj(n_draws = 4L), newdata = NULL, process_error = pe,
      latent_state = "conditional", draw_ids = 1:4
    )
    expect_equal(out, matrix(3, 4, 3))
  }
})


test_that("a fit with no latent state falls back to the submodel", {
  # A deterministic trend (PW, none) carries no `trend[t, s]` draws, so
  # the conditional read has nothing to condition on and the trend
  # contributes its deterministic submodel alone.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata, component, ...) {
      if (component == "obs") matrix(1, 4, 3) else matrix(0.5, 4, 3)
    },
    extract_trend_latent_states = function(mvgam_fit, newdata, full_draws) {
      NULL
    },
    has_stochastic_trend = function(object) FALSE,
    .package = "mvgam"
  )
  out <- get_combined_linpred(
    stub_obj(n_draws = 4L), newdata = NULL, process_error = TRUE,
    latent_state = "conditional", draw_ids = 1:4
  )
  expect_equal(out, matrix(1.5, 4, 3))
})


test_that("latent_state rejects an unknown surface", {
  expect_error(
    get_combined_linpred(stub_obj(), newdata = NULL,
                          latent_state = "nonsense"),
    "should be one of"
  )
})
