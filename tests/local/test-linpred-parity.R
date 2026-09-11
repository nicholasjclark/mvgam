# Every linear predictor mvgam composes is brms's own, read from the
# same draws.
#
# mvgam rebuilds each predictor term by term from the Stan data brms
# writes for `newdata`, and brms names every piece of a predictor by
# one rule: a suffix for a distributional parameter, then for a
# response of a model with several, then for a non-linear parameter.
# A reader that takes one spelling and misses another drops a term
# without a word, and each term type is therefore checked under each
# suffix: the random effects and smooths of an `mvbf()` response, a
# random effect in a parameter's own formula such as `sigma ~ (1 | g)`,
# and a smooth or an offset that belongs to one predictor and not to
# its neighbour.
#
# The reference is brms itself. The observation model mvgam keeps is a
# brms model whose parameters carry the names the combined program gave
# them, and handing it mvgam's draws lets brms compute each predictor
# from them. `brms:::rename_pars()` is internal to brms; it serves here
# as an oracle and nowhere in the package.
#
# Seven fits hold every term type under every suffix: a univariate
# model with a correlated random effect, an offset, a Matern GP and a
# modelled `sigma` carrying a smooth, a monotonic term and a random
# effect; an `mvbf()` model whose two responses carry a GP, a monotonic
# term, an offset, a smooth and a correlated random effect between
# them; a non-linear model with a smooth and a random effect in one
# parameter; a hurdle model whose `hu` has a random effect; a model
# whose mean has three smooths, one split by a factor, beside a smooth
# in `sigma`; a latent trend whose own formula has a smooth, a slope
# and a random effect; and a latent trend with an intercept alone.
#
# Run with:
#   testthat::test_file("tests/local/test-linpred-parity.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

fit_cached <- function(name, formula, family = gaussian(), ...) {
  path <- cache_path(paste0("val_linpred_", name, ".rds"))
  if (file.exists(path)) {
    cat("[cache]", name, "\n")
    return(readRDS(path))
  }
  cat("[fit  ]", name, "\n")
  fit <- mvgam(formula, family = family, data = parity_data, chains = 2L,
               iter = 600L, warmup = 300L, silent = 2,
               backend = "cmdstanr", seed = 1L, ...)
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  fit
}

parity_data <- local({
  set.seed(21)
  n_g <- 8L
  n <- 160L
  g <- factor(sample(letters[seq_len(n_g)], n, replace = TRUE))
  x <- stats::runif(n, -2, 2)
  z <- stats::rnorm(n)
  ord <- factor(sample(c("lo", "mid", "hi"), n, replace = TRUE),
                levels = c("lo", "mid", "hi"), ordered = TRUE)
  o <- stats::runif(n)
  u <- stats::rnorm(n_g, 0, 0.8)[g]
  v <- stats::rnorm(n_g, 0, 0.4)[g]
  out <- data.frame(
    y = 1 + (0.5 + v) * x + u + o +
      stats::rnorm(n, 0, exp(-0.5 + 0.3 * sin(x) + 0.2 * as.integer(ord))),
    y1 = sin(x) + 0.4 * as.integer(ord) + o + stats::rnorm(n, 0, 0.3),
    y2 = 0.5 * x^2 + u + v * x + stats::rnorm(n, 0, 0.4),
    y3 = 2 * exp(0.3 * x + u / 4) + stats::rnorm(n, 0, 0.3),
    cnt = ifelse(stats::runif(n) < stats::plogis(-0.5 + 0.5 * z + v),
                 0L, stats::rpois(n, exp(0.8 + 0.3 * x + u))),
    x = x, z = z, g = g, ord = ord, o = o,
    time = seq_len(n), series = factor("s1")
  )
  # Drawn after every other column, which keeps them as they were.
  out$gs <- factor(sample(c("p", "q", "r"), n, replace = TRUE))
  out$ys <- sin(2 * x) + ifelse(out$gs == "q", cos(z), 0) +
    stats::rnorm(n, 0, exp(-1 + 0.3 * z))
  out
})

fits <- list(
  uni = fit_cached(
    "uni",
    bf(y ~ x + (1 + x | g) + offset(o) + gp(x, k = 5, cov = "matern32"),
       sigma ~ s(x, k = 5) + mo(ord) + (1 | g))
  ),
  mv = fit_cached(
    "mv",
    bf(y1 ~ gp(x, k = 5) + mo(ord) + offset(o), sigma ~ (1 | g)) +
      bf(y2 ~ s(x, k = 5) + (1 + x | g)) + set_rescor(FALSE)
  ),
  nl = fit_cached(
    "nl",
    bf(y3 ~ a * exp(b * x), a ~ 1 + s(z, k = 5) + (1 | g), b ~ 1,
       nl = TRUE)
  ),
  hurdle = fit_cached(
    "hurdle", bf(cnt ~ x + (1 | g), hu ~ z + (1 | g)), hurdle_poisson()
  ),
  smooths = fit_cached(
    "smooths",
    bf(ys ~ s(x, k = 5) + s(z, by = gs, k = 5) + t2(x, z, k = c(4, 4)),
       sigma ~ s(z, k = 5))
  ),
  trend = fit_cached(
    "trend", bf(y ~ s(x, k = 5)),
    trend_formula = ~ s(z, k = 5) + x + (1 | g) + AR()
  ),
  trend_intercept = fit_cached(
    "trend_intercept", bf(y ~ x), trend_formula = ~ 1 + AR()
  )
)

# One side's brms model, given the fit's draws. The combined program
# holds both sides' parameters, and brms reads some of them by pattern:
# its reader of the smooth `s_1_1` also takes the trend's
# `s_1_1_trend`. The side's own names are kept, the trend's stripped of
# the `_trend` its own model does not write, and every other name is
# moved aside, which leaves brms reading that side alone.
as_brms <- function(fit, side = "obs") {
  b <- side_model(fit, side)
  sf <- fit$fit
  names_now <- sf@sim$fnames_oi
  own <- if (side == "trend") strip_trend_infix(names_now) else names_now
  renamed <- ifelse(names_now %in% side_parameters(fit, side), own,
                    paste0("zzother", seq_along(names_now)))
  sf@sim$fnames_oi <- renamed
  for (i in seq_along(sf@sim$samples)) {
    names(sf@sim$samples[[i]]) <- renamed
  }
  b$fit <- sf
  brms:::rename_pars(b)
}

ids <- 1:50

# Every predictor of one fit, against brms, under one `re_formula`.
predictor_gap <- function(fit, newdata, re_formula) {
  b <- as_brms(fit)
  keys <- names(response_columns(fit$obs_model$formula))
  several <- length(keys) > 1L
  gaps <- c()
  for (key in keys) {
    resp <- if (several) key
    family <- model_families(fit, resp)
    for (component in c("mu", predicted_dpar_names(
      fit, setdiff(family$dpars, "mu"), resp = resp
    ))) {
      mine <- extract_component_linpred(
        fit, newdata = newdata, draw_ids = ids, resp = resp,
        component = if (component == "mu") "obs" else component,
        re_formula = re_formula
      )
      theirs <- brms::posterior_linpred(
        b, newdata = newdata, draw_ids = ids, resp = resp,
        dpar = if (component != "mu") component, re_formula = re_formula
      )
      gaps[paste(key, component)] <- max(abs(unname(mine) -
                                               unname(theirs)))
    }
  }
  gaps
}


for (nm in names(fits)) {
  fit <- fits[[nm]]

  test_that(paste0(nm, ": every predictor is brms's own"), {
    for (re_formula in list(NULL, NA)) {
      gaps <- predictor_gap(fit, fit$data, re_formula)
      expect_true(length(gaps) > 0L)
      expect_lt(max(gaps), 1e-8)
    }
  })
}


test_that("a formula choosing some group-level terms is refused", {
  # brms numbers the terms such a formula keeps afresh, and the fitted
  # draws keep the whole model's numbers. A formula is refused on every
  # fit, including one where the two numberings happen to agree.
  expect_error(
    posterior_epred(fits$uni, draw_ids = ids, re_formula = ~ (1 | g)),
    "takes NULL or NA"
  )
  expect_error(
    posterior_linpred(fits$mv, draw_ids = ids, resp = "y2",
                      re_formula = ~ (1 | g)),
    "takes NULL or NA"
  )
})


test_that("a frame lacking the lowest ordered level reads its own", {
  # brms codes a monotonic variable 0..D in every frame. A frame
  # holding only the top level is coded 2 throughout, and a reader
  # taking its lowest value as level 0 would move every row down.
  for (nm in c("uni", "mv")) {
    fit <- fits[[nm]]
    top <- fit$data[fit$data$ord == "hi", , drop = FALSE]
    expect_lt(max(predictor_gap(fit, top, NULL)), 1e-8)
  }
})


test_that("each smooth is brms's own, less the offset brms adds", {
  # `posterior_smooths()` reads a smooth with the composer's own
  # reader, picking the term's own objects out of its predictor's:
  # sigma's in `uni`, the second response's in `mv`, the non-linear
  # parameter's in `nl`, and four in `smooths`, where a `by` factor's
  # three objects sit between two other terms. brms's
  # `posterior_smooths()` adds the predictor's offsets to each smooth,
  # and mvgam returns the smooth alone.
  n_smooths <- c(uni = 1L, mv = 1L, nl = 1L, smooths = 4L, trend = 1L)
  for (nm in names(n_smooths)) {
    fit <- fits[[nm]]
    b <- as_brms(fit)
    hits <- Filter(function(hit) hit$side == "obs", mvgam_smooth_terms(fit))
    expect_length(hits, n_smooths[[nm]])
    for (hit in hits) {
      mine <- posterior_smooths(fit, hit$term, resp = hit$resp,
                                dpar = hit$dpar, nlpar = hit$nlpar,
                                newdata = fit$data, draw_ids = ids)
      theirs <- brms::posterior_smooths(b, hit$term, resp = hit$resp,
                                        dpar = hit$dpar, nlpar = hit$nlpar,
                                        newdata = fit$data, draw_ids = ids)
      offsets <- brms::standata(
        fit$obs_model, newdata = fit$data, internal = TRUE
      )[[paste0("offsets", do.call(predictor_suffix, hit[
        c("resp", "dpar", "nlpar")
      ]))]] %||% numeric(ncol(theirs))
      gap <- unname(mine) - unname(theirs) +
        matrix(offsets, nrow(theirs), ncol(theirs), byrow = TRUE)
      expect_lt(max(abs(gap)), 1e-8)
    }
  }
})


test_that("the trend's predictor and its smooth are brms's own", {
  # The trend is a brms model of its own, whose names the combined
  # program marks with `_trend`. One trend's predictor carries a
  # smooth, a slope and a random effect; the other's is an intercept
  # alone, read as `b_Intercept_trend`.
  for (nm in c("trend", "trend_intercept")) {
    fit <- fits[[nm]]
    b <- as_brms(fit, "trend")
    frame <- fit$trend_model$data
    full <- subset_draws_rows(posterior::as_draws_matrix(fit$fit),
                              draw_ids = ids)
    mine <- extract_linpred_from_prep(prepare_linpred_data(
      side_draws(fit, full, "trend"), fit$trend_model, newdata = frame
    ))
    theirs <- brms::posterior_linpred(b, newdata = frame, draw_ids = ids,
                                      incl_autocor = FALSE)
    expect_lt(max(abs(unname(mine) - unname(theirs))), 1e-8)
    expect_identical(dim(posterior_epred(fit, draw_ids = ids)),
                     c(length(ids), nrow(fit$data)))
  }

  fit <- fits$trend
  b <- as_brms(fit, "trend")
  frame <- fit$trend_model$data
  hits <- Filter(function(hit) hit$side == "trend", mvgam_smooth_terms(fit))
  expect_length(hits, 1L)
  expect_lt(max(abs(
    unname(posterior_smooths(fit, hits[[1L]]$term, newdata = frame,
                             draw_ids = ids)) -
      unname(brms::posterior_smooths(b, hits[[1L]]$term, newdata = frame,
                                     draw_ids = ids))
  )), 1e-8)
})


test_that("the mean and the likelihood follow the predictors", {
  # A predictor that is right can still be combined wrongly. The two
  # quantities a user reads are compared as well: the mean, through the
  # family's own kernel, and the density `loo()` is built from.
  for (nm in c("uni", "hurdle")) {
    fit <- fits[[nm]]
    b <- as_brms(fit)
    expect_equal(
      unname(posterior_epred(fit, draw_ids = ids)),
      unname(brms::posterior_epred(b, draw_ids = ids)),
      tolerance = 1e-8
    )
    expect_equal(
      unname(log_lik(fit, draw_ids = ids)),
      unname(brms::log_lik(b, draw_ids = ids)),
      tolerance = 1e-8
    )
  }
})

cat("\nDone.\n")
