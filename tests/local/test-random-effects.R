# Observation-side random effects, and the accessors that read them.
#
# Three shapes over one frame, so what differs between them is the
# random-effect term and nothing else:
#
#   y ~ 1 + x + (1 | grp)              one varying intercept
#   y ~ 1 + x + (1 | grp) + s(z)       the same, beside a smooth
#   y ~ 1 + x + (x | grp)              a correlated varying slope
#
# `ranef()`, `VarCorr()`, `ngrps()` and `coef()` are what a user
# reads a group-level model back through, and none of them had
# executable coverage until these blocks. Each is held to the frame
# that built the model: the grouping factor has six levels named
# a to f, so a table keyed by anything else is wrong however well
# formed it looks.
#
# The fits are built and cached here rather than read from a fixture
# another script wrote, so the assertions and the model they describe
# stay in one file.

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}


re_data <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(42L)
    n_time <- 30L
    ar_coef <- 0.7
    sigma <- 0.5
    latent <- numeric(n_time)
    latent[1L] <- rnorm(1L, 0, sigma / sqrt(1 - ar_coef^2))
    for (t in 2L:n_time) {
      latent[t] <- ar_coef * latent[t - 1L] + rnorm(1L, 0, sigma)
    }
    z <- seq(-2, 2, length.out = n_time)
    cached <<- data.frame(
      y = rpois(n_time, exp(2 + latent + 0.5 * sin(z * pi))),
      x = rnorm(n_time),
      z = z,
      time = seq_len(n_time),
      series = factor("s1"),
      grp = factor(rep(letters[1:6], each = 5L))
    )
    cached
  }
})


fit_re <- local({
  cached <- list()
  function(nm) {
    if (!is.null(cached[[nm]])) return(cached[[nm]])
    path <- cache_path(paste0("val_mvgam_", nm, ".rds"))
    if (file.exists(path)) {
      cached[[nm]] <<- readRDS(path)
      return(cached[[nm]])
    }
    form <- switch(
      nm,
      ar1_re = y ~ 1 + x + (1 | grp),
      ar1_re_smooth = y ~ 1 + x + (1 | grp) + s(z),
      ar1_cor_re = y ~ 1 + x + (x | grp)
    )
    fit <- mvgam(
      formula = form, trend_formula = ~ AR(p = 1),
      data = re_data(), family = poisson(),
      chains = 2L, iter = 1000L, warmup = 500L,
      silent = 2, backend = "cmdstanr"
    )
    saveRDS(fit, path)
    cached[[nm]] <<- fit
    fit
  }
})


# A group-level table is keyed by the levels of the grouping factor
# in the frame's own order. Checking the dimensions alone cannot
# separate a table keyed correctly from one keyed by another column
# that happens to have six levels too.
expect_ranef_keyed <- function(mv, coefs) {
  re <- ranef(mv)
  lev <- levels(mv$data$grp)
  expect_named(re, "grp")
  expect_identical(dim(re$grp), c(length(lev), 4L, length(coefs)))
  expect_identical(dimnames(re$grp)[[1L]], lev)
  expect_identical(dimnames(re$grp)[[2L]],
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  expect_identical(dimnames(re$grp)[[3L]], coefs)
  for (cf in coefs) {
    est <- re$grp[, "Estimate", cf]
    expect_true(all(is.finite(est)))
    # Group-level effects are deviations from the population term,
    # so they are centred and they vary. A block of zeros, or one
    # constant repeated down the levels, satisfies every claim above
    # and would mean no level was distinguished.
    expect_lt(abs(mean(est)), 0.5)
    expect_gt(stats::sd(est), 1e-6)
    expect_true(all(re$grp[, "Est.Error", cf] > 0))
    expect_true(all(re$grp[, "Q2.5", cf] <= est))
    expect_true(all(est <= re$grp[, "Q97.5", cf]))
  }
  invisible(re)
}


test_that("the frame gives the grouping factor six named levels", {
  d <- re_data()
  # The premise every claim below rests on. Six levels over 30 rows
  # means five rows each, so a table with one row per observation
  # cannot be mistaken for one row per level.
  expect_identical(nlevels(d$grp), 6L)
  expect_identical(levels(d$grp), letters[1:6])
  expect_identical(nrow(d), 30L)
  expect_false(nlevels(d$grp) == nrow(d))
})


test_that("ranef is keyed by the grouping factor's own levels", {
  expect_ranef_keyed(fit_re("ar1_re"), "Intercept")
})


test_that("ranef keeps that keying beside a smooth", {
  expect_ranef_keyed(fit_re("ar1_re_smooth"), "Intercept")
})


test_that("ranef carries both coefficients when the slope varies", {
  mv <- fit_re("ar1_cor_re")
  re <- expect_ranef_keyed(mv, c("Intercept", "x"))
  # The two coefficients have to be told apart. A correlated-slope
  # model that returned the intercept deviations twice would pass
  # every claim in the helper.
  expect_false(isTRUE(all.equal(re$grp[, "Estimate", "Intercept"],
                                re$grp[, "Estimate", "x"],
                                check.attributes = FALSE)))
})


test_that("ranef(summary = FALSE) returns the draws behind that table", {
  mv <- fit_re("ar1_cor_re")
  raw <- ranef(mv, summary = FALSE)
  expect_identical(dim(raw$grp),
                   c(as.integer(ndraws(mv)),
                     nlevels(mv$data$grp), 2L))
  expect_false(is.null(attr(raw$grp, "nchains")))

  # The summary is the summary of these draws, which is what ties
  # the two calls to one another rather than to two computations
  # that happen to agree in shape.
  summ <- ranef(mv)$grp
  expect_equal(summ[, "Estimate", "Intercept"],
               apply(raw$grp[, , 1L], 2L, mean),
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(summ[, "Est.Error", "x"],
               apply(raw$grp[, , 2L], 2L, stats::sd),
               tolerance = 1e-8, ignore_attr = TRUE)
})


test_that("VarCorr reports scale alone when one term varies", {
  mv <- fit_re("ar1_re")
  vc <- VarCorr(mv)
  expect_named(vc, "grp")
  expect_named(vc$grp, "sd")
  expect_identical(dim(vc$grp$sd), c(1L, 4L))
  expect_identical(dimnames(vc$grp$sd)[[1L]], "Intercept")
  expect_identical(dimnames(vc$grp$sd)[[2L]],
                   c("Estimate", "Est.Error", "Q2.5", "Q97.5"))
  # A single varying term has no correlation to report, so offering
  # one would be offering a number with no content.
  expect_false("cor" %in% names(vc$grp))
  expect_true(vc$grp$sd[1L, "Estimate"] > 0)
})


test_that("VarCorr's three blocks describe one covariance", {
  mv <- fit_re("ar1_cor_re")
  vc <- VarCorr(mv)
  coefs <- c("Intercept", "x")
  expect_true(all(c("sd", "cor", "cov") %in% names(vc$grp)))
  expect_identical(dimnames(vc$grp$sd)[[1L]], coefs)

  # Within a draw the covariance diagonal is the squared scale and
  # the correlation is the covariance rescaled by it. The identity
  # holds per draw and not on the summaries, because a posterior
  # mean of a square is not the square of a posterior mean:
  # measured, E[cov_11] is 0.774 against 0.525 for (E[sd_1])^2, and
  # the gap is Var(sd_1) to three decimals. So the claim is made
  # where it is true, and exactly rather than under a tolerance.
  raw <- VarCorr(mv, summary = FALSE)
  sd_d <- raw$grp$sd
  cov_d <- raw$grp$cov
  cor_d <- raw$grp$cor
  expect_identical(dim(sd_d), c(as.integer(ndraws(mv)), 2L))
  expect_identical(dim(cov_d), c(as.integer(ndraws(mv)), 2L, 2L))
  expect_equal(as.numeric(cbind(cov_d[, 1L, 1L], cov_d[, 2L, 2L])),
               as.numeric(sd_d^2))
  expect_equal(as.numeric(cor_d[, 1L, 2L]),
               as.numeric(cov_d[, 1L, 2L] /
                            (sd_d[, 1L] * sd_d[, 2L])))
  expect_equal(as.numeric(cor_d[, 1L, 1L]), rep(1, nrow(cor_d)))
  expect_equal(as.numeric(cor_d[, 2L, 2L]), rep(1, nrow(cor_d)))
  expect_true(all(sd_d > 0))
  expect_true(all(abs(cor_d[, 1L, 2L]) <= 1))

  # And the gap between the two levels is the variance the draws
  # carry, which is what a summary taken over the wrong margin
  # would not reproduce.
  expect_equal(
    unname(vc$grp$cov[1L, "Estimate", 1L] -
             vc$grp$sd[1L, "Estimate"]^2),
    stats::var(sd_d[, 1L]) * (nrow(sd_d) - 1L) / nrow(sd_d),
    tolerance = 1e-6
  )
})


test_that("ngrps counts the levels the frame actually holds", {
  for (nm in c("ar1_re", "ar1_re_smooth", "ar1_cor_re")) {
    mv <- fit_re(nm)
    ng <- ngrps(mv)
    expect_named(ng, "grp")
    expect_identical(as.integer(ng$grp), nlevels(mv$data$grp))
    # It agrees with the table keyed by those same levels, so the
    # count and the keying cannot drift apart.
    expect_identical(as.integer(ng$grp), dim(ranef(mv)$grp)[1L])
  }
})


test_that("the group-level draws are named and reach the diagnostics", {
  mv <- fit_re("ar1_cor_re")
  vars <- variables(mv)
  # brms spells these `r_<group>[level, coef]`, `sd_<group>__<coef>`
  # and `cor_<group>__<a>__<b>`. One deviation per level per
  # coefficient is twelve, and a model that fitted one shared
  # deviation would declare six.
  r_cols <- grep("^r_grp\\[", vars, value = TRUE)
  expect_length(r_cols, nlevels(mv$data$grp) * 2L)
  expect_length(grep("^sd_grp__", vars, value = TRUE), 2L)
  expect_length(grep("^cor_grp__", vars, value = TRUE), 1L)
  expect_true(all(is.finite(rhat(mv)[r_cols])))
  dm <- as_draws_matrix(mv)
  expect_true(all(as.numeric(dm[, grep("^sd_grp__", vars,
                                       value = TRUE)]) > 0))
})


test_that("update refits without losing the group-level structure", {
  mv <- fit_re("ar1_cor_re")
  # A refit rebuilds the object, so it is where a structure can be
  # resolved a second time and disagree with the first.
  refit <- suppressWarnings(suppressMessages(
    update(mv, iter = 200L, warmup = 100L, chains = 1L,
           silent = 2, refresh = 0)
  ))
  expect_s3_class(refit, "mvgam")
  vars <- variables(refit)
  expect_true(any(grepl("^r_grp\\[", vars)))
  expect_true(any(grepl("^sd_grp__", vars)))
  expect_true(any(grepl("^cor_grp__", vars)))
  # The grouping survives as the same factor, with the same levels
  # in the same order, and the accessors still key by it.
  expect_identical(levels(refit$data$grp), levels(mv$data$grp))
  expect_identical(dimnames(ranef(refit)$grp)[[1L]],
                   dimnames(ranef(mv)$grp)[[1L]])
  expect_identical(ngrps(refit)$grp, ngrps(mv)$grp)
})


cat("\nDone.\n")


test_that("re_formula decides whether the group effect is carried", {
  mv <- fit_re("ar1_cor_re")
  d <- mv$data
  # `?posterior_epred.mvgam` documents NULL as including all random
  # effects and NA as excluding them. `~0` is the other spelling of
  # none, and the two have to agree exactly or a user gets a third
  # answer from a synonym.
  with_re <- posterior_epred(mv, re_formula = NULL)
  without <- posterior_epred(mv, re_formula = NA)
  zero_re <- posterior_epred(mv, re_formula = ~0)
  expect_identical(dim(with_re), dim(without))
  expect_equal(without, zero_re)

  # Dropping the group effect has to change the answer, and change
  # it in the direction that says a group effect was there: the
  # spread between group means collapses. Measured, 3.82 falls to
  # 0.41. An `re_formula` read and discarded returns the first
  # matrix three times and passes every dimension check.
  expect_gt(max(abs(with_re - without)), 1)
  grp_spread <- function(e) stats::sd(tapply(colMeans(e), d$grp, mean))
  expect_gt(grp_spread(with_re), 2)
  expect_lt(grp_spread(without), grp_spread(with_re) / 2)
})


test_that("coef reports the fixed effects as posterior means", {
  mv <- fit_re("ar1_cor_re")
  # `?mvgam_diagnostics` documents `coef()` as summarising the
  # fixed-effect block to posterior means, which is a deliberate
  # divergence from brms, where `coef()` adds the group-level
  # deviations to the population terms. So the claim is mvgam's own
  # contract, checked against the draws it summarises.
  dm <- as_draws_matrix(mv, variable = "betas")
  expect_setequal(colnames(dm), c("b_x", "b_Intercept"))
  expect_equal(coef(mv), colMeans(dm))
  expect_identical(dim(coef(mv, summary = FALSE)),
                   c(as.integer(ndraws(mv)), 2L))
  # It is the population block and not the group one, so it has two
  # entries where `ranef()` has one per level.
  expect_length(coef(mv), 2L)
  expect_false(length(coef(mv)) == nlevels(mv$data$grp))
})


test_that("a grouping level the fit never saw is refused either way", {
  mv <- fit_re("ar1_re")
  nd <- mv$data
  nd$grp <- factor("zzz", levels = c(levels(mv$data$grp), "zzz"))
  # `?posterior_linpred.mvgam` records that `allow_new_levels` is
  # accepted for brms compatibility and that a new level is refused
  # whatever it is set to, because there is no fitted effect to
  # predict from. Both routes have to refuse, and name the level.
  err <- expect_error(posterior_epred(mv, newdata = nd))
  expect_match(conditionMessage(err), "zzz")
  err2 <- expect_error(
    posterior_epred(mv, newdata = nd, allow_new_levels = TRUE)
  )
  expect_match(conditionMessage(err2), "grp")
  # A frame holding only levels the fit knows still answers, so the
  # refusal is about the new level and not about the frame.
  expect_identical(ncol(posterior_epred(mv, newdata = mv$data)),
                   nrow(mv$data))
})


# -- The brms interval and error accessors ----------------------------
#
# These read the posterior of a fit carrying a random effect and a
# smooth, so a summary taken over the wrong margin has several
# parameter blocks to land in.

test_that("posterior_interval quantiles the draws it names", {
  mv <- fit_re("ar1_re_smooth")
  pi <- posterior_interval(mv)
  expect_identical(ncol(pi), 2L)
  expect_identical(colnames(pi), c("2.5%", "97.5%"))
  expect_true(all(pi[, 1L] <= pi[, 2L]))

  # The interval has to be the interval of this fit's own draws.
  # Matching another package's column names says nothing about which
  # parameter each row describes.
  dm <- posterior::as_draws_matrix(mv)
  shared <- intersect(rownames(pi), colnames(dm))
  # The random effect and the smooth both have to be reachable here,
  # so a table covering the population block alone fails.
  expect_true(any(grepl("^sd_", shared)))
  expect_true(any(grepl("^sds_", shared)))
  for (v in shared) {
    expect_equal(
      unname(pi[v, ]),
      unname(stats::quantile(as.numeric(dm[, v]), c(0.025, 0.975))),
      tolerance = 1e-8
    )
  }
  # A narrower request nests inside the wider one, strictly for a
  # parameter whose posterior is not a point mass.
  narrow <- posterior_interval(mv, prob = 0.5)
  expect_true(all(narrow[shared, 1L] >= pi[shared, 1L]))
  expect_true(all(narrow[shared, 2L] <= pi[shared, 2L]))
})


test_that("predictive_interval brackets the response it predicts", {
  mv <- fit_re("ar1_re_smooth")
  pi <- predictive_interval(mv)
  expect_identical(dim(pi), c(nrow(mv$data), 2L))
  expect_identical(colnames(pi), c("5%", "95%"))
  expect_true(all(pi[, 2L] >= pi[, 1L]))
  # A 90 per cent predictive band read against the data it was fitted
  # to. A band computed on the wrong scale, or summarised over draws
  # of one observation for all of them, misses most of the response.
  covered <- mean(mv$data$y >= pi[, 1L] & mv$data$y <= pi[, 2L])
  expect_gt(covered, 0.75)
  # Coverage alone is satisfied by a band wide enough to hold
  # anything, so the width is bounded too. The bound is not a
  # calibration claim and is deliberately loose: it exists to rule
  # out the degenerate band, and a tighter number here would be read
  # off this fit rather than derived from it.
  expect_lt(mean(pi[, 2L] - pi[, 1L]), 6 * stats::sd(mv$data$y))
  # The band also has to move with the data rather than sitting in
  # one place: a single interval repeated for every row would satisfy
  # both claims above.
  expect_gt(stats::sd(pi[, 1L]), 0)
  expect_gt(stats::cor(rowMeans(pi), mv$data$y), 0.5)
  tight <- predictive_interval(mv, prob = 0.5)
  expect_true(all(tight[, 1L] >= pi[, 1L]))
  expect_true(all(tight[, 2L] <= pi[, 2L]))
})


test_that("predictive_error is the response less a draw of it", {
  mv <- fit_re("ar1_re_smooth")
  ids <- 1:50
  err <- predictive_error(mv, draw_ids = ids)
  expect_identical(dim(err), c(length(ids), nrow(mv$data)))

  # Adding the error back to the response has to return a draw the
  # family could have produced: a non-negative whole number for a
  # Poisson. An error taken against the expectation, or against
  # another row's response, breaks one of the two.
  y_mat <- matrix(mv$data$y, nrow = length(ids),
                  ncol = nrow(mv$data), byrow = TRUE)
  drawn <- y_mat - unname(as.matrix(err))
  expect_true(all(drawn == round(drawn)))
  expect_true(all(drawn >= 0))
  # Comparing the two matrices cell by cell is not available: the
  # predictive draw is sampled afresh on each call, so two calls
  # naming the same iterations still differ. What is repeatable is
  # where the errors sit, which follows the response row by row.
  #
  # The bar is not a number read off this fit. It is the null in
  # which the errors are attached to the wrong rows, which is the
  # mistake the claim exists to catch, so it moves with the data.
  # Beating every one of 500 shuffles is a permutation p-value below
  # 1/500, and it is the whole claim: a standardised distance on top
  # of it would be a second threshold with no separate meaning, and
  # thirty observations do not support one. Measured, the true
  # assignment sits 4.2 null standard deviations out.
  target <- mv$data$y - colMeans(posterior_epred(mv))
  observed <- colMeans(as.matrix(err))
  agree <- stats::cor(observed, target)
  set.seed(404L)
  null <- replicate(500L, {
    stats::cor(observed[sample(nrow(mv$data))], target)
  })
  expect_gt(agree, max(null))
  # The null is a null: shuffled, the rows carry no information.
  expect_lt(abs(mean(null)), 0.1)
  expect_gt(stats::sd(null), 1e-3)

  # The epred branch subtracts the expectation instead, which is
  # continuous where a predictive draw is a whole number. That `err`
  # itself is whole follows from `drawn` being whole above, so it is
  # not restated.
  err_e <- predictive_error(mv, method = "posterior_epred",
                            draw_ids = ids)
  expect_identical(dim(err_e), dim(err))
  expect_false(all(err_e == round(err_e)))
})


test_that("the deprecated brms aliases reach the current methods", {
  mv <- fit_re("ar1_re_smooth")
  # parnames -> variables, nsamples -> ndraws. Each has to answer
  # with what the current method answers rather than with a value of
  # its own.
  #
  # `parnames()` is the one brms marks deprecated, so its notice is
  # asserted first, then muted at the one call that reads its value.
  # Nothing else here is muted: a warning that arrives from any of
  # these calls is one the suite should report.
  expect_warning(parnames(mv))
  expect_identical(suppressWarnings(parnames(mv)), variables(mv))
  expect_identical(
    nsamples(mv),
    posterior::ndraws(posterior::as_draws(mv$fit))
  )
})


# -- How a grouping factor is reported to other packages --------------

# The three fits are asked together and the answers are collected
# before anything is asserted, so one defect costs one failure per
# surface rather than one per fit. `lme4::lmer` and `brms` both put a
# grouping factor under `random` and leave `conditional` to the fixed
# terms; the split is what every downstream term list is built from.

re_fits <- c("ar1_re", "ar1_re_smooth", "ar1_cor_re")
grouping_report <- lapply(re_fits, function(nm) {
  mv <- fit_re(nm)
  list(
    conditional = insight::find_predictors(mv)$conditional,
    all_random = insight::find_predictors(mv, effects = "all")$random,
    find_random = insight::find_random(mv)$random,
    variables_random = insight::find_variables(mv)$random
  )
})
names(grouping_report) <- re_fits


test_that("the fixed side of the term list is right", {
  # Stated first, so a failure below is about the split and not
  # about the terms being wrong altogether.
  for (nm in re_fits) {
    expect_true("x" %in% grouping_report[[nm]]$conditional)
  }
  expect_true("z" %in% grouping_report[["ar1_re_smooth"]]$conditional)
})


test_that("the grouping factor is kept out of the conditional terms", {
  offered <- vapply(grouping_report,
                    function(r) "grp" %in% r$conditional, logical(1L))
  expect_identical(unname(offered), rep(FALSE, length(re_fits)))
})


test_that("the grouping factor is reported as a grouping factor", {
  for (field in c("all_random", "find_random", "variables_random")) {
    got <- lapply(grouping_report, function(r) r[[field]])
    expect_identical(unname(got), rep(list("grp"), length(re_fits)))
  }
})


test_that("no marginal effect is taken over the grouping factor", {
  # What the split above is for. marginaleffects reads the
  # conditional list to decide what can be contrasted, so a grouping
  # factor offered there is contrasted: measured on `ar1_re`, the
  # default call returns five rows of `grp` contrasts, b - a through
  # f - a, reaching -7.8 on the response scale.
  #
  # Those numbers are group-level deviations shrunk toward zero by
  # the prior on `sd_grp`, reported as though they were population
  # contrasts a reader could act on. Nothing in the table says the
  # levels are exchangeable draws rather than fixed categories.
  library(marginaleffects)
  options("marginaleffects_model_classes" = "mvgam")
  terms_by_fit <- lapply(re_fits, function(nm) {
    as.character(avg_slopes(fit_re(nm))$term)
  })
  names(terms_by_fit) <- re_fits
  # The tables are not empty, so the claim below is about what is in
  # them rather than about there being nothing to read.
  expect_true(all(vapply(terms_by_fit,
                         function(t) "x" %in% t, logical(1L))))
  contrasted <- vapply(terms_by_fit,
                       function(t) "grp" %in% t, logical(1L))
  expect_identical(unname(contrasted), rep(FALSE, length(re_fits)))
})
