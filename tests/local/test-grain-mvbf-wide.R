# Structure and post-fit coverage for a wide `mvbf()` frame, fitted
# in this file.
#
# A wide frame holds one row per occasion and one column per
# response, so the series an observation sits on is a property of the
# `(row, response)` pair rather than of the row. That is the one
# shape a data-frame attribute cannot express, and the defect it
# produced was a frame cut into a row-block per response: each
# response owned a stretch of the timeline instead of a series, and
# every count, dimension and bound stayed correct.
#
# It is also the cheapest place to hold several families side by
# side. Three responses with three different likelihoods share one
# trend, so a claim about how a family reaches a surface is made
# three times by one fit.
#
#   truth: 3 responses on 60 shared occasions, one latent AR per
#          response with correlated innovations, and a disjoint set
#          of unobserved occasions per response
#   model: mvbf(count ~ x, seen ~ x, mass ~ x) with poisson,
#          bernoulli and gaussian arms, trend_formula = ~ AR(cor)
#
# Responses are named out of alphabetical order and occasions are
# numbered from 3, so neither the axis nor the time index can be
# recovered by sorting.
#
# Run with:
#   testthat::test_file("tests/local/test-grain-mvbf-wide.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

# Several blocks below state what the package does not yet do, and
# testthat stops a file after ten failures by default, which would
# leave the blocks after them unrun and looking clean. The limit is
# read when the reporter is built, before this file is sourced, so it
# has to come from the environment:
#   TESTTHAT_MAX_FAILS=1000 Rscript -e "..."

# This file fits its own models and caches them beside itself, so it
# depends on no shared fixture and no build step.
# Resolved from where this file is running rather than from what is
# already on disk. testthat sets the working directory to the test
# file's own, so asking whether `fixtures` exists picks the wrong
# branch on a clean tree and writes tests/local/tests/local/fixtures.
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

# The value of `expr` alongside every warning raised computing it.
# The frame carries unobserved occasions on purpose, so the notices
# about them are counted rather than discarded.
with_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = seen)
}

set.seed(5150L)

n_time <- 60L
time_vals <- seq_len(n_time) + 2L

# Formula order, which is the axis. Alphabetical order is mass,
# count, seen, so an axis rebuilt by sorting differs from this one.
responses <- c("count", "seen", "mass")
stopifnot(!identical(responses, sort(responses)))
n_resp <- length(responses)

ar_true <- c(0.70, 0.35, 0.55)
sigma_true <- 0.35
R_true <- matrix(c(1, 0.5, -0.3,
                   0.5, 1, 0.2,
                   -0.3, 0.2, 1), nrow = n_resp)
L_true <- chol(sigma_true^2 * R_true)

latent <- matrix(0, nrow = n_time, ncol = n_resp)
for (t in 2:n_time) {
  innov <- as.numeric(crossprod(L_true, rnorm(n_resp)))
  latent[t, ] <- ar_true * latent[t - 1L, ] + innov
}

x <- as.numeric(scale(rnorm(n_time)))

dat <- data.frame(time = time_vals, x = x)
dat$count <- rpois(n_time, exp(2.4 + 0.35 * x + latent[, 1L]))
dat$seen <- rbinom(n_time, 1L,
                   plogis(0.2 + 0.8 * x + latent[, 2L]))
dat$mass <- 1.5 + 0.6 * x + latent[, 3L] + rnorm(n_time, 0, 0.3)

# Disjoint gaps, of three different sizes, so no response's occasions
# can be recovered from another's and a swapped pair of arms shows up
# in the lengths alone. `mass` stops before the other two do, so the
# per-response last-observed times differ: a frame whose responses
# end together cannot tell a permuted record from the right one.
na_rows <- list(
  count = c(4L, 5L, 19L),
  seen = c(11L, 33L),
  mass = c(7L, 57L, 58L, 59L, 60L)
)
for (r in responses) {
  dat[[r]][na_rows[[r]]] <- NA
}
observed_rows <- lapply(na_rows, function(idx) setdiff(seq_len(n_time), idx))
stopifnot(length(unique(lengths(observed_rows))) == n_resp)

obs_formula <- bf(count ~ x, family = poisson()) +
  bf(seen ~ x, family = bernoulli()) +
  bf(mass ~ x, family = gaussian()) +
  set_rescor(FALSE)

# The support each response's draws have to land in. This is the
# claim that a shared trend has not flattened three likelihoods into
# one: a wide fit that applied the first response's family to all of
# them returns counts everywhere and passes every shape check.
support_ok <- list(
  count = function(v) all(v >= 0) && all(v == floor(v)),
  seen = function(v) all(v %in% c(0, 1)),
  mass = function(v) any(abs(v - round(v)) > 1e-8)
)

sim_truth <- list(
  n_time = n_time, time_vals = time_vals, responses = responses,
  ar_true = ar_true, latent = latent, x = x, na_rows = na_rows
)

# A future frame in the wide shape: one row per occasion, every
# response left missing, and every covariate the model reads.
make_future <- function(h, x_future = 0) {
  data.frame(
    time = max(time_vals) + seq_len(h),
    x = rep(x_future, length.out = h),
    count = NA_integer_,
    seen = NA_integer_,
    mass = NA_real_
  )
}


# -- Prefit: the axis a wide frame has no column for ------------------

built <- with_warnings(mvgam(
  formula = obs_formula, trend_formula = ~ AR(p = 1, cor = TRUE),
  data = dat, run_model = FALSE, silent = 2
))
prefit <- built$value


test_that("the dropped occasions are reported once per response", {
  # brms drops a row whose response is missing and says so once for
  # the frame, whichever arm the gap was in. The count is the claim:
  # assembling a model runs the code generator more than once over
  # the same frame, so a notice raised per internal pass is a defect
  # this package has had before, and a notice that stops arriving
  # means rows leave the likelihood in silence.
  #
  # One notice for three arms with disjoint gaps cannot say which
  # response lost what, which is recorded rather than asserted: the
  # count below is the contract as it stands.
  na_notice <- grep("Rows containing NAs", built$warnings, value = TRUE)
  expect_length(na_notice, 1L)
  expect_identical(setdiff(built$warnings, na_notice), character(0))
})


test_that("the series axis is the responses, in formula order", {
  ax <- mvgam:::mvgam_axes(prefit)
  expect_identical(as.character(ax$series$levels), responses)
  expect_identical(as.integer(ax$series$n), n_resp)
  # How the axis was arrived at, not merely what it holds. A wide
  # frame that decided its axis was explicit would have read a column
  # that is not there.
  expect_identical(ax$series$source, "multivariate")
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(as.integer(ax$time$n), n_time)
})


test_that("a wide frame has no per-row series to answer with", {
  # `axis_row_series()` asks which series a row belongs to. On a wide
  # frame that question has no answer: a row carries every response,
  # so it sits on every series at once. Returning `NULL` is the
  # contract, and this is the only shape that exercises the branch.
  # A function that answered with the first series instead would give
  # each row a plausible label and place every observation on one
  # column.
  expect_null(mvgam:::axis_row_series(prefit, dat))
})


test_that("each response owns a column, and they cover the axis", {
  sd <- prefit$standata
  expect_identical(as.integer(sd$N_series_trend), n_resp)
  expect_identical(as.integer(sd$N_time_trend), n_time)

  columns <- vapply(responses, function(r) {
    idx <- unique(as.integer(sd[[paste0("obs_trend_series_", r)]]))
    if (length(idx) == 1L) idx else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
  # A response spread over two columns is the row-block defect: it
  # owned a stretch of the timeline rather than a series.
  expect_false(anyNA(columns))
  expect_identical(columns, seq_len(n_resp))
})


test_that("each arm is as long as its own response, not the frame", {
  # The row-block split gave every response the same number of rows,
  # so equal arm lengths is exactly what it produced. The gaps here
  # are three different sizes for that reason.
  sd <- prefit$standata
  for (i in seq_along(responses)) {
    r <- responses[i]
    expect_identical(as.integer(sd[[paste0("N_", r)]]),
                     length(observed_rows[[r]]))
    expect_length(as.integer(sd[[paste0("obs_trend_series_", r)]]),
                  length(observed_rows[[r]]))
    # And the occasions it reads are the ones it was measured on,
    # ranked against the shared grid. A block of the timeline handed
    # to one response has the right length and the wrong entries.
    expect_identical(
      as.integer(sd[[paste0("obs_trend_time_", r)]]),
      as.integer(observed_rows[[r]])
    )
  }
})


test_that("the program reads the trend on both axes, per response", {
  sc <- paste(as.character(stancode(prefit)), collapse = "\n")
  for (r in responses) {
    expect_true(grepl(
      paste0("trend[obs_trend_time_", r, "[n], obs_trend_series_",
             r, "[n]]"),
      sc, fixed = TRUE
    ))
  }
  # One latent dimension per response, and the innovations are drawn
  # jointly, which is what `cor = TRUE` buys.
  expect_true(grepl("cov_matrix[N_lv_trend] Sigma_trend", sc,
                    fixed = TRUE))
})


test_that("only the closure-unit families are classified as such", {
  # A question about the family table rather than about this fit.
  # `occ()` and `nmix()` model a detection process over repeat visits
  # to a closed unit; every other family mvgam offers does not. The
  # misclassification this catches sent `mvn()` fits into a
  # `pp_check` type their own family refuses and made `augment()`
  # demand a `cap` column that has no meaning for them.
  closure <- c("occ", "nmix")
  others <- c("tweedie", "mvn", "mvt", "diri", "beta_nb",
              "com_binomial")
  for (nm in closure) {
    fam <- do.call(nm, list())
    expect_true(mvgam:::is_closure_unit_family(fam),
                label = paste(nm, "is a closure-unit family"))
  }
  for (nm in others) {
    fam <- do.call(nm, list())
    expect_false(mvgam:::is_closure_unit_family(fam),
                 label = paste(nm, "is not a closure-unit family"))
  }
  for (fam in list(gaussian(), poisson(), bernoulli(), Gamma(),
                   brms::negbinomial(), brms::Beta())) {
    expect_false(mvgam:::is_closure_unit_family(fam))
  }
})


# -- Fit --------------------------------------------------------------

cache <- cache_path("val_mvgam_mvbf_wide.rds")
if (file.exists(cache)) {
  cat("[cache] Loading wide mvbf fit.\n")
  fit <- readRDS(cache)
} else {
  cat("[fit ] mvgam(mvbf(count, seen, mass), AR(cor = TRUE))\n")
  # Captured, not asserted: this runs only on a cache miss, so a
  # count here would be a claim the file makes on some runs and not
  # others. The same claim is made unconditionally on the prefit.
  fit <- with_warnings(mvgam(
    formula = obs_formula, trend_formula = ~ AR(p = 1, cor = TRUE),
    data = dat, chains = 2L, iter = 1000L, warmup = 500L,
    silent = 2, backend = "cmdstanr"
  ))$value
}
if (!identical(attr(fit, "sim_truth"), sim_truth)) {
  attr(fit, "sim_truth") <- sim_truth
  saveRDS(fit, cache)
}


test_that("the fitted object keeps the response axis", {
  expect_identical(as.character(fit$response_names), responses)
  expect_identical(
    as.character(mvgam:::mvgam_axes(fit)$series$levels), responses
  )
  expect_identical(as.integer(fit$standata$N_series_trend), n_resp)
})


test_that("each response holds a latent state of its own", {
  # Two responses sharing one column is the shape the defect took,
  # and it survives every check that counts rather than compares.
  dm <- posterior::as_draws_matrix(fit$fit)
  state_of <- function(k) {
    vapply(seq_len(n_time), function(t) {
      mean(dm[, paste0("trend[", t, ",", k, "]")])
    }, numeric(1))
  }
  states <- lapply(seq_len(n_resp), state_of)
  for (i in seq_len(n_resp)) {
    for (j in seq_len(n_resp)) {
      if (j <= i) next
      expect_false(isTRUE(all.equal(states[[i]], states[[j]])))
    }
  }
})


test_that("posterior_predict respects each response's own family", {
  # The claim that says three likelihoods survived being given one
  # trend. A fit that applied the first arm's family throughout
  # returns counts for all three and passes every dimension check.
  for (r in responses) {
    pp <- posterior_predict(fit, resp = r, ndraws = 50L)
    expect_identical(nrow(pp), 50L)
    v <- as.numeric(pp)
    v <- v[!is.na(v)]
    expect_true(length(v) > 0L)
    expect_true(support_ok[[r]](v),
                label = paste(r, "draws land in its family's support"))
  }
})


test_that("every prediction surface answers per response", {
  for (r in responses) {
    ep <- posterior_epred(fit, resp = r, ndraws = 20L)
    lp <- posterior_linpred(fit, resp = r, ndraws = 20L)
    expect_identical(nrow(ep), 20L)
    expect_identical(dim(ep), dim(lp))
    expect_true(all(is.finite(ep)))
    # The expectation is on the response scale and the predictor is
    # not, so on the two non-identity arms they have to differ.
    if (r != "mass") {
      expect_false(isTRUE(all.equal(unname(ep), unname(lp))))
    }
    expect_identical(nrow(fitted(fit, resp = r, ndraws = 20L)),
                     ncol(ep))
    expect_identical(nrow(residuals(fit, resp = r, ndraws = 20L)),
                     ncol(ep))
  }
})


test_that("log_lik is per response, and the joint is their sum", {
  # brms's own contract, and the one place a wide fit's arms are
  # combined into a single number. A joint density that dropped an
  # arm is finite, correctly shaped and wrong.
  per <- lapply(responses, function(r) {
    log_lik(fit, resp = r, draw_ids = 1:10)
  })
  names(per) <- responses
  for (r in responses) {
    expect_identical(nrow(per[[r]]), 10L)
    expect_identical(ncol(per[[r]]), n_time)
    # Finite where the response was measured, and missing exactly
    # where it was not. Asserting finiteness everywhere would demand
    # a density for an observation that does not exist; asserting it
    # nowhere would pass a column of `NA`. The gaps differ per
    # response, so this also places each arm's missingness on its own
    # occasions rather than on another arm's.
    obs <- observed_rows[[r]]
    gaps <- na_rows[[r]]
    expect_true(all(is.finite(per[[r]][, obs, drop = FALSE])))
    expect_true(all(is.na(per[[r]][, gaps, drop = FALSE])))
  }
  joint <- log_lik(fit, draw_ids = 1:10)
  expect_identical(nrow(joint), 10L)

  # An occasion is missing from the joint only where every arm is
  # missing. Here the three sets of gaps are disjoint, so no occasion
  # qualifies and the joint owes a density at all sixty.
  #
  # This fails today: the joint marks an occasion missing whenever
  # any one arm is, so the arms that were observed there are dropped
  # with it. Ten of sixty occasions lose real contributions on this
  # frame, and `loo()` is computed from exactly these numbers.
  # Recorded as finding 15.
  all_missing <- Reduce(intersect, na_rows)
  expect_length(all_missing, 0L)
  joint_missing <- which(apply(joint, 2L, function(col) all(is.na(col))))
  expect_length(joint_missing, 0L)

  expect_equal(
    as.numeric(rowSums(joint, na.rm = TRUE)),
    as.numeric(Reduce(`+`, lapply(per, rowSums, na.rm = TRUE)))
  )
})


test_that("a wide fit forecasts every response over the horizon", {
  h <- 4L
  future <- make_future(h)
  fc <- forecast(fit, newdata = future, type = "link")
  expect_identical(names(fc), responses)

  for (r in responses) {
    expect_identical(as.character(fc[[r]]$series_names), responses)
    arm <- fc[[r]]$forecasts[[r]]
    expect_false(is.null(arm))
    expect_identical(ncol(arm), h)
    expect_false(all(is.na(arm)))
    # The occasions are the ones asked for, in the user's own
    # numbering rather than 1..h.
    expect_identical(as.integer(fc[[r]]$test_times[[r]]),
                     as.integer(future$time))
  }
  # No response reads another's state.
  first <- fc[[responses[1L]]]$forecasts[[responses[1L]]]
  second <- fc[[responses[2L]]]$forecasts[[responses[2L]]]
  expect_false(isTRUE(all.equal(as.numeric(first), as.numeric(second))))
})


test_that("each forecast arm carries its own response's history", {
  # The training tail a forecast extends. Each response stops being
  # observed at a different occasion here, so an arm carrying
  # another's history is visible in the length alone.
  fc <- forecast(fit, newdata = make_future(2L), type = "link")
  for (r in responses) {
    train <- fc[[r]]$train_observations[[r]]
    expect_equal(as.numeric(train),
                 as.numeric(dat[[r]][observed_rows[[r]]]))
  }
})


test_that("a forecast responds to the covariate it is given", {
  # Past the training grid there is no state to read, so the
  # observation predictor has to be evaluated at what the caller
  # supplied. A forecast that ignored `x`, or took it from the
  # training frame, returns the same numbers whatever is asked for.
  lo <- forecast(fit, newdata = make_future(3L, x_future = -1.5),
                 ndraws = 200L, type = "expected")
  hi <- forecast(fit, newdata = make_future(3L, x_future = 1.5),
                 ndraws = 200L, type = "expected")
  for (r in responses) {
    a <- colMeans(lo[[r]]$forecasts[[r]])
    b <- colMeans(hi[[r]]$forecasts[[r]])
    expect_gt(max(abs(a - b)), 1e-3)
  }
})


test_that("forecast and hindcast agree about what can be drawn", {
  # One quantity reached over the training grid and over its
  # extension. If one of them refuses a family the other draws from,
  # exactly one of the two is wrong.
  hc <- hindcast(fit, type = "response", ndraws = 20L)
  expect_identical(names(hc), responses)
  fc <- forecast(fit, newdata = make_future(2L), type = "response",
                 ndraws = 20L)
  expect_identical(names(fc), responses)
  for (r in responses) {
    expect_false(is.null(hc[[r]]$hindcasts[[r]]))
    expect_false(is.null(fc[[r]]$forecasts[[r]]))
    # And a response-scale draw is in that response's support on
    # both sides of the training boundary.
    for (m in list(hc[[r]]$hindcasts[[r]], fc[[r]]$forecasts[[r]])) {
      v <- as.numeric(m)
      v <- v[!is.na(v)]
      expect_true(support_ok[[r]](v))
    }
  }
})


test_that("a horizon the trend cannot step to is refused", {
  # A discrete-time trend steps one occasion at a time along a grid
  # every response shares, so a frame that skips occasions is turned
  # away rather than forecast as though the gap were not there.
  future <- make_future(1L)
  future$time <- max(time_vals) + 10L
  expect_error(
    forecast(fit, newdata = future, type = "link"),
    "continue the training series"
  )
})


test_that("each prediction type answers with the quantity it names", {
  # mvgam separates the two things brms and marginaleffects both
  # spell `"response"`. Here `"expected"` is the family's mean and
  # `"response"` samples from the observation family, summarised by
  # its median, so on a count or a binary arm it lands on a whole
  # number by contract rather than by accident.
  #
  # Pinning all three together is what makes this bite: a type that
  # quietly answered with another's quantity satisfies any check made
  # on one type alone, and the arms here differ enough that no two
  # types can be confused for one another.
  withr::local_options(marginaleffects_model_classes = "mvgam")
  grid <- dat[seq(1L, n_time, length.out = 6L), , drop = FALSE]

  for (r in responses) {
    # marginaleffects keeps a whitelist of arguments it knows a model
    # class accepts, and mvgam has not registered `resp` on it, so
    # every multivariate user meets this notice. Captured and named
    # so it neither leaks nor hides an unrelated one.
    ask <- function(ty) {
      got <- with_warnings(marginaleffects::predictions(
        fit, newdata = grid, type = ty, resp = r
      ))
      expect_true(all(grepl("not known to be supported", got$warnings)))
      as.numeric(got$value$estimate)
    }

    # marginaleffects summarises a Bayesian posterior by its median,
    # so the comparison is made against the median of the same draws
    # rather than their mean. Against the mean the two agree only to
    # about 0.04 on the bernoulli arm, whose logit-scale posterior is
    # the most skewed of the three, and a tolerance wide enough to
    # absorb that is wide enough to hide a wrong arm.
    ep <- apply(posterior_epred(fit, newdata = grid, resp = r),
                2L, stats::median)
    lp <- apply(posterior_linpred(fit, newdata = grid, resp = r),
                2L, stats::median)
    pp <- posterior_predict(fit, newdata = grid, resp = r,
                            ndraws = 400L)
    med <- apply(pp, 2L, stats::median)

    # Every draw is shared, so these are the same number twice.
    expect_equal(ask("expected"), as.numeric(ep), tolerance = 1e-8)
    expect_equal(ask("link"), as.numeric(lp), tolerance = 1e-8)
    # A median of draws from a discrete family moves in whole units,
    # so two estimates of it from different draws differ by one at
    # the granularity of the family rather than by a small fraction.
    # The continuous arm has no such floor and is held tightly.
    tol_response <- if (r == "mass") 0.1 else 1.0
    expect_lte(max(abs(ask("response") - as.numeric(med))),
               tol_response)

    # And the outcome-scale answer sits in the family's support,
    # which is what says the type reached this arm's likelihood
    # rather than the first one's.
    expect_true(support_ok[[r]](ask("response")))

    # The three are distinct wherever the link is not the identity,
    # so a type collapsed onto another cannot pass unnoticed.
    if (r != "mass") {
      expect_false(isTRUE(all.equal(ask("expected"), ask("link"))))
    }
  }
})


test_that("a wide fit refuses an argument nothing reads", {
  # `resp` is the argument these methods do take, so a neighbour of it
  # that nothing reads is the one most likely to be mistyped here.
  expect_error(posterior_epred(fit, zzz_unknown = 1))
  expect_error(posterior_predict(fit, zzz_unknown = 1))
  expect_error(fitted(fit, zzz_unknown = 1))
  expect_error(log_lik(fit, zzz_unknown = 1))
})


test_that("pp_check says which argument it ignored", {
  # This one already tells the user, and the notice comes from
  # bayesplot checking its own dots rather than from mvgam. It is the
  # weaker half of what `forecast()` does, since a plot is still
  # returned on the default the caller was overriding, but it names
  # the argument and so cannot be missed silently. Pinned here
  # because it is the only call on this surface that says anything,
  # and it would go if the route to bayesplot changed.
  seen <- character(0)
  suppressWarnings(withCallingHandlers(
    pp_check(fit, resp = "count", ndraws = 10L, zzz_unknown = 1),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  expect_true(any(grepl("zzz_unknown", seen, fixed = TRUE)))
  expect_true(any(grepl("unrecognized|ignored", seen)))
})


test_that("pp_check names the response it is asked for", {
  # A wide fit has one set of observations per response, so a check
  # of observed against replicated has to be told which. Answering
  # without being told would plot one arm and label it as the fit.
  # Named, one arm at a time. Each arm has unobserved occasions of
  # its own, so each owes exactly one notice about them, and a check
  # told to draw `seen` must not report `mass`'s gaps.
  for (r in responses) {
    one <- with_warnings(pp_check(fit, resp = r, ndraws = 20L))
    expect_s3_class(one$value, "ggplot")
    miss_one <- grep("missing response", one$warnings, value = TRUE)
    expect_length(miss_one, 1L)
    expect_identical(setdiff(one$warnings, miss_one), character(0))
  }
  # Naming none answers for every arm rather than picking one, so
  # the result is a plot per response and not a plot of the fit.
  # Silently drawing the first arm and labelling it as the fit is
  # what this rules out.
  all_arms <- with_warnings(pp_check(fit, ndraws = 20L))
  expect_type(all_arms$value, "list")
  expect_length(all_arms$value, n_resp)
  for (p in all_arms$value) expect_s3_class(p, "ggplot")
  # Drawing every arm draws every arm's unobserved occasions, so the
  # notice is owed once per response and nothing else is owed at all.
  miss <- grep("missing response", all_arms$warnings, value = TRUE)
  expect_length(miss, n_resp)
  expect_identical(setdiff(all_arms$warnings, miss), character(0))

  # A response the fit never had is refused, and the refusal names
  # the ones that would have worked.
  err <- expect_error(pp_check(fit, resp = "gravity", ndraws = 20L),
                      "Invalid resp")
  for (r in responses) {
    expect_match(conditionMessage(err), r, fixed = TRUE)
  }
})


test_that("the fit reports the family of every arm it was given", {
  # Three responses, three likelihoods. `family()` is what other
  # packages call to decide how to treat a fit, so one answer for
  # three arms sends every one of them down the same path.
  fams <- family(fit)
  reported <- if (is.list(fams) && !inherits(fams, "family")) {
    vapply(fams, function(f) f$family, character(1L))
  } else {
    fams$family
  }
  expect_length(reported, n_resp)
  expect_setequal(as.character(reported),
                  c("poisson", "bernoulli", "gaussian"))

  # `glance()` reads the formula and gets all three, so the answer is
  # on the object and this is the accessor that loses it.
  expect_setequal(as.character(glance(fit)$family),
                  c("poisson", "bernoulli", "gaussian"))

  # Naming a response has to answer about that response. The argument
  # is not in the signature, so it reaches `...` and nothing reads it.
  per_resp <- vapply(responses, function(r) {
    f <- family(fit, resp = r)
    if (is.list(f) && !inherits(f, "family")) NA_character_ else f$family
  }, character(1L))
  expect_identical(unname(per_resp),
                   c("poisson", "bernoulli", "gaussian"))
})


test_that("model.frame carries the responses as well as the terms", {
  # `model.frame()` is the standard route to a fitted model's data. A
  # frame holding the predictors and none of the responses cannot be
  # used for anything it is normally reached for, and the response
  # names are on the formula the whole time.
  mf <- model.frame(fit)
  expect_true(all(responses %in% names(mf)))
  expect_true(all(c("x", "time") %in% names(mf)))

  # The two accessors a caller pairs. `terms()` has no method at all.
  expect_true(inherits(terms(fit), "terms"))
})


test_that("the summary and tidiers name every response", {
  txt <- capture.output(summary(fit))
  expect_gt(length(txt), 10L)
  for (r in responses) {
    expect_true(any(grepl(r, txt, fixed = TRUE)))
  }

  # Every coefficient the sampler estimated reaches the printed
  # summary. On a wide fit the blocks are per response, so an arm
  # omitted entirely is what this catches.
  smry <- summary(fit)
  shown <- unlist(lapply(
    grep("fixed", names(smry), value = TRUE),
    function(k) rownames(smry[[k]])
  ))
  expect_gt(length(shown), 0L)

  aug <- augment(fit)
  expect_true(is.data.frame(aug))
  expect_true(is.data.frame(tidy(fit)))
  expect_true(is.data.frame(glance(fit)))
  expect_true(any(grepl("^ar1_trend\\[", variables(fit))))
})


test_that("conditional_effects answers per response, on its own scale", {
  # A wide fit has one effect per response, so the result is keyed by
  # response first and by covariate within. Returning a single set
  # would give one curve for three likelihoods.
  got <- with_warnings(conditional_effects(fit))
  expect_true(all(grepl("not known to be supported", got$warnings)))
  ce <- got$value
  expect_identical(names(ce), responses)

  for (r in responses) {
    expect_s3_class(ce[[r]], "mvgam_conditional_effects")
    expect_identical(names(ce[[r]]), "x")
    d <- ce[[r]][["x"]]$data
    expect_true(all(is.finite(d$estimate)))
    expect_true(all(d$conf.low <= d$estimate))
    expect_true(all(d$estimate <= d$conf.high))
    expect_gt(stats::sd(d$estimate), 0)
    # Drawn over the covariate's own range rather than over a rank.
    expect_equal(range(d$x), range(dat$x), tolerance = 1e-6)
  }

  # Each curve is on its own family's scale, which is what says the
  # response-specific inverse link was applied rather than the first
  # arm's. A binary arm reported on a count scale, or a count arm on
  # a probability scale, is a wrong curve that is finite, ordered
  # and correctly shaped throughout.
  expect_true(all(ce[["count"]][["x"]]$data$estimate > 0))
  seen_est <- ce[["seen"]][["x"]]$data$estimate
  expect_true(all(seen_est > 0 & seen_est < 1))
  expect_gt(max(ce[["count"]][["x"]]$data$estimate), 1)
})


test_that("the criticism surface runs on a wide fit", {
  loo_warnings <- character(0)
  ic <- withCallingHandlers(
    loo(fit),
    warning = function(w) {
      loo_warnings <<- c(loo_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  # A high Pareto-k is what a latent state-space fit is expected to
  # produce. The claim is that the user is told the truth about it:
  # the notice arrives exactly when there is something to report.
  expect_identical(
    any(pareto_k > 0.7),
    any(grepl("Pareto k", loo_warnings))
  )
  expect_true(all(grepl("Pareto k", loo_warnings)))
})


test_that("quantile residuals sit on the scale they are defined on", {
  # A randomised quantile residual is standard normal by construction,
  # so the claim is about a known distribution rather than about this
  # fit. Each arm is checked on its own family, since a scale error
  # that reached every arm equally would be a different fault.
  rq <- residuals(fit, type = "quantile", ndraws = 200L,
                  summary = FALSE)
  for (r in responses) {
    v <- as.numeric(rq[[r]])
    v <- v[is.finite(v)]
    expect_gt(length(v), 0L)
    # Half again either way is generous for 200 draws over ~57 rows.
    expect_gt(stats::sd(v), 0.6)
    expect_lt(stats::sd(v), 1.6)
    # A standard normal puts 0.27 per cent beyond three.
    expect_lt(mean(abs(v) > 3), 0.02)
  }
})


test_that("each trend panel draws its own response's latent state", {
  # The sampler holds one latent column per response and they differ.
  # Three panels drawn from one of them is the failure this file
  # exists for: every strip correct, every panel the right width, one
  # trajectory repeated three times.
  dm <- posterior::as_draws_matrix(fit$fit)
  n_t <- fit$standata$N_time_trend
  drawn_state <- lapply(seq_len(n_resp), function(k) {
    vapply(seq_len(n_t),
           function(t) mean(dm[, paste0("trend[", t, ",", k, "]")]),
           numeric(1L))
  })
  # The premise: the states the sampler drew are not one state.
  for (i in seq_len(n_resp)) {
    for (j in seq_len(n_resp)) {
      if (j <= i) next
      expect_gt(max(abs(drawn_state[[i]] - drawn_state[[j]])), 1e-6)
    }
  }

  p <- plot(fit, type = "trend")
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  strip_col <- intersect(c("series", "trend"), names(lay))[1L]
  expect_false(is.na(strip_col))

  # Panels follow the order the responses were declared in.
  expect_identical(as.character(lay[[strip_col]]), responses)

  # The line each panel draws, keyed by the occasion it sits at.
  line_layers <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1L)
  ))
  expect_gt(length(line_layers), 0L)
  per_panel <- list()
  for (i in line_layers) {
    dd <- b$data[[i]]
    for (pn in unique(as.integer(dd$PANEL))) {
      v <- dd[as.integer(dd$PANEL) == pn, c("x", "y"), drop = FALSE]
      key <- as.character(pn)
      per_panel[[key]] <- rbind(per_panel[[key]], v)
    }
  }
  expect_identical(length(per_panel), as.integer(n_resp))

  # Two panels drawing one series agree wherever they overlap, so the
  # claim is that they disagree somewhere.
  keys <- names(per_panel)
  for (i in seq_along(keys)) {
    for (j in seq_along(keys)) {
      if (j <= i) next
      shared <- merge(per_panel[[keys[i]]], per_panel[[keys[j]]],
                      by = "x")
      expect_gt(nrow(shared), 0L)
      expect_gt(max(abs(shared$y.x - shared$y.y)), 1e-6)
    }
  }
})


test_that("the series plot draws every response, and names them", {
  # One panel labelled NA leaves two responses undrawn and the third
  # unnamed.
  p <- plot(fit, type = "series")
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  strip_col <- intersect(c("series", "trend"), names(lay))[1L]
  expect_identical(nrow(lay), as.integer(n_resp))
  expect_false(any(is.na(as.character(lay[[strip_col]]))))
  expect_setequal(as.character(lay[[strip_col]]), responses)
})


test_that("the plotting methods render for a wide fit", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Which plots answer per response and which answer once is the
  # claim. `residuals` compares observations against fitted values,
  # and a wide fit has a set of each per response, so it returns one
  # panel per arm keyed by response. `trend` and `series` describe
  # the shared latent process and answer once. A method that folded
  # three arms into one panel, or split the shared trend into three,
  # returns a plottable object either way.
  drawn <- list()
  drawn$residuals <- with_warnings(plot(fit, type = "residuals"))
  expect_type(drawn$residuals$value, "list")
  expect_identical(names(drawn$residuals$value), responses)
  for (pl in drawn$residuals$value) expect_s3_class(pl, "ggplot")

  for (ty in c("trend", "series")) {
    drawn[[ty]] <- with_warnings(plot(fit, type = ty))
    expect_s3_class(drawn[[ty]]$value, "ggplot")
  }
  drawn$mcmc <- with_warnings(mcmc_plot(fit))
  expect_s3_class(drawn$mcmc$value, "ggplot")

  # Every arm has unobserved occasions, so a plot drawing
  # observations owes a notice about them, once per arm it drew.
  # Counted rather than silenced, and the plots that draw none owe
  # nothing.
  miss <- function(w) grep("missing response", w, value = TRUE)
  expect_length(miss(drawn$residuals$warnings), n_resp)
  expect_identical(miss(drawn$trend$warnings), character(0))
  expect_identical(miss(drawn$series$warnings), character(0))
  expect_identical(miss(drawn$mcmc$warnings), character(0))
  for (nm in names(drawn)) {
    expect_identical(
      setdiff(drawn[[nm]]$warnings, miss(drawn[[nm]]$warnings)),
      character(0)
    )
  }
})


# -- The newdata battery ----------------------------------------------

ref_epred <- posterior_epred(fit, newdata = dat, resp = responses[1L],
                             draw_ids = 1:10, incl_autocor = TRUE)

test_that("the reference the battery compares against actually varies", {
  # Every check below compares a rearranged or cut frame against the
  # same columns of `ref_epred`. That is a comparison of the
  # prediction with itself, so a predictor returning one constant for
  # every row satisfies all of it. This is the guard that makes the
  # rest mean something.
  colm <- colMeans(ref_epred)
  expect_gt(stats::sd(colm), 0)
  expect_gt(length(unique(round(colm, 8))), 1L)
})


test_that("a shuffled newdata answers the same, in the new order", {
  set.seed(88L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           resp = responses[1L], draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )
})


test_that("single, repeated and cut frames read the right occasions", {
  for (j in c(1L, 25L, n_time)) {
    one <- posterior_epred(fit, newdata = dat[j, , drop = FALSE],
                           resp = responses[1L], draw_ids = 1:10,
                           incl_autocor = TRUE)
    expect_identical(dim(one), c(10L, 1L))
    expect_equal(unname(one), unname(ref_epred[, j, drop = FALSE]))
  }
  rep_nd <- dat[c(9L, 9L), , drop = FALSE]
  got <- posterior_epred(fit, newdata = rep_nd, resp = responses[1L],
                         draw_ids = 1:10, incl_autocor = TRUE)
  expect_equal(unname(got[, 1L]), unname(got[, 2L]))

  rows <- seq(2L, n_time, by = 3L)
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[rows, , drop = FALSE],
                           resp = responses[1L], draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred[, rows, drop = FALSE])
  )
})


test_that("feeding the training frame back as newdata is a no-op", {
  bare <- posterior_epred(fit, resp = responses[1L], draw_ids = 1:5,
                          process_error = FALSE)
  with_nd <- posterior_epred(fit, newdata = dat, resp = responses[1L],
                             draw_ids = 1:5, process_error = FALSE)
  expect_equal(bare, with_nd)
})


# -- The marginaleffects backend on a wide frame ----------------------

library(marginaleffects)
options("marginaleffects_model_classes" = "mvgam")


test_that("get_predict refuses a wide fit that names no response", {
  # A wide fit has one predictive surface per response, so a call
  # that names none has no answer to give. Returning the first arm
  # would be well formed and silently wrong, and the refusal has to
  # say what is missing rather than fail later on a dimension.
  err <- expect_error(
    get_predict(fit, newdata = dat, type = "response"),
    regexp = "requires"
  )
  expect_match(conditionMessage(err), "resp", fixed = TRUE)
})


test_that("each arm answers on its own rows and its own draws", {
  for (r in responses) {
    out <- get_predict(fit, newdata = dat, type = "expected", resp = r)
    expect_identical(nrow(out), nrow(dat))
    draws <- attr(out, "posterior_draws")
    # marginaleffects stores draws as [nobs x ndraws], the transpose
    # of mvgam's layout, so both margins are the claim.
    expect_identical(dim(draws),
                     c(nrow(dat), as.integer(ndraws(fit))))
    expect_equal(out$estimate, unname(apply(draws, 1L, stats::median)))
    # The draws are this response's own, not the first arm's repeated
    # under three names. Compared against the public method for the
    # same arm, they agree exactly.
    expect_equal(draws, t(posterior_epred(fit, newdata = dat, resp = r)),
                 ignore_attr = TRUE)
  }
})


test_that("each arm predicts on the support its own family has", {
  # The three arms carry a poisson, a bernoulli and a gaussian, so
  # their expectations occupy three different sets. An arm answered
  # under another's family keeps every dimension and lands in the
  # wrong one. Stated as membership rather than as a comparison
  # between arms, so the claim does not depend on which values this
  # particular simulation happened to draw.
  #
  # Naming an arm raises a marginaleffects notice saying `resp` is
  # not known to be supported for this class, once per arm. mvgam
  # forwards and honours it, so the notice is wrong and the class has
  # not been registered on that whitelist. Captured here rather than
  # left to leak, and asserted as the absence it should be.
  resp_warnings <- character(0)
  est <- lapply(responses, function(r) {
    withCallingHandlers(
      predictions(fit, newdata = dat, type = "expected", resp = r),
      warning = function(w) {
        resp_warnings <<- c(resp_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  })
  names(est) <- responses
  expect_identical(
    grep("not known to be supported", resp_warnings, value = TRUE),
    character(0)
  )

  # A bernoulli expectation is a probability, bounds included. A
  # prediction that escaped to the link scale keeps every dimension
  # and leaves the unit interval.
  seen <- est[["seen"]]
  expect_true(all(seen$estimate > 0 & seen$estimate < 1))
  expect_true(all(seen$conf.low > 0 & seen$conf.high < 1))
  expect_true(all(seen$conf.low <= seen$estimate))
  expect_true(all(seen$estimate <= seen$conf.high))
  # One row of the frame is one row of the output, in order, so a
  # prediction placed by position rather than by content fails.
  expect_equal(as.numeric(seen$x), as.numeric(dat$x))

  # A poisson expectation is a positive rate and is not confined to
  # the unit interval on this frame, whose counts run in the tens.
  count <- est[["count"]]
  expect_true(all(count$estimate > 0))
  expect_gt(max(count$estimate), 1)
  # A gaussian expectation is bounded on neither side by the family,
  # which is what separates it from the other two.
  mass <- est[["mass"]]
  expect_true(all(is.finite(mass$estimate)))
  # And no two arms are the same numbers under different names.
  for (pair in list(c("count", "seen"), c("count", "mass"),
                    c("seen", "mass"))) {
    expect_false(isTRUE(all.equal(est[[pair[1L]]]$estimate,
                                  est[[pair[2L]]]$estimate)))
  }
})


test_that("a wide fit can be scored by leaving future occasions out", {
  # A wide frame has no series column: the series is the response,
  # which is why the axis record answers `multivariate` and lists
  # the three names. `lfo_cv()` asked the frame for a column it
  # cannot have, and once that demand went it split the frame by a
  # row-series that a response-keyed axis has no answer for, leaving
  # a factor of no rows.
  #
  # Every row of a wide frame carries every response, so the
  # responses cannot disagree about the time grid and there is
  # nothing to split. What the call has to produce is a score at
  # each occasion it rolled over.
  lfo <- lfo_cv(fit, min_t = 45, silent = 2)
  expect_s3_class(lfo, "mvgam_lfo")
  expect_gt(length(lfo$eval_timepoints), 0L)
  # The occasions evaluated are occasions the frame holds, not ranks
  # standing in for them.
  expect_true(all(lfo$eval_timepoints %in% sort(unique(dat$time))))
  expect_true(all(lfo$eval_timepoints > 45))
  expect_identical(length(lfo$elpds), length(lfo$eval_timepoints))
  expect_true(all(is.finite(lfo$elpds)))
  # A log density is negative, and a sum of them over seventeen
  # occasions of three responses is well away from zero.
  expect_lt(sum(lfo$elpds), 0)
})


test_that("a proper score runs beside the elpd", {
  # `elpd` used to be the only rule `lfo_cv()` could report. Every
  # other score needs predictive draws rather than a density, and
  # those were fetched with `forecast()`, which extends the grid past
  # the last observed occasion. A fold is held out by masking, so the
  # window sits inside the grid and the call was refused; a
  # `tryCatch()` turned the refusal into a silent `NA`.
  #
  # The draws now come from the state the fold was scored on, so the
  # two scores describe one predictive.
  lfo <- lfo_cv(fit, min_t = 45, score = c("elpd", "crps"),
                silent = 2)
  expect_true("crps" %in% names(lfo$scores))
  expect_identical(length(lfo$scores$crps),
                   length(lfo$eval_timepoints))
  # A CRPS is a non-negative loss, and one that came back as `NA`
  # per fold is what the swallowed refusal looked like.
  expect_false(anyNA(lfo$scores$crps))
  expect_true(all(lfo$scores$crps >= 0))
  expect_true(all(is.finite(lfo$scores$crps)))
  # The two rules are scored on one predictive, so a fold the model
  # found surprising should cost on both. Rank agreement is the
  # claim that survives their different scales.
  expect_gt(
    stats::cor(lfo$elpds, -lfo$scores$crps, method = "spearman"),
    0
  )
})


cat("\nDone.\n")
