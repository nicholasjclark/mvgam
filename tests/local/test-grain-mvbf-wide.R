# Structure and post-fit coverage for a wide `mvbf()` frame.
#
# A wide frame holds one row per occasion and one column per response.
# The series of an observation is a property of the (row, response)
# pair. A frame cut into one block of rows per response gives each
# response a stretch of the timeline in place of a series, with every
# count, dimension and bound still correct. The tests compare values
# for that reason.
#
# Three responses with three likelihoods share one trend. Each claim
# about how a family reaches a surface is then made three times by one
# fit.
#
#   truth: 3 responses on 60 shared occasions, one latent AR per
#          response with correlated innovations; each response
#          misses its own disjoint set of occasions
#   model: mvbf(count ~ x, seen ~ x, mass ~ x) with poisson,
#          bernoulli and gaussian responses, trend_formula = ~ AR(cor)
#
# The responses run against alphabetical order and the occasions start
# at 3. Sorting recovers neither the axis nor the time index.
#
# Run with:
#   testthat::test_file("tests/local/test-grain-mvbf-wide.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})

set.seed(5150L)

n_time <- 60L
time_vals <- seq_len(n_time) + 2L

# Formula order is the axis. Alphabetical order is mass, count, seen.
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

# Disjoint gaps of three sizes. Swapping two responses changes their
# lengths. `mass` stops four occasions early. Each response then ends
# at its own last observed time, which exposes a permuted record.
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

# The support each response's draws must land in. A wide fit that
# applied the first response's family to all three returns counts
# everywhere and passes every shape check.
support_ok <- list(
  count = function(v) all(v >= 0) && all(v == floor(v)),
  seen = function(v) all(v %in% c(0, 1)),
  mass = function(v) any(abs(v - round(v)) > 1e-8)
)

sim_truth <- list(
  n_time = n_time, time_vals = time_vals, responses = responses,
  ar_true = ar_true, latent = latent, x = x, na_rows = na_rows
)

# marginaleffects warns that `resp` is not known to be supported for
# this class. Its `sanity_dots()` hard-codes the accepted names per
# class, outside mvgam's control. The helper asserts that this notice
# is the only warning.
predictions_resp <- function(...) {
  got <- with_warnings(marginaleffects::predictions(...))
  expect_true(all(grepl("not known to be supported", got$warnings)))
  got$value
}

# The titles of a patchwork's panels, in drawing order
panel_titles <- function(pw) {
  vapply(c(pw$patches$plots, list(pw)),
         function(p) p$labels$title %||% NA_character_, character(1L))
}

# A future frame in the wide shape. Each row holds one occasion, the
# covariate `x` and a missing value for every response.
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


test_that("the dropped occasions are reported once for the frame", {
  # brms drops a row whose response is missing and says so once for
  # the frame. Building a model runs the code generator more than once
  # over the same frame. A notice per internal pass repeats it. A
  # missing notice drops rows from the likelihood in silence.
  na_notice <- grep("Rows containing NAs", built$warnings, value = TRUE)
  expect_length(na_notice, 1L)
  expect_identical(setdiff(built$warnings, na_notice), character(0))
})


test_that("the series axis is the responses, in formula order", {
  ax <- mvgam:::mvgam_axes(prefit)
  expect_identical(as.character(ax$series$levels), responses)
  expect_identical(as.integer(ax$series$n), n_resp)
  # An axis recorded as explicit would look for a series column that a
  # wide frame lacks
  expect_identical(ax$series$source, "multivariate")
  expect_identical(as.integer(ax$time$values), time_vals)
  expect_identical(as.integer(ax$time$n), n_time)
})


test_that("a wide frame gives no per-row series", {
  # `axis_row_series()` returns the series of each row. A row of a wide
  # frame carries every response and belongs to every series.
  # Returning the first series would place every observation in one
  # column.
  expect_null(mvgam:::axis_row_series(prefit, dat))
})


test_that("each response indexes its own column of the latent grid", {
  sd <- prefit$standata
  expect_identical(as.integer(sd$N_series_trend), n_resp)
  expect_identical(as.integer(sd$N_time_trend), n_time)

  # Each response indexes one column at its own observed occasions,
  # ranked against the shared grid. A block of rows cut per response
  # spreads a response over columns and gives it the wrong times.
  for (k in seq_along(responses)) {
    r <- responses[k]
    obs <- observed_rows[[r]]
    expect_identical(as.integer(sd[[paste0("N_", r)]]), length(obs))
    expect_identical(as.integer(sd[[paste0("obs_trend_series_", r)]]),
                     rep(k, length(obs)))
    expect_identical(as.integer(sd[[paste0("obs_trend_time_", r)]]),
                     as.integer(obs))
  }
})


test_that("the program indexes the trend by series and time", {
  sc <- paste(as.character(stancode(prefit)), collapse = "\n")
  flat <- gsub("\\s+", " ", sc)
  for (r in responses) {
    expect_true(grepl(
      paste0("obs_trend_idx_", r, "[n] = (obs_trend_series_", r,
             "[n] - 1) * N_time_trend + obs_trend_time_", r, "[n];"),
      flat, fixed = TRUE
    ))
  }
  # `cor = TRUE` draws the innovations of the three latent dimensions
  # jointly
  expect_true(grepl("cov_matrix[N_lv_trend] Sigma_trend", sc,
                    fixed = TRUE))
})


# -- Fit --------------------------------------------------------------

# The build runs only on a cache miss. It captures its warnings
# without asserting them. The prefit block asserts them on every run.
fit <- cached_fit(
  "val_mvgam_mvbf_wide.rds",
  function() {
    with_warnings(mvgam(
      formula = obs_formula, trend_formula = ~ AR(p = 1, cor = TRUE),
      data = dat, chains = 2L, iter = 1000L, warmup = 500L,
      silent = 2, backend = "cmdstanr"
    ))$value
  },
  key = sim_truth
)


test_that("the fitted object keeps the response axis", {
  expect_identical(names(mvgam:::response_columns(fit)), responses)
  expect_identical(
    as.character(mvgam:::mvgam_axes(fit)$series$levels), responses
  )
  expect_identical(as.integer(fit$standata$N_series_trend), n_resp)
})


# The sampler's posterior mean latent state, one column per response,
# by occasion. Later blocks compare what a method reports against it.
sampler_draws <- posterior::as_draws_matrix(fit$fit)
sampler_state <- vapply(seq_len(n_resp), function(k) {
  vapply(seq_len(n_time), function(t) {
    mean(sampler_draws[, paste0("trend[", t, ",", k, "]")])
  }, numeric(1L))
}, numeric(n_time))
colnames(sampler_state) <- responses


test_that("each response holds a latent state of its own", {
  # Two responses sharing one column pass every check that counts
  expect_gt(min(stats::dist(t(sampler_state), method = "maximum")), 0.1)
})


test_that("posterior_predict draws from each response's own family", {
  for (r in responses) {
    pp <- posterior_predict(fit, resp = r, ndraws = 50L)
    expect_identical(nrow(pp), 50L)
    expect_true(support_ok[[r]](pp[!is.na(pp)]),
                label = paste(r, "draws land in its family's support"))
  }
})


test_that("each response's expectation applies its own inverse link", {
  inv_link <- list(count = exp, seen = stats::plogis, mass = identity)
  for (r in responses) {
    ep <- posterior_epred(fit, resp = r, draw_ids = 1:20)
    lp <- posterior_linpred(fit, resp = r, draw_ids = 1:20)
    expect_identical(nrow(ep), 20L)
    expect_equal(unname(ep), unname(inv_link[[r]](lp)), tolerance = 1e-8)
    expect_identical(nrow(fitted(fit, resp = r, ndraws = 20L)),
                     ncol(ep))
    expect_identical(nrow(residuals(fit, resp = r, ndraws = 20L)),
                     ncol(ep))
  }
})


test_that("the joint log_lik sums the per-response densities", {
  # A joint density that dropped a response is finite, correctly shaped
  # and wrong
  per <- lapply(stats::setNames(responses, responses), function(r) {
    log_lik(fit, resp = r, draw_ids = 1:10)
  })
  for (r in responses) {
    expect_identical(dim(per[[r]]), c(10L, n_time))
    # Finite where the response was measured and missing exactly where
    # it was not. The gaps differ per response, which places each
    # response's missing values on its own occasions.
    expect_true(all(is.finite(per[[r]][, observed_rows[[r]]])))
    expect_true(all(is.na(per[[r]][, na_rows[[r]]])))
  }

  # The gaps are disjoint. Every occasion holds at least one observed
  # response. The joint density then covers all sixty occasions.
  # `loo()` uses these numbers.
  joint <- log_lik(fit, draw_ids = 1:10)
  stopifnot(length(Reduce(intersect, na_rows)) == 0L)
  expect_false(anyNA(joint))
  expect_equal(unname(joint),
               unname(Reduce(`+`, lapply(per, function(m) {
                 m[is.na(m)] <- 0
                 m
               }))))
})


test_that("a wide fit forecasts every response over the horizon", {
  h <- 4L
  future <- make_future(h)
  fc <- forecast(fit, newdata = future, type = "link")
  expect_identical(names(fc), responses)

  for (r in responses) {
    # Each response of a wide frame is one series
    expect_identical(as.character(fc[[r]]$series_names), r)
    draws <- fc[[r]]$forecasts[[r]]
    expect_identical(ncol(draws), h)
    expect_false(anyNA(draws))
    # The horizon keeps the user's own time numbering
    expect_identical(as.integer(fc[[r]]$test_times[[r]]),
                     as.integer(future$time))
    # The training history is this response's own observations
    expect_equal(as.numeric(fc[[r]]$train_observations[[r]]),
                 as.numeric(dat[[r]][observed_rows[[r]]]))
  }
})


test_that("a forecast responds to the covariate it is given", {
  # The forecast evaluates the observation predictor at the supplied
  # `x`. A forecast that ignored `x`, or took it from the training
  # frame, returns the same numbers for both frames.
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


test_that("hindcast and forecast draw from each response's family", {
  hc <- hindcast(fit, type = "response", ndraws = 20L)
  fc <- forecast(fit, newdata = make_future(2L), type = "response",
                 ndraws = 20L)
  expect_identical(names(hc), responses)
  expect_identical(names(fc), responses)
  for (r in responses) {
    for (m in list(hc[[r]]$hindcasts[[r]], fc[[r]]$forecasts[[r]])) {
      expect_true(support_ok[[r]](m[!is.na(m)]))
    }
  }
})


test_that("a horizon the trend cannot step to is refused", {
  # A discrete-time trend steps one occasion at a time along the shared
  # grid. A frame that skips occasions is refused.
  future <- make_future(1L)
  future$time <- max(time_vals) + 10L
  expect_error(
    forecast(fit, newdata = future, type = "link"),
    "continue the training series"
  )
})


test_that("each prediction type gives the quantity it names", {
  # `"expected"` is the family's mean. `"response"` samples from the
  # observation family, summarised by its median. A count or binary
  # response then gives a whole number.
  # The response-scale estimate draws a fresh sample on each call
  withr::local_seed(2027L)
  grid <- dat[seq(1L, n_time, length.out = 6L), , drop = FALSE]

  for (r in responses) {
    ask <- function(ty) {
      as.numeric(predictions_resp(fit, newdata = grid, type = ty,
                                  resp = r)$estimate)
    }
    # marginaleffects summarises a Bayesian posterior by its median.
    # Each estimate equals the median of the same draws.
    expect_equal(ask("expected"),
                 apply(posterior_epred(fit, newdata = grid, resp = r),
                       2L, stats::median), ignore_attr = TRUE,
                 tolerance = 1e-8)
    expect_equal(ask("link"),
                 apply(posterior_linpred(fit, newdata = grid, resp = r),
                       2L, stats::median), ignore_attr = TRUE,
                 tolerance = 1e-8)
    # The median of a discrete sample moves in whole units. Two samples
    # of a count or binary response differ by up to one.
    pp <- posterior_predict(fit, newdata = grid, resp = r, ndraws = 400L)
    tol_response <- if (r == "mass") 0.1 else 1.0
    resp_est <- ask("response")
    expect_lte(max(abs(resp_est - apply(pp, 2L, stats::median))),
               tol_response)
    expect_true(support_ok[[r]](resp_est))
  }
})


test_that("the prediction methods refuse an unused argument by name", {
  for (method in list(posterior_epred, posterior_predict, fitted,
                      log_lik)) {
    expect_error(method(fit, zzz_unknown = 1), "zzz_unknown",
                 fixed = TRUE)
  }
  # The name belongs to neither the prediction method nor the
  # bayesplot kernel. bayesplot named it in a warning and then used the
  # default the caller meant to override.
  expect_error(pp_check(fit, resp = "count", ndraws = 10L, zzz_unknown = 1),
               "zzz_unknown", fixed = TRUE)

  # `alpha` reaches the kernel. `process_error` reaches the prediction
  # method.
  expect_drawn(pp_check(fit, resp = "count", ndraws = 10L, alpha = 0.5))
  expect_drawn(
    pp_check(fit, resp = "count", ndraws = 10L, process_error = FALSE)
  )
})


test_that("nobs counts the frame's rows", {
  # On a wide frame `standata$N` counts one response's observations.
  # `nobs()` once fell back to it when the data slot was empty.
  expect_identical(as.integer(nobs(fit)), n_time)
  expect_identical(nrow(model.frame(fit)), n_time)
})


test_that("pp_check names the response it is asked for", {
  # Each response has unobserved occasions. The check omits them and
  # says so once per session.
  withr::local_envvar(TESTTHAT = "")
  rlang::reset_message_verbosity("mvgam_pp_check_missing_response")
  expect_message(
    expect_drawn(pp_check(fit, resp = "count", ndraws = 20L)),
    "missing response are omitted"
  )
  # Each check draws its own response's observed rows
  for (r in responses) {
    expect_no_warning(
      one <- pp_check(fit, type = "dens_overlay", resp = r, ndraws = 20L)
    )
    expect_equal(one$data$value[one$data$is_y],
                 dat[[r]][observed_rows[[r]]])
  }
  # A binary response defaults to bars, a count to a rootogram and a
  # continuous response to a density overlay
  geom_of <- function(p) class(p$layers[[1L]]$geom)[1L]
  seen_bars <- pp_check(fit, resp = "seen", ndraws = 20L)
  expect_identical(geom_of(seen_bars), "GeomCol")
  expect_identical(geom_of(pp_check(fit, resp = "count", ndraws = 20L)),
                   "GeomTile")
  expect_identical(geom_of(pp_check(fit, resp = "mass", ndraws = 20L)),
                   "GeomLine")
  # The binary axis labels its two values alone
  x_breaks <- ggplot2::ggplot_build(seen_bars)$layout$panel_params[[1L]]$x$breaks
  expect_identical(x_breaks[!is.na(x_breaks)], c(0, 1))
  # Without `resp` the check draws one figure with a panel per
  # response, titled by that response
  expect_no_warning(all_resp <- pp_check(fit, ndraws = 20L))
  expect_identical(panel_titles(all_resp), responses)

  # The refusal of an unknown response names the valid ones
  err <- expect_error(pp_check(fit, resp = "gravity", ndraws = 20L),
                      "must be a response of this model")
  for (r in responses) {
    expect_match(conditionMessage(err), r, fixed = TRUE)
  }
})


test_that("family reports each response's family", {
  # Other packages call `family()` to decide how to treat a fit
  families <- c(count = "poisson", seen = "bernoulli", mass = "gaussian")
  expect_identical(vapply(family(fit), `[[`, character(1L), "family"),
                   families)
  for (r in responses) {
    expect_identical(family(fit, resp = r)$family, families[[r]])
  }
  expect_setequal(as.character(glance(fit)$family), families)
})


test_that("model.frame and terms hold the responses and the terms", {
  mf <- model.frame(fit)
  expect_true(all(c(responses, "x", "time") %in% names(mf)))
  # `terms()` builds a `cbind()` left-hand side from the three responses
  tt <- terms(fit)
  expect_identical(deparse(tt[[2L]]), "cbind(count, seen, mass)")
  expect_identical(attr(tt, "term.labels"), "x")
})


test_that("the summary and tidiers name every response", {
  txt <- capture.output(summary(fit))
  for (r in responses) {
    expect_true(any(grepl(r, txt, fixed = TRUE)))
  }

  # Every coefficient the sampler estimated reaches the printed
  # summary. The blocks of a wide fit are per response. A response
  # omitted entirely fails here.
  smry <- summary(fit)
  shown <- unlist(lapply(
    grep("fixed", names(smry), value = TRUE),
    function(k) rownames(smry[[k]])
  ))
  coefs <- sub("^b_", "", grep("^b_", variables(fit), value = TRUE))
  coefs <- coefs[!grepl("_trend$", coefs)]
  for (r in responses) {
    expect_true(any(startsWith(coefs, paste0(r, "_"))))
  }
  expect_identical(paste(setdiff(coefs, shown), collapse = ", "), "")

  expect_tidy_glance(fit)
  expect_identical(glance(fit)$resp, responses)
  # Each response's rows are stacked and named
  aug <- augment(fit)
  expect_identical(as.vector(table(aug$.resp)[responses]),
                   rep(nrow(fit$data), length(responses)))
  expect_true(any(grepl("^ar1_trend\\[", variables(fit))))
})


test_that("conditional_effects gives each response its own curve", {
  # The result is keyed by response, then by covariate. Each response
  # reaches get_predict.mvgam on its own copy of the model. marginaleffects
  # then receives no `resp` argument to warn about.
  expect_no_warning(ce <- conditional_effects(fit))
  expect_identical(names(ce), responses)
  # The drawn panels form one flat list of ggplots, as in brms
  expect_identical(names(plot(ce, plot = FALSE)),
                   paste0(responses, ".x"))

  for (r in responses) {
    expect_identical(names(ce[[r]]), "x")
    d <- ce[[r]][["x"]]$data
    expect_true(all(d$conf.low <= d$estimate & d$estimate <= d$conf.high))
    expect_gt(stats::sd(d$estimate), 0)
    # The x axis spans the covariate's own range
    expect_equal(range(d$x), range(dat$x), tolerance = 1e-6)
  }

  # Each curve takes its own family's inverse link. A binary response
  # drawn on a count scale is finite, ordered and correctly shaped.
  count_est <- ce[["count"]][["x"]]$data$estimate
  seen_est <- ce[["seen"]][["x"]]$data$estimate
  expect_true(all(count_est > 0))
  expect_gt(max(count_est), 1)
  expect_true(all(seen_est > 0 & seen_est < 1))
})


test_that("a wide fit draws its observations for every response", {
  # Each response names the column its points and rug come from
  for (r in responses) {
    got <- with_warnings(
      conditional_effects(fit, resp = r, points = TRUE, rug = TRUE)
    )
    panel <- plot(got$value, plot = FALSE)[[1L]]
    geoms <- vapply(panel$layers, function(l) class(l$geom)[1L],
                    character(1L))
    expect_true(any(grepl("GeomPoint", geoms, fixed = TRUE)))
    expect_true(any(grepl("GeomRug", geoms, fixed = TRUE)))
  }
})


test_that("loo warns about Pareto k exactly when a value is high", {
  caught <- with_warnings(loo(fit))
  ic <- caught$value
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  pareto_k <- ic$diagnostics$pareto_k
  expect_true(all(is.finite(pareto_k)))
  # A latent state-space fit often gives high Pareto k values
  expect_identical(any(pareto_k > 0.7),
                   any(grepl("Pareto k", caught$warnings)))
  expect_true(all(grepl("Pareto k", caught$warnings)))
})


test_that("quantile residuals of every response are standard normal", {
  # A randomised quantile residual is standard normal under the model
  rq <- residuals(fit, type = "quantile", ndraws = 200L,
                  summary = FALSE)
  for (r in responses) {
    v <- as.numeric(rq[[r]])
    v <- v[is.finite(v)]
    expect_gt(stats::sd(v), 0.6)
    expect_lt(stats::sd(v), 1.6)
    # A standard normal puts 0.27 per cent beyond three
    expect_lt(mean(abs(v) > 3), 0.02)
  }
})


test_that("a wide hindcast holds each response's own state, once", {
  # Each response of a wide frame is one series. The per-response loop
  # and the per-series loop inside it once returned three copies of
  # each response's state, two under other responses' names.
  hc <- hindcast(fit, type = "trend")
  expect_identical(names(hc), responses)
  for (r in responses) {
    expect_identical(names(hc[[r]]$hindcasts), r)
    expect_identical(as.character(hc[[r]]$series_names), r)
    state <- colMeans(hc[[r]]$hindcasts[[r]])
    rows <- match(hc[[r]]$train_times[[r]], time_vals)
    gap <- vapply(responses, function(k) {
      max(abs(state - sampler_state[rows, k]))
    }, numeric(1L))
    expect_lt(gap[[r]], 1e-8)
    expect_gt(min(gap[setdiff(responses, r)]), 0.1)
  }
})


test_that("each response is forecast from its own latent state", {
  # The first forecast step is the response's AR coefficient times its
  # last state, plus an innovation. A forecast grid holding one series
  # index for all three responses stepped each from the first
  # response's state. The comparison runs draw by draw.
  step_mean <- vapply(seq_len(n_resp), function(k) {
    as.numeric(sampler_draws[, paste0("ar1_trend[", k, "]")]) *
      as.numeric(sampler_draws[, paste0("trend[", n_time, ",", k, "]")])
  }, numeric(nrow(sampler_draws)))
  colnames(step_mean) <- responses
  # The three conditional means differ
  expect_lt(max(abs(stats::cor(step_mean)[upper.tri(diag(n_resp))])),
            0.9)

  fc <- forecast(fit, newdata = make_future(2L), type = "trend")
  for (r in responses) {
    first_step <- fc[[r]]$forecasts[[r]][, 1L]
    # One row per posterior draw, in the posterior's order
    expect_identical(length(first_step), nrow(sampler_draws))
    tracks <- vapply(responses, function(k) {
      stats::cor(first_step, step_mean[, k])
    }, numeric(1L))
    expect_identical(names(which.max(tracks)), r)
    expect_gt(tracks[[r]], 0.2)
  }
})


test_that("process noise is each response's own, drawn jointly", {
  # A marginal prediction adds a draw of each response's process
  # noise. A single draw added to all three gave every response the
  # first one's scale. The three then moved in lockstep.
  ids <- 1:200
  on <- posterior_linpred(fit, newdata = dat, draw_ids = ids,
                          process_error = TRUE, incl_autocor = FALSE)
  off <- posterior_linpred(fit, newdata = dat, draw_ids = ids,
                           process_error = FALSE, incl_autocor = FALSE)
  noise <- lapply(stats::setNames(responses, responses), function(r) {
    as.numeric(on[[r]] - off[[r]])
  })
  sigma <- colMeans(
    sampler_draws[, paste0("sigma_trend[", seq_len(n_resp), "]")]
  )
  names(sigma) <- responses
  # `mass` has the smallest innovation scale by a wide margin. Its
  # noise must then be the quietest of the three.
  expect_lt(sigma[["mass"]], 0.75 * sigma[["count"]])
  expect_lt(stats::sd(noise$mass), 0.85 * stats::sd(noise$count))
  # The noise is drawn jointly. No pair moves in lockstep.
  for (pair in list(c("count", "seen"), c("count", "mass"),
                    c("seen", "mass"))) {
    expect_lt(abs(stats::cor(noise[[pair[1L]]], noise[[pair[2L]]])),
              0.9)
  }
})


test_that("a wide forecast plots one panel per response", {
  # Each response's draws span its horizon. Draws with zero columns
  # paired with a list of horizon times made `plot()` fail.
  fc <- forecast(fit, newdata = make_future(3L), type = "response",
                 ndraws = 50L)
  for (r in responses) {
    expect_identical(ncol(fc[[r]]$forecasts[[r]]), 3L)
  }
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  b <- ggplot2::ggplot_build(plot(fc))
  expect_identical(as.character(b$layout$layout$series), responses)
  expect_gt(sum(vapply(b$data, nrow, integer(1L))), 0L)
})


test_that("a wide forecast can be scored as forecast returns it", {
  # `forecast()` on a wide fit returns one forecast per response.
  # `score()` takes that object whole.
  held_out <- make_future(4L)
  held_out$count <- c(9L, 12L, 7L, 11L)
  held_out$seen <- c(1L, 0L, 1L, 1L)
  held_out$mass <- c(1.2, 1.8, 1.4, 1.6)
  fc <- forecast(fit, newdata = held_out, type = "response",
                 ndraws = 200L)
  sc <- score(fc, score = "crps")
  for (r in responses) {
    expect_identical(nrow(sc[[r]]), 4L)
    expect_true(all(is.finite(sc[[r]]$score)))
    # The whole object scores each response as its own forecast does
    expect_equal(sc[[r]]$score, score(fc[[r]], score = "crps")[[r]]$score)
  }
  # The Brier score needs a bernoulli response. The refusal for the
  # whole object names the other families.
  seen_fc <- fc[["seen"]]
  expect_identical(seen_fc$family, "bernoulli")
  expect_equal(score(seen_fc, score = "brier")[["seen"]]$score,
               (held_out$seen - colMeans(seen_fc$forecasts$seen))^2)
  expect_error(score(fc, score = "brier"), "poisson")
})


test_that("each trend panel draws its own response's latent state", {
  # Three panels drawn from one latent column keep every strip and
  # every width correct
  p <- plot(fit, type = "trend")
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  # Panels follow the order the responses were declared in
  expect_identical(as.character(lay$series), responses)

  line_layers <- which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1L)
  ))
  expect_gt(length(line_layers), 0L)
  for (k in seq_len(n_resp)) {
    panel <- do.call(rbind, lapply(line_layers, function(i) {
      dd <- b$data[[i]]
      dd[as.integer(dd$PANEL) == k, c("x", "y")]
    }))
    # The median line tracks the sampler's own column more closely
    # than any other column
    rows <- match(panel$x, time_vals)
    gap <- vapply(responses, function(r) {
      mean(abs(panel$y - sampler_state[rows, r]))
    }, numeric(1L))
    expect_identical(names(which.min(gap)), responses[k])
  }
})


test_that("the series plot labels and draws every response", {
  p <- plot(fit, type = "series")
  b <- ggplot2::ggplot_build(p)
  lay <- b$layout$layout
  expect_identical(as.character(lay$series), responses)
  # Each panel holds its own response's observations in time order
  drawn <- b$data[[1L]]
  for (k in seq_len(nrow(lay))) {
    r <- as.character(lay$series[k])
    got <- drawn[as.integer(drawn$PANEL) == as.integer(lay$PANEL[k]), ]
    got <- got[order(got$x), ]
    expect_equal(got$x, time_vals)
    expect_equal(got$y, dat[[r]])
  }

  # Naming a response draws the single-series grid. Its first panel is
  # the series itself.
  one <- plot(fit, type = "series", resp = "seen")
  ts <- ggplot2::ggplot_build(one[[1L]])$data[[1L]]
  expect_equal(ts$y[order(ts$x)], dat$seen)
})


test_that("the plotting methods render for a wide fit", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # The residual plot draws one grid per response, titled by that
  # response. The trend and series plots draw one faceted figure.
  drawn <- list()
  drawn$residuals <- with_warnings(plot(fit, type = "residuals"))
  expect_identical(panel_titles(drawn$residuals$value), responses)

  for (ty in c("trend", "series")) {
    drawn[[ty]] <- with_warnings(plot(fit, type = ty))
    expect_drawn(drawn[[ty]]$value)
  }
  drawn$mcmc <- with_warnings(mcmc_plot(fit))
  expect_drawn(drawn$mcmc$value)

  # The omitted occasions are reported by a once-per-session message
  for (nm in names(drawn)) {
    expect_identical(drawn[[nm]]$warnings, character(0))
  }
})


# -- The newdata battery ----------------------------------------------

ref_epred <- posterior_epred(fit, newdata = dat, resp = responses[1L],
                             draw_ids = 1:10, incl_autocor = TRUE)

test_that("the battery's reference prediction varies by row", {
  # The checks below compare rearranged frames against `ref_epred`. A
  # predictor returning one constant for every row passes all of them.
  expect_gt(stats::sd(colMeans(ref_epred)), 0.1)
})


test_that("a shuffled newdata gives the same predictions, reordered", {
  set.seed(88L)
  perm <- sample(nrow(dat))
  expect_equal(
    unname(posterior_epred(fit, newdata = dat[perm, , drop = FALSE],
                           resp = responses[1L], draw_ids = 1:10,
                           incl_autocor = TRUE)),
    unname(ref_epred[, perm, drop = FALSE])
  )
})


test_that("single, repeated and cut frames predict at their occasions", {
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

test_that("get_predict refuses a wide fit that names no response", {
  # A wide fit has one predictive surface per response. The first
  # response's surface would be well formed and wrong.
  err <- expect_error(
    marginaleffects::get_predict(fit, newdata = dat, type = "response"),
    "takes one response of a multivariate fit"
  )
  expect_match(conditionMessage(err), "resp", fixed = TRUE)
})


test_that("get_predict returns each response's own draws", {
  for (r in responses) {
    out <- marginaleffects::get_predict(fit, newdata = dat,
                                        type = "expected", resp = r)
    draws <- attr(out, "posterior_draws")
    # marginaleffects stores draws as [nobs x ndraws], the transpose of
    # mvgam's layout
    expect_equal(draws, t(posterior_epred(fit, newdata = dat, resp = r)),
                 ignore_attr = TRUE)
    expect_equal(out$estimate, unname(apply(draws, 1L, stats::median)))
  }
})


test_that("predictions gives each response's expectation in order", {
  # The interval of a bernoulli expectation stays in the unit interval.
  # A prediction left on the link scale leaves it.
  seen <- predictions_resp(fit, newdata = dat, type = "expected",
                           resp = "seen")
  expect_true(all(seen$conf.low > 0 & seen$conf.high < 1))
  expect_true(all(seen$conf.low <= seen$estimate &
                    seen$estimate <= seen$conf.high))
  # Row k of the output is row k of the frame
  expect_equal(as.numeric(seen$x), as.numeric(dat$x))
  count <- predictions_resp(fit, newdata = dat, type = "expected",
                            resp = "count")
  expect_true(all(count$estimate > 0))
  expect_gt(max(count$estimate), 1)
})


test_that("lfo_cv scores a wide fit by elpd and CRPS", {
  # A wide frame has no series column. Its responses share one time
  # grid. `lfo_cv()` once asked the frame for a series column.
  lfo <- lfo_cv(fit, min_t = 45, score = c("elpd", "crps"), silent = 2)
  # The evaluated occasions are the frame's own times past `min_t`
  expect_true(all(lfo$eval_timepoints %in% dat$time))
  expect_true(all(lfo$eval_timepoints > 45))
  expect_gt(length(lfo$eval_timepoints), 0L)
  expect_identical(length(lfo$elpds), length(lfo$eval_timepoints))
  expect_true(all(is.finite(lfo$elpds)))
  expect_lt(sum(lfo$elpds), 0)
  # The CRPS draws come from the state each fold was scored on. A
  # forecast past the grid once refused the masked fold. A caught
  # error then left the score NA.
  expect_identical(length(lfo$scores$crps), length(lfo$eval_timepoints))
  expect_true(all(is.finite(lfo$scores$crps) & lfo$scores$crps >= 0))
  # Both rules score one predictive. A surprising fold costs on both.
  expect_gt(stats::cor(lfo$elpds, -lfo$scores$crps, method = "spearman"),
            0)
})


test_that("the predictor's parts sum to it: each response's own state", {
  expect_parts_sum(fit, "state")
})

cat("\nDone.\n")
