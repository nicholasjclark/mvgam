# The tests of test-closure-unit-families.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("prepare_closure_unit_family() groups each family's rows by site", {
  # Every family groups the same shape: five sites of four rows each
  cases <- multi_response_cases()
  for (nm in names(cases)) {
    case <- cases[[nm]]
    fam_prep <- mvgam:::prepare_closure_unit_family(
      case$fam, case$data, response_var = "y",
      has_obs_covariates = FALSE, has_det_covariates = FALSE,
      series_var = "series", time_var = "time"
    )
    expect_identical(fam_prep$vars, case$vars, label = nm)
    # The prepared family goes to brms itself, as mvgam's own call does
    sd <- mvgam:::brms_generator("standata")(
      bf(y ~ env, family = fam_prep), data = case$data,
      stanvars = attr(fam_prep, "mvgam_stanvars", exact = TRUE)
    )
    expect_identical(sd$N_unit, 5L, label = nm)
    expect_identical(sd$n_rep, rep(4L, 5L), label = nm)
    expect_identical(dim(sd$visit_idx), c(5L, 4L), label = nm)
    # The family's own stanvars declare the component index. The trend
    # block's `obs_trend_series` holds the same fact but is out of
    # scope where these stanvars are parsed. A simplex likelihood
    # pairs each row with its own softmax probability and declares no
    # component index.
    if ("visit_component" %in% case$vars) {
      expect_identical(
        sd$visit_component,
        matrix(rep(seq_len(4L), each = 5L), nrow = 5L),
        label = nm
      )
    } else {
      expect_null(sd$visit_component, label = nm)
    }
  }
})

test_that("every detection family dispatches its units through reduce_sum", {
  skip_if_not_installed("V8")
  for (key in names(detection_cases)) {
    case <- detection_cases[[key]]
    sc <- detection_stancode(key)
    partial_sum <- paste0("partial_sum_", case$name, "_lpmf")
    expect_match(sc, paste0("real ", partial_sum, "("), fixed = TRUE,
                 label = key)
    expect_match(sc, paste0("reduce_sum(", partial_sum, ","), fixed = TRUE,
                 label = key)
    # stanc wraps the one-statement loop body onto its own line
    expect_match(sc, "array[N_unit] int g_seq;", fixed = TRUE, label = key)
    expect_match(sc, "g_seq[g] = g;", fixed = TRUE, label = key)
    # `%/%` states the integer division that `/` between two ints
    # made stanc report at every closure-unit compile
    expect_match(sc, "int grainsize = N_unit >= 8 ? N_unit %/% 8 : 1;",
                 fixed = TRUE, label = key)
    expect_true(stan_pattern(case$call, sc, fixed = TRUE), label = key)
    # The data arrays have one entry per unit, and visit_idx one
    # column per visit
    expect_match(sc, "int<lower=1> N_unit;", fixed = TRUE, label = key)
    expect_match(sc, "array[N_unit] int<lower=1> n_rep;", fixed = TRUE,
                 label = key)
    expect_match(sc, "array[N_unit, 3] int<lower=1> visit_idx;",
                 fixed = TRUE, label = key)
    y_max <- if (case$binary) "int<lower=0, upper=1>" else "int<lower=0>"
    expect_match(sc, paste0("array[N_unit] ", y_max, " Y_max;"),
                 fixed = TRUE, label = key)
    expect_identical(
      grepl("array[N_unit] int<lower=1> K_max;", sc, fixed = TRUE),
      case$k_max, label = key
    )
    lookup <- c("int K_max_global = max(K_max);",
                "vector[K_max_global] log_n_lookup;",
                "log_n_lookup[n_lk] = log(n_lk);")
    expect_identical(
      unname(vapply(lookup, grepl, logical(1L), x = sc, fixed = TRUE)),
      rep(case$log_n_lookup, 3L), label = key
    )
  }
})

test_that(
  "nmix('poisson_poisson') k_start_ppm collapses to 1 for low counts", {
  # Low Y_max stays at k_start = 1 (no skipped cells); high Y_max
  # gets a Poisson-tail-derived lower bound (~ Y_max - 7*sqrt(Y_max),
  # floor 1). Asserts the standata column matches the closed form.
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  # Force a known Y_max profile by overriding y. Per-unit max
  # detection counts: {0, 5, 100, 500}. The data prep computes
  # Y_max[g] = max(y) over visits in unit g. Raise cap so the
  # closure-unit validator (K_max >= Y_max) is satisfied.
  d$y <- as.integer(c(
    0, 0, 0,
    1, 5, 4,
    80, 100, 95,
    490, 500, 480
  ))
  d$cap <- 600L
  mf <- mvgam_formula(y ~ 1)
  sd <- standata(mf, data = d, family = nmix("poisson_poisson"))
  expect_identical(as.integer(sd$Y_max), c(0L, 5L, 100L, 500L))
  # k_start_ppm[g] = max(1, Y_max[g] - 7 * ceil(sqrt(Y_max[g]))): a
  # floor of 1 up to Y_max = 49, then 100 - 70 and 500 - 161
  expect_identical(as.integer(sd$k_start_ppm), c(1L, 1L, 30L, 339L))
})

test_that(
  "standata under nmix() carries the closure-unit array values", {
  d <- make_nmix_data(n_unit = 5, n_visit = 2, seed = 7)
  mf <- mvgam_formula(y ~ elev)
  sd <- standata(mf, data = d, family = nmix())
  arrs <- build_closure_unit_arrays(d, response_var = "y")
  expect_identical(as.integer(sd$N_unit), arrs$N_unit)
  expect_identical(as.integer(sd$n_rep),  arrs$n_rep)
  expect_identical(as.integer(sd$K_max),  arrs$K_max)
  expect_identical(as.integer(sd$Y_max),  arrs$Y_max)
  expect_equal(dim(sd$visit_idx), c(arrs$N_unit, arrs$max_rep))
  expect_equal(as.integer(sd$visit_idx), as.integer(arrs$visit_idx))
})

test_that(
  "standata K_max updates from a different cap column on newdata", {
  d_fit  <- make_nmix_data(n_unit = 4, n_visit = 3)
  d_pred <- d_fit
  d_pred$cap <- 99L
  mf <- mvgam_formula(y ~ elev)
  sd_fit  <- standata(mf, data = d_fit,  family = nmix())
  sd_pred <- standata(mf, data = d_pred, family = nmix())
  expect_identical(as.integer(sd_fit$K_max),  rep(20L, 4))
  expect_identical(as.integer(sd_pred$K_max), rep(99L, 4))
  # Visit structure is identical; only K_max responds to the
  # edited cap column.
  expect_identical(sd_fit$N_unit, sd_pred$N_unit)
  expect_identical(sd_fit$n_rep,  sd_pred$n_rep)
  expect_identical(sd_fit$visit_idx, sd_pred$visit_idx)
})

test_that(
  "stancode under nmix() emits the vector-p path for a detection formula", {
  d <- make_nmix_data(n_unit = 4, n_visit = 3)
  d$tod <- stats::runif(nrow(d))
  mf <- mvgam_formula(brms::bf(y ~ elev, p ~ tod))
  sc <- as.character(stancode(mf, data = d, family = nmix(), validate = FALSE))
  # brms emits a vector p with inv_logit applied when the
  # sub-formula supplies a design matrix for the detection dpar.
  expect_match(sc, "vector[N] p", fixed = TRUE)
  expect_match(sc, "p = inv_logit(p)", fixed = TRUE)
})

test_that("closure units are keyed by the columns the trend names", {
  # A trend naming its series `site` and its time `yr` keys each unit
  # on those columns. Renaming the columns changes no unit.
  set.seed(1)
  d <- expand.grid(visit = 1:3, series = factor(1:4), time = 1:5)
  d$y <- pmin(rpois(nrow(d), 5), 20L)
  d$cap <- 20L
  renamed <- d
  names(renamed)[match(c("series", "time"), names(renamed))] <-
    c("site", "yr")
  sd_default <- standata(mvgam(
    y ~ 1, trend_formula = ~ AR(), data = d, family = nmix(),
    run_model = FALSE, silent = 2, validate = FALSE
  ))
  sd_renamed <- standata(mvgam(
    y ~ 1, trend_formula = ~ AR(time = yr, series = site),
    data = renamed, family = nmix(), run_model = FALSE, silent = 2,
    validate = FALSE
  ))
  expect_identical(sd_renamed$N_unit, 20L)
  for (nm in c("n_rep", "K_max", "Y_max", "visit_idx")) {
    expect_identical(sd_renamed[[nm]], sd_default[[nm]])
  }
})

test_that(
  "prepare_closure_unit_family() routes multi_season into N_unit", {
  # Build a 3-axis closure-unit dataset: 2 species x 3 sites x 2
  # seasons x 2 visits = 24 rows. Single-season grouping (series,
  # time) collapses across sites and yields 2 * 2 = 4 closure
  # units; multi-season grouping (series, site, time) yields the
  # full 2 * 3 * 2 = 12 units. Asserting the standata N_unit on
  # the same data, switching only the family arg, pins both
  # branches of prepare_closure_unit_family() in one test.
  d <- expand.grid(
    series = factor(c("sp1", "sp2")),
    site   = factor(seq_len(3L)),
    time   = seq_len(2L),
    visit  = seq_len(2L),
    KEEP.OUT.ATTRS = FALSE
  )
  d$y <- 0L
  mf <- mvgam_formula(y ~ 1)
  sd_single <- standata(mf, data = d, family = occ())
  sd_multi  <- standata(mf, data = d, family = occ(multi_season = TRUE))
  # Single-season: closure units = (series, time) = 2 * 2 = 4.
  expect_identical(as.integer(sd_single$N_unit), 4L)
  # Multi-season: closure units = (series, site, time) = 12.
  expect_identical(as.integer(sd_multi$N_unit), 12L)
})

test_that("occ(multi_season = TRUE) rejects data without a 'site' column", {
  d <- data.frame(
    series = factor(c("sp1", "sp1", "sp2", "sp2")),
    time   = c(1L, 2L, 1L, 2L),
    visit  = c(1L, 1L, 1L, 1L),
    y      = 0L
  )
  mf <- mvgam_formula(y ~ 1)
  expect_error(
    standata(mf, data = d, family = occ(multi_season = TRUE)),
    "'site'"
  )
})

test_that("the identifiability guards count the visits that happened", {
  # A missing response is a visit that did not happen, and brms drops
  # those rows from the likelihood. So the model that gets fitted has
  # the observed counts, not the frame's, and a guard reading the
  # frame describes a model nobody fitted.
  #
  # Both guards below were defeated that way: a unit whose rows were
  # all missing still counted toward `n_unit`, and a unit left with
  # one observed visit still counted as two. `closure_unit_index()`
  # is now the one derivation, and it is the builder's.
  mk <- function(y) {
    data.frame(
      series = factor(rep(c("s1", "s2"), each = 2L)),
      time = rep(1L, 4L), visit = rep(1:2, 2L),
      cap = 5L, y = y
    )
  }

  # The control: two units, two visits each, nothing missing.
  sound <- mk(c(1L, 0L, 1L, 1L))
  expect_s3_class(
    mvgam(y ~ 1, data = sound, family = nmix(), run_model = FALSE,
          validate = FALSE),
    "mvgam_prefit"
  )

  # One unit loses every visit, so the fitted model holds one unit
  # and cannot identify the state distribution from it.
  one_unit <- mk(c(1L, 0L, NA, NA))
  expect_identical(
    mvgam:::build_closure_unit_arrays(
      one_unit, response_var = "y",
      unit_grouping_vars = c("series", "time")
    )$N_unit,
    1L
  )
  expect_error(
    mvgam(y ~ 1, data = one_unit, family = nmix(), run_model = FALSE,
          validate = FALSE),
    "at least two closure units"
  )

  # Each unit keeps one visit, which puts lambda and p on the
  # product isocurve with nothing to separate them.
  single_visit <- mk(c(1L, NA, 1L, NA))
  expect_identical(
    mvgam:::build_closure_unit_arrays(
      single_visit, response_var = "y",
      unit_grouping_vars = c("series", "time")
    )$n_rep,
    c(1L, 1L)
  )
  expect_error(
    mvgam(y ~ 1, data = single_visit, family = nmix(),
          run_model = FALSE, validate = FALSE),
    "not identified"
  )
})

test_that("a count family with a cap buffer needs no cap column", {
  # `nmix()` declares `mvgam_default_cap_buffer` in place of a fixed
  # cap. Each unit's bound is `max(y) + buffer` and a `cap` column is
  # optional. The validator once fabricated a cap of 1 and refused any
  # count above it. The post-fit rebuild once demanded the column.
  set.seed(4L)
  d <- expand.grid(visit = 1:3, series = factor(paste0("s", 1:4)))
  d$time <- 1L
  d$y <- rpois(nrow(d), 6)

  expect_s3_class(
    mvgam(y ~ 1, data = d, family = nmix(), run_model = FALSE,
          validate = FALSE),
    "mvgam_prefit"
  )

  # The bound the buffer gives, per unit, is the unit's own largest
  # count plus the buffer -- not a constant, and not one.
  arrays <- mvgam:::build_closure_unit_arrays(d, response_var = "y",
                                              family = nmix())
  expect_identical(arrays$K_max, arrays$Y_max + 100L)

  # A cap column the user does supply is still checked.
  bad <- d
  bad$cap <- 2L
  expect_error(
    mvgam(y ~ 1, data = bad, family = nmix(), run_model = FALSE,
          validate = FALSE),
    "below the observed counts"
  )
})

test_that("a factor count past the identification bound warns, not refuses", {
  set.seed(11L)
  mv_frame <- function(K, n_sites = 20L) {
    sp <- paste0("y", seq_len(K))
    d <- expand.grid(series = factor(sp, levels = sp),
                     time = seq_len(n_sites))
    d$y <- rnorm(nrow(d))
    d$env <- rnorm(nrow(d))
    d[order(d$time, d$series), , drop = FALSE]
  }
  # Four species with two factors is over the bound, and it is what
  # `jsdgam()` supplies by default.
  expect_warning(
    fit <- mvgam(y ~ env, data = mv_frame(4L), family = mvn(),
                 trend_formula = ~ ZMVN(n_lv = 2), run_model = FALSE,
                 silent = 2, validate = FALSE),
    "not identified"
  )
  # Warned about, not refused: the covariance the model reports
  # is estimable there even though its split is not.
  # Five species carry the same two factors, and say nothing.
  expect_no_warning(
    mvgam(y ~ env, data = mv_frame(5L), family = mvn(),
          trend_formula = ~ ZMVN(n_lv = 2), run_model = FALSE,
          silent = 2, validate = FALSE)
  )
  # Neither does one factor on four species.
  expect_no_warning(
    mvgam(y ~ env, data = mv_frame(4L), family = mvn(),
          trend_formula = ~ ZMVN(n_lv = 1), run_model = FALSE,
          silent = 2, validate = FALSE)
  )
})

test_that(
  "stancode under nmix() includes the lpdf signature and data decls", {
  skip_if_not_installed("V8")
  sc <- detection_stancode("nmix")
  expect_match(sc, "real nmix_lpmf\\(\\s*array\\[\\] int y,", fixed = FALSE)
  # Log-space ratio recurrence: per-unit baseline at K_min_g plus
  # the Horner accumulation inside the marginalisation loop. The
  # recurrence avoids the per-k binomial_logit_lpmf re-evaluation
  # that the naive log_sum_exp form pays.
  expect_match(sc, "poisson_log_lpmf(K_min_g | log_lam)", fixed = TRUE)
  expect_match(
    sc, "binomial_logit_lpmf(counts | K_min_g, lp_visits)",
    fixed = TRUE
  )
  # log1m_p is hoisted to the wrapper so the partial sum body reads
  # `log1m_p[idx]` instead of recomputing log1m(p) per chunk. The
  # wrapper precomputes all three link-scale vectors (log_mu,
  # logit_p, log1m_p) before dispatching to reduce_sum.
  expect_match(sc, "real log_ff = log_lam + sum(log1m_p[idx]);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] log1m_p = log1m(p);",
               fixed = TRUE)
  expect_match(
    sc,
    "log_prob_n = log_sum_exp(0, log_prob_n + log_ff + log_k_obs - log_N);",
    fixed = TRUE
  )
  # Scalar-p case: p declared as a bounded probability so the
  # lpdf's logit(p) call is well-defined even without a
  # `p ~ ...` sub-formula.
  expect_match(sc, "real<lower=0, upper=1> p;", fixed = TRUE)
})

test_that("stancode under occ() marginalises the binary state", {
  skip_if_not_installed("V8")
  sc <- detection_stancode("occ")
  # The wrapper hoists logit(mu) -> logit_psi and logit(p) -> logit_p
  # once, and each reduce_sum chunk reuses those vectors.
  expect_match(sc, "vector[num_elements(mu)] logit_psi = logit(mu);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] logit_p = logit(p);",
               fixed = TRUE)
  # Per-unit marginalisation branches on whether anything was
  # detected: z = 1 certain when Y_max[g] >= 1, otherwise
  # log_sum_exp over {z = 0, z = 1}.
  expect_match(sc, "if (Y_max[g] >= 1)", fixed = TRUE)
  expect_match(sc, "log_sum_exp(loglik_z1, loglik_z0)", fixed = TRUE)
  # brms passes p as a scalar when it has no formula. It passes mu as
  # a vector always.
  for (sig in c("vector mu, vector p", "vector mu, real p")) {
    expect_match(
      sc, paste0("real occ_lpmf(array[] int y, ", sig, ", int N_unit,"),
      fixed = TRUE
    )
  }
})

test_that(
  "stancode under nmix('royle_nichols') marginalises abundance", {
  skip_if_not_installed("V8")
  sc <- detection_stancode("royle_nichols")
  # The wrapper hoists log(mu) -> log_mu and log1m(p) -> log_1m_r.
  expect_match(sc, "vector[num_elements(mu)] log_mu = log(mu);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] log_1m_r = log1m(p);",
               fixed = TRUE)
  # Closure constraint: k < cmax pre-filled with -Inf so log_sum_exp
  # over the K_max marginalisation cells skips infeasible states.
  expect_match(sc, "component_lps[k + 1] = negative_infinity();",
               fixed = TRUE)
  # Inner k loop: k * sum_non_det_log_1m_r is the closed-form
  # non-detection contribution; dot_product(counts_v, log1m_exp(k * log_1m_r_v))
  # is the per-visit at-least-one-detection contribution.
  expect_match(sc, "k * sum_non_det_log_1m_r", fixed = TRUE)
  expect_match(sc, "log1m_exp(k * log_1m_r_v)", fixed = TRUE)
  # Haines (2016) closed-form fast-path for the all-zero detection
  # case. cmax == 0 collapses sum_{N=0}^infty Poisson(N|lambda) *
  # prod_t (1-r_t)^N to exp(lambda * (prod_t (1-r_t) - 1)) via the
  # Poisson MGF -- exact, scalar, no log_sum_exp, no K_max
  # truncation bias on the dominant all-zero units.
  expect_match(sc, "if (cmax == 0)", fixed = TRUE)
  expect_match(sc, "real log_z = sum(log_1m_r_v);", fixed = TRUE)
  expect_match(sc, "lp += exp(log_lam) * (exp(log_z) - 1.0);",
               fixed = TRUE)
})

test_that(
  "stancode under nmix('poisson_poisson') factors the visit sum", {
  skip_if_not_installed("V8")
  sc <- detection_stancode("poisson_poisson")
  # The wrapper hoists log(mu) -> log_mu and log(p) -> log_p once.
  # The partial sum also takes the raw p, whose visit sum k * sum(p_v)
  # is the Poisson rate of the inner cell.
  expect_match(sc, "vector[num_elements(mu)] log_mu = log(mu);",
               fixed = TRUE)
  expect_match(sc, "vector[num_elements(p)] log_p = log(p);",
               fixed = TRUE)
  # Inner k loop uses log_n_lookup[k] instead of scalar log(k) so
  # the AD tape skips the per-iter math op.
  expect_match(sc, "log_n_lookup[k] * sum_counts", fixed = TRUE)
  # k = 0 only feasible when no detections; flip to -Inf otherwise
  # so log_sum_exp over the full K range marginalises cleanly.
  expect_match(sc, "if (any_detection)", fixed = TRUE)
  expect_match(sc, "component_lps[1] = negative_infinity();",
               fixed = TRUE)
  # Per-unit Poisson-tail lower bound on the latent-N loop. The
  # lpmf reads k_start_ppm[g] as the inner-loop start; skipping
  # cells below it costs negligible mass (see make_nmix_poisson_
  # poisson_stanvars derivation).
  expect_match(sc, "array[N_unit] int<lower=1> k_start_ppm;",
               fixed = TRUE)
  expect_match(sc, "int kg_lo = k_start_ppm[g];", fixed = TRUE)
  expect_match(sc, "for (k in kg_lo : Kg)", fixed = TRUE)
})
