# The tests of test-brms-observation-specials.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# Adjacency for the `car()` test: a chain over the 10 location levels
make_loc_adjacency <- function(d) {
  M <- matrix(0L, 10L, 10L)
  diag(M[-1L, ]) <- 1L
  M <- M + t(M)
  rownames(M) <- colnames(M) <- levels(d$loc)
  M
}

test_that("trials() plumbs through to mvgam stancode and standata", {
  d <- make_specials_data()
  prog <- specials_program(ybin | trials(ntrials) ~ x, binomial(), d)
  # The column the user named is what arrives. A length check passed
  # on a constant, and on any other column of the frame.
  expect_identical(as.integer(prog$data$trials), as.integer(d$ntrials))
  expect_match(prog$code, "binomial.*\\|.*trials")
})

test_that("cens() plumbs through and produces censored-likelihood blocks", {
  d <- make_specials_data()
  prog <- specials_program(y | cens(cens_ind) ~ x, gaussian(), d)
  expect_identical(as.integer(prog$data$cens),
                   as.integer(d$cens_ind == "right"))
  # Right-censored rows emit a `normal_lccdf` term in the likelihood.
  expect_match(prog$code, "normal_lccdf")
})

test_that("me() plumbs through measurement-error parameters and prior", {
  d <- make_specials_data()
  prog <- specials_program(y ~ me(x, sdx), gaussian(), d)
  # The noisy covariate and its standard error arrive as the columns
  # `me(x, sdx)` names. Lengths alone passed with the two swapped.
  expect_identical(as.numeric(prog$data$Xn_1), as.numeric(d$x))
  expect_identical(as.numeric(prog$data$noise_1), as.numeric(d$sdx))
  # Latent-observation prior connecting noisy x to error-free Xme.
  expect_match(prog$code, "normal_lpdf\\(Xn_1 \\| Xme_1, noise_1\\)")
})

test_that("mm() plumbs through multiple-membership grouping structure", {
  prog <- specials_program(y ~ x + (1 | mm(g1, g2)), gaussian())
  # brms emits one J_/W_ block per membership level.
  expect_true(all(c("J_1_1", "J_1_2", "W_1_1", "W_1_2") %in%
                    names(prog$data)))
  # Weighted sum across membership levels appears in the linear predictor.
  expect_match(prog$code, "W_1_1\\[n\\] \\* r_1_1\\[J_1_1\\[n\\]\\]")
})

test_that("cs() plumbs through category-specific effects for cumulative", {
  # brms warns that cs() is experimental for cumulative families.
  # mvgam passes the warning to the user once per call.
  expect_warning(
    prog <- specials_program(yord ~ cs(x), cumulative()),
    "Category specific effects"
  )
  expect_true(all(c("Xcs", "Kcs", "nthres") %in% names(prog$data)))
  expect_match(prog$code, "matrix\\[N, nthres\\] mucs = Xcs \\* bcs")
})

test_that("one mvgam() call shows a brms warning once", {
  # Validation and the Stan build each pass the formula through brms,
  # and each pass raises the experimental-effects warning again.
  d <- make_specials_data()
  w <- testthat::capture_warnings(mvgam(
    yord ~ cs(x), trend_formula = ~ AR(), data = d, family = cumulative(),
    run_model = FALSE, silent = 2
  ))
  expect_identical(sum(grepl("Category specific", w)), 1L)
})

test_that("car() resolves its data2 adjacency and emits spatial CAR code", {
  # A special that looks up an object outside `data` once failed with
  # "Object 'M' was not found in 'data2'."
  d <- make_specials_data()
  M <- make_loc_adjacency(d)
  prog <- specials_program(y ~ x + car(M, gr = loc, type = "icar"),
                           gaussian(), d, data2 = list(M = M))
  # ICAR graph keys derived from the adjacency.
  expect_true(all(c("Jloc", "Nedges", "edges1", "edges2") %in%
                    names(prog$data)))
  expect_identical(as.integer(prog$data$Nloc), 10L)
  # Spatial-CAR pairwise-difference penalty appears in the model.
  expect_match(prog$code,
               "dot_self\\(zcar\\[edges1\\] - zcar\\[edges2\\]\\)")
})

test_that("a response named with an underscore or a dot builds a trend model", {
  # The frame is read by the column and the program is written with the
  # key. Reading the frame by the key refused both of these outright.
  set.seed(3L)
  dat <- data.frame(time = 1:20, x = rnorm(20L), series = factor("a"))
  dat$my_y <- rpois(20L, 4)
  uni <- mvgam(brms::bf(my_y ~ x), trend_formula = ~ AR(), data = dat,
               family = poisson(), run_model = FALSE, silent = 2,
               validate = FALSE)
  expect_identical(as.integer(uni$standata$N), 20L)
  expect_identical(response_columns(uni), c(myy = "my_y"))

  wide <- data.frame(time = 1:20, x = rnorm(20L))
  wide$my_count <- rpois(20L, 4)
  wide$mass.kg <- rnorm(20L)
  wide$my_count[c(3L, 9L)] <- NA
  arms <- brms::bf(my_count ~ x, family = poisson()) +
    brms::bf(mass.kg ~ x, family = gaussian()) + brms::set_rescor(FALSE)
  # brms notes the gaps once; that notice is the only one expected.
  seen <- character(0L)
  fit <- withCallingHandlers(
    mvgam(arms, trend_formula = ~ AR(), data = wide,
          run_model = FALSE, silent = 2, validate = FALSE),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(seen, "Rows containing NAs were excluded from the model.")
  # The series axis holds the keys, and each arm maps the rows its own
  # column was observed at.
  expect_identical(mvgam_axes(fit)$series$levels, c("mycount", "masskg"))
  expect_identical(as.integer(fit$standata$obs_trend_time_mycount),
                   setdiff(1:20, c(3L, 9L)))
  expect_identical(as.integer(fit$standata$obs_trend_time_masskg), 1:20)
})
