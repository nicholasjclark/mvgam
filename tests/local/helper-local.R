# Checks several local files share. testthat sources this file before
# each local test file in this directory.


# The path of a fixture. testthat sets the working directory to the
# test file's own and Rscript runs from the package root. The
# directory depends on which of the two runs the file: asking whether
# `fixtures` exists picks the wrong branch on a clean tree and writes
# tests/local/tests/local/fixtures.
#
# @param name File name of the fixture
cache_path <- function(name) {
  dir <- if (dir.exists(file.path("tests", "local"))) {
    file.path("tests", "local", "fixtures")
  } else {
    "fixtures"
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}


# Whether this run should refit a fixture. `MVGAM_REFIT` names the
# fixtures to refit, comma-separated, with or without `.rds`, or
# `all`. Unset, every cached fixture is loaded as it is:
#   MVGAM_REFIT=all Rscript -e "..."
#   MVGAM_REFIT=val_mvgam_var,val_trend_map Rscript -e "..."
#
# A fixture is refitted at most once per run. A file that loads one
# fixture in several tests gets the refitted copy after the first.
#
# @param name File name of the fixture
refit_requested <- function(name) {
  if (name %in% refitted_this_run$names) return(FALSE)
  wanted <- trimws(strsplit(Sys.getenv("MVGAM_REFIT"), ",")[[1L]])
  "all" %in% wanted ||
    sub("\\.rds$", "", name) %in% sub("\\.rds$", "", wanted)
}
refitted_this_run <- new.env()
refitted_this_run$names <- character(0)


# A fit loaded from the fixture cache. It is refitted when the cache is
# absent, was built from other data, or `MVGAM_REFIT` names it. Every
# local file loads its fits through this.
#
# @param name File name of the fixture
# @param build A function of no arguments returning the fit
# @param key Any value identifying the data the fit belongs to, such
#   as the simulated truth. A cached fit carrying another key is stale.
# @return The fit, carrying `key` as its `fixture_key` attribute
cached_fit <- function(name, build, key = NULL) {
  path <- cache_path(name)
  if (file.exists(path) && !refit_requested(name)) {
    fit <- readRDS(path)
    if (identical(attr(fit, "fixture_key"), key)) {
      message("[cache] ", name)
      return(fit)
    }
  }
  message("[fit  ] ", name)
  fit <- build()
  attr(fit, "fixture_key") <- key
  part <- paste0(path, ".part")
  saveRDS(fit, part)
  file.rename(part, path)
  refitted_this_run$names <- c(refitted_this_run$names, name)
  fit
}


# One side's brms model, given the fit's draws. The combined program
# holds both sides' parameters. brms selects some of them by pattern:
# its smooth `s_1_1` also matches the trend's `s_1_1_trend`. The side's
# own names are kept, the trend's without the `_trend` its own model
# does not write. Every other name is replaced by a placeholder, which
# leaves brms the side's parameters alone. `brms:::rename_pars()` is
# internal to brms. It serves here as an oracle and nowhere in the
# package.
#
# @param fit A fitted mvgam object
# @param side `"obs"` or `"trend"`
# @return A brmsfit holding the side's draws under brms's names
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


# The largest gap between each observation predictor mvgam composes and
# brms's own from the same draws. Each predictor's parts also sum to it.
# brms uses a fit's latent values only when given no `newdata`. mvgam
# uses them at each row repeating a fitted row. A frame repeating the
# fitted rows in order goes to brms as no `newdata`.
#
# @param fit A fitted mvgam object
# @param newdata Rows to predict for
# @param re_formula As the prediction methods take it
# @param ids Draw indices
# @return Named numeric vector, one gap per predictor and per sum
predictor_gap <- function(fit, newdata, re_formula = NULL, ids = 1:50) {
  b <- as_brms(fit)
  fitted <- identical(fitted_row_index(newdata, b$data), seq_len(nrow(b$data)))
  keys <- names(response_columns(fit$obs_model$formula))
  several <- length(keys) > 1L
  gaps <- c()
  for (key in keys) {
    resp <- if (several) key
    family <- model_families(fit, resp)
    for (component in c("mu", predicted_dpar_names(
      fit, setdiff(family$dpars, "mu"), resp = resp
    ))) {
      args <- list(fit, newdata = newdata, draw_ids = ids, resp = resp,
                   component = if (component == "mu") "obs" else component,
                   re_formula = re_formula)
      mine <- do.call(extract_component_linpred, args)
      theirs <- brms::posterior_linpred(
        b, newdata = if (!fitted) newdata,
        draw_ids = ids, resp = resp,
        dpar = if (component != "mu") component, re_formula = re_formula
      )
      gaps[paste(key, component)] <- max(abs(unname(mine) -
                                               unname(theirs)))
      parts <- do.call(extract_component_parts, args)
      gaps[paste(key, component, "parts")] <- max(abs(Reduce(`+`, parts) -
                                                        mine))
    }
  }
  gaps
}


# A ggplot is returned whether or not a layer received any data.
# Building the plot forces the layers to resolve, and the row count
# says something was drawn.
#
# @param p A ggplot
# @return The built layers, invisibly
expect_drawn <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  layers <- ggplot2::ggplot_build(p)$data
  testthat::expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  invisible(layers)
}


# `tidy()` and `glance()` held to what they report. Each tidy row is
# a parameter's posterior mean and standard deviation. A fixed effect
# takes the name `fixef()` gives it. That name locates its draws. Each
# glance row counts the responses the likelihood used.
#
# @param fit A fitted mvgam object
# @param observed Number of non-missing responses, or `NULL` to skip
#   the count for a fit with several responses
# @return The tidy table, invisibly
expect_tidy_glance <- function(fit, observed = NULL) {
  td <- tidy(fit)
  dm <- posterior::as_draws_matrix(fit)
  cols <- td$term
  fixed <- td$type %in% c("observation_beta", "trend_beta")
  betas <- grep(mvgam:::MVGAM_PAR_FIXEF_PREFIX, colnames(dm), value = TRUE)
  cols[fixed] <- betas[match(td$term[fixed], mvgam:::fixef_name(betas))]
  testthat::expect_identical(setdiff(cols, colnames(dm)), character(0))
  draws <- dm[, cols, drop = FALSE]
  testthat::expect_equal(td$estimate, unname(colMeans(draws)))
  testthat::expect_equal(td$std.error, unname(apply(draws, 2L, stats::sd)))
  gl <- glance(fit)
  # A multivariate fit has one row per response.
  n_resp <- if (inherits(fit$formula, "mvbrmsformula")) {
    length(mvgam:::response_columns(fit))
  } else {
    1L
  }
  testthat::expect_identical(nrow(gl), n_resp)
  if (!is.null(observed)) {
    testthat::expect_identical(as.integer(gl$nobs), as.integer(observed))
  }
  invisible(td)
}


# `hypothesis()` with `class = NULL` reaches each name `variables()`
# lists. Each estimate it returns is finite.
#
# @param fit A fitted mvgam object
# @param names Parameter names as `variables()` lists them
expect_hypothesis_reaches <- function(fit, names) {
  testthat::expect_gt(length(names), 0L)
  vars <- variables(fit)
  for (nm in names) {
    testthat::expect_true(nm %in% vars, label = nm)
    h <- hypothesis(fit, paste0("`", nm, "` = 0"), class = NULL)
    testthat::expect_true(all(is.finite(h$hypothesis$Estimate)), label = nm)
  }
}


# The range of x a plot draws over, from its built layers.
#
# @param p A ggplot
# @return The finite x range
drawn_x <- function(p) {
  xs <- unlist(lapply(ggplot2::ggplot_build(p)$data,
                      function(l) if ("x" %in% names(l)) l$x))
  xs <- xs[is.finite(xs)]
  testthat::expect_gt(length(xs), 0L)
  range(xs)
}


# The facet labels of a plot, in panel order.
#
# @param p A ggplot
# @return Character vector, empty for an unfaceted plot
panel_order <- function(p) {
  lay <- ggplot2::ggplot_build(p)$layout$layout
  facets <- setdiff(names(lay),
                    c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))
  if (!length(facets)) return(character(0))
  as.character(lay[[facets[1L]]])
}


# Evaluate an expression and collect the warnings it raises.
#
# @param expr Expression to evaluate
# @return List with `value` and the warning messages
with_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = seen)
}


# Ground truth for the stationary distribution of an AR() or VAR()
# trend, written from the definition.
#
# The companion state is `(y_t, ..., y_{t-p+1}[, eps_t])`, block i at
# rows `(i - 1) m + 1` to `i m`. An AR() trend passes the diagonal of
# each coefficient matrix as a vector. A VAR() trend passes the matrix
# itself. A moving-average term adds the block holding `eps_t`, which
# enters `y_{t+1}` through `theta`. The stationary covariance is the Neumann
# sum `Omega = sum_k A^k Q A'^k`. It calls nothing from the package.
# Every assertion taking it as ground truth tests the package against
# the definition.
#
# Each doubling step adds `A^n S A'^n` to the partial sum `S` of the
# first `n` terms and squares `A^n`. Sixty steps cover 2^60 terms. A
# spectral radius of 1 - 1e-9 needs about 10^10 terms to settle, which
# a plain sum cannot reach.
#
# @param Sigma Innovation covariance (m x m)
# @param phi List of coefficient vectors or matrices, one per lag
#   from 1 to p
# @param theta Moving-average coefficient vector or matrix, or NULL
# @return The covariance of the companion state, side `(p + q) m`
neumann_omega <- function(Sigma, phi, theta = NULL) {
  m <- nrow(Sigma)
  p <- length(phi)
  n <- p + if (is.null(theta)) 0L else 1L
  d <- n * m
  blk <- function(i) ((i - 1L) * m + 1L):(i * m)
  coef_matrix <- function(x) if (is.matrix(x)) x else diag(x, nrow = m)
  A <- matrix(0, d, d)
  for (k in seq_len(p)) A[blk(1L), blk(k)] <- coef_matrix(phi[[k]])
  for (i in seq_len(p)[-1L]) A[blk(i), blk(i - 1L)] <- diag(m)
  Q <- matrix(0, d, d)
  Q[blk(1L), blk(1L)] <- Sigma
  if (!is.null(theta)) {
    A[blk(1L), blk(n)] <- coef_matrix(theta)
    Q[blk(n), blk(n)] <- Sigma
    Q[blk(1L), blk(n)] <- Sigma
    Q[blk(n), blk(1L)] <- Sigma
  }
  Omega <- Q
  An <- A
  for (step in 1:60) {
    Omega <- Omega + An %*% Omega %*% t(An)
    An <- An %*% An
  }
  (Omega + t(Omega)) / 2
}


# The parts of the conditional linear predictor sum to it in every draw,
# and the latent parts carry the names given. A model with several
# responses is checked response by response.
#
# @param fit A fitted mvgam object
# @param latent The names of the latent parts, in order
# @param newdata Rows at fitted times
expect_parts_sum <- function(fit, latent, newdata = fit$data) {
  ids <- 1:10
  parts <- get_linpred_parts(fit, newdata, draw_ids = ids)
  whole <- get_combined_linpred(fit, newdata, trend_state = "conditional",
                                draw_ids = ids)
  if (length(response_formulas(fit)) == 1L) {
    parts <- list(parts)
    whole <- list(whole)
  }
  for (i in seq_along(whole)) {
    expect_identical(names(parts[[i]]$latent), latent)
    total <- Reduce(`+`, c(parts[[i]]$obs, parts[[i]]$trend,
                           parts[[i]]$latent))
    # cmdstanr writes each draw to 6 significant digits. The stored
    # `trend` and the `Z lv_trend + mu_trend` built from the stored
    # states then differ by that rounding.
    expect_equal(total, whole[[i]], tolerance = 1e-6)
  }
}


# The response-scale parts take the fit's own expectation. The
# expectation from the whole predictor is posterior_epred()'s from the
# fitted state, an ordinal response's as its expected level. Given
# `mean_fn`, the family's mean as a function of the predictor, each
# part's contribution is that mean of the whole predictor less its mean
# without the part.
#
# @param fit A fitted mvgam object
# @param mean_fn Function of a predictor matrix, or NULL
# @param newdata Rows at fitted times
expect_epred_parts <- function(fit, mean_fn = NULL, newdata = fit$data) {
  ids <- 1:10
  link <- get_linpred_parts(fit, newdata, draw_ids = ids)
  scale <- get_epred_parts(fit, newdata, draw_ids = ids)
  epred <- posterior_epred(fit, newdata = newdata, incl_autocor = TRUE,
                           draw_ids = ids)
  if (length(response_formulas(fit)) == 1L) {
    link <- list(link)
    scale <- list(scale)
    epred <- list(epred)
  }
  for (i in seq_along(epred)) {
    want <- epred[[i]]
    if (length(dim(want)) == 3L) {
      want <- ordinal_category_mean(want)
    }
    # The stored trend and the one rebuilt from the stored states
    # differ by cmdstanr's rounding to 6 significant digits
    expect_equal(scale[[i]]$expected, want, tolerance = 1e-6)
    if (is.null(mean_fn)) {
      next
    }
    whole <- Reduce(`+`, c(link[[i]]$obs, link[[i]]$trend,
                           link[[i]]$latent))
    for (group in c("obs", "trend", "latent")) {
      for (nm in names(link[[i]][[group]])) {
        without <- whole - link[[i]][[group]][[nm]]
        expect_equal(scale[[i]][[group]][[nm]],
                     mean_fn(whole) - mean_fn(without))
      }
    }
  }
}
