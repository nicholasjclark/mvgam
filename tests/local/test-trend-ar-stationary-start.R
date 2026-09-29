# Where each correlated, grouped or moving-average AR() trend starts,
# checked against each posterior draw of six fitted models.
#
# A stationary trend starts at its stationary distribution. The Stan
# program draws the first `p` latent states from it, and the shape of
# the trend decides how:
#
#   AR(p = 2, gr = )             joint covariance of each group's
#                                companion state
#   AR(p, ma = TRUE, cor = TRUE)
#   AR(p, ma = TRUE, gr = )      the filter `x_t + theta x_{t-1}` of an
#   AR(p = 2, ma = TRUE)         AR(p) path `x` drawn at its joint
#                                stationary covariance, at p = 1 and 2
#
# `test-stationary-start-functions.R` checks the Stan code these starts
# emit against the definition on random inputs. What only a fit
# can check is the wiring around them: which innovation rows fill which
# variates, which coefficients reach which series, and in a grouped
# trend, which series make up a group. The member scan that collects a
# group walks the series in ascending order, the same order that fills
# `scaled_innovations_trend`. A scan walked in another order would mix
# two groups' coefficients and keep every dimension right.
#
# Each block below rebuilds `lv_trend[1:p, ]` from the draw's own
# innovations and parameters and requires it to the precision a stored
# draw holds. Ground truth is a Neumann sum over the full companion,
# `neumann_omega()` in helper-local.R.
#
#   truth: 2 regions x 3 species, 70 occasions, gaussian, ARMA(2, 1)
#          latent with innovations correlated within a region
#
# Fits are cached at tests/local/fixtures/val_mvgam_ar_start_*.rds.
# Delete one to refit it.
#
# Run with:
#   testthat::test_file("tests/local/test-trend-ar-stationary-start.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(posterior)
  library(testthat)
})


set.seed(6607L)

region_levels <- c("south", "north")
species_levels <- c("sp_c", "sp_a", "sp_b")
n_sub <- length(species_levels)
n_series <- length(region_levels) * n_sub
n_time <- 70L
time_vals <- seq_len(n_time) + 2L

phi1_true <- c(0.5, 0.2, -0.3, 0.6, 0.1, 0.3)
phi2_true <- c(0.2, 0.3, 0.25, -0.2, 0.35, 0.1)
theta_true <- c(0.4, -0.3, 0.2, 0.5, -0.2, 0.3)
sigma_true <- 0.4
R_region <- matrix(0.5, n_sub, n_sub)
diag(R_region) <- 1
L_region <- t(chol(sigma_true^2 * R_region))

burn <- 300L
nt_sim <- n_time + burn
E <- matrix(0, nt_sim, n_series)
for (g in seq_along(region_levels)) {
  cols <- (g - 1L) * n_sub + seq_len(n_sub)
  E[, cols] <- matrix(rnorm(nt_sim * n_sub), nt_sim, n_sub) %*% t(L_region)
}
lat <- matrix(0, nt_sim, n_series)
for (t in 3:nt_sim) {
  lat[t, ] <- phi1_true * lat[t - 1L, ] + phi2_true * lat[t - 2L, ] +
    E[t, ] + theta_true * E[t - 1L, ]
}
latent <- lat[(burn + 1L):nt_sim, , drop = FALSE]

# Region-major columns, the order the grouping declares.
grid <- expand.grid(
  time = time_vals,
  species = factor(species_levels, levels = species_levels),
  region = factor(region_levels, levels = region_levels)
)
dat_grouped <- data.frame(
  time = grid$time, region = grid$region, species = grid$species,
  y = as.numeric(latent) + rnorm(nrow(grid), 0, 0.15)
)
dat_flat <- dat_grouped
dat_flat$series <- factor(
  paste(dat_flat$region, dat_flat$species, sep = "_"),
  levels = as.vector(t(outer(region_levels, species_levels, paste,
                             sep = "_")))
)


# -- Ground truth, written from the definition ------------------------

# The first `p` states of a set of series, from the standard variates
# the program drew. `z` holds innovation rows 1 to p, one column per
# series. `u` is `init_innovations_trend` and `eps` the scaled
# innovation at t = p, both needed with a moving-average term alone.
# Returns a `p x m` matrix in time order.
#
# The `p` most recent states of the AR(p) path `x` take the lower
# Cholesky factor of their stationary covariance, the one factor with
# a positive diagonal. Block i is the i-th most recent state. Without a
# moving-average term those are `lv_p, ..., lv_1`, from rows 1 to p.
# With one they are `x_{p-1}, ..., x_0`, from rows 1 to p - 1 and `u`,
# and `lv_t = x_t + theta x_{t-1}`, where `x_p` steps from them with
# `eps`.
start_states <- function(Sigma, phi, theta, z, u = NULL, eps = NULL) {
  m <- nrow(Sigma)
  p <- length(phi)
  has_ma <- !is.null(theta)
  zz <- c(as.numeric(t(z[seq_len(p - has_ma), , drop = FALSE])), u)
  x <- matrix(t(chol(neumann_omega(Sigma, phi))) %*% zz, m, p)
  if (!has_ma) {
    return(t(x[, p:1, drop = FALSE]))
  }
  x_p <- eps
  for (k in seq_len(p)) x_p <- x_p + phi[[k]] * x[, k]
  # Columns x_p, x_{p-1}, ..., x_0, most recent first.
  path <- cbind(x_p, x)
  lv <- path[, 1:p, drop = FALSE] + theta * path[, 2:(p + 1L), drop = FALSE]
  t(lv[, p:1, drop = FALSE])
}

vec_at <- function(dm, k, fmt, idx) {
  vapply(idx, function(i) as.numeric(dm[k, sprintf(fmt, i)]), numeric(1))
}

row_at <- function(dm, k, par, t, cols) {
  vapply(cols, function(s) {
    as.numeric(dm[k, sprintf("%s[%d,%d]", par, t, s)])
  }, numeric(1))
}

draw_parts <- function(dm, k, p, has_ma) {
  s <- seq_len(n_series)
  rows <- function(par) {
    t(vapply(seq_len(p), function(t) row_at(dm, k, par, t, s),
             numeric(n_series)))
  }
  list(
    phi = lapply(seq_len(p), function(l) {
      vec_at(dm, k, paste0("ar", l, "_trend[%d]"), s)
    }),
    theta = if (has_ma) vec_at(dm, k, "theta1_trend[%d]", s),
    z = rows("innovations_trend"),
    u = if (has_ma) vec_at(dm, k, "init_innovations_trend[%d]", s),
    eps = if (has_ma) row_at(dm, k, "scaled_innovations_trend", p, s),
    lv = rows("lv_trend")
  )
}

draw_sigma_flat <- function(dm, k) {
  s <- seq_len(n_series)
  sg <- vec_at(dm, k, "sigma_trend[%d]", s)
  Lw <- matrix(0, n_series, n_series)
  for (a in s) for (b in s) {
    Lw[a, b] <- as.numeric(dm[k, sprintf("L_Omega_trend[%d,%d]", a, b)])
  }
  diag(sg) %*% tcrossprod(Lw) %*% diag(sg)
}

draw_sigma_group <- function(dm, k, g) {
  S <- matrix(0, n_sub, n_sub)
  for (a in seq_len(n_sub)) for (b in seq_len(n_sub)) {
    S[a, b] <- as.numeric(
      dm[k, sprintf("Sigma_group_trend[%d,%d,%d]", g, a, b)]
    )
  }
  S
}

# The start each branch claims, rebuilt for every series of one draw.
# `groups` is `NULL` for a flat trend, and otherwise the member series
# of each group in the order the rebuild takes them.
rebuild <- function(dm, k, sh, groups = NULL) {
  d <- draw_parts(dm, k, sh$p, sh$ma)
  sub <- function(cols, Sigma) {
    start_states(Sigma, lapply(d$phi, `[`, cols), d$theta[cols],
                 d$z[, cols, drop = FALSE], d$u[cols], d$eps[cols])
  }
  want <- matrix(NA_real_, sh$p, n_series)
  if (identical(sh$form, "diagonal")) {
    sg <- vec_at(dm, k, "sigma_trend[%d]", seq_len(n_series))
    for (s in seq_len(n_series)) want[, s] <- sub(s, matrix(sg[s]^2, 1, 1))
  } else if (identical(sh$form, "correlated")) {
    want[, ] <- sub(seq_len(n_series), draw_sigma_flat(dm, k))
  } else {
    for (g in seq_along(groups)) {
      want[, groups[[g]]] <- sub(groups[[g]], draw_sigma_group(dm, k, g))
    }
  }
  list(want = want, got = d$lv)
}


# -- Fits -------------------------------------------------------------

shapes <- list(
  ar2_gr = list(rhs = ~ AR(p = 2, gr = region, subgr = species),
                p = 2L, ma = FALSE, form = "grouped"),
  ma1_cor = list(rhs = ~ AR(p = 1, ma = TRUE, cor = TRUE),
                 p = 1L, ma = TRUE, form = "correlated"),
  ma1_gr = list(rhs = ~ AR(p = 1, ma = TRUE, gr = region,
                           subgr = species),
                p = 1L, ma = TRUE, form = "grouped"),
  ma2 = list(rhs = ~ AR(p = 2, ma = TRUE),
             p = 2L, ma = TRUE, form = "diagonal"),
  ma2_cor = list(rhs = ~ AR(p = 2, ma = TRUE, cor = TRUE),
                 p = 2L, ma = TRUE, form = "correlated"),
  ma2_gr = list(rhs = ~ AR(p = 2, ma = TRUE, gr = region,
                           subgr = species),
                p = 2L, ma = TRUE, form = "grouped")
)

fit_shape <- function(name) {
  sh <- shapes[[name]]
  dat <- if (identical(sh$form, "grouped")) dat_grouped else dat_flat
  fit <- cached_fit(sprintf("val_mvgam_ar_start_%s.rds", name), function() {
    mvgam(y ~ 1, trend_formula = sh$rhs, data = dat,
          family = gaussian(), chains = 2L, iter = 600L,
          warmup = 300L, silent = 2, backend = "cmdstanr")
  })
  list(fit = fit, dm = posterior::as_draws_matrix(fit$fit))
}

fits <- lapply(stats::setNames(names(shapes), names(shapes)), fit_shape)

# The member series of each group, ascending: the order the program's
# scan takes them in.
group_members <- function(fit) {
  inds <- as.integer(fit$standata$group_inds_trend)
  lapply(sort(unique(inds)), function(g) which(inds == g))
}

check_draws <- function(dm) {
  unique(round(seq(1, nrow(dm), length.out = 15L)))
}



test_that("each start draws its first states from the stationary law", {
  for (nm in names(shapes)) {
    sh <- shapes[[nm]]
    fx <- fits[[nm]]
    groups <- if (identical(sh$form, "grouped")) group_members(fx$fit)
    for (k in check_draws(fx$dm)) {
      r <- rebuild(fx$dm, k, sh, groups)
      expect_equal(r$got, r$want, tolerance = 1e-5)
    }
  }
})


test_that("a grouped start takes each group's series in ascending order", {
  # The gate that shows the grouped rebuilds above can fail. Taking a
  # group's members in descending order hands each position another
  # series' coefficients and variates, with every dimension intact.
  for (nm in c("ar2_gr", "ma1_gr", "ma2_gr")) {
    fx <- fits[[nm]]
    reversed <- lapply(group_members(fx$fit), rev)
    r <- rebuild(fx$dm, 1L, shapes[[nm]], reversed)
    expect_gt(max(abs(r$got - r$want)), 1e-2)
  }
})


test_that("the marginal covariance is each group's leading block", {
  # `stationary_group_params()` takes its coefficients through the same
  # ascending scan the program uses. Under a moving-average term the
  # leading block is where the state settles.
  for (nm in c("ar2_gr", "ma1_gr", "ma2_gr")) {
    sh <- shapes[[nm]]
    fx <- fits[[nm]]
    groups <- group_members(fx$fit)
    ids <- seq_len(10L)
    cs <- get_trend_covariance_structure(fx$fit, draw_ids = ids)
    for (k in ids) {
      d <- draw_parts(fx$dm, k, sh$p, sh$ma)
      for (g in seq_along(groups)) {
        cols <- groups[[g]]
        want <- neumann_omega(
          draw_sigma_group(fx$dm, k, g), lapply(d$phi, `[`, cols),
          d$theta[cols]
        )[seq_len(n_sub), seq_len(n_sub)]
        L <- matrix(cs$params$L_group_stationary[k, g, , ], n_sub, n_sub)
        expect_equal(tcrossprod(L), want, tolerance = 1e-5)
      }
    }
  }
})


test_that("the marginal covariance is the correlated start's block", {
  for (nm in c("ma1_cor", "ma2_cor")) {
    sh <- shapes[[nm]]
    fx <- fits[[nm]]
    ids <- seq_len(10L)
    cs <- get_trend_covariance_structure(fx$fit, draw_ids = ids)
    for (k in ids) {
      d <- draw_parts(fx$dm, k, sh$p, sh$ma)
      want <- neumann_omega(draw_sigma_flat(fx$dm, k), d$phi, d$theta)
      want <- want[seq_len(n_series), seq_len(n_series)]
      s_k <- cs$params$sigma_trend[k, ]
      L_k <- matrix(cs$params$L_Omega_trend[k, , ], n_series, n_series)
      expect_equal(diag(s_k) %*% tcrossprod(L_k) %*% diag(s_k), want,
                   tolerance = 1e-5)
    }
  }
})


# -- What a user calls next --------------------------------------------

series_levels <- levels(dat_flat$series)
n_steps <- 3L
last_time <- length(time_vals)

future_frame <- function(sh) {
  g <- expand.grid(
    time = max(time_vals) + seq_len(n_steps),
    species = factor(species_levels, levels = species_levels),
    region = factor(region_levels, levels = region_levels)
  )
  out <- data.frame(time = g$time, region = g$region, species = g$species,
                    y = NA_real_)
  if (!identical(sh$form, "grouped")) {
    out$series <- factor(paste(out$region, out$species, sep = "_"),
                         levels = series_levels)
  }
  out
}


test_that("the hindcast names each trend column after its series", {
  # `lv_trend[, s]` is the state of the s-th series in the order the
  # frame declares. A hindcast keyed by name that took another column
  # would keep every dimension right.
  for (nm in names(fits)) {
    fx <- fits[[nm]]
    hc <- hindcast(fx$fit, type = "trend")
    expect_identical(names(hc$hindcasts), series_levels)
    for (s in seq_len(n_series)) {
      lv <- as.numeric(fx$dm[, sprintf("lv_trend[%d,%d]", last_time, s)])
      expect_equal(hc$hindcasts[[series_levels[s]]][, last_time], lv)
    }
    # `draw_ids` takes the rows asked for, in the order asked. A
    # caller pairing the hindcast with another method's draws relies
    # on both.
    ids <- c(250L, 7L, 91L)
    hc_ids <- hindcast(fx$fit, type = "trend", draw_ids = ids)
    for (s in seq_len(n_series)) {
      expect_identical(hc_ids$hindcasts[[series_levels[s]]],
                       hc$hindcasts[[series_levels[s]]][ids, ,
                                                        drop = FALSE])
    }
  }
})


test_that("the one-step forecast continues the fitted state", {
  # Each forecast draw steps one posterior draw forward from its own
  # final states and adds a fresh innovation. Less that draw's
  # conditional mean,
  #   sum_l ar_l * lv[T + 1 - l] + theta1 * eps[T],
  # what remains is the innovation alone, which centres on zero across
  # the draws. A forecast that seeded `eps[T]` with zero leaves
  # `theta1 * eps[T]` in the remainder, 0.13 on one series here.
  for (nm in names(fits)) {
    sh <- shapes[[nm]]
    fx <- fits[[nm]]
    fc <- forecast(fx$fit, newdata = future_frame(sh), type = "trend")
    expect_identical(names(fc$forecasts), series_levels)
    for (s in seq_len(n_series)) {
      at <- function(par, t) {
        as.numeric(fx$dm[, sprintf("%s[%d,%d]", par, t, s)])
      }
      coef <- function(par) as.numeric(fx$dm[, sprintf("%s[%d]", par, s)])
      cond <- 0
      for (l in seq_len(sh$p)) {
        cond <- cond +
          coef(paste0("ar", l, "_trend")) * at("lv_trend", last_time + 1L - l)
      }
      if (sh$ma) {
        cond <- cond +
          coef("theta1_trend") * at("scaled_innovations_trend", last_time)
      }
      nm_s <- series_levels[s]
      expect_identical(as.integer(fc$test_times[[nm_s]]),
                       as.integer(max(time_vals) + seq_len(n_steps)))
      resid <- fc$forecasts[[nm_s]][, 1L] - cond
      expect_lt(abs(mean(resid)), 4 * stats::sd(resid) / sqrt(length(resid)))
    }
  }
})


test_that("every prediction and summary surface returns on each fit", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  labels <- c(ar2_gr = "AR(2)", ma1_cor = "ARMA(1, 1)",
              ma1_gr = "ARMA(1, 1)", ma2 = "ARMA(2, 1)",
              ma2_cor = "ARMA(2, 1)", ma2_gr = "ARMA(2, 1)")
  n_obs <- nrow(dat_flat)
  for (nm in names(fits)) {
    fx <- fits[[nm]]
    ep <- posterior_epred(fx$fit, draw_ids = 1:20)
    expect_identical(dim(ep), c(20L, n_obs))
    expect_true(all(is.finite(ep)))
    # The marginal envelope integrates over the stationary covariance
    # each shape lifts to, the path the covariance blocks above check.
    marg <- posterior_predict(fx$fit, draw_ids = 1:20,
                              incl_autocor = FALSE, process_error = TRUE)
    expect_identical(dim(marg), c(20L, n_obs))
    expect_true(all(is.finite(marg)))
    expect_identical(nrow(fitted(fx$fit, draw_ids = 1:20)), n_obs)
    expect_identical(nrow(residuals(fx$fit, draw_ids = 1:20)), n_obs)
    expect_s3_class(summary(fx$fit), "mvgam_summary")
    txt <- utils::capture.output(print(fx$fit))
    i <- grep("^Trend model", txt)
    expect_true(startsWith(trimws(txt[i + 1L]), labels[[nm]]))
    # Independent innovations leave no correlation to summarise. A
    # grouping correlates the species within a region, and the summary
    # is keyed by species.
    form <- shapes[[nm]]$form
    if (identical(form, "diagonal")) {
      expect_error(residual_cor(fx$fit))
    } else {
      rc <- residual_cor(fx$fit)
      keys <- if (identical(form, "grouped")) species_levels else series_levels
      expect_identical(dimnames(rc$cor), list(keys, keys))
      expect_true(all(is.finite(rc$cor)))
    }
    for (ty in c("trend", "series", "residuals")) {
      expect_s3_class(plot(fx$fit, type = ty), "ggplot")
    }
    expect_s3_class(pp_check(fx$fit, ndraws = 10L), "ggplot")
  }
})


cat("\nDone.\n")
