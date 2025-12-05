# prepare predictions for Gaussian processes
# @param new is new data used?
# @param nug small numeric value to avoid numerical problems in GPs
prepare_predictions_gp <- function(bframe, draws, sdata, new = FALSE,
                                   nug = NULL, ...) {
  stopifnot(is.bframel(bframe))
  gpframe <- bframe$frame$gp
  if (!has_rows(gpframe)) {
    return(list())
  }
  p <- usc(combine_prefix(bframe))
  if (is.null(nug)) {
    # nug for old data must be the same as in the Stan code as even tiny
    # differences (e.g., 1e-12 vs. 1e-11) will matter for larger lscales
    nug <- ifelse(new, 1e-8, 1e-12)
  }
  out <- named_list(gpframe$label)
  for (i in seq_along(out)) {
    cons <- gpframe$cons[[i]]
    if (length(cons)) {
      gp <- named_list(cons)
      for (j in seq_along(cons)) {
        gp[[j]] <- .prepare_predictions_gp(
          gpframe, draws = draws, sdata = sdata,
          nug = nug, new = new, byj = j, p = p, i = i
        )
      }
      attr(gp, "byfac") <- TRUE
    } else {
      gp <- .prepare_predictions_gp(
        gpframe, draws = draws, sdata = sdata,
        nug = nug, new = new, p = p, i = i
      )
    }
    out[[i]] <- gp
  }
  out
}

# prepare predictions for Gaussian processes
# @param gpframe output of frame_gp
# @param p prefix created by combine_prefix()
# @param i index of the Gaussian process
# @param byj index for the contrast of a categorical 'by' variable
# @return a list to be evaluated by .predictor_gp()
.prepare_predictions_gp <- function(gpframe, draws, sdata, nug,
                                    new, p, i, byj = NULL) {
  sfx1 <- escape_all(gpframe$sfx1[[i]])
  sfx2 <- escape_all(gpframe$sfx2[[i]])
  if (is.null(byj)) {
    lvl <- ""
  } else {
    lvl <- gpframe$bylevels[[i]][byj]
    sfx1 <- sfx1[byj]
    sfx2 <- sfx2[byj, ]
  }
  j <- usc(byj)
  pi <- paste0(p, "_", i)
  gp <- list()
  gp$cov <- gpframe$cov[i]
  sdgp <- paste0("^sdgp", p, "_", sfx1, "$")
  gp$sdgp <- as.vector(prepare_draws(draws, sdgp, regex = TRUE))
  lscale <- paste0("^lscale", p, "_", sfx2, "$")
  gp$lscale <- prepare_draws(draws, lscale, regex = TRUE)
  zgp_regex <- paste0("^zgp", p, "_", sfx1, "\\[")
  gp$zgp <- prepare_draws(draws, zgp_regex, regex = TRUE)
  Xgp_name <- paste0("Xgp", pi, j)
  Igp_name <- paste0("Igp", pi, j)
  Jgp_name <- paste0("Jgp", pi, j)
  if (new && isNA(gpframe$k[i])) {
    # in exact GPs old covariate values are required for predictions
    gp$x <- sdata[[paste0(Xgp_name, "_old")]]
    # nug for old data must be the same as in the Stan code as even tiny
    # differences (e.g., 1e-12 vs. 1e-11) will matter for larger lscales
    gp$nug <- 1e-12
    # computing GPs for new data requires the old GP terms
    gp$yL <- .predictor_gp(gp)
    gp$x_new <- sdata[[Xgp_name]]
    gp$Igp <- sdata[[Igp_name]]
  } else {
    gp$x <- sdata[[Xgp_name]]
    gp$Igp <- sdata[[Igp_name]]
    if (!isNA(gpframe$k[i])) {
      gp$slambda <- sdata[[paste0("slambda", pi, j)]]
    }
  }
  gp$Jgp <- sdata[[Jgp_name]]
  # possible factor from 'by' variable
  gp$Cgp <- sdata[[paste0("Cgp", pi, j)]]
  gp$nug <- nug
  gp
}

# workhorse function of predictor_gp
# @param gp a list returned by '.prepare_predictions_gp'
# @return A S x N matrix to be added to the linear predictor
# @note does not work with pointwise evaluation
.predictor_gp <- function(gp) {
  if (is.null(gp[["slambda"]])) {
    # predictions for exact GPs
    ndraws <- length(gp[["sdgp"]])
    eta <- as.list(rep(NA, ndraws))
    if (!is.null(gp[["x_new"]])) {
      for (i in seq_along(eta)) {
        eta[[i]] <- with(gp, .predictor_gp_new(
          x_new = x_new, yL = yL[i, ], x = x,
          sdgp = sdgp[i], lscale = lscale[i, ],
          cov = cov, nug = nug
        ))
      }
    } else {
      for (i in seq_along(eta)) {
        eta[[i]] <- with(gp, .predictor_gp_old(
          x = x, sdgp = sdgp[i], lscale = lscale[i, ],
          zgp = zgp[i, ], cov = cov, nug = nug
        ))
      }
    }
    eta <- do_call(rbind, eta)
  } else {
    # predictions for approximate GPs
    eta <- with(gp, .predictor_gpa(
      x = x, sdgp = sdgp, lscale = lscale,
      zgp = zgp, slambda = slambda, cov = cov
    ))
  }
  if (!is.null(gp[["Jgp"]])) {
    eta <- eta[, gp[["Jgp"]], drop = FALSE]
  }
  if (!is.null(gp[["Cgp"]])) {
    eta <- eta * data2draws(gp[["Cgp"]], dim = dim(eta))
  }
  eta
}

# make exact GP predictions for old data points
# vectorized over posterior draws
# @param x old predictor values
# @param sdgp sample of parameter sdgp
# @param lscale sample of parameter lscale
# @param zgp draws of parameter vector zgp
# @param nug very small positive value to ensure numerical stability
.predictor_gp_old <- function(x, sdgp, lscale, zgp, cov, nug) {
  Sigma <- cov_gp(x, sdgp = sdgp, lscale = lscale, cov = cov)
  lx <- nrow(x)
  Sigma <- Sigma + diag(rep(nug, lx), lx, lx)
  L_Sigma <- try_nug(t(chol(Sigma)), nug = nug)
  as.numeric(L_Sigma %*% zgp)
}

# make exact GP predictions for new data points
# vectorized over posterior draws
# @param x_new new predictor values
# @param yL linear predictor of the old data
# @param x old predictor values
# @param sdgp sample of parameter sdgp
# @param lscale sample of parameter lscale
# @param nug very small positive value to ensure numerical stability
.predictor_gp_new <- function(x_new, yL, x, sdgp, lscale, cov, nug) {
  Sigma <- cov_gp(x, sdgp = sdgp, lscale = lscale, cov = cov)
  lx <- nrow(x)
  lx_new <- nrow(x_new)
  Sigma <- Sigma + diag(rep(nug, lx), lx, lx)
  L_Sigma <- try_nug(t(chol(Sigma)), nug = nug)
  L_Sigma_inverse <- solve(L_Sigma)
  K_div_yL <- L_Sigma_inverse %*% yL
  K_div_yL <- t(t(K_div_yL) %*% L_Sigma_inverse)
  k_x_x_new <- cov_gp(x, x_new, sdgp = sdgp, lscale = lscale, cov = cov)
  mu_yL_new <- as.numeric(t(k_x_x_new) %*% K_div_yL)
  v_new <- L_Sigma_inverse %*% k_x_x_new
  cov_yL_new <- cov_gp(x_new, sdgp = sdgp, lscale = lscale, cov = cov) -
    t(v_new) %*% v_new + diag(rep(nug, lx_new), lx_new, lx_new)
  yL_new <- try_nug(
    rmulti_normal(1, mu = mu_yL_new, Sigma = cov_yL_new),
    nug = nug
  )
  return(yL_new)
}

# make predictions for approximate GPs
# vectorized over posterior draws
# @param x matrix of evaluated eigenfunctions of the cov matrix
# @param sdgp sample of parameter sdgp
# @param lscale sample of parameter lscale
# @param zgp draws of parameter vector zgp
# @param slambda vector of eigenvalues of the cov matrix
# @note no need to differentiate between old and new data points
.predictor_gpa <- function(x, sdgp, lscale, zgp, slambda, cov) {
  spd <- sqrt(spd_gp(slambda, sdgp = sdgp, lscale = lscale, cov = cov))
  (spd * zgp) %*% t(x)
}

# spectral density function for approximate Gaussian processes
# vectorized over parameter values
spd_gp <- function(x, sdgp = 1, lscale = 1, cov = "exp_quad") {
  spd_fun <- paste0("spd_gp_", cov)
  spd_fun <- get(spd_fun, asNamespace("brms"))
  spd_fun(x, sdgp = sdgp, lscale = lscale)
}

# spectral density function of the squared exponential kernel
# vectorized over parameter values
spd_gp_exp_quad <- function(x, sdgp = 1, lscale = 1) {
  NB <- NROW(x)
  D <- NCOL(x)
  Dls <- NCOL(lscale)
  constant <- sdgp^2 * sqrt(2 * pi)^D
  out <- matrix(nrow = length(sdgp), ncol = NB)
  if (Dls == 1L) {
    # one dimensional or isotropic GP
    constant <- constant * lscale^D
    neg_half_lscale2 <- -0.5 * lscale^2
    for (m in seq_len(NB)) {
      out[, m] <- constant * exp(neg_half_lscale2 * sum(x[m, ]^2))
    }
  } else {
    # multi-dimensional non-isotropic GP
    constant <- constant * matrixStats::rowProds(lscale)
    neg_half_lscale2 = -0.5 * lscale^2
    for (m in seq_len(NB)) {
      x2 <- data2draws(x[m, ]^2, dim = dim(lscale))
      out[, m] <- constant * exp(rowSums(neg_half_lscale2 * x2))
    }
  }
  out
}

# spectral density function of the exponential kernel
# vectorized over parameter values
spd_gp_exponential <- function(x, sdgp = 1, lscale = 1) {
  NB <- NROW(x)
  D <- NCOL(x)
  Dls <- NCOL(lscale)
  constant = square(sdgp) *
    (2^D * pi^(D / 2) * gamma((D + 1) / 2)) / sqrt(pi)
  expo = -(D + 1) / 2
  lscale2 <- lscale^2
  out <- matrix(nrow = length(sdgp), ncol = NB)
  if (Dls == 1L) {
    # one dimensional or isotropic GP
    constant <- constant * lscale^D
    for (m in seq_len(NB)) {
      out[, m] <- constant * (1 + lscale2 * sum(x[m, ]^2))^expo;
    }
  } else {
    # multi-dimensional non-isotropic GP
    constant <- constant * matrixStats::rowProds(lscale)
    for (m in seq_len(NB)) {
      x2 <- data2draws(x[m, ]^2, dim = dim(lscale))
      out[, m] <- constant * (1 + rowSums(lscale2 * x2))^expo
    }
  }
  out
}

# spectral density function of the Matern 3/2 kernel
# vectorized over parameter values
spd_gp_matern32 <- function(x, sdgp = 1, lscale = 1) {
  NB <- NROW(x)
  D <- NCOL(x)
  Dls <- NCOL(lscale)
  constant = square(sdgp) *
    (2^D * pi^(D / 2) * gamma((D + 3) / 2) * 3^(3 / 2)) / (0.5 * sqrt(pi))
  expo = -(D + 3) / 2
  lscale2 <- lscale^2
  out <- matrix(nrow = length(sdgp), ncol = NB)
  if (Dls == 1L) {
    # one dimensional or isotropic GP
    constant <- constant * lscale^D
    for (m in seq_len(NB)) {
      out[, m] <- constant * (3 + lscale2 * sum(x[m, ]^2))^expo;
    }
  } else {
    # multi-dimensional non-isotropic GP
    constant <- constant * matrixStats::rowProds(lscale)
    for (m in seq_len(NB)) {
      x2 <- data2draws(x[m, ]^2, dim = dim(lscale))
      out[, m] <- constant * (3 + rowSums(lscale2 * x2))^expo
    }
  }
  out
}

# spectral density function of the Matern 5/2 kernel
# vectorized over parameter values
spd_gp_matern52 <- function(x, sdgp = 1, lscale = 1) {
  NB <- NROW(x)
  D <- NCOL(x)
  Dls <- NCOL(lscale)
  constant = square(sdgp) *
    (2^D * pi^(D / 2) * gamma((D + 5) / 2) * 5^(5 / 2)) / (0.75 * sqrt(pi))
  expo = -(D + 5) / 2
  lscale2 <- lscale^2
  out <- matrix(nrow = length(sdgp), ncol = NB)
  if (Dls == 1L) {
    # one dimensional or isotropic GP
    constant <- constant * lscale^D
    for (m in seq_len(NB)) {
      out[, m] <- constant * (5 + lscale2 * sum(x[m, ]^2))^expo;
    }
  } else {
    # multi-dimensional non-isotropic GP
    constant <- constant * matrixStats::rowProds(lscale)
    for (m in seq_len(NB)) {
      x2 <- data2draws(x[m, ]^2, dim = dim(lscale))
      out[, m] <- constant * (5 + rowSums(lscale2 * x2))^expo
    }
  }
  out
}

# compute the mth eigen value of an approximate GP
eigen_val_laplacian <- function(m, L) {
  ((m * pi) / (2 * L))^2
}

# compute the mth eigen function of an approximate GP
eigen_fun_laplacian <- function(x, m, L) {
  x <- as.matrix(x)
  D <- ncol(x)
  stopifnot(length(m) == D, length(L) == D)
  out <- vector("list", D)
  for (i in seq_cols(x)) {
    out[[i]] <- 1 / sqrt(L[i]) *
      sin((m[i] * pi) / (2 * L[i]) * (x[, i] + L[i]))
  }
  Reduce("*", out)
}

# extended range of input data for which predictions should be made
choose_L <- function(x, c) {
  if (!length(x)) {
    range <- 1
  } else {
    range <- max(1, max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  c * range
}
