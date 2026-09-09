# Post-fit handling for the brms addition terms that change what a
# per-observation likelihood contribution means: `weights()`, `cens()`
# and `trunc()`.
#
# Each is applied to the `[ndraws x nobs]` matrix that
# `dispatch_log_lik()` returns, so `loo()`, `waic()`, `lfo_cv()` and
# `kfold()` inherit the corrected contributions without knowing the
# terms exist.
#
# Censoring and truncation need the family's distribution function
# rather than its density. Both read the same parameterisation the
# density does, so `family_dist_spec()` names the R distribution and
# builds its arguments once and the two callers share it.


# Internal: a beta's two shapes from a mean and a precision, in the
# parameterisation brms uses.
#
# Four callers share it: the univariate beta family, the
# beta-binomial's mixing law, one component of a Dirichlet, and the
# simulator that draws a beta response.
#
# `boundary_na` is for the one caller whose spec is read only as a
# distribution function. At a mean of zero or one a shape reaches
# zero and the transform has no interior: every value returns a
# boundary, which `qnorm` reports as an eight-sigma residual rather
# than as the undefined quantity it is. A density and a draw are
# both well defined there -- degenerate, but not undefined -- so
# they keep the shapes they were given.
#' @noRd
beta_shapes <- function(mu, phi, boundary_na = FALSE) {
  if (boundary_na) {
    mu[!is.na(mu) & (mu <= 0 | mu >= 1)] <- NA_real_
  }
  list(shape1 = mu * phi, shape2 = (1 - mu) * phi)
}


# Internal: the R distribution a family maps onto, with a builder that
# returns that distribution's arguments for one column of draws.
#
# Returns NULL for a family with no single R distribution, which is
# what makes `cens()` and `trunc()` unavailable for it.
#' @noRd
family_dist_spec <- function(family_name, link, linpred, family_pars,
                             trials) {
  mu <- .linkinv(linpred, link)
  sigma <- family_pars$sigma
  shape <- family_pars$shape
  phi <- family_pars$phi
  nu <- family_pars$nu
  trials <- if (is.null(trials)) NULL else as.integer(trials)

  spec <- function(dist, args) list(dist = dist, args = args)

  switch(family_name,
    gaussian = spec("norm", function(j) {
      list(mean = mu[, j], sd = sigma[, j])
    }),
    # A location-scale Student t is standardised before `pt`, so its
    # bound has to be standardised the same way.
    student = spec("t", function(j) {
      list(df = nu[, j], .shift = mu[, j], .scale = sigma[, j])
    }),
    lognormal = spec("lnorm", function(j) {
      list(meanlog = linpred[, j], sdlog = sigma[, j])
    }),
    gamma = spec("gamma", function(j) {
      list(shape = shape[, j], rate = shape[, j] / mu[, j])
    }),
    weibull = spec("weibull", function(j) {
      list(shape = shape[, j],
           scale = mu[, j] / gamma(1 + 1 / shape[, j]))
    }),
    exponential = spec("exp", function(j) {
      list(rate = 1 / mu[, j])
    }),
    beta = spec("beta", function(j) {
      beta_shapes(mu[, j], phi[, j])
    }),
    bernoulli = spec("binom", function(j) {
      list(size = 1L, prob = mu[, j])
    }),
    binomial = spec("binom", function(j) {
      list(size = trials[j], prob = mu[, j])
    }),
    beta_binomial = spec("bbinom", function(j) {
      shapes <- beta_shapes(mu[, j], phi[, j])
      list(size = trials[j],
           alpha = shapes$shape1, beta = shapes$shape2)
    }),
    poisson = spec("pois", function(j) {
      list(lambda = mu[, j])
    }),
    negbinomial = spec("nbinom", function(j) {
      list(mu = mu[, j], size = shape[, j])
    }),
    geometric = spec("nbinom", function(j) {
      list(mu = mu[, j], size = 1)
    }),
    com_binomial = spec("cmb", function(j) {
      list(mu = mu[, j], nu = nu[, j], size = trials[j])
    }),
    # `mvn` and `mvt` are written row-wise: conditional on the latent
    # state a row is normal or location-scale Student t, which is the
    # density `log_lik()` evaluates, so the distribution function is
    # that row's own.
    mvn = spec("norm", function(j) {
      list(mean = mu[, j], sd = family_pars$Psi_row[, j])
    }),
    mvt = spec("t", function(j) {
      list(df = family_pars$nu, .shift = mu[, j],
           .scale = family_pars$Psi_row[, j])
    }),
    # A Dirichlet is a joint over the rows of a unit, and the
    # marginal of one component is Beta(alpha_j, alpha_0 - alpha_j).
    # The softmax makes a unit's probabilities sum to one and `phi`
    # is constant across it, so alpha_0 is `phi` and the row's mean
    # is its softmax probability: the beta family's own two
    # arguments, read at the row grain.
    #
    # Being a marginal, it answers a per-row question such as a
    # residual exactly and says nothing about what bounding one row
    # does to the density of the unit that row belongs to. Only the
    # residual asks: `cens()` and `trunc()` reach the univariate
    # path alone, since a closure-unit fit refuses an addition term
    # before one is parsed.
    diri = spec("beta", function(j) {
      beta_shapes(family_pars$prob_row[, j], family_pars$phi[, j],
                  boundary_na = TRUE)
    }),
    # A Tweedie puts a point mass at zero and is continuous above it,
    # so the spec records where that mass sits; a caller forming a
    # probability-integral transform needs to know the interval an
    # observation of zero occupies.
    tweedie = c(
      spec("tweedie", function(j) {
        list(power = family_pars$mtheta[, j], mu = mu[, j],
             phi = family_pars$mphi[, j])
      }),
      list(atom = 0)
    ),
    NULL
  )
}


# Internal: the density or distribution function a spec's
# distribution is served by. Most are base R's; `bbinom` comes from
# extraDistr and `cmb` from the package's own COM-binomial kernels,
# so the lookup is written once rather than branched in both callers.
#' @noRd
dist_fun <- function(dist, kind = c("d", "p", "q")) {
  kind <- match.arg(kind)
  switch(
    dist,
    bbinom = {
      insight::check_if_installed(
        "extraDistr",
        reason = paste0(
          "to compute censored or truncated beta_binomial ",
          "likelihoods"
        )
      )
      switch(kind, d = extraDistr::dbbinom, p = extraDistr::pbbinom,
             q = extraDistr::qbbinom)
    },
    # A COM-binomial and a Tweedie have no quantile function here, so
    # a caller asking for one is told rather than handed a base R
    # function that does not exist.
    cmb = switch(kind, d = dcmb, p = pcmb, q = NULL),
    tweedie = switch(kind, d = dtweedie_cpg, p = ptweedie_cpg,
                     q = NULL),
    get(paste0(kind, dist), mode = "function",
        envir = asNamespace("stats"))
  )
}


# Internal: whether a family is one `family_dist_spec()` names.
#
# Asked before a predictor is computed, so a family with no spec does
# not pay for a prediction only the spec would use. It answers by
# building the spec against an empty predictor rather than by keeping
# a second list of family names beside the switch, which would be the
# duplication the spec exists to remove.
#' @noRd
family_has_dist_spec <- function(family_name, link) {
  !is.null(
    family_dist_spec(family_name, link, matrix(0), list(), NULL)
  )
}


# Internal: a location-scale family is evaluated on its standardised
# variate, because the base distribution takes neither a location nor
# a scale. Standardises the value, drops those two entries from the
# argument list, and returns the scale a density needs to correct by
# and a sampler needs to undo.
#' @noRd
standardise_for_dist <- function(value, args) {
  scale <- args$.scale
  if (!is.null(args$.shift)) {
    value <- (value - args$.shift) / scale
    args$.shift <- NULL
    args$.scale <- NULL
  }
  list(value = value, args = args, scale = scale)
}


# Internal: `P(Y <= q)` for every draw and observation, as an
# `[ndraws x nobs]` matrix.
#
# `lower.tail` and `log.p` are handed to the distribution function
# rather than applied afterwards, which is what keeps a far-tail
# survival probability accurate.
#' @noRd
dist_cdf <- function(spec, linpred, q, lower.tail = TRUE,
                     log.p = FALSE) {
  n <- ncol(linpred)
  q <- if (length(q) == 1L) rep(q, n) else q
  pfun <- dist_fun(spec$dist, "p")
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = n)
  for (j in seq_len(n)) {
    std <- standardise_for_dist(q[j], spec$args(j))
    out[, j] <- do.call(
      pfun,
      c(list(std$value), std$args,
        list(lower.tail = lower.tail, log.p = log.p))
    )
  }
  out
}


# Internal: draws from a family restricted to `[lb, ub]`, by the
# inverse-transform method: a uniform on `[F(lb), F(ub)]` mapped back
# through the quantile function lands inside the bounds by
# construction. One pass and exact, where rejecting and redrawing
# costs a draw per attempt and still leaves whatever it could not
# place, which then has to be clamped onto the bound and shows up as
# a spike of mass sitting on it.
#
# `cells` is a logical matrix marking which of the `[ndraws x nobs]`
# cells to redraw. A discrete family opens the interval at
# `F(lb - 1)` so that `lb` itself remains attainable. Returns NULL
# when the family has no quantile function, which is what sends the
# caller to rejection sampling.
#' @noRd
truncated_dist_draws <- function(spec, cells, lb, ub, discrete) {
  qfun <- dist_fun(spec$dist, "q")
  if (is.null(qfun)) return(NULL)
  pfun <- dist_fun(spec$dist, "p")
  out <- matrix(NA_real_, nrow = nrow(cells), ncol = ncol(cells))
  for (j in seq_len(ncol(cells))) {
    rows <- which(cells[, j])
    if (!length(rows)) next
    args <- spec$args(j)
    shift <- args$.shift
    scale <- args$.scale
    args$.shift <- NULL
    args$.scale <- NULL
    args <- lapply(args, function(v) {
      if (length(v) <= 1L) v else v[rows]
    })
    bound <- function(x) {
      if (is.null(shift)) x else (x - shift[rows]) / scale[rows]
    }
    lo <- if (discrete) lb[j] - 1 else lb[j]
    p_lo <- do.call(pfun, c(list(bound(lo)), args))
    p_hi <- do.call(pfun, c(list(bound(ub[j])), args))
    u <- stats::runif(length(rows), pmin(p_lo, p_hi), pmax(p_lo, p_hi))
    z <- do.call(qfun, c(list(u), args))
    out[rows, j] <- if (is.null(shift)) {
      z
    } else {
      z * scale[rows] + shift[rows]
    }
  }
  out
}


# Internal: log density from the same spec the CDF reads, so a family
# is parameterised in exactly one place.
#' @noRd
dist_log_density <- function(spec, linpred, y) {
  n <- ncol(linpred)
  y <- if (length(y) == 1L) rep(y, n) else y
  dfun <- dist_fun(spec$dist, "d")
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = n)
  for (j in seq_len(n)) {
    std <- standardise_for_dist(y[j], spec$args(j))
    val <- do.call(dfun, c(list(std$value), std$args, list(log = TRUE)))
    # Standardising divides by the scale, which a density pays for.
    out[, j] <- if (is.null(std$scale)) val else val - log(std$scale)
  }
  out
}


# Internal: the addition-term data brms recorded for one response.
#
# brms writes `weights`, `cens`, `rcens`, `lb` and `ub` into standata,
# one value per observation and suffixed with the response name on a
# multivariate fit. Bounds arrive already expanded per row, so a
# constant bound and a per-observation bound read the same way.
#' @noRd
addition_term_data <- function(object, resp, nobs) {
  sdata <- object$standata
  if (is.null(sdata)) {
    return(list())
  }
  # A multivariate fit suffixes each key with the response, so an
  # unsuffixed key belongs to a univariate fit and must not be read
  # when a response was named.
  pick <- function(key) {
    nm <- if (!is.null(resp)) paste0(key, "_", resp) else key
    val <- sdata[[nm]]
    if (is.null(val)) {
      return(NULL)
    }
    val <- as.numeric(val)
    if (length(val) == 1L) val <- rep(val, nobs)
    if (length(val) != nobs) {
      return(structure(val, mismatch = TRUE))
    }
    val
  }
  out <- list(
    weights = pick("weights"), cens = pick("cens"),
    rcens = pick("rcens"), lb = pick("lb"), ub = pick("ub")
  )
  out <- out[!vapply(out, is.null, logical(1L))]

  # A closure-unit family scores at the unit grain while the addition
  # terms are recorded per visit. Silently dropping them would report
  # an information criterion that looks ordinary and is wrong, so the
  # mismatch is refused instead.
  bad <- names(out)[vapply(out, function(v) isTRUE(attr(v, "mismatch")),
                           logical(1L))]
  if (length(bad)) {
    stop(insight::format_error(c(
      cli::format_inline(paste0(
        "Addition term{?s} {.field {bad}} {?does/do} not line up ",
        "with the likelihood."
      )),
      x = cli::format_inline(paste0(
        "{length(out[[bad[1]]])} value{?s} recorded against ",
        "{nobs} likelihood contribution{?s}."
      )),
      i = paste0(
        "A closure-unit family scores once per unit, not per visit, ",
        "so per-visit weights, censoring or bounds cannot be applied."
      )
    )), call. = FALSE)
  }
  out
}


# Internal: replace the density at censored rows with the probability
# the censoring actually observed.
#
# brms codes `cens` as 0 observed, -1 left, 1 right and 2 interval,
# the interval's upper end being `rcens`. A right-censored row
# contributes `log(1 - F(y))`, a left-censored row `log F(y)`, and an
# interval `log(F(rcens) - F(y))`.
#' @noRd
apply_censoring <- function(ll, cens, rcens, spec, linpred, y) {
  if (is.null(cens) || all(cens == 0)) {
    return(ll)
  }
  right <- which(cens == 1)
  if (length(right)) {
    surv <- dist_cdf(spec, linpred, y, lower.tail = FALSE, log.p = TRUE)
    ll[, right] <- surv[, right, drop = FALSE]
  }
  left <- which(cens == -1)
  if (length(left)) {
    lower <- dist_cdf(spec, linpred, y, log.p = TRUE)
    ll[, left] <- lower[, left, drop = FALSE]
  }
  interval <- which(cens == 2)
  if (length(interval)) {
    if (is.null(rcens)) {
      stop(insight::format_error(c(
        "Interval-censored rows carry no upper bound.",
        i = paste0(
          "Give the interval end as the third argument to 'cens()', ",
          "as in 'y | cens(censored, upper) ~ ...'."
        )
      )), call. = FALSE)
    }
    cdf_y <- dist_cdf(spec, linpred, y)
    cdf_u <- dist_cdf(spec, linpred, rcens)
    ll[, interval] <- log(
      cdf_u[, interval, drop = FALSE] - cdf_y[, interval, drop = FALSE]
    )
  }
  ll
}


# Internal: renormalise the density over the truncation bounds.
#
# A truncated model places all its mass inside `[lb, ub]`, so each
# contribution loses `log(F(ub) - F(lb))`. Without this every
# information criterion reads an un-normalised density, and
# `pp_check(type = "loo_pit*")` mixes truncated draws with weights
# that were never renormalised.
#
# A discrete family puts mass on the lower bound itself, so the
# excluded tail stops one step below it. This follows the Stan program
# brms generates, `log_diff_exp(lcdf(ub), lcdf(lb - 1))`, which is
# what the posterior was actually sampled under.
#' @noRd
apply_truncation_to_loglik <- function(ll, lb, ub, spec, linpred,
                                       discrete) {
  has_lb <- !is.null(lb) && any(is.finite(lb))
  has_ub <- !is.null(ub) && any(is.finite(ub))
  if (!has_lb && !has_ub) {
    return(ll)
  }
  log_cdf_ub <- if (has_ub) {
    dist_cdf(spec, linpred, ub, log.p = TRUE)
  } else {
    matrix(0, nrow(ll), ncol(ll))
  }
  log_cdf_lb <- if (has_lb) {
    dist_cdf(spec, linpred, if (discrete) lb - 1 else lb, log.p = TRUE)
  } else {
    matrix(-Inf, nrow(ll), ncol(ll))
  }
  ll - log_diff_exp(log_cdf_ub, log_cdf_lb)
}


# Internal: `log(exp(a) - exp(b))` without leaving the log scale.
#' @noRd
log_diff_exp <- function(a, b) {
  a + log1p(-exp(b - a))
}


# Internal: scale each contribution by its case weight.
#
# `weights()` states how many observations a row stands for, so its
# log density enters every sum that many times.
#' @noRd
apply_case_weights <- function(ll, weights) {
  if (is.null(weights) || all(weights == 1)) {
    return(ll)
  }
  sweep(ll, 2L, weights, `*`)
}


# Internal: apply every addition term recorded for this response.
#
# The order follows brms: censoring replaces the contribution at a
# censored row, truncation then renormalises whatever that row
# contributes, and the case weight scales the result. Renormalising
# first would be undone by the censored replacement.
#' @noRd
apply_addition_terms <- function(ll, object, resp, family_name, link,
                                 linpred, y, family_pars, trials) {
  terms <- addition_term_data(object, resp, nobs = ncol(ll))
  if (!length(terms)) {
    return(ll)
  }
  needs_cdf <- !is.null(terms$cens) && any(terms$cens != 0) ||
    !is.null(terms$lb) && any(is.finite(terms$lb)) ||
    !is.null(terms$ub) && any(is.finite(terms$ub))

  if (needs_cdf) {
    spec <- family_dist_spec(family_name, link, linpred, family_pars,
                             trials)
    if (is.null(spec)) {
      stop(insight::format_error(c(
        cli::format_inline(paste0(
          "'cens()' and 'trunc()' are unavailable for family ",
          "{.val {family_name}}."
        )),
        i = paste0(
          "Both need the family's distribution function, which mvgam ",
          "does not define for this family."
        )
      )), call. = FALSE)
    }
    ll <- apply_censoring(ll, terms$cens, terms$rcens, spec, linpred, y)
    ll <- apply_truncation_to_loglik(
      ll, terms$lb, terms$ub, spec, linpred,
      discrete = family_uses_integers(family_name)
    )
  }
  apply_case_weights(ll, terms$weights)
}
