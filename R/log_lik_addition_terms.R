# Post-fit handling for the brms addition terms that change what a
# per-observation likelihood contribution means: `weights()`, `cens()`
# and `trunc()`.
#
# Each is applied to the `[ndraws x nobs]` matrix that
# `dispatch_log_lik()` returns, so `loo()`, `waic()`, `lfo_cv()` and
# `kfold()` inherit the corrected contributions without knowing the
# terms exist.
#
# Censoring and truncation need the family's distribution function. It
# takes the parameterisation the density takes. `family_dist_spec()`
# names the R distribution and builds its arguments once for both.


#' The two shapes of a beta from its mean and precision
#'
#' Uses the brms parametrisation. The beta family, the beta-binomial
#' mixing law, a Dirichlet component and the simulator all call it.
#' `boundary_na = TRUE` serves the caller that uses the spec only as a
#' distribution function. A mean of 0 or 1 sets a shape to zero.
#' `qnorm()` would then turn the boundary value into an eight-sigma
#' residual. A density and a draw stay defined there and keep the
#' shapes they were given.
#'
#' @param mu Numeric mean in `[0, 1]`
#' @param phi Numeric precision
#' @param boundary_na Logical flag to set a mean of 0 or 1 to `NA`
#' @return List holding `shape1` and `shape2`
#' @noRd
beta_shapes <- function(mu, phi, boundary_na = FALSE) {
  checkmate::assert_numeric(mu)
  checkmate::assert_numeric(phi)
  checkmate::assert_flag(boundary_na)
  if (boundary_na) {
    mu[!is.na(mu) & (mu <= 0 | mu >= 1)] <- NA_real_
  }
  list(shape1 = mu * phi, shape2 = (1 - mu) * phi)
}


#' The R distribution a family maps onto
#'
#' @param family_name Character family name
#' @param link Character link name
#' @param linpred Numeric linear predictor matrix, one row per draw
#' @param family_pars Named list of distributional parameter matrices
#' @param trials Numeric vector of trials, or `NULL`
#' @return List holding the distribution name `dist` and `args`, a
#'   function giving the distribution's arguments for one column of
#'   draws. `NULL` for a family without a single R distribution, which
#'   makes `cens()` and `trunc()` unavailable for it.
#' @noRd
family_dist_spec <- function(family_name, link, linpred, family_pars,
                             trials) {
  checkmate::assert_string(family_name)
  checkmate::assert_string(link, null.ok = TRUE)
  checkmate::assert_matrix(linpred)
  checkmate::assert_list(family_pars)
  checkmate::assert_numeric(trials, null.ok = TRUE)
  mu <- inv_link(linpred, link)
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
    # `pt` takes a standard Student t. A location-scale value and its
    # bound are standardised before it.
    student = spec("t", function(j) {
      list(df = nu[, j], .shift = mu[, j], .scale = sigma[, j])
    }),
    asym_laplace = spec("asym_laplace", function(j) {
      list(mu = mu[, j], sigma = sigma[, j],
           quantile = family_pars$quantile[, j])
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
    com_poisson = spec("com_poisson", function(j) {
      list(mu = mu[, j], shape = shape[, j])
    }),
    beta_nb = spec("beta_nb", function(j) {
      list(mu = mu[, j], shape = shape[, j],
           mtail = family_pars$mtail[, j])
    }),
    # `mvn` and `mvt` are written row by row. Given the latent state a
    # row is normal or location-scale Student t. `log_lik()` evaluates
    # that density. The distribution function is the row's own.
    mvn = spec("norm", function(j) {
      list(mean = mu[, j], sd = family_pars$Psi_row[, j])
    }),
    mvt = spec("t", function(j) {
      list(df = family_pars$nu, .shift = mu[, j],
           .scale = family_pars$Psi_row[, j])
    }),
    # A Dirichlet is a joint law over the rows of a unit. The marginal
    # of one component is Beta(alpha_j, alpha_0 - alpha_j). A unit's
    # softmax probabilities sum to one and `phi` is constant across
    # it. alpha_0 is then `phi` and the row's mean is its softmax
    # probability. These are the beta family's two arguments, taken
    # row by row.
    #
    # The marginal gives a per-row quantity such as a residual exactly.
    # It leaves out what bounding one row does to its unit's density.
    # Only the residual uses it. A closure-unit fit refuses an addition
    # term before parsing one. `cens()` and `trunc()` reach the
    # univariate path alone.
    diri = spec("beta", function(j) {
      beta_shapes(family_pars$prob_row[, j], family_pars$phi[, j],
                  boundary_na = TRUE)
    }),
    # The other two compositions have a marginal of the same kind. A
    # multinomial draws its K counts jointly against the site's total.
    # One component of that draw is `Binomial(N_site, p_k)`. A
    # categorical unit records one species, which is that binomial at
    # a single trial.
    multi = spec("binom", function(j) {
      list(size = family_pars$unit_total_row[j],
           prob = family_pars$prob_row[, j])
    }),
    categ = spec("binom", function(j) {
      list(size = 1L, prob = family_pars$prob_row[, j])
    }),
    # A Tweedie puts a point mass at zero and is continuous above it.
    # The spec records the location of that mass. A caller forming a
    # probability-integral transform needs the interval an observation
    # of zero occupies.
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


#' The density, distribution or quantile function of a distribution
#'
#' Most come from base R. `bbinom` comes from extraDistr and
#' `asym_laplace` from brms. `cmb`, `com_poisson`, `beta_nb` and
#' `tweedie` use the package's own kernels.
#'
#' @param dist Character distribution name from `family_dist_spec()`
#' @param kind Character function kind, `"d"`, `"p"` or `"q"`
#' @return A function, or `NULL` for a quantile function the
#'   distribution lacks
#' @noRd
dist_fun <- function(dist, kind = c("d", "p", "q")) {
  checkmate::assert_string(dist)
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
      # extraDistr has no beta-binomial quantile function; `NULL`
      # sends the truncation sampler to rejection sampling.
      switch(kind, d = extraDistr::dbbinom, p = extraDistr::pbbinom,
             q = NULL)
    },
    # A COM-binomial, a COM-Poisson, a beta negative binomial and a
    # Tweedie have no quantile function here. `NULL` tells a caller
    # asking for one to take rejection sampling.
    cmb = switch(kind, d = dcmb, p = pcmb, q = NULL),
    com_poisson = switch(kind, d = dcom_poisson_mvgam,
                         p = pcom_poisson_mvgam, q = NULL),
    asym_laplace = switch(kind, d = brms::dasym_laplace,
                          p = brms::pasym_laplace, q = brms::qasym_laplace),
    beta_nb = switch(kind, d = dbeta_nb_mvgam, p = pbeta_nb_mvgam,
                     q = NULL),
    tweedie = switch(kind, d = dtweedie_cpg, p = ptweedie_cpg,
                     q = NULL),
    get(paste0(kind, dist), mode = "function",
        envir = asNamespace("stats"))
  )
}


#' Whether `family_dist_spec()` names a family
#'
#' Called before a predictor is computed, sparing a family without a
#' spec the prediction. The spec is built against an empty predictor,
#' keeping the family list in one switch.
#'
#' @param family_name Character family name
#' @param link Character link name
#' @return Logical scalar
#' @noRd
family_has_dist_spec <- function(family_name, link) {
  checkmate::assert_string(family_name)
  checkmate::assert_string(link, null.ok = TRUE)
  !is.null(
    family_dist_spec(family_name, link, matrix(0), list(), NULL)
  )
}


#' A value standardised for a location-scale distribution
#'
#' The base distribution of a location-scale family takes neither a
#' location nor a scale. The value is standardised and both entries are
#' dropped from the arguments.
#'
#' @param value Numeric value to evaluate
#' @param args Named list of distribution arguments, possibly holding
#'   `.shift` and `.scale`
#' @return List holding the standardised `value`, the remaining `args`
#'   and the `scale` a density corrects by and a sampler undoes
#' @noRd
standardise_for_dist <- function(value, args) {
  checkmate::assert_list(args)
  scale <- args$.scale
  if (!is.null(args$.shift)) {
    value <- (value - args$.shift) / scale
    args$.shift <- NULL
    args$.scale <- NULL
  }
  list(value = value, args = args, scale = scale)
}


#' `P(Y <= q)` for every draw and observation
#'
#' `lower.tail` and `log.p` go to the distribution function itself,
#' keeping far-tail survival probabilities accurate.
#'
#' @param spec Distribution spec from `family_dist_spec()`
#' @param linpred Numeric linear predictor matrix, one row per draw
#' @param q Numeric quantiles, one per observation or a single value
#' @param lower.tail Logical flag for `P(Y <= q)`
#' @param log.p Logical flag to return log probabilities
#' @return Numeric matrix with dimensions `[ndraws, nobs]`
#' @noRd
dist_cdf <- function(spec, linpred, q, lower.tail = TRUE,
                     log.p = FALSE) {
  checkmate::assert_list(spec)
  checkmate::assert_matrix(linpred)
  checkmate::assert_numeric(q)
  checkmate::assert_flag(lower.tail)
  checkmate::assert_flag(log.p)
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


#' Draws from a family restricted to `[lb, ub]`
#'
#' Uses the inverse-transform method. A uniform on `[F(lb), F(ub)]`
#' mapped through the quantile function lands inside the bounds. One
#' pass is exact. Rejection sampling would leave unplaced draws to clamp
#' onto a bound. A discrete family opens the interval at `F(lb - 1)`,
#' keeping `lb` attainable.
#'
#' @param spec Distribution spec from `family_dist_spec()`
#' @param cells Logical matrix marking the `[ndraws, nobs]` cells to
#'   redraw
#' @param lb Numeric lower bounds
#' @param ub Numeric upper bounds
#' @param discrete Logical flag for an integer-valued family
#' @return Numeric matrix shaped as `cells`, or `NULL` when the family
#'   has no quantile function. `NULL` sends the caller to rejection
#'   sampling.
#' @noRd
truncated_dist_draws <- function(spec, cells, lb, ub, discrete) {
  checkmate::assert_list(spec)
  checkmate::assert_matrix(cells, mode = "logical")
  checkmate::assert_numeric(lb)
  checkmate::assert_numeric(ub)
  checkmate::assert_flag(discrete)
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


#' The log density of every draw and observation
#'
#' Uses the spec `dist_cdf()` uses, defining each family's
#' parametrisation in one place.
#'
#' @param spec Distribution spec from `family_dist_spec()`
#' @param linpred Numeric linear predictor matrix, one row per draw
#' @param y Numeric responses, one per observation or a single value
#' @return Numeric matrix with dimensions `[ndraws, nobs]`
#' @noRd
dist_log_density <- function(spec, linpred, y) {
  checkmate::assert_list(spec)
  checkmate::assert_matrix(linpred)
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


#' The addition-term values of one response, row by row
#'
#' The Stan data hold each addition term's values for the observed rows
#' of the training frame. A prediction or a score covers the rows of the
#' frame it is asked for, including rows with a missing response and
#' rows of new data. Each term is evaluated against that frame. A name
#' the frame lacks resolves in the formula's environment.
#'
#' @param form One response's `brmsformula`
#' @param family That response's family
#' @param frame Data frame of the rows
#' @return Named list holding any of `trials`, `weights`, `cens`,
#'   `rcens`, `lb` and `ub`, one value per row. A term the formula
#'   lacks is left out. So is a term naming a variable that neither
#'   `frame` nor the formula's environment holds.
#' @noRd
addition_term_values <- function(form, family, frame) {
  checkmate::assert_class(form, "brmsformula")
  checkmate::assert_list(family)
  checkmate::assert_data_frame(frame)
  form$family <- family
  adforms <- brms::brmsterms(form)$adforms
  env <- environment(form$formula) %||% globalenv()
  # brms's `resp_*()` constructors record each argument as a string.
  term_spec <- function(term) {
    if (is.null(adforms[[term]])) return(NULL)
    eval(rlang::f_rhs(adforms[[term]]), envir = asNamespace("brms"))
  }
  term_value <- function(spec, var) {
    expr <- str2lang(spec$vars[[var]])
    unbound <- setdiff(all.vars(expr), names(frame))
    if (!all(vapply(unbound, exists, logical(1L), envir = env))) {
      return(NULL)
    }
    rep_len(eval(expr, frame, env), nrow(frame))
  }

  out <- list()
  if (!is.null(spec <- term_spec("trials"))) {
    out$trials <- term_value(spec, "trials")
  }
  if (!is.null(spec <- term_spec("weights"))) {
    w <- term_value(spec, "weights")
    # `weights(w, scale = TRUE)` rescales the weights to sum to the row
    # count.
    if (isTRUE(spec$flags$scale) && !is.null(w)) {
      w <- w / sum(w) * length(w)
    }
    out$weights <- w
  }
  if (!is.null(spec <- term_spec("cens"))) {
    cens <- term_value(spec, "cens")
    out$cens <- if (!is.null(cens)) censoring_codes(cens)
    if (!identical(spec$vars$y2, "NA")) {
      out$rcens <- term_value(spec, "y2")
    }
  }
  if (!is.null(spec <- term_spec("trunc"))) {
    out$lb <- as.numeric(term_value(spec, "lb"))
    out$ub <- as.numeric(term_value(spec, "ub"))
  }
  Filter(length, out)
}


#' The addition-term values of one response of a fitted model
#'
#' @param object Fitted `mvgam` object
#' @param frame Data frame of the rows
#' @param resp Response key, or `NULL` on a model with one
#' @param family That response's family
#' @return As `addition_term_values()`
#' @noRd
response_addition_terms <- function(object, frame, resp = NULL,
                                    family = model_families(object, resp)) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(resp, null.ok = TRUE)
  addition_term_values(
    response_formulas(object)[[resp %||% 1L]], family, frame
  )
}


#' Censoring indicators as numeric codes
#'
#' `-1` is left, `0` none, `1` right and `2` interval censoring. A
#' character value matches a prefix of `"left"`, `"none"`, `"right"` or
#' `"interval"`. A logical `TRUE` is right censoring.
#'
#' @param x Censoring indicators as character, factor, logical or
#'   numeric codes
#' @return Numeric vector of codes
#' @noRd
censoring_codes <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x) || is.numeric(x)) {
    out <- as.numeric(x)
  } else {
    labels <- c(left = -1, none = 0, right = 1, interval = 2)
    out <- vapply(x, function(v) {
      hit <- labels[startsWith(names(labels), v)]
      if (length(hit) == 1L) hit[[1L]] else NA_real_
    }, numeric(1L), USE.NAMES = FALSE)
  }
  bad <- !is.na(x) & !out %in% -1:2
  if (any(bad)) {
    stop(insight::format_error(c(
      "Censoring indicators must be 'left', 'none', 'right' or 'interval'.",
      x = paste0("Got ", paste0("'", unique(x[bad]), "'", collapse = ", "),
                 "."),
      i = "Codes -1, 0, 1 and 2 and TRUE or FALSE are also accepted."
    )), call. = FALSE)
  }
  out
}


#' Log-likelihood with censored rows given their censoring probability
#'
#' brms codes `cens` as 0 for observed, -1 for left, 1 for right and 2
#' for interval censoring, with `rcens` the upper end of an interval. A
#' right-censored row contributes `log(1 - F(y))`. A left-censored row
#' contributes `log F(y)`. An interval contributes
#' `log(F(rcens) - F(y))`.
#'
#' @param ll Numeric log-likelihood matrix, one row per draw
#' @param cens Numeric censoring codes, or `NULL`
#' @param rcens Numeric interval upper bounds, or `NULL`
#' @param spec Distribution spec from `family_dist_spec()`
#' @param linpred Numeric linear predictor matrix
#' @param y Numeric responses
#' @return `ll` with the censored columns replaced
#' @noRd
apply_censoring <- function(ll, cens, rcens, spec, linpred, y) {
  checkmate::assert_matrix(ll)
  checkmate::assert_numeric(cens, null.ok = TRUE)
  checkmate::assert_numeric(rcens, null.ok = TRUE)
  checkmate::assert_list(spec)
  checkmate::assert_matrix(linpred)
  if (is.null(cens) || isTRUE(all(cens == 0))) {
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
        "Interval-censored rows need an upper bound.",
        i = "Give it as the third argument: 'cens(censored, upper)'."
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


#' Log-likelihood renormalised over the truncation bounds
#'
#' A truncated model places all its mass inside the window
#' `truncation_window()` gives. Each contribution loses the log of that
#' mass.
#'
#' @param ll Numeric log-likelihood matrix, one row per draw
#' @param lb Numeric lower bounds, or `NULL`
#' @param ub Numeric upper bounds, or `NULL`
#' @param spec Distribution spec from `family_dist_spec()`
#' @param linpred Numeric linear predictor matrix
#' @param discrete Logical flag for an integer-valued family
#' @return `ll` renormalised over the bounds
#' @noRd
apply_truncation_to_loglik <- function(ll, lb, ub, spec, linpred,
                                       discrete) {
  checkmate::assert_matrix(ll)
  checkmate::assert_list(spec)
  checkmate::assert_matrix(linpred)
  window <- truncation_window(lb, ub, ncol(ll), discrete)
  if (is.null(window)) {
    return(ll)
  }
  ll - log_interval_mass(spec_cdf(spec, linpred), window$lo, window$hi)
}


#' The `(lo, hi]` window a pair of truncation bounds keeps
#'
#' A count family keeps the mass at its lower bound, as the Stan
#' likelihood `log_diff_exp(lcdf(ub), lcdf(lb - 1))` does. Its window
#' then opens one step below `lb`.
#'
#' @param lb,ub Numeric bounds, one per observation, or `NULL`
#' @param n Integer number of observations
#' @param discrete Logical flag for an integer-valued family
#' @return List holding `lo` and `hi`, one per observation, or `NULL`
#'   when neither bound is finite
#' @noRd
truncation_window <- function(lb, ub, n, discrete) {
  checkmate::assert_numeric(lb, null.ok = TRUE)
  checkmate::assert_numeric(ub, null.ok = TRUE)
  checkmate::assert_count(n)
  checkmate::assert_flag(discrete)
  if (!any(is.finite(lb)) && !any(is.finite(ub))) {
    return(NULL)
  }
  lo <- rep_len(lb %||% -Inf, n)
  list(lo = if (discrete) lo - 1 else lo, hi = rep_len(ub %||% Inf, n))
}


#' A spec's distribution function as a function of its quantiles
#'
#' @param spec Distribution spec from `family_dist_spec()`
#' @param linpred Numeric linear predictor matrix, one row per draw
#' @return Function of `(q, lower.tail, log.p)` returning the
#'   `[ndraws, nobs]` matrix `dist_cdf()` gives
#' @noRd
spec_cdf <- function(spec, linpred) {
  function(q, lower.tail = TRUE, log.p = FALSE) {
    dist_cdf(spec, linpred, q, lower.tail = lower.tail, log.p = log.p)
  }
}


#' Log-likelihood scaled by case weights
#'
#' A case weight gives the number of observations a row represents. Its
#' log density enters every sum that many times.
#'
#' @param ll Numeric log-likelihood matrix, one row per draw
#' @param weights Numeric weights, one per column, or `NULL`
#' @return `ll` with each column multiplied by its weight
#' @noRd
apply_case_weights <- function(ll, weights) {
  checkmate::assert_matrix(ll)
  checkmate::assert_numeric(weights, null.ok = TRUE)
  if (is.null(weights) || isTRUE(all(weights == 1))) {
    return(ll)
  }
  sweep(ll, 2L, weights, `*`)
}


#' Log-likelihood with every recorded addition term applied
#'
#' Follows the brms order. Censoring replaces a censored row's
#' contribution. Truncation then renormalises each row. The case weight
#' scales the result. Renormalising first would be undone by the
#' censored replacement.
#'
#' @param ll Numeric log-likelihood matrix, one row per draw
#' @param object Fitted `mvgam` object
#' @param frame Data frame of the scored rows
#' @param resp Character response name, or `NULL`
#' @param family_name Character family name
#' @param link Character link name
#' @param linpred Numeric linear predictor matrix
#' @param y Numeric responses
#' @param family_pars Named list of distributional parameter matrices
#' @param trials Numeric vector of trials, or `NULL`
#' @return `ll` with the addition terms applied
#' @noRd
apply_addition_terms <- function(ll, object, frame, resp, family_name,
                                 link, linpred, y, family_pars, trials) {
  checkmate::assert_matrix(ll)
  checkmate::assert_string(family_name)
  terms <- response_addition_terms(object, frame, resp)
  if (!length(terms)) {
    return(ll)
  }
  needs_cdf <- any(terms$cens != 0, na.rm = TRUE) ||
    any(is.finite(terms$lb)) || any(is.finite(terms$ub))

  if (needs_cdf) {
    spec <- family_dist_spec(family_name, link, linpred, family_pars,
                             trials)
    if (is.null(spec)) {
      stop(insight::format_error(paste0(
        "'cens()' and 'trunc()' are not supported for family '",
        family_name, "'."
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
