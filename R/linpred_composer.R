# Composing a linear predictor from a fitted model's draws.
#
# brms writes one linear predictor per response, per distributional
# parameter and per non-linear parameter, and names every piece of each
# by one rule, which `predictor_suffix()` states. Every term reader here
# takes that suffix. One body of code composes the mean of a model with
# one response, a response of a model with several, a distributional
# parameter and a non-linear parameter. Each reader returns its own
# `[ndraws x nobs]` contribution, zero when the predictor carries no
# term of that kind.


#' The suffix brms gives the names of one linear predictor
#'
#' brms names each piece of a predictor with the distributional
#' parameter unless it is the mean, then the response of a model with
#' several, then the non-linear parameter, each after an underscore.
#' `sigma` of the response `y1` reads its design from `X_sigma_y1` and
#' its coefficients from `b_sigma_y1`; the mean of a model with one
#' response reads `X` and `b`. A grouping index is named by the response
#' alone, as `J_1_y1`.
#'
#' @param resp The response's key on a model with several, or `NULL`
#' @param dpar The distributional parameter, or `NULL` for the mean
#' @param nlpar The non-linear parameter, or `NULL`
#' @return `""`, or the suffix with its leading underscore
#' @noRd
predictor_suffix <- function(resp = NULL, dpar = NULL, nlpar = NULL) {
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_string(dpar, null.ok = TRUE)
  checkmate::assert_string(nlpar, null.ok = TRUE)
  parts <- c(if (!identical(dpar %||% "mu", "mu")) dpar, resp, nlpar)
  if (length(parts) == 0L) {
    return("")
  }
  paste0("_", paste(parts, collapse = "_"))
}


#' Every linear predictor a brms model writes
#'
#' One for each response's mean unless the mean is non-linear, one for
#' each distributional parameter given a formula, and one for each
#' non-linear parameter.
#'
#' @param formula A `brmsformula` or `mvbrmsformula`
#' @return A list of `list(resp, dpar, nlpar)` as `predictor_suffix()`
#'   takes them, each `NULL` where it does not apply
#' @noRd
model_predictors <- function(formula) {
  forms <- response_formulas(formula)
  several <- length(forms) > 1L
  out <- list()
  for (key in names(forms)) {
    resp <- if (several) key
    nlpars <- nonlinear_parameters(forms[[key]])
    dpars <- setdiff(names(forms[[key]]$pforms), nlpars)
    out <- c(
      out,
      if (length(nlpars) == 0L) list(list(resp = resp, dpar = NULL,
                                          nlpar = NULL)),
      lapply(dpars, function(d) list(resp = resp, dpar = d, nlpar = NULL)),
      lapply(nlpars, function(p) list(resp = resp, dpar = NULL, nlpar = p))
    )
  }
  out
}


#' Linear predictor of a prepared model, on the link scale
#'
#' @param prep A `mvgam_prep` from `prepare_linpred_data()`
#' @param resp One response of a model with several, or `NULL`, which
#'   answers for each
#' @param dpar The distributional parameter, or `NULL` for the mean
#' @return A `[ndraws x nobs]` matrix, or a list of them named by
#'   response when a model with several is asked for none
#' @noRd
extract_linpred_from_prep <- function(prep, resp = NULL, dpar = NULL) {
  checkmate::assert_class(prep, "mvgam_prep")
  checkmate::assert_names(
    names(prep), must.include = c("draws", "sdata", "nobs", "formula")
  )
  checkmate::assert_string(dpar, null.ok = TRUE)
  resolve_resp(prep$formula, resp)
  forms <- response_formulas(prep$formula)
  if (length(forms) == 1L) {
    return(compose_linpred(prep, forms[[1L]], resp = NULL, dpar = dpar))
  }
  if (!is.null(resp)) {
    return(compose_linpred(prep, forms[[resp]], resp = resp, dpar = dpar))
  }
  lapply(stats::setNames(nm = names(forms)), function(r) {
    compose_linpred(prep, forms[[r]], resp = r, dpar = dpar)
  })
}


#' One response's mean or distributional parameter, on the link scale
#'
#' A non-linear mean is its expression evaluated at the non-linear
#' parameters, each of which is a linear predictor of its own. Every
#' other predictor is the sum of its terms.
#'
#' @param prep A `mvgam_prep`
#' @param form The response's `brmsformula`
#' @param resp The response's key on a model with several, or `NULL`
#' @param dpar The distributional parameter, or `NULL` for the mean
#' @return A `[ndraws x nobs]` matrix
#' @noRd
compose_linpred <- function(prep, form, resp, dpar) {
  draws <- as_plain_matrix(posterior::as_draws_matrix(prep$draws))
  n_obs <- predictor_nobs(prep, resp)
  if (identical(dpar %||% "mu", "mu")) {
    if (isTRUE(attr(form$formula, "nl"))) {
      return(nonlinear_linpred(prep, draws, form, resp, n_obs))
    }
    return(linear_terms_pred(prep, draws, form$formula, resp, NULL, NULL,
                             n_obs))
  }
  dpar_form <- form$pforms[[dpar]]
  if (is.null(dpar_form)) {
    stop_mvgam_fault(
      paste0("A linear predictor was asked for '", dpar,
             "', which has no formula of its own."),
      "Only a parameter given a formula has a linear predictor."
    )
  }
  if (isTRUE(attr(dpar_form, "nl"))) {
    stop(insight::format_error(c(
      paste0("mvgam cannot predict '", dpar,
             "' from a non-linear formula."),
      i = paste0("Write the formula for '", dpar, "' as a linear ",
                 "predictor, or give the non-linear form to the mean.")
    )), call. = FALSE)
  }
  linear_terms_pred(prep, draws, dpar_form, resp, dpar, NULL, n_obs)
}


#' The rows one response's predictors cover
#'
#' A model with one response covers every row of the prediction. brms
#' writes `N_<resp>` for each response of a model with several.
#'
#' @param prep A `mvgam_prep`
#' @param resp The response's key on a model with several, or `NULL`
#' @return A positive count
#' @noRd
predictor_nobs <- function(prep, resp) {
  n_obs <- if (is.null(resp)) prep$nobs else prep$sdata[[paste0("N_", resp)]]
  checkmate::assert_count(n_obs, positive = TRUE)
  n_obs
}


#' The non-linear parameters of one response's formula
#'
#' A non-linear mean names its parameters in its expression, and each
#' one has a formula of its own in `pforms`. Every other formula there
#' belongs to a distributional parameter.
#'
#' @param form The response's `brmsformula`
#' @return Character vector, empty when the mean is linear
#' @noRd
nonlinear_parameters <- function(form) {
  if (!isTRUE(attr(form$formula, "nl"))) {
    return(character(0L))
  }
  intersect(all.vars(form$formula[[3L]]), names(form$pforms))
}


#' The sum of every term of one linear predictor
#'
#' @param prep A `mvgam_prep`
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param rhs The formula this predictor was written as, read for the
#'   kernel of each `gp()` term
#' @param resp,dpar,nlpar The predictor, as `predictor_suffix()` takes it
#' @param n_obs Number of rows the predictor covers
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
linear_terms_pred <- function(prep, draws, rhs, resp, dpar, nlpar, n_obs) {
  sfx <- predictor_suffix(resp, dpar, nlpar)
  sdata <- prep$sdata
  n_cs <- sdata[[paste0("Kcs", sfx)]]
  if (!is.null(n_cs) && n_cs > 0L) {
    stop(insight::format_error(c(
      "mvgam cannot predict from category-specific effects.",
      i = "Drop the 'cs()' terms from the formula."
    )), call. = FALSE)
  }
  eta <- fixed_pred(draws, sdata, sfx, n_obs) +
    smooth_pred(draws, sdata, sfx, n_obs) +
    group_level_pred(prep, draws, resp, dpar, nlpar, n_obs) +
    gp_pred(draws, sdata, sfx, gp_kernels(rhs), n_obs) +
    monotonic_pred(draws, sdata, sfx, n_obs)
  offsets <- sdata[[paste0("offsets", sfx)]]
  if (!is.null(offsets)) {
    checkmate::assert_numeric(as.vector(offsets), any.missing = FALSE,
                              finite = TRUE, len = n_obs)
    eta <- eta + matrix(offsets, nrow(draws), n_obs, byrow = TRUE)
  }
  eta
}


#' Posterior columns a term reads
#'
#' The design and the draws both come from the fitted model. A name the
#' design implies and the draws lack is a fault in mvgam.
#'
#' @param draws Plain `[ndraws x npar]` matrix of the posterior
#' @param cols The column names to read, in order
#' @return `draws[, cols]` as a matrix
#' @noRd
draw_columns <- function(draws, cols) {
  missing <- setdiff(cols, colnames(draws))
  if (length(missing) > 0L) {
    stop_mvgam_fault(
      "A term of the linear predictor has no posterior draws.",
      paste0("Missing: ", paste(utils::head(missing, 3L), collapse = ", "),
             if (length(missing) > 3L) ", ...", ".")
    )
  }
  draws[, cols, drop = FALSE]
}


#' A data entry a term reads
#'
#' @param sdata The prediction's Stan data
#' @param name The entry brms writes
#' @return The entry
#' @noRd
sdata_entry <- function(sdata, name) {
  out <- sdata[[name]]
  if (is.null(out)) {
    stop_mvgam_fault(
      "A term of the linear predictor has no prediction data.",
      paste0("Missing: ", name, ".")
    )
  }
  out
}


#' The numbers brms gives the terms of one kind
#'
#' @param x Names to search
#' @param stem The name before the term's number, as `"nb_sigma"`
#' @return Sorted integer vector, empty when there are none
#' @noRd
term_numbers <- function(x, stem) {
  pattern <- paste0("^", stem, "_([0-9]+)$")
  sort(as.integer(sub(pattern, "\\1", grep(pattern, x, value = TRUE))))
}


#' Population-level terms
#'
#' brms samples the slopes against the design with its intercept column
#' removed and writes the intercept on its own, as `b_Intercept`. A
#' predictor without that parameter, such as `y ~ 0 + x` or a non-linear
#' parameter, keeps every column of the design.
#'
#' @inheritParams linear_terms_pred
#' @param sdata The prediction's Stan data
#' @param sfx The predictor's `predictor_suffix()`
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
fixed_pred <- function(draws, sdata, sfx, n_obs) {
  intercept <- paste0("b", sfx, "_Intercept")
  has_intercept <- intercept %in% colnames(draws)
  eta <- matrix(if (has_intercept) draws[, intercept] else 0,
                nrow(draws), n_obs)
  X <- sdata[[paste0("X", sfx)]]
  if (is.null(X) || ncol(X) == 0L) {
    return(eta)
  }
  checkmate::assert_matrix(X, nrows = n_obs, col.names = "named")
  if (has_intercept) {
    X <- X[, colnames(X) != "Intercept", drop = FALSE]
  }
  if (ncol(X) == 0L) {
    return(eta)
  }
  b <- draw_columns(draws, paste0("b", sfx, "[", seq_len(ncol(X)), "]"))
  eta + b %*% t(X)
}


#' Smooth terms
#'
#' brms numbers the smooth objects of a predictor in order, one per
#' level of a `by` factor, and splits each into an unpenalised part and
#' one or more penalised parts. The unpenalised parts share `Xs`, read
#' against `bs`, and `attr(Xs, "smcols")` gives each object's columns
#' of it. Object `i` has `nb_<i>` penalised parts, the `Zs_<i>_<j>`
#' bases read against `s_<i>_<j>`.
#'
#' @inheritParams fixed_pred
#' @param objects The numbers of the smooth objects to read, or `NULL`
#'   for every one
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
smooth_pred <- function(draws, sdata, sfx, n_obs, objects = NULL) {
  eta <- matrix(0, nrow(draws), n_obs)
  Xs <- sdata[[paste0("Xs", sfx)]]
  if (is.null(objects)) {
    objects <- term_numbers(names(sdata), paste0("nb", sfx))
    cols <- seq_len(NCOL(Xs))
  } else {
    checkmate::assert_integerish(objects, lower = 1L, any.missing = FALSE,
                                 min.len = 1L)
    smcols <- attr(sdata_entry(sdata, paste0("Xs", sfx)), "smcols")
    if (max(objects) > length(smcols)) {
      stop_mvgam_fault(
        "A smooth term names an object its predictor does not have.",
        paste0("Object ", max(objects), " of ", length(smcols), ".")
      )
    }
    cols <- unlist(smcols[objects])
  }
  if (length(cols) > 0L) {
    bs <- draw_columns(draws, paste0("bs", sfx, "[", cols, "]"))
    eta <- eta + bs %*% t(Xs[, cols, drop = FALSE])
  }
  for (i in objects) {
    for (j in seq_len(sdata_entry(sdata, paste0("nb", sfx, "_", i)))) {
      stem <- paste0(sfx, "_", i, "_", j)
      Zs <- sdata_entry(sdata, paste0("Zs", stem))
      checkmate::assert_matrix(Zs, nrows = n_obs)
      s <- draw_columns(draws, paste0("s", stem, "[", seq_len(ncol(Zs)), "]"))
      eta <- eta + s %*% t(Zs)
    }
  }
  eta
}


#' Group-level terms
#'
#' Each row of the fit's `ranef` table is one term of one grouping
#' factor, and brms numbers it by its correlation block `id` and its
#' place `cn` in that block: its design is `Z_<id><sfx>_<cn>`, its
#' coefficients `r_<id><sfx>_<cn>[level]`, and its grouping index
#' `J_<id>` suffixed by the response alone. `re_formula = NA` leaves
#' every term out; otherwise every term is kept, and a missing design is
#' a fault.
#'
#' @inheritParams linear_terms_pred
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
group_level_pred <- function(prep, draws, resp, dpar, nlpar, n_obs) {
  eta <- matrix(0, nrow(draws), n_obs)
  ranef <- prep$ranef
  if (is.null(ranef) || nrow(ranef) == 0L ||
      checkmate::test_scalar_na(prep$re_formula)) {
    return(eta)
  }
  # brms leaves these blank for a model with one response and for the
  # mean, which may also be spelled out as `mu`.
  blank <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }
  mean_as_blank <- function(x) {
    x <- blank(x)
    x[x == "mu"] <- ""
    x
  }
  rows <- ranef[blank(ranef$resp) == (resp %||% "") &
                  mean_as_blank(ranef$dpar) == mean_as_blank(dpar %||% "") &
                  blank(ranef$nlpar) == (nlpar %||% ""), ,
                drop = FALSE]
  special <- blank(rows$gtype) == "mm" | nzchar(blank(rows$type))
  if (any(special)) {
    stop(insight::format_error(c(
      "mvgam cannot predict from this group-level term.",
      x = paste0("Grouping factor '", rows$group[special][1L],
                 "' is written as a multi-membership or special term."),
      i = "Write it as an ordinary '(term | group)'."
    )), call. = FALSE)
  }
  sfx <- predictor_suffix(resp, dpar, nlpar)
  levels <- attr(ranef, "levels")
  for (k in seq_len(nrow(rows))) {
    id <- rows$id[k]
    Z <- sdata_entry(prep$sdata, paste0("Z_", id, sfx, "_", rows$cn[k]))
    J <- as.integer(sdata_entry(prep$sdata,
                                paste0("J_", id, predictor_suffix(resp))))
    group <- as.character(rows$group[k])
    n_levels <- length(levels[[group]])
    if (length(J) > 0L && max(J) > n_levels) {
      stop(insight::format_error(c(
        paste0("Prediction for a grouping level the model never saw is ",
               "not supported."),
        x = paste0("Grouping factor '", group, "' was fitted with ",
                   n_levels, " level", if (n_levels != 1L) "s",
                   ". 'newdata' asks for ", max(J), "."),
        i = paste0("Use 're_formula = NA' to predict from the population ",
                   "effects or supply 'newdata' whose levels the model ",
                   "was fitted to.")
      )), call. = FALSE)
    }
    r <- draw_columns(draws, paste0("r_", id, sfx, "_", rows$cn[k], "[",
                                    seq_len(n_levels), "]"))
    eta <- eta + r[, J, drop = FALSE] *
      matrix(as.vector(Z), nrow(draws), n_obs, byrow = TRUE)
  }
  eta
}


#' The kernel of each `gp()` term of one predictor, in brms's numbering
#'
#' brms numbers the Gaussian processes of a predictor in the order its
#' formula writes them: the `i`-th `gp()` call is the term whose draws
#' are `sdgp_<i>`. A term that sets no `cov` takes brms's own default.
#'
#' @param rhs The formula the predictor was written as
#' @return Character vector of kernel names, one per `gp()` term
#' @noRd
gp_kernels <- function(rhs) {
  calls <- formula_calls(rhs[[length(rhs)]], "gp")
  vapply(calls, function(call) gp_call_to_spec(call)$cov, character(1L))
}


#' Approximate Gaussian process terms
#'
#' Each term reads its basis `Xgp`, its eigenvalues `slambda`, its
#' marginal standard deviation `sdgp`, its length scales `lscale` and
#' its basis coefficients `zgp`, all under `<sfx>_<i>`. A term with a
#' factor `by` holds one of each per level, suffixed by the level, and
#' places that level's rows with `Igp`. `Jgp` maps rows onto the unique
#' covariate values the basis was built on, and `Cgp` scales a term by a
#' continuous `by`.
#'
#' @inheritParams fixed_pred
#' @param kernels What `gp_kernels()` gives for this predictor
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
gp_pred <- function(draws, sdata, sfx, kernels, n_obs) {
  eta <- matrix(0, nrow(draws), n_obs)
  sd_pattern <- paste0("^sdgp", sfx, "_([0-9]+)\\[[0-9]+\\]$")
  sd_cols <- grep(sd_pattern, colnames(draws), value = TRUE)
  for (i in sort(unique(as.integer(sub(sd_pattern, "\\1", sd_cols))))) {
    stem <- paste0(sfx, "_", i)
    kernel <- kernels[i]
    if (is.na(kernel)) {
      stop_mvgam_fault(
        "A Gaussian process term has no gp() call in its formula.",
        paste0("Term ", i, " of the predictor suffixed '", sfx, "'.")
      )
    }
    n_levels <- sum(startsWith(sd_cols, paste0("sdgp", stem, "[")))
    if (n_levels == 1L) {
      eta <- eta + gp_level_pred(draws, sdata, stem, 1L, stem, kernel)
      next
    }
    for (g in seq_len(n_levels)) {
      rows <- sdata_entry(sdata, paste0("Igp", stem, "_", g))
      # A level absent from the prediction data has no rows to fill.
      if (length(rows) == 0L) {
        next
      }
      eta[, rows] <- eta[, rows] +
        gp_level_pred(draws, sdata, stem, g, paste0(stem, "_", g), kernel)
    }
  }
  eta
}


#' One Gaussian process, or one level of a `by` factor, at its rows
#'
#' @inheritParams fixed_pred
#' @param stem The term's `<sfx>_<i>`
#' @param level The level's position in `sdgp` and `lscale`
#' @param data_stem The name its data and `zgp` coefficients carry:
#'   `stem`, or `<stem>_<level>` for a level of a `by` factor
#' @param kernel The term's kernel
#' @return A `[ndraws x nrow]` matrix, one column per row the term covers
#' @noRd
gp_level_pred <- function(draws, sdata, stem, level, data_stem, kernel) {
  Xgp <- sdata_entry(sdata, paste0("Xgp", data_stem))
  lscale_cols <- grep(paste0("^lscale", stem, "\\[", level, ","),
                      colnames(draws), value = TRUE)
  out <- approx_gp_pred(
    Xgp = Xgp,
    slambda = sdata_entry(sdata, paste0("slambda", data_stem)),
    zgp = draw_columns(draws, paste0("zgp", data_stem, "[",
                                     seq_len(ncol(Xgp)), "]")),
    sdgp = draw_columns(draws, paste0("sdgp", stem, "[", level, "]"))[, 1L],
    lscale = draw_columns(draws, lscale_cols),
    kernel = kernel
  )
  Jgp <- sdata[[paste0("Jgp", data_stem)]]
  if (!is.null(Jgp)) {
    out <- out[, as.integer(Jgp), drop = FALSE]
  }
  Cgp <- sdata[[paste0("Cgp", data_stem)]]
  if (!is.null(Cgp)) {
    checkmate::assert_numeric(as.vector(Cgp), len = ncol(out))
    out <- out * matrix(Cgp, nrow(out), ncol(out), byrow = TRUE)
  }
  out
}


#' Monotonic terms
#'
#' A `mo()` term is `bsp * D * sum(simo[1:x])` at the ordered level `x`
#' of a variable with `D + 1` levels. brms numbers the special terms of a
#' predictor in `bsp` and the monotonic variables in `Xmo` and `simo`,
#' and the two agree only while every special term is a single `mo()`.
#' An interaction of `mo()` terms, a covariate multiplying one, or an
#' `me()` or `mi()` term breaks that and is refused.
#'
#' @inheritParams fixed_pred
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
monotonic_pred <- function(draws, sdata, sfx, n_obs) {
  eta <- matrix(0, nrow(draws), n_obs)
  n_sp <- sdata[[paste0("Ksp", sfx)]] %||% 0L
  if (n_sp == 0L) {
    return(eta)
  }
  n_mo <- sdata[[paste0("Imo", sfx)]] %||% 0L
  # A covariate multiplying a special term, as in `mo(x):z`, is `Csp`.
  n_csp <- length(term_numbers(names(sdata), paste0("Csp", sfx)))
  if (n_mo != n_sp || n_csp > 0L) {
    stop(insight::format_error(c(
      "mvgam predicts from 'mo()' terms that stand alone.",
      x = paste0("This predictor has ", n_sp, " special term",
                 if (n_sp != 1L) "s", " and ", n_mo, " monotonic ",
                 "variable", if (n_mo != 1L) "s", "."),
      i = paste0("Interactions involving 'mo()' and 'me()' or 'mi()' ",
                 "terms are not supported.")
    )), call. = FALSE)
  }
  bsp <- draw_columns(draws, paste0("bsp", sfx, "[", seq_len(n_sp), "]"))
  n_cats <- sdata_entry(sdata, paste0("Jmo", sfx))
  for (i in seq_len(n_mo)) {
    stem <- paste0(sfx, "_", i)
    D <- n_cats[i]
    simo <- draw_columns(draws, paste0("simo", stem, "[", seq_len(D), "]"))
    x <- validate_monotonic_indices(
      sdata_entry(sdata, paste0("Xmo", stem)), paste0("Xmo", stem), D, n_obs
    )
    # The share of the full effect reached at each level, 0 at the first.
    cumulative <- matrix(0, nrow(simo), D + 1L)
    for (k in seq_len(D)) {
      cumulative[, k + 1L] <- cumulative[, k] + simo[, k]
    }
    eta <- eta + bsp[, i] * D * cumulative[, x + 1L, drop = FALSE]
  }
  eta
}


#' A non-linear mean, evaluated at its non-linear parameters
#'
#' The mean is the formula's expression. Each name in it that has a
#' formula of its own is a non-linear parameter, and is the linear
#' predictor that formula describes; every other name is a covariate,
#' which brms passes as `C_<i>` in the order the expression uses them.
#'
#' @inheritParams linear_terms_pred
#' @param form The response's `brmsformula`
#' @return A `[ndraws x n_obs]` matrix
#' @noRd
nonlinear_linpred <- function(prep, draws, form, resp, n_obs) {
  expr <- form$formula[[3L]]
  nlpars <- nonlinear_parameters(form)
  covariates <- setdiff(all.vars(expr), nlpars)
  env <- lapply(stats::setNames(nm = nlpars), function(p) {
    linear_terms_pred(prep, draws, form$pforms[[p]], resp, NULL, p, n_obs)
  })
  for (i in seq_along(covariates)) {
    values <- sdata_entry(prep$sdata,
                          paste0("C", predictor_suffix(resp), "_", i))
    checkmate::assert_numeric(as.vector(values), len = n_obs)
    env[[covariates[i]]] <- matrix(values, nrow(draws), n_obs, byrow = TRUE)
  }
  mu <- eval(expr, envir = env)
  if (!is.matrix(mu) || !identical(dim(mu), c(nrow(draws), n_obs))) {
    stop(insight::format_error(c(
      "The non-linear formula did not give one value per draw and row.",
      x = paste0("Expected ", nrow(draws), " x ", n_obs, "; got ",
                 paste(dim(as.matrix(mu)), collapse = " x "), "."),
      i = "Write the expression with element-wise operators."
    )), call. = FALSE)
  }
  mu
}
