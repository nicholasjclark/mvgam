#' @title Extract posterior draws from fitted \pkg{mvgam} objects
#'
#' @name mvgam_draws
#'
#' @description
#' Extract posterior draws as a data frame, matrix or array, or as one
#' of the `posterior::draws_*` classes. Parameters take their brms
#' names, such as `b_x` and `sd_grp__Intercept`. A trend parameter
#' indexed by series takes the series name, as in
#' `sigma_trend[series_1]`. The
#' `r mvgam_article("idm", "integrated data model article")` contrasts
#' intercepts drawn with `as_draws_df()`.
#'
#' @param x A fitted `mvgam` object.
#' @param variable Either a keyword shortcut, a character vector of
#'   parameter names, or (when `regex = TRUE`) a vector of regular
#'   expressions. Recognised keywords:
#'   \itemize{
#'     \item `"betas"`: population-level coefficients, the set
#'       [brms::fixef()] reports (`b_*`, `bs_*`, `bsp_*` and `bcs_*`).
#'     \item `"obs_params"`: observation-family distributional
#'       parameters (`sigma`, `phi`, `shape`, ...).
#'     \item `"smooth_params"`: smooth-term standard deviations
#'       (`sds_*`).
#'     \item `"trend_betas"`: the trend formula's population-level
#'       coefficients (`b_*_trend`, `bs_*_trend` and `bsp_*_trend`).
#'     \item `"trend_params"`: trend-dynamics parameters
#'       (autoregressive coefficients, innovation SDs, ...).
#'     \item `"trend_smooth_params"`: trend-side smooth SDs.
#'   }
#'   `NULL` (the default) returns every parameter. A bare vector name,
#'   such as `"sigma_trend"`, selects every element. Group-level
#'   effects, factor loadings and latent states have no keyword. Select
#'   them by name. [variables.mvgam()] lists the names a fit holds.
#' @param regex Logical. When `TRUE`, each entry of `variable` is
#'   treated as a regular expression and any parameter matching at
#'   least one pattern is selected. Keyword shortcuts are still
#'   honoured. Defaults to `FALSE`.
#' @param draw Optional positive integer indices of the draws to keep,
#'   counted across chains. `NULL` (the default) keeps every draw.
#' @param inc_warmup Logical. Include warmup draws? Defaults to
#'   `FALSE`. A fit holds them only when sampled with
#'   `save_warmup = TRUE`.
#' @param row.names,optional Ignored.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `data.frame`, `matrix`, `array`, or one of the
#'   `posterior::draws_*` classes (`draws_matrix`, `draws_array`,
#'   `draws_df`, `draws_list`, `draws_rvars`) depending on the method
#'   dispatched.
#'
#' @seealso
#'   [mvgam_diagnostics] for higher-level summaries derived from
#'   these draws (`rhat`, `neff_ratio`, `fixef`, `bayes_R2`,
#'   `posterior_summary`, ...),
#'   [variables.mvgam()] to list every parameter name in the fit,
#'   [mcmc_plot.mvgam()] for diagnostic visualisations,
#'   [summary.mvgam()] for a printed model summary,
#'   [tidy.mvgam()] for a tibble of parameter summaries,
#'   [posterior_epred.mvgam()], [posterior_linpred.mvgam()],
#'   [posterior_predict.mvgam()] for prediction-scale outputs,
#'   [posterior::draws] for the underlying draws classes.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Trend-dynamics parameters as a data frame of draws
#' head(as.data.frame(mod, variable = "trend_params"))
#'
#' # Regular expressions select by pattern, here the smooth's basis
#' # coefficients and its smoothing SD
#' colnames(as.matrix(mod, variable = c("^s_", "^sds_"), regex = TRUE))
#'
#' # One random variable per parameter, printed as mean and SD
#' as_draws_rvars(mod, variable = "trend_params")
#'
#' # Next, mcmc_plot(mod) plots the same draws
#' }
#'
#' @author Nicholas J Clark
#'
#' @family posterior summaries
NULL


# The keyword shortcuts `variable =` accepts, defined once.
mvgam_keyword_shortcuts <- c(
  "betas", "obs_params", "smooth_params",
  "trend_betas", "trend_params", "trend_smooth_params"
)


#' The parameter names a keyword shortcut selects
#'
#' Each keyword is a pair of side and kind from the parameter taxonomy.
#' `smooth_params` selects the smoothing standard deviations alone. The
#' smooth group `tidy()` reports also holds the basis coefficients and
#' the Gaussian-process hyperparameters.
#'
#' @param keyword One of `mvgam_keyword_shortcuts`
#' @param all_vars Character vector of user-facing parameter names
#' @return Character vector of the names the keyword selects
#' @noRd
resolve_mvgam_keyword <- function(keyword, all_vars) {
  checkmate::assert_choice(keyword, mvgam_keyword_shortcuts)
  checkmate::assert_character(all_vars)
  kind <- mvgam_par_kind(all_vars)
  side <- mvgam_par_side(all_vars)
  pick <- function(k, sd) all_vars[kind %in% k & side == sd]
  switch(keyword,
    "betas" = pick(c("beta", "basis"), "observation"),
    "trend_betas" = pick(c("beta", "basis"), "trend"),
    "obs_params" = pick("family", "observation"),
    "smooth_params" = pick("smooth_sd", "observation"),
    "trend_smooth_params" = pick("smooth_sd", "trend"),
    "trend_params" = pick("dynamics", "trend")
  )
}


#' The brms names of the population-level coefficients
#'
#' After a fit, the internal brms function `rename_pars()` turns `b[k]`
#' into `b_<term>`, with `<term>` the `k`th non-intercept column of the
#' design matrix. A trend formula maps `b_trend[k]` to `b_<term>_trend`
#' over `standata$X_trend`. Every method that goes through
#' `extract_mvgam_draws()` then sees the brms names.
#'
#' @param x Fitted `mvgam` object
#' @return Named character vector mapping brms names to Stan names
#' @noRd
mvgam_beta_aliases <- function(x) {
  checkmate::assert_class(x, "mvgam")
  # brms's stancode centres the design matrix whenever an Intercept
  # column is present: `b[k]` then enumerates the K - 1 non-Intercept
  # columns. Without an Intercept the mapping is direct. Every linear
  # predictor gets its own design-matrix block in standata, named by
  # the suffix `model_predictors()` gives it:
  #   `X`             obs main formula  -> b[k]            -> b_<term>
  #   `X_<resp>`      MV response       -> b_<resp>[k]     -> b_<resp>_<term>
  #   `X_<dpar>`      dpar formula      -> b_<dpar>[k]     -> b_<dpar>_<term>
  #   `X_<nlpar>`     nl sub-formula    -> b_<nlpar>[k]    -> b_<nlpar>_<term>
  #   `X_trend`       mvgam-only        -> b_trend[k]      -> b_<term>_trend
  # The trend block takes a name suffix and every other block a
  # prefix. brms does not centre a non-linear sub-formula's design:
  # the columns of `X_<nlpar>` match `b_<nlpar>` one to one, with the
  # Intercept at position 1. `strip_intercept = FALSE` states that.
  build <- function(X, pos_prefix, alias_prefix, alias_suffix,
                    strip_intercept = TRUE) {
    if (is.null(X) || !is.matrix(X) || ncol(X) == 0L) {
      return(character(0L))
    }
    cn <- colnames(X)
    if (strip_intercept && identical(cn[1L], "Intercept")) {
      cn <- cn[-1L]
    }
    if (length(cn) == 0L) {
      return(character(0L))
    }
    new <- paste0(alias_prefix, cn, alias_suffix)
    old <- paste0(pos_prefix, "[", seq_along(cn), "]")
    stats::setNames(old, new)
  }
  sdata <- x$standata
  obs <- lapply(model_predictors(side_model(x, "obs")$formula), function(lp) {
    p <- lp$suffix
    c(
      build(sdata[[paste0("X", p)]], paste0("b", p),
            paste0("b", p, "_"), "", strip_intercept = is.null(lp$nlpar)),
      # The unpenalised part of each smooth: `bs<p>[k]` maps to
      # `bs<p>_<colname>`. brms keeps no intercept column in `Xs`.
      build(sdata[[paste0("Xs", p)]], paste0("bs", p), paste0("bs", p, "_"),
            "", strip_intercept = FALSE),
      cs_aliases(sdata[[paste0("Xcs", p)]], sdata[[paste0("nthres", p)]], p)
    )
  })
  # A slope that varies by factor is reported in the identified factor
  # basis
  rotated <- isTRUE(x$trend_metadata$has_by_lv) &&
    identical(fit_factor_identification(x), "rotation")
  c(unlist(obs),
    build(sdata$X_trend, if (rotated) "b_trend_tilde" else "b_trend", "b_",
          "_trend"),
    build(sdata$Xs_trend, "bs_trend", "bs_", "_trend",
          strip_intercept = FALSE))
}


#' The brms names of a predictor's category-specific coefficients
#'
#' `bcs<p>[j, k]` is column `j` of `Xcs<p>` at threshold `k`, which
#' brms names `bcs<p>_<column>[k]`.
#'
#' @param Xcs The predictor's category-specific design, or `NULL`
#' @param n_thres The response's number of thresholds, or `NULL`
#' @param p The predictor's suffix
#' @return Named character vector mapping brms names to Stan names
#' @noRd
cs_aliases <- function(Xcs, n_thres, p) {
  if (is.null(Xcs) || ncol(Xcs) == 0L || is.null(n_thres)) {
    return(character(0L))
  }
  coefs <- cs_coef_frame(colnames(Xcs), n_thres)
  j <- match(coefs$var, colnames(Xcs))
  stats::setNames(paste0("bcs", p, "[", j, ",", coefs$threshold, "]"),
                  paste0("bcs", p, "_", coefs$name))
}


#' The brms names of the smoothing SDs and basis coefficients
#'
#' The brms Stan code numbers each smooth object. `sds<p>_<i>[k]` is the
#' SD of its `k`th penalty and `s<p>_<i>_<k>[m]` the `m`th coefficient
#' under that penalty. The internal brms function `rename_sm()` names
#' the object by its label, as `sds<p>_<label>_<k>` and
#' `s<p>_<label>_<k>[m]`. A trend-side name takes `_trend` before its
#' index.
#'
#' @param x Fitted `mvgam` object
#' @param raw Character vector of the Stan names in the posterior
#' @return Named character vector mapping brms names to Stan names
#' @noRd
mvgam_smooth_aliases <- function(x, raw) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(raw)
  # `mvgam_smooth_terms()` rebuilds each side's Stan data. A fit
  # without a smoothing standard deviation has no smooth to name.
  if (!any(startsWith(raw, "sds_"))) {
    return(character(0L))
  }
  parts <- lapply(mvgam_smooth_terms(x), function(t) {
    p <- predictor_suffix(t$resp, t$dpar, t$nlpar)
    sfx <- if (identical(t$side, "trend")) "_trend" else ""
    labels <- brms_smooth_labels(t)
    unlist(Map(function(i, label) {
      sds_old <- raw[startsWith(raw, paste0("sds", p, "_", i, sfx, "["))]
      sds_new <- paste0("sds", p, "_", label, "_", stan_index(sds_old), sfx)
      s_old <- raw[grepl(
        paste0("^s", p, "_", i, "_[0-9]+", sfx, "\\["), raw
      )]
      k <- sub(paste0("^s", p, "_", i, "_([0-9]+).*$"), "\\1", s_old)
      s_new <- paste0("s", p, "_", label, "_", k, sfx,
                      "[", stan_index(s_old), "]")
      stats::setNames(c(sds_old, s_old), c(sds_new, s_new))
    }, t$objects, labels))
  })
  unlist(parts)
}


#' The brms names of Gaussian-process and special terms
#'
#' The Stan names number these by position within each predictor `<p>`.
#' The brms names name them by term.
#' * `sdgp<p>_<i>[k]` becomes `sdgp<p>_<label k>`
#' * `lscale<p>_<i>[k, d]` becomes `lscale<p>_<label k><covariate d>`,
#'   or `lscale<p>_<label k>` when isotropic
#' * `zgp<p>_<i>[_j][m]` becomes `zgp<p>_<label j>[m]`
#' * `bsp<p>[k]` becomes `bsp<p>_<coef k>`, one per column of the
#'   special terms' design
#' * `simo<p>_<i>[m]` becomes `simo<p>_<coef><n>[m]` for the `n`-th
#'   `mo()` call of a coefficient
#'
#' A GP label joins `gp`, its covariates, its `by` variable and each
#' level of a `by` factor. A trend-side name takes `_trend` before its
#' index.
#'
#' @param x Fitted `mvgam` object
#' @param raw Character vector of the Stan names in the posterior
#' @return Named character vector mapping brms names to Stan names
#' @noRd
mvgam_special_aliases <- function(x, raw) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(raw)
  if (!any(grepl("^(sdgp|lscale|zgp|bsp|simo)(_|\\[)", raw))) {
    return(character(0L))
  }
  rename_block <- function(old_prefix, new_names, sfx) {
    old <- raw[startsWith(raw, paste0(old_prefix, sfx, "["))]
    if (length(old) == 0L) {
      return(character(0L))
    }
    if (length(old) != length(new_names)) {
      stop_mvgam_fault(
        "Each draw of a parameter block must have one brms name.",
        paste0("'", old_prefix, sfx, "' has ", length(old), " draws and ",
               length(new_names), " names.")
      )
    }
    stats::setNames(old, paste0(new_names, sfx))
  }
  rename_vector <- function(old_prefix, new_prefix, sfx) {
    old <- raw[startsWith(raw, paste0(old_prefix, sfx, "["))]
    stats::setNames(old, paste0(new_prefix, sfx, "[", stan_index(old), "]"))
  }
  out <- character(0L)
  for (side in c("obs", "trend")) {
    model <- side_model(x, side)
    if (is.null(model)) next
    sfx <- if (identical(side, "trend")) "_trend" else ""
    for (lp in model_predictors(model$formula)) {
      p <- lp$suffix
      gp_terms <- brms_gp_terms(lp$pred[["gp"]])
      for (i in seq_along(gp_terms)) {
        g <- gp_terms[[i]]
        labels <- brms_gp_labels(g, model$data)
        cells <- if (isTRUE(g$iso)) labels else {
          as.vector(outer(labels, g$term, paste0))
        }
        out <- c(
          out,
          rename_block(paste0("sdgp", p, "_", i),
                       paste0("sdgp", p, "_", labels), sfx),
          rename_block(paste0("lscale", p, "_", i),
                       paste0("lscale", p, "_", cells), sfx)
        )
        zgp <- if (length(labels) > 1L) {
          Map(function(j, lab) {
            rename_vector(paste0("zgp", p, "_", i, "_", j),
                          paste0("zgp", p, "_", lab), sfx)
          }, seq_along(labels), labels)
        } else {
          list(rename_vector(paste0("zgp", p, "_", i),
                             paste0("zgp", p, "_", labels), sfx))
        }
        out <- c(out, unlist(zgp))
      }
      special <- special_term_frame(lp$pred, model$data)
      if (NROW(special) > 0L) {
        out <- c(out, rename_block(paste0("bsp", p),
                                   paste0("bsp", p, "_", special$coef), sfx))
        # `Imo` numbers each coefficient's `mo()` calls in turn. A
        # coefficient without one has no simplex.
        simo <- unlist(Map(function(coef, mo) {
          if (length(mo)) paste0(coef, seq_along(mo))
        }, special$coef, special$Imo), use.names = FALSE)
        for (i in seq_along(simo)) {
          out <- c(out, rename_vector(paste0("simo", p, "_", i),
                                      paste0("simo", p, "_", simo[i]), sfx))
        }
      }
    }
  }
  out
}


#' The label of each Gaussian process a `gp()` term fits
#'
#' The label joins
#' `gp`, the covariates and the `by` variable. A `by` factor fits one
#' process per column of its cell-means design, labelled by level.
#'
#' @param g Object `brms::gp()` returns for the term
#' @param data Data frame the model was fitted to
#' @return Character vector with one label per process
#' @noRd
brms_gp_labels <- function(g, data) {
  checkmate::assert_list(g)
  checkmate::assert_data_frame(data)
  label <- paste0("gp", brms_rename(paste(g$term, collapse = "")))
  if (identical(g$by, "NA")) {
    return(label)
  }
  label <- paste0(label, brms_rename(g$by))
  byval <- data[[g$by]]
  if (!is.factor(byval) && !is.character(byval) && !is.logical(byval)) {
    return(label)
  }
  byval <- unique(as.factor(byval))
  design <- stats::model.matrix(
    if (isTRUE(g$cmc)) ~ 0 + byval else ~ 1 + byval
  )
  levels <- gsub("[ \t\r\n]+", "", sub("^byval", "",
                                          brms_rename(colnames(design))))
  paste0(label, levels)
}


#' The term labels of a component formula
#'
#' @param form Formula, or any other object for none
#' @return Character vector of term labels, empty for a non-formula
#' @noRd
formula_term_labels <- function(form) {
  if (!inherits(form, "formula")) {
    return(character(0L))
  }
  attr(stats::terms(form), "term.labels")
}


#' The term calls of a component formula
#'
#' @inheritParams formula_term_labels
#' @return List of language objects, one per term label and named by
#'   it, empty for a non-formula
#' @noRd
formula_term_calls <- function(form) {
  labels <- formula_term_labels(form)
  if (length(labels) == 0L) {
    return(list())
  }
  stats::setNames(lapply(labels, str2lang), labels)
}


#' What `brms::gp()` gives for each `gp()` term of a component formula
#'
#' Each term is evaluated in the formula's environment, where a setting
#' such as `k = kk` finds its object.
#'
#' @param gp_form The `gp` entry of a predictor's `brmsterms`, or
#'   `NULL`
#' @return Named list, one entry per term, named by its label
#' @noRd
brms_gp_terms <- function(gp_form) {
  lapply(formula_term_calls(gp_form), eval, envir = list(gp = brms::gp),
         enclos = environment(gp_form) %||% baseenv())
}


#' The indices a Stan element name carries
#'
#' @param v Character vector of Stan element names, all with the same
#'   number of indices, such as `"Sigma_trend[2,1]"`
#' @return Character matrix with one row per name and one column per
#'   index
#' @noRd
stan_indices <- function(v) {
  checkmate::assert_character(v)
  if (!length(v)) return(matrix(character(0L), 0L, 0L))
  do.call(rbind, strsplit(sub("^.*\\[(.*)\\]$", "\\1", v), ",",
                          fixed = TRUE))
}


#' The index a one-dimensional Stan element name carries
#'
#' @param v Character vector of Stan element names, such as `"x[3]"`
#' @return Character vector of indices, such as `"3"`
#' @noRd
stan_index <- function(v) {
  checkmate::assert_character(v)
  if (!length(v)) return(character(0L))
  stan_indices(v)[, 1L]
}


#' Names for the parameters a Stan index alone labels
#'
#' brms labels an index that runs over a factor's levels with the
#' level, as in `r_<group>[level,term]`. The same holds here. Each row
#' of the loadings `Z[i, k]` and `Z_tilde[i, k]` is a series and takes
#' the series' name. In a fit without factors, a trend dimension of
#' `N_lv_trend` or `N_series_trend` runs over the series.
#' `sigma_trend[k]` then takes the name of series `k`, as do both
#' indices of `Sigma_trend[i,j]` and the second of the changepoint
#' matrix `delta_trend[c,k]`. A VAR's `Phi_trend[l,i,j]` keeps its lag
#' `l` and names `i` and `j`. A variable with a time dimension, such as
#' the states `lv_trend[t,k]`, keeps its numeric indices.
#' `theta_features[k]` is the length scale of the `k`th column of the
#' encoded features matrix. It takes that column's name, as each
#' distance length scale is named `theta_dist_<name>`.
#'
#' @param x Fitted `mvgam` object
#' @param raw Character vector of the Stan names in the posterior
#' @return Named character vector mapping user names to Stan names
#' @noRd
mvgam_index_aliases <- function(x, raw) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(raw)
  out <- character(0L)
  theta <- raw[startsWith(raw, "theta_features[")]
  features <- colnames(x$standata$row_features)
  if (length(theta) > 0L && length(theta) == length(features)) {
    theta <- theta[order(as.integer(stan_index(theta)))]
    out <- stats::setNames(theta, paste0("theta_features_", features))
  }
  series <- fitted_series_levels(x)
  loadings <- grep("^Z(_tilde)?\\[[0-9]+,[0-9]+\\]$", raw, value = TRUE)
  idx <- stan_indices(loadings)
  if (length(loadings) > 0L && max(as.integer(idx[, 1L])) <= length(series)) {
    named <- paste0(sub("\\[.*$", "", loadings), "[",
                    series[as.integer(idx[, 1L])], ",", idx[, 2L], "]")
    out <- c(out, stats::setNames(loadings, named))
  }
  # A trend-free fit has no series dimensions to name
  if (length(series) == 0L || is.null(x$stancode) ||
        !is.null(detect_factor_n_lv(x, length(series)))) {
    return(out)
  }
  declared <- stancode_declarations(paste(x$stancode, collapse = "\n"))
  square <- c("cov_matrix", "corr_matrix", "cholesky_factor_corr",
              "cholesky_factor_cov")
  split_dims <- function(size) {
    trimws(strsplit(size, ",", fixed = TRUE)[[1L]])
  }
  for (nm in names(declared)) {
    d <- declared[[nm]]
    dims <- split_dims(d$size)
    if (d$type %in% square) dims <- rep(dims, 2L)
    dims <- c(split_dims(d$array), dims)
    on_series <- dims %in% c("N_lv_trend", "N_series_trend")
    if (!any(on_series) || any(startsWith(dims, "N_time"))) next
    el <- raw[startsWith(raw, paste0(nm, "["))]
    if (length(el) == 0L) next
    idx <- stan_indices(el)
    if (ncol(idx) != length(dims) ||
          max(as.integer(idx[, on_series])) > length(series)) next
    idx[, on_series] <- series[as.integer(idx[, on_series])]
    named <- paste0(nm, "[", apply(idx, 1L, paste, collapse = ","), "]")
    out <- c(out, stats::setNames(el, named))
  }
  out
}


#' The label brms gives each smooth object of one term
#'
#' Follows the internal brms function `frame_sm()`. The label joins the
#' covariates with the `by` variable and strips the punctuation that
#' `brms_rename()` strips. It then takes the smooth function as a prefix
#' and the object's `by` level as a suffix. `s(time, by = series)`
#' labels its objects `stimeseriesDM`, `stimeseriesDO` and onward.
#'
#' @param term One entry of `mvgam_smooth_terms()`
#' @return Character vector with one label per smooth object
#' @noRd
brms_smooth_labels <- function(term) {
  checkmate::assert_list(term)
  sfun <- sub("\\(.*$", "", term$term)
  vars <- c(term$covars, stats::na.omit(term$by_var))
  base <- paste0(sfun, brms_rename(paste(vars, collapse = "")))
  if (is.null(term$bylevels)) {
    return(base)
  }
  paste0(base, brms_rename(gsub("[ \t\r\n]+", "", term$bylevels)))
}


#' A term rewritten for use in a parameter name
#'
#' Applies the default substitutions of the internal brms function
#' `rename()`.
#'
#' @param x Character vector of terms
#' @return Character vector of renamed terms
#' @noRd
brms_rename <- function(x) {
  checkmate::assert_character(x)
  from <- c(" ", "(", ")", "[", "]", ",", "\"", "'", "?",
            "+", "-", "*", "/", "^", "=", "$")
  to <- c(rep("", 9L), "P", "M", "MU", "D", "E", "EQ", "USD")
  for (i in seq_along(from)) {
    x <- gsub(from[i], to[i], x, fixed = TRUE)
  }
  x
}


#' Stan parameter names replaced by their brms names
#'
#' Names absent from the map are unchanged.
#'
#' @param vars Character vector of Stan parameter names
#' @param alias_map Named character vector mapping brms names to Stan
#'   names, as the `mvgam_*_aliases()` builders return
#' @return Character vector shaped as `vars`
#' @noRd
apply_mvgam_aliases <- function(vars, alias_map) {
  checkmate::assert_character(vars)
  checkmate::assert_character(alias_map)
  if (length(alias_map) == 0L) {
    return(vars)
  }
  idx <- match(alias_map, vars)
  has <- !is.na(idx)
  if (any(has)) {
    vars[idx[has]] <- names(alias_map)[has]
  }
  vars
}


#' The user-facing names of a fit's parameters
#'
#' `variables.mvgam()`, `extract_mvgam_draws()`, `summary()` and
#' `tidy.mvgam()` all take their names from here. With one source they
#' agree on which parameters exist and on their names, such as `b_x`
#' for the positional `b[1]`. The empty-observation placeholder is
#' dropped. Unless `all = TRUE`, the working arrays, the accumulators
#' and the hidden forms are dropped too.
#'
#' @param x Fitted `mvgam` object
#' @param pars Character vector of Stan names to project, or `NULL` for
#'   every name in the posterior
#' @param all Logical flag to keep every parameter, skipping the
#'   exclusion list and the rotation-indeterminate filter. Only a caller
#'   whose user named a parameter sets it.
#' @return Named character vector mapping user-facing names to Stan
#'   names
#' @noRd
mvgam_user_pars <- function(x, pars = NULL, all = FALSE) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_flag(all)
  checkmate::assert_character(pars, null.ok = TRUE)
  raw <- pars %||% fit_variables(x$fit)
  user <- apply_mvgam_aliases(
    raw, c(mvgam_beta_aliases(x), mvgam_ranef_aliases(x),
           mvgam_smooth_aliases(x, raw), mvgam_special_aliases(x, raw),
           mvgam_index_aliases(x, raw))
  )
  # The empty-observation placeholder is a structural column mvgam
  # adds for a design brms cannot build. The user wrote no such
  # parameter. brms names its coefficient `b_<coef>`, or
  # `b_<resp>_<coef>` in one response of a multivariate formula.
  keep <- !endsWith(user, paste0("_", MVGAM_EMPTY_OBS_PLACEHOLDER))
  # Stan's own working arrays are held out of the parameter set this
  # projection returns. The same rule holds out `lprior` and `lp__`,
  # of kind `bookkeeping`: the taxonomy records that no summary
  # claims them.
  # A caller that named a parameter sets `all`, which reaches the
  # working arrays and the accumulators together with the hidden
  # block. Naming one is how a user retrieves a quantity the
  # default view leaves out.
  if (!all) {
    keep <- keep &
      !(mvgam_par_kind(user) %in% c("internal", "bookkeeping"))
    keep <- keep & !is_hidden_par(user)
    # A symmetric matrix lists each off-diagonal cell once. The test
    # takes the positional Stan indices, which the series aliases
    # replace.
    sym <- grepl(MVGAM_PAR_SYMMETRIC_PATTERN, raw)
    if (any(sym)) {
      stems <- sub("\\[.*$", "", raw[sym])
      lower <- lapply(split(raw[sym], stems), function(v) {
        idx <- stan_indices(v)
        n <- ncol(idx)
        as.integer(idx[, n - 1L]) >= as.integer(idx[, n])
      })
      keep[sym] <- keep[sym] & unsplit(lower, stems)
    }
  }
  raw <- raw[keep]
  user <- user[keep]
  # brms orders a fitted object's parameters by class. Every reader of
  # this projection inherits that order.
  ord <- mvgam_par_order(user)
  stats::setNames(raw[ord], user[ord])
}


#' The brms group-level metadata of one side of a fit
#'
#' Each side stores a lightweight brmsfit holding the `ranef` table,
#' built at model setup. `mvgam_ranef_aliases()`, `ranef.mvgam()` and
#' `VarCorr.mvgam()` take their names from here. Each row gains the
#' three name parts brms builds. `row_prefix` is the predictor the row
#' enters, from `make_row_prefix()`. `alias_key` is the group with that
#' prefix, as in `r_g__sigma_y1[a,Intercept]`. `coef_alias` is the
#' coefficient with that prefix, as in `sd_g__sigma_y1_Intercept`.
#' Whitespace in a level becomes `.`, as the internal brms function
#' `rename_re_levels()` writes it.
#'
#' @param x Fitted `mvgam` object
#' @param side Character side, `"observation"` or `"trend"`
#' @return List holding the `reframe` table and the named list of
#'   `group_levels`, or `NULL` for a side without group-level effects
#' @noRd
mvgam_ranef_metadata <- function(x,
                                 side = c("observation", "trend")) {
  checkmate::assert_class(x, "mvgam")
  side <- match.arg(side)
  reframe <- if (identical(side, "trend")) {
    x$trend_model$ranef
  } else {
    x$obs_model$ranef
  }
  if (NROW(reframe) == 0L) {
    return(NULL)
  }
  group_levels <- lapply(attr(reframe, "levels"), function(lv) {
    gsub("[ \t\r\n]", ".", lv)
  })
  prefix <- make_row_prefix(reframe$nlpar, reframe$dpar, reframe$resp)
  has_prefix <- nzchar(prefix)
  reframe$row_prefix <- prefix
  reframe$alias_key <- ifelse(
    has_prefix, paste0(reframe$group, "__", prefix), reframe$group
  )
  reframe$coef_alias <- ifelse(
    has_prefix, paste0(prefix, "_", reframe$coef), reframe$coef
  )
  list(reframe = reframe, group_levels = group_levels)
}


#' The brms names of the group-level parameters
#'
#' The brms Stan code names group-level parameters by position, as
#' `r_<id>[<level>,<coef>]`, `sd_<id>[<coef>]` and `cor_<id>[<k>]`. The
#' internal brms function `rename_pars()` names them by group and
#' coefficient, as `r_<group>[<level>,<coef>]`, `sd_<group>__<coef>` and
#' `cor_<group>__<coef1>__<coef2>`. mvgam does not call
#' `rename_pars()`. This function builds the same map for both sides.
#' Multivariate responses, distributional parameters, nesting and
#' `gr(g, by = f)` all arrive through the same `ranef` table.
#'
#' @param x Fitted `mvgam` object
#' @return Named character vector mapping brms names to Stan names,
#'   empty for a fit without group-level effects
#' @noRd
mvgam_ranef_aliases <- function(x) {
  checkmate::assert_class(x, "mvgam")
  c(mvgam_ranef_aliases_side(x, "observation"),
    mvgam_ranef_aliases_side(x, "trend"))
}


#' The brms names of one side's group-level parameters
#'
#' A trend-side random effect has the observation-side positional names
#' with `_trend` after the id, as in `sd_1_trend[1]`. Its brms name
#' takes the `_trend` suffix, as in `sd_grp__Intercept_trend`.
#'
#' @param x Fitted `mvgam` object
#' @param side Character side, `"observation"` or `"trend"`
#' @return Named character vector mapping brms names to Stan names
#' @noRd
mvgam_ranef_aliases_side <- function(x,
                                     side = c("observation", "trend")) {
  checkmate::assert_class(x, "mvgam")
  side <- match.arg(side)
  meta <- mvgam_ranef_metadata(x, side)
  if (is.null(meta)) {
    return(character(0L))
  }
  pos_sfx <- if (identical(side, "trend")) "_trend" else ""
  alias_sfx <- pos_sfx
  reframe <- meta$reframe
  group_levels <- meta$group_levels
  ids <- unique(reframe$id)
  parts <- lapply(ids, function(id) {
    rows <- reframe[reframe$id == id, , drop = FALSE]
    group <- rows$group[1L]
    coefs <- rows$coef
    levels <- group_levels[[group]]
    if (is.null(levels) || length(coefs) == 0L) {
      return(character(0L))
    }
    n_lvl <- length(levels)
    n_coef <- length(coefs)
    has_cor <- isTRUE(rows$cor[1L]) && n_coef > 1L
    # The shared-ID syntax `(1 | sp | series)` puts two nlpars under
    # one id. Each row builds its own names, which keeps the names of
    # the two nlpars distinct.
    coef_alias <- rows$coef_alias
    group_token_per_coef <- rows$alias_key
    # brms writes the per-level deviations twice. A correlated block is
    # scaled into the matrix `r_<id>[<level_idx>, <coef_idx>]`. The
    # per-coefficient vectors `r_<id>_<coef_idx>[<level_idx>]` repeat
    # it in a form brms finds faster to index. A fit saves the vectors
    # alone. brms renames those.
    grid <- expand.grid(
      level_idx = seq_len(n_lvl),
      coef_idx = seq_len(n_coef),
      KEEP.OUT.ATTRS = FALSE
    )
    # brms's stancode infixes the row prefix between the id and the
    # coefficient index, the spelling `brms:::rename_re_levels()` matches:
    #   univariate:          r_<id>_<coef_idx>[<lev>]
    #   dpar / resp / nlpar: r_<id>_<prefix>_<coef_idx>[<lev>]
    coef_infix <- ifelse(nzchar(rows$row_prefix),
                         paste0("_", rows$row_prefix), "")
    r_old <- sprintf("r_%d%s_%d%s[%d]", id,
                     coef_infix[grid$coef_idx],
                     grid$coef_idx, pos_sfx, grid$level_idx)
    # The suffix belongs on the variable name, before the index.
    # `posterior` parses a name as `variable[element]`, so
    # `r_grp[a,Intercept]_trend` would not select as an element of
    # `r_grp_trend` and `subset_draws(variable = "r_grp_trend")`
    # would find nothing.
    r_new <- sprintf(
      "r_%s%s[%s,%s]", group_token_per_coef[grid$coef_idx],
      alias_sfx, levels[grid$level_idx], coefs[grid$coef_idx]
    )
    r_map <- stats::setNames(r_old, r_new)
    # sd_<id>[<coef_idx>] -> sd_<group>__<coef>. A `by` factor makes
    # `sd_<id>` a matrix with one column per level, which Stan saves
    # column by column, in the order `ranef_rnames()` names the cells.
    rnames <- ranef_rnames(rows)
    by_levels <- length(rnames) %/% n_coef
    sd_old <- if (by_levels > 1L) {
      sprintf("sd_%d%s[%d,%d]", id, pos_sfx, rep(seq_len(n_coef), by_levels),
              rep(seq_len(by_levels), each = n_coef))
    } else {
      sprintf("sd_%d%s[%d]", id, pos_sfx, seq_len(n_coef))
    }
    sd_map <- stats::setNames(
      sd_old, paste0("sd_", group, "__", rnames, alias_sfx)
    )
    # cor_<id>[<k>] -> cor_<group>__<coef_j>__<coef_k>, in the order
    # `ranef_cor_names()` gives. Each level of a `by` factor has its
    # own vector `cor_<id>_<level>`.
    cor_map <- character(0L)
    if (has_cor) {
      cells <- matrix(rnames, nrow = n_coef)
      cor_map <- unlist(lapply(seq_len(ncol(cells)), function(j) {
        new <- paste0(ranef_cor_names(group, cells[, j]), alias_sfx)
        vec <- if (by_levels > 1L) paste0("cor_", id, "_", j) else {
          paste0("cor_", id)
        }
        stats::setNames(
          sprintf("%s%s[%d]", vec, pos_sfx, seq_along(new)), new
        )
      }))
    }
    c(r_map, sd_map, cor_map)
  })
  unlist(parts)
}


#' The posterior draws of a fit under their user-facing names
#'
#' Every draws method calls it and converts the result to its own
#' shape.
#'
#' @param x Fitted `mvgam` object
#' @param variable Character vector of parameter names or regular
#'   expressions, or `NULL` for the default set
#' @param regex Logical. `TRUE` treats `variable` as regular expressions.
#' @param inc_warmup Logical. `TRUE` includes the warmup draws.
#' @return A `draws_array`
#' @noRd
extract_mvgam_draws <- function(x, variable = NULL, regex = FALSE,
                                inc_warmup = FALSE) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(variable, min.len = 1L, null.ok = TRUE)
  checkmate::assert_flag(regex)
  checkmate::assert_flag(inc_warmup)
  # Every draws method reaches this guard. A prefit has an empty `fit`
  # slot.
  require_fitted_model(x)
  # One projection decides which parameters exist and what they are
  # called. Without a named `variable`, the rotation-indeterminate raw
  # factor block is hidden as well. The prior fixes no rotation, which
  # leaves their Rhat and ESS arbitrary. Their QR-identified
  # counterparts are in the same posterior. Naming one explicitly
  # keeps it. The selection is settled on the names. Only the selected
  # parameters' draws are then converted.
  raw_vars <- fit_variables(x$fit)
  reported_map <- mvgam_user_pars(x, pars = raw_vars)
  user_map <- if (is.null(variable)) {
    reported_map
  } else {
    mvgam_user_pars(x, pars = raw_vars, all = TRUE)
  }
  selected <- if (is.null(variable)) {
    names(user_map)
  } else {
    match_draws_variables(variable, regex, names(user_map),
                          names(reported_map))
  }
  keep <- user_map[selected]
  drws <- stanfit_draws(x$fit, inc_warmup, pars = unname(keep))
  drws <- posterior::subset_draws(drws, variable = unname(keep))
  renames <- keep[names(keep) != keep]
  if (length(renames) > 0L) {
    drws <- do.call(
      posterior::rename_variables,
      c(list(drws), as.list(renames))
    )
  }
  drws
}


#' The user-facing names a `variable` argument selects
#'
#' Keyword shortcuts and free names or patterns compose in one call,
#' as `variable = c("betas", "^sd_", "Intercept")`. A keyword names
#' the reported parameters. Only an explicit name reaches a hidden one.
#'
#' @param variable Character vector of keywords, names or regular
#'   expressions
#' @param regex Logical. `TRUE` treats the free entries as regular
#'   expressions.
#' @param all_vars Every user-facing name, hidden ones included
#' @param reported The user-facing names of the reported parameters
#' @return Character vector of selected names, possibly empty when only
#'   keywords were given
#' @noRd
match_draws_variables <- function(variable, regex, all_vars, reported) {
  is_keyword <- variable %in% mvgam_keyword_shortcuts
  matched <- unlist(lapply(variable[is_keyword], resolve_mvgam_keyword,
                           reported))
  free <- variable[!is_keyword]
  free_matched <- character(0L)
  if (length(free) > 0L) {
    free_matched <- if (isTRUE(regex)) {
      unique(unlist(lapply(free, grep, x = all_vars, value = TRUE)))
    } else {
      intersect(expand_variable_names(free, all_vars), all_vars)
    }
  }
  # A keyword can resolve to nothing, as `obs_params` does for a
  # Poisson model. Free names that match nothing signal a typo.
  if (length(free) > 0L && length(free_matched) == 0L) {
    stop(insight::format_error(c(
      "No parameters matched the supplied 'variable' argument.",
      x = paste0(
        "Requested: ",
        paste0("'", free, "'", collapse = ", "), "."
      ),
      i = "Call variables(x) to list available parameter names."
    )), call. = FALSE)
  }
  unique(c(as.character(matched), free_matched))
}


#' The draws array of a fit's `stanfit`
#'
#' Without warmup this is the conversion every other draws reader
#' uses. With warmup the chains come from `fit@sim$samples`, as the
#' internal brms function `.as_draws_list()` takes them. Each chain
#' begins with the saved warmup iterations, whose count
#' `fit@sim$warmup2` records.
#'
#' @param fit A `stanfit` object, or a `draws` object, which holds no
#'   warmup
#' @param inc_warmup Logical. `TRUE` includes the warmup iterations
#' @param pars Character vector of parameter names to convert, or
#'   `NULL` for every saved quantity
#' @return A `draws_array`
#' @noRd
stanfit_draws <- function(fit, inc_warmup = FALSE, pars = NULL) {
  checkmate::assert(
    checkmate::check_class(fit, "stanfit"),
    checkmate::check_class(fit, "draws")
  )
  checkmate::assert_flag(inc_warmup)
  checkmate::assert_character(pars, any.missing = FALSE, null.ok = TRUE)
  if (!inc_warmup) {
    if (!is.null(pars) && inherits(fit, "stanfit")) {
      return(posterior::as_draws_array(stanfit_pars_array(fit, pars)))
    }
    return(posterior::as_draws_array(fit))
  }
  n_warmup <- if (inherits(fit, "stanfit")) fit@sim$warmup2 else 0L
  if (!any(n_warmup > 0L)) {
    stop(insight::format_error(c(
      "'inc_warmup = TRUE' needs the warmup draws of the fit.",
      x = "The fit was sampled with 'save_warmup = FALSE'.",
      i = "Refit with 'save_warmup = TRUE' to keep them."
    )), call. = FALSE)
  }
  posterior::as_draws_array(posterior::as_draws_list(fit@sim$samples))
}


#' The names of the quantities a fit saved
#'
#' A stanfit records them. `posterior::variables()` on the converted
#' draws gives the same names in the same order after copying every
#' draw.
#'
#' @param fit A stanfit or draws object
#' @return Character vector of variable names
#' @noRd
fit_variables <- function(fit) {
  if (inherits(fit, "stanfit")) {
    return(fit@sim$fnames_oi)
  }
  posterior::variables(posterior::as_draws_matrix(fit))
}


#' The draws of `lp__` alone, with the chains and iterations of the fit
#'
#' Counting draws, chains or iterations needs their shape. Converting
#' the whole fit copies every saved quantity.
#'
#' @param fit A stanfit or draws object
#' @return A draws array
#' @noRd
fit_draws_shape <- function(fit) {
  stanfit_draws(fit, pars = character(0L))
}


#' Draws kept by their indices across chains
#'
#' A draw index counts across chains. `posterior::subset_draws()`
#' merges the chains to apply it and prints a message saying so.
#' Merging first applies the same index without the message.
#'
#' @param drws A `draws` object
#' @param draw Positive integer draw indices, or `NULL` for every draw
#' @return `drws`, subset when `draw` is given
#' @noRd
subset_draw_ids <- function(drws, draw = NULL) {
  checkmate::assert_class(drws, "draws")
  checkmate::assert_integerish(draw, lower = 1L, any.missing = FALSE,
                               min.len = 1L, null.ok = TRUE)
  if (is.null(draw)) {
    return(drws)
  }
  posterior::subset_draws(posterior::merge_chains(drws), draw = draw)
}


#' Parameter names a request selects
#'
#' A name selects itself. A bare vector name, such as `sigma_trend`,
#' selects every element, as `posterior::subset_draws()` does. A
#' request that matches nothing is kept for the caller's error to name.
#'
#' @param variable Character vector of requested names
#' @param available Character vector of the names a fit holds
#' @return Character vector of names
#' @noRd
expand_variable_names <- function(variable, available) {
  checkmate::assert_character(variable)
  checkmate::assert_character(available)
  base <- sub("\\[.*\\]$", "", available)
  unique(unlist(lapply(variable, function(v) {
    hit <- available[available == v | base == v]
    if (length(hit) > 0L) hit else v
  })))
}


#' @rdname mvgam_draws
#' @export
as.data.frame.mvgam <- function(x, row.names = NULL, optional = TRUE,
                                 variable = NULL, draw = NULL,
                                 regex = FALSE, ...) {
  rlang::check_dots_empty()
  as.data.frame(subset_draw_ids(
    posterior::as_draws_df(extract_mvgam_draws(x, variable, regex)),
    draw
  ))
}


#' @rdname mvgam_draws
#' @export
as.matrix.mvgam <- function(x, variable = NULL, draw = NULL,
                            regex = FALSE, ...) {
  rlang::check_dots_empty()
  # Unclassed, as `brms::as.matrix.brmsfit()` unclasses. A
  # `draws_matrix` keeps its class through subsetting, arithmetic and
  # `as.matrix()` alike. Code expecting a bare matrix refuses it.
  unclass(subset_draw_ids(
    posterior::as_draws_matrix(extract_mvgam_draws(x, variable, regex)),
    draw
  ))
}


#' @rdname mvgam_draws
#' @export
as.array.mvgam <- function(x, variable = NULL, draw = NULL,
                           regex = FALSE, ...) {
  rlang::check_dots_empty()
  # Unclassed for the reason `as.matrix.mvgam()` gives.
  # `brms::as.array.brmsfit()` also returns a bare array.
  # `as_draws_array()` returns the draws object itself.
  unclass(subset_draw_ids(extract_mvgam_draws(x, variable, regex), draw))
}


#' @rdname mvgam_draws
#' @method as_draws mvgam
#' @export
as_draws.mvgam <- function(x, variable = NULL, regex = FALSE,
                            inc_warmup = FALSE, ...) {
  rlang::check_dots_empty()
  posterior::as_draws(
    extract_mvgam_draws(x, variable, regex, inc_warmup)
  )
}


#' @export
#' @importFrom posterior as_draws
posterior::as_draws


#' @rdname mvgam_draws
#' @method as_draws_matrix mvgam
#' @export
as_draws_matrix.mvgam <- function(x, variable = NULL, regex = FALSE,
                                   inc_warmup = FALSE, ...) {
  rlang::check_dots_empty()
  posterior::as_draws_matrix(
    extract_mvgam_draws(x, variable, regex, inc_warmup)
  )
}


#' @export
#' @importFrom posterior as_draws_matrix
posterior::as_draws_matrix


#' @rdname mvgam_draws
#' @method as_draws_df mvgam
#' @export
as_draws_df.mvgam <- function(x, variable = NULL, regex = FALSE,
                               inc_warmup = FALSE, ...) {
  rlang::check_dots_empty()
  posterior::as_draws_df(
    extract_mvgam_draws(x, variable, regex, inc_warmup)
  )
}


#' @export
#' @importFrom posterior as_draws_df
posterior::as_draws_df


#' @rdname mvgam_draws
#' @method as_draws_array mvgam
#' @export
as_draws_array.mvgam <- function(x, variable = NULL, regex = FALSE,
                                  inc_warmup = FALSE, ...) {
  rlang::check_dots_empty()
  extract_mvgam_draws(x, variable, regex, inc_warmup)
}


#' @export
#' @importFrom posterior as_draws_array
posterior::as_draws_array


#' @rdname mvgam_draws
#' @method as_draws_list mvgam
#' @export
as_draws_list.mvgam <- function(x, variable = NULL, regex = FALSE,
                                 inc_warmup = FALSE, ...) {
  rlang::check_dots_empty()
  posterior::as_draws_list(
    extract_mvgam_draws(x, variable, regex, inc_warmup)
  )
}


#' @export
#' @importFrom posterior as_draws_list
posterior::as_draws_list


#' @rdname mvgam_draws
#' @method as_draws_rvars mvgam
#' @export
as_draws_rvars.mvgam <- function(x, variable = NULL, regex = FALSE,
                                  inc_warmup = FALSE, ...) {
  rlang::check_dots_empty()
  posterior::as_draws_rvars(
    extract_mvgam_draws(x, variable, regex, inc_warmup)
  )
}


#' @export
#' @importFrom posterior as_draws_rvars
posterior::as_draws_rvars
