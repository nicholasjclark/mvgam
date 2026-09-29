#' @title Extract posterior draws from fitted \pkg{mvgam} objects
#'
#' @name mvgam_draws
#'
#' @description
#' Extract posterior draws in conventional formats as data.frames,
#' matrices, arrays, or `posterior::draws_*` objects.
#'
#' @param x A fitted `mvgam` object.
#' @param variable Either a keyword shortcut, a character vector of
#'   parameter names, or (when `regex = TRUE`) a vector of regular
#'   expressions. Recognised keywords:
#'   \itemize{
#'     \item `"betas"`: population-level coefficients, the set
#'       [brms::fixef()] reports (`b_*`, `bs_*` and `bsp_*`).
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
#'   `NULL` (the default) returns every parameter.
#' @param regex Logical. When `TRUE`, each entry of `variable` is
#'   treated as a regular expression and any parameter matching at
#'   least one pattern is selected. Keyword shortcuts are still
#'   honoured. Defaults to `FALSE`.
#' @param draw For `as.data.frame.mvgam`, an optional integer
#'   vector of draw indices (1-based) to subset after extraction.
#'   `NULL` (the default) returns all draws.
#' @param inc_warmup Logical. Include warmup draws? Defaults to
#'   `FALSE`.
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
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Trend-dynamics parameters as a draws data frame.
#' as.data.frame(mod, variable = "trend_params")
#' # The same draws as a posterior::draws_array.
#' as_draws_array(mod, variable = "trend_params")
#' }
#'
#' @author Nicholas J Clark
#'
NULL


# The keyword shortcuts `variable =` accepts, defined once.
mvgam_keyword_shortcuts <- c(
  "betas", "obs_params", "smooth_params",
  "trend_betas", "trend_params", "trend_smooth_params"
)


# Resolve a single keyword to the names in `all_vars` it selects.
# Each keyword is a (side, kind) pair from the one taxonomy.
# `?mvgam_draws` documents `smooth_params` as the smoothing standard
# deviations alone. The smooth bucket `tidy()` reports from also holds
# the basis coefficients and the Gaussian-process hyperparameters.
#'@noRd
resolve_mvgam_keyword <- function(keyword, all_vars) {
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


# Internal: build the positional -> brms-style alias map for the
# fixed-effects block. brms's `rename_pars` translates `b[k]` to
# `b_<term>` post-fit, where `<term>` is the k-th non-Intercept
# column of the design matrix (the Intercept is centred out into
# its own scalar). For trend-formula fits the analogous map is
# `b_trend[k]` -> `b_<term>_trend` over `standata$X_trend`. We
# rebuild the map at draw-extraction time so every method routed
# through `extract_mvgam_draws` sees the brms-native names.
#'@noRd
mvgam_beta_aliases <- function(x) {
  checkmate::assert_class(x, "mvgam")
  # brms's stancode centres the design matrix whenever an Intercept
  # column is present: `b[k]` then enumerates the K - 1 non-Intercept
  # columns. Without an Intercept the mapping is direct. Every
  # linear predictor gets its own design-matrix block in standata:
  #   `X`             obs main formula  -> b[k]            -> b_<term>
  #   `X_<resp>`      MV response       -> b_<resp>[k]     -> b_<resp>_<term>
  #   `X_<dpar>`      dpar formula      -> b_<dpar>[k]     -> b_<dpar>_<term>
  #   `X_<nlpar>`     nl sub-formula    -> b_<nlpar>[k]    -> b_<nlpar>_<term>
  #   `X_trend`       mvgam-only        -> b_trend[k]      -> b_<term>_trend
  # The trend block takes a name suffix and every other block a
  # prefix. One sweep over `names(x$standata)` builds the whole map.
  # brms does not centre a non-linear sub-formula's design: the
  # columns of `X_<nlpar>` match `b_<nlpar>` one to one, with the
  # Intercept at position 1. `strip_intercept = FALSE` states that.
  build <- function(X, pos_prefix, alias_prefix, alias_suffix,
                    strip_intercept = TRUE) {
    if (is.null(X) || !is.matrix(X) || ncol(X) == 0L) {
      return(character(0L))
    }
    cn <- colnames(X)
    if (length(cn) == 0L) {
      return(character(0L))
    }
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
  # brms stores a non-linear sub-formula in `pforms` under its nlpar
  # name, and treats it as one only when the top-level formula has
  # `attr(., "nl") = TRUE`. The per-block branch below tests the
  # names found here.
  obs_form <- x$formula
  is_nl <- isTRUE(attr(
    if (inherits(obs_form, "brmsformula")) obs_form$formula else obs_form,
    "nl"
  ))
  nlpar_names <- if (is_nl && inherits(obs_form, "brmsformula")) {
    names(obs_form$pforms %||% list())
  } else {
    character(0L)
  }
  X_blocks <- grep("^X(_.+)?$", names(x$standata), value = TRUE)
  parts <- lapply(X_blocks, function(blk) {
    X <- x$standata[[blk]]
    if (identical(blk, "X")) {
      build(X, "b", "b_", "")
    } else if (identical(blk, "X_trend")) {
      build(X, "b_trend", "b_", "_trend")
    } else {
      suffix <- sub("^X_", "", blk)
      build(
        X,
        pos_prefix = paste0("b_", suffix),
        alias_prefix = paste0("b_", suffix, "_"),
        alias_suffix = "",
        strip_intercept = !(suffix %in% nlpar_names)
      )
    }
  })
  # The unpenalised part of each smooth: `bs<p>[k]` maps to
  # `bs<p>_<colname>`, the column named in the matching `Xs<p>`
  # block, for every predictor `<p>` as the `X` sweep above names
  # them. brms keeps no intercept column in `Xs`, and the index is
  # direct.
  Xs_blocks <- grep("^Xs(_.+)?$", names(x$standata), value = TRUE)
  bs_parts <- lapply(Xs_blocks, function(blk) {
    suffix <- sub("^Xs", "", blk)
    if (identical(suffix, "_trend")) {
      build(x$standata[[blk]], "bs_trend", "bs_", "_trend",
            strip_intercept = FALSE)
    } else {
      build(x$standata[[blk]], paste0("bs", suffix),
            paste0("bs", suffix, "_"), "", strip_intercept = FALSE)
    }
  })
  c(unlist(parts), unlist(bs_parts))
}


# Internal: the alias map for the smoothing standard deviations and
# basis coefficients. brms's stancode numbers each smooth object:
# `sds<p>_<i>[k]` is the standard deviation of its k-th penalty and
# `s<p>_<i>_<k>[m]` the m-th coefficient under that penalty.
# `brms:::rename_sm()` then names the object by its label:
# `sds<p>_<label>_<k>` and `s<p>_<label>_<k>[m]`. A trend-side name
# takes `_trend` before its index, as the other aliases do.
#
# @param x A fitted `mvgam` object.
# @param raw The raw Stan names the posterior carries.
#'@noRd
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


# Internal: the alias map for Gaussian-process and special terms.
#
# brms numbers these by position within each predictor `<p>`, and
# `brms:::rename_gp()` and `brms:::rename_sp()` name them by term:
#   `sdgp<p>_<i>[k]`       -> `sdgp<p>_<label k>`
#   `lscale<p>_<i>[k, d]`  -> `lscale<p>_<label k><covariate d>`, or
#                             `lscale<p>_<label k>` when isotropic
#   `zgp<p>_<i>[_j][m]`    -> `zgp<p>_<label j>[m]`
#   `bsp<p>[k]`            -> `bsp<p>_<term k>`
#   `simo<p>_<i>[m]`       -> `simo<p>_<term><n>[m]`, one per `mo()`
# A GP's label is `gp` and its covariates, then its `by` variable and
# each level of a `by` factor. A trend-side name takes `_trend` before
# its index, as the other aliases do.
#
# @param x A fitted `mvgam` object.
# @param raw The raw Stan names the posterior carries.
#'@noRd
mvgam_special_aliases <- function(x, raw) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(raw)
  if (!any(grepl("^(sdgp|lscale|zgp|bsp|simo)(_|\\[)", raw))) {
    return(character(0L))
  }
  rename_block <- function(old_prefix, new_names, sfx) {
    old <- raw[startsWith(raw, paste0(old_prefix, sfx, "["))]
    if (length(old) != length(new_names)) {
      return(character(0L))
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
      gp_terms <- formula_term_labels(lp$pred[["gp"]])
      for (i in seq_along(gp_terms)) {
        g <- eval(str2lang(gp_terms[i]), list(gp = brms::gp))
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
      sp_terms <- formula_term_labels(lp$pred[["sp"]])
      if (length(sp_terms)) {
        coefs <- brms_rename(sp_terms)
        out <- c(out, rename_block(paste0("bsp", p),
                                   paste0("bsp", p, "_", coefs), sfx))
        n_mo <- lengths(regmatches(sp_terms,
                                   gregexpr("\\bmo\\(", sp_terms)))
        simo <- unlist(Map(function(coef, n) {
          if (n > 0L) paste0(coef, seq_len(n))
        }, coefs, n_mo), use.names = FALSE)
        for (i in seq_along(simo)) {
          out <- c(out, rename_vector(paste0("simo", p, "_", i),
                                      paste0("simo", p, "_", simo[i]), sfx))
        }
      }
    }
  }
  out
}


# Internal: the label of each Gaussian process a `gp()` term fits.
# `brms:::frame_gp()` writes `gp` and the covariates, appends the `by`
# variable, and for a `by` factor fits one process per column of the
# factor's cell-means design, labelled by its level.
#
# @param g The object `brms::gp()` returns for the term.
# @param data The frame the model was fitted to.
# @return Character vector, one label per process.
#'@noRd
brms_gp_labels <- function(g, data) {
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


# Internal: the term labels of a component formula, or none.
#'@noRd
formula_term_labels <- function(form) {
  if (!inherits(form, "formula")) {
    return(character(0L))
  }
  attr(stats::terms(form), "term.labels")
}


# Internal: the index a Stan element name carries, `3` for `x[3]`.
#'@noRd
stan_index <- function(v) {
  sub("^.*\\[([0-9]+)\\]$", "\\1", v)
}


# Internal: the label brms gives each smooth object of one term.
# `brms:::frame_sm()` joins the covariates and the `by` variable,
# strips the punctuation `brms:::rename()` strips, prefixes the
# smooth function and appends the object's `by` level: `s(time, by =
# series)` labels its objects `stimeseriesDM`, `stimeseriesDO`.
#
# @param term One entry of `mvgam_smooth_terms()`.
# @return Character vector, one label per smooth object of the term.
#'@noRd
brms_smooth_labels <- function(term) {
  sfun <- sub("\\(.*$", "", term$term)
  vars <- c(term$covars, stats::na.omit(term$by_var))
  base <- paste0(sfun, brms_rename(paste(vars, collapse = "")))
  if (is.null(term$bylevels)) {
    return(base)
  }
  paste0(base, brms_rename(gsub("[ \t\r\n]+", "", term$bylevels)))
}


# Internal: the substitutions `brms:::rename()` applies by default
# when it turns a term into part of a parameter name.
#'@noRd
brms_rename <- function(x) {
  from <- c(" ", "(", ")", "[", "]", ",", "\"", "'", "?",
            "+", "-", "*", "/", "^", "=", "$")
  to <- c(rep("", 9L), "P", "M", "MU", "D", "E", "EQ", "USD")
  for (i in seq_along(from)) {
    x <- gsub(from[i], to[i], x, fixed = TRUE)
  }
  x
}


# Internal: replace positional Stan parameter names in `vars` with
# their brms-native aliases. Each element of `alias_map` names the
# alias and holds the positional Stan name, the shape every
# `mvgam_*_aliases()` builder returns. Names not in the map pass
# through unchanged.
#'@noRd
apply_mvgam_aliases <- function(vars, alias_map) {
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


# Internal: the user-facing projection of a fit's parameter names.
# Returns a named character vector: each name is the name a user
# sees and each value the raw Stan name it aliases. The
# empty-observation placeholder is dropped, and unless `all` is set
# so are the working arrays, the accumulators and the hidden forms.
#
# `variables.mvgam()`, `extract_mvgam_draws()`, `summary()` and
# `tidy.mvgam()` all take their names from here. They agree about
# which parameters exist and what they are called: `b_x` for the
# positional `b[1]`, and the identified loadings in place of
# `L_Omega_trend`.
#
# @param x A fitted `mvgam` object.
# @param pars Optional raw Stan names to project. Defaults to every
#   name in the posterior.
# @param all Return every parameter under its user-facing name,
#   skipping the exclusion list and the rotation-indeterminate
#   filter. Only a caller that named a parameter explicitly should
#   set this, since asking for one by name is how a user reaches a
#   parameter the default view hides.
#'@noRd
mvgam_user_pars <- function(x, pars = NULL, all = FALSE) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_flag(all)
  raw <- pars %||% posterior::variables(posterior::as_draws(x$fit))
  user <- apply_mvgam_aliases(
    raw, c(mvgam_beta_aliases(x), mvgam_ranef_aliases(x),
           mvgam_smooth_aliases(x, raw), mvgam_special_aliases(x, raw))
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
  }
  raw <- raw[keep]
  user <- user[keep]
  # brms orders a fitted object's parameters by class, and every
  # reader of this projection inherits that order
  ord <- mvgam_par_order(user)
  stats::setNames(raw[ord], user[ord])
}


# Internal: the brms group-level metadata for a fitted mvgam
# object. Returns a list with `reframe` (the brmsfit `$ranef`
# data.frame: one row per (group, coef) pair) and `group_levels`
# (the named list of levels per grouping factor), or NULL when the
# fit has no group-level effects.
#
# Each side stores a lightweight brmsfit holding this table, built
# when the model was set up.
#
# `mvgam_ranef_aliases()`, `ranef.mvgam()` and `VarCorr.mvgam()` all
# take the names they look up from here. Each row gains the three
# name parts brms builds: `row_prefix`, the predictor it enters, as
# `make_row_prefix()` gives it; `alias_key`, the group with that
# prefix, as in `r_g__sigma_y1[a,Intercept]`; and `coef_alias`, the
# coefficient with that prefix, as in `sd_g__sigma_y1_Intercept`.
# `brms:::rename_re_levels()` writes each whitespace character of a
# level as `.`, and so do the levels here.
#'@noRd
mvgam_ranef_metadata <- function(x,
                                 side = c("observation", "trend")) {
  checkmate::assert_class(x, "mvgam")
  side <- match.arg(side)
  reframe <- if (identical(side, "trend")) {
    x$trend_model$ranef
  } else {
    x$obs_model$ranef
  }
  if (is.null(reframe) || nrow(reframe) == 0L) {
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


# Internal: build alias map for random-effect parameters.
# brms's stancode emits group-level parameters in positional form
# (`r_<id>[<level_idx>,<coef_idx>]`, `sd_<id>[<coef_idx>]`,
# `cor_<id>[<flat_off_diag>]`). brms's `rename_pars` then promotes
# these to user-facing aliases keyed on the grouping factor name
# and the coefficient name (`r_<group>[<level>,<coef>]`,
# `sd_<group>__<coef>`, `cor_<group>__<coef1>__<coef2>`). mvgam
# delegates Stan-code generation to brms and does not run
# `rename_pars`. This helper rebuilds the same map.
#
# Returns a named character vector in the same shape as
# `mvgam_beta_aliases`: names are the brms-native aliases, values
# are the positional Stan names. An empty character vector is
# returned when the fit has no group-level effects.
#
# Both sides are aliased. The observation and trend blocks reach
# Stan under the same positional names, the trend's with `_trend`
# after the id. `mvgam_ranef_metadata()` takes each side's table from
# the lightweight brmsfit stored for it.
#
# The other brms group-level patterns come through the same
# `$ranef` table and so need no special case here: multivariate
# response (`bf(mvbind(y1, y2) ~ (1 | g))`), distributional
# parameters (`bf(y ~ ..., sigma ~ (1 | g))`), nesting
# (`(1 | g1/g2)`, expanded to `(1|g1) + (1|g1:g2)`) and by-factor
# grouping (`gr(g, by = f)`).
#'@noRd
mvgam_ranef_aliases <- function(x) {
  c(mvgam_ranef_aliases_side(x, "observation"),
    mvgam_ranef_aliases_side(x, "trend"))
}


# Internal: the alias map for one side's group-level block.
#
# A random effect declared in `trend_formula` reaches Stan under the
# same positional names the observation side uses, with `_trend`
# after the id: `sd_1_trend[1]` where the observation side has
# `sd_1[1]`. Its alias takes the `_trend` suffix the beta aliaser
# applies to `b_trend[k]`: `sd_grp__Intercept_trend`.
#'@noRd
mvgam_ranef_aliases_side <- function(x,
                                     side = c("observation", "trend")) {
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
    # one id. The names are built per row. Built from the first row
    # alone, the second nlpar's names collided with the first's.
    coef_alias <- rows$coef_alias
    group_token_per_coef <- rows$alias_key
    # brms writes the per-level deviations twice: as the matrix
    # `r_<id>[<level_idx>, <coef_idx>]` a correlated block is scaled
    # into, and as the per-coefficient vectors
    # `r_<id>_<coef_idx>[<level_idx>]` its own comment calls faster to
    # index. A fit leaves the matrix out with the other working
    # variables. The posterior holds the vectors, and brms renames
    # the vectors too.
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


# Internal: pull a `draws_array` from the stanfit slot, optionally
# filtered by keyword / explicit names / regex. Public methods just
# need to coerce the result to their target shape.
#'@noRd
extract_mvgam_draws <- function(x, variable = NULL, regex = FALSE,
                                inc_warmup = FALSE) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_logical(regex, len = 1L)
  checkmate::assert_logical(inc_warmup, len = 1L)
  # Every draws method in the package calls this function, and the
  # one guard serves them all. A prefit has an empty `fit` slot, and
  # the line below met it with "Don't know how to transform an object
  # of class 'NULL'", naming neither the state nor the argument that
  # made it.
  require_fitted_model(x)
  drws <- posterior::as_draws_array(x$fit, inc_warmup = inc_warmup)
  # One projection decides which parameters exist and what they are
  # called. When the user did not name a `variable`, the
  # rotation-indeterminate raw factor block is hidden as well: those
  # have arbitrary Rhat and ESS because no rotation is fixed under
  # the prior, and their QR-identified counterparts are in the same
  # posterior. Naming one explicitly keeps it.
  user_map <- mvgam_user_pars(
    x, pars = posterior::variables(drws), all = !is.null(variable)
  )
  renames <- user_map[names(user_map) != user_map]
  if (length(renames) > 0L) {
    drws <- do.call(
      posterior::rename_variables,
      c(list(drws), as.list(renames))
    )
  }
  drws <- posterior::subset_draws(drws, variable = names(user_map))
  if (is.null(variable)) {
    return(drws)
  }
  checkmate::assert_character(variable, min.len = 1L)
  all_vars <- posterior::variables(drws)
  # Split user input into keyword shortcuts and "free" patterns so
  # both can compose in a single call (e.g. variable = c("betas",
  # "^sd_", "Intercept")).
  is_keyword <- variable %in% mvgam_keyword_shortcuts
  matched <- character(0L)
  if (any(is_keyword)) {
    matched <- unlist(lapply(variable[is_keyword], function(k) {
      resolve_mvgam_keyword(k, all_vars)
    }))
  }
  free <- variable[!is_keyword]
  free_matched <- character(0L)
  if (length(free) > 0L) {
    free_matched <- if (isTRUE(regex)) {
      unique(unlist(lapply(free, function(p) {
        grep(p, all_vars, value = TRUE)
      })))
    } else {
      intersect(free, all_vars)
    }
  }
  # Keyword shortcuts may legitimately return an empty set (e.g.
  # `obs_params` for Poisson, which has no distributional params).
  # Only error when the user supplied free patterns / names that did
  # not match, which is a typo signal worth catching.
  if (length(free) > 0L && length(free_matched) == 0L) {
    stop(insight::format_error(c(
      "No parameters matched the supplied 'variable' argument.",
      x = paste0(
        "Requested: ",
        paste0("'", free, "'", collapse = ", "), "."
      ),
      i = "Call variables(x) to list available parameter names."
    )))
  }
  matched <- unique(c(matched, free_matched))
  if (length(matched) == 0L) {
    # Every keyword resolved to empty (e.g. obs_params on Poisson).
    # Return a zero-column draws_array so downstream coercions stay
    # well-defined.
    return(posterior::subset_draws(drws, variable = character(0L)))
  }
  posterior::subset_draws(drws, variable = matched)
}


#' @rdname mvgam_draws
#' @export
as.data.frame.mvgam <- function(x, row.names = NULL, optional = TRUE,
                                 variable = NULL, draw = NULL,
                                 regex = FALSE, ...) {
  rlang::check_dots_empty()
  drws <- posterior::as_draws_df(
    extract_mvgam_draws(x, variable, regex)
  )
  if (!is.null(draw)) {
    drws <- posterior::subset_draws(drws, draw = draw)
  }
  as.data.frame(drws)
}


#' @rdname mvgam_draws
#' @export
as.matrix.mvgam <- function(x, variable = NULL, regex = FALSE, ...) {
  rlang::check_dots_empty()
  # Unclassed, as `brms::as.matrix.brmsfit` unclasses: a caller
  # asking for a matrix is handed one. A `draws_matrix` keeps its
  # class through subsetting, arithmetic and `as.matrix()` alike, so
  # anything typed to a bare matrix refuses it downstream.
  unclass(posterior::as_draws_matrix(
    extract_mvgam_draws(x, variable, regex)
  ))
}


#' @rdname mvgam_draws
#' @export
as.array.mvgam <- function(x, variable = NULL, regex = FALSE, ...) {
  rlang::check_dots_empty()
  # Unclassed for the same reason `as.matrix.mvgam()` is, and to
  # match `brms::as.array.brmsfit()`. `as_draws_array()` remains the
  # way to ask for the draws object itself.
  unclass(extract_mvgam_draws(x, variable, regex))
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
