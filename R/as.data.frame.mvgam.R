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
#'     \item `"betas"`: population-level fixed effects (brms `b_*`).
#'     \item `"obs_params"`: observation-family distributional
#'       parameters (`sigma`, `phi`, `shape`, ...).
#'     \item `"smooth_params"`: smooth-term standard deviations
#'       (`sds_*`).
#'     \item `"trend_betas"`: trend-side fixed effects (`b_trend[*]`).
#'     \item `"trend_params"`: trend-dynamics parameters (AR, GP,
#'       innovation SD, ...). With a `trend_formula` this resolves to
#'       the `_trend`-suffixed parameter block; without it, the
#'       top-level trend dynamics.
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
#' @param use_alias Retained for backwards-compatibility; parameter
#'   names are now brms-native so no aliasing is performed.
#' @param row.names,optional Ignored.
#' @param ... Ignored.
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
#' @author Nicholas J Clark
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = Gamma())
#'
#' mod1 <- mvgam(
#'   y ~ s(season, bs = "cc"),
#'   trend_model = AR(),
#'   data = sim$data_train,
#'   family = Gamma(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' head(as.matrix(mod1, variable = "betas"))
#' head(as.matrix(mod1, variable = "trend_params"))
#' head(as.matrix(mod1, variable = "^sd_", regex = TRUE))
#' }
NULL


# Single source of truth for the keyword shortcuts. Each entry maps
# a keyword to a function that returns the matching parameter names
# given the current stanfit's variables and the parent fit object
# (the latter is needed to disambiguate trend-formula vs obs-only
# fits when the same parameter name can mean different things).
mvgam_keyword_shortcuts <- c(
  "betas", "obs_params", "smooth_params",
  "trend_betas", "trend_params", "trend_smooth_params"
)


# Resolve a single keyword to a character vector of parameter names
# present in `all_vars`. Patterns are brms-native: keywords map
# directly to the parameter names brms emits. The trend-dynamics
# block (`trend_params`) is the one place context matters — with
# a `trend_formula` those parameters carry the `_trend` suffix,
# otherwise they sit at the top level.
#'@noRd
resolve_mvgam_keyword <- function(keyword, x, all_vars) {
  has_trend_f <- !is.null(x$trend_formula)
  pick <- function(re, ignore_case = FALSE) {
    grep(re, all_vars, value = TRUE, ignore.case = ignore_case)
  }
  drop <- function(set, re) {
    if (length(set) == 0L) return(set)
    set[!grepl(re, set)]
  }
  switch(keyword,
    "betas" = drop(pick("^b_"), "^b_trend\\["),
    "trend_betas" = pick("^b_trend\\["),
    "obs_params" = drop(
      pick(paste0(
        "^(sigma|sigmay|phi|shape|nu|hu|zi|kappa|alpha|delta|",
        "disc|tail_df)(\\[|$)"
      ), ignore_case = TRUE),
      "_trend"
    ),
    "smooth_params" = drop(pick("^sds_"), "_trend"),
    "trend_smooth_params" = pick("^sds_.*_trend"),
    "trend_params" = if (has_trend_f) {
      # With a trend_formula every trend-dynamics parameter carries
      # the `_trend` suffix; drop trend-side fixed effects, smooth
      # SDs, and the bulk per-obs / per-state arrays so the keyword
      # returns scalars and short summaries only.
      cands <- pick("_trend")
      drop(cands, paste0(
        "^(b_trend\\[|sds_.*_trend|innovations_trend\\[|",
        "scaled_innovations_trend\\[|lv_trend\\[|mu_trend\\[|",
        "trend\\[|trend_states\\[|Y_pred_trend\\[)"
      ))
    } else {
      # Obs-only fits keep trend-dynamics parameters at the top level.
      cands <- pick(paste0(
        "^(ar\\d|alpha_gp|rho_gp|sdgp|lscale|sigma|tau)(\\[|$|_)"
      ))
      drop(cands, "^(trend\\[|innovations\\[|lv\\[|mu\\[)")
    }
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
#
# Univariate only in v1; multivariate fits store per-response
# standata blocks (`X_<resp>`) and need per-response prefixes —
# the helper returns an empty map for MV so positional names are
# preserved unchanged.
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
  # The trend block is the only one with a name suffix (rather than
  # prefix); all others reduce to the same template, so we drive the
  # whole map from a single sweep over `names(x$standata)`.
  # Reason: nl sub-formulas do NOT get the Intercept-centring
  # transform, so the column count of `X_<nlpar>` matches the
  # length of `b_<nlpar>` directly (Intercept stays at position 1).
  # `strip_intercept = FALSE` opts out of the centring assumption.
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
  # Non-linear sub-formulas surface via brmsformula$pforms keyed by
  # the nlpar name, but only when the top-level formula is flagged
  # with attr(., "nl") = TRUE. Capture once at the top of the
  # sweep so the per-block branch can decide whether to strip.
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
  # Smooth-term fixed-effect basis: `bs[k]` / `bs_trend[k]` map to
  # `bs_<colname>` / `bs_<colname>_trend` where colname comes from
  # the corresponding standata block (`Xs` / `Xs_trend`). The
  # `bs` vector is NOT intercept-centred (no leading "Intercept"
  # column to strip), so the index is direct.
  build_bs <- function(Xs, pos_name, alias_suffix) {
    if (is.null(Xs) || !is.matrix(Xs) || ncol(Xs) == 0L) {
      return(character(0L))
    }
    cn <- colnames(Xs)
    if (length(cn) == 0L) {
      return(character(0L))
    }
    new <- paste0("bs_", cn, alias_suffix)
    old <- paste0(pos_name, "[", seq_along(cn), "]")
    stats::setNames(old, new)
  }
  bs_parts <- c(
    build_bs(x$standata$Xs, "bs", ""),
    build_bs(x$standata$Xs_trend, "bs_trend", "_trend")
  )
  c(unlist(parts), bs_parts)
}


# Internal: replace positional Stan parameter names in `vars` with
# their brms-native aliases. The map is named character vector
# where each element's name is the alias and the value is the
# positional Stan name (the same shape produced by
# `mvgam_beta_aliases` and `mvgam_ranef_aliases`). Names not in
# the map pass through unchanged. Used by `variables.mvgam` to
# expose aliases at the character-vector layer.
#'@noRd
apply_mvgam_beta_aliases <- function(vars, alias_map) {
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


# Internal: reconstruct brms group-level metadata for a fitted
# mvgam object. Returns a list with `reframe` (the brmsfit
# `$ranef` data.frame: one row per (group, coef) pair) and
# `group_levels` (the named list of levels per grouping factor),
# or NULL when the fit has no group-level effects.
#
# The cheap gate `^M_<id>$` on `x$standata` short-circuits no-RE
# fits without paying the brms-setup cost. Trend-side blocks have
# the suffixed key `M_<id>_trend` and are deliberately excluded.
#
# Both `mvgam_ranef_aliases` and the user-facing `ranef.mvgam` /
# `VarCorr.mvgam` methods consume this metadata so the
# `brm(empty = TRUE)` setup happens via one entry point.
#'@noRd
mvgam_ranef_metadata <- function(x) {
  checkmate::assert_class(x, "mvgam")
  # brms's standata convention: obs-side group blocks are exactly
  # `M_<id>` (integer suffix only). Trend-side blocks carry the
  # `_trend` suffix (`M_<id>_trend`) and are intentionally
  # excluded from this gate. If brms ever changes the obs-side
  # key naming, the gate falls closed (no aliasing) rather than
  # producing an incorrect map — safe failure mode.
  std_names <- names(x$standata)
  has_obs_re <- any(grepl("^M_\\d+$", std_names))
  if (!has_obs_re) {
    return(NULL)
  }
  empty <- brms::brm(
    formula = x$formula, data = x$data, family = x$family,
    empty = TRUE, silent = 2
  )
  reframe <- empty$ranef
  if (is.null(reframe) || nrow(reframe) == 0L) {
    return(NULL)
  }
  list(reframe = reframe, group_levels = attr(reframe, "levels"))
}


# Internal: build alias map for random-effect parameters.
# brms's stancode emits group-level parameters in positional form
# (`r_<id>[<level_idx>,<coef_idx>]`, `sd_<id>[<coef_idx>]`,
# `cor_<id>[<flat_off_diag>]`). brms's `rename_pars` then promotes
# these to user-facing aliases keyed on the grouping factor name
# and the coefficient name (`r_<group>[<level>,<coef>]`,
# `sd_<group>__<coef>`, `cor_<group>__<coef1>__<coef2>`). mvgam
# delegates Stan-code generation to brms but does not run
# `rename_pars`, so this helper rebuilds the same map.
#
# Returns a named character vector in the same shape as
# `mvgam_beta_aliases`: names are the brms-native aliases, values
# are the positional Stan names. An empty character vector is
# returned when the fit has no group-level effects.
#
# Multi-coef correlated groups: brms stores the correlation matrix
# off-diagonals in a vector `cor_<id>[1:NC]` where
# `NC = M*(M-1)/2`. brms's stancode packs them via
# `cor_<id>[choose(k - 1, 2) + j] = Cor_<id>[j, k]` for j < k, i.e.
# column-major upper-triangle order: for M = 4 the pairs are
# (1,2), (1,3), (2,3), (1,4), (2,4), (3,4). The helper below
# walks pairs in the same order so the alias index matches brms
# byte-for-byte at any M.
#
# Trend-side random effects (REs in `trend_formula`) are deferred.
# v1 aliases only the observation-side group structure; trend REs
# remain accessible via their positional names. Extending here:
# iterate over the trend brmsterms via
# `brms::brm(formula = x$trend_formula, ..., empty = TRUE)`,
# build the same r_/sd_/cor_ maps, and append a `_trend` suffix
# to each alias name (mirrors the `_trend` suffix the beta
# aliaser already applies for the `b_trend[k]` block).
#
# Other brms RE patterns the helper inherits from `brm(empty=TRUE)`:
# multivariate response (`bf(mvbind(y1, y2) ~ (1 | g))`),
# distributional-parameter REs (`bf(y ~ ..., sigma ~ (1 | g))`),
# nested REs (`(1 | g1/g2)` expanded to `(1|g1) + (1|g1:g2)`),
# by-factor REs (`gr(g, by = f)`). brms's metadata for these
# scenarios is exposed via the same `empty$ranef` table, so the
# aliaser produces correct maps without special-casing. No test
# fixtures exist for them yet; add concordance coverage when
# user demand surfaces.
#'@noRd
mvgam_ranef_aliases <- function(x) {
  meta <- mvgam_ranef_metadata(x)
  if (is.null(meta)) {
    return(character(0L))
  }
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
    # Stan parameter form depends on whether brms estimates a
    # correlation matrix for this group:
    #   - correlated (M >= 2, cor = TRUE): a single matrix
    #     `r_<id>[<level_idx>, <coef_idx>]` is emitted.
    #   - uncorrelated or single-coef: per-coef vectors
    #     `r_<id>_<coef_idx>[<level_idx>]` are emitted.
    # Both forms alias to the same user-facing
    # `r_<group>[<level>, <coef>]` name.
    grid <- expand.grid(
      level_idx = seq_len(n_lvl),
      coef_idx = seq_len(n_coef),
      KEEP.OUT.ATTRS = FALSE
    )
    r_old <- if (has_cor) {
      sprintf("r_%d[%d,%d]", id, grid$level_idx, grid$coef_idx)
    } else {
      sprintf("r_%d_%d[%d]", id, grid$coef_idx, grid$level_idx)
    }
    r_new <- sprintf(
      "r_%s[%s,%s]", group,
      levels[grid$level_idx], coefs[grid$coef_idx]
    )
    r_map <- stats::setNames(r_old, r_new)
    # sd_<id>[<coef_idx>] -> sd_<group>__<coef>
    sd_old <- sprintf("sd_%d[%d]", id, seq_len(n_coef))
    sd_new <- sprintf("sd_%s__%s", group, coefs)
    sd_map <- stats::setNames(sd_old, sd_new)
    # cor_<id>[<k>] -> cor_<group>__<coef_j>__<coef_k>
    # Pair order follows brms's column-major upper-triangle packing
    # (`choose(k - 1, 2) + j` for j < k); see comment block above.
    cor_map <- character(0L)
    if (has_cor) {
      ks <- rep(2:n_coef, times = seq_len(n_coef - 1L))
      js <- unlist(lapply(2:n_coef, function(k) seq_len(k - 1L)))
      cor_old <- sprintf("cor_%d[%d]", id, seq_along(js))
      cor_new <- sprintf(
        "cor_%s__%s__%s", group, coefs[js], coefs[ks]
      )
      cor_map <- stats::setNames(cor_old, cor_new)
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
  drws <- posterior::as_draws_array(x$fit, inc_warmup = inc_warmup)
  alias_map <- c(mvgam_beta_aliases(x), mvgam_ranef_aliases(x))
  if (length(alias_map) > 0L) {
    # Only rename entries whose positional name is actually present
    # in the draws. brms's rename_pars is similarly tolerant: a fit
    # that pre-aliased itself (e.g. test stubs) becomes a no-op.
    have <- alias_map %in% posterior::variables(drws)
    if (any(have)) {
      drws <- do.call(
        posterior::rename_variables,
        c(list(drws), as.list(alias_map[have]))
      )
    }
  }
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
      resolve_mvgam_keyword(k, x, all_vars)
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
  # not match — that's a typo signal worth catching.
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
                                 regex = FALSE, use_alias = TRUE,
                                 ...) {
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
as.matrix.mvgam <- function(x, variable = NULL, regex = FALSE,
                             use_alias = TRUE, ...) {
  posterior::as_draws_matrix(
    extract_mvgam_draws(x, variable, regex)
  )
}


#' @rdname mvgam_draws
#' @export
as.array.mvgam <- function(x, variable = NULL, regex = FALSE,
                            use_alias = TRUE, ...) {
  extract_mvgam_draws(x, variable, regex)
}


#' @rdname mvgam_draws
#' @method as_draws mvgam
#' @export
as_draws.mvgam <- function(x, variable = NULL, regex = FALSE,
                            inc_warmup = FALSE, use_alias = TRUE,
                            ...) {
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
                                   inc_warmup = FALSE,
                                   use_alias = TRUE, ...) {
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
                               inc_warmup = FALSE,
                               use_alias = TRUE, ...) {
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
                                  inc_warmup = FALSE,
                                  use_alias = TRUE, ...) {
  extract_mvgam_draws(x, variable, regex, inc_warmup)
}


#' @export
#' @importFrom posterior as_draws_array
posterior::as_draws_array


#' @rdname mvgam_draws
#' @method as_draws_list mvgam
#' @export
as_draws_list.mvgam <- function(x, variable = NULL, regex = FALSE,
                                 inc_warmup = FALSE,
                                 use_alias = TRUE, ...) {
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
  posterior::as_draws_rvars(
    extract_mvgam_draws(x, variable, regex, inc_warmup)
  )
}


#' @export
#' @importFrom posterior as_draws_rvars
posterior::as_draws_rvars
