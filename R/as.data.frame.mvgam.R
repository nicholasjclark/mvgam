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
# present in `all_vars`. The patterns below are brms-native: master
# called into `x$mgcv_model`, `x$sp_names`, `trend_par_names()` and
# `family_param_info()`, none of which apply on the brms-integrated
# branch. The trend-dynamics block (`trend_params`) is the one place
# context matters — with a `trend_formula` those parameters carry the
# `_trend` suffix, otherwise they sit at the top level.
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
  # brms's stancode centres the design matrix whenever an Intercept
  # column is present: `b[k]` then enumerates the K - 1 non-Intercept
  # columns. Without an Intercept the mapping is direct. Every
  # linear predictor gets its own design-matrix block in standata:
  #   `X`             obs main formula  -> b[k]            -> b_<term>
  #   `X_<resp>`      MV response       -> b_<resp>[k]     -> b_<resp>_<term>
  #   `X_<dpar>`      dpar formula      -> b_<dpar>[k]     -> b_<dpar>_<term>
  #   `X_trend`       mvgam-only        -> b_trend[k]      -> b_<term>_trend
  # The trend block is the only one with a name suffix (rather than
  # prefix); all others reduce to the same template, so we drive the
  # whole map from a single sweep over `names(x$standata)`.
  build <- function(X, pos_prefix, alias_prefix, alias_suffix) {
    if (is.null(X) || !is.matrix(X) || ncol(X) == 0L) {
      return(character(0L))
    }
    cn <- colnames(X)
    if (length(cn) == 0L) {
      return(character(0L))
    }
    if (identical(cn[1L], "Intercept")) {
      cn <- cn[-1L]
    }
    if (length(cn) == 0L) {
      return(character(0L))
    }
    new <- paste0(alias_prefix, cn, alias_suffix)
    old <- paste0(pos_prefix, "[", seq_along(cn), "]")
    stats::setNames(old, new)
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
        alias_suffix = ""
      )
    }
  })
  unlist(parts)
}


# Internal: replace `b[k]` / `b_trend[k]` entries in `vars` with
# their brms-native aliases. Names that are not in the map pass
# through unchanged. Used by `variables.mvgam` to expose the alias
# at the character-vector layer.
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
  alias_map <- mvgam_beta_aliases(x)
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
                                 variable = NULL, regex = FALSE,
                                 use_alias = TRUE, ...) {
  as.data.frame(posterior::as_draws_df(
    extract_mvgam_draws(x, variable, regex)
  ))
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
