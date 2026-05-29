#' brms-delegation prediction path
#'
#' Builds a brmsfit suitable for `brms::posterior_linpred` /
#' `posterior_epred` / `posterior_predict` from an mvgam fit by:
#'
#'  1. slicing the relevant component's parameter columns out of
#'     `mvgam_fit$fit` (the combined Stan output);
#'  2. attaching them as the brmsfit template's `$fit` slot;
#'  3. running brms's param renamer so the positional Stan names map
#'     to brms's formula-derived friendly names (the names brms's
#'     posterior_* code expects to find).
#'
#' After rename the brmsfit is a fully-functional brms object: brms
#' handles all observation-model prediction work (fixed effects,
#' smooths, GPs including by-variable, random effects, monotonic,
#' ordinal-3D, multivariate). mvgam's only specific contribution to
#' the linear predictor is the latent state `trend[t, s]`, which is
#' added on top by the caller.
#'
#' This path is the architectural intent expressed in
#' `architecture/architecture-decisions.md` (Stan Optimization
#' Preservation: "Let brms handle observation model complexity
#' entirely.")
#'
#' @importFrom cli format_inline
#' @name mvgam_brms_delegation
#' @keywords internal
NULL


#' Reconstitute a component brmsfit for brms-side prediction
#'
#' @param mvgam_fit A fitted `mvgam` object.
#' @param component One of `"obs"` or `"trend"`. For `"trend"` the
#'   `_trend` suffix is stripped from parameter names so they line up
#'   with the trend submodel's brms-generated stancode.
#' @return A brmsfit with a populated, renamed `$fit` slot. Errors if
#'   the trend submodel was not stored on the mvgam object.
#' @noRd
build_component_brmsfit <- function(mvgam_fit, component = c("obs", "trend")) {
  component <- match.arg(component)
  brm_template <- switch(component,
    obs   = mvgam_fit$obs_model,
    trend = mvgam_fit$trend_model
  )
  if (is.null(brm_template)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "No {.field {component}_model} brmsfit template on the mvgam fit."
      ),
      i = "This component cannot be predicted via brms delegation."
    )))
  }

  param_names <- collect_component_params(mvgam_fit, component)
  fit_slice <- slice_stanfit(
    mvgam_fit$fit,
    keep = param_names,
    strip_suffix = if (component == "trend") "_trend" else NULL
  )
  brm_template$fit <- fit_slice
  getFromNamespace("rename_pars", "brms")(brm_template)
}


#' Collect the parameter names that belong to a given component
#'
#' For obs we use the existing obs-parameter classifier plus the
#' multivariate residual-correlation params (`Lrescor`, `rescor`)
#' which `extract_obs_parameters` does not currently emit but brms
#' needs for `mvbind` posterior_linpred.
#'
#' For trend we use the existing trend-parameter classifier; the
#' `_trend` suffix is removed downstream by `slice_stanfit`.
#'
#' @noRd
collect_component_params <- function(mvgam_fit, component) {
  if (component == "obs") {
    # Anchor `(L?)rescor` to a `[` index or a string end so
    # user-named columns containing `rescor` cannot accidentally
    # match (e.g. a covariate called `rescore`).
    extra <- grep(
      "^(L)?rescor($|\\[)",
      colnames(posterior::as_draws_matrix(mvgam_fit$fit)),
      value = TRUE
    )
    c(extract_obs_parameters(mvgam_fit), extra)
  } else {
    extract_trend_parameters(mvgam_fit)
  }
}


#' Slice a stanfit down to a parameter subset
#'
#' Returns a stanfit copy whose `@sim$samples` chain lists contain
#' only the `keep` parameter flat-names. Optionally strips a fixed
#' suffix (e.g. `_trend`) from each retained name so they align with
#' the brms template's stancode position names. All `@sim` and
#' top-level slots that index parameters (`pars_oi`, `dims_oi`,
#' `fnames_oi`, `n_flatnames`, `model_pars`, `par_dims`) are kept in
#' sync.
#'
#' @noRd
slice_stanfit <- function(stanfit, keep, strip_suffix = NULL) {
  checkmate::assert_class(stanfit, "stanfit")
  checkmate::assert_character(keep, min.len = 1L, any.missing = FALSE)
  checkmate::assert_string(strip_suffix, null.ok = TRUE)
  sim <- stanfit@sim
  all_flat <- names(sim$samples[[1L]])
  keep_flat <- intersect(all_flat, keep)
  if (length(keep_flat) == 0L) {
    stop(insight::format_error(c(
      "No matching parameters found in stanfit for slice.",
      i = cli::format_inline(
        "Asked for {length(keep)} parameters; none matched the stanfit's flat names."
      )
    )))
  }

  # Anchor `strip_suffix` to the END of each name so internal
  # occurrences (e.g. a user-named parameter whose root happens to
  # contain the suffix) survive untouched. Bracket indices like
  # [t, s] sit after the parameter root, so split into (root, idx)
  # and strip only from the root.
  renamed_flat <- if (is.null(strip_suffix)) {
    keep_flat
  } else {
    roots <- sub("(\\[.*\\])?$", "", keep_flat)
    idxs <- regmatches(keep_flat, regexpr("\\[.*\\]$", keep_flat))
    # regmatches returns character(0) for entries without a match;
    # pad to a per-name vector of "" so paste stays aligned
    idx_vec <- character(length(keep_flat))
    has_idx <- grepl("\\[.*\\]$", keep_flat)
    idx_vec[has_idx] <- idxs
    roots <- sub(paste0(strip_suffix, "$"), "", roots)
    paste0(roots, idx_vec)
  }

  fit <- stanfit
  fit@sim$samples <- lapply(sim$samples, function(chain) {
    out <- chain[keep_flat]
    names(out) <- renamed_flat
    out
  })

  scalar_pars <- unique(sub("\\[.*\\]$", "", renamed_flat))
  fit@sim$pars_oi <- scalar_pars

  dims_renamed <- list()
  for (p in scalar_pars) {
    src <- if (is.null(strip_suffix)) p else paste0(p, strip_suffix)
    if (src %in% names(sim$dims_oi)) {
      dims_renamed[[p]] <- sim$dims_oi[[src]]
    }
  }
  fit@sim$dims_oi <- dims_renamed
  fit@sim$fnames_oi <- renamed_flat
  fit@sim$n_flatnames <- length(renamed_flat)
  fit@model_pars <- scalar_pars
  fit@par_dims <- dims_renamed
  fit
}


#' Component linpred via brms delegation
#'
#' Drop-in alternative to `extract_component_linpred` that lets brms
#' compute the covariate / smooth / GP / RE / monotonic / ordinal-3D
#' contributions, returning the same `[ndraws x nobs]` matrix (or
#' `[ndraws x nobs x ncat]` array for ordinal epred) shape.
#'
#' Caller is responsible for adding the latent state `trend[t, s]` on
#' top via `extract_trend_latent_states()`; this function returns
#' only the covariate-side linpred.
#'
#' @param mvgam_fit A fitted `mvgam`.
#' @param newdata Data frame for prediction.
#' @param component `"obs"` or `"trend"`.
#' @param resp Optional response name for multivariate models.
#' @param ndraws Optional integer subset.
#' @param re_formula brms `re_formula` argument.
#' @param allow_new_levels brms argument; defaults to `TRUE` so new
#'   factor levels in newdata (common for prediction grids) are
#'   accepted.
#' @param sample_new_levels brms argument.
#' @return Matrix (or list for multivariate when `resp = NULL`).
#' @noRd
extract_component_linpred_via_brms <- function(mvgam_fit, newdata,
                                               component = "obs",
                                               resp = NULL,
                                               ndraws = NULL,
                                               re_formula = NULL,
                                               allow_new_levels = TRUE,
                                               sample_new_levels = "uncertainty") {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_choice(component, c("obs", "trend"))
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_logical(allow_new_levels, len = 1L)
  rebuilt <- build_component_brmsfit(mvgam_fit, component)
  brms::posterior_linpred(
    rebuilt,
    newdata = newdata,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp,
    incl_autocor = FALSE
  )
}
