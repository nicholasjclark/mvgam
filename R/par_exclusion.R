# Which parameters a fit does not keep
#
# brms computes this list before sampling and passes it to Stan, and a
# model's working variables never reach the posterior: the
# standardised deviates a group-level block is scaled from, the
# unscaled coefficients a shrinkage prior scales, the ordered
# intercepts a mixture is identified by. The model reports each of
# these under another name, and a reader offered both would meet one
# parameter twice.
#
# mvgam assembles its program from two brms models and applies the
# same rules to each. The rules are `brms:::exclude_pars()`, which
# brms does not export, and this file uses only brms's exported
# functions. `brmsterms()` gives the response, distributional and
# non-linear parameters whose names a working variable takes, and the
# stored brmsfit holds the group-level table. Every trend-side name
# takes mvgam's `_trend` suffix.
#
# mvgam's own working arrays stay in the posterior, where forecasting
# and the post-fit summaries use them. `mvgam_par_kind()` hides them
# when a reader is shown parameter names.


#' The parameters a fit excludes
#'
#' @param obs_model The observation side's stored brmsfit.
#' @param trend_model The trend side's stored brmsfit, or NULL.
#' @param standata The combined Stan data, which names the smooth and
#'   measurement-error blocks the program declares.
#' @param save_pars A `brms::save_pars()` object.
#' @param stancode The combined Stan program, or NULL.
#' @return Character vector of Stan variable names. Both backends
#'   ignore a name the program does not declare. The list is not
#'   intersected with the program.
#' @noRd
mvgam_excluded_pars <- function(obs_model, trend_model = NULL,
                                standata = NULL,
                                save_pars = brms::save_pars(),
                                stancode = NULL) {
  checkmate::assert_class(save_pars, "save_pars")
  out <- brms_working_pars(obs_model, standata, save_pars, suffix = "")
  if (!is.null(trend_model)) {
    out <- c(
      out,
      brms_working_pars(trend_model, standata, save_pars, suffix = "_trend")
    )
  }
  if (!save_pars$all && mu_trend_is_zero(stancode)) {
    out <- c(out, "mu_trend")
  }
  unique(setdiff(out, save_pars$manual))
}


#' Does the program leave `mu_trend` at its zero start?
#'
#' A trend formula with no terms gives `mu_trend` no design. The
#' program declares it as `rep_vector(0.0, N_trend)` and never assigns
#' to it again, and every draw of it is zero. Stored, it adds `N_trend`
#' columns of zeros to each draw, and its R-hat and effective sample
#' size are undefined: rstan warns on every fit, and `rhat()` and
#' `neff_ratio()` report `NA` for each entry. `trend` carries
#' `mu_trend`, and no post-fit method extracts the vector by name.
#'
#' @param stancode The combined Stan program, or NULL.
#' @return `TRUE` when the program declares `mu_trend` at zero and
#'   never assigns to it afterwards.
#' @noRd
mu_trend_is_zero <- function(stancode) {
  if (is.null(stancode)) {
    return(FALSE)
  }
  code <- stan_line_code(strsplit(
    paste(stancode, collapse = "\n"), "\n", fixed = TRUE
  )[[1L]])
  zero_start <- grepl(
    "\\bmu_trend\\s*=\\s*rep_vector\\(\\s*0(\\.0*)?\\s*,", code
  )
  assigned <- grepl(
    "\\bmu_trend\\b(\\s*\\[[^]]*\\])?\\s*[-+*/]?=(?!=)", code,
    perl = TRUE
  )
  sum(zero_start) == 1L && sum(assigned) == 1L &&
    identical(which(zero_start), which(assigned))
}


#' The working parameters of one side's brms model
#'
#' @param bfit A stored brmsfit, or NULL.
#' @param standata The combined Stan data.
#' @param save_pars A `brms::save_pars()` object.
#' @param suffix `"_trend"` for the trend side, `""` for the
#'   observation side.
#' @return Character vector of Stan variable names.
#' @noRd
brms_working_pars <- function(bfit, standata, save_pars, suffix = "") {
  checkmate::assert_string(suffix)
  if (is.null(bfit) || is.null(bfit$formula)) {
    return(character(0L))
  }
  bterms <- brms::brmsterms(bfit$formula)
  frames <- if (inherits(bterms, "mvbrmsterms")) bterms$terms else list(bterms)

  out <- character(0L)
  # A model with several responses estimates their residual
  # correlations jointly
  if (inherits(bterms, "mvbrmsterms")) {
    out <- c(out, "Rescor", "Sigma")
    if (!save_pars$all) {
      out <- c(out, "Lrescor", "LSigma")
    }
  }
  for (frame in frames) {
    resp <- predictor_suffix(resp = null_if_blank(frame$resp))
    out <- c(out, paste0(c("Lncor", "Cortime"), resp))
    if (!save_pars$all) {
      out <- c(out, paste0(
        c("ordered_Intercept", "fixed_Intercept", "theta", "Llncor",
          "Lcortime"),
        resp
      ))
    }
    # A response modelled with `mi()` carries its own latent values,
    # and one with a residual correlation structure carries a residual
    # term. `save_pars()` names which of each a user keeps.
    if (inherits(frame$adforms$mi, "formula") &&
        !(isTRUE(save_pars$latent) || frame$resp %in% save_pars$latent)) {
      out <- c(out, paste0("Yl", resp))
    }
    if (!(isTRUE(save_pars$group) || ".err" %in% save_pars$group)) {
      out <- c(out, paste0("err", resp))
    }
  }
  # Every linear predictor names its working variables by its own
  # suffix.
  for (lp in model_predictors(bfit$formula)) {
    out <- c(out, paste0("chol_cor", lp$suffix))
    if (!save_pars$all) {
      out <- c(out, paste0(
        c("bQ", "zb", "zbsp", "zbs", "zar", "zma", "hs_local", "R2D2_phi",
          "scales", "merged_Intercept", "zcar", "nszcar", "zerr"),
        lp$suffix
      ))
    }
  }
  if (!save_pars$all) {
    out <- c(out, smooth_working_pars(standata, suffix))
  }
  out <- c(out, ranef_working_pars(bfit$ranef, save_pars))
  out <- c(out, me_working_pars(standata, save_pars, suffix))
  if (nzchar(suffix)) {
    out <- paste0(out, suffix)
  }
  out
}


#' The standardised smooth coefficients a program declares
#'
#' brms gives each penalised part of a smooth a standardised
#' coefficient `zs<sfx>_<i>_<j>`, scales it into `s<sfx>_<i>_<j>` and
#' keeps the scaled one. The data block holds the matching `Zs` basis
#' for each part, and the program's own data name the parts.
#'
#' @param standata The combined Stan data.
#' @param suffix `"_trend"` on the trend side, `""` otherwise.
#' @return Character vector of `zs...` names, without the side suffix,
#'   which the caller applies.
#' @noRd
smooth_working_pars <- function(standata, suffix = "") {
  if (is.null(standata)) {
    return(character(0L))
  }
  bases <- grep(paste0("^Zs(_.+)?_[0-9]+_[0-9]+", suffix, "$"),
                names(standata), value = TRUE)
  if (length(bases) == 0L) {
    return(character(0L))
  }
  sub("^Zs", "zs", strip_side_suffix(bases, suffix))
}


#' The working parameters of a group-level block
#'
#' @param reframe The stored brmsfit's `$ranef` table, or NULL.
#' @param save_pars A `brms::save_pars()` object.
#' @return Character vector of Stan variable names.
#' @noRd
ranef_working_pars <- function(reframe, save_pars) {
  if (is.null(reframe) || nrow(reframe) == 0L) {
    return(character(0L))
  }
  classes <- c(if (!save_pars$all) c("z", "L"), "Cor", "r")
  out <- unlist(lapply(unique(reframe$id), function(id) {
    paste0(classes, "_", id)
  }))
  # `group` names the grouping factors whose per-level deviations a
  # user keeps; FALSE keeps none of them
  if (isFALSE(save_pars$group)) {
    out <- c(out, ranef_coef_pars(reframe))
  } else if (is.character(save_pars$group)) {
    dropped <- reframe[!reframe$group %in% save_pars$group, , drop = FALSE]
    out <- c(out, ranef_coef_pars(dropped))
  }
  # A block with student-t deviations carries two scaling variables
  if (!save_pars$all && !is.null(reframe$dist)) {
    student <- reframe[reframe$dist == "student", , drop = FALSE]
    if (nrow(student) > 0L) {
      out <- c(out, paste0(c("udf_", "dfm_"), student$ggn))
    }
  }
  as.character(out)
}


#' The per-level coefficients of a group-level table's rows
#'
#' @param reframe Rows of a stored brmsfit's `$ranef` table.
#' @return Character vector of `r_<id><sfx>_<cn>` names, one per row.
#' @noRd
ranef_coef_pars <- function(reframe) {
  if (nrow(reframe) == 0L) {
    return(character(0L))
  }
  sfx <- vapply(
    seq_len(nrow(reframe)),
    function(i) {
      predictor_suffix(
        resp = null_if_blank(reframe$resp[i]),
        dpar = null_if_blank(reframe$dpar[i]),
        nlpar = null_if_blank(reframe$nlpar[i])
      )
    },
    character(1L)
  )
  paste0("r_", reframe$id, sfx, "_", reframe$cn)
}


#' The working parameters of a measurement-error block
#'
#' `me()` gives each noisy covariate a latent value scaled from a
#' standardised deviate. The data block carries one `Xn_<k>` per
#' covariate and one `Mme_<i>` per group of them.
#'
#' @param standata The combined Stan data.
#' @param save_pars A `brms::save_pars()` object.
#' @param suffix `"_trend"` on the trend side, `""` otherwise.
#' @return Character vector of Stan variable names, without the side
#'   suffix, which the caller applies.
#' @noRd
me_working_pars <- function(standata, save_pars, suffix = "") {
  if (is.null(standata)) {
    return(character(0L))
  }
  covariates <- strip_side_suffix(
    grep(paste0("^Xn_[0-9]+", suffix, "$"), names(standata), value = TRUE),
    suffix
  )
  if (length(covariates) == 0L) {
    return(character(0L))
  }
  groups <- strip_side_suffix(
    grep(paste0("^Mme_[0-9]+", suffix, "$"), names(standata), value = TRUE),
    suffix
  )
  k <- sub("^Xn_", "", covariates)
  i <- sub("^Mme_", "", groups)
  out <- paste0("Corme_", i)
  if (!save_pars$all) {
    out <- c(out, paste0("zme_", k), paste0("Lme_", i))
  }
  # `latent` names the covariates whose latent values a user keeps
  if (isFALSE(save_pars$latent)) {
    out <- c(out, paste0("Xme_", k))
  } else if (is.character(save_pars$latent)) {
    kept <- vapply(k, function(idx) {
      entry <- standata[[paste0("Xn_", idx, suffix)]]
      any(save_pars$latent %in% c(colnames(entry), names(entry)))
    }, logical(1L))
    out <- c(out, paste0("Xme_", k[!kept]))
  }
  out
}


#' Names with the side suffix taken off
#'
#' @param x Character vector of Stan data names.
#' @param suffix `"_trend"` or `""`.
#' @return `x` without the suffix.
#' @noRd
strip_side_suffix <- function(x, suffix) {
  if (!nzchar(suffix)) {
    return(x)
  }
  sub(paste0(suffix, "$"), "", x)
}
