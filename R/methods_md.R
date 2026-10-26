#' The equations of a model, as Markdown
#'
#' Writes the model in mathematical notation for a methods section. The
#' Data section names the response, the dimensions and the predictors.
#' The Model section holds the likelihood, the linear predictors and the
#' latent dynamics in one display equation, with a glossary of every
#' symbol. The Priors section states every prior the Stan program
#' applies. The Implementation section gives the call that fits the
#' model and the software versions the fit recorded. The equations are
#' LaTeX inside `$$` fences, which Quarto, R Markdown and MathJax render.
#'
#' @param object A fitted `mvgam` or `jsdgam` object, or a prefit from
#'   `mvgam(..., run_model = FALSE)`.
#' @param file Optional path. The Markdown is written there and returned
#'   invisibly.
#' @param implementation Logical. `TRUE` (the default) adds the
#'   Implementation section.
#' @param heading_level Integer Markdown level of the section headings.
#'   The default `2` writes `## Model`. A higher level nests the
#'   sections under a heading of the host document.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A character string of class `mvgam_methods_md`, which prints
#'   as its Markdown.
#'
#' @author Nicholas J Clark
#' @seealso [how_to_cite()] writes the matching methods paragraph and
#'   its references. [prior_summary()] lists the priors as a table.
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
#' # The equations, glossary and priors as Markdown
#' md <- methods_md(mod)
#' md
#'
#' # Written to a file, nested under a level-two heading
#' methods_md(mod, file = tempfile(fileext = ".md"), heading_level = 3L)
#'
#' # Next, how_to_cite() writes the methods paragraph
#' }
#'
#' @family posterior summaries
#' @export
methods_md <- function(object, file = NULL, implementation = TRUE, ...) {
  UseMethod("methods_md", object)
}

#' @rdname methods_md
#' @export
methods_md.mvgam <- function(object, file = NULL,
                             implementation = TRUE,
                             heading_level = 2L, ...) {
  checkmate::assert_class(object, "mvgam")
  # Every section renders the model's family and link from here. A
  # fit and a prefit both store the family `mvgam()` validated.
  checkmate::assert_class(object$family, "family",
                          .var.name = "object$family")
  checkmate::assert_string(file, null.ok = TRUE)
  checkmate::assert_flag(implementation)
  checkmate::assert_int(heading_level, lower = 1L, upper = 6L)
  rlang::check_dots_empty()

  sections <- methods_md_section_registry(
    implementation = implementation
  )
  parts <- vapply(sections, function(render) render(object), character(1L))
  body <- paste(parts, collapse = "\n\n")
  # Each section writes its heading at level two.
  body <- gsub("(^|\n)## ", paste0("\\1", strrep("#", heading_level), " "),
               body)

  out <- structure(
    body,
    class = c("mvgam_methods_md", "character")
  )

  if (!is.null(file)) {
    writeLines(body, con = file)
    return(invisible(out))
  }
  out
}

#' @export
print.mvgam_methods_md <- function(x, ...) {
  cat(x, sep = "")
  invisible(x)
}


#' The section renderers of `methods_md()`, in page order
#'
#' Each renderer takes the model and returns its section as one
#' Markdown string.
#'
#' @param implementation Logical. `TRUE` adds the Implementation
#'   section.
#' @return Named list of functions
#' @noRd
methods_md_section_registry <- function(implementation = TRUE) {
  reg <- list(
    data   = render_data_section,
    model  = render_model_section,
    priors = render_priors_section
  )
  if (implementation) {
    reg$implementation <- render_implementation_section
  }
  reg
}


#' One display equation from a list of statements
#'
#' Each row is a statement `list(lhs, op, rhs)`, where `op` is a
#' relation such as `"\\sim"` or `"="`. The rows form one
#' `aligned` environment inside `$$`, aligned at the relation. Quarto,
#' R Markdown, MathJax and KaTeX render it.
#'
#' @param rows List of statements
#' @return Character vector of lines, empty for no rows
#' @noRd
align_block <- function(rows) {
  if (length(rows) == 0L) return(character(0L))
  body <- vapply(rows, function(r) {
    paste0(r$lhs, " &", r$op, " ", r$rhs)
  }, character(1L))
  body[-length(body)] <- paste0(body[-length(body)], " \\\\")
  c("$$", "\\begin{aligned}", body, "\\end{aligned}", "$$")
}


#' The Data section: the response, the dimensions and the predictors
#'
#' @param obj An `mvgam` object
#' @return Markdown string
#' @noRd
render_data_section <- function(obj) {
  dims <- describe_data_dimensions(obj)

  # A multivariate model names each response column and its family
  responses <- unname(response_columns(obj))
  resp_line <- if (length(responses) > 1L) {
    labels <- vapply(model_families(obj), family_data_label, character(1L))
    responses <- vapply(responses, escape_math_text, character(1L),
                        USE.NAMES = FALSE)
    paste0(
      "$\\mathbf{Y} = (",
      paste0(responses, collapse = ", "),
      ")$, observed jointly per (i, t): ",
      paste0("$", responses, "$ ", labels, collapse = "; ")
    )
  } else {
    paste0("$", response_letter(obj), "$: ", family_data_label(obj$family))
  }

  lines <- c(
    "## Data",
    "",
    resp_line
  )
  if (length(dims) > 0L) {
    lines <- c(lines, "", paste0(dims, collapse = ", "))
  }
  pred_lines <- describe_predictors(obj)
  if (length(pred_lines) > 0L) {
    lines <- c(lines, "", "Predictors:", "", pred_lines)
  }
  paste(lines, collapse = "\n")
}

#' One Markdown bullet per covariate of the linear predictors
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown lines
#' @noRd
describe_predictors <- function(obj) {
  data <- obj$data %||% data.frame()
  if (nrow(data) == 0L) return(character(0L))
  resp_cols <- unname(response_columns(obj))
  # Only the terms of a linear predictor are listed. Trial counts,
  # offsets and grouping factors are left out.
  used_vars <- mvgam_term_list(obj)$conditional
  # The dimensions line counts the response, time and series columns
  vars <- axis_vars(obj)
  skip <- unique(c(resp_cols, vars$time_var, vars$series_var))
  predictors <- intersect(setdiff(names(data), skip), used_vars)
  if (length(predictors) == 0L) return(character(0L))
  out <- character(0L)
  for (nm in predictors) {
    col <- data[[nm]]
    desc <- if (is.factor(col)) {
      paste0(
        "factor with ", nlevels(col), " levels (",
        paste(utils::head(levels(col), 4L), collapse = ", "),
        if (nlevels(col) > 4L) ", ..." else "",
        ")"
      )
    } else if (is.integer(col)) {
      "integer covariate"
    } else if (is.numeric(col)) {
      "continuous covariate"
    } else if (is.logical(col)) {
      "logical covariate"
    } else if (inherits(col, "Date") || inherits(col, "POSIXt")) {
      "date / time covariate"
    } else {
      paste0(class(col)[[1L]], " covariate")
    }
    out <- c(out, paste0("- $", escape_math_text(nm), "$: ", desc))
  }
  out
}

#' The dimensions line of the Data section
#'
#' The counts are those `print()` shows, from the axes the model
#' resolved. A response-keyed frame has no series to count.
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown phrases
#' @noRd
describe_data_dimensions <- function(obj) {
  n_obs <- nrow(obj$data %||% data.frame())
  counts <- printed_axis_counts(obj)
  parts <- character(0L)
  if (n_obs > 0L) {
    parts <- c(parts, paste0("$N = ", n_obs, "$ observations"))
  }
  if (!is.null(counts$n_series)) {
    parts <- c(parts, paste0("$S = ", counts$n_series, "$ series"))
  }
  if (!is.null(counts$n_timepoints)) {
    parts <- c(parts, paste0("$T = ", counts$n_timepoints, "$ time points"))
  }
  parts <- c(parts, closure_unit_data_dimensions(obj))
  parts <- c(parts, factor_loadings_data_dimensions(obj))
  parts
}

#' The number of closure units and of visits per unit
#'
#' Only the occupancy and N-mixture families have repeat visits. A
#' multi-response family holds one vector observation per unit.
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown phrases, empty for other
#'   families
#' @noRd
closure_unit_data_dimensions <- function(obj) {
  if (!is_closure_unit_family(obj$family)) return(character(0L))
  data <- obj$data %||% data.frame()
  if (nrow(data) == 0L) return(character(0L))
  keys <- fit_closure_unit_keys(obj)
  assert_closure_unit_columns(data, keys)
  index <- closure_unit_index(data, keys)
  visits <- index$n_rep
  c(
    paste0("$G = ", length(index$levels), "$ closure units"),
    paste0("$\\bar J = ", round(mean(visits), 2L), "$ visits per unit ",
           "(range ", min(visits), "-", max(visits), ")")
  )
}

#' The dimensions of structured factor loadings
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown phrases
#' @noRd
factor_loadings_data_dimensions <- function(obj) {
  spec <- first_trend_spec(obj)
  if (is.null(spec)) return(character(0L))
  ls <- spec$loadings_prior_spec
  if (is.null(ls)) return(character(0L))
  traits <- loadings_spec_traits(ls)
  parts <- character(0L)
  if (traits$features) {
    parts <- c(parts, paste0(
      "$p_{\\text{features}} = ", ls$N_features_trend, "$ ",
      ngettext(ls$N_features_trend, "trait feature", "trait features")
    ))
  }
  if (traits$distances) {
    parts <- c(parts, paste0(
      "$p_{\\text{distances}} = ", ls$n_distances, "$ ",
      ngettext(ls$n_distances, "distance matrix", "distance matrices")
    ))
  }
  parts
}

#' The kind of data a family describes, for the Data section
#'
#' @param family A `family` object
#' @return Character phrase
#' @noRd
family_data_label <- function(family) {
  fam_name <- resolve_family_name(family)
  seasons <- if (is_multi_season_family(family)) "multi-season" else {
    "single-season"
  }
  switch(
    fam_name,
    poisson = "non-negative integer counts",
    binomial = "binomial counts",
    com_binomial = "counts out of a known number of trials",
    beta_binomial = "counts out of a known number of trials",
    bernoulli = "binary observations",
    gaussian = "real-valued observations",
    student = "real-valued observations",
    asym_laplace = "real-valued observations",
    lognormal = "positive real observations",
    gamma = "positive real observations",
    beta = "proportions in (0, 1)",
    negbinomial = "non-negative integer counts",
    tweedie = "non-negative real observations (compound Poisson-gamma)",
    beta_nb = "heavy-tailed counts (beta negative binomial)",
    hurdle_poisson = "non-negative integer counts with point mass at zero",
    hurdle_negbinomial = "non-negative integer counts with point mass at zero",
    hurdle_gamma = "non-negative real observations with point mass at zero",
    hurdle_lognormal = "non-negative real observations with point mass at zero",
    zero_inflated_poisson = "zero-inflated non-negative integer counts",
    zero_inflated_negbinomial = "zero-inflated non-negative integer counts",
    zero_inflated_beta = "zero-inflated proportions in [0, 1)",
    zero_inflated_binomial = "zero-inflated binomial counts",
    cumulative = "ordered categorical observations",
    sratio = "ordered categorical observations",
    cratio = "ordered categorical observations",
    acat = "ordered categorical observations",
    occ = paste0("per-visit binary detections (", seasons, " occupancy)"),
    nmix = paste0(
      "per-visit binomial counts (", seasons, " N-mixture, ",
      "Poisson abundance + binomial detection)"
    ),
    nmix_royle_nichols = paste0(
      "per-visit binary detections (", seasons, " Royle-Nichols, ",
      "Poisson abundance + per-individual detection)"
    ),
    nmix_poisson_poisson = paste0(
      "per-visit counts (", seasons, " N-mixture, Poisson abundance + ",
      "Poisson encounter rate, Neyman Type A marginal)"
    ),
    mvn = "joint multivariate normal observations per unit",
    mvt = "joint multivariate Student-t observations per unit",
    diri = "per-unit simplex compositions",
    multi = "per-unit multinomial category counts",
    categ = "per-unit single categorical draws",
    paste0(fam_name, " observations")
  )
}


#' The Model section: one display equation and its glossary
#'
#' The equation holds the index ranges, the likelihood and its link,
#' the definitions of the mean's terms, the distributional and
#' non-linear predictors with their terms, the trend predictor, the
#' latent dynamics and the factor loadings, in that order.
#'
#' @param obj An `mvgam` object
#' @return Markdown string
#' @noRd
render_model_section <- function(obj) {
  rows <- c(
    index_range_rows(obj),
    likelihood_rows(obj),
    unlist(lapply(response_views(obj), term_definition_rows),
           recursive = FALSE),
    sub_predictor_rows(obj),
    trend_predictor_rows(obj),
    latent_dynamics_rows(obj),
    factor_model_rows(obj)
  )
  paste(c("## Model", "", align_block(rows), "", model_glossary(obj)),
        collapse = "\n")
}

#' The likelihood and the link of the mean
#'
#' With correlated residuals, the responses of a multivariate model
#' share one multivariate normal. Otherwise each response has its own
#' likelihood. The occupancy, N-mixture and multi-response families
#' write rows of their own. A non-linear formula writes its top-level
#' equation with each non-linear parameter indexed.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
likelihood_rows <- function(obj) {
  responses <- names(response_columns(obj))
  if (length(responses) > 1L) {
    views <- lapply(responses, subset_obj_to_response, obj = obj)
    tags <- escape_math_text(responses)
    mus <- paste0("\\mu^{(", tags, ")}_{i,t}")
    link_rows <- Map(function(view, mu) {
      list(lhs = predictor_lhs(view$family, mu), op = "=",
           rhs = linear_predictor_rhs(view))
    }, views, mus)
    if (!has_rescor(obj)) {
      return(unlist(Map(function(view, mu, r, link_row) {
        list(list(lhs = paste0(r, "_{i,t}"), op = "\\sim",
                  rhs = family_distribution_text(
                    resolve_family_name(view$family), mu, view
                  )),
             link_row)
      }, views, mus, tags, link_rows), recursive = FALSE))
    }
    sigmas <- paste0("\\sigma^{(", tags, ")}", collapse = ", ")
    return(c(list(
      list(lhs = "\\mathbf{Y}_{i,t}", op = "\\sim",
           rhs = paste0("\\text{MVNormal}\\!\\left((",
                        paste(mus, collapse = ", "),
                        ")^{\\top}, \\boldsymbol{\\Sigma}\\right)")),
      list(lhs = "\\boldsymbol{\\Sigma}", op = "=",
           rhs = scaled_cor_text(sigmas, group_matrix_symbol("Omega")))
    ), unname(link_rows)))
  }
  if (!is.null(closure_unit_family_kind(obj))) {
    return(closure_unit_likelihood_rows(obj))
  }
  if (!is.null(mv_custom_family_kind(obj))) {
    return(mv_custom_likelihood_rows(obj))
  }
  mu <- mu_symbol(obj)
  list(
    list(lhs = response_subscripted(obj), op = "\\sim",
         rhs = family_distribution_text(resolve_family_name(obj$family),
                                        mu, obj)),
    list(lhs = predictor_lhs(obj$family, mu), op = "=",
         rhs = if (is_nonlinear_formula(obj$formula)) {
           nl_top_linpred_rhs(obj)
         } else {
           linear_predictor_rhs(obj)
         })
  )
}

#' The trend formula's predictor and its term definitions
#'
#' @param obj An `mvgam` object
#' @return List of statements, empty without trend formula terms
#' @noRd
trend_predictor_rows <- function(obj) {
  if (!methods_md_has_latent_trend(obj)) return(list())
  mu <- trend_mu_symbol(obj)
  if (is.null(mu)) return(list())
  view <- trend_side_view(obj)
  c(
    list(list(lhs = mu, op = "=", rhs = linear_predictor_rhs(view))),
    term_definition_rows(view)
  )
}

#' One view per distributional or non-linear predictor
#'
#' A formula such as `bf(y ~ x, sigma ~ s(z) + (1 | g))` gives `sigma`
#' a predictor of its own. Each view holds that predictor's prior rows
#' with their `dpar`, `nlpar` and `resp` cleared. The mean's extractors
#' and renderers then describe the predictor as they describe the
#' mean. Each symbol carries the parameter as a tag. A multivariate
#' model's tag names the response too. brms writes a non-linear
#' parameter's intercept as the `b` coefficient `Intercept`. The view
#' moves it to the `Intercept` class a distributional intercept has.
#'
#' @param obj An `mvgam` object
#' @return List of views, each with `methods_md_pred` holding the
#'   predictor's `resp`, `dpar` and `nlpar`
#' @noRd
sub_predictor_views <- function(obj) {
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  preds <- Filter(function(lp) !is.null(lp$dpar) || !is.null(lp$nlpar),
                  model_predictors(side_model(obj, "obs")$formula))
  lapply(preds, function(lp) {
    resp <- lp$resp %||% ""
    own <- prior_field(prior, "dpar") == (lp$dpar %||% "") &
      prior_field(prior, "nlpar") == (lp$nlpar %||% "") &
      prior_field(prior, "resp") == resp
    rows <- prior[own, , drop = FALSE]
    for (col in intersect(c("dpar", "nlpar", "resp"), names(rows))) {
      rows[[col]] <- rep("", nrow(rows))
    }
    int <- rows$class == "b" & rows$coef == "Intercept"
    rows$class[int] <- "Intercept"
    rows$coef[int] <- ""
    view <- obj
    view$prior <- rows
    view$trend_metadata <- NULL
    visit <- closure_unit_spec(obj)$visit
    view$methods_md_index <- paste0("_", visit %||% "{i,t}")
    view$methods_md_pred <- lp[c("resp", "dpar", "nlpar")]
    view$methods_md_tag <- paste(
      escape_math_text(c(if (nzchar(resp)) resp, lp$dpar %||% lp$nlpar)),
      collapse = ", "
    )
    view
  })
}

#' One equation per distributional or non-linear predictor
#'
#' A distributional parameter takes the link its family gives it. Each
#' predictor's smooth, Gaussian process, monotonic, measurement-error
#' and group-level terms follow with their definitions.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
sub_predictor_rows <- function(obj) {
  unlist(lapply(sub_predictor_views(obj), function(view) {
    pred <- view$methods_md_pred
    lhs <- if (is.null(pred$dpar)) {
      indexed(view, escape_math_text(pred$nlpar))
    } else {
      link_application(
        dpar_link(model_families(obj, pred$resp), pred$dpar),
        dpar_symbol(pred$dpar, view, pred$resp %||% "")
      )
    }
    c(list(list(lhs = lhs, op = "=", rhs = linear_predictor_rhs(view))),
      term_definition_rows(view))
  }), recursive = FALSE)
}

#' The indexed symbol of a distributional parameter
#'
#' The parameter carries the index of its predictor's covariates. A
#' response of a multivariate model is named in a superscript, as its
#' mean is.
#'
#' @param dp Character parameter name
#' @param view The parameter's view from `sub_predictor_views()`
#' @param resp Response key, or `""`
#' @return Character LaTeX symbol
#' @noRd
dpar_symbol <- function(dp, view, resp = "") {
  superscript <- if (nzchar(resp)) {
    paste0("^{(", escape_math_text(resp), ")}")
  }
  indexed(view, paste0(family_par_symbol(dp), superscript))
}

#' The top-level equation of a non-linear formula
#'
#' The right side of `bf(y ~ a + b * env, a + b ~ ..., nl = TRUE)` as
#' written, with each non-linear parameter and covariate indexed by
#' `(i, t)`, `*` set as a thin space and `exp()`, `log()` and
#' `inv_logit()` set as LaTeX functions. Each non-linear parameter has
#' its own equation in `sub_predictor_rows()`.
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX expression
#' @noRd
nl_top_linpred_rhs <- function(obj) {
  f <- obj$formula
  nlpars <- names(f$pforms %||% list())
  if (length(nlpars) == 0L) {
    return(linear_predictor_rhs(obj))
  }
  rhs_src <- deparse1(f$formula[[3L]], collapse = " ")
  # Stan's functions as LaTeX sets them
  for (fn in c("exp", "log")) {
    rhs_src <- gsub(paste0("\\b", fn, "\\("), paste0("\\\\", fn, "("),
                    rhs_src)
  }
  rhs_src <- gsub("\\binv_logit\\(", "\\\\text{logit}^{-1}(", rhs_src)
  # The indexed, escaped name a token becomes. A backslash in a
  # `gsub()` replacement is an escape. The escaped name holds one,
  # which is doubled here.
  subscripted <- function(name) {
    gsub("\\", "\\\\", paste0(escape_math_text(name), "_{i,t}"),
         fixed = TRUE)
  }
  # Word boundaries keep the parameter `b` out of the covariate `bx`
  for (np in nlpars) {
    rhs_src <- gsub(paste0("\\b", np, "\\b"), subscripted(np), rhs_src)
  }
  # The variables of the model, skipping a token already indexed
  covariates <- setdiff(insight::find_variables(obj, flatten = TRUE),
                        nlpars)
  for (v in covariates) {
    rhs_src <- gsub(paste0("\\b", v, "\\b(?!_\\{)"), subscripted(v),
                    rhs_src, perl = TRUE)
  }
  rhs_src <- gsub("\\s*\\*\\s*", " \\\\, ", rhs_src)
  paste(c(rhs_src, trend_linpred_terms(obj)), collapse = " + ")
}

#' One view per response of a multivariate model, or the model
#'
#' @param obj An `mvgam` object
#' @return List of `subset_obj_to_response()` views, or of `obj`
#' @noRd
response_views <- function(obj) {
  keys <- names(response_columns(obj))
  if (length(keys) < 2L) return(list(obj))
  lapply(keys, subset_obj_to_response, obj = obj)
}

#' The glossary entries of a family's own parameters
#'
#' The occupancy, N-mixture and multi-response families define theirs
#' with their likelihood.
#'
#' @param obj An `mvgam` object or a view of one response
#' @return Character vector of Markdown bullets
#' @noRd
family_glossary <- function(obj) {
  if (!is.null(closure_unit_family_kind(obj)) ||
        !is.null(mv_custom_family_kind(obj))) {
    return(character(0L))
  }
  fam <- resolve_family_name(obj$family)
  what <- c(
    sigma = switch(fam,
      asym_laplace = "scale",
      lognormal = ,
      hurdle_lognormal = "standard deviation of the log response",
      "residual standard deviation"
    ),
    quantile = "quantile level",
    shape = "shape",
    nu = if (identical(fam, "com_binomial")) {
      "dispersion, with 1 giving the binomial"
    } else {
      "degrees of freedom"
    },
    phi = "precision", kappa = "precision",
    hu = "hurdle probability", zi = "zero-inflation probability",
    mphi = "dispersion", mtheta = "power, in (1, 2)",
    mtail = "tail index, with the negative binomial as its limit"
  )
  dpars <- intersect(names(what), obj$family$dpars %||% character(0L))
  # `bf(y ~ x, quantile = 0.25)` fixes a parameter at a value
  fixed <- obj$formula$pfix %||% list()
  c(
    if (fam %in% binomial_family_names()) {
      paste0("- $", indexed(obj, "n"), "$: number of trials")
    },
    vapply(dpars, function(dp) {
      value <- if (!is.null(fixed[[dp]])) paste0(" = ", fixed[[dp]])
      paste0("- $", dpar_aware_param(dp, obj), value, "$: ", what[[dp]],
             if (!is.null(value)) ", fixed")
    }, character(1L), USE.NAMES = FALSE),
    if (is_ordinal_family(obj$family)) {
      "- $\\boldsymbol{\\theta}$: ordered thresholds between categories"
    }
  )
}

#' The families whose mean is a success probability over trials
#'
#' @return Character vector of family names
#' @noRd
binomial_family_names <- function() {
  c("binomial", "zero_inflated_binomial", "com_binomial", "beta_binomial")
}

#' The glossary entry of a response's mean
#'
#' A binomial family's mean is the success probability of each trial.
#' An asymmetric Laplace family's is a quantile. An ordinal family's
#' predictor is a latent score. Its link `F` maps each threshold
#' against the score to a probability (Burkner and Vuorre 2019).
#'
#' @param mu Character LaTeX symbol of the mean
#' @param y Character LaTeX symbol of the response
#' @param family The response's `family`
#' @return Character Markdown bullet
#' @noRd
mean_glossary <- function(mu, y, family) {
  fam <- resolve_family_name(family)
  ordinal <- c(
    cumulative = "P(%y \\le c) = F(\\theta_c - %mu)",
    sratio = "P(%y = c \\mid %y \\ge c) = F(\\theta_c - %mu)",
    cratio = "P(%y > c \\mid %y \\ge c) = F(%mu - \\theta_c)",
    acat = "P(%y = c + 1 \\mid %y \\in \\{c, c + 1\\}) = F(%mu - \\theta_c)"
  )
  if (fam %in% names(ordinal)) {
    law <- gsub("%mu", mu, gsub("%y", y, ordinal[[fam]], fixed = TRUE),
                fixed = TRUE)
    return(paste0(
      "- $", mu, "$: latent score of $", y, "$, with $", law,
      "$ and $F$ the inverse of the ", family$link, " link"
    ))
  }
  what <- if (fam %in% binomial_family_names()) {
    "success probability of each trial of"
  } else if (identical(fam, "asym_laplace")) {
    "conditional $q$-quantile of"
  } else {
    "conditional mean of"
  }
  paste0("- $", mu, "$: ", what, " $", y, "$")
}

#' The glossary entries of the terms of one linear predictor
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character vector of Markdown bullets
#' @noRd
term_glossary <- function(obj) {
  defs <- character(0L)
  classes <- classify_obs_parameters(obj)
  if ("Intercept" %in% classes$fixed) {
    pred <- obj$methods_md_pred
    of <- if (!is.null(pred$dpar)) {
      paste0(" of the predictor of $", family_par_symbol(pred$dpar), "$")
    } else if (!is.null(pred$nlpar)) {
      paste0(" of the predictor of $", escape_math_text(pred$nlpar), "$")
    } else if (identical(obj$methods_md_tag, "\\eta")) {
      " of the trend predictor"
    }
    defs <- c(defs, paste0(
      "- $", response_symbol(obj, "\\alpha"), "$: population intercept", of
    ))
  }
  for (spec in fixed_term_specs(obj, setdiff(classes$fixed, "Intercept"))) {
    defs <- c(defs, paste0(
      "- $", response_symbol(obj, paste0("\\beta_{", spec$sub, "}")),
      "$: ", spec$gloss
    ))
  }
  for (spec in classes$smooth) {
    sym <- term_symbols("smooth", spec, obj)
    key <- spec_key(spec)
    if (is_re_basis(spec)) {
      defs <- c(defs, paste0(
        "- $", sym[["f"]], "$: random effect smooth, one coefficient ",
        "per level of $", spec_subscript(spec), "$ with SD $", sym[["sd"]],
        "$"
      ))
      next
    }
    k_label <- if (!is.na(spec$k)) {
      # A tensor product takes one basis size per margin, `k = c(4, 4)`
      k <- str2lang(spec$k)
      if (is.call(k)) k <- vapply(as.list(k)[-1L], deparse1, character(1L))
      paste0("$K_{", key, "} = ", paste(k, collapse = " \\times "), "$")
    } else {
      paste0("$K_{", key, "}$ (mgcv default)")
    }
    defs <- c(defs, paste0(
      "- $", sym[["f"]], "$: ", basis_label(spec$bs, spec$fname),
      " in $", paste(escape_math_text(spec$vars), collapse = "$, $"),
      "$", by_clause(spec), ", basis size ",
      k_label, ", smoothing SD $", sym[["sd"]], "$"
    ))
  }
  for (spec in classes$gp) {
    sym <- term_symbols("gp", spec, obj)
    k_text <- if (!is.null(spec$k) && !is.na(spec$k)) {
      paste0(", approximated with ", spec$k, " basis functions")
    } else {
      ", exact (full covariance kernel)"
    }
    defs <- c(defs, paste0(
      "- $", sym[["f"]], "$: Gaussian process in $",
      spec_subscript(spec), "$", by_clause(spec), " with ",
      gp_kernel_names(spec$cov)[["label"]], " kernel, length scale $",
      sym[["rho"]], "$ and marginal SD $", sym[["sd"]], "$", k_text
    ))
  }
  for (s in classes$mo) {
    v <- escape_math_text(s$var)
    sym <- term_symbols("mo", s, obj)
    defs <- c(defs, paste0(
      "- $", sym[["m"]], "(", v, ")$: monotonic step transform of ",
      "ordinal $", v, "$, built from a Dirichlet simplex $", sym[["zeta"]],
      "$ over the $D_{", v, "} - 1$ step increments and scaled by ",
      "population effect $", sym[["beta"]], "$"
    ))
  }
  for (s in classes$me) {
    v <- escape_math_text(s$var)
    sym <- term_symbols("me", s, obj)
    defs <- c(defs, paste0(
      "- $", indexed(obj, paste0("\\tilde{", v, "}")), "$: latent true ",
      "covariate underlying noisy observation $", indexed(obj, v),
      "$, with known per-observation measurement-error SD $",
      indexed(obj, sym[["sdx"]]), "$, population hyper-mean $",
      sym[["mu"]], "$ and hyper-SD $", sym[["sd"]], "$, entering with ",
      "effect $", sym[["beta"]], "$"
    ))
  }
  for (s in classes$re) {
    grp <- escape_math_text(s$group)
    sym <- term_symbols("re", s, obj)
    if (!s$has_slope) {
      defs <- c(defs, paste0(
        "- $", sym[["alpha_i"]], "$: varying intercept across levels of $",
        grp, "$ with hyper-SD $", sym[["sd"]], "$"
      ))
      next
    }
    defs <- c(defs, paste0(
      "- $", paste(c(sym[["alpha_i"]], sym[paste0("slope_i:", s$slopes)]),
                   collapse = "$, $"),
      "$: correlated varying intercept and slopes across levels of $",
      grp, "$, jointly distributed as MVNormal with covariance $",
      sym[["Sigma"]], "$ built from per-coefficient SDs and correlation ",
      "matrix $", sym[["Omega"]], "$"
    ))
  }
  offsets <- obs_offset_symbols(obj)
  if (length(offsets)) {
    defs <- c(defs, paste0(
      "- $", offsets, "$: offset `", names(offsets),
      "`, with its coefficient fixed at one"
    ))
  }
  defs
}

#' The glossary of the Model section
#'
#' One Markdown bullet per symbol of the equation. A blank line follows
#' the "where:" lead-in. Markdown then starts a list.
#'
#' @param obj An `mvgam` object
#' @return Markdown string
#' @noRd
model_glossary <- function(obj) {
  keys <- names(response_columns(obj))
  means <- if (length(keys) > 1L) {
    tags <- escape_math_text(keys)
    unlist(Map(mean_glossary, paste0("\\mu^{(", tags, ")}_{i,t}"),
               paste0(tags, "_{i,t}"), model_families(obj)),
           use.names = FALSE)
  } else if (is.null(closure_unit_family_kind(obj)) &&
               is.null(mv_custom_family_kind(obj))) {
    mean_glossary(mu_symbol(obj), response_subscripted(obj), obj$family)
  }
  defs <- c(
    if (is_multi_season_family(obj$family)) {
      "- $i$ indexes series, $s$ indexes sites, $t$ indexes seasons"
    } else if (!is.null(mv_custom_family_kind(obj))) {
      "- $i$ indexes series, $t$ indexes units"
    } else {
      "- $i$ indexes series, $t$ indexes time"
    },
    closure_unit_glossary(obj),
    mv_custom_glossary(obj),
    means,
    unlist(lapply(response_views(obj), function(v) {
      c(term_glossary(v), family_glossary(v))
    })),
    unlist(lapply(sub_predictor_views(obj), term_glossary)),
    trend_glossary(obj)
  )
  paste(c("where:", "", defs), collapse = "\n")
}

#' The glossary entries of the latent trend
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets, empty without a trend
#' @noRd
trend_glossary <- function(obj) {
  if (!methods_md_has_latent_trend(obj)) return(character(0L))
  is_factor <- methods_md_has_factor_model(obj)
  mu <- trend_mu_symbol(obj)
  defs <- c(
    "- $\\eta_{i,t}$: latent state at series $i$ time $t$",
    if (!is.null(mu)) {
      c(paste0("- $", mu, "$: ",
               if (trend_mu_on_factors(obj)) "factor " else "trend ",
               "predictor from 'trend_formula'"),
        term_glossary(trend_side_view(obj)))
    }
  )
  if (is_factor) {
    defs <- c(
      defs,
      paste0("- $", trend_eta(obj), "$: latent ", trend_order_label(obj),
             " factor $k = 1, \\ldots, ", obj$trend_metadata$n_lv,
             "$ at time $t$"),
      "- $Z_{i,k}$: loading of series $i$ on factor $k$",
      loadings_glossary(first_trend_spec(obj)$loadings_prior_spec)
    )
    a <- obj$standata[c("mgp_a1", "mgp_a2")]
    if (!is.null(a$mgp_a1)) {
      defs <- c(defs, paste0(
        "- $\\varrho_h$, $\\Psi_k$: shrinkage increment of factor ",
        "$h$ and the cumulative shrinkage of factor $k$, with ",
        "inverse-gamma shapes $a_1 = ", a$mgp_a1, "$ and $a_2 = ",
        a$mgp_a2, "$"
      ))
    }
  }
  tt <- obj$trend_metadata$trend_type
  defs <- c(
    defs,
    innovation_glossary(obj),
    trend_method(tt, trend_method_templates[["glossary"]])(obj)
  )
  gr <- trend_grouping_var(obj)
  if (is.null(gr)) return(defs)
  sym <- trend_symbols(obj)
  c(
    defs,
    paste0(
      "- $", group_matrix_symbol("Omega", gr), "$: innovation correlation ",
      "within each level of $", gr, "$, pooling a shared matrix $",
      sym[["L_Omega_global_trend"]], "$ and a level's own matrix $",
      sym[["L_deviation_group_trend"]], "$ with weight $",
      sym[["alpha_cor_trend"]], "$"
    ),
    paste0("- $", sym[["sigma_group_trend"]], "$: innovation SDs within ",
           "each level of $", gr, "$")
  )
}

#' The glossary entries for a trend's innovations
#'
#' Written in the form `innovation_rows()` gives the innovations.
#'
#' @param obj Fitted `mvgam` object
#' @return Character vector of Markdown glossary lines
#' @noRd
innovation_glossary <- function(obj) {
  checkmate::assert_class(obj, "mvgam")
  is_factor <- methods_md_has_factor_model(obj)
  tt <- obj$trend_metadata$trend_type
  if (identical(tt, "PW")) return(character(0L))
  sd <- innovation_sd(obj)
  sd_vec <- innovation_sd(obj, vector = TRUE)
  what <- if (is_factor) "factor innovation" else "process innovation"
  form <- innovation_form(obj, identical(tt, "VAR"))
  gr <- trend_grouping_var(obj)
  scale <- function(sd, label) {
    if (identical(sd, "1")) "unit SD" else paste0(label, " $", sd, "$")
  }
  # The priors name each entry of the SD vector
  sd_entries <- if (!identical(sd_vec, sd)) {
    paste0(", with $", sd_vec, "$ the vector of SDs $",
           trend_symbols(obj)[["sigma_trend"]], "$")
  }
  cov <- innovation_cov_symbol(obj)
  out <- switch(form,
    scalar = paste0("- $", trend_eps(obj), "$: ", what, ", ",
                    scale(sd, "SD")),
    diag = paste0("- $", trend_eps(obj, vector = TRUE), "$: ", what,
                  "s at time $t$, independent with ", scale(sd_vec, "SDs")),
    cor = paste0("- $", trend_eps(obj, vector = TRUE), "$: ", what,
                 "s at time $t$, covariance $", cov, "$", sd_entries)
  )
  # ZMVN draws the states themselves and has no innovations.
  of <- "innovations"
  if (identical(tt, "ZMVN")) {
    out <- if (identical(form, "cor") && !is.null(sd_entries)) {
      paste0("- $", cov, "$: covariance of the latent states", sd_entries)
    }
    of <- "latent states"
  }
  if (identical(tt, "CAR")) {
    of <- "shocks over an instant"
  }
  if (identical(form, "cor") && is.null(gr)) {
    out <- c(out, paste0(
      "- $\\boldsymbol{\\Omega}$: correlation matrix of the ", of
    ))
  }
  out
}

#' Whether a trend has a moving-average part
#'
#' `AR(ma = TRUE)` and `VAR(ma = TRUE)` are recorded under their base
#' trend type. The stored MA lags mark the moving-average part.
#'
#' @param obj Fitted `mvgam` object
#' @return Logical scalar
#' @noRd
trend_has_ma <- function(obj) {
  checkmate::assert_class(obj, "mvgam")
  length(obj$trend_metadata$ma_lags %||% integer(0L)) > 0L
}


#' The glossary entry of a univariate trend's moving-average part
#'
#' Every trend defines `glossary_<type>(obj)` for its own parameters,
#' which `trend_glossary()` calls through `trend_method()`.
#'
#' @param obj An `mvgam` object
#' @return Character Markdown bullet, or an empty vector
#' @noRd
ma_glossary <- function(obj) {
  if (!trend_has_ma(obj)) return(character(0L))
  paste0(
    "- $", lag_symbol("\\theta", "l", trend_process_index(obj)),
    "$: moving-average coefficient at lag $l$"
  )
}

#' The glossary entries of a random walk: its moving-average part
#' @noRd
glossary_rw <- ma_glossary

#' The glossary entries of a ZMVN trend, which has no dynamics
#'
#' @param obj An `mvgam` object
#' @return An empty character vector
#' @noRd
glossary_zmvn <- function(obj) {
  character(0L)
}

#' The glossary entries for a VAR trend's coefficient matrices
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets
#' @noRd
glossary_var <- function(obj) {
  out <- paste0(
    "- $\\boldsymbol{\\Phi}_l$: autoregressive coefficient matrix at ",
    "lag $l$, mapped into the stationary region from an unconstrained ",
    "matrix $\\mathbf{A}_l$"
  )
  if ("Amu_trend" %in% obj$prior$class) {
    out <- c(out, paste0(
      "- $\\mu^{(A)}$, $\\omega^{(A)}$: mean and precision of the ",
      "normal prior on the entries of $\\mathbf{A}_l$, one pair for ",
      "the diagonal and one for the off-diagonal"
    ))
  }
  if (trend_has_ma(obj)) {
    out <- c(out, paste0(
      "- $\\boldsymbol{\\Theta}_l$: moving-average coefficient matrix ",
      "at lag $l$, mapped into the invertible region from an ",
      "unconstrained matrix $\\mathbf{D}_l$ whose entries take normal ",
      "priors with means $\\mu^{(D)}$ and precisions $\\omega^{(D)}$"
    ))
  }
  out
}

#' The glossary entries for a continuous-time AR trend
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets
#' @noRd
glossary_car <- function(obj) {
  out <- paste0(
    "- $", trend_symbols(obj)[["ar1_trend"]],
    "$: autocorrelation over a gap of one, and $", car_gap_symbol,
    "$ the time between occasions $t - 1$ and $t$ in units of the ",
    "median gap between a series' observations"
  )
  if (trend_samples_cor(obj)) {
    out <- c(out, paste0(
      "- $", group_matrix_symbol("Sigma"), "$: innovation covariance over ",
      "a gap of one, and $\\lambda_i = -\\log \\rho_i$"
    ))
  }
  out
}

#' The glossary entries for an AR trend's coefficients
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets
#' @noRd
glossary_ar <- function(obj) {
  classes <- obj$prior$class
  idx <- ar_coef_index(obj)
  phi <- lag_symbol("\\phi", "l", idx)
  of <- if (ar_coefs_shared(obj)) " shared by every series" else ""
  out <- paste0("- $", phi, "$: autoregressive coefficient at lag $l$", of)
  pacf <- any(grepl("_pacf_trend$", classes))
  if (pacf) {
    out <- c(out, paste0(
      "- $", lag_symbol("\\psi", "l", idx), "$: partial autocorrelation ",
      "at lag $l$, which fixes the coefficients $", phi, "$ inside the ",
      "stationary region"
    ))
  }
  if (any(startsWith(classes, "mu_ar"))) {
    base <- if (pacf) "\\psi" else "\\phi"
    out <- c(out, paste0(
      "- $\\mu^{(", base, ")}_{l}$, $\\sigma^{(", base, ")}_{l}$: mean ",
      "and SD of the lag-$l$ coefficients across series"
    ))
  }
  c(out, ma_glossary(obj))
}


#' The conventional label of a trend
#'
#' Labels take forms such as `AR(2)` or `ARMA(2, 1)`. A contiguous lag
#' set is named by its order. A sparse one lists its lags in braces, as
#' in `AR({1, 12})`. No single order describes a sparse set. A bare list
#' of lags would be mistaken for an ARIMA order.
#'
#' @param obj Fitted `mvgam` object
#' @return Character scalar label, or `""` for a model without a trend
#' @noRd
trend_order_label <- function(obj) {
  checkmate::assert_class(obj, "mvgam")
  trend_metadata_label(obj$trend_metadata)
}

#' The conventional label of a trend, from its metadata
#'
#' A fit and an unfitted constructor share this label.
#' `print.mvgam_trend()` builds the metadata from the constructor.
#'
#' @param meta List with `trend_type`, `ar_lags`, `ma_lags` and, for
#'   PW, the growth form, or `NULL`
#' @return Character scalar label, or `""` without a trend type
#' @noRd
trend_metadata_label <- function(meta) {
  checkmate::assert_list(meta, null.ok = TRUE)
  tt <- meta$trend_type
  if (is.null(tt)) return("")
  ar_lags <- meta$ar_lags %||% integer(0L)
  ma_lags <- meta$ma_lags %||% integer(0L)
  order_of <- function(lags) {
    if (ar_lags_contiguous(lags)) {
      return(as.character(max(lags)))
    }
    paste0("{", paste(lags, collapse = ", "), "}")
  }
  arma_label <- function(prefix) {
    if (!length(ma_lags)) {
      return(paste0(prefix, "(", order_of(ar_lags), ")"))
    }
    paste0(prefix, "MA(", order_of(ar_lags), ", ", order_of(ma_lags), ")")
  }
  switch(
    tt,
    "RW"   = "RW",
    "AR"   = arma_label("AR"),
    "VAR"  = arma_label("VAR"),
    "CAR"  = "CAR",
    "ZMVN" = "ZMVN",
    "PW"   = paste0("PW(", pw_growth(meta), ")"),
    tt
  )
}

#' The ranges of the series, site and time indices
#'
#' The counts are those of the Data section. A single series takes no
#' range for `i`. A multi-season occupancy or N-mixture model indexes
#' its sites by `s`.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
index_range_rows <- function(obj) {
  counts <- printed_axis_counts(obj)
  index_row <- function(lhs, n) {
    list(lhs = lhs, op = "\\in", rhs = paste0("\\{1, \\ldots, ", n, "\\}"))
  }
  n_sites <- if (is_multi_season_family(obj$family) && !is.null(obj$data)) {
    length(unique(obj$data$site))
  }
  c(
    if (isTRUE(counts$n_series > 1L)) list(index_row("i", counts$n_series)),
    if (!is.null(n_sites)) list(index_row("s", n_sites)),
    if (!is.null(counts$n_timepoints)) {
      list(index_row("t", counts$n_timepoints))
    }
  ) %||% list()
}

#' The response's symbol
#'
#' The column name, which keeps the underscore of a column such as
#' `y_occ`. brms's key for it is `yocc`. A multivariate model's
#' responses are the vector `Y`.
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX symbol
#' @noRd
response_letter <- function(obj) {
  nm <- unname(response_columns(obj))
  if (length(nm) > 1L) return("\\mathbf{Y}")
  escape_math_text(nm[[1L]])
}

#' A variable name as it is set in LaTeX math
#'
#' A bare `_` is a subscript marker. Math italic sets `mintemp` as a
#' product of seven symbols. A name holding a `_` or more than one
#' character is set upright by `math_upright()`. A single letter and a
#' number are set as they are.
#'
#' @param s Character vector, or another value returned unchanged
#' @return `s`, each name escaped
#' @noRd
escape_math_text <- function(s) {
  if (is.null(s) || !is.character(s)) return(s)
  out <- vapply(s, function(x) {
    if (!grepl("_", x, fixed = TRUE) &&
        !grepl("^[A-Za-z.][A-Za-z0-9.]+$", x)) {
      return(x)
    }
    math_upright(x)
  }, character(1L))
  if (length(out) == 1L) unname(out) else out
}

#' An identifier set upright in math, its underscores escaped
#'
#' LaTeX and MathJax both render `\mathrm{}` with an escaped `\_`.
#' MathJax prints `\_` inside `\text{}` with its backslash.
#'
#' @param x Character vector
#' @return Character vector of LaTeX
#' @noRd
math_upright <- function(x) {
  paste0("\\mathrm{", gsub("_", "\\\\_", x), "}")
}

#' The response's symbol, indexed by observation
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX symbol
#' @noRd
response_subscripted <- function(obj) {
  indexed(obj, response_letter(obj))
}

#' The mean's symbol, a vector for a multivariate model
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX symbol
#' @noRd
mu_symbol <- function(obj) {
  indexed(obj, if (length(response_columns(obj)) > 1L) {
    "\\boldsymbol{\\mu}"
  } else {
    "\\mu"
  })
}

#' The left side of a mean's predictor equation
#'
#' An ordinal family's link maps the thresholds less its predictor to
#' cumulative or conditional probabilities. The predictor itself is the
#' latent score `\mu`, which `mean_glossary()` relates to the
#' probabilities. Any other family's link maps its mean to the
#' predictor.
#'
#' @param family The response's `family`
#' @param mu Character LaTeX symbol of the mean
#' @return Character LaTeX expression
#' @noRd
predictor_lhs <- function(family, mu) {
  if (is_ordinal_family(family)) return(mu)
  link_application(family$link, mu)
}

#' A link function applied to a symbol, in LaTeX
#'
#' @param link Character link name
#' @param mu Character LaTeX symbol
#' @return Character LaTeX expression, `g(mu)` for an unnamed link
#' @noRd
link_application <- function(link, mu) {
  switch(
    link,
    identity = mu,
    log = paste0("\\log ", mu),
    logit = paste0("\\text{logit}(", mu, ")"),
    probit = paste0("\\Phi^{-1}(", mu, ")"),
    cloglog = paste0("\\log(-\\log(1 - ", mu, "))"),
    inverse = paste0("1 / ", mu),
    sqrt = paste0("\\sqrt{", mu, "}"),
    paste0("g(", mu, ")")
  )
}

#' The symbols of the families' own parameters
#'
#' One map for the likelihood, the distributional sub-formulas and the
#' priors. brms's `shape` is the Gamma shape and the negative binomial
#' size alike. The Tweedie dispersion `mphi` takes `\omega`, clear of
#' the autoregressive `\phi`.
#'
#' @return Named character vector of LaTeX symbols
#' @noRd
family_par_symbols <- function() {
  c(
    sigma = "\\sigma", phi = "\\phi", shape = "\\varphi",
    kappa = "\\kappa", nu = "\\nu", mu = "\\mu",
    hu = "\\pi^{(\\text{hu})}", zi = "\\pi^{(\\text{zi})}",
    p = "p", mphi = "\\omega", mtheta = "\\xi", mtail = "\\tau",
    quantile = "q"
  )
}

#' The symbol of a family's own parameter
#'
#' @param dp Character parameter name
#' @return Character LaTeX symbol, upright for a name with none
#' @noRd
family_par_symbol <- function(dp) {
  sym <- family_par_symbols()
  if (dp %in% names(sym)) sym[[dp]] else math_upright(dp)
}

#' The symbol of a family parameter, indexed when it has a formula
#'
#' A parameter such as `sigma` is one scalar, `\sigma`. A formula of
#' its own makes it vary by observation, `\sigma_{i,t}`.
#'
#' @param name Character parameter name
#' @param obj An `mvgam` object or a view of one response
#' @return Character LaTeX symbol
#' @noRd
dpar_aware_param <- function(name, obj) {
  prior <- obj$prior
  has_dpar <- !is.null(prior) && nrow(prior) > 0L &&
    any(prior_field(prior, "dpar") == name)
  base <- family_par_symbol(name)
  response_symbol(obj, if (has_dpar) paste0(base, "_{i,t}") else base)
}

#' The likelihood of one response, in LaTeX
#'
#' Each family's parameters take the symbols of `family_par_symbols()`,
#' indexed when they have a formula. A binomial family's trials are
#' `n`. A family with no notation of its own is named as text.
#'
#' @param fam_name Character family name
#' @param mu Character LaTeX symbol of the mean
#' @param obj An `mvgam` object or a view of one response
#' @return Character LaTeX expression
#' @noRd
family_distribution_text <- function(fam_name, mu, obj) {
  par <- function(name) dpar_aware_param(name, obj)
  sigma <- par("sigma")
  phi <- par("phi")
  nu <- par("nu")
  shape <- par("shape")
  hu <- par("hu")
  zi <- par("zi")
  trials <- indexed(obj, "n")
  switch(
    fam_name,
    poisson     = paste0("\\text{Poisson}(", mu, ")"),
    bernoulli   = paste0("\\text{Bernoulli}(", mu, ")"),
    binomial    = paste0("\\text{Binomial}(", trials, ", ", mu, ")"),
    com_binomial = paste0("\\text{COMBinomial}(", trials, ", ", mu, ", ",
                          nu, ")"),
    beta_binomial = paste0("\\text{BetaBinomial}(", trials, ", ", mu, ", ",
                           phi, ")"),
    gaussian    = paste0("\\text{Normal}(", mu, ", ", sigma, ")"),
    student     = paste0("\\text{StudentT}(", nu, ", ", mu,
                          ", ", sigma, ")"),
    asym_laplace = paste0("\\text{AsymLaplace}(", mu, ", ", sigma, ", ",
                          par("quantile"), ")"),
    lognormal   = paste0("\\text{LogNormal}(", mu, ", ", sigma, ")"),
    gamma       = paste0("\\text{Gamma}(", shape, ", ", mu, ")"),
    beta        = paste0("\\text{Beta}(", mu, ", ", phi, ")"),
    negbinomial = paste0("\\text{NegBin}(", mu, ", ", shape, ")"),
    beta_nb     = paste0(
      "\\text{BetaNegBinomial}(", mu, ", ", shape, ", ", par("mtail"), ")"
    ),
    tweedie     = paste0(
      "\\text{Tweedie}(", mu, ", ", par("mphi"), ", ", par("mtheta"), ")"
    ),
    hurdle_poisson = paste0(
      "\\text{Hurdle-Poisson}(", mu, ", ", hu, ")"
    ),
    hurdle_negbinomial = paste0(
      "\\text{Hurdle-NegBin}(", mu, ", ", shape, ", ", hu, ")"
    ),
    hurdle_gamma = paste0(
      "\\text{Hurdle-Gamma}(", mu, ", ", shape, ", ", hu, ")"
    ),
    hurdle_lognormal = paste0(
      "\\text{Hurdle-LogNormal}(", mu, ", ", sigma, ", ", hu, ")"
    ),
    zero_inflated_poisson = paste0(
      "\\text{ZIPoisson}(", mu, ", ", zi, ")"
    ),
    zero_inflated_negbinomial = paste0(
      "\\text{ZINegBin}(", mu, ", ", shape, ", ", zi, ")"
    ),
    zero_inflated_beta = paste0(
      "\\text{ZIBeta}(", mu, ", ", phi, ", ", zi, ")"
    ),
    zero_inflated_binomial = paste0(
      "\\text{ZIBinomial}(", trials, ", ", mu, ", ", zi, ")"
    ),
    cumulative  = paste0(
      "\\text{OrderedCumulative}(\\boldsymbol{\\theta}, ", mu, ")"
    ),
    sratio      = paste0(
      "\\text{OrderedStoppingRatio}(\\boldsymbol{\\theta}, ", mu, ")"
    ),
    cratio      = paste0(
      "\\text{OrderedContinuationRatio}(\\boldsymbol{\\theta}, ", mu, ")"
    ),
    acat        = paste0(
      "\\text{OrderedAdjacentCategory}(\\boldsymbol{\\theta}, ", mu, ")"
    ),
    paste0("\\text{", gsub("_", "\\\\_", fam_name), "}(", mu, ")")
  )
}


#' The occupancy or N-mixture variant of a family
#'
#' @param obj An `mvgam` object
#' @return `"occ"`, `"nmix_pb"` (Poisson-binomial), `"nmix_rn"`
#'   (Royle-Nichols), `"nmix_ppm"` (Poisson-Poisson), or `NULL` for any
#'   other family
#' @noRd
closure_unit_family_kind <- function(obj) {
  if (!is_closure_unit_family(obj$family)) return(NULL)
  switch(
    resolve_family_name(obj$family),
    occ                    = "occ",
    nmix                   = "nmix_pb",
    nmix_poisson_binomial  = "nmix_pb",
    nmix_royle_nichols     = "nmix_rn",
    nmix_poisson_poisson   = "nmix_ppm",
    NULL
  )
}

#' The symbols and laws of an occupancy or N-mixture model
#'
#' The parameterisations of the Stan likelihoods in `R/families.R`.
#' Occupancy draws a latent state `z ~ Bernoulli(psi)`, observed as
#' `y | z ~ Bernoulli(z p)`. The N-mixture variants draw an abundance
#' `N ~ Poisson(lambda)`. They observe it as `Binomial(N, p)`, as
#' `Bernoulli(1 - (1 - p)^N)` (Royle-Nichols) or as `Poisson(N p)`
#' (Poisson-Poisson). Every variant names its detection parameter `p`,
#' as brms names the distributional parameter. A closure unit is a
#' series `i` at time `t`. A multi-season unit is a series `i` at site
#' `s` in season `t`. Visit `j` indexes the observations of a unit.
#'
#' @param obj An `mvgam` object
#' @return List of LaTeX pieces, or `NULL` for any other family
#' @noRd
closure_unit_spec <- function(obj) {
  kind <- closure_unit_family_kind(obj)
  if (is.null(kind)) return(NULL)
  occ <- identical(kind, "occ")
  unit <- if (is_multi_season_family(obj$family)) "{i,s,t}" else "{i,t}"
  visit <- sub("\\}$", ",j}", unit)
  latent <- paste0(if (occ) "z" else "N", "_", unit)
  state <- paste0(if (occ) "\\psi" else "\\lambda", "_", unit)
  p <- paste0("p_", visit)
  list(
    kind = kind,
    unit = unit,
    visit = visit,
    latent = latent,
    state = state,
    link = if (occ) "logit" else "log",
    latent_rhs = paste0(
      if (occ) "\\text{Bernoulli}(" else "\\text{Poisson}(", state, ")"
    ),
    obs_rhs = switch(
      kind,
      occ = paste0("\\text{Bernoulli}(", latent, " \\cdot ", p, ")"),
      nmix_pb = paste0("\\text{Binomial}(", latent, ", ", p, ")"),
      nmix_rn = paste0("\\text{Bernoulli}(1 - (1 - ", p, ")^{", latent,
                       "})"),
      nmix_ppm = paste0("\\text{Poisson}(", latent, " \\cdot ", p, ")")
    ),
    detection = switch(
      kind,
      nmix_rn = "\\in (0, 1)$: per-individual detection probability",
      nmix_ppm = "> 0$: per-visit encounter rate",
      "\\in (0, 1)$: per-visit detection probability"
    )
  )
}

#' The likelihood rows of an occupancy or N-mixture model
#'
#' The observation given the latent state, the latent state, and the
#' link of the state parameter. The detection predictor comes from
#' `sub_predictor_rows()`. A trend enters the state predictor
#' through `linear_predictor_rhs()`.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
closure_unit_likelihood_rows <- function(obj) {
  spec <- closure_unit_spec(obj)
  if (is.null(spec)) return(list())
  list(
    list(lhs = paste0("y_", spec$visit, " \\mid ", spec$latent), op = "\\sim",
         rhs = spec$obs_rhs),
    list(lhs = spec$latent, op = "\\sim", rhs = spec$latent_rhs),
    list(lhs = link_application(spec$link, spec$state), op = "=",
         rhs = linear_predictor_rhs(obj))
  )
}


#' The multi-response family of a model
#'
#' @param obj An `mvgam` object
#' @return `"mvn"`, `"mvt"`, `"diri"`, `"multi"`, `"categ"`, or `NULL`
#'   for any other family
#' @noRd
mv_custom_family_kind <- function(obj) {
  if (!is_multi_response_family(obj$family)) return(NULL)
  kind <- resolve_family_name(obj$family)
  if (kind %in% c("mvn", "mvt", "diri", "multi", "categ")) kind
}

#' The likelihood rows of a multi-response family
#'
#' Each unit `t` holds one vector observation with one entry per series
#' `i`. The `mvn()` and `mvt()` kernels in `R/families.R` evaluate
#' independent entries with one scale each,
#' `normal_lpdf(y_unit | mu_unit, psi_unit)`. The simplex kernels
#' subtract the first series' predictor before the softmax, which
#' makes the first series the reference.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
mv_custom_likelihood_rows <- function(obj) {
  kind <- mv_custom_family_kind(obj)
  if (is.null(kind)) return(list())
  obs_rhs <- switch(
    kind,
    mvn = "\\text{MVNormal}(\\boldsymbol{\\mu}_t, \\boldsymbol{\\Sigma})",
    mvt = paste0("\\text{MVStudentT}(\\nu, \\boldsymbol{\\mu}_t, ",
                 "\\boldsymbol{\\Sigma})"),
    diri = "\\text{Dirichlet}(\\phi \\, \\boldsymbol{\\pi}_t)",
    multi = "\\text{Multinomial}(N_t, \\boldsymbol{\\pi}_t)",
    categ = "\\text{Categorical}(\\boldsymbol{\\pi}_t)"
  )
  c(
    list(list(lhs = "\\mathbf{Y}_t", op = "\\sim", rhs = obs_rhs)),
    if (kind %in% c("mvn", "mvt")) {
      list(list(lhs = "\\boldsymbol{\\Sigma}", op = "=",
                rhs = "\\text{diag}(\\boldsymbol{\\Psi}^2)"))
    } else {
      list(list(lhs = "\\boldsymbol{\\pi}_t", op = "=",
                rhs = "\\text{softmax}(\\boldsymbol{\\mu}_t - \\mu_{1,t})"))
    },
    list(list(lhs = "\\mu_{i,t}", op = "=", rhs = linear_predictor_rhs(obj)))
  )
}

#' The glossary entries of a multi-response family
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets, empty for any other
#'   family
#' @noRd
mv_custom_glossary <- function(obj) {
  kind <- mv_custom_family_kind(obj)
  if (is.null(kind)) return(character(0L))
  normal <- kind %in% c("mvn", "mvt")
  c(
    paste0("- $\\mathbf{Y}_t$: response vector of unit $t$, one entry per ",
           "series"),
    if (normal) {
      c("- $\\mu_{i,t}$: mean of series $i$ at unit $t$",
        paste0("- $\\boldsymbol{\\Sigma}$: residual covariance, one SD in ",
               "$\\boldsymbol{\\Psi}$ per series"))
    } else {
      c(paste0("- $\\mu_{i,t}$: linear predictor of series $i$ at unit $t$, ",
               "with series 1 the reference"),
        "- $\\boldsymbol{\\pi}_t$: probabilities of the series at unit $t$")
    },
    switch(kind,
      mvt = "- $\\nu$: degrees of freedom",
      diri = "- $\\phi$: Dirichlet precision",
      multi = "- $N_t$: number of trials at unit $t$"
    )
  )
}

#' The glossary entries of an occupancy or N-mixture model
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets, empty for any other
#'   family
#' @noRd
closure_unit_glossary <- function(obj) {
  spec <- closure_unit_spec(obj)
  if (is.null(spec)) return(character(0L))
  unit <- spec$unit
  state <- if (identical(spec$kind, "occ")) {
    c(paste0("- $z_", unit, " \\in \\{0, 1\\}$: latent occupancy state"),
      paste0("- $\\psi_", unit, " \\in (0, 1)$: occupancy probability, ",
             "logit link"))
  } else {
    c(paste0("- $N_", unit, " \\in \\mathbb{Z}_{\\ge 0}$: latent ",
             "abundance"),
      paste0("- $\\lambda_", unit, " > 0$: expected abundance, log link"))
  }
  tuple <- paste0("(", gsub(",", ", ", gsub("[{}]", "", unit)), ")")
  c(
    paste0("- $j$ indexes the visits to closure unit $", tuple, "$"),
    state,
    paste0("- $p_", spec$visit, " ", spec$detection)
  )
}

#' The right side of a linear predictor
#'
#' The terms in a fixed order: population effects, smooths, Gaussian
#' processes, monotonic and measurement-error effects, group-level
#' effects, offsets and the trend.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX expression, `"0"` without terms
#' @noRd
linear_predictor_rhs <- function(obj) {
  classes <- classify_obs_parameters(obj)
  parts <- c(
    render_fixed_inline(classes$fixed, obj),
    render_function_inline("smooth", classes$smooth, obj),
    render_function_inline("gp", classes$gp, obj),
    render_mo_inline(classes$mo, obj),
    render_me_inline(classes$me, obj),
    render_re_inline(classes$re, obj),
    unname(obs_offset_symbols(obj)),
    trend_linpred_terms(obj)
  )
  if (length(parts) == 0L) "0" else paste(parts, collapse = " + ")
}

#' The terms of a linear predictor, by kind
#'
#' The prior table holds one row per parameter class, for a fit and a
#' prefit alike.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return List with elements `fixed`, `smooth`, `gp`, `mo`, `me` and
#'   `re`
#' @noRd
classify_obs_parameters <- function(obj) {
  prior <- obj$prior
  list(
    fixed  = obs_fixed_terms_from_prior(obj),
    smooth = obs_smooth_specs_from_prior(prior),
    gp     = obs_gp_specs(obj),
    mo     = obs_mo_specs_from_prior(obj),
    me     = obs_me_specs(obj),
    re     = obs_re_specs_from_prior(prior)
  )
}

#' Prior rows of the mean's own linear predictor
#'
#' Rows scoped to a distributional or non-linear parameter belong to
#' that parameter's predictor.
#'
#' @param prior A `brmsprior` table
#' @return Logical vector, one entry per row
#' @noRd
mean_predictor_rows <- function(prior) {
  !nzchar(prior_field(prior, "dpar")) & !nzchar(prior_field(prior, "nlpar"))
}

#' The intercept and the population effects of a linear predictor
#'
#' The `Intercept` rows of an ordinal mean are its thresholds. That
#' mean has no intercept of its own.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character vector of `"Intercept"` and covariate names
#' @noRd
obs_fixed_terms_from_prior <- function(obj) {
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(character(0L))
  own <- mean_predictor_rows(prior)
  ordinal_mean <- is.null(obj$methods_md_pred) &&
    is_ordinal_family(obj$family)
  has_int <- !ordinal_mean && any(prior$class == "Intercept" & own)
  b_rows <- prior$class == "b" & own & nzchar(prior$coef)
  coefs <- unique(prior$coef[b_rows])
  # A smooth's unpenalised columns and the special terms each render
  # through their own block.
  terms <- coefs[is.na(obs_term_coef_kinds(obj, coefs))]
  if (has_int) c("Intercept", terms) else terms
}

#' The kind of term that owns each observation-mean `b` coefficient
#'
#' brms names a special term's coefficient after the term, `mo(x)` as
#' `mox`. It names a smooth's unpenalised columns after its label and
#' column, `s(x)` as `sx_1`. A covariate named `month` or `size_2`
#' takes the same shape. The formula's terms decide ownership.
#'
#' @param obj An `mvgam` object.
#' @param coefs Coefficient names from its prior table.
#' @return Character vector along `coefs` of `"mo"`, `"me"`, `"mi"` or
#'   `"sm"`. A population effect takes `NA`.
#' @noRd
obs_term_coef_kinds <- function(obj, coefs) {
  out <- rep(NA_character_, length(coefs))
  for (lp in view_predictors(obj)) {
    sp <- formula_term_labels(lp$pred[["sp"]])
    kind <- ifelse(grepl("\\bmo\\(", sp), "mo",
                   ifelse(grepl("\\bme\\(", sp), "me", "mi"))
    hit <- match(coefs, brms_rename(sp))
    out[!is.na(hit)] <- kind[hit[!is.na(hit)]]
    # A smooth's columns take its function and covariates, then the
    # `by` level after a colon: `sx_1`, `t2xz_1`, `stime:seriessp_1_1`.
    for (term in formula_term_labels(lp$pred[["sm"]])) {
      sm <- eval(str2lang(term), list(s = mgcv::s, t2 = mgcv::t2))
      stem <- brms_smooth_labels(list(term = term, covars = sm$term,
                                      by_var = NA))
      out[startsWith(coefs, stem) & grepl("_[0-9]+$", coefs)] <- "sm"
    }
  }
  out
}

#' The parsed predictors a view describes
#'
#' The brms model's formula carries its family, which brms needs to
#' parse a family's own distributional parameters. A model describes
#' the mean of each response. A view of one response describes that
#' response's mean. A view from `sub_predictor_views()` describes its
#' own predictor.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return List of predictors as `model_predictors()` returns them
#' @noRd
view_predictors <- function(obj) {
  want <- obj$methods_md_pred %||% list(resp = obj$methods_md_resp)
  Filter(function(lp) {
    identical(lp$dpar, want$dpar) && identical(lp$nlpar, want$nlpar) &&
      (is.null(want$resp) || identical(lp$resp, want$resp))
  }, model_predictors(side_model(obj, "obs")$formula))
}

#' The calls of one kind of term in the predictors a view describes
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @param part The `brmsterms` part holding the terms, such as `"gp"`
#' @param fname The function the term calls, such as `"me"`
#' @return List of calls
#' @noRd
view_term_calls <- function(obj, part, fname) {
  labels <- unlist(lapply(view_predictors(obj), function(lp) {
    formula_term_labels(lp$pred[[part]])
  }), use.names = FALSE)
  calls <- lapply(labels, str2lang)
  Filter(function(e) identical(as.character(e[[1L]]), fname), calls)
}

#' The offsets of the observation mean, with their symbols
#'
#' An offset that names a column takes the column's symbol. Any other
#' expression takes `o`, indexed when there are several.
#'
#' @param obj An `mvgam` object or a view of one response
#' @return Named character vector of symbols, named by the offset
#'   expression as written
#' @noRd
obs_offset_symbols <- function(obj) {
  exprs <- unique(unlist(lapply(view_predictors(obj), function(lp) {
    off <- lp$pred[["offset"]]
    if (is.null(off)) return(NULL)
    vapply(formula_calls(off[[2L]], "offset"),
           function(e) formula_arg_text(e[[2L]]), character(1L))
  }), use.names = FALSE))
  if (!length(exprs)) return(character(0L))
  named <- vapply(exprs, function(e) is.name(str2lang(e)), logical(1L))
  sym <- escape_math_text(exprs)
  other <- which(!named)
  sym[other] <- if (length(other) == 1L) "o" else {
    paste0("o^{(", seq_along(other), ")}")
  }
  sym <- indexed(obj, sym)
  stats::setNames(sym, exprs)
}

#' The measurement-error terms of the predictors a view describes
#'
#' brms names the effect of `me(x, sdx)` `mexsdx`.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return List of specs holding the covariate `var`, its known SD
#'   `sdvar` and the brms coefficient `coef`
#' @noRd
obs_me_specs <- function(obj) {
  lapply(view_term_calls(obj, "sp", "me"), function(e) {
    args <- call_args_as_written(e)
    pos <- args$positional
    var <- args$named("x", pos[1L])
    sdvar <- args$named("sdx",
                        if (length(pos) >= 2L) pos[[2L]] else NA_character_)
    list(var = var, sdvar = sdvar,
         coef = paste0("me", var, if (!is.na(sdvar)) sdvar))
  })
}

#' The monotonic terms of the predictors a view describes
#'
#' One `b` row per `mo()` term carries the scale of its effect. Its
#' simplex of step increments has class `simo`.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return List of specs holding the covariate `var` and the brms
#'   coefficient `coef`
#' @noRd
obs_mo_specs_from_prior <- function(obj) {
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  coefs <- unique(prior$coef[prior$class == "b" & nzchar(prior$coef) &
                               mean_predictor_rows(prior)])
  coefs <- coefs[obs_term_coef_kinds(obj, coefs) %in% "mo"]
  lapply(coefs, function(c) {
    list(var = sub("^mo", "", c), coef = c)
  })
}


#' The smooth terms of a linear predictor, from their `sds` rows
#'
#' A smooth with several penalties has one `sds` row per penalty. Each
#' term, keyed by its variables and `by` variable, is described once.
#'
#' @param prior Rows of a brms prior table
#' @return List of `parse_smooth_coef()` specs
#' @noRd
obs_smooth_specs_from_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  sds_rows <- prior$class == "sds" & nzchar(prior$coef) &
    mean_predictor_rows(prior)
  specs <- lapply(prior$coef[sds_rows], parse_smooth_coef)
  specs <- Filter(function(s) !all(is.na(s$vars)), specs)
  keys <- vapply(specs, function(s) {
    paste(c(s$vars, stats::na.omit(s$by)), collapse = ":")
  }, character(1L))
  specs[!duplicated(keys)]
}

#' The arguments of a term's call, as the user wrote them
#'
#' @param call A call such as `s(x, k = 5)` or `gp(x, by = g)`
#' @return List of two elements. `positional` holds the text of each
#'   unnamed argument. `named(name, default)` returns the text of a
#'   named argument, or `default` when the call omits it.
#' @noRd
call_args_as_written <- function(call) {
  checkmate::assert_class(call, "call")
  args <- as.list(call)[-1L]
  arg_names <- names(args) %||% rep("", length(args))
  list(
    positional = unname(vapply(args[arg_names == ""], formula_arg_text,
                               character(1L))),
    named = function(name, default) {
      if (name %in% arg_names) formula_arg_text(args[[name]]) else default
    }
  )
}

#' The smooth term a brms `sds` label names
#'
#' brms labels a smooth's SD with the term as written, such as
#' `"s(x, k = 5, bs = \"cr\")"`, which parses as a call. `k`, `bs` and
#' `by` keep the text the user wrote. Their defaults are `NA`, mgcv's
#' `"tp"` and `NA`.
#'
#' @param coef_str Character `sds` coefficient label
#' @return List holding `var`, `vars`, `k`, `bs`, `by` and `fname`
#' @noRd
parse_smooth_coef <- function(coef_str) {
  checkmate::assert_string(coef_str)
  expr <- str2lang(coef_str)
  args <- call_args_as_written(expr)
  vars <- args$positional
  if (length(vars) == 0L) vars <- NA_character_
  list(
    var = vars[[1L]], vars = vars, k = args$named("k", NA_character_),
    bs = args$named("bs", "tp"), by = args$named("by", NA_character_),
    fname = as.character(expr[[1L]])
  )
}

#' The name and the symbol of a Gaussian process kernel
#'
#' @param cov Character brms kernel name, such as `"exp_quad"`
#' @return Character vector with elements `label`, for the glossary,
#'   and `symbol`, a LaTeX kernel symbol
#' @noRd
gp_kernel_names <- function(cov) {
  checkmate::assert_string(cov)
  known <- list(
    exp_quad = c("exponentiated-quadratic", "k_{\\text{ExpQuad}}"),
    matern52 = c("Matern (5/2)", "k_{\\text{Matern}_{5/2}}"),
    matern32 = c("Matern (3/2)", "k_{\\text{Matern}_{3/2}}"),
    exponential = c("exponential", "k_{\\text{Exp}}")
  )
  out <- known[[cov]] %||% c(cov, paste0("k_{\\text{", cov, "}}"))
  stats::setNames(out, c("label", "symbol"))
}

#' The name of a smooth's basis, for the glossary
#'
#' @param bs Character mgcv basis code
#' @param fname Character smooth function, such as `"s"` or `"t2"`
#' @return Character phrase
#' @noRd
basis_label <- function(bs, fname) {
  if (identical(fname, "gp")) {
    return("Gaussian process smooth")
  }
  if (identical(fname, "t2")) {
    return("tensor product smooth (t2)")
  }
  checkmate::assert_string(bs)
  switch(
    bs,
    "tp"  = "thin plate regression spline",
    "ts"  = "thin plate regression spline with shrinkage",
    "cr"  = "cubic regression spline",
    "cc"  = "cyclic cubic regression spline",
    "cs"  = "shrinkage cubic regression spline",
    "ps"  = "P-spline",
    "bs"  = "B-spline",
    "gp"  = "Gaussian process spline",
    "re"  = "random effect basis",
    "fs"  = "factor-smooth interaction",
    "mrf" = "Markov random field",
    paste0("'", bs, "' basis spline")
  )
}


#' The group-level terms of a linear predictor, from their `sd` rows
#'
#' A group's slopes are the coefficients of its `sd` rows other than
#' `Intercept`. brms writes an `L` row for a group whose intercept and
#' slopes are correlated.
#'
#' @param prior Rows of a brms prior table
#' @return List of specs holding `group`, `slopes`, `has_slope` and
#'   `has_corr`
#' @noRd
obs_re_specs_from_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  own <- mean_predictor_rows(prior) & nzchar(prior$group)
  sd_rows <- prior$class == "sd" & own
  if (!any(sd_rows)) return(list())
  groups <- unique(prior$group[sd_rows])
  l_groups <- prior$group[prior$class == "L" & own]
  lapply(groups, function(g) {
    g_rows <- sd_rows & prior$group == g & nzchar(prior$coef)
    coefs <- prior$coef[g_rows]
    slopes <- setdiff(coefs, "Intercept")
    list(
      group = g,
      slopes = slopes,
      has_slope = length(slopes) > 0L,
      has_corr = g %in% l_groups
    )
  })
}

#' The Gaussian process terms of the predictors a view describes
#'
#' A term two responses share is described once.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return List of `gp_call_to_spec()` specs
#' @noRd
obs_gp_specs <- function(obj) {
  unique(lapply(view_term_calls(obj, "gp", "gp"), gp_call_to_spec))
}

#' The settings of a `gp()` term, as written
#'
#' A setting left out takes brms's default.
#'
#' @param call A `gp()` call
#' @return List holding `vars`, `k`, `by`, `cov` and the brms
#'   coefficient stem `coef`
#' @noRd
gp_call_to_spec <- function(call) {
  args <- call_args_as_written(call)
  vars <- args$positional
  if (length(vars) == 0L) {
    stop(insight::format_error(c(
      "A 'gp()' term needs at least one variable.",
      x = paste0("Got '", deparse1(call), "'.")
    )), call. = FALSE)
  }
  list(
    vars = vars,
    k = args$named("k", NA_character_),
    by = args$named("by", NA_character_),
    cov = args$named("cov", "exp_quad"),
    coef = paste0("gp", paste(vars, collapse = ""))
  )
}

#' The population intercept and effects of a linear predictor
#'
#' @param terms Character vector of `"Intercept"` and covariate names
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX expression, or an empty vector
#' @noRd
render_fixed_inline <- function(terms, obj) {
  specs <- fixed_term_specs(obj, setdiff(terms, "Intercept"))
  bits <- c(
    if ("Intercept" %in% terms) response_symbol(obj, "\\alpha"),
    vapply(specs, function(s) {
      paste(c(response_symbol(obj, paste0("\\beta_{", s$sub, "}")),
              s$covariate), collapse = " ")
    }, character(1L))
  )
  if (length(bits)) paste(bits, collapse = " + ") else character(0L)
}

#' The population effects of a linear predictor, as the equations set
#' them
#'
#' brms names a population effect after its model-matrix column. A
#' covariate `elev` gives `elev`. Level `lower` of the factor `region`
#' gives `regionlower`. Their interaction gives `elev:regionlower`.
#' Each part between colons is a covariate, a factor level, or the
#' factor axis `.trend1`, `.trend2`, ... of a factor model's trend. A
#' factor level enters through its indicator. The coefficients of one
#' covariate on each factor form one term indexed by `k`. A factor's
#' coefficients are contrasts against the level that has none.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @param coefs Character population-effect names, without `Intercept`
#' @return List of specs, one per term, holding its `coefs`, the
#'   subscript `sub`, the `covariate` it multiplies and its glossary
#'   text `gloss`
#' @noRd
fixed_term_specs <- function(obj, coefs) {
  if (!length(coefs)) return(list())
  data <- obj$data %||% data.frame()
  series_var <- axis_vars(obj)$series_var
  pieces <- lapply(strsplit(coefs, ":", fixed = TRUE), function(p) {
    lapply(p, coef_piece, data = data)
  })
  # The levels each factor's coefficients name. The one level left out
  # is the reference.
  named_levels <- unlist(lapply(pieces, function(p) {
    lv <- Filter(function(x) identical(x$kind, "level"), p)
    stats::setNames(vapply(lv, `[[`, "", "level"),
                    vapply(lv, `[[`, "", "var"))
  }))
  reference <- function(v) {
    ref <- setdiff(levels(as.factor(data[[v]])),
                   named_levels[names(named_levels) == v])
    if (length(ref) == 1L) math_upright(ref)
  }
  specs <- Map(function(p, coef) {
    kinds <- vapply(p, `[[`, "", "kind")
    vars <- p[kinds == "var"]
    lvls <- p[kinds == "level"]
    on_factor <- any(kinds == "axis")
    var_syms <- escape_math_text(vapply(vars, `[[`, "", "var"))
    lvl_subs <- vapply(lvls, function(x) {
      paste0(escape_math_text(x$var), "[", math_upright(x$level), "]")
    }, "")
    lvl_covs <- vapply(lvls, function(x) {
      at <- if (identical(x$var, series_var)) "i" else {
        indexed(obj, escape_math_text(x$var))
      }
      paste0("\\mathbb{1}[", at, " = ", math_upright(x$level), "]")
    }, "")
    sub <- paste(c(if (length(p) > sum(kinds == "axis")) {
      paste(c(var_syms, lvl_subs), collapse = ":")
    }, if (on_factor) "k"), collapse = ", ")
    covs <- c(if (length(var_syms)) indexed(obj, var_syms), lvl_covs)
    covariate <- if (length(covs)) paste(covs, collapse = " \\, ")
    effect <- if (length(var_syms)) {
      paste0("$", paste(var_syms, collapse = " \\times "), "$")
    }
    ref <- if (length(lvls) == 1L) reference(lvls[[1L]]$var)
    level_text <- vapply(lvls, function(x) {
      paste0("level $", math_upright(x$level), "$ of $",
             escape_math_text(x$var), "$")
    }, "")
    gloss <- if (!is.null(ref)) {
      paste0(
        "difference ", if (!is.null(effect)) paste0("in the effect of ",
                                                    effect, " "),
        "between level $", math_upright(lvls[[1L]]$level),
        "$ and reference level $", ref, "$ of $",
        escape_math_text(lvls[[1L]]$var), "$"
      )
    } else if (length(vars) || length(lvls)) {
      paste0("population effect of ",
             paste(c(effect, if (length(lvls)) {
               paste(level_text, collapse = " and ")
             }), collapse = " at "))
    } else {
      "intercept"
    }
    if (on_factor) {
      gloss <- paste0(gloss, if (identical(gloss, "intercept")) " of" else {
        " on"
      }, " factor $k$")
    }
    list(coefs = coef, sub = sub, covariate = covariate, gloss = gloss)
  }, pieces, coefs)
  # A covariate's coefficient on each factor is one term
  subs <- vapply(specs, `[[`, "", "sub")
  lapply(unique(subs), function(s) {
    out <- specs[[match(s, subs)]]
    out$coefs <- coefs[subs == s]
    out
  })
}

#' One part of a population effect's name
#'
#' A part naming a column of the data is a covariate. A part made of a
#' factor's name and one of its levels, as brms joins them, is that
#' level. The factor axis of a factor model's trend is `.trend` and the
#' factor's number.
#'
#' @param piece Character part of a coefficient name
#' @param data The model's data frame
#' @return List holding `kind` (`"var"`, `"level"` or `"axis"`) with
#'   `var` and `level` where they apply
#' @noRd
coef_piece <- function(piece, data) {
  axis <- paste0("^", gsub(".", "\\.", factor_axis_column, fixed = TRUE),
                 "[0-9]+$")
  if (grepl(axis, piece)) return(list(kind = "axis"))
  if (!piece %in% names(data)) {
    is_factor <- vapply(data, function(x) is.factor(x) || is.character(x),
                        logical(1L))
    candidates <- names(data)[is_factor & startsWith(piece, names(data))]
    # The longest name wins: `region2` before `region` for `region2a`
    for (v in candidates[order(-nchar(candidates))]) {
      levs <- levels(as.factor(data[[v]]))
      lvl <- substring(piece, nchar(v) + 1L)
      hit <- levs[levs == lvl | brms_rename(levs) == lvl]
      if (length(hit)) {
        return(list(kind = "level", var = v, level = hit[[1L]]))
      }
    }
  }
  list(kind = "var", var = piece)
}

#' A symbol tagged with the predictor a view carries: its response,
#' or the trend
#' @noRd
response_symbol <- function(obj, sym) {
  r <- obj$methods_md_tag %||% escape_math_text(obj$methods_md_resp)
  if (is.null(r)) return(sym)
  vapply(sym, apply_resp_superscript, character(1L), r = r,
         USE.NAMES = FALSE)
}

#' The symbols of one term, as every section of the page names them
#'
#' The equations, the glossary and the priors each take a term's
#' parameter names from here. A view of one predictor tags each symbol
#' with that predictor.
#'
#' @param kind One of `"smooth"`, `"gp"`, `"mo"`, `"me"`, `"re"`.
#' @param spec The term's spec, as its `obs_*_specs_*()` extractor
#'   returns it.
#' @param obj The `mvgam` object or view whose predictor holds the
#'   term, or NULL for untagged symbols.
#' @return Named character vector.
#' @noRd
term_symbols <- function(kind, spec, obj = NULL) {
  out <- switch(kind,
    smooth = {
      sub <- by_subscript(spec)
      key <- spec_key(spec)
      c(f = paste0("f_{", sub, "}"),
        basis = paste0("\\beta^{(", key, ")}_m"),
        sd = paste0("\\sigma^{(s)}_{", key, "}"))
    },
    gp = {
      sub <- by_subscript(spec)
      rho <- if (length(spec$vars) > 1L) "\\boldsymbol{\\rho}" else "\\rho"
      c(f = paste0("f^{(\\text{gp})}_{", sub, "}"),
        rho = paste0(rho, "_{", sub, "}"),
        sd = paste0("\\sigma^{(\\text{gp})}_{", sub, "}"))
    },
    mo = {
      v <- escape_math_text(spec$var)
      c(beta = paste0("\\beta^{(\\text{mo})}_{", v, "}"),
        m = paste0("m_{", v, "}"),
        zeta = paste0("\\boldsymbol{\\zeta}_{", v, "}"),
        zeta_j = paste0("\\zeta_{", v, ",j}"))
    },
    me = {
      v <- escape_math_text(spec$var)
      # The priors section names a term by its variable alone.
      sdvar <- spec$sdvar %||% NA_character_
      sdx <- escape_math_text(if (is.na(sdvar)) "se" else sdvar)
      c(beta = paste0("\\beta^{(\\text{me})}_{", v, "}"),
        sdx = sdx,
        mu = paste0("\\mu^{(\\text{me})}_{", v, "}"),
        sd = paste0("\\sigma^{(\\text{me})}_{", v, "}"))
    },
    re = {
      # brms's slope names key the symbols. The symbols print them
      # escaped.
      g <- escape_math_text(spec$group)
      sl_key <- spec$slopes %||% character(0L)
      sl <- escape_math_text(sl_key)
      c(alpha = paste0("\\alpha_{", g, "}"),
        alpha_i = paste0("\\alpha_{", g, "[i]}"),
        sd = paste0("\\sigma_{", g, "}"),
        sd_alpha = paste0("\\sigma^{(\\alpha)}_{", g, "}"),
        Sigma = paste0("\\boldsymbol{\\Sigma}_{", g, "}"),
        Omega = paste0("\\boldsymbol{\\Omega}_{", g, "}"),
        stats::setNames(paste0("\\beta^{(", g, ")}_{", sl, ", ", g, "}"),
                        paste0("slope:", sl_key)),
        stats::setNames(paste0("\\beta^{(", g, ")}_{", sl, ", ", g, "[i]}"),
                        paste0("slope_i:", sl_key)),
        stats::setNames(paste0("\\sigma^{(\\beta_{", sl, "})}_{", g, "}"),
                        paste0("sd_slope:", sl_key)))
    }
  )
  if (is.null(obj)) return(out)
  stats::setNames(response_symbol(obj, out), names(out))
}

#' The subscript the covariates of a predictor carry
#'
#' A covariate varies by series and time. In an occupancy or N-mixture
#' model, the state predictor's covariates vary by closure unit. A view
#' from `sub_predictor_views()` sets the visit grain of the detection
#' predictor.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX subscript, such as `"_{i,t}"`
#' @noRd
term_index <- function(obj) {
  if (!is.null(obj$methods_md_index)) return(obj$methods_md_index)
  spec <- closure_unit_spec(obj)
  if (is.null(spec)) "_{i,t}" else paste0("_", spec$unit)
}

#' Symbols indexed as a predictor's covariates are
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @param x Character LaTeX symbols
#' @return `x` with `term_index()` appended
#' @noRd
indexed <- function(obj, x) {
  paste0(x, term_index(obj))
}

#' The terms of one kind, joined into one sum
#'
#' @param specs List of term specs
#' @param term_composer Function of one spec returning its LaTeX
#' @return Character LaTeX expression, or an empty vector
#' @noRd
compose_inline_terms <- function(specs, term_composer) {
  if (length(specs) == 0L) return(character(0L))
  paste(vapply(specs, term_composer, character(1L)), collapse = " + ")
}

#' Smooth or Gaussian process terms, each a function of its variables
#'
#' @param kind `"smooth"` or `"gp"`
#' @param specs List of term specs
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX expression, or an empty vector
#' @noRd
render_function_inline <- function(kind, specs, obj) {
  compose_inline_terms(specs, function(s) {
    args <- paste(indexed(obj, escape_math_text(s$vars)), collapse = ", ")
    paste0(term_symbols(kind, s, obj)[["f"]], "(", args, ")")
  })
}

#' Measurement-error terms of a linear predictor
#'
#' A term `me(x, sdx)` enters through the latent true covariate
#' `\tilde{x}`.
#'
#' @param specs List of `obs_me_specs()` specs
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX expression, or an empty vector
#' @noRd
render_me_inline <- function(specs, obj) {
  compose_inline_terms(specs, function(s) {
    paste0(term_symbols("me", s, obj)[["beta"]], " \\, ",
           indexed(obj, paste0("\\tilde{", escape_math_text(s$var), "}")))
  })
}

#' Monotonic terms of a linear predictor
#'
#' Each `mo(x)` term enters as its effect times `m_x(x)`, a cumulative
#' step transform from a Dirichlet simplex (Burkner and Charpentier
#' 2020).
#'
#' @param specs List of `obs_mo_specs_from_prior()` specs
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX expression, or an empty vector
#' @noRd
render_mo_inline <- function(specs, obj) {
  compose_inline_terms(specs, function(s) {
    sym <- term_symbols("mo", s, obj)
    paste0(sym[["beta"]], " \\, ", sym[["m"]], "(",
           indexed(obj, escape_math_text(s$var)), ")")
  })
}

#' A term's variables as a subscript, such as `x, z`
#'
#' @param spec A term spec holding `vars`
#' @return Character LaTeX
#' @noRd
spec_subscript <- function(spec) {
  paste(escape_math_text(spec$vars), collapse = ", ")
}

#' A term's key inside a subscript, such as `x:z`
#'
#' The key has no comma. It names the basis size `K_{key}` and the
#' basis coefficients `\beta^{(key)}`.
#'
#' @param spec A term spec holding `vars`
#' @return Character LaTeX
#' @noRd
spec_key <- function(spec) {
  paste(escape_math_text(spec$vars), collapse = ":")
}

#' Whether a smooth is a random effect basis in one factor
#'
#' @param spec A `parse_smooth_coef()` spec
#' @return Logical scalar
#' @noRd
is_re_basis <- function(spec) {
  identical(spec$bs, "re") && length(spec$vars) == 1L
}

#' Whether a smooth or GP term is fitted per level of a `by` variable
#' @noRd
has_by <- function(spec) {
  is.character(spec$by) && !is.na(spec$by) && nzchar(spec$by)
}

#' The symbol of a term's `by` variable
#'
#' A trend term with `by = lv_axis()` in a factor model varies by
#' factor, which the equations index by `k`.
#'
#' @param spec A term spec holding `by`
#' @return Character LaTeX symbol
#' @noRd
by_symbol <- function(spec) {
  if (identical(spec$by, factor_axis_column)) "k" else {
    escape_math_text(spec$by)
  }
}

#' Term subscript that names the `by` variable, as `x \mid grp`
#'
#' @param spec A term spec holding `vars` and `by`
#' @return Character LaTeX subscript
#' @noRd
by_subscript <- function(spec) {
  base <- spec_subscript(spec)
  if (has_by(spec)) paste0(base, " \\mid ", by_symbol(spec)) else base
}

#' Glossary clause naming the `by` variable of a term
#'
#' @param spec A term spec holding `by`
#' @return Character phrase, empty without `by`
#' @noRd
by_clause <- function(spec) {
  if (!has_by(spec)) return("")
  if (identical(spec$by, factor_axis_column)) return(", one per factor $k$")
  paste0(", stratified by $", by_symbol(spec), "$")
}

#' Group-level terms of a linear predictor
#'
#' Each group adds its varying intercept, `\alpha_{g[i]}`. Each of its
#' varying slopes enters times its covariate.
#'
#' @param specs List of `obs_re_specs_from_prior()` specs
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX expression, or an empty vector
#' @noRd
render_re_inline <- function(specs, obj) {
  compose_inline_terms(specs, function(s) {
    sym <- term_symbols("re", s, obj)
    slopes <- if (length(s$slopes)) {
      paste0(sym[paste0("slope_i:", s$slopes)], " ",
             indexed(obj, escape_math_text(s$slopes)))
    }
    paste(c(sym[["alpha_i"]], slopes), collapse = " + ")
  })
}

#' The equations defining each term of a linear predictor
#'
#' A smooth is a sum of basis functions. A Gaussian process takes its
#' kernel. A monotonic term is a cumulative step transform from a
#' Dirichlet simplex (Burkner and Charpentier 2020). A measurement-error
#' term observes a latent covariate with known SD. The latent covariate
#' has a population mean and SD. A group's intercepts and varying slopes
#' are jointly normal. The Priors section states each hyperparameter's
#' prior.
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return List of statements
#' @noRd
term_definition_rows <- function(obj) {
  classes <- classify_obs_parameters(obj)
  row <- function(lhs, op, rhs) list(lhs = lhs, op = op, rhs = rhs)
  smooth <- unlist(lapply(classes$smooth, function(spec) {
    sym <- term_symbols("smooth", spec, obj)
    vars <- spec_subscript(spec)
    lhs <- paste0(sym[["f"]], "(", vars, ")")
    # A random effect basis holds one coefficient per level of its
    # factor, each normal with the smooth's SD
    if (is_re_basis(spec)) {
      level <- paste0(sub("_m$", "", sym[["basis"]]), "_{", vars, "}")
      return(list(
        row(lhs, "=", level),
        row(sym[["basis"]], "\\sim",
            paste0("\\text{Normal}(0, ", sym[["sd"]], ")"))
      ))
    }
    list(row(lhs, "=",
             paste0("\\sum_{m=1}^{K_{", spec_key(spec), "}} ",
                    sym[["basis"]], " B_m(", vars, ")")))
  }), recursive = FALSE)
  gp <- lapply(classes$gp, function(spec) {
    sym <- term_symbols("gp", spec, obj)
    row(paste0(sym[["f"]], "(", spec_subscript(spec), ")"), "\\sim",
        paste0("\\text{GP}\\left(0, ",
               gp_kernel_names(spec$cov)[["symbol"]], "(", sym[["rho"]],
               ", ", sym[["sd"]], ")\\right)"))
  })
  mo <- lapply(classes$mo, function(s) {
    v <- escape_math_text(s$var)
    sym <- term_symbols("mo", s, obj)
    row(paste0(sym[["m"]], "(", v, ")"), "=",
        paste0("(D_{", v, "} - 1) \\sum_{j=1}^{", v, "} ", sym[["zeta_j"]]))
  })
  me <- unlist(lapply(classes$me, function(s) {
    v <- escape_math_text(s$var)
    sym <- term_symbols("me", s, obj)
    latent <- indexed(obj, paste0("\\tilde{", v, "}"))
    list(
      row(indexed(obj, v), "\\sim",
          paste0("\\text{Normal}\\!\\left(", latent, ", ",
                 indexed(obj, sym[["sdx"]]), "\\right)")),
      row(latent, "\\sim",
          paste0("\\text{Normal}\\!\\left(", sym[["mu"]], ", ", sym[["sd"]],
                 "\\right)"))
    )
  }), recursive = FALSE)
  re <- unlist(lapply(classes$re, function(s) {
    sym <- term_symbols("re", s, obj)
    if (!s$has_slope) {
      return(list(row(sym[["alpha"]], "\\sim",
                      paste0("\\text{Normal}(0, ", sym[["sd"]], ")"))))
    }
    coefs <- c(sym[["alpha"]], sym[paste0("slope:", s$slopes)])
    sds <- paste(c(sym[["sd_alpha"]], sym[paste0("sd_slope:", s$slopes)]),
                 collapse = ", ")
    list(
      row(paste0("(", paste(coefs, collapse = ", "), ")^\\top"), "\\sim",
          paste0("\\text{MVNormal}\\!\\left(\\mathbf{0}, ", sym[["Sigma"]],
                 "\\right)")),
      row(sym[["Sigma"]], "=", scaled_cor_text(sds, sym[["Omega"]]))
    )
  }), recursive = FALSE)
  c(smooth, gp, mo, me, re)
}

#' Whether a model has a latent trend
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Logical scalar
#' @noRd
methods_md_has_latent_trend <- function(obj) {
  tt <- obj$trend_metadata$trend_type
  !is.null(tt) && !identical(tt, "None") && !identical(tt, "none")
}

#' The trend's linear predictor, viewed as a model of its own
#'
#' The trend formula's terms enter the trend after its dynamics, as
#' `trend = Z lv + mu_trend`. The view carries the trend's brms
#' formula and its brms prior rows under brms's own class names. The
#' observation-side extractors and renderers then describe `mu_trend`
#' with no argument of their own for it. The rows mvgam writes keep
#' their `_trend` names.
#'
#' @param obj An `mvgam` object.
#' @return The view, with no latent trend of its own.
#' @noRd
trend_side_view <- function(obj) {
  out <- obj
  out$formula <- obj$trend_model$formula
  out$obs_model <- obj$trend_model
  out$methods_md_tag <- "\\eta"
  out$methods_md_index <- if (trend_mu_on_factors(obj)) "_{t}" else "_{i,t}"
  out$trend_metadata <- NULL
  out$prior <- NULL
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(out)
  prior <- prior[grepl("_trend$", prior$class), , drop = FALSE]
  bare <- sub("_trend$", "", prior$class)
  brms_classes <- setdiff(
    obj$trend_model$prior$class, brms_trend_dropped_params
  )
  owned <- bare %in% brms_classes
  prior$class[owned] <- bare[owned]
  out$prior <- prior
  out
}

#' The terms the trend adds to an observation linear predictor
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Character LaTeX terms, empty without a trend
#' @noRd
trend_linpred_terms <- function(obj) {
  if (!methods_md_has_latent_trend(obj)) return(character(0L))
  c(if (!trend_mu_on_factors(obj)) trend_mu_symbol(obj), "\\eta_{i,t}")
}

#' Whether the trend formula's predictor acts on the latent factors
#'
#' Under `by = lv_axis()` in a factor model, the trend data runs over
#' time and factor. Each factor's predictor joins its state before the
#' loadings, as `generate_trend_computation_tparameters()` writes it.
#' Its covariates vary by time alone.
#'
#' @param obj An `mvgam` object
#' @return Logical scalar
#' @noRd
trend_mu_on_factors <- function(obj) {
  mvgam_had_by_lv(obj) && methods_md_has_factor_model(obj)
}

#' The symbol of the trend formula's predictor
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX symbol, or `NULL` without trend formula terms
#' @noRd
trend_mu_symbol <- function(obj) {
  if (!methods_md_has_latent_trend(obj) ||
        !has_linear_terms(trend_side_view(obj))) {
    return(NULL)
  }
  paste0("\\mu^{(\\eta)}_{", if (trend_mu_on_factors(obj)) "k" else "i",
         ",t}")
}

#' Whether a view's predictor has any term
#'
#' @param obj An `mvgam` object or a view of one predictor
#' @return Logical scalar
#' @noRd
has_linear_terms <- function(obj) {
  !is.null(obj$prior) && any(lengths(classify_obs_parameters(obj)) > 0L)
}

#' The grouping variable of a hierarchical trend, as the equations set it
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX symbol of `gr`, or `NULL` for an ungrouped
#'   trend
#' @noRd
trend_grouping_var <- function(obj) {
  gr <- first_trend_spec(obj)$gr
  if (named_var(gr)) escape_math_text(gr)
}


#' The time subscript of each lag, such as `t` or `t-2`
#'
#' @param lag Integer vector of lags
#' @return Character vector along `lag`
#' @noRd
time_subscript <- function(lag) {
  ifelse(lag == 0L, "t", paste0("t-", lag))
}

#' A symbol of the latent dynamics, indexed by process and time
#'
#' The dynamics of a factor model act on its factors `k`, each symbol
#' written with a tilde. `factor_model_rows()` maps the factors to the
#' series. Every other trend's dynamics act on the series `i`.
#'
#' @param obj An `mvgam` object
#' @param sym Character LaTeX symbol, such as `"\\eta"`
#' @param lag Integer vector of lags
#' @param vector Logical. `TRUE` writes the bold vector over processes.
#' @return Character vector along `lag`
#' @noRd
trend_state_symbol <- function(obj, sym, lag = 0L, vector = FALSE) {
  is_factor <- methods_md_has_factor_model(obj)
  time <- time_subscript(lag)
  if (vector) {
    body <- paste0("\\boldsymbol{", sym, "}")
    if (is_factor) body <- paste0("\\tilde{", body, "}")
    return(paste0(body, "_{", time, "}"))
  }
  paste0(if (is_factor) "\\tilde", sym, "_{", if (is_factor) "k" else "i",
         ",", time, "}")
}

#' The latent state symbol at time `t` or a lag
#' @inheritParams trend_state_symbol
#' @noRd
trend_eta <- function(obj, lag = 0L, vector = FALSE) {
  trend_state_symbol(obj, "\\eta", lag, vector)
}

#' The innovation symbol at time `t` or a lag
#' @inheritParams trend_state_symbol
#' @noRd
trend_eps <- function(obj, lag = 0L, vector = FALSE) {
  trend_state_symbol(obj, if (vector) "\\epsilon" else "\\epsilon^{(\\eta)}",
                     lag, vector)
}

#' The rows giving the law of a trend's innovations
#'
#' A heavy-tailed trend writes its innovations as Student-t with its
#' degrees of freedom, fixed or estimated as `\nu_\eta`.
#'
#' @param obj An `mvgam` object
#' @param is_vector Logical. `TRUE` for a trend written as a vector.
#' @return List of statements
#' @noRd
innovation_rows <- function(obj, is_vector) {
  sd <- innovation_sd(obj)
  sd_vec <- innovation_sd(obj, vector = TRUE)
  form <- innovation_form(obj, is_vector)
  eps_vec <- trend_eps(obj, vector = TRUE)
  df <- obj$trend_metadata$df %||% Inf
  heavy <- !is_gaussian_df(df)
  nu <- if (heavy && is.na(df)) "\\nu_\\eta" else format(df)
  mvn <- if (heavy) {
    paste0("\\text{MVStudentT}(", nu, ", \\mathbf{0}, ")
  } else {
    "\\text{MVNormal}(\\mathbf{0}, "
  }

  rows <- switch(form,
    cor = list(list(
      lhs = eps_vec, op = "\\sim",
      rhs = paste0(mvn, innovation_cov_symbol(obj), ")")
    )),
    diag = list(list(
      lhs = eps_vec, op = "\\sim",
      rhs = paste0(mvn, diag_cov_text(sd_vec), ")")
    )),
    list(list(
      lhs = trend_eps(obj), op = "\\sim",
      rhs = if (heavy) {
        paste0("\\text{StudentT}(", nu, ", 0, ", sd, ")")
      } else {
        paste0("\\text{Normal}(0, ", sd, ")")
      }
    ))
  )
  c(rows, covariance_rows(obj))
}

#' The rows that build a correlated trend covariance from its parts
#' @noRd
covariance_rows <- function(obj) {
  sd <- innovation_sd(obj, vector = TRUE)
  gr <- trend_grouping_var(obj)
  if (!is.null(gr)) return(hierarchical_cor_rows(obj, sd))
  if (!trend_samples_cor(obj)) return(list())
  is_car <- identical(obj$trend_metadata$trend_type, "CAR")
  c(car_gap_covariance_rows(obj),
    list(cov_decomposition_row(sd, scale = if (is_car) car_coherence_symbol)),
    if (is_car) list(car_coherence_row()))
}

# The matrix taking a continuous-time AR's instantaneous correlation
# to its innovation correlation over a gap of one
car_coherence_symbol <- "\\mathbf{C}"

#' The row defining `car_coherence_symbol`
#' @noRd
car_coherence_row <- function() {
  gain <- function(i) {
    paste0("\\sqrt{2 \\lambda_{", i, "} / (1 - \\rho_{", i, "}^2)}")
  }
  list(
    lhs = "C_{ij}", op = "=",
    rhs = paste0(
      gain("i"), " \\, ", gain("j"),
      " \\, (1 - \\rho_{i} \\rho_{j}) / (\\lambda_{i} + \\lambda_{j})"
    )
  )
}

# The time between two successive occasions of a continuous-time AR
car_gap_symbol <- "\\Delta_t"

#' The covariance the innovation vector is written with
#'
#' A correlated continuous-time AR draws its innovations from a
#' covariance that depends on the gap they span. Every other
#' correlated trend draws them from one covariance.
#' @noRd
innovation_cov_symbol <- function(obj) {
  if (identical(obj$trend_metadata$trend_type, "CAR")) {
    return("\\mathbf{Q}_t")
  }
  group_matrix_symbol("Sigma", trend_grouping_var(obj))
}

#' The row giving a correlated continuous-time AR its gap covariance
#' @noRd
car_gap_covariance_rows <- function(obj) {
  if (!identical(obj$trend_metadata$trend_type, "CAR")) return(list())
  rho <- function(i) paste0("\\rho_{", i, "}")
  cross <- paste0(rho("i"), " ", rho("j"))
  list(list(
    lhs = "Q_{t,ij}", op = "=",
    rhs = paste0("\\Sigma_{ij} \\, (1 - (", cross, ")^{", car_gap_symbol,
                 "}) / (1 - ", cross, ")")
  ))
}

#' How the trend's innovations are written
#'
#' @return `"cor"` for a vector with a correlated covariance, `"diag"`
#'   for a vector with independent entries, `"scalar"` for one
#'   innovation per series.
#' @noRd
innovation_form <- function(obj, is_vector) {
  if (trend_samples_cor(obj)) return("cor")
  if (is_vector) "diag" else "scalar"
}

#' Whether the trend samples an innovation correlation
#'
#' The stored prior table holds the correlation's prior exactly when
#' the model samples one. A factor VAR fixes it at the identity.
#' @noRd
trend_samples_cor <- function(obj) {
  !is.null(trend_grouping_var(obj)) ||
    "L_Omega_trend" %in% obj$prior$class
}

#' The innovation scale the model samples, derives or fixes
#'
#' The stored prior table holds `sigma_trend` exactly when the model
#' samples it. A grouped trend samples one scale vector per group.
#' Multiplicative gamma process shrinkage derives the factor scales
#' from the shrinkage product. Sampled loadings fix the factors' scale
#' at 1.
#'
#' @noRd
innovation_sd <- function(obj, vector = FALSE) {
  if ("sigma_trend" %in% obj$prior$class) {
    sym <- trend_symbols(obj)
    # A vector of innovations has one SD per process.
    if (vector && !is.null(trend_process_index(obj))) {
      return("\\boldsymbol{\\sigma}_\\eta")
    }
    # A continuous-time AR scales each innovation to its gap, which
    # holds the stationary variance fixed at any spacing.
    if (identical(obj$trend_metadata$trend_type, "CAR")) {
      rho <- sym[["ar1_trend"]]
      return(paste0(sym[["sigma_trend"]], " \\sqrt{(1 - ", rho,
                    "^{2 ", car_gap_symbol, "}) / (1 - ", rho, "^2)}"))
    }
    return(sym[["sigma_trend"]])
  }
  if ("sigma_group_trend" %in% obj$prior$class) {
    return(trend_symbols(obj)[["sigma_group_trend"]])
  }
  spec <- first_trend_spec(obj)$loadings_prior_spec
  if (!is.null(spec) && isTRUE(loadings_spec_traits(spec)$mgp)) {
    return("\\sqrt{\\Psi_k}")
  }
  "1"
}

#' The covariance of independent innovations with the given SDs
#'
#' Under multiplicative gamma process shrinkage the variances are the
#' shrinkage products.
#'
#' @param sd Character LaTeX SD symbol from `innovation_sd()`
#' @return Character LaTeX matrix
#' @noRd
diag_cov_text <- function(sd) {
  if (identical(sd, "1")) return("\\mathbf{I}")
  if (identical(sd, "\\sqrt{\\Psi_k}")) {
    return("\\text{diag}(\\boldsymbol{\\Psi})")
  }
  paste0("\\text{diag}(", sd, "^2)")
}

#' A covariance written as its SDs scaling a correlation matrix
#'
#' @param sd Character LaTeX SD vector, `"1"` for unit scale
#' @param omega Character LaTeX correlation matrix
#' @return Character LaTeX matrix
#' @noRd
scaled_cor_text <- function(sd, omega) {
  if (identical(sd, "1")) return(omega)
  paste0("\\text{diag}(", sd, ") \\, ", omega, " \\, \\text{diag}(", sd, ")")
}

#' A bold matrix symbol, subscripted by a grouping variable
#'
#' @param name Character LaTeX letter name, such as `"Sigma"`
#' @param gr Character grouping variable, or `NULL`
#' @return Character LaTeX symbol
#' @noRd
group_matrix_symbol <- function(name, gr = NULL) {
  paste0("\\boldsymbol{\\", name, "}", if (!is.null(gr)) paste0("_{", gr, "}"))
}

#' The row building the trend's innovation covariance
#'
#' A continuous-time AR multiplies the correlation, element by element,
#' by its coherence matrix.
#'
#' @param sd Character LaTeX SD vector
#' @param gr Character grouping variable, or `NULL`
#' @param scale Character LaTeX matrix scaling the correlation, or
#'   `NULL`
#' @return A statement
#' @noRd
cov_decomposition_row <- function(sd, gr = NULL, scale = NULL) {
  omega <- group_matrix_symbol("Omega", gr)
  if (!is.null(scale)) omega <- paste0("(", omega, " \\circ ", scale, ")")
  list(lhs = group_matrix_symbol("Sigma", gr), op = "=",
       rhs = scaled_cor_text(sd, omega))
}

#' The rows of a grouped trend's innovation covariance
#'
#' A trend given `gr` pools each group's correlation with a global one.
#'
#' @param obj An `mvgam` object
#' @param sd Character LaTeX SD vector
#' @return List of statements
#' @noRd
hierarchical_cor_rows <- function(obj, sd) {
  gr <- trend_grouping_var(obj)
  sym <- trend_symbols(obj)
  list(
    list(
      lhs = group_matrix_symbol("Omega", gr), op = "=",
      rhs = paste0(
        sym[["alpha_cor_trend"]], " ", sym[["L_Omega_global_trend"]],
        " + (1 - ", sym[["alpha_cor_trend"]], ") ",
        sym[["L_deviation_group_trend"]]
      )
    ),
    cov_decomposition_row(sd, gr)
  )
}

#' Symbols for the trend parameters whose notation depends on the model
#'
#' The equations, the glossary and the priors section each take these
#' parameters' symbols from here.
#'
#' @param obj An `mvgam` object.
#' @return Named character vector keyed by prior class.
#' @noRd
trend_symbols <- function(obj) {
  gr <- trend_grouping_var(obj) %||% "g"
  tt <- obj$trend_metadata$trend_type %||% ""
  lags <- obj$trend_metadata$ar_lags %||% 1L
  ma_lags <- obj$trend_metadata$ma_lags %||% integer(0L)
  idx <- trend_process_index(obj)
  # Each lag's coefficient takes the process index. Its shared form
  # drops the index. A hierarchical fit adds a mean and an SD per lag.
  lag_family <- function(base, suffix) {
    if (length(lags) == 0L) return(character(0L))
    nm <- paste0("ar", lags, suffix, "_trend")
    c(
      stats::setNames(lag_symbol(base, lags, idx), nm),
      stats::setNames(lag_symbol(base, lags), paste0("shared_", nm)),
      stats::setNames(paste0("\\mu^{(", base, ")}_{", lags, "}"),
                      paste0("mu_", nm)),
      stats::setNames(paste0("\\sigma^{(", base, ")}_{", lags, "}"),
                      paste0("sigma_", nm))
    )
  }
  c(
    sigma_group_trend = paste0("\\boldsymbol{\\sigma}_{\\eta,", gr, "}"),
    alpha_cor_trend = "\\alpha_{\\mathrm{cor}}",
    L_Omega_global_trend = "\\boldsymbol{\\Omega}_{\\text{global}}",
    L_deviation_group_trend = paste0(
      "\\boldsymbol{\\Omega}_{", gr, ", \\text{local}}"
    ),
    sigma_trend = if (is.null(idx)) "\\sigma_\\eta" else {
      paste0("\\sigma_{\\eta,", idx, "}")
    },
    llt_symbols(obj, idx),
    # A continuous-time AR has one decay rate per series.
    if (identical(tt, "CAR")) {
      c(ar1_trend = if (is.null(idx)) "\\rho" else paste0("\\rho_{", idx, "}"))
    } else {
      lag_family("\\phi", "")
    },
    # A contiguous AR(p) samples its partial autocorrelations and
    # maps them to stationary coefficients.
    lag_family("\\psi", "_pacf"),
    if (length(ma_lags) > 0L) {
      stats::setNames(lag_symbol("\\theta", ma_lags, idx),
                      paste0("theta", ma_lags, "_trend"))
    }
  )
}

#' Symbols for the slope parameters of a local linear trend
#'
#' Sampled factor loadings give the slope one scale and one damping,
#' each written without an index. The initial slope keeps one value per
#' process.
#' @param obj An `mvgam` object
#' @param idx The process index, or `NULL` for one process
#' @return Named character vector keyed by prior class
#' @noRd
llt_symbols <- function(obj, idx) {
  shared <- methods_md_has_factor_model(obj) &&
    samples_factor_loadings(first_trend_spec(obj))
  sub <- if (shared || is.null(idx)) "" else paste0(",", idx)
  start <- if (methods_md_has_factor_model(obj)) "\\tilde\\beta_{k,1}" else
    "\\beta_{i,1}"
  c(sigma_slope_trend = paste0("\\sigma_{\\beta", sub, "}"),
    ar1_slope_trend = paste0("\\phi_{\\beta", sub, "}"),
    init_slope_trend = start)
}

#' The index a trend's per-process parameters carry
#'
#' @return `NULL` for a trend with one process, `"k"` for a factor
#'   model's factors and `"i"` for the series.
#' @noRd
trend_process_index <- function(obj) {
  is_factor <- methods_md_has_factor_model(obj)
  n <- if (is_factor) {
    obj$trend_metadata$n_lv
  } else {
    printed_axis_counts(obj)$n_series
  }
  if (is.null(n) || n <= 1L) return(NULL)
  if (is_factor) "k" else "i"
}

#' A lag coefficient's symbol, such as `\\phi_{i,12}`
#' @noRd
lag_symbol <- function(base, lag, idx = NULL) {
  if (length(lag) == 0L) return(character(0L))
  paste0(base, "_{", if (!is.null(idx)) paste0(idx, ","), lag, "}")
}

#' Whether every process shares the autoregressive coefficients
#'
#' @param obj An `mvgam` object
#' @return Logical scalar
#' @noRd
ar_coefs_shared <- function(obj) {
  any(startsWith(obj$prior$class, "shared_ar"))
}

#' The index an autoregressive coefficient carries
#'
#' A shared coefficient is one value for every process and carries no
#' process index.
#'
#' @param obj An `mvgam` object
#' @return `trend_process_index()`, or `NULL` for shared coefficients
#' @noRd
ar_coef_index <- function(obj) {
  if (!ar_coefs_shared(obj)) trend_process_index(obj)
}

#' The autoregressive coefficient symbols the dynamics are written with
#'
#' @param obj An `mvgam` object
#' @return Character LaTeX symbols, one per lag
#' @noRd
ar_coef_symbols <- function(obj) {
  lag_symbol("\\phi", obj$trend_metadata$ar_lags %||% 1L, ar_coef_index(obj))
}

#' The moving-average terms of a trend's dynamics
#' @noRd
ma_terms <- function(obj) {
  if (!trend_has_ma(obj)) return(character(0L))
  ma_lags <- obj$trend_metadata$ma_lags
  paste(
    paste0(trend_symbols(obj)[paste0("theta", ma_lags, "_trend")], " ",
           trend_eps(obj, lag = ma_lags)),
    collapse = " + "
  )
}


# ---------------------------------------------------------------
# Latent dynamics
# ---------------------------------------------------------------
# Every trend defines `render_latent_<type>(obj)`, which
# returns the {lhs, op, rhs} rows for the dynamics of eta_{i,t} and
# its innovation distribution. The composer appends these rows to
# the model's align* block.

#' The rows of the latent dynamics, from the trend's own renderer
#'
#' Each trend type defines `render_latent_<type>(obj)`, which
#' `trend_method()` finds.
#'
#' @param obj An `mvgam` object
#' @return List of statements, empty without a trend
#' @noRd
latent_dynamics_rows <- function(obj) {
  if (!methods_md_has_latent_trend(obj)) {
    return(list())
  }
  trend_method(obj$trend_metadata$trend_type,
               trend_method_templates[["latent_rows"]])(obj)
}

#' The dynamics of a random walk, with any moving-average part
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_rw <- function(obj) {
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste(c(trend_eta(obj, lag = 1L), trend_eps(obj), ma_terms(obj)),
                  collapse = " + ")
    )),
    innovation_rows(obj, is_vector = FALSE)
  )
}

#' The dynamics of an autoregression, with any moving-average part
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_ar <- function(obj) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  rhs <- paste(c(
    paste0(ar_coef_symbols(obj), " ", trend_eta(obj, lag = lags)),
    trend_eps(obj), ma_terms(obj)
  ), collapse = " + ")
  c(
    list(list(lhs = trend_eta(obj), op = "=", rhs = rhs)),
    innovation_rows(obj, is_vector = FALSE)
  )
}

#' The dynamics of a vector autoregression, as a vector equation
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_var <- function(obj) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  ma_lags <- if (trend_has_ma(obj)) obj$trend_metadata$ma_lags
  rhs <- paste(c(
    paste0("\\boldsymbol{\\Phi}_{", lags, "} ",
           trend_eta(obj, lag = lags, vector = TRUE)),
    trend_eps(obj, vector = TRUE),
    if (length(ma_lags)) {
      paste0("\\boldsymbol{\\Theta}_{", ma_lags, "} ",
             trend_eps(obj, lag = ma_lags, vector = TRUE))
    }
  ), collapse = " + ")
  c(
    list(list(lhs = trend_eta(obj, vector = TRUE), op = "=", rhs = rhs)),
    innovation_rows(obj, is_vector = TRUE)
  )
}

#' The dynamics of a continuous-time autoregression
#'
#' Each step's coefficient is the decay over the gap it spans.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_car <- function(obj) {
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste0(
        trend_symbols(obj)[["ar1_trend"]], "^{", car_gap_symbol, "} ",
        trend_eta(obj, lag = 1L),
        " + ", trend_eps(obj)
      )
    )),
    innovation_rows(obj, is_vector = FALSE)
  )
}

#' The slope symbol of a local linear trend, at `t` or a lag
#' @inheritParams trend_state_symbol
#' @noRd
llt_slope <- function(obj, lag = 0L) {
  trend_state_symbol(obj, "\\beta", lag)
}

#' The dynamics of a local linear trend: a level and a slope
#'
#' A damped trend shrinks the slope towards zero at each step.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_llt <- function(obj) {
  sym <- trend_symbols(obj)
  zeta <- trend_state_symbol(obj, "\\epsilon^{(\\beta)}")
  damping <- if ("ar1_slope_trend" %in% obj$prior$class) {
    paste0(sym[["ar1_slope_trend"]], " ")
  } else {
    ""
  }
  c(
    list(
      list(
        lhs = trend_eta(obj), op = "=",
        rhs = paste(trend_eta(obj, lag = 1L), llt_slope(obj, lag = 1L),
                    trend_eps(obj), sep = " + ")
      ),
      list(
        lhs = llt_slope(obj), op = "=",
        rhs = paste0(damping, llt_slope(obj, lag = 1L), " + ", zeta)
      ),
      llt_slope_shock_row(obj, zeta)
    ),
    innovation_rows(obj, is_vector = FALSE)
  )
}

#' The law of the slope shocks: the level's correlation when it has one
#' @noRd
llt_slope_shock_row <- function(obj, zeta) {
  sd <- llt_symbols(obj, trend_process_index(obj))[["sigma_slope_trend"]]
  if (!trend_samples_cor(obj)) {
    return(list(lhs = zeta, op = "\\sim",
                rhs = paste0("\\text{Normal}(0, ", sd, ")")))
  }
  omega <- group_matrix_symbol("Omega", trend_grouping_var(obj))
  list(
    lhs = "\\boldsymbol{\\epsilon}^{(\\beta)}_t", op = "\\sim",
    rhs = paste0("\\text{MVNormal}(\\mathbf{0}, ",
                 scaled_cor_text("\\boldsymbol{\\sigma}_\\beta", omega), ")")
  )
}

#' The glossary entries for a local linear trend's slope
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets
#' @noRd
glossary_llt <- function(obj) {
  sym <- trend_symbols(obj)
  out <- c(
    paste0("- $", llt_slope(obj), "$: slope, the level's growth from ",
           "$t$ to $t + 1$"),
    paste0("- $", sym[["sigma_slope_trend"]], "$: SD of the slope shocks",
           if (trend_samples_cor(obj)) {
             ", which take the correlation of the level shocks"
           })
  )
  if ("ar1_slope_trend" %in% obj$prior$class) {
    out <- c(out, paste0(
      "- $", sym[["ar1_slope_trend"]], "$: slope damping"
    ))
  }
  c(out, paste0("- $", sym[["init_slope_trend"]], "$: initial slope, ",
                "estimated"))
}

#' The states of a ZMVN trend, drawn afresh at each time
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_zmvn <- function(obj) {
  gr <- trend_grouping_var(obj)
  cov <- if (trend_samples_cor(obj)) {
    group_matrix_symbol("Sigma", gr)
  } else {
    diag_cov_text(innovation_sd(obj, vector = TRUE))
  }
  rows <- list(list(
    lhs = trend_eta(obj, vector = TRUE),
    op  = "\\sim",
    rhs = paste0("\\text{MVNormal}(\\mathbf{0}, ", cov, ")")
  ))
  c(rows, covariance_rows(obj))
}

#' The piecewise linear or logistic growth of a PW trend
#'
#' A PW trend takes no latent factors. Its state is `\eta_{i,t}`.
#' The Priors section states the priors on `k`, `m` and `delta`.
#'
#' @param obj An `mvgam` object
#' @return List of statements
#' @noRd
render_latent_pw <- function(obj) {
  slope <- "k_i + \\mathbf{a}(t)^\\top \\boldsymbol{\\delta}_i"
  rhs <- if (pw_is_logistic(obj$trend_metadata)) {
    paste0(
      "C_{i,t} \\, \\text{logit}^{-1}\\!\\left((", slope, ") (t - m_i - ",
      "\\mathbf{a}(t)^\\top \\boldsymbol{\\gamma}_i)\\right)"
    )
  } else {
    paste0(
      "(", slope, ") t - \\mathbf{a}(t)^\\top ",
      "(\\mathbf{s} \\odot \\boldsymbol{\\delta}_i)"
    )
  }
  list(list(lhs = "\\eta_{i,t}", op = "=", rhs = rhs))
}

#' The glossary entries for a PW trend's growth
#'
#' @param obj An `mvgam` object
#' @return Character vector of Markdown bullets
#' @noRd
glossary_pw <- function(obj) {
  out <- c(
    "- $k_i$: base growth rate of series $i$",
    paste0(
      "- $\\boldsymbol{\\delta}_i$: changes in growth rate at the ",
      "changepoints $\\mathbf{s}$, and $\\mathbf{a}(t)$ flags the ",
      "changepoints reached by time $t$"
    )
  )
  if (!pw_is_logistic(obj$trend_metadata)) return(out)
  c(out,
    "- $m_i$: offset that places the logistic curve along time",
    paste0(
      "- $C_{i,t}$: carrying capacity, and $\\boldsymbol{\\gamma}_i$ ",
      "the offset adjustments that keep the curve continuous"
    )
  )
}


#' Whether a model's trend has latent factors
#'
#' `detect_factor_n_lv()` counts factors only where the user asked for
#' them. Every fit's `n_lv` slot counts the trend's latent columns.
#'
#' @param obj An `mvgam` object
#' @return Logical scalar
#' @noRd
methods_md_has_factor_model <- function(obj) {
  !is.null(detect_factor_n_lv(obj))
}

#' The rows mapping latent factors to the series through the loadings
#'
#' The equation states the sampled parameterisation, `eta = Z lv`. The
#' equation leaves out the QR rotation that `ordinate()` and the factor
#' plots apply after the fit.
#'
#' @param obj An `mvgam` object
#' @return List of statements, empty without latent factors
#' @noRd
factor_model_rows <- function(obj) {
  n_lv <- detect_factor_n_lv(obj)
  if (is.null(n_lv)) return(list())
  state <- trend_eta(obj)
  if (trend_mu_on_factors(obj)) {
    state <- paste0("(", paste(c(state, trend_mu_symbol(obj)),
                               collapse = " + "), ")")
  }
  c(
    list(list(lhs = "\\eta_{i,t}", op = "=",
              rhs = paste0("\\sum_{k=1}^{", n_lv, "} Z_{i,k} ", state))),
    # A softmax ignores a shift shared by every series. Each loadings
    # column of a simplex family is a `sum_to_zero_vector`.
    if (is_simplex_response_family(obj$family)) {
      list(list(lhs = "\\textstyle\\sum_{i} Z_{i,k}", op = "=", rhs = "0"))
    },
    loadings_prior_rows(first_trend_spec(obj)$loadings_prior_spec,
                        obj$trend_metadata$fixed_Z)
  )
}

#' The rows building the loadings' prior
#'
#' `trend_map` fixes the loadings. Free loadings with no kernel and no
#' shrinkage take the stored prior, which the Priors section states.
#'
#' @param spec The trend's loadings prior spec, or `NULL`
#' @param fixed_Z The loadings `trend_map` fixes, or `NULL`
#' @return List of statements
#' @noRd
loadings_prior_rows <- function(spec, fixed_Z) {
  if (!is.null(fixed_Z)) {
    return(list(list(lhs = "Z_{i,k}", op = "=",
                     rhs = "\\text{fixed by \\texttt{trend\\_map}}")))
  }
  if (is.null(spec)) return(list())
  traits <- loadings_spec_traits(spec)
  c(
    if (traits$kernel) kernel_assembly_rows(spec),
    if (traits$mgp) mgp_shrinkage_rows(),
    # A kernel correlates each loadings column across the series, at
    # unit scale. Shrinkage scales the factor innovations instead.
    if (traits$kernel) {
      list(list(lhs = "Z_{\\cdot,k}", op = "\\sim",
                rhs = "\\text{MVNormal}(\\mathbf{0}, L_\\Phi L_\\Phi^\\top)"))
    }
  )
}

#' The rows building the loadings kernel
#'
#' The kernel multiplies, element by element, one exponential kernel
#' per distance matrix and an exponential Gaussian process on the
#' series features, as `make_loadings_prior_stanvars()` writes it.
#'
#' @param spec The trend's loadings prior spec
#' @return List of statements
#' @noRd
kernel_assembly_rows <- function(spec) {
  dnames <- escape_math_text(names(spec$distance_mats) %||% character(0L))
  terms <- c(
    if (length(dnames)) {
      paste0("\\exp(-d_{", dnames, "} / \\theta_{", dnames, "})")
    },
    if (loadings_spec_traits(spec)$features) {
      paste0("\\text{GP}_{\\text{exp}}(\\text{features}; 1, ",
             "\\boldsymbol{\\theta}_{\\text{features}})")
    }
  )
  list(
    list(lhs = "\\boldsymbol{\\Phi}", op = "=",
         rhs = paste(terms, collapse = " \\odot ")),
    list(lhs = "L_\\Phi", op = "=",
         rhs = "\\text{Cholesky}(\\boldsymbol{\\Phi})")
  )
}

#' The glossary entries of the loadings kernel
#'
#' @param spec The trend's loadings prior spec, or `NULL`
#' @return Character vector of Markdown bullets, empty without a kernel
#' @noRd
loadings_glossary <- function(spec) {
  traits <- loadings_spec_traits(spec)
  if (!traits$kernel) return(character(0L))
  dnames <- names(spec$distance_mats) %||% character(0L)
  sources <- c(if (traits$features) "trait features",
               if (traits$distances) "distances")
  c(
    paste0("- $\\boldsymbol{\\Phi}$: correlation of each factor's ",
           "loadings across the series, built from the series' ",
           paste(sources, collapse = " and ")),
    if (length(dnames)) {
      d <- escape_math_text(dnames)
      paste0("- $d_{", d, "}$: distances between the series from '",
             dnames, "', with length scale $\\theta_{", d, "}$")
    },
    if (traits$features) {
      paste0("- $\\text{GP}_{\\text{exp}}(\\text{features}; 1, ",
             "\\boldsymbol{\\theta}_{\\text{features}})$: exponential ",
             "kernel of unit variance on the ", spec$N_features_trend, " ",
             ngettext(spec$N_features_trend, "trait feature",
                      "trait features"),
             ", one length scale per feature")
    }
  )
}

#' The cumulative shrinkage of multiplicative gamma process loadings
#'
#' The Priors section states each increment's prior.
#'
#' @return List of statements
#' @noRd
mgp_shrinkage_rows <- function() {
  list(list(lhs = "\\Psi_k", op = "=",
            rhs = "\\prod_{h \\le k} \\varrho_h"))
}

#' The Priors section: one display equation of every proper prior
#'
#' The mean's rows come first, then each distributional and non-linear
#' predictor's, then the trend's.
#'
#' @param obj An `mvgam` object
#' @return Markdown string
#' @noRd
render_priors_section <- function(obj) {
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) {
    return("## Priors\n\n(no prior table on this fit)")
  }
  # Each view gives its predictor's rows the names the mean's term
  # extractors match on. Its tag marks each symbol as in the equations.
  sub <- !mean_predictor_rows(prior)
  on_trend <- grepl("_trend$", prior$class)
  symbols <- trend_symbols(obj)
  views <- c(sub_predictor_views(obj), list(trend_side_view(obj)))
  rows <- c(
    prior_align_rows(obj, prior[!on_trend & !sub, , drop = FALSE], symbols),
    unlist(lapply(views, function(v) prior_align_rows(v, v$prior, symbols)),
           recursive = FALSE)
  )
  if (length(rows) == 0L) {
    return("## Priors\n\n(all parameters are improper flat)")
  }
  text <- backfill_umbrella_priors(prior)$prior
  flat <- any(is.na(text) | !nzchar(text) | text == "(flat)")
  paste(c(
    "## Priors", "", align_block(rows),
    if (flat) {
      c("", paste("Every other parameter takes an improper flat",
                  "prior on its support."))
    }
  ), collapse = "\n")
}

#' One aligned row per proper prior in one predictor's table
#'
#' brms lists a class's prior on one row and each coefficient of the
#' class on a row of its own. The page states each coefficient's prior
#' under the symbol the equations give it. A class with no prior is
#' improper flat and has no row. Rows run in the order brms's defaults,
#' the user's priors, then the priors mvgam writes.
#'
#' @param obj The `mvgam` object, or a view of it, whose formula owns
#'   the table's terms.
#' @param prior Rows of its prior table.
#' @param symbols The model's `trend_symbols()`.
#' @return A list of `align_block()` rows, possibly empty.
#' @noRd
prior_align_rows <- function(obj, prior, symbols) {
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  prior <- label_prior_terms(obj, backfill_umbrella_priors(prior))
  prior <- prior[!is.na(prior$prior) & nzchar(prior$prior), , drop = FALSE]
  prior <- cor_rows_by_response(obj, drop_shadowed_priors(prior))
  if (nrow(prior) == 0L) return(list())
  src <- prior_field(prior, "source")
  prior <- prior[order(match(src, c("default", "(vectorized)", "user",
                                    "mvgam"), nomatch = 99L)), ,
                 drop = FALSE]
  # The MGP shapes are Stan data. The glossary gives their values.
  dist_symbols <- c(symbols, mgp_a1 = "a_1", mgp_a2 = "a_2")
  rows <- lapply(seq_len(nrow(prior)), function(i) {
    row <- prior[i, , drop = FALSE]
    # The rows mvgam writes keep their `_trend` names and symbols. The
    # trend view tags the brms rows it renders.
    sym <- if (row$class %in% names(symbols)) {
      symbols[[row$class]]
    } else if (grepl("_trend$", row$class)) {
      format_parameter_symbol(row)
    } else {
      response_symbol(obj, format_parameter_symbol(row))
    }
    list(lhs = sym, op = "\\sim",
         rhs = format_prior_distribution(row$prior, dist_symbols))
  })
  # A covariate's coefficients on each factor share one symbol
  rows[!duplicated(vapply(rows, function(r) paste(r$lhs, r$rhs), ""))]
}

#' Tag each shared correlation prior with the response it covers
#'
#' brms writes the prior of a group's correlation matrix with no
#' response. In a multivariate model each response correlating varying
#' effects of the group has its own matrix. A row covering one
#' response takes that response, as its matrix does in the equations.
#' A row covering several stays untagged, as brms applies it to each
#' matrix.
#'
#' @param obj An `mvgam` object
#' @param prior Rows of its prior table
#' @return `prior`, with the response of each such row filled in
#' @noRd
cor_rows_by_response <- function(obj, prior) {
  views <- response_views(obj)
  shared <- prior$class == "L" & nzchar(prior$group) &
    !nzchar(prior_field(prior, "resp"))
  if (length(views) < 2L || !any(shared)) return(prior)
  for (i in which(shared)) {
    owners <- Filter(function(v) {
      any(vapply(obs_re_specs_from_prior(v$prior), function(s) {
        s$has_slope && identical(s$group, prior$group[[i]])
      }, logical(1L)))
    }, views)
    if (length(owners) == 1L) prior$resp[[i]] <- owners[[1L]]$methods_md_resp
  }
  prior
}

#' A prior row's scope: its class and the predictor it belongs to
#'
#' @param prior A brms prior table
#' @return Character key per row
#' @noRd
prior_scope <- function(prior) {
  paste(prior$class, prior_field(prior, "dpar"), prior_field(prior, "nlpar"),
        prior_field(prior, "resp"), sep = "\x1f")
}

#' Copy each class's prior onto the coefficients that inherit it
#'
#' brms gives a class's prior on a row with no `coef` and no `group`.
#' Each coefficient that inherits it has a row with no prior. A
#' coefficient inherits from the class row of its own predictor.
#'
#' @param prior A brms prior table
#' @return `prior`, with each inherited prior filled in
#' @noRd
backfill_umbrella_priors <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(prior)
  text <- prior_field(prior, "prior")
  has_text <- !is.na(text) & nzchar(text) & text != "(flat)"
  umbrella <- has_text & !nzchar(prior_field(prior, "coef")) &
    !nzchar(prior_field(prior, "group"))
  scope <- prior_scope(prior)
  source <- match(scope, scope[umbrella])
  fill <- !has_text & !is.na(source)
  prior$prior[fill] <- text[umbrella][source[fill]]
  prior
}

#' Name each prior row's coefficient as the equations name it
#'
#' Each `b` row of the mean learns the kind of term that owns it. A
#' smooth's unpenalised columns, such as `sx_1`, take the smooth's
#' prior through its `sds` row. Their own rows are dropped. brms names
#' a measurement-error effect `me<x><sdx>`, which the page shortens to
#' `me<x>`. A population effect and a GP's hyperparameters take the
#' symbols the equations give them in a `tex` column. brms names a GP's
#' hyperparameters by term, `gpxhabA` for level `A` of
#' `gp(x, by = hab)`.
#'
#' @param obj The `mvgam` object, or a view of it
#' @param prior Rows of its prior table
#' @return `prior` with `term_kind` and `tex` columns
#' @noRd
label_prior_terms <- function(obj, prior) {
  prior$term_kind <- NA_character_
  prior$tex <- NA_character_
  obs_b <- prior$class == "b" & mean_predictor_rows(prior)
  prior$term_kind[obs_b] <- obs_term_coef_kinds(obj, prior$coef[obs_b])
  prior <- prior[!prior$term_kind %in% "sm", , drop = FALSE]
  for (s in obs_me_specs(obj)) {
    if (is.na(s$sdvar)) next
    hit <- prior$class == "b" & prior$coef == paste0("me", s$var, s$sdvar)
    prior$coef[hit] <- paste0("me", s$var)
  }
  fixed <- prior$class == "b" & mean_predictor_rows(prior) &
    nzchar(prior$coef) & is.na(prior$term_kind)
  for (spec in fixed_term_specs(obj, unique(prior$coef[fixed]))) {
    prior$tex[fixed & prior$coef %in% spec$coefs] <-
      paste0("\\beta_{", spec$sub, "}")
  }
  for (sp in obs_gp_specs(obj)) {
    base <- paste0("gp", brms_rename(paste(sp$vars, collapse = "")),
                   if (!is.na(sp$by)) brms_rename(sp$by))
    hit <- prior$class %in% c("lscale", "sdgp") &
      startsWith(prior$coef, base)
    level <- substring(prior$coef[hit], nchar(base) + 1L)
    sym <- term_symbols("gp", sp)
    # Each level of a `by` factor has its own hyperparameters
    prior$tex[hit] <- paste0(
      sub("\\}$", "", sym[ifelse(prior$class[hit] == "sdgp", "sd", "rho")]),
      ifelse(nzchar(level), paste0(" = ", escape_math_text(level)), ""), "}"
    )
  }
  prior
}

#' Drop the prior rows another row states more precisely
#'
#' A class row repeats the prior of the coefficient rows in its scope,
#' under a placeholder symbol. A group's `sd` row and its coefficient
#' rows describe the same SDs. A group with only an intercept SD needs
#' no superscript to tell its SDs apart. Its row takes the group's
#' plain symbol.
#'
#' @param prior A brms prior table
#' @return `prior` without the repeated rows
#' @noRd
drop_shadowed_priors <- function(prior) {
  vec_classes <- c(
    "Intercept", "b", "sd", "sds", "L", "cor",
    "sdgp", "lscale", "meanme", "sdme", "simo"
  )
  scope <- prior_scope(prior)
  general <- !nzchar(prior$coef) & !nzchar(prior$group)
  vec <- prior$class %in% vec_classes
  prior <- prior[!(vec & general & scope %in% scope[vec & !general]), ,
                 drop = FALSE]
  group_sd <- function(p) {
    list(key = paste(prior_scope(p), p$group, sep = "\x1f"),
         is_sd = p$class == "sd" & nzchar(p$group))
  }
  g <- group_sd(prior)
  per_coef <- g$is_sd & nzchar(prior$coef)
  prior <- prior[!(g$is_sd & !per_coef & g$key %in% g$key[per_coef]), ,
                 drop = FALSE]
  g <- group_sd(prior)
  lone <- g$is_sd & prior$coef == "Intercept" &
    !g$key %in% g$key[duplicated(g$key)]
  prior$coef[lone] <- ""
  prior
}

#' The Implementation section: the call that refits the model
#'
#' The call names each argument as `mvgam()` takes it. Sampler control
#' appears only when the fit moved it from Stan's defaults of
#' `adapt_delta = 0.8` and `max_treedepth = 10`. The fenced block has
#' no language tag, which Pandoc renders as plain verbatim.
#'
#' @param obj An `mvgam` object
#' @return Markdown string
#' @noRd
render_implementation_section <- function(obj) {
  fn <- if (inherits(obj, "jsdgam")) "jsdgam" else "mvgam"
  info <- extract_implementation_info(obj)
  data_name <- obj$data.name
  # A fit built by `do.call()` records its data as the dots name `..3`
  if (is.null(data_name) || !nzchar(data_name) ||
        identical(data_name, "<inline data>") ||
        grepl("^\\.\\.[0-9]+$", data_name)) {
    data_name <- "data"
  }
  quoted <- function(x) paste0("\"", x, "\"")
  control <- c(
    if (!is.na(info$adapt_delta) && info$adapt_delta != 0.8) {
      paste0("    adapt_delta = ", info$adapt_delta)
    },
    if (!is.na(info$max_treedepth) && info$max_treedepth != 10L) {
      paste0("    max_treedepth = ", info$max_treedepth)
    }
  )
  model_args <- if (identical(fn, "jsdgam")) {
    jsdgam_call_args(obj)
  } else {
    c(formula = formula_text(obj$formula),
      trend_formula = trend_formula_text(obj))
  }
  args <- c(
    model_args,
    data = data_name,
    # A response of a multivariate formula may name its family in its
    # own `bf()`. The `family` argument covers every other response.
    family = if (!all(vapply(obj$formula$forms %||% list(obj$formula),
                             function(f) !is.null(f$family),
                             logical(1L)))) {
      family_call_text(obj$family)
    },
    backend = quoted(info$backend),
    chains = if (!is.na(info$chains)) info$chains,
    iter = if (!is.na(info$iter)) info$iter,
    warmup = if (!is.na(info$warmup)) info$warmup,
    threads = if (!is.na(info$threads) && info$threads > 1L) info$threads,
    control = if (length(control)) {
      paste0("list(\n", paste(control, collapse = ",\n"), "\n  )")
    },
    init = if (!is.na(info$init) && nzchar(info$init)) quoted(info$init),
    algorithm = if (!info$algorithm %in% c("sampling", "none")) {
      quoted(info$algorithm)
    }
  )
  call_lines <- paste0(
    "  ", formatC(names(args), width = -13L), " = ", args,
    c(rep(",", length(args) - 1L), "")
  )
  paste(c(
    "## Implementation", "", implementation_prose(fn, info), "",
    "```", paste0(fn, "("), call_lines, ")", "```"
  ), collapse = "\n")
}

#' The software, algorithm and sampler settings a fit recorded
#'
#' A prefit records `algorithm = "none"`. The versions are those the
#' fit was built with, which may differ from those installed.
#'
#' @param obj An `mvgam` object
#' @return Named list
#' @noRd
extract_implementation_info <- function(obj) {
  backend <- obj$backend
  algorithm <- obj$algorithm
  checkmate::assert_choice(backend, backend_choices(),
                           .var.name = "obj$backend")
  checkmate::assert_choice(algorithm, c(algorithm_choices(), "none"),
                           .var.name = "obj$algorithm")
  # `extract_sampling_info()` returns (chains, warmup, iter, threads,
  # adapt_delta, max_treedepth, init) for a NUTS fit and NULL for any
  # other algorithm.
  sampling <- extract_sampling_info(obj)
  if (is.null(sampling)) {
    sampling <- list(
      chains = NA_integer_, warmup = NA_integer_,
      iter = NA_integer_, threads = NA_integer_,
      adapt_delta = NA_real_, max_treedepth = NA_integer_,
      init = NA_character_
    )
  }
  # The start the user wrote replays on any machine. For list-valued
  # and Pathfinder starts Stan records the path of a file it wrote for
  # the run.
  if (!is.null(obj$init)) {
    sampling$init <- printable_init(obj$init)
  }
  recorded <- function(v) {
    if (is.null(v) || is.na(v)) "(version not recorded)" else format(v)
  }
  c(
    list(
      backend   = backend,
      algorithm = algorithm,
      mvgam_v   = recorded(obj$mvgam_version),
      brms_v    = recorded(obj$brms_version),
      stan_v    = recorded(obj$stan_version)
    ),
    sampling
  )
}

#' The sentence naming the software and sampler of a fit
#'
#' @param fn `"mvgam"` or `"jsdgam"`
#' @param info List from `extract_implementation_info()`
#' @return Character string
#' @noRd
implementation_prose <- function(fn, info) {
  algo_text <- switch(
    info$algorithm,
    "sampling"  = "Hamiltonian Monte Carlo",
    "meanfield" = "Stan's mean-field ADVI",
    "fullrank"  = "Stan's full-rank ADVI",
    "laplace"   = "Stan's Laplace approximation",
    "pathfinder" = "Stan's Pathfinder",
    "none"      = NULL,
    info$algorithm
  )
  interface <- if (identical(info$backend, "cmdstanr")) "cmdstanr" else "rstan"
  stan_lib <- paste0("Stan ", info$stan_v, " via ", interface)
  base <- paste0(
    "Fitted with mvgam ", info$mvgam_v,
    " (brms ", info$brms_v, "; ", stan_lib, ")."
  )
  if (is.null(algo_text)) return(base)
  if (!is.na(info$chains) && !is.na(info$iter) &&
      !is.na(info$warmup)) {
    paste0(
      base, " ", algo_text, ": ",
      info$chains, " chains, ", info$warmup,
      " warmup + ", info$iter - info$warmup,
      " sampling iterations per chain."
    )
  } else {
    paste0(base, " Posterior drawn via ", algo_text, ".")
  }
}

#' The model arguments of the `jsdgam()` call that refits a fit
#'
#' `jsdgam()` builds its trend from `factor_formula` and `n_lv`. Its
#' `trait_slopes` rewrites `formula`. The call gives the arguments the
#' user passed. A setting equal to its default in `jsdgam()` is left
#' out. The traits, phylogeny and loadings prior are data objects. The
#' call names each by the expression the user wrote. A fit built by
#' `do.call()` holds the object itself, which the call names by its
#' argument, as it names the data.
#'
#' @param obj A `jsdgam` object
#' @return Named character vector of argument texts
#' @noRd
jsdgam_call_args <- function(obj) {
  args <- obj$jsdgam_args
  defaults <- formals(jsdgam)
  # `unit` and `species` are stored as column names
  differs <- function(name, text) {
    if (!identical(text, deparse1(defaults[[name]]))) text
  }
  written <- function(name) {
    expr <- obj$call[[name]]
    if (is.null(expr)) return(NULL)
    if (is.language(expr)) deparse1(expr) else name
  }
  c(
    formula = formula_text(args$formula),
    factor_formula = differs("factor_formula",
                             formula_text(args$factor_formula)),
    unit = differs("unit", args$unit),
    species = differs("species", args$species),
    n_lv = differs("n_lv", paste0(as.integer(args$n_lv), "L")),
    traits = written("traits"),
    trait_slopes = written("trait_slopes"),
    phylo = written("phylo"),
    loadings_prior = written("loadings_prior")
  )
}

#' A formula as the call to refit the model writes it
#'
#' A `brmsformula` prints as the `bf()` call that builds it, with its
#' distributional formulas, its fixed parameters such as
#' `quantile = 0.25`, `nl = TRUE` and a response's own family. A
#' multivariate formula joins one `bf()` per response and its residual
#' correlation setting. The call omits the placeholder term mvgam adds
#' to an empty formula.
#'
#' @param f A formula, `brmsformula`, `mvbrmsformula` or `NULL`
#' @return Character string
#' @noRd
formula_text <- function(f) {
  if (is.null(f)) return("NULL")
  if (inherits(f, "mvbrmsformula")) {
    out <- paste(vapply(f$forms, formula_text, character(1L)),
                 collapse = " + ")
    if (isTRUE(f$rescor) || isFALSE(f$rescor)) {
      out <- paste0(out, " + set_rescor(", f$rescor, ")")
    }
    return(out)
  }
  deparsed <- function(x) {
    strip_empty_obs_placeholder(deparse1(x, collapse = " "))
  }
  if (!inherits(f, c("brmsformula", "bform"))) return(deparsed(f))
  fixed <- f$pfix %||% list()
  parts <- c(
    deparsed(f$formula %||% f),
    vapply(f$pforms %||% list(), deparsed, character(1L),
           USE.NAMES = FALSE),
    if (length(fixed)) {
      paste0(names(fixed), " = ", vapply(fixed, deparse1, character(1L)))
    },
    # brms records `nl = TRUE` as an attribute of the formula
    if (isTRUE(attr(f$formula, "nl"))) "nl = TRUE",
    if (!is.null(f$family)) paste0("family = ", family_call_text(f$family))
  )
  paste0("bf(", paste(parts, collapse = ", "), ")")
}

#' The trend formula as the call to refit the model writes it
#'
#' @param obj An `mvgam` object
#' @return Character string, or `NULL` without a trend formula
#' @noRd
trend_formula_text <- function(obj) {
  if (is.null(obj$trend_call)) return(NULL)
  formula_text(obj$trend_call)
}

#' A family as the call that builds it
#'
#' The call names each setting that differs from the constructor's
#' default. A brms family's settings are its links. brms names these
#' families in lower case, but their constructors are `Gamma()` and
#' `Beta()`. The mean's default link is the constructor's own:
#' `stats::Gamma()` takes the inverse link, where brms takes the log.
#' A constructor from `stats` takes no link of a distributional
#' parameter. A family that sets one is written as `brmsfamily()`.
#'
#' @param family A `family` object
#' @return Character string, such as `"poisson()"`
#' @noRd
family_call_text <- function(family) {
  checkmate::assert_class(family, "family")
  fam <- resolve_family_name(family)
  name <- if (startsWith(fam, "nmix")) "nmix" else fam
  mvgam_ctor <- mvgam_family_constructors()[[name]]
  if (!is.null(mvgam_ctor)) {
    return(deparse1(as.call(c(as.name(name),
                              mvgam_family_settings(family, mvgam_ctor)))))
  }
  name <- switch(fam, gamma = "Gamma", beta = "Beta", fam)
  ctor <- get0(name, envir = asNamespace("brms"), mode = "function",
               inherits = FALSE) %||%
    get(name, envir = asNamespace("stats"), mode = "function")
  links <- grep("^link(_|$)", names(family), value = TRUE)
  links <- links[vapply(family[links], is.character, logical(1L))]
  defaults <- brms::brmsfamily(fam)[links]
  defaults$link <- ctor()$link
  changed <- links[!mapply(identical, family[links], defaults[links])]
  if (all(changed %in% names(formals(ctor)))) {
    return(deparse1(as.call(c(as.name(name), family[changed]))))
  }
  defaults$link <- brms::brmsfamily(fam)$link
  changed <- links[!mapply(identical, family[links], defaults[links])]
  deparse1(as.call(c(as.name("brmsfamily"), fam, family[changed])))
}

#' The settings of an mvgam family that differ from its defaults
#'
#' Each family records its settings as it was built: the Tweedie series
#' truncation `M`, the N-mixture `type`, `multi_season` and the link of
#' `com_binomial()`. A setting with a choice of values defaults to its
#' first choice.
#'
#' @param family A family built by one of `mvgam_family_constructors()`
#' @param ctor Its constructor
#' @return Named list of settings
#' @noRd
mvgam_family_settings <- function(family, ctor) {
  defaults <- formals(ctor)
  settings <- list(
    M = attr(family, "mvgam_stanvars")$M$sdata,
    type = attr(family, "mvgam_nmix_type"),
    multi_season = is_multi_season_family(family),
    link = family$link
  )
  settings <- settings[intersect(names(settings), names(defaults))]
  Filter(Negate(is.null), Map(function(value, name) {
    if (!identical(value, eval(defaults[[name]])[1L])) value
  }, settings, names(settings)))
}

#' The symbol of the parameter a prior row describes
#'
#' The symbol of a row scoped to one response of a multivariate model
#' takes the response as a superscript tag.
#'
#' @param row One row of a brms prior table
#' @return Character LaTeX symbol
#' @noRd
format_parameter_symbol <- function(row) {
  sym <- format_parameter_symbol_base(row)
  resp <- row$resp %||% ""
  if (nzchar(resp)) apply_resp_superscript(sym, escape_math_text(resp)) else {
    sym
  }
}

#' A symbol with a tag added to its superscript
#'
#' A symbol with a superscript such as `\beta^{(\text{mo})}_{x}` takes
#' the tag inside it, as `\beta^{(\text{mo}, y1)}_{x}`. A symbol with
#' only a subscript takes the superscript before it. A plain symbol
#' takes it after. No symbol gains a second superscript.
#'
#' @param sym Character LaTeX symbol
#' @param r Character tag, such as a response key or `"\\eta"`
#' @return Character LaTeX symbol
#' @noRd
apply_resp_superscript <- function(sym, r) {
  super_pat <- "\\^\\{\\(([^)]+)\\)\\}"
  # `sub()` treats a backslash in its replacement as an escape. The
  # doubled copy keeps a tag such as `\\eta` intact.
  r_sub <- gsub("\\\\", "\\\\\\\\", r)
  if (grepl(super_pat, sym)) {
    return(sub(super_pat, paste0("^{(\\1, ", r_sub, ")}"), sym))
  }
  if (grepl("_\\{", sym)) {
    return(sub("_\\{", paste0("^{(", r_sub, ")}_{"), sym))
  }
  paste0(sym, "^{(", r, ")}")
}

#' The symbol of a prior row's parameter, as the equations name it
#'
#' The class decides the symbol. Its `coef` and `group` decide the
#' indices. A class with no symbol of its own is named upright, as
#' written.
#'
#' @param row One row of a brms prior table
#' @return Character LaTeX symbol
#' @noRd
format_parameter_symbol_base <- function(row) {
  # `label_prior_terms()` names a term's parameters from its spec
  tex <- row$tex %||% NA_character_
  if (!is.na(tex)) return(tex)
  cls <- row$class %||% ""
  coef <- row$coef %||% ""
  group <- row$group %||% ""
  if (identical(cls, "Intercept")) {
    # An ordinal family's thresholds are `Intercept` rows numbered in
    # `coef`.
    if (grepl("^[0-9]+$", coef)) return(paste0("\\theta_{", coef, "}"))
    return("\\alpha")
  }
  if (identical(cls, "b")) {
    if (nzchar(coef)) {
      kind <- row$term_kind %||% NA_character_
      if (kind %in% c("mo", "me")) {
        # `label_prior_terms()` names a measurement-error effect
        # `me<x>`.
        spec <- list(var = sub("^m[oe]", "", coef))
        return(term_symbols(kind, spec)[["beta"]])
      }
      return(paste0("\\beta_{", escape_math_text(coef), "}"))
    }
    return("\\boldsymbol{\\beta}")
  }
  if (identical(cls, "simo")) {
    bare <- if (nzchar(coef)) {
      sub("[0-9]+$", "", sub("^mo", "", coef))
    } else "j"
    return(term_symbols("mo", list(var = bare))[["zeta"]])
  }
  if (cls %in% c("meanme", "sdme")) {
    bare <- if (nzchar(coef)) sub("^me", "", coef) else "j"
    sym <- term_symbols("me", list(var = bare))
    return(if (identical(cls, "meanme")) sym[["mu"]] else sym[["sd"]])
  }
  if (cls %in% setdiff(names(family_par_symbols()), "mu")) {
    return(family_par_symbol(cls))
  }
  if (cls %in% c("sd", "L", "cor")) {
    grp <- if (nzchar(group)) group else "j"
    # Intercept and slope SDs of one group are told apart by the
    # coefficient they scale. brms places a correlation prior on the
    # Cholesky factor of the correlation matrix the equations name.
    sym <- term_symbols("re", list(group = grp, slopes = coef))
    key <- if (!identical(cls, "sd")) {
      "Omega"
    } else if (!nzchar(coef)) {
      "sd"
    } else if (identical(coef, "Intercept")) {
      "sd_alpha"
    } else {
      paste0("sd_slope:", coef)
    }
    return(sym[[key]])
  }
  if (identical(cls, "sds")) {
    spec <- if (nzchar(coef)) parse_smooth_coef(coef) else list(vars = "j")
    return(term_symbols("smooth", spec)[["sd"]])
  }
  if (cls %in% c("sdgp", "lscale")) {
    bare <- if (nzchar(coef)) sub("^gp", "", coef) else "j"
    sym <- term_symbols("gp", list(vars = bare, by = NA))
    return(if (identical(cls, "sdgp")) sym[["sd"]] else sym[["rho"]])
  }
  if (identical(cls, "Lrescor")) {
    return("\\boldsymbol{\\Omega}")
  }
  if (identical(cls, "Z_free_vec")) {
    return("\\mathbf{Z}_{\\text{free}}")
  }
  if (identical(cls, "theta_features")) {
    return("\\boldsymbol{\\theta}_{\\text{features}}")
  }
  if (grepl("^theta_dist_", cls)) {
    nm <- escape_math_text(sub("^theta_dist_", "", cls))
    return(paste0("\\theta_{", nm, "}"))
  }
  if (identical(cls, "varrho_inv")) {
    # `varrho_inv[1]` and `varrho_inv[2:N_lv_trend]`: the first
    # increment and every later one.
    idx <- if (identical(coef, "1")) "1" else "h \\ge 2"
    return(paste0("\\varrho_{", idx, "}"))
  }
  if (identical(cls, "Z")) return("Z_{i,k}")
  if (identical(cls, "Psi")) {
    return("\\boldsymbol{\\Psi}")
  }
  # Trend classes `trend_symbols()` does not name, such as
  # Intercept_trend and phi_trend.
  if (grepl("_trend$", cls)) {
    if (identical(cls, "Intercept_trend")) return("\\alpha^{(\\eta)}")
    if (identical(cls, "phi_trend"))   return("\\phi_\\eta")
    if (identical(cls, "L_Omega_trend")) return("\\boldsymbol{\\Omega}")
    pw <- c(k_trend = "k_i", m_trend = "m_i",
            delta_trend = "\\boldsymbol{\\delta}_i")
    if (cls %in% names(pw)) return(pw[[cls]])
    # The VAR and VARMA hyperpriors: means and precisions of the
    # entries of the unconstrained coefficient matrices.
    hyper <- c(Amu_trend = "\\mu^{(A)}", Aomega_trend = "\\omega^{(A)}",
               Dmu_trend = "\\mu^{(D)}", Domega_trend = "\\omega^{(D)}")
    if (cls %in% names(hyper)) return(hyper[[cls]])
    bare <- sub("_trend$", "", cls)
    return(paste0(math_upright(bare), "_\\eta"))
  }
  math_upright(cls)
}

#' A prior's distribution as the equations write it
#'
#' Each Stan distribution takes its conventional name. brms writes a
#' default location computed with an offset at full precision. Three
#' significant digits describe it on the page. An argument naming a
#' sampled hyperparameter takes its symbol. Any other literal `_` is
#' escaped, as a bare `mgp_a1` would set `a1` as a subscript. A
#' distribution with no name of its own is set verbatim as text.
#'
#' @param prior_str Character brms prior, such as `"normal(0, 1)"`
#' @param symbols Named character vector of hyperparameter symbols
#' @return Character LaTeX expression
#' @noRd
format_prior_distribution <- function(prior_str, symbols = NULL) {
  checkmate::assert_string(prior_str)
  s <- trimws(prior_str)
  if (!nzchar(s)) return("\\text{flat}")
  if (identical(s, "(flat)")) return("\\text{flat}")

  # Stan's name for each distribution and the name the page gives it.
  labels <- c(
    normal = "Normal", std_normal = "Normal", student_t = "StudentT",
    lognormal = "LogNormal", exponential = "Exponential",
    gamma = "Gamma", inv_gamma = "InvGamma", cauchy = "Cauchy",
    beta = "Beta", uniform = "Uniform", logistic = "Logistic",
    double_exponential = "Laplace", weibull = "Weibull",
    lkj_corr_cholesky = "LKJCorr", lkj = "LKJCorr",
    lkj_corr = "LKJCorr", dirichlet = "Dirichlet",
    constant = "Constant"
  )
  m <- regmatches(s, regexec("^([a-z_]+)\\((.*)\\)$", s))[[1L]]
  if (length(m) == 3L && m[[2L]] %in% names(labels)) {
    args <- trimws(strsplit(m[[3L]], "\\s*,\\s*")[[1L]])
    if (identical(m[[2L]], "std_normal")) args <- c("0", "1")
    numeric_arg <- grepl("^-?[0-9]*\\.?[0-9]+(e-?[0-9]+)?$", args)
    args[numeric_arg] <- vapply(as.numeric(args[numeric_arg]), function(v) {
      format(signif(v, 3L), drop0trailing = TRUE)
    }, character(1L))
    args <- vapply(args, function(a) {
      if (a %in% names(symbols)) symbols[[a]] else escape_math_text(a)
    }, character(1L))
    args <- gsub("sqrt\\(([^()]*)\\)", "\\\\sqrt{\\1}", args)
    return(paste0(
      "\\text{", labels[[m[[2L]]]], "}(", paste(args, collapse = ", "), ")"
    ))
  }
  paste0("\\text{", gsub("([{}_])", "\\\\\\1", s), "}")
}
