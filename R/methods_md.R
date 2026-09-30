#' Render a model's mathematical description
#'
#' Produce a markdown document describing the fitted `mvgam` or
#' `jsdgam` model in mathematical form. Three sections are
#' emitted: Data (dimensions and response), Model (likelihood,
#' linear predictor, latent dynamics, all in one `align*` block),
#' and Priors (every prior the Stan model uses, brms-tracked and
#' mvgam-emitted alike, in a second `align*` block). No prose
#' justification is included; the output is a math-only spec the
#' user can paste straight into a methods write-up.
#'
#' @param object A fitted `mvgam` / `jsdgam` object, or a prefit
#'   produced by `mvgam(..., run_model = FALSE)`.
#' @param file Optional path to write the rendered markdown to.
#'   When `NULL` (default), the result is returned invisibly and
#'   printed if the caller is interactive.
#' @param notation Character; symbol convention to use. One of
#'   `"default"` (Greek coefficients, `\eqn{R[i]}` subscript for
#'   varying intercepts) or `"brms"` (keep brms parameter names
#'   verbatim). Defaults to `"default"`.
#' @param implementation Logical; append an Implementation
#'   section that reconstructs the `mvgam` / `jsdgam` call used
#'   to fit the model. Defaults to `TRUE`.
#' @param heading_level Integer; the Markdown level of the section
#'   headings. The default `2` writes `## Model`. Raise it to nest the
#'   sections under a heading of the document they are placed in.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class `mvgam_methods_md` carrying the
#'   rendered markdown as a character string. Use `print()` to
#'   display, or pass to `writeLines()` / `cat()` to emit verbatim.
#'
#' @author Nicholas J Clark
#' @seealso \code{\link{how_to_cite}}, \code{\link[brms]{prior_summary}},
#'   \code{\link{mvgam}}, \code{\link{jsdgam}}.
#'   `how_to_cite()` produces the prose methods paragraph;
#'   `methods_md()` produces the matching math statement of
#'   the model.
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
#' # Markdown / LaTeX statement of the model. cat() emits the
#' # raw markdown; print() pretty-prints in the console.
#' cat(methods_md(mod))
#' }
#'
#' @export
methods_md <- function(object, file = NULL, notation = "default",
                       implementation = TRUE, ...) {
  UseMethod("methods_md", object)
}

#' @rdname methods_md
#' @export
methods_md.mvgam <- function(object, file = NULL,
                             notation = "default",
                             implementation = TRUE,
                             heading_level = 2L, ...) {
  checkmate::assert_class(object, "mvgam")
  # Every section renders the model's family and link from here. A
  # fit and a prefit both store the family `mvgam()` validated.
  checkmate::assert_class(object$family, "family",
                          .var.name = "object$family")
  checkmate::assert_string(file, null.ok = TRUE)
  checkmate::assert_choice(notation, c("default", "brms"))
  checkmate::assert_flag(implementation)
  checkmate::assert_int(heading_level, lower = 1L, upper = 6L)
  rlang::check_dots_empty()

  sections <- methods_md_section_registry(
    implementation = implementation
  )
  ctx <- list(object = object, notation = notation)
  parts <- vapply(
    names(sections),
    function(nm) sections[[nm]](ctx),
    character(1L)
  )
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


# ---------------------------------------------------------------
# Section registry
# ---------------------------------------------------------------
# Each entry receives the rendering context list (object +
# notation) and returns the section as a single markdown string.
# New top-level sections register by adding a named entry here.

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


# ---------------------------------------------------------------
# align* block helper
# ---------------------------------------------------------------
# All math output is a single align* per section. Each row in
# `rows` is one statement of the form (lhs, op, rhs) where op is
# typically "\\sim" or "=". The composer wraps the rows in a
# fenced LaTeX block so Pandoc / Quarto renders them as display
# math.

#' @noRd
align_block <- function(rows) {
  if (length(rows) == 0L) return(character(0L))
  # Single `$$\begin{aligned}...\end{aligned}$$` block. Pandoc /
  # Quarto / RStudio / MathJax / KaTeX all render this as a
  # centered, tightly stacked display equation. `\\` ends each
  # row, `&` aligns at the relation operator. Plain-text fallback
  # is degraded (raw LaTeX commands show through) but every other
  # surface gets the canonical methods-section look.
  body <- vapply(rows, function(r) {
    paste0(r$lhs, " &", r$op, " ", r$rhs)
  }, character(1L))
  body[-length(body)] <- paste0(body[-length(body)], " \\\\")
  c("$$", "\\begin{aligned}", body, "\\end{aligned}", "$$")
}


# ---------------------------------------------------------------
# Data section
# ---------------------------------------------------------------
# Plain markdown (no math block): list the response variable, its
# data type, the dimensions of the design (N observations, S
# series, T time points).

#' @noRd
render_data_section <- function(ctx) {
  obj <- ctx$object
  fam_name <- resolve_family_name(obj$family)
  dims <- describe_data_dimensions(obj)

  # Multi-response: list response columns explicitly so the
  # reader sees the mvbind structure (`yA`, `yB`) rather than
  # an opaque bold `Y` vector label, each described by its own
  # family.
  responses <- unname(response_columns(obj))
  resp_line <- if (length(responses) > 1L) {
    labels <- vapply(model_families(obj), function(f) {
      family_data_label(resolve_family_name(f))
    }, character(1L))
    responses <- vapply(responses, escape_math_text, character(1L),
                        USE.NAMES = FALSE)
    paste0(
      "$\\mathbf{Y} = (",
      paste0(responses, collapse = ", "),
      ")$, observed jointly per (i, t): ",
      paste0("$", responses, "$ ", labels, collapse = "; ")
    )
  } else {
    paste0("$", response_letter(obj),
            "$: ", family_data_label(fam_name))
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

#' @noRd
describe_predictors <- function(obj) {
  data <- obj$data %||% data.frame()
  if (nrow(data) == 0L) return(character(0L))
  resp_cols <- unname(response_columns(obj))
  # Restrict to the terms that enter a linear predictor. Without
  # this guard every column of the user's frame is described, and a
  # denominator, an offset and a grouping factor each name a column
  # no reader would take to be a covariate.
  used_vars <- mvgam_term_list(obj)$conditional
  # The panel keys and the response columns are covered by the
  # dimensions line above.
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
        paste(head(levels(col), 4L), collapse = ", "),
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

#' @noRd
formula_used_vars <- function(obj) {
  # Every variable referenced anywhere in the model spec: both
  # submodels, the distributional and non-linear sub-formulas beneath
  # them, the responses, and the addition terms beside the response.
  # That list is what `find_variables()` answers with, and reading it
  # here keeps one account of what the model names.
  insight::find_variables(obj, flatten = TRUE)
}

#' @noRd
describe_data_dimensions <- function(obj) {
  n_obs <- nrow(obj$data %||% data.frame())
  # `print()` shows the same counts, taken from the axes the model
  # resolved. A response-keyed frame has no series column to count.
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

#' @noRd
closure_unit_data_dimensions <- function(obj) {
  # Only the detection families (occ / nmix variants) have a
  # per-visit / per-unit grain that needs the G + J-bar callout
  # in the data section. The mv-custom families (mvn / mvt / diri
  # / multi / categ) also carry `mvgam_closure_unit = TRUE` but
  # each unit holds one K-vector observation, so the "visits per
  # unit" framing does not apply.
  if (!is_closure_unit_family(obj$family)) return(character(0L))
  data <- obj$data %||% data.frame()
  if (nrow(data) == 0L) return(character(0L))
  # The columns that key a closure unit, default included. The
  # guard above establishes that this family has one, and the
  # accessor returns its keys. Counting units over the keys the frame
  # holds reports fewer units than the model has. A frame missing a
  # key is refused.
  ug <- fit_closure_unit_keys(obj)
  absent <- setdiff(ug, names(data))
  if (length(absent)) {
    stop(insight::format_error(c(
      "The stored data lacks a column that keys the closure units.",
      x = paste0(
        "Missing: ", paste0("'", absent, "'", collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  units_df <- unique(data[, ug, drop = FALSE])
  n_unit <- nrow(units_df)
  # Visits per unit -- count rows per unit-key combination.
  unit_key <- do.call(paste, c(data[, ug, drop = FALSE], sep = "\v"))
  visits <- tabulate(match(unit_key, unique(unit_key)))
  mean_visits <- round(mean(visits), 2L)
  parts <- c(
    paste0("$G = ", n_unit, "$ closure units"),
    paste0("$\\bar J = ", mean_visits, "$ visits per unit ",
           "(range ", min(visits), "-", max(visits), ")")
  )
  parts
}

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
      "$p_{\\text{features}} = ", ls$N_features_trend,
      "$ trait features"
    ))
  }
  if (traits$distances) {
    parts <- c(parts, paste0(
      "$p_{\\text{distances}} = ", ls$n_distances,
      "$ distance matrices"
    ))
  }
  parts
}

#' @noRd
family_data_label <- function(fam_name) {
  switch(
    fam_name,
    poisson = "non-negative integer counts",
    binomial = "binomial counts",
    bernoulli = "binary observations",
    gaussian = "real-valued observations",
    student = "real-valued observations",
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
    occ = "per-visit binary detections (single-season occupancy)",
    nmix = paste0(
      "per-visit binomial counts (N-mixture, ",
      "Poisson abundance + binomial detection)"
    ),
    nmix_poisson_binomial = paste0(
      "per-visit binomial counts (N-mixture, ",
      "Poisson abundance + binomial detection)"
    ),
    nmix_royle_nichols = paste0(
      "per-visit binary detections (Royle-Nichols, ",
      "Poisson abundance + per-individual detection)"
    ),
    nmix_poisson_poisson = paste0(
      "per-visit counts (Poisson abundance + ",
      "Poisson encounter rate; Neyman Type A marginal)"
    ),
    mvn = "joint multivariate normal observations per unit",
    mvt = "joint multivariate Student-t observations per unit",
    diri = "per-unit simplex compositions",
    multi = "per-unit multinomial category counts",
    categ = "per-unit single categorical draws",
    paste0(fam_name, " observations")
  )
}


# ---------------------------------------------------------------
# Model section
# ---------------------------------------------------------------
#
# One align* block. Rows in order:
#   1. likelihood line: Y_{it} \sim Family(mu_{it}, ...)
#   2. link line: link(mu_{it}) = beta_0 + ... + eta_{it}
#   3. one row per multilevel / smooth term definition
#   4. latent dynamics rows from the latent registry

#' @noRd
render_model_section <- function(ctx) {
  obj <- ctx$object
  notation <- ctx$notation
  fam_name <- resolve_family_name(obj$family)
  link <- obj$family$link

  rows <- list()
  for (ir in index_range_rows(obj)) {
    rows[[length(rows) + 1L]] <- ir
  }

  responses <- names(response_columns(obj))
  if (length(responses) > 1L) {
    # mvbind / multivariate brmsformula. Two flavours:
    #
    #   * `set_rescor(TRUE)` -- residuals are jointly MVNormal
    #     across responses with an LKJ-distributed correlation
    #     matrix. Render ONE joint likelihood + per-response
    #     linpred, plus the Sigma + Omega decomposition rows.
    #   * `set_rescor(FALSE)` (or unset) -- responses are
    #     independent. Render one likelihood + one linpred per
    #     response via the slice pattern.
    if (has_rescor(obj)) {
      mu_vec <- paste0("(\\mu^{(", responses, ")}_{i,t})",
                        collapse = ", ")
      rows[[length(rows) + 1L]] <- list(
        lhs = paste0("\\mathbf{Y}_{i,t}"),
        op  = "\\sim",
        rhs = paste0(
          "\\text{MVNormal}\\!\\left(",
          "(",
          paste0("\\mu^{(", responses, ")}_{i,t}",
                  collapse = ", "),
          ")^{\\top}, \\boldsymbol{\\Sigma}\\right)"
        )
      )
      sigma_vec <- paste0("\\sigma^{(", responses, ")}",
                           collapse = ", ")
      rows[[length(rows) + 1L]] <- list(
        lhs = "\\boldsymbol{\\Sigma}",
        op  = "=",
        rhs = paste0(
          "\\text{diag}(", sigma_vec, ") \\, \\boldsymbol{\\Omega} \\,",
          " \\text{diag}(", sigma_vec, ")"
        )
      )
      for (r in responses) {
        obj_r <- subset_obj_to_response(obj, r)
        mu_r <- paste0("\\mu^{(", r, ")}_{i,t}")
        rows[[length(rows) + 1L]] <- list(
          lhs = link_application(obj_r$family$link, mu_r),
          op  = "=",
          rhs = linear_predictor_rhs(obj_r, notation)
        )
      }
    } else {
      # Slice the obj down to a single-response view per
      # response and reuse the existing helpers wholesale --
      # the slice filters the prior table and carries the
      # response's own family, so every downstream extractor /
      # symbol formatter sees the per-response subset without any
      # new threading.
      for (r in responses) {
        obj_r <- subset_obj_to_response(obj, r)
        mu_r <- paste0("\\mu^{(", r, ")}_{i,t}")
        rows[[length(rows) + 1L]] <- list(
          lhs = paste0(r, "_{i,t}"),
          op  = "\\sim",
          rhs = family_distribution_text(
            resolve_family_name(obj_r$family), mu_r, obj_r
          )
        )
        rows[[length(rows) + 1L]] <- list(
          lhs = link_application(obj_r$family$link, mu_r),
          op  = "=",
          rhs = linear_predictor_rhs(obj_r, notation)
        )
      }
    }
  } else if (!is.null(closure_unit_family_kind(obj))) {
    # Closure-unit detection family (occ / nmix variants). The
    # per-visit observation row + per-unit latent state row +
    # state linpred replace the standard `y ~ Family(mu)` /
    # `link(mu) = ...` pair. The detection linpred (`logit(p) =
    # ...` or `log p = ...`) is emitted by
    # `dpar_linear_predictor_rows` below via the `p` dpar rows
    # brms generates from `bf(..., p ~ <covs>)`.
    rows <- c(rows, closure_unit_likelihood_rows(obj, notation))
  } else if (!is.null(mv_custom_family_kind(obj))) {
    # Multi-response custom family (mvn / mvt / diri / multi /
    # categ): joint likelihood + Sigma decomposition (mvn /
    # mvt) or softmax composition (diri / multi / categ).
    rows <- c(rows, mv_custom_likelihood_rows(obj, notation))
  } else {
    mu <- mu_symbol(obj)
    rows[[length(rows) + 1L]] <- list(
      lhs = response_subscripted(obj),
      op  = "\\sim",
      rhs = family_distribution_text(fam_name, mu, obj)
    )
    # Non-linear formulas (`bf(y ~ a + b*env, a + b ~ ..., nl = TRUE)`)
    # render the top-level mu with the nlpar tokens used verbatim
    # (a_{i,t}, b_{i,t}); each nlpar then gets its own decomposition
    # row below. Linear formulas use the standard
    # `linear_predictor_rhs` walker.
    if (is_nonlinear_formula(obj$formula)) {
      rows[[length(rows) + 1L]] <- list(
        lhs = link_application(link, mu),
        op  = "=",
        rhs = nl_top_linpred_rhs(obj)
      )
    } else {
      rows[[length(rows) + 1L]] <- list(
        lhs = link_application(link, mu),
        op  = "=",
        rhs = linear_predictor_rhs(obj, notation)
      )
    }
  }

  rows <- c(rows, dpar_linear_predictor_rows(obj, notation))
  rows <- c(rows, nlpar_linear_predictor_rows(obj, notation))
  rows <- c(rows, unlist(lapply(response_views(obj), term_definition_rows,
                                notation = notation), recursive = FALSE))
  rows <- c(rows, trend_predictor_rows(obj, notation))
  rows <- c(rows, latent_dynamics_rows(obj, notation))
  rows <- c(rows, factor_model_rows(obj, notation))

  block <- align_block(rows)
  glossary <- model_glossary(obj)
  paste(c("## Model", "", block, "", glossary), collapse = "\n")
}

#' @noRd
trend_predictor_rows <- function(obj, notation) {
  if (!methods_md_has_latent_trend(obj)) return(list())
  view <- trend_side_view(obj)
  if (!has_linear_terms(view)) return(list())
  c(
    list(list(
      lhs = "\\mu^{(\\eta)}_{i,t}",
      op  = "=",
      rhs = linear_predictor_rhs(view, notation)
    )),
    term_definition_rows(view, notation)
  )
}

#' @noRd
dpar_linear_predictor_rows <- function(obj, notation) {
  # Distributional parameter sub-formulas (`bf(y ~ x, sigma ~ x)`)
  # emit prior rows with non-empty `dpar`. For each dpar present,
  # per response on a multivariate model, render one extra row in
  # the model section showing its own linear predictor under the
  # link that response's family gives it.
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  dpars <- prior$dpar %||% rep("", nrow(prior))
  resps <- prior$resp %||% rep("", nrow(prior))
  present <- unique(data.frame(resp = resps, dpar = dpars)[nzchar(dpars), ,
                                                          drop = FALSE])
  if (nrow(present) == 0L) return(list())
  visit_grain <- is_closure_unit_family(obj$family)
  rows <- list()
  for (i in seq_len(nrow(present))) {
    r <- present$resp[i]
    dp <- present$dpar[i]
    family <- model_families(obj, if (nzchar(r)) r)
    sym <- dpar_symbol(dp, visit_grain = visit_grain, resp = r)
    rows[[length(rows) + 1L]] <- list(
      lhs = link_application(dpar_link(family, dp), sym),
      op = "=",
      rhs = dpar_predictor_rhs(prior[resps == r, , drop = FALSE], dp)
    )
  }
  rows
}

#' @noRd
dpar_symbol <- function(dp, visit_grain = FALSE, resp = "") {
  # Map common dpar names to their Greek / mathematical form.
  # `visit_grain = TRUE` swaps the `_{i,t}` subscript for
  # `_{i,j}` (closure-unit detection sub-formulas operate per
  # visit `j` within unit `i`). A response of a multivariate model
  # is named in a superscript, as its mean is.
  base <- family_par_symbol(dp)
  subscript <- if (visit_grain) "_{i,j}" else "_{i,t}"
  superscript <- if (nzchar(resp)) paste0("^{(", resp, ")}") else ""
  paste0(base, superscript, subscript)
}

#' @noRd
dpar_predictor_rhs <- function(prior, dp) {
  # Shared shape with nl sub-formulas; see `sub_predictor_rhs`.
  # dpar fits emit `Intercept` as its own class row keyed on the
  # dpar column, so `intercept_in_b = FALSE`.
  sub_predictor_rhs(prior, dp, col = "dpar",
                     intercept_in_b = FALSE)
}

#' @noRd
sub_predictor_rhs <- function(prior, value, col,
                                intercept_in_b = FALSE) {
  # Build the linear-predictor RHS for one sub-formula entry
  # (a dpar or an nlpar). Mirrors the main linear predictor:
  # intercept (when present) + per-coef b rows. Smooth / GP / RE /
  # mo / me on sub-formulas are uncommon enough that the bare
  # fixed-effect form is rendered here and other shapes surface
  # via the priors block.
  #
  # `intercept_in_b` toggles where brms emits the per-sub-formula
  # intercept:
  #   * dpar (`sigma ~ x`): a separate `Intercept` class row tagged
  #     with the dpar column. Detect via `class == "Intercept"`.
  #   * nlpar (`bf(..., a ~ ..., nl = TRUE)`): brms folds the
  #     intercept INTO the `b` class as `b_<nlpar>_Intercept` with
  #     no separate `Intercept` row. Detect via the coef name.
  col_vals <- prior[[col]] %||% rep("", nrow(prior))
  b_rows <- prior$class == "b" & col_vals == value &
    nzchar(prior$coef)
  b_coefs <- prior$coef[b_rows]
  if (intercept_in_b) {
    has_int <- "Intercept" %in% b_coefs
    coefs <- unique(b_coefs[b_coefs != "Intercept"])
  } else {
    has_int <- any(prior$class == "Intercept" & col_vals == value)
    coefs <- unique(b_coefs)
  }
  parts <- character(0L)
  if (has_int) {
    parts <- c(parts, paste0("\\alpha^{(", value, ")}"))
  }
  for (co in coefs) {
    parts <- c(parts, paste0(
      "\\beta_{", escape_math_text(value), ",", escape_math_text(co),
      "} \\, ", escape_math_text(co), "_{i,t}"
    ))
  }
  if (length(parts) == 0L) return("0")
  paste(parts, collapse = " + ")
}

#' @noRd
nl_top_linpred_rhs <- function(obj) {
  # Render the top-level RHS of a `bf(..., nl = TRUE)` formula
  # verbatim, with each nlpar token decorated with the `(i, t)`
  # subscript so the reader knows it stands for the per-obs
  # value defined in the per-nlpar decomposition below. The
  # nlpars are the names of `$pforms`. Other tokens (covariates
  # like `env`, operators like `*`, `+`) pass through unchanged
  # via `deparse` then a per-token rewrite. Math output is
  # intentionally plain (no `\beta`-style coefs) because in nl
  # form the top-level RHS encodes the structural equation
  # rather than a linear-coefficient sum.
  f <- obj$formula
  nlpars <- names(f$pforms %||% list())
  if (length(nlpars) == 0L) {
    return(linear_predictor_rhs(obj, "default"))
  }
  rhs_src <- paste(deparse(f$formula[[3L]], width.cutoff = 80L),
                    collapse = " ")
  # The indexed, escaped name a token becomes. A backslash in a
  # replacement string is an escape, and the escaped name carries one.
  subscripted <- function(name) {
    gsub("\\", "\\\\", paste0(escape_math_text(name), "_{i,t}"),
         fixed = TRUE)
  }
  # Decorate each nlpar token. Use word boundaries so `b` in `bx`
  # is not rewritten when only `b` is the nlpar.
  for (np in nlpars) {
    pat <- paste0("\\b", np, "\\b")
    rhs_src <- gsub(pat, subscripted(np), rhs_src)
  }
  # Subscript bare data covariates `x` -> `x_{i,t}` so they look
  # like the rest of the math block. Skip already-subscripted
  # tokens via the same word-boundary trick (no `_{` immediately
  # after). Pull the covariate list from formula_used_vars and
  # rewrite only those (avoids touching constants / numbers).
  for (v in setdiff(formula_used_vars(obj), nlpars)) {
    pat <- paste0("\\b", v, "\\b(?!_\\{)")
    rhs_src <- gsub(pat, subscripted(v), rhs_src, perl = TRUE)
  }
  # `*` is the multiplication marker; render as a thin space so
  # `a + b * env` becomes `a_{i,t} + b_{i,t} env_{i,t}` rather
  # than dropping the operator entirely.
  rhs_src <- gsub("\\s*\\*\\s*", " \\\\, ", rhs_src)
  paste(c(rhs_src, trend_linpred_terms(obj)), collapse = " + ")
}

#' @noRd
nlpar_linear_predictor_rows <- function(obj, notation) {
  # Per-nlpar decomposition rows for `bf(..., nl = TRUE)`. Each
  # entry in `$pforms` (e.g. `a ~ trait1 + (1 | sp | species)`)
  # gets one row in the Model section showing how that nlpar
  # decomposes into intercept + fixed coefs. Random-effect terms
  # in the sub-formula surface in the Priors block and via the
  # standard ranef glossary; v1 of the nlpar row covers the
  # fixed-effect part only.
  if (!is_nonlinear_formula(obj$formula)) {
    return(list())
  }
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  nlpars <- unique(prior$nlpar[nzchar(prior$nlpar %||% "")])
  if (length(nlpars) == 0L) return(list())
  out <- vector("list", length(nlpars))
  for (i in seq_along(nlpars)) {
    np <- nlpars[i]
    out[[i]] <- list(
      lhs = paste0(escape_math_text(np), "_{i,t}"),
      op  = "=",
      rhs = nlpar_predictor_rhs(prior, np)
    )
  }
  out
}

#' @noRd
nlpar_predictor_rhs <- function(prior, np) {
  # nl sub-formula intercept lives inside the `b` class as
  # `b_<nlpar>_Intercept`, so split via the coef name not the
  # row class.
  sub_predictor_rhs(prior, np, col = "nlpar",
                     intercept_in_b = TRUE)
}

#' One view per response of a multi-response model, or the model
#' @noRd
response_views <- function(obj) {
  keys <- names(response_columns(obj))
  if (length(keys) < 2L) return(list(obj))
  lapply(keys, function(r) subset_obj_to_response(obj, r))
}

#' Glossary rows for a family's own parameters
#'
#' Closure-unit and multi-response families define theirs in their
#' own likelihood rows.
#' @noRd
family_glossary <- function(obj) {
  if (!is.null(closure_unit_family_kind(obj)) ||
        !is.null(mv_custom_family_kind(obj))) {
    return(character(0L))
  }
  what <- c(
    sigma = "residual standard deviation", shape = "shape",
    nu = "degrees of freedom", phi = "precision", kappa = "precision",
    hu = "hurdle probability", zi = "zero-inflation probability"
  )
  dpars <- intersect(names(what), obj$family$dpars %||% character(0L))
  c(
    vapply(dpars, function(dp) {
      paste0("- $", dpar_aware_param(dp, obj), "$: ", what[[dp]])
    }, character(1L), USE.NAMES = FALSE),
    if (is_ordinal_family(obj$family)) {
      "- $\\boldsymbol{\\theta}$: ordered thresholds between categories"
    }
  )
}

#' Glossary row for the terms of distributional and non-linear
#' sub-predictors, which share one notation
#' @noRd
sub_predictor_glossary <- function(obj) {
  prior <- obj$prior
  subs <- unique(c(prior$dpar, prior$nlpar %||% character(0L)))
  subs <- subs[nzchar(subs)]
  if (length(subs) == 0L) return(character(0L))
  paste0(
    "- $\\alpha^{(p)}$, $\\beta_{p,j}$: intercept and effect of ",
    "covariate $j$ in the predictor of parameter $p$ (",
    paste0("$", subs, "$", collapse = ", "), ")"
  )
}

#' Glossary rows for the terms of one linear predictor
#' @noRd
term_glossary <- function(obj) {
  defs <- character(0L)
  classes <- classify_obs_parameters(obj)
  if ("Intercept" %in% classes$fixed) {
    defs <- c(defs, paste0(
      "- $", response_symbol(obj, "\\alpha"), "$: population intercept"
    ))
  }
  covariates <- escape_math_text(setdiff(classes$fixed, "Intercept"))
  if (length(covariates) > 0L) {
    defs <- c(defs, paste0(
      "- $", response_symbol(obj, paste0("\\beta_{", covariates, "}")),
      "$: population effect of $", covariates, "$"
    ))
  }
  for (spec in classes$smooth) {
    sym <- term_symbols("smooth", spec, obj)
    key <- spec_key(spec)
    k_label <- if (!is.na(spec$k)) {
      paste0("$K_{", key, "} = ", spec$k, "$")
    } else {
      paste0("$K_{", key, "}$ (mgcv default)")
    }
    defs <- c(defs, paste0(
      "- $", sym[["f"]], "$: ", basis_label(spec$bs, spec$fname),
      " in $", paste(escape_math_text(spec$vars), collapse = "$, $"),
      "$, basis size ",
      k_label, ", smoothing SD $", sym[["sd"]], "$"
    ))
  }
  for (spec in classes$gp) {
    sym <- term_symbols("gp", spec, obj)
    by_text <- if (!is.null(spec$by) && !is.na(spec$by) &&
                    nzchar(spec$by)) {
      paste0(", stratified by $", escape_math_text(spec$by), "$")
    } else ""
    k_text <- if (!is.null(spec$k) && !is.na(spec$k)) {
      paste0(", approximated with ", spec$k, " basis functions")
    } else {
      ", exact (full covariance kernel)"
    }
    defs <- c(defs, paste0(
      "- $", sym[["f"]], "$: Gaussian process in $",
      spec_subscript(spec), "$", by_text, " with ",
      gp_kernel_human_label(spec$cov), " kernel, length scale $",
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
    sdv <- escape_math_text(if (!is.na(s$sdvar)) s$sdvar else "se")
    sym <- term_symbols("me", s, obj)
    defs <- c(defs, paste0(
      "- $\\tilde{", v, "}_{i,t}$: latent true covariate underlying ",
      "noisy observation $", v, "_{i,t}$, with known per-observation ",
      "measurement-error SD $", sdv, "_{i,t}$, population hyper-mean $",
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
  defs
}

#' @noRd
model_glossary <- function(obj) {
  defs <- c(
    paste0("- $i$ indexes series, $t$ indexes time")
  )
  defs <- c(defs, closure_unit_glossary(obj))
  defs <- c(defs, mv_custom_glossary(obj))
  keys <- names(response_columns(obj))
  if (length(keys) > 1L) {
    defs <- c(defs, vapply(keys, function(r) {
      paste0("- $\\mu^{(", r, ")}_{i,t}$: conditional mean of $", r,
             "_{i,t}$")
    }, character(1L), USE.NAMES = FALSE))
  } else if (is.null(closure_unit_family_kind(obj)) &&
               is.null(mv_custom_family_kind(obj))) {
    defs <- c(defs, paste0(
      "- $\\mu_{i,t}$: conditional mean of $",
      response_letter(obj), "_{i,t}$"
    ))
  }
  defs <- c(defs, unlist(lapply(response_views(obj), function(v) {
    c(term_glossary(v), family_glossary(v), sub_predictor_glossary(v))
  })))
  if (methods_md_has_latent_trend(obj)) {
    tt <- obj$trend_metadata$trend_type
    label <- trend_order_label(obj)
    is_factor <- methods_md_has_factor_model(obj)
    defs <- c(defs, paste0(
      "- $\\eta_{i,t}$: latent state at series $i$ time $t$"
    ))
    view <- trend_side_view(obj)
    if (has_linear_terms(view)) {
      defs <- c(
        defs,
        "- $\\mu^{(\\eta)}_{i,t}$: trend predictor from 'trend_formula'",
        term_glossary(view)
      )
    }
    if (is_factor) {
      n_lv <- obj$trend_metadata$n_lv
      defs <- c(defs, paste0(
        "- $\\tilde\\eta_{k,t}$: latent ", label,
        " factor $k = 1, \\ldots, ", n_lv, "$ at time $t$"
      ))
      defs <- c(defs, paste0(
        "- $Z_{i,k}$: loading of series $i$ on factor $k$"
      ))
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
    defs <- c(defs, innovation_glossary(obj, is_factor))
    if (identical(tt, "AR")) {
      defs <- c(defs, ar_glossary(obj))
    }
    if (identical(tt, "VAR")) {
      defs <- c(defs, paste0(
        "- $\\boldsymbol{\\Phi}_l$: autoregressive coefficient matrix at ",
        "lag $l$, mapped into the stationary region from an unconstrained ",
        "matrix $\\mathbf{A}_l$"
      ))
      if ("Amu_trend" %in% obj$prior$class) {
        defs <- c(defs, paste0(
          "- $\\mu^{(A)}$, $\\omega^{(A)}$: mean and precision of the ",
          "normal prior on the entries of $\\mathbf{A}_l$, one pair for ",
          "the diagonal and one for the off-diagonal"
        ))
      }
    }
    if (trend_has_ma(obj) && identical(tt, "VAR")) {
      defs <- c(defs, paste0(
        "- $\\boldsymbol{\\Theta}_l$: moving-average coefficient matrix ",
        "at lag $l$, mapped into the invertible region from an ",
        "unconstrained matrix $\\mathbf{D}_l$ whose entries take normal ",
        "priors with means $\\mu^{(D)}$ and precisions $\\omega^{(D)}$"
      ))
    } else if (trend_has_ma(obj)) {
      defs <- c(defs, paste0(
        "- $", lag_symbol("\\theta", "l", trend_process_index(obj)),
        "$: moving-average coefficient at lag $l$"
      ))
    }
    if (identical(tt, "CAR")) {
      defs <- c(defs, paste0(
        "- $", trend_symbols(obj)[["ar1_trend"]],
        "$: autocorrelation over one time unit, and ",
        "$\\Delta t_{i,t}$ the time since series $i$ was last observed"
      ))
    }
    if (identical(tt, "PW")) defs <- c(defs, pw_glossary(obj))
    gr <- trend_grouping_var(obj)
    if (!is.null(gr)) {
      sym <- trend_symbols(obj)
      defs <- c(defs, paste0(
        "- $\\boldsymbol{\\Omega}_{", gr, "}$: innovation correlation ",
        "within each level of $", gr, "$, pooling a shared matrix $",
        sym[["L_Omega_global_trend"]], "$ and a level's own matrix $",
        sym[["L_deviation_group_trend"]], "$ with weight $",
        sym[["alpha_cor_trend"]], "$"
      ), paste0(
        "- $", sym[["sigma_group_trend"]], "$: innovation SDs within ",
        "each level of $", gr, "$"
      ))
    }
  }
  # Blank line before the bullets so Pandoc / Quarto picks up the
  # list rather than running it on as one paragraph.
  paste(c("where:", "", defs), collapse = "\n")
}

# Internal: the glossary entries for the trend's innovations, written
# in the form `innovation_rows()` gives them.
#' @noRd
innovation_glossary <- function(obj, is_factor) {
  tt <- obj$trend_metadata$trend_type
  if (identical(tt, "PW")) return(character(0L))
  sd <- innovation_sd(obj)
  sd_vec <- innovation_sd(obj, vector = TRUE)
  what <- if (is_factor) "factor innovation" else "process innovation"
  form <- innovation_form(obj, identical(tt, "VAR"))
  gr <- trend_grouping_var(obj)
  out <- switch(form,
    scalar = paste0("- $", trend_eps(obj), "$: ", what, ", SD $", sd, "$"),
    diag = paste0("- $", trend_eps_vec(obj), "$: ", what,
                  "s at time $t$, independent with SDs $", sd_vec, "$"),
    cor = paste0("- $", trend_eps_vec(obj), "$: ", what,
                 "s at time $t$, covariance $", sigma_symbol(gr), "$")
  )
  # ZMVN draws the states themselves and has no innovations.
  of <- "innovations"
  if (identical(tt, "ZMVN")) {
    out <- character(0L)
    of <- "latent states"
  }
  if (identical(form, "cor") && is.null(gr)) {
    out <- c(out, paste0(
      "- $\\boldsymbol{\\Omega}$: correlation matrix of the ", of
    ))
  }
  out
}

# Internal: whether a trend carries a moving-average part.
#
# `AR(ma = TRUE)` and `VAR(ma = TRUE)` are recorded under their base
# trend type, so nothing ever stores the spelling "ARMA". The MA
# lags are what say one is present, and asking for the spelling
# instead meant an ARMA model was described as the AR model it is
# built on, with its order and its coefficient both dropped.
#' @noRd
trend_has_ma <- function(obj) {
  length(obj$trend_metadata$ma_lags %||% integer(0L)) > 0L
}


# Internal: the glossary entries for an AR trend's coefficients.
#' @noRd
ar_glossary <- function(obj) {
  classes <- obj$prior$class
  shared <- any(startsWith(classes, "shared_ar"))
  idx <- if (!shared) trend_process_index(obj)
  phi <- lag_symbol("\\phi", "l", idx)
  of <- if (shared) " shared by every series" else ""
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
  out
}


# Internal: the conventional label of a trend, such as `AR(2)` or
# `ARMA(2, 1)`. A contiguous lag set is named by its order. A sparse
# one lists its lags in braces, `AR({1, 12})`: no order expresses it,
# and a bare list of lags would spell an ARIMA order.
#' @noRd
trend_order_label <- function(obj) {
  tt <- obj$trend_metadata$trend_type
  if (is.null(tt)) return("")
  ar_lags <- obj$trend_metadata$ar_lags %||% integer(0L)
  ma_lags <- obj$trend_metadata$ma_lags %||% integer(0L)
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
    "PW"   = paste0(
      "PW(", pw_growth(obj$trend_metadata), ")"
    ),
    tt
  )
}

#' @noRd
index_range_rows <- function(obj) {
  # The data section's axis counts. They count a renamed or derived
  # series or time axis as the model resolved it.
  counts <- printed_axis_counts(obj)
  index_row <- function(lhs, n) {
    list(lhs = lhs, op = "\\in",
         rhs = paste0("\\{1, \\ldots, ", n, "\\}"))
  }
  rows <- list()
  # A single series needs no range for `i`.
  if (isTRUE(counts$n_series > 1L)) {
    rows[[length(rows) + 1L]] <- index_row("i", counts$n_series)
  }
  if (!is.null(counts$n_timepoints)) {
    rows[[length(rows) + 1L]] <- index_row("t", counts$n_timepoints)
  }
  rows
}

#' @noRd
response_letter <- function(obj) {
  # The column rather than brms's key for it, so the rendered name
  # keeps any `_` in the user's column (e.g. `y_occ`) where the key
  # would read `yocc`.
  nm <- unname(response_columns(obj))
  if (length(nm) > 1L) return("\\mathbf{Y}")
  escape_math_text(nm[[1L]])
}

#' @noRd
escape_math_text <- function(s) {
  # Inside `$...$` a `_` is a subscript marker. Left bare, `y_diri`
  # renders as `y` with subscript `diri`. A name carrying one is set
  # upright.
  if (is.null(s) || !is.character(s)) return(s)
  out <- vapply(s, function(x) {
    if (!grepl("_", x, fixed = TRUE)) return(x)
    math_upright(x)
  }, character(1L))
  if (length(out) == 1L) unname(out) else out
}

#' @noRd
math_upright <- function(x) {
  # An identifier set upright in math mode, its underscores escaped.
  # `\mathrm{}` keeps the escape a math macro, which LaTeX and
  # MathJax both render. MathJax prints `\_` inside `\text{}`
  # literally, backslash and all.
  paste0("\\mathrm{", gsub("_", "\\\\_", x), "}")
}

#' @noRd
response_subscripted <- function(obj) {
  paste0(response_letter(obj), "_{i,t}")
}

#' @noRd
mu_symbol <- function(obj) {
  if (length(response_columns(obj)) > 1L) {
    return("\\boldsymbol{\\mu}_{i,t}")
  }
  "\\mu_{i,t}"
}

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

#' The symbol of a family's own parameter
#'
#' One map for the likelihood, the distributional sub-formulas and the
#' priors. brms's `shape` is the Gamma shape and the negative binomial
#' size alike.
#' @noRd
family_par_symbol <- function(dp) {
  sym <- c(
    sigma = "\\sigma", phi = "\\phi", shape = "\\varphi",
    kappa = "\\kappa", nu = "\\nu", mu = "\\mu",
    hu = "\\pi^{(\\text{hu})}", zi = "\\pi^{(\\text{zi})}",
    p = "p", r = "r"
  )
  if (dp %in% names(sym)) sym[[dp]] else paste0("\\text{", dp, "}")
}

#' @noRd
dpar_aware_param <- function(name, obj) {
  # Render a family auxiliary parameter (sigma, phi, nu, ...)
  # either as the bare symbol (`\\sigma`) when it is a single
  # population-level scalar, or as the indexed symbol
  # (`\\sigma_{i,t}`) when the user attached a dpar sub-formula
  # that makes it vary across observations.
  prior <- obj$prior
  has_dpar <- !is.null(prior) && nrow(prior) > 0L &&
    any((prior$dpar %||% "") == name)
  base <- family_par_symbol(name)
  response_symbol(obj, if (has_dpar) paste0(base, "_{i,t}") else base)
}

#' @noRd
family_distribution_text <- function(fam_name, mu, obj) {
  sigma <- dpar_aware_param("sigma", obj)
  phi   <- dpar_aware_param("phi", obj)
  nu    <- dpar_aware_param("nu", obj)
  shape <- dpar_aware_param("shape", obj)
  hu    <- dpar_aware_param("hu", obj)
  zi    <- dpar_aware_param("zi", obj)
  switch(
    fam_name,
    poisson     = paste0("\\text{Poisson}(", mu, ")"),
    bernoulli   = paste0("\\text{Bernoulli}(", mu, ")"),
    binomial    = paste0("\\text{Binomial}(n_{i,t}, ", mu, ")"),
    gaussian    = paste0("\\text{Normal}(", mu, ", ", sigma, ")"),
    student     = paste0("\\text{StudentT}(", nu, ", ", mu,
                          ", ", sigma, ")"),
    lognormal   = paste0("\\text{LogNormal}(", mu, ", ", sigma, ")"),
    gamma       = paste0("\\text{Gamma}(", shape, ", ", mu, ")"),
    beta        = paste0("\\text{Beta}(", mu, ", ", phi, ")"),
    negbinomial = paste0("\\text{NegBin}(", mu, ", ", shape, ")"),
    beta_nb     = paste0(
      "\\text{BetaNegBinomial}(", mu, ", r, \\tau)"
    ),
    tweedie     = paste0(
      "\\text{Tweedie}(", mu, ", \\phi, \\xi)"
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
      "\\text{ZIBinomial}(n_{i,t}, ", mu, ", ", zi, ")"
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
    paste0("\\text{", fam_name, "}(", mu, ")")
  )
}


# ---------------------------------------------------------------
# Closure-unit family rows (occ / nmix variants)
# ---------------------------------------------------------------
# Per-visit observation + per-unit latent state + state linpred
# + detection linpred. Replaces the standard `y ~ Family(mu)` /
# `link(mu) = ...` two-row pair that the main composer emits
# for non-closure-unit families. Stan parameterisations verified
# against R/families.R:
#   * occ              : z_i ~ Bernoulli(psi_i); y | z ~ Bern
#   * nmix (PB)        : N_i ~ Poisson(lambda); y | N ~ Bin
#   * nmix (RN)        : N_i ~ Poisson(lambda); y | N ~ Bern(1 - (1-r)^N)
#   * nmix (PPM)       : N_i ~ Poisson(lambda); y | N ~ Poisson(N r)
#   (PPM marginal y_{i,j} ~ Poisson(lambda_i r_{i,j}) -- the
#    Neyman Type A; both forms render correctly.)

#' @noRd
closure_unit_family_kind <- function(obj) {
  # Returns one of: "occ", "nmix_pb", "nmix_rn", "nmix_ppm",
  # or NULL when the family is not a detection family.
  if (!is_closure_unit_family(obj$family)) return(NULL)
  fam_name <- resolve_family_name(obj$family)
  switch(
    fam_name,
    occ                    = "occ",
    nmix                   = "nmix_pb",
    nmix_poisson_binomial  = "nmix_pb",
    nmix_royle_nichols     = "nmix_rn",
    nmix_poisson_poisson   = "nmix_ppm",
    NULL
  )
}

#' @noRd
closure_unit_state_symbol <- function(kind) {
  # The state parameter (\psi for occupancy, \lambda for
  # abundance) that the state linpred maps onto.
  switch(
    kind,
    occ      = "\\psi",
    nmix_pb  = "\\lambda",
    nmix_rn  = "\\lambda",
    nmix_ppm = "\\lambda",
    NULL
  )
}

#' @noRd
closure_unit_detection_symbol <- function(kind) {
  # Per-visit detection-probability symbol. All variants name
  # the brms dpar `p`, so the math output uses `p` for naming
  # parity with `obj$prior$dpar == "p"` (which drives the
  # detection linpred row via `dpar_linear_predictor_rows`).
  # The mathematical interpretation of `p` differs by kind --
  # per-visit detection (PB / occ), per-individual detection
  # (RN), or per-visit encounter rate (PPM) -- and is
  # documented in `closure_unit_glossary()`.
  "p"
}

#' @noRd
closure_unit_likelihood_rows <- function(obj, notation) {
  kind <- closure_unit_family_kind(obj)
  if (is.null(kind)) return(list())
  det_sym   <- closure_unit_detection_symbol(kind)
  # Multi-season fits index closure units by `(series, site,
  # season)`, so the latent state and state probability carry a
  # `_{i,t}` (season) subscript. Single-season uses bare `_i`.
  unit_idx <- if (is_multi_season_family(obj$family)) "{i,t}" else "{i}"
  state_sym <- paste0(
    closure_unit_state_symbol(kind), "_", unit_idx
  )
  latent_lhs <- paste0(
    if (identical(kind, "occ")) "z" else "N",
    "_", unit_idx
  )

  obs_rhs <- switch(
    kind,
    occ      = paste0(
      "\\text{Bernoulli}(", latent_lhs, " \\cdot ",
      det_sym, "_{i,j})"
    ),
    nmix_pb  = paste0(
      "\\text{Binomial}(", latent_lhs, ", ", det_sym, "_{i,j})"
    ),
    nmix_rn  = paste0(
      "\\text{Bernoulli}(1 - (1 - ", det_sym, "_{i,j})^{",
      latent_lhs, "})"
    ),
    nmix_ppm = paste0(
      "\\text{Poisson}(", latent_lhs, " \\cdot ",
      det_sym, "_{i,j})"
    )
  )
  latent_rhs <- switch(
    kind,
    occ      = paste0("\\text{Bernoulli}(", state_sym, ")"),
    nmix_pb  = paste0("\\text{Poisson}(", state_sym, ")"),
    nmix_rn  = paste0("\\text{Poisson}(", state_sym, ")"),
    nmix_ppm = paste0("\\text{Poisson}(", state_sym, ")")
  )
  obs_lhs <- paste0("y_{i,j} \\mid ", latent_lhs)

  rows <- list(
    list(lhs = obs_lhs, op = "\\sim", rhs = obs_rhs),
    list(lhs = latent_lhs, op = "\\sim", rhs = latent_rhs)
  )

  # State linpred -- reuses linear_predictor_rhs unchanged so
  # every covariate machinery (fixed / smooth / GP / RE / mo /
  # me / trend) flows in automatically. Only the LHS symbol
  # and link change. When the user adds `trend_formula = ~
  # AR(p = 1)` to a multi-season fit, the AR-on-logit-psi
  # dynamics arrive automatically through `latent_dynamics_rows`
  # (no extra renderer needed -- the AR rendering treats `eta`
  # as the season-indexed addition to the state linpred).
  state_link <- closure_unit_state_link(kind)
  state_link_lhs <- link_application(state_link, state_sym)
  rows[[length(rows) + 1L]] <- list(
    lhs = state_link_lhs,
    op  = "=",
    rhs = linear_predictor_rhs(obj, notation)
  )
  rows
}

#' @noRd
closure_unit_state_link <- function(kind) {
  switch(
    kind,
    occ      = "logit",
    nmix_pb  = "log",
    nmix_rn  = "log",
    nmix_ppm = "log",
    "log"
  )
}


# ---------------------------------------------------------------
# Multi-response custom family rows (mvn / mvt / diri / multi /
# categ)
# ---------------------------------------------------------------
# These families take a vector observation per unit -- the
# joint MVNormal / Dirichlet / Multinomial / Categorical
# replaces the standard `y ~ Family(mu)` single row. Sigma
# decomposition rows for mvn / mvt mirror the LKJ-Cholesky
# parameterisation used in `mvn_stan_funs()` / `mvt_stan_funs()`
# (R/families.R).

#' @noRd
mv_custom_family_kind <- function(obj) {
  if (!is_multi_response_family(obj$family)) return(NULL)
  fam_name <- resolve_family_name(obj$family)
  switch(
    fam_name,
    mvn   = "mvn",
    mvt   = "mvt",
    diri  = "diri",
    multi = "multi",
    categ = "categ",
    NULL
  )
}

#' @noRd
mv_custom_likelihood_rows <- function(obj, notation) {
  kind <- mv_custom_family_kind(obj)
  if (is.null(kind)) return(list())
  rows <- list()
  # Joint observation row.
  obs_row <- switch(
    kind,
    mvn = list(
      lhs = "\\mathbf{Y}_i",
      op  = "\\sim",
      rhs = paste0(
        "\\text{MVNormal}(\\boldsymbol{\\mu}_i, ",
        "\\boldsymbol{\\Sigma})"
      )
    ),
    mvt = list(
      lhs = "\\mathbf{Y}_i",
      op  = "\\sim",
      rhs = paste0(
        "\\text{MVStudentT}(\\nu, \\boldsymbol{\\mu}_i, ",
        "\\boldsymbol{\\Sigma})"
      )
    ),
    diri = list(
      lhs = "\\mathbf{Y}_i",
      op  = "\\sim",
      rhs = "\\text{Dirichlet}(\\boldsymbol{\\alpha}_i)"
    ),
    multi = list(
      lhs = "\\mathbf{Y}_i",
      op  = "\\sim",
      rhs = paste0(
        "\\text{Multinomial}(N_i, \\boldsymbol{\\pi}_i)"
      )
    ),
    categ = list(
      lhs = "Y_i",
      op  = "\\sim",
      rhs = "\\text{Categorical}(\\boldsymbol{\\pi}_i)"
    )
  )
  rows[[length(rows) + 1L]] <- obs_row
  # Per-family parameter decomposition rows.
  rows <- c(rows, mv_custom_decomposition_rows(kind))
  # Composition / linpred rows -- one shared softmax-style line
  # for simplex families; explicit mu / Sigma for mvn / mvt.
  rows <- c(rows, mv_custom_composition_rows(kind, obj, notation))
  rows
}

#' @noRd
mv_custom_decomposition_rows <- function(kind) {
  switch(
    kind,
    mvn  = mv_scale_rows(),
    mvt  = mv_scale_rows(),
    diri = list(list(
      lhs = "\\boldsymbol{\\alpha}_i",
      op  = "=",
      rhs = "\\phi \\, \\boldsymbol{\\pi}_i"
    )),
    list()
  )
}

#' @noRd
# The `mvn` and `mvt` kernels evaluate independent normals with a
# per-element scale: `normal_lpdf(y_unit | mu_unit, psi_unit)` in
# R/families.R. Rendering a Cholesky decomposition with an LKJ prior
# here described a correlation the emitted Stan does not carry, and
# named a parameter the model never declares.
#'@noRd
mv_scale_rows <- function() {
  list(
    list(
      lhs = "\\boldsymbol{\\Sigma}",
      op  = "=",
      rhs = "\\text{diag}(\\boldsymbol{\\Psi}^2)"
    ),
    list(
      lhs = "\\boldsymbol{\\Psi}",
      op  = "\\sim",
      rhs = "\\text{Exponential}(1)"
    )
  )
}

#' @noRd
mv_custom_glossary <- function(obj) {
  kind <- mv_custom_family_kind(obj)
  if (is.null(kind)) return(character(0L))
  if (kind %in% c("mvn", "mvt")) {
    defs <- c(
      paste0(
        "- $\\mathbf{Y}_i$: per-unit response vector ",
        "(one entry per category)"
      ),
      paste0(
        "- $\\boldsymbol{\\mu}_i$: per-unit mean vector"
      ),
      paste0(
        "- $\\boldsymbol{\\Sigma}$: joint residual covariance, ",
        "$\\text{diag}(\\boldsymbol{\\Psi}^2)$"
      )
    )
    if (identical(kind, "mvt")) {
      defs <- c(defs, "- $\\nu$: degrees of freedom (Student-t)")
    }
    defs
  } else if (kind %in% c("diri", "multi", "categ")) {
    defs <- c(
      paste0(
        "- $\\mathbf{Y}_i$: per-unit response vector ",
        "(one entry per category)"
      ),
      paste0(
        "- $\\boldsymbol{\\pi}_i$: per-unit category ",
        "probabilities (simplex)"
      ),
      paste0(
        "- $\\eta^{(c)}_i$: linear predictor for category ",
        "$c$ at unit $i$ (category 1 fixed at 0 as reference)"
      )
    )
    if (identical(kind, "diri")) {
      defs <- c(defs, paste0(
        "- $\\phi$: Dirichlet precision parameter ",
        "($\\boldsymbol{\\alpha}_i = \\phi \\boldsymbol{\\pi}_i$)"
      ))
    }
    if (identical(kind, "multi")) {
      defs <- c(defs, "- $N_i$: per-unit trial count")
    }
    defs
  } else {
    character(0L)
  }
}

#' @noRd
mv_custom_composition_rows <- function(kind, obj, notation) {
  # Simplex families (diri / multi / categ) all map a vector of
  # per-category linpreds to a probability simplex via softmax
  # with the first category fixed as reference (Stan / brms
  # convention -- see R/families.R::diri_stan_funs et al.).
  if (kind %in% c("diri", "multi", "categ")) {
    return(list(list(
      lhs = "\\boldsymbol{\\pi}_i",
      op  = "=",
      rhs = paste0(
        "\\text{softmax}([0, \\eta^{(2)}_i, \\ldots, ",
        "\\eta^{(K)}_i]^\\top)"
      )
    )))
  }
  # mvn / mvt: emit the per-response mu linpred row (the obs
  # formula targets `mu` for category 1; further categories are
  # tied to mu by brms's mv response stacking), so a single shared
  # placeholder stands for the set.
  list(list(
    lhs = "\\boldsymbol{\\mu}_i",
    op  = "=",
    rhs = linear_predictor_rhs(obj, notation)
  ))
}

#' @noRd
closure_unit_glossary <- function(obj) {
  kind <- closure_unit_family_kind(obj)
  if (is.null(kind)) return(character(0L))
  det_sym <- closure_unit_detection_symbol(kind)
  unit_idx <- if (is_multi_season_family(obj$family)) "{i,t}" else "{i}"
  multi_season <- is_multi_season_family(obj$family)
  defs <- c(
    "- $j$ indexes visits within closure unit $i$"
  )
  if (multi_season) {
    defs <- c(defs, paste0(
      "- $t$ indexes seasons (closure units = ",
      "(series, site, season))"
    ))
  }
  if (identical(kind, "occ")) {
    defs <- c(defs,
      paste0(
        "- $z_", unit_idx, " \\in \\{0, 1\\}$: latent ",
        "occupancy state"
      ),
      paste0(
        "- $\\psi_", unit_idx, " \\in (0, 1)$: occupancy ",
        "probability (state linpred on the logit scale)"
      ),
      paste0(
        "- $", det_sym, "_{i,j} \\in (0, 1)$: per-visit ",
        "detection probability"
      )
    )
  } else {
    defs <- c(defs,
      paste0(
        "- $N_", unit_idx, " \\in \\mathbb{Z}_{\\ge 0}$: ",
        "latent abundance"
      ),
      paste0(
        "- $\\lambda_", unit_idx, " > 0$: abundance ",
        "intensity (state linpred on the log scale)"
      )
    )
    if (identical(kind, "nmix_rn")) {
      defs <- c(defs, paste0(
        "- $", det_sym, "_{i,j} \\in (0, 1)$: ",
        "per-individual detection probability"
      ))
    } else if (identical(kind, "nmix_ppm")) {
      defs <- c(defs, paste0(
        "- $", det_sym, "_{i,j} > 0$: per-visit encounter rate"
      ))
    } else {
      defs <- c(defs, paste0(
        "- $", det_sym, "_{i,j} \\in (0, 1)$: per-visit ",
        "detection probability"
      ))
    }
  }
  defs
}

#' @noRd
linear_predictor_rhs <- function(obj, notation) {
  classes <- classify_obs_parameters(obj)
  parts <- character(0L)

  if (length(classes$fixed) > 0L) {
    parts <- c(parts, render_fixed_inline(classes$fixed, obj))
  }
  if (length(classes$smooth) > 0L) {
    parts <- c(parts, render_smooth_inline(classes$smooth, obj))
  }
  if (length(classes$gp) > 0L) {
    parts <- c(parts, render_gp_inline(classes$gp, obj))
  }
  if (length(classes$mo) > 0L) {
    parts <- c(parts, render_mo_inline(classes$mo, obj))
  }
  if (length(classes$me) > 0L) {
    parts <- c(parts, render_me_inline(classes$me, obj))
  }
  if (length(classes$re) > 0L) {
    parts <- c(parts, render_re_inline(classes$re, obj))
  }
  parts <- c(parts, trend_linpred_terms(obj))

  if (length(parts) == 0L) {
    return("0")
  }
  paste(parts, collapse = " + ")
}

#' @noRd
classify_obs_parameters <- function(obj) {
  # Read structure off the prior table directly: it carries one
  # row per parameter class regardless of whether the fit has
  # posterior draws yet. This works for both fitted mvgam objects
  # and prefits (run_model = FALSE).
  prior <- obj$prior
  list(
    fixed  = obs_fixed_terms_from_prior(obj),
    smooth = obs_smooth_specs_from_prior(prior),
    gp     = obs_gp_specs_from_formula(obj),
    mo     = obs_mo_specs_from_prior(obj),
    me     = obs_me_specs_from_formula(obj),
    re     = obs_re_specs_from_prior(prior)
  )
}

#' @noRd
obs_fixed_terms_from_prior <- function(obj) {
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(character(0L))
  # Rows scoped to a dpar or nlpar drive their own predictor row.
  nlpar_col <- prior$nlpar %||% rep("", nrow(prior))
  # An ordinal family's `Intercept` rows are its thresholds, and its
  # predictor has no intercept of its own.
  has_int <- !is_ordinal_family(obj$family) && any(
    prior$class == "Intercept" &
      !nzchar(prior$dpar) &
      !nzchar(nlpar_col)
  )
  b_rows <- prior$class == "b" & !nzchar(prior$dpar) &
    !nzchar(nlpar_col) & nzchar(prior$coef)
  coefs <- unique(prior$coef[b_rows])
  # A smooth's unpenalised columns and the special terms each render
  # through their own block.
  terms <- coefs[is.na(obs_term_coef_kinds(obj, coefs))]
  if (has_int) c("Intercept", terms) else terms
}

#' The kind of term that owns each observation-mean `b` coefficient
#'
#' brms names a special term's coefficient after the term, `mo(x)` as
#' `mox`, and a smooth's unpenalised columns after its label and
#' column, `s(x)` as `sx_1`. A covariate named `month` or `size_2`
#' carries the same shape. The formula's terms decide ownership.
#'
#' @param obj An `mvgam` object.
#' @param coefs Coefficient names from its prior table.
#' @return Character vector along `coefs`: `"mo"`, `"me"`, `"mi"` or
#'   `"sm"`, and `NA` for a population effect.
#' @noRd
obs_term_coef_kinds <- function(obj, coefs) {
  out <- rep(NA_character_, length(coefs))
  # The brms model's formula carries its family, which brms needs to
  # parse a family's own distributional parameters.
  for (lp in model_predictors(side_model(obj, "obs")$formula)) {
    if (!is.null(lp$dpar) || !is.null(lp$nlpar)) next
    if (!is.null(obj$methods_md_resp) &&
          !identical(lp$resp, obj$methods_md_resp)) next
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

#' @noRd
obs_me_specs_from_formula <- function(obj) {
  # Walk the obs formula AST for `me(x, sdx)` calls and recover
  # the latent-variable name plus the measurement-error SD column.
  # Used to filter the synthesised `me<var><sdvar>` coef out of
  # the plain fixed list and to drive the me() block renderers.
  if (is.null(obj$formula)) return(list())
  f <- mvgam_obs_formula(obj)
  if (!inherits(f, "formula") || length(f) < 3L) return(list())
  calls <- formula_calls(f[[3L]], "me")
  calls <- calls[lengths(calls) >= 2L]
  lapply(calls, function(e) {
    args <- as.list(e)[-1L]
    var <- formula_arg_text(args[[1L]])
    sdvar <- if (length(args) >= 2L) {
      formula_arg_text(args[[2L]])
    } else NA_character_
    list(
      var = var, sdvar = sdvar,
      coef = paste0("me", var, if (!is.na(sdvar)) sdvar else "")
    )
  })
}

#' @noRd
obs_mo_specs_from_prior <- function(obj) {
  # One `b` row per mo() term carries the scale of its effect; its
  # simplex of step increments has class `simo`.
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  coefs <- unique(prior$coef[prior$class == "b" & nzchar(prior$coef)])
  coefs <- coefs[obs_term_coef_kinds(obj, coefs) %in% "mo"]
  lapply(coefs, function(c) {
    list(var = sub("^mo", "", c), coef = c)
  })
}


#' @noRd
obs_smooth_specs_from_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  sds_rows <- prior$class == "sds" & nzchar(prior$coef)
  if (!any(sds_rows)) return(list())
  coefs <- prior$coef[sds_rows]
  specs <- lapply(coefs, parse_smooth_coef)
  # Dedupe by full variable list; first-occurrence wins.
  seen <- character(0L)
  out <- list()
  for (s in specs) {
    if (is.null(s$vars) || all(is.na(s$vars))) next
    key <- paste(s$vars, collapse = ":")
    if (key %in% seen) next
    out[[length(out) + 1L]] <- s
    seen <- c(seen, key)
  }
  out
}

#' Parse a brms `sds` smooth coef label (e.g. `"s(x, k = 5, bs = \"cr\")"`)
#' into a spec list with `var / vars / k / bs / fname` fields. brms
#' writes the label from the term the formula holds, and it parses as
#' a call. `k` and `bs` are kept as the text the user wrote, or `NA`
#' and mgcv's own `"tp"` when they were left out.
#' @noRd
parse_smooth_coef <- function(coef_str) {
  checkmate::assert_string(coef_str)
  expr <- str2lang(coef_str)
  fname <- as.character(expr[[1L]])
  call_args <- as.list(expr)[-1L]
  arg_names <- names(call_args) %||% rep("", length(call_args))
  pos_idx <- which(arg_names == "")
  vars <- if (length(pos_idx) >= 1L) {
    vapply(call_args[pos_idx], formula_arg_text, character(1L))
  } else NA_character_
  k <- if ("k" %in% arg_names) {
    formula_arg_text(call_args$k)
  } else NA_character_
  bs <- if ("bs" %in% arg_names) formula_arg_text(call_args$bs) else "tp"
  list(
    var = vars[[1L]], vars = unname(vars), k = k, bs = bs, fname = fname
  )
}

#' @noRd
gp_kernel_human_label <- function(cov) {
  checkmate::assert_string(cov)
  switch(
    cov,
    "exp_quad"    = "exponentiated-quadratic",
    "matern52"    = "Matern (5/2)",
    "matern32"    = "Matern (3/2)",
    "exponential" = "exponential",
    cov
  )
}

#' @noRd
gp_kernel_label <- function(cov) {
  checkmate::assert_string(cov)
  switch(
    cov,
    "exp_quad"      = "k_{\\text{ExpQuad}}",
    "matern52"      = "k_{\\text{Matern}_{5/2}}",
    "matern32"      = "k_{\\text{Matern}_{3/2}}",
    "exponential"   = "k_{\\text{Exp}}",
    paste0("k_{\\text{", cov, "}}")
  )
}

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


#' @noRd
obs_re_specs_from_prior <- function(prior) {
  # Per-group spec: list(group, slopes, has_slope, has_corr).
  # `slopes` is the set of slope coefs under `(x + y | grp)`
  # (i.e. coef-keyed `sd` rows other than the bare Intercept).
  # `has_corr` true when brms emits an L row keyed on the group
  # (correlated random intercept + slope under `|grp`).
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  sd_rows <- prior$class == "sd" & nzchar(prior$group)
  if (!any(sd_rows)) return(list())
  groups <- unique(prior$group[sd_rows])
  l_groups <- if (any(prior$class == "L" & nzchar(prior$group))) {
    prior$group[prior$class == "L" & nzchar(prior$group)]
  } else character(0L)
  lapply(groups, function(g) {
    g_rows <- prior$class == "sd" & prior$group == g & nzchar(prior$coef)
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

#' @noRd
obs_gp_specs_from_formula <- function(obj) {
  # Walk each response's formula for `gp(...)` calls and recover the
  # spec per term: variable list, k, by and covariance kernel. A term
  # two responses share is described once.
  if (is.null(obj$formula)) return(list())
  calls <- unlist(lapply(response_formulas(obj), function(form) {
    f <- form$formula
    if (length(f) < 3L) list() else formula_calls(f[[3L]], "gp")
  }), recursive = FALSE, use.names = FALSE)
  unique(lapply(calls, gp_call_to_spec))
}

#' @noRd
gp_call_to_spec <- function(call) {
  args <- as.list(call)[-1L]
  arg_names <- names(args) %||% rep("", length(args))
  pos_mask <- arg_names == ""
  vars <- unname(vapply(args[pos_mask], formula_arg_text, character(1L)))
  if (length(vars) == 0L) {
    stop(insight::format_error(
      "gp() call has no positional variable arguments."
    ))
  }
  # Each setting is kept as written, and brms's own default where it
  # was left out.
  written <- function(name, default) {
    if (name %in% arg_names) formula_arg_text(args[[name]]) else default
  }
  list(
    vars = vars,
    k = written("k", NA_character_),
    by = written("by", NA_character_),
    cov = written("cov", "exp_quad"),
    coef = paste0("gp", paste(vars, collapse = ""))
  )
}

#' @noRd
render_fixed_inline <- function(terms, obj) {
  covariates <- escape_math_text(setdiff(terms, "Intercept"))
  bits <- c(
    if ("Intercept" %in% terms) response_symbol(obj, "\\alpha"),
    if (length(covariates)) paste0(
      response_symbol(obj, paste0("\\beta_{", covariates, "}")), " ",
      covariates, "_{i,t}"
    )
  )
  paste(bits, collapse = " + ")
}

#' A symbol tagged with the predictor a view carries: its response,
#' or the trend
#' @noRd
response_symbol <- function(obj, sym) {
  r <- obj$methods_md_tag %||% obj$methods_md_resp
  if (is.null(r)) return(sym)
  vapply(sym, apply_resp_superscript, character(1L), r = r,
         USE.NAMES = FALSE)
}

#' The symbols of one term, as every section of the page names them
#'
#' The equations, the glossary and the priors each name a term's
#' parameters, and take the names from here. A view of one response,
#' or of the trend, tags each symbol with its predictor.
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
      sub <- spec_subscript(spec)
      key <- spec_key(spec)
      c(f = paste0("f_{", sub, "}"),
        basis = paste0("\\beta^{(", key, ")}_k"),
        sd = paste0("\\sigma^{(s)}_{", key, "}"))
    },
    gp = {
      sub <- gp_subscript(spec)
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
      c(beta = paste0("\\beta^{(\\text{me})}_{", v, "}"),
        mu = paste0("\\mu^{(\\text{me})}_{", v, "}"),
        sd = paste0("\\sigma^{(\\text{me})}_{", v, "}"))
    },
    re = {
      # The slope names key the symbols as brms spells them, and the
      # symbols print them escaped.
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

#' @noRd
compose_inline_terms <- function(specs, term_composer) {
  # Shared shape for every per-spec inline renderer: apply
  # `term_composer` to each spec and join the resulting LaTeX
  # fragments with the additive separator. Centralised so that
  # adding a new effect type is one new `render_*_inline` line
  # rather than another copy of the paste/vapply/collapse skeleton.
  if (length(specs) == 0L) return(character(0L))
  paste(
    vapply(specs, term_composer, character(1L)),
    collapse = " + "
  )
}

#' @noRd
render_smooth_inline <- function(specs, obj) {
  compose_inline_terms(specs, function(s) {
    paste0(term_symbols("smooth", s, obj)[["f"]], "(",
           spec_vars_indexed(s), ")")
  })
}

#' @noRd
render_me_inline <- function(specs, obj) {
  # Measurement-error effects (brms `me(x, sdx)`): the linear
  # predictor uses the latent true covariate `\\tilde{x}_{i,t}`
  # rather than the noisy observation `x_{i,t}`.
  compose_inline_terms(specs, function(s) {
    paste0(term_symbols("me", s, obj)[["beta"]], " \\, \\tilde{",
           escape_math_text(s$var), "}_{i,t}")
  })
}

#' @noRd
render_mo_inline <- function(specs, obj) {
  # Monotonic effects (Burkner & Charpentier 2020). Each mo()
  # term contributes b^{(mo)}_x * m_x(x_{i,t}), where m_x is a
  # cumulative step transform built from a Dirichlet simplex.
  compose_inline_terms(specs, function(s) {
    sym <- term_symbols("mo", s, obj)
    paste0(sym[["beta"]], " \\, ", sym[["m"]], "(",
           escape_math_text(s$var), "_{i,t})")
  })
}

#' @noRd
render_gp_inline <- function(specs, obj) {
  compose_inline_terms(specs, function(s) {
    paste0(term_symbols("gp", s, obj)[["f"]], "(",
           spec_vars_indexed(s), ")")
  })
}

#' @noRd
spec_subscript <- function(spec) {
  # Math-subscript form of the variable list: "x" for univariate
  # smooths/GPs, "x, z" for tensor / multi-dim. Shared by every
  # renderer that needs a per-term subscript label.
  paste(escape_math_text(spec$vars), collapse = ", ")
}

#' @noRd
spec_key <- function(spec) {
  # Stable identifier per term, safe to embed in a LaTeX
  # subscript that already nests inside `_{...}` (no commas).
  # Used as the per-term key in basis-size $K_{key}$ and basis
  # coefficient $\beta^{(key)}$ tags.
  paste(escape_math_text(spec$vars), collapse = ":")
}

#' @noRd
spec_vars_indexed <- function(spec, suffix = "_{i,t}") {
  # "x_{i,t}, z_{i,t}" -- the indexed argument list used inside
  # a function call f_{sub}(x_{i,t}, z_{i,t}). suffix is a hook
  # for callers that want a different index pattern.
  paste(paste0(escape_math_text(spec$vars), suffix), collapse = ", ")
}

#' @noRd
gp_subscript <- function(spec) {
  base <- spec_subscript(spec)
  if (!is.null(spec$by) && !is.na(spec$by) && nzchar(spec$by)) {
    paste0(base, " \\mid ", escape_math_text(spec$by))
  } else {
    base
  }
}

#' @noRd
render_re_inline <- function(specs, obj) {
  # Per-group inline contribution to the linear predictor:
  #   intercept-only group:  alpha_{grp[i]}
  #   varying-slope group:   alpha_{grp[i]} + beta^{(grp)}_{x, grp[i]} x_{i,t}
  compose_inline_terms(specs, function(s) {
    sym <- term_symbols("re", s, obj)
    slopes <- if (length(s$slopes)) {
      paste0(sym[paste0("slope_i:", s$slopes)], " ",
             escape_math_text(s$slopes), "_{i,t}")
    }
    paste(c(sym[["alpha_i"]], slopes), collapse = " + ")
  })
}

#' @noRd
term_definition_rows <- function(obj, notation) {
  classes <- classify_obs_parameters(obj)
  row <- function(lhs, op, rhs) list(lhs = lhs, op = op, rhs = rhs)
  rows <- list()
  for (spec in classes$smooth) {
    sym <- term_symbols("smooth", spec, obj)
    vars_in <- spec_subscript(spec)
    rows[[length(rows) + 1L]] <- row(
      paste0(sym[["f"]], "(", vars_in, ")"), "=",
      paste0("\\sum_{k=1}^{K_{", spec_key(spec), "}} ", sym[["basis"]],
             " B_k(", vars_in, ")")
    )
  }
  for (spec in classes$gp) {
    sym <- term_symbols("gp", spec, obj)
    rows[[length(rows) + 1L]] <- row(
      paste0(sym[["f"]], "(", spec_subscript(spec), ")"), "\\sim",
      paste0("\\text{GP}\\left(0, ", gp_kernel_label(spec$cov), "(",
             sym[["rho"]], ", ", sym[["sd"]], ")\\right)")
    )
  }
  for (s in classes$mo) {
    v <- escape_math_text(s$var)
    sym <- term_symbols("mo", s, obj)
    # Cumulative step transform from a Dirichlet simplex over the D-1
    # step increments (Burkner & Charpentier 2020). The priors
    # section states the simplex's prior.
    rows[[length(rows) + 1L]] <- row(
      paste0(sym[["m"]], "(", v, ")"), "=",
      paste0("(D_{", v, "} - 1) \\sum_{j=1}^{", v, "} ", sym[["zeta_j"]])
    )
  }
  for (s in classes$me) {
    v <- escape_math_text(s$var)
    sdv <- escape_math_text(if (!is.na(s$sdvar)) s$sdvar else "se")
    sym <- term_symbols("me", s, obj)
    # Observation layer: noisy x_i is centred on the latent tilde{x}_i
    # with known SD sdvar_i (data). Latent layer: tilde{x}_i drawn
    # from a population-level Normal with hyper-mean and hyper-SD.
    rows[[length(rows) + 1L]] <- row(
      paste0(v, "_{i,t}"), "\\sim",
      paste0("\\text{Normal}\\!\\left(\\tilde{", v, "}_{i,t}, ", sdv,
             "_{i,t}\\right)")
    )
    rows[[length(rows) + 1L]] <- row(
      paste0("\\tilde{", v, "}_{i,t}"), "\\sim",
      paste0("\\text{Normal}\\!\\left(", sym[["mu"]], ", ", sym[["sd"]],
             "\\right)")
    )
  }
  for (s in classes$re) {
    sym <- term_symbols("re", s, obj)
    if (!s$has_slope) {
      rows[[length(rows) + 1L]] <- row(
        sym[["alpha"]], "\\sim",
        paste0("\\text{Normal}(0, ", sym[["sd"]], ")")
      )
      next
    }
    # Varying slopes: joint MVNormal over (alpha, beta_x, ...) with
    # covariance built from per-coefficient SDs and the correlation
    # matrix whose prior the priors section states.
    rows[[length(rows) + 1L]] <- row(
      paste0("(", paste(c(sym[["alpha"]], sym[paste0("slope:", s$slopes)]),
                        collapse = ", "), ")^\\top"),
      "\\sim",
      paste0("\\text{MVNormal}\\!\\left(\\mathbf{0}, ", sym[["Sigma"]],
             "\\right)")
    )
    sds <- paste(c(sym[["sd_alpha"]], sym[paste0("sd_slope:", s$slopes)]),
                 collapse = ", ")
    rows[[length(rows) + 1L]] <- row(
      sym[["Sigma"]], "=",
      paste0("\\text{diag}(", sds, ")\\,", sym[["Omega"]],
             "\\,\\text{diag}(", sds, ")")
    )
  }
  rows
}

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
#' @noRd
trend_linpred_terms <- function(obj) {
  if (!methods_md_has_latent_trend(obj)) return(character(0L))
  c(
    if (has_linear_terms(trend_side_view(obj))) "\\mu^{(\\eta)}_{i,t}",
    "\\eta_{i,t}"
  )
}

#' @noRd
has_linear_terms <- function(obj) {
  !is.null(obj$prior) && any(lengths(classify_obs_parameters(obj)) > 0L)
}


# ---------------------------------------------------------------
# Family-kind predicates
# ---------------------------------------------------------------
# Three orthogonal questions, each with one shared predicate in
# `R/families.R`:
#   * `uses_closure_unit_layout()`: the family takes per-visit rows
#     keyed by a unit.
#   * `is_closure_unit_family()`: the family models a detection
#     process over those visits. Only occ and nmix do.
#   * `is_multi_response_family()`: the family's likelihood is one
#     joint density per unit (mvn / mvt / diri / multi / categ).
#
# Whether the fit indexes seasons is asked of
# `is_multi_season_family()`, which reads the closure-unit grouping
# the likelihood was built on.




# ---------------------------------------------------------------
# Shared helpers for latent-trend renderers
# ---------------------------------------------------------------

#' @noRd
trend_grouping_var <- function(obj) {
  spec <- first_trend_spec(obj)
  gr <- spec$gr
  if (!named_var(gr)) NULL else gr
}

#' @noRd
sigma_symbol <- function(gr = NULL) {
  if (is.null(gr)) {
    "\\boldsymbol{\\Sigma}"
  } else {
    paste0("\\boldsymbol{\\Sigma}_{", gr, "}")
  }
}

# Factor-mode awareness for the dynamics symbols. When n_lv > 0,
# the trend dynamics live on the latent factors
# (`\tilde\eta_{k,t}`) and are mapped to the per-series state
# `\eta_{i,t}` through the loadings (see `factor_model_rows`).
# Outside factor mode the symbols collapse to the original
# per-series form so the diff is invisible.

#' @noRd
time_subscript <- function(lag) {
  # Build the time subscript as a character of length(lag).
  # Vector input arrives from AR / VAR / ARMA with multiple lags.
  ifelse(lag == 0L, "t", paste0("t-", lag))
}

#' @noRd
trend_eta <- function(obj, lag = 0L) {
  is_factor <- methods_md_has_factor_model(obj)
  idx <- if (is_factor) "k" else "i"
  tilde <- if (is_factor) "\\tilde" else ""
  paste0(tilde, "\\eta_{", idx, ",", time_subscript(lag), "}")
}

#' @noRd
trend_eta_vec <- function(obj, lag = 0L) {
  is_factor <- methods_md_has_factor_model(obj)
  body <- if (is_factor) {
    "\\tilde{\\boldsymbol{\\eta}}"
  } else {
    "\\boldsymbol{\\eta}"
  }
  paste0(body, "_{", time_subscript(lag), "}")
}

#' @noRd
trend_eps <- function(obj) {
  is_factor <- methods_md_has_factor_model(obj)
  idx <- if (is_factor) "k" else "i"
  tilde <- if (is_factor) "\\tilde" else ""
  paste0(tilde, "\\epsilon^{(\\eta)}_{", idx, ",t}")
}

#' @noRd
trend_eps_lag <- function(obj, lag) {
  is_factor <- methods_md_has_factor_model(obj)
  idx <- if (is_factor) "k" else "i"
  tilde <- if (is_factor) "\\tilde" else ""
  paste0(tilde, "\\epsilon^{(\\eta)}_{", idx, ",t-", lag, "}")
}

#' @noRd
trend_eps_vec <- function(obj) {
  is_factor <- methods_md_has_factor_model(obj)
  body <- if (is_factor) {
    "\\tilde{\\boldsymbol{\\epsilon}}"
  } else {
    "\\boldsymbol{\\epsilon}"
  }
  paste0(body, "_t")
}

#' @noRd
innovation_rows <- function(obj, is_vector) {
  sd <- innovation_sd(obj)
  sd_vec <- innovation_sd(obj, vector = TRUE)
  gr <- trend_grouping_var(obj)
  form <- innovation_form(obj, is_vector)
  eps_vec <- trend_eps_vec(obj)

  # Heavy-tailed innovations report as a Student-t rather than a normal,
  # with the degrees of freedom carried alongside the scale. The
  # univariate row still uses the multivariate form when several series
  # share one process, because the shared tail is what distinguishes it
  # from independent per-series heavy tails.
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
      rhs = paste0(mvn, sigma_symbol(gr), ")")
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
  list(cov_decomposition_row(sigma_symbol(), sd, ""))
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
                    "^{2 \\Delta t_{i,t}}) / (1 - ", rho, "^2)}"))
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

#' @noRd
diag_cov_text <- function(sd) {
  if (identical(sd, "1")) return("\\mathbf{I}")
  # Under MGP shrinkage the variances are the shrinkage products.
  if (identical(sd, "\\sqrt{\\Psi_k}")) return("\\text{diag}(\\boldsymbol{\\Psi})")
  paste0("\\text{diag}(", sd, "^2)")
}

#' @noRd
cov_decomposition_row <- function(lhs, sd, omega_sub) {
  omega <- paste0("\\boldsymbol{\\Omega}", omega_sub)
  list(
    lhs = lhs, op = "=",
    rhs = if (identical(sd, "1")) omega else paste0(
      "\\text{diag}(", sd, ") ", omega, " \\text{diag}(", sd, ")"
    )
  )
}

#' @noRd
hierarchical_cor_rows <- function(obj, sd) {
  # Hierarchical residual correlation decomposition emitted when
  # the user supplies `gr` to AR() / VAR().
  gr <- trend_grouping_var(obj)
  sym <- trend_symbols(obj)
  list(
    list(
      lhs = paste0("\\boldsymbol{\\Omega}_{", gr, "}"),
      op  = "=",
      rhs = paste0(
        sym[["alpha_cor_trend"]], " ", sym[["L_Omega_global_trend"]],
        " + (1 - ", sym[["alpha_cor_trend"]], ") ",
        sym[["L_deviation_group_trend"]]
      )
    ),
    cov_decomposition_row(sigma_symbol(gr), sd, paste0("_{", gr, "}"))
  )
}

#' Symbols for the trend parameters whose notation depends on the model
#'
#' The equations, the glossary and the priors section each name these
#' parameters, and they take the symbol from here.
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
  # Each lag's coefficient takes the process index, its shared form
  # drops it, and a hierarchical fit adds a mean and an SD per lag.
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
    alpha_cor_trend = "\\alpha_{cor}",
    L_Omega_global_trend = "\\boldsymbol{\\Omega}_{\\text{global}}",
    L_deviation_group_trend = paste0(
      "\\boldsymbol{\\Omega}_{", gr, ", \\text{local}}"
    ),
    sigma_trend = if (is.null(idx)) "\\sigma_\\eta" else {
      paste0("\\sigma_{\\eta,", idx, "}")
    },
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

#' The autoregressive coefficient symbols the dynamics are written with
#'
#' A shared coefficient is one value for every process and carries no
#' process index.
#' @noRd
ar_coef_symbols <- function(obj) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  shared <- any(startsWith(obj$prior$class, "shared_ar"))
  lag_symbol("\\phi", lags, if (!shared) trend_process_index(obj))
}

#' The moving-average terms of a trend's dynamics
#' @noRd
ma_terms <- function(obj) {
  if (!trend_has_ma(obj)) return(character(0L))
  ma_lags <- obj$trend_metadata$ma_lags
  paste(
    paste0(trend_symbols(obj)[paste0("theta", ma_lags, "_trend")], " ",
           trend_eps_lag(obj, ma_lags)),
    collapse = " + "
  )
}


# ---------------------------------------------------------------
# Latent registry
# ---------------------------------------------------------------
# Each entry returns a list of {lhs, op, rhs} rows for the
# dynamics of eta_{i,t} and its innovation distribution. The
# composer appends these rows to the model's align* block.

#' @noRd
methods_md_latent_registry <- function() {
  list(
    None  = NULL,
    RW    = render_latent_rw,
    AR    = render_latent_ar,
    VAR   = render_latent_var,
    CAR   = render_latent_car,
    ZMVN  = render_latent_zmvn,
    PW    = render_latent_pw
  )
}

#' @noRd
latent_dynamics_rows <- function(obj, notation) {
  tt <- obj$trend_metadata$trend_type
  if (is.null(tt) || identical(tt, "None") || identical(tt, "none")) {
    return(list())
  }
  reg <- methods_md_latent_registry()
  renderer <- reg[[tt]]
  if (is.null(renderer)) {
    return(list(list(
      lhs = "\\eta_{i,t}",
      op  = "\\sim",
      rhs = paste0("\\text{", tt, "}")
    )))
  }
  renderer(obj, notation)
}

#' @noRd
render_latent_rw <- function(obj, notation) {
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

#' @noRd
render_latent_ar <- function(obj, notation) {
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

#' @noRd
render_latent_var <- function(obj, notation) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  rhs <- paste(
    paste0("\\boldsymbol{\\Phi}_{", lags, "} ",
             trend_eta_vec(obj, lag = lags)),
    collapse = " + "
  )
  rhs <- paste0(rhs, " + ", trend_eps_vec(obj))
  if (trend_has_ma(obj)) {
    rhs <- paste0(rhs, " + ", paste(
      paste0("\\boldsymbol{\\Theta}_{", obj$trend_metadata$ma_lags,
             "} \\boldsymbol{\\epsilon}_{",
             time_subscript(obj$trend_metadata$ma_lags), "}"),
      collapse = " + "
    ))
  }
  c(
    list(list(lhs = trend_eta_vec(obj), op = "=", rhs = rhs)),
    innovation_rows(obj, is_vector = TRUE)
  )
}

#' @noRd
render_latent_car <- function(obj, notation) {
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste0(
        trend_symbols(obj)[["ar1_trend"]], "^{\\Delta t_{i,t}} ",
        trend_eta(obj, lag = 1L),
        " + ", trend_eps(obj)
      )
    )),
    innovation_rows(obj, is_vector = FALSE)
  )
}

#' @noRd
render_latent_zmvn <- function(obj, notation) {
  gr <- trend_grouping_var(obj)
  cov <- if (trend_samples_cor(obj)) {
    sigma_symbol(gr)
  } else {
    diag_cov_text(innovation_sd(obj, vector = TRUE))
  }
  rows <- list(list(
    lhs = trend_eta_vec(obj),
    op  = "\\sim",
    rhs = paste0("\\text{MVNormal}(\\mathbf{0}, ", cov, ")")
  ))
  c(rows, covariance_rows(obj))
}

#' @noRd
render_latent_pw <- function(obj, notation) {
  # PW does not use n_lv (factor mode incompatible at validator),
  # so the symbol stays \eta_{i,t} unconditionally. The priors
  # section states the priors on k, m and delta.
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

#' @noRd
pw_glossary <- function(obj) {
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


# ---------------------------------------------------------------
# Factor-model block
# ---------------------------------------------------------------
# Triggered when n_lv > 0. Emits the loadings decomposition
# eta_{i,t} = sum_k Z_{i,k} tilde-eta_{k,t}, the Z prior (one of
# iid / kernel-only / pure-MGP / kernel + MGP / partial-Z /
# fully-fixed), and the QR thin-identification annotation
# (skipped when trend_map pins Z). Mirrors the Stan emission in
# generate_factor_model() / make_loadings_prior_stanvars().

# `detect_factor_n_lv()` returns a count only where the user asked for
# latent factors. The raw `n_lv` slot counts the trend's latent
# columns, which every fit has, and a description built from that slot
# described a `Z` the model never sampled, with a prior and a QR
# annotation to match.
#' @noRd
methods_md_has_factor_model <- function(obj) {
  !is.null(detect_factor_n_lv(obj))
}

#' @noRd
factor_model_rows <- function(obj, notation) {
  n_lv <- detect_factor_n_lv(obj)
  if (is.null(n_lv)) return(list())
  fixed_Z <- obj$trend_metadata$fixed_Z
  spec <- first_trend_spec(obj)
  loadings_spec <- spec$loadings_prior_spec

  # Factor-model decomposition is the sampled parameterisation
  # eta = sum_k Z * tilde-eta. The post-hoc thin-QR rotation of
  # (Z, tilde-eta) is post-processing for ordinate() / biplots
  # and is not part of fitting the model, so it does not appear
  # here.
  rows <- list(list(
    lhs = "\\eta_{i,t}",
    op  = "=",
    rhs = paste0(
      "\\sum_{k=1}^{", n_lv, "} Z_{i,k} \\tilde\\eta_{k,t}"
    )
  ))
  rows <- c(rows, loadings_prior_rows(loadings_spec, fixed_Z))
  rows
}

#' @noRd
loadings_prior_rows <- function(spec, fixed_Z) {
  if (!is.null(fixed_Z)) {
    return(list(list(
      lhs = "Z_{i,k}",
      op  = "=",
      rhs = "\\text{fixed by \\texttt{trend\\_map}}"
    )))
  }
  # The priors section states the stored prior on free loadings.
  if (is.null(spec)) return(list())
  traits <- loadings_spec_traits(spec)

  rows <- list()
  if (traits$kernel) {
    rows <- c(rows, kernel_assembly_rows(spec))
  }
  if (traits$mgp) {
    rows <- c(rows, mgp_shrinkage_rows())
  }
  rows <- c(rows, z_column_prior_row(traits$kernel))
  rows
}

#' @noRd
z_column_prior_row <- function(has_kernel) {
  # Z is drawn at unit scale in every branch emitted by
  # make_loadings_prior_stanvars(). A kernel correlates each column
  # across series; otherwise the priors section states the stored
  # prior on Z. Under MGP the column scale enters through the factor
  # innovations.
  if (!has_kernel) return(list())
  list(list(
    lhs = "Z_{\\cdot,k}",
    op  = "\\sim",
    rhs = "\\text{MVNormal}(\\mathbf{0}, L_\\Phi L_\\Phi^\\top)"
  ))
}

#' @noRd
kernel_assembly_rows <- function(spec) {
  # Phi = (prod_d exp(-d / theta_d)) * gp_exponential(features;
  # theta_features). Per R/stan_assembly.R:3241-3268; one factor
  # per supplied distance matrix, plus the features GP factor
  # when N_features_trend > 0.
  has_features <- loadings_spec_traits(spec)$features
  dnames <- names(spec$distance_mats) %||% character(0L)
  terms <- character(0L)
  for (nm in dnames) {
    terms <- c(terms, paste0(
      "\\exp(-d_{", nm, "} / \\theta_{d_{", nm, "}})"
    ))
  }
  if (has_features) {
    terms <- c(terms, paste0(
      "\\text{GP}_{\\text{exp}}(\\text{features}; 1, ",
      "\\boldsymbol{\\theta}_{\\text{features}})"
    ))
  }
  rhs <- paste(terms, collapse = " \\odot ")
  list(
    list(
      lhs = "\\boldsymbol{\\Phi}",
      op  = "=",
      rhs = rhs
    ),
    list(
      lhs = "L_\\Phi",
      op  = "=",
      rhs = "\\text{Cholesky}(\\boldsymbol{\\Phi})"
    )
  )
}

#' @noRd
mgp_shrinkage_rows <- function() {
  # The priors section states the priors on each increment.
  list(list(
    lhs = "\\Psi_k",
    op  = "=",
    rhs = "\\prod_{l \\le k} \\varrho_l"
  ))
}


# ---------------------------------------------------------------
# Priors section
# ---------------------------------------------------------------
# One align* block. One row per row of object$prior whose prior
# string is non-empty and whose class maps to a renderable
# symbol. Rows are ordered: brms-default rows in their table
# order, then mvgam-emitted rows tagged source = "mvgam".

#' @noRd
render_priors_section <- function(ctx) {
  obj <- ctx$object
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) {
    return("## Priors\n\n(no prior table on this fit)")
  }
  # The trend view gives the trend's brms rows the class names its
  # term extractors match on.
  view <- trend_side_view(obj)
  on_trend <- grepl("_trend$", prior$class)
  symbols <- trend_symbols(obj)
  rows <- c(
    prior_align_rows(obj, prior[!on_trend, , drop = FALSE], symbols),
    prior_align_rows(view, view$prior, symbols)
  )
  if (length(rows) == 0L) {
    return("## Priors\n\n(all parameters are improper flat)")
  }
  paste(c("## Priors", "", align_block(rows)), collapse = "\n")
}

#' One aligned row per proper prior in one predictor's table
#'
#' @param obj The `mvgam` object, or a view of it, whose formula owns
#'   the table's terms.
#' @param prior Rows of its prior table.
#' @param symbols The model's `trend_symbols()`.
#' @return A list of `align_block()` rows, possibly empty.
#' @noRd
prior_align_rows <- function(obj, prior, symbols) {
  if (is.null(prior) || nrow(prior) == 0L) return(list())

  # brms emits two row kinds per parameter class:
  #   1. an "umbrella" row tagged source = "default" / "user" with
  #      the prior text but empty coef / group
  #   2. one or more "(vectorized)" rows with empty prior text but
  #      the specific coef / group labels
  # Backfill the umbrella prior text onto matching vectorized rows
  # so each row carries both the prior expression and the label.
  prior <- backfill_umbrella_priors(prior)

  # Each `b` row learns the term that owns it. A smooth's
  # unpenalised columns (`sx_1`, `sx_2`, ...) share the smooth's
  # hyperprior through the `sds` class and are dropped.
  prior$term_kind <- NA_character_
  obs_b <- prior$class == "b" & !nzchar(prior$dpar) &
    !nzchar(prior$nlpar %||% rep("", nrow(prior)))
  prior$term_kind[obs_b] <- obs_term_coef_kinds(obj, prior$coef[obs_b])
  prior <- prior[!prior$term_kind %in% "sm", , drop = FALSE]

  # Rewrite measurement-error b coefs from `me<var><sdvar>`
  # (brms's concatenated label) to just `me<var>` so the symbol
  # formatter emits `\\beta^{(me)}_<var>` rather than the
  # opaque `\\beta^{(me)}_<varsdvar>`. The me_specs from the
  # formula walker carry both var and sdvar so we can dissect
  # unambiguously.
  me_specs <- obs_me_specs_from_formula(obj)
  if (length(me_specs) > 0L) {
    for (s in me_specs) {
      if (is.na(s$sdvar)) next
      old_coef <- paste0("me", s$var, s$sdvar)
      mask <- prior$class == "b" & prior$coef == old_coef
      if (any(mask)) {
        prior$coef[mask] <- paste0("me", s$var)
      }
    }
  }

  # brms names a GP's hyperparameters by term, `gpxhabA` for level
  # `A` of `gp(x, by = hab)`. The page names them as the model does.
  for (sp in obs_gp_specs_from_formula(obj)) {
    base <- paste0("gp", brms_rename(paste(sp$vars, collapse = "")),
                   if (!is.na(sp$by)) brms_rename(sp$by))
    hit <- prior$class %in% c("lscale", "sdgp") &
      startsWith(prior$coef, base)
    level <- substring(prior$coef[hit], nchar(base) + 1L)
    prior$coef[hit] <- paste0(
      "gp", gp_subscript(sp), ifelse(nzchar(level), paste0(" = ", level), "")
    )
  }

  # Drop rows still missing a prior string after backfill: those
  # had no umbrella, so they are improper flat by default.
  keep <- !is.na(prior$prior) & nzchar(prior$prior)
  prior <- prior[keep, , drop = FALSE]
  if (nrow(prior) == 0L) {
    return(list())
  }

  # When both umbrella and specific rows survive for the same
  # (class, dpar, nlpar, resp) tuple, drop the umbrella. The
  # specifics carry the actual labels; the umbrella renders as
  # a generic placeholder (e.g. `\\sigma^{(gp)}_j` next to
  # `\\sigma^{(gp)}_x`). Scoping by dpar / nlpar / resp matters
  # under distributional regression, nl formulas, and mvbind --
  # an umbrella for response y1 must not be dropped just because
  # response y2 has a specific row.
  vec_classes <- c(
    "Intercept", "b", "sd", "sds", "L", "cor",
    "sdgp", "lscale", "meanme", "sdme", "simo"
  )
  drop <- logical(nrow(prior))
  scope_dpar <- prior$dpar %||% rep("", nrow(prior))
  scope_nlpar <- prior$nlpar %||% rep("", nrow(prior))
  scope_resp <- prior$resp %||% rep("", nrow(prior))
  scope_key <- paste(
    prior$class, scope_dpar, scope_nlpar, scope_resp,
    sep = "\x1f"
  )
  for (k in unique(scope_key[prior$class %in% vec_classes])) {
    k_rows <- scope_key == k
    has_specific <- any(
      k_rows & (nzchar(prior$coef) | nzchar(prior$group))
    )
    if (has_specific) {
      drop <- drop | (k_rows & !nzchar(prior$coef) &
                        !nzchar(prior$group))
    }
  }
  prior <- prior[!drop, , drop = FALSE]
  if (nrow(prior) == 0L) {
    return(list())
  }

  # For class `sd` with both a group-level row (group="g", coef="")
  # and a coef-level row (group="g", coef="X"), keep the coef-level
  # row only. Both describe the same Stan parameter `sd_g__X`.
  is_sd <- prior$class == "sd"
  groups <- unique(prior$group[is_sd & nzchar(prior$group)])
  drop <- logical(nrow(prior))
  for (g in groups) {
    g_rows <- is_sd & prior$group == g
    if (any(g_rows & nzchar(prior$coef))) {
      drop <- drop | (g_rows & !nzchar(prior$coef))
    }
  }
  prior <- prior[!drop, , drop = FALSE]

  # For an intercept-only group (single sd row, coef == "Intercept"),
  # blank out the coef so format_parameter_symbol emits the plain
  # `\sigma_{grp}` form. The `\sigma^{(\alpha)}_{grp}` superscript
  # is only needed to disambiguate against slope SDs under the same
  # group; one-row groups have nothing to disambiguate against.
  is_sd <- prior$class == "sd"
  for (g in unique(prior$group[is_sd & nzchar(prior$group)])) {
    g_rows <- which(is_sd & prior$group == g)
    if (length(g_rows) == 1L &&
          identical(prior$coef[g_rows], "Intercept")) {
      prior$coef[g_rows] <- ""
    }
  }

  # Order: defaults first, then user overrides, then mvgam lifts.
  src <- prior$source %||% rep("", nrow(prior))
  ord <- order(
    match(src, c("default", "(vectorized)", "user", "mvgam"),
           nomatch = 99L)
  )
  prior <- prior[ord, , drop = FALSE]

  rows <- list()
  for (i in seq_len(nrow(prior))) {
    row <- prior[i, , drop = FALSE]
    sym <- if (row$class %in% names(symbols)) {
      symbols[[row$class]]
    } else if (!grepl("_trend$", row$class)) {
      # The trend view tags the brms rows it renders. The rows mvgam
      # writes keep their `_trend` names and their own symbols.
      response_symbol(obj, format_parameter_symbol(row))
    } else {
      format_parameter_symbol(row)
    }
    if (is.null(sym)) next
    # The MGP shapes are Stan data, and the glossary gives their values.
    dist <- format_prior_distribution(
      row$prior, c(symbols, mgp_a1 = "a_1", mgp_a2 = "a_2")
    )
    if (is.null(dist)) next
    rows[[length(rows) + 1L]] <- list(
      lhs = sym, op = "\\sim", rhs = dist
    )
  }
  rows
}

#' @noRd
render_implementation_section <- function(ctx) {
  obj <- ctx$object
  fn <- if (inherits(obj, "jsdgam")) "jsdgam" else "mvgam"
  obs <- formula_text(obj$formula)
  trend <- trend_formula_text(obj)
  fam <- family_call_text(obj$family)
  data_name <- obj$data.name
  if (is.null(data_name) || !nzchar(data_name) ||
      identical(data_name, "<inline data>")) {
    data_name <- "data"
  }
  # Match the canonical brms / rstan / cmdstanr sampler arg names
  # so the rendered block is a copy-paste-ready call.
  info <- extract_implementation_info(obj)
  call_lines <- c(paste0("  formula       = ", obs))
  if (!is.null(trend)) {
    call_lines <- c(call_lines, paste0("  trend_formula = ", trend))
  }
  call_lines <- c(
    call_lines,
    paste0("  data          = ", data_name),
    paste0("  family        = ", fam),
    paste0("  backend       = \"", info$backend, "\"")
  )
  if (!is.na(info$chains)) {
    call_lines <- c(call_lines, paste0("  chains        = ", info$chains))
  }
  if (!is.na(info$iter)) {
    call_lines <- c(call_lines, paste0("  iter          = ", info$iter))
  }
  if (!is.na(info$warmup)) {
    call_lines <- c(call_lines, paste0("  warmup        = ", info$warmup))
  }
  # Within-chain threading. mvgam exposes it as `threads_per_chain`
  # (R/mvgam_core.R); the Stan slot uses the same name.
  if (!is.na(info$threads) && info$threads > 1L) {
    call_lines <- c(call_lines, paste0(
      "  threads_per_chain = ", info$threads
    ))
  }
  # NUTS sampler control: only surface when the user moved them
  # off Stan defaults (adapt_delta = 0.8, max_treedepth = 10), so
  # readers see what was tuned rather than what wasn't.
  control_lines <- character(0L)
  if (!is.na(info$adapt_delta) && info$adapt_delta != 0.8) {
    control_lines <- c(control_lines, paste0(
      "    adapt_delta = ", info$adapt_delta
    ))
  }
  if (!is.na(info$max_treedepth) && info$max_treedepth != 10L) {
    control_lines <- c(control_lines, paste0(
      "    max_treedepth = ", info$max_treedepth
    ))
  }
  if (length(control_lines) > 0L) {
    call_lines <- c(call_lines, paste0(
      "  control       = list(\n",
      paste(control_lines, collapse = ",\n"),
      "\n  )"
    ))
  }
  if (!is.na(info$init) && nzchar(info$init)) {
    call_lines <- c(call_lines, paste0(
      "  init          = \"", info$init, "\""
    ))
  }
  if (!identical(info$algorithm, "sampling") &&
        !identical(info$algorithm, "none") &&
        nzchar(info$algorithm)) {
    call_lines <- c(call_lines, paste0(
      "  algorithm     = \"", info$algorithm, "\""
    ))
  }
  # Align trailing commas: every line except the last gets one.
  call_lines <- paste0(
    call_lines, c(rep(",", length(call_lines) - 1L), "")
  )
  prose <- implementation_prose(fn, info)
  # Plain fenced block (no language tag) so Pandoc renders the
  # call as verbatim with no syntax highlighting.
  body <- c(
    prose,
    "",
    "```",
    paste0(fn, "("),
    call_lines,
    ")",
    "```"
  )
  paste(c("## Implementation", "", body), collapse = "\n")
}

#' @noRd
extract_implementation_info <- function(obj) {
  # A fit records what it ran under and a prefit records
  # `algorithm = "none"`. Both come from the object's constructor.
  backend <- obj$backend
  algorithm <- obj$algorithm
  checkmate::assert_choice(backend, backend_choices(),
                           .var.name = "obj$backend")
  checkmate::assert_choice(algorithm, c(algorithm_choices(), "none"),
                           .var.name = "obj$algorithm")
  # Reuse the shared sampling-args extractor from how_to_cite.R.
  # `extract_sampling_info()` reads `obj$fit@stan_args` and
  # returns (chains, warmup, iter, threads, adapt_delta,
  # max_treedepth, init), or NULL for variational / Laplace
  # / pathfinder fits with empty stan_args.
  sampling <- extract_sampling_info(obj)
  if (is.null(sampling)) {
    sampling <- list(
      chains = NA_integer_, warmup = NA_integer_,
      iter = NA_integer_, threads = NA_integer_,
      adapt_delta = NA_real_, max_treedepth = NA_integer_,
      init = NA_character_
    )
  }
  # The specification the user wrote outranks the value Stan recorded,
  # which for list-valued and Pathfinder starts is a temporary file
  # path that would not replay on another machine.
  if (!is.null(obj$init)) {
    sampling$init <- printable_init(obj$init)
  }
  # Each version is the one the fit recorded when it was built. The
  # session describing it may have other versions installed, and
  # reporting those would describe software the model never met.
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

#' @noRd
formula_text <- function(f) {
  if (is.null(f)) return("NULL")
  # brmsformula / mvbrmsformula deparse to an unreadable
  # `structure(list(formula = y ~ x, pforms = list(...)))`
  # by default. Reconstruct a `bf(...)` call instead so
  # the Implementation code block is copy-paste-ready.
  if (inherits(f, "mvbrmsformula")) {
    # Compose `bf(yA ~ x) + bf(yB ~ x) +
    # set_rescor(FALSE)` by recursing into each per-
    # response brmsformula in $forms and appending the rescor
    # flag when set.
    bfs <- vapply(f$forms, formula_text, character(1L))
    out <- paste(bfs, collapse = " + ")
    if (isFALSE(f$rescor)) {
      out <- paste0(out, " + set_rescor(FALSE)")
    } else if (isTRUE(f$rescor)) {
      out <- paste0(out, " + set_rescor(TRUE)")
    }
    return(out)
  }
  # The stored formula carries the pinned placeholder an empty
  # formula is built with, which the user never wrote.
  deparsed <- function(x) {
    strip_empty_obs_placeholder(
      paste(deparse(x, width.cutoff = 60L), collapse = " ")
    )
  }
  if (inherits(f, c("brmsformula", "bform"))) {
    main <- if (!is.null(f$formula)) f$formula else f
    # A distributional formula names its parameter on its own left
    # side, as `bf()` takes it.
    parts <- c(deparsed(main),
               vapply(f$pforms %||% list(), deparsed, character(1L),
                      USE.NAMES = FALSE))
    # brms records the nl status as an attribute on the inner
    # formula slot (`attr(f$formula, "nl")`), not as `f$nl`.
    # Also accept `f$nl` for forward-compat with any wrapper
    # that promotes it to a top-level slot.
    is_nl <- isTRUE(f$nl) ||
      isTRUE(attr(f$formula, "nl"))
    if (is_nl) {
      parts <- c(parts, "nl = TRUE")
    }
    # A response of a multivariate formula names its own family in
    # its `bf()`, and a call without it fits another model.
    if (!is.null(f$family)) {
      parts <- c(parts, paste0("family = ", family_call_text(f$family)))
    }
    return(paste0("bf(", paste(parts, collapse = ", "), ")"))
  }
  deparsed(f)
}

#' @noRd
trend_formula_text <- function(obj) {
  tcall <- obj$trend_call
  if (is.null(tcall)) return(NULL)
  formula_text(tcall)
}

#' @noRd
family_call_text <- function(family) {
  checkmate::assert_class(family, "family")
  # `family$family` collapses every brms customfamily to the
  # literal "custom"; the user-visible constructor lives on
  # `family$name`. resolve_family_name() routes through both.
  fam <- resolve_family_name(family)
  link <- family$link
  # nmix() carries the variant in its name (nmix_royle_nichols /
  # nmix_poisson_poisson / nmix_poisson_binomial). Reconstruct
  # the user-facing nmix("...") call rather than printing the
  # internal name.
  if (grepl("^nmix_", fam)) {
    variant <- sub("^nmix_", "", fam)
    return(paste0("nmix(\"", variant, "\")"))
  }
  default_link <- switch(
    fam,
    poisson = "log", bernoulli = "logit", binomial = "logit",
    gaussian = "identity", student = "identity",
    gamma = "inverse", lognormal = "identity",
    beta = "logit", negbinomial = "log",
    occ = "logit",
    diri = "identity", multi = "identity", categ = "identity",
    mvn = "identity", mvt = "identity",
    tweedie = "log",
    beta_nb = "log",
    nmix = "log",
    "identity"
  )
  if (identical(link, default_link)) {
    paste0(fam, "()")
  } else {
    paste0(fam, "(link = \"", link, "\")")
  }
}

#' @noRd
backfill_umbrella_priors <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(prior)
  dpar <- prior$dpar %||% rep("", nrow(prior))
  nlpar <- prior$nlpar %||% rep("", nrow(prior))
  resp <- prior$resp %||% rep("", nrow(prior))
  pri <- prior$prior %||% rep("", nrow(prior))
  has_text <- !is.na(pri) & nzchar(pri) & pri != "(flat)"
  # Umbrella rows: class set, dpar / nlpar / resp set or unset, coef
  # and group both empty, prior text non-empty.
  umbrella_mask <- has_text &
    !nzchar(prior$coef %||% "") &
    !nzchar(prior$group %||% "")
  umbrella <- prior[umbrella_mask, , drop = FALSE]
  if (nrow(umbrella) == 0L) return(prior)
  u_dpar <- umbrella$dpar %||% rep("", nrow(umbrella))
  u_nlpar <- umbrella$nlpar %||% rep("", nrow(umbrella))
  u_resp <- umbrella$resp %||% rep("", nrow(umbrella))
  for (i in seq_len(nrow(prior))) {
    if (has_text[i]) next
    matches <- umbrella$class == prior$class[i] &
      u_dpar == dpar[i] &
      u_nlpar == nlpar[i] &
      u_resp == resp[i]
    if (any(matches)) {
      prior$prior[i] <- umbrella$prior[which(matches)[1L]]
    }
  }
  prior
}

#' Map a single brms-prior-table row to its LaTeX math symbol.
#' Dispatches on `class`, with `coef`, `group`, and `dpar` modifying
#' the rendered subscript / superscript when set. Returns `NULL` for
#' unhandled classes; the prior-section walker drops `NULL` rows
#' from the output rather than rendering an opaque placeholder, so
#' a prior class with no dedicated renderer is silently skipped.
#'
#' Per-response scoping (mvbind / mvbrmsformula): when `row$resp` is
#' non-empty, the resolved symbol is wrapped with a `(<resp>)`
#' superscript via `apply_resp_superscript()` -- this is the only
#' place in the file that knows about resp on prior rows, so every
#' branch below stays oblivious to multi-response.
#' @noRd
format_parameter_symbol <- function(row) {
  sym <- format_parameter_symbol_base(row)
  if (is.null(sym)) return(NULL)
  resp <- row$resp %||% ""
  if (nzchar(resp)) sym <- apply_resp_superscript(sym, resp)
  sym
}

#' @noRd
apply_resp_superscript <- function(sym, r) {
  # Three cases for inserting `^{(<r>)}` into an already-formatted
  # symbol without producing the invalid `^{...}^{(...)}` double
  # superscript:
  #   1. Symbol already has a `^{(...)}` group (e.g. `\\alpha^{(sigma)}`
  #      from dpar, or `\\beta^{(\\text{mo})}_{x}` from monotonic):
  #      merge into that group as a comma-separated tag.
  #   2. Symbol has a subscript but no superscript (e.g. `\\beta_{x}`):
  #      insert `^{(<r>)}` between the base and the subscript so the
  #      conventional super-then-sub order survives.
  #   3. Plain symbol (e.g. `\\alpha`): append.
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

#' @noRd
format_parameter_symbol_base <- function(row) {
  cls <- row$class %||% ""
  coef <- row$coef %||% ""
  group <- row$group %||% ""
  dpar <- row$dpar %||% ""
  nlpar <- row$nlpar %||% ""

  if (identical(cls, "Intercept")) {
    # Ordinal threshold rows arrive as class "Intercept" with the
    # threshold index in `coef` (e.g. "1", "2", ..., "K-1") for
    # cumulative / sratio / cratio / acat families.
    if (nzchar(coef) && grepl("^[0-9]+$", coef)) {
      return(paste0("\\theta_{", coef, "}"))
    }
    if (nzchar(dpar)) {
      return(paste0("\\alpha^{(", dpar, ")}"))
    }
    if (nzchar(nlpar)) {
      return(paste0("\\alpha^{(", nlpar, ")}"))
    }
    return("\\alpha")
  }
  if (identical(cls, "b")) {
    if (nzchar(dpar)) {
      lbl <- if (nzchar(coef)) paste0(dpar, ",", coef) else dpar
      return(paste0("\\beta_{", escape_math_text(lbl), "}"))
    }
    # nl sub-formula coefficients: brms emits b_<nlpar>_<term>
    # priors with nzchar(nlpar). Carry the nlpar through as a
    # superscript so each nlpar's coefficient set is visually
    # distinct from the top-level beta_{term} family.
    if (nzchar(nlpar)) {
      base <- paste0("\\beta^{(", nlpar, ")}")
      if (nzchar(coef)) {
        return(paste0(base, "_{", escape_math_text(coef), "}"))
      }
      return(base)
    }
    if (nzchar(coef)) {
      kind <- row$term_kind %||% NA_character_
      if (kind %in% c("mo", "me")) {
        # A me() coef is `me<var><sdvar>`; the section rewrites it
        # to `me<var>` from the formula before this point.
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
  if (cls %in% c("sigma", "shape", "nu", "phi", "zi", "hu", "kappa")) {
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
    nm <- sub("^theta_dist_", "", cls)
    return(paste0("\\theta^{(", nm, ")}_{\\text{dist}}"))
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
  # Unrecognised class: render verbatim so the row still appears
  # (math-only spec, no silent drops).
  math_upright(cls)
}

#' @noRd
format_prior_distribution <- function(prior_str, symbols = NULL) {
  if (is.null(prior_str)) return(NULL)
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
    # An argument naming a sampled hyperparameter takes its symbol.
    # Any other literal `_` is escaped, as a bare `mgp_a1` would
    # render `mgp` subscript `a1`.
    args <- vapply(args, function(a) {
      if (a %in% names(symbols)) symbols[[a]] else escape_math_text(a)
    }, character(1L))
    args <- gsub("sqrt\\(([^()]*)\\)", "\\\\sqrt{\\1}", args)
    return(paste0(
      "\\text{", labels[[m[[2L]]]], "}(", paste(args, collapse = ", "), ")"
    ))
  }
  # Unrecognised distribution: render verbatim wrapped in \text{}
  # so the row still appears.
  paste0("\\text{", gsub("([{}_])", "\\\\\\1", s), "}")
}
