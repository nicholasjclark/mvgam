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
#' @param ... Currently ignored.
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
                             implementation = TRUE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(file, null.ok = TRUE)
  checkmate::assert_choice(notation, c("default", "brms"))
  checkmate::assert_flag(implementation)

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

#' @rdname methods_md
#' @export
methods_md.mvgam_prefit <- methods_md.mvgam

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
  fam <- obj$family
  fam_name <- resolve_family_name(fam) %||% "gaussian"
  link <- fam$link %||% "identity"
  dims <- describe_data_dimensions(obj)

  # Multi-response: list response columns explicitly so the
  # reader sees the mvbind structure (`yA`, `yB`) rather than
  # an opaque bold `Y` vector label.
  responses <- unname(response_columns(obj))
  resp_line <- if (length(responses) > 1L) {
    paste0(
      "$\\mathbf{Y} = (",
      paste0(responses, collapse = ", "),
      ")$: ", family_data_label(fam_name),
      " observed jointly per (i, t)"
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
    lines <- c(lines, paste0(dims, collapse = ", "))
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
  # Restrict to columns the model formula actually references.
  # Without this guard, every column in the user's data frame
  # surfaces here, including ones the model ignores -- which
  # misleads readers about what enters the linear predictor.
  used_vars <- formula_used_vars(obj)
  # Skip canonical panel keys plus the response column(s); they are
  # already covered by the dimensions line above.
  skip <- unique(c(resp_cols, "time", "series"))
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
    out <- c(out, paste0("- $", nm, "$: ", desc))
  }
  out
}

#' @noRd
formula_used_vars <- function(obj) {
  # Every variable referenced anywhere in the model spec:
  # obs formula (LHS + RHS, plus dpar / nlpar sub-formulas) and
  # the trend formula. Delegates to `mvgam_formula_predictors()`
  # in insight.mvgam.R for the obs side -- that function already
  # walks `$pforms`. Trend side gets the same `mvgam_rhs_predictors`
  # treatment.
  obs <- obj$formula
  obs_vars <- mvgam_formula_predictors(obs)
  # Every variable on the left-hand side, addition terms included,
  # since a `trials()` denominator is a column the model reads.
  resp_vars <- if (!is.null(obs)) {
    f <- if (inherits(obs, c("brmsformula", "bform", "mvbrmsformula"))) {
      obs$formula %||% obs
    } else obs
    if (inherits(f, "formula") && length(f) >= 3L) all.vars(f[[2L]])
    else character(0L)
  } else character(0L)
  tf <- obj$trend_formula %||% obj$trend_model$formula
  trend_vars <- mvgam_formula_predictors(tf)
  unique(c(obs_vars, resp_vars, trend_vars))
}

#' @noRd
describe_data_dimensions <- function(obj) {
  data <- obj$data %||% data.frame()
  n_obs <- nrow(data)
  n_series <- if (!is.null(data$series)) {
    length(unique(data$series))
  } else NA_integer_
  n_time <- if (!is.null(data$time)) {
    length(unique(data$time))
  } else NA_integer_
  parts <- character(0L)
  if (!is.na(n_obs) && n_obs > 0L) {
    parts <- c(parts, paste0("$N = ", n_obs, "$ observations"))
  }
  if (!is.na(n_series)) {
    parts <- c(parts, paste0("$S = ", n_series, "$ series"))
  }
  if (!is.na(n_time)) {
    parts <- c(parts, paste0("$T = ", n_time, "$ time points"))
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
  if (!methods_md_is_detection_family(obj)) return(character(0L))
  data <- obj$data %||% data.frame()
  if (nrow(data) == 0L) return(character(0L))
  # The columns that key a closure unit, default included. The
  # guard above establishes that this family has one, so the
  # accessor answers rather than returning NULL.
  ug <- closure_unit_key_vars(obj$family)
  ug <- intersect(ug %||% character(0L), names(data))
  if (!length(ug)) return(character(0L))
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
    Gamma = "positive real observations",
    beta = "proportions in (0, 1)",
    negbinomial = "non-negative integer counts",
    nb = "non-negative integer counts",
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
  fam <- obj$family
  fam_name <- resolve_family_name(fam) %||% "gaussian"
  link <- fam$link %||% "identity"

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
      rows[[length(rows) + 1L]] <- list(
        lhs = "\\boldsymbol{\\Omega}",
        op  = "\\sim",
        rhs = "\\text{LKJCorr}(\\eta)"
      )
      for (r in responses) {
        obj_r <- subset_obj_to_response(obj, r)
        mu_r <- paste0("\\mu^{(", r, ")}_{i,t}")
        rows[[length(rows) + 1L]] <- list(
          lhs = link_application(link, mu_r),
          op  = "=",
          rhs = linear_predictor_rhs(obj_r, notation)
        )
      }
    } else {
      # Slice the obj down to a single-response view per
      # response and reuse the existing helpers wholesale --
      # the slice filters the prior table so every downstream
      # extractor / symbol formatter sees the per-response
      # subset without any new threading.
      for (r in responses) {
        obj_r <- subset_obj_to_response(obj, r)
        mu_r <- paste0("\\mu^{(", r, ")}_{i,t}")
        rows[[length(rows) + 1L]] <- list(
          lhs = paste0(r, "_{i,t}"),
          op  = "\\sim",
          rhs = family_distribution_text(fam_name, mu_r, obj_r)
        )
        rows[[length(rows) + 1L]] <- list(
          lhs = link_application(link, mu_r),
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
  rows <- c(rows, term_definition_rows(obj, notation))
  rows <- c(rows, latent_dynamics_rows(obj, notation))
  rows <- c(rows, factor_model_rows(obj, notation))

  block <- align_block(rows)
  glossary <- model_glossary(obj)
  paste(c("## Model", "", block, "", glossary), collapse = "\n")
}

#' @noRd
dpar_linear_predictor_rows <- function(obj, notation) {
  # Distributional parameter sub-formulas (`bf(y ~ x, sigma ~ x)`)
  # emit prior rows with non-empty `dpar`. For each unique dpar
  # present, render one extra row in the model section showing
  # its own (linked) linear predictor. Default link map mirrors
  # brms's per-dpar default link (sigma -> log, phi -> log,
  # shape -> log, nu -> identity, ...). Unknown dpars fall
  # through to identity.
  prior <- obj$prior
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  dpars <- prior$dpar %||% rep("", nrow(prior))
  present <- unique(dpars[nzchar(dpars)])
  if (length(present) == 0L) return(list())
  visit_grain <- methods_md_is_closure_unit(obj)
  rows <- list()
  for (dp in present) {
    link <- dpar_default_link(dp, family = obj$family)
    sym <- dpar_symbol(dp, visit_grain = visit_grain)
    lhs <- link_application(link, sym)
    rhs <- dpar_predictor_rhs(prior, dp)
    rows[[length(rows) + 1L]] <- list(
      lhs = lhs, op = "=", rhs = rhs
    )
  }
  rows
}

#' @noRd
dpar_default_link <- function(dp, family = NULL) {
  # brms stashes each custom_family's per-dpar link as
  # `family$link_<dp>` (e.g. `nmix("poisson_poisson")$link_p
  # == "log"` while `nmix("poisson_binomial")$link_p == "logit"`).
  # Read from the family object when present; fall back to the
  # static map for the standard brms families that don't carry
  # the slot.
  if (!is.null(family)) {
    link_slot <- family[[paste0("link_", dp)]]
    if (!is.null(link_slot) && nzchar(link_slot)) return(link_slot)
  }
  switch(
    dp,
    sigma = "log", phi = "log", shape = "log", kappa = "log",
    nu = "identity", hu = "logit",
    zi = "logit", mu = "identity",
    p = "logit", r = "logit",
    "identity"
  )
}

#' @noRd
dpar_symbol <- function(dp, visit_grain = FALSE) {
  # Map common dpar names to their Greek / mathematical form.
  # `visit_grain = TRUE` swaps the `_{i,t}` subscript for
  # `_{i,j}` (closure-unit detection sub-formulas operate per
  # visit `j` within unit `i`).
  base <- switch(
    dp,
    sigma = "\\sigma", phi = "\\phi", shape = "\\alpha",
    kappa = "\\kappa", nu = "\\nu",
    hu = "\\pi_{\\text{hu}}", zi = "\\pi_{\\text{zi}}",
    mu = "\\mu",
    p = "p", r = "r",
    paste0("\\text{", dp, "}")
  )
  subscript <- if (visit_grain) "_{i,j}" else "_{i,t}"
  paste0(base, subscript)
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
      "\\beta_{", value, ",", co, "} \\, ", co, "_{i,t}"
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
  # Decorate each nlpar token. Use word boundaries so `b` in `bx`
  # is not rewritten when only `b` is the nlpar.
  for (np in nlpars) {
    pat <- paste0("\\b", np, "\\b")
    rhs_src <- gsub(pat, paste0(np, "_{i,t}"), rhs_src)
  }
  # Subscript bare data covariates `x` -> `x_{i,t}` so they look
  # like the rest of the math block. Skip already-subscripted
  # tokens via the same word-boundary trick (no `_{` immediately
  # after). Pull the covariate list from formula_used_vars and
  # rewrite only those (avoids touching constants / numbers).
  for (v in setdiff(formula_used_vars(obj), nlpars)) {
    pat <- paste0("\\b", v, "\\b(?!_\\{)")
    rhs_src <- gsub(pat, paste0(v, "_{i,t}"),
                     rhs_src, perl = TRUE)
  }
  # `*` is the multiplication marker; render as a thin space so
  # `a + b * env` becomes `a_{i,t} + b_{i,t} env_{i,t}` rather
  # than dropping the operator entirely.
  rhs_src <- gsub("\\s*\\*\\s*", " \\\\, ", rhs_src)
  rhs_src
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
      lhs = paste0(np, "_{i,t}"),
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

#' @noRd
model_glossary <- function(obj) {
  fam <- obj$family
  fam_name <- resolve_family_name(fam) %||% "gaussian"
  link <- fam$link %||% "identity"
  defs <- c(
    paste0("- $i$ indexes observations, $t$ indexes time")
  )
  defs <- c(defs, closure_unit_glossary(obj))
  defs <- c(defs, mv_custom_glossary(obj))
  if (is.null(closure_unit_family_kind(obj)) &&
        is.null(mv_custom_family_kind(obj))) {
    defs <- c(defs, paste0(
      "- $\\mu_{i,t}$: conditional mean of $",
      response_letter(obj), "_{i,t}$ on the ", link, "-link scale"
    ))
  }
  defs <- c(
    defs,
    "- $\\alpha$: population intercept",
    "- $\\beta_{j}$: population effect on covariate $j$"
  )
  prior <- obj$prior
  smooth_specs <- obs_smooth_specs_from_prior(prior)
  gp_specs <- get_gp_specs(obj)
  mo_specs <- obs_mo_specs_from_prior(prior)
  me_specs <- obs_me_specs_from_formula(obj)
  re_specs <- obs_re_specs_from_prior(prior)
  for (spec in smooth_specs) {
    sub <- spec_subscript(spec)
    sub_key <- spec_key(spec)
    in_phrase <- paste0(
      "$", paste(spec$vars, collapse = "$, $"), "$"
    )
    k_label <- if (!is.na(spec$k)) {
      paste0("$K_{", sub_key, "} = ", spec$k, "$")
    } else {
      paste0("$K_{", sub_key, "}$ (mgcv default)")
    }
    defs <- c(defs, paste0(
      "- $f_{", sub, "}$: ",
      basis_label(spec$bs, spec$fname),
      " in ", in_phrase, ", basis size ", k_label,
      ", smoothness $\\lambda_{", sub_key, "}$"
    ))
  }
  for (spec in gp_specs) {
    sub <- gp_subscript(spec)
    vars <- spec$vars
    dims_text <- paste(vars, collapse = ", ")
    by_text <- if (!is.null(spec$by) && !is.na(spec$by) &&
                    nzchar(spec$by)) {
      paste0(", stratified by $", spec$by, "$")
    } else ""
    k_text <- if (!is.null(spec$k) && !is.na(spec$k)) {
      paste0(", approximated with ", spec$k, " basis functions")
    } else {
      ", exact (full covariance kernel)"
    }
    kern_text <- gp_kernel_human_label(spec$cov %||% "exp_quad")
    rho_sym <- if (length(vars) > 1L) {
      paste0("$\\boldsymbol{\\rho}_{", sub, "}$")
    } else {
      paste0("$\\rho_{", sub, "}$")
    }
    defs <- c(defs, paste0(
      "- $f^{(\\text{gp})}_{", sub, "}$: Gaussian process in $",
      dims_text, "$", by_text, " with ", kern_text,
      " kernel, length scale ", rho_sym,
      " and marginal SD $\\sigma^{(\\text{gp})}_{", sub, "}$",
      k_text
    ))
  }
  for (s in mo_specs) {
    v <- s$var
    defs <- c(defs, paste0(
      "- $m_{", v, "}(", v,
      ")$: monotonic step transform of ordinal $", v,
      "$, built from a Dirichlet simplex $\\boldsymbol{\\zeta}_{",
      v, "}$ over the $D_{", v,
      "} - 1$ step increments and scaled by population effect ",
      "$\\beta^{(\\text{mo})}_{", v, "}$"
    ))
  }
  for (s in me_specs) {
    v <- s$var
    sdv <- if (!is.na(s$sdvar)) s$sdvar else "se"
    defs <- c(defs, paste0(
      "- $\\tilde{", v, "}_{i,t}$: latent true covariate ",
      "underlying noisy observation $", v,
      "_{i,t}$, with known per-observation measurement-error SD ",
      "$", sdv, "_{i,t}$ and population hyper-mean ",
      "$\\mu^{(\\text{me})}_{", v,
      "}$ and hyper-SD $\\sigma^{(\\text{me})}_{", v, "}$"
    ))
  }
  for (s in re_specs) {
    grp <- s$group
    if (!s$has_slope) {
      defs <- c(defs, paste0(
        "- $\\alpha_{", grp, "[i]}$: varying intercept across ",
        "levels of $", grp, "$ with hyper-SD $\\sigma_{", grp, "}$"
      ))
      next
    }
    slope_terms <- paste(
      paste0(
        "$\\beta^{(", grp, ")}_{", s$slopes, ", ", grp, "[i]}$"
      ),
      collapse = ", "
    )
    defs <- c(defs, paste0(
      "- $\\alpha_{", grp, "[i]}$, ", slope_terms,
      ": correlated varying intercept and slopes across ",
      "levels of $", grp, "$, jointly distributed as MVNormal ",
      "with covariance $\\boldsymbol{\\Sigma}_{", grp,
      "}$ built from per-coefficient SDs and LKJ-prior ",
      "correlation matrix $\\boldsymbol{\\Omega}_{", grp, "}$"
    ))
  }
  if (methods_md_has_latent_trend(obj)) {
    tt <- obj$trend_metadata$trend_type
    label <- trend_order_label(obj)
    is_factor <- methods_md_has_factor_model(obj)
    defs <- c(defs, paste0(
      "- $\\eta_{i,t}$: latent state at series $i$ time $t$"
    ))
    if (is_factor) {
      n_lv <- obj$trend_metadata$n_lv
      defs <- c(defs, paste0(
        "- $\\tilde\\eta_{k,t}$: latent ", label,
        " factor $k = 1, \\ldots, ", n_lv, "$ at time $t$"
      ))
      defs <- c(defs, paste0(
        "- $Z_{i,k}$: loading of series $i$ on factor $k$"
      ))
      defs <- c(defs, paste0(
        "- $\\tilde\\epsilon^{(\\eta)}_{k,t}$: factor ",
        "innovation, SD $\\sigma_\\eta$"
      ))
    } else {
      defs <- c(defs, paste0(
        "- $\\epsilon^{(\\eta)}_{i,t}$: process innovation, ",
        "SD $\\sigma_\\eta$"
      ))
    }
    if (identical(tt, "AR") || identical(tt, "VAR")) {
      defs <- c(defs, paste0(
        "- $\\phi_l$: autoregressive coefficient at lag $l$"
      ))
    }
    if (trend_has_ma(obj)) {
      defs <- c(defs, paste0(
        "- $\\theta_l$: moving-average coefficient at lag $l$"
      ))
    }
    if (identical(tt, "CAR")) {
      defs <- c(defs, paste0(
        "- $\\rho$: continuous-time AR decay rate"
      ))
    }
    gr <- trend_grouping_var(obj)
    if (!is.null(gr)) {
      defs <- c(defs, paste0(
        "- $\\boldsymbol{\\Omega}_{", gr, "}$, ",
        "$\\boldsymbol{\\Omega}_{\\text{global}}$, ",
        "$\\alpha_{cor}$: per-group correlation matrix, ",
        "shared global correlation matrix, and pooling weight ",
        "for the hierarchical residual structure"
      ))
    }
  }
  # Blank line before the bullets so Pandoc / Quarto picks up the
  # list rather than running it on as one paragraph.
  paste(c("where:", "", defs), collapse = "\n")
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


#' @noRd
trend_order_label <- function(obj) {
  tt <- obj$trend_metadata$trend_type
  if (is.null(tt)) return("")
  ar_lags <- obj$trend_metadata$ar_lags %||% integer(0L)
  ma_lags <- obj$trend_metadata$ma_lags %||% integer(0L)
  arma_label <- function(prefix) {
    if (!length(ma_lags)) {
      return(paste0(prefix, "(", paste(ar_lags, collapse = ", "), ")"))
    }
    paste0(
      prefix, "MA(", paste(ar_lags, collapse = ", "), ", ",
      paste(ma_lags, collapse = ", "), ")"
    )
  }
  switch(
    tt,
    "RW"   = "RW",
    "AR"   = arma_label("AR"),
    "VAR"  = arma_label("VAR"),
    "CAR"  = "CAR",
    "ZMVN" = "ZMVN",
    "PW"   = paste0(
      "PW(", obj$trend_metadata$pw_growth %||% "linear", ")"
    ),
    tt
  )
}

#' @noRd
index_range_rows <- function(obj) {
  data <- obj$data %||% data.frame()
  if (nrow(data) == 0L) return(list())
  rows <- list()
  if (!is.null(data$series)) {
    rows[[length(rows) + 1L]] <- list(
      lhs = "i",
      op  = "\\in",
      rhs = paste0("\\{1, \\ldots, ", length(unique(data$series)), "\\}")
    )
  }
  if (!is.null(data$time)) {
    rows[[length(rows) + 1L]] <- list(
      lhs = "t",
      op  = "\\in",
      rhs = paste0("\\{1, \\ldots, ", length(unique(data$time)), "\\}")
    )
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
  # Inside `$...$` pandoc / LaTeX treats `_` as a subscript
  # marker, so a multi-character name like `y_diri` would
  # render as `y` with subscript `diri`. A bare `\_` is still
  # rendered awkwardly by xelatex in math mode (visible gaps
  # around the literal). Wrap any identifier that contains a
  # `_` in `\text{...}` so it renders as upright text, with
  # the `_` itself escaped to keep LaTeX happy.
  if (is.null(s) || !is.character(s)) return(s)
  out <- vapply(s, function(x) {
    if (!grepl("_", x, fixed = TRUE)) return(x)
    paste0("\\text{", gsub("_", "\\\\_", x, fixed = FALSE), "}")
  }, character(1L))
  if (length(out) == 1L) unname(out) else out
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
  base <- switch(
    name,
    sigma = "\\sigma", phi = "\\phi", nu = "\\nu",
    alpha = "\\alpha", shape = "\\alpha", kappa = "\\kappa",
    paste0("\\", name)
  )
  if (has_dpar) paste0(base, "_{i,t}") else base
}

#' @noRd
family_distribution_text <- function(fam_name, mu, obj) {
  sigma <- dpar_aware_param("sigma", obj)
  phi   <- dpar_aware_param("phi", obj)
  nu    <- dpar_aware_param("nu", obj)
  shape <- dpar_aware_param("shape", obj)
  switch(
    fam_name,
    poisson     = paste0("\\text{Poisson}(", mu, ")"),
    bernoulli   = paste0("\\text{Bernoulli}(", mu, ")"),
    binomial    = paste0("\\text{Binomial}(n_{i,t}, ", mu, ")"),
    gaussian    = paste0("\\text{Normal}(", mu, ", ", sigma, ")"),
    student     = paste0("\\text{StudentT}(", nu, ", ", mu,
                          ", ", sigma, ")"),
    lognormal   = paste0("\\text{LogNormal}(", mu, ", ", sigma, ")"),
    Gamma       = paste0("\\text{Gamma}(", shape, ", ", mu, ")"),
    beta        = paste0("\\text{Beta}(", mu, ", ", phi, ")"),
    negbinomial = paste0("\\text{NegBin}(", mu, ", ", phi, ")"),
    nb          = paste0("\\text{NegBin}(", mu, ", ", phi, ")"),
    beta_nb     = paste0(
      "\\text{BetaNegBinomial}(", mu, ", r, \\tau)"
    ),
    tweedie     = paste0(
      "\\text{Tweedie}(", mu, ", \\phi, \\xi)"
    ),
    hurdle_poisson = paste0(
      "\\text{Hurdle-Poisson}(", mu, ", \\pi_{\\text{hu}})"
    ),
    hurdle_negbinomial = paste0(
      "\\text{Hurdle-NegBin}(", mu,
      ", \\phi, \\pi_{\\text{hu}})"
    ),
    hurdle_gamma = paste0(
      "\\text{Hurdle-Gamma}(", mu,
      ", \\alpha, \\pi_{\\text{hu}})"
    ),
    hurdle_lognormal = paste0(
      "\\text{Hurdle-LogNormal}(", mu,
      ", \\sigma, \\pi_{\\text{hu}})"
    ),
    zero_inflated_poisson = paste0(
      "\\text{ZIPoisson}(", mu, ", \\pi_{\\text{zi}})"
    ),
    zero_inflated_negbinomial = paste0(
      "\\text{ZINegBin}(", mu, ", \\phi, \\pi_{\\text{zi}})"
    ),
    zero_inflated_beta = paste0(
      "\\text{ZIBeta}(", mu, ", \\phi, \\pi_{\\text{zi}})"
    ),
    zero_inflated_binomial = paste0(
      "\\text{ZIBinomial}(n_{i,t}, ", mu,
      ", \\pi_{\\text{zi}})"
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
  if (!methods_md_is_detection_family(obj)) return(NULL)
  fam_name <- resolve_family_name(obj$family) %||% ""
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
  if (!methods_md_is_mv_custom_family(obj)) return(NULL)
  fam_name <- resolve_family_name(obj$family) %||% ""
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
    parts <- c(parts, render_fixed_inline(classes$fixed, notation))
  }
  if (length(classes$smooth) > 0L) {
    parts <- c(parts, render_smooth_inline(classes$smooth))
  }
  if (length(classes$gp) > 0L) {
    parts <- c(parts, render_gp_inline(classes$gp))
  }
  if (length(classes$mo) > 0L) {
    parts <- c(parts, render_mo_inline(classes$mo))
  }
  if (length(classes$me) > 0L) {
    parts <- c(parts, render_me_inline(classes$me))
  }
  if (length(classes$re) > 0L) {
    parts <- c(parts, render_re_inline(classes$re))
  }
  if (methods_md_has_latent_trend(obj)) {
    parts <- c(parts, "\\eta_{i,t}")
  }

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
    fixed  = obs_fixed_terms_from_prior(prior),
    smooth = obs_smooth_specs_from_prior(prior),
    gp     = get_gp_specs(obj),
    mo     = obs_mo_specs_from_prior(prior),
    me     = obs_me_specs_from_formula(obj),
    re     = obs_re_specs_from_prior(prior)
  )
}

#' @noRd
obs_fixed_terms_from_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(character(0L))
  # Skip rows scoped to a dpar or nlpar; those drive their own
  # per-dpar / per-nlpar predictor row, not the top-level mu.
  nlpar_col <- prior$nlpar %||% rep("", nrow(prior))
  has_int <- any(
    prior$class == "Intercept" &
      !nzchar(prior$dpar) &
      !nzchar(nlpar_col)
  )
  b_rows <- prior$class == "b" & !nzchar(prior$dpar) &
    !nzchar(nlpar_col) & nzchar(prior$coef)
  coefs <- prior$coef[b_rows]
  # Drop basis stubs and the bare "" umbrella row; those are not
  # user-supplied population effects. Also drop the monotonic
  # and measurement-error coefs (`mo<var>`, `me<var><sdvar>`);
  # each renders through its own block, not the linear
  # `\\beta_{<term>} <term>` shape.
  #   s(x) basis    -> sx_1, sx_2, ...
  #   t2(x, z) tensor -> t2xz_1, t2xz_2, ...
  #   te / ti are not in scope (brms rejects them at term parse)
  coefs <- coefs[!grepl("^s[A-Za-z0-9_]+_[0-9]+$", coefs)]
  coefs <- coefs[!grepl("^t2[A-Za-z0-9_]+_[0-9]+$", coefs)]
  coefs <- coefs[!grepl("^mo[A-Za-z_.][A-Za-z0-9_.]*$", coefs)]
  coefs <- coefs[!grepl("^me[A-Za-z_.][A-Za-z0-9_.]*$", coefs)]
  terms <- unique(coefs)
  if (has_int) c("Intercept", terms) else terms
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
obs_mo_specs_from_prior <- function(prior) {
  # brms emits class = "b" + coef = "mo<var>" for the magnitude
  # of each monotonic effect, plus class = "simo" + coef like
  # "mo<var>1" for the Dirichlet simplex of step increments.
  # Detect via the b rows -- one per mo() term in the formula.
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  mo_rows <- prior$class == "b" &
    grepl("^mo[A-Za-z_.][A-Za-z0-9_.]*$", prior$coef)
  if (!any(mo_rows)) return(list())
  coefs <- unique(prior$coef[mo_rows])
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
  switch(
    cov %||% "exp_quad",
    "exp_quad"    = "exponentiated-quadratic",
    "matern52"    = "Matern (5/2)",
    "matern32"    = "Matern (3/2)",
    "exponential" = "exponential",
    cov
  )
}

#' @noRd
gp_kernel_label <- function(cov) {
  switch(
    cov %||% "exp_quad",
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
  if (identical(fname, "ti")) {
    return("tensor interaction smooth")
  }
  if (identical(fname, "te")) {
    return("tensor product smooth")
  }
  if (identical(fname, "t2")) {
    return("tensor product smooth (t2)")
  }
  switch(
    bs %||% "tp",
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
obs_gp_specs_from_prior <- function(prior) {
  # Kept for back-compat: returns a stripped one-per-term list
  # from the prior table. Drops 2D + by-factor detail because
  # brms concatenates names without a separator (gpz1z2 etc).
  # Prefer obs_gp_specs_from_formula() when the formula is
  # available -- it recovers vars, k, by, and cov_kernel cleanly.
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  gp_rows <- prior$class == "sdgp" & nzchar(prior$coef)
  if (!any(gp_rows)) return(list())
  coefs <- prior$coef[gp_rows]
  unique_coefs <- unique(coefs)
  lapply(unique_coefs, function(co) {
    list(
      vars = sub("^gp", "", co), k = NA_integer_,
      by = NA_character_, cov = "exp_quad", coef = co
    )
  })
}

#' @noRd
get_gp_specs <- function(obj) {
  # Single entry point for everything that walks gp() terms.
  # The formula walker recovers vars / k / by / cov; the
  # prior-table extractor is the fallback when the formula
  # round-trip drops the gp() call (unusual but defensive).
  specs <- obs_gp_specs_from_formula(obj)
  if (length(specs) == 0L) {
    specs <- obs_gp_specs_from_prior(obj$prior)
  }
  specs
}

#' @noRd
obs_gp_specs_from_formula <- function(obj) {
  # Walk the obs formula AST for `gp(...)` calls and recover the
  # full spec per term: variable list, k, by, cov kernel. This
  # is the authoritative extractor; the prior-table fallback
  # loses 2D and by-factor detail.
  if (is.null(obj$formula)) return(list())
  f <- mvgam_obs_formula(obj)
  if (!inherits(f, "formula") || length(f) < 3L) return(list())
  lapply(formula_calls(f[[3L]], "gp"), gp_call_to_spec)
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
render_fixed_inline <- function(terms, notation) {
  if ("Intercept" %in% terms) {
    others <- setdiff(terms, "Intercept")
    bits <- c("\\alpha")
    if (length(others) > 0L) {
      bits <- c(bits, paste0(
        "\\beta_{", others, "} ", others, "_{i,t}"
      ))
    }
    return(paste(bits, collapse = " + "))
  }
  paste(
    paste0("\\beta_{", terms, "} ", terms, "_{i,t}"),
    collapse = " + "
  )
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
render_smooth_inline <- function(specs) {
  compose_inline_terms(specs, function(s) {
    paste0(
      "f_{", spec_subscript(s), "}(",
      spec_vars_indexed(s), ")"
    )
  })
}

#' @noRd
render_me_inline <- function(specs) {
  # Measurement-error effects (brms `me(x, sdx)`): the linear
  # predictor uses the latent true covariate `\\tilde{x}_{i,t}`
  # rather than the noisy observation `x_{i,t}`.
  compose_inline_terms(specs, function(s) {
    paste0(
      "\\beta^{(\\text{me})}_{", s$var, "} \\, \\tilde{",
      s$var, "}_{i,t}"
    )
  })
}

#' @noRd
render_mo_inline <- function(specs) {
  # Monotonic effects (Burkner & Charpentier 2020). Each mo()
  # term contributes b^{(mo)}_x * m_x(x_{i,t}), where m_x is a
  # cumulative step transform built from a Dirichlet simplex.
  compose_inline_terms(specs, function(s) {
    paste0(
      "\\beta^{(\\text{mo})}_{", s$var, "} \\, m_{",
      s$var, "}(", s$var, "_{i,t})"
    )
  })
}

#' @noRd
render_gp_inline <- function(specs) {
  compose_inline_terms(specs, function(s) {
    paste0(
      "f^{(\\text{gp})}_{", gp_subscript(s), "}(",
      spec_vars_indexed(s), ")"
    )
  })
}

#' @noRd
spec_subscript <- function(spec) {
  # Math-subscript form of the variable list: "x" for univariate
  # smooths/GPs, "x, z" for tensor / multi-dim. Shared by every
  # renderer that needs a per-term subscript label.
  paste(spec$vars, collapse = ", ")
}

#' @noRd
spec_key <- function(spec) {
  # Stable identifier per term, safe to embed in a LaTeX
  # subscript that already nests inside `_{...}` (no commas).
  # Used as the per-term key in basis-size $K_{key}$ and basis
  # coefficient $\beta^{(key)}$ tags.
  paste(spec$vars, collapse = ":")
}

#' @noRd
spec_vars_indexed <- function(spec, suffix = "_{i,t}") {
  # "x_{i,t}, z_{i,t}" -- the indexed argument list used inside
  # a function call f_{sub}(x_{i,t}, z_{i,t}). suffix is a hook
  # for callers that want a different index pattern.
  paste(paste0(spec$vars, suffix), collapse = ", ")
}

#' @noRd
gp_subscript <- function(spec) {
  base <- spec_subscript(spec)
  if (!is.null(spec$by) && !is.na(spec$by) && nzchar(spec$by)) {
    paste0(base, " \\mid ", spec$by)
  } else {
    base
  }
}

#' @noRd
render_re_inline <- function(specs) {
  # Per-group inline contribution to the linear predictor:
  #   intercept-only group:  alpha_{grp[i]}
  #   varying-slope group:   alpha_{grp[i]} + beta^{(grp)}_{x, grp[i]} x_{i,t}
  compose_inline_terms(specs, function(s) {
    pieces <- paste0("\\alpha_{", s$group, "[i]}")
    for (slope in s$slopes) {
      pieces <- c(pieces, paste0(
        "\\beta^{(", s$group, ")}_{", slope, ", ",
        s$group, "[i]} ", slope, "_{i,t}"
      ))
    }
    paste(pieces, collapse = " + ")
  })
}

#' @noRd
term_definition_rows <- function(obj, notation) {
  prior <- obj$prior
  smooth_specs <- obs_smooth_specs_from_prior(prior)
  gp_specs <- get_gp_specs(obj)
  mo_specs <- obs_mo_specs_from_prior(prior)
  me_specs <- obs_me_specs_from_formula(obj)
  re_specs <- obs_re_specs_from_prior(prior)

  rows <- list()
  for (spec in smooth_specs) {
    sub <- spec_subscript(spec)
    sub_key <- spec_key(spec)
    vars_in <- sub
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("f_{", sub, "}(", vars_in, ")"),
      op  = "=",
      rhs = paste0(
        "\\sum_{k=1}^{K_{", sub_key, "}} ",
        "\\beta^{(", sub_key, ")}_k B_k(", vars_in, ")"
      )
    )
  }
  for (spec in gp_specs) {
    sub <- gp_subscript(spec)
    vars_in <- spec_subscript(spec)
    rho_arg <- if (length(spec$vars) > 1L) {
      paste0("\\boldsymbol{\\rho}_{", sub, "}")
    } else {
      paste0("\\rho_{", sub, "}")
    }
    kernel_name <- gp_kernel_label(spec$cov %||% "exp_quad")
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0(
        "f^{(\\text{gp})}_{", sub, "}(", vars_in, ")"
      ),
      op  = "\\sim",
      rhs = paste0(
        "\\text{GP}\\left(0, ", kernel_name,
        "(", rho_arg, ", \\sigma^{(\\text{gp})}_{",
        sub, "})\\right)"
      )
    )
  }
  for (s in mo_specs) {
    v <- s$var
    # Cumulative step transform from a Dirichlet simplex over
    # the D-1 step increments (Burkner & Charpentier 2020).
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("m_{", v, "}(", v, ")"),
      op  = "=",
      rhs = paste0(
        "(D_{", v, "} - 1) \\sum_{j=1}^{", v, "} ",
        "\\zeta_{", v, ",j}"
      )
    )
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("\\boldsymbol{\\zeta}_{", v, "}"),
      op  = "\\sim",
      rhs = paste0(
        "\\text{Dirichlet}(\\boldsymbol{\\alpha}_{", v, "})"
      )
    )
  }
  for (s in me_specs) {
    v <- s$var
    sdv <- if (!is.na(s$sdvar)) s$sdvar else "se"
    # Observation layer: noisy x_i is centred on the latent
    # tilde{x}_i with known SD sdvar_i (data).
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0(v, "_{i,t}"),
      op  = "\\sim",
      rhs = paste0(
        "\\text{Normal}\\!\\left(\\tilde{", v,
        "}_{i,t}, ", sdv, "_{i,t}\\right)"
      )
    )
    # Latent layer: tilde{x}_i drawn from a population-level
    # Normal with hyper-mean and hyper-SD.
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("\\tilde{", v, "}_{i,t}"),
      op  = "\\sim",
      rhs = paste0(
        "\\text{Normal}\\!\\left(\\mu^{(\\text{me})}_{", v,
        "}, \\sigma^{(\\text{me})}_{", v, "}\\right)"
      )
    )
  }
  for (s in re_specs) {
    grp <- s$group
    if (!s$has_slope) {
      # Intercept-only group: alpha_{grp} ~ Normal(0, sigma_{grp}).
      rows[[length(rows) + 1L]] <- list(
        lhs = paste0("\\alpha_{", grp, "}"),
        op  = "\\sim",
        rhs = paste0("\\text{Normal}(0, \\sigma_{", grp, "})")
      )
      next
    }
    # Varying-slope group: joint MVNormal over (alpha, beta_x, ...)
    # with LKJ correlation on Omega and a diagonal of SDs.
    slope_syms <- paste0(
      "\\beta^{(", grp, ")}_{", s$slopes, ", ", grp, "}"
    )
    vec_lhs <- paste0(
      "(\\alpha_{", grp, "}, ",
      paste(slope_syms, collapse = ", "), ")^\\top"
    )
    rows[[length(rows) + 1L]] <- list(
      lhs = vec_lhs,
      op  = "\\sim",
      rhs = paste0(
        "\\text{MVNormal}\\!\\left(\\mathbf{0}, ",
        "\\boldsymbol{\\Sigma}_{", grp, "}\\right)"
      )
    )
    # Sigma_grp = diag(sigma) Omega_grp diag(sigma).
    sd_diag_syms <- c(
      paste0("\\sigma^{(\\alpha)}_{", grp, "}"),
      paste0("\\sigma^{(\\beta_{", s$slopes, "})}_{", grp, "}")
    )
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("\\boldsymbol{\\Sigma}_{", grp, "}"),
      op  = "=",
      rhs = paste0(
        "\\text{diag}(", paste(sd_diag_syms, collapse = ", "), ")",
        "\\,\\boldsymbol{\\Omega}_{", grp, "}\\,",
        "\\text{diag}(", paste(sd_diag_syms, collapse = ", "), ")"
      )
    )
    # LKJ on Omega via brms' Cholesky-factor parameterisation.
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("\\boldsymbol{\\Omega}_{", grp, "}"),
      op  = "\\sim",
      rhs = "\\text{LKJCorr}(\\eta)"
    )
  }
  rows
}

#' @noRd
methods_md_has_latent_trend <- function(obj) {
  tt <- obj$trend_metadata$trend_type
  !is.null(tt) && !identical(tt, "None") && !identical(tt, "none")
}


# ---------------------------------------------------------------
# Family-kind predicates
# ---------------------------------------------------------------
# Three orthogonal predicates read off the family object
# attributes (set in `R/families.R::occ()` / `::nmix()` /
# `::mvn()` etc.):
#   * is_closure_unit:   `attr(family, "mvgam_closure_unit")`.
#                        Family uses the closure-unit data layout
#                        (per-visit obs, per-unit latent state).
#   * is_detection:      `attr(family, "mvgam_predict_types")`
#                        carries `"detection"`. Only occ / nmix
#                        variants have a detection sub-formula.
#   * is_mv_custom:      `attr(family, "mvgam_multi_response")`.
#                        Multi-response custom families
#                        (mvn / mvt / diri / multi / categ) whose
#                        likelihood is a single joint MVNormal /
#                        Dirichlet / Multinomial / Categorical
#                        per unit.
#
# Whether the fit indexes seasons is asked of
# `is_multi_season_family()`, which reads the closure-unit grouping
# the likelihood was built on.

#' @noRd
methods_md_is_closure_unit <- function(obj) {
  is_closure_unit_family(obj$family)
}

#' @noRd
methods_md_is_detection_family <- function(obj) {
  "detection" %in% family_predict_types(obj$family)
}

#' @noRd
methods_md_is_mv_custom_family <- function(obj) {
  is_multi_response_family(obj$family)
}



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
innovation_rows <- function(obj, is_vector, has_cor, gr = NULL) {
  # Hierarchical grouping forces cross-series correlation
  # (see AR() / VAR() `@param gr`).
  if (!is.null(gr)) has_cor <- TRUE
  sig <- sigma_symbol(gr)
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

  rows <- if (has_cor) {
    list(list(
      lhs = eps_vec,
      op  = "\\sim",
      rhs = paste0(mvn, sig, ")")
    ))
  } else if (is_vector) {
    list(list(
      lhs = eps_vec,
      op  = "\\sim",
      rhs = paste0(mvn, "\\text{diag}(\\sigma_\\eta^2))")
    ))
  } else {
    list(list(
      lhs = trend_eps(obj),
      op  = "\\sim",
      rhs = if (heavy) {
        paste0("\\text{StudentT}(", nu, ", 0, \\sigma_\\eta)")
      } else {
        "\\text{Normal}(0, \\sigma_\\eta)"
      }
    ))
  }

  if (!is.null(gr)) {
    rows <- c(rows, hierarchical_cor_rows(gr))
  }
  rows
}

#' @noRd
hierarchical_cor_rows <- function(gr) {
  # Hierarchical residual correlation decomposition emitted when
  # the user supplies `gr` to AR() / VAR().
  list(
    list(
      lhs = paste0("\\boldsymbol{\\Omega}_{", gr, "}"),
      op  = "=",
      rhs = paste0(
        "\\alpha_{cor} \\boldsymbol{\\Omega}_{\\text{global}}",
        " + (1 - \\alpha_{cor}) \\boldsymbol{\\Omega}_{", gr,
        ", \\text{local}}"
      )
    ),
    list(
      lhs = sigma_symbol(gr),
      op  = "=",
      rhs = paste0(
        "\\text{diag}(\\sigma_\\eta) \\boldsymbol{\\Omega}_{",
        gr, "} \\text{diag}(\\sigma_\\eta)"
      )
    )
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
    ARMA  = render_latent_arma,
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
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  gr <- trend_grouping_var(obj)
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste0(trend_eta(obj, lag = 1L), " + ", trend_eps(obj))
    )),
    innovation_rows(obj, is_vector = FALSE,
                    has_cor = has_cor, gr = gr)
  )
}

#' @noRd
render_latent_ar <- function(obj, notation) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  gr <- trend_grouping_var(obj)
  rhs_terms <- paste(
    paste0("\\phi_{", lags, "} ", trend_eta(obj, lag = lags)),
    collapse = " + "
  )
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste0(rhs_terms, " + ", trend_eps(obj))
    )),
    innovation_rows(obj, is_vector = FALSE,
                    has_cor = has_cor, gr = gr)
  )
}

#' @noRd
render_latent_var <- function(obj, notation) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  gr <- trend_grouping_var(obj)
  rhs_terms <- paste(
    paste0("\\boldsymbol{\\Phi}_{", lags, "} ",
             trend_eta_vec(obj, lag = lags)),
    collapse = " + "
  )
  c(
    list(list(
      lhs = trend_eta_vec(obj),
      op  = "=",
      rhs = paste0(rhs_terms, " + ", trend_eps_vec(obj))
    )),
    innovation_rows(obj, is_vector = TRUE,
                    has_cor = has_cor, gr = gr)
  )
}

#' @noRd
render_latent_arma <- function(obj, notation) {
  ar_lags <- obj$trend_metadata$ar_lags %||% 1L
  ma_lags <- obj$trend_metadata$ma_lags %||% 1L
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  gr <- trend_grouping_var(obj)
  ar_rhs <- paste(
    paste0("\\phi_{", ar_lags, "} ",
             trend_eta(obj, lag = ar_lags)),
    collapse = " + "
  )
  ma_rhs <- paste(
    paste0("\\theta_{", ma_lags, "} ",
             trend_eps_lag(obj, ma_lags)),
    collapse = " + "
  )
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste0(
        ar_rhs, " + ", trend_eps(obj), " + ", ma_rhs
      )
    )),
    innovation_rows(obj, is_vector = FALSE,
                    has_cor = has_cor, gr = gr)
  )
}

#' @noRd
render_latent_car <- function(obj, notation) {
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  gr <- trend_grouping_var(obj)
  c(
    list(list(
      lhs = trend_eta(obj),
      op  = "=",
      rhs = paste0(
        "\\rho^{\\Delta t_{i,t}} ", trend_eta(obj, lag = 1L),
        " + ", trend_eps(obj)
      )
    )),
    innovation_rows(obj, is_vector = FALSE,
                    has_cor = has_cor, gr = gr)
  )
}

#' @noRd
render_latent_zmvn <- function(obj, notation) {
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  gr <- trend_grouping_var(obj)
  if (!is.null(gr)) has_cor <- TRUE
  cov <- if (has_cor) {
    sigma_symbol(gr)
  } else {
    "\\text{diag}(\\sigma_\\eta^2)"
  }
  rows <- list(list(
    lhs = trend_eta_vec(obj),
    op  = "\\sim",
    rhs = paste0("\\text{MVNormal}(\\mathbf{0}, ", cov, ")")
  ))
  if (!is.null(gr)) {
    rows <- c(rows, hierarchical_cor_rows(gr))
  }
  rows
}

#' @noRd
render_latent_pw <- function(obj, notation) {
  # PW does not use n_lv (factor mode incompatible at validator),
  # so the symbol stays \eta_{i,t} unconditionally.
  list(
    list(
      lhs = "\\eta_{i,t}",
      op  = "=",
      rhs = paste0(
        "(k + \\boldsymbol{\\delta}^\\top \\mathbf{a}(t)) t",
        " + (m + \\boldsymbol{\\delta}^\\top \\boldsymbol{\\gamma})"
      )
    ),
    list(
      lhs = "\\delta_j",
      op  = "\\sim",
      rhs = "\\text{Laplace}(0, \\tau)"
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

#' @noRd
methods_md_has_factor_model <- function(obj) {
  n_lv <- obj$trend_metadata$n_lv
  !is.null(n_lv) && is.numeric(n_lv) && length(n_lv) == 1L &&
    n_lv > 0L
}

#' @noRd
factor_model_rows <- function(obj, notation) {
  if (!methods_md_has_factor_model(obj)) return(list())
  n_lv <- obj$trend_metadata$n_lv
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
  if (is.null(spec)) {
    return(list(list(
      lhs = "Z_{i,k}",
      op  = "\\sim",
      rhs = "\\text{Student-t}(3, 0, 0.5)"
    )))
  }
  traits <- loadings_spec_traits(spec)

  rows <- list()
  if (traits$kernel) {
    rows <- c(rows, kernel_assembly_rows(spec))
  }
  if (traits$mgp) {
    rows <- c(rows, mgp_shrinkage_rows())
  }
  rows <- c(rows, list(z_column_prior_row(traits$mgp, traits$kernel)))
  rows
}

#' @noRd
z_column_prior_row <- function(uses_mgp, has_kernel) {
  # Picks the matching one of the four Z-prior branches emitted
  # by make_loadings_prior_stanvars():
  #   * iid default (no kernel, no MGP)
  #   * pure MGP (column shrinkage only)
  #   * kernel only
  #   * kernel + MGP (multiplicative)
  if (uses_mgp && has_kernel) {
    list(
      lhs = "Z_{\\cdot,k}",
      op  = "\\sim",
      rhs = paste0(
        "\\text{MVNormal}(\\mathbf{0}, \\sqrt{\\Psi_k} \\, ",
        "L_\\Phi L_\\Phi^\\top \\sqrt{\\Psi_k})"
      )
    )
  } else if (uses_mgp) {
    list(
      lhs = "Z_{i,k}",
      op  = "\\sim",
      rhs = "\\text{Normal}(0, \\sqrt{\\Psi_k})"
    )
  } else if (has_kernel) {
    list(
      lhs = "Z_{\\cdot,k}",
      op  = "\\sim",
      rhs = "\\text{MVNormal}(\\mathbf{0}, L_\\Phi L_\\Phi^\\top)"
    )
  } else {
    list(
      lhs = "Z_{i,k}",
      op  = "\\sim",
      rhs = "\\text{Student-t}(3, 0, 0.5)"
    )
  }
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
  list(
    list(
      lhs = "\\varrho_1",
      op  = "\\sim",
      rhs = "\\text{InvGamma}(a_1, 1)"
    ),
    list(
      lhs = "\\varrho_h",
      op  = "\\sim",
      rhs = "\\text{InvGamma}(a_2, 1) \\quad (h \\ge 2)"
    ),
    list(
      lhs = "\\Psi_k",
      op  = "=",
      rhs = "\\prod_{l \\le k} \\varrho_l"
    )
  )
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
  prior <- merge_trend_priors(obj)
  if (is.null(prior) || nrow(prior) == 0L) {
    return("## Priors\n\n(no prior table on this fit)")
  }

  # brms emits two row kinds per parameter class:
  #   1. an "umbrella" row tagged source = "default" / "user" with
  #      the prior text but empty coef / group
  #   2. one or more "(vectorized)" rows with empty prior text but
  #      the specific coef / group labels
  # Backfill the umbrella prior text onto matching vectorized rows
  # so each row carries both the prior expression and the label.
  prior <- backfill_umbrella_priors(prior)

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

  # Drop smooth-basis "b" stubs (`sx_1`, `sx_2`, ...): they share
  # the smooth's hyperprior via the `sds` class.
  is_smooth_basis <- prior$class == "b" &
    grepl("^s[A-Za-z0-9_]+_[0-9]+$", prior$coef)
  prior <- prior[!is_smooth_basis, , drop = FALSE]

  # Drop rows still missing a prior string after backfill: those
  # had no umbrella, so they are improper flat by default.
  keep <- !is.na(prior$prior) & nzchar(prior$prior)
  prior <- prior[keep, , drop = FALSE]
  if (nrow(prior) == 0L) {
    return("## Priors\n\n(all parameters are improper flat)")
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
    "b", "sd", "sds", "L", "cor",
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
    return("## Priors\n\n(all parameters are improper flat)")
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
    sym <- format_parameter_symbol(row)
    if (is.null(sym)) next
    dist <- format_prior_distribution(row$prior)
    if (is.null(dist)) next
    rows[[length(rows) + 1L]] <- list(
      lhs = sym, op = "\\sim", rhs = dist
    )
  }
  if (length(rows) == 0L) {
    return("## Priors\n\n(all parameters are improper flat)")
  }
  block <- align_block(rows)
  paste(c("## Priors", "", block), collapse = "\n")
}

#' @noRd
merge_trend_priors <- function(obj) {
  obs <- obj$prior
  tm <- obj$trend_model
  if (is.null(tm) || is.null(tm$prior) || nrow(tm$prior) == 0L) {
    return(obs)
  }
  trend_prior <- tm$prior
  # Render trend-side rows under the names the combined program
  # gives them, so a reader can match the description to the Stan.
  trend_prior$class <- apply_trend_class_suffix(trend_prior$class)
  if (is.null(obs) || nrow(obs) == 0L) {
    return(trend_prior)
  }
  missing_cols <- setdiff(names(obs), names(trend_prior))
  for (col in missing_cols) {
    trend_prior[[col]] <- if (is.character(obs[[col]])) "" else NA
  }
  missing_in_obs <- setdiff(names(trend_prior), names(obs))
  for (col in missing_in_obs) {
    obs[[col]] <- if (is.character(trend_prior[[col]])) "" else NA
  }
  trend_prior <- trend_prior[, names(obs), drop = FALSE]
  out <- rbind(obs, trend_prior)
  class(out) <- class(obs)
  out
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
  backend <- obj$backend %||% "rstan"
  algorithm <- obj$algorithm %||% "none"
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
  stan_lib <- if (identical(info$backend, "cmdstanr")) {
    paste0("Stan ", info$stan_v, " (via cmdstanr)")
  } else {
    paste0("Stan ", info$stan_v, " (via rstan)")
  }
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
  if (inherits(f, c("brmsformula", "bform"))) {
    main <- if (!is.null(f$formula)) f$formula else f
    parts <- paste(deparse(main, width.cutoff = 60L),
                    collapse = " ")
    pforms <- f$pforms %||% list()
    if (length(pforms) > 0L) {
      for (nm in names(pforms)) {
        parts <- c(parts, paste0(nm, " = ",
                                  paste(deparse(pforms[[nm]],
                                                 width.cutoff = 60L),
                                        collapse = " ")))
      }
    }
    # brms records the nl status as an attribute on the inner
    # formula slot (`attr(f$formula, "nl")`), not as `f$nl`.
    # Also accept `f$nl` for forward-compat with any wrapper
    # that promotes it to a top-level slot.
    is_nl <- isTRUE(f$nl) ||
      isTRUE(attr(f$formula, "nl"))
    if (is_nl) {
      parts <- c(parts, "nl = TRUE")
    }
    return(paste0("bf(", paste(parts, collapse = ", "), ")"))
  }
  paste(deparse(f, width.cutoff = 60L), collapse = " ")
}

#' @noRd
trend_formula_text <- function(obj) {
  tcall <- obj$trend_call
  if (is.null(tcall)) return(NULL)
  formula_text(tcall)
}

#' @noRd
family_call_text <- function(family) {
  if (is.null(family)) return("gaussian()")
  # `family$family` collapses every brms customfamily to the
  # literal "custom"; the user-visible constructor lives on
  # `family$name`. resolve_family_name() routes through both.
  fam <- resolve_family_name(family) %||% "gaussian"
  link <- family$link %||% "identity"
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
    Gamma = "inverse", lognormal = "identity",
    beta = "logit", negbinomial = "log", nb = "log",
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
  if (grepl(super_pat, sym)) {
    return(sub(super_pat, paste0("^{(\\1, ", r, ")}"), sym))
  }
  if (grepl("_\\{", sym)) {
    return(sub("_\\{", paste0("^{(", r, ")}_{"), sym))
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
      return(paste0("\\beta_{", lbl, "}"))
    }
    # nl sub-formula coefficients: brms emits b_<nlpar>_<term>
    # priors with nzchar(nlpar). Carry the nlpar through as a
    # superscript so each nlpar's coefficient set is visually
    # distinct from the top-level beta_{term} family.
    if (nzchar(nlpar)) {
      base <- paste0("\\beta^{(", nlpar, ")}")
      if (nzchar(coef)) {
        return(paste0(base, "_{", coef, "}"))
      }
      return(base)
    }
    if (nzchar(coef)) {
      if (grepl("^mo[A-Za-z_.][A-Za-z0-9_.]*$", coef)) {
        return(paste0(
          "\\beta^{(\\text{mo})}_{", sub("^mo", "", coef), "}"
        ))
      }
      if (grepl("^me[A-Za-z_.][A-Za-z0-9_.]*$", coef)) {
        # Coef is `me<var><sdvar>` concatenated; the variable
        # name is the longest leading alpha-numeric prefix that
        # leaves a non-empty suffix for the sdvar. Without the
        # formula we cannot split unambiguously, so render the
        # raw `me<...>` tail without trying to dissect it.
        return(paste0(
          "\\beta^{(\\text{me})}_{", sub("^me", "", coef), "}"
        ))
      }
      return(paste0("\\beta_{", coef, "}"))
    }
    return("\\boldsymbol{\\beta}")
  }
  if (identical(cls, "simo")) {
    bare <- if (nzchar(coef)) {
      sub("[0-9]+$", "", sub("^mo", "", coef))
    } else "j"
    return(paste0("\\boldsymbol{\\zeta}_{", bare, "}"))
  }
  if (identical(cls, "meanme")) {
    bare <- if (nzchar(coef)) sub("^me", "", coef) else "j"
    return(paste0("\\mu^{(\\text{me})}_{", bare, "}"))
  }
  if (identical(cls, "sdme")) {
    bare <- if (nzchar(coef)) sub("^me", "", coef) else "j"
    return(paste0("\\sigma^{(\\text{me})}_{", bare, "}"))
  }
  if (identical(cls, "sigma")) return("\\sigma")
  if (identical(cls, "shape")) return("\\alpha")
  if (identical(cls, "nu"))    return("\\nu")
  if (identical(cls, "phi"))   return("\\phi")
  if (identical(cls, "zi"))    return("\\pi_{\\text{zi}}")
  if (identical(cls, "hu"))    return("\\pi_{\\text{hu}}")
  if (identical(cls, "sd")) {
    grp <- if (nzchar(group)) group else "j"
    if (!nzchar(coef)) {
      return(paste0("\\sigma_{", grp, "}"))
    }
    # Disambiguate intercept vs slope SDs under a varying-slope
    # group (`(x | grp)` emits one sd row per coef under the
    # same group). Render `sigma^{(\\alpha)}_{grp}` for the
    # intercept and `sigma^{(\\beta_{<coef>})}_{grp}` for slopes.
    sup <- if (identical(coef, "Intercept")) {
      "\\alpha"
    } else {
      paste0("\\beta_{", coef, "}")
    }
    return(paste0("\\sigma^{(", sup, ")}_{", grp, "}"))
  }
  if (identical(cls, "sds")) {
    bare <- if (nzchar(coef)) {
      sub("^[a-z2]+\\(\\s*([A-Za-z_.][A-Za-z0-9_.]*).*", "\\1", coef)
    } else "j"
    return(paste0("\\lambda_{", bare, "}"))
  }
  if (identical(cls, "sdgp")) {
    bare <- if (nzchar(coef)) sub("^gp", "", coef) else "j"
    return(paste0("\\sigma^{(\\text{gp})}_{", bare, "}"))
  }
  if (identical(cls, "lscale")) {
    bare <- if (nzchar(coef)) sub("^gp", "", coef) else "j"
    return(paste0("\\rho_{", bare, "}"))
  }
  if (identical(cls, "L")) {
    grp <- if (nzchar(group)) group else "j"
    return(paste0("\\mathbf{L}_{", grp, "}"))
  }
  if (identical(cls, "Lrescor")) {
    return("\\mathbf{L}_{\\text{rescor}}")
  }
  if (identical(cls, "cor")) {
    grp <- if (nzchar(group)) group else "j"
    return(paste0("\\boldsymbol{\\Omega}_{", grp, "}"))
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
    idx <- if (nzchar(coef)) escape_math_text(coef) else ""
    return(paste0("\\varrho^{-1}_{", idx, "}"))
  }
  if (identical(cls, "Psi")) {
    return("\\boldsymbol{\\Psi}")
  }
  # AR / VAR / RW / CAR scales emitted by brms with class names
  # like ar1_trend / sigma_trend / Intercept_trend / phi_trend.
  if (grepl("_trend$", cls)) {
    if (identical(cls, "Intercept_trend")) return("\\alpha^{(\\eta)}")
    if (grepl("^ar[0-9]+_trend", cls)) {
      lag <- sub("^ar([0-9]+)_trend.*", "\\1", cls)
      return(paste0("\\phi_{", lag, "}"))
    }
    if (grepl("^ma[0-9]+_trend", cls)) {
      lag <- sub("^ma([0-9]+)_trend.*", "\\1", cls)
      return(paste0("\\theta_{", lag, "}"))
    }
    if (identical(cls, "sigma_trend")) return("\\sigma_\\eta")
    if (identical(cls, "phi_trend"))   return("\\phi_\\eta")
    bare <- sub("_trend$", "", cls)
    return(paste0("\\text{", bare, "}_\\eta"))
  }
  # Unrecognised class: render verbatim so the row still appears
  # (math-only spec, no silent drops).
  paste0("\\text{", cls, "}")
}

#' @noRd
format_prior_distribution <- function(prior_str) {
  if (is.null(prior_str)) return(NULL)
  s <- trimws(prior_str)
  if (!nzchar(s)) return("\\text{flat}")
  if (identical(s, "(flat)")) return("\\text{flat}")

  match_args <- function(pat) {
    m <- regmatches(s, regexec(pat, s))[[1L]]
    if (length(m) == 0L) return(NULL)
    args <- strsplit(m[[2L]], "\\s*,\\s*")[[1L]]
    trimws(args)
  }
  # Shared formatter so every distribution emits `\text{Name}(...)`
  # with literal `_` characters in hyper-parameter names escaped
  # (a bare `mgp_a1` would render `mgp` subscript `a1`).
  emit <- function(label, args) {
    paste0(
      "\\text{", label, "}(",
      paste(vapply(args, escape_math_text, character(1L)),
            collapse = ", "),
      ")"
    )
  }

  args <- match_args("^normal\\((.*)\\)$")
  if (!is.null(args)) return(emit("Normal", args))
  args <- match_args("^student_t\\((.*)\\)$")
  if (!is.null(args)) return(emit("StudentT", args))
  args <- match_args("^lognormal\\((.*)\\)$")
  if (!is.null(args)) return(emit("LogNormal", args))
  args <- match_args("^exponential\\((.*)\\)$")
  if (!is.null(args)) return(emit("Exponential", args))
  args <- match_args("^gamma\\((.*)\\)$")
  if (!is.null(args)) return(emit("Gamma", args))
  args <- match_args("^inv_gamma\\((.*)\\)$")
  if (!is.null(args)) return(emit("InvGamma", args))
  args <- match_args("^cauchy\\((.*)\\)$")
  if (!is.null(args)) return(emit("Cauchy", args))
  args <- match_args("^beta\\((.*)\\)$")
  if (!is.null(args)) return(emit("Beta", args))
  args <- match_args("^uniform\\((.*)\\)$")
  if (!is.null(args)) return(emit("Uniform", args))
  args <- match_args("^lkj_corr_cholesky\\((.*)\\)$")
  if (!is.null(args)) return(emit("LKJCholesky", args))
  args <- match_args("^lkj(_corr)?\\((.*)\\)$")
  if (!is.null(args)) return(emit("LKJCorr", args))
  args <- match_args("^dirichlet\\((.*)\\)$")
  if (!is.null(args)) return(emit("Dirichlet", args))
  # Unrecognised distribution: render verbatim wrapped in \text{}
  # so the row still appears.
  paste0("\\text{", gsub("([{}_])", "\\\\\\1", s), "}")
}
