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
#' @examples
#' \donttest{
#' set.seed(0)
#' simdat <- sim_mvgam(
#'   seasonality = "hierarchical",
#'   trend_model = AR(p = 1),
#'   family = gaussian()
#' )
#' mod <- mvgam(
#'   y ~ s(season, bs = "cc", k = 6),
#'   trend_formula = ~ AR(p = 1),
#'   data = simdat$data_train,
#'   family = gaussian(),
#'   chains = 2,
#'   silent = 2
#' )
#' methods_md(mod)
#' }
#'
#' @author Nicholas J Clark
#' @seealso \code{\link{how_to_cite}}, \code{\link{prior_summary}}
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
  fam_name <- fam$family %||% "gaussian"
  link <- fam$link %||% "identity"
  dims <- describe_data_dimensions(obj)

  # Multi-response: list response columns explicitly so the
  # reader sees the mvbind structure (`yA`, `yB`) rather than
  # an opaque bold `Y` vector label.
  responses <- get_response_names(obj)
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
  # Response column(s). Prefer obj$response_names; fall back to
  # the formula LHS via mvgam_obs_formula() so prefits (where
  # response_names is NULL) still skip the response column.
  resp_cols <- obj$response_names %||% character(0L)
  if (length(resp_cols) == 0L && !is.null(obj$formula)) {
    f <- mvgam_obs_formula(obj)
    if (inherits(f, "formula") && length(f) >= 3L) {
      resp_cols <- all.vars(f[[2L]])
    }
  }
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
  # Response columns from the LHS surface separately via
  # response_names; include them so Predictors filtering does
  # not blank out single-response fits.
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
  fam_name <- fam$family %||% "gaussian"
  link <- fam$link %||% "identity"

  rows <- list()
  for (ir in index_range_rows(obj)) {
    rows[[length(rows) + 1L]] <- ir
  }

  responses <- get_response_names(obj)
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
  } else {
    mu <- mu_symbol(obj)
    rows[[length(rows) + 1L]] <- list(
      lhs = response_subscripted(obj),
      op  = "\\sim",
      rhs = family_distribution_text(fam_name, mu, obj)
    )
    rows[[length(rows) + 1L]] <- list(
      lhs = link_application(link, mu),
      op  = "=",
      rhs = linear_predictor_rhs(obj, notation)
    )
  }

  rows <- c(rows, dpar_linear_predictor_rows(obj, notation))
  rows <- c(rows, term_definition_rows(obj, notation))
  rows <- c(rows, latent_dynamics_rows(obj, notation))

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
  rows <- list()
  for (dp in present) {
    link <- dpar_default_link(dp)
    sym <- paste0("\\", dp, "_{i,t}")
    # Greek for canonical dpars; else just the name in roman.
    sym <- dpar_symbol(dp)
    lhs <- link_application(link, sym)
    rhs <- dpar_predictor_rhs(prior, dp)
    rows[[length(rows) + 1L]] <- list(
      lhs = lhs, op = "=", rhs = rhs
    )
  }
  rows
}

#' @noRd
dpar_default_link <- function(dp) {
  # Mirrors brms's default link per distributional parameter.
  switch(
    dp,
    sigma = "log", phi = "log", shape = "log", kappa = "log",
    nu = "identity", hu = "logit",
    zi = "logit", mu = "identity",
    "identity"
  )
}

#' @noRd
dpar_symbol <- function(dp) {
  # Map common dpar names to their Greek / mathematical form.
  base <- switch(
    dp,
    sigma = "\\sigma", phi = "\\phi", shape = "\\alpha",
    kappa = "\\kappa", nu = "\\nu",
    hu = "\\pi_{\\text{hu}}", zi = "\\pi_{\\text{zi}}",
    mu = "\\mu",
    paste0("\\text{", dp, "}")
  )
  paste0(base, "_{i,t}")
}

#' @noRd
dpar_predictor_rhs <- function(prior, dp) {
  # Build the linear-predictor RHS for one dpar, mirroring the
  # main linear predictor: intercept (when present) + per-coef
  # b rows. Smooth / GP / RE / mo / me on dpar sub-formulas are
  # uncommon enough that we render the bare fixed-effect form
  # here and surface them via the prior table.
  has_int <- any(prior$class == "Intercept" & prior$dpar == dp)
  b_rows <- prior$class == "b" & prior$dpar == dp & nzchar(prior$coef)
  coefs <- unique(prior$coef[b_rows])
  parts <- character(0L)
  if (has_int) {
    parts <- c(parts, paste0("\\alpha^{(", dp, ")}"))
  }
  for (co in coefs) {
    parts <- c(parts, paste0(
      "\\beta_{", dp, ",", co, "} ", co, "_{i,t}"
    ))
  }
  if (length(parts) == 0L) return("0")
  paste(parts, collapse = " + ")
}

#' @noRd
model_glossary <- function(obj) {
  fam <- obj$family
  fam_name <- fam$family %||% "gaussian"
  link <- fam$link %||% "identity"
  defs <- c(
    paste0("- $i$ indexes observations, $t$ indexes time"),
    paste0(
      "- $\\mu_{i,t}$: conditional mean of $",
      response_letter(obj), "_{i,t}$ on the ", link, "-link scale"
    ),
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
    defs <- c(defs, paste0(
      "- $\\eta_{i,t}$: latent ", label,
      " trend at series $i$ time $t$"
    ))
    defs <- c(defs, paste0(
      "- $\\epsilon^{(\\eta)}_{i,t}$: process innovation, ",
      "SD $\\sigma_\\eta$"
    ))
    if (identical(tt, "AR") || identical(tt, "VAR") ||
        identical(tt, "ARMA")) {
      defs <- c(defs, paste0(
        "- $\\phi_l$: autoregressive coefficient at lag $l$"
      ))
    }
    if (identical(tt, "CAR")) {
      defs <- c(defs, paste0(
        "- $\\rho$: continuous-time AR decay rate"
      ))
    }
  }
  # Blank line before the bullets so Pandoc / Quarto picks up the
  # list rather than running it on as one paragraph.
  paste(c("where:", "", defs), collapse = "\n")
}

#' @noRd
trend_order_label <- function(obj) {
  tt <- obj$trend_metadata$trend_type
  if (is.null(tt)) return("")
  ar_lags <- obj$trend_metadata$ar_lags %||% integer(0L)
  ma_lags <- obj$trend_metadata$ma_lags %||% integer(0L)
  switch(
    tt,
    "RW"   = "RW",
    "AR"   = paste0("AR(", paste(ar_lags, collapse = ", "), ")"),
    "VAR"  = paste0("VAR(", paste(ar_lags, collapse = ", "), ")"),
    "ARMA" = paste0(
      "ARMA(", paste(ar_lags, collapse = ", "), ", ",
      paste(ma_lags, collapse = ", "), ")"
    ),
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
  nm <- obj$response_names
  if (is.null(nm) || length(nm) == 0L) return("Y")
  if (length(nm) > 1L) return("\\mathbf{Y}")
  nm[[1L]]
}

#' @noRd
response_subscripted <- function(obj) {
  paste0(response_letter(obj), "_{i,t}")
}

#' @noRd
mu_symbol <- function(obj) {
  if (length(get_response_names(obj)) > 1L) {
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
  has_int <- any(prior$class == "Intercept" & nzchar(prior$dpar) == FALSE)
  b_rows <- prior$class == "b" & !nzchar(prior$dpar) &
    nzchar(prior$coef)
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
  rhs <- f[[3L]]
  specs <- list()
  walk <- function(e) {
    if (is.call(e)) {
      head <- tryCatch(as.character(e[[1L]]),
                        error = function(err) "")
      if (identical(head, "me")) {
        args <- as.list(e)[-1L]
        if (length(args) >= 1L) {
          var <- as.character(args[[1L]])
          sdvar <- if (length(args) >= 2L) {
            as.character(args[[2L]])
          } else NA_character_
          specs[[length(specs) + 1L]] <<- list(
            var = var, sdvar = sdvar,
            coef = paste0("me", var,
                          if (!is.na(sdvar)) sdvar else "")
          )
        }
      } else {
        for (k in seq_along(e)[-1L]) walk(e[[k]])
      }
    }
  }
  walk(rhs)
  specs
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
obs_smooth_terms_from_prior <- function(prior) {
  # Returns one subscript label per smooth term. For 1D smooths
  # this is the bare variable; for tensor smooths it is the
  # comma-joined variable list ("x, z") so the math subscript
  # matches what the renderer emits.
  specs <- obs_smooth_specs_from_prior(prior)
  vapply(specs, function(s) paste(s$vars, collapse = ", "),
         character(1L))
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
#' into a spec list with `var / vars / k / bs / fname` fields. Returns
#' a default spec with all-NA fields when `coef_str` does not parse
#' as a call; this keeps the term-definition loop tolerant of stray
#' prior-table rows without raising during methods_md rendering.
#' @noRd
parse_smooth_coef <- function(coef_str) {
  default <- list(
    var = NA_character_, vars = NA_character_, k = NA_integer_,
    bs = "tp", fname = "s"
  )
  expr <- tryCatch(
    parse(text = coef_str)[[1L]], error = function(e) NULL
  )
  if (is.null(expr) || !is.call(expr)) return(default)
  fname <- as.character(expr[[1L]])
  call_args <- as.list(expr)[-1L]
  arg_names <- names(call_args) %||% rep("", length(call_args))
  pos_idx <- which(arg_names == "")
  vars <- if (length(pos_idx) >= 1L) {
    vapply(
      pos_idx,
      function(i) as.character(call_args[[i]]),
      character(1L)
    )
  } else NA_character_
  k <- if ("k" %in% arg_names) {
    tryCatch(
      as.integer(eval(call_args$k)),
      error = function(e) NA_integer_
    )
  } else NA_integer_
  bs <- if ("bs" %in% arg_names) {
    tryCatch(
      as.character(eval(call_args$bs)),
      error = function(e) "tp"
    )
  } else "tp"
  list(
    var = vars[1L], vars = vars, k = k, bs = bs, fname = fname
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
obs_re_groups_from_prior <- function(prior) {
  # Returns a bare character vector for back-compat (existing
  # call sites that only want group names).
  specs <- obs_re_specs_from_prior(prior)
  vapply(specs, function(s) s$group, character(1L))
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
  rhs <- f[[3L]]
  specs <- list()
  walk <- function(e) {
    if (is.call(e)) {
      head <- tryCatch(as.character(e[[1L]]),
                        error = function(err) "")
      if (identical(head, "gp")) {
        specs[[length(specs) + 1L]] <<- gp_call_to_spec(e)
      } else {
        for (k in seq_along(e)[-1L]) walk(e[[k]])
      }
    }
  }
  walk(rhs)
  specs
}

#' @noRd
gp_call_to_spec <- function(call) {
  args <- as.list(call)[-1L]
  arg_names <- names(args) %||% rep("", length(args))
  pos_mask <- arg_names == ""
  vars <- vapply(
    args[pos_mask],
    function(a) as.character(a),
    character(1L)
  )
  if (length(vars) == 0L) {
    stop(insight::format_error(
      "gp() call has no positional variable arguments."
    ))
  }
  k <- if ("k" %in% arg_names) {
    tryCatch(
      as.integer(eval(args[["k"]])),
      error = function(e) NA_integer_
    )
  } else NA_integer_
  by <- if ("by" %in% arg_names) {
    tryCatch(
      as.character(args[["by"]]),
      error = function(e) NA_character_
    )
  } else NA_character_
  cov <- if ("cov" %in% arg_names) {
    tryCatch(
      as.character(eval(args[["cov"]])),
      error = function(e) "exp_quad"
    )
  } else "exp_quad"
  list(
    vars = vars, k = k, by = by, cov = cov,
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
  list(
    list(
      lhs = "\\eta_{i,t}",
      op  = "=",
      rhs = "\\eta_{i,t-1} + \\epsilon^{(\\eta)}_{i,t}"
    ),
    list(
      lhs = "\\epsilon^{(\\eta)}_{i,t}",
      op  = "\\sim",
      rhs = "\\text{Normal}(0, \\sigma_\\eta)"
    )
  )
}

#' @noRd
render_latent_ar <- function(obj, notation) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  rhs_terms <- paste(
    paste0("\\phi_{", lags, "} \\eta_{i,t-", lags, "}"),
    collapse = " + "
  )
  list(
    list(
      lhs = "\\eta_{i,t}",
      op  = "=",
      rhs = paste0(rhs_terms, " + \\epsilon^{(\\eta)}_{i,t}")
    ),
    list(
      lhs = "\\epsilon^{(\\eta)}_{i,t}",
      op  = "\\sim",
      rhs = "\\text{Normal}(0, \\sigma_\\eta)"
    )
  )
}

#' @noRd
render_latent_var <- function(obj, notation) {
  lags <- obj$trend_metadata$ar_lags %||% 1L
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  rhs_terms <- paste(
    paste0("\\boldsymbol{\\Phi}_{", lags,
             "} \\boldsymbol{\\eta}_{t-", lags, "}"),
    collapse = " + "
  )
  innovations <- if (has_cor) {
    "\\text{MVNormal}(\\mathbf{0}, \\boldsymbol{\\Sigma})"
  } else {
    "\\text{MVNormal}(\\mathbf{0}, \\text{diag}(\\sigma_\\eta^2))"
  }
  list(
    list(
      lhs = "\\boldsymbol{\\eta}_t",
      op  = "=",
      rhs = paste0(rhs_terms, " + \\boldsymbol{\\epsilon}_t")
    ),
    list(
      lhs = "\\boldsymbol{\\epsilon}_t",
      op  = "\\sim",
      rhs = innovations
    )
  )
}

#' @noRd
render_latent_arma <- function(obj, notation) {
  ar_lags <- obj$trend_metadata$ar_lags %||% 1L
  ma_lags <- obj$trend_metadata$ma_lags %||% 1L
  ar_rhs <- paste(
    paste0("\\phi_{", ar_lags, "} \\eta_{i,t-", ar_lags, "}"),
    collapse = " + "
  )
  ma_rhs <- paste(
    paste0("\\theta_{", ma_lags,
             "} \\epsilon^{(\\eta)}_{i,t-", ma_lags, "}"),
    collapse = " + "
  )
  list(
    list(
      lhs = "\\eta_{i,t}",
      op  = "=",
      rhs = paste0(
        ar_rhs, " + \\epsilon^{(\\eta)}_{i,t} + ", ma_rhs
      )
    ),
    list(
      lhs = "\\epsilon^{(\\eta)}_{i,t}",
      op  = "\\sim",
      rhs = "\\text{Normal}(0, \\sigma_\\eta)"
    )
  )
}

#' @noRd
render_latent_car <- function(obj, notation) {
  list(
    list(
      lhs = "\\eta_{i,t}",
      op  = "=",
      rhs = paste0(
        "\\rho^{\\Delta t_{i,t}} \\eta_{i,t-1} + ",
        "\\epsilon^{(\\eta)}_{i,t}"
      )
    ),
    list(
      lhs = "\\epsilon^{(\\eta)}_{i,t}",
      op  = "\\sim",
      rhs = "\\text{Normal}(0, \\sigma_\\eta)"
    )
  )
}

#' @noRd
render_latent_zmvn <- function(obj, notation) {
  has_cor <- isTRUE(obj$trend_metadata$has_cor)
  cov <- if (has_cor) {
    "\\boldsymbol{\\Sigma}"
  } else {
    "\\text{diag}(\\sigma_\\eta^2)"
  }
  list(list(
    lhs = "\\boldsymbol{\\eta}_t",
    op  = "\\sim",
    rhs = paste0("\\text{MVNormal}(\\mathbf{0}, ", cov, ")")
  ))
}

#' @noRd
render_latent_pw <- function(obj, notation) {
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
  # Suffix the class so trend-side rows render as
  # sigma_trend / ar1_trend / Intercept_trend etc., matching the
  # Stan parameter names emitted in the combined model.
  cls <- trend_prior$class
  cls_suffixed <- ifelse(
    nzchar(cls) & !grepl("_trend$", cls),
    paste0(cls, "_trend"),
    cls
  )
  trend_prior$class <- cls_suffixed
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
  mvgam_v <- obj$mvgam_version %||% utils::packageVersion("mvgam")
  brms_v <- obj$brms_version %||% utils::packageVersion("brms")
  stan_v <- backend_stan_version(backend)
  info <- list(
    backend   = backend,
    algorithm = algorithm,
    mvgam_v   = format(mvgam_v),
    brms_v    = format(brms_v),
    stan_v    = stan_v,
    chains    = NA_integer_,
    warmup    = NA_integer_,
    iter      = NA_integer_
  )
  fit <- obj$fit
  if (!is.null(fit) &&
      methods::is(fit, "stanfit")) {
    sa <- tryCatch(
      methods::slot(fit, "stan_args"),
      error = function(e) list()
    )
    if (length(sa) > 0L) {
      info$chains <- length(sa)
      info$warmup <- as.integer(sa[[1L]]$warmup %||% NA_integer_)
      info$iter   <- as.integer(sa[[1L]]$iter   %||% NA_integer_)
    }
  }
  info
}

#' @noRd
backend_stan_version <- function(backend) {
  if (identical(backend, "cmdstanr")) {
    v <- tryCatch(
      cmdstanr::cmdstan_version(),
      error = function(e) NULL
    )
    if (!is.null(v)) return(as.character(v))
    v <- tryCatch(
      utils::packageVersion("cmdstanr"),
      error = function(e) NULL
    )
    return(if (is.null(v)) "unknown" else format(v))
  }
  v <- tryCatch(
    utils::packageVersion("rstan"),
    error = function(e) NULL
  )
  if (is.null(v)) "unknown" else format(v)
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
  # by default. Reconstruct a `brms::bf(...)` call instead so
  # the Implementation code block is copy-paste-ready.
  if (inherits(f, "mvbrmsformula")) {
    # Compose `brms::bf(yA ~ x) + brms::bf(yB ~ x) +
    # brms::set_rescor(FALSE)` by recursing into each per-
    # response brmsformula in $forms and appending the rescor
    # flag when set.
    bfs <- vapply(f$forms, formula_text, character(1L))
    out <- paste(bfs, collapse = " + ")
    if (isFALSE(f$rescor)) {
      out <- paste0(out, " + brms::set_rescor(FALSE)")
    } else if (isTRUE(f$rescor)) {
      out <- paste0(out, " + brms::set_rescor(TRUE)")
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
    if (isTRUE(f$nl)) {
      parts <- c(parts, "nl = TRUE")
    }
    return(paste0("brms::bf(", paste(parts, collapse = ", "), ")"))
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
  fam <- family$family %||% "gaussian"
  link <- family$link %||% "identity"
  default_link <- switch(
    fam,
    poisson = "log", bernoulli = "logit", binomial = "logit",
    gaussian = "identity", student = "identity",
    Gamma = "inverse", lognormal = "identity",
    beta = "logit", negbinomial = "log", nb = "log",
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
#' new prior classes are silently skipped until a renderer lands.
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

  if (identical(cls, "Intercept")) {
    if (nzchar(dpar)) {
      return(paste0("\\alpha^{(", dpar, ")}"))
    }
    return("\\alpha")
  }
  if (identical(cls, "b")) {
    if (nzchar(dpar)) {
      lbl <- if (nzchar(coef)) paste0(dpar, ",", coef) else dpar
      return(paste0("\\beta_{", lbl, "}"))
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
    idx <- if (nzchar(coef)) coef else ""
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

  args <- match_args("^normal\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{Normal}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^student_t\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0(
      "\\text{StudentT}(", paste(args, collapse = ", "), ")"
    ))
  }
  args <- match_args("^lognormal\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{LogNormal}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^exponential\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{Exponential}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^gamma\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{Gamma}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^inv_gamma\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0(
      "\\text{InvGamma}(", paste(args, collapse = ", "), ")"
    ))
  }
  args <- match_args("^cauchy\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{Cauchy}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^beta\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{Beta}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^uniform\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{Uniform}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^lkj_corr_cholesky\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{LKJCorr}(", paste(args, collapse = ", "), ")"))
  }
  args <- match_args("^lkj(_corr)?\\((.*)\\)$")
  if (!is.null(args)) {
    return(paste0("\\text{LKJCorr}(", paste(args, collapse = ", "), ")"))
  }
  # Unrecognised distribution: render verbatim wrapped in \text{}
  # so the row still appears.
  paste0("\\text{", gsub("([{}_])", "\\\\\\1", s), "}")
}
