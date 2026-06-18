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
  resp <- response_letter(obj)
  dims <- describe_data_dimensions(obj)

  lines <- c(
    "## Data",
    "",
    paste0(
      "$", resp,
      "$: ", family_data_label(fam_name)
    )
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
  resp_cols <- obj$response_names %||% character(0L)
  # Skip canonical panel keys plus the response column(s); they are
  # already covered by the dimensions line above.
  skip <- unique(c(resp_cols, "time", "series"))
  predictors <- setdiff(names(data), skip)
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
  mu <- mu_symbol(obj)
  resp <- response_subscripted(obj)

  rows <- list()
  for (ir in index_range_rows(obj)) {
    rows[[length(rows) + 1L]] <- ir
  }
  rows[[length(rows) + 1L]] <- list(
    lhs = resp,
    op  = "\\sim",
    rhs = family_distribution_text(fam_name, mu, obj)
  )
  rows[[length(rows) + 1L]] <- list(
    lhs = link_application(link, mu),
    op  = "=",
    rhs = linear_predictor_rhs(obj, notation)
  )

  rows <- c(rows, term_definition_rows(obj, notation))
  rows <- c(rows, latent_dynamics_rows(obj, notation))

  block <- align_block(rows)
  glossary <- model_glossary(obj)
  paste(c("## Model", "", block, "", glossary), collapse = "\n")
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
  gp_specs <- obs_gp_specs_from_formula(obj)
  if (length(gp_specs) == 0L) {
    gp_specs <- obs_gp_specs_from_prior(prior)
  }
  re_groups <- obs_re_groups_from_prior(prior)
  for (spec in smooth_specs) {
    bare <- spec$var
    k_label <- if (!is.na(spec$k)) {
      paste0("$K_{", bare, "} = ", spec$k, "$")
    } else {
      paste0("$K_{", bare, "}$ (mgcv default)")
    }
    defs <- c(defs, paste0(
      "- $f_{", bare, "}$: ",
      basis_label(spec$bs, spec$fname),
      " in $", bare, "$, basis size ", k_label,
      ", smoothness $\\lambda_{", bare, "}$"
    ))
  }
  for (spec in gp_specs) {
    sub <- gp_subscript(spec)
    vars <- if (!is.null(spec$vars)) spec$vars else spec$var
    dims_text <- paste(vars, collapse = ", ")
    by_text <- if (!is.null(spec$by) && !is.na(spec$by) &&
                    nzchar(spec$by)) {
      paste0(", stratified by $", spec$by, "$")
    } else ""
    k_text <- if (!is.null(spec$k) && !is.na(spec$k)) {
      paste0(", approximated with ", spec$k, " basis functions")
    } else ", approximated via Hilbert-space basis"
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
  for (grp in re_groups) {
    defs <- c(defs, paste0(
      "- $\\alpha_{", grp, "[i]}$: varying intercept across ",
      "levels of $", grp, "$ with hyper-SD $\\sigma_{", grp, "}$"
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
  resp_names <- obj$response_names
  if (length(resp_names) > 1L) {
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
family_distribution_text <- function(fam_name, mu, obj) {
  switch(
    fam_name,
    poisson     = paste0("\\text{Poisson}(", mu, ")"),
    bernoulli   = paste0("\\text{Bernoulli}(", mu, ")"),
    binomial    = paste0("\\text{Binomial}(n_{i,t}, ", mu, ")"),
    gaussian    = paste0("\\text{Normal}(", mu, ", \\sigma)"),
    student     = paste0("\\text{StudentT}(\\nu, ", mu, ", \\sigma)"),
    lognormal   = paste0("\\text{LogNormal}(", mu, ", \\sigma)"),
    Gamma       = paste0("\\text{Gamma}(\\alpha, ", mu, ")"),
    beta        = paste0("\\text{Beta}(", mu, ", \\phi)"),
    negbinomial = paste0("\\text{NegBin}(", mu, ", \\phi)"),
    nb          = paste0("\\text{NegBin}(", mu, ", \\phi)"),
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
  gp <- obs_gp_specs_from_formula(obj)
  if (length(gp) == 0L) gp <- obs_gp_specs_from_prior(prior)
  list(
    fixed  = obs_fixed_terms_from_prior(prior),
    smooth = obs_smooth_terms_from_prior(prior),
    gp     = gp,
    re     = obs_re_groups_from_prior(prior)
  )
}

#' @noRd
obs_fixed_terms_from_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(character(0L))
  has_int <- any(prior$class == "Intercept" & nzchar(prior$dpar) == FALSE)
  b_rows <- prior$class == "b" & !nzchar(prior$dpar) &
    nzchar(prior$coef)
  coefs <- prior$coef[b_rows]
  # Drop smooth-basis stubs (sx_1 etc) and the bare "" umbrella row;
  # those are not user-supplied population effects.
  coefs <- coefs[!grepl("^s[A-Za-z0-9_]+_[0-9]+$", coefs)]
  terms <- unique(coefs)
  if (has_int) c("Intercept", terms) else terms
}

#' @noRd
obs_smooth_terms_from_prior <- function(prior) {
  specs <- obs_smooth_specs_from_prior(prior)
  vapply(specs, function(s) s$var, character(1L))
}

#' @noRd
obs_smooth_specs_from_prior <- function(prior) {
  if (is.null(prior) || nrow(prior) == 0L) return(list())
  sds_rows <- prior$class == "sds" & nzchar(prior$coef)
  if (!any(sds_rows)) return(list())
  coefs <- prior$coef[sds_rows]
  specs <- lapply(coefs, parse_smooth_coef)
  # Dedupe by variable; first-occurrence wins.
  seen <- character(0L)
  out <- list()
  for (s in specs) {
    if (is.null(s$var) || is.na(s$var) || s$var %in% seen) next
    out[[length(out) + 1L]] <- s
    seen <- c(seen, s$var)
  }
  out
}

#' @noRd
parse_smooth_coef <- function(coef_str) {
  default <- list(
    var = NA_character_, k = NA_integer_,
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
  var <- if (length(pos_idx) >= 1L) {
    as.character(call_args[[pos_idx[1L]]])
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
  list(var = var, k = k, bs = bs, fname = fname)
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
  if (is.null(prior) || nrow(prior) == 0L) return(character(0L))
  sd_rows <- prior$class == "sd" & nzchar(prior$group)
  if (!any(sd_rows)) return(character(0L))
  unique(prior$group[sd_rows])
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
obs_gp_specs_from_formula <- function(obj) {
  # Walk the obs formula AST for `gp(...)` calls and recover the
  # full spec per term: variable list, k, by, cov kernel. This
  # is the authoritative extractor; the prior-table fallback
  # loses 2D and by-factor detail.
  f <- obj$formula
  if (is.null(f)) return(list())
  if (inherits(f, c("brmsformula", "bform", "mvbrmsformula"))) {
    f <- f$formula %||% f
  }
  rhs <- tryCatch(stats::as.formula(f)[[length(stats::as.formula(f))]],
                   error = function(e) NULL)
  if (is.null(rhs)) return(list())
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
render_smooth_inline <- function(smooths) {
  bare <- sub("^s_", "", smooths)
  paste(
    paste0("f_{", bare, "}(", bare, "_{i,t})"),
    collapse = " + "
  )
}

#' @noRd
render_gp_inline <- function(specs) {
  paste(
    vapply(specs, function(s) {
      vars <- if (!is.null(s$vars)) s$vars else s$var
      sub <- gp_subscript(s)
      vars_in <- paste(
        paste0(vars, "_{i,t}"),
        collapse = ", "
      )
      paste0(
        "f^{(\\text{gp})}_{", sub, "}(", vars_in, ")"
      )
    }, character(1L)),
    collapse = " + "
  )
}

#' @noRd
gp_subscript <- function(spec) {
  vars <- if (!is.null(spec$vars)) spec$vars else spec$var
  base <- paste(vars, collapse = ", ")
  if (!is.null(spec$by) && !is.na(spec$by) && nzchar(spec$by)) {
    paste0(base, " \\mid ", spec$by)
  } else {
    base
  }
}

#' @noRd
render_re_inline <- function(groups) {
  # McElreath-style varying intercepts: alpha_{group[i]}
  paste(
    paste0("\\alpha_{", groups, "[i]}"),
    collapse = " + "
  )
}

#' @noRd
term_definition_rows <- function(obj, notation) {
  prior <- obj$prior
  smooths <- obs_smooth_terms_from_prior(prior)
  gp_specs <- obs_gp_specs_from_formula(obj)
  if (length(gp_specs) == 0L) {
    gp_specs <- obs_gp_specs_from_prior(prior)
  }
  re_groups <- obs_re_groups_from_prior(prior)

  rows <- list()
  for (bare in smooths) {
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("f_{", bare, "}(", bare, ")"),
      op  = "=",
      rhs = paste0(
        "\\sum_{k=1}^{K_{", bare, "}} ",
        "\\beta^{(", bare, ")}_k B_k(", bare, ")"
      )
    )
  }
  for (spec in gp_specs) {
    sub <- gp_subscript(spec)
    vars <- if (!is.null(spec$vars)) spec$vars else spec$var
    vars_in <- paste(vars, collapse = ", ")
    rho_arg <- if (length(vars) > 1L) {
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
  for (grp in re_groups) {
    # Bare cluster letter in the definition (no [i]); the [i]
    # subscript only appears when the varying intercept is used
    # in the linear predictor above.
    rows[[length(rows) + 1L]] <- list(
      lhs = paste0("\\alpha_{", grp, "}"),
      op  = "\\sim",
      rhs = paste0("\\text{Normal}(0, \\sigma_{", grp, "})")
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
  # parameter class, drop the umbrella. The specifics carry the
  # actual labels; the umbrella renders as a generic placeholder.
  vec_classes <- c("b", "sd", "sds")
  drop <- logical(nrow(prior))
  for (cls in unique(prior$class[prior$class %in% vec_classes])) {
    cls_rows <- prior$class == cls
    has_specific <- any(
      cls_rows & (nzchar(prior$coef) | nzchar(prior$group))
    )
    if (has_specific) {
      drop <- drop | (cls_rows & !nzchar(prior$coef) &
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
  pri <- prior$prior %||% rep("", nrow(prior))
  has_text <- !is.na(pri) & nzchar(pri) & pri != "(flat)"
  # Umbrella rows: class set, dpar / nlpar set or unset, coef and
  # group both empty, prior text non-empty.
  umbrella_mask <- has_text &
    !nzchar(prior$coef %||% "") &
    !nzchar(prior$group %||% "")
  umbrella <- prior[umbrella_mask, , drop = FALSE]
  if (nrow(umbrella) == 0L) return(prior)
  for (i in seq_len(nrow(prior))) {
    if (has_text[i]) next
    cls <- prior$class[i]
    dp <- dpar[i]
    nlp <- nlpar[i]
    matches <- umbrella$class == cls &
      (umbrella$dpar %||% rep("", nrow(umbrella))) == dp &
      (umbrella$nlpar %||% rep("", nrow(umbrella))) == nlp
    if (any(matches)) {
      prior$prior[i] <- umbrella$prior[which(matches)[1L]]
    }
  }
  prior
}

#' @noRd
format_parameter_symbol <- function(row) {
  cls <- row$class %||% ""
  coef <- row$coef %||% ""
  group <- row$group %||% ""
  dpar <- row$dpar %||% ""

  if (identical(cls, "Intercept")) {
    return("\\alpha")
  }
  if (identical(cls, "b")) {
    if (nzchar(dpar)) {
      lbl <- if (nzchar(coef)) paste0(dpar, ",", coef) else dpar
      return(paste0("\\beta_{", lbl, "}"))
    }
    if (nzchar(coef)) {
      return(paste0("\\beta_{", coef, "}"))
    }
    return("\\boldsymbol{\\beta}")
  }
  if (identical(cls, "sigma")) return("\\sigma")
  if (identical(cls, "shape")) return("\\alpha")
  if (identical(cls, "nu"))    return("\\nu")
  if (identical(cls, "phi"))   return("\\phi")
  if (identical(cls, "zi"))    return("\\pi_{\\text{zi}}")
  if (identical(cls, "hu"))    return("\\pi_{\\text{hu}}")
  if (identical(cls, "sd")) {
    grp <- if (nzchar(group)) group else "j"
    return(paste0("\\sigma_{", grp, "}"))
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
    return("\\mathbf{L}")
  }
  if (identical(cls, "cor")) {
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
