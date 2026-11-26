#' Trait-mediated responses for `jsdgam()`
#'
#' Internal helpers that expand `formula` and `trait_slopes` into the
#' fourth-corner model
#'
#' \deqn{\beta_{s,k} = \gamma_{0,k} + \sum_l T_{s,l}\, \gamma_{l,k} +
#'   \epsilon_{s,k}}
#'
#' where each term `k` of `formula` takes a shared effect, an effect of
#' each trait `l` and a species deviation. The expansion adds terms to
#' `formula` and leaves its own terms as written:
#'
#'   `formula      = y ~ env + s(x)`
#'   `trait_slopes = ~ size`
#'
#' becomes
#'
#'   `y ~ env + s(x) + size + env:size + s(x, by = size) +
#'      (1 + env | sp | series) + s(x, by = series, m = 1)`
#'
#' The `|sp|` tag correlates the species deviations of the intercept and
#' the linear terms under one LKJ prior. A smooth's species deviation is
#' a smooth per species penalised towards a flat line (the "GI" model of
#' Pedersen et al. 2019). [species_effects()] reports the species'
#' responses and the trait effects.
#'
#' @references Pedersen, E. J., Miller, D. L., Simpson, G. L. and Ross,
#'   N. (2019). Hierarchical generalized additive models in ecology: an
#'   introduction with mgcv. *PeerJ* 7:e6876. \doi{10.7717/peerj.6876}
#'
#' @name trait_slopes_helpers
#' @keywords internal
NULL


#' The kind of one term of a formula
#'
#' @param label A term label, as `stats::terms()` writes it
#' A smooth with `bs = "re"` or `bs = "fs"` is a group-level term
#' written as a smooth.
#'
#' @return `"group"` for a group-level term, `"smooth"` for `s()` and
#'   `t2()` (and the `te()` and `ti()` brms refuses), `"gp"` for `gp()`,
#'   or `"linear"` for a term entering through its design columns
#' @noRd
trait_term_kind <- function(label) {
  if (grepl("|", label, fixed = TRUE)) {
    return("group")
  }
  if (grepl("^(s|t2|te|ti)\\(", label)) {
    basis <- as.character(str2lang(label)$bs)
    return(if (any(c("re", "fs") %in% basis)) "group" else "smooth")
  }
  if (grepl("^gp\\(", label)) {
    return("gp")
  }
  "linear"
}


#' The terms of a one- or two-sided formula with their kinds
#'
#' @param f A formula
#' @return List of `labels`, their `kinds` and `intercept`, whether the
#'   formula keeps its intercept
#' @noRd
formula_term_kinds <- function(f) {
  tt <- stats::terms(f, keep.order = TRUE)
  labels <- attr(tt, "term.labels")
  list(
    labels = labels,
    kinds = vapply(labels, trait_term_kind, character(1L), USE.NAMES = FALSE),
    intercept = attr(tt, "intercept") == 1L
  )
}


#' A smooth or `gp()` term with arguments set
#'
#' @param label The term label
#' @param ... Named arguments to set, as symbols or values. `NULL`
#'   removes the argument.
#' @return The term label with the arguments set
#' @noRd
set_term_args <- function(label, ...) {
  call <- str2lang(label)
  args <- list(...)
  for (nm in names(args)) {
    if (!is.null(args[[nm]])) {
      call[[nm]] <- args[[nm]]
    } else if (nm %in% names(call)) {
      call[[nm]] <- NULL
    }
  }
  paste(deparse(call, width.cutoff = 500L), collapse = " ")
}


#' The covariates of a smooth or `gp()` term, without its options
#'
#' @param label The term label
#' @return List of the positional arguments, as language objects
#' @noRd
term_covariates <- function(label) {
  args <- as.list(str2lang(label))[-1L]
  named <- names(args) %||% rep("", length(args))
  args[!nzchar(named)]
}


#' The named arguments of a smooth or `gp()` term, its options
#'
#' @param label The term label
#' @return Named list of the options, as language objects or values
#' @noRd
term_options <- function(label) {
  args <- as.list(str2lang(label))[-1L]
  named <- names(args) %||% rep("", length(args))
  args[nzchar(named)]
}


#' The margins a smooth gives a `t2()` surface
#'
#' An `s()` of several covariates is one isotropic margin. A `t2()`
#' gives one margin per entry of its `d`, one per covariate by default.
#'
#' @param label The smooth's term label
#' @return List of margins, each a list of its dimension `d`, its basis
#'   size `k` (-1 for mgcv's default) and its basis `bs`
#' @noRd
smooth_margins <- function(label) {
  options <- term_options(label)
  n_vars <- length(term_covariates(label))
  d <- if (identical(str2lang(label)[[1L]], as.name("t2"))) {
    eval(options$d %||% rep(1, n_vars))
  } else {
    n_vars
  }
  k <- rep_len(eval(options$k %||% -1), length(d))
  bs <- rep_len(eval(options$bs %||% "tp"), length(d))
  lapply(seq_along(d), function(i) list(d = d[[i]], k = k[[i]], bs = bs[[i]]))
}


#' Give a `t2()` surface margins with no unpenalised space
#'
#' A first-order margin penalises everything but its constant, which
#' `t2()` removes. The surface then repeats no column of the main
#' smooths. A margin of one covariate is a thin plate spline with
#' `m = 1`, or keeps a cyclic basis. A thin plate margin of several
#' covariates needs `m >= 2`. It takes a Duchon spline with
#' `m = c(1, 0.5)`, isotropic as the `s()` it comes from.
#'
#' @param surface The `t2()` call
#' @param margins The `smooth_margins()` of its smooths, in order
#' @return The call with `d`, `bs`, `m` and `k` set
#' @noRd
set_surface_margins <- function(surface, margins) {
  d <- vapply(margins, `[[`, numeric(1L), "d")
  k <- vapply(margins, `[[`, numeric(1L), "k")
  bs <- vapply(margins, function(margin) {
    if (margin$d > 1) "ds" else if (margin$bs %in% c("cc", "cp")) {
      margin$bs
    } else {
      "tp"
    }
  }, character(1L))
  m <- lapply(d, function(dim) if (dim > 1) c(1, 0.5) else 1)
  if (any(d > 1)) {
    surface$d <- d
  }
  surface$bs <- if (all(bs == "tp")) "tp" else bs
  surface$m <- if (all(d == 1)) 1 else m
  if (any(k != -1)) {
    surface$k <- k
  }
  surface
}


#' A term's `by` variable, or `NULL`
#'
#' @param label The term label
#' @return The `by` argument as a language object, or `NULL` for a
#'   term without one, a bare column among them
#' @noRd
term_by <- function(label) {
  expr <- str2lang(label)
  if (is.call(expr)) expr$by
}


#' Whether a term is one numeric column of the data
#'
#' A `by` variable has to be one. A smooth or a term written as an
#' expression has no single column.
#'
#' @param label The term label
#' @param data The data `jsdgam()` fits
#' @return Logical scalar
#' @noRd
is_numeric_column_term <- function(label, data) {
  expr <- str2lang(label)
  is.name(expr) && is.numeric(data[[as.character(expr)]])
}


#' The point a smooth scaled by a numeric column passes through zero at
#'
#' mgcv leaves a smooth with a numeric `by` uncentred. Its constant times
#' the `by` column would repeat that column's own coefficient. The point
#' constraint `pc` at the covariates' means removes it, leaving the
#' column's coefficient as the effect at the means.
#'
#' @param label The smooth's term label
#' @param data The data `jsdgam()` fits
#' @return Named list of each covariate's mean
#' @noRd
smooth_mean_point <- function(label, data) {
  vars <- vapply(term_covariates(label), deparse, character(1L))
  lapply(stats::setNames(vars, vars), function(v) {
    signif(mean(data[[v]], na.rm = TRUE), 4L)
  })
}


#' A smooth or `gp()` term scaled by one numeric column
#'
#' A smooth takes the point constraint of `smooth_mean_point()` when
#' `centre` is `TRUE`. A Gaussian process has no unpenalised constant
#' and takes none.
#'
#' @param label The smooth or `gp()` term label
#' @param column The numeric column's name
#' @param data The data `jsdgam()` fits
#' @param centre Whether the scaled smooth passes through zero at the
#'   covariates' means
#' @return The term label
#' @noRd
scale_by_column <- function(label, column, data, centre = TRUE) {
  if (trait_term_kind(label) == "gp" || !centre) {
    return(set_term_args(label, by = str2lang(column)))
  }
  # mgcv's `t2()` tests a point constraint's length against each
  # margin's dimension, an error for two margins or more
  if (identical(str2lang(label)[[1L]], as.name("t2")) &&
      length(term_covariates(label)) > 1L) {
    stop(insight::format_error(c(
      paste0("Scaling '", label, "' by '", column, "' is not supported."),
      x = "mgcv's 't2()' rejects the point constraint that centres it.",
      i = "An 's()' of the same covariates, as in 's(x, z)', scales."
    )), call. = FALSE)
  }
  set_term_args(label, by = str2lang(column),
                pc = smooth_mean_point(label, data))
}


#' The design of a formula's linear terms, fixed on the fitted data
#'
#' The terms carry `predvars`, which hold the coefficients of `poly()`
#' and `scale()`. The factor levels and contrasts are those of the
#' fitted data. `trait_design_matrix()` rebuilds the same columns on
#' any frame.
#'
#' @param form A one-sided formula of linear terms
#' @param data The data `jsdgam()` fits
#' @return List of `terms`, `xlev` and `contrasts`
#' @noRd
trait_design <- function(form, data) {
  mf <- stats::model.frame(form, data, na.action = stats::na.pass)
  tt <- stats::terms(mf)
  X <- stats::model.matrix(tt, mf)
  list(terms = tt, xlev = stats::.getXlevels(tt, mf),
       contrasts = attr(X, "contrasts"))
}


#' The columns of a `trait_design()` on a frame
#'
#' @param design A `trait_design()`
#' @param data Frame holding the design's variables
#' @return Model matrix with columns named as [fixef.mvgam()] names
#'   them. Its `assign` attribute numbers each column's term.
#' @noRd
trait_design_matrix <- function(design, data) {
  mf <- stats::model.frame(design$terms, data, xlev = design$xlev,
                           na.action = stats::na.pass)
  X <- stats::model.matrix(design$terms, mf,
                           contrasts.arg = design$contrasts)
  colnames(X) <- brms_rename(colnames(X))
  X
}


#' The columns `trait_slopes` adds to the data
#'
#' A smooth or a `gp()` takes one column of the data as its `by`. A
#' term whose design spans other columns enters through derived
#' columns:
#' \itemize{
#'   \item a product of design columns, such as `habb` for the level
#'     `b` of a factor `hab`, or `env_seriessp1` for `env` in the rows
#'     of species `sp1`;
#'   \item an interaction of factors, such as `hab_series`.
#' }
#' The registry records each column as the expansion asks for it.
#' `derive_trait_columns()` computes them on the fitted data and on
#' every prediction frame.
#'
#' @param data The data `jsdgam()` fits
#' @param env The environment of the observation formula
#' @return List of functions: `pieces()`, `product()`, `interaction()`
#'   and `spec()`
#' @noRd
trait_column_registry <- function(data, env) {
  designs <- list()
  derived <- list()
  # A derived name avoids the user's columns
  free_name <- function(base, pieces) {
    name <- gsub("[^[:alnum:]_.]", "_", base)
    while (name %in% names(data) ||
             (!is.null(derived[[name]]) &&
                !identical(derived[[name]]$pieces, pieces))) {
      name <- paste0(name, "_")
    }
    name
  }
  list(
    # One piece per design column of a term of `form`
    pieces = function(form, label) {
      key <- deparse1(form)
      if (is.null(designs[[key]])) {
        designs[[key]] <<- trait_design(form, data)
      }
      X <- trait_design_matrix(designs[[key]], data)
      at <- match(label, attr(designs[[key]]$terms, "term.labels"))
      columns <- colnames(X)[attr(X, "assign") == at]
      lapply(columns, function(column) {
        list(design = key, column = column, term = label,
             width = length(columns))
      })
    },
    # The column of a product of pieces. A lone column of the data is
    # its own name.
    product = function(pieces) {
      if (length(pieces) == 1L && is.null(pieces[[1L]]$design)) {
        return(pieces[[1L]]$column)
      }
      columns <- vapply(pieces, `[[`, character(1L), "column")
      name <- free_name(paste(columns, collapse = "_"), pieces)
      derived[[name]] <<- list(type = "product", pieces = pieces)
      name
    },
    # The factor crossing the values of `exprs`
    interaction = function(exprs) {
      pieces <- lapply(exprs, deparse1)
      name <- free_name(paste(vapply(pieces, brms_rename, character(1L)),
                              collapse = "_"), pieces)
      values <- lapply(exprs, eval, envir = data, enclos = env)
      derived[[name]] <<- list(
        type = "interaction", pieces = pieces,
        levels = levels(interaction(values, sep = "_", drop = TRUE))
      )
      name
    },
    spec = function() {
      if (!length(derived)) {
        return(NULL)
      }
      list(designs = designs, derived = derived, env = env)
    }
  )
}


#' The variables each derived column is computed from
#'
#' @param spec What the registry's `spec()` gave, or `NULL`
#' @return Named list of character vectors, one per derived column
#' @noRd
derived_column_sources <- function(spec) {
  lapply(spec$derived, function(d) {
    unique(unlist(lapply(d$pieces, function(p) {
      if (identical(d$type, "interaction")) {
        return(all.vars(str2lang(p)))
      }
      # A design column comes from its own term. The design holds every
      # linear term of one formula.
      if (is.null(p$design)) p$column else all.vars(str2lang(p$term))
    })))
  })
}


#' Give a frame the columns `trait_slopes` derived
#'
#' Each column is computed from the frame's own variables, replacing any
#' value the frame holds. A prediction that changes a covariate then
#' changes its derived columns.
#'
#' @param spec What the registry's `spec()` gave, or `NULL`
#' @param data A data frame
#' @return `data` with every derived column
#' @noRd
derive_trait_columns <- function(spec, data) {
  if (is.null(spec) || !is.data.frame(data)) {
    return(data)
  }
  missing <- setdiff(unlist(derived_column_sources(spec)), names(data))
  if (length(missing)) {
    stop(insight::format_error(c(
      "The frame lacks variables the 'trait_slopes' terms are built from.",
      x = paste0("Missing: ", paste0("'", missing, "'", collapse = ", "),
                 ".")
    )), call. = FALSE)
  }
  X <- lapply(spec$designs, trait_design_matrix, data = data)
  for (name in names(spec$derived)) {
    d <- spec$derived[[name]]
    data[[name]] <- if (identical(d$type, "interaction")) {
      values <- lapply(d$pieces, function(p) {
        eval(str2lang(p), envir = data, enclos = spec$env)
      })
      factor(as.character(interaction(values, sep = "_")),
             levels = d$levels)
    } else {
      Reduce(`*`, lapply(d$pieces, function(p) {
        if (is.null(p$design)) data[[p$column]] else {
          X[[p$design]][, p$column]
        }
      }))
    }
  }
  data
}


#' What a derived column holds, as the methods text describes it
#'
#' A factor level is written `hab = b`. A term of one column is written
#' as the formula writes it, `I(env^2)`. A column of a wider term adds
#' its name, `poly(x, 2) (polyx21)`.
#'
#' @param spec What the registry's `spec()` gave
#' @param name The derived column
#' @return Character phrase, such as "product of hab = b and size"
#' @noRd
derived_column_text <- function(spec, name) {
  d <- spec$derived[[name]]
  if (identical(d$type, "interaction")) {
    return(paste0("factor crossing ", paste(d$pieces, collapse = " with ")))
  }
  pieces <- vapply(d$pieces, function(p) {
    if (is.null(p$design)) {
      return(p$column)
    }
    xlev <- spec$designs[[p$design]]$xlev
    for (v in names(xlev)) {
      at <- brms_rename(paste0(v, xlev[[v]])) == p$column
      if (any(at)) {
        return(paste0(v, " = ", xlev[[v]][at][1L]))
      }
    }
    if (p$width == 1L) p$term else paste0(p$term, " (", p$column, ")")
  }, character(1L))
  if (length(pieces) == 1L) {
    return(paste0("column of ", pieces))
  }
  paste0("product of ", paste(pieces, collapse = " and "))
}


#' The formula of linear terms whose design columns a smooth is scaled by
#'
#' The special terms `mo()`, `me()` and `mi()` have no design columns.
#' They are left out.
#'
#' @param labels Term labels
#' @param intercept Whether the formula keeps its intercept
#' @param env The formula's environment
#' @return A one-sided formula, or `NULL` without a linear term
#' @noRd
trait_design_formula <- function(labels, intercept, env) {
  labels <- labels[!vapply(labels, is_brms_special_term, logical(1L))]
  if (!length(labels)) {
    return(NULL)
  }
  stats::reformulate(labels, intercept = intercept, env = env)
}


#' Whether a term holds a `mo()`, `me()` or `mi()` call
#'
#' @param label The term label
#' @return Logical scalar
#' @noRd
is_brms_special_term <- function(label) {
  length(find_calls(str2lang(label), c("mo", "me", "mi"))) > 0L
}


#' The pieces a term scales a smooth by
#'
#' A numeric column of the data is its own piece. Any other term takes
#' one piece per design column of `form`.
#'
#' @param label The term label, a term of `form`
#' @param form The `trait_design_formula()` holding it
#' @param reg The `trait_column_registry()`
#' @param data The data `jsdgam()` fits
#' @return List of pieces
#' @noRd
term_pieces <- function(label, form, reg, data) {
  if (is_numeric_column_term(label, data)) {
    return(list(list(design = NULL, column = label)))
  }
  reg$pieces(form, label)
}


#' The pieces of a smooth's `by` variable
#'
#' A factor gives one indicator per level, a numeric `by` one piece.
#'
#' @param by The `by` argument, as a language object
#' @param reg The `trait_column_registry()`
#' @param data The data `jsdgam()` fits
#' @param env The environment of the observation formula
#' @return List of pieces
#' @noRd
by_pieces <- function(by, reg, data, env) {
  label <- deparse1(by)
  form <- stats::reformulate(label, intercept = FALSE, env = env)
  term_pieces(attr(stats::terms(form), "term.labels"), form, reg, data)
}


#' Whether a smooth's `by` variable is a factor
#'
#' @param by The `by` argument, as a language object
#' @param data The data `jsdgam()` fits
#' @param env The environment of the observation formula
#' @return Logical scalar
#' @noRd
is_factor_by <- function(by, data, env) {
  value <- eval(by, envir = data, enclos = env)
  is.factor(value) || is.character(value)
}


#' Refuse to scale a smooth by a special term
#'
#' @param label The term label
#' @param smooth The smooth it would scale
#' @return `NULL`, invisibly
#' @noRd
refuse_special_scale <- function(label, smooth) {
  if (!is_brms_special_term(label)) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    paste0("Scaling '", smooth, "' by '", label, "' is not supported."),
    x = "brms scales a smooth by a column of the data.",
    i = paste0("A linear trait, as in '~ size', crosses '", label, "'.")
  )), call. = FALSE)
}


#' The terms giving a covariate's response to one trait
#'
#' A linear covariate and a linear trait take their interaction. A
#' smooth of one scaled by the other takes one smooth per design column
#' of the linear term. A smooth with a `by` variable scales each of its
#' smooths by the trait. A smooth of a covariate and a smooth of a trait
#' take a `t2()` surface with the first-order margins of
#' `set_surface_margins()`.
#'
#' @param covariate,trait Term labels of `formula` and of `trait_slopes`
#' @param reg The `trait_column_registry()`
#' @param forms The `trait_design_formula()` of the covariates and of
#'   the traits
#' @param data The data `jsdgam()` fits
#' @return Character vector of term labels
#' @noRd
trait_crossing <- function(covariate, trait, reg, forms, data) {
  cov_kind <- trait_term_kind(covariate)
  trait_kind <- trait_term_kind(trait)
  if (cov_kind == "linear" && trait_kind == "linear") {
    return(paste0(covariate, ":", trait))
  }
  if (cov_kind == "linear") {
    # A smooth of the trait, scaled by each of the covariate's columns
    refuse_special_scale(covariate, trait)
    pieces <- term_pieces(covariate, forms$covariates, reg, data)
    return(vapply(pieces, function(piece) {
      scale_by_column(trait, reg$product(list(piece)), data)
    }, character(1L)))
  }
  if (trait_kind == "linear") {
    # The covariate's smooths, each scaled by the trait's columns
    refuse_special_scale(trait, covariate)
    by <- term_by(covariate)
    base <- set_term_args(covariate, by = NULL)
    by_cols <- if (is.null(by)) list(NULL) else {
      by_pieces(by, reg, data, forms$env)
    }
    # mgcv centres a smooth without `by` and each smooth of a factor
    # `by`. Its trait crossing is centred too. A smooth with a numeric
    # `by` is uncentred. Its constant times the trait is the trait's
    # effect on that column's slope. The crossing keeps that constant.
    centre <- is.null(by) || is_factor_by(by, data, forms$env)
    trait_cols <- term_pieces(trait, forms$traits, reg, data)
    pairs <- expand.grid(b = seq_along(by_cols), t = seq_along(trait_cols))
    return(vapply(seq_len(nrow(pairs)), function(i) {
      pieces <- Filter(Negate(is.null),
                       c(by_cols[pairs$b[i]], trait_cols[pairs$t[i]]))
      scale_by_column(base, reg$product(pieces), data, centre)
    }, character(1L)))
  }
  # A smooth of both: one surface over the covariate and the trait. A
  # `gp()` surface keeps the covariate's options, which brms applies to
  # every dimension: its `k` keeps the process approximate.
  vars <- c(term_covariates(covariate), term_covariates(trait))
  surface <- as.call(c(as.name(if (cov_kind == "gp") "gp" else "t2"), vars))
  if (cov_kind == "gp") {
    options <- term_options(covariate)
    for (nm in setdiff(names(options), "by")) {
      surface[[nm]] <- options[[nm]]
    }
  } else {
    surface <- set_surface_margins(
      surface, c(smooth_margins(covariate), smooth_margins(trait))
    )
  }
  by <- term_by(covariate)
  if (!is.null(by)) {
    surface$by <- by
  }
  paste(deparse(surface, width.cutoff = 500L), collapse = " ")
}


#' The species deviations of one smooth or `gp()` term
#'
#' A smooth without `by` takes one per species. A smooth with a factor
#' `by` takes one per level and species, through their interaction. A
#' numeric `by` scales one smooth per species, through a column holding
#' its values in that species' rows.
#'
#' @param covariate The term label
#' @param species_var The species column
#' @param reg The `trait_column_registry()`
#' @param data The data `jsdgam()` fits
#' @param env The environment of the observation formula
#' @return Character vector of term labels
#' @noRd
species_smooth_deviation <- function(covariate, species_var, reg, data,
                                     env) {
  species <- as.name(species_var)
  deviation <- function(by) {
    if (trait_term_kind(covariate) == "gp") {
      return(set_term_args(covariate, by = by))
    }
    set_term_args(covariate, by = by, m = 1)
  }
  by <- term_by(covariate)
  if (is.null(by)) {
    return(deviation(species))
  }
  if (is_factor_by(by, data, env)) {
    return(deviation(as.name(reg$interaction(list(by, species)))))
  }
  species_form <- stats::reformulate(species_var, intercept = FALSE,
                                     env = env)
  scale <- by_pieces(by, reg, data, env)
  vapply(reg$pieces(species_form, species_var), function(piece) {
    deviation(as.name(reg$product(c(scale, list(piece)))))
  }, character(1L))
}


#' Refuse a `trait_slopes` the expansion cannot take
#'
#' @param trait_slopes One-sided formula of traits, or `NULL`
#' @param obs_formula The observation formula
#' @param data The data `jsdgam()` fits
#' @param species_chr The species column of `data`
#' @return `NULL`, invisibly
#' @noRd
validate_trait_slopes <- function(trait_slopes, obs_formula, data,
                                  species_chr) {
  if (is.null(trait_slopes)) {
    return(invisible(NULL))
  }
  if (!inherits(trait_slopes, "formula") || length(trait_slopes) != 2L) {
    stop(insight::format_error(c(
      "'trait_slopes' must be a one-sided formula.",
      i = "Example: trait_slopes = ~ trait1 + trait2."
    )), call. = FALSE)
  }
  trait_vars <- all.vars(trait_slopes)
  if (length(trait_vars) == 0L) {
    stop(insight::format_error(
      "'trait_slopes' must reference at least one trait column."
    ), call. = FALSE)
  }
  missing <- setdiff(trait_vars, names(data))
  if (length(missing) > 0L) {
    stop(insight::format_error(c(
      "Each trait must be a column of 'data'.",
      x = paste0("Missing: ", paste0("'", missing, "'", collapse = ", "),
                 "."),
      i = "Join the trait table to 'data', one row per species."
    )), call. = FALSE)
  }
  traits <- formula_term_kinds(trait_slopes)
  if (any(traits$kinds == "group")) {
    stop(insight::format_error(c(
      "'trait_slopes' takes species-level traits alone.",
      x = paste0("Group-level term: '",
                 traits$labels[traits$kinds == "group"][1L], "'."),
      i = "Write group-level terms in 'formula'."
    )), call. = FALSE)
  }
  # A trait that varies within a species is a covariate of each row
  for (v in trait_vars) {
    by_species <- vapply(
      split(data[[v]], data[[species_chr]]),
      function(x) length(unique(x[!is.na(x)])),
      FUN.VALUE = integer(1L)
    )
    if (any(by_species > 1L)) {
      bad <- names(by_species[by_species > 1L])
      stop(insight::format_error(c(
        paste0("Trait '", v, "' varies within species in 'data'."),
        x = paste0(
          "Species: ",
          paste0("'", utils::head(bad, 6L), "'", collapse = ", "),
          if (length(bad) > 6L) " ...", "."
        ),
        i = "A covariate that varies within species belongs in 'formula'."
      )), call. = FALSE)
    }
  }
  invisible(NULL)
}


#' The formula of a `trait_slopes` fit
#'
#' Adds to the main formula the traits' effects on the intercept, each
#' covariate's response to each trait and the species deviations. Group
#' terms of `formula` enter as written. A `brmsformula` keeps its
#' sub-formulas.
#'
#' @param obs_formula The observation formula
#' @param trait_slopes One-sided formula of traits
#' @param data The data `jsdgam()` fits
#' @param species_var The species column, `series` once `jsdgam()` has
#'   copied the user's column into it
#' @param species_names Every name of the species column, the user's
#'   included
#' @return List of two. `formula` is `obs_formula` with the expanded
#'   main formula. `columns` holds the derived columns as
#'   `derive_trait_columns()` takes them, `NULL` for none.
#' @noRd
build_trait_slopes_formula <- function(obs_formula, trait_slopes, data,
                                       species_var = "series",
                                       species_names = species_var) {
  main <- obs_arm_main_formula(obs_formula)
  covs <- formula_term_kinds(main)
  traits <- formula_term_kinds(trait_slopes)
  responding <- covs$labels[covs$kinds != "group"]
  if (!covs$intercept && !length(responding)) {
    stop(insight::format_error(c(
      "'trait_slopes' needs a 'formula' with an intercept or a covariate.",
      i = "Add an intercept or covariate to 'formula'."
    )), call. = FALSE)
  }
  # A species term already gives each species its own coefficient. Its
  # trait regression would repeat the species deviation.
  per_species <- Filter(function(label) {
    any(all.vars(str2lang(label)) %in% species_names)
  }, responding)
  if (length(per_species)) {
    stop(insight::format_error(c(
      "'trait_slopes' gives each species its own response.",
      x = paste0("Species term in 'formula': '", per_species[1L], "'."),
      i = "Remove the species terms from 'formula'."
    )), call. = FALSE)
  }

  env <- environment(main)
  linear <- covs$labels[covs$kinds == "linear"]
  forms <- list(
    covariates = trait_design_formula(linear, covs$intercept, env),
    traits = trait_design_formula(traits$labels[traits$kinds == "linear"],
                                  TRUE, env),
    env = env
  )
  reg <- trait_column_registry(data, env)
  crossings <- unlist(lapply(responding, function(covariate) {
    lapply(traits$labels, function(trait) {
      trait_crossing(covariate, trait, reg, forms, data)
    })
  }))

  smooths <- covs$labels[covs$kinds %in% c("smooth", "gp")]
  linear_deviation <- if (covs$intercept || length(linear)) {
    paste0("(", paste(c(if (covs$intercept) "1" else "0", linear),
                      collapse = " + "),
           " | sp | ", species_var, ")")
  }
  smooth_deviations <- unlist(lapply(smooths, species_smooth_deviation,
                                     species_var = species_var, reg = reg,
                                     data = data, env = env))

  added <- c(if (covs$intercept) traits$labels, crossings, linear_deviation,
             smooth_deviations)
  rhs <- paste(deparse(main[[3L]], width.cutoff = 500L), collapse = " ")
  expanded <- stats::as.formula(
    paste(deparse(main[[2L]]), "~", paste(c(rhs, added), collapse = " + ")),
    env = env
  )
  if (inherits(obs_formula, "brmsformula")) {
    obs_formula$formula <- brms::bf(expanded)$formula
    expanded <- obs_formula
  }
  list(formula = expanded, columns = reg$spec())
}


#' The SD of each population-level design column of a formula
#'
#' brms gives each column of `X` a class `b` coefficient named as the
#' column. Each unpenalised column of a smooth's `Xs` takes one too. The
#' design depends on the right-hand side alone. A placeholder response
#' then lets a Gaussian family build it for any family the fit uses.
#'
#' @param formula The expanded observation formula
#' @param data The data `jsdgam()` fits, its derived columns included
#' @return Named numeric vector of SDs, one per coefficient. The
#'   intercept is left out.
#' @noRd
trait_slopes_column_sds <- function(formula, data) {
  main <- obs_arm_main_formula(formula)
  placeholder <- ".mvgam_design_y"
  design <- stats::as.formula(call("~", as.name(placeholder), main[[3L]]),
                              env = environment(main))
  data[[placeholder]] <- 0
  sdata <- without_brms_fit_advisories(
    brms::make_standata(design, data = data, family = stats::gaussian())
  )
  X <- cbind(sdata$X, sdata$Xs)
  X <- X[, setdiff(colnames(X), "Intercept"), drop = FALSE]
  apply(X, 2L, stats::sd)
}


#' The scale of the response a link leaves in its own units
#'
#' An identity link puts every coefficient in the response's units. Its
#' priors then take the response's SD as their unit, as rstanarm's
#' autoscaled priors do for a Gaussian model. Any other link puts the
#' coefficients on a unit-free scale, which takes 1.
#'
#' @param formula The observation formula
#' @param family The observation family
#' @param data The data `jsdgam()` fits
#' @return Positive number
#' @noRd
trait_slopes_response_scale <- function(formula, family, data) {
  if (!identical(family$link, "identity")) {
    return(1)
  }
  main <- obs_arm_main_formula(formula)
  y <- data[[all.vars(main[[2L]])[1L]]]
  scale <- stats::sd(y, na.rm = TRUE)
  if (is.finite(scale) && scale > 0) scale else 1
}


#' The default priors of a `trait_slopes` fit
#'
#' Each population-level coefficient, a shared or a trait effect, takes
#' `normal(0, r / s)`. `s` is the SD of its design column and `r` the
#' response scale from `trait_slopes_response_scale()`. A scale of 1
#' takes `std_normal()`. Every covariate then has one prior scale on
#' the link, whatever its units or the basis `poly()` gives it. A
#' constant column has no SD. Its coefficient takes the class default,
#' `normal(0, r)`, which is set only when such a column exists.
#' `student_t(3, 0, 2.5 r)` goes on the SDs of the correlated species
#' deviations, the `|sp|` block. The rules follow
#' rstanarm's autoscaled priors.
#'
#' A user prior on one coefficient replaces its default. A user prior
#' on the whole class replaces every coefficient's default.
#'
#' @param species_var The species column
#' @param column_sds Named numeric vector from
#'   `trait_slopes_column_sds()`
#' @param user_prior The user's `brmsprior`, or `NULL`
#' @param response_scale Positive number from
#'   `trait_slopes_response_scale()`
#' @return A `brmsprior`
#' @noRd
default_trait_slopes_priors <- function(species_var = "series",
                                        column_sds = numeric(0),
                                        user_prior = NULL,
                                        response_scale = 1) {
  checkmate::assert_string(species_var)
  checkmate::assert_numeric(column_sds, names = "unique")
  checkmate::assert_class(user_prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_number(response_scale, lower = 0,
                           finite = TRUE)
  unit_normal <- function(scale) {
    scale <- signif(scale, 3L)
    if (scale == 1) "std_normal()" else sprintf("normal(0, %s)", scale)
  }
  class_wide <- !is.null(user_prior) && any(
    user_prior$class == "b" & !nzchar(user_prior$coef) &
      !nzchar(user_prior$dpar) & !nzchar(user_prior$nlpar)
  )
  scaled <- column_sds[is.finite(column_sds) & column_sds > 0]
  rows <- list()
  # brms warns of a class prior that every coefficient overrides. The
  # class default is kept only for a coefficient without a column SD.
  if (!class_wide && length(scaled) < length(column_sds)) {
    rows <- list(brms::prior_string(unit_normal(response_scale),
                                    class = "b"))
  }
  if (!class_wide) {
    rows <- c(rows, lapply(names(scaled), function(coef) {
      brms::prior_string(unit_normal(response_scale / scaled[[coef]]),
                         class = "b", coef = coef)
    }))
  }
  rows <- c(rows, list(brms::prior_string(
    sprintf("student_t(3, 0, %s)", signif(2.5 * response_scale, 3L)),
    class = "sd", group = species_var
  )))
  out <- Reduce(`+`, rows)
  out$source <- "default"
  out
}


#' Report the untouched `trait_slopes` defaults as defaults
#'
#' `mvgam()` labels every row it is given `"user"`. A default no user
#' row replaced keeps `"default"` in the merged table. Its row in the
#' fit's table takes that label back.
#'
#' @param prior The fit's prior table
#' @param merged The defaults merged with the user's rows
#' @return `prior` with the untouched defaults labelled `"default"`
#' @noRd
label_trait_slopes_defaults <- function(prior, merged) {
  cols <- c("class", "coef", "group", "resp", "dpar", "nlpar")
  defaults <- merged[identical_source(merged$source, "default"), ,
                     drop = FALSE]
  at <- brmsprior_row_keys(prior, cols) %in%
    brmsprior_row_keys(defaults, cols)
  prior$source[at] <- "default"
  prior
}
