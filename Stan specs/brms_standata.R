#' @title Stan data for Bayesian models
#'
#' @description \code{standata} is a generic function that can be used to
#'   generate data for Bayesian models to be passed to Stan. Its original use is
#'   within the \pkg{brms} package, but new methods for use
#'   with objects from other packages can be registered to the same generic.
#'
#' @param object A formula object whose class will determine which method will
#'   be used. A symbolic description of the model to be fitted.
#' @param formula Synonym of \code{object} for use in \code{make_standata}.
#' @param ... Further arguments passed to the specific method.
#'
#' @return A named list of objects containing the required data to fit a
#'   Bayesian model with \pkg{Stan}.
#'
#' @details
#' See \code{\link{standata.default}} for the default method applied for
#' \pkg{brms} models. You can view the available methods by typing
#' \code{methods(standata)}. The \code{make_standata} function is an alias
#' of \code{standata}.
#'
#' @examples
#' sdata1 <- standata(rating ~ treat + period + carry + (1|subject),
#'                    data = inhaler, family = "cumulative")
#' str(sdata1)
#'
#' @seealso
#'   \code{\link{standata.default}}, \code{\link{standata.brmsfit}}
#'
#' @export
standata <- function(object, ...) {
  UseMethod("standata")
}

#' @rdname standata
#' @export
make_standata <- function(formula, ...) {
  # became an alias of standata in 2.20.14.
  standata(formula, ...)
}

#' Data for \pkg{brms} Models
#'
#' Generate data for \pkg{brms} models to be passed to \pkg{Stan}.
#'
#' @inheritParams brm
#' @param object An object of class \code{\link[stats:formula]{formula}},
#'   \code{\link{brmsformula}}, or \code{\link{mvbrmsformula}} (or one that can
#'   be coerced to that classes): A symbolic description of the model to be
#'   fitted. The details of model specification are explained in
#'   \code{\link{brmsformula}}.
#' @param ... Other arguments for internal use.
#'
#' @return A named list of objects containing the required data
#'   to fit a \pkg{brms} model with \pkg{Stan}.
#'
#' @examples
#' sdata1 <- standata(rating ~ treat + period + carry + (1|subject),
#'                    data = inhaler, family = "cumulative")
#' str(sdata1)
#'
#' sdata2 <- standata(count ~ zAge + zBase * Trt + (1|patient),
#'                    data = epilepsy, family = "poisson")
#' str(sdata2)
#'
#' @export
standata.default <- function(object, data, family = gaussian(), prior = NULL,
                             autocor = NULL, data2 = NULL, cov_ranef = NULL,
                             sample_prior = "no", stanvars = NULL,
                             threads = getOption("brms.threads", NULL),
                             knots = NULL, drop_unused_levels = TRUE, ...) {

  object <- validate_formula(
    object, data = data, family = family,
    autocor = autocor, cov_ranef = cov_ranef
  )
  bterms <- brmsterms(object)
  data2 <- validate_data2(
    data2, bterms = bterms,
    get_data2_autocor(object),
    get_data2_cov_ranef(object)
  )
  data <- validate_data(
    data, bterms = bterms,
    knots = knots, data2 = data2,
    drop_unused_levels = drop_unused_levels
  )
  bframe <- brmsframe(bterms, data)
  prior <- .validate_prior(
    prior, bframe = bframe,
    sample_prior = sample_prior
  )
  stanvars <- validate_stanvars(stanvars)
  threads <- validate_threads(threads)
  .standata(
    bframe, data = data, prior = prior,
    data2 = data2, stanvars = stanvars,
    threads = threads, ...
  )
}

# internal work function of 'standata'
# @param check_response check validity of the response?
# @param only_response extract data related to the response only?
# @param internal prepare Stan data for use in post-processing methods?
# @param basis original Stan data as prepared by 'frame_basis'
# @param ... currently ignored
# @return names list of data passed to Stan
.standata <- function(bframe, data, prior, stanvars, data2,
                      threads = threading(), check_response = TRUE,
                      only_response = FALSE, internal = FALSE, ...) {

  stopifnot(is.anybrmsframe(bframe))
  check_response <- as_one_logical(check_response)
  only_response <- as_one_logical(only_response)
  internal <- as_one_logical(internal)
  out <- data_response(
    bframe, data, check_response = check_response,
    internal = internal
  )
  if (!only_response) {
    # pass as sdata so that data_special_prior knows about data_gr_global
    # TODO: compute sdata_gr_global in brmsframe in brms 3.0
    # this would require passing data2 to brmsframe
    sdata_gr_global <- data_gr_global(bframe, data2 = data2)
    c(out) <- data_predictor(
      bframe, data = data, prior = prior, data2 = data2,
      sdata = sdata_gr_global
    )
    c(out) <- sdata_gr_global
    c(out) <- data_Xme(bframe, data = data)
  }
  out$prior_only <- as.integer(is_prior_only(prior))
  if (use_threading(threads)) {
    out$grainsize <- threads$grainsize
    if (is.null(out$grainsize)) {
      out$grainsize <- ceiling(out$N / (2 * threads$threads))
      out$grainsize <- max(100, out$grainsize)
    }
  }
  if (is.stanvars(stanvars)) {
    stanvars <- subset_stanvars(stanvars, block = "data")
    inv_names <- intersect(names(stanvars), names(out))
    if (length(inv_names)) {
      stop2("Cannot overwrite existing variables: ",
            collapse_comma(inv_names))
    }
    out[names(stanvars)] <- from_list(stanvars, "sdata")
  }
  if (internal) {
    # allows to recover the original order of the data
    attr(out, "old_order") <- attr(data, "old_order")
    # ensures currently used grouping levels are known in post-processing
    set_levels(out, "used") <- get_levels(bframe, prefix = "used")
  }
  structure(out, class = c("standata", "list"))
}

#' Extract data passed to Stan from \code{brmsfit} objects
#'
#' Extract all data that was used by Stan to fit a \pkg{brms} model.
#'
#' @param object An object of class \code{brmsfit}.
#' @param ... More arguments passed to
#'   \code{\link[brms:standata.default]{standata.default}}.
#'   and \code{\link{validate_newdata}}.
#' @inheritParams prepare_predictions
#'
#' @return A named list containing the data passed to Stan.
#'
#' @export
standata.brmsfit <- function(object, newdata = NULL, re_formula = NULL,
                             newdata2 = NULL, new_objects = NULL,
                             incl_autocor = TRUE, ...) {

  # allows functions to fall back to old default behavior
  # which was used when originally fitting the model
  options(.brmsfit_version = object$version$brms)
  on.exit(options(.brmsfit_version = NULL))

  object <- exclude_terms(object, incl_autocor = incl_autocor)
  formula <- update_re_terms(object$formula, re_formula)
  bterms <- brmsterms(formula)

  newdata2 <- use_alias(newdata2, new_objects)
  data2 <- current_data2(object, newdata2)
  data <- current_data(
    object, newdata, newdata2 = data2,
    re_formula = re_formula, ...
  )
  stanvars <- add_newdata_stanvars(object$stanvars, data2)

  basis <- object$basis
  if (is.null(basis)) {
    # this case should not happen actually, perhaps when people use
    # the 'empty' feature. But computing it here will be fine
    # for almost all models, only causing potential problems for processing
    # of splines on new machines (#1465)
    bframe_old <- brmsframe(object$formula, data = object$data)
    basis <- frame_basis(bframe_old, data = object$data)
  }
  bframe <- brmsframe(bterms, data = data, basis = basis)
  .standata(
    bframe, data = data, prior = object$prior,
    data2 = data2, stanvars = stanvars,
    threads = object$threads, ...
  )
}

#' User-defined variables passed to Stan
#'
#' Prepare user-defined variables to be passed to one of Stan's
#' program blocks. This is primarily useful for defining more complex
#' priors, for refitting models without recompilation despite
#' changing priors, or for defining custom Stan functions.
#'
#' @aliases stanvars
#'
#' @param x An \R object containing data to be passed to Stan.
#'   Only required if \code{block = 'data'} and ignored otherwise.
#' @param name Optional character string providing the desired variable
#'  name of the object in \code{x}. If \code{NULL} (the default)
#'  the variable name is directly inferred from \code{x}.
#' @param scode Line of Stan code to define the variable
#'  in Stan language. If \code{block = 'data'}, the
#'  Stan code is inferred based on the class of \code{x} by default.
#' @param block Name of one of Stan's program blocks in
#'  which the variable should be defined. Can be \code{'data'},
#'  \code{'tdata'} (transformed data), \code{'parameters'},
#'  \code{'tparameters'} (transformed parameters), \code{'model'},
#'  \code{'likelihood'} (part of the model block where the likelihood is given),
#'  \code{'genquant'} (generated quantities) or \code{'functions'}.
#' @param position Name of the position within the block where the
#'  Stan code should be placed. Currently allowed are \code{'start'}
#'  (the default) and \code{'end'} of the block.
#' @param pll_args Optional Stan code to be put into the header
#'  of \code{partial_log_lik} functions. This ensures that the variables
#'  specified in \code{scode} can be used in the likelihood even when
#'  within-chain parallelization is activated via \code{\link{threading}}.
#'
#' @return An object of class \code{stanvars}.
#'
#' @details
#' The \code{stanvar} function is not vectorized. Instead, multiple
#' \code{stanvars} objects can be added together via \code{+} (see Examples).
#'
#' Special attention is necessary when using \code{stanvars} to inject
#' code into the \code{'likelihood'} block while having \code{\link{threading}}
#' activated. In this case, your custom Stan code may need adjustments to ensure
#' correct observation indexing. Please investigate the generated Stan code via
#' \code{\link[brms:stancode.default]{stancode}} to see which adjustments are necessary in your case.
#'
#' @examples
#' bprior <- prior(normal(mean_intercept, 10), class = "Intercept")
#' stanvars <- stanvar(5, name = "mean_intercept")
#' stancode(count ~ Trt, epilepsy, prior = bprior,
#'          stanvars = stanvars)
#'
#' # define a multi-normal prior with known covariance matrix
#' bprior <- prior(multi_normal(M, V), class = "b")
#' stanvars <- stanvar(rep(0, 2), "M", scode = "  vector[K] M;") +
#'   stanvar(diag(2), "V", scode = "  matrix[K, K] V;")
#' stancode(count ~ Trt + zBase, epilepsy,
#'          prior = bprior, stanvars = stanvars)
#'
#' # define a hierachical prior on the regression coefficients
#' bprior <- set_prior("normal(0, tau)", class = "b") +
#'   set_prior("target += normal_lpdf(tau | 0, 10)", check = FALSE)
#' stanvars <- stanvar(scode = "real<lower=0> tau;",
#'                     block = "parameters")
#' stancode(count ~ Trt + zBase, epilepsy,
#'          prior = bprior, stanvars = stanvars)
#'
#' # ensure that 'tau' is passed to the likelihood of a threaded model
#' # not necessary for this example but may be necessary in other cases
#' stanvars <- stanvar(scode = "real<lower=0> tau;",
#'                     block = "parameters", pll_args = "real tau")
#' stancode(count ~ Trt + zBase, epilepsy,
#'          stanvars = stanvars, threads = threading(2))
#'
#' @export
stanvar <- function(x = NULL, name = NULL, scode = NULL,
                    block = "data", position = "start",
                    pll_args = NULL) {
  vblocks <- c(
    "data", "tdata", "parameters", "tparameters",
    "model", "genquant", "functions", "likelihood"
  )
  block <- match.arg(block, vblocks)
  vpositions <- c("start", "end")
  position <- match.arg(position, vpositions)
  if (block == "data") {
    if (is.null(x)) {
      stop2("Argument 'x' is required if block = 'data'.")
    }
    if (is.null(name)) {
      name <- deparse0(substitute(x))
    }
    name <- as_one_character(name)
    if (!is_equal(name, make.names(name)) || grepl("\\.", name)) {
      stop2("'", limit_chars(name, 30), "' is not ",
            "a valid variable name in Stan.")
    }
    if (is.null(scode)) {
      # infer scode from x
      if (is.integer(x)) {
        if (length(x) == 1L) {
          scode <- paste0("int ", name)
        } else {
          scode <- paste0("array[", length(x), "] int ", name)
        }
      } else if (is.vector(x)) {
        if (length(x) == 1L) {
          scode <- paste0("real ", name)
        } else {
          scode <- paste0("vector[", length(x), "] ", name)
        }
      } else if (is.array(x)) {
        if (length(dim(x)) == 1L) {
          scode <- paste0("vector[", length(x), "] ", name)
        } else if (is.matrix(x)) {
          scode <- paste0("matrix[", nrow(x), ", ", ncol(x), "] ", name)
        }
      }
      if (is.null(scode)) {
        stop2(
          "'stanvar' could not infer the Stan code for an object ",
          "of class '", class(x), "'. Please specify the Stan code ",
          "manually via argument 'scode'."
        )
      }
      scode <- paste0(scode, ";")
    }
    if (is.null(pll_args)) {
      # infer pll_args from x
      pll_type <- str_if(block %in% c("data", "tdata"), "data ")
      if (is.integer(x)) {
        if (length(x) == 1L) {
          pll_type <- paste0(pll_type, "int")
        } else {
          pll_type <- paste0(pll_type, "array[] int")
        }
      } else if (is.vector(x)) {
        if (length(x) == 1L) {
          pll_type <- paste0(pll_type, "real")
        } else {
          pll_type <- paste0(pll_type, "vector")
        }
      } else if (is.array(x)) {
        if (length(dim(x)) == 1L) {
          pll_type <- paste0(pll_type, "vector")
        } else if (is.matrix(x)) {
          pll_type <- paste0(pll_type, "matrix")
        }
      }
      if (!is.null(pll_type)) {
        pll_args <- paste0(pll_type, " ", name)
      } else {
        # don't throw an error because most people will not use threading
        pll_args <- character(0)
      }
    }
  } else {
    x <- NULL
    if (is.null(name)) {
      name <- ""
    }
    name <- as_one_character(name)
    if (is.null(scode)) {
      stop2("Argument 'scode' is required if block is not 'data'.")
    }
    scode <- as.character(scode)
    pll_args <- as.character(pll_args)
  }
  if (position == "end" && block %in% c("functions", "data")) {
    stop2("Position '", position, "' is not sensible for block '", block, "'.")
  }
  out <- nlist(name, sdata = x, scode, block, position, pll_args)
  structure(setNames(list(out), name), class = "stanvars")
}

# take a subset of a stanvars object
# @param x a stanvars object
# @param ... conditions defining the desired subset
subset_stanvars <- function(x, ...) {
  x <- validate_stanvars(x)
  structure_not_null(x[find_elements(x, ...)], class = "stanvars")
}

# collapse Stan code provided in a stanvars object
collapse_stanvars <- function(x, block = NULL, position = NULL) {
  x <- validate_stanvars(x)
  if (!length(x)) {
    return(character(0))
  }
  if (!is.null(block)) {
    x <- subset_stanvars(x, block = block)
  }
  if (!is.null(position)) {
    x <- subset_stanvars(x, position = position)
  }
  if (!length(x)) {
    return("")
  }
  collapse(wsp(nsp = 2), ufrom_list(x, "scode"), "\n")
}

# collapse partial log-lik code provided in a stanvars object
collapse_stanvars_pll_args <- function(x) {
  x <- validate_stanvars(x)
  if (!length(x)) {
    return(character(0))
  }
  out <- ufrom_list(x, "pll_args")
  if (!length(out)) {
    return("")
  }
  collapse(", ", out)
}

# validate 'stanvars' objects
validate_stanvars <- function(x, stan_funs = NULL) {
  if (is.null(x)) {
    x <- empty_stanvars()
  }
  if (!is.stanvars(x)) {
    stop2("Argument 'stanvars' is invalid. See ?stanvar for help.")
  }
  if (length(stan_funs) > 0) {
    warning2("Argument 'stan_funs' is deprecated. Please use argument ",
             "'stanvars' instead. See ?stanvar for more help.")
    stan_funs <- as_one_character(stan_funs)
    x <- x + stanvar(scode = stan_funs, block = "functions")
  }
  x
}

# add new data to stanvars
# @param x a 'stanvars' object
# @param newdata2 a list with new 'data2' objects
# @return a 'stanvars' object
add_newdata_stanvars <- function(x, newdata2) {
  stopifnot(is.stanvars(x))
  stanvars_data <- subset_stanvars(x, block = "data")
  for (name in names(stanvars_data)) {
    if (name %in% names(newdata2)) {
      x[[name]]$sdata <- newdata2[[name]]
    }
  }
  x
}

#' @export
c.stanvars <- function(x, ...) {
  dots <- lapply(list(...), validate_stanvars)
  class(x) <- "list"
  out <- unlist(c(list(x), dots), recursive = FALSE)
  svnames <- names(out)[nzchar(names(out))]
  if (any(duplicated(svnames))) {
    stop2("Duplicated names in 'stanvars' are not allowed.")
  }
  structure(out, class = "stanvars")
}

#' @export
"+.stanvars" <- function(e1, e2) {
  c(e1, e2)
}

is.stanvars <- function(x) {
  inherits(x, "stanvars")
}

empty_stanvars <- function() {
  structure(list(), class = "stanvars")
}

#' Prepare Predictor Data
#'
#' Prepare data related to predictor variables in \pkg{brms}.
#' Only exported for use in package development.
#'
#' @param x An \R object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A named list of data related to predictor variables.
#'
#' @keywords internal
#' @export
data_predictor <- function(x, ...) {
  UseMethod("data_predictor")
}

#' @export
data_predictor.mvbrmsterms <- function(x, data, sdata = NULL, ...) {
  out <- list(N = nrow(data))
  for (r in names(x$terms)) {
    c(out) <- data_predictor(x$terms[[r]], data = data, sdata = sdata, ...)
  }
  out
}

#' @export
data_predictor.brmsterms <- function(x, data, data2, prior, sdata = NULL, ...) {
  out <- list()
  data <- subset_data(data, x)
  resp <- usc(combine_prefix(x))
  args_eff <- nlist(data, data2, prior, sdata, ...)
  for (dp in names(x$dpars)) {
    args_eff_spec <- list(x = x$dpars[[dp]])
    c(out) <- do_call(data_predictor, c(args_eff_spec, args_eff))
  }
  for (dp in names(x$fdpars)) {
    if (is.numeric(x$fdpars[[dp]]$value)) {
      out[[paste0(dp, resp)]] <- x$fdpars[[dp]]$value
    }
  }
  for (nlp in names(x$nlpars)) {
    args_eff_spec <- list(x = x$nlpars[[nlp]])
    c(out) <- do_call(data_predictor, c(args_eff_spec, args_eff))
  }
  c(out) <- data_gr_local(x, data = data)
  c(out) <- data_mixture(x, data2 = data2, prior = prior)
  out
}

# prepare data for all types of effects for use in Stan
# @param data the data passed by the user
# @param prior an object of class brmsprior
# @param ... currently ignored
# @return a named list of data to be passed to Stan
#' @export
data_predictor.btl <- function(x, data, data2 = list(), prior = brmsprior(),
                               sdata = NULL, ...) {
  out <- c(
    data_fe(x, data),
    data_sp(x, data, data2 = data2, prior = prior),
    data_re(x, data),
    data_cs(x, data),
    data_sm(x, data),
    data_gp(x, data),
    data_ac(x, data, data2 = data2),
    data_offset(x, data),
    data_bhaz(x, data, data2 = data2, prior = prior)
  )
  c(out) <- data_special_prior(x, data, prior = prior, sdata = c(sdata, out))
  out
}

# prepare data for non-linear parameters for use in Stan
#' @export
data_predictor.btnl <- function(x, data, data2 = list(), prior = brmsprior(),
                                ...) {
  out <- list()
  c(out) <- data_cnl(x, data)
  c(out) <- data_ac(x, data, data2 = data2)
  c(out) <- data_bhaz(x, data, data2 = data2, prior = prior)
  out
}

# prepare data of fixed effects
data_fe <- function(bframe, data) {
  stopifnot(is.btl(bframe))
  if (!is.null(bframe$sdata$fe)) {
    # standata was already precomputed
    return(bframe$sdata$fe)
  }
  out <- list()
  p <- usc(combine_prefix(bframe))
  # the intercept is removed inside the Stan code for non-ordinal models
  is_ord <- is_ordinal(bframe)
  cols2remove <- if (is_ord) "(Intercept)"
  X <- get_model_matrix(rhs(bframe$fe), data, cols2remove = cols2remove)
  avoid_dpars(colnames(X), bframe)
  out[[paste0("K", p)]] <- ncol(X)
  if (stan_center_X(bframe)) {
    # relevant if the intercept is treated separately to enable centering
    out[[paste0("Kc", p)]] <- ncol(X) - ifelse(is_ord, 0, 1)
  }
  out[[paste0("X", p)]] <- X
  out
}

# data preparation for splines
data_sm <- function(bframe, data) {
  stopifnot(is.btl(bframe))
  if (!is.null(bframe$sdata$sm)) {
    # standata was already precomputed
    return(bframe$sdata$sm)
  }
  out <- list()
  smterms <- all_terms(bframe[["sm"]])
  if (!length(smterms)) {
    return(out)
  }
  p <- usc(combine_prefix(bframe))
  # basis contains information on the smooths from the original data
  basis <- bframe$basis$sm
  new <- length(basis) > 0L
  knots <- get_knots(data)
  diagonal.penalty <- !require_old_default("2.8.7")
  bylevels <- named_list(smterms)
  ns <- 0
  lXs <- list()
  for (i in seq_along(smterms)) {
    if (new) {
      sm <- basis[[i]]$sm
    } else {
      sm <- smoothCon(
        eval2(smterms[i]), data = data,
        knots = knots, absorb.cons = TRUE,
        diagonal.penalty = diagonal.penalty
      )
    }
    # may contain multiple terms when 'by' is a factor
    for (j in seq_along(sm)) {
      ns <- ns + 1
      if (length(sm[[j]]$by.level)) {
        bylevels[[i]][j] <- sm[[j]]$by.level
      }
      if (new) {
        # prepare smooths for use with new data
        # mgcv smooths are based on machine-specific SVD (#1465)
        re <- s2rPred(sm[[j]], re = basis[[i]]$re[[j]], data = data)
      } else {
        re <- mgcv::smooth2random(sm[[j]], names(data), type = 2)
      }
      lXs[[ns]] <- re$Xf
      if (NCOL(lXs[[ns]])) {
        colnames(lXs[[ns]]) <- paste0(sm[[j]]$label, "_", seq_cols(lXs[[ns]]))
      }
      Zs <- re$rand
      sfx <- paste0(p, "_", ns)
      out[[paste0("nb", sfx)]] <- length(Zs)
      if (length(Zs)) {
        names(Zs) <- paste0("Zs", sfx, "_", seq_along(Zs))
        c(out) <- Zs
        out[[paste0("knots", sfx)]] <- as.array(ulapply(Zs, ncol))
      } else {
        out[[paste0("knots", sfx)]] <- integer(0)
      }
    }
  }
  Xs <- do_call(cbind, lXs)
  avoid_dpars(colnames(Xs), bframe)
  smcols <- lapply(lXs, function(x) which(colnames(Xs) %in% colnames(x)))
  Xs <- structure(Xs, smcols = smcols, bylevels = bylevels)
  colnames(Xs) <- rename(colnames(Xs))
  out[[paste0("Ks", p)]] <- ncol(Xs)
  out[[paste0("Xs", p)]] <- Xs
  out
}

# prepare data for group-level effects for use in Stan
data_re <- function(bframe, data) {
  stopifnot(is.bframel(bframe))
  out <- list()
  px <- check_prefix(bframe)
  reframe <- subset2(bframe$frame$re, type = "sp", fun = "%notin%")
  if (!has_rows(reframe)) {
    return(out)
  }
  gn <- unique(reframe$gn)
  for (i in seq_along(gn)) {
    r <- subset2(reframe, gn = gn[i])
    Z <- get_model_matrix(r$form[[1]], data = data, rename = FALSE)
    idp <- paste0(r$id[1], usc(combine_prefix(px)))
    Znames <- paste0("Z_", idp, "_", r$cn)
    if (r$gtype[1] == "mm") {
      ng <- length(r$gcall[[1]]$groups)
      if (r$type[1] == "cs") {
        stop2("'cs' is not supported in multi-membership terms.")
      }
      if (r$type[1] == "mmc") {
        # see issue #353 for the general idea
        mmc_expr <- "^mmc\\([^:]*\\)"
        mmc_terms <- get_matches_expr(mmc_expr, colnames(Z))
        for (t in mmc_terms) {
          pos <- which(grepl_expr(escape_all(t), colnames(Z)))
          if (length(pos) != ng) {
            stop2("Invalid term '", t, "': Expected ", ng,
                  " coefficients but found ", length(pos), ".")
          }
          for (j in seq_along(Znames)) {
            for (k in seq_len(ng)) {
              out[[paste0(Znames[j], "_", k)]] <- as.array(Z[, pos[k]])
            }
          }
        }
      } else {
        for (j in seq_along(Znames)) {
          out[paste0(Znames[j], "_", seq_len(ng))] <- list(as.array(Z[, j]))
        }
      }
    } else {
      if (r$type[1] == "cs") {
        ncatM1 <- nrow(r) / ncol(Z)
        Z_temp <- vector("list", ncol(Z))
        for (k in seq_along(Z_temp)) {
          Z_temp[[k]] <- replicate(ncatM1, Z[, k], simplify = FALSE)
        }
        Z <- do_call(cbind, unlist(Z_temp, recursive = FALSE))
      }
      if (r$type[1] == "mmc") {
        stop2("'mmc' is only supported in multi-membership terms.")
      }
      for (j in seq_cols(Z)) {
        out[[Znames[j]]] <- as.array(Z[, j])
      }
    }
  }
  out
}

# compute data for each group-level-ID per univariate model
data_gr_local <- function(bframe, data) {
  stopifnot(is.brmsframe(bframe))
  out <- list()
  reframe <- subset2(bframe$frame$re, resp = bframe$resp)
  resp <- usc(bframe$resp)
  for (id in unique(reframe$id)) {
    id_reframe <- subset2(reframe, id = id)
    idresp <- paste0(id, resp)
    nranef <- nrow(id_reframe)
    group <- id_reframe$group[1]
    levels <- get_levels(reframe)[[group]]
    if (id_reframe$gtype[1] == "mm") {
      # multi-membership grouping term
      gs <- id_reframe$gcall[[1]]$groups
      ngs <- length(gs)
      weights <- id_reframe$gcall[[1]]$weights
      if (is.formula(weights)) {
        scale <- isTRUE(attr(weights, "scale"))
        weights <- as.matrix(eval_rhs(weights, data))
        if (!identical(dim(weights), c(nrow(data), ngs))) {
          stop2(
            "Grouping structure 'mm' expects 'weights' to be ",
            "a matrix with as many columns as grouping factors."
          )
        }
        if (scale) {
          if (isTRUE(any(weights < 0))) {
            stop2("Cannot scale negative weights.")
          }
          weights <- sweep(weights, 1, rowSums(weights), "/")
        }
      } else {
        # all members get equal membership weights by default
        weights <- matrix(1 / ngs, nrow = nrow(data), ncol = ngs)
      }
      for (i in seq_along(gs)) {
        gdata <- get(gs[i], data)
        J <- match(gdata, levels)
        if (anyNA(J)) {
          # occurs for new levels only
          new_gdata <- gdata[!gdata %in% levels]
          new_levels <- unique(new_gdata)
          J[is.na(J)] <- match(new_gdata, new_levels) + length(levels)
        }
        out[[paste0("J_", idresp, "_", i)]] <- as.array(J)
        out[[paste0("W_", idresp, "_", i)]] <- as.array(weights[, i])
      }
    } else {
      # ordinary grouping term
      g <- id_reframe$gcall[[1]]$groups
      gdata <- get(g, data)
      J <- match(gdata, levels)
      if (anyNA(J)) {
        # occurs for new levels only
        new_gdata <- gdata[!gdata %in% levels]
        new_levels <- unique(new_gdata)
        J[is.na(J)] <- match(new_gdata, new_levels) + length(levels)
      }
      out[[paste0("J_", idresp)]] <- as.array(J)
    }
    # prepare data for group prior weights if specified
    if (nzchar(id_reframe$gcall[[1]]$pw)) {
      if (id_reframe$gtype[1] == "mm") {
        J <- unlist(out[paste0("J_", idresp, "_", seq_along(gs))])
      }
      # extract and validate prior weights
      group_prior_weights <- str2formula(id_reframe$gcall[[1]]$pw)
      group_prior_weights <- as.vector(eval_rhs(group_prior_weights, data))
      if (!is.numeric(group_prior_weights)) {
        stop2("Prior weights of grouping factors must be numeric.")
      }
      if (any(group_prior_weights < 0)) {
        warning2("Negative prior weights detected. Make sure this is intentional.")
      }
      # check that group-level weights do not vary within a group
      group_weights_consistent <- tapply(
        X = group_prior_weights, INDEX = J,
        FUN = function(x) length(unique(x)) == 1
      )
      if (!all(group_weights_consistent)) {
        stop2("Prior weights cannot vary within a group.")
      }
      # deduplicate weights vector (so length matches number of groups)
      # and order the weights vector to match groups' assigned indices
      distinct_J_indices <- !duplicated(J)
      group_prior_weights <- group_prior_weights[distinct_J_indices]
      group_prior_weights <- group_prior_weights[order(J[distinct_J_indices])]
      out[[paste0("PW_", id)]] <- as.array(group_prior_weights)
    }
  }
  out
}

# prepare global data for each group-level-ID
data_gr_global <- function(bframe, data2) {
  stopifnot(is.anybrmsframe(bframe))
  out <- list()
  reframe <- bframe$frame$re
  for (id in unique(reframe$id)) {
    tmp <- list()
    id_reframe <- subset2(reframe, id = id)
    nranef <- nrow(id_reframe)
    group <- id_reframe$group[1]
    levels <- attr(reframe, "levels")[[group]]
    tmp$N <- length(levels)
    tmp$M <- nranef
    tmp$NC <- as.integer(nranef * (nranef - 1) / 2)
    # prepare number of levels of an optional 'by' variable
    if (nzchar(id_reframe$by[1])) {
      stopifnot(!nzchar(id_reframe$type[1]))
      bylevels <- id_reframe$bylevels[[1]]
      Jby <- match(attr(levels, "by"), bylevels)
      tmp$Nby <- length(bylevels)
      tmp$Jby <- as.array(Jby)
    }
    # prepare within-group covariance matrices
    cov <- id_reframe$cov[1]
    if (nzchar(cov)) {
      # validation is only necessary here for compatibility with 'cov_ranef'
      cov_mat <- validate_recov_matrix(data2[[cov]])
      found_levels <- rownames(cov_mat)
      found <- levels %in% found_levels
      if (any(!found)) {
        stop2("Levels of the within-group covariance matrix for '", group,
              "' do not match names of the grouping levels.")
      }
      cov_mat <- cov_mat[levels, levels, drop = FALSE]
      tmp$Lcov <- t(chol(cov_mat))
    }
    names(tmp) <- paste0(names(tmp), "_", id)
    c(out) <- tmp
  }
  out
}

# prepare data for special effects for use in Stan
data_sp <- function(bframe, data, data2, prior) {
  stopifnot(is.bframel(bframe))
  if (!is.null(bframe$sdata$sp)) {
    # standata was already precomputed
    return(bframe$sdata$sp)
  }
  out <- list()
  spframe <- bframe$frame$sp
  if (!has_rows(spframe)) {
    return(out)
  }
  basis <- bframe$basis$sp
  px <- check_prefix(bframe)
  p <- usc(combine_prefix(px))
  # prepare general data
  out[[paste0("Ksp", p)]] <- nrow(spframe)
  Csp <- sp_model_matrix(bframe$sp, data)
  avoid_dpars(colnames(Csp), bframe)
  Csp <- Csp[, spframe$Ic > 0, drop = FALSE]
  Csp <- lapply(seq_cols(Csp), function(i) as.array(Csp[, i]))
  if (length(Csp)) {
    Csp_names <- paste0("Csp", p, "_", seq_along(Csp))
    out <- c(out, setNames(Csp, Csp_names))
  }
  if (any(lengths(spframe$Imo) > 0)) {
    # prepare data specific to monotonic effects
    out[[paste0("Imo", p)]] <- max(unlist(spframe$Imo))
    Xmo <- lapply(unlist(spframe$calls_mo), get_mo_values, data = data)
    Xmo_names <- paste0("Xmo", p, "_", seq_along(Xmo))
    c(out) <- setNames(Xmo, Xmo_names)
    if (!is.null(basis$Jmo)) {
      # take information from original data
      Jmo <- basis$Jmo
    } else {
      Jmo <- as.array(ulapply(Xmo, attr, "max"))
    }
    out[[paste0("Jmo", p)]] <- Jmo
    # prepare prior concentration of simplex parameters
    simo_coef <- get_simo_labels(spframe, use_id = TRUE)
    ids <- unlist(spframe$ids_mo)
    for (j in seq_along(simo_coef)) {
      # index of first ID appearance
      j_id <- match(ids[j], ids)
      if (is.na(ids[j]) || j_id == j) {
        # only evaluate priors without ID or first appearance of the ID
        # all other parameters will be copied over in the Stan code
        simo_prior <- subset2(prior,
                              class = "simo", coef = simo_coef[j], ls = px
        )
        con_simo <- eval_dirichlet(simo_prior$prior, Jmo[j], data2)
        out[[paste0("con_simo", p, "_", j)]] <- as.array(con_simo)
      }
    }
  }
  uni_mi <- attr(spframe, "uni_mi")
  index <- bframe$frame$index
  for (j in seq_rows(uni_mi)) {
    if (!is.na(uni_mi$idx[j])) {
      idxl <- get(uni_mi$idx[j], data)
      if (is.null(index[[uni_mi$var[j]]])) {
        # the 'idx' argument needs to be mapped against 'index' addition terms
        stop2("Response '", uni_mi$var[j], "' needs to have an 'index' addition ",
              "term to compare with 'idx'. See ?mi for examples.")
      }
      idxl <- match(idxl, index[[uni_mi$var[j]]])
      if (anyNA(idxl)) {
        stop2("Could not match all indices in response '", uni_mi$var[j], "'.")
      }
      idxl_name <- paste0("idxl", p, "_", uni_mi$var[j], "_", uni_mi$idx2[j])
      out[[idxl_name]] <- as.array(idxl)
    } else if (isTRUE(attr(index[[uni_mi$var[j]]], "subset"))) {
      # cross-formula referencing is required for subsetted variables
      stop2("mi() terms of subsetted variables require ",
            "the 'idx' argument to be specified.")
    }
  }
  out
}

# prepare data for category specific effects
data_cs <- function(bframe, data) {
  stopifnot(is.btl(bframe))
  if (!is.null(bframe$sdata$cs)) {
    # standata was already precomputed
    return(bframe$sdata$cs)
  }
  out <- list()
  if (length(all_terms(bframe[["cs"]]))) {
    p <- usc(combine_prefix(bframe))
    Xcs <- get_model_matrix(bframe$cs, data)
    avoid_dpars(colnames(Xcs), bframe)
    out <- c(out, list(Kcs = ncol(Xcs), Xcs = Xcs))
    out <- setNames(out, paste0(names(out), p))
  }
  out
}

# prepare global data for noise free variables
data_Xme <- function(bframe, data) {
  stopifnot(is.anybrmsframe(bframe))
  meframe <- bframe$frame$me
  stopifnot(is.meframe(meframe))
  out <- list()
  groups <- unique(meframe$grname)
  for (i in seq_along(groups)) {
    g <- groups[i]
    K <- which(meframe$grname %in% g)
    Mme <- length(K)
    out[[paste0("Mme_", i)]] <- Mme
    out[[paste0("NCme_", i)]] <- Mme * (Mme - 1) / 2
    if (nzchar(g)) {
      levels <- get_levels(meframe)[[g]]
      gr <- get_me_group(meframe$term[K[1]], data)
      Jme <- match(gr, levels)
      if (anyNA(Jme)) {
        # occurs for new levels only
        # replace NAs with unique values; fixes issue #706
        gr[is.na(gr)] <- paste0("new_", seq_len(sum(is.na(gr))), "__")
        new_gr <- gr[!gr %in% levels]
        new_levels <- unique(new_gr)
        Jme[is.na(Jme)] <- length(levels) + match(new_gr, new_levels)
      }
      ilevels <- unique(Jme)
      out[[paste0("Nme_", i)]] <- length(ilevels)
      out[[paste0("Jme_", i)]] <- Jme
    }
    for (k in K) {
      Xn <- get_me_values(meframe$term[k], data)
      noise <- get_me_noise(meframe$term[k], data)
      if (nzchar(g)) {
        for (l in ilevels) {
          # validate values of the same level
          take <- Jme %in% l
          if (length(unique(Xn[take])) > 1L ||
              length(unique(noise[take])) > 1L) {
            stop2(
              "Measured values and measurement error should be ",
              "unique for each group. Occured for level '",
              levels[l], "' of group '", g, "'."
            )
          }
        }
        Xn <- get_one_value_per_group(Xn, Jme)
        noise <- get_one_value_per_group(noise, Jme)
      }
      out[[paste0("Xn_", k)]] <- as.array(Xn)
      out[[paste0("noise_", k)]] <- as.array(noise)
    }
  }
  out
}

# prepare data for Gaussian process terms
# @param internal store some intermediate data for internal post-processing?
# @param ... passed to '.data_gp'
data_gp <- function(bframe, data, internal = FALSE, ...) {
  stopifnot(is.bframel(bframe))
  if (!is.null(bframe$sdata$gp)) {
    # standata was already precomputed
    return(bframe$sdata$gp)
  }
  out <- list()
  internal <- as_one_logical(internal)
  px <- check_prefix(bframe)
  p <- usc(combine_prefix(px))
  basis <- bframe$basis$gp
  gpframe <- bframe$frame$gp
  for (i in seq_rows(gpframe)) {
    pi <- paste0(p, "_", i)
    Xgp <- lapply(gpframe$covars[[i]], eval2, data)
    D <- length(Xgp)
    out[[paste0("Dgp", pi)]] <- D
    invalid <- ulapply(Xgp, function(x)
      !is.numeric(x) || isTRUE(length(dim(x)) > 1L)
    )
    if (any(invalid)) {
      stop2("Predictors of Gaussian processes should be numeric vectors.")
    }
    Xgp <- do_call(cbind, Xgp)
    cmc <- gpframe$cmc[i]
    scale <- gpframe$scale[i]
    gr <- gpframe$gr[i]
    k <- gpframe$k[i]
    c <- gpframe$c[[i]]
    if (!isNA(k)) {
      out[[paste0("NBgp", pi)]] <- k ^ D
      Ks <- as.matrix(do_call(expand.grid, repl(seq_len(k), D)))
    }
    byvar <- gpframe$byvars[[i]]
    byfac <- length(gpframe$cons[[i]]) > 0L
    bynum <- !is.null(byvar) && !byfac
    if (byfac) {
      # for categorical 'by' variables prepare one GP per level
      # as.factor will keep unused levels needed for new data
      byval <- as.factor(get(byvar, data))
      byform <- str2formula(c(ifelse(cmc, "0", "1"), "byval"))
      con_mat <- model.matrix(byform)
      cons <- colnames(con_mat)
      out[[paste0("Kgp", pi)]] <- length(cons)
      Ngp <- Nsubgp <- vector("list", length(cons))
      for (j in seq_along(cons)) {
        # loop along contrasts of 'by'
        Cgp <- con_mat[, j]
        sfx <- paste0(pi, "_", j)
        tmp <- .data_gp(
          Xgp, k = k, gr = gr, sfx = sfx, Cgp = Cgp, c = c,
          scale = scale, internal = internal, basis = basis,
          ...
        )
        Ngp[[j]] <- attributes(tmp)[["Ngp"]]
        Nsubgp[[j]] <- attributes(tmp)[["Nsubgp"]]
        c(out) <- tmp
      }
      out[[paste0("Ngp", pi)]] <- unlist(Ngp)
      if (gr) {
        out[[paste0("Nsubgp", pi)]] <- unlist(Nsubgp)
      }
    } else {
      out[[paste0("Kgp", pi)]] <- 1L
      c(out) <- .data_gp(
        Xgp, k = k, gr = gr, sfx = pi, c = c,
        scale = scale, internal = internal, basis = basis,
        ...
      )
      if (bynum) {
        Cgp <- as.numeric(get(byvar, data))
        out[[paste0("Cgp", pi)]] <- as.array(Cgp)
      }
    }
  }
  if (length(basis)) {
    # original covariate values are required in new GP prediction
    Xgp_old <- basis[grepl("^Xgp", names(basis))]
    names(Xgp_old) <- paste0(names(Xgp_old), "_old")
    out[names(Xgp_old)] <- Xgp_old
  }
  out
}

# helper function to preparae GP related data
# @inheritParams data_gp
# @param Xgp matrix of covariate values
# @param k, gr, c see 'frame_gp'
# @param sfx suffix to put at the end of data names
# @param Cgp optional vector of values belonging to
#   a certain contrast of a factor 'by' variable
.data_gp <- function(Xgp, k, gr, sfx, Cgp = NULL, c = NULL,
                     scale = TRUE, internal = FALSE, basis = NULL) {
  out <- list()
  if (!is.null(Cgp)) {
    Cgp <- unname(Cgp)
    Igp <- which(Cgp != 0)
    Xgp <- Xgp[Igp, , drop = FALSE]
    out[[paste0("Igp", sfx)]] <- as.array(Igp)
    out[[paste0("Cgp", sfx)]] <- as.array(Cgp[Igp])
    attr(out, "Ngp") <- length(Igp)
  }
  if (gr) {
    groups <- factor(match_rows(Xgp, Xgp))
    ilevels <- levels(groups)
    Jgp <- match(groups, ilevels)
    Nsubgp <- length(ilevels)
    if (!is.null(Cgp)) {
      attr(out, "Nsubgp") <- Nsubgp
    } else {
      out[[paste0("Nsubgp", sfx)]]  <- Nsubgp
    }
    out[[paste0("Jgp", sfx)]] <- as.array(Jgp)
    not_dupl_Jgp <- !duplicated(Jgp)
    Xgp <- Xgp[not_dupl_Jgp, , drop = FALSE]
  }
  if (scale) {
    # scale predictor for easier specification of priors
    if (length(basis)) {
      # scale Xgp based on the original data
      dmax <- basis[[paste0("dmax", sfx)]]
    } else {
      dmax <- sqrt(max(diff_quad(Xgp)))
    }
    if (!isTRUE(dmax > 0)) {
      stop2("Could not scale GP covariates. Please set 'scale' to FALSE in 'gp'.")
    }
    if (internal) {
      # required for scaling of GPs with new data
      out[[paste0("dmax", sfx)]] <- dmax
    }
    Xgp <- Xgp / dmax
  }
  if (length(basis)) {
    # center Xgp based on the original data
    cmeans <- basis[[paste0("cmeans", sfx)]]
  } else {
    cmeans <- colMeans(Xgp)
  }
  if (internal) {
    # required for centering of approximate GPs with new data
    out[[paste0("cmeans", sfx)]] <- cmeans
    # required to compute inverse-gamma priors for length-scales
    out[[paste0("Xgp_prior", sfx)]] <- Xgp
  }
  if (!isNA(k)) {
    # basis function approach requires centered variables
    Xgp <- sweep(Xgp, 2, cmeans)
    D <- NCOL(Xgp)

    if (length(basis)) {
      L <- basis[[paste0("Lgp", sfx)]]
    } else {
      # compute boundary factor L
      L <- choose_L(Xgp, c = c)
    }

    if (internal) {
      # required to compute eigenfunctions of approximate GPs with new data
      out[[paste0("Lgp", sfx)]] <- L
    }

    Ks <- as.matrix(do_call(expand.grid, repl(seq_len(k), D)))
    XgpL <- matrix(nrow = NROW(Xgp), ncol = NROW(Ks))
    slambda <- matrix(nrow = NROW(Ks), ncol = D)
    for (m in seq_rows(Ks)) {
      XgpL[, m] <- eigen_fun_laplacian(Xgp, m = Ks[m, ], L = L)
      slambda[m, ] <- sqrt(eigen_val_laplacian(m = Ks[m, ], L = L))
    }
    out[[paste0("Xgp", sfx)]] <- XgpL
    out[[paste0("slambda", sfx)]] <- slambda
  } else {
    out[[paste0("Xgp", sfx)]] <- as.array(Xgp)
  }
  out
}

# data for autocorrelation variables
data_ac <- function(bframe, data, data2, ...) {
  if (!is.null(bframe$sdata$ac)) {
    # standata was already precomputed
    return(bframe$sdata$ac)
  }
  out <- list()
  N <- nrow(data)
  basis <- bframe$basis$ac
  acframe <- bframe$frame$ac
  stopifnot(is.acframe(acframe))
  if (has_ac_subset(bframe, dim = "time")) {
    gr <- get_ac_vars(acframe, "gr", dim = "time")
    if (isTRUE(nzchar(gr))) {
      tgroup <- as.numeric(factor(data[[gr]]))
    } else {
      tgroup <- rep(1, N)
    }
  }
  if (has_ac_class(acframe, "arma")) {
    # ARMA correlations
    acframe_arma <- subset2(acframe, class = "arma")
    out$Kar <- acframe_arma$p
    out$Kma <- acframe_arma$q
    if (!use_ac_cov_time(acframe_arma)) {
      # data for the 'predictor' version of ARMA
      max_lag <- max(out$Kar, out$Kma)
      out$J_lag <- as.array(rep(0, N))
      for (n in seq_len(N)[-N]) {
        ind <- n:max(1, n + 1 - max_lag)
        # indexes errors to be used in the n+1th prediction
        out$J_lag[n] <- sum(tgroup[ind] %in% tgroup[n + 1])
      }
    }
  }
  if (use_ac_cov_time(acframe)) {
    # data for the 'covariance' versions of time-series structures
    # TODO: change begin[i]:end[i] notation to slice[i]:(slice[i+1] - 1)
    #   see comment on PR #1435
    out$N_tg <- length(unique(tgroup))
    out$begin_tg <- as.array(ulapply(unique(tgroup), match, tgroup))
    out$nobs_tg <- as.array(with(out,
                                 c(if (N_tg > 1L) begin_tg[2:N_tg], N + 1) - begin_tg
    ))
    out$end_tg <- with(out, begin_tg + nobs_tg - 1)
    if (has_ac_class(acframe, "unstr")) {
      time <- get_ac_vars(bframe, "time", dim = "time")
      time_data <- get(time, data)
      new_times <- extract_levels(time_data)
      if (length(basis)) {
        times <- basis$times
        # unstr estimates correlations only for given time points
        invalid_times <- setdiff(new_times, times)
        if (length(invalid_times)) {
          stop2("Cannot handle new time points in UNSTR models.")
        }
      } else {
        times <- new_times
      }
      out$n_unique_t <- length(times)
      out$n_unique_cortime <- out$n_unique_t * (out$n_unique_t - 1) / 2
      Jtime <- match(time_data, times)
      out$Jtime_tg <- matrix(0L, out$N_tg, max(out$nobs_tg))
      for (i in seq_len(out$N_tg)) {
        out$Jtime_tg[i, seq_len(out$nobs_tg[i])] <-
          Jtime[out$begin_tg[i]:out$end_tg[i]]
      }
    }
  }
  if (has_ac_class(acframe, "sar")) {
    acframe_sar <- subset2(acframe, class = "sar")
    M <- data2[[acframe_sar$M]]
    rmd_rows <- attr(data, "na.action")
    if (!is.null(rmd_rows)) {
      class(rmd_rows) <- NULL
      M <- M[-rmd_rows, -rmd_rows, drop = FALSE]
    }
    if (!is_equal(dim(M), rep(N, 2))) {
      stop2("Dimensions of 'M' for SAR terms must be equal to ",
            "the number of observations.")
    }
    out$Msar <- as.matrix(M)
    out$eigenMsar <- eigen(M)$values
    # simplifies code of choose_N
    out$N_tg <- 1
  }
  if (has_ac_class(acframe, "car")) {
    acframe_car <- subset2(acframe, class = "car")
    locations <- NULL
    if (length(basis)) {
      locations <- basis$locations
    }
    M <- data2[[acframe_car$M]]
    if (acframe_car$gr != "NA") {
      loc_data <- get(acframe_car$gr, data)
      new_locations <- extract_levels(loc_data)
      if (is.null(locations)) {
        locations <- new_locations
      } else {
        invalid_locations <- setdiff(new_locations, locations)
        if (length(invalid_locations)) {
          stop2("Cannot handle new locations in CAR models.")
        }
      }
      Nloc <- length(locations)
      Jloc <- as.array(match(loc_data, locations))
      if (is.null(rownames(M))) {
        stop2("Row names are required for 'M' in CAR terms.")
      }
      found <- locations %in% rownames(M)
      if (any(!found)) {
        stop2("Row names of 'M' for CAR terms do not match ",
              "the names of the grouping levels.")
      }
      M <- M[locations, locations, drop = FALSE]
    } else {
      warning2(
        "Using CAR terms without a grouping factor is deprecated. ",
        "Please use argument 'gr' even if each observation ",
        "represents its own location."
      )
      Nloc <- N
      Jloc <- as.array(seq_len(Nloc))
      if (!is_equal(dim(M), rep(Nloc, 2))) {
        if (length(basis)) {
          stop2("Cannot handle new data in CAR terms ",
                "without a grouping factor.")
        } else {
          stop2("Dimensions of 'M' for CAR terms must be equal ",
                "to the number of observations.")
        }
      }
    }
    edges_rows <- (Matrix::tril(M)@i + 1)
    edges_cols <- sort(Matrix::triu(M)@i + 1) ## sort to make consistent with rows
    edges <- cbind("rows" = edges_rows, "cols" = edges_cols)
    c(out) <- nlist(
      Nloc, Jloc, Nedges = length(edges_rows),
      edges1 = as.array(edges_rows),
      edges2 = as.array(edges_cols)
    )
    if (acframe_car$type %in% c("escar", "esicar")) {
      Nneigh <- Matrix::colSums(M)
      if (any(Nneigh == 0) && !length(basis)) {
        stop2(
          "For exact sparse CAR, all locations should have at ",
          "least one neighbor within the provided data set. ",
          "Consider using type = 'icar' instead."
        )
      }
      inv_sqrt_D <- diag(1 / sqrt(Nneigh))
      eigenMcar <- t(inv_sqrt_D) %*% M %*% inv_sqrt_D
      eigenMcar <- eigen(eigenMcar, TRUE, only.values = TRUE)$values
      c(out) <- nlist(Nneigh, eigenMcar)
    } else if (acframe_car$type %in% "bym2") {
      c(out) <- list(car_scale = .car_scale(edges, Nloc))
    }
  }
  if (has_ac_class(acframe, "fcor")) {
    acframe_fcor <- subset2(acframe, class = "fcor")
    M <- data2[[acframe_fcor$M]]
    rmd_rows <- attr(data, "na.action")
    if (!is.null(rmd_rows)) {
      class(rmd_rows) <- NULL
      M <- M[-rmd_rows, -rmd_rows, drop = FALSE]
    }
    if (nrow(M) != N) {
      stop2("Dimensions of 'M' for FCOR terms must be equal ",
            "to the number of observations.")
    }
    out$Mfcor <- M
    # simplifies code of choose_N
    out$N_tg <- 1
  }
  if (length(out)) {
    resp <- usc(combine_prefix(bframe))
    out <- setNames(out, paste0(names(out), resp))
  }
  out
}

# prepare data of offsets for use in Stan
data_offset <- function(bframe, data) {
  stopifnot(is.btl(bframe))
  if (!is.null(bframe$sdata$offset)) {
    # standata was already precomputed
    return(bframe$sdata$offset)
  }
  out <- list()
  px <- check_prefix(bframe)
  if (is.formula(bframe$offset)) {
    p <- usc(combine_prefix(px))
    mf <- rm_attr(data, "terms")
    mf <- model.frame(bframe$offset, mf, na.action = na.pass)
    offset <- model.offset(mf)
    if (length(offset) == 1L) {
      offset <- rep(offset, nrow(data))
    }
    # use 'offsets' as 'offset' will be reserved in stanc3
    out[[paste0("offsets", p)]] <- as.array(offset)
  }
  out
}

# data for covariates in non-linear models
# @param x a btnl object
# @return a named list of data passed to Stan
data_cnl <- function(bframe, data) {
  stopifnot(is.btnl(bframe))
  if (!is.null(bframe$sdata$cnl)) {
    # standata was already precomputed
    return(bframe$sdata$cnl)
  }
  out <- list()
  covars <- all.vars(bframe$covars)
  if (!length(covars)) {
    return(out)
  }
  p <- usc(combine_prefix(bframe))
  for (i in seq_along(covars)) {
    cvalues <- get(covars[i], data)
    if (is_like_factor(cvalues)) {
      # need to apply factor contrasts
      cform <- str2formula(covars[i])
      cvalues <- get_model_matrix(cform, data, cols2remove = "(Intercept)")
      if (NCOL(cvalues) == 1L) {
        dim(cvalues) <- NULL
      }
    }
    if (isTRUE(dim(cvalues) > 2L)) {
      stop2("Non-linear covariates should be vectors or matrices.")
    }
    out[[paste0("C", p, "_", i)]] <- as.array(cvalues)
  }
  out
}

# compute the spatial scaling factor of CAR models
# @param edges matrix with two columns defining the adjacency of the locations
# @param Nloc number of locations
# @return a scalar scaling factor
.car_scale <- function(edges, Nloc) {
  # amended from Imad Ali's code of CAR models in rstanarm
  stopifnot(is.matrix(edges), NCOL(edges) == 2)
  # Build the adjacency matrix
  adj_matrix <- Matrix::sparseMatrix(
    i = edges[, 1], j = edges[, 2], x = 1,
    symmetric = TRUE
  )
  # The ICAR precision matrix (which is singular)
  Q <- Matrix::Diagonal(Nloc, Matrix::rowSums(adj_matrix)) - adj_matrix
  # Add a small jitter to the diagonal for numerical stability
  Q_pert <- Q + Matrix::Diagonal(Nloc) *
    max(Matrix::diag(Q)) * sqrt(.Machine$double.eps)
  # Compute the diagonal elements of the covariance matrix subject to the
  # constraint that the entries of the ICAR sum to zero.
  .Q_inv <- function(Q) {
    Sigma <- Matrix::solve(Q)
    A <- matrix(1, 1, NROW(Sigma))
    W <- Sigma %*% t(A)
    Sigma <- Sigma - W %*% solve(A %*% W) %*% Matrix::t(W)
    return(Sigma)
  }
  Q_inv <- .Q_inv(Q_pert)
  # Compute the geometric mean of the variances (diagonal of Q_inv)
  exp(mean(log(Matrix::diag(Q_inv))))
}

# data for special priors such as horseshoe and R2D2
data_special_prior <- function(bframe, data, prior, sdata = NULL) {
  out <- list()
  px <- check_prefix(bframe)
  p <- usc(combine_prefix(px))
  if (!has_special_prior(prior, px)) {
    return(out)
  }

  # number of coefficients affected by the shrinkage prior
  # fully compute this here to avoid having to pass the prior around
  # to all the individual data preparation functions
  # the order of adding things to Kscales doesn't matter but for consistency
  # it is still the same as the order in the Stan code
  Kscales <- 0
  if (has_special_prior(prior, px, class = "b")) {
    Kscales <- Kscales +
      sdata[[paste0("Kc", p)]] %||% sdata[[paste0("K", p)]] %||% 0 +
      sdata[[paste0("Ksp", p)]] %||% 0 +
      sdata[[paste0("Ks", p)]] %||% 0
  }
  if (has_special_prior(prior, px, class = "sds")) {
    take <- grepl(paste0("^nb", p, "_"), names(sdata))
    Kscales <- Kscales + sum(unlist(sdata[take]))
  }
  if (has_special_prior(prior, px, class = "sdgp")) {
    take <- grepl(paste0("^Kgp", p, "_"), names(sdata))
    Kscales <- Kscales + sum(unlist(sdata[take]))
  }
  if (has_special_prior(prior, px, class = "ar")) {
    Kscales <- Kscales + sdata[[paste0("Kar", p)]]
  }
  if (has_special_prior(prior, px, class = "ma")) {
    Kscales <- Kscales + sdata[[paste0("Kma", p)]]
  }
  if (has_special_prior(prior, px, class = "sderr")) {
    Kscales <- Kscales + 1
  }
  if (has_special_prior(prior, px, class = "sdcar")) {
    Kscales <- Kscales + 1
  }
  if (has_special_prior(prior, px, class = "sd")) {
    ids <- unique(bframe$frame$re$id)
    Kscales <- Kscales + sum(unlist(sdata[paste0("M_", ids)]))
  }
  out[[paste0("Kscales", p)]] <- Kscales

  special <- get_special_prior(prior, px, main = TRUE)
  if (special$name == "horseshoe") {
    # data for the horseshoe prior
    hs_names <- c("df", "df_global", "df_slab", "scale_global", "scale_slab")
    hs_data <- special[hs_names]
    if (!is.null(special$par_ratio)) {
      hs_data$scale_global <- special$par_ratio / sqrt(nrow(data))
    }
    names(hs_data) <- paste0("hs_", hs_names, p)
    c(out) <- hs_data
  } else if (special$name == "R2D2") {
    # data for the R2D2 prior
    R2D2_names <- c("mean_R2", "prec_R2", "cons_D2")
    R2D2_data <- special[R2D2_names]
    if (length(R2D2_data$cons_D2) == 1L) {
      R2D2_data$cons_D2 <- rep(R2D2_data$cons_D2, Kscales)
    }
    if (length(R2D2_data$cons_D2) != Kscales) {
      stop2("Argument 'cons_D2' of the R2D2 prior must be of length 1 or ", Kscales)
    }
    R2D2_data$cons_D2 <- as.array(R2D2_data$cons_D2)
    names(R2D2_data) <- paste0("R2D2_", R2D2_names, p)
    c(out) <- R2D2_data
  }
  out
}

# Construct design matrices for brms models
# @param formula a formula object
# @param data A data frame created with model.frame.
#   If another sort of object, model.frame is called first.
# @param cols2remove names of the columns to remove from
#   the model matrix; mainly used for intercepts
# @param rename rename column names via rename()?
# @param ... passed to stats::model.matrix
# @return
#   The design matrix for the given formula and data.
#   For details see ?stats::model.matrix
get_model_matrix <- function(formula, data = environment(formula),
                             cols2remove = NULL, rename = TRUE, ...) {
  stopifnot(is_atomic_or_null(cols2remove))
  terms <- validate_terms(formula)
  if (is.null(terms)) {
    return(NULL)
  }
  if (no_int(terms)) {
    cols2remove <- union(cols2remove, "(Intercept)")
  }
  X <- stats::model.matrix(terms, data, ...)
  cols2remove <- which(colnames(X) %in% cols2remove)
  if (length(cols2remove)) {
    X <- X[, -cols2remove, drop = FALSE]
  }
  if (rename) {
    colnames(X) <- rename(colnames(X), check_dup = TRUE)
  }
  X
}

# convenient wrapper around mgcv::PredictMat
PredictMat <- function(object, data, ...) {
  data <- sm_prepare_data(object, data)
  out <- mgcv::PredictMat(object, data = data, ...)
  if (length(dim(out)) < 2L) {
    # fixes issue #494
    out <- matrix(out, nrow = 1)
  }
  out
}

# convenient wrapper around mgcv::smoothCon
smoothCon <- function(object, data, ...) {
  data <- sm_prepare_data(object, data)
  mgcv::smoothCon(object, data = data, ...)
}

# mgcv doesn't handle a lot of special data types well
# need to prepare these variables manually beforehand
sm_prepare_data <- function(object, data) {
  data <- rm_attr(data, "terms")
  vars <- setdiff(c(object$term, object$by), "NA")
  for (v in vars) {
    if (is_like_factor(data[[v]])) {
      # allow factor-like variables #562
      data[[v]] <- as.factor(data[[v]])
    } else if (inherits(data[[v]], "difftime")) {
      # mgcv cannot handle 'difftime' variables
      data[[v]] <- as.numeric(data[[v]])
    }
  }
  data
}

# Aid prediction from smooths represented as 'type = 2'
# code obtained from the doc of ?mgcv::smooth2random
# @param sm output of mgcv::smoothCon
# @param re output of mgcv::smooth2random
# @param data new data supplied for prediction
# @return A list of the same structure as returned by mgcv::smooth2random
s2rPred <- function(sm, re, data) {
  # prediction matrix for new data
  X <- PredictMat(sm, data)
  # transform to RE parameterization
  if (!is.null(re$trans.U)) {
    X <- X %*% re$trans.U
  }
  if (is.null(re$trans.D)) {
    # regression spline without penalization
    out <- list(Xf = X)
  } else {
    X <- t(t(X) * re$trans.D)
    # re-order columns according to random effect re-ordering
    X[, re$rind] <- X[, re$pen.ind != 0]
    # re-order penalization index in same way
    pen.ind <- re$pen.ind
    pen.ind[re$rind] <- pen.ind[pen.ind > 0]
    # start returning the object
    Xf <- X[, which(re$pen.ind == 0), drop = FALSE]
    out <- list(rand = list(), Xf = Xf)
    for (i in seq_along(re$rand)) {
      # loop over random effect matrices
      out$rand[[i]] <- X[, which(pen.ind == i), drop = FALSE]
      attr(out$rand[[i]], "s.label") <- attr(re$rand[[i]], "s.label")
    }
    names(out$rand) <- names(re$rand)
  }
  out
}

#' Extract response values
#'
#' Extract response values from a \code{\link{brmsfit}} object.
#'
#' @param x A \code{\link{brmsfit}} object.
#' @param resp Optional names of response variables for which to extract values.
#' @param warn For internal use only.
#' @param ... Further arguments passed to \code{\link{standata}}.
#' @inheritParams posterior_predict.brmsfit
#'
#' @return Returns a vector of response values for univariate models and a
#'   matrix of response values with one column per response variable for
#'   multivariate models.
#'
#' @keywords internal
#' @export
get_y <- function(x, resp = NULL, sort = FALSE, warn = FALSE,  ...) {
  stopifnot(is.brmsfit(x))
  resp <- validate_resp(resp, x)
  sort <- as_one_logical(sort)
  warn <- as_one_logical(warn)
  args <- list(x, resp = resp, ...)
  args$re_formula <- NA
  args$check_response <- TRUE
  args$only_response <- TRUE
  args$internal <- TRUE
  sdata <- do_call(standata, args)
  if (warn) {
    if (any(paste0("cens", usc(resp)) %in% names(sdata))) {
      warning2("Results may not be meaningful for censored models.")
    }
  }
  Ynames <- paste0("Y", usc(resp))
  if (length(Ynames) > 1L) {
    out <- do_call(cbind, sdata[Ynames])
    colnames(out) <- resp
  } else {
    out <- sdata[[Ynames]]
  }
  old_order <- attr(sdata, "old_order")
  if (!is.null(old_order) && !sort) {
    stopifnot(length(old_order) == NROW(out))
    out <- p(out, old_order)
  }
  out
}

#' Prepare Response Data
#'
#' Prepare data related to response variables in \pkg{brms}.
#' Only exported for use in package development.
#'
#' @param x An \R object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A named list of data related to response variables.
#'
#' @keywords internal
#' @export
data_response <- function(x, ...) {
  UseMethod("data_response")
}

#' @export
data_response.mvbrmsframe <- function(x, ...) {
  out <- list()
  for (i in seq_along(x$terms)) {
    c(out) <- data_response(x$terms[[i]], ...)
  }
  if (x$rescor) {
    out$nresp <- length(x$responses)
    out$nrescor <- out$nresp * (out$nresp - 1) / 2
  }
  out
}

#' @export
data_response.brmsframe <- function(x, data, check_response = TRUE,
                                    internal = FALSE, ...) {
  data <- subset_data(data, x)
  N <- nrow(data)
  # TODO: rename 'Y' to 'y'
  Y <- model.response(model.frame(x$respform, data, na.action = na.pass))
  out <- list(N = N, Y = unname(Y))
  if (is_binary(x$family)) {
    bin_levels <- x$frame$basis$resp_levels
    if (is.null(bin_levels)) {
      bin_levels <- levels(as.factor(out$Y))
    }
    # fixes issues #1298 and #1511
    if (is.numeric(out$Y) && length(bin_levels) == 1L) {
      if (0 %in% bin_levels) {
        # 1 as default event level
        bin_levels <- c(0, 1)
      } else {
        # 0 as default non-event level
        bin_levels <- c(0, bin_levels)
      }
    }
    out$Y <- as.integer(as_factor(out$Y, levels = bin_levels)) - 1
  }
  if (is_categorical(x$family)) {
    out$Y <- as.integer(as_factor(out$Y, levels = x$frame$basis$resp_levels))
  }
  if (is_ordinal(x$family) && is.ordered(out$Y)) {
    diff <- ifelse(has_extra_cat(x$family), 1L, 0L)
    out$Y <- as.integer(out$Y) - diff
  }
  if (check_response) {
    family4error <- family_names(x$family)
    if (is.mixfamily(x$family)) {
      family4error <- paste0(family4error, collapse = ", ")
      family4error <- paste0("mixture(", family4error, ")")
    }
    if (!allow_factors(x$family) && !is.numeric(out$Y)) {
      stop2("Family '", family4error, "' requires numeric responses.")
    }
    if (is_binary(x$family)) {
      if (any(!out$Y %in% c(0, 1))) {
        stop2("Family '", family4error, "' requires responses ",
              "to contain only two different values.")
      }
    }
    if (is_ordinal(x$family)) {
      extra_cat <- has_extra_cat(x$family)
      min_int <- ifelse(extra_cat, 0L, 1L)
      msg <- ifelse(extra_cat, "non-negative", "positive")
      if (any(!is_wholenumber(out$Y)) || any(out$Y < min_int)) {
        stop2("Family '", family4error, "' requires either ", msg,
              " integers or ordered factors as responses.")
      }
    }
    if (use_int(x$family)) {
      if (!all(is_wholenumber(out$Y))) {
        stop2("Family '", family4error, "' requires integer responses.")
      }
    }
    if (has_multicol(x$family)) {
      if (!is.matrix(out$Y)) {
        stop2("This model requires a response matrix.")
      }
    }
    if (is_simplex(x$family)) {
      if (!is_equal(rowSums(out$Y), rep(1, nrow(out$Y)))) {
        stop2("Response values in simplex models must sum to 1.")
      }
    }
    ybounds <- family_info(x$family, "ybounds")
    closed <- family_info(x$family, "closed")
    if (is.finite(ybounds[1])) {
      y_min <- min(out$Y, na.rm = TRUE)
      if (closed[1] && y_min < ybounds[1]) {
        stop2("Family '", family4error, "' requires response greater ",
              "than or equal to ", ybounds[1], ".")
      } else if (!closed[1] && y_min <= ybounds[1]) {
        stop2("Family '", family4error, "' requires response greater ",
              "than ", round(ybounds[1], 2), ".")
      }
    }
    if (is.finite(ybounds[2])) {
      y_max <- max(out$Y, na.rm = TRUE)
      if (closed[2] && y_max > ybounds[2]) {
        stop2("Family '", family4error, "' requires response smaller ",
              "than or equal to ", ybounds[2], ".")
      } else if (!closed[2] && y_max >= ybounds[2]) {
        stop2("Family '", family4error, "' requires response smaller ",
              "than ", round(ybounds[2], 2), ".")
      }
    }
    out$Y <- as.array(out$Y)
  }

  # data for addition arguments of the response
  # TODO: replace is.formula(x$adforms$term) pattern with has_ad_terms()
  if (has_trials(x$family) || is.formula(x$adforms$trials)) {
    if (!length(x$adforms$trials)) {
      stop2("Specifying 'trials' is required for this model.")
    }
    if (!is.formula(x$adforms$trials)) {
      stop2("Argument 'trials' is misspecified.")
    }
    trials <- get_ad_values(x, "trials", "trials", data)
    if (!is.numeric(trials)) {
      stop2("Number of trials must be numeric.")
    }
    if (any(!is_wholenumber(trials) | trials < 0)) {
      stop2("Number of trials must be non-negative integers.")
    }
    if (length(trials) == 1L) {
      trials <- rep(trials, nrow(data))
    }
    if (check_response) {
      if (is_multinomial(x$family)) {
        if (!is_equal(rowSums(out$Y), trials)) {
          stop2("Number of trials does not match the number of events.")
        }
      } else if (has_trials(x$family)) {
        if (max(trials) == 1L && !internal) {
          message("Only 2 levels detected so that family 'bernoulli' ",
                  "might be a more efficient choice.")
        }
        if (any(out$Y > trials)) {
          stop2("Number of trials is smaller than the number of events.")
        }
      }
    }
    out$trials <- as.array(trials)
  }
  if (has_cat(x$family)) {
    ncat <- length(get_cats(x$family))
    if (min(ncat) < 2L) {
      stop2("At least two response categories are required.")
    }
    if (!has_multicol(x$family)) {
      if (ncat == 2L && !internal) {
        message("Only 2 levels detected so that family 'bernoulli' ",
                "might be a more efficient choice.")
      }
      if (check_response && any(out$Y > ncat)) {
        stop2("Number of categories is smaller than the response ",
              "variable would suggest.")
      }
    }
    out$ncat <- ncat
  }
  if (has_thres(x$family)) {
    thres <- family_info(x, "thres")
    if (has_thres_groups(x$family)) {
      groups <- get_thres_groups(x)
      out$ngrthres <- length(groups)
      grthres <- get_ad_values(x, "thres", "gr", data)
      grthres <- factor(rename(grthres), levels = groups)
      # create an matrix of threshold indices per observation
      Jgrthres <- match(grthres, groups)
      nthres <- as.array(rep(NA, length(groups)))
      for (i in seq_along(groups)) {
        nthres[i] <- max(subset2(thres, group = groups[i])$thres)
      }
      if (check_response && any(out$Y > nthres[Jgrthres] + 1)) {
        stop2("Number of thresholds is smaller than required by the response.")
      }
      Kthres_cumsum <- cumsum(nthres)
      Kthres_start <- c(1, Kthres_cumsum[-length(nthres)] + 1)
      Kthres_end <- Kthres_cumsum
      Jthres <- cbind(Kthres_start, Kthres_end)[Jgrthres, , drop = FALSE]
      out$Jthres <- Jthres
    } else {
      nthres <- max(thres$thres)
      if (check_response && any(out$Y > nthres + 1)) {
        stop2("Number of thresholds is smaller than required by the response.")
      }
    }
    if (max(nthres) == 1L && !internal) {
      message("Only 2 levels detected so that family 'bernoulli' ",
              "might be a more efficient choice.")
    }
    out$nthres <- nthres
  }
  if (is.formula(x$adforms$cat)) {
    warning2("Addition argument 'cat' is deprecated. Use 'thres' instead. ",
             "See ?brmsformula for more details.")
  }

  if (is.formula(x$adforms$se)) {
    se <- get_ad_values(x, "se", "se", data)
    if (!is.numeric(se)) {
      stop2("Standard errors must be numeric.")
    }
    if (min(se) < 0) {
      stop2("Standard errors must be non-negative.")
    }
    out$se <- as.array(se)
  }
  if (is.formula(x$adforms$weights)) {
    weights <- get_ad_values(x, "weights", "weights", data)
    if (!is.numeric(weights)) {
      stop2("Weights must be numeric.")
    }
    if (min(weights) < 0) {
      stop2("Weights must be non-negative.")
    }
    if (get_ad_flag(x, "weights", "scale")) {
      weights <- weights / sum(weights) * length(weights)
    }
    out$weights <- as.array(weights)
  }
  if (is.formula(x$adforms$dec)) {
    dec <- get_ad_values(x, "dec", "dec", data)
    if (is.character(dec) || is.factor(dec)) {
      if (!all(unique(dec) %in% c("lower", "upper"))) {
        stop2("Decisions should be 'lower' or 'upper' ",
              "when supplied as characters or factors.")
      }
      dec <- ifelse(dec == "lower", 0, 1)
    } else {
      dec <- as.numeric(as.logical(dec))
    }
    out$dec <- as.array(dec)
  }
  if (is.formula(x$adforms$rate)) {
    denom <- get_ad_values(x, "rate", "denom", data)
    if (!is.numeric(denom)) {
      stop2("Rate denomiators should be numeric.")
    }
    if (isTRUE(any(denom <= 0))) {
      stop2("Rate denomiators should be positive.")
    }
    out$denom <- as.array(denom)
  }
  if (is.formula(x$adforms$cens) && check_response) {
    cens <- get_ad_values(x, "cens", "cens", data)
    cens <- prepare_cens(cens)
    if (!all(is_wholenumber(cens) & cens %in% -1:2)) {
      stop2(
        "Invalid censoring data. Accepted values are ",
        "'left', 'none', 'right', and 'interval'\n",
        "(abbreviations are allowed) or -1, 0, 1, and 2.\n",
        "TRUE and FALSE are also accepted ",
        "and refer to 'right' and 'none' respectively."
      )
    }
    if (length(cens) == 1L) {
      cens <- rep(cens, N)
    }
    if (length(cens) != N) {
      stop2("Censoring information needs to have length ",
            "equal to the number of data rows.")
    }
    out$cens <- as.array(cens)
    icens <- cens %in% 2
    if (any(icens) || has_interval_cens(x)) {
      # interval censoring is required
      y2 <- unname(get_ad_values(x, "cens", "y2", data))
      if (is.null(y2)) {
        stop2("Argument 'y2' is required for interval censored data.")
      }
      if (length(y2) != N) {
        stop2("Argument 'y2' needs to have length equal to the number of data rows.")
      }
      if (anyNA(y2[icens])) {
        stop2("'y2' should not be NA for interval censored observations.")
      }
      if (any(out$Y[icens] >= y2[icens])) {
        stop2("Left censor points must be smaller than right ",
              "censor points for interval censored data.")
      }
      y2[!icens] <- 0  # not used in Stan
      out$rcens <- as.array(y2)
    }
  }
  if (is.formula(x$adforms$trunc)) {
    lb <- as.numeric(get_ad_values(x, "trunc", "lb", data))
    ub <- as.numeric(get_ad_values(x, "trunc", "ub", data))
    if (any(lb >= ub)) {
      stop2("Truncation bounds are invalid: lb >= ub")
    }
    if (length(lb) == 1L) {
      lb <- rep(lb, N)
    }
    if (length(ub) == 1L) {
      ub <- rep(ub, N)
    }
    if (length(lb) != N || length(ub) != N) {
      stop2("Invalid truncation bounds.")
    }
    inv_bounds <- out$Y < lb | out$Y > ub
    if (check_response && isTRUE(any(inv_bounds))) {
      stop2("Some responses are outside of the truncation bounds.")
    }
    out$lb <- lb
    out$ub <- ub
  }
  if (is.formula(x$adforms$mi)) {
    sdy <- get_sdy(x, data)
    if (is.null(sdy)) {
      # missings only
      which_mi <- which(is.na(out$Y))
      out$Jmi <- as.array(which_mi)
      out$Nmi <- length(out$Jmi)
    } else {
      # measurement error in the response
      if (length(sdy) == 1L) {
        sdy <- rep(sdy, length(out$Y))
      }
      if (length(sdy) != length(out$Y)) {
        stop2("'sdy' must have the same length as the response.")
      }
      # all observations will have a latent score
      which_mi <- which(is.na(out$Y) | is.infinite(sdy))
      out$Jme <- as.array(setdiff(seq_along(out$Y), which_mi))
      out$Nme <- length(out$Jme)
      out$noise <- as.array(sdy)
      if (!internal) {
        out$noise[which_mi] <- Inf
      }
    }
    # bounds are required for predicting new missing values
    # not required in Stan right now as bounds are hard-coded there
    tbounds <- trunc_bounds(x, data, incl_family = TRUE)
    out$lbmi <- tbounds$lb
    out$ubmi <- tbounds$ub
    if (!internal) {
      # Stan does not allow NAs in data
      # use Inf to that min(Y) is not affected
      out$Y[which_mi] <- Inf
    }
  }
  if (is.formula(x$adforms$vreal)) {
    # vectors of real values for use in custom families
    vreal <- eval_rhs(x$adforms$vreal)
    vreal <- lapply(vreal$vars, eval2, data)
    names(vreal) <- paste0("vreal", seq_along(vreal))
    for (i in seq_along(vreal)) {
      if (length(vreal[[i]]) == 1L) {
        vreal[[i]] <- rep(vreal[[i]], N)
      }
      vreal[[i]] <- as.array(as.numeric(vreal[[i]]))
    }
    c(out) <- vreal
  }
  if (is.formula(x$adforms$vint)) {
    # vectors of integer values for use in custom families
    vint <- eval_rhs(x$adforms$vint)
    vint <- lapply(vint$vars, eval2, data)
    names(vint) <- paste0("vint", seq_along(vint))
    for (i in seq_along(vint)) {
      if (length(vint[[i]]) == 1L) {
        vint[[i]] <- rep(vint[[i]], N)
      }
      if (!all(is_wholenumber(vint[[i]]))) {
        stop2("'vint' requires whole numbers as input.")
      }
      vint[[i]] <- as.array(vint[[i]])
    }
    c(out) <- vint
  }
  if (length(out)) {
    resp <- usc(combine_prefix(x))
    out <- setNames(out, paste0(names(out), resp))
  }
  out
}

# data specific for mixture models
data_mixture <- function(bframe, data2, prior) {
  stopifnot(is.brmsterms(bframe))
  out <- list()
  if (is.mixfamily(bframe$family)) {
    families <- family_names(bframe$family)
    dp_classes <- dpar_class(names(c(bframe$dpars, bframe$fdpars)))
    if (!any(dp_classes %in% "theta")) {
      # estimate mixture probabilities directly
      take <- find_rows(prior, class = "theta", resp = bframe$resp)
      theta_prior <- prior$prior[take]
      con_theta <- eval_dirichlet(theta_prior, length(families), data2)
      out$con_theta <- as.array(con_theta)
      p <- usc(combine_prefix(bframe))
      names(out) <- paste0(names(out), p)
    }
  }
  out
}

# data for the baseline functions of Cox models
data_bhaz <- function(bframe, data, data2, prior) {
  out <- list()
  if (!is_cox(bframe$family)) {
    return(out)
  }
  y <- bframe$frame$resp$values
  bhaz <- family_info(bframe, "bhaz")
  bs <- bframe$basis$bhaz$basis_matrix
  out$Zbhaz <- bhaz_basis_matrix(y, bhaz$args, basis = bs)
  out$Zcbhaz <- bhaz_basis_matrix(y, bhaz$args, integrate = TRUE, basis = bs)
  out$Kbhaz <- NCOL(out$Zbhaz)
  groups <- bhaz$groups
  if (!is.null(groups)) {
    out$ngrbhaz <- length(groups)
    gr <- get_ad_values(bframe, "bhaz", "gr", data)
    gr <- factor(rename(gr), levels = groups)
    out$Jgrbhaz <- match(gr, groups)
    out$con_sbhaz <- matrix(nrow = out$ngrbhaz, ncol = out$Kbhaz)
    sbhaz_prior <- subset2(prior, class = "sbhaz", resp = bframe$resp)
    sbhaz_prior_global <- subset2(sbhaz_prior, group = "")
    con_sbhaz_global <- eval_dirichlet(sbhaz_prior_global$prior, out$Kbhaz, data2)
    for (k in seq_along(groups)) {
      sbhaz_prior_group <- subset2(sbhaz_prior, group = groups[k])
      if (nzchar(sbhaz_prior_group$prior)) {
        out$con_sbhaz[k, ] <- eval_dirichlet(sbhaz_prior_group$prior, out$Kbhaz, data2)
      } else {
        out$con_sbhaz[k, ] <- con_sbhaz_global
      }
    }
  } else {
    sbhaz_prior <- subset2(prior, class = "sbhaz", resp = bframe$resp)
    con_sbhaz <- eval_dirichlet(sbhaz_prior$prior, out$Kbhaz, data2)
    out$con_sbhaz <- as.array(con_sbhaz)
  }
  out
}

# Basis matrices for baseline hazard functions of the Cox model
# @param y vector of response values
# @param args arguments passed to the spline generating functions
# @param integrate compute the I-spline instead of the M-spline basis?
# @param basis optional precomputed basis matrix
# @return the design matrix of the baseline hazard function
bhaz_basis_matrix <- function(y, args = list(), integrate = FALSE,
                              basis = NULL) {
  # version check is required due to class name changes #1580
  require_package("splines2", version = "0.5.0")
  if (!is.null(basis)) {
    # perform predictions based on an existing basis matrix
    stopifnot(inherits(basis, "MSpline"))
    if (integrate) {
      # for predictions just the attributes are required
      # which are the same of M-Splines and I-Splines
      class(basis) <- c("matrix", "ISpline")
    }
    return(predict(basis, y))
  }
  stopifnot(is.list(args))
  args$x <- y
  if (is.null(args$Boundary.knots)) {
    # avoid 'knots' outside 'Boundary.knots' error (#1143)
    # we also need a smaller lower boundary knot to avoid lp = -Inf
    # the below choices are ad-hoc and may need further thought
    min_y <- min(y, na.rm = TRUE)
    max_y <- max(y, na.rm = TRUE)
    diff_y <- max_y - min_y
    lower_knot <- max(min_y - diff_y / 50, 0)
    upper_knot <- max_y + diff_y / 50
    args$Boundary.knots <- c(lower_knot, upper_knot)
  }
  if (integrate) {
    out <- do_call(splines2::iSpline, args)
  } else {
    out <- do_call(splines2::mSpline, args)
  }
  out
}

# extract baseline hazard information from data for storage in the model family
# @return a named list with elements:
#  args: arguments that can be passed to bhaz_basis_matrix
#  groups: optional names of the groups for which to stratify
extract_bhaz <- function(x, data) {
  stopifnot(is.brmsformula(x) || is.brmsterms(x), is_cox(x))
  if (is.null(x$adforms)) {
    x$adforms <- terms_ad(x$formula, x$family)
  }
  out <- list()
  if (is.null(x$adforms$bhaz)) {
    # bhaz is an optional addition term so defaults need to be listed here too
    out$args <- list(df = 5, intercept = TRUE)
  } else {
    out$args <- eval_rhs(x$adforms$bhaz)$flags
    gr <- get_ad_values(x, "bhaz", "gr", data)
    if (!is.null(gr)) {
      out$groups <- rename(levels(factor(gr)))
    }
  }
  out
}

# extract names of response categories
# @param x a brmsterms object or one that can be coerced to it
# @param data user specified data
# @return a vector of category names
extract_cat_names <- function(x, data) {
  stopifnot(is.brmsformula(x) || is.brmsterms(x))
  respform <- validate_resp_formula(x$formula)
  mr <- model.response(model.frame(respform, data))
  if (has_multicol(x)) {
    mr <- as.matrix(mr)
    out <- as.character(colnames(mr))
    if (!length(out)) {
      out <- as.character(seq_cols(mr))
    }
  } else {
    out <- levels(factor(mr))
  }
  out
}

# extract names of ordinal thresholds
# @param x a brmsterms object or one that can be coerced to it
# @param data user specified data
# @return a data.frame with columns 'thres' and 'group'
extract_thres_names <- function(x, data) {
  stopifnot(is.brmsformula(x) || is.brmsterms(x), has_thres(x))
  if (is.null(x$adforms)) {
    x$adforms <- terms_ad(x$formula, x$family)
  }
  nthres <- get_ad_values(x, "thres", "thres", data)
  if (any(!is_wholenumber(nthres) | nthres < 1L)) {
    stop2("Number of thresholds must be a positive integer.")
  }
  # has an extra category that is not part of the ordinal scale? (#1429)
  extra_cat <- has_extra_cat(x$family)
  grthres <- get_ad_values(x, "thres", "gr", data)
  if (!is.null(grthres)) {
    # grouping variable was specified
    if (!is_like_factor(grthres)) {
      stop2("Variable 'gr' in 'thres' needs to be factor-like.")
    }
    grthres <- factor(grthres)
    group <- levels(grthres)
    if (!length(nthres)) {
      # extract number of thresholds from the response values
      nthres <- rep(NA, length(group))
      for (i in seq_along(group)) {
        take <- grthres %in% group[i]
        nthres[i] <- extract_nthres(
          x$formula, data[take, , drop = FALSE],
          extra_cat = extra_cat
        )
      }
    } else if (length(nthres) == 1L) {
      # replicate number of thresholds across groups
      nthres <- rep(nthres, length(group))
    } else {
      # number of thresholds is a variable in the data
      for (i in seq_along(group)) {
        # validate values of the same level
        take <- grthres %in% group[i]
        if (length(unique(nthres[take])) > 1L) {
          stop2("Number of thresholds should be unique for each group.")
        }
      }
      nthres <- get_one_value_per_group(nthres, grthres)
    }
    group <- rep(rename(group), nthres)
    thres <- ulapply(unname(nthres), seq_len)
  } else {
    # no grouping variable was specified
    group <- ""
    if (!length(nthres)) {
      # extract number of thresholds from the response values
      nthres <- extract_nthres(x$formula, data, extra_cat = extra_cat)
    }
    if (length(nthres) > 1L) {
      stop2("Number of thresholds needs to be a single value.")
    }
    thres <- seq_len(nthres)
  }
  data.frame(thres, group, stringsAsFactors = FALSE)
}

# extract number of thresholds from the response values
# @param formula with the response on the LHS
# @param data a data.frame from which to extract responses
# @param extra_cat is the first category an extra (hurdle) category?
# @return a single value for the number of thresholds
extract_nthres <- function(formula, data, extra_cat = FALSE) {
  extra_cat <- as_one_logical(extra_cat)
  respform <- validate_resp_formula(formula)
  mr <- model.response(model.frame(respform, data))
  if (is_like_factor(mr)) {
    # the first factor level is the extra category
    diff <- ifelse(extra_cat, 2L, 1L)
    out <- length(levels(factor(mr))) - diff
  } else {
    # 0 is the extra category which does not affect max
    out <- max(mr) - 1L
  }
  if (out < 1L) {
    stop2("Could not extract the number of thresholds. Use ordered factors ",
          "or positive integers as your ordinal response and ensure that ",
          "more than on response category is present.")
  }
  out
}

# update data for use in brms functions
# @param data the data passed by the user
# @param bterms object of class brmsterms
# @param na_action function defining how to treat NAs
# @param drop_unused_levels should unused factor levels be removed?
# @param attr_terms a list of attributes of the terms object of
#   the original model.frame; only used with newdata;
#   this ensures that (1) calls to 'poly' work correctly
#   and (2) that the number of variables matches the number
#   of variable names; fixes issue #73
# @param knots: a list of knot values for GAMMs
# @param data_name: optional name of the data frame as passed by the user
# @return model.frame for use in brms functions
validate_data <- function(data, bterms, data2 = list(), knots = NULL,
                          na_action = na_omit, drop_unused_levels = TRUE,
                          attr_terms = NULL, data_name = "") {
  if (missing(data)) {
    stop2("Data must be specified using the 'data' argument.")
  }
  if (is.null(knots)) {
    knots <- get_knots(data)
  }
  data <- try(as.data.frame(data), silent = TRUE)
  if (is_try_error(data)) {
    stop2("Argument 'data' must be coercible to a data.frame.")
  }
  if (!isTRUE(nrow(data) > 0L)) {
    stop2("Argument 'data' does not contain observations.")
  }
  data <- data_rsv_intercept(data, bterms = bterms)
  all_vars_formula <- bterms$allvars
  missing_vars <- setdiff(all_vars(all_vars_formula), names(data))
  if (length(missing_vars)) {
    missing_vars2 <- setdiff(missing_vars, names(data2))
    if (length(missing_vars2)) {
      stop2("The following variables can neither be found in ",
            "'data' nor in 'data2':\n", collapse_comma(missing_vars2))
    }
    # all initially missing variables can be found in 'data2'
    # they are not necessarily of the length required for 'data'
    # so need to be excluded from the evaluation of 'model.frame'
    missing_vars_formula <- paste0(". ~ . ", collapse(" - ", missing_vars))
    all_vars_formula <- update(all_vars_formula, missing_vars_formula)
  }
  all_vars_terms <- terms(all_vars_formula)
  # ensure that 'data2' comes first in the search path
  # during the evaluation of model.frame
  terms_env <- environment(all_vars_terms)
  environment(all_vars_terms) <- as.environment(as.list(data2))
  parent.env(environment(all_vars_terms)) <- terms_env
  attributes(all_vars_terms)[names(attr_terms)] <- attr_terms
  # 'terms' prevents correct validation in 'model.frame'
  attr(data, "terms") <- NULL
  # ensures that na_action can be passed to model.frame
  na_action_bterms <- function(object, ...) {
    na_action(object, bterms = bterms, ...)
  }
  data <- model.frame(
    all_vars_terms, data, na.action = na_action_bterms,
    drop.unused.levels = drop_unused_levels
  )
  if (any(grepl("__|_$", colnames(data)))) {
    stop2("Variable names may not contain double underscores ",
          "or underscores at the end.")
  }
  if (!isTRUE(nrow(data) > 0L)) {
    stop2("All observations in the data were removed ",
          "presumably because of NA values.")
  }
  groups <- get_group_vars(bterms)
  data <- combine_groups(data, groups)
  data <- fix_factor_contrasts(data, ignore = groups)
  data <- order_data(data, bterms = bterms)
  attr(data, "knots") <- knots
  attr(data, "drop_unused_levels") <- drop_unused_levels
  attr(data, "data_name") <- data_name
  data
}

# validate the 'data2' argument
# @param data2 a named list of data objects
# @param bterms object returned by 'brmsterms'
# @param ... more named list to pass objects to data2 from other sources
#   only required for backwards compatibility with deprecated arguments
# @return a validated named list of data objects
validate_data2 <- function(data2, bterms, ...) {
  # TODO: specify spline-related matrices in 'data2'
  # this requires adding another parser layer with bterms and data as input
  if (is.null(data2)) {
    data2 <- list()
  }
  if (!is.list(data2)) {
    stop2("'data2' must be a list.")
  }
  if (length(data2) && !is_named(data2)) {
    stop2("All elements of 'data2' must be named.")
  }
  dots <- list(...)
  for (i in seq_along(dots)) {
    if (length(dots[[i]])) {
      stopifnot(is.list(dots[[i]]), is_named(dots[[i]]))
      data2[names(dots[[i]])] <- dots[[i]]
    }
  }
  # validate autocorrelation matrices
  acframe <- frame_ac(bterms)
  sar_M_names <- get_ac_vars(acframe, "M", class = "sar")
  for (M in sar_M_names) {
    data2[[M]] <- validate_sar_matrix(get_from_data2(M, data2))
    attr(data2[[M]], "obs_based_matrix") <- TRUE
  }
  car_M_names <- get_ac_vars(acframe, "M", class = "car")
  for (M in car_M_names) {
    data2[[M]] <- validate_car_matrix(get_from_data2(M, data2))
    # observation based CAR matrices are deprecated and
    # there is no need to label them as observation based
  }
  fcor_M_names <- get_ac_vars(acframe, "M", class = "fcor")
  for (M in fcor_M_names) {
    data2[[M]] <- validate_fcor_matrix(get_from_data2(M, data2))
    attr(data2[[M]], "obs_based_matrix") <- TRUE
  }
  # validate within-group covariance matrices
  cov_names <- ufrom_list(get_re(bterms)$gcall, "cov")
  cov_names <- cov_names[nzchar(cov_names)]
  for (cov in cov_names) {
    data2[[cov]] <- validate_recov_matrix(get_from_data2(cov, data2))
  }
  data2
}

# get an object from the 'data2' argument
get_from_data2 <- function(x, data2) {
  if (!x %in% names(data2)) {
    stop2("Object '", x, "' was not found in 'data2'.")
  }
  get(x, data2)
}

# index observation based elements in 'data2'
# @param data2 a named list of objects
# @param i observation based indices
# @return data2 with potentially indexed elements
subset_data2 <- function(data2, i) {
  if (!length(data2)) {
    return(data2)
  }
  stopifnot(is.list(data2), is_named(data2))
  for (var in names(data2)) {
    if (isTRUE(attr(data2[[var]], "obs_based_matrix"))) {
      # matrices with dimensions equal to the number of observations
      data2[[var]] <- data2[[var]][i, i, drop = FALSE]
      attr(data2[[var]], "obs_based_matrix") <- TRUE
    }
  }
  data2
}

# add the reserved intercept variables to the data
data_rsv_intercept <- function(data, bterms) {
  fe_forms <- get_effect(bterms, "fe")
  if (any(ulapply(fe_forms, no_int))) {
    if ("intercept" %in% ulapply(fe_forms, all_vars)) {
      warning2("Reserved variable name 'intercept' is deprecated. ",
               "Please use 'Intercept' instead.")
    }
    if (any(data[["intercept"]] != 1)) {
      stop2("Variable name 'intercept' is reserved in models ",
            "without a population-level intercept.")
    }
    if (any(data[["Intercept"]] != 1)) {
      stop2("Variable name 'Intercept' is reserved in models ",
            "without a population-level intercept.")
    }
    data$intercept <- data$Intercept <- rep(1, length(data[[1]]))
  }
  data
}

# combine grouping factors to form new variables
# @param data data.frame to be updated
# @param ... the grouping factors to be combined
# @return 'data' including the new combined grouping factors
combine_groups <- function(data, ...) {
  group <- c(...)
  for (i in seq_along(group)) {
    sgroup <- unlist(strsplit(group[[i]], ":"))
    if (length(sgroup) > 1L && !group[[i]] %in% names(data)) {
      new_var <- get(sgroup[1], data)
      for (j in 2:length(sgroup)) {
        new_var <- paste0(new_var, "_", get(sgroup[j], data))
      }
      data[[group[[i]]]] <- new_var
    }
  }
  data
}

# hard code factor contrasts to be independent of the global "contrasts" option
# @param data data.frame to be updated
# @param olddata: optional data.frame from which contrasts are taken if present
# @param ignore: names of variables for which not to fix contrasts
# @return 'data' with amended contrasts attributes
fix_factor_contrasts <- function(data, olddata = NULL, ignore = NULL) {
  stopifnot(is(data, "data.frame"))
  stopifnot(is.null(olddata) || is.list(olddata))
  olddata <- as.data.frame(olddata)  # fixes issue #105
  for (i in seq_along(data)) {
    needs_contrast <- is.factor(data[[i]]) && !names(data)[i] %in% ignore
    if (needs_contrast && is.null(attr(data[[i]], "contrasts"))) {
      old_contrasts <- attr(olddata[[names(data)[i]]], "contrasts")
      if (!is.null(old_contrasts)) {
        # take contrasts from olddata
        contrasts(data[[i]]) <- old_contrasts
      } else if (length(unique(data[[i]])) > 1L) {
        # avoid error when supplying only a single level
        # hard code current global "contrasts" option
        contrasts(data[[i]]) <- contrasts(data[[i]])
      }
    }
  }
  data
}

# order data for use in time-series models
# @param data data.frame to be ordered
# @param bterms brmsterms of mvbrmsterms object
# @return 'data' potentially ordered differently
order_data <- function(data, bterms) {
  # ordering does only matter for time-series models
  time <- get_ac_vars(bterms, "time", dim = "time")
  gr <- get_ac_vars(bterms, "gr", dim = "time")
  if (length(time) > 1L || length(gr) > 1L) {
    stop2("All time-series structures must have the same ",
          "'time' and 'gr' variables.")
  }
  if (length(time) || length(gr)) {
    if (length(gr)) {
      gv <- data[[gr]]
    } else {
      gv <- rep(1L, nrow(data))
    }
    if (length(time)) {
      tv <- data[[time]]
    } else {
      tv <- seq_rows(data)
    }
    if (any(duplicated(data.frame(gv, tv)))) {
      stop2("Time points within groups must be unique.")
    }
    new_order <- do_call(order, list(gv, tv))
    data <- data[new_order, , drop = FALSE]
    # old_order will allow to retrieve the initial order of the data
    attr(data, "old_order") <- order(new_order)
  }
  data
}

# subset data according to addition argument 'subset'
subset_data <- function(data, bterms) {
  if (has_subset(bterms)) {
    # only evaluate a subset of the data
    subset <- as.logical(get_ad_values(bterms, "subset", "subset", data))
    if (length(subset) != nrow(data)) {
      stop2("Length of 'subset' does not match the rows of 'data'.")
    }
    if (anyNA(subset)) {
      stop2("Subset variables may not contain NAs.")
    }
    # cross-formula indexing is no longer trivial for subsetted models
    check_cross_formula_indexing(bterms)
    data <- data[subset, , drop = FALSE]
    attr(data, "subset") <- subset
  }
  if (!NROW(data)) {
    stop2(
      "All rows of 'data' were removed via 'subset'. ",
      "Please make sure that variables do not contain NAs ",
      "for observations in which they are supposed to be used. ",
      "Please also make sure that each subset variable is ",
      "TRUE for at least one observation."
    )
  }
  data
}

# like stats:::na.omit.data.frame but allows to certain NA values
na_omit <- function(object, bterms, ...) {
  stopifnot(is.data.frame(object))
  nobs <- nrow(object)
  if (is.mvbrmsterms(bterms)) {
    responses <- names(bterms$terms)
    subsets <- lapply(bterms$terms, get_ad_values, "subset", "subset", object)
    vars_sub <- lapply(bterms$terms, function(x) all_vars(x$allvars))
  }
  vars_keep_na <- vars_keep_na(bterms)
  omit <- logical(nobs)
  for (v in names(object)) {
    x <- object[[v]]
    vars_v <- all_vars(v)
    keep_all_na <- all(vars_v %in% vars_keep_na)
    if (!is.atomic(x) || keep_all_na) {
      next
    }
    if (!is.mvbrmsterms(bterms)) {
      # remove all NAs in this variable
      keep_na <- rep(FALSE, nobs)
    } else {
      # allow to retain NAs in subsetted variables
      keep_na <- rep(TRUE, nobs)
      for (r in responses) {
        if (any(vars_v %in% vars_sub[[r]])) {
          if (!is.null(subsets[[r]])) {
            # keep NAs ignored because of 'subset'
            keep_na <- keep_na & !subsets[[r]]
          } else {
            # remove all NAs in this variable
            keep_na <- keep_na & FALSE
          }
        }
      }
    }
    is_na <- is.na(x)
    d <- dim(is_na)
    if (is.null(d) || length(d) != 2L) {
      omit <- omit | (is_na & !keep_na)
    } else {
      for (ii in seq_len(d[2L])) {
        omit <- omit | (is_na[, ii] & !keep_na)
      }
    }
  }
  if (any(omit > 0L)) {
    out <- object[!omit, , drop = FALSE]
    temp <- setNames(seq(omit)[omit], attr(object, "row.names")[omit])
    attr(temp, "class") <- "omit"
    attr(out, "na.action") <- temp
    warning2("Rows containing NAs were excluded from the model.")
  } else {
    out <- object
  }
  out
}

# get a single value per group
# @param x vector of values to extract one value per group
# @param gr vector of grouping values
# @return a vector of the same length as unique(group)
get_one_value_per_group <- function(x, gr) {
  stopifnot(length(x) == length(gr))
  not_dupl_gr <- !duplicated(gr)
  gr_unique <- gr[not_dupl_gr]
  to_order <- order(gr_unique)
  gr_unique <- gr_unique[to_order]
  out <- x[not_dupl_gr][to_order]
  names(out) <- gr_unique
  out
}

# extract knots values for use in spline terms
get_knots <- function(data) {
  attr(data, "knots", TRUE)
}

get_drop_unused_levels <- function(data) {
  out <- attr(data, "drop_unused_levels", TRUE) %||% TRUE
}

# extract name of the data as originally passed by the user
get_data_name <- function(data) {
  out <- attr(data, "data_name", TRUE)
  if (is.null(out)) {
    out <- "NULL"
  }
  out
}

#' Validate New Data
#'
#' Validate new data passed to post-processing methods of \pkg{brms}. Unless you
#' are a package developer, you will rarely need to call \code{validate_newdata}
#' directly.
#'
#' @inheritParams prepare_predictions
#' @param newdata A \code{data.frame} containing new data to be validated.
#' @param object A \code{brmsfit} object.
#' @param check_response Logical; Indicates if response variables should
#'   be checked as well. Defaults to \code{TRUE}.
#' @param group_vars Optional names of grouping variables to be validated.
#'   Defaults to all grouping variables in the model.
#' @param req_vars Optional names of variables required in \code{newdata}.
#'   If \code{NULL} (the default), all variables in the original data
#'   are required (unless ignored for some other reason).
#' @param ... Currently ignored.
#'
#' @return A validated \code{'data.frame'} based on \code{newdata}.
#'
#' @export
validate_newdata <- function(
    newdata, object, re_formula = NULL, allow_new_levels = FALSE,
    newdata2 = NULL, resp = NULL, check_response = TRUE,
    incl_autocor = TRUE, group_vars = NULL, req_vars = NULL, ...
) {
  newdata <- try(as.data.frame(newdata), silent = TRUE)
  if (is_try_error(newdata)) {
    stop2("Argument 'newdata' must be coercible to a data.frame.")
  }
  object <- restructure(object)
  object <- exclude_terms(object, incl_autocor = incl_autocor)
  resp <- validate_resp(resp, object)
  new_formula <- update_re_terms(formula(object), re_formula)
  bterms <- brmsterms(new_formula, resp_rhs_all = FALSE)

  # fill values of not required variables
  all_vars <- all.vars(bterms$allvars)
  if (is.null(req_vars)) {
    req_vars <- all_vars
  } else {
    req_vars <- as.character(req_vars)
    req_vars <- intersect(req_vars, all_vars)
  }
  if (is.mvbrmsterms(bterms) && !is.null(resp)) {
    # variables not used in the included model parts
    # do not need to be specified in newdata
    resp <- validate_resp(resp, bterms$responses)
    form_req_vars <- from_list(bterms$terms[resp], "allvars")
    form_req_vars <- allvars_formula(form_req_vars)
    req_vars <- intersect(req_vars, all.vars(form_req_vars))
  }
  not_req_vars <- setdiff(all_vars, req_vars)
  not_req_vars <- setdiff(not_req_vars, names(newdata))
  newdata <- fill_newdata(newdata, not_req_vars, object$data)
  # check response and addition variables
  only_resp <- all.vars(bterms$respform)
  only_resp <- setdiff(only_resp, all.vars(rhs(bterms$allvars)))
  # always require 'dec' variables to be specified
  dec_vars <- get_ad_vars(bterms, "dec")
  missing_resp <- setdiff(c(only_resp, dec_vars), names(newdata))
  if (length(missing_resp)) {
    if (check_response) {
      stop2("Response variables must be specified in 'newdata'.\n",
            "Missing variables: ", collapse_comma(missing_resp))
    } else {
      newdata <- fill_newdata(newdata, missing_resp)
    }
  }
  # censoring and weighting vars are unused in post-processing methods
  cens_vars <- get_ad_vars(bterms, "cens")
  for (v in setdiff(cens_vars, names(newdata))) {
    newdata[[v]] <- 0
  }
  weights_vars <- get_ad_vars(bterms, "weights")
  for (v in setdiff(weights_vars, names(newdata))) {
    newdata[[v]] <- 1
  }
  mf <- model.frame(object)
  for (i in seq_along(mf)) {
    if (is_like_factor(mf[[i]])) {
      mf[[i]] <- as.factor(mf[[i]])
    }
  }
  pw_vars <- ufrom_list(get_re(bterms)$gcall, "pw")
  for (v in setdiff(pw_vars, names(newdata))) {
    newdata[[v]] <- 1
  }
  # fixes issue #279
  newdata <- data_rsv_intercept(newdata, bterms)
  new_group_vars <- get_group_vars(bterms)
  if (allow_new_levels && length(new_group_vars)) {
    # grouping factors do not need to be specified
    # by the user if new levels are allowed
    mis_group_vars <- new_group_vars[!grepl(":", new_group_vars)]
    mis_group_vars <- setdiff(mis_group_vars, names(newdata))
    newdata <- fill_newdata(newdata, mis_group_vars)
  }
  newdata <- combine_groups(newdata, new_group_vars)
  # validate factor levels in newdata
  if (is.null(group_vars)) {
    group_vars <- get_group_vars(object)
  }
  do_check <- union(get_pred_vars(bterms), get_int_vars(bterms))
  # do not check variables from the 'unused' argument #1238
  unused_arg_vars <- get_unused_arg_vars(bterms)
  dont_check <- unique(c(group_vars, cens_vars, unused_arg_vars))
  dont_check <- setdiff(dont_check, do_check)
  dont_check <- names(mf) %in% dont_check
  is_factor <- ulapply(mf, is.factor)
  factors <- mf[is_factor & !dont_check]
  if (length(factors)) {
    factor_names <- names(factors)
    for (i in seq_along(factors)) {
      new_factor <- newdata[[factor_names[i]]]
      if (!is.null(new_factor)) {
        if (!is.factor(new_factor)) {
          new_factor <- factor(new_factor)
        }
        old_levels <- levels(factors[[i]])
        if (length(old_levels) <= 1L) {
          # contrasts are not defined for factors with 1 or fewer levels
          next
        }
        new_levels <- levels(new_factor)
        old_contrasts <- contrasts(factors[[i]])
        old_ordered <- is.ordered(factors[[i]])
        to_zero <- is.na(new_factor) | new_factor %in% "zero__"
        # don't add the 'zero__' level to response variables
        is_resp <- factor_names[i] %in% all.vars(bterms$respform)
        if (!is_resp && any(to_zero)) {
          levels(new_factor) <- c(new_levels, "zero__")
          new_factor[to_zero] <- "zero__"
          old_levels <- c(old_levels, "zero__")
          old_contrasts <- rbind(old_contrasts, zero__ = 0)
        }
        if (any(!new_levels %in% old_levels)) {
          stop2(
            "New factor levels are not allowed.",
            "\nLevels allowed: ", collapse_comma(old_levels),
            "\nLevels found: ", collapse_comma(new_levels)
          )
        }
        newdata[[factor_names[i]]] <-
          factor(new_factor, old_levels, ordered = old_ordered)
        # don't use contrasts(.) here to avoid dimension checks
        attr(newdata[[factor_names[i]]], "contrasts") <- old_contrasts
      }
    }
  }
  # check if originally numeric variables are still numeric
  num_names <- names(mf)[!is_factor]
  num_names <- setdiff(num_names, group_vars)
  for (nm in intersect(num_names, names(newdata))) {
    if (!anyNA(newdata[[nm]]) && !is.numeric(newdata[[nm]])) {
      stop2("Variable '", nm, "' was originally ",
            "numeric but is not in 'newdata'.")
    }
  }
  # validate monotonic variables
  mo_vars <- get_sp_vars(bterms, "mo")
  if (length(mo_vars)) {
    # factors have already been checked
    num_mo_vars <- names(mf)[!is_factor & names(mf) %in% mo_vars]
    for (v in num_mo_vars) {
      new_values <- get(v, newdata)
      min_value <- min(mf[[v]])
      invalid <- new_values < min_value | new_values > max(mf[[v]])
      invalid <- invalid | !is_wholenumber(new_values)
      if (sum(invalid)) {
        stop2("Invalid values in variable '", v, "': ",
              collapse_comma(new_values[invalid]))
      }
      attr(newdata[[v]], "min") <- min_value
    }
  }
  # update_data expects all original variables to be present
  used_vars <- c(names(newdata), all.vars(bterms$allvars))
  used_vars <- union(used_vars, rsv_vars(bterms))
  all_vars <- all.vars(str2formula(names(mf)))
  unused_vars <- setdiff(all_vars, used_vars)
  newdata <- fill_newdata(newdata, unused_vars)
  # validate grouping factors
  old_levels <- get_levels(bterms, data = mf)
  if (!allow_new_levels) {
    new_levels <- get_levels(bterms, data = newdata)
    for (g in names(old_levels)) {
      unknown_levels <- setdiff(new_levels[[g]], old_levels[[g]])
      # NA is not found by get_levels but still behaves like a new level (#1652)
      if (anyNA(newdata[[g]])) {
        c(unknown_levels) <- NA
      }
      if (length(unknown_levels)) {
        unknown_levels <- collapse_comma(unknown_levels)
        stop2(
          "Levels ", unknown_levels, " of grouping factor '", g, "' ",
          "cannot be found in the fitted model. ",
          "Consider setting argument 'allow_new_levels' to TRUE."
        )
      }
    }
  }
  # ensure correct handling of functions like 'poly' or 'scale'
  old_terms <- attr(object$data, "terms")
  attr_terms <- c("variables", "predvars")
  attr_terms <- attributes(old_terms)[attr_terms]
  newdata <- validate_data(
    newdata, bterms = bterms, na_action = na.pass,
    drop_unused_levels = FALSE, attr_terms = attr_terms,
    data2 = current_data2(object, newdata2),
    knots = get_knots(object$data)
  )
  newdata
}

# fill newdata with values for not required variables
# @param newdata data.frame to be filled
# @param vars character vector of not required variables
# @param olddata optional data.frame to take values from
# @param n row number of olddata to extract values from
fill_newdata <- function(newdata, vars, olddata = NULL, n = 1L) {
  stopifnot(is.data.frame(newdata), is.character(vars))
  vars <- setdiff(vars, names(newdata))
  if (is.null(olddata)) {
    if (length(vars)) {
      newdata[, vars] <- NA
    }
    return(newdata)
  }
  stopifnot(is.data.frame(olddata), length(n) == 1L)
  for (v in vars) {
    # using NA for variables is not safe in all cases
    # for example when processing splines using mgcv
    # hence it is safer to use existing data values
    cval <- olddata[n, v] %||% NA
    if (length(dim(cval)) == 2L) {
      # matrix columns don't have automatic broadcasting apparently
      cval <- matrix(cval, nrow(newdata), ncol(cval), byrow = TRUE)
    }
    newdata[[v]] <- cval
  }
  newdata
}

# validate new data2
validate_newdata2 <- function(newdata2, object, ...) {
  stopifnot(is.brmsfit(object))
  bterms <- brmsterms(object$formula)
  validate_data2(newdata2, bterms = bterms, ...)
}

# extract the current data
current_data <- function(object, newdata = NULL, skip_validation = FALSE, ...) {
  stopifnot(is.brmsfit(object))
  skip_validation <- as_one_logical(skip_validation)
  if (is.null(newdata)) {
    data <- object$data
  } else if (skip_validation) {
    data <- newdata
  } else {
    data <- validate_newdata(newdata, object = object, ...)
  }
  data
}

# extract the current data2
current_data2 <- function(object, newdata2 = NULL, skip_validation = FALSE, ...) {
  stopifnot(is.brmsfit(object))
  skip_validation <- as_one_logical(skip_validation)
  if (is.null(newdata2)) {
    data2 <- object$data2
  } else if (skip_validation) {
    data2 <- newdata2
  } else {
    data2 <- validate_newdata2(newdata2, object = object, ...)
  }
  data2
}
