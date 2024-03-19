#' Interpret the formula specified to mvgam and replace any dynamic terms
#' with the correct Gaussian Process smooth specification
#' @importFrom stats formula terms as.formula terms.formula
#' @noRd
interpret_mvgam = function(formula, N, family){

  # Check for proper binomial specification
  if(!missing(family)){
    if(is.character(family)){
      if(family == 'beta')
        family <- betar()

      family <- try(eval(parse(text = family)), silent = TRUE)

      if(inherits(family, 'try-error'))
        stop("family not recognized",
             call. = FALSE)
    }

    if(is.function(family))
      family <- family()

    if(family$family %in% c('binomial', 'beta_binomial')){
      # Check that response terms use the cbind() syntax
      resp_terms <- as.character(terms(formula(formula))[[2]])
      if(length(resp_terms) == 1){
        stop('Binomial family requires cbind() syntax in the formula left-hand side',
             call. = FALSE)
      } else {
        if(any(grepl('cbind', resp_terms))){
        } else {
          stop('Binomial family requires cbind() syntax in the formula left-hand side',
               call. = FALSE)
        }
      }
    }
  }

  facs <- colnames(attr(terms.formula(formula), 'factors'))

  # Check if formula has an intercept
  keep_intercept <- attr(terms(formula), 'intercept') == 1

  # Re-arrange so that random effects always come last
  if(any(grepl('bs = \"re\"', facs, fixed = TRUE))){
    newfacs <- facs[!grepl('bs = \"re\"', facs, fixed = TRUE)]
    refacs <- facs[grepl('bs = \"re\"', facs, fixed = TRUE)]
    int <- attr(terms.formula(formula), 'intercept')

    # Preserve offset if included
    if(!is.null(attr(terms(formula(formula)), 'offset'))){
      newformula <- as.formula(paste(dimnames(attr(terms(formula), 'factors'))[[1]][1],
                                     '~',
                                     grep('offset', rownames(attr(terms.formula(formula), 'factors')),
                                          value = TRUE),
                                     '+',
                                     paste(paste(newfacs, collapse = '+'),
                                           '+',
                                           paste(refacs, collapse = '+'),
                                           collapse = '+'),
                                     ifelse(int == 0, ' - 1', '')))

    } else {
      newformula <- as.formula(paste(dimnames(attr(terms(formula), 'factors'))[[1]][1],
                                     '~',
                                     paste(paste(newfacs, collapse = '+'),
                                           '+',
                                           paste(refacs, collapse = '+'),
                                           collapse = '+'),
                                     ifelse(int == 0, ' - 1', '')))
    }

  } else {
    newformula <- formula
  }

  attr(newformula, '.Environment') <- attr(formula, '.Environment')

  # Check if any terms use the gp wrapper, as mvgam cannot handle
  # multivariate GPs yet
  response <- terms.formula(newformula)[[2]]
  tf <- terms.formula(newformula, specials = c("gp"))
  which_gp <- attr(tf,"specials")$gp
  if(length(which_gp) != 0L){
    gp_details <- vector(length = length(which_gp),
                         mode = 'list')
    for(i in seq_along(which_gp)){
      gp_details[[i]] <- eval(parse(text = rownames(attr(tf,
                                                         "factors"))[which_gp[i]]))
    }
    if(any(unlist(lapply(purrr::map(gp_details, 'term'), length)) > 1)){
      stop('mvgam cannot yet handle multidimensional gps',
           call. = FALSE)
    }
  }

  # Check if any terms use the dynamic wrapper
  response <- terms.formula(newformula)[[2]]
  tf <- attr(terms.formula(newformula, keep.order = TRUE),
             'term.labels')
  which_dynamics <- grep('dynamic(', tf, fixed = TRUE)

  # Update the formula to the correct Gaussian Process implementation
  if(length(which_dynamics) != 0L){
    dyn_details <- vector(length = length(which_dynamics),
                          mode = 'list')
    if(length(which_dynamics > 1)){
      for(i in seq_along(which_dynamics)){
        dyn_details[[i]] <- eval(parse(text = tf[which_dynamics[i]]))
      }
    }

    # k is set based on the number of timepoints available; want to ensure
    # it is large enough to capture the expected wiggliness of the latent GP
    # (a smaller rho will require more basis functions for accurate approximation)
    dyn_to_gpspline = function(term, N){

      if(term$rho > N - 1){
        stop('Argument "rho" in dynamic() cannot be larger than (max(time) - 1)',
             call. = FALSE)
      }

      k <- term$k
      if(is.null(k)){
        if(N > 8){
          k <- min(50, min(N, max(8, ceiling(N / (term$rho - (term$rho / 10))))))
        } else {
          k <- N
        }
      }

      paste0("s(time,by=", term$term,
             ",bs='gp',m=c(",
             ifelse(term$stationary, '-', ''),"2,",
             term$rho, ",2),",
             "k=", k, ")")
    }

    dyn_to_gphilbert = function(term, N){

      k <- term$k
      if(is.null(k)){
        if(N > 8){
          k <- min(40, min(N - 1, max(8, N - 1)))
        } else {
          k <- N - 1
        }
      }

      paste0("gp(time,by=", term$term,
             ",c=5/4,",
             "k=", k, ",scale=",
             term$scale,
             ")")
    }
    # Replace dynamic terms with the correct specification
    termlabs <- attr(terms(newformula, keep.order = TRUE), 'term.labels')
    for(i in seq_along(which_dynamics)){
      if(is.null(dyn_details[[i]]$rho)){
        termlabs[which_dynamics[i]] <- dyn_to_gphilbert(dyn_details[[i]], N = N)
      } else {
        termlabs[which_dynamics[i]] <- dyn_to_gpspline(dyn_details[[i]], N = N)
      }
    }

    # Return the updated formula for passing to mgcv
    updated_formula <- reformulate(termlabs, rlang::f_lhs(newformula))
    attr(updated_formula, '.Environment') <- attr(newformula, '.Environment')

  } else {
    updated_formula <- newformula
  }

  if(!keep_intercept){
    updated_formula <- update(updated_formula, . ~ . - 1)
    attr(updated_formula, '.Environment') <- attr(newformula, '.Environment')
  }

  return(updated_formula)
}
