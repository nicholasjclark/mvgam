#'Extract information on default prior distributions for an \pkg{mvgam} model
#'
#'This function lists the parameters that can have their prior distributions
#'changed for a given model, as well listing their default distributions
#'
#'@inheritParams mvgam
#'@inheritParams jsdgam
#'@param ... Not currently used
#'@param factor_formula Can be supplied instead `trend_formula` to match syntax from
#'[jsdgam]
#'@details Users can supply a model formula, prior to fitting the model, so that default priors can be inspected and
#'altered. To make alterations, change the contents of the `prior` column and supplying this
#'\code{data.frame} to the \code{\link{mvgam}} or \code{\link{jsdgam}} functions using the argument `priors`. If using `Stan` as the backend,
#'users can also modify the parameter bounds by modifying the `new_lowerbound` and/or `new_upperbound` columns.
#'This will be necessary if using restrictive distributions on some parameters, such as a Beta distribution
#'for the trend sd parameters for example (Beta only has support on  \code{(0,1)}), so the upperbound cannot
#'be above `1`. Another option is to make use of the prior modification functions in \pkg{brms}
#'(i.e. \code{\link[brms]{prior}}) to change prior distributions and bounds (just use the name of the parameter that
#'you'd like to change as the `class` argument; see examples below)
#' @note Only the `prior`, `new_lowerbound` and/or `new_upperbound` columns of the output
#' should be altered when defining the user-defined priors for the model. Use only if you are
#' familiar with the underlying probabilistic programming language. There are no sanity checks done to
#' ensure that the code is legal (i.e. to check that lower bounds are smaller than upper bounds, for
#' example)
#'@author Nicholas J Clark
#'@seealso \code{\link{mvgam}}, \code{\link{mvgam_formulae}}, \code{\link[brms]{prior}}
#'@return either a \code{data.frame} containing the prior definitions (if any suitable
#'priors can be altered by the user) or \code{NULL}, indicating that no priors in the model
#'can be modified
#'
#'@examples
#'\donttest{
#'# Simulate three integer-valued time series
#'library(mvgam)
#'dat <- sim_mvgam(trend_rel = 0.5)
#'
#'# Get a model file that uses default mvgam priors for inspection (not always necessary,
#'# but this can be useful for testing whether your updated priors are written correctly)
#'mod_default <- mvgam(y ~ s(series, bs = 're') +
#'               s(season, bs = 'cc') - 1,
#'               family = nb(),
#'               data = dat$data_train,
#'               trend_model = AR(p = 2),
#'               run_model = FALSE)
#'
#'# Inspect the model file with default mvgam priors
#'stancode(mod_default)
#'
#'# Look at which priors can be updated in mvgam
#'test_priors <- get_mvgam_priors(y ~ s(series, bs = 're') +
#'                               s(season, bs = 'cc') - 1,
#'                               family = nb(),
#'                               data = dat$data_train,
#'                               trend_model = AR(p = 2))
#'test_priors
#'
#'# Make a few changes; first, change the population mean for the series-level
#'# random intercepts
#'test_priors$prior[2] <- 'mu_raw ~ normal(0.2, 0.5);'
#'
#'# Now use stronger regularisation for the series-level AR2 coefficients
#'test_priors$prior[5] <- 'ar2 ~ normal(0, 0.25);'
#'
#'# Check that the changes are made to the model file without any warnings by
#'# setting 'run_model = FALSE'
#'mod <- mvgam(y ~ s(series, bs = 're') +
#'             s(season, bs = 'cc') - 1,
#'             family = nb(),
#'             data = dat$data_train,
#'             trend_model = AR(p = 2),
#'             priors = test_priors,
#'             run_model = FALSE)
#'stancode(mod)
#'
#'# No warnings, the model is ready for fitting now in the usual way with the addition
#'# of the 'priors' argument
#'
#'# The same can be done using 'brms' functions; here we will also change the ar1 prior
#'# and put some bounds on the ar coefficients to enforce stationarity; we set the
#'# prior using the 'class' argument in all brms prior functions
#'brmsprior <- c(prior(normal(0.2, 0.5), class = mu_raw),
#'               prior(normal(0, 0.25), class = ar1, lb = -1, ub = 1),
#'               prior(normal(0, 0.25), class = ar2, lb = -1, ub = 1))
#'brmsprior
#'
#'mod <- mvgam(y ~ s(series, bs = 're') +
#'              s(season, bs = 'cc') - 1,
#'            family = nb(),
#'            data = dat$data_train,
#'            trend_model = AR(p = 2),
#'            priors = brmsprior,
#'            run_model = FALSE)
#'stancode(mod)
#'
#'# Look at what is returned when an incorrect spelling is used
#'test_priors$prior[5] <- 'ar2_bananas ~ normal(0, 0.25);'
#'mod <- mvgam(y ~ s(series, bs = 're') +
#'              s(season, bs = 'cc') - 1,
#'             family = nb(),
#'             data = dat$data_train,
#'             trend_model = AR(p = 2),
#'             priors = test_priors,
#'             run_model = FALSE)
#'stancode(mod)
#'
#'# Example of changing parametric (fixed effect) priors
#'simdat <- sim_mvgam()
#'
#'# Add a fake covariate
#'simdat$data_train$cov <- rnorm(NROW(simdat$data_train))
#'
#'priors <- get_mvgam_priors(y ~ cov + s(season),
#'                           data = simdat$data_train,
#'                           family = poisson(),
#'                           trend_model = AR())
#'
#'# Change priors for the intercept and fake covariate effects
#'priors$prior[1] <- '(Intercept) ~ normal(0, 1);'
#'priors$prior[2] <- 'cov ~ normal(0, 0.1);'
#'
#'mod2 <- mvgam(y ~ cov + s(season),
#'              data = simdat$data_train,
#'              trend_model = AR(),
#'              family = poisson(),
#'              priors = priors,
#'              run_model = FALSE)
#'stancode(mod2)
#'
#'# Likewise using 'brms' utilities (note that you can use
#'# Intercept rather than `(Intercept)`) to change priors on the intercept
#'brmsprior <- c(prior(normal(0.2, 0.5), class = cov),
#'               prior(normal(0, 0.25), class = Intercept))
#'brmsprior
#'
#'mod2 <- mvgam(y ~ cov + s(season),
#'              data = simdat$data_train,
#'              trend_model = AR(),
#'              family = poisson(),
#'              priors = brmsprior,
#'              run_model = FALSE)
#'stancode(mod2)
#'
#'# The "class = 'b'" shortcut can be used to put the same prior on all
#'# 'fixed' effect coefficients (apart from any intercepts)
#'set.seed(0)
#'dat <- mgcv::gamSim(1, n = 200, scale = 2)
#'dat$time <- 1:NROW(dat)
#'mod <- mvgam(y ~ x0 + x1 + s(x2) + s(x3),
#'             priors = prior(normal(0, 0.75), class = 'b'),
#'             data = dat,
#'             family = gaussian(),
#'             run_model = FALSE)
#'stancode(mod)
#'}
#'@export
get_mvgam_priors = function(
  formula,
  trend_formula,
  factor_formula,
  knots,
  trend_knots,
  trend_model = 'None',
  family = poisson(),
  data,
  unit = time,
  species = series,
  use_lv = FALSE,
  n_lv,
  trend_map,
  ...
) {
  # Validate the data
  dots <- list(...)
  if (missing("data")) {
    if ('data_train' %in% names(dots)) {
      message('argument "data_train" is deprecated; supply as "data" instead')
      data <- dots$data_train
      dots$data_train <- NULL
    } else {
      stop('Argument "data" is missing with no default', call. = FALSE)
    }
  }
  if (!missing("data")) {
    data_train <- data
  }
  orig_data <- data_train

  # Set trend_formula
  if (!missing(factor_formula)) {
    if (missing(n_lv)) {
      n_lv <- 2
    }
    validate_pos_integer(n_lv)
    unit <- deparse0(substitute(unit))
    subgr <- deparse0(substitute(species))
    prepped_trend <- prep_jsdgam_trend(unit = unit, subgr = subgr, data = data)
    trend_model <- 'None'
    data_train <- validate_series_time(data = data, trend_model = prepped_trend)
    trend_map <- prep_jsdgam_trendmap(data_train, n_lv)
    if (!missing(trend_formula)) {
      warning(
        'Both "trend_formula" and "factor_formula" supplied\nUsing "factor_formula" as default'
      )
    }
    trend_formula <- factor_formula
  }

  # Validate the trend arguments
  if ('drift' %in% names(dots)) {
    message(
      'The "drift" argument is deprecated; use fixed effects of "time" instead'
    )
    dots$drift <- NULL
  }
  drift <- FALSE
  orig_trend_model <- trend_model
  trend_model <- validate_trend_model(
    orig_trend_model,
    drift = drift,
    noncentred = FALSE,
    warn = FALSE
  )

  # Ensure series and time variables are present
  data_train <- validate_series_time(
    data_train,
    name = 'data',
    trend_model = orig_trend_model
  )

  # Validate the formula to convert any dynamic() terms
  formula <- interpret_mvgam(formula, N = max(data_train$time), family = family)

  # Check for gp terms in the validated formula
  list2env(
    check_gp_terms(formula, data_train, family = family),
    envir = environment()
  )

  # Check for missing rhs in formula
  list2env(check_obs_intercept(formula, orig_formula), envir = environment())

  # Validate observation formula
  formula <- interpret_mvgam(formula, N = max(data_train$time))
  data_train <- validate_obs_formula(formula, data = data_train, refit = FALSE)

  # Validate the family argument
  use_stan <- TRUE
  family <- validate_family(family, use_stan = use_stan)
  family_char <- match.arg(arg = family$family, choices = family_char_choices())

  # Nmixture additions?
  list2env(
    check_nmix(
      family,
      family_char,
      trend_formula,
      trend_model,
      trend_map,
      data_train,
      priors = TRUE
    ),
    envir = environment()
  )

  # Validate remaining trend arguments
  trend_val <- validate_trend_restrictions(
    trend_model = trend_model,
    formula = formula,
    trend_formula = trend_formula,
    trend_map = trend_map,
    drift = drift,
    drop_obs_intercept = drop_obs_intercept,
    use_lv = use_lv,
    n_lv = n_lv,
    data_train = data_train,
    use_stan = use_stan,
    priors = TRUE
  )
  list2env(trend_val, envir = environment())
  if (is.null(trend_map)) trend_map <- rlang::missing_arg()
  if (is.null(n_lv)) n_lv <- rlang::missing_arg()

  # If trend_formula supplied, first run get_mvgam_priors for the observation model
  # and then modify the resulting output
  if (!missing(trend_formula)) {
    if (trend_model == 'None') trend_model <- 'RW'
    validate_trend_formula(trend_formula)
    prior_df <- get_mvgam_priors(
      formula = orig_formula,
      data = data_train,
      family = family,
      use_lv = FALSE,
      trend_model = if (trend_model == 'None') {
        RW()
      } else {
        orig_trend_model
      },
      trend_map = trend_map,
      knots = knots
    )

    # Replace any terms labelled 'trend' with 'series' for creating the necessary
    # structures
    trend_formula <- formula(paste(
      gsub('trend', 'series', as.character(trend_formula), fixed = TRUE),
      collapse = " "
    ))

    # Drop any intercept from the formula if this is not an N-mixture model or if a
    # trend_map was not originally supplied
    if (family_char == 'nmix') drop_trend_int <- FALSE else
      drop_trend_int <- TRUE
    if (!missing(trend_map)) drop_trend_int <- FALSE
    if (drop_trend_int) {
      if (attr(terms(trend_formula), 'intercept') == 1) {
        trend_formula <- update(trend_formula, trend_y ~ . - 1)
      } else {
        trend_formula <- update(trend_formula, trend_y ~ .)
      }
    } else {
      trend_formula <- update(trend_formula, trend_y ~ .)
    }

    trend_train <- data_train
    trend_train$time <- trend_train$index..time..index
    trend_train$trend_y <- rnorm(length(trend_train$time))

    # Add indicators of trend names as factor levels using the trend_map
    trend_indicators <- vector(length = length(trend_train$time))
    for (i in 1:length(trend_train$time)) {
      trend_indicators[i] <- trend_map$trend[which(
        trend_map$series == trend_train$series[i]
      )]
    }
    trend_indicators <- as.factor(paste0('trend', trend_indicators))
    trend_train$series <- trend_indicators
    trend_train$y <- NULL

    # Only keep one time observation per trend
    data.frame(
      series = trend_train$series,
      time = trend_train$time,
      row_num = 1:length(trend_train$time)
    ) %>%
      dplyr::group_by(series, time) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(row_num) -> inds_keep

    if (inherits(trend_train, 'list')) {
      trend_train <- lapply(trend_train, function(x) {
        if (is.matrix(x)) {
          matrix(x[inds_keep, ], ncol = NCOL(x))
        } else {
          x[inds_keep]
        }
      })
    } else {
      trend_train <- trend_train[inds_keep, ]
    }

    # Now get the priors related to the trend model
    trend_prior_df <- get_mvgam_priors(
      trend_formula,
      data = trend_train,
      family = gaussian(),
      trend_model = 'None',
      knots = trend_knots
    )

    # Modify some of the term names and return
    if (any(grepl('fixed effect', trend_prior_df$param_info))) {
      para_lines <- grep('fixed effect', trend_prior_df$param_info)
      for (i in para_lines) {
        trend_prior_df$param_name[i] <- paste0(
          trend_prior_df$param_name[i],
          '_trend'
        )
        trend_prior_df$prior[i] <- paste0(
          trimws(strsplit(trend_prior_df$prior[i], "[~]")[[1]][1]),
          '_trend ~ student_t(3, 0, 2);'
        )
        trend_prior_df$example_change[i] <- paste0(
          trimws(strsplit(trend_prior_df$example_change[i], "[~]")[[1]][1]),
          '_trend ~ normal(0, 1);'
        )
      }
    }

    if (any(grepl('(Intercept)', trend_prior_df$param_info))) {
      para_lines <- grep('(Intercept)', trend_prior_df$param_info)
      for (i in para_lines) {
        trend_prior_df$param_name[i] <- paste0(
          trend_prior_df$param_name[i],
          '_trend'
        )
        trend_prior_df$prior[i] <- paste0(
          trimws(strsplit(trend_prior_df$prior[i], "[~]")[[1]][1]),
          '_trend ~ student_t(3, 0, 2);'
        )
        trend_prior_df$example_change[i] <- paste0(
          trimws(strsplit(trend_prior_df$example_change[i], "[~]")[[1]][1]),
          '_trend ~ normal(0, 1);'
        )
        trend_prior_df$param_info[i] <- '(Intercept) for the trend'
      }
    }

    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("lambda", "lambda_trend", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("n_sp", "n_sp_trend", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("mu_raw", "mu_raw_trend", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("sigma_raw", "sigma_raw_trend", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("n_series", "n_lv", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("series", "trend", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("alpha_gp", "alpha_gp_trend", x)
    )
    trend_prior_df[] <- lapply(
      trend_prior_df,
      function(x) gsub("rho_gp", "rho_gp_trend", x)
    )
    trend_prior_df <- trend_prior_df[
      !trend_prior_df$param_info == 'observation error sd',
    ]
    out <- rbind(prior_df, trend_prior_df)
    out[] <- lapply(out, function(x) gsub("trend sd", "process error sd", x))
    out[] <- lapply(out, function(x) gsub("trend AR1", "process model AR1", x))
    out[] <- lapply(out, function(x) gsub("trend AR2", "process model AR2", x))
    out[] <- lapply(out, function(x) gsub("trend AR3", "process model AR3", x))
    out[] <- lapply(
      out,
      function(x) gsub("trend drift", "process model drift", x)
    )
    out[] <- lapply(
      out,
      function(x)
        gsub(
          "vector<lower=0>[n_series] sigma;",
          "vector<lower=0>[n_lv] sigma;",
          x,
          fixed = TRUE
        )
    )

    # Remove intercept prior if an intercept was suppressed from the
    # observation model
    if (drop_obs_intercept) {
      if (any(grepl('Intercept', out$param_name))) {
        which_obs_int <- grep('Intercept', out$param_name) &
          !grep('(Intercept)_trend', out$param_name)
        if (length(which_obs_int) > 0L) out <- out[-which_obs_int, ]
      }
    }

    # Remove sigma prior if this is an N-mixture with no dynamics
    if (add_nmix & trend_model == 'None') {
      out <- out[
        -grep('vector<lower=0>[n_lv] sigma;', out$param_name, fixed = TRUE),
      ]
    }
  } else {
    # JAGS cannot support latent GP, VAR or piecewise trends
    if (
      !use_stan & trend_model %in% c('GP', 'VAR1', 'PWlinear', 'PWlogistic')
    ) {
      stop(
        'Gaussian Process, VAR and piecewise trends not supported for JAGS',
        call. = FALSE
      )
    }

    if (use_stan & family_char == 'tweedie') {
      warning('Tweedie family not supported for Stan; reverting to JAGS')
      use_stan <- FALSE
    }

    # Number of latent variables cannot be greater than number of series
    if (use_lv) {
      if (missing(n_lv)) {
        n_lv <- min(2, floor(length(unique(data_train$series)) / 2))
      }
      if (n_lv > length(unique(data_train$series))) {
        stop(
          'number of latent variables cannot be greater than number of series',
          call. = FALSE
        )
      }
    }

    # # No point in latent variables if trend model is None
    # if(trend_model == 'None' & use_lv){
    #   use_lv <- FALSE
    #   warning('No point in latent variables if trend model is None; changing use_lv to FALSE')
    # }

    # Fill in missing observations in data_train so the size of the dataset is correct when
    # building the initial JAGS model
    resp_terms <- as.character(terms(formula(formula))[[2]])
    if (length(resp_terms) == 1) {
      out_name <- as.character(terms(formula(formula))[[2]])
    } else {
      if (any(grepl('cbind', resp_terms))) {
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        out_name <- resp_terms[1]
      }
    }
    data_train[[out_name]] <- replace_nas(data_train[[out_name]])

    # Some general family-level restrictions can now be checked
    validate_family_restrictions(
      response = data_train[[out_name]],
      family = family
    )

    # Use a small fit from mgcv to extract relevant information on smooths included
    # in the model
    ss_gam <- try(
      mvgam_setup(
        formula = formula,
        family = family_to_mgcvfam(family),
        dat = data_train,
        knots = knots
      ),
      silent = TRUE
    )
    if (inherits(ss_gam, 'try-error')) {
      if (grepl('missing values', ss_gam[1])) {
        stop(
          paste(
            'Missing values found in data predictors:\n',
            attr(ss_gam, 'condition')
          ),
          call. = FALSE
        )
      } else {
        stop(paste(ss_gam[1]), call. = FALSE)
      }
    }

    # Parametric effect priors
    if (use_stan) {
      smooth_labs <- do.call(
        rbind,
        lapply(seq_along(ss_gam$smooth), function(x) {
          data.frame(
            label = ss_gam$smooth[[x]]$label,
            term = paste(ss_gam$smooth[[x]]$term, collapse = ','),
            class = class(ss_gam$smooth[[x]])[1]
          )
        })
      )
      lpmat <- suppressWarnings(predict(
        ss_gam,
        type = 'lpmatrix',
        exclude = smooth_labs$label
      ))
      para_indices <- which(apply(lpmat, 2, function(x) !all(x == 0)) == TRUE)

      int_included <- attr(ss_gam$pterms, 'intercept') == 1L
      if (int_included) {
        other_pterms <- names(para_indices)[-1]
      } else {
        other_pterms <- names(para_indices)
      }
      all_paras <- other_pterms

      para_priors <- c()
      para_info <- c()

      if (length(other_pterms) > 0) {
        para_priors <- c(
          para_priors,
          paste(other_pterms, '~ student_t(3, 0, 2);')
        )
        para_info <- c(para_info, paste(other_pterms, 'fixed effect'))
      }

      if (int_included) {
        all_paras <- c('(Intercept)', all_paras)
        # Compute default intercept prior using brms
        def_int <- make_default_int(response = data_train$y, family = family)
        para_priors <- c(
          paste0(def_int$class, ' ~ ', def_int$prior, ';'),
          para_priors
        )
        para_info <- c('(Intercept)', para_info)
      }

      if (length(all_paras) == 0) {
        para_df <- NULL
      } else {
        para_df <- data.frame(
          param_name = all_paras,
          param_length = 1,
          param_info = para_info,
          prior = para_priors,
          example_change = c(
            paste0(all_paras, ' ~ normal(0, 1);')
          )
        )
      }
    } else {
      para_df <- NULL
    }

    # Extract information on the number of smoothing parameters and
    # random effects
    smooth_labs <- do.call(
      rbind,
      lapply(seq_along(ss_gam$smooth), function(x) {
        data.frame(
          label = ss_gam$smooth[[x]]$label,
          class = class(ss_gam$smooth[[x]])[1],
          nsp = ss_gam$smooth[[x]]$last.sp -
            ss_gam$smooth[[x]]$first.sp +
            1
        )
      })
    )

    # Check for gp() terms
    if (!is.null(gp_terms)) {
      gp_additions <- make_gp_additions(
        gp_details = gp_details,
        orig_formula = orig_formula,
        data = data_train,
        newdata = NULL,
        model_data = list(X = t(predict(ss_gam, type = 'lpmatrix'))),
        mgcv_model = ss_gam,
        gp_terms = gp_terms,
        family = family
      )
      gp_names <- unlist(
        purrr::map(gp_additions$gp_att_table, 'name'),
        use.names = FALSE
      )
      gp_isos <- unlist(
        purrr::map(gp_additions$gp_att_table, 'iso'),
        use.names = FALSE
      )
      abbv_names <- vector(mode = 'list', length = length(gp_names))
      full_names <- vector(mode = 'list', length = length(gp_names))
      for (i in seq_len(length(gp_names))) {
        if (gp_isos[i]) {
          abbv_names[[i]] <- gp_names[i]
          full_names[[i]] <- paste0(gp_names[i], '[1]')
        } else {
          abbv_names[[i]] <- paste0(gp_names[i], '[1][', 1:2, ']')
          full_names[[i]] <- paste0(gp_names[i], '[1][', 1:2, ']')
        }
      }
      full_names <- unlist(full_names, use.names = FALSE)
      abbv_names <- unlist(abbv_names, use.names = FALSE)
      alpha_priors <- unlist(
        purrr::map(gp_additions$gp_att_table, 'def_alpha'),
        use.names = FALSE
      )
      rho_priors <- unlist(
        purrr::map(gp_additions$gp_att_table, 'def_rho'),
        use.names = FALSE
      )
      rho_2_priors <- unlist(
        purrr::map(gp_additions$gp_att_table, 'def_rho_2'),
        use.names = FALSE
      )
      full_priors <- vector(mode = 'list', length = length(gp_names))
      for (i in seq_len(length(gp_names))) {
        if (gp_isos[i]) {
          full_priors[[i]] <- rho_priors[i]
        } else {
          full_priors[[i]] <- c(rho_priors[i], rho_2_priors[i])
        }
      }
      full_priors <- unlist(full_priors, use.names = FALSE)
      smooth_labs <- smooth_labs %>%
        dplyr::filter(
          !label %in%
            gsub('gp(', 's(', gp_names, fixed = TRUE)
        )

      alpha_df <- data.frame(
        param_name = paste0('real<lower=0> alpha_', gp_names, ';'),
        param_length = 1,
        param_info = paste(gp_names, 'marginal deviation'),
        prior = paste0('alpha_', gp_names, ' ~ ', alpha_priors, ';'),
        example_change = paste0(
          'alpha_',
          gp_names,
          ' ~ ',
          'normal(0, ',
          round(runif(length(gp_names), 0.5, 1), 2),
          ');'
        )
      )
      rho_df <- data.frame(
        param_name = paste0('real<lower=0> rho_', abbv_names, ';'),
        param_length = 1,
        param_info = paste(abbv_names, 'length scale'),
        prior = paste0('rho_', full_names, ' ~ ', full_priors, ';'),
        example_change = paste0(
          'rho_',
          full_names,
          ' ~ ',
          'normal(0, ',
          round(runif(length(full_names), 0.5, 1), 2),
          ');'
        )
      )
      gp_df <- rbind(alpha_df, rho_df)
    } else {
      gp_df <- NULL
    }

    # Smoothing parameter priors for non-random effect smooths
    if (any(smooth_labs$class != 'random.effect')) {
      n_smooth_params <- smooth_labs %>%
        dplyr::filter(class != 'random.effect') %>%
        dplyr::pull(nsp)
      nonre_smooths <- smooth_labs %>%
        dplyr::filter(class != 'random.effect') %>%
        dplyr::pull(label)

      if (use_stan) {
        sp_df <- data.frame(
          param_name = 'vector<lower=0>[n_sp] lambda;',
          param_length = sum(smooth_labs$nsp),
          param_info = c(paste(
            nonre_smooths,
            'smooth parameters',
            collapse = ', '
          )),
          prior = 'lambda ~ normal(5, 30);',
          # Add an example for changing the prior; note that it is difficult to
          # understand how to change individual smoothing parameter priors because each
          # one acts on a different subset of the smooth function parameter space
          example_change = c(
            paste0(
              'lambda ~ exponential(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      } else {
        # Not recommended to alter smoothing parameter priors for JAGS as the Gibbs sampler
        # needs to have informative priors to have any hope of convergence
        sp_df <- NULL
      }
    } else {
      sp_df <- NULL
    }

    # Population mean and sd priors for random effect smooths
    if (any(smooth_labs$class == 'random.effect')) {
      re_smooths <- smooth_labs %>%
        dplyr::filter(class == 'random.effect') %>%
        dplyr::pull(label)
      n_re_terms <- length(re_smooths)

      if (use_stan) {
        re_df <- data.frame(
          param_name = c(
            paste0('vector[', n_re_terms, '] mu_raw;'),
            paste0('vector<lower=0>[', n_re_terms, '] sigma_raw;')
          ),
          param_length = rep(n_re_terms, 2),
          param_info = c(
            paste(re_smooths, 'pop mean', collapse = ', '),
            paste(re_smooths, 'pop sd', collapse = ', ')
          ),
          prior = c('mu_raw ~ std_normal();', 'sigma_raw ~ exponential(0.5);')
        )

        # Add example change that users could implement to put different priors
        # on each re's mean and sd
        if (n_re_terms > 1) {
          re_df <- cbind(
            re_df,
            data.frame(
              example_change = c(
                paste(
                  paste0(
                    'mu_raw[',
                    1:n_re_terms,
                    '] ~ normal(',
                    round(runif(min = -1, max = 1, n = n_re_terms), 2),
                    ', ',
                    round(runif(min = 0.1, max = 1, n = n_re_terms), 2),
                    ');'
                  ),
                  collapse = '\n'
                ),
                paste(
                  paste0(
                    'sigma_raw[',
                    1:n_re_terms,
                    '] ~ exponential(',
                    round(runif(min = 0.01, max = 1, n = n_re_terms), 2),
                    ');'
                  ),
                  collapse = '\n'
                )
              )
            )
          )
        } else {
          re_df <- cbind(
            re_df,
            data.frame(
              example_change = c(
                paste0(
                  'mu_raw ~ normal(',
                  round(runif(min = -1, max = 1, n = 1), 2),
                  ', ',
                  round(runif(min = 0.1, max = 1, n = 1), 2),
                  ');'
                ),
                paste0(
                  'sigma_raw ~ exponential(',
                  round(runif(min = 0.01, max = 1, n = 1), 2),
                  ');'
                )
              )
            )
          )
        }
      } else {
        # If using JAGS as the backend
        re_df <- data.frame(
          param_name = c(
            paste0('mu_raw', 1:n_re_terms),
            paste0('sigma_raw', 1:n_re_terms, '<lower=0>')
          ),
          param_length = 1,
          param_info = c(
            paste(re_smooths, 'pop mean'),
            paste(re_smooths, 'pop sd')
          ),
          prior = c(
            paste0('mu_raw', 1:n_re_terms, ' ~ dnorm(0, 1)'),
            paste0('sigma_raw', 1:n_re_terms, ' ~ dexp(0.5)')
          )
        )

        # Add example change that users could implement to put different priors
        # on each re's mean and sd
        if (n_re_terms > 1) {
          re_df <- cbind(
            re_df,
            data.frame(
              example_change = c(
                paste(paste0(
                  'mu_raw',
                  1:n_re_terms,
                  ' ~ dnorm(',
                  round(runif(min = -1, max = 1, n = n_re_terms), 2),
                  ', ',
                  round(runif(min = 0.1, max = 10, n = n_re_terms), 2),
                  ')'
                )),
                paste(paste0(
                  'sigma_raw',
                  1:n_re_terms,
                  ' ~ dexp(',
                  round(runif(min = 0.01, max = 1, n = n_re_terms), 2),
                  ')'
                ))
              )
            )
          )
        } else {
          re_df <- cbind(
            re_df,
            data.frame(
              example_change = c(
                paste0(
                  'mu_raw ~ dnorm(',
                  round(runif(min = -1, max = 1, n = 1), 2),
                  ', ',
                  round(runif(min = 0.1, max = 1, n = 1), 2),
                  ')'
                ),
                paste0(
                  'sigma_raw ~ dexp(',
                  round(runif(min = 0.01, max = 1, n = 1), 2),
                  ')'
                )
              )
            )
          )
        }
      }
    } else {
      re_df <- NULL
    }

    # Extract information on priors for trend components
    trend_df <- NULL

    if (trend_model %in% c('PWlinear', 'PWlogistic')) {
      # Need to fix this as a next priority
      # trend_df <- NULL
      trend_df <- data.frame(
        param_name = c(
          'vector[n_series] k_trend;',
          'vector[n_series] m_trend;'
        ),
        param_length = length(unique(data_train$series)),
        param_info = c('base trend growth rates', 'trend offset parameters'),
        prior = c('k_trend ~ std_normal();', 'm_trend ~ student_t(3, 0, 2.5);'),
        example_change = c(
          paste0(
            'k ~ normal(',
            round(runif(min = -1, max = 1, n = 1), 2),
            ', ',
            round(runif(min = 0.1, max = 1, n = 1), 2),
            ');'
          ),
          paste0(
            'm ~ normal(',
            round(runif(min = -1, max = 1, n = 1), 2),
            ', ',
            round(runif(min = 0.1, max = 1, n = 1), 2),
            ');'
          )
        )
      )
    }
    if (trend_model == 'GP') {
      if (use_lv) {
        trend_df <- data.frame(
          param_name = c('vector<lower=0>[n_lv] rho_gp;'),
          param_length = n_lv,
          param_info = c('trend length scale'),
          prior = c('rho_gp ~ inv_gamma(1.499007, 5.670433);'),
          example_change = paste0(
            'rho_gp ~ exponential(',
            round(runif(min = 0.01, max = 1, n = 1), 2),
            ');'
          )
        )
      } else {
        trend_df <- data.frame(
          param_name = c(
            'vector<lower=0>[n_series] alpha_gp;',
            'vector<lower=0>[n_series] rho_gp;'
          ),
          param_length = length(unique(data_train$series)),
          param_info = c('trend amplitude', 'trend length scale'),
          prior = c(
            'alpha_gp ~ normal(0, 0.5);',
            'rho_gp ~ inv_gamma(1.499007, 5.670433);'
          ),
          example_change = c(
            paste0(
              'alpha_gp ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'rho_gp ~ exponential(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      }

      trend_df <- rbind(
        trend_df,
        data.frame(
          param_name = c('int<lower=1> num_gp_basis;'),
          param_length = 1,
          param_info = c('basis dimension for approximate GP'),
          prior = c('num_gp_basis = min(20, n);'),
          example_change = 'num_gp_basis = 12;'
        )
      )
    }

    if (trend_model %in% c('ZMVN', 'ZMVNhiercor')) {
      trend_df <- data.frame(
        param_name = c(paste0(
          'vector<lower=0>[',
          ifelse(use_lv, 'n_lv', 'n_series'),
          '] sigma;'
        )),
        param_length = ifelse(use_lv, n_lv, length(unique(data_train$series))),
        param_info = c('residual sd'),
        prior = c('sigma ~ exponential(2);'),
        example_change = c(
          paste0(
            'sigma ~ exponential(',
            round(runif(min = 0.01, max = 1, n = 1), 2),
            ');'
          )
        )
      )
    }

    if (trend_model %in% c('RW', 'RWcor', 'RWhiercor')) {
      if (use_stan) {
        trend_df <- data.frame(
          param_name = c(paste0(
            'vector<lower=0>[',
            ifelse(use_lv, 'n_lv', 'n_series'),
            '] sigma;'
          )),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c('trend sd'),
          prior = c('sigma ~ exponential(2);'),
          example_change = c(
            paste0(
              'sigma ~ exponential(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      } else {
        trend_df <- data.frame(
          param_name = c('vector<lower=0>[n_series] sigma'),
          param_length = length(unique(data_train$series)),
          param_info = 'trend sd (for each series s)',
          prior = c('sigma[s] ~ dexp(1)T(0.075, 5)'),
          example_change = c(
            paste0(
              'sigma[s] ~ dexp(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ')'
            )
          )
        )
      }
    }

    if (trend_model == 'VAR1') {
      trend_df <- data.frame(
        param_name = c('vector<lower=0>[n_series] sigma;'),
        param_length = c(length(unique(data_train$series))),
        param_info = c('trend sd'),
        prior = c('sigma ~ inv_gamma(2.3693353, 0.7311319);'),
        example_change = c(
          paste0(
            'sigma ~ exponential(',
            round(runif(min = 0.01, max = 1, n = 1), 2),
            ');'
          )
        )
      )
      trend_df <- rbind(
        trend_df,
        data.frame(
          param_name = c(
            "real es[1];",
            "real es[2];",
            "real<lower=0> fs[1];",
            "real<lower=0> fs[2];",
            "real<lower=0> gs[1];",
            "real<lower=0> gs[2];",
            "real<lower=0> hs[1];",
            "real<lower=0> hs[2];"
          ),
          param_length = 1,
          param_info = c(
            'diagonal autocorrelation population mean',
            'off-diagonal autocorrelation population mean',
            'diagonal autocorrelation population variance',
            'off-diagonal autocorrelation population variance',
            'shape1 for diagonal autocorrelation precision',
            'shape1 for off-diagonal autocorrelation precision',
            'shape2 for diagonal autocorrelation precision',
            'shape2 for off-diagonal autocorrelation precision'
          ),
          prior = c(
            "es[1] = 0;",
            "es[2] = 0;",
            "fs[1] = sqrt(0.455);",
            "fs[2] = sqrt(0.455);",
            "gs[1] = 1.365;",
            "gs[2] = 1.365;",
            "hs[1] = 0.071175;",
            "hs[2] = 0.071175;"
          ),
          example_change = c(
            "es[1] = 0.5;",
            "es[2] = 0.1;",
            "fs[1] = 0.6;",
            "fs[2] = 0.3;",
            "gs[1] = 1.1;",
            "gs[2] = 1.07;",
            "hs[1] = 0.08;",
            "hs[2] = 0.1;"
          )
        )
      )
    }

    if (trend_model %in% c('VAR1cor', 'VARhiercor', 'VARMA1,1cor')) {
      trend_df <- data.frame(
        param_name = c('vector<lower=0>[n_series] sigma;'),
        param_length = c(length(unique(data_train$series))),
        param_info = c('trend sd'),
        prior = c('sigma ~ inv_gamma(2.3693353, 0.7311319);'),
        example_change = c(
          paste0(
            'sigma ~ exponential(',
            round(runif(min = 0.01, max = 1, n = 1), 2),
            ');'
          )
        )
      )
      trend_df <- rbind(
        trend_df,
        data.frame(
          param_name = c(
            "real es[1];",
            "real es[2];",
            "real<lower=0> fs[1];",
            "real<lower=0> fs[2];",
            "real<lower=0> gs[1];",
            "real<lower=0> gs[2];",
            "real<lower=0> hs[1];",
            "real<lower=0> hs[2];",
            "real<lower=0> L_Omega;"
          ),
          param_length = 1,
          param_info = c(
            'diagonal autocorrelation population mean',
            'off-diagonal autocorrelation population mean',
            'diagonal autocorrelation population variance',
            'off-diagonal autocorrelation population variance',
            'shape1 for diagonal autocorrelation precision',
            'shape1 for off-diagonal autocorrelation precision',
            'shape2 for diagonal autocorrelation precision',
            'shape2 for off-diagonal autocorrelation precision',
            'LKJ prior on trend error correlations'
          ),
          prior = c(
            "es[1] = 0;",
            "es[2] = 0;",
            "fs[1] = sqrt(0.455);",
            "fs[2] = sqrt(0.455);",
            "gs[1] = 1.365;",
            "gs[2] = 1.365;",
            "hs[1] = 0.071175;",
            "hs[2] = 0.071175;",
            "L_Omega ~ lkj_corr_cholesky(2);"
          ),
          example_change = c(
            "es[1] = 0.5;",
            "es[2] = 0.1;",
            "fs[1] = 0.6;",
            "fs[2] = 0.3;",
            "gs[1] = 1.1;",
            "gs[2] = 1.07;",
            "hs[1] = 0.08;",
            "hs[2] = 0.1;",
            "L_Omega ~ lkj_corr_cholesky(4);"
          )
        )
      )
    }

    if (trend_model == 'CAR1') {
      trend_df <- data.frame(
        param_name = c(
          paste0(
            'vector<lower=0,upper=1>[',
            ifelse(use_lv, 'n_lv', 'n_series'),
            '] ar1;'
          ),
          paste0(
            'vector<lower=0>[',
            ifelse(use_lv, 'n_lv', 'n_series'),
            '] sigma;'
          )
        ),
        param_length = ifelse(use_lv, n_lv, length(unique(data_train$series))),
        param_info = c('trend AR1 coefficient', 'trend sd'),
        prior = c('ar1 ~ std_normal();', 'sigma ~ exponential(2);'),
        example_change = c(
          paste0(
            'ar1 ~ normal(',
            round(runif(min = 0.1, max = 1, n = 1), 2),
            ', ',
            round(runif(min = 0.1, max = 1, n = 1), 2),
            ');'
          ),
          paste0(
            'sigma ~ exponential(',
            round(runif(min = 0.01, max = 1, n = 1), 2),
            ');'
          )
        )
      )
    }

    if (trend_model %in% c('AR1', 'AR1cor', 'AR1hiercor')) {
      if (use_stan) {
        trend_df <- data.frame(
          param_name = c(
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar1;'
            ),
            paste0(
              'vector<lower=0>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] sigma;'
            )
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c('trend AR1 coefficient', 'trend sd'),
          prior = c('ar1 ~ std_normal();', 'sigma ~ exponential(2);'),
          example_change = c(
            paste0(
              'ar1 ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'sigma ~ exponential(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      } else {
        trend_df <- data.frame(
          param_name = c(
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar1;'
            ),
            paste0(
              'vector<lower=0>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] sigma;'
            )
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c(
            'trend AR1 coefficient (for each series s)',
            'trend sd (for each series s)'
          ),
          prior = c('ar1[s] ~ dnorm(0, 10)', 'sigma[s] ~ dexp(2)T(0.075, 5)'),
          example_change = c(
            paste0(
              'ar1[s] ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            ),
            paste0(
              'sigma[s] ~ dexp(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ')'
            )
          )
        )
      }
    }

    if (trend_model %in% c('AR2', 'AR2cor', 'AR2hiercor')) {
      if (use_stan) {
        trend_df <- data.frame(
          param_name = c(
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar1;'
            ),
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar2;'
            ),
            paste0(
              'vector<lower=0>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] sigma;'
            )
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c(
            'trend AR1 coefficient',
            'trend AR2 coefficient',
            'trend sd'
          ),
          prior = c(
            'ar1 ~ std_normal();',
            'ar2 ~ std_normal();',
            'sigma ~ exponential(2);'
          ),
          example_change = c(
            paste0(
              'ar1 ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'ar2 ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'sigma ~ exponential(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      } else {
        trend_df <- data.frame(
          param_name = c(
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar1;'
            ),
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar2;'
            ),
            paste0(
              'vector<lower=0>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] sigma;'
            )
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c(
            'trend AR1 coefficient (for each series s)',
            'trend AR2 coefficient (for each series s)',
            'trend sd (for each series s)'
          ),
          prior = c(
            'ar1[s] ~ dnorm(0, 10)',
            'ar2[s] ~ dnorm(0, 10)',
            'sigma[s] ~ dexp(2)T(0.075, 5)'
          ),
          example_change = c(
            paste0(
              'ar1[s] ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            ),
            paste0(
              'ar2[s] ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            ),
            paste0(
              'sigma[s] ~ dexp(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ')'
            )
          )
        )
      }
    }

    if (trend_model %in% c('AR3', 'AR3cor', 'AR3hiercor')) {
      if (use_stan) {
        trend_df <- data.frame(
          param_name = c(
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar1;'
            ),
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar2;'
            ),
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar3;'
            ),
            paste0(
              'vector<lower=0>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] sigma;'
            )
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c(
            'trend AR1 coefficient',
            'trend AR2 coefficient',
            'trend AR3 coefficient',
            'trend sd'
          ),
          prior = c(
            'ar1 ~ std_normal();',
            'ar2 ~ std_normal();',
            'ar3 ~ std_normal();',
            'sigma ~ exponential(2);'
          ),
          example_change = c(
            paste0(
              'ar1 ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'ar2 ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'ar3 ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            ),
            paste0(
              'sigma ~ exponential(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      } else {
        trend_df <- data.frame(
          param_name = c(
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar1;'
            ),
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar2;'
            ),
            paste0(
              'vector<lower=-1,upper=1>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] ar3;'
            ),
            paste0(
              'vector<lower=0>[',
              ifelse(use_lv, 'n_lv', 'n_series'),
              '] sigma;'
            )
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c(
            'trend AR1 coefficient (for each series s)',
            'trend AR2 coefficient (for each series s)',
            'trend AR3 coefficient (for each series s)',
            'trend sd (for each series s)'
          ),
          prior = c(
            'ar1[s] ~ dnorm(0, 10)',
            'ar2[s] ~ dnorm(0, 10)',
            'ar3[s] ~ dnorm(0, 10)',
            'sigma[s] ~ dexp(2)T(0.075, 5)'
          ),
          example_change = c(
            paste0(
              'ar1[s] ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            ),
            paste0(
              'ar2[s] ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            ),
            paste0(
              'ar3[s] ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            ),
            paste0(
              'sigma[s] ~ dexp(',
              round(runif(min = 0.01, max = 1, n = 1), 2),
              ')'
            )
          )
        )
      }
    }

    # Remove options for trend variance priors if using a dynamic factor model
    if (use_lv) {
      if (missing(trend_map)) {
        trend_df %>%
          dplyr::filter(
            !grepl(
              paste0(
                'vector<lower=0>[',
                ifelse(use_lv, 'n_lv', 'n_series'),
                '] sigma;'
              ),
              param_name,
              fixed = TRUE
            )
          ) -> trend_df
      }

      if (use_stan) {
        if (missing(trend_map)) {
          trend_df <- rbind(
            trend_df,
            data.frame(
              param_name = c('vector[M] L;'),
              param_length = n_lv * length(unique(data_train$series)),
              param_info = c('factor loadings'),
              prior = c('L ~ student_t(5, 0, 1);'),
              example_change = 'L ~ std_normal();'
            )
          )
        }
      }
    }

    # Extract drift parameter information
    if (drift) {
      if (use_stan) {
        drift_df <- data.frame(
          param_name = paste0(
            'vector[',
            ifelse(use_lv, 'n_lv', 'n_series'),
            '] drift;'
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c('trend drift'),
          prior = c('drift ~ std_normal();'),
          example_change = c(
            paste0(
              'drift ~ normal(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ');'
            )
          )
        )
      } else {
        drift_df <- data.frame(
          param_name = paste0(
            'vector[',
            ifelse(use_lv, 'n_lv', 'n_series'),
            '] drift;'
          ),
          param_length = ifelse(
            use_lv,
            n_lv,
            length(unique(data_train$series))
          ),
          param_info = c('trend drift (for each series s)'),
          prior = c('drift ~ dnorm(0, 10)'),
          example_change = c(
            paste0(
              'drift ~ dnorm(',
              round(runif(min = -1, max = 1, n = 1), 2),
              ', ',
              round(runif(min = 0.1, max = 1, n = 1), 2),
              ')'
            )
          )
        )
      }
    } else {
      drift_df <- NULL
    }

    # Extract information for family-specific parameters
    family_df <- family_prior_info(
      family = family_char,
      use_stan = use_stan,
      data = data_train
    )

    # Return the dataframe of prior information
    prior_df <- rbind(
      para_df,
      gp_df,
      sp_df,
      re_df,
      trend_df,
      drift_df,
      family_df
    )

    prior_df$new_lowerbound <- NA
    prior_df$new_upperbound <- NA

    # Final update to use more brms-like default priors on
    # scale parameters
    def_scale_prior <- update_default_scales(
      response = replace_nas(data_train[[out_name]]),
      family = family
    )

    # Update in priors df
    if (any(grepl('sigma|sigma_raw|sigma_obs', prior_df$prior))) {
      lines_with_scales <- grep('sigma|sigma_raw|sigma_obs', prior_df$prior)
      for (i in lines_with_scales) {
        prior_df$prior[i] <- paste0(
          trimws(strsplit(prior_df$prior[i], "[~]")[[1]][1]),
          ' ~ ',
          def_scale_prior,
          ';'
        )
      }
    }
    out <- prior_df
  }

  return(out)
}

#' @export
#' @importFrom brms prior
brms::prior

#' @export
#' @importFrom brms prior_
brms::prior_

#' @export
#' @importFrom brms set_prior
brms::set_prior

#' @export
#' @importFrom brms prior_string
brms::prior_string

#' Use informative scale and intercept priors following brms example
#' @importFrom stats mad qcauchy setNames
#' @importFrom brms logm1 prior_string get_prior
#' @noRd
make_default_scales = function(response, family) {
  def_scale_prior <- update_default_scales(response, family)
  c(
    prior_string(def_scale_prior, class = 'sigma'),
    prior_string(def_scale_prior, class = 'sigma_raw'),
    prior_string(def_scale_prior, class = 'sigma_obs')
  )
}

#' @noRd
make_default_int = function(response, family) {
  if (all(is.na(response))) {
    out <- prior_string("student_t(3, 0, 3.5)", class = '(Intercept)')
  } else if (family$family == 'nmix') {
    # Intercept prior in N-Mixtures applies to avg detection probability
    out <- prior_string("normal(0, 1.5)", class = '(Intercept)')
  } else {
    resp_dat <- data.frame(y = response[!is.na(response)])
    int_prior <- get_prior(
      y ~ 1,
      data = resp_dat,
      family = family_to_brmsfam(family)
    )
    out <- prior_string(
      int_prior$prior[which(int_prior$class == 'Intercept')],
      class = '(Intercept)'
    )
  }
  return(out)
}

#' @noRd
linkfun = function(x, link) {
  switch(
    link,
    identity = x,
    log = log(x),
    logm1 = logm1(x),
    log1p = log1p(x),
    inverse = 1 / x,
    sqrt = sqrt(x),
    `1/mu^2` = 1 / x^2,
    tan_half = tan(x / 2),
    logit = plogis(x),
    probit = qnorm(x),
    cauchit = qcauchy(x),
    probit_approx = qnorm(x),
    squareplus = (x^2 - 1) / x,
    stop("Link '", link, "' is not supported.", call. = FALSE)
  )
}

#' @noRd
update_default_scales = function(
  response,
  family,
  df = 3,
  center = TRUE
) {
  if (all(is.na(response))) {
    out <- paste0(
      "student_t(",
      paste0(as.character(c(df, '0', '3')), collapse = ", "),
      ")"
    )
  } else {
    y <- response[!is.na(response)]
    link <- family$link
    if (link %in% c("log", "inverse", "1/mu^2")) {
      # avoid Inf in link(y)
      y <- ifelse(y == 0, y + 0.1, y)
    }

    y_link <- suppressWarnings(linkfun(y, link = link))
    scale_y <- round(mad(y_link, na.rm = TRUE), 1)

    if (scale_y <= 5) {
      out <- 'inv_gamma(1.418, 0.452)'
    }

    if (
      scale_y > 5 &
        scale_y <= 20
    ) {
      out <- 'inv_gamma(0.9187, 0.3516)'
    }

    if (scale_y > 20) {
      out <- paste0(
        "student_t(",
        paste0(as.character(c(df, 0, scale_y)), collapse = ", "),
        ")"
      )
    }
  }

  return(out)
}
