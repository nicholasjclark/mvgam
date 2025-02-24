#'Argument validation functions
#'@param data Data to be validated (list or data.frame)
#'@noRd
validate_series_time = function(
  data,
  name = 'data',
  trend_model,
  check_levels = TRUE,
  check_times = TRUE
) {
  # First validation requires the full trend_model object
  if (!inherits(trend_model, 'mvgam_trend')) {
    trend_model <- list(
      trend_model = trend_model,
      unit = 'time',
      gr = 'NA',
      subgr = 'series'
    )
  }

  # Respect any original additions of implicit_vars
  implicit_series <- implicit_time <- FALSE
  if (!is.null(attr(data, 'implicit_vars'))) {
    implicit_series <- 'series' %in% attr(data, 'implicit_vars')
    implicit_time <- 'time' %in% attr(data, 'implicit_vars')
  }

  # Validate any grouping structure and update the data accordingly
  data <- validate_series_groups(
    data = data,
    trend_model = trend_model,
    name = name
  )
  if (!is.null(attr(data, 'implicit_vars'))) {
    implicit_series <- 'series' %in% attr(data, 'implicit_vars')
    implicit_time <- 'time' %in% attr(data, 'implicit_vars')
  }

  # Now we only need the character trend_model string
  trend_model <- trend_model$trend_model

  # Series label must be present as a factor and
  # must contain a time variable
  if (inherits(data, 'data.frame')) {
    data %>%
      dplyr::ungroup() -> data

    if (!'series' %in% colnames(data)) {
      data$series <- factor('series1')
      implicit_series <- TRUE
    }

    # Series factor must have all unique levels present
    if (!is.factor(data$series)) {
      stop('Variable "series" must be a factor type', call. = FALSE)
    }

    if (!'time' %in% colnames(data)) {
      if (trend_model == 'None') {
        # Add a time indicator if missing
        data %>%
          dplyr::group_by(series) %>%
          dplyr::mutate(time = dplyr::row_number()) %>%
          dplyr::ungroup() -> data
        implicit_time <- TRUE
      } else {
        stop(name, " does not contain a 'time' variable", call. = FALSE)
      }
    }
  }

  if (inherits(data, 'list')) {
    if (!'series' %in% names(data)) {
      data$series <- factor('series1')
      implicit_series <- TRUE
    }

    # Series factor must have all unique levels present
    if (!is.factor(data$series)) {
      stop('Variable "series" must be a factor type', call. = FALSE)
    }

    if (!'time' %in% names(data)) {
      if (trend_model == 'None') {
        # Add a time indicator if missing
        data.frame(series = data$series) %>%
          dplyr::group_by(series) %>%
          dplyr::mutate(time = dplyr::row_number()) %>%
          dplyr::pull(time) -> times
        implicit_time <- TRUE
        data$time <- times
      } else {
        stop(name, " does not contain a 'time' variable", call. = FALSE)
      }
    }
  }

  # Add an identifier so post-processing functions know
  # what the original supplied data ordering was; this is needed
  # for ensuring that functions such as fitted() and
  # residuals() return objects that match the original order that
  # the user supplied, if no newdata are given
  data$index..orig..order <- 1:length(data$time)

  # Add a new 'time' variable that will be useful for rearranging data for
  # modeling, in case 'time' is also supplied as a covariate or if this is
  # a continuous time model (CAR1)
  data$index..time..index <- data$time

  # Use the data ordering to set the index of time for CAR1
  if (trend_model == 'CAR1') {
    data.frame(series = data$series, time = data$time) %>%
      dplyr::mutate(orig_rows = dplyr::row_number()) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(idx = dplyr::row_number()) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(time. = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(orig_rows) %>%
      dplyr::pull(time.) -> times
    data$index..time..index <- times
  }

  # Series factor must have all unique levels present if this is a
  # forecast check
  if (check_levels) {
    if (!all(levels(data$series) %in% unique(data$series))) {
      stop(
        paste0(
          'Mismatch between factor levels of "series" and unique values of "series"',
          '\n',
          'Use\n  `setdiff(levels(data$series), unique(data$series))` \nand',
          '\n',
          '  `intersect(levels(data$series), unique(data$series))`\nfor guidance'
        ),
        call. = FALSE
      )
    }
  }

  # Ensure each series has an observation, even if NA, for each
  # unique timepoint (only for trend models that require discrete time with
  # regularly spaced sampling intervals)
  if (check_times) {
    all_times_avail = function(time, min_time, max_time) {
      identical(
        as.numeric(sort(time)),
        as.numeric(seq.int(from = min_time, to = max_time))
      )
    }
    min_time <- as.numeric(min(data$index..time..index))
    max_time <- as.numeric(max(data$index..time..index))
    data.frame(series = data$series, time = data$index..time..index) %>%
      dplyr::group_by(series) %>%
      dplyr::summarise(
        all_there = all_times_avail(time, min_time, max_time)
      ) -> checked_times
    if (any(checked_times$all_there == FALSE)) {
      stop(
        "One or more series in ",
        name,
        " is missing observations for one or more timepoints",
        call. = FALSE
      )
    }
  }

  if (implicit_series & implicit_time) {
    attr(data, 'implicit_vars') <- c('series', 'time')
  }

  if (implicit_series & !implicit_time) {
    attr(data, 'implicit_vars') <- 'series'
  }

  if (implicit_time & !implicit_series) {
    attr(data, 'implicit_vars') <- 'time'
  }

  if (!implicit_time & !implicit_series) {
    attr(data, 'implicit_vars') <- NULL
  }

  return(data)
}

# Function to ensure units of analysis, groups and subgroups are arranged
# and formatted properly for mvgam processing and modelling
#'@noRd
validate_series_groups = function(data, trend_model, name = 'data') {
  implicit_series <- implicit_time <- FALSE

  # Checks only needed if trend_model isn't 'None'
  if (trend_model$trend_model != 'None') {
    # Check that unit and subgr exist in data and are the correct type
    if (is.null(trend_model$gr)) trend_model$gr <- 'NA'
    if (is.null(trend_model$unit)) trend_model$unit <- 'time'
    if (is.null(trend_model$subgr)) trend_model$subgr <- 'series'

    if (
      trend_model$gr == 'NA' &
        trend_model$unit == 'time' &
        trend_model$subgr == 'series'
    ) {
      if (!'series' %in% names(data)) {
        data$series <- factor('series1')
        implicit_series <- TRUE
      }

      if (!'time' %in% names(data)) {
        if (trend_model$trend_model %in% c('ZMVNcor', 'ZMVNhiercor')) {
          # Add a time indicator if missing
          data.frame(series = data$series) %>%
            dplyr::group_by(series) %>%
            dplyr::mutate(time = dplyr::row_number()) %>%
            dplyr::pull(time) -> times
          implicit_time <- TRUE
          data$time <- times
        } else {
          stop(name, " does not contain a 'time' variable", call. = FALSE)
        }
      }
    }
    validate_var_exists(
      data = data,
      variable = trend_model$unit,
      type = 'num/int',
      name = name,
      trend_char = trend_model$trend_model
    )

    validate_var_exists(
      data = data,
      variable = trend_model$subgr,
      type = 'factor',
      name = name,
      trend_char = trend_model$trend_model
    )

    # If gr is supplied, check it exists and is the correct type
    if (trend_model$gr != 'NA') {
      implicit_series <- implicit_time <- TRUE
      validate_var_exists(
        data = data,
        variable = trend_model$gr,
        type = 'factor',
        name = name,
        trend_char = trend_model$trend_model
      )
      if (trend_model$subgr == 'series') {
        stop(
          'argument "subgr" cannot be set to "series" if "gr" is also supplied',
          call. = FALSE
        )
      }

      # Add necessary 'time' variable
      gr_dat <- data.frame(
        time = data[[trend_model$unit]],
        gr = data[[trend_model$gr]],
        subgr = data[[trend_model$subgr]]
      )

      # Check that each level of gr contains all possible levels of subgr
      gr_total_levels <- gr_dat %>%
        dplyr::group_by(gr) %>%
        dplyr::summarise(tot_subgrs = length(unique(subgr))) %>%
        dplyr::ungroup() %>%
        dplyr::pull(tot_subgrs)
      if (length(gr_total_levels) > 1) {
        if (stats::var(gr_total_levels) != 0) {
          stop(
            paste0(
              'Some levels of "',
              trend_model$gr,
              '" do not contain all\n',
              'unique levels of "',
              trend_model$subgr,
              '"',
              " in ",
              name
            ),
            call. = FALSE
          )
        }
      }

      gr_dat %>%
        dplyr::mutate(
          series = interaction(
            gr,
            subgr,
            drop = TRUE,
            sep = '_',
            lex.order = TRUE
          )
        ) -> gr_dat
    } else {
      if (trend_model$unit != 'time') {
        implicit_time <- TRUE
      }
      if (trend_model$subgr != 'series') {
        implicit_series <- TRUE
      }
      gr_dat <- data.frame(
        time = data[[trend_model$unit]],
        subgr = data[[trend_model$subgr]]
      ) %>%
        dplyr::mutate(series = as.factor(subgr))
    }

    # Add the possibly new 'series' and 'time' variables to
    # the data and return
    data$series <- gr_dat$series
    data$time <- gr_dat$time
  }

  if (implicit_series & implicit_time) {
    attr(data, 'implicit_vars') <- c('series', 'time')
  }

  if (implicit_series & !implicit_time) {
    attr(data, 'implicit_vars') <- 'series'
  }

  if (implicit_time & !implicit_series) {
    attr(data, 'implicit_vars') <- 'time'
  }

  if (!implicit_time & !implicit_series) {
    attr(data, 'implicit_vars') <- NULL
  }

  return(data)
}

#'@noRd
validate_var_exists = function(
  data,
  variable,
  type = 'factor',
  name = 'data',
  trend_char
) {
  if (trend_char != 'None') {
    if (!exists(variable, data)) {
      stop(
        paste0('Variable "', variable, '" not found in ', name),
        call. = FALSE
      )
    }

    if (type == 'num/int') {
      if (!is.numeric(data[[variable]])) {
        stop(
          paste0(
            'Variable "',
            variable,
            '" must be either numeric or integer type'
          ),
          call. = FALSE
        )
      }
    }

    if (type == 'factor') {
      if (!is.factor(data[[variable]])) {
        stop(
          paste0('Variable "', variable, '" must be a factor type'),
          call. = FALSE
        )
      }
    }
  }
}

#'@noRd
deparse_variable = function(...) {
  deparse0(substitute(...))
}

#'@noRd
as_one_logical = function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single logical value.", call. = FALSE)
  }
  x
}

#'@noRd
as_one_integer <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single integer value.", call. = FALSE)
  }
  x
}

#'@noRd
deparse0 <- function(x, max_char = NULL, ...) {
  out <- collapse(deparse(x, ...))
  if (isTRUE(max_char > 0)) {
    out <- substr(out, 1L, max_char)
  }
  out
}

#'@noRd
collapse <- function(..., sep = "") {
  paste(..., sep = sep, collapse = "")
}

#'@noRd
validate_silent <- function(silent) {
  silent <- as_one_integer(silent)
  if (silent < 0 || silent > 2) {
    stop("'silent' must be between 0 and 2.", call. = FALSE)
  }
  silent
}

#'@importFrom rlang warn
#'@noRd
validate_family = function(family, use_stan = TRUE) {
  if (is.character(family)) {
    if (family == 'beta') family <- betar()

    family <- try(eval(parse(text = family)), silent = TRUE)

    if (inherits(family, 'try-error'))
      stop("family not recognized", call. = FALSE)
  }

  if (is.function(family)) family <- family()

  if (is.null(family$family)) stop("family not recognized", call. = FALSE)

  if (!inherits(family, 'family')) stop('family not recognized', call. = FALSE)

  if (family$family == 'Beta regression') family$family <- 'beta'

  if (family$family == 'tweedie')
    insight::check_if_installed(
      "tweedie",
      reason = 'to simulate from Tweedie distributions'
    )

  if (
    !family$family %in% c('poisson', 'negative binomial', 'tweedie') & !use_stan
  )
    stop(
      'JAGS only supports poisson(), nb() or tweedie() families',
      call. = FALSE
    )

  # Stan cannot support Tweedie
  if (use_stan & family$family == 'tweedie')
    stop('Tweedie family not supported for stan', call. = FALSE)

  if (family$family %in% c('binomial', 'beta_binomial'))
    rlang::warn(
      paste0(
        "Binomial and Beta-binomial families require cbind(n_successes, n_trials)\n",
        "in the formula left-hand side. Do not use cbind(n_successes, n_failures)!"
      ),
      .frequency = "once",
      .frequency_id = 'cbind_binomials'
    )
  return(family)
}

#'@noRd
validate_family_restrictions = function(response, family) {
  response <- response[!is.na(response)]

  # 0s and 1s only for Bernoulli
  if (family$family == 'bernoulli') {
    y <- response
    nobs <- length(response)
    weights <- rep(1, length(response))
    eval(binomial()$initialize)
  }

  # 0s and 1s not allowed for Beta
  if (family$family == 'beta') {
    if (any(response <= 0)) {
      stop('Values <= 0 not allowed for beta responses', call. = FALSE)
    }
    if (any(response >= 1)) {
      stop('Values >= 1 not allowed for beta responses', call. = FALSE)
    }
  }

  # negatives not allowed for several families
  if (
    family$family %in%
      c('poisson', 'negative binomial', 'tweedie', 'binomial', 'beta_binomial')
  ) {
    if (any(response < 0)) {
      stop(
        paste0('Values < 0 not allowed for count family responses'),
        call. = FALSE
      )
    }
  }

  # negatives and/or zeros not allowed for several families
  if (family$family %in% c('lognormal', 'Gamma')) {
    if (any(response <= 0)) {
      stop(
        paste0('Values <= 0 not allowed for ', family$family, ' responses'),
        call. = FALSE
      )
    }
  }
}

#'@noRd
validate_trend_model = function(
  trend_model,
  drift = FALSE,
  noncentred = FALSE,
  warn = TRUE
) {
  if (inherits(trend_model, 'mvgam_trend')) {
    ma_term <- if (trend_model$ma) {
      'MA'
    } else {
      NULL
    }
    cor_term <- if (trend_model$cor) {
      'cor'
    } else {
      NULL
    }
    if (is.null(trend_model$gr)) trend_model$gr <- 'NA'
    if (trend_model$gr != 'NA') {
      gr_term <- 'hier'
    } else {
      gr_term <- NULL
    }
    trend_model <- paste0(
      trend_model$trend_model,
      trend_model$p,
      ma_term,
      gr_term,
      cor_term
    )
  } else {
    if (trend_model != 'None' & warn) {
      rlang::warn(
        paste0(
          "Supplying trend_model as a character string is deprecated\n",
          "Please use the dedicated functions (i.e. RW() or ZMVN()) instead"
        ),
        .frequency = "once",
        .frequency_id = 'trend_characters'
      )
    }
  }

  trend_model <- match.arg(arg = trend_model, choices = trend_model_choices())

  if (trend_model == 'VAR') {
    trend_model <- 'VAR1'
  }
  if (trend_model == 'VARcor') {
    trend_model <- 'VAR1cor'
  }
  if (trend_model == 'VARhiercor') {
    trend_model <- 'VAR1hiercor'
  }
  if (trend_model %in% c('VARMA', 'VARMAcor')) {
    trend_model <- 'VARMA1,1cor'
  }

  if (
    !trend_model %in% c('None', 'RW', 'AR1', 'AR2', 'AR3', 'CAR1') & noncentred
  ) {
    message('Non-centering of trends currently not available for this model')
  }

  if (trend_model %in% c('PWlinear', 'PWlogistic'))
    insight::check_if_installed(
      "extraDistr",
      reason = 'to simulate from piecewise trends'
    )
  return(trend_model)
}

#'@noRd
validate_obs_formula = function(formula, data, refit = FALSE) {
  if (attr(terms(formula), "response") == 0L) {
    stop('response variable is missing from formula', call. = FALSE)
  }

  # Check that response terms are in the data; account for possible
  # 'cbind' in there if this is a binomial model
  resp_terms <- as.character(terms(formula(formula))[[2]])
  if (length(resp_terms) == 1) {
    out_name <- as.character(terms(formula(formula))[[2]])
    if (!as.character(terms(formula(formula))[[2]]) %in% names(data)) {
      stop(
        paste0('variable ', terms(formula(formula))[[2]], ' not found in data'),
        call. = FALSE
      )
    }
  } else {
    if (any(grepl('cbind', resp_terms))) {
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      out_name <- resp_terms[1]
      for (i in 1:length(resp_terms)) {
        if (!resp_terms[i] %in% names(data)) {
          stop(
            paste0('variable ', resp_terms[i], ' not found in data'),
            call. = FALSE
          )
        }
      }
    } else {
      stop(
        'Not sure how to deal with this response variable specification',
        call. = FALSE
      )
    }
  }

  if (any(attr(terms(formula), 'term.labels') %in% 'y')) {
    stop(
      'due to internal data processing, "y" should not be used as the name of a predictor in mvgam',
      call. = FALSE
    )
  }

  # Add a y outcome for sending to the modelling backend
  data$y <- data[[out_name]]

  return(data)
}

#'@noRd
validate_trend_formula = function(formula) {
  if (!is.null(rlang::f_lhs(formula))) {
    stop(
      'Argument "trend_formula" should not have a left-hand side',
      call. = FALSE
    )
  }

  if (any(grepl('series', as.character(formula)))) {
    stop(
      'Argument "trend_formula" should not have the identifier "series" in it.\nUse "trend" instead for varying effects',
      call. = FALSE
    )
  }

  if (!is.null(attr(terms(formula(formula)), 'offset'))) {
    stop('Offsets not allowed in argument "trend_formula"', call. = FALSE)
  }
}

#'@noRd
validate_gr_subgr = function(gr, subgr, cor) {
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))

  if (gr != 'NA') {
    if (subgr == 'NA') {
      stop(
        'argument "subgr" must be supplied if "gr" is also supplied',
        call. = FALSE
      )
    }
  }

  if (subgr != 'NA') {
    if (gr == 'NA') {
      stop(
        'argument "gr" must be supplied if "subgr" is also supplied',
        call. = FALSE
      )
    } else {
      cor <- TRUE
    }
  }

  list(.group = gr, .subgroup = subgr, .cor = cor)
}

#'@noRd
validate_proportional = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (x < 0 || x > 1) {
    stop(
      "Argument '",
      s,
      "' must be a proportion ranging from 0 to 1, inclusive",
      call. = FALSE
    )
  }
}

#'@noRd
validate_equaldims = function(x, y) {
  s <- substitute(x)
  q <- substitute(y)

  if (NCOL(x) != NCOL(y)) {
    stop(
      "Argument '",
      s,
      "' and argument '",
      q,
      "' must have equal dimensions",
      call. = FALSE
    )
  }

  if (NROW(x) != NROW(y)) {
    stop(
      "Argument '",
      s,
      "' and argument '",
      q,
      "' must have equal dimensions",
      call. = FALSE
    )
  }
}

#'@noRd
validate_pos_integer = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (sign(x) != 1) {
    stop("Argument '", s, "' must be a positive integer", call. = FALSE)
  } else {
    if (x %% 1 != 0) {
      stop("Argument '", s, "' must be a positive integer", call. = FALSE)
    }
  }
}

#'@noRd
validate_pos_integers = function(x) {
  s <- substitute(x)

  val_pos = function(y, s) {
    y <- base::suppressWarnings(as.numeric(y))
    if (sign(y) != 1) {
      stop("Negative values in ", s, " detected", call. = FALSE)
    } else {
      if (y %% 1 != 0) {
        stop("Non-integer values in ", s, " detected", call. = FALSE)
      }
    }
  }
  res <- lapply(seq_along(x), function(i) val_pos(x[i], s))
}

#'@noRd
validate_even <- function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (x %% 2 != 0) {
    stop("Argument '", s, "'  must be an even integer", call. = FALSE)
  }
}

#'@noRd
validate_pos_real = function(x) {
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value", call. = FALSE)
  }

  if (sign(x) != 1) {
    stop("Argument '", s, "' must be a positive real value", call. = FALSE)
  }
}

#'@noRd
validate_trendmap = function(trend_map, data_train, trend_model, use_stan) {
  # Trend mapping not supported by JAGS
  if (!use_stan) {
    stop('trend mapping not available for JAGS', call. = FALSE)
  }

  # trend_map must have an entry for each unique time series
  if (!all(sort(trend_map$series) == sort(unique(data_train$series)))) {
    stop(
      'Argument "trend_map" must have an entry for every unique time series in "data"',
      call. = FALSE
    )
  }

  # trend_map must not specify a greater number of trends than there are series
  if (max(trend_map$trend) > length(unique(data_train$series))) {
    stop(
      'Argument "trend_map" specifies more latent trends than there are series in "data"',
      call. = FALSE
    )
  }

  # trend_map must not skip any trends, but can have zeros for some entries
  drop_zero = function(x) {
    x[x != 0]
  }

  if (
    !all(
      drop_zero(sort(unique(trend_map$trend))) == seq(1:max(trend_map$trend))
    )
  ) {
    stop(
      'Argument "trend_map" must link at least one series to each latent trend',
      call. = FALSE
    )
  }

  # series variable must be a factor with same levels as the series variable
  # in the data
  if (!is.factor(trend_map$series)) {
    stop(
      'trend_map$series must be a factor with levels matching levels of data$series',
      call. = FALSE
    )
  }

  if (!all(levels(trend_map$series) == levels(data_train$series))) {
    stop(
      'trend_map$series must be a factor with levels matching levels of data$series',
      call. = FALSE
    )
  }
}

#'@noRd
validate_trend_restrictions = function(
  trend_model,
  formula,
  trend_formula,
  trend_map,
  drift = FALSE,
  drop_obs_intercept = FALSE,
  use_lv = FALSE,
  n_lv,
  data_train,
  use_stan = TRUE,
  priors = FALSE
) {
  # Assess whether additional moving average or correlated errors are needed
  ma_cor_adds <- ma_cor_additions(trend_model)
  list2env(ma_cor_adds, envir = environment())

  if (length(unique(data_train$series)) == 1 & add_cor) {
    warning(
      'Correlated process errors not possible with only 1 series',
      call. = FALSE
    )
    add_cor <- FALSE
  }

  # Some checks on general trend setup restrictions
  if (!priors) {
    if (trend_model %in% c('PWlinear', 'PWlogistic')) {
      if (attr(terms(formula), 'intercept') == 1 & !drop_obs_intercept) {
        warning(
          paste0(
            'It is difficult / impossible to estimate intercepts\n',
            'and piecewise trend offset parameters. You may want to\n',
            'consider dropping the intercept from the formula'
          ),
          call. = FALSE
        )
      }

      if (use_lv)
        stop(
          'Cannot estimate piecewise trends using dynamic factors',
          call. = FALSE
        )
    }
  }

  if (use_lv & (add_ma | add_cor) & missing(trend_formula)) {
    stop(
      'Cannot estimate moving averages or correlated errors for dynamic factors',
      call. = FALSE
    )
  }

  if (use_lv & drift) {
    warning(
      'Cannot identify drift terms for this model\ninclude "time" as a fixed effect instead',
      call. = FALSE
    )
    drift <- FALSE
  }

  if (drift && trend_model == 'CAR1') {
    warning(
      'Cannot identify drift terms for CAR models; setting "drift = FALSE"',
      call. = FALSE
    )
    drift <- FALSE
  }

  if (
    trend_model %in% c('VAR', 'VAR1', 'VAR1cor', 'VARMA1,1cor', 'GP') & drift
  ) {
    warning(
      'Cannot identify drift terms for VAR or GP models; setting "drift = FALSE"',
      call. = FALSE
    )
    drift <- FALSE
  }

  if (use_lv & trend_model == 'VAR1' & missing(trend_formula)) {
    stop(
      'Cannot identify dynamic factor models that evolve as VAR processes',
      call. = FALSE
    )
  }

  if (!use_stan & trend_model %in% c('GP', 'VAR1', 'PWlinear', 'PWlogistic')) {
    stop(
      'Gaussian Process, VAR and piecewise trends not supported for JAGS',
      call. = FALSE
    )
  }

  # Check trend formula and create the trend_map if missing
  if (!missing(trend_formula)) {
    validate_trend_formula(trend_formula)
    if (missing(trend_map)) {
      trend_map <- data.frame(
        series = factor(
          levels(data_train$series),
          levels = levels(data_train$series)
        ),
        trend = 1:length(unique(data_train$series))
      )
    }

    if (
      !trend_model %in%
        c('None', 'RW', 'AR1', 'AR2', 'AR3', 'VAR1', 'CAR1', 'ZMVN')
    ) {
      stop(
        'only None, ZMVN, RW, AR1, AR2, AR3, CAR1 and VAR trends currently supported for trend predictor models',
        call. = FALSE
      )
    }
  }

  # Check trend_map is correctly specified
  if (!missing(trend_map)) {
    validate_trendmap(
      trend_map = trend_map,
      data_train = data_train,
      trend_model = trend_model,
      use_stan = use_stan
    )

    # If trend_map correctly specified, set use_lv to TRUE for
    # most models (but not yet for VAR models, which require additional
    # modifications)
    if (trend_model == 'VAR1') {
      use_lv <- FALSE
    } else {
      use_lv <- TRUE
    }
    n_lv <- max(trend_map$trend)
  }

  # Number of latent variables cannot be greater than number of series
  if (use_lv) {
    if (missing(n_lv)) {
      n_lv <- min(2, floor(length(unique(data_train$series)) / 2))
    }
    if (n_lv > length(unique(data_train$series))) {
      stop('number of latent variables cannot be greater than number of series')
    }
  }

  if (missing(trend_map)) {
    trend_map <- NULL
  }

  if (missing(n_lv)) {
    n_lv <- NULL
  }

  return(list(
    trend_model = trend_model,
    add_cor = add_cor,
    add_ma = add_ma,
    use_var1 = use_var1,
    use_var1cor = use_var1cor,
    use_lv = use_lv,
    n_lv = n_lv,
    trend_map = trend_map,
    drift = drift
  ))
}

#'@noRd
check_priorsim = function(prior_simulation, data_train, orig_y, formula) {
  # Fill y with NAs if this is a simulation from the priors
  if (prior_simulation) {
    data_train$y <- rep(NA, length(data_train$y))
  } else {
    data_train$y <- orig_y
  }

  # Fill response variable with original supplied values
  resp_terms <- as.character(terms(formula(formula))[[2]])
  if (length(resp_terms) == 1) {
    out_name <- as.character(terms(formula(formula))[[2]])
  } else {
    if (any(grepl('cbind', resp_terms))) {
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      out_name <- resp_terms[1]
    }
  }
  data_train[[out_name]] <- orig_y

  return(data_train)
}

#'@noRd
check_gp_terms = function(formula, data_train, family) {
  # Check for proper binomial specification
  if (!missing(family)) {
    if (is.character(family)) {
      if (family == 'beta') family <- betar()

      family <- try(eval(parse(text = family)), silent = TRUE)

      if (inherits(family, 'try-error'))
        stop("family not recognized", call. = FALSE)
    }

    if (is.function(family)) family <- family()

    if (family$family %in% c('binomial', 'beta_binomial')) {
      # Check that response terms use the cbind() syntax
      resp_terms <- as.character(terms(formula(formula))[[2]])
      if (length(resp_terms) == 1) {
        stop(
          'Binomial family requires cbind() syntax in the formula left-hand side',
          call. = FALSE
        )
      } else {
        if (any(grepl('cbind', resp_terms))) {
        } else {
          stop(
            'Binomial family requires cbind() syntax in the formula left-hand side',
            call. = FALSE
          )
        }
      }
    }
  }

  # Check for gp terms in the validated formula
  orig_formula <- gp_terms <- gp_details <- NULL
  if (any(grepl('gp(', attr(terms(formula), 'term.labels'), fixed = TRUE))) {
    formula <- interpret_mvgam(
      formula,
      N = max(data_train$time),
      family = family
    )
    orig_formula <- formula

    # Keep intercept?
    keep_intercept <- attr(terms(formula), 'intercept') == 1

    # Indices of gp() terms in formula
    gp_terms <- which_are_gp(formula)

    # Extract attributes
    gp_details <- get_gp_attributes(formula, data_train, family)

    # Replace with s() terms so the correct terms are included
    # in the model.frame
    formula <- gp_to_s(formula, data_train, family)
    if (!keep_intercept) formula <- update(formula, . ~ . - 1)
  }

  return(list(
    orig_formula = orig_formula,
    gp_terms = gp_terms,
    formula = formula,
    gp_details = gp_details
  ))
}

#'@noRd
check_obs_intercept = function(formula, orig_formula) {
  # Check for missing rhs in formula
  # If there are no terms in the observation formula (i.e. y ~ -1),
  # we will use an intercept-only observation formula and fix
  # the intercept coefficient at zero
  drop_obs_intercept <- FALSE
  if (
    length(attr(terms(formula), 'term.labels')) == 0 &
      !attr(terms(formula), 'intercept') == 1
  ) {
    formula_envir <- attr(formula, '.Environment')

    if (length(attr(terms(formula), 'factors')) == 0) {
      resp <- as.character(attr(terms(formula), 'variables'))[2]
    } else {
      resp <- dimnames(attr(terms(formula), 'factors'))[[1]][1]
    }

    if (!is.null(attr(terms(formula(formula)), 'offset'))) {
      formula <- formula(paste0(
        resp,
        ' ~ ',
        paste(gsub(' - 1', ' + 1', rlang::f_text(formula)))
      ))
    } else {
      formula <- formula(paste(resp, '~ 1'))
    }
    attr(formula, '.Environment') <- formula_envir
    drop_obs_intercept <- TRUE
  }

  if (is.null(orig_formula)) orig_formula <- formula

  return(list(
    orig_formula = orig_formula,
    formula = formula,
    drop_obs_intercept = drop_obs_intercept
  ))
}

#'@noRd
check_nmix = function(
  family,
  family_char,
  trend_formula,
  trend_model,
  trend_map,
  data_train,
  priors = FALSE
) {
  # Check for N-mixture modifications
  add_nmix <- FALSE
  nmix_trendmap <- TRUE
  if (family_char == 'nmix') {
    if (!(exists('cap', where = data_train))) {
      stop(
        'Max abundances must be supplied as a variable named "cap" for N-mixture models',
        call. = FALSE
      )
    }

    add_nmix <- TRUE
    if (!priors) {
      family <- poisson()
      family_char <- 'poisson'
      if (missing(trend_formula)) {
        stop('Argument "trend_formula" required for nmix models', call. = FALSE)
      }
    }

    if (!missing(trend_map)) {
      nmix_trendmap <- TRUE
    }
    use_lv <- TRUE
    if (trend_model == 'None') {
      trend_model <- 'RW'
    }
  }

  return(list(
    trend_model = trend_model,
    add_nmix = add_nmix,
    nmix_trendmap = nmix_trendmap,
    family = family,
    family_char = family_char
  ))
}

#'@noRd
validate_threads = function(family_char, threads) {
  if (
    threads > 1 &
      !family_char %in%
        c(
          'poisson',
          'negative binomial',
          'gaussian',
          'lognormal',
          'beta',
          'student',
          'Gamma'
        )
  ) {
    warning(
      'multithreading not yet supported for this family; setting threads = 1'
    )
    threads <- 1
  }
  return(threads)
}

#'@noRd
find_jags = function(jags_path) {
  if (!requireNamespace('runjags', quietly = TRUE)) {
    stop('runjags library is required but not found', call. = FALSE)
  }

  if (missing(jags_path)) {
    requireNamespace('runjags', quietly = TRUE)
    jags_path <- runjags::findjags()
  }

  # Code borrowed from the runjags package
  jags_status <- runjags::testjags(jags_path, silent = TRUE)
  if (!jags_status$JAGS.available) {
    if (jags_status$os == "windows") {
      Sys.sleep(0.2)
      jags_status <- runjags::testjags(jags_path, silent = TRUE)
    }

    if (!jags_status$JAGS.available) {
      cat(
        "Unable to call JAGS using '",
        jags_path,
        "'\nTry specifying the path to the JAGS binary as jags_path argument, or re-installing the rjags package.\nUse the runjags::testjags() function for more detailed diagnostics.\n",
        sep = ""
      )
      stop(
        "Unable to call JAGS.\nEither use the Stan backend or follow examples in ?mvgam to generate data / model files and run outside of mvgam",
        call. = FALSE
      )
    }
  }
}

#'@noRd
find_stan = function() {
  if (!requireNamespace('rstan', quietly = TRUE)) {
    warning('rstan library not found; checking for cmdstanr library')

    if (!requireNamespace('cmdstanr', quietly = TRUE)) {
      stop('cmdstanr library not found', call. = FALSE)
    }
  }
}

#'@noRd
as_one_character <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.character(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse0(s, max_char = 100L)
    stop("Cannot coerce '", s, "' to a single character value.", call. = FALSE)
  }
  x
}
