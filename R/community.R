#' Community quantities from a fitted model
#'
#' @description
#' Computes quantities of a whole community, draw by draw, from a model
#' of many series. A community is every series at one site and time.
#' Each gets its expected number of species, its diversity and its
#' community-weighted mean traits. Each species gets its probability of
#' presence. Each pair of species gets how often it occurs together
#' beyond what the covariates predict. Pass `newdata` as a covariate
#' gradient, such as one from [datagrid()], to see how the community
#' changes along it.
#'
#' @param object A fitted `mvgam` object.
#' @param newdata Data frame with one row per community, holding its
#'   covariates. Each row is crossed with every fitted series, which
#'   replaces any series column it holds. `NULL` takes the communities of
#'   the training data.
#' @param ... Unused. Anything passed here is refused.
#' @param threshold A species counts as present when its response
#'   exceeds `threshold`. `NULL` (the default) takes zero, which a count
#'   or binary family needs. A continuous family needs a value, such as
#'   a biomass. Under [nmix()] it applies to the latent abundance.
#' @param traits Species traits for community-weighted means: a data
#'   frame with one row per species, matched as [jsdgam()] matches its
#'   `traits`, or `"auto"` for the traits the fit was built with. `NULL`
#'   (the default) computes none.
#' @param latent How the latent state enters. `"marginal"` (the default)
#'   averages over it. `"conditional"` takes the state the model fitted,
#'   for communities in the training data. `"zero"` sets it to zero, a
#'   typical site. See *Latent state*.
#' @param n_nodes Number of latent states each draw averages over under
#'   `latent = "marginal"`.
#' @param ndraws,draw_ids The posterior draws to use, as
#'   [hindcast.mvgam()] takes them. `NULL` for both uses every draw.
#' @param resp The response to report, needed on a model with several.
#'
#' @details
#' *Richness*. Each draw sums the species' probabilities of presence,
#' giving the expected number present. A count family's probability is
#' one less its probability of a zero, which covers zero inflation and
#' hurdles. Under [occ()] it is the occupancy probability and under
#' [nmix()] the probability that the latent abundance exceeds
#' `threshold`. Richness counts the species the model holds.
#'
#' *Latent state*. Species share their latent factors or trends, which
#' move them together. Because presence is a non-linear function of the
#' latent state, a typical site differs from an average one. Under
#' `latent = "marginal"` each draw averages presence over `n_nodes`
#' latent states drawn from the trend's stationary covariance across
#' series. The draws of the latent state are shared by every community,
#' which makes a difference between communities free of their noise. A
#' `RW()` or `LLT()` trend has no stationary covariance. Each draw then
#' averages over the latent states it fitted at the training times.
#'
#' *Diversity*. A family with an abundance, such as a count or a
#' biomass, weights each species by its expected value. Under [nmix()]
#' the weight is the latent abundance. Hill numbers of order one and two
#' (the exponential of Shannon entropy and inverse Simpson) follow from
#' the relative weights. A binary or occupancy family has no abundance.
#' Its weight is the probability of presence, which gives the expected
#' trait of the species present. Hill numbers are then left out. Each
#' measure is averaged over the latent states, giving the expected value
#' at one site. Under a count family the weights confound abundance
#' with detection.
#'
#' A factor trait gives one column per level, whose weighted mean is the
#' share of that level.
#'
#' *Co-occurrence*. Species sharing a latent state appear together more
#' or less often than their covariates alone predict. A pair's excess
#' co-occurrence is the probability that both are present less the
#' product of their probabilities, averaged over the communities. A
#' pair the latent state moves independently has none. Neither does any
#' pair under `latent = "conditional"` or `"zero"`, which fix the state.
#'
#' @return An object of class `mvgam_community`. `print()` gives each
#'   community's richness. `summary()` takes `probs` and returns a list
#'   of tibbles, each with its median and interval. `richness` and
#'   `diversity` hold one row per community, `presence` one per
#'   community and series and `cooccurrence` one per pair of species.
#'   `plot()` draws richness or diversity along a column of `newdata`,
#'   or the excess co-occurrence of each pair.
#'
#'   `as_draws_rvars()` returns the posterior draws as
#'   [posterior::rvar()]s, ready for arithmetic between communities and
#'   for [posterior::summarise_draws()]. Community `i` is row `i` of the
#'   object's `communities` element.
#'   \describe{
#'     \item{`richness`}{one per community}
#'     \item{`presence`}{`[community, species]`, `NA` where a community
#'       lacks the species}
#'     \item{`diversity`}{`[community, measure]`, one measure per Hill
#'       number and trait, when there are any}
#'     \item{`cooccurrence`}{`[species, species]` excess co-occurrence}
#'   }
#'   `as_draws_df()` returns the same draws as a data frame, one column
#'   per element.
#'
#' @seealso [residual_cor()] for the associations between species,
#'   [variance_partition()] for what moves each species
#'
#' @inherit jsdgam examples
#'
#' @author Nicholas J Clark
#' @export
community <- function(object, ...) {
  UseMethod("community")
}


#' @rdname community
#' @method community mvgam
#' @export
community.mvgam <- function(object, newdata = NULL, ..., threshold = NULL,
                            traits = NULL,
                            latent = c("marginal", "conditional", "zero"),
                            n_nodes = 100L, ndraws = NULL, draw_ids = NULL,
                            resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  checkmate::assert_data_frame(newdata, min.rows = 1L, null.ok = TRUE)
  checkmate::assert_number(threshold, finite = TRUE, null.ok = TRUE)
  latent <- match.arg(latent)
  checkmate::assert_count(n_nodes, positive = TRUE)
  validate_draw_selectors(ndraws, draw_ids)
  require_fitted_model(object, "community")
  require_series_axis(object, "community")
  resolve_resp(object, resp, required = TRUE, caller = "community")
  draw_ids <- resolve_draw_indices(posterior::ndraws(object), ndraws,
                                   draw_ids)
  if (latent == "conditional" && !is.null(newdata)) {
    unseen <- is.na(fitted_time_index(object, newdata))
    if (any(unseen)) {
      stop(insight::format_error(c(
        "'latent = \"conditional\"' takes the state fitted at each time.",
        x = paste0(sum(unseen), " row(s) of 'newdata' fall outside the ",
                   "fitted times."),
        i = "Use 'latent = \"marginal\"' for new sites and times."
      )), call. = FALSE)
    }
  }
  layout <- community_layout(object, newdata)
  linpred <- posterior_linpred(object, newdata = layout$frame,
                               draw_ids = draw_ids,
                               incl_autocor = latent == "conditional",
                               resp = resp)
  present <- presence_map(object, layout$frame, draw_ids, resp, threshold,
                          dim(linpred))
  abundance <- abundance_map(object, layout$frame, draw_ids, resp,
                             dim(linpred))
  species <- fitted_series_levels(object)
  trait_rows <- community_traits(traits, object, species)[layout$series, ,
                                                          drop = FALSE]
  # Every measure of one latent state, summed over the states
  measure <- function(state) {
    eta <- if (is.null(state)) linpred else {
      linpred + state[, layout$series, drop = FALSE]
    }
    at <- present(eta)
    c(list(presence = at, joint = pairwise_presence(at, layout$cell)),
      community_diversity(if (is.null(abundance)) at else abundance(eta),
                          layout$incidence, trait_rows,
                          hill = !is.null(abundance)))
  }
  states <- if (latent == "marginal") {
    latent_nodes(object, draw_ids, n_nodes)
  }
  states <- states %||% list(NULL)
  sums <- NULL
  for (state in states) {
    at <- measure(state)
    sums <- if (is.null(sums)) at else Map(`+`, sums, at)
  }
  means <- lapply(sums, `/`, length(states))
  excess <- (means$joint - pairwise_presence(means$presence, layout$cell)) /
    nrow(layout$cell)
  structure(
    list(
      communities = community_columns(object, layout$communities),
      community = layout$community,
      series = layout$series,
      species_column = jsdgam_axis_names(object)$species %||%
        axis_vars(object)$series_var,
      frame = layout$frame,
      richness = means$presence %*% layout$incidence,
      presence = means$presence,
      diversity = means[setdiff(names(means), c("presence", "joint"))],
      cooccurrence = excess,
      species = species,
      latent = latent,
      threshold = threshold,
      draw_ids = draw_ids
    ),
    class = "mvgam_community"
  )
}


#' The rows a community quantity is computed over
#'
#' A frame of communities is crossed with every fitted series. The
#' training data group their rows by every axis but the series.
#'
#' @param object A fitted `mvgam` object
#' @param newdata Frame of communities, or `NULL`
#' @return List of `frame`, one row per community and series;
#'   `communities`, one row per community; `series`, each row's series
#'   index; `community`, each row's community index; `incidence`, the
#'   `[rows, communities]` matrix summing each
#'   community's rows; and `cell`, the `[communities, series]` matrix of
#'   rows, `NA` where a community lacks a series
#' @noRd
community_layout <- function(object, newdata) {
  vars <- axis_vars(object)
  training <- fitted_data(object)
  # A `jsdgam()` fit copies the user's species column into its own
  series_cols <- intersect(
    c(vars$series_var, vars$gr_var, vars$subgr_var,
      jsdgam_axis_names(object)$species),
    names(training)
  )
  if (is.null(newdata)) {
    keys <- setdiff(intersect(unlist(vars), names(training)), series_cols)
    row_series <- axis_row_series(object, training, required = TRUE)
    # A unit's repeat visits share its state. One row per series stays.
    keep <- !duplicated(cbind(training[keys], row_series))
    frame <- training[keep, , drop = FALSE]
    community <- interaction(frame[keys], drop = TRUE, lex.order = TRUE)
    communities <- frame[!duplicated(community), keys, drop = FALSE]
  } else {
    species <- training[!duplicated(training[series_cols]), series_cols,
                        drop = FALSE]
    species <- species[order(axis_row_series(object, species,
                                              required = TRUE)), ,
                       drop = FALSE]
    rows <- rep(seq_len(nrow(newdata)), each = nrow(species))
    frame <- newdata[rows, , drop = FALSE]
    frame[series_cols] <- species[rep(seq_len(nrow(species)),
                                      nrow(newdata)), , drop = FALSE]
    community <- factor(rows)
    communities <- newdata[setdiff(names(newdata), series_cols)]
  }
  rownames(frame) <- NULL
  rownames(communities) <- NULL
  series <- as.integer(axis_row_series(object, frame, required = TRUE))
  rows <- cbind(as.integer(community), series)
  incidence <- matrix(0, nrow(frame), nlevels(community))
  incidence[cbind(seq_len(nrow(frame)), rows[, 1L])] <- 1
  cell <- matrix(NA_integer_, nlevels(community),
                 length(fitted_series_levels(object)))
  cell[rows] <- seq_len(nrow(frame))
  list(frame = frame, communities = communities, series = series,
       community = rows[, 1L], incidence = incidence, cell = cell)
}


#' The columns that tell communities apart
#'
#' A community is shown by the model's covariates and by any axis, such
#' as time, that differs between communities. A response placeholder or
#' a row number from [datagrid()] is left out.
#'
#' @param object A fitted `mvgam` object
#' @param communities Data frame, one row per community
#' @return `communities` with those columns alone
#' @noRd
community_columns <- function(object, communities) {
  covariates <- unlist(find_predictors(object, effects = "all",
                                       flatten = TRUE))
  axes <- intersect(unlist(axis_vars(object)), names(communities))
  varying <- axes[vapply(communities[axes], function(col) {
    length(unique(col)) > 1L
  }, logical(1L))]
  communities[intersect(names(communities),
                        union(varying, setdiff(covariates, axes)))]
}


#' Each row's probability of presence as a function of its predictor
#'
#' Everything but the predictor is resolved once, as `epred_map()` does.
#'
#' @param object A fitted `mvgam` object
#' @param frame The rows
#' @param draw_ids Indices of the draws
#' @param resp A response name, or `NULL`
#' @param threshold The response a species must exceed, or `NULL`
#' @param dims `c(ndraws, nrow)` of the predictors the map takes
#' @return A function of one `[ndraws, nrow]` predictor
#' @noRd
presence_map <- function(object, frame, draw_ids, resp, threshold, dims) {
  family <- model_families(object, resp)
  name <- resolve_family_name(family)
  if (is_closure_unit_family(family)) {
    state <- family$linkinv
    if (models_latent_count(family)) {
      return(function(eta) {
        stats::ppois(threshold %||% 0, state(eta), lower.tail = FALSE)
      })
    }
    return(state)
  }
  if (is_multi_response_family(family) || is_ordinal_family(family)) {
    stop(insight::format_error(c(
      paste0("community() is not supported for family '", name, "'."),
      i = "community() covers count, binary, continuous and detection families."
    )), call. = FALSE)
  }
  if (is.null(threshold) && !family_uses_integers(name)) {
    stop(insight::format_error(c(
      "A continuous family needs a 'threshold' for presence.",
      i = "Pass the response a species must exceed, such as a biomass."
    )), call. = FALSE)
  }
  pars <- resolve_family_pars(object, get_family_dpars(name), dims[1L],
                              dims[2L], draw_ids, frame, resp)
  trials <- extract_trials_for_family(object, family, frame, resp)
  if (is.null(threshold)) {
    # One less the probability of a zero, from the family's own density
    return(function(eta) {
      -expm1(dispatch_log_lik(name, family$link, eta, numeric(ncol(eta)),
                              pars, trials))
    })
  }
  function(eta) {
    spec <- family_dist_spec(name, family$link, eta, pars, trials)
    if (is.null(spec)) {
      refuse_missing_family_dispatch(name, "a distribution function")
    }
    dist_cdf(spec, eta, rep(threshold, ncol(eta)), lower.tail = FALSE)
  }
}


#' Each species' weight in a community's diversity and mean traits
#'
#' A family with an abundance gives its expected value, `nmix()` its
#' latent abundance.
#'
#' @inheritParams presence_map
#' @return A function of one `[ndraws, nrow]` predictor, or `NULL` for a
#'   family whose weight is the probability of presence
#' @noRd
abundance_map <- function(object, frame, draw_ids, resp, dims) {
  family <- model_families(object, resp)
  if (is_closure_unit_family(family)) {
    return(if (models_latent_count(family)) family$linkinv)
  }
  support <- mvgam_response_support[[resolve_family_name(family)]]
  binary <- isTRUE(support$integer) && identical(support$bounds, c(0, 1))
  if (is.null(support) || support$bounds[1L] < 0 || binary) {
    return(NULL)
  }
  epred_map(object, frame, draw_ids, resp, dims, ordinal = "mean")
}


#' Species traits as numeric columns in the order of the species
#'
#' A factor trait gives one 0 / 1 column per level.
#'
#' @param traits A data frame, matrix, `"auto"` or `NULL`
#' @param object A fitted `mvgam` object
#' @param species The fitted series
#' @return `[n_species, n_traits]` numeric matrix, or `NULL`
#' @noRd
community_traits <- function(traits, object, species) {
  traits <- resolve_auto_traits(traits, object)
  if (is.null(traits)) {
    return(NULL)
  }
  traits <- align_feature_rows(as.data.frame(traits), species, "traits")
  do.call(cbind, lapply(names(traits), function(col) {
    value <- traits[[col]]
    if (is.character(value)) value <- factor(value)
    if (is.factor(value) && !is.ordered(value)) {
      return(one_hot_columns(value, col))
    }
    matrix(as.numeric(value), dimnames = list(NULL, col))
  }))
}


#' Hill numbers and weighted mean traits of every community
#'
#' @param weight `[ndraws, rows]` weight of each species in each row
#' @param incidence The `[rows, communities]` matrix summing a community
#' @param traits `[rows, n_traits]` trait of each row's species, or
#'   `NULL`
#' @param hill Whether the weights are abundances
#' @return Named list of `[ndraws, communities]` matrices
#' @noRd
community_diversity <- function(weight, incidence, traits, hill) {
  total <- weight %*% incidence
  weighted_mean <- function(value) {
    (weight * rep(value, each = nrow(weight))) %*% incidence / total
  }
  # Zero weight takes no part in entropy, as 0 log 0 = 0
  entropy <- weight * log(pmax(weight, .Machine$double.xmin))
  c(
    if (hill) {
      list(`Hill q = 1` = exp(log(total) - (entropy %*% incidence) / total),
           `Hill q = 2` = total^2 / (weight^2 %*% incidence))
    },
    lapply(stats::setNames(nm = colnames(traits)), function(col) {
      weighted_mean(traits[, col])
    })
  )
}


#' Each pair of species' presence, multiplied and summed over communities
#'
#' A species a community lacks counts as absent there.
#'
#' @param presence `[ndraws, rows]` probabilities of presence
#' @param cell The `[communities, series]` matrix of rows
#' @return `[ndraws, n_series, n_series]` array
#' @noRd
pairwise_presence <- function(presence, cell) {
  out <- array(0, dim = c(nrow(presence), ncol(cell), ncol(cell)))
  for (d in seq_len(nrow(presence))) {
    at <- matrix(presence[d, cell], nrow(cell))
    at[is.na(at)] <- 0
    out[d, , ] <- crossprod(at)
  }
  out
}


#' Draws of the latent state across series to average presence over
#'
#' A trend with a stationary covariance gives `n_nodes` states per draw,
#' each that draw's covariance root times one shared vector of standard
#' normal variates. The vectors come in antithetic pairs. A `RW()` or `LLT()` trend gives the states the draw
#' fitted at each training time.
#'
#' @param object A fitted `mvgam` object
#' @param draw_ids Indices of the draws
#' @param n_nodes Number of states
#' @return List of `[ndraws, n_series]` matrices, or `NULL` for a fit
#'   whose latent state never varies
#' @noRd
latent_nodes <- function(object, draw_ids, n_nodes) {
  type <- get_trend_type(object)
  if (identical(type, "None")) {
    return(NULL)
  }
  if (type %in% c("RW", "LLT")) {
    return(fitted_latent_states(object, draw_ids))
  }
  cov <- trend_series_cov_draws(object, draw_ids)
  if (all(cov == 0)) {
    return(NULL)
  }
  n_series <- dim(cov)[2L]
  roots <- lapply(seq_len(dim(cov)[1L]), function(d) {
    root <- mgcv::mroot(matrix(cov[d, , ], n_series))
    cbind(root, matrix(0, n_series, n_series - ncol(root)))
  })
  # Antithetic pairs cancel the odd moments of the nodes' error
  half <- matrix(stats::rnorm(n_series * ceiling(n_nodes / 2)), n_series)
  z <- cbind(half, -half)[, seq_len(n_nodes), drop = FALSE]
  lapply(seq_len(n_nodes), function(m) {
    matrix(vapply(roots, function(root) drop(root %*% z[, m]),
                  numeric(n_series)),
           ncol = n_series, byrow = TRUE)
  })
}


#' The latent state each draw fitted at each training time
#'
#' @param object A fitted `mvgam` object
#' @param draw_ids Indices of the draws
#' @return List of `[ndraws, n_series]` matrices, one per training time
#'   at which every series was fitted
#' @noRd
fitted_latent_states <- function(object, draw_ids) {
  training <- fitted_data(object)
  parts <- get_linpred_parts(object, training, draw_ids = draw_ids)
  state <- Reduce(`+`, parts$latent)
  series <- as.integer(axis_row_series(object, training, required = TRUE))
  times <- training[[axis_vars(object)$time_var]]
  n_series <- length(fitted_series_levels(object))
  by_time <- split(seq_len(nrow(training)), times)
  by_time <- Filter(function(rows) {
    setequal(series[rows], seq_len(n_series))
  }, by_time)
  lapply(by_time, function(rows) {
    rows <- rows[match(seq_len(n_series), series[rows])]
    state[, rows, drop = FALSE]
  })
}


#' @param probs Interval probabilities of the tables. Defaults to
#'   `c(0.025, 0.975)`.
#' @rdname community
#' @method summary mvgam_community
#' @export
summary.mvgam_community <- function(object, probs = c(0.025, 0.975), ...) {
  rlang::check_dots_empty()
  pairs <- cooccurrence_table(object, probs)
  n <- length(object$species)
  list(
    richness = tibble::as_tibble(cbind(
      object$communities, median_interval_cols(object$richness, probs, "")
    )),
    diversity = tibble::as_tibble(do.call(rbind, Map(function(name, draws) {
      cbind(object$communities, measure = name,
            median_interval_cols(draws, probs, ""))
    }, names(object$diversity), object$diversity))),
    presence = tibble::as_tibble(cbind(
      object$communities[object$community, , drop = FALSE],
      stats::setNames(data.frame(object$species[object$series]),
                      object$species_column),
      median_interval_cols(object$presence, probs, "")
    )),
    cooccurrence = tibble::as_tibble(
      pairs[as.vector(lower.tri(diag(n))), , drop = FALSE]
    )
  )
}


#' Each pair of species' excess co-occurrence, median and interval
#'
#' @param x An `mvgam_community` object
#' @param probs Interval probabilities
#' @return Data frame of the species and the one it is paired `with`,
#'   one row per entry of the matrix in column order, with the median
#'   and interval columns
#' @noRd
cooccurrence_table <- function(x, probs) {
  cbind(
    stats::setNames(expand.grid(x$species, x$species,
                                stringsAsFactors = FALSE),
                    c(x$species_column, "with")),
    median_interval_cols(matrix(x$cooccurrence, dim(x$cooccurrence)[1L]),
                         probs, "")
  )
}


#' @param x An `mvgam_community` object.
#' @rdname community
#' @method as_draws_rvars mvgam_community
#' @export
as_draws_rvars.mvgam_community <- function(x, ...) {
  rlang::check_dots_empty()
  n_draws <- nrow(x$richness)
  n_communities <- nrow(x$communities)
  # A community lacking a species holds NA in its column
  presence <- matrix(NA_real_, n_draws, n_communities * length(x$species))
  presence[, (x$series - 1L) * n_communities + x$community] <- x$presence
  by_community <- function(draws, labels) {
    posterior::rvar(array(draws, c(n_draws, n_communities, length(labels))),
                    dimnames = list(NULL, labels))
  }
  # A binary family without traits has no diversity measure
  diversity <- if (length(x$diversity) > 0L) {
    by_community(unlist(x$diversity), names(x$diversity))
  }
  do.call(posterior::draws_rvars, c(
    list(richness = posterior::rvar(x$richness),
         presence = by_community(presence, x$species)),
    list(diversity = diversity)[!is.null(diversity)],
    list(cooccurrence = posterior::rvar(x$cooccurrence,
                                        dimnames = list(x$species, x$species)))
  ))
}


#' @rdname community
#' @method as_draws_df mvgam_community
#' @export
as_draws_df.mvgam_community <- function(x, ...) {
  posterior::as_draws_df(as_draws_rvars(x, ...))
}


#' @param digits Integer number of significant digits. Defaults to 2.
#' @rdname community
#' @method print mvgam_community
#' @export
print.mvgam_community <- function(x, digits = 2L, ...) {
  above <- if (!is.null(x$threshold)) paste0(" above ", x$threshold)
  cat_titled_table(
    paste0("Expected richness", above, ", latent state ", x$latent, ", ",
           length(x$draw_ids), " draws"),
    summary(x)$richness, digits,
    "summary() also gives presence, diversity and co-occurrence.",
    max_rows = 10L
  )
  invisible(x)
}


#' @param x An `mvgam_community` object from [community()].
#' @param type `"richness"` (the default) draws richness along a column
#'   of the communities. `"diversity"` draws each diversity measure.
#'   `"cooccurrence"` draws each pair's excess co-occurrence, outlining
#'   a pair whose 95% interval excludes zero.
#' @param along The column of the communities to draw along. `NULL`
#'   takes the first numeric column that varies.
#' @rdname community
#' @method plot mvgam_community
#' @export
plot.mvgam_community <- function(x,
                                 type = c("richness", "diversity",
                                          "cooccurrence"),
                                 along = NULL, ...) {
  type <- match.arg(type)
  rlang::check_dots_empty()
  set_color_scheme_local()
  if (type == "cooccurrence") {
    pairs <- cooccurrence_table(x, c(0.025, 0.975))
    as_matrix <- function(col) {
      matrix(pairs[[col]], length(x$species),
             dimnames = list(x$species, x$species))
    }
    panel <- residcor_panel_data(
      as_matrix("Q50"),
      clear = interval_excludes_zero(as_matrix("Q2.5"), as_matrix("Q97.5"))
    )
    return(residcor_heatmap(panel, "Excess\nco-occurrence", rescale = TRUE))
  }
  varying <- names(Filter(function(col) {
    is.numeric(col) && length(unique(col)) > 1L
  }, x$communities))
  along <- along %||% varying[1L]
  checkmate::assert_choice(along, names(x$communities))
  measures <- if (type == "richness") {
    list(`Expected richness` = x$richness)
  } else {
    x$diversity
  }
  if (!length(measures)) {
    stop(insight::format_error(c(
      "'type = \"diversity\"' needs a diversity measure.",
      i = "Pass 'traits' to community() for community-weighted means.",
      i = "Hill numbers need a family with an abundance, such as a count."
    )), call. = FALSE)
  }
  draws <- do.call(cbind, measures)
  values <- rep(x$communities[[along]], length(measures))
  group <- factor(rep(names(measures), each = nrow(x$communities)),
                  levels = names(measures))
  p <- ggplot2::ggplot() +
    mvgam_band_layer(draws, times = values, probs = c(0.2, 0.4, 0.6, 0.8),
                     group = group) +
    mvgam_median_layer(draws, times = values, group = group) +
    ggplot2::labs(x = along, y = if (length(measures) == 1L) {
      names(measures)
    }) +
    mvgam_theme()
  if (length(measures) > 1L) p <- p + mvgam_facet_series()
  p
}
