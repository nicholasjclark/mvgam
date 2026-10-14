# Stan program assembly. brms writes the observation program with the
# trend's stanvars. mvgam then adds the trend to each linear predictor
# and checks the result. The trend generators live here too.

# =============================================================================
# Assembly of the observation and trend programs
# =============================================================================

#' Stan code and data for the observation and trend models
#'
#' Combines the brms observation program with the trend. The trend
#' stanvars are generated from the trend setup and handed to brms with
#' the observation program, then the trend is injected into each
#' response's linear predictor. A multivariate model shares one trend
#' across its responses.
#'
#' @param obs_setup Observation setup from `setup_brms_lightweight()`,
#'   holding `stancode` and `standata`
#' @param trend_setup Trend setup, or NULL for a trend-free model
#' @param trend_specs Trend specifications from
#'   `parse_multivariate_trends()`, or NULL
#' @param prior A `brmsprior` with user priors on observation and trend
#'   parameters, or NULL for the defaults
#'
#' @return A list holding `stancode`, the program, and `standata`, its
#'   data. Without a trend setup the observation program is returned.
#'
#' @noRd
generate_combined_stancode <- function(obs_setup, trend_setup = NULL,
                                      trend_specs = NULL, prior = NULL) {
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, null.ok = TRUE)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # A trend-free program keeps the sampling statements its families
  # wrote, such as the `Psi` prior of `mvn()` and `mvt()`. It takes the
  # same normalising pass as an assembled program.
  if (is.null(trend_setup) || is.null(trend_specs)) {
    return(list(
      stancode = normalise_mvgam_sampling_statements(
        obs_setup$stancode,
        normalize = isTRUE(obs_setup$codegen$normalize %||% TRUE)
      ),
      standata = obs_setup$standata
    ))
  }

  # `parse_multivariate_trends()` gives every response of a
  # multivariate model a copy of one trend specification. The trend
  # stanvars are generated once from that copy. Each response's
  # predictor takes the same trend.
  if (is_trend_spec_list(trend_specs)) {
    response_name <- NULL
    responses_with_trends <- names(trend_specs)
  } else {
    # The key the response's mapping is filed under, which is the name
    # `response_columns()` gives it.
    response_name <- names(response_columns(obs_setup$formula))[1L]
    # A univariate program names its predictor without a suffix
    responses_with_trends <- ""
  }
  trend_stanvars <- extract_trend_stanvars_from_setup(
    trend_setup = trend_setup,
    trend_specs = trend_spec_head(trend_specs),
    response_name = response_name,
    obs_setup = obs_setup,
    prior = prior
  )

  base_stancode <- generate_base_stancode_with_stanvars(
    obs_setup, trend_stanvars
  )

  # The trend joins each response's linear predictor
  combined_stancode <- if (length(trend_stanvars)) {
    inject_trend_into_linear_predictors(base_stancode, responses_with_trends)
  } else {
    base_stancode
  }

  # A Gaussian process in both formulas writes the same functions twice
  combined_stancode <- deduplicate_stan_functions(combined_stancode)

  # The trend emitters write their priors in the tilde form, which
  # Stan strips the normalising constant from. `normalize` covers the
  # whole program. This one pass reaches every emitted statement.
  combined_stancode <- normalise_mvgam_sampling_statements(
    combined_stancode,
    normalize = isTRUE(obs_setup$codegen$normalize %||% TRUE)
  )

  # The data take the trend stanvars and the observation stanvars, such
  # as the data integer `M` that `tweedie()` declares. `data2` holds
  # `car()` adjacency matrices and other objects the specials look up.
  # `prior`, `threads` and `codegen` match the arguments the program
  # was generated with. A program and its data built under different
  # arguments do not fit together. A shrinkage prior declares Stan data
  # of its own.
  standata_stanvars <- combine_stanvars(obs_setup$stanvars, trend_stanvars)
  combined_standata <- generate_base_brms_standata(
    formula = obs_setup$formula,
    data = obs_setup$data,
    family = obs_setup$family,
    stanvars = standata_stanvars,
    data2 = obs_setup$data2,
    codegen = obs_setup$codegen,
    prior = obs_setup$prior,
    threads = obs_setup$threads
  )

  list(stancode = combined_stancode, standata = combined_standata)
}

# Stan spells the unnormalised form of a density `_lupdf` / `_lupmf`,
# and brms emits that form throughout under `normalize = FALSE`. Every
# pattern that has to recognise a density call allows both spellings,
# written once here so a pattern cannot learn only one.
stan_density_call_rx <- "lu?p[dm]f"

#' Regex matching a Stan density call on one distribution stem
#'
#' @param stem Distribution name, e.g. `"poisson_log_glm"`. Empty
#'   matches any density call.
#' @return A regex string covering the normalised and unnormalised
#'   spellings alike.
#' @noRd
stan_density_call_pattern <- function(stem = "") {
  checkmate::assert_string(stem)
  paste0(stem, "_", stan_density_call_rx)
}

#' Write one prior as a Stan sampling statement
#'
#' Stan drops the normalising constant from `x ~ dist(args);`. The
#' `lp__` of a program whose priors are written that way differs from
#' the log joint density by a fixed amount. `bridge_sampler()` computes
#' from `lp__`. The offset then cancels in a Bayes factor only when
#' both models have priors matching in family, hyperparameters and
#' dimension. `target += dist_lpdf(x | args);` keeps the constant, as
#' `normalize = TRUE` promises.
#'
#' The unnormalised branch keeps the tilde, the form the emitters
#' write. `bridge_sampler.mvgam()` looks for it before it trusts
#' `lp__`.
#'
#' Every prior mvgam emits is on a Stan parameter. Stan parameters are
#' continuous, which makes the density `_lpdf` in every case.
#'
#' @param lhs Left-hand side, passed through verbatim. Every shape
#'   the emitters use is legal as a density's first argument: a bare
#'   parameter (`sigma_trend`), a container call (`to_vector(Z)`,
#'   `diagonal(A_trend[lag])`), an index or slice
#'   (`varrho_inv[2:N_lv_trend]`) and a transpose (`lv_trend[t, :]'`).
#' @param dist Distribution call as the prior table spells it, for
#'   example `"student_t(3, 0, 2.5)"` or `"std_normal()"`. Arguments
#'   may nest parentheses, as `"normal(0, sqrt(0.455))"` does.
#' @param normalize Whether to keep the normalising constants.
#' @return One Stan statement, semicolon-terminated and unindented.
#' @noRd
stan_prior_statement <- function(lhs, dist, normalize = TRUE) {
  checkmate::assert_string(lhs, min.chars = 1)
  checkmate::assert_string(dist, min.chars = 1)
  checkmate::assert_flag(normalize)
  lhs <- trimws(lhs)
  dist <- trimws(dist)

  # The name runs to the first `(` and the arguments to the last `)`.
  # Parentheses inside the arguments pass through unchanged.
  open <- regexpr("(", dist, fixed = TRUE)
  chars <- strsplit(dist, "", fixed = TRUE)[[1L]]
  balanced <- sum(chars == "(") == sum(chars == ")")
  if (open < 2L || !endsWith(dist, ")") || !balanced) {
    stop(insight::format_error(c(
      paste0("The prior on '", lhs, "' must be a Stan distribution call."),
      x = paste0("Got '", dist, "'."),
      i = "Give the prior as a call such as 'normal(0, 1)'.",
      i = "Truncation, written 'T[lb, ub]', is not supported in these priors."
    )), call. = FALSE)
  }

  # The left-hand side must name a parameter. A block opener or a
  # string literal here means the caller matched text that is no
  # sampling statement. Stan written from it would fail to compile or
  # would drop the real prior.
  if (grepl("[{}\"/*]|\\b(for|if|else|while)\\b", lhs)) {
    stop_mvgam_fault(
      "A Stan prior statement needs a parameter on its left-hand side.",
      paste0("Got '", lhs, "'.")
    )
  }

  name <- trimws(substr(dist, 1L, open - 1L))
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
    stop(insight::format_error(c(
      paste0("The prior on '", lhs, "' names an invalid distribution."),
      x = paste0("Got '", name, "'."),
      i = "Stan distribution names use letters, digits and underscores."
    )), call. = FALSE)
  }

  args <- trimws(substr(dist, open + 1L, nchar(dist) - 1L))
  if (!normalize) {
    return(paste0(lhs, " ~ ", name, "(", args, ");"))
  }
  # A parameter-free density takes no bar: `std_normal_lpdf(x)`
  operands <- if (nzchar(args)) paste0(lhs, " | ", args) else lhs
  paste0("target += ", name, "_lpdf(", operands, ");")
}


#' Stan code with its comments removed
#'
#' A comment can hold a formula, as `// bf(y ~ s(x))` does. A block
#' comment can span the statement it documents. Neither is a sampling
#' statement.
#'
#' @param x Stan code
#' @return `x` with both comment spellings removed
#' @noRd
strip_stan_comments <- function(x) {
  checkmate::assert_string(x)
  # `stan_line_code()` also removes string literals. A `;` or a `~`
  # inside a string belongs to the string. Statement splitting counts
  # neither.
  paste(stan_line_code(strsplit(x, "\n", fixed = TRUE)[[1L]]),
        collapse = "\n")
}


#' A sampling statement apart from a block opened on its line
#'
#' An emitter may write `for (g in 1:G) { x[g] ~ dist(...); }` on one
#' line. Everything up to the last brace before the tilde opens that
#' block.
#'
#' @param body One line of Stan code
#' @return List of two strings: `opener`, the text through that brace,
#'   then `statement`, the text after it
#' @noRd
split_block_opener <- function(body) {
  checkmate::assert_string(body)
  tilde_at <- regexpr("~", body, fixed = TRUE)
  braces <- if (tilde_at > 0L) {
    gregexpr("{", substr(body, 1L, tilde_at), fixed = TRUE)[[1L]]
  } else {
    -1L
  }
  if (braces[1L] < 0L) {
    return(list(opener = "", statement = body))
  }
  cut <- max(braces)
  list(opener = substr(body, 1L, cut),
       statement = substr(body, cut + 1L, nchar(body)))
}


#' The left-hand side of a sampling statement
#'
#' @param body One line of Stan code, possibly opening a block before
#'   the statement
#' @return The trimmed text before the tilde. NA when the line lacks
#'   a tilde.
#' @noRd
stan_statement_lhs <- function(body) {
  checkmate::assert_string(body)
  statement <- split_block_opener(body)$statement
  tilde_at <- regexpr("~", statement, fixed = TRUE)
  if (tilde_at < 1L) {
    return(NA_character_)
  }
  trimws(substr(statement, 1L, tilde_at - 1L))
}


#' Normalise every sampling statement mvgam wrote
#'
#' The emitters write their priors as `x ~ dist(args);`, the form Stan
#' strips the normalising constant from. brms writes its own as
#' `lprior += dist_lpdf(...)` and never emits a tilde. Every tilde in
#' an assembled program came from an mvgam emitter. This one pass
#' reaches all of them. A new trend generator gets the
#' rewrite without code of its own.
#'
#' Statements are split on `;`. Several emitters wrap a long argument
#' list over two or three lines, which a split by line would break.
#'
#' @param stancode Character scalar holding the assembled program.
#' @param normalize Whether the program should keep its normalising
#'   constants. `FALSE` leaves the tilde form in place, which is what
#'   brms writes `_lupdf` for.
#' @return The program, with every mvgam sampling statement rewritten
#'   when `normalize` is `TRUE`.
#' @noRd
normalise_mvgam_sampling_statements <- function(stancode,
                                                  normalize = TRUE) {
  checkmate::assert_flag(normalize)
  sc <- paste(as.character(stancode), collapse = "\n")
  if (!normalize || !nzchar(sc) || !grepl("~", sc, fixed = TRUE)) {
    return(stancode)
  }
  chunks <- strsplit(sc, ";", fixed = TRUE)[[1L]]
  rewritten <- vapply(chunks, function(chunk) {
    # A tilde inside a comment is a formula. Detection uses the code
    # with comments removed. The rewrite keeps the text before the
    # statement intact.
    lines <- strsplit(chunk, "\n", fixed = TRUE)[[1L]]
    bare <- stan_line_code(lines)
    first <- which(grepl("~", bare, fixed = TRUE))[1L]
    if (is.na(first)) {
      return(chunk)
    }
    # Lines before the statement hold indentation, blank space and
    # comments, which pass through unchanged. The statement runs from
    # its first line to the end of the chunk.
    prefix <- if (first > 1L) {
      paste0(paste(lines[seq_len(first - 1L)], collapse = "\n"), "\n")
    } else {
      ""
    }
    split <- split_block_opener(
      paste(bare[first:length(bare)], collapse = " ")
    )
    indent <- if (nzchar(split$opener)) "" else stan_line_indent(lines[first])
    # An argument list wrapped over several lines collapses to one
    # line. stanc reformats it.
    dist <- gsub("[[:space:]]+", " ",
                 trimws(sub("^[^~]*~", "", split$statement)))
    stmt <- stan_prior_statement(stan_statement_lhs(split$statement), dist,
                                 normalize = TRUE)
    paste0(prefix, split$opener, indent, sub(";$", "", stmt))
  }, character(1L), USE.NAMES = FALSE)
  # `strsplit()` drops the empty piece after a final `;`
  paste0(paste(rewritten, collapse = ";"), if (endsWith(sc, ";")) ";")
}


#' The brms program with the trend stanvars
#'
#' Calls `brms::make_stancode()` on the observation model with its own
#' stanvars and the trend's, sorted into declaration order.
#'
#' @param obs_setup List containing observation model setup
#' @param trend_stanvars Trend stanvars, or NULL
#' @return Character string of Stan code
#' @noRd
generate_base_stancode_with_stanvars <- function(obs_setup, trend_stanvars) {
  checkmate::assert_list(obs_setup)
  if (!is.null(trend_stanvars) &&
      !inherits(trend_stanvars, c("stanvar", "stanvars"))) {
    stop_wrong_class("The trend stanvars", trend_stanvars,
                     "a stanvar or stanvars")
  }

  # The trend computation must come before the code that adds the
  # trend to `mu`, which `sort_stanvars()` orders
  all_stanvars <- combine_stanvars(obs_setup$stanvars, trend_stanvars)
  if (!is.null(trend_stanvars)) {
    all_stanvars <- sort_stanvars(all_stanvars)
  }

  # `data2` holds the objects specials look up, such as `car()`
  # adjacency matrices and `cov_ranef()` covariance matrices. With
  # `threads`, brms writes the `reduce_sum` likelihood for its own
  # families. Closure-unit families declare their threading in their
  # own stanvars.
  # The cached code-generation options match the program the mock fit
  # was built under: `knots` fixes the basis, `drop_unused_levels` the
  # factor contrasts, `sample_prior` whether the prior is sampled and
  # `normalize` whether the sampling statements drop constants.
  do.call(brms::make_stancode, c(
    list(
      formula = obs_setup$formula,
      data = obs_setup$data,
      family = obs_setup$family,
      data2 = obs_setup$data2,
      stanvars = all_stanvars,
      threads = obs_setup$threads,
      prior = obs_setup$prior
    ),
    codegen_args_for(obs_setup$codegen, mvgam_codegen_stancode_options)
  ))
}

#' Trend stanvars for one model
#'
#' The stanvars a trend adds to the brms observation program: the
#' trend-formula parameters renamed with a `_trend` suffix, the arrays
#' mapping each observation to its trend cell, the trend generator's
#' own stanvars.
#'
#' @param trend_setup List containing trend model setup
#' @param trend_specs Trend specification. Its `dimensions` field,
#'   recorded by `extract_time_series_dimensions()`, holds the time
#'   grid and the observation-to-trend mappings.
#' @param response_name Response whose mapping a multivariate model
#'   uses, or NULL. With NULL and several mappings every response gets
#'   its own suffixed mapping arrays.
#' @param obs_setup Observation model setup
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object, or NULL
#' @noRd
extract_trend_stanvars_from_setup <- function(trend_setup, trend_specs,
                                              response_name = NULL,
                                              obs_setup = NULL,
                                              prior = NULL) {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_string(trend_specs$trend, .var.name = "trend_specs$trend")
  checkmate::assert_string(response_name, null.ok = TRUE)

  # `extract_and_validate_trend_components()` records the dimensions
  dimensions <- trend_specs$dimensions
  if (is.null(dimensions)) {
    stop_missing_fields("The trend specification", "dimensions")
  }
  data_info <- list(
    n_obs = dimensions$n_obs,
    n_series = dimensions$n_series,
    n_lv = trend_specs$n_lv,
    n_time = dimensions$n_time,
    # The occasions a response was actually seen at. The trend grid
    # runs past them on a padded frame or one given `newdata`. Anything
    # placed over the history counts these.
    n_time_observed = count_observed_times(
      data = obs_setup$data %||% trend_setup$data,
      time_var = dimensions$time_var,
      response_vars = unname(response_columns(obs_setup$formula))
    ),
    time_var = dimensions$time_var,
    series_var = dimensions$series_var,
    unique_times = dimensions$unique_times,
    # The occasions in the user's own units, from the axis record. The
    # gaps a continuous-time trend steps over are `diff()` of them.
    time_axis = dimensions$axes$time,
    unique_series = dimensions$unique_series,
    series_groups = dimensions$series_groups,
    row_time = dimensions$row_time,
    row_series = dimensions$row_series,
    # A logistic `PW()` takes its capacity column from the data,
    # transformed by every response's link
    data = obs_setup$data %||% trend_setup$data,
    family = obs_setup$family,
    families = formula_families(obs_setup$formula, obs_setup$family),
    # With `by = lv_axis()` the trend predictor joins each factor
    has_by_lv = isTRUE(dimensions$has_by_lv)
  )
  hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)
  data_info$n_groups <- hierarchical_info$n_groups
  data_info$n_subgroups <- hierarchical_info$n_subgroups

  # `extract_and_rename_trend_parameters()` creates `mu_trend` from
  # the trend formula. The trend generators use it without assigning
  # to it. A trend formula without terms declares no `mu_trend`. The
  # trend assembly then adds no predictor.
  brms_stanvars <- extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions,
    suffix = "_trend"
  )
  data_info$mu_trend_zero <- !any(grepl(
    "vector\\[[^]]*\\]\\s+mu_trend\\b",
    vapply(brms_stanvars, `[[`, character(1L), "scode")
  ))

  # A multivariate model without a chosen response maps every
  # response, each under its own suffix. Otherwise the chosen
  # response's mapping applies, or the mapping of a single response.
  mappings <- dimensions$mappings
  shared_mapping <- is.null(response_name) && length(mappings) > 1L
  mapping_stanvars <- if (shared_mapping) {
    checkmate::assert_list(mappings, names = "named")
    do.call(combine_stanvars, lapply(names(mappings), function(resp) {
      trend_mapping_stanvars(mappings[[resp]], paste0("_", resp), resp)
    }))
  } else {
    mapping <- if (!is.null(response_name) &&
                   response_name %in% names(mappings)) {
      mappings[[response_name]]
    } else if (length(mappings) == 1L) {
      mappings[[1L]]
    }
    if (!is.null(mapping)) {
      trend_mapping_stanvars(mapping, "", response_name %||% "response")
    }
  }

  trend_stanvars <- generate_trend_specific_stanvars(
    trend_specs, data_info, prior = prior
  )

  combine_stanvars(trend_setup$stanvars, brms_stanvars, mapping_stanvars,
                   trend_stanvars)
}

#' Stanvars mapping each observation to its trend cell
#'
#' `obs_trend_time` and `obs_trend_series` are data. The R methods
#' use them too. Transformed data turns each pair into
#' `obs_trend_idx`, the cell's position in the column-major
#' `to_vector(trend)`. The model block adds the trend to the predictor
#' with one gather through that index.
#'
#' @param mapping List holding `obs_trend_time` and `obs_trend_series`
#' @param suffix Suffix on the array names and on the observation
#'   count, `""` for a univariate program
#' @param label Name used in the error when a field is missing
#' @return A stanvars object
#' @noRd
trend_mapping_stanvars <- function(mapping, suffix, label) {
  checkmate::assert_list(mapping)
  checkmate::assert_string(suffix)
  missing_fields <- setdiff(c("obs_trend_time", "obs_trend_series"),
                            names(mapping))
  if (length(missing_fields) > 0L) {
    stop_missing_fields(
      paste0("The trend mapping for '", label, "'"), missing_fields
    )
  }
  # An index past its axis still lands inside `to_vector(trend)`, on
  # the next series. The bounds make Stan refuse it at data load.
  upper <- c(obs_trend_time = "N_time_trend",
             obs_trend_series = "N_series_trend")
  arrays <- lapply(names(upper), function(nm) {
    brms::stanvar(
      x = mapping[[nm]],
      name = paste0(nm, suffix),
      scode = paste0("array[N", suffix, "] int<lower=1, upper=",
                     upper[[nm]], "> ", nm, suffix, ";"),
      block = "data"
    )
  })
  idx <- paste0("obs_trend_idx", suffix)
  linear_index <- brms::stanvar(
    scode = paste0(
      "array[N", suffix, "] int ", idx, ";\n",
      "  for (n in 1:N", suffix, ") {\n",
      "    ", idx, "[n] = (obs_trend_series", suffix, "[n] - 1)",
      " * N_time_trend\n",
      "      + obs_trend_time", suffix, "[n];\n",
      "  }"
    ),
    block = "tdata"
  )
  do.call(combine_stanvars, c(arrays, list(linear_index)))
}

#' Line bounds of a Stan program block
#'
#' The end line is the brace that closes the block, found by brace
#' matching.
#'
#' @param code_lines Character vector of Stan code lines
#' @param block_name Block name, such as "model" or
#'   "transformed parameters"
#' @return List of `start_idx` and `end_idx`, or NULL when the program
#'   has no such block
#' @noRd
find_stan_block <- function(code_lines, block_name) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_string(block_name)

  bounds <- stan_block_bounds(code_lines, block_name)
  if (is.null(bounds)) {
    return(NULL)
  }

  # stan_block_bounds() ends the block at the last line when the braces
  # do not balance. Callers here splice code into the block. Given the
  # end of the program, a caller would write past it.
  end_idx <- find_matching_closing_brace(code_lines, bounds$start)
  if (is.na(end_idx)) {
    stop(insight::format_error(c(
      cli::format_inline("Cannot find end of {block_name} block."),
      x = "Stan code structure is invalid or malformed."
    )), call. = FALSE)
  }

  return(list(start_idx = bounds$start, end_idx = end_idx))
}

#' The line after which the likelihood code of a model block starts
#'
#' The line opening `if (!prior_only)`, or the line after it when that
#' line opens the brace. A model block without that
#' conditional gives its own opening line.
#'
#' @param code_lines Character vector of Stan code lines
#' @param model_block_info List with `start_idx` and `end_idx` from
#'   `find_stan_block()`
#' @return Integer line index
#' @noRd
find_prior_only_insertion_point <- function(code_lines, model_block_info) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_list(model_block_info, names = "named")
  checkmate::assert_names(names(model_block_info),
                          must.include = c("start_idx", "end_idx"))
  checkmate::assert_int(model_block_info$start_idx, lower = 1)
  checkmate::assert_int(model_block_info$end_idx,
                        lower = model_block_info$start_idx)

  span <- seq.int(model_block_info$start_idx + 1,
                  min(model_block_info$end_idx, length(code_lines)))
  hit <- span[grepl("if\\s*\\(\\s*!prior_only\\s*\\)", code_lines[span])][1L]
  if (is.na(hit)) {
    return(model_block_info$start_idx)
  }
  brace_next <- !grepl("{", code_lines[hit], fixed = TRUE) &&
    hit < length(code_lines) && grepl("^\\s*\\{", code_lines[hit + 1L])
  if (brace_next) hit + 1L else hit
}

#' Add the latent trend to every response's linear predictor
#'
#' brms writes each response's predictor into the observation program.
#' The trend is added to it as
#' `mu += to_vector(trend)[obs_trend_idx]`, through the index the trend
#' stanvars build. A univariate program
#' names these `mu`, `N` and `Y`. A multivariate one suffixes each with
#' the response key brms gives it.
#'
#' @param base_stancode Character string of the observation program
#'   with the trend stanvars already declared
#' @param resps Response keys as brms suffixes them, or `""` for a
#'   univariate program
#' @return The program with the trend added to each predictor
#' @noRd
inject_trend_into_linear_predictors <- function(base_stancode, resps) {
  checkmate::assert_string(base_stancode, min.chars = 1)
  checkmate::assert_character(resps, min.len = 1, any.missing = FALSE)
  code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]
  for (resp in resps) {
    code_lines <- inject_trend_for_response(code_lines, resp)
  }
  paste(code_lines, collapse = "\n")
}


#' Add the latent trend to one response's linear predictor
#'
#' A response whose likelihood brms wrote as a GLM call takes the
#' trend as `response_glm_call()` decides. Every other predictor is a
#' `mu` the program builds. The trend is added once it is built.
#'
#' @param code_lines Character vector of Stan source lines
#' @param resp The response key, or `""` for a univariate program
#' @return `code_lines` with the trend added to that response
#' @noRd
inject_trend_for_response <- function(code_lines, resp) {
  checkmate::assert_character(code_lines, min.len = 1)
  checkmate::assert_string(resp)
  model <- find_stan_block(code_lines, "model")
  if (is.null(model)) {
    stop_mvgam_fault(
      "The observation program lacks a model block.",
      "The trend is added to the predictor that block builds."
    )
  }
  glm_call <- response_glm_call(code_lines, resp)
  if (!is.null(glm_call) && !identical(glm_call$route, "declared")) {
    return(rewrite_glm_for_trend(code_lines, model, resp, glm_call))
  }
  add_trend_to_built_predictor(code_lines, model, resp)
}


#' The trend cell of every observation of one response
#'
#' @param resp The response key, or `""` for a univariate program
#' @return A Stan expression for an `N`-vector
#' @noRd
trend_gather <- function(resp) {
  paste0("to_vector(trend)[obs_trend_idx", predictor_suffix(resp), "]")
}


#' The Stan statement that adds the trend to one response's predictor
#'
#' @param resp The response key, or `""` for a univariate program
#' @param indent Leading whitespace for the statement
#' @return One Stan line
#' @noRd
trend_addition_lines <- function(resp, indent) {
  checkmate::assert_string(indent)
  paste0(indent, response_mu(resp), " += ", trend_gather(resp), ";")
}


#' Put the trend into a response's GLM likelihood
#'
#' On the `"alpha"` route the trend joins the intercept argument,
#' which Stan's GLM densities take as a vector. On the `"density"`
#' route, for a GLM without an intercept, the program builds
#' `mu = X * b` plus the trend. The call then becomes the family's
#' vectorised density on `mu`, as in
#' `ordered_logistic_lpmf(Y | mu, Intercept)`.
#'
#' @param code_lines Character vector of Stan source lines
#' @param model List with the model block's `start_idx` and `end_idx`
#' @param resp The response key, or `""` for a univariate program
#' @param glm_call The response's GLM call from `response_glm_call()`,
#'   on the `"alpha"` or `"density"` route
#' @return `code_lines` with the call rewritten
#' @noRd
rewrite_glm_for_trend <- function(code_lines, model, resp, glm_call) {
  checkmate::assert_choice(glm_call$route, c("alpha", "density"))
  at <- glm_call$at
  glm_type <- glm_call$glm_type
  params <- glm_call$params
  arguments <- unlist(params[glm_roles_for(glm_type)])
  if (identical(glm_call$route, "alpha")) {
    # brms writes the intercept `0` for a formula without one
    arguments[["intercept"]] <- if (identical(params$intercept, "0")) {
      trend_gather(resp)
    } else {
      paste0(params$intercept, " + ", trend_gather(resp))
    }
    code_lines[at] <- rewrite_glm_call(code_lines[at], glm_type,
                                       params$y_var, unname(arguments))
    return(code_lines)
  }
  mu_var <- response_mu(resp)
  code_lines[at] <- rewrite_glm_call(
    code_lines[at], glm_type, params$y_var,
    c(mu_var, params$cutpoints), glm = FALSE
  )
  # A prior-only run carries no trend addition. The lines go inside
  # the `if (!prior_only)` guard where brms wrote one.
  indent <- stan_line_indent(code_lines[at])
  addition <- c(
    paste0(indent, "vector[N", predictor_suffix(resp), "] ", mu_var,
           " = ", params$design_matrix, " * ", params$coefficients, ";"),
    trend_addition_lines(resp, indent)
  )
  append(code_lines, addition,
         after = find_prior_only_insertion_point(code_lines, model))
}


#' Add the trend to a predictor the program builds
#'
#' brms builds `mu` in statements that add to it (`mu += ...`,
#' `mu[n] += ...`) or assign it from other terms (`mu[n] = ...` in a
#' non-linear model). Some families then transform it in place, with an
#' assignment whose right-hand side uses `mu`: an inverse link
#' (`mu = inv_logit(mu)`) or the skew-normal mean shift
#' (`mu[n] = mu[n] - ...`). The trend is part of the predictor. It is
#' added before the first transform, or after the last building
#' statement where there is none. A statement inside a loop is placed
#' by its loop. The trend addition then runs once, outside the loop.
#'
#' @param code_lines Character vector of Stan source lines
#' @param model List with the model block's `start_idx` and `end_idx`
#' @param resp The response key, or `""` for a univariate program
#' @return `code_lines` with the trend addition inserted
#' @noRd
add_trend_to_built_predictor <- function(code_lines, model, resp) {
  mu_var <- response_mu(resp)
  model_range <- seq.int(model$start_idx, model$end_idx)
  # A statement assigning into `mu`, possibly declaring it or indexing
  # it, with the loop header brms sometimes writes on the same line
  assign_re <- paste0(
    "^\\s*(for\\s*\\([^)]*\\)\\s*\\{?\\s*)?",
    "(vector\\[[^]]*\\]\\s+)?", mu_var, "(\\[[^]]*\\])?\\s*",
    "(\\+=|=(?!=))(.*)$"
  )
  hits <- model_range[grepl(assign_re, code_lines[model_range],
                            perl = TRUE)]
  parts <- regmatches(code_lines[hits],
                      regexec(assign_re, code_lines[hits], perl = TRUE))
  reads_mu <- vapply(parts, function(p) {
    grepl(paste0("\\b", mu_var, "\\b"), p[6L], perl = TRUE)
  }, logical(1L))
  declares <- vapply(parts, function(p) nzchar(p[3L]), logical(1L))
  assigns <- vapply(parts, function(p) identical(p[5L], "="),
                    logical(1L))
  is_transform <- assigns & reads_mu & !declares
  transforms <- hits[is_transform]
  builds <- hits[!is_transform]
  if (!length(builds)) {
    stop_mvgam_fault(
      paste0("The model block lacks a statement building '", mu_var, "'."),
      "The trend joins the predictor where the model block builds it."
    )
  }
  if (length(transforms)) {
    first <- min(transforms)
    if (any(builds > first)) {
      stop_mvgam_fault(
        paste0("'", mu_var, "' is transformed before it is fully built."),
        "The trend joins after the last build and before the first transform."
      )
    }
    loop <- enclosing_stan_loop(code_lines, first, model$start_idx)
    anchor <- if (is.na(loop$start)) first else loop$start
    after <- anchor - 1L
  } else {
    last <- max(builds)
    loop <- enclosing_stan_loop(code_lines, last, model$start_idx)
    anchor <- if (is.na(loop$start)) last else loop$start
    after <- if (is.na(loop$end)) last else loop$end
  }
  indent <- stan_line_indent(code_lines[anchor])
  append(code_lines, trend_addition_lines(resp, indent), after = after)
}


#' The loop enclosing a Stan statement
#'
#' A statement written on the loop's own line (`for (n in 1:N) mu[n]
#' += ...;`) is its own loop. Otherwise the innermost `for` above the
#' statement whose braces enclose it is returned.
#'
#' @param lines Character vector of Stan source lines
#' @param idx Line index of the statement
#' @param lower First line of the enclosing block
#' @return List with the loop's `start` and `end` lines, both `NA`
#'   outside a loop
#' @noRd
enclosing_stan_loop <- function(lines, idx, lower) {
  for_re <- "^\\s*for\\s*\\("
  if (grepl(for_re, lines[idx])) {
    end <- if (grepl("\\{", lines[idx])) {
      find_matching_closing_brace(lines, idx)
    } else {
      idx
    }
    return(list(start = idx, end = end))
  }
  for (j in rev(seq.int(lower, idx - 1L))) {
    if (grepl(for_re, lines[j]) && grepl("\\{", lines[j])) {
      end <- find_matching_closing_brace(lines, j)
      if (!is.na(end) && end >= idx) {
        return(list(start = j, end = end))
      }
    }
  }
  list(start = NA_integer_, end = NA_integer_)
}


#' Stan data for the brms observation program
#'
#' Calls `brms::make_standata()`. A multi-response model then takes
#' each response's own observed rows through
#' `expand_per_response_standata()`.
#'
#' @param formula Formula for the model
#' @param data Data for the model
#' @param family Family specification
#' @param stanvars Optional stanvars
#' @param data2 Objects the brms specials look up, such as `car()`
#'   adjacency matrices and `cov_ranef()` covariance matrices
#' @param codegen A list from `mvgam_codegen_options()`, or NULL
#' @param prior The `brmsprior` the program was generated under.
#'   Shrinkage priors (`horseshoe()`, `R2D2()`) and mixture families
#'   declare their own Stan data. A program built with one needs the
#'   same one here, or it declares variables nothing supplies.
#' @param threads Thread count. brms emits `grainsize` when it is set.
#' @return List of Stan data
#' @noRd
generate_base_brms_standata <- function(formula, data, family = gaussian(),
                                        stanvars = NULL, data2 = NULL,
                                        codegen = NULL, prior = NULL,
                                        threads = NULL) {
  checkmate::assert(
    checkmate::check_class(formula, "formula"),
    checkmate::check_class(formula, "bform"),
    .var.name = "formula"
  )
  checkmate::assert_data_frame(data)

  standata <- do.call(brms::make_standata, c(
    list(
      formula = formula,
      data = data,
      family = family,
      stanvars = stanvars,
      data2 = data2,
      prior = prior,
      threads = threads
    ),
    codegen_args_for(codegen, mvgam_codegen_standata_options)
  ))

  # brms deletes a row of a multi-response model when any response is
  # missing there. Each response keeps its own observed rows instead,
  # and informs the shared latent state at every time it was seen.
  if (inherits(formula, "mvbrmsformula")) {
    standata <- expand_per_response_standata(
      combined_sd = standata,
      formula     = formula,
      data        = data,
      family      = family,
      stanvars    = stanvars,
      data2       = data2,
      codegen     = codegen
    )
  }

  standata
}


#' Substitute per-response data into a multi-response combined standata
#'
#' brms deletes a row of a multi-response model when any response is
#' missing there. mvgam keeps each response's own observed rows, which
#' informs the shared latent state at every time any response was
#' observed. Each response's `bf()` formula is passed to brms with the
#' rows that response observed. Its data arrays then replace the
#' combined ones under brms's multi-response names.
#'
#' `map_combined_key_to_single()` strips `_<resp>` from each combined
#' key, at the end or in the middle. The result is matched against the
#' single-response standata. That covers every key shape brms emits:
#' * trailing suffix: `Y_<resp>`, `K_<resp>`, `Xs_<resp>`
#' * mid-position suffix: `nb_<resp>_1`, `Zs_<resp>_1_1`, `Z_1_<resp>_1`
#' * dpar prefix: `K_sigma_<resp>`, `X_shape_<resp>`
#' Keys that contain no response name (`M_1`, `NC_1`, `prior_only`,
#' `N`) are shared across responses and left untouched.
#'
#' Data with every response observed in every row return unchanged.
#'
#' @param combined_sd Combined standata from `brms::make_standata`
#'   on the multi-response formula.
#' @param formula `mvbrmsformula` object, a sum of `bf()` formulas.
#' @param data The original data frame (may contain NAs).
#' @param family The family given with the formula. A response
#'   formula naming no family of its own is rebuilt under it.
#' @param stanvars Optional stanvars passed through to brms.
#' @param data2 Optional auxiliary data passed through to brms.
#' @param codegen A list from `mvgam_codegen_options()`, or NULL. The
#'   prior is left out. The combined prior names parameters of other
#'   responses, which brms rejects for a single response. Stan data a
#'   prior declares comes from the combined call.
#' @return The combined standata with per-response data arrays
#'   substituted in and the global `N` set to the maximum
#'   per-response valid row count.
#' @noRd
expand_per_response_standata <- function(combined_sd, formula, data,
                                          family = NULL,
                                          stanvars = NULL, data2 = NULL,
                                          codegen = NULL) {
  bf_list <- response_formulas(formula)
  if (length(bf_list) < 2L) return(combined_sd)

  # Columns index the data and keys index the standata. brms writes
  # the response column `y_1` under the key `y1`, as `N_y1` and
  # `Y_y1`. A key taken from the column misses every response whose
  # name holds a `_` or a `.`.
  columns <- response_columns(formula)
  keys <- names(columns)
  families <- formula_families(formula, family)

  # Fast path: every response is fully observed -> brms's
  # listwise-deleted standata already equals the per-response one.
  observed <- lapply(columns, function(col) !is.na(data[[col]]))
  if (all(vapply(observed, all, logical(1L)))) return(combined_sd)

  for (key in keys) {
    keep_i <- observed[[key]]
    if (sum(keep_i) == 0L) {
      stop(insight::format_error(c(
        cli::format_inline(
          "Response variable {.field {columns[[key]]}} has no observed values."
        ),
        i = "A multi-response model needs observations of every response."
      )), call. = FALSE)
    }
    data_i <- data[keep_i, , drop = FALSE]

    # `data2` passes unchanged. It usually holds objects indexed by
    # group, such as covariance matrices. brms refuses an object
    # indexed by data row, whose length differs from the response's
    # row count.
    single_sd <- do.call(brms::make_standata, c(
      list(
        formula  = bf_list[[key]],
        data     = data_i,
        family   = families[[key]],
        stanvars = stanvars,
        data2    = data2
      ),
      codegen_args_for(codegen, mvgam_codegen_standata_options)
    ))
    single_keys <- names(single_sd)

    for (combined_key in names(combined_sd)) {
      single_key <- map_combined_key_to_single(
        combined_key, key, single_keys
      )
      if (!is.null(single_key)) {
        combined_sd[[combined_key]] <- single_sd[[single_key]]
      }
    }
  }

  # The global `N` takes the largest response count, which keeps its
  # `int<lower=1>` declaration valid.
  resp_Ns <- vapply(keys, function(key) {
    as.integer(combined_sd[[paste0("N_", key)]])
  }, integer(1L))
  combined_sd$N <- max(resp_Ns)

  # Every per-response count, response, design matrix and group index
  # must hold one entry per observed row of that response. A brms key
  # shape `map_combined_key_to_single()` fails to strip shows up here.
  for (resp_i in keys) {
    expected <- sum(observed[[resp_i]])
    yk <- paste0("Y_", resp_i)
    xk <- paste0("X_", resp_i)
    j_keys <- grep(paste0("^J_[0-9]+_", resp_i, "$"),
                   names(combined_sd), value = TRUE)
    sizes <- c(
      stats::setNames(resp_Ns[[resp_i]], paste0("N_", resp_i)),
      if (!is.null(combined_sd[[yk]])) {
        stats::setNames(length(combined_sd[[yk]]), yk)
      },
      if (is.matrix(combined_sd[[xk]])) {
        stats::setNames(nrow(combined_sd[[xk]]), xk)
      },
      vapply(combined_sd[j_keys], length, integer(1L))
    )
    wrong <- sizes[sizes != expected]
    if (length(wrong)) {
      stop_mvgam_fault(
        paste0("Response '", resp_i, "' has ", expected,
               " observed rows in its Stan data."),
        paste0("Sizes that differ: ",
               paste0(names(wrong), " = ", wrong, collapse = ", "), ".")
      )
    }
  }

  combined_sd
}


#' The key in one response's standata matching a combined key
#'
#' Strips `_<resp>` from the end of the combined key, then from the
#' middle. A result found among the single-response keys is kept.
#' A key shared by the responses or belonging to another response
#' gives `NULL`.
#'
#' @noRd
map_combined_key_to_single <- function(combined_key, resp_name,
                                         single_keys) {
  # Trailing `_<resp>`: e.g. `Y_count`, `K_sigma_contvar`, `J_1_count`.
  cand <- sub(paste0("_", resp_name, "$"), "", combined_key)
  if (cand != combined_key && cand %in% single_keys) return(cand)

  # Mid-position `_<resp>_`: e.g. `Z_1_count_1`, `Zs_biomass_1_1`,
  # `nb_biomass_1`, `knots_biomass_1`.
  cand <- sub(paste0("_", resp_name, "_"), "_", combined_key)
  if (cand != combined_key && cand %in% single_keys) return(cand)

  NULL
}


# =============================================================================
# Trend Stan code generators
# =============================================================================
# Each trend type emits its own parameters, priors and dynamics. The
# registry records which generator a type uses.


#' Combine stanvar components with brms's `c()` method
#'
#' @param ... NULL, a stanvar, a stanvars collection, or a list of
#'   them
#' @return A stanvars collection, or NULL when every component is NULL
#' @noRd
combine_stanvars <- function(...) {
  is_stanvar <- function(x) inherits(x, c("stanvar", "stanvars"))
  flat <- list()
  for (component in list(...)) {
    items <- if (is_stanvar(component)) list(component) else component
    for (item in items) {
      if (is.null(item)) next
      if (!is_stanvar(item)) {
        stop_wrong_class("A stanvar component", item,
                         "a stanvar or stanvars")
      }
      flat <- c(flat, list(item))
    }
  }
  if (length(flat) == 0L) {
    return(NULL)
  }
  Reduce(c, flat)
}

# =============================================================================
# Shared innovation system
# =============================================================================
# A trend registered with `shared_innovations = TRUE` takes its scale,
# correlation, grouped correlations and heavy tails from here. The
# trend's own generator applies a moving-average term.

#' Stanvars of the shared innovations
#'
#' The standard innovations with their scale and correlation. They
#' build `scaled_innovations_trend`, the input to the dynamics of every
#' shared-innovation trend.
#'
#' @param cor Logical, whether to include correlation parameters
#' @param unit_factors Logical, `TRUE` when the model samples every
#'   loading, which fixes the factor innovations at unit scale and
#'   zero correlation (see `samples_factor_loadings()`)
#' @param mgp_scale Logical, `TRUE` under multiplicative gamma process
#'   shrinkage of the loadings
#' @param hierarchical_info List with hierarchical structure info, or
#'   NULL
#' @return A stanvars object
#' @noRd
generate_shared_innovation_stanvars <- function(cor = FALSE,
                                               unit_factors = FALSE,
                                               mgp_scale = FALSE,
                                               hierarchical_info = NULL) {
  checkmate::assert_flag(cor)
  checkmate::assert_flag(mgp_scale)
  checkmate::assert_flag(unit_factors)

  effective_dim <- "N_lv_trend"
  is_hierarchical <- isTRUE(hierarchical_info$has_groups)

  # A grouped trend scales its innovations by `sigma_group_trend`, from
  # `generate_hierarchical_correlation_parameters()`. Under MGP
  # shrinkage the prior gives each column a scale through `Psi_diag`.
  # A second, sampled `sigma_trend` would leave only the product of
  # the two identified. The scale is derived below instead.
  sigma_stanvar <- if (!is_hierarchical && !mgp_scale && !unit_factors) {
    brms::stanvar(
      name = "sigma_trend",
      scode = paste0("vector<lower=0>[", effective_dim, "] sigma_trend;"),
      block = "parameters"
    )
  }
  # Unit-scale factors fix the correlation at the identity below and
  # declare no parameter for it. Sampled scales derive `Sigma_trend`
  # from the Cholesky factor that scales the innovations below.
  scaled_cor <- !is_hierarchical && cor && !unit_factors
  correlation_stanvars <- if (!is_hierarchical && cor) {
    innovation_correlation_stanvars(effective_dim, sampled = !unit_factors,
                                    covariance = !scaled_cor)
  }
  innovations_trend_stanvar <- brms::stanvar(
    name = "innovations_trend",
    scode = paste0("matrix[N_time_trend, ", effective_dim,
                   "] innovations_trend;"),
    block = "parameters"
  )

  # A derived scale is declared in the block that uses it. Stan needs
  # the declaration before the first use.
  sigma_decl <- if (mgp_scale) {
    paste0(
      "\n    ", mgp_psi_diag_scode(effective_dim),
      "\n    vector<lower=0>[", effective_dim,
      "] sigma_trend = sqrt(Psi_diag);"
    )
  } else if (unit_factors) {
    unit_factor_scale_scode(effective_dim)
  } else {
    ""
  }
  if (unit_factors && cor && !is_hierarchical) {
    sigma_decl <- paste0(
      sigma_decl, "\n    cholesky_factor_corr[", effective_dim,
      "] L_Omega_trend = identity_matrix(", effective_dim, ");"
    )
  }
  scaled_decl <- paste0(
    "\n    matrix[N_time_trend, ", effective_dim, "] scaled_innovations_trend;"
  )
  final_innovations_code <- if (is_hierarchical) {
    paste0("\n    // Innovations scaled by each group's Cholesky factor,",
           " filled below", scaled_decl)
  } else if (scaled_cor) {
    paste0(
      scaled_decl,
      "\n    cov_matrix[", effective_dim, "] Sigma_trend;\n\n",
      "    // Standard normals times the Cholesky factor of Sigma_trend\n",
      "    {\n",
      "      matrix[", effective_dim, ", ", effective_dim,
      "] L_Sigma_trend\n",
      "        = diag_pre_multiply(sigma_trend, L_Omega_trend);\n",
      "      Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);\n",
      "      scaled_innovations_trend = innovations_trend * L_Sigma_trend';\n",
      "    }"
    )
  } else {
    paste0(
      scaled_decl, "\n    ",
      if (unit_factors) {
        "scaled_innovations_trend = innovations_trend;"
      } else {
        independent_scaling_stancode()
      }
    )
  }
  scaled_innovations_stanvar <- brms::stanvar(
    name = "scaled_innovations_trend",
    scode = paste0(sigma_decl, final_innovations_code),
    block = "tparameters"
  )

  combine_stanvars(sigma_stanvar, correlation_stanvars,
                   innovations_trend_stanvar, scaled_innovations_stanvar)
}

#' Stanvars declaring a trend's innovation correlation
#'
#' `L_Omega_trend` is the Cholesky factor of the correlation among the
#' innovations. `Sigma_trend` is the covariance it gives with
#' `sigma_trend`. The shared innovation system and `CAR()` take their
#' declarations from here. The post-fit extractors then meet one
#' spelling. A caller holding the Cholesky factor of the covariance
#' computes `Sigma_trend` from it.
#'
#' The factor is declared at one latent dimension too, where it is a
#' 1x1 identity. The post-fit extractors take `L_Omega_trend` without
#' first checking the dimension.
#'
#' @param effective_dim Stan name of the innovations' dimension
#' @param sampled Whether to declare `L_Omega_trend` as a parameter.
#'   `FALSE` leaves its declaration to the caller.
#' @param scale Stan expression for a matrix that multiplies the
#'   covariance element by element, or `NULL` for none. `CAR()` passes
#'   the ratio of its unit-gap innovation covariance to its diffusion
#'   correlation.
#' @param covariance Whether to declare `Sigma_trend`. `FALSE` leaves
#'   it to the caller.
#' @return List of stanvar objects
#' @noRd
innovation_correlation_stanvars <- function(effective_dim = "N_lv_trend",
                                            sampled = TRUE,
                                            scale = NULL,
                                            covariance = TRUE) {
  checkmate::assert_string(effective_dim, min.chars = 1L)
  checkmate::assert_flag(sampled)
  checkmate::assert_string(scale, min.chars = 1L, null.ok = TRUE)
  checkmate::assert_flag(covariance)
  l_omega <- brms::stanvar(
    name = "L_Omega_trend",
    scode = paste0("cholesky_factor_corr[", effective_dim,
                   "] L_Omega_trend;"),
    block = "parameters"
  )
  # The VAR generator and the post-fit extractors take `Sigma_trend`
  # as a covariance. The scaled Cholesky factor is an intermediate.
  sigma_matrix <- brms::stanvar(
    name = "Sigma_trend",
    scode = paste0(
      "cov_matrix[", effective_dim, "] Sigma_trend = ",
      "multiply_lower_tri_self_transpose(",
      "diag_pre_multiply(sigma_trend, L_Omega_trend))",
      if (!is.null(scale)) paste0("\n    .* ", scale),
      ";"
    ),
    block = "tparameters"
  )
  c(if (sampled) list(l_omega), if (covariance) list(sigma_matrix))
}

#' Are the trend innovations Gaussian?
#'
#' `df = Inf` is the default and gives Gaussian innovations. A t with
#' infinite degrees of freedom is a normal, which makes `Inf` the exact
#' value.
#' @param df Degrees of freedom, `Inf`, `NA` or a number above 2
#' @return Logical scalar
#' @noRd
is_gaussian_df <- function(df) {
  is.null(df) || (length(df) == 1L && !is.na(df) && is.infinite(df))
}

#' Prior for one vector of identity-scale innovations
#'
#' The innovation law for a single vector. The matrix form loops over
#' rows and calls this for each. `init_innovations_trend` supplies the
#' extra standard variates an ARMA start needs. It takes the same law
#' as an innovation row.
#'
#' @param target Stan expression for the vector
#' @param effective_dim Number of latent series
#' @param df `Inf` for Gaussian innovations, `NA` to estimate the
#'   degrees of freedom, or a finite number above 2 to fix them
#' @return Character vector of Stan statements
#' @noRd
innovation_vector_prior <- function(target, effective_dim, df = Inf) {
  checkmate::assert_string(target, min.chars = 1L)
  checkmate::assert(
    checkmate::check_string(effective_dim, min.chars = 1L),
    checkmate::check_integerish(effective_dim, lower = 1L, len = 1L),
    .var.name = "effective_dim"
  )
  checkmate::assert_number(df, na.ok = TRUE, lower = 2)
  if (is_gaussian_df(df)) {
    return(paste0(target, " ~ std_normal();"))
  }
  nu <- if (is.na(df)) "nu_trend" else format(df, digits = 15)
  c(
    paste0(target, " ~ multi_student_t_cholesky(", nu, ","),
    paste0("  rep_vector(0.0, ", effective_dim, "),"),
    paste0("  identity_matrix(", effective_dim, "));")
  )
}

#' Stan sampling statement for the trend innovations
#'
#' Innovations are drawn from a standard multivariate t when `df` is
#' finite. Two properties make this the right form.
#'
#' A linear map of a multivariate t is again a multivariate t, with the
#' mapped scale matrix. The innovations are sampled at the identity
#' scale. The `L_Sigma_trend` and `sigma_trend` transforms in
#' transformed parameters apply unchanged. A comparison against
#' `mvtnorm::rmvt` confirmed this.
#'
#' The identity scale still ties the series together. The components
#' are uncorrelated but share the latent scale inside the density.
#' Large innovations then tend to arrive at the same time across
#' series, with signs and magnitudes set by each component's own
#' standard normal. Element-wise `student_t()` gives each series an
#' independent tail, with extremes unaligned in time.
#'
#' The scale mixture is left implicit. Written out with an auxiliary
#' variable per time point it funnels: on 200 to 400 time points it
#' gave 3 to 14 times less effective sample size for the degrees of
#' freedom and reached R-hat 1.072.
#'
#' @param effective_dim Number of latent series
#' @param df `Inf` for Gaussian innovations, `NA` to estimate the
#'   degrees of freedom, or a finite number above 2 to fix them
#' @return Character vector of Stan statements
#' @noRd
innovation_sampling_code <- function(effective_dim, df = Inf) {
  # `effective_dim` is either a Stan dimension name such as
  # "N_lv_trend" or a literal count, depending on the call site.
  checkmate::assert(
    checkmate::check_string(effective_dim, min.chars = 1L),
    checkmate::check_integerish(effective_dim, lower = 1L, len = 1L),
    .var.name = "effective_dim"
  )
  if (is_gaussian_df(df)) {
    return("to_vector(innovations_trend) ~ std_normal();")
  }
  c(
    "for (t_inn in 1 : N_time_trend) {",
    paste0("  ", innovation_vector_prior("innovations_trend[t_inn]'",
                                         effective_dim, df)),
    "}"
  )
}

#' Parameter and prior stanvars for estimated innovation degrees of freedom
#'
#' Only emitted when `df` is `NA`. The AR stationary initialisation
#' divides by `sqrt(1 - phi^2)`, which presumes innovations with a
#' finite second moment. A t has one only above 2 degrees of freedom,
#' which sets the lower bound.
#'
#' The `gamma(4, 0.3)` default was chosen on a prior predictive check
#' of the largest multiplicative excursion over a 200-step series at
#' typical trend scale. It admits a hundred-fold shock at the 99th
#' percentile against roughly ten-fold under Gaussian innovations,
#' while placing only 0.02 percent of its mass above 50 where the
#' model cannot be told apart from a Gaussian one.
#'
#' @param df Degrees of freedom specification
#' @param prior A `brmsprior` or NULL
#' @return A `stanvars` object, or `NULL` when nothing is estimated
#' @noRd
nu_trend_stanvars <- function(df, prior = NULL) {
  checkmate::assert_number(df, na.ok = TRUE, lower = 2)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  if (is_gaussian_df(df) || !is.na(df)) {
    return(NULL)
  }
  # The shared prior lookup takes a user override, as for
  # `sigma_trend`: `prior(..., class = "nu_trend")`.
  nu_prior <- get_trend_parameter_prior(prior, "nu_trend")
  combine_stanvars(
    brms::stanvar(
      name = "nu_trend",
      scode = "  real<lower=2> nu_trend;",
      block = "parameters"
    ),
    brms::stanvar(
      name = "nu_trend_prior",
      scode = glue::glue("  nu_trend ~ {nu_prior};"),
      block = "model"
    )
  )
}

#' Model-block stanvars for the shared innovations
#'
#' The sampling statement for the standard innovations with the priors
#' on their scale and correlation. Estimated degrees of freedom add
#' their own parameter and prior. A grouped trend takes its scale and
#' correlation priors from `generate_hierarchical_correlation_model()`.
#'
#' @param effective_dim Stan name or count of the innovations' dimension
#' @param cor Logical, whether correlation parameters exist
#' @param is_hierarchical Logical, whether the trend is grouped
#' @param prior A `brmsprior` or NULL
#' @param df Innovation degrees of freedom: `Inf` for Gaussian, `NA` to
#'   estimate, or a finite number above 2 to fix
#' @param mgp_scale Logical, `TRUE` under multiplicative gamma process
#'   shrinkage, which derives `sigma_trend`
#' @param unit_factors Logical, `TRUE` when sampled loadings fix the
#'   factor scale and correlation
#' @param trend_obj The trend specification, for the default
#'   `sigma_trend` prior
#' @return A stanvars object
#' @noRd
generate_innovation_model <- function(effective_dim, cor = FALSE,
                                      is_hierarchical = FALSE,
                                      prior = NULL, df = Inf,
                                      mgp_scale = FALSE,
                                      unit_factors = FALSE,
                                      trend_obj = NULL) {
  checkmate::assert_flag(cor)
  checkmate::assert_flag(is_hierarchical)
  checkmate::assert_flag(mgp_scale)
  checkmate::assert_flag(unit_factors)

  prior_code <- "// Shared innovation priors"
  if (!is_hierarchical) {
    # Under MGP shrinkage `sigma_trend` is the transformed parameter
    # `sqrt(Psi_diag)`, which the MGP prior already covers. A sampling
    # statement on it would be a second, contradictory prior.
    sigma_prior_str <- get_trend_parameter_prior(prior, "sigma_trend",
                                                 trend_obj = trend_obj)
    if (nzchar(sigma_prior_str) && !mgp_scale && !unit_factors) {
      prior_code <- c(prior_code,
                      paste0("sigma_trend ~ ", sigma_prior_str, ";"))
    }
    # The shared resolver takes a user prior on the correlation factor
    # and reports it in the prior table
    if (cor && !unit_factors) {
      prior_code <- c(prior_code, paste0(
        "L_Omega_trend ~ ",
        get_trend_parameter_prior(prior, "L_Omega_trend"), ";"
      ))
    }
  }
  prior_code <- c(prior_code, innovation_sampling_code(effective_dim, df))

  combine_stanvars(
    brms::stanvar(
      name = "innovation_priors",
      scode = paste(prior_code, collapse = "\n  "),
      block = "model"
    ),
    nu_trend_stanvars(df, prior)
  )
}

#' The number of series a trend runs on
#'
#' @param data_info Data information holding the resolved `n_series`
#' @return Integer scalar
#' @noRd
data_info_n_series <- function(data_info) {
  checkmate::assert_list(data_info)
  checkmate::assert_int(data_info$n_series, lower = 1L,
                        .var.name = "data_info$n_series")
  as.integer(data_info$n_series)
}


#' The group of each series, in axis order
#'
#' `extract_time_series_dimensions()` resolves it for every trend that
#' names `gr`. Stan subscripts `group_inds_trend` with it.
#'
#' @param data_info Data information holding `series_groups`
#' @return Vector with one group per series
#' @noRd
hierarchical_series_groups <- function(data_info) {
  checkmate::assert_list(data_info)
  series_groups <- data_info$series_groups
  if (is.null(series_groups)) {
    stop_mvgam_fault(
      "A hierarchical trend needs the group of each series.",
      "The data information lacks 'series_groups'."
    )
  }
  series_groups
}


#' The group counts of a grouped trend
#'
#' @param data_info Data information list
#' @param trend_specs Trend specification list
#' @return NULL for an ungrouped trend. Otherwise a list holding
#'   `has_groups`, `n_groups`, `n_subgroups` and `gr_var`.
#' @noRd
extract_hierarchical_info <- function(data_info, trend_specs) {
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_list(trend_specs, names = "named")
  if (!named_var(trend_specs$gr)) {
    return(NULL)
  }
  gr_var <- trend_specs$gr
  assert_grouping_columns(data_info$data, gr_var, trend_specs$subgr)

  unique_groups <- observed_levels(data_info$data[[gr_var]])
  n_groups <- length(unique_groups)

  if (n_groups < 1) {
    stop(insight::format_error(
      paste0("Grouping variable '", gr_var, "' has no unique values.")
    ), call. = FALSE)
  }

  # The number of series in each group, from the group of each entry
  # on the resolved series axis. The Stan program declares one block
  # size for every group. `validate_gr_balanced_groups()` has
  # established that the counts agree, which makes `max()` the shared
  # count.
  series_groups <- hierarchical_series_groups(data_info)
  n_subgroups <- max(as.integer(table(series_groups)))

  list(
    has_groups = TRUE,
    n_groups = n_groups,
    n_subgroups = n_subgroups,
    gr_var = gr_var
  )
}

#' Stanvars of a grouped trend's correlations
#'
#' The group data, the function mixing the global and group
#' correlations, their parameters and their priors.
#'
#' @param hierarchical_info From `extract_hierarchical_info()`, or NULL
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @param trend_specs The trend specification
#' @param scale_innovations Whether the group loop fills
#'   `scaled_innovations_trend`. A VAR samples its states directly and
#'   uses the group covariances alone.
#' @return A list of stanvars, or NULL for an ungrouped trend
#' @noRd
grouped_trend_stanvars <- function(hierarchical_info, data_info, prior,
                                   trend_specs, scale_innovations = TRUE) {
  if (is.null(hierarchical_info)) {
    return(NULL)
  }
  list(
    generate_hierarchical_data_structures(hierarchical_info, data_info),
    generate_hierarchical_functions(),
    generate_hierarchical_correlation_parameters(
      hierarchical_info$n_groups, hierarchical_info$n_subgroups,
      scale_innovations = scale_innovations
    ),
    generate_hierarchical_correlation_model(prior, trend_obj = trend_specs)
  )
}

#' Stanvars around the dynamics of a shared-innovation trend
#'
#' A trend registered with `shared_innovations = TRUE` turns
#' `scaled_innovations_trend` into states by its own recursion and
#' shares everything else. Its generator passes that recursion as
#' `dynamics`. This adds the loadings, the group correlations, a
#' first-order moving-average term, the map from states to series and
#' the loadings prior.
#'
#' @param dynamics List of the trend's own stanvars
#' @param trend_specs The trend specification
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @param rotated Factor-indexed quantities the dynamics add, rotated
#'   with `lv_trend`, as `generate_factor_model()` takes them
#' @return A stanvars object
#' @noRd
assemble_shared_innovation_trend <- function(dynamics, trend_specs,
                                             data_info, prior = NULL,
                                             rotated = list()) {
  checkmate::assert_list(dynamics)
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  is_factor_model <- is_factor_model_spec(trend_specs$n_lv,
                                          data_info_n_series(data_info))
  # Under `gr` the group loop fills `scaled_innovations_trend` before
  # the dynamics recurse on it
  hierarchical <- grouped_trend_stanvars(
    extract_hierarchical_info(data_info, trend_specs), data_info, prior,
    trend_specs
  )

  ma <- if (isTRUE(trend_specs$ma)) {
    list(
      brms::stanvar(
        name = "ma_parameters",
        scode = paste0("// MA coefficient\n",
                       "vector<lower=-1,upper=1>[N_lv_trend] theta1_trend;"),
        block = "parameters"
      ),
      generate_trend_priors_stanvar(
        param_names = "theta1_trend",
        prior = prior,
        stanvar_name = "ma_model",
        heading = "// MA coefficient prior"
      )
    )
  }

  combine_stanvars(
    generate_matrix_z_multiblock_stanvars(
      is_factor_model,
      fixed_Z = trend_specs$fixed_Z,
      family = data_info$family,
      prior = prior,
      trend_obj = trend_specs
    ),
    hierarchical,
    ma,
    dynamics,
    generate_trend_computation_tparameters(
      data_info,
      identity_z = z_is_identity(is_factor_model, trend_specs$fixed_Z)
    ),
    generate_factor_model(trend_specs, data_info, prior = prior,
                          rotated = rotated)
  )
}

#' Data stanvars for the trend dimensions
#'
#' `N_series_trend`, `N_lv_trend` and `N_time_trend`, which every trend
#' type declares. `N_time_trend` indexes the time axis of the states,
#' the innovations and the trend matrix. `N_trend`, declared with the
#' trend design, counts its rows: one per time and series.
#'
#' @param n_obs Number of observations, the time count when `n_time`
#'   is NULL
#' @param n_series Number of observed series
#' @param n_lv Number of latent variables
#' @param is_factor_model Logical. The caller decides, because `n_lv`
#'   defaults to `n_series` for a model without factors. A factor model
#'   at its ceiling, which `n_lv = n_series` reaches under an MGP
#'   loadings prior or a `by = lv_axis()` term, has the same count.
#' @param n_time Number of unique time points
#' @return A stanvars object
#' @noRd
generate_common_trend_data <- function(n_obs, n_series, n_lv,
                                       is_factor_model, n_time = NULL) {
  checkmate::assert_number(n_obs, lower = 1)
  checkmate::assert_number(n_series, lower = 1)
  checkmate::assert_number(n_lv, lower = 1)
  checkmate::assert_flag(is_factor_model)
  checkmate::assert_number(n_time, lower = 1, null.ok = TRUE)

  dims <- list(
    N_series_trend = n_series,
    N_lv_trend = if (is_factor_model) n_lv else n_series,
    N_time_trend = n_time %||% n_obs
  )
  do.call(combine_stanvars, lapply(names(dims), function(nm) {
    brms::stanvar(
      x = dims[[nm]], name = nm, scode = paste0("int<lower=1> ", nm, ";"),
      block = "data", position = "start"
    )
  }))
}

#' Sort stanvars by dependency
#'
#' Reorders stanvars into an order Stan can declare, which prevents
#' "identifier not in scope" errors.
#'
#' @param stanvars A stanvars object
#' @return The stanvars, reordered, with the same class
#' @details
#' The order, each level depending on the ones above it:
#' - the trend's dimensions (`N_trend`, `N_series_trend`, ...)
#' - whatever the trend's linear predictor is built from, which the
#'   names that predictor references identify
#' - the linear predictor `mu_trend` itself
#' - the latent states, the scaled innovations and the factor loadings
#' - the arrays indexing those dimensions, then everything else
#' - the trend matrix, which the states and `mu_trend` produce
#'
#' Within each level the original order is kept.
#' @noRd
sort_stanvars <- function(stanvars) {
  checkmate::assert_list(stanvars, null.ok = TRUE)
  if (length(stanvars) == 0L) {
    return(stanvars)
  }
  for (sv in stanvars) {
    if (!is.null(sv)) {
      checkmate::assert_list(sv, names = "named")
      checkmate::assert_string(sv$scode, min.chars = 1, null.ok = TRUE)
    }
  }

  # The code of each stanvar, as one string. A stanvar that delegates
  # to a shared system has none.
  scodes <- vapply(stanvars, function(sv) {
    if (is.character(sv$scode) && length(sv$scode) == 1L) sv$scode else ""
  }, character(1L))

  # The trend's linear predictor
  is_mu_trend <- grepl("vector\\s*\\[\\s*N_trend\\s*\\]\\s+mu_trend\\s*=",
                       scodes)

  # Whatever that predictor is built from. A group-level block computes
  # `r_1_1_trend` and a shrinkage prior computes `b_trend`. The
  # predictor adds both, which places each before it. The names come
  # from the predictor's own code and cover every term type. Comments
  # are prose, which the comparison leaves out.
  code_only <- function(scode) {
    paste(stan_line_code(strsplit(scode, "\n", fixed = TRUE)[[1L]]),
          collapse = "\n")
  }
  mu_references <- unique(unlist(lapply(
    scodes[is_mu_trend], function(scode) {
      setdiff(extract_stan_identifiers(scode), stan_function_calls(scode))
    }
  )))
  builds_mu_trend <- vapply(scodes, function(scode) {
    nzchar(scode) &&
      any(extract_computed_variables(code_only(scode)) %in% mu_references)
  }, logical(1L), USE.NAMES = FALSE)

  is_dimension <- grepl(paste0(
    "^\\s*int<lower=1>\\s+(N_series_trend|N_lv_trend|N_time_trend|",
    "N_trend)\\s*;"
  ), scodes)
  is_dimension_array <- grepl(
    "^\\s*array\\[.*\\b(N_series_trend|N_lv_trend)\\b.*\\]", scodes
  )
  is_foundation <- grepl(paste0(
    "matrix\\s*\\[\\s*N_time_trend\\s*,\\s*N_lv_trend\\s*\\]\\s+",
    "(lv_trend|scaled_innovations_trend)\\s*[;=]",
    "|\\bmatrix\\s*\\[[^]]*\\]\\s+Z\\s*[;=]"
  ), scodes)
  is_trend_matrix <- grepl(paste0(
    "matrix\\s*\\[\\s*N_time_trend\\s*,\\s*N_series_trend\\s*\\]\\s+",
    "trend\\s*[;=]"
  ), scodes)

  # Each stanvar takes the first level it matches
  level <- ifelse(!nzchar(scodes), "other",
    ifelse(is_dimension, "dimension",
    ifelse(is_dimension_array, "dimension_array",
    ifelse(is_mu_trend, "mu_trend",
    ifelse(builds_mu_trend, "mu_input",
    ifelse(is_foundation, "foundation",
    ifelse(is_trend_matrix, "trend_matrix", "other")))))))
  order_of_levels <- c("dimension", "mu_input", "mu_trend", "foundation",
                       "dimension_array", "other", "trend_matrix")
  sort_order <- order(match(level, order_of_levels))
  sorted_stanvars <- stanvars[sort_order]
  class(sorted_stanvars) <- class(stanvars)
  sorted_stanvars
}

#' Parameter stanvars for sampled loadings
#'
#' A simplex multi-response family (`diri()`, `multi()`, `categ()`)
#' needs every column of `Z` to sum to zero exactly, which removes the
#' softmax shift in each column. Stan's `sum_to_zero_vector[K]`
#' (Stan >= 2.36) holds `K - 1` unconstrained parameters under a
#' transform that gives them a unit-normal marginal under a flat prior.
#' A soft `sum(Z[, l]) ~ normal(0, sigma)` creates a ridge instead.
#' `Z` is built from `Z_cols` in transformed parameters. The rest of
#' the trend code uses `Z` unchanged.
#'
#' @param simplex Logical, `TRUE` for a simplex multi-response family
#' @return A stanvars object
#' @noRd
generate_matrix_z_parameters <- function(simplex = FALSE) {
  checkmate::assert_flag(simplex)
  if (!simplex) {
    return(brms::stanvar(
      name = "Z",
      scode = "matrix[N_series_trend, N_lv_trend] Z;",
      block = "parameters"
    ))
  }
  combine_stanvars(
    brms::stanvar(
      name = "Z_cols",
      scode = "  array[N_lv_trend] sum_to_zero_vector[N_series_trend] Z_cols;",
      block = "parameters"
    ),
    brms::stanvar(
      name = "Z_from_Z_cols",
      scode = paste(
        "  matrix[N_series_trend, N_lv_trend] Z;",
        "  for (l in 1:N_lv_trend) {",
        "    Z[, l] = Z_cols[l];",
        "  }",
        sep = "\n"
      ),
      block = "tparameters"
    )
  )
}

#' A fixed loadings matrix as Stan data
#'
#' The fully fixed `Z` a `trend_map` with no `NA` entries gives, for
#' every trend type.
#'
#' @param fixed_Z A numeric `n_series x n_lv` matrix from
#'   `normalise_trend_map()`, without `NA` entries
#' @return A stanvar declaring `Z` in the data block
#' @noRd
make_fixed_z_stanvars <- function(fixed_Z) {
  checkmate::assert_matrix(fixed_Z, mode = "numeric", any.missing = FALSE)
  brms::stanvar(
    x = fixed_Z,
    name = "Z",
    scode = "matrix[N_series_trend, N_lv_trend] Z;",
    block = "data"
  )
}


#' A loadings matrix with fixed and sampled entries
#'
#' A `trend_map` matrix marks sampled loadings with `NA`. The Stan
#' program receives:
#' - `Z_template`: data matrix with the `NA` entries set to 0
#' - `Z_is_free`: data integer matrix, 1 where the entry is sampled
#' - `N_free_Z`: data integer counting the sampled entries
#' - `Z_free_vec`: parameter vector of the sampled entries
#'
#' Transformed parameters fill `Z` column by column, taking the
#' template value or the next element of `Z_free_vec`. The sampled
#' entries take the `Z` prior from the table, as a fully sampled
#' loadings matrix does.
#'
#' @param fixed_Z A numeric `n_series x n_lv` matrix from
#'   `normalise_trend_map()` with at least one `NA`
#' @param prior A `brmsprior` carrying any user prior on class `Z`
#' @param trend_obj The trend specification, for the default `Z` prior
#' @return A stanvars object covering the data, parameters,
#'   transformed parameters and model blocks
#' @noRd
make_partial_z_stanvars <- function(fixed_Z, prior = NULL,
                                    trend_obj = NULL) {
  checkmate::assert_matrix(fixed_Z, mode = "numeric")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  if (!anyNA(fixed_Z)) {
    stop_mvgam_fault(
      "A partial loadings matrix was requested for a fully fixed 'Z'.",
      "'make_partial_z_stanvars()' takes a 'fixed_Z' with NA entries."
    )
  }
  free_mask <- is.na(fixed_Z)
  template <- fixed_Z
  template[free_mask] <- 0
  is_free_int <- matrix(as.integer(free_mask), nrow = nrow(fixed_Z))
  n_free <- as.integer(sum(free_mask))

  data_template <- brms::stanvar(
    x = template, name = "Z_template",
    scode = "matrix[N_series_trend, N_lv_trend] Z_template;",
    block = "data"
  )
  data_mask <- brms::stanvar(
    x = is_free_int, name = "Z_is_free",
    scode = paste0(
      "array[N_series_trend, N_lv_trend] ",
      "int<lower=0, upper=1> Z_is_free;"
    ),
    block = "data"
  )
  data_n_free <- brms::stanvar(
    x = n_free, name = "N_free_Z",
    scode = "int<lower=1> N_free_Z;",
    block = "data"
  )
  param_free <- brms::stanvar(
    name = "Z_free_vec",
    scode = "vector[N_free_Z] Z_free_vec;",
    block = "parameters"
  )
  z_assembly <- brms::stanvar(
    name = "Z",
    scode = paste0(
      "matrix[N_series_trend, N_lv_trend] Z;\n",
      "  {\n",
      "    int idx = 1;\n",
      "    for (j in 1:N_lv_trend) {\n",
      "      for (i in 1:N_series_trend) {\n",
      "        if (Z_is_free[i, j] == 1) {\n",
      "          Z[i, j] = Z_free_vec[idx];\n",
      "          idx += 1;\n",
      "        } else {\n",
      "          Z[i, j] = Z_template[i, j];\n",
      "        }\n",
      "      }\n",
      "    }\n",
      "  }"
    ),
    block = "tparameters"
  )
  z_prior <- brms::stanvar(
    name = "Z_free_prior",
    scode = paste0(
      "Z_free_vec ~ ",
      get_trend_parameter_prior(prior, "Z", trend_obj = trend_obj), ";"
    ),
    block = "model"
  )
  combine_stanvars(
    data_template, data_mask, data_n_free,
    param_free, z_assembly, z_prior
  )
}

#' Stanvars for the loadings matrix `Z`
#'
#' Four exclusive cases:
#' - A `fixed_Z` with `NA` entries: fixed and sampled entries, from
#'   `make_partial_z_stanvars()`. The user's fixed pattern stays on `Z`
#'   and takes no QR identification.
#' - A `fixed_Z` without `NA` entries: `Z` as data.
#' - A factor model: `Z` sampled, under the prior and, where
#'   `factor_identification()` asks for it, the QR rotation of
#'   `generate_factor_model()`.
#' - Otherwise: the identity `Z` in transformed data.
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param fixed_Z Optional user-supplied numeric `Z`
#' @param family The observation family. The columns of a sampled `Z`
#'   sum to zero under a simplex family.
#' @param prior A `brmsprior` carrying any user prior on class `Z`
#' @param trend_obj The trend specification, for the default `Z` prior
#' @return A stanvars object
#' @noRd
generate_matrix_z_multiblock_stanvars <- function(is_factor_model,
                                                  fixed_Z = NULL,
                                                  family = NULL,
                                                  prior = NULL,
                                                  trend_obj = NULL) {
  checkmate::assert_flag(is_factor_model)
  if (!is.null(fixed_Z)) {
    if (anyNA(fixed_Z)) {
      return(make_partial_z_stanvars(fixed_Z, prior = prior,
                                     trend_obj = trend_obj))
    }
    return(make_fixed_z_stanvars(fixed_Z))
  }
  if (z_is_identity(is_factor_model, fixed_Z)) {
    return(brms::stanvar(
      name = "Z",
      scode = paste0("matrix[N_series_trend, N_lv_trend] Z = ",
                     "diag_matrix(rep_vector(1.0, N_lv_trend));"),
      block = "tdata"
    ))
  }
  generate_matrix_z_parameters(simplex = is_simplex_response_family(family))
}

#' Whether the loadings `generate_matrix_z_multiblock_stanvars()` emits
#' are the identity
#'
#' @param is_factor_model Logical indicating if this is a factor model
#' @param fixed_Z Optional user-supplied numeric `Z`
#' @return Logical scalar
#' @noRd
z_is_identity <- function(is_factor_model, fixed_Z = NULL) {
  checkmate::assert_flag(is_factor_model)
  !is_factor_model && is.null(fixed_Z)
}

#' The prior on sampled loadings and their QR identification
#'
#' `Z` is sampled as an unconstrained matrix under its prior. Where
#' `factor_identification()` asks for a rotation, generated quantities
#' rotate each draw to a lower-triangular `Z_tilde` by a thin QR
#' decomposition (Heaps and Jermyn 2024). The factor paths rotate to
#' `lv_trend_tilde` with the same matrix.
#' `Z_tilde %*% t(lv_trend_tilde)` equals `Z %*% t(lv_trend)` in every
#' draw. `qr_thin_R()` returns a non-negative diagonal, which fixes the
#' sign of each factor. A VAR factor model also rotates its
#' coefficients, as `Phi_trend_tilde[lag] = Q * Phi_trend[lag] * Q'`.
#' Factors with their own coefficients are relabelled after sampling by
#' `relabel_factors()` and take no rotation.
#'
#' A simplex family constrains the columns of `Z` in their declaration,
#' in `generate_matrix_z_parameters()`.
#'
#' @param trend_specs The trend specification
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @param rotated The factor-indexed quantities a trend adds to
#'   `lv_trend`, rotated with it. A list of `paths`, `[N_time, N_lv]`
#'   matrices rotated as `lv_trend`; `vectors`, `[N_lv]` vectors
#'   rotated as `Q v`; and `transitions`, arrays of `[N_lv, N_lv]`
#'   matrices over the lags rotated as `Q A Q'`. A VAR passes its
#'   coefficients and a local linear trend its slope.
#' @return A stanvars object. NULL for a model without factors and for
#'   a `trend_map` that supplies `Z`.
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#' @noRd
generate_factor_model <- function(trend_specs, data_info, prior = NULL,
                                  rotated = list()) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_list(rotated)
  checkmate::assert_subset(names(rotated),
                           c("paths", "vectors", "transitions"))
  is_factor_model <- is_factor_model_spec(trend_specs$n_lv,
                                          data_info_n_series(data_info))
  if (!is_factor_model || !is.null(trend_specs$fixed_Z)) {
    return(NULL)
  }

  # A structured loadings prior writes per-column matrix-normal priors
  # from features or pairwise distances. The default puts an iid prior
  # on each entry. The framework of Heaps and Jermyn corresponds to a
  # Gaussian entry prior. A `student_t(3)` entry prior has heavier
  # tails. Its induced law on `Z_tilde` lacks the closed form of their
  # Corollary S1. Any iid entry prior on `Z` still induces a
  # proper prior on the QR-identified `Z_tilde`.
  spec <- trend_specs$loadings_prior_spec
  z_prior <- if (!is.null(spec)) {
    make_loadings_prior_stanvars(spec, prior = prior)
  } else {
    brms::stanvar(
      name = "factor_z_priors",
      scode = paste0(
        "to_vector(Z) ~ ",
        get_trend_parameter_prior(prior, "Z", trend_obj = trend_specs), ";"
      ),
      block = "model"
    )
  }
  rotate <- identical(
    factor_identification(trend_specs, data_info$has_by_lv), "rotation"
  )
  if (!rotate) {
    return(z_prior)
  }

  # The rotation `Q` is a working matrix of a local block and is not
  # stored. Stan requires a data expression for a top-level array
  # size, which `N_lags_trend` gives the transition arrays.
  paths <- c("lv_trend", rotated$paths)
  vectors <- rotated$vectors
  transitions <- rotated$transitions
  qr_lines <- c(
    "matrix[N_series_trend, N_lv_trend] Z_tilde = qr_thin_R(Z')';",
    paste0("matrix[N_time_trend, N_lv_trend] ", paths, "_tilde;"),
    if (length(vectors)) paste0("vector[N_lv_trend] ", vectors, "_tilde;"),
    if (length(transitions)) {
      paste0("array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] ",
             transitions, "_tilde;")
    },
    "{",
    "  matrix[N_lv_trend, N_lv_trend] Q = qr_thin_Q(Z')';",
    paste0("  ", paths, "_tilde = ", paths, " * Q';"),
    if (length(vectors)) paste0("  ", vectors, "_tilde = Q * ", vectors, ";"),
    if (length(transitions)) {
      c("  for (lag in 1:N_lags_trend) {",
        paste0("    ", transitions, "_tilde[lag] = Q * ", transitions,
               "[lag] * Q';"),
        "  }")
    },
    "}"
  )
  combine_stanvars(
    z_prior,
    brms::stanvar(
      name = "factor_z_identification",
      scode = paste(qr_lines, collapse = "\n"),
      block = "genquant"
    )
  )
}


# The structure of a loadings spec. The emitter, the `Z` prior
# classifier below and the model description in `methods_md()` all
# branch on it. A spec missing a count field is treated as a count of
# zero, which gives a length-one logical.
#' @noRd
loadings_spec_traits <- function(spec) {
  if (is.null(spec)) {
    return(list(features = FALSE, distances = FALSE,
                kernel = FALSE, mgp = FALSE))
  }
  features <- isTRUE((spec$N_features_trend %||% 0L) > 0L)
  distances <- isTRUE((spec$n_distances %||% 0L) > 0L)
  list(
    features = features,
    distances = distances,
    kernel = features || distances,
    mgp = identical(spec$column_shrinkage, "mgp")
  )
}


# The length-scales a loadings spec declares, in the order the
# emitter declares them. They are derived from the spec. A model then
# reports exactly the length-scales its distance sources give.
#' @noRd
loadings_monitor_params <- function(spec) {
  traits <- loadings_spec_traits(spec)
  c(
    if (traits$features) "theta_features" else character(0),
    if (traits$distances) {
      paste0("theta_dist_", names(spec$distance_mats) %||% character(0))
    } else {
      character(0)
    }
  )
}


# The prior `Z` is drawn from, given the loadings spec. The emitter
# below and the prior table that `create_trend_parameter_prior()`
# builds both use this one classifier. Separate decisions could make
# the table report a prior the model does not sample. A kernel takes
# precedence because it writes the whole multivariate statement. MGP
# then enters through the column scale.
#' @noRd
loadings_z_branch <- function(spec) {
  traits <- loadings_spec_traits(spec)
  if (traits$kernel) return("kernel")
  if (traits$mgp) return("mgp")
  "unstructured"
}


# The Stan statement each branch emits for `Z`, keyed by
# `loadings_z_branch()`. The prior table reports the same strings.
#'@noRd
loadings_z_prior_string <- function(branch) {
  switch(
    branch,
    kernel = paste0(
      "multi_normal_cholesky(rep_vector(0.0, N_series_trend),",
      " L_Phi_loadings)"
    ),
    mgp = "std_normal()",
    NULL
  )
}


#' Stan declaration of the MGP column scales
#'
#' @param dim Stan name of the number of columns
#' @return Character scalar declaring `Psi_diag`, the cumulative
#'   products of `varrho_inv`
#' @noRd
mgp_psi_diag_scode <- function(dim = "N_lv_trend") {
  checkmate::assert_string(dim, min.chars = 1)
  paste0(
    "vector<lower=0>[", dim, "] Psi_diag",
    " = exp(cumulative_sum(log(varrho_inv)));"
  )
}


#' The structured prior of Heaps and Jermyn on the loadings matrix
#'
#' Replaces the default `to_vector(Z) ~ student_t(3, 0, 0.5)` with the
#' per-column matrix-normal prior
#' `Z[, i] ~ multi_normal_cholesky(zero_vec, L_Phi)`. The kernel is
#' `Phi = exp(-d_1 / theta_dist_1) * ... * exp(-d_K / theta_dist_K) *
#' gp_exponential_cov(features, 1.0, theta_features)`, a product over
#' every pairwise-distance source and the feature-derived ARD
#' exponential kernel (Heaps and Jermyn 2024, Sect. 3.4.2 and Supp.
#' S4.2.1).
#'
#' The column scale is absent here. Under `column_shrinkage = "mgp"`
#' it reaches the trend through `sigma_trend`, derived as
#' `sqrt(Psi_diag)` in `generate_shared_innovation_stanvars()`.
#' Scaling this prior by it as well would give the column two scales
#' whose product alone reaches the likelihood. `Psi_diag` is
#' `rep_vector(1.0, N_lv_trend)` under `column_shrinkage = "iid"`, or
#' the multiplicative gamma process scale under `"mgp"` (Bhattacharya
#' and Dunson 2011, Heaps Eq. 9).
#'
#' Length-scales take a `log(theta) ~ normal(0, 1)` default, as Heaps
#' used across the simulation, Finnish birds and gas demand
#' applications (Sects. 6.1.2, 6.2.2, 6.3.1). Each appears in
#' `get_prior()` as `theta_features` or `theta_dist_<name>` and takes
#' an override. For a phylogenetic kernel the decay of covariance with
#' relatedness is a modelling choice. `validate_pairwise_distance()`
#' rescales each distance matrix to `max(d) == 1`, as Heaps scaled
#' branch lengths to a unit root-to-tip distance (Supp. S4.2.1). The
#' unit-scale length-scale prior is then a sensible default.
#'
#' @param spec A normalised loadings-prior list from
#'   `normalise_loadings_prior()`, holding `features_mat` (numeric
#'   p x c matrix or NULL), `distance_mats` (named list of p x p
#'   matrices, possibly empty), `column_shrinkage` ("iid" or "mgp"),
#'   `mgp_a1`, `mgp_a2`, `n_series`, `N_features_trend` and
#'   `n_distances`
#' @param prior A `brmsprior` carrying overrides of the length-scale
#'   and MGP priors
#' @return A stanvars object
#' @references
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0}
#'
#' Bhattacharya, A. and Dunson, D. B. (2011). Sparse Bayesian
#' infinite factor models. \emph{Biometrika}, 98:291-306.
#' @noRd
make_loadings_prior_stanvars <- function(spec, prior = NULL) {
  assert_loadings_prior_spec_consistent(spec)
  traits <- loadings_spec_traits(spec)
  has_features <- traits$features
  has_distances <- traits$distances
  uses_mgp <- traits$mgp
  dist_names <- names(spec$distance_mats) %||% character(0)
  data_vars <- list()
  tdata_vars <- list()
  param_vars <- list()
  tparam_vars <- list()
  model_vars <- list()
  if (has_features) {
    data_vars <- c(data_vars, list(
      brms::stanvar(
        x = as.integer(spec$N_features_trend),
        name = "N_features_trend",
        scode = "int<lower=1> N_features_trend;",
        block = "data"
      ),
      brms::stanvar(
        x = spec$features_mat,
        name = "row_features",
        scode = "matrix[N_series_trend, N_features_trend] row_features;",
        block = "data"
      )
    ))
    # `gp_exponential_cov` in Stan takes an array of vectors, so
    # convert the data-block matrix once in transformed data and
    # re-use the array form when assembling Phi each iteration.
    tdata_vars <- c(tdata_vars, list(
      brms::stanvar(
        name = "row_features_arr",
        scode = paste(
          "array[N_series_trend] vector[N_features_trend] row_features_arr;",
          "for (i_rf in 1:N_series_trend) {",
          "  row_features_arr[i_rf] = row_features[i_rf, ]';",
          "}",
          sep = "\n"
        ),
        block = "tdata"
      )
    ))
    # The ARD signature of `gp_exponential_cov` takes its
    # length-scales as `array[] real`
    param_vars <- c(param_vars, list(
      brms::stanvar(
        name = "theta_features",
        scode = "array[N_features_trend] real<lower=0> theta_features;",
        block = "parameters"
      )
    ))
    model_vars <- c(model_vars, list(
      brms::stanvar(
        name = "loadings_prior_theta_features",
        scode = paste0(
          "theta_features ~ ",
          get_trend_parameter_prior(prior, "theta_features"), ";"
        ),
        block = "model"
      )
    ))
  }
  if (has_distances) {
    for (nm in dist_names) {
      data_vars <- c(data_vars, list(
        brms::stanvar(
          x = spec$distance_mats[[nm]],
          name = paste0("dist_", nm),
          scode = paste0(
            "matrix<lower=0>[N_series_trend, N_series_trend] dist_",
            nm, ";"
          ),
          block = "data"
        )
      ))
      param_vars <- c(param_vars, list(
        brms::stanvar(
          name = paste0("theta_dist_", nm),
          scode = paste0("real<lower=0> theta_dist_", nm, ";"),
          block = "parameters"
        )
      ))
      model_vars <- c(model_vars, list(
        brms::stanvar(
          name = paste0("loadings_prior_theta_dist_", nm),
          scode = paste0(
            "theta_dist_", nm, " ~ ",
            get_trend_parameter_prior(prior, paste0("theta_dist_", nm)),
            ";"
          ),
          block = "model"
        )
      ))
    }
  }
  # `Phi_loadings` is the element-wise product over the sources, as
  # Heaps builds it: a factor `exp(-d / theta_dist)` per distance and
  # one ARD `gp_exponential_cov` for the features. A diagonal jitter
  # keeps the Cholesky factor stable for a near-singular kernel. MGP
  # without a kernel takes no `Phi_loadings`.
  if (traits$kernel) {
    phi_terms <- c(
      if (has_distances) {
        paste0("exp(-dist_", dist_names, " / theta_dist_", dist_names, ")")
      },
      if (has_features) {
        "gp_exponential_cov(row_features_arr, 1.0, theta_features)"
      }
    )
    tparam_vars <- c(tparam_vars, list(
      brms::stanvar(
        name = "loadings_prior_chol",
        scode = paste(
          paste0("matrix[N_series_trend, N_series_trend] Phi_loadings = ",
                 paste(phi_terms, collapse = " .* "), ";"),
          paste0("matrix[N_series_trend, N_series_trend] L_Phi_loadings = ",
                 "cholesky_decompose(add_diag(Phi_loadings, 1e-8));"),
          sep = "\n"
        ),
        block = "tparameters"
      )
    ))
  }
  if (uses_mgp) {
    data_vars <- c(data_vars, list(
      brms::stanvar(
        x = spec$mgp_a1,
        name = "mgp_a1",
        scode = "real<lower=0> mgp_a1;",
        block = "data"
      ),
      brms::stanvar(
        x = spec$mgp_a2,
        name = "mgp_a2",
        scode = "real<lower=0> mgp_a2;",
        block = "data"
      )
    ))
    param_vars <- c(param_vars, list(
      brms::stanvar(
        name = "varrho_inv",
        scode = "vector<lower=0>[N_lv_trend] varrho_inv;",
        block = "parameters"
      )
    ))
    # `generate_shared_innovation_stanvars()` declares `Psi_diag`,
    # where the innovation transform uses it
    model_vars <- c(model_vars, list(
      brms::stanvar(
        name = "mgp_priors",
        scode = paste(
          "varrho_inv[1] ~ inv_gamma(mgp_a1, 1);",
          "if (N_lv_trend > 1) {",
          "  varrho_inv[2:N_lv_trend] ~ inv_gamma(mgp_a2, 1);",
          "}",
          sep = "\n"
        ),
        block = "model"
      )
    ))
  }
  # `Z` is drawn at unit scale. A kernel gives each column the
  # multivariate normal under `L_Phi_loadings`. MGP without a kernel
  # gives every entry a standard normal, which equals the kernel form
  # on an identity `Phi` without its Cholesky factor. The MGP column
  # scale enters non-centred through `sigma_trend`, as
  # `generate_shared_innovation_stanvars()` explains.
  z_branch <- loadings_z_branch(spec)
  z_lines <- switch(
    z_branch,
    mgp = "to_vector(Z) ~ std_normal();",
    kernel = c(
      "for (i_z in 1:N_lv_trend) {",
      "  Z[, i_z] ~ multi_normal_cholesky(",
      "    rep_vector(0.0, N_series_trend),",
      "    L_Phi_loadings",
      "  );",
      "}"
    ),
    stop_mvgam_fault(
      "A structured loadings prior needs a kernel or MGP shrinkage.",
      paste0("The spec gives the branch '", z_branch, "'.")
    )
  )
  model_vars <- c(model_vars, list(
    brms::stanvar(
      name = "factor_z_priors",
      scode = paste(z_lines, collapse = "\n"),
      block = "model"
    )
  ))
  do.call(
    combine_stanvars,
    c(data_vars, tdata_vars, param_vars, tparam_vars, model_vars)
  )
}


#' Transformed-parameters stanvar assembling the trend matrix
#'
#' Every trend assembles `trend[i, s] = Z[s, ] lv_trend[i, ] +
#' mu_trend[times_trend[i, s]]`, as one product of the states with the
#' loadings and one gather of the trend predictor per column. An
#' identity `Z` drops the product. A `mu_trend` the trend formula
#' leaves at zero drops the gather.
#'
#' Under `by = lv_axis()`, `trend_cell_frame()` builds the trend data
#' at (time, factor) grain, and `times_trend` is
#' `[N_time_trend, N_lv_trend]`. `mu_trend` then holds one value per
#' time and factor. Each factor's predictor joins its state before the
#' loadings, which gives series `s` the trend
#' `Z[s, ] (lv_trend[i, ] + mu_factor[i, ])`.
#'
#' @param data_info Data information. `has_by_lv` is `TRUE` under
#'   `by = lv_axis()`. `mu_trend_zero` is `TRUE` when the trend formula
#'   has no terms.
#' @param identity_z Logical, `TRUE` when `Z` is the identity, as
#'   `z_is_identity()` decides
#' @return A stanvar
#' @noRd
generate_trend_computation_tparameters <- function(data_info,
                                                   identity_z) {
  checkmate::assert_list(data_info)
  checkmate::assert_flag(identity_z)
  loaded <- function(states) {
    if (identity_z) states else paste0(states, " * Z'")
  }
  add_mu <- !isTRUE(data_info$mu_trend_zero)
  scode_lines <- if (isTRUE(data_info$has_by_lv) && add_mu) {
    c(
      "// Each factor's predictor joins its state before the loadings",
      "matrix[N_time_trend, N_series_trend] trend;",
      "{",
      "  matrix[N_time_trend, N_lv_trend] lv_mu_trend = lv_trend;",
      "  for (k in 1:N_lv_trend) {",
      "    lv_mu_trend[ : , k] += mu_trend[times_trend[ : , k]];",
      "  }",
      paste0("  trend = ", loaded("lv_mu_trend"), ";"),
      "}"
    )
  } else {
    c(
      paste0(
        "// Each series' trend is ",
        if (identity_z) "the series' state" else "the loaded states",
        if (add_mu) ", plus the trend_formula predictor"
      ),
      paste0("matrix[N_time_trend, N_series_trend] trend = ",
             loaded("lv_trend"), ";"),
      if (add_mu) {
        c(
          "for (s in 1:N_series_trend) {",
          "  trend[ : , s] += mu_trend[times_trend[ : , s]];",
          "}"
        )
      }
    )
  }
  brms::stanvar(name = "trend", scode = paste(scode_lines, collapse = "\n"),
                block = "tparameters")
}

#' Stan function mixing a global and a group correlation
#'
#' `combine_cholesky()` returns the Cholesky factor of
#' `alpha * global + (1 - alpha) * local`, which every grouped trend
#' uses for each group's correlation.
#'
#' @return A functions-block stanvar
#' @noRd
generate_hierarchical_functions <- function() {
  brms::stanvar(
    name = "combine_cholesky",
    scode = "
      /* The Cholesky factor of a partially pooled correlation matrix,
       * alpha * global + (1 - alpha) * local. alpha = 1 gives the
       * global correlation and alpha = 0 the local one.
       */
      matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                              real alpha) {
        return cholesky_decompose(
          alpha * multiply_lower_tri_self_transpose(global_chol_cor)
          + (1 - alpha) * multiply_lower_tri_self_transpose(local_chol_cor));
      }
    ",
    block = "functions"
  )
}

#' Data stanvars of a grouped trend
#'
#' `N_groups_trend` and `N_subgroups_trend` count the groups and the
#' series in each. `group_inds_trend` holds the group of each series.
#' `group_members_trend` lists the member series of each group.
#'
#' @param hierarchical_info From `extract_hierarchical_info()`
#' @param data_info Data information list
#' @return A stanvars object
#' @noRd
generate_hierarchical_data_structures <- function(hierarchical_info,
                                                  data_info) {
  checkmate::assert_list(hierarchical_info, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_names(names(hierarchical_info),
                          must.include = c("n_groups", "n_subgroups", "gr_var"))
  checkmate::assert_count(hierarchical_info$n_groups, positive = TRUE)
  checkmate::assert_count(hierarchical_info$n_subgroups, positive = TRUE)
  checkmate::assert_string(hierarchical_info$gr_var)
  n_subgroups <- hierarchical_info$n_subgroups

  # Stan subscripts `group_inds_trend` by the trend's own series axis,
  # whose order the axis record resolves
  group_levels <- observed_levels(data_info$data[[hierarchical_info$gr_var]])
  group_inds <- match(hierarchical_series_groups(data_info), group_levels)
  # Every block of the program that takes a group's series takes them
  # from `group_members_trend`, built by the derivation every R path
  # shares
  group_members_mat <- do.call(rbind, group_members(group_inds, n_subgroups))

  combine_stanvars(
    brms::stanvar(
      x = hierarchical_info$n_groups, name = "N_groups_trend",
      scode = "int<lower=1> N_groups_trend;", block = "data",
      position = "start"
    ),
    brms::stanvar(
      x = n_subgroups, name = "N_subgroups_trend",
      scode = "int<lower=1> N_subgroups_trend;", block = "data",
      position = "start"
    ),
    brms::stanvar(
      x = group_inds, name = "group_inds_trend",
      scode = "array[N_series_trend] int<lower=1> group_inds_trend;",
      block = "data"
    ),
    brms::stanvar(
      x = group_members_mat, name = "group_members_trend",
      scode = paste0(
        "array[N_groups_trend, N_subgroups_trend] ",
        "int<lower=1, upper=N_series_trend> group_members_trend;"
      ),
      block = "data"
    )
  )
}

#' Parameters and transformed parameters of grouped correlations
#'
#' Declares each group's correlation as a mix of a global factor and a
#' group deviation. Transformed parameters compute each group's
#' Cholesky factor `L_group_trend` and covariance `Sigma_group_trend`.
#'
#' @param n_groups Number of groups
#' @param n_subgroups Number of series within a group
#' @param scale_innovations Logical. TRUE also scales the shared
#'   innovations by `L_group_trend`. A VAR samples its states directly
#'   and takes the covariances alone.
#' @return A stanvars object
#' @noRd
generate_hierarchical_correlation_parameters <- function(
    n_groups, n_subgroups, scale_innovations = TRUE) {
  checkmate::assert_integer(n_groups, len = 1, lower = 1)
  checkmate::assert_integer(n_subgroups, len = 1, lower = 1)
  checkmate::assert_flag(scale_innovations)

  sampled <- combine_stanvars(
    brms::stanvar(
      name = "L_Omega_global_trend",
      scode = paste0(
        "cholesky_factor_corr[N_subgroups_trend] L_Omega_global_trend;"
      ),
      block = "parameters"
    ),
    brms::stanvar(
      name = "L_deviation_group_trend",
      scode = paste0(
        "array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] ",
        "L_deviation_group_trend;"
      ),
      block = "parameters"
    ),
    brms::stanvar(
      name = "alpha_cor_trend",
      scode = "real<lower=0, upper=1> alpha_cor_trend;",
      block = "parameters"
    ),
    brms::stanvar(
      name = "sigma_group_trend",
      scode = paste0(
        "array[N_groups_trend] vector<lower=0>[N_subgroups_trend] ",
        "sigma_group_trend;"
      ),
      block = "parameters"
    )
  )
  computed <- brms::stanvar(
    name = "hierarchical_correlation_computation",
    scode = paste0(
      "array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] ",
      "L_Omega_group_trend;\n",
      "array[N_groups_trend] matrix[N_subgroups_trend, N_subgroups_trend] ",
      "L_group_trend;\n",
      "array[N_groups_trend] cov_matrix[N_subgroups_trend] ",
      "Sigma_group_trend;\n",
      "// Each group's correlation: the global factor and the group's own\n",
      "// deviation, mixed by alpha_cor_trend\n",
      "for (g_idx in 1:N_groups_trend) {\n",
      "  L_Omega_group_trend[g_idx] = combine_cholesky(\n",
      "    L_Omega_global_trend, L_deviation_group_trend[g_idx],\n",
      "    alpha_cor_trend\n",
      "  );\n",
      "  L_group_trend[g_idx] = diag_pre_multiply(\n",
      "    sigma_group_trend[g_idx], L_Omega_group_trend[g_idx]\n",
      "  );\n",
      "  Sigma_group_trend[g_idx] =\n",
      "    multiply_lower_tri_self_transpose(L_group_trend[g_idx]);\n",
      "}",
      if (scale_innovations) {
        paste0(
          "\n// Scale each group's innovations by its Cholesky factor,\n",
          "// every occasion in one product\n",
          "for (g_idx in 1:N_groups_trend) {\n",
          "  scaled_innovations_trend[ : , group_members_trend[g_idx]]\n",
          "    = innovations_trend[ : , group_members_trend[g_idx]]\n",
          "      * L_group_trend[g_idx]';\n",
          "}"
        )
      }
    ),
    block = "tparameters"
  )
  combine_stanvars(sampled, computed)
}

#' Model-block stanvars of a grouped trend's correlation priors
#'
#' Each prior takes a user override through the shared prior lookup:
#' `alpha_cor_trend`, the global factor `L_Omega_global_trend`, each
#' group's deviation `L_deviation_group_trend` and each group's scales
#' `sigma_group_trend`.
#'
#' @param prior A `brmsprior` or NULL
#' @param trend_obj The trend specification, for the default scale prior
#' @return A stanvars object
#' @noRd
generate_hierarchical_correlation_model <- function(prior = NULL,
                                                    trend_obj = NULL) {
  lookup <- function(cls, ...) get_trend_parameter_prior(prior, cls, ...)
  combine_stanvars(
    brms::stanvar(
      name = "alpha_cor_trend_prior",
      scode = paste0("alpha_cor_trend ~ ", lookup("alpha_cor_trend"), ";"),
      block = "model"
    ),
    brms::stanvar(
      name = "L_Omega_global_trend_prior",
      scode = paste0("L_Omega_global_trend ~ ",
                     lookup("L_Omega_global_trend"), ";"),
      block = "model"
    ),
    brms::stanvar(
      name = "L_deviation_group_trend_prior",
      scode = paste0(
        "for (g_idx in 1:N_groups_trend) { ",
        "L_deviation_group_trend[g_idx] ~ ",
        lookup("L_deviation_group_trend"), "; }"
      ),
      block = "model"
    ),
    brms::stanvar(
      name = "sigma_group_trend_prior",
      scode = paste0(
        "for (g_idx in 1:N_groups_trend) { ",
        "to_vector(sigma_group_trend[g_idx]) ~ ",
        lookup("sigma_group_trend", trend_obj = trend_obj), "; }"
      ),
      block = "model"
    )
  )
}

#' Stanvars of one trend
#'
#' Every trend declares the data dimensions and the latent states. A
#' trend registered with `shared_innovations` adds the shared
#' innovations and their priors. The registry names the generator that
#' writes the rest.
#'
#' @param trend_specs Trend specification object
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_trend_specific_stanvars <- function(trend_specs, data_info,
                                             prior = NULL) {
  checkmate::assert_list(trend_specs)
  checkmate::assert_list(data_info)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  trend_type <- trend_specs$trend
  if (is.null(trend_type)) {
    stop_missing_fields("The trend specification", "trend")
  }

  n_obs <- data_info$n_obs
  n_series <- data_info_n_series(data_info)
  n_lv <- trend_specs$n_lv %||% n_series
  checkmate::assert_int(n_obs, lower = 1)
  checkmate::assert_int(n_series, lower = 1)
  checkmate::assert_int(n_lv, lower = 1)
  validate_no_factor_hierarchical(trend_specs, n_series, trend_type)

  ensure_registry_initialized()
  generator_function <- get_trend_info(trend_type)$generator

  shared_stanvars <- NULL
  if (trend_property(trend_type, "shared_innovations")) {
    hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)
    cor <- trend_specs$cor %||% FALSE
    unit_factors <- samples_factor_loadings(trend_specs)
    # Under multiplicative gamma process shrinkage the innovation scale
    # is the column scale `sqrt(Psi_diag)`, which the model derives. A
    # prior on `sigma_trend` would name a quantity nothing samples.
    mgp_scale <- loadings_spec_traits(trend_specs$loadings_prior_spec)$mgp
    if (mgp_scale && !is.null(prior) &&
        !is.null(extract_prior_string(prior, "sigma_trend"))) {
      stop(insight::format_error(c(
        paste0(
          "A 'sigma_trend' prior is not supported under ",
          "multiplicative gamma process shrinkage."
        ),
        x = paste0("With column_shrinkage = 'mgp' the innovation scale ",
                   "is derived as sqrt(Psi_diag)."),
        i = paste0("Shape the column scale through 'mgp_a1' and 'mgp_a2' ",
                   "on 'loadings_prior' or drop the 'sigma_trend' prior.")
      )), call. = FALSE)
    }
    refuse_unit_factor_priors(prior, unit_factors)
    shared_stanvars <- combine_stanvars(
      generate_shared_innovation_stanvars(
        cor = cor,
        unit_factors = unit_factors,
        mgp_scale = mgp_scale,
        hierarchical_info = hierarchical_info
      ),
      generate_innovation_model(
        effective_dim = "N_lv_trend",
        cor = cor,
        is_hierarchical = isTRUE(hierarchical_info$has_groups),
        prior = prior,
        df = trend_specs$df %||% Inf,
        mgp_scale = mgp_scale,
        unit_factors = unit_factors,
        trend_obj = trend_specs
      )
    )
  }

  common_dimensions <- generate_common_trend_data(
    n_obs, n_series, n_lv,
    is_factor_model_spec(trend_specs$n_lv, n_series),
    n_time = data_info$n_time
  )
  trend_stanvars <- generator_function(trend_specs, data_info, prior = prior)
  # A generator that samples its states declares `lv_trend` itself.
  # Every other trend builds them in transformed parameters.
  lv_trend_stanvar <- if (!"lv_trend" %in% names(trend_stanvars)) {
    brms::stanvar(
      name = "shared_lv_trend",
      scode = "matrix[N_time_trend, N_lv_trend] lv_trend;",
      block = "tparameters"
    )
  }
  combine_stanvars(common_dimensions, shared_stanvars, lv_trend_stanvar,
                   trend_stanvars)
}


#' Stanvars of a RW trend
#'
#' Each state adds one innovation, or one moving-average innovation, to
#' the last. `assemble_shared_innovation_trend()` adds the rest.
#'
#' @param trend_specs Trend specification from `RW()`
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_rw_trend_stanvars <- function(trend_specs, data_info,
                                       prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  innovations <- if (isTRUE(trend_specs$ma)) {
    "ma_innovations_trend"
  } else {
    "scaled_innovations_trend"
  }
  dynamics <- brms::stanvar(
    name = "rw_tparameters",
    scode = paste0(
      ma_innovations_stanblock(trend_specs$ma), "\n",
      "// Random walk: each state adds one innovation to the last\n",
      "lv_trend[1, :] = ", innovations, "[1, :];\n",
      "for (i in 2:N_time_trend) {\n",
      "  lv_trend[i, :] = lv_trend[i - 1, :] + ", innovations, "[i, :];\n",
      "}"
    ),
    block = "tparameters"
  )
  assemble_shared_innovation_trend(
    list(dynamics), trend_specs, data_info, prior
  )
}

#' Stanvars of an LLT trend
#'
#' The level adds the slope and a shared innovation to its last value.
#' The slope adds its own Gaussian shock to the last slope, damped by
#' `ar1_slope_trend` when `damped = TRUE`. The slope starts at the
#' free `init_slope_trend` either way. The slope shocks take the
#' level's correlation factor with their own scales. Under sampled
#' loadings the factors share one slope scale and one damping.
#' `assemble_shared_innovation_trend()` adds the rest.
#'
#' @param trend_specs Trend specification from `LLT()`
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_llt_trend_stanvars <- function(trend_specs, data_info,
                                        prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  damped <- isTRUE(trend_specs$damped)
  shared <- samples_factor_loadings(trend_specs)
  has_gr <- named_var(trend_specs$gr)
  cor <- isTRUE(trend_specs$cor) && !shared
  n_par <- if (shared) "1" else "N_lv_trend"
  # A parameter at every latent process, broadcast when shared
  per_process <- function(name) {
    if (shared) paste0("rep_vector(", name, "[1], N_lv_trend)") else name
  }
  # The slope parameters are sampled in units of their default prior
  # scales. A random start in (-2, 2) on the unconstrained scale then
  # lies within a few prior scales of zero. A slope scale near one,
  # integrated twice over the series, would put the predictor far
  # outside the data.
  scales <- llt_prior_scales(data_info$n_time, damped, shared,
                             trend_prior_unit_of(trend_specs))

  data <- combine_stanvars(
    brms::stanvar(
      x = scales$sigma, name = "slope_scale_trend",
      scode = "real<lower=0> slope_scale_trend;", block = "data"
    ),
    brms::stanvar(
      x = scales$init, name = "init_slope_scale_trend",
      scode = "real<lower=0> init_slope_scale_trend;", block = "data"
    )
  )

  parameters <- brms::stanvar(
    name = "llt_parameters",
    scode = paste0(
      "// Slope shock scales in units of slope_scale_trend, the initial\n",
      "// slope and the standard shocks of every later slope\n",
      "vector<lower=0>[", n_par, "] sigma_slope_raw_trend;\n",
      if (damped) {
        paste0("vector<lower=0, upper=1>[", n_par, "] ar1_slope_trend;\n")
      },
      "vector<multiplier=init_slope_scale_trend>[N_lv_trend] ",
      "init_slope_trend;\n",
      "matrix[N_time_trend - 1, N_lv_trend] innovations_slope_trend;"
    ),
    block = "parameters"
  )

  # The slope shocks take the level's correlation: the global factor,
  # or each group's own
  scale_shocks <- if (has_gr) {
    paste0(
      group_members_stanblock(),
      "        slope_shocks[ : , members]\n",
      "          = innovations_slope_trend[ : , members]\n",
      "            * diag_pre_multiply(slope_sd[members],\n",
      "                                L_Omega_group_trend[g_idx])';\n",
      "      }"
    )
  } else if (cor) {
    paste0("      slope_shocks = innovations_slope_trend\n",
           "        * diag_pre_multiply(slope_sd, L_Omega_trend)';")
  } else {
    paste0("      slope_shocks = diag_post_multiply(innovations_slope_trend,",
           " slope_sd);")
  }

  dynamics <- brms::stanvar(
    name = "llt_tparameters",
    scode = paste0(
      "// A fixed rescaling: the prior on sigma_slope_trend needs no\n",
      "// Jacobian adjustment\n",
      "vector<lower=0>[", n_par, "] sigma_slope_trend\n",
      "  = slope_scale_trend * sigma_slope_raw_trend;\n",
      "matrix[N_time_trend, N_lv_trend] slope_trend;\n",
      "    {\n",
      "      vector[N_lv_trend] slope_sd = ", per_process("sigma_slope_trend"),
      ";\n",
      if (damped) {
        paste0("      vector[N_lv_trend] slope_phi = ",
               per_process("ar1_slope_trend"), ";\n")
      },
      "      matrix[N_time_trend - 1, N_lv_trend] slope_shocks;\n",
      scale_shocks, "\n",
      "      slope_trend[1] = init_slope_trend';\n",
      "      // The level adds the last slope, and the slope its shock\n",
      "      lv_trend[1] = scaled_innovations_trend[1];\n",
      "      for (i in 2:N_time_trend) {\n",
      "        lv_trend[i] = lv_trend[i - 1] + slope_trend[i - 1]\n",
      "                      + scaled_innovations_trend[i];\n",
      "        slope_trend[i] = ",
      if (damped) "(slope_phi .* slope_trend[i - 1]')'" else
        "slope_trend[i - 1]",
      " + slope_shocks[i - 1];\n",
      "      }\n",
      "    }"
    ),
    block = "tparameters"
  )

  model <- combine_stanvars(
    brms::stanvar(
      name = "llt_shocks_model",
      scode = "to_vector(innovations_slope_trend) ~ std_normal();",
      block = "model"
    ),
    generate_trend_priors_stanvar(
      param_names = c("sigma_slope_trend",
                      if (damped) "ar1_slope_trend", "init_slope_trend"),
      prior = prior,
      stanvar_name = "llt_model",
      heading = "// Slope priors",
      trend_obj = trend_specs
    )
  )

  assemble_shared_innovation_trend(
    list(data, parameters, dynamics, model), trend_specs, data_info, prior,
    rotated = list(paths = "slope_trend", vectors = "init_slope_trend")
  )
}

#' Model-block stanvar of trend parameter priors
#'
#' One sampling statement per parameter, each under the prior
#' `get_trend_parameter_prior()` resolves. A parameter without a prior
#' takes no statement.
#'
#' @param param_names Parameter names, such as `c("ar1_trend",
#'   "sigma_trend")`
#' @param prior A `brmsprior` or NULL
#' @param stanvar_name Name for the stanvar object
#' @param heading A Stan comment written above the statements, or NULL
#' @param trend_obj The `mvgam_trend`, for a trend whose defaults
#'   depend on its own arguments or dimensions, or NULL
#' @return A stanvar, or NULL when no parameter takes a prior
#' @noRd
generate_trend_priors_stanvar <- function(param_names, prior = NULL,
                                          stanvar_name = "trend_priors",
                                          heading = NULL,
                                          trend_obj = NULL) {
  checkmate::assert_character(param_names, min.len = 1, any.missing = FALSE)
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_string(stanvar_name, min.chars = 1)
  checkmate::assert_string(heading, null.ok = TRUE)
  laws <- vapply(param_names, get_trend_parameter_prior, character(1L),
                 prior = prior, trend_obj = trend_obj, USE.NAMES = FALSE)
  keep <- nzchar(laws)
  if (!any(keep)) {
    return(NULL)
  }
  brms::stanvar(
    name = stanvar_name,
    scode = paste(c(heading, paste0(param_names[keep], " ~ ", laws[keep],
                                    ";")), collapse = "\n"),
    block = "model"
  )
}


#' AR coefficient stanvars for the three `coef_sharing` modes
#'
#' * none: one `vector<lower=-1,upper=1>\[N_lv_trend\] ar{lag}_trend`
#'   per lag, sampled directly under the standard trend prior.
#' * shared: one `vector<lower=-1,upper=1>\[1\] shared_ar{lag}_trend`
#'   per lag. Transformed parameters broadcast it to the per-series
#'   `ar{lag}_trend`, which every other part of the program uses.
#' * hierarchical: the per-series `ar{lag}_trend` take
#'   `normal(mu_ar{lag}_trend, sigma_ar{lag}_trend)` truncated to
#'   (-1, 1), with both hyperparameters sampled per lag.
#'
#' A contiguous lag set samples partial autocorrelations instead and
#' derives the coefficients through the Levinson-Durbin recursion.
#'
#' @param ar_lags Integer vector of active lags, such as `1:p` or
#'   `c(1, 12)` for sparse lags
#' @param coef_sharing One of `"none"`, `"shared"` or `"hierarchical"`
#' @param prior A `brmsprior` or NULL
#' @return A list of stanvars
#' @noRd
build_ar_coef_stanvars <- function(ar_lags, coef_sharing, prior = NULL) {
  checkmate::assert_integerish(ar_lags, lower = 1L, any.missing = FALSE)
  checkmate::assert_choice(coef_sharing,
                           c("none", "shared", "hierarchical"))
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  # A contiguous lag set samples partial autocorrelations and
  # derives the coefficients from them. `ar{lag}_trend` holds the
  # coefficient under either parameterisation, which keeps every
  # reader of a fitted model on one quantity.
  stationary <- ar_lags_stationary(ar_lags)
  stem <- paste0(ar_coef_stem(ar_lags), "_trend")
  shared_names <- ar_shared_coef_names(ar_lags)
  par_lines <- character(0)
  tpar_decl_lines <- character(0)
  tpar_assign_lines <- character(0)
  prior_lines <- character(0)
  for (li in seq_along(ar_lags)) {
    lag <- ar_lags[li]
    pooled <- paste0("ar", lag, stem)
    sampled <- switch(coef_sharing,
      none = pooled,
      shared = shared_names[li],
      hierarchical = pooled
    )
    if (coef_sharing == "shared") {
      par_lines <- c(par_lines, paste0(
        "vector<lower=-1,upper=1>[1] ", sampled, ";"
      ))
      tpar_decl_lines <- c(tpar_decl_lines, paste0(
        "vector[N_lv_trend] ", pooled, ";"
      ))
      tpar_assign_lines <- c(tpar_assign_lines, paste0(
        pooled, " = rep_vector(", sampled, "[1], N_lv_trend);"
      ))
      prior_str <- get_trend_parameter_prior(prior, sampled)
      if (!nzchar(prior_str)) {
        prior_str <- get_trend_parameter_prior(prior, pooled)
      }
      prior_lines <- c(prior_lines, paste0(sampled, " ~ ", prior_str, ";"))
    } else if (coef_sharing == "hierarchical") {
      mu_name <- paste0("mu_ar", lag, stem)
      sigma_name <- paste0("sigma_ar", lag, stem)
      par_lines <- c(par_lines,
        paste0("real<lower=-1,upper=1> ", mu_name, ";"),
        paste0("real<lower=0> ", sigma_name, ";"),
        paste0("vector<lower=-1,upper=1>[N_lv_trend] ", pooled, ";")
      )
      mu_prior <- get_trend_parameter_prior(prior, mu_name)
      sigma_prior <- get_trend_parameter_prior(prior, sigma_name)
      prior_lines <- c(prior_lines,
        paste0(mu_name, " ~ ", mu_prior, ";"),
        paste0(sigma_name, " ~ ", sigma_prior, ";"),
        paste0(pooled, " ~ normal(", mu_name, ", ", sigma_name, ");"),
        # Each series' coefficient is bounded to (-1, 1), and the
        # population density truncated there carries a normaliser that
        # moves with the hyperparameters.
        paste0("target += -N_lv_trend * log_diff_exp(normal_lcdf(1 | ",
               mu_name, ", ", sigma_name, "), normal_lcdf(-1 | ",
               mu_name, ", ", sigma_name, "));")
      )
    } else {
      par_lines <- c(par_lines, paste0(
        "vector<lower=-1,upper=1>[N_lv_trend] ", pooled, ";"
      ))
      prior_str <- get_trend_parameter_prior(prior, sampled)
      prior_lines <- c(prior_lines, paste0(sampled, " ~ ", prior_str, ";"))
    }
  }
  # The Levinson-Durbin recursion, once per latent series. Every
  # partial autocorrelation inside (-1, 1) gives a stationary
  # coefficient vector, which is the constraint the declared
  # bounds state exactly for one lag and overstate beyond it.
  if (stationary) {
    p_max <- max(as.integer(ar_lags))
    idx <- seq_len(p_max)
    tpar_decl_lines <- c(tpar_decl_lines, paste0(
      "vector[N_lv_trend] ar", idx, "_trend;"
    ))
    tpar_assign_lines <- c(tpar_assign_lines, paste0(
      "for (j in 1:N_lv_trend) {\n",
      "  vector[", p_max, "] ar_coef_j = ar_pacf_to_coef(\n",
      "    to_vector({",
      paste0("ar", idx, "_pacf_trend[j]", collapse = ", "),
      "})\n",
      "  );\n",
      paste0("  ar", idx, "_trend[j] = ar_coef_j[", idx, "];",
             collapse = "\n"), "\n",
      "}"
    ))
  }
  parameters_sv <- brms::stanvar(
    name = "ar_parameters",
    scode = paste0(
      "// AR coefficient parameters\n",
      paste(par_lines, collapse = "\n")
    ),
    block = "parameters"
  )
  out <- list(parameters_sv)
  if (length(tpar_decl_lines)) {
    tpar_sv <- brms::stanvar(
      name = "ar_sharing_tparameters",
      scode = paste0(
        "// AR coefficients derived from the sampled parameters\n",
        paste(tpar_decl_lines, collapse = "\n"), "\n",
        paste(tpar_assign_lines, collapse = "\n")
      ),
      block = "tparameters"
    )
    out <- c(out, list(tpar_sv))
  }
  model_sv <- brms::stanvar(
    name = "ar_model",
    scode = paste0(
      "// AR coefficient priors\n",
      paste(prior_lines, collapse = "\n")
    ),
    block = "model"
  )
  c(out, list(model_sv))
}


#' Stan functions for a stationary joint initial distribution
#'
#' Each function gives the covariance a stationary trend draws its
#' first `p` states from. Under a moving-average term the MA error
#' before the first occasion joins them.
#'
#' A `VAR()` trend without a moving-average term needs no solve. The
#' map of Heaps (2023) returns the stationary autocovariances at lags
#' 0 to `p - 1` with the coefficients. `var_stationary_joint` places
#' them in the block Toeplitz covariance of `p` consecutive states.
#'
#' A `VAR(ma = TRUE)` trend takes its autocovariances from
#' `varma_autocov`, which solves the Lyapunov equation of `p`
#' consecutive states at side `(p m)^2`. The MA error enters that
#' equation through its noise covariance alone and takes no place in
#' the state. A companion state that also held the error would need
#' side `((p + 1) m)^2`, at 64 times the cost for one lag.
#' `varma_stationary_joint` adds the error's blocks. A grouped trend
#' solves each group on its own and places the autocovariances at the
#' group's members.
#'
#' An `AR()` trend has diagonal coefficient matrices, which lets its
#' solve split by pairs of series. `ar_diag_joint_var` takes one
#' companion of side `p` per series and solves each pair on its own.
#' The full companion costs one dense solve of side `(p m)^2` at every
#' gradient. Measured on a correlated `AR(p = 2)` against `AR(p = 1)`,
#' that put the gradient at 9 times the cost of the one-lag model on 6
#' series and 170 times on 15. The pairwise solve brings 15 series to
#' 2.2 times. A moving-average start filters a draw from this same
#' covariance and needs nothing further.
#'
#' One trend constructor is allowed per formula. A program emits this
#' block once, holding only the functions it calls.
#'
#' @param kernel `"var"` for the placement a `VAR()` trend calls,
#'   `"varma"` for the solve and placement a `VAR(ma = TRUE)` trend
#'   calls, `"ar"` for the pairwise solve an `AR()` trend calls
#' @return A `functions` block stanvar
#' @noRd
stationary_joint_functions_stanvar <- function(kernel = "var") {
  checkmate::assert_choice(kernel, c("var", "varma", "ar"))
  kronecker <- "
      /**
       * Kronecker product of two matrices
       * @param A Matrix (m x n)
       * @param B Matrix (p x q)
       * @return A (x) B (mp x nq)
       */
      matrix kronecker_prod(matrix A, matrix B) {
        int m = rows(A);
        int n = cols(A);
        int p = rows(B);
        int q = cols(B);
        matrix[m * p, n * q] C;
        for (i in 1:m) {
          for (j in 1:n) {
            C[((i - 1) * p + 1):(i * p), ((j - 1) * q + 1):(j * q)]
              = A[i, j] * B;
          }
        }
        return C;
      }
"
  placement <- "
      /**
       * Joint stationary covariance of p consecutive states of a VAR(p)
       *
       * Block i is the state i - 1 steps back. `Gamma[k]` is the
       * covariance of a state with the state k - 1 steps after it, as
       * `rev_mapping` returns it. Block (i, j) with j > i is the
       * transpose of `Gamma[j - i + 1]`.
       * @param Gamma Autocovariances at lags 0 to p - 1
       * @return Covariance of side p * m
       */
      matrix var_stationary_joint(array[] matrix Gamma) {
        int p = size(Gamma);
        int m = rows(Gamma[1]);
        matrix[p * m, p * m] Omega;
        for (i in 1:p) {
          for (j in i:p) {
            Omega[((i - 1) * m + 1):(i * m), ((j - 1) * m + 1):(j * m)]
              = Gamma[j - i + 1]';
            if (j > i) {
              Omega[((j - 1) * m + 1):(j * m), ((i - 1) * m + 1):(i * m)]
                = Gamma[j - i + 1];
            }
          }
        }
        return Omega;
      }
"
  varma <- "
      /**
       * Autocovariances at lags 0 to p - 1 of a stationary VARMA(p, 1)
       *
       * The state x_t = (y_t, ..., y_{t-p+1}) follows
       * x_t = F x_{t-1} + u_t. The leading block of u_t is
       * eps_t + theta eps_{t-1}. It meets x_{t-1} through
       * Cov(y_{t-1}, eps_{t-1}) = Sigma alone. C = Cov(x_{t-1}, u_t) is
       * then Sigma theta' in its leading block and zero elsewhere.
       * Omega = F Omega F' + Q, with Q = Cov(u_t) + F C + (F C)'.
       * @param Sigma Innovation covariance (m x m)
       * @param phi Stationary VAR coefficient matrices, one per lag
       * @param theta Invertible MA coefficient matrix
       * @return Autocovariances as `var_stationary_joint` takes them
       */
      array[] matrix varma_autocov(matrix Sigma, array[] matrix phi,
                                   matrix theta) {
        int p = size(phi);
        int m = rows(Sigma);
        int d = p * m;
        matrix[d, d] F = rep_matrix(0.0, d, d);
        matrix[d, d] FC = rep_matrix(0.0, d, d);
        matrix[d, d] Omega;
        array[p] matrix[m, m] Gamma;

        // The leading m rows hold the coefficients. Each later block
        // row shifts its lag down by one.
        for (i in 1:p) {
          F[1:m, ((i - 1) * m + 1):(i * m)] = phi[i];
        }
        for (i in 2:p) {
          for (j in 1:m) {
            F[(i - 1) * m + j, (i - 2) * m + j] = 1.0;
          }
        }
        FC[ : , 1:m] = F[ : , 1:m] * Sigma * theta';
        matrix[d, d] Q = FC + FC';
        Q[1:m, 1:m] += Sigma + quad_form_sym(Sigma, theta');

        // vec(Omega) = (I - F (x) F)^{-1} vec(Q)
        Omega = to_matrix((identity_matrix(d * d) - kronecker_prod(F, F))
                          \\ to_vector(Q), d, d);
        for (k in 1:p) {
          Gamma[k] = Omega[((k - 1) * m + 1):(k * m), 1:m];
        }
        // The solve leaves rounding asymmetry in the lag 0 block, which
        // `var_stationary_joint` places on the diagonal
        Gamma[1] = 0.5 * (Gamma[1] + Gamma[1]');
        return Gamma;
      }

      /**
       * Joint stationary covariance of p consecutive states of a
       * VARMA(p, 1) and the MA error eps_0
       *
       * The error is white noise. Its covariance with y_0 is Sigma,
       * and with every earlier state zero.
       * @param Gamma Autocovariances at lags 0 to p - 1
       * @param Sigma Innovation covariance (m x m)
       * @return Covariance of side (p + 1) * m
       */
      matrix varma_stationary_joint(array[] matrix Gamma, matrix Sigma) {
        int m = rows(Sigma);
        int d = size(Gamma) * m;
        matrix[d + m, d + m] Omega = rep_matrix(0.0, d + m, d + m);
        Omega[1:d, 1:d] = var_stationary_joint(Gamma);
        Omega[(d + 1):(d + m), (d + 1):(d + m)] = Sigma;
        Omega[1:m, (d + 1):(d + m)] = Sigma;
        Omega[(d + 1):(d + m), 1:m] = Sigma;
        return Omega;
      }
"
  pairwise <- "
      /**
       * Joint stationary covariance of p consecutive states of an AR(p)
       * with diagonal coefficients.
       *
       * The state runs `(y_t, ..., y_{t-p+1})`, block i at rows
       * `(i - 1) * m + 1` to `i * m`, the layout `var_stationary_joint`
       * uses. Series a and b evolve as two scalar companions `A_a` and
       * `A_b` driven by innovations with covariance `Sigma[a, b]`. Their
       * cross-covariance is `Sigma[a, b] * X`, where
       * `X = A_a X A_b' + e_1 e_1'`. Each pair is a solve of side `p^2`.
       *
       * @param Sigma Innovation covariance (m x m)
       * @param phi Coefficient vectors, one per lag
       * @return Covariance of side `p * m`
       */
      matrix ar_diag_joint_var(matrix Sigma, array[] vector phi) {
        int p = size(phi);
        int m = rows(Sigma);
        array[m] matrix[p, p] A;
        vector[p * p] q = rep_vector(0.0, p * p);
        matrix[p * m, p * m] Omega;
        q[1] = 1;
        for (a in 1:m) {
          A[a] = rep_matrix(0.0, p, p);
          for (k in 1:p) {
            A[a][1, k] = phi[k][a];
          }
          for (k in 2:p) {
            A[a][k, k - 1] = 1;
          }
        }
        for (a in 1:m) {
          for (b in a:m) {
            matrix[p, p] X = to_matrix(
              (identity_matrix(p * p) - kronecker_prod(A[b], A[a])) \\ q,
              p, p);
            for (i in 1:p) {
              for (j in 1:p) {
                Omega[(i - 1) * m + a, (j - 1) * m + b] = Sigma[a, b] * X[i, j];
                Omega[(j - 1) * m + b, (i - 1) * m + a] = Sigma[a, b] * X[i, j];
              }
            }
          }
        }
        return Omega;
      }
"
  brms::stanvar(
    name = "stationary_joint_functions",
    scode = switch(kernel,
      var = placement,
      varma = paste0(kronecker, placement, varma),
      ar = paste0(kronecker, pairwise)
    ),
    block = "functions"
  )
}


#' Stan functions for the partial autocorrelation parameterisation
#'
#' `ar_pacf_to_coef` maps one partial autocorrelation per lag, each
#' inside (-1, 1), to the AR(p) coefficients they imply. Every such
#' input gives a stationary process. `ar_stationary_init` applies
#' the same recursion at each order to draw the first p states from
#' their joint stationary distribution.
#'
#' @param with_init Emit `ar_stationary_init()`, which only an
#'   `AR(p > 1)` with independent innovations and no moving-average
#'   term calls
#' @return A `functions` block stanvar
#' @noRd
ar_pacf_functions_stanvar <- function(with_init = TRUE) {
  checkmate::assert_flag(with_init)
  brms::stanvar(
    name = "ar_functions",
    scode = paste0(
      "  // Partial autocorrelations to AR coefficients by the\n",
      "  // Levinson-Durbin recursion. Each element of `pacf` lies\n",
      "  // inside (-1, 1), and the result is a stationary\n",
      "  // coefficient vector for every such input.\n",
      "  vector ar_pacf_to_coef(vector pacf) {\n",
      "    int p = num_elements(pacf);\n",
      "    vector[p] phi = rep_vector(0.0, p);\n",
      "    vector[p] work;\n",
      "    for (k in 1:p) {\n",
      "      work = phi;\n",
      "      work[k] = pacf[k];\n",
      "      for (m in 1:(k - 1)) {\n",
      "        work[m] = phi[m] - pacf[k] * phi[k - m];\n",
      "      }\n",
      "      phi = work;\n",
      "    }\n",
      "    return phi;\n",
      "  }",
      if (!with_init) "" else paste0(
      "\n",
      "\n",
      "  // Stationary initial states for a scalar AR(p), from the\n",
      "  // partial autocorrelations the program samples. `pacf`\n",
      "  // gives one value per lag inside (-1, 1), `sigma` the\n",
      "  // innovation standard deviation and `z` the p standard\n",
      "  // normal draws. The marginal variance is\n",
      "  // sigma^2 / prod(1 - pacf^2), and conditioning on m\n",
      "  // earlier states multiplies it by prod_{k<=m}(1 - pacf_k^2).\n",
      "  // Each order's coefficients come from the recursion above,\n",
      "  // applied to the leading m partial autocorrelations.\n",
      "  vector ar_stationary_init(vector pacf, real sigma, vector z) {\n",
      "    int p = num_elements(pacf);\n",
      "    vector[p] state;\n",
      "    real v = square(sigma);\n",
      "    for (k in 1:p) {\n",
      "      v /= (1 - square(pacf[k]));\n",
      "    }\n",
      "    state[1] = sqrt(v) * z[1];\n",
      "    for (m in 1:(p - 1)) {\n",
      "      vector[m] phi_m = ar_pacf_to_coef(pacf[1:m]);\n",
      "      real mean_m = 0;\n",
      "      v *= (1 - square(pacf[m]));\n",
      "      for (j in 1:m) {\n",
      "        mean_m += phi_m[j] * state[m - j + 1];\n",
      "      }\n",
      "      state[m + 1] = mean_m + sqrt(v) * z[m + 1];\n",
      "    }\n",
      "    return state;\n",
      "  }"
      )
    ),
    block = "functions"
  )
}


#' Stan block declaring and filling the moving-average innovations
#'
#' `RW()` and `AR()` carry one moving-average term. One emitter keeps
#' the two generators on the same recursion.
#' `ma_innovations_trend[i]` holds `e[i] + theta * e[i - 1]`, a
#' first-order moving average of the scaled innovations. Row one is
#' the innovation itself, which is what a first-order term gives at
#' the first occasion.
#'
#' @param has_ma Logical, whether the trend carries an `ma` term
#' @return Character scalar, empty when the term was not asked for
#' @noRd
ma_innovations_stanblock <- function(has_ma) {
  if (!isTRUE(has_ma)) {
    return("")
  }
  paste0(
    "matrix[N_time_trend, N_lv_trend] ma_innovations_trend",
    " = scaled_innovations_trend;\n",
    "      // Moving-average term of order one: row i adds theta times\n",
    "      // innovation row i - 1\n",
    "      ma_innovations_trend[2:N_time_trend]\n",
    "        += diag_post_multiply(\n",
    "          scaled_innovations_trend[1:(N_time_trend - 1)], theta1_trend);"
  )
}

#' Stan declaration fixing the factor innovations at unit scale
#'
#' Used where the model samples every loading. See
#' `samples_factor_loadings()` for the identification argument.
#'
#' @param dim Stan dimension of the factors.
#' @return Character scalar, led by a newline.
#' @noRd
unit_factor_scale_scode <- function(dim = "N_lv_trend") {
  checkmate::assert_string(dim, min.chars = 1L)
  paste0(
    "\n    // Sampled loadings absorb the factors' scale, and unit",
    "\n    // innovations identify it",
    "\n    vector<lower=0>[", dim, "] sigma_trend = rep_vector(1.0, ",
    dim, ");"
  )
}

#' Refuse a prior on a factor scale the model fixes
#'
#' With every loading sampled, `sigma_trend` is 1 and `L_Omega_trend`
#' is the identity. A prior on either has nothing to act on.
#'
#' @param prior A `brmsprior`, or NULL.
#' @param unit_factors Whether the model samples every loading.
#' @return `NULL`, invisibly, when nothing is refused.
#' @noRd
refuse_unit_factor_priors <- function(prior, unit_factors) {
  checkmate::assert_flag(unit_factors)
  if (!unit_factors || is.null(prior)) {
    return(invisible(NULL))
  }
  fixed <- c("sigma_trend", "L_Omega_trend")
  given <- fixed[vapply(fixed, function(cls) {
    !is.null(extract_prior_string(prior, cls))
  }, logical(1L))]
  if (length(given)) {
    stop(insight::format_error(c(
      paste0("'", given[1L], "' is fixed in a factor model with ",
             "sampled loadings."),
      x = paste0("The loadings 'Z' set the scale and correlation of ",
                 "unit-scale, uncorrelated factors."),
      i = "Set a prior on 'Z' or fix loadings with 'trend_map'."
    )), call. = FALSE)
  }
  invisible(NULL)
}

#' Stan statement scaling independent innovations by their SDs
#'
#' Column j of the innovations takes `sigma_trend[j]`. The
#' column-scaling product costs `O(T n)`, against `O(T n^2)` for a
#' product with a dense diagonal matrix.
#'
#' @return Character scalar
#' @noRd
independent_scaling_stancode <- function() {
  paste0("scaled_innovations_trend",
         " = diag_post_multiply(innovations_trend, sigma_trend);")
}

#' Stan text opening a loop over groups with each group's members
#'
#' `group_members_trend` lists each group's series in ascending order,
#' the order `scaled_innovations_trend` takes them in. Position `k`
#' names the same series in both. Every grouped init opens with this.
#'
#' @return Character scalar opening a per-group loop
#' @noRd
group_members_stanblock <- function() {
  paste0(
    "      for (g_idx in 1:N_groups_trend) {\n",
    "        array[N_subgroups_trend] int members\n",
    "          = group_members_trend[g_idx];\n"
  )
}

#' Stan text for the scalar stationary start of one lag
#'
#' Independent innovations at one lag. Without a moving-average term the
#' t = 1 innovation divided by `sqrt(1 - ar1^2)` has the stationary
#' marginal. With one, `lv_1` given the first innovation, which the
#' t = 2 step reuses, has mean `eps_1` and standard deviation
#' `(ar1 + theta1) sigma / sqrt(1 - ar1^2)`. `CAR()` takes the first
#' form, the stationary marginal over a unit gap.
#'
#' @param has_ma Whether a moving-average term applies
#' @return Character scalar of Stan statements
#' @noRd
scalar_init_stanblock <- function(has_ma) {
  checkmate::assert_flag(has_ma)
  if (!has_ma) {
    return(paste0(
      "      // Stationary start: the t = 1 innovation scaled to the\n",
      "      // marginal SD sigma / sqrt(1 - ar1^2)\n",
      "      lv_trend[1] = scaled_innovations_trend[1]\n",
      "                    ./ sqrt(1 - square(ar1_trend))';"
    ))
  }
  paste0(
    "      // ARMA(1,1) stationary start: lv_1 takes its conditional\n",
    "      // mean, the t = 1 innovation, plus the conditional spread\n",
    "      //   (ar1 + theta1) * sigma / sqrt(1 - ar1^2).\n",
    "      // The pair (lv_1, eps_1) then holds its stationary joint\n",
    "      // distribution, and eps_1 reaches the t = 2 step unchanged.\n",
    "      lv_trend[1] = scaled_innovations_trend[1]\n",
    "                    + ((ar1_trend + theta1_trend) .* sigma_trend\n",
    "                       ./ sqrt(1 - square(ar1_trend))\n",
    "                       .* init_innovations_trend)';"
  )
}

#' Stan text for the independent AR(p) stationary start
#'
#' Each series draws its first `p` states through
#' `ar_stationary_init()`, from its own partial autocorrelations.
#'
#' @param max_lag Order p, at least 2
#' @return Character scalar of Stan statements
#' @noRd
pacf_init_stanblock <- function(max_lag) {
  checkmate::assert_int(max_lag, lower = 2L)
  idx_init <- seq_len(max_lag)
  paste0(
    "      // AR(p) stationary initialisation: the first ", max_lag,
    " states are\n",
    "      // drawn from their joint stationary distribution, built\n",
    "      // from the partial autocorrelations by the\n",
    "      // Levinson-Durbin recursion.\n",
    "      for (j in 1:N_lv_trend) {\n",
    "        vector[", max_lag, "] pacf_j;\n",
    "        vector[", max_lag, "] z_j;\n",
    paste0("        pacf_j[", idx_init, "] = ar", idx_init,
           "_pacf_trend[j];", collapse = "\n"), "\n",
    paste0("        z_j[", idx_init, "] = innovations_trend[",
           idx_init, ", j];", collapse = "\n"), "\n",
    "        lv_trend[1:", max_lag, ", j]\n",
    "          = ar_stationary_init(pacf_j, sigma_trend[j], z_j);\n",
    "      }"
  )
}

#' Stan text for a joint stationary start over a set of series
#'
#' The first `p` latent states of a contiguous AR(p) over one set of
#' series: every series at once, the members of one group, or a single
#' series. `idx` names the set as a Stan multi-index, `":"`, `"members"`
#' or `"{j_ser}"`. Every coefficient, variate and state is taken
#' through it.
#'
#' The `p` most recent states of a stationary AR(p) path `x` have the
#' covariance `ar_diag_joint_var()` returns. At `p = 1` it has the
#' closed form `Sigma[a, b] / (1 - phi[a] phi[b])`. Its Cholesky factor maps
#' standard variates onto them. Block i of the drawn state is the i-th
#' most recent state.
#'
#' Without a moving-average term the drawn states are `lv_1, ..., lv_p`
#' themselves, from innovation rows 1 to p.
#'
#' With one, the latent path is the filter `lv_t = x_t + theta x_{t-1}`
#' of `x` driven by the same innovations. The states `x_{p-1}, ..., x_0`
#' are drawn, taking innovation rows 1 to p - 1 and
#' `init_innovations_trend` as variates. `x_p` steps from them with the
#' innovation at t = p, which the step at t = p + 1 reuses. The filter
#' then gives `lv_1, ..., lv_p`. At `p = 1` this is
#' `lv_1 = eps_1 + (phi + theta) x_0`, the conditional of `lv_1` on the
#' innovation the t = 2 step reuses. Only the AR(p) covariance is
#' factored. It is positive definite for every stationary draw, those
#' where the AR and MA polynomials share a root included.
#'
#' Under Gaussian innovations the drawn states hold the stationary law.
#' Under Student-t innovations they hold its mean and covariance.
#'
#' @param max_lag Order p
#' @param dim_expr Stan dimension of the set
#' @param cov_expr Stan expression for the set's innovation covariance
#' @param idx Stan multi-index naming the set's series
#' @param has_ma Whether a moving-average term applies
#' @param header Comment lines introducing the block
#' @param open,close Lines wrapping the block
#' @param indent Leading whitespace inside the wrapper
#' @return Character scalar of Stan statements
#' @noRd
joint_init_stanblock <- function(max_lag, dim_expr, cov_expr, idx,
                                 has_ma, header = "",
                                 open = "      {\n",
                                 close = "      }",
                                 indent = "        ") {
  checkmate::assert_int(max_lag, lower = 1L)
  checkmate::assert_string(idx, min.chars = 1L)
  checkmate::assert_flag(has_ma)
  single <- identical(dim_expr, "1")
  # A parameter vector at the set's series. The set of every series is
  # the whole vector.
  at <- function(name) {
    if (identical(idx, ":")) name else paste0(name, "[", idx, "]")
  }
  # `k` copies of the set's dimension, folded at one.
  times <- function(k) {
    ifelse(k == 1L, dim_expr, paste0(k, " * ", dim_expr))
  }
  # Blocks k of a vector stacking one value per series of the set.
  blk <- function(k) {
    if (single) return(paste0(k, ":", k))
    paste0(ifelse(k == 1L, "1", paste0(times(k - 1L), " + 1")), ":",
           times(k))
  }
  line <- function(...) paste0(indent, ..., ";\n", collapse = "")
  lags <- seq_len(max_lag)
  n_state <- if (single) max_lag else times(max_lag)
  state_cov <- if (max_lag == 1L) {
    paste0(cov_expr, " ./ (1 - phi_init[1] * phi_init[1]')")
  } else {
    paste0("ar_diag_joint_var(", cov_expr, ", phi_init)")
  }
  rows <- seq_len(max_lag - has_ma)
  draw <- paste0(
    line("array[", max_lag, "] vector[", dim_expr, "] phi_init"),
    line("phi_init[", lags, "] = ", at(paste0("ar", lags, "_trend"))),
    line("vector[", n_state, "] z_init"),
    if (length(rows) > 0L) {
      line("z_init[", blk(rows), "] = innovations_trend[", rows, ", ", idx,
           "]'")
    },
    if (has_ma) {
      line("z_init[", blk(max_lag), "] = ", at("init_innovations_trend"))
    },
    line("vector[", n_state, "] state_init\n",
         indent, "  = cholesky_decompose(", state_cov, ") * z_init")
  )
  fill <- if (has_ma) {
    older <- seq_len(max_lag - 1L)
    theta <- at("theta1_trend")
    paste0(
      line("lv_trend[", max_lag, ", ", idx, "]\n",
           indent, "  = (scaled_innovations_trend[", max_lag, ", ", idx,
           "]'\n",
           paste0(indent, "     + phi_init[", lags, "] .* state_init[",
                  blk(lags), "]\n", collapse = ""),
           indent, "     + ", theta, " .* state_init[", blk(1L), "])'"),
      if (length(older) > 0L) {
        line("lv_trend[", max_lag - older, ", ", idx, "]\n",
             indent, "  = (state_init[", blk(older), "]\n",
             indent, "     + ", theta, " .* state_init[", blk(older + 1L),
             "])'")
      }
    )
  } else {
    line("lv_trend[", max_lag - lags + 1L, ", ", idx, "] = state_init[",
         blk(lags), "]'")
  }
  paste0(header, open, draw, fill, close)
}

#' Stanvars of an AR trend
#'
#' The coefficients, under each `coef_sharing` mode, the stationary
#' start and the AR(p) recursion over the active lags.
#' `assemble_shared_innovation_trend()` adds the rest.
#'
#' @param trend_specs Trend specification from `AR()`
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_ar_trend_stanvars <- function(trend_specs, data_info,
                                       prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  ar_lags <- resolve_active_lags(trend_specs$p)
  checkmate::assert_integerish(ar_lags, lower = 1, any.missing = FALSE)
  has_ma <- trend_specs$ma %||% FALSE
  max_lag <- max(ar_lags)
  # The start branch below and the gate on the joint stationary
  # functions both use `cor`. Sampled loadings fix the factor
  # correlation at the identity, which gives each factor its own
  # independent start.
  cor <- isTRUE(trend_specs$cor %||% FALSE) &&
    !samples_factor_loadings(trend_specs)
  has_gr <- named_var(trend_specs$gr)
  # A contiguous lag set above one lag samples partial
  # autocorrelations, which makes every draw jointly stationary. The
  # multi-lag starts below need that guarantee. A sparse set bounds
  # its coefficients one at a time and lacks it.
  pacf_lags <- ar_lags_stationary(ar_lags)
  stationary_start <- identical(max_lag, 1L) || pacf_lags
  # Independent innovations at one lag take the scalar closed form.
  # Above one lag without a moving-average term, each series takes its
  # own partial autocorrelations through `ar_stationary_init()`.
  scalar_start <- identical(max_lag, 1L) && !cor
  pacf_init_start <- pacf_lags && !cor && !has_ma
  # Every other stationary shape takes `joint_init_stanblock()`:
  # correlated or grouped innovations at any order, or a
  # moving-average term above one lag.
  joint_start <- stationary_start && !scalar_start && !pacf_init_start
  # A moving-average start needs one vector of standard variates
  # beyond the innovation rows it takes.
  ma_variates <- stationary_start && has_ma
  innov_df <- trend_specs$df %||% Inf
  components <- list()

  # `build_ar_coef_stanvars()` writes the coefficients for each
  # `coef_sharing` mode
  coef_sharing <- ar_coef_sharing(trend_specs)
  if (pacf_lags) {
    components <- append(
      components,
      list(ar_pacf_functions_stanvar(with_init = pacf_init_start))
    )
  }
  if (joint_start && max_lag > 1L) {
    components <- append(components,
                         list(stationary_joint_functions_stanvar("ar")))
  }
  ar_blocks <- build_ar_coef_stanvars(
    ar_lags = ar_lags,
    coef_sharing = coef_sharing,
    prior = prior
  )
  components <- append(components, ar_blocks)

  if (ma_variates) {
    components <- append(components, list(
      brms::stanvar(
        name = "ar_arma_init_parameters",
        scode = "vector[N_lv_trend] init_innovations_trend;",
        block = "parameters"
      ),
      brms::stanvar(
        name = "ar_arma_init_model",
        scode = paste(
          c("// Standard variates for the ARMA stationary start",
            innovation_vector_prior("init_innovations_trend",
                                    "N_lv_trend", innov_df)),
          collapse = "\n"
        ),
        block = "model"
      )
    ))
  }

  # The first `p` latent states start at the joint distribution the
  # process settles into. `scalar_init_stanblock()`,
  # `pacf_init_stanblock()` and `joint_init_stanblock()` give each
  # shape's law. A grouped trend scales its innovations within each
  # group, which makes the group the set one covariance spans.
  # `stationary_correlated_params()` and `stationary_group_params()`
  # give marginal prediction the same covariance. A sparse lag set
  # keeps the raw innovation start. Its declared bounds leave
  # stationarity unchecked. `cholesky_decompose` would reject some of
  # its draws.
  ar_init_block <- if (scalar_start) {
    scalar_init_stanblock(has_ma)
  } else if (pacf_init_start) {
    pacf_init_stanblock(max_lag)
  } else if (joint_start) {
    # The set of series one covariance spans: a group's members, every
    # series when their innovations correlate, or each series alone.
    set <- if (has_gr) {
      list(dim = "N_subgroups_trend", cov = "Sigma_group_trend[g_idx]",
           idx = "members", open = group_members_stanblock(),
           label = "each group")
    } else if (cor) {
      list(dim = "N_lv_trend", cov = "Sigma_trend", idx = ":",
           open = "      {\n", label = "the correlated series")
    } else {
      list(dim = "1",
           cov = "rep_matrix(square(sigma_trend[j_ser]), 1, 1)",
           idx = "{j_ser}",
           open = "      for (j_ser in 1:N_lv_trend) {\n",
           label = "each series")
    }
    joint_init_stanblock(
      max_lag = max_lag, dim_expr = set$dim, cov_expr = set$cov,
      idx = set$idx, has_ma = has_ma,
      header = paste0(
        "      // ", if (has_ma) "ARMA(p, 1)" else "AR(p)",
        " stationary initialisation over ", set$label, ":\n",
        "      // the first ",
        if (max_lag == 1L) "state" else paste(max_lag, "states"),
        " from the joint stationary covariance.\n"
      ),
      open = set$open
    )
  } else {
    paste0(
      "      // The first ", max_lag, " states are the innovations. A sparse\n",
      "      // lag set bounds its coefficients one at a time, which\n",
      "      // leaves the joint stationary distribution undefined for\n",
      "      // some draws.\n",
      "      for (i in 1:", max_lag, ") {\n",
      "        lv_trend[i, :] = ",
      if (has_ma) "ma_innovations_trend" else "scaled_innovations_trend",
      "[i, :];\n",
      "      }"
    )
  }

  # Each lag's coefficient scales the state that lag back. Scalar
  # loops differentiate faster than a row update per occasion at the
  # dimensions these trends take.
  innovations <- if (has_ma) "ma_innovations_trend" else
    "scaled_innovations_trend"
  ar_sum <- paste0("ar", ar_lags, "_trend[j] * lv_trend[i - ", ar_lags,
                   ", j]", collapse = "\n            + ")
  ar_tparameters_stanvar <- brms::stanvar(
    name = "ar_tparameters",
    scode = paste0(
      "\n      ", ma_innovations_stanblock(has_ma), "\n\n",
      ar_init_block, "\n\n",
      "      // AR dynamics\n",
      "      for (i in ", max_lag + 1, ":N_time_trend) {\n",
      "        for (j in 1:N_lv_trend) {\n",
      "          lv_trend[i, j] = ", ar_sum, "\n",
      "            + ", innovations, "[i, j];\n",
      "        }\n",
      "      }\n"
    ),
    block = "tparameters"
  )
  components <- append(components, list(ar_tparameters_stanvar))
  assemble_shared_innovation_trend(components, trend_specs, data_info, prior)
}

#' Hyperpriors on a VAR or VARMA coefficient prior
#'
#' The means and precisions are declared `array[2] vector[lags]`:
#' element `[1]` for the diagonal and `[2]` for the off-diagonal. One
#' statement on each array element covers every lag. A scalar
#' statement on the whole array fails Stan's dimension check.
#'
#' @param prior The prior table, for user overrides
#' @param mu,omega Names of the mean and precision parameters
#' @return Stan model-block code
#' @noRd
var_hyperprior_statements <- function(prior, mu, omega) {
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  checkmate::assert_string(mu, min.chars = 1L)
  checkmate::assert_string(omega, min.chars = 1L)
  paste0(
    "for (k in 1:2) {\n",
    "        ", mu, "[k] ~ ", get_trend_parameter_prior(prior, mu), ";\n",
    "        ", omega, "[k] ~ ", get_trend_parameter_prior(prior, omega),
    ";\n      }"
  )
}

#' Stan functions of the stationary VAR map of Heaps (2023)
#'
#' The map takes each lag's unconstrained matrix to a partial
#' autocorrelation matrix and these, with the innovation covariance,
#' to the stationary coefficients and the stationary autocovariances at
#' lags 0 to `p - 1`. `stationary_map()` runs both steps. Every VAR
#' and VARMA program calls it once for each set of lag matrices.
#'
#' @return A `functions` block stanvar
#' @noRd
var_heaps_functions_stanvar <- function() {
  brms::stanvar(
    name = "var_functions",
    scode = "
      /**
       * Square root of a symmetric positive definite matrix, by its
       * eigendecomposition. The eigenvalues are the diagonal of
       * V' A V, which takes one decomposition where a call for the
       * values takes a second. The test for definiteness is relative to
       * the largest eigenvalue. A covariance in small data units has
       * every eigenvalue small and stays well conditioned.
       * @param A Symmetric positive definite matrix (m x m)
       * @return The symmetric square root of A
       */
      matrix sqrtm(matrix A) {
        int m = rows(A);
        matrix[m, m] evecs = eigenvectors_sym(A);
        vector[m] eigenvals = diagonal(quad_form(A, evecs));
        if (min(eigenvals) <= 1e-12 * max(eigenvals)) {
          reject(\"sqrtm needs a positive definite matrix\");
        }
        return tcrossprod(diag_post_multiply(evecs,
                                             sqrt(sqrt(eigenvals))));
      }

      /**
       * Partial autocorrelation matrix of one lag from an unconstrained
       * matrix: (I + A A')^(-1/2) A, whose singular values lie in [0, 1)
       * @param P_real Unconstrained matrix (m x m)
       * @return Partial autocorrelation matrix
       */
      matrix partial_autocorrelation(matrix P_real) {
        return mdivide_left_spd(sqrtm(add_diag(tcrossprod(P_real), 1.0)),
                                P_real);
      }

      /**
       * Stationary VAR coefficients and autocovariances from partial
       * autocorrelations, by the reverse mapping of Heaps (2023)
       * @param P Partial autocorrelation matrices, one per lag
       * @param Sigma Innovation covariance matrix
       * @return Row 1 holds the coefficients of lags 1 to p. Row 2 holds
       *   the autocovariances at lags 0 to p - 1.
       */
      array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {
        int p = size(P);
        int m = rows(Sigma);
        array[p, p] matrix[m, m] phi_for;
        array[p, p] matrix[m, m] phi_rev;
        array[p + 1] matrix[m, m] Sigma_for;
        array[p + 1] matrix[m, m] Sigma_rev;
        matrix[m, m] S_for;
        matrix[m, m] S_rev;
        array[p] matrix[m, m] S_for_list;
        array[p + 1] matrix[m, m] Gamma_trans;
        array[2, p] matrix[m, m] phiGamma;

        // Backward from lag p: the forward innovation covariance at
        // each lag and its square root
        Sigma_for[p + 1] = Sigma;
        for (s in 1:p) {
          S_rev = sqrtm(add_diag(-tcrossprod(P[p - s + 1]), 1.0));
          S_for_list[p - s + 1] = mdivide_right_spd(
            mdivide_left_spd(S_rev,
                             sqrtm(quad_form_sym(Sigma_for[p - s + 2],
                                                 S_rev))),
            S_rev
          );
          Sigma_for[p - s + 1] = tcrossprod(S_for_list[p - s + 1]);
        }

        // Forward from lag 1: the Durbin-Levinson recursion. Each step
        // gives one lag's coefficients and one autocovariance.
        Sigma_rev[1] = Sigma_for[1];
        Gamma_trans[1] = Sigma_for[1];
        for (s in 0:(p - 1)) {
          S_for = S_for_list[s + 1];
          S_rev = sqrtm(Sigma_rev[s + 1]);
          phi_for[s + 1, s + 1] = mdivide_right_spd(S_for * P[s + 1], S_rev);
          phi_rev[s + 1, s + 1] = mdivide_right_spd(S_rev * P[s + 1]', S_for);
          Gamma_trans[s + 2] = phi_for[s + 1, s + 1] * Sigma_rev[s + 1];
          for (k in 1:s) {
            phi_for[s + 1, k] = phi_for[s, k]
                                - phi_for[s + 1, s + 1] * phi_rev[s, s - k + 1];
            phi_rev[s + 1, k] = phi_rev[s, k]
                                - phi_rev[s + 1, s + 1] * phi_for[s, s - k + 1];
            Gamma_trans[s + 2] = Gamma_trans[s + 2]
                                 + phi_for[s, k] * Gamma_trans[s + 2 - k];
          }
          Sigma_rev[s + 2] = Sigma_rev[s + 1]
                             - quad_form_sym(Sigma_for[s + 1],
                                             phi_rev[s + 1, s + 1]');
        }

        for (i in 1:p) {
          phiGamma[1, i] = phi_for[p, i];
          phiGamma[2, i] = Gamma_trans[i]';
        }
        return phiGamma;
      }

      /**
       * Stationary coefficients and autocovariances from unconstrained
       * matrices, one per lag: each lag's partial autocorrelation, then
       * the reverse mapping
       * @param A Unconstrained matrices, one per lag
       * @param Sigma Innovation covariance matrix
       * @return As rev_mapping returns them
       */
      array[,] matrix stationary_map(array[] matrix A, matrix Sigma) {
        int p = size(A);
        int m = rows(Sigma);
        array[p] matrix[m, m] P;
        for (i in 1:p) {
          P[i] = partial_autocorrelation(A[i]);
        }
        return rev_mapping(P, Sigma);
      }

      /**
       * The off-diagonal elements of a square matrix, column by column
       * @param A Square matrix (m x m)
       * @return Vector of the m (m - 1) off-diagonal elements
       */
      vector offdiag_elements(matrix A) {
        int m = rows(A);
        vector[m * (m - 1)] v;
        int k = 1;
        for (j in 1:m) {
          for (i in 1:m) {
            if (i != j) {
              v[k] = A[i, j];
              k += 1;
            }
          }
        }
        return v;
      }
    ",
    block = "functions"
  )
}


#' Stan statements for the exchangeable prior on one raw VAR matrix
#'
#' The diagonal takes mean `mu[1, lag]` and precision `omega[1, lag]`.
#' The off-diagonal elements take `mu[2, lag]` and `omega[2, lag]`.
#' This is the prior of Heaps (2023). `offdiag_elements()` gathers the
#' off-diagonal elements for one vectorised statement.
#'
#' @param coef Stan expression of the matrix
#' @param mu,omega Names of the hyperparameter arrays
#' @param lag Stan expression of the lag index
#' @param indent Spaces before each statement
#' @return Character scalar ending in a newline
#' @noRd
var_coef_prior_scode <- function(coef, mu, omega, lag, indent) {
  checkmate::assert_string(coef)
  checkmate::assert_string(mu)
  checkmate::assert_string(omega)
  checkmate::assert_string(lag)
  checkmate::assert_count(indent)
  pad <- strrep(" ", indent)
  law <- function(k) {
    paste0("normal(", mu, "[", k, ", ", lag, "], 1 / sqrt(", omega, "[",
           k, ", ", lag, "]));\n")
  }
  paste0(
    pad, "diagonal(", coef, ") ~ ", law(1L),
    pad, "offdiag_elements(", coef, ") ~ ", law(2L)
  )
}


#' Stanvars of a VAR or VARMA trend
#'
#' The states are sampled directly, each given its conditional mean
#' under the lag matrices `Phi_trend` and, with `ma = TRUE`, the
#' moving-average matrix `Theta_trend`. Both are mapped from
#' unconstrained matrices `A_trend` and `D_trend` into the stationary
#' and invertible regions (Heaps 2023). The states before the first
#' occasion take their joint stationary law, `Omega_trend`. A
#' grouped VAR maps each group's lags jointly and assembles
#' block-diagonal matrices over the groups.
#'
#' The cost of a gradient grows with the series. A VAR takes
#' `Omega_trend` from the autocovariances the map returns. A VARMA
#' solves a Lyapunov equation of side `(p m)^2`. A grouped VARMA solves
#' one per group, at side `(p s)^2` for `s` series. The conditional
#' means are one matrix product per lag. The states' density is one
#' triangular solve against `L_Sigma_trend`. Sampled loadings make the
#' innovations standard normal. Their density takes no solve.
#'
#' @param trend_specs Trend specification from `VAR()`
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_var_trend_stanvars <- function(trend_specs, data_info,
                                        prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  # `VAR()` checks its order and `validate_n_lv_ceiling()` the factor
  # count. A moving-average term is of order one.
  lags <- trend_specs$p
  checkmate::assert_int(lags, lower = 1)
  ma_lags <- if (isTRUE(trend_specs$ma)) 1L else 0L
  is_varma <- ma_lags > 0
  hierarchical_info <- extract_hierarchical_info(data_info, trend_specs)
  is_hierarchical <- !is.null(hierarchical_info)
  # The unset `trend_specs$n_lv` decides a factor model
  is_factor_model <- is_factor_model_spec(trend_specs$n_lv,
                                          data_info_n_series(data_info))
  # Sampled loadings absorb the factors' scale and correlation, which
  # fixes Sigma_trend at the identity
  unit_factors <- samples_factor_loadings(trend_specs)
  refuse_unit_factor_priors(prior, unit_factors)

  # `init_trend` stores the states before the first occasion. With a
  # moving-average term it also stores the MA error.
  init_dim_expr <- if (is_varma) {
    paste0("(N_lags_trend + ", ma_lags, ") * N_lv_trend")
  } else {
    "N_lags_trend * N_lv_trend"
  }

  # Every declaration and loop takes the lag count from this datum
  var_data_stanvar <- brms::stanvar(
    x = lags,
    name = "N_lags_trend",
    scode = "int<lower=1> N_lags_trend;",
    block = "data"
  )

  var_specific_params <- if (is_hierarchical) {
    paste0(
      "      // Each group's raw coefficient matrices, one per lag, under\n",
      "      // the shared hyperpriors\n",
      "      array[N_groups_trend, N_lags_trend]\n",
      "        matrix[N_subgroups_trend, N_subgroups_trend] A_group_trend;"
    )
  } else {
    paste0(
      "      // Raw coefficient matrices, one per lag\n",
      "      array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] A_trend;",
      if (!unit_factors) {
        paste0(
          "\n\n      // Innovation scales and correlation\n",
          "      vector<lower=0>[N_lv_trend] sigma_trend;\n",
          "      cholesky_factor_corr[N_lv_trend] L_Omega_trend;"
        )
      }
    )
  }

  var_parameters_stanvar <- brms::stanvar(
    name = "var_parameters",
    scode = glue::glue("
      {var_specific_params}

      // Means and precisions of the raw coefficient priors,
      // [1] for the diagonal and [2] for the off-diagonal elements
      array[2] vector[N_lags_trend] Amu_trend;
      array[2] vector<lower=0>[N_lags_trend] Aomega_trend;

      // The states before the first occasion, and with an MA term
      // the MA error, under their joint stationary law
      vector[{init_dim_expr}] init_trend;
    "),
    block = "parameters"
  )

  # A VARMA samples raw MA matrices, which the map takes to the
  # invertible `Theta_trend` (Heaps 2023)
  var_ma_parameters_stanvar <- if (is_varma) {
    raw_ma <- if (is_hierarchical) {
      paste0("array[N_groups_trend, ", ma_lags,
             "] matrix[N_subgroups_trend, N_subgroups_trend] D_group_trend;")
    } else {
      paste0("array[", ma_lags, "] matrix[N_lv_trend, N_lv_trend] D_trend;")
    }
    brms::stanvar(
      name = "var_ma_parameters",
      scode = glue::glue("
      // Raw MA matrices, mapped to the invertible Theta_trend
      {raw_ma}

      // Means and precisions of the raw MA coefficient priors,
      // [1] for the diagonal and [2] for the off-diagonal elements
      array[2] vector[{ma_lags}] Dmu_trend;
      array[2] vector<lower=0>[{ma_lags}] Domega_trend;
      "),
      block = "parameters"
    )
  }

  # VAR/VARMA transformed parameters. `stationary_map()` returns each
  # lag's stationary coefficients and the stationary autocovariances at
  # lags 0 to p - 1. Applied to the raw MA matrices, it returns the
  # negated invertible MA coefficient (Heaps 2023). A VARMA takes its
  # autocovariances from `varma_autocov()`, per group in a grouped
  # trend. `joint_of()` places the autocovariances in `Omega_trend`,
  # with the MA error's blocks under a VARMA. The working arrays are
  # local to a block and are not saved with the draws.
  joint_of <- function(gamma) {
    if (is_varma) {
      paste0("varma_stationary_joint(", gamma, ", Sigma_trend)")
    } else {
      paste0("var_stationary_joint(", gamma, ")")
    }
  }
  zero_lags <- paste0(
    "rep_array(rep_matrix(0, N_lv_trend, N_lv_trend), N_lags_trend)"
  )
  var_tparams_main <- if (is_hierarchical) {
    paste0(
      "      // Each group's stationary coefficients, every lag mapped\n",
      "      // jointly from its partial autocorrelations, placed at the\n",
      "      // group's member series. The groups do not interact, and\n",
      "      // each group's Cholesky factor placed at its members is a\n",
      "      // lower triangular factor of Sigma_trend.\n",
      "      array[N_groups_trend, N_lags_trend]\n",
      "        matrix[N_subgroups_trend, N_subgroups_trend] Phi_group_trend;\n",
      "      cov_matrix[N_lv_trend] Sigma_trend\n",
      "        = rep_matrix(0, N_lv_trend, N_lv_trend);\n",
      "      matrix[N_lv_trend, N_lv_trend] L_Sigma_trend\n",
      "        = rep_matrix(0, N_lv_trend, N_lv_trend);\n",
      "      array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] Phi_trend = ",
      zero_lags, ";\n",
      if (is_varma) {
        paste0(
          "      array[", ma_lags, "] matrix[N_lv_trend, N_lv_trend]\n",
          "        Theta_trend = rep_array(",
          "rep_matrix(0, N_lv_trend, N_lv_trend), ", ma_lags, ");\n"
        )
      },
      "      matrix[", init_dim_expr, ", ", init_dim_expr, "] Omega_trend;\n",
      "      {\n",
      "        array[N_lags_trend] matrix[N_lv_trend, N_lv_trend]\n",
      "          Gamma_trend = ", zero_lags, ";\n",
      group_members_stanblock(),
      "        array[2, N_lags_trend]\n",
      "          matrix[N_subgroups_trend, N_subgroups_trend] result_group\n",
      "          = stationary_map(A_group_trend[g_idx],\n",
      "                           Sigma_group_trend[g_idx]);\n",
      "        Phi_group_trend[g_idx] = result_group[1];\n",
      "        Sigma_trend[members, members] = Sigma_group_trend[g_idx];\n",
      "        L_Sigma_trend[members, members] = L_group_trend[g_idx];\n",
      if (is_varma) {
        paste0(
          "        array[2, ", ma_lags, "]\n",
          "          matrix[N_subgroups_trend, N_subgroups_trend] result_ma\n",
          "          = stationary_map(D_group_trend[g_idx],\n",
          "                           Sigma_group_trend[g_idx]);\n",
          "        Theta_trend[1][members, members] = -result_ma[1, 1];\n",
          "        array[N_lags_trend]\n",
          "          matrix[N_subgroups_trend, N_subgroups_trend]\n",
          "          gamma_group = varma_autocov(Sigma_group_trend[g_idx],\n",
          "                          result_group[1], -result_ma[1, 1]);\n"
        )
      },
      "        for (lag in 1:N_lags_trend) {\n",
      "          Phi_trend[lag][members, members] = result_group[1, lag];\n",
      "          Gamma_trend[lag][members, members] = ",
      if (is_varma) "gamma_group[lag]" else "result_group[2, lag]", ";\n",
      "        }\n",
      "      }\n",
      "        Omega_trend = ", joint_of("Gamma_trend"), ";\n",
      "      }"
    )
  } else {
    paste0(
      if (unit_factors) {
        paste0(
          unit_factor_scale_scode(), "\n",
          "    cholesky_factor_corr[N_lv_trend] L_Omega_trend = ",
          "identity_matrix(N_lv_trend);\n\n"
        )
      },
      if (unit_factors) {
        paste0(
          "      matrix[N_lv_trend, N_lv_trend] L_Sigma_trend\n",
          "        = identity_matrix(N_lv_trend);\n",
          "      cov_matrix[N_lv_trend] Sigma_trend",
          " = identity_matrix(N_lv_trend);\n\n"
        )
      } else {
        paste0(
          "      matrix[N_lv_trend, N_lv_trend] L_Sigma_trend\n",
          "        = diag_pre_multiply(sigma_trend, L_Omega_trend);\n",
          "      cov_matrix[N_lv_trend] Sigma_trend\n",
          "        = multiply_lower_tri_self_transpose(L_Sigma_trend);\n\n"
        )
      },
      "      // Stationary coefficients from the partial autocorrelations\n",
      "      array[N_lags_trend] matrix[N_lv_trend, N_lv_trend] Phi_trend;\n",
      if (is_varma) {
        paste0("      array[", ma_lags, "] matrix[N_lv_trend, N_lv_trend] ",
               "Theta_trend;\n")
      },
      "      matrix[", init_dim_expr, ", ", init_dim_expr, "] Omega_trend;\n",
      "      {\n",
      "        array[2, N_lags_trend]\n",
      "          matrix[N_lv_trend, N_lv_trend] result_var\n",
      "          = stationary_map(A_trend, Sigma_trend);\n",
      "        Phi_trend = result_var[1];\n",
      if (is_varma) {
        paste0(
          "        array[2, ", ma_lags, "]\n",
          "          matrix[N_lv_trend, N_lv_trend] result_ma\n",
          "          = stationary_map(D_trend, Sigma_trend);\n",
          "        Theta_trend[1] = -result_ma[1, 1];\n"
        )
      },
      "        Omega_trend = ", joint_of(
        if (is_varma) {
          "varma_autocov(Sigma_trend, Phi_trend, Theta_trend[1])"
        } else {
          "result_var[2]"
        }
      ), ";\n",
      "      }"
    )
  }

  varma_tparams <- if (is_varma) {
    paste0(
      "      // The MA error before the first occasion, from init_trend\n",
      "      vector[N_lv_trend] ma_init_trend = init_trend[\n",
      "        (N_lags_trend * N_lv_trend + 1):",
      "((N_lags_trend + 1) * N_lv_trend)];"
    )
  } else {
    ""
  }

  var_tparameters_stanvar <- brms::stanvar(
    name = "var_tparameters",
    scode = paste0(var_tparams_main, "\n\n", varma_tparams),
    block = "tparameters"
  )

  # The unconstrained coefficient matrices take exchangeable normal
  # priors, one law for the diagonal and one for the off-diagonal
  # elements, each a single vectorised statement per lag. A grouped
  # VAR gives every group its own raw matrices under the shared
  # hyperpriors.
  raw_coef_priors <- function(raw, mu, omega, n_lags) {
    if (is_hierarchical) {
      paste0(
        "      for (g_idx in 1:N_groups_trend) {\n",
        "        for (lag in 1:", n_lags, ") {\n",
        var_coef_prior_scode(
          paste0(sub("_trend$", "_group_trend", raw), "[g_idx, lag]"),
          mu, omega, "lag", indent = 10L
        ),
        "        }\n",
        "      }"
      )
    } else {
      paste0(
        "      for (lag in 1:", n_lags, ") {\n",
        var_coef_prior_scode(paste0(raw, "[lag]"), mu, omega, "lag",
                             indent = 8L),
        "      }"
      )
    }
  }
  var_priors_block <- paste0(
    "      // Raw coefficients under the hyperpriors\n",
    raw_coef_priors("A_trend", "Amu_trend", "Aomega_trend", "N_lags_trend")
  )

  # `Dmu_trend` and `Domega_trend` are declared `array[2] vector`.
  # `var_hyperprior_statements()` emits a user override per element.
  varma_ma_priors <- if (is_varma) {
    paste0(
      "      // Raw MA coefficients under their hyperpriors\n",
      raw_coef_priors("D_trend", "Dmu_trend", "Domega_trend", ma_lags),
      "\n\n",
      var_hyperprior_statements(prior, "Dmu_trend", "Domega_trend")
    )
  } else ""

  # The conditional mean of every state, one row per occasion. Lag i
  # takes the state i occasions back, or, for the first i occasions,
  # a state before the series from `init_trend`. That vector runs most
  # recent first as `Omega_trend` orders it: block k is lv_{1 - k},
  # which puts lv_{t - i} at block i - t + 1. Each lag adds one matrix
  # product over every occasion. A VARMA then adds the MA error at
  # t - 1, the state less its full conditional mean. Because that mean
  # includes the MA term added at t - 1, the MA loop steps through the
  # occasions in order.
  init_lag_scode <- paste0(
    "int init_idx = i - t + 1;\n",
    "          mu_lv_trend[t] += (Phi_trend[i] * init_trend[",
    "((init_idx - 1) * N_lv_trend + 1):(init_idx * N_lv_trend)])';"
  )
  ar_mean_scode <- paste0(
    "      for (i in 1:N_lags_trend) {\n",
    "        for (t in 1:min(i, N_time_trend)) {\n",
    "          ", init_lag_scode, "\n",
    "        }\n",
    "        if (N_time_trend > i) {\n",
    "          mu_lv_trend[(i + 1):N_time_trend]\n",
    "            += lv_trend[1:(N_time_trend - i)] * Phi_trend[i]';\n",
    "        }\n",
    "      }"
  )
  var_mean_scode <- if (is_varma) {
    paste0(
      ar_mean_scode, "\n",
      "      mu_lv_trend[1] += (Theta_trend[1] * ma_init_trend)';\n",
      "      for (t in 2:N_time_trend) {\n",
      "        mu_lv_trend[t] += (lv_trend[t - 1] - mu_lv_trend[t - 1])\n",
      "          * Theta_trend[1]';\n",
      "      }"
    )
  } else {
    ar_mean_scode
  }

  # A grouped VAR has no `L_Omega_trend`. Its grouped correlation
  # stanvars write their own priors.
  omega_prior <- if (!is_hierarchical && !unit_factors) {
    paste0("// Correlation prior on the Cholesky factor\n      ",
           "L_Omega_trend ~ ",
           get_trend_parameter_prior(prior, "L_Omega_trend"), ";")
  } else {
    ""
  }

  var_hyperpriors <- var_hyperprior_statements(
    prior, "Amu_trend", "Aomega_trend"
  )

  # Each state's multivariate normal density given its conditional mean,
  # summed over the occasions. Unit factors have independent standard
  # normal innovations. Correlated innovations take one triangular solve.
  state_density <- if (unit_factors) {
    "target += std_normal_lpdf(to_vector(lv_trend - mu_lv_trend));"
  } else {
    paste0(
      "target += std_normal_lpdf(to_vector(mdivide_left_tri_low(\n",
      "                  L_Sigma_trend, (lv_trend - mu_lv_trend)')))\n",
      "                - N_time_trend * sum(log(diagonal(L_Sigma_trend)));"
    )
  }

  var_model_stanvar <- brms::stanvar(
    name = "var_model",
    scode = glue::glue("
      // The states before the first occasion, at their joint
      // stationary law
      init_trend ~ multi_normal_cholesky(mu_init_trend,
                                         cholesky_decompose(Omega_trend));

      // The conditional mean of every state
      matrix[N_time_trend, N_lv_trend] mu_lv_trend
        = rep_matrix(0.0, N_time_trend, N_lv_trend);
{var_mean_scode}

      // Each state's density given its conditional mean
      {state_density}

      {var_priors_block}

      {varma_ma_priors}

      {omega_prior}

      {var_hyperpriors}
    "),
    block = "model"
  )

  # The innovation scales take the shared prior lookup. A grouped VAR
  # scales through `sigma_group_trend`. Sampled loadings fix the factor
  # scale at 1. The hyperpriors above are written per element,
  # which their `array[2] vector` declaration requires.
  var_centralized_priors <- if (!is_hierarchical && !unit_factors) {
    generate_trend_priors_stanvar(
      param_names = "sigma_trend",
      prior = prior,
      stanvar_name = "var_centralized_priors"
    )
  }

  matrix_z <- generate_matrix_z_multiblock_stanvars(
    is_factor_model,
    fixed_Z = trend_specs$fixed_Z,
    family = data_info$family,
    prior = prior,
    trend_obj = trend_specs
  )

  trend_computation <- generate_trend_computation_tparameters(
    data_info,
    identity_z = z_is_identity(is_factor_model, trend_specs$fixed_Z)
  )

  # A grouped VAR takes each group's covariance from the shared
  # grouped correlations, which the coefficient map needs first
  hierarchical <- grouped_trend_stanvars(
    hierarchical_info, data_info, prior, trend_specs,
    scale_innovations = FALSE
  )

  # The stationary law of the first states has mean zero
  var_tdata_stanvar <- brms::stanvar(
    name = "var_tdata",
    scode = paste0("vector[", init_dim_expr, "] mu_init_trend = ",
                   "rep_vector(0.0, ", init_dim_expr, ");"),
    block = "tdata"
  )

  # A VAR samples its states directly, as parameters.
  lv_trend_stanvar <- brms::stanvar(
    name = "lv_trend",
    scode = "matrix[N_time_trend, N_lv_trend] lv_trend;",
    block = "parameters"
  )
  components <- list(
    matrix_z, var_heaps_functions_stanvar(),
    stationary_joint_functions_stanvar(if (is_varma) "varma" else "var"),
    var_data_stanvar, var_tdata_stanvar, var_parameters_stanvar,
    var_ma_parameters_stanvar,
    lv_trend_stanvar, hierarchical, var_tparameters_stanvar,
    var_model_stanvar, trend_computation
  )

  # The VAR coefficients are stated for the sampled factors. They
  # rotate with `Z_tilde` and `lv_trend_tilde`.
  factor_priors <- generate_factor_model(
    trend_specs, data_info, prior = prior,
    rotated = list(transitions = "Phi_trend")
  )
  do.call(combine_stanvars,
          c(components, list(factor_priors, var_centralized_priors)))
}

#' The time unit a CAR trend measures its gaps in
#'
#' The typical gap between two observations of one series, which the
#' axis record holds as `observation_gap`. `ar1_trend` and
#' `sigma_trend` are then the damping and the innovation scale over a
#' typical step, whatever unit the time column is recorded in. A
#' regular grid has gaps of one. The unit stays the same when another
#' series adds times to the grid. The fit and `forecast()` both take
#' it from the axis record through this function.
#'
#' @param time_axis The `time` entry of the axis record, or a list
#'   holding the occasions of one series as `values`
#' @return A positive number, 1 for a grid of one occasion
#' @noRd
car_time_scale <- function(time_axis) {
  checkmate::assert_list(time_axis)
  if (!is.null(time_axis$observation_gap)) {
    return(time_axis$observation_gap)
  }
  axis_observation_gap(list(as.numeric(time_axis$values)))
}

# Smallest gap a CAR trend steps over, in units of `car_time_scale()`
car_min_gap <- 1e-6

#' Gaps of a CAR trend in units of its time scale
#'
#' @param gaps Differences between consecutive times
#' @param scale The value `car_time_scale()` returns for the fitted grid
#' @return `gaps / scale`, no smaller than `car_min_gap`
#' @noRd
car_scaled_gaps <- function(gaps, scale) {
  checkmate::assert_numeric(gaps, any.missing = FALSE)
  checkmate::assert_number(scale, lower = 0, finite = TRUE)
  pmax(car_min_gap, gaps / scale)
}

#' Time gaps a CAR trend steps over
#'
#' Every series is observed on one shared grid. The gap between two
#' steps belongs to the grid.
#'
#' Step `t` spans the distance from `t - 1` in units of
#' `car_time_scale()`. The first has no predecessor and takes 1. The
#' floor `car_min_gap` keeps a repeated timestamp from collapsing the
#' autocorrelation to zero distance.
#'
#' A correlated CAR factorises one innovation covariance per distinct
#' gap. Both forms take step `t`'s gap as `gap_values[gap_index[t]]`.
#'
#' @param data_info Data information holding `time_axis`, the time
#'   entry of the axis record
#' @return List of `gaps`, the gap of each step; `gap_values`, the
#'   distinct gaps; `gap_index`, the position of each step's gap
#'   among them
#' @noRd
calculate_car_time_distances <- function(data_info) {
  # The axis record holds the occasions once and the time index is
  # `match()` into them. The gaps are the differences in that order.
  times <- data_info$time_axis$values
  checkmate::assert_numeric(times, min.len = 1L, any.missing = FALSE,
                            .var.name = "data_info$time_axis$values")
  dis_time <- c(1, car_scaled_gaps(diff(times),
                                   car_time_scale(data_info$time_axis)))

  # Gaps that differ by rounding alone share one factorisation
  gap_key <- signif(dis_time, 10)

  list(
    gaps = dis_time,
    gap_values = dis_time[!duplicated(gap_key)],
    gap_index = match(gap_key, unique(gap_key))
  )
}

#' Stan dynamics of a CAR trend with independent innovations
#'
#' @return A stanvars object for the transformed parameters block
#' @noRd
car_independent_dynamics_stanvars <- function() {
  combine_stanvars(
    brms::stanvar(
      name = "car_innovation_computation",
      scode = paste0(
        "matrix[N_time_trend, N_lv_trend] scaled_innovations_trend;\n  ",
        independent_scaling_stancode()
      ),
      block = "tparameters"
    ),
    brms::stanvar(
      name = "car_tparameters",
      scode = paste0(
        scalar_init_stanblock(has_ma = FALSE), "

      // The gap scales the decay and the innovation together. The
      // marginal variance is the same at every occasion.
      for (i in 2:N_time_trend) {
        real gap = gap_trend[gap_index_trend[i]];
        for (j in 1:N_lv_trend) {
          lv_trend[i, j] = pow(ar1_trend[j], gap) * lv_trend[i - 1, j]
                           + scaled_innovations_trend[i, j]
                             * sqrt((1 - pow(ar1_trend[j], 2 * gap))
                                    / (1 - square(ar1_trend[j])));
        }
      }
    "),
      block = "tparameters"
    )
  )
}

#' Stan dynamics of a CAR trend with correlated innovations
#'
#' The exact transition of the continuous-time process. Series `a`
#' and `b` hold covariance `Sigma_trend[a, b] / (1 - ar_a * ar_b)` at
#' every occasion. The innovations across a gap `d` take that
#' covariance times `1 - (ar_a * ar_b)^d`. The diagonal is the scaling
#' the independent form applies. A scaling of each series by its own
#' gap factor would hold the innovation correlation fixed at every
#' gap. Adding a time point between two others would then change the
#' model.
#'
#' `L_Omega_trend` is the correlation of the diffusion, the shocks
#' over an instant. `car_unit_coherence()` derives `Sigma_trend`, the
#' innovation covariance over a unit gap, from it. `sigma_trend` stays
#' the unit-gap innovation scale of each series. Series that damp at
#' different rates make some unit-gap correlations unreachable by any
#' continuous-time process. A correlation placed on the unit-gap
#' innovations directly admits those values. The innovation covariance
#' over a gap below one is then not positive definite.
#'
#' @return A stanvars object: `L_Omega_trend`,
#'   `Sigma_trend` and the dynamics
#' @noRd
car_correlated_dynamics_stanvars <- function() {
  coherence <- brms::stanvar(
    name = "car_unit_coherence",
    scode = "
      /**
       * Ratio of the unit-gap innovation covariance of a continuous-time
       * AR(1) to the product of the series' unit-gap scales and their
       * diffusion correlation. One on the diagonal.
       * @param ar Damping of each series over a unit gap, in (0, 1)
       * @return Symmetric matrix with entries in (0, 1]
       */
      matrix car_unit_coherence(vector ar) {
        int K = rows(ar);
        vector[K] rate = -log(ar);
        vector[K] gain = sqrt(2 * rate ./ (1 - square(ar)));
        matrix[K, K] out;
        for (a in 1:K) {
          out[a, a] = 1;
          for (b in (a + 1):K) {
            out[a, b] = gain[a] * gain[b] * (1 - ar[a] * ar[b])
                        / (rate[a] + rate[b]);
            out[b, a] = out[a, b];
          }
        }
        return out;
      }
    ",
    block = "functions"
  )
  dynamics <- brms::stanvar(
    name = "car_tparameters",
    scode = "
      {
        matrix[N_lv_trend, N_lv_trend] log_ar_cross_trend
          = log(ar1_trend * ar1_trend');
        // Covariance the states hold at every occasion
        matrix[N_lv_trend, N_lv_trend] Gamma_trend
          = Sigma_trend ./ -expm1(log_ar_cross_trend);
        // Cholesky factor of the innovation covariance, one per
        // distinct gap
        array[N_gaps_trend] matrix[N_lv_trend, N_lv_trend] L_gap_trend;
        for (g in 1:N_gaps_trend) {
          L_gap_trend[g] = cholesky_decompose(
            Gamma_trend .* -expm1(gap_trend[g] * log_ar_cross_trend));
        }

        // Start at the stationary law of the continuous-time AR(1)
        lv_trend[1] = (cholesky_decompose(Gamma_trend)
                       * innovations_trend[1]')';

        // The gap scales the decay and the innovation together
        for (i in 2:N_time_trend) {
          lv_trend[i] = (pow(ar1_trend, gap_trend[gap_index_trend[i]])
                           .* lv_trend[i - 1]'
                         + L_gap_trend[gap_index_trend[i]]
                           * innovations_trend[i]')';
        }
      }
    ",
    block = "tparameters"
  )
  do.call(combine_stanvars, c(
    list(coherence),
    innovation_correlation_stanvars(
      scale = "car_unit_coherence(ar1_trend)"
    ),
    list(dynamics)
  ))
}

#' Stanvars of a CAR trend
#'
#' A continuous-time AR(1) trend, which decays by `ar1_trend^gap` over
#' the time gaps the data records. `cor = TRUE` correlates the
#' innovations of the series. `build_stan_components()` refuses factor
#' models, from the registered `supports_factors`. Grouped correlations
#' are refused before assembly.
#'
#' `ar1_trend` lies strictly inside (0, 1) because Stan raises it to a
#' real power. CAR scales its own innovations: the shared innovation
#' system emits none of its parameters or priors.
#'
#' @param trend_specs Trend specification from `CAR()`
#' @param data_info Data information list holding the time axis the
#'   gaps come from
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_car_trend_stanvars <- function(trend_specs, data_info,
                                        prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  has_cor <- isTRUE(trend_specs$cor)
  df <- trend_specs$df %||% Inf
  gaps <- calculate_car_time_distances(data_info)

  combine_stanvars(
    generate_matrix_z_multiblock_stanvars(
      is_factor_model = FALSE,
      fixed_Z = trend_specs$fixed_Z,
      prior = prior,
      trend_obj = trend_specs
    ),
    brms::stanvar(
      x = length(gaps$gap_values),
      name = "N_gaps_trend",
      scode = "int<lower=1> N_gaps_trend;",
      block = "data"
    ),
    brms::stanvar(
      x = as.array(gaps$gap_values),
      name = "gap_trend",
      scode = "vector<lower=0>[N_gaps_trend] gap_trend;",
      block = "data"
    ),
    brms::stanvar(
      x = as.array(as.integer(gaps$gap_index)),
      name = "gap_index_trend",
      scode = paste0("array[N_time_trend] int<lower=1, upper=N_gaps_trend>",
                     " gap_index_trend;"),
      block = "data"
    ),
    brms::stanvar(
      name = "car_parameters",
      scode = paste0(
        "vector<lower=0>[N_lv_trend] sigma_trend;\n",
        "matrix[N_time_trend, N_lv_trend] innovations_trend;\n",
        "// CAR AR1 damping\n",
        "vector<lower=0.001,upper=0.999>[N_lv_trend] ar1_trend;"
      ),
      block = "parameters"
    ),
    if (has_cor) {
      car_correlated_dynamics_stanvars()
    } else {
      car_independent_dynamics_stanvars()
    },
    generate_trend_priors_stanvar(
      param_names = c("sigma_trend", "ar1_trend",
                      if (has_cor) "L_Omega_trend"),
      prior = prior,
      stanvar_name = "car_model",
      trend_obj = trend_specs
    ),
    # Standard innovations, normal or Student-t by `df`
    brms::stanvar(
      name = "car_innovations_sampling",
      scode = paste(
        innovation_sampling_code("N_lv_trend", df), collapse = "\n  "
      ),
      block = "model"
    ),
    nu_trend_stanvars(df, prior),
    generate_trend_computation_tparameters(
      data_info, identity_z = z_is_identity(FALSE, trend_specs$fixed_Z)
    )
  )
}

#' Stanvars of a ZMVN trend
#'
#' The states are the scaled innovations themselves, with no
#' dependence over time. `assemble_shared_innovation_trend()` adds the
#' rest.
#'
#' @param trend_specs Trend specification from `ZMVN()`
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_zmvn_trend_stanvars <- function(trend_specs, data_info,
                                         prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  dynamics <- brms::stanvar(
    name = "zmvn_tparameters",
    scode = "lv_trend = scaled_innovations_trend;",
    block = "tparameters"
  )
  assemble_shared_innovation_trend(
    list(dynamics), trend_specs, data_info, prior
  )
}

#' The capacity matrix of a logistic PW trend
#'
#' The `cap` column of the training data, as the
#' `[n_time, n_series]` matrix that Stan takes as `cap_trend`. The cap
#' is on the response scale. `transform_pw_cap_to_link()` puts it on
#' the link scale, where the kernel evaluates `cap * inv_logit(...)`.
#'
#' @param data Training data frame
#' @param cap_var Name of the capacity column
#' @param row_time,row_series Time and series index of each row, from
#'   the resolved axes. `row_series` is NULL for a response-keyed
#'   frame, whose rows give every series at their time.
#' @param n_time,n_series Dimensions of the trend grid
#' @param family Family whose link the capacity takes
#' @return Numeric matrix on the link scale
#' @noRd
build_pw_cap_matrix <- function(data, cap_var, row_time, row_series,
                                n_time, n_series, family) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(cap_var, min.chars = 1L)
  checkmate::assert_integer(row_time, len = nrow(data), lower = 1L,
                            any.missing = FALSE)
  checkmate::assert_integer(row_series, len = nrow(data), lower = 1L,
                            any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_int(n_time, lower = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_function(family$linkfun, .var.name = "family$linkfun")

  if (!(cap_var %in% names(data))) {
    stop(insight::format_error(c(
      paste0("A logistic 'PW()' trend requires a '", cap_var,
             "' column in the data."),
      i = paste0("Supply '", cap_var, "' or use growth = 'linear'.")
    )), call. = FALSE)
  }
  cap_vals <- data[[cap_var]]
  if (!is.numeric(cap_vals) || any(!is.finite(cap_vals)) ||
      any(cap_vals <= 0)) {
    stop(insight::format_error(c(
      paste0("Column '", cap_var, "' must hold positive, finite ",
             "numbers."),
      i = "The capacity is the ceiling of the response scale."
    )), call. = FALSE)
  }

  out <- matrix(NA_real_, nrow = n_time, ncol = n_series)
  if (is.null(row_series)) {
    out[row_time, ] <- cap_vals
  } else {
    out[cbind(row_time, row_series)] <- cap_vals
  }
  if (anyNA(out)) {
    stop(insight::format_error(c(
      paste0("A logistic 'PW()' trend needs '", cap_var,
             "' for every series at every time."),
      i = paste0("Add the missing '", cap_var, "' values to the data.")
    )), call. = FALSE)
  }
  transform_pw_cap_to_link(out, family, cap_var)
}

#' Stanvars of a PW trend
#'
#' Each latent series follows a Prophet piecewise trend: a base growth
#' rate `k_trend` that each changepoint shifts by a column of
#' `delta_trend`. The logistic form also places its inflection at
#' `m_trend` and saturates at the `cap` capacity. Time runs in steps
#' `1, ..., N`. The parameters are sampled in units of their default
#' prior scales from `pw_prior_scales()`.
#'
#' @param trend_specs Trend specification from `PW()`
#' @param data_info Data information list
#' @param prior A `brmsprior` or NULL
#' @return A stanvars object
#' @noRd
generate_pw_trend_stanvars <- function(trend_specs, data_info,
                                       prior = NULL) {
  checkmate::assert_list(trend_specs, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)

  n_changepoints <- trend_specs$n_changepoints
  logistic <- identical(pw_growth(trend_specs), "logistic")
  n_series <- data_info_n_series(data_info)
  checkmate::assert_count(n_changepoints, positive = TRUE)
  changepoint_range <- trend_specs$changepoint_range
  checkmate::assert_number(changepoint_range, lower = 0, upper = 1)

  # Each series takes its own changepoints. Z is the identity.
  matrix_z <- generate_matrix_z_multiblock_stanvars(
    is_factor_model = FALSE,
    fixed_Z = trend_specs$fixed_Z,
    prior = prior,
    trend_obj = trend_specs
  )

  # The piecewise functions of Prophet (Taylor & Letham 2018). Each
  # growth form emits the one trend function its transformed
  # parameters call.
  pw_changepoint_scode <- "
      /**
       * Indicator of the changepoints each time has passed
       * @param t Times, ascending
       * @param t_change_trend Changepoint times, ascending
       * @param T Number of times
       * @param S Number of changepoints
       * @return Matrix (T x S) with entry 1 where t[i] >= t_change[j]
       */
      matrix get_changepoint_matrix(vector t, vector t_change_trend,
                                    int T, int S) {
        matrix[T, S] Kappa;
        row_vector[S] a_row = rep_row_vector(0, S);
        int cp_idx = 1;
        for (i in 1:T) {
          while ((cp_idx <= S) && (t[i] >= t_change_trend[cp_idx])) {
            a_row[cp_idx] = 1;
            cp_idx = cp_idx + 1;
          }
          Kappa[i] = a_row;
        }
        return Kappa;
      }
  "
  pw_growth_scode <- if (logistic) {
    "
      /**
       * Offsets that keep a logistic trend continuous at each
       * changepoint
       * @return Vector of S offset adjustments
       */
      vector logistic_gamma(real k, real m, vector delta,
                            vector t_change_trend, int S) {
        vector[S] gamma;
        // Growth rate in each segment
        vector[S + 1] k_s = append_row(k, k + cumulative_sum(delta));
        // Offset in the previous segment
        real m_pr = m;
        for (i in 1:S) {
          gamma[i] = (t_change_trend[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
          m_pr = m_pr + gamma[i];
        }
        return gamma;
      }

      /**
       * Logistic trend with changepoints, saturating at the capacity
       * @return Vector of the trend at each time
       */
      vector logistic_trend(real k, real m, vector delta, vector t,
                            vector cap_trend, matrix Kappa_trend,
                            vector t_change_trend, int S) {
        vector[S] gamma = logistic_gamma(k, m, delta, t_change_trend, S);
        return cap_trend .* inv_logit((k + Kappa_trend * delta)
                                      .* (t - (m + Kappa_trend * gamma)));
      }
    "
  } else {
    "
      /**
       * Linear trend with changepoints. Prophet's offset m is left
       * out: the observation formula supplies the level.
       * @return Vector of the trend at each time
       */
      vector linear_trend(real k, vector delta, vector t,
                          matrix Kappa_trend, vector t_change_trend) {
        return (k + Kappa_trend * delta) .* t
               + Kappa_trend * (-t_change_trend .* delta);
      }
    "
  }
  pw_functions_stanvar <- brms::stanvar(
    name = "pw_functions",
    scode = paste0(pw_changepoint_scode, pw_growth_scode),
    block = "functions"
  )

  # The changepoints are spaced evenly over the first
  # `changepoint_range` of the history, as in Prophet (Taylor & Letham
  # 2018). The history is the occasions carrying a response. The trend
  # grid reaches past the last response when the frame holds
  # unobserved rows. A changepoint there would have nothing to fit.
  n_time_trend <- data_info$n_time
  checkmate::assert_int(n_time_trend, lower = 1L,
                        .var.name = "data_info$n_time")
  history_size <- data_info$n_time_observed %||% n_time_trend
  if (history_size < 2L) {
    stop(insight::format_error(c(
      "A 'PW()' trend needs at least two observed time points.",
      x = paste0("Observed time points: ", history_size, ".")
    )), call. = FALSE)
  }
  hist_size <- max(1L, floor(history_size * changepoint_range))
  t_change_values <- unique(round(
    seq.int(1, hist_size, length.out = n_changepoints + 1L)[-1L]
  ))
  scales <- pw_prior_scales(n_time_trend, trend_specs$changepoint_scale,
                            pw_growth_spread(trend_specs))

  data_stanvars <- combine_stanvars(
    brms::stanvar(
      x = length(t_change_values), name = "N_change_trend",
      scode = "int<lower=0> N_change_trend;", block = "data"
    ),
    brms::stanvar(
      x = as.numeric(t_change_values), name = "t_change_trend",
      scode = "vector[N_change_trend] t_change_trend;", block = "data"
    ),
    brms::stanvar(
      x = scales$rate, name = "rate_scale_trend",
      scode = "real<lower=0> rate_scale_trend;", block = "data"
    ),
    brms::stanvar(
      x = scales$changepoint, name = "delta_scale_trend",
      scode = "real<lower=0> delta_scale_trend;", block = "data"
    ),
    if (logistic) {
      brms::stanvar(
        x = build_pw_cap_matrix(
          data = data_info$data,
          cap_var = pw_cap_var(trend_specs),
          row_time = data_info$row_time,
          row_series = data_info$row_series,
          n_time = n_time_trend,
          n_series = n_series,
          family = pw_cap_link_family(data_info$families)
        ),
        name = "cap_trend",
        scode = "matrix[N_time_trend, N_lv_trend] cap_trend;",
        block = "data"
      )
    }
  )

  pw_transformed_data_stanvar <- brms::stanvar(
    name = "pw_transformed_data",
    scode = paste0(
      "// Time in steps, and the changepoints each step has passed\n",
      "vector[N_time_trend] time_trend = linspaced_vector(N_time_trend, 1, ",
      "N_time_trend);\n",
      "matrix[N_time_trend, N_change_trend] Kappa_trend = ",
      "get_changepoint_matrix(time_trend, t_change_trend, N_time_trend, ",
      "N_change_trend);"
    ),
    block = "tdata"
  )

  # Each parameter is sampled in units of its default prior scale. A
  # random start in (-2, 2) then stays within a few prior scales of
  # zero, where a growth rate of one per step would put the predictor
  # far outside the data.
  pw_parameters_stanvar <- brms::stanvar(
    name = "pw_parameters",
    scode = paste0(
      "// Base growth rates, per step\n",
      "vector<multiplier=rate_scale_trend>[N_lv_trend] k_trend;\n",
      if (logistic) {
        paste0(
          "// Time of each logistic inflection\n",
          "vector<offset=0.5 * (N_time_trend + 1), ",
          "multiplier=0.5 * N_time_trend>[N_lv_trend] m_trend;\n"
        )
      },
      "// Growth rate changes at each changepoint\n",
      "matrix<multiplier=delta_scale_trend>[N_change_trend, N_lv_trend] ",
      "delta_trend;"
    ),
    block = "parameters"
  )

  pw_transformed_parameters_stanvar <- brms::stanvar(
    name = "pw_transformed_parameters",
    scode = if (logistic) {
      paste0(
        "// Each latent series follows its own logistic piecewise trend\n",
        "for (s in 1 : N_lv_trend) {\n",
        "  lv_trend[1 : N_time_trend, s] = logistic_trend(k_trend[s], ",
        "m_trend[s],\n",
        "    to_vector(delta_trend[ : , s]), time_trend,\n",
        "    to_vector(cap_trend[ : , s]), Kappa_trend, t_change_trend,\n",
        "    N_change_trend);\n",
        "}"
      )
    } else {
      paste0(
        "// Each latent series follows its own linear piecewise trend\n",
        "for (s in 1 : N_lv_trend) {\n",
        "  lv_trend[1 : N_time_trend, s] = linear_trend(k_trend[s],\n",
        "    to_vector(delta_trend[ : , s]), time_trend, Kappa_trend,\n",
        "    t_change_trend);\n",
        "}"
      )
    },
    block = "tparameters"
  )

  trend_computation <- generate_trend_computation_tparameters(
    data_info, identity_z = z_is_identity(FALSE, trend_specs$fixed_Z)
  )

  # One statement per parameter, under the user's prior where they
  # gave one and the resolver's default otherwise. Stan takes the
  # statement for the matrix `delta_trend` on `to_vector()`.
  pw_prior_line <- function(param, vectorise = FALSE) {
    chosen <- get_trend_parameter_prior(prior, param,
                                        trend_obj = trend_specs)
    lhs <- if (vectorise) paste0("to_vector(", param, ")") else param
    paste0(lhs, " ~ ", chosen, ";")
  }
  pw_model_stanvar <- brms::stanvar(
    name = "pw_model",
    scode = paste(c(
      "// PW trend priors",
      if (logistic) pw_prior_line("m_trend"),
      pw_prior_line("k_trend"),
      pw_prior_line("delta_trend", vectorise = TRUE)
    ), collapse = "\n"),
    block = "model"
  )

  combine_stanvars(
    matrix_z,
    pw_functions_stanvar,
    data_stanvars,
    pw_transformed_data_stanvar,
    pw_parameters_stanvar,
    pw_transformed_parameters_stanvar,
    trend_computation,
    pw_model_stanvar
  )
}

#' The trend formula's brms program as stanvars with `_trend` names
#'
#' brms writes a program for the trend formula alone, a univariate
#' gaussian model. Its declarations, priors and linear predictor become
#' stanvars, every name taking the suffix that keeps them apart from
#' the observation model's.
#' `times_trend` then indexes the trend predictor by time and series.
#'
#' @param trend_setup Trend setup from `setup_brms_lightweight()`,
#'   holding `stancode`, `standata` and `brmsfit`
#' @param dimensions Resolved axes from
#'   `extract_time_series_dimensions()`
#' @param suffix Suffix appended to every renamed identifier
#' @return A stanvars object
#' @noRd
extract_and_rename_trend_parameters <- function(trend_setup, dimensions,
                                                suffix = "_trend") {
  checkmate::assert_list(trend_setup, names = "named")
  checkmate::assert_subset(c("stancode", "standata", "brmsfit"),
                           names(trend_setup))
  checkmate::assert_list(dimensions, names = "named")
  checkmate::assert_string(suffix, pattern = "^_[a-zA-Z][a-zA-Z0-9_]*$")

  stan_code_result <- extract_and_rename_stan_blocks(
    stancode = trend_setup$stancode,
    suffix = suffix,
    mapping = list(original_to_renamed = list(),
                   renamed_to_original = list())
  )
  standata_stanvars <- extract_and_rename_standata_objects(
    standata = trend_setup$standata,
    suffix = suffix,
    mapping = stan_code_result$mapping
  )

  # brms's `N` is the row count of the trend data. With `by = lv_axis()`
  # the second axis of `times_trend` runs over the factors.
  times_trend_stanvars <- create_times_trend_matrix(
    n_time = dimensions$n_time,
    n_series = dimensions$n_series,
    n_trend_rows = trend_setup$standata$N %||%
      (dimensions$n_time * dimensions$n_series),
    has_by_lv = isTRUE(dimensions$has_by_lv),
    n_lv = dimensions$n_lv_for_grain
  )

  combine_stanvars(stan_code_result$stanvars, standata_stanvars,
                   times_trend_stanvars)
}

#' The blocks of the trend's brms program, renamed
#'
#' Each block of the trend program becomes a stanvar with its
#' identifiers suffixed. The model block keeps its priors and drops the
#' likelihood. The linear predictor becomes `mu_trend` in transformed
#' parameters, rebuilt from the statements brms wrote for `mu` or, with
#' none, from the intercept and design matrix the program declares.
#' The renamed data block goes into the mapping as `data_declarations`,
#' for `extract_and_rename_standata_objects()`.
#'
#' @param stancode The trend program
#' @param suffix Suffix appended to every renamed identifier
#' @param mapping List of `original_to_renamed` and
#'   `renamed_to_original`, which this function extends
#' @return List of `stanvars` and the extended `mapping`
#' @noRd
extract_and_rename_stan_blocks <- function(stancode, suffix, mapping) {
  stancode <- as.character(stancode)
  checkmate::assert_string(stancode, min.chars = 1L)
  checkmate::assert_string(suffix, min.chars = 1L)
  checkmate::assert_list(mapping, names = "named")

  has_content <- function(x) !is.null(x) && nzchar(trimws(x))
  rename <- function(code) {
    result <- rename_parameters_in_block(code, suffix, mapping)
    mapping <<- result$mapping
    result$code
  }
  filtered_block <- function(code, name, type) {
    content <- extract_stan_block_content(code, name)
    if (has_content(content)) filter_block_content(content, type)
  }
  block_stanvar <- function(scode, block) {
    brms::stanvar(scode = scode, block = block)
  }
  stanvar_list <- list()

  # Functions are global and keep their names. The names a brms
  # program defines, such as `scale_r_cor`, are kept from renaming.
  functions_block <- extract_stan_block_content(stancode, "functions")
  if (has_content(functions_block)) {
    functions_list <- parse_stan_functions(functions_block)
    if (length(functions_list) > 0L) {
      mapping$custom_functions <- vapply(functions_list,
                                         function(f) f$name, character(1L))
    }
    stanvar_list$trend_functions <- block_stanvar(functions_block,
                                                  "functions")
  }

  data_block <- extract_stan_block_content(stancode, "data")
  if (has_content(data_block)) {
    mapping$data_declarations <- rename(data_block)
  }

  tdata_block <- extract_stan_block_content(stancode, "transformed data")
  if (has_content(tdata_block)) {
    stanvar_list$trend_transformed_data <- block_stanvar(
      rename(tdata_block), "tdata"
    )
  }

  params_block <- filtered_block(stancode, "parameters", "parameters")
  if (has_content(params_block)) {
    stanvar_list$trend_parameters <- block_stanvar(rename(params_block),
                                                   "parameters")
  }

  # The stanvar for these waits until `mu_trend` has taken the
  # statements it needs
  tparams_block <- filtered_block(stancode, "transformed parameters",
                                  "transformed_parameters")
  trend_tparams_code <- if (has_content(tparams_block)) {
    rename(tparams_block)
  }

  mu_name <- paste0("mu", suffix)
  extracted_mu_lines <- character(0)
  mu_result <- extract_mu_construction(stancode)
  if (length(mu_result$mu_construction) > 0L) {
    # The statements and the declarations they use leave the model
    # block together
    extracted_mu_lines <- c(mu_result$mu_construction,
                            mu_result$supporting_declarations)
    mapping <- map_mu_references(stancode, mu_result$referenced_variables,
                                 mapping, suffix)
    mu_code <- paste(reconstruct_mu_trend_with_renamed_vars(
      mu_construction = mu_result$mu_construction,
      supporting_declarations = mu_result$supporting_declarations,
      variable_mapping = mapping$original_to_renamed,
      suffix = suffix
    ), collapse = "\n")
  } else {
    mu_code <- mu_from_declarations(stancode, suffix, paste0("N", suffix))
  }
  # A trend formula without terms leaves `mu_trend` at zero. The
  # program then declares no `mu_trend`. The trend is the states alone.
  if (!mu_is_zero(mu_code, mu_name)) {
    stanvar_list$trend_model_mu_creation <- block_stanvar(mu_code,
                                                          "tparameters")
  }
  mapping$original_to_renamed$mu <- mu_name
  mapping$renamed_to_original[[mu_name]] <- "mu"

  # brms declares a group-level effect and a smooth coefficient here and
  # assigns each below its declaration. `mu_trend` takes both. A
  # statement written in two places is computed twice per iteration.
  if (!is.null(trend_tparams_code)) {
    mu_creation <- stanvar_list$trend_model_mu_creation
    if (!is.null(mu_creation)) {
      trend_tparams_code <- drop_repeated_statements(
        trend_tparams_code, mu_creation[[1L]]$scode
      )
    }
    if (has_content(trend_tparams_code)) {
      stanvar_list$trend_tparameters <- block_stanvar(trend_tparams_code,
                                                      "tparameters")
    }
  }

  # The priors of the model block, less the likelihood and the
  # statements `mu_trend` took
  model_block <- extract_stan_block_content(stancode, "model")
  if (has_content(model_block)) {
    priors <- extract_non_likelihood_from_model_block(model_block,
                                                      extracted_mu_lines)
    if (has_content(priors)) {
      stanvar_list$trend_model_priors <- block_stanvar(rename(priors),
                                                       "model")
    }
  }

  genquant_block <- extract_stan_block_content(stancode,
                                               "generated quantities")
  if (has_content(genquant_block)) {
    stanvar_list$trend_generated_quantities <- block_stanvar(
      rename(genquant_block), "genquant"
    )
  }

  list(stanvars = combine_stanvars(stanvar_list), mapping = mapping)
}

#' Map the identifiers the trend's mu statements use
#'
#' The statements brms wrote for `mu` can name a variable no renamed
#' block declared, such as a model-block local. Each one the program
#' declares or computes takes the suffix.
#'
#' @param stancode The trend program
#' @param referenced Identifiers the mu statements use
#' @param mapping The renaming map
#' @param suffix Suffix appended to every renamed identifier
#' @return `mapping`, extended
#' @noRd
map_mu_references <- function(stancode, referenced, mapping, suffix) {
  checkmate::assert_character(referenced, any.missing = FALSE)
  checkmate::assert_list(mapping, names = "named")
  checkmate::assert_string(suffix, min.chars = 1L)
  unmapped <- setdiff(referenced, names(mapping$original_to_renamed))
  if (length(unmapped) == 0L) {
    return(mapping)
  }
  computed <- extract_computed_variables(stancode)
  declared <- find_variable_declarations(stancode, unmapped)
  found <- vapply(unmapped, function(v) {
    v %in% computed || any(grepl(paste0("\\b", v, "\\b"), declared))
  }, logical(1L))
  if (!all(found)) {
    stop_mvgam_fault(
      "The trend's mu statements use undeclared variables.",
      paste0("Missing: ", paste(unmapped[!found], collapse = ", "), ".")
    )
  }
  for (v in unmapped) {
    mapping$original_to_renamed[[v]] <- paste0(v, suffix)
    mapping$renamed_to_original[[paste0(v, suffix)]] <- v
  }
  mapping
}

#' The trend's mu statement when brms wrote none
#'
#' A trend program whose linear predictor brms folds into a GLM call
#' writes no statement for `mu`. The intercept and the design matrix it
#' declares give `mu_trend` directly.
#'
#' @param stancode The trend program
#' @param suffix Suffix appended to every renamed identifier
#' @param time_param Name of the length of `mu_trend`
#' @return Character scalar of Stan statements
#' @noRd
mu_from_declarations <- function(stancode, suffix, time_param) {
  checkmate::assert_string(stancode)
  checkmate::assert_string(suffix, min.chars = 1L)
  checkmate::assert_string(time_param, min.chars = 1L)
  mu_decl <- paste0("vector[", time_param, "] mu", suffix)
  intercept <- if (grepl("real.*Intercept[^_]", stancode)) {
    paste0("Intercept", suffix)
  }
  design <- if (grepl("matrix\\[.*\\]\\s+Xc[^_]", stancode)) {
    "Xc"
  } else if (grepl("matrix\\[.*\\]\\s+X[^_]", stancode)) {
    "X"
  }
  if (!is.null(design) && grepl("vector\\[.*\\]\\s+b[^_]", stancode)) {
    return(paste0(
      mu_decl, " = rep_vector(0.0, ", time_param, ");\n  mu", suffix,
      " += ", if (!is.null(intercept)) paste0(intercept, " + "),
      design, suffix, " * b", suffix, ";"
    ))
  }
  paste0(mu_decl, " = rep_vector(", intercept %||% "0.0", ", ",
         time_param, ");")
}

#' Do the statements building a predictor leave it at zero?
#'
#' A trend formula with no terms gives `mu_trend` no design. Its
#' statement declares it as `rep_vector(0.0, N_trend)` and never
#' assigns to it again.
#'
#' @param mu_code The Stan statements that build the predictor
#' @param mu_name The predictor's name, such as `"mu_trend"`
#' @return `TRUE` when the statements declare the predictor at zero
#'   and never assign to it afterwards
#' @noRd
mu_is_zero <- function(mu_code, mu_name) {
  checkmate::assert_string(mu_code)
  checkmate::assert_string(mu_name, min.chars = 1L)
  code <- stan_line_code(strsplit(mu_code, "\n", fixed = TRUE)[[1L]])
  zero_start <- grepl(paste0(
    "\\b", mu_name, "\\s*=\\s*rep_vector\\(\\s*0(\\.0*)?\\s*,"
  ), code)
  assigned <- grepl(
    paste0("\\b", mu_name, "\\b(\\s*\\[[^]]*\\])?\\s*[-+*/]?=(?!=)"),
    code, perl = TRUE
  )
  sum(zero_start) == 1L && sum(assigned) == 1L &&
    identical(which(zero_start), which(assigned))
}

#' Does a supporting declaration compute a value?
#'
#' A declaration without a value belongs to the data or parameters
#' block. A constant, such as `rep_vector(0.0, N)` or `1.5`, needs no
#' place in the order of dependence. Every other assignment is a
#' computation that `mu_trend` waits for.
#'
#' @param declaration One Stan declaration
#' @return Logical scalar
#' @noRd
should_include_in_transformed_parameters <- function(declaration) {
  checkmate::assert_string(declaration, min.chars = 1L)
  if (!grepl("=", declaration)) {
    return(FALSE)
  }
  constants <- c("=\\s*rep_(vector|matrix)\\s*\\(\\s*[0-9]",
                 "=\\s*[0-9]+\\.?[0-9]*\\s*;\\s*$")
  !any(vapply(constants, grepl, logical(1L), x = declaration, perl = TRUE))
}

#' The variables on the right of an assignment
#'
#' @param declaration One Stan statement
#' @return Identifiers on the right of its first `=`, less Stan's
#'   reserved words and the functions it calls
#' @noRd
extract_dependencies_from_declaration <- function(declaration) {
  checkmate::assert_string(declaration)
  rhs <- regmatches(declaration,
                    regexpr("=\\s*([^;]+)", declaration, perl = TRUE))
  if (length(rhs) == 0L) {
    return(character(0))
  }
  rhs <- sub("^=\\s*", "", rhs)
  identifiers <- extract_stan_identifiers(rhs)
  identifiers[!identifiers %in% c(get_stan_reserved_words(),
                                  stan_function_calls(rhs))]
}

#' Declarations of variables and of everything they depend on
#'
#' A depth-first search over the program: each variable's declarations
#' and assignments, preceded by those of the variables on their right.
#' A GP prediction `gp_pred_1` then follows `rgp_1`, its input. brms
#' writes a smooth coefficient as a bare declaration and a later
#' assignment. The search returns both.
#'
#' @param stancode A Stan program
#' @param referenced_vars Identifiers to find
#' @param search_blocks Blocks to search
#' @return Character vector of statements in dependency order
#' @noRd
find_variable_declarations <- function(stancode, referenced_vars,
                                       search_blocks = c(
                                         "data", "transformed data",
                                         "parameters",
                                         "transformed parameters", "model"
                                       )) {
  checkmate::assert_character(referenced_vars, any.missing = FALSE,
                              null.ok = TRUE)
  if (length(referenced_vars) == 0L) {
    return(character(0))
  }
  checkmate::assert_string(stancode)
  checkmate::assert_character(search_blocks, min.len = 1L)

  lines <- unlist(lapply(search_blocks, function(block) {
    content <- extract_stan_block_content(stancode, block)
    if (is.null(content)) {
      return(character(0))
    }
    out <- trimws(strsplit(content, "\n", fixed = TRUE)[[1L]])
    out[nzchar(out)]
  }))

  searched <- character(0)
  found <- character(0)
  visit <- function(var) {
    if (var %in% searched) {
      return(invisible(NULL))
    }
    searched <<- c(searched, var)
    pattern <- paste0(
      "^\\s*((real|int|vector|matrix|array).*?\\b", var, "\\b.*?[;=]|",
      var, "\\s*=\\s*[^;]+;)"
    )
    matches <- grep(pattern, lines, value = TRUE, perl = TRUE)
    if (length(matches) == 0L) {
      return(invisible(NULL))
    }
    for (dependency in unique(unlist(lapply(
      matches, extract_dependencies_from_declaration
    )))) {
      visit(dependency)
    }
    found <<- c(found, matches)
  }
  for (var in referenced_vars) {
    visit(var)
  }
  unique(found)
}

#' The trend's mu statements, renamed into `mu_trend`
#'
#' `mu` becomes `mu_trend` and every mapped identifier takes its
#' suffix. A statement indexed by `n` goes into a loop over the trend
#' rows. The order brms wrote is a valid order of dependence. The
#' statements keep it: a declaration of a computed variable comes
#' first, then the supporting declarations, then the statements on
#' `mu_trend`.
#'
#' A supporting declaration enters when it computes a value. A bare
#' declaration enters with the assignment brms split from it, as for
#' a smooth coefficient (`vector[k] s_1_1;` then
#' `s_1_1 = sds_1[1] * zs_1_1;`).
#'
#' @param mu_construction Statements brms wrote for `mu`
#' @param supporting_declarations Declarations those statements use
#' @param variable_mapping Named list from original to renamed names
#' @param suffix Suffix appended to every renamed identifier
#' @return Character vector of Stan statements
#' @noRd
reconstruct_mu_trend_with_renamed_vars <- function(mu_construction,
                                                   supporting_declarations,
                                                   variable_mapping,
                                                   suffix = "_trend") {
  checkmate::assert_character(mu_construction)
  checkmate::assert_character(supporting_declarations)
  checkmate::assert_list(variable_mapping, types = "character",
                         names = "named", min.len = 1L)
  checkmate::assert_string(suffix, pattern = "^_[A-Za-z][A-Za-z0-9_]*$")
  mu_construction <- mu_construction[nzchar(trimws(mu_construction))]
  if (length(mu_construction) == 0L) {
    return(character(0))
  }
  mu_name <- paste0("mu", suffix)
  time_param <- paste0("N", suffix)
  rename_all <- function(code, map) {
    for (from in names(map)[order(-nchar(names(map)))]) {
      code <- gsub(paste0("\\b", from, "\\b"), map[[from]], code)
    }
    code
  }

  # Each computed supporting variable takes the suffix too
  supporting <- supporting_declarations[nzchar(trimws(
    supporting_declarations
  ))]
  local_mapping <- variable_mapping
  renamed_supporting <- character(0)
  for (decl in supporting) {
    include <- should_include_in_transformed_parameters(decl)
    if (!include && !grepl("=", decl) &&
        grepl("^\\s*(vector|matrix|real|int|array)", decl)) {
      declared <- regmatches(
        decl, regexec("([a-zA-Z_][a-zA-Z0-9_]*)\\s*;", decl)
      )[[1L]]
      include <- length(declared) > 1L && any(grepl(
        paste0("^\\s*", declared[2L], "\\s*="), supporting
      ))
    }
    if (!include) {
      next
    }
    assigned <- regmatches(decl, regexec(paste0(
      "\\b(?:vector|matrix|real|int|array)\\s*(?:\\[[^\\]]*\\])*\\s+",
      "([a-zA-Z_][a-zA-Z0-9_]*)\\s*="
    ), decl, perl = TRUE))[[1L]]
    if (length(assigned) > 1L) {
      var <- assigned[2L]
      if (!var %in% names(local_mapping) &&
          !endsWith(var, suffix)) {
        local_mapping[[var]] <- paste0(var, suffix)
      }
    }
    renamed_supporting <- c(renamed_supporting,
                            rename_all(decl, local_mapping))
  }

  indexed <- grepl("\\bmu\\[n\\]", mu_construction)
  loop_statements <- rename_all(
    gsub("\\bmu\\[n\\]", paste0(mu_name, "[n]"), mu_construction[indexed]),
    variable_mapping
  )
  statements <- rename_all(
    gsub("\\bmu\\b", mu_name, mu_construction[!indexed]),
    variable_mapping
  )

  # A declaration with a value, `mu_trend`'s own among them
  computed <- grepl(paste0(
    "^\\s*(vector|matrix|array)\\[.*\\]\\s+[a-zA-Z_][a-zA-Z0-9_]*",
    suffix, "\\s*="
  ), statements)
  initialised <- any(grepl("vector\\[.*\\]\\s+mu\\s*=\\s*rep_vector",
                           mu_construction))
  c(
    if (!initialised) {
      paste0("vector[", time_param, "] ", mu_name, " = rep_vector(0.0, ",
             time_param, ");")
    },
    statements[computed],
    renamed_supporting,
    statements[!computed],
    if (length(loop_statements) > 0L) {
      c("", paste0("for (n in 1:", time_param, ") {"),
        paste0("  ", loop_statements), "}")
    }
  )
}


#' The lines of a trend block the combined program keeps
#'
#' The trend program is a gaussian brms model on a placeholder
#' response. Its likelihood, the `sigma` of that likelihood and the
#' `lprior` bookkeeping belong to the observation program. Every block
#' drops comments, blank lines and the `prior_only` guard, whose
#' contents stay.
#'
#' @param block_content Body of one block of the trend program
#' @param block_type "model", "transformed_parameters" or "parameters"
#' @return The kept lines as one string, or NULL when none remain
#' @noRd
filter_block_content <- function(block_content, block_type = "model") {
  checkmate::assert_string(block_content, min.chars = 1L)
  checkmate::assert_choice(block_type, c("model", "transformed_parameters",
                                         "parameters"))
  lines <- trimws(strsplit(block_content, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "//")]

  # The prior on `sigma` carries its truncation term on the next line
  sigma_prior <- grepl(paste0(
    "lprior\\s*\\+=\\s*student_t_", stan_density_call_rx,
    "\\s*\\(\\s*sigma\\s*\\|"
  ), lines)
  sigma_truncation <- c(FALSE, utils::head(sigma_prior, -1L)) &
    grepl("^-\\s*1\\s*\\*\\s*student_t_lccdf\\s*\\(\\s*0\\s*\\|", lines)
  drop <- seq_along(lines) %in% prior_only_guard_indices(lines) |
    grepl("^real\\s+lprior\\s*=\\s*0\\s*;", lines) |
    sigma_prior | sigma_truncation
  drop <- drop | if (identical(block_type, "model")) {
    grepl("^target\\s*\\+=\\s*[A-Za-z_]+\\s*\\(\\s*Y\\b", lines) |
      grepl("^target\\s*\\+=\\s*lprior\\s*;", lines)
  } else {
    grepl("^real\\s*(<[^>]*>)?\\s+sigma\\s*;", lines)
  }
  kept <- lines[!drop]
  if (length(kept) == 0L) {
    return(NULL)
  }
  paste(kept, collapse = "\n")
}

#' The priors of the trend program's model block
#'
#' @param model_block Body of the trend program's model block
#' @param exclude_mu_lines Statements `mu_trend` took, which leave the
#'   block. A loop left governing nothing goes with them.
#' @return The kept lines as one string, or NULL when none remain
#' @noRd
extract_non_likelihood_from_model_block <- function(
    model_block, exclude_mu_lines = character(0)) {
  checkmate::assert_string(model_block)
  checkmate::assert_character(exclude_mu_lines)
  if (length(exclude_mu_lines) > 0L) {
    model_block <- drop_repeated_statements(
      model_block, paste(exclude_mu_lines, collapse = "\n")
    )
  }
  if (!nzchar(trimws(model_block))) {
    return(NULL)
  }
  filter_block_content(model_block, "model")
}

#' The body of a named Stan block
#'
#' The lines strictly between the block's header and its closing brace.
#'
#' @param stancode Character string containing full Stan code.
#' @param block_name One of `stan_block_names`, in any case and with
#'   any internal spacing.
#' @return Character string of the block's inner lines: `""` for a
#'   block whose braces enclose nothing, NULL for an absent block.
#' @details
#' The functions block goes through `extract_stan_functions_block()`,
#' which handles the nested braces of a function definition. Every
#' other block takes its bounds from `stan_block_bounds()`.
#' @noRd
extract_stan_block_content <- function(stancode, block_name) {
  checkmate::assert_string(stancode, min.chars = 1)
  checkmate::assert_string(block_name, min.chars = 1)

  if (!grepl("\\{", stancode)) {
    stop_mvgam_fault(
      "The Stan code to search lacks any block.",
      paste0("mvgam searched it for the '", block_name, "' block.")
    )
  }

  if (block_name == "functions") {
    # One extractor for this block.
    result <- extract_stan_functions_block(stancode)
    if (is.null(result)) {
      return(NULL)  # Block not found - consistent with other blocks
    }
    if (nchar(trimws(result)) == 0) {
      return("")  # Empty functions block
    }
    return(result)

  } else {
    # The block spans the brace its header opened. A blank line or a
    # comment after that brace leaves it outside the body.
    lines <- strsplit(stancode, "\n")[[1]]
    block <- tolower(gsub("\\s+", " ", trimws(block_name)))
    bounds <- stan_block_bounds(lines, block)
    if (is.null(bounds)) {
      return(NULL)  # Block not found
    }
    # A block whose braces meet on one line has an empty body. An
    # absent block gives NULL.
    body <- if (bounds$end <= bounds$start + 1L) {
      character(0)
    } else {
      lines[seq.int(bounds$start + 1L, bounds$end - 1L)]
    }
    return(paste(body, collapse = "\n"))
  }
}

#' Suffix every renameable identifier in a block of Stan code
#'
#' Longer names are replaced first. A name that contains a shorter
#' one then keeps its own suffix.
#'
#' @param block_code Stan code of one block
#' @param suffix Suffix appended to every renamed identifier
#' @param mapping The renaming map. Its `custom_functions` keep their
#'   names.
#' @return List of the renamed `code` and the extended `mapping`
#' @noRd
rename_parameters_in_block <- function(block_code, suffix, mapping) {
  checkmate::assert_string(block_code)
  checkmate::assert_string(suffix, min.chars = 1L)
  checkmate::assert_list(mapping)

  renameable <- filter_renameable_identifiers(
    extract_stan_identifiers(block_code),
    mapping = mapping,
    exclude = stan_function_calls(block_code)
  )
  for (identifier in renameable[order(-nchar(renameable))]) {
    renamed <- paste0(identifier, suffix)
    mapping$original_to_renamed[[identifier]] <- renamed
    mapping$renamed_to_original[[renamed]] <- identifier
    block_code <- gsub(paste0("\\b", identifier, "\\b"), renamed,
                       block_code, perl = TRUE)
  }
  list(code = block_code, mapping = mapping)
}

#' Stan's reserved words and the built-in names renaming leaves alone
#'
#' A block names keywords, types and constraints as bare words. The
#' built-in functions listed here also appear as calls, which
#' `stan_function_calls()` finds in any case. `normalize = FALSE` makes
#' brms call the unnormalised form of every density, `_lupdf` or
#' `_lupmf`. Those spellings are derived from the normalised ones.
#'
#' @return Character vector of names
#' @noRd
get_stan_reserved_words <- function() {
  reserved <- c(
    # Stan data types
    "int", "real", "vector", "row_vector", "matrix", "array", "void",
    "simplex", "unit_vector", "ordered", "positive_ordered",
    "corr_matrix", "cov_matrix", "cholesky_factor_corr", "cholesky_factor_cov",

    # Stan constraints and bounds
    "lower", "upper", "multiplier", "offset",

    # Stan block keywords
    "functions", "data", "transformed", "parameters", "model",
    "generated", "quantities",

    # Stan control flow
    "if", "else", "for", "while", "in", "break", "continue", "return",

    # Stan operators and special symbols
    "and", "or", "not",

    # Stan built-in variables and functions
    "target", "increment_log_prob", "get_lp", "print", "reject",
    "size", "length", "dimensions", "rows", "cols", "num_elements",

    # brms internal variables
    "prior_only", "lprior", "temp_intercept", "temp_Intercept",

    # Functions brms programs call
    "scale_r_cor", "multiply_lower_tri_self_transpose", "choose",

    # Stan mathematical functions
    "abs", "acos", "acosh", "asin", "asinh", "atan", "atan2", "atanh",
    "cos", "cosh", "sin", "sinh", "tan", "tanh", "exp", "exp2", "expm1",
    "log", "log10", "log1p", "log2", "sqrt", "square", "pow", "inv",
    "inv_sqrt", "inv_square", "fabs", "fdim", "fmin", "fmax", "fmod",
    "floor", "ceil", "round", "trunc", "nearbyint", "rint",

    # Stan linear algebra functions
    "dot_product", "columns_dot_product", "rows_dot_product",
    "dot_self", "columns_dot_self", "rows_dot_self", "tcrossprod",
    "crossprod", "quad_form", "quad_form_diag", "quad_form_sym",
    "trace", "determinant", "log_determinant", "inverse", "eigenvalues",
    "eigenvectors", "cholesky_decompose", "singular_values",
    "qr_Q", "qr_R", "sort_asc", "sort_desc", "sort_indices_asc",
    "sort_indices_desc", "rank", "reverse",

    # Stan array/vector/matrix functions
    "rep_array", "rep_vector", "rep_row_vector", "rep_matrix",
    "ones_array", "ones_vector", "ones_row_vector", "ones_matrix",
    "zeros_array", "zeros_vector", "zeros_row_vector", "zeros_matrix",
    "uniform_simplex", "unit_vector", "linspaced_array", "linspaced_vector",
    "linspaced_row_vector", "one_hot_array", "one_hot_vector",
    "one_hot_row_vector", "identity_matrix", "diag_matrix",
    "diag_pre_multiply", "diag_post_multiply", "block", "sub_col",
    "sub_row", "head", "tail", "segment", "append_array", "append_col",
    "append_row", "to_vector", "to_row_vector", "to_matrix", "to_array_1d",
    "to_array_2d",

    # Stan probability distributions (lpdf/lpmf)
    "normal_lpdf", "std_normal_lpdf", "normal_id_glm_lpdf", "student_t_lpdf",
    "cauchy_lpdf",
    "double_exponential_lpdf", "logistic_lpdf", "gumbel_lpdf",
    "lognormal_lpdf", "chi_square_lpdf", "inv_chi_square_lpdf",
    "scaled_inv_chi_square_lpdf", "exponential_lpdf", "gamma_lpdf",
    "inv_gamma_lpdf", "weibull_lpdf", "frechet_lpdf", "rayleigh_lpdf",
    "wiener_lpdf", "pareto_lpdf", "pareto_type_2_lpdf", "beta_lpdf",
    "beta_proportion_lpdf", "uniform_lpdf", "bernoulli_lpdf",
    "bernoulli_logit_lpdf", "binomial_lpdf", "binomial_logit_lpdf",
    "beta_binomial_lpdf", "hypergeometric_lpdf", "categorical_lpdf",
    "categorical_logit_lpdf", "discrete_range_lpdf", "ordered_logistic_lpdf",
    "ordered_probit_lpdf", "poisson_lpdf", "poisson_log_lpdf",
    "neg_binomial_lpdf", "neg_binomial_2_lpdf", "neg_binomial_2_log_lpdf",
    "multinomial_lpdf", "dirichlet_lpdf", "multi_normal_lpdf",
    "multi_normal_prec_lpdf", "multi_normal_cholesky_lpdf",
    "multi_student_t_lpdf", "gaussian_dlm_obs_lpdf", "wishart_lpdf",
    "inv_wishart_lpdf", "lkj_corr_lpdf", "lkj_corr_cholesky_lpdf",

    # Stan distribution CDFs
    "normal_cdf", "normal_ccdf", "normal_lcdf", "normal_lccdf",
    "std_normal_cdf", "std_normal_ccdf", "std_normal_lcdf", "std_normal_lccdf",
    "student_t_cdf", "student_t_ccdf", "student_t_lcdf", "student_t_lccdf",
    "cauchy_cdf", "cauchy_ccdf", "cauchy_lcdf", "cauchy_lccdf",
    "double_exponential_cdf", "double_exponential_ccdf",
    "double_exponential_lcdf", "double_exponential_lccdf",
    "logistic_cdf", "logistic_ccdf", "logistic_lcdf", "logistic_lccdf",
    "gumbel_cdf", "gumbel_ccdf", "gumbel_lcdf", "gumbel_lccdf",
    "lognormal_cdf", "lognormal_ccdf", "lognormal_lcdf", "lognormal_lccdf",
    "chi_square_cdf", "chi_square_ccdf", "chi_square_lcdf", "chi_square_lccdf",
    "inv_chi_square_cdf", "inv_chi_square_ccdf", "inv_chi_square_lcdf",
    "inv_chi_square_lccdf", "scaled_inv_chi_square_cdf",
    "scaled_inv_chi_square_ccdf", "scaled_inv_chi_square_lcdf",
    "scaled_inv_chi_square_lccdf", "exponential_cdf", "exponential_ccdf",
    "exponential_lcdf", "exponential_lccdf", "gamma_cdf", "gamma_ccdf",
    "gamma_lcdf", "gamma_lccdf", "inv_gamma_cdf", "inv_gamma_ccdf",
    "inv_gamma_lcdf", "inv_gamma_lccdf", "weibull_cdf", "weibull_ccdf",
    "weibull_lcdf", "weibull_lccdf", "frechet_cdf", "frechet_ccdf",
    "frechet_lcdf", "frechet_lccdf", "rayleigh_cdf", "rayleigh_ccdf",
    "rayleigh_lcdf", "rayleigh_lccdf", "pareto_cdf", "pareto_ccdf",
    "pareto_lcdf", "pareto_lccdf", "pareto_type_2_cdf", "pareto_type_2_ccdf",
    "pareto_type_2_lcdf", "pareto_type_2_lccdf", "beta_cdf", "beta_ccdf",
    "beta_lcdf", "beta_lccdf", "beta_proportion_cdf", "beta_proportion_ccdf",
    "beta_proportion_lcdf", "beta_proportion_lccdf", "uniform_cdf",
    "uniform_ccdf", "uniform_lcdf", "uniform_lccdf", "bernoulli_cdf",
    "bernoulli_ccdf", "bernoulli_lcdf", "bernoulli_lccdf",
    "bernoulli_logit_cdf", "bernoulli_logit_ccdf", "bernoulli_logit_lcdf",
    "bernoulli_logit_lccdf", "binomial_cdf", "binomial_ccdf", "binomial_lcdf",
    "binomial_lccdf", "binomial_logit_cdf", "binomial_logit_ccdf",
    "binomial_logit_lcdf", "binomial_logit_lccdf", "beta_binomial_cdf",
    "beta_binomial_ccdf", "beta_binomial_lcdf", "beta_binomial_lccdf",
    "hypergeometric_cdf", "hypergeometric_ccdf", "hypergeometric_lcdf",
    "hypergeometric_lccdf", "poisson_cdf", "poisson_ccdf", "poisson_lcdf",
    "poisson_lccdf", "poisson_log_cdf", "poisson_log_ccdf", "poisson_log_lcdf",
    "poisson_log_lccdf", "neg_binomial_cdf", "neg_binomial_ccdf",
    "neg_binomial_lcdf", "neg_binomial_lccdf", "neg_binomial_2_cdf",
    "neg_binomial_2_ccdf", "neg_binomial_2_lcdf", "neg_binomial_2_lccdf",
    "neg_binomial_2_log_cdf", "neg_binomial_2_log_ccdf",
    "neg_binomial_2_log_lcdf", "neg_binomial_2_log_lccdf",

    # Stan random number generators
    "normal_rng", "std_normal_rng", "student_t_rng", "cauchy_rng",
    "double_exponential_rng",
    "logistic_rng", "gumbel_rng", "lognormal_rng", "chi_square_rng",
    "inv_chi_square_rng", "scaled_inv_chi_square_rng", "exponential_rng",
    "gamma_rng", "inv_gamma_rng", "weibull_rng", "frechet_rng",
    "rayleigh_rng", "wiener_rng", "pareto_rng", "pareto_type_2_rng",
    "beta_rng", "beta_proportion_rng", "uniform_rng", "bernoulli_rng",
    "bernoulli_logit_rng", "binomial_rng", "binomial_logit_rng",
    "beta_binomial_rng", "hypergeometric_rng", "categorical_rng",
    "categorical_logit_rng", "discrete_range_rng", "ordered_logistic_rng",
    "ordered_probit_rng", "poisson_rng", "poisson_log_rng",
    "neg_binomial_rng", "neg_binomial_2_rng", "neg_binomial_2_log_rng",
    "multinomial_rng", "dirichlet_rng", "multi_normal_rng",
    "multi_normal_prec_rng", "multi_normal_cholesky_rng",
    "multi_student_t_rng", "wishart_rng", "inv_wishart_rng",
    "lkj_corr_rng", "lkj_corr_cholesky_rng",

    # Special Stan functions
    "step", "is_inf", "is_nan", "positive_infinity", "negative_infinity",
    "not_a_number", "machine_precision", "max", "min", "sum", "prod",
    "log_sum_exp", "mean", "variance", "sd", "distance", "squared_distance",
    "fma", "multiply_log", "log1m", "log1p_exp", "log1m_exp", "log_diff_exp",
    "log_mix", "log_falling_factorial", "log_rising_factorial", "lgamma",
    "digamma", "trigamma", "lbeta", "binomial_coefficient_log",
    "bessel_first_kind", "bessel_second_kind", "modified_bessel_first_kind",
    "modified_bessel_second_kind",
    "falling_factorial", "rising_factorial", "expm1", "log1p", "hypot"
  )
  reserved <- unique(reserved)
  normalised <- grep("_lp[dm]f$", reserved, value = TRUE)
  c(reserved, sub("_lp([dm])f$", "_lup\\1f", normalised))
}

#' The identifiers of a piece of Stan code
#'
#' Comments and string literals go first. A name inside a string is no
#' identifier. A `//` inside one starts no comment.
#'
#' @param stan_code Stan code
#' @return Unique identifiers, in order of first appearance
#' @noRd
extract_stan_identifiers <- function(stan_code) {
  checkmate::assert_string(stan_code)
  code <- paste(stan_line_code(strsplit(stan_code, "\n")[[1L]]),
                collapse = "\n")
  unique(regmatches(code, gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*\\b", code,
                                   perl = TRUE))[[1L]])
}

#' The functions a piece of Stan code calls
#'
#' A name followed by `(` is a function, or a keyword such as `for`.
#' Neither takes a suffix.
#'
#' @param stan_code Stan code
#' @return Unique names
#' @noRd
stan_function_calls <- function(stan_code) {
  checkmate::assert_string(stan_code)
  code <- paste(stan_line_code(strsplit(stan_code, "\n")[[1L]]),
                collapse = "\n")
  calls <- regmatches(code, gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*\\s*\\(",
                                     code, perl = TRUE))[[1L]]
  unique(sub("\\s*\\($", "", calls))
}

#' The variables a piece of Stan code assigns
#'
#' A declaration with a value, `vector[N] gp_pred_1 = ...`, assigns
#' one. So does an assignment at the start of a line, `s_1_1 = ...`.
#'
#' @param stan_code Stan code
#' @return Character vector of names, reserved words excluded
#' @noRd
extract_computed_variables <- function(stan_code) {
  checkmate::assert_string(stan_code)
  typed <- paste0("\\b(?:vector|matrix|real|int|array)\\s*",
                  "(?:\\[[^\\]]*\\])*\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*=")
  declared <- regmatches(stan_code, gregexpr(typed, stan_code,
                                             perl = TRUE))[[1L]]
  declared <- sub(typed, "\\1", declared, perl = TRUE)
  lines <- strsplit(stan_code, "\n")[[1L]]
  direct <- "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*=.*$"
  assigned <- sub(direct, "\\1", grep(direct, lines, value = TRUE,
                                       perl = TRUE), perl = TRUE)
  out <- c(declared, assigned)
  out[nzchar(out) & !out %in% get_stan_reserved_words()]
}

#' The identifiers of a trend block that take the suffix
#'
#' Stan's reserved words, the functions the program defines or calls,
#' the placeholder response `Y` and the parameters the trend program
#' drops keep their names.
#'
#' @param identifiers Identifiers of the block
#' @param mapping The renaming map, whose `custom_functions` are the
#'   functions the trend program defines
#' @param exclude Further names to leave alone
#' @return The identifiers to rename
#' @noRd
filter_renameable_identifiers <- function(identifiers, mapping = NULL,
                                          exclude = character(0)) {
  checkmate::assert_character(identifiers)
  checkmate::assert_list(mapping, null.ok = TRUE)
  checkmate::assert_character(exclude)
  keep <- c(get_stan_reserved_words(), mapping$custom_functions, exclude,
            "Y", brms_trend_dropped_params)
  identifiers[!identifiers %in% keep]
}

#' The trend program's data, renamed
#'
#' Each data object the renamed data block declares becomes a stanvar
#' with that declaration, in the block's order. A later declaration
#' then takes its size from an earlier one, as `knots_1_trend` sizes a
#' spline basis.
#'
#' @param standata The trend program's data
#' @param suffix Suffix appended to every renamed identifier
#' @param mapping The renaming map, holding the renamed data block as
#'   `data_declarations`
#' @return A stanvars object, or NULL for a program without data
#' @noRd
extract_and_rename_standata_objects <- function(standata, suffix,
                                                mapping) {
  checkmate::assert_list(standata, names = "named")
  checkmate::assert_string(suffix, min.chars = 1L)
  checkmate::assert_list(mapping, names = "named")
  declarations <- mapping$data_declarations
  if (is.null(declarations)) {
    return(NULL)
  }
  checkmate::assert_string(declarations)
  lines <- trimws(strsplit(declarations, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines)]

  data_names <- filter_renameable_identifiers(names(standata),
                                              mapping = mapping)
  renamed <- paste0(data_names, suffix)
  line_of <- vapply(renamed, function(name) {
    hit <- grep(paste0("\\b", name, "\\b"), lines)
    if (length(hit) > 0L) hit[1L] else NA_integer_
  }, integer(1L))
  declared <- which(!is.na(line_of))
  declared <- declared[order(line_of[declared])]
  combine_stanvars(lapply(declared, function(i) {
    brms::stanvar(x = standata[[data_names[i]]], name = renamed[i],
                  scode = paste0("  ", lines[line_of[i]]), block = "data")
  }))
}

#' The rows of the trend data each state draws its predictor from
#'
#' `times_trend[i, k]` indexes the trend data at time `i` and position
#' `k` on the second axis. `mu_trend` shares that row order. The trend
#' data is arranged by time, then by the second axis. Three layouts:
#' - a shared trend: one row per time, which every series takes,
#' - one trend per series: `(i - 1) * n_series + k`,
#' - `by = lv_axis()`: one row per time and factor,
#'   `(i - 1) * n_lv + k`, over `N_lv_trend`.
#'
#' @param n_time Number of times
#' @param n_series Number of series, the default second axis
#' @param n_trend_rows Rows of the trend data. NULL gives one per time
#'   and second-axis position.
#' @param has_by_lv Whether the second axis runs over the factors
#' @param n_lv Number of factors, needed with `has_by_lv`
#' @return A data stanvar named `times_trend`
#' @noRd
create_times_trend_matrix <- function(n_time, n_series,
                                      n_trend_rows = NULL,
                                      has_by_lv = FALSE, n_lv = NULL) {
  checkmate::assert_count(n_time, positive = TRUE)
  checkmate::assert_count(n_series, positive = TRUE)
  checkmate::assert_count(n_trend_rows, positive = TRUE, null.ok = TRUE)
  checkmate::assert_flag(has_by_lv)
  checkmate::assert_integerish(n_lv, lower = 1L, len = 1L, null.ok = TRUE)
  if (has_by_lv && is.null(n_lv)) {
    stop_mvgam_fault(
      "A factor-grain time index was requested without 'n_lv'.",
      "'create_times_trend_matrix()' needs 'n_lv' with 'has_by_lv'."
    )
  }

  second_axis_size <- if (has_by_lv) as.integer(n_lv) else as.integer(n_series)
  second_axis_dim_name <- if (has_by_lv) "N_lv_trend" else "N_series_trend"

  n_trend_rows <- as.integer(n_trend_rows %||% (n_time * second_axis_size))
  n_unique_trend_second <- as.integer(n_trend_rows / n_time)
  # `refuse_ragged_trend_grid()` has refused a ragged frame already
  if (n_unique_trend_second * n_time != n_trend_rows) {
    stop_mvgam_fault(
      "The trend data does not fill its time grid.",
      paste0("Got ", n_trend_rows, " rows over ", n_time, " times.")
    )
  }

  if (n_unique_trend_second == 1L) {
    times_array <- matrix(rep(seq_len(n_time), times = second_axis_size),
                          nrow = n_time, ncol = second_axis_size)
  } else {
    times_array <- (row(matrix(0L, n_time, second_axis_size)) - 1L) *
      n_unique_trend_second +
      col(matrix(0L, n_time, second_axis_size))
  }
  storage.mode(times_array) <- "integer"

  brms::stanvar(
    x = times_array,
    name = "times_trend",
    scode = paste0("array[N_time_trend, ", second_axis_dim_name,
                   "] int times_trend;"),
    block = "data"
  )
}

# =============================================================================
# Stan function deduplication
# =============================================================================
# The observation and trend programs may define the same function,
# such as `gp_exp_quad` for Gaussian processes. Stan refuses a
# function defined twice. One copy of each is kept.

#' A Stan program with one copy of each function
#'
#' The observation and trend programs can define the same function.
#' The first definition stays, with the comments above it.
#'
#' @param stan_code A Stan program
#' @return The program
#' @noRd
deduplicate_stan_functions <- function(stan_code) {
  checkmate::assert_string(stan_code)
  functions_content <- extract_stan_functions_block(stan_code)
  if (is.null(functions_content)) {
    return(stan_code)
  }
  functions_list <- parse_stan_functions(functions_content)
  if (length(functions_list) <= 1L) {
    return(stan_code)
  }
  replace_stan_functions_block(
    stan_code,
    reconstruct_functions_block(remove_duplicate_functions(functions_list))
  )
}

#' The body of a program's functions block
#'
#' @param stan_code A Stan program
#' @return The lines between the block's braces, `""` for an empty
#'   block, or NULL when the block is absent
#' @noRd
extract_stan_functions_block <- function(stan_code) {
  checkmate::assert_character(stan_code, len = 1, any.missing = FALSE)

  lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]

  # Find functions block start
  functions_start <- grep(stan_block_header("functions"), lines,
                          ignore.case = TRUE)
  if (length(functions_start) == 0) {
    return(NULL)  # No functions block
  }

  # The brace the header opens closes the block. A nested brace in a
  # function body closes earlier. That brace bounds the content
  # exactly. The block's own closing line falls outside it.
  closes_at <- find_matching_closing_brace(lines, functions_start[1])
  if (!is.na(closes_at)) {
    if (closes_at <= functions_start[1] + 1L) {
      return("")
    }
    content_lines <- lines[(functions_start[1] + 1):(closes_at - 1L)]
    return(paste(content_lines, collapse = "\n"))
  }

  # Braces that do not balance leave the next block's header as the
  # bound. The block's own closing line falls inside that span.
  next_block <- stan_next_block_line(
    lines, after = functions_start[1], exclude = "functions"
  )
  last <- if (is.na(next_block)) length(lines) else next_block - 1L
  if (last <= functions_start[1]) {
    return("")
  }
  content_lines <- lines[(functions_start[1] + 1):last]
  if (grepl("^\\s*}\\s*$", content_lines[length(content_lines)])) {
    content_lines <- content_lines[-length(content_lines)]
  }

  return(paste(content_lines, collapse = "\n"))
}

#' The function definitions of a functions block
#'
#' A signature written over several lines is joined into one. Each
#' definition keeps the comment lines written directly above it. A
#' blank line or a stray closing brace ends a run of comments.
#'
#' @param functions_content Body of a functions block
#' @return List with one entry per function: its `name`, its
#'   `signature`, its `full_lines` (the joined signature then the body),
#'   its `body_lines` and its `doc_lines`
#' @noRd
parse_stan_functions <- function(functions_content) {
  checkmate::assert_string(functions_content)
  lines <- strsplit(functions_content, "\n", fixed = TRUE)[[1L]]
  header <- paste0("^\\s*([a-zA-Z_][a-zA-Z0-9_<>,\\[\\]\\s]*?)\\s+",
                   "([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(([^)]*)\\)\\s*\\{")
  functions_list <- list()
  doc <- character(0)
  i <- 1L
  while (i <= length(lines)) {
    line <- lines[i]
    if (grepl("^\\s*//", line)) {
      doc <- c(doc, line)
      i <- i + 1L
      next
    }
    if (grepl("^\\s*/\\*", line)) {
      closes <- i - 1L + which(grepl("\\*/", lines[i:length(lines)]))[1L]
      if (is.na(closes)) {
        closes <- length(lines)
      }
      doc <- c(doc, lines[i:closes])
      i <- closes + 1L
      next
    }
    parts <- character(0)
    if (grepl("^\\s*[a-zA-Z_].*\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s*\\(", line)) {
      # The signature runs to the line holding its opening brace
      j <- i
      while (j < length(lines) &&
             !grepl("{", paste(lines[i:j], collapse = " "), fixed = TRUE)) {
        j <- j + 1L
      }
      signature_lines <- lines[i:j]
      signature <- paste(signature_lines, collapse = " ")
      parts <- regmatches(signature, regexec(header, signature,
                                             perl = TRUE))[[1L]]
    }
    if (length(parts) == 0L) {
      doc <- character(0)
      i <- i + 1L
      next
    }

    # The body runs to the brace that balances the signature's
    depth <- sum(vapply(signature_lines, count_stan_braces, integer(1L)))
    body <- character(0)
    i <- j + 1L
    while (i <= length(lines) && depth > 0L) {
      body <- c(body, lines[i])
      depth <- depth + count_stan_braces(lines[i])
      i <- i + 1L
    }
    name <- trimws(parts[3L])
    functions_list[[length(functions_list) + 1L]] <- list(
      name = name,
      signature = paste0(trimws(parts[2L]), " ", name, "(",
                         trimws(parts[4L]), ")"),
      full_lines = c(signature, body),
      body_lines = body,
      doc_lines = doc
    )
    doc <- character(0)
  }
  functions_list
}

#' One copy of each function definition
#'
#' Two definitions are the same function when their signatures match
#' and their bodies match with comments and whitespace removed. The
#' first is kept.
#' Two bodies under one signature are a fault in the generated code.
#'
#' @param functions_list Entries from `parse_stan_functions()`
#' @return The entries, one per signature
#' @noRd
remove_duplicate_functions <- function(functions_list) {
  checkmate::assert_list(functions_list, min.len = 1L)
  signatures <- vapply(functions_list, function(f) {
    normalize_function_signature(f$signature)
  }, character(1L))
  lapply(unique(signatures), function(sig) {
    matching <- functions_list[signatures == sig]
    bodies <- vapply(matching, function(f) {
      normalize_function_body(f$body_lines)
    }, character(1L))
    if (length(unique(bodies)) > 1L) {
      stop_mvgam_fault(
        "Two generated Stan functions share a signature.",
        paste0("Function '", matching[[1L]]$name, "' has two bodies.")
      )
    }
    matching[[1L]]
  })
}

#' A function signature with its spacing normalised
#'
#' @param signature Signature from `parse_stan_functions()`
#' @return Character scalar
#' @noRd
normalize_function_signature <- function(signature) {
  checkmate::assert_string(signature)
  normalized <- gsub("\\s+", " ", trimws(signature))
  gsub("\\s*([<>,])\\s*", "\\1", normalized)
}

#' A function body with its comments and spacing removed
#'
#' Two bodies that differ inside a string literal are different
#' functions. The literal stays. A block comment spanning lines is
#' stripped over the whole body first.
#'
#' @param body_lines Lines of the body
#' @return Character scalar
#' @noRd
normalize_function_body <- function(body_lines) {
  checkmate::assert_character(body_lines, any.missing = FALSE)
  body_lines <- stan_drop_block_comments(body_lines)
  body_lines <- trimws(vapply(body_lines, stan_drop_line_comment,
                              character(1L), USE.NAMES = FALSE))
  paste(gsub("\\s+", " ", body_lines[nzchar(body_lines)]), collapse = " ")
}

#' A functions block from its function entries
#'
#' @param unique_functions Entries from `parse_stan_functions()`
#' @return The block body, each function after its comments
#' @noRd
reconstruct_functions_block <- function(unique_functions) {
  checkmate::assert_list(unique_functions)
  paste(vapply(unique_functions, function(f) {
    paste(c(f$doc_lines, f$full_lines), collapse = "\n")
  }, character(1L)), collapse = "\n\n")
}

#' A Stan program with a new functions block
#'
#' The block runs from its header to the line before the next block.
#' An empty body removes the block.
#'
#' @param stan_code A Stan program
#' @param new_functions_content The new block body
#' @return The program
#' @noRd
replace_stan_functions_block <- function(stan_code, new_functions_content) {
  checkmate::assert_string(stan_code)
  checkmate::assert_string(new_functions_content)
  lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1L]]
  start <- grep(stan_block_header("functions"), lines,
                ignore.case = TRUE)[1L]
  if (is.na(start)) {
    return(stan_code)
  }
  next_block <- stan_next_block_line(lines, after = start,
                                     exclude = "functions")
  end <- if (is.na(next_block)) length(lines) else next_block - 1L
  block <- if (nzchar(trimws(new_functions_content))) {
    c("functions {", new_functions_content, "}")
  }
  paste(c(lines[seq_len(start - 1L)], block,
          lines[seq_len(length(lines) - end) + end]), collapse = "\n")
}

#' How many occasions carry a response
#'
#' The trend grid spans every row of the frame. A panel padded with
#' `NA`, or a `newdata` that extends it, reaches past the last
#' response. A share of the history counts the occasions where some
#' response was measured. The history belongs to the time axis. One
#' measured response makes an occasion observed.
#'
#' @param data The observation frame
#' @param time_var Name of the time column
#' @param response_vars Response column names
#' @return The count. NULL when the time column is absent or every
#'   response is `NA`.
#' @noRd
count_observed_times <- function(data, time_var, response_vars) {
  checkmate::assert_string(time_var, min.chars = 1L)
  checkmate::assert_character(response_vars, any.missing = FALSE)
  if (!is.data.frame(data) || is.null(data[[time_var]])) {
    return(NULL)
  }
  absent <- setdiff(response_vars, names(data))
  if (length(absent) > 0L) {
    stop(insight::format_error(c(
      "The observation frame lacks a response column.",
      x = paste0("Missing: ", paste0("'", absent, "'", collapse = ", "),
                 ".")
    )), call. = FALSE)
  }
  observed <- Reduce(`|`, lapply(response_vars, function(resp) {
    !is.na(data[[resp]])
  }), logical(nrow(data)))
  if (!any(observed)) {
    return(NULL)
  }
  length(unique(data[[time_var]][observed]))
}
