# Post-fit surface sweep across every cached model fit.
#
# Not a test file. Invoke manually with, for example:
#   Rscript tests/local/postfit_sweep.R
#   Rscript tests/local/postfit_sweep.R --fixtures=beta_ar1,mv_gauss
#   Rscript tests/local/postfit_sweep.R --groups=invariants
#   Rscript tests/local/postfit_sweep.R --include-caches --budget=60
#
# Loads each cached fit once and drives every applicable post-fit
# method against it, recording a status and a shape string per call.
# Results are written to tests/local/postfit_sweep_results.tsv and a
# failure digest is printed at the end. Nothing is re-fitted: paths
# that need missing responses are reached by injecting NAs into a
# loaded fit.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(marginaleffects)
})

options(marginaleffects_model_classes = "mvgam")

# -- CLI ---------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^--", flag, "="), args, value = TRUE)
  if (length(hit) == 0L) return(default)
  strsplit(sub(paste0("^--", flag, "="), "", hit[1L]), ",")[[1L]]
}
only_fixtures <- arg_value("fixtures")
only_groups <- arg_value("groups")
out_path <- arg_value("out", "tests/local/postfit_sweep_results.tsv")[1L]

# The pkgdown caches hold fits from whenever their article was last
# rendered, so a failure on one says the cache is old at least as often
# as it says the code is wrong. They are swept only on request, and
# reported apart from the current fixtures either way, so a stale cache
# cannot bury a live defect under a pile of its own.
include_caches <- "--include-caches" %in% args

# A single pathological fit should not be able to stall the sweep or
# exhaust the machine: a wide VAR builds draws x n_series x n_series
# arrays and will take both if allowed to. Calls over budget are
# recorded as such rather than waited on.
call_budget <- as.numeric(arg_value("budget", "120")[1L])

FIXTURE_DIR <- "tests/local/fixtures"
PKGDOWN_DIRS <- Sys.glob("pkgdown/*_cache")

# -- Result collection -------------------------------------------------

results <- new.env(parent = emptyenv())
results$rows <- list()

# Record one call. `expr` is evaluated lazily so a failure is captured
# rather than aborting the sweep. `shape` summarises the returned
# object so a silently wrong dimension is visible in the log.
# Provenance of the fit currently being swept, set by the driver loop
# so every row records whether it came from a maintained fixture or
# from a cache that may simply be old.
current_source <- "fixture"

run_call <- function(fixture, group, label, expr, shape = shape_of) {
  if (!is.null(only_groups) && !(group %in% only_groups)) return(invisible(NULL))
  warns <- character(0)
  t0 <- Sys.time()
  setTimeLimit(elapsed = call_budget, transient = TRUE)
  val <- withCallingHandlers(
    tryCatch(suppressMessages(force(expr)),
             error = function(e) structure(conditionMessage(e),
                                           class = "sweep_error")),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  setTimeLimit(elapsed = Inf, transient = FALSE)
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  failed <- inherits(val, "sweep_error")
  # A call stopped by the budget is neither a pass nor a defect; it is
  # a statement about cost, and is kept apart from both.
  timed_out <- failed &&
    grepl("reached elapsed time limit", as.character(val), fixed = TRUE)
  detail <- if (timed_out) {
    paste0("exceeded ", call_budget, "s budget")
  } else if (failed) {
    squash(as.character(val))
  } else {
    squash(shape(val))
  }
  # An invariant reports its verdict in the detail rather than by
  # raising, so a broken one returns normally and would otherwise be
  # filed as a pass. The status column is what a reader scans, and it
  # has to say what the detail says.
  row <- list(
    fixture = fixture,
    source = current_source,
    group = group,
    label = label,
    status = if (timed_out) {
      "SLOW"
    } else if (failed) {
      "ERR"
    } else if (grepl("^VIOLATED", detail)) {
      "BAD"
    } else {
      "OK"
    },
    detail = detail,
    warnings = squash(paste(unique(warns), collapse = " | ")),
    secs = secs
  )
  results$rows[[length(results$rows) + 1L]] <- row
  flush_row(row)
  cat(sprintf("  %-4s %-22s %-46s %s\n", row$status, group, label,
              substr(row$detail, 1L, 70L)))
  invisible(if (failed) structure(as.character(val), class = "sweep_error") else val)
}


# Rows are appended as they are produced. The sweep is long enough that
# it is often interrupted, and results that only exist at the end are
# results that get lost.
flush_started <- FALSE
flush_row <- function(row) {
  df <- as.data.frame(row, stringsAsFactors = FALSE)
  utils::write.table(
    df, out_path, sep = "\t", row.names = FALSE,
    col.names = !flush_started, append = flush_started, quote = TRUE
  )
  flush_started <<- TRUE
}

squash <- function(x) {
  if (length(x) == 0L) return("")
  x <- paste(as.character(x), collapse = " ")
  gsub("[\r\n\t]+", " ", x)
}

# Compact description of whatever a method returned, so the log
# distinguishes "ran" from "returned the right thing".
shape_of <- function(x) {
  if (is.null(x)) return("NULL")
  cls <- paste(class(x), collapse = "/")
  if (inherits(x, "ggplot")) return(paste0("ggplot rows=", nrow_safe(x$data)))
  if (inherits(x, "patchwork")) return("patchwork")
  if (is.array(x) || is.matrix(x)) {
    return(paste0(cls, " dim=", paste(dim(x), collapse = "x"),
                  " finite=", all(is.finite(as.numeric(x)))))
  }
  if (is.data.frame(x)) {
    return(paste0(cls, " dim=", nrow(x), "x", ncol(x)))
  }
  if (is.list(x)) return(paste0(cls, " len=", length(x)))
  if (is.numeric(x)) {
    return(paste0(cls, " len=", length(x), " finite=",
                  all(is.finite(x))))
  }
  cls
}

nrow_safe <- function(x) if (is.data.frame(x)) nrow(x) else NA_integer_

# Some methods print rather than return; capture the printed text so
# an empty or malformed print surfaces as a short detail string.
printed <- function(expr) {
  txt <- utils::capture.output(suppressMessages(print(expr)))
  paste0("lines=", length(txt), " chars=", sum(nchar(txt)))
}

# -- Fixture discovery -------------------------------------------------

discover_fits <- function() {
  paths <- character(0)
  names_ <- character(0)

  vals <- Sys.glob(file.path(FIXTURE_DIR, "val_mvgam_*.rds"))
  vals <- vals[!grepl("_truth\\.rds$", vals)]
  paths <- c(paths, vals)
  names_ <- c(names_, sub("^val_mvgam_", "", tools::file_path_sans_ext(basename(vals))))

  for (extra in c("val_jsdgam_trait.rds", "val_occ_mvgam.rds")) {
    p <- file.path(FIXTURE_DIR, extra)
    if (file.exists(p)) {
      paths <- c(paths, p)
      names_ <- c(names_, tools::file_path_sans_ext(extra))
    }
  }

  n_fixture <- length(paths)

  if (include_caches) {
    for (d in PKGDOWN_DIRS) {
      for (p in Sys.glob(file.path(d, "*.rds"))) {
        paths <- c(paths, p)
        names_ <- c(names_,
                    paste0(sub("_cache$", "", basename(d)), ":",
                           tools::file_path_sans_ext(basename(p))))
      }
    }
  }

  data.frame(
    name = names_,
    path = paths,
    source = c(rep("fixture", n_fixture),
               rep("cache", length(paths) - n_fixture)),
    stringsAsFactors = FALSE
  )
}

# What a given fit can legally be asked to do. Every group below gates
# on these rather than on tryCatch, so a surface the fit cannot answer
# is skipped instead of logged as a failure.
probe <- function(fit) {
  fam <- tryCatch(fit$family$family, error = function(e) NA_character_)
  si <- fit$series_info
  tm <- fit$trend_metadata
  dat <- fit$data %||% fit$obs_data
  resp <- fit$response_names

  # `series_info$is_multivariate` is also TRUE for a binomial fit,
  # whose `response_names` carries a structural `trials` entry;
  # the formula class is the authoritative test.
  is_mv <- isTRUE(brms::is.mvbrmsformula(fit$formula))
  trend_type <- if (is.null(tm$trend_type)) NA_character_ else tm$trend_type[1L]
  vars <- tryCatch(variables(fit), error = function(e) character(0))

  list(
    family = fam,
    data = dat,
    responses = resp,
    is_mv = is_mv,
    mv_resp = if (is_mv) names(fit$formula$forms) else NULL,
    n_series = si$n_series %||% 1L,
    trend_type = trend_type,
    has_trend = !is.na(trend_type),
    is_var = identical(trend_type, "VAR"),
    is_factor = !is.null(tm$n_lv) && length(tm$n_lv) && !is.na(tm$n_lv[1L]),
    is_closure = isTRUE(tryCatch(is_closure_unit_family(fit$family),
                                 error = function(e) FALSE)),
    is_multi_resp_fam = isTRUE(tryCatch(is_multi_response_family(fit$family),
                                        error = function(e) FALSE)),
    is_ordinal = fam %in% c("cumulative", "sratio", "cratio", "acat"),
    is_jsdgam = inherits(fit, "jsdgam"),
    has_time = !is.null(dat) && "time" %in% names(dat),
    has_series_col = !is.null(dat) && "series" %in% names(dat),
    has_ranef = any(grepl("^sd_|^r_", vars)),
    has_smooth = any(grepl("^sds_|^s_", vars)),
    vars = vars
  )
}

# A copy of the fit with NAs punched into the response, used to reach
# the missing-data branches without re-fitting.
with_na_response <- function(fit, cap) {
  dat <- cap$data
  if (is.null(dat)) return(NULL)
  y <- cap$responses[1L]
  if (is.null(y) || !y %in% names(dat)) return(NULL)
  idx <- unique(round(seq(2, nrow(dat), length.out = 3L)))
  idx <- idx[idx >= 1L & idx <= nrow(dat)]
  if (!length(idx)) return(NULL)
  fit$data[[y]][idx] <- NA
  fit
}

# -- Surface groups ----------------------------------------------------

ND <- 25L

# For a multivariate fit most methods need to be told which response
# to work on; drive the first one so the surface is at least reached.
r1 <- function(cap) if (cap$is_mv) cap$mv_resp[1L] else NULL

# Build a method call, dropping `resp` when the fit is univariate so
# univariate methods are not handed an argument they reject.
mcall <- function(fn, fit, cap, ...) {
  a <- list(fit, ...)
  rr <- r1(cap)
  if (!is.null(rr)) a$resp <- rr
  do.call(fn, a)
}

group_summary <- function(nm, fit, cap) {
  run_call(nm, "summary", "summary()", summary(fit))
  run_call(nm, "summary", "print(summary())", printed(summary(fit)),
           shape = identity)
  run_call(nm, "summary", "summary(include_betas=FALSE)",
           summary(fit, include_betas = FALSE))
  run_call(nm, "summary", "as.data.frame(variable='^trend[')",
           as.data.frame(fit, variable = "^trend\\[", regex = TRUE))
  run_call(nm, "summary", "print()", printed(fit), shape = identity)
  run_call(nm, "summary", "family()", family(fit))
  run_call(nm, "summary", "formula()", formula(fit))
  run_call(nm, "summary", "nobs()", nobs(fit))
  run_call(nm, "summary", "how_to_cite()", how_to_cite(fit))
  run_call(nm, "summary", "methods_md()", methods_md(fit))
}

group_predict <- function(nm, fit, cap) {
  types <- c("response", "link", "expected", "variance")
  if (cap$is_closure) types <- c(types, "latent_state", "detection")
  # `terms` is offered by match.arg() and then rejected; call it so
  # the log records the advertised-then-refused behaviour.
  types <- c(types, "terms")
  for (ty in types) {
    run_call(nm, "predict", paste0("predict(type='", ty, "')"),
             mcall(predict, fit, cap, type = ty, ndraws = ND))
  }
  run_call(nm, "predict", "predict(process_error=TRUE)",
           mcall(predict, fit, cap, ndraws = ND, process_error = TRUE))
  run_call(nm, "predict", "predict(summary=FALSE)",
           mcall(predict, fit, cap, ndraws = ND, summary = FALSE))
  run_call(nm, "predict", "posterior_predict()",
           mcall(posterior_predict, fit, cap, ndraws = ND))
  run_call(nm, "predict", "posterior_predict(process_error=FALSE)",
           mcall(posterior_predict, fit, cap, ndraws = ND,
                 process_error = FALSE))
  run_call(nm, "predict", "posterior_epred()",
           mcall(posterior_epred, fit, cap, ndraws = ND))
  run_call(nm, "predict", "posterior_linpred()",
           mcall(posterior_linpred, fit, cap, ndraws = ND))
  run_call(nm, "predict", "posterior_linpred(transform=TRUE)",
           mcall(posterior_linpred, fit, cap, ndraws = ND, transform = TRUE))
  run_call(nm, "predict", "fitted()", mcall(fitted, fit, cap, ndraws = ND))
  run_call(nm, "predict", "fitted(scale='linear')",
           mcall(fitted, fit, cap, ndraws = ND, scale = "linear"))
  run_call(nm, "predict", "residuals(type='quantile')",
           mcall(residuals, fit, cap, ndraws = ND, type = "quantile"))
  run_call(nm, "predict", "residuals(type='ordinary')",
           mcall(residuals, fit, cap, ndraws = ND, type = "ordinary"))
  run_call(nm, "predict", "log_lik()", mcall(log_lik, fit, cap, ndraws = ND))

  # newdata round-trip: feeding the training data back in must return
  # the same shape as the no-newdata call.
  if (!is.null(cap$data)) {
    run_call(nm, "predict", "posterior_epred(newdata=training)",
             mcall(posterior_epred, fit, cap, newdata = cap$data, ndraws = ND))
    run_call(nm, "predict", "posterior_predict(newdata=training)",
             mcall(posterior_predict, fit, cap, newdata = cap$data,
                   ndraws = ND))
  }
}

group_temporal <- function(nm, fit, cap) {
  if (!cap$has_trend || !cap$has_time) return(invisible(NULL))
  hc <- run_call(nm, "temporal", "hindcast()",
                 mcall(hindcast, fit, cap, ndraws = ND))
  if (cap$is_closure) {
    run_call(nm, "temporal", "hindcast(type='latent_state')",
             mcall(hindcast, fit, cap, ndraws = ND, type = "latent_state"))
  }
  if (!inherits(hc, "sweep_error") && !is.null(hc)) {
    run_call(nm, "temporal", "summary(hindcast)", summary(hc))
    run_call(nm, "temporal", "plot(hindcast)", plot(hc))
    run_call(nm, "temporal", "score(hindcast)", score(hc))
  }
  # Forecast one step beyond the training grid.
  nd <- future_grid(cap)
  if (!is.null(nd)) {
    fc <- run_call(nm, "temporal", "forecast(newdata)",
                   mcall(forecast, fit, cap, newdata = nd, ndraws = ND))
    if (!inherits(fc, "sweep_error") && !is.null(fc)) {
      run_call(nm, "temporal", "summary(forecast)", summary(fc))
      run_call(nm, "temporal", "plot(forecast)", plot(fc))
      run_call(nm, "temporal", "score(forecast)", score(fc))
    }
  }
}

# Extend the training grid by `h` time steps, carrying covariates
# forward from each series' last observed row.
future_grid <- function(cap, h = 3L) {
  dat <- cap$data
  if (is.null(dat) || !all(c("time") %in% names(dat))) return(NULL)
  if (cap$is_mv || cap$is_closure || cap$is_multi_resp_fam) return(NULL)
  key <- if ("series" %in% names(dat)) "series" else NULL
  split_by <- if (is.null(key)) list(dat) else split(dat, dat[[key]])
  out <- lapply(split_by, function(d) {
    if (!nrow(d)) return(NULL)
    last <- d[which.max(d$time), , drop = FALSE]
    reps <- last[rep(1L, h), , drop = FALSE]
    reps$time <- max(d$time) + seq_len(h)
    for (y in cap$responses) if (y %in% names(reps)) reps[[y]] <- NA
    reps
  })
  out <- do.call(rbind, out[!vapply(out, is.null, logical(1))])
  rownames(out) <- NULL
  out
}

group_marginaleffects <- function(nm, fit, cap) {
  if (cap$is_multi_resp_fam || cap$is_closure) {
    # These reach get_predict through a different type vocabulary;
    # exercise the entry points that are legal for them.
    run_call(nm, "marginaleffects", "avg_predictions()",
             mcall(marginaleffects::avg_predictions, fit, cap))
    return(invisible(NULL))
  }
  run_call(nm, "marginaleffects", "avg_predictions()",
           mcall(marginaleffects::avg_predictions, fit, cap))
  run_call(nm, "marginaleffects", "predictions()",
           mcall(marginaleffects::predictions, fit, cap))
  run_call(nm, "marginaleffects", "avg_slopes()",
           mcall(marginaleffects::avg_slopes, fit, cap))
  run_call(nm, "marginaleffects", "avg_comparisons()",
           mcall(marginaleffects::avg_comparisons, fit, cap))
  run_call(nm, "marginaleffects", "plot_predictions()",
           me_plot(marginaleffects::plot_predictions, fit, cap))
  run_call(nm, "marginaleffects", "plot_slopes()",
           me_plot(marginaleffects::plot_slopes, fit, cap))
  run_call(nm, "marginaleffects", "plot_comparisons()",
           me_plot(marginaleffects::plot_comparisons, fit, cap))
  run_call(nm, "marginaleffects", "hypotheses()",
           mcall(marginaleffects::hypotheses, fit, cap, hypothesis = "b1 = 0"))
  run_call(nm, "marginaleffects", "hypothesis()",
           hypothesis(fit, "Intercept = 0"))
  ce <- run_call(nm, "marginaleffects", "conditional_effects()",
                 mcall(conditional_effects, fit, cap))
  if (!inherits(ce, "sweep_error") && !is.null(ce)) {
    run_call(nm, "marginaleffects", "plot(conditional_effects)", plot(ce))
  }
  run_call(nm, "marginaleffects", "conditional_effects(type='link')",
           mcall(conditional_effects, fit, cap, type = "link"))
  if (cap$has_smooth) {
    run_call(nm, "marginaleffects", "conditional_smooths()",
             mcall(conditional_smooths, fit, cap))
    run_call(nm, "marginaleffects", "smooths()", smooths(fit))
    run_call(nm, "marginaleffects", "posterior_smooths()",
             posterior_smooths(fit, smooth = smooths(fit)[1L], ndraws = ND))
  }
}

# The plot_* trio needs a variable to put on the x axis; pick the
# first non-response, non-index column the model actually used.
me_plot <- function(fn, fit, cap) {
  preds <- tryCatch(unlist(insight::find_predictors(fit)),
                    error = function(e) character(0))
  preds <- setdiff(preds, c("time", "series", cap$responses))
  if (!length(preds)) return(structure("no plottable predictor",
                                       class = "sweep_error"))
  a <- list(fit, condition = preds[1L])
  if (identical(deparse(substitute(fn)), "marginaleffects::plot_slopes") ||
      identical(deparse(substitute(fn)), "marginaleffects::plot_comparisons")) {
    a$variables <- preds[1L]
  }
  rr <- r1(cap)
  if (!is.null(rr)) a$resp <- rr
  tryCatch(do.call(fn, a),
           error = function(e) {
             a$variables <- preds[1L]
             do.call(fn, a)
           })
}

PPC_TYPES <- c("dens_overlay", "hist", "bars", "ecdf_overlay", "stat",
               "stat_2d", "scatter_avg", "error_hist", "intervals",
               "ribbon", "loo_pit_overlay", "rootogram")
PPC_MVGAM <- c("resid_hist", "resid_ribbon", "resid_acf", "resid_pacf",
               "resid_qq", "resid_vs_fitted", "fit_stat")

group_ppcheck <- function(nm, fit, cap) {
  for (ty in c(PPC_TYPES, PPC_MVGAM)) {
    run_call(nm, "ppcheck", paste0("pp_check(type='", ty, "')"),
             mcall(pp_check, fit, cap, type = ty, ndraws = ND))
  }
  run_call(nm, "ppcheck", "pp_check(default type)",
           mcall(pp_check, fit, cap, ndraws = ND))

  # The same surface against a response carrying NAs. This is the
  # branch that unvisited closure-unit occasions land in.
  fit_na <- with_na_response(fit, cap)
  if (!is.null(fit_na)) {
    for (ty in c("dens_overlay", "bars", "ribbon", "fit_stat")) {
      run_call(nm, "ppcheck-na", paste0("pp_check(NA resp, type='", ty, "')"),
               mcall(pp_check, fit_na, cap, type = ty, ndraws = ND))
    }
    run_call(nm, "ppcheck-na", "residuals(NA resp)",
             mcall(residuals, fit_na, cap, ndraws = ND))
    run_call(nm, "ppcheck-na", "log_lik(NA resp)",
             mcall(log_lik, fit_na, cap, ndraws = ND))
    run_call(nm, "ppcheck-na", "loo(NA resp)", loo(fit_na))
  }
}

group_criticism <- function(nm, fit, cap) {
  run_call(nm, "criticism", "loo()", loo(fit))
  run_call(nm, "criticism", "waic()", waic(fit))
  run_call(nm, "criticism", "bayes_R2()", bayes_R2(fit))
  run_call(nm, "criticism", "loo_R2()", loo_R2(fit))
  run_call(nm, "criticism", "add_criterion()",
           add_criterion(fit, "loo")$criteria)
  run_call(nm, "criticism", "loo_predict()", loo_predict(fit, type = "mean"))
  run_call(nm, "criticism", "loo_epred()", loo_epred(fit))
  run_call(nm, "criticism", "loo_linpred()", loo_linpred(fit))
  run_call(nm, "criticism", "loo_predictive_interval()",
           loo_predictive_interval(fit))
  run_call(nm, "criticism", "predictive_error()",
           mcall(predictive_error, fit, cap, ndraws = ND))
  run_call(nm, "criticism", "predictive_interval()",
           mcall(predictive_interval, fit, cap, ndraws = ND))
  run_call(nm, "criticism", "posterior_interval()", posterior_interval(fit))
}

group_draws <- function(nm, fit, cap) {
  run_call(nm, "draws", "as_draws_df()", as_draws_df(fit))
  run_call(nm, "draws", "as.data.frame()", as.data.frame(fit))
  run_call(nm, "draws", "as.matrix()", as.matrix(fit))
  run_call(nm, "draws", "variables()", variables(fit))
  run_call(nm, "draws", "ndraws()", ndraws(fit))
  run_call(nm, "draws", "nvariables()", nvariables(fit))
  run_call(nm, "draws", "fixef()", fixef(fit))
  if (cap$has_ranef) {
    run_call(nm, "draws", "ranef()", ranef(fit))
    run_call(nm, "draws", "VarCorr()", VarCorr(fit))
  }
  run_call(nm, "draws", "vcov()", vcov(fit))
  run_call(nm, "draws", "coef()", coef(fit))
  run_call(nm, "draws", "prior_summary()", prior_summary(fit))
  run_call(nm, "draws", "rhat()", rhat(fit))
  run_call(nm, "draws", "neff_ratio()", neff_ratio(fit))
  run_call(nm, "draws", "posterior_summary()", posterior_summary(fit))
  run_call(nm, "draws", "tidy()", tidy(fit))
  run_call(nm, "draws", "glance()", glance(fit))
  run_call(nm, "draws", "augment()", augment(fit))
  run_call(nm, "draws", "mcmc_plot()", mcmc_plot(fit))
  run_call(nm, "draws", "stancode()", nchar(as.character(stancode(fit))),
           shape = function(x) paste0("chars=", x))
  run_call(nm, "draws", "standata()", standata(fit))
}

group_plot <- function(nm, fit, cap) {
  for (ty in c("residuals", "smooths", "trend", "series", "factors",
               "latent_state")) {
    run_call(nm, "plot", paste0("plot(type='", ty, "')"),
             mcall(plot, fit, cap, type = ty))
  }
  run_call(nm, "plot", "pairs()", pairs(fit))
}

group_structure <- function(nm, fit, cap) {
  if (cap$is_factor || cap$is_jsdgam || cap$n_series > 1L) {
    run_call(nm, "structure", "residual_cor()", residual_cor(fit))
    run_call(nm, "structure", "shared_variation()", shared_variation(fit))
  }
  if (cap$is_factor || cap$is_jsdgam) {
    run_call(nm, "structure", "active_factors()", active_factors(fit))
    run_call(nm, "structure", "ordinate()", ordinate(fit))
    run_call(nm, "structure", "compare_loadings()", compare_loadings(fit, fit))
  }
  if (cap$is_var) {
    run_call(nm, "structure", "stability()", stability(fit))
    # Both shapes: the summary a reader gets by default, and the
    # draws behind it, capped so a wide VAR does not return hundreds
    # of megabytes to a sweep that only checks its shape.
    run_call(nm, "structure", "irf()", irf(fit, h = 5L, ndraws = 50L))
    run_call(nm, "structure", "fevd()", fevd(fit, h = 5L, ndraws = 50L))
    run_call(nm, "structure", "irf(summary = FALSE)",
             irf(fit, h = 5L, ndraws = 50L, summary = FALSE))
    run_call(nm, "structure", "posterior_transition_matrix()",
             posterior_transition_matrix(fit))
  }
  if (cap$is_closure) {
    run_call(nm, "structure", "latent_N_saturation()",
             latent_N_saturation(fit))
  }
}

# -- Driver ------------------------------------------------------------

# -- Invariants --------------------------------------------------------
# Everything above asks whether a method ran. This asks whether what it
# returned can be true. A method that errors announces itself; one that
# returns a plausible-looking matrix of the wrong thing does not, and
# those are the defects that reach a release. Each check states a
# property that must hold for any fit, so it catches faults nobody has
# thought to look for yet rather than the ones already known.

# Report a property rather than a value: `run_call` logs the string, so
# a violated invariant reads as a finding instead of a shape.
holds <- function(ok, detail = "") {
  if (isTRUE(ok)) "holds" else paste0("VIOLATED ", detail)
}

group_invariants <- function(nm, fit, cap) {
  if (cap$is_closure || cap$is_multi_resp_fam || cap$is_mv) {
    # These answer at a different grain (per unit, per category, per
    # response); their contracts are checked by their own suites.
    return(invisible(NULL))
  }
  nobs <- nrow(cap$data)
  total <- tryCatch(ndraws(fit), error = function(e) NA_integer_)
  if (!is.finite(total)) return(invisible(NULL))
  n <- min(12L, total)
  ids <- sort(sample.int(total, n))

  # 1. Shape. A draw-level surface answers for every row of the data on
  #    every draw asked for, whatever the response contained.
  run_call(nm, "invariants", "shape: epred/predict/log_lik", {
    ep <- dim(posterior_epred(fit, draw_ids = ids))
    pp <- dim(posterior_predict(fit, draw_ids = ids))
    ll <- dim(log_lik(fit, draw_ids = ids))
    # An ordinal mean is a probability per category, so it answers
    # [draws x rows x categories] where a draw and a density answer
    # [draws x rows].
    ep_ok <- if (cap$is_ordinal) {
      length(ep) == 3L && identical(ep[1:2], c(n, nobs))
    } else {
      identical(ep, c(n, nobs))
    }
    holds(ep_ok && identical(pp, c(n, nobs)) && identical(ll, c(n, nobs)),
          paste(vapply(list(ep, pp, ll), paste, character(1),
                       collapse = "x"), collapse = " / "))
  }, shape = identity)

  # 2. Determinism. Naming the same draws twice has to give the same
  #    answer, or something below is re-drawing rather than reading.
  run_call(nm, "invariants", "determinism: same draw_ids", {
    a <- posterior_epred(fit, draw_ids = ids, process_error = FALSE)
    b <- posterior_epred(fit, draw_ids = ids, process_error = FALSE)
    holds(isTRUE(all.equal(a, b)))
  }, shape = identity)

  # 3. Draw alignment. Subsetting draws may drop rows and reorder them;
  #    it cannot produce a row no single draw gives. A violation means
  #    two extractions chose their draws independently.
  run_call(nm, "invariants", "alignment: subset rows are full rows", {
    key <- function(m) {
      unname(apply(round(as.matrix(m), 8), 1, paste, collapse = "|"))
    }
    full <- key(posterior_linpred(fit, draw_ids = seq_len(total),
                                  process_error = FALSE))
    part <- key(posterior_linpred(fit, draw_ids = ids,
                                  process_error = FALSE))
    holds(all(part %in% full),
          paste0(sum(!part %in% full), " of ", length(part), " unmatched"))
  }, shape = identity)

  # 4. epred is the inverse link of linpred wherever the family has no
  #    further transformation between them. Where it does, the two are
  #    expected to differ and the check is skipped rather than failed.
  plain_epred <- cap$family %in% c("gaussian", "poisson", "bernoulli",
                                   "negbinomial", "Gamma", "lognormal",
                                   "student", "beta")
  if (plain_epred) {
    run_call(nm, "invariants", "epred == linkinv(linpred)", {
      lp <- posterior_linpred(fit, draw_ids = ids, process_error = FALSE)
      ep <- posterior_epred(fit, draw_ids = ids, process_error = FALSE)
      inv <- fit$family$linkinv(lp)
      holds(isTRUE(all.equal(as.numeric(ep), as.numeric(inv),
                             tolerance = 1e-8)),
            paste0("max|diff| = ",
                   signif(max(abs(as.numeric(ep) - as.numeric(inv))), 3)))
    }, shape = identity)
  }

  # 5. Support. A draw from the response distribution has to be a value
  #    that response can take.
  run_call(nm, "invariants", "support of posterior_predict", {
    y <- as.numeric(posterior_predict(fit, draw_ids = ids))
    y <- y[!is.na(y)]
    bad <- switch(
      cap$family,
      poisson = ,
      negbinomial = y < 0 | y != floor(y),
      bernoulli = !y %in% c(0, 1),
      binomial = y < 0 | y != floor(y),
      beta = y <= 0 | y >= 1,
      Gamma = ,
      lognormal = y <= 0,
      rep(FALSE, length(y))
    )
    holds(!any(bad), paste0(sum(bad), " of ", length(y), " out of support"))
  }, shape = identity)

  # 6. Coverage of missing responses. A row the likelihood never saw is
  #    still a row the model can predict for, so predictions span the
  #    data rather than the fitted subset.
  run_call(nm, "invariants", "predictions span rows, not just fitted", {
    n_fitted <- fit$standata$N %||% nobs
    ep <- posterior_epred(fit, draw_ids = ids)
    holds(ncol(ep) == nobs,
          paste0("epred cols = ", ncol(ep), ", data rows = ", nobs,
                 ", likelihood rows = ", n_fitted))
  }, shape = identity)

  # 6b. The surface argument reaches the answer. `incl_autocor`
  #     picks between the fitted latent state and the deterministic
  #     submodel, and on a fit that carries a trend those are two
  #     different numbers. A method that accepts the argument and
  #     returns the same draws either way is not reading it.
  if (cap$has_trend) {
    run_call(nm, "invariants", "epred honours incl_autocor", {
      cond <- posterior_epred(fit, draw_ids = ids, incl_autocor = TRUE)
      marg <- posterior_epred(fit, draw_ids = ids, incl_autocor = FALSE)
      holds(!isTRUE(all.equal(cond, marg)))
    }, shape = identity)

    run_call(nm, "invariants", "predict honours incl_autocor", {
      cond <- posterior_predict(fit, draw_ids = ids, incl_autocor = TRUE)
      marg <- posterior_predict(fit, draw_ids = ids, incl_autocor = FALSE)
      holds(!isTRUE(all.equal(cond, marg)))
    }, shape = identity)

    # 6c. The conditional surface is the one `hindcast()` reports.
    #     Both read the fitted state and add it to the observation
    #     linear predictor with no sampling on either side, so they
    #     are the same quantity reached two ways and must agree
    #     exactly rather than closely.
    #
    #     `hindcast()` answers per series and stacks its arms, so its
    #     columns run series-major while `posterior_epred()` follows
    #     the rows of the data. Comparing the two as they stand asks
    #     whether the data happens to be sorted by series, which on a
    #     time-major frame it is not, and reports a violation that is
    #     only the two layouts disagreeing. The cells have to be
    #     matched before the values are.
    # An ordinal mean is a probability per category, and a hindcast
    # arm is [draws x times] per series, so the two answer at
    # different grains and `hindcast(type = "expected")` refuses
    # an ordinal fit outright.
    if (cap$has_time && !cap$is_ordinal) {
      run_call(nm, "invariants", "conditional epred == hindcast", {
        d <- as.data.frame(cap$data)
        tv <- fit$trend_metadata$variables$time_var %||% "time"
        # Named by the series identity the fit reports, which is not
        # always the column the frame carries.
        lab <- training_series_labels(fit, d)
        hc <- hindcast(fit, type = "expected")$hindcasts
        cells <- unlist(lapply(names(hc), function(s) {
          rows <- which(lab == s)
          rows[order(d[[tv]][rows])]
        }))
        ep <- posterior_epred(fit, incl_autocor = TRUE)[, cells, drop = FALSE]
        stacked <- do.call(cbind, hc)
        holds(isTRUE(all.equal(unname(ep), unname(stacked),
                               tolerance = 1e-10)),
              paste0("max |diff| = ",
                     signif(max(abs(unname(ep) - unname(stacked))), 3)))
      }, shape = identity)
    }
  }

  # 6d. The series index a prediction resolves is an index into the
  #     fitted trend matrix, and `standata$obs_trend_series` records
  #     the one the fit gave each training row. Deriving it a second
  #     time from the data is how it comes to disagree, and a
  #     disagreement that is a permutation costs no error and no
  #     missing value: every series simply reads another's state.
  if (!is.null(fit$standata$obs_trend_series)) {
    run_call(nm, "invariants", "series index matches standata", {
      d <- as.data.frame(cap$data)
      recorded <- as.integer(fit$standata$obs_trend_series)
      resolved <- as.integer(
        get_observation_structure(fit, newdata = d)$series_int
      )
      holds(length(resolved) == length(recorded) &&
              identical(resolved, recorded),
            paste0(sum(resolved != recorded), " of ", length(recorded),
                   " rows read the wrong trend column"))
    }, shape = identity)
  }

  # 6e. The labels a fit stores have to name the trend columns in the
  #     order the columns run. `obs_trend_series` says which series
  #     occupies each column, so reading a label off that and
  #     comparing it against the stored list settles the order
  #     outright. A stored list that is a permutation of the axis
  #     costs no error: every summary, plot and correlation matrix
  #     simply prints one series' name over another's estimates.
  stored_levels <- fit$trend_metadata$levels$series
  recorded_series <- fit$standata$obs_trend_series
  if (!is.null(stored_levels) && !is.null(recorded_series) &&
        length(stored_levels) > 1L) {
    run_call(nm, "invariants", "stored levels follow the trend axis", {
      labels <- as.character(training_series_labels(fit))
      s_idx <- as.integer(recorded_series)
      n_col <- as.integer(fit$standata$N_series_trend)
      occupant <- vapply(seq_len(n_col), function(k) {
        hit <- which(s_idx == k)
        if (!length(hit)) NA_character_ else labels[hit[1L]]
      }, character(1))
      holds(identical(occupant, as.character(stored_levels)),
            paste0("column ", which(occupant != stored_levels)[1L],
                   " holds '", occupant[occupant != stored_levels][1L],
                   "' but is stored as '",
                   stored_levels[occupant != stored_levels][1L], "'"))
    }, shape = identity)
  }

  # 6f. A multi-response fit records its mapping once per response,
  #     under a suffixed name, so the plain check above never reaches
  #     it. Each response must read one column, and between them the
  #     responses must use the whole axis.
  resp_names <- fit$response_names
  if (length(resp_names) > 1L &&
        !is.null(fit$standata[[paste0("obs_trend_series_",
                                      resp_names[1L])]])) {
    run_call(nm, "invariants", "each response reads its own column", {
      per <- lapply(resp_names, function(r) {
        unique(as.integer(fit$standata[[paste0("obs_trend_series_", r)]]))
      })
      names(per) <- resp_names
      spread <- resp_names[lengths(per) > 1L]
      n_col <- as.integer(fit$standata$N_series_trend)
      covered <- sort(unique(unlist(per)))
      holds(
        length(spread) == 0L && identical(covered, seq_len(n_col)),
        if (length(spread)) {
          paste0("spread across columns: ", paste(spread, collapse = ", "))
        } else {
          paste0("columns used: ", paste(covered, collapse = ","),
                 " of ", n_col)
        }
      )
    }, shape = identity)
  }

  # 7. The summary reports what the formula asked for. Every
  #    population-level coefficient the design matrix carries has to
  #    appear somewhere in the printed summary.
  run_call(nm, "invariants", "summary reports every fixed effect", {
    smry <- summary(fit)
    shown <- unlist(lapply(
      grep("^(fixed|dpar_.*_fixed|trend_fixed)$", names(smry), value = TRUE),
      function(k) rownames(smry[[k]])
    ))
    coefs <- sub("^b_", "",
                 grep("^b_", variables(fit), value = TRUE))
    coefs <- coefs[!grepl("_trend$", coefs)]
    # A parameter given a formula of its own gets a block of its own,
    # and the block heading carries the name, so the rows inside drop
    # the prefix: `b_b1_Intercept` is printed as `Intercept` under the
    # heading for `b1`.
    prefixes <- get_dpar_names(fit$formula)
    bare <- vapply(coefs, function(cf) {
      hit <- prefixes[startsWith(cf, paste0(prefixes, "_"))]
      if (length(hit)) sub(paste0("^", hit[1L], "_"), "", cf) else cf
    }, character(1), USE.NAMES = FALSE)
    missing <- unique(coefs[!bare %in% shown])
    holds(length(missing) == 0,
          paste0("absent from summary: ", paste(missing, collapse = ", ")))
  }, shape = identity)
}


GROUPS <- list(
  summary = group_summary,
  predict = group_predict,
  temporal = group_temporal,
  marginaleffects = group_marginaleffects,
  ppcheck = group_ppcheck,
  criticism = group_criticism,
  draws = group_draws,
  plot = group_plot,
  structure = group_structure,
  invariants = group_invariants
)

fits <- discover_fits()
if (!is.null(only_fixtures)) {
  fits <- fits[fits$name %in% only_fixtures, , drop = FALSE]
}

cat("Discovered", nrow(fits), "candidate fixtures\n\n")

skipped <- list()
reached <- list()

for (i in seq_len(nrow(fits))) {
  nm <- fits$name[i]
  obj <- tryCatch(readRDS(fits$path[i]), error = function(e) NULL)
  if (!inherits(obj, "mvgam")) {
    skipped[[nm]] <- paste0("not an mvgam object (",
                            paste(class(obj), collapse = "/"), ")")
    next
  }
  cap <- tryCatch(probe(obj), error = function(e) NULL)
  if (is.null(cap)) {
    skipped[[nm]] <- "capability probe failed"
    next
  }
  reached[[nm]] <- sprintf("%s / %s / n_series=%s%s%s%s",
                           cap$family, cap$trend_type, cap$n_series,
                           if (cap$is_mv) " / mv" else "",
                           if (cap$is_factor) " / factor" else "",
                           if (cap$is_closure) " / closure-unit" else "")
  current_source <<- fits$source[i]
  cat("\n==========================================================\n")
  cat(nm, " -- ", reached[[nm]], " [", current_source, "]\n", sep = "")
  cat("==========================================================\n")

  for (gname in names(GROUPS)) {
    if (!is.null(only_groups) && !(gname %in% only_groups)) next
    tryCatch(GROUPS[[gname]](nm, obj, cap),
             error = function(e) {
               results$rows[[length(results$rows) + 1L]] <<- list(
                 fixture = nm, group = gname, label = "<group aborted>",
                 status = "ERR", detail = squash(conditionMessage(e)),
                 warnings = "", secs = NA_real_
               )
               cat("  ERR ", gname, " <group aborted> ",
                   conditionMessage(e), "\n")
             })
  }
  rm(obj); invisible(gc(verbose = FALSE))
}

# -- Output ------------------------------------------------------------

res <- do.call(rbind, lapply(results$rows, function(r) {
  as.data.frame(r, stringsAsFactors = FALSE)
}))

cat("\n\n##########################################################\n")
cat("SWEEP SUMMARY\n")
cat("##########################################################\n\n")
cat("Fixtures swept :", length(reached), "\n")
cat("Fixtures skipped:", length(skipped), "\n")
cat("Calls made     :", nrow(res), "\n")
cat("Failures       :", sum(res$status == "ERR"), "\n")
cat("Over budget    :", sum(res$status == "SLOW"), "\n")
cat("With warnings  :", sum(nzchar(res$warnings)), "\n\n")

# A failure on a maintained fixture is evidence about the code. A
# failure on a cache is evidence about the cache until the cache is
# rebuilt, so the two are never added together.
if (any(res$source == "cache")) {
  cat("--- failures by provenance ---\n")
  for (src in c("fixture", "cache")) {
    sub <- res[res$source == src, , drop = FALSE]
    if (!nrow(sub)) next
    cat(sprintf("  %-8s %4d calls, %3d failures, %2d over budget\n",
                src, nrow(sub), sum(sub$status == "ERR"),
                sum(sub$status == "SLOW")))
  }
  cat("\n")
}

violations <- res[res$group == "invariants" &
                    grepl("^VIOLATED", res$detail), , drop = FALSE]
cat("--- invariants ---\n")
cat(sprintf("  %d checked, %d violated\n",
            sum(res$group == "invariants"), nrow(violations)))
if (nrow(violations)) {
  for (i in seq_len(nrow(violations))) {
    cat(sprintf("  %-28s %-40s %s\n", violations$fixture[i],
                violations$label[i], substr(violations$detail[i], 1L, 80L)))
  }
}
cat("\n")

if (length(skipped)) {
  cat("--- skipped fixtures ---\n")
  for (n in names(skipped)) cat(sprintf("  %-40s %s\n", n, skipped[[n]]))
  cat("\n")
}

fails <- res[res$status == "ERR" & res$source == "fixture", , drop = FALSE]
if (nrow(fails)) cat("--- failures on maintained fixtures ---\n")
if (nrow(fails)) {
  cat("--- failures by surface ---\n")
  tb <- sort(table(paste(fails$group, fails$label, sep = " :: ")),
             decreasing = TRUE)
  for (k in names(tb)) cat(sprintf("  %3d  %s\n", tb[[k]], k))
  cat("\n--- distinct failure messages ---\n")
  msgs <- sort(table(fails$detail), decreasing = TRUE)
  for (k in names(msgs)) {
    cat(sprintf("  %3d  %s\n", msgs[[k]], substr(k, 1L, 160L)))
  }
}

warned <- res[nzchar(res$warnings), , drop = FALSE]
if (nrow(warned)) {
  cat("\n--- distinct warnings ---\n")
  wt <- sort(table(warned$warnings), decreasing = TRUE)
  for (k in names(wt)) {
    cat(sprintf("  %3d  %s\n", wt[[k]], substr(k, 1L, 160L)))
  }
}

cat("\n--- coverage reached ---\n")
for (n in names(reached)) cat(sprintf("  %-40s %s\n", n, reached[[n]]))

slow <- res[res$status == "SLOW", , drop = FALSE]
if (nrow(slow)) {
  cat("\n--- calls over the ", call_budget, "s budget ---\n", sep = "")
  for (i in seq_len(nrow(slow))) {
    cat(sprintf("  %-28s %s :: %s\n", slow$fixture[i], slow$group[i],
                slow$label[i]))
  }
}

cat("\nResults written to", out_path, "\n")
