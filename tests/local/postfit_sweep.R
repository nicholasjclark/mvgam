# Post-fit surface sweep across every cached model fit.
#
# Not a test file. Invoke manually with, for example:
#   Rscript tests/local/postfit_sweep.R
#   Rscript tests/local/postfit_sweep.R --fixtures=beta_ar1,mv_gauss
#   Rscript tests/local/postfit_sweep.R --groups=ppcheck,marginaleffects
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

FIXTURE_DIR <- "tests/local/fixtures"
PKGDOWN_DIRS <- Sys.glob("pkgdown/*_cache")

# -- Result collection -------------------------------------------------

results <- new.env(parent = emptyenv())
results$rows <- list()

# Record one call. `expr` is evaluated lazily so a failure is captured
# rather than aborting the sweep. `shape` summarises the returned
# object so a silently wrong dimension is visible in the log.
run_call <- function(fixture, group, label, expr, shape = shape_of) {
  if (!is.null(only_groups) && !(group %in% only_groups)) return(invisible(NULL))
  warns <- character(0)
  t0 <- Sys.time()
  val <- withCallingHandlers(
    tryCatch(suppressMessages(force(expr)),
             error = function(e) structure(conditionMessage(e),
                                           class = "sweep_error")),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  failed <- inherits(val, "sweep_error")
  row <- list(
    fixture = fixture,
    group = group,
    label = label,
    status = if (failed) "ERR" else "OK",
    detail = if (failed) squash(as.character(val)) else squash(shape(val)),
    warnings = squash(paste(unique(warns), collapse = " | ")),
    secs = secs
  )
  results$rows[[length(results$rows) + 1L]] <- row
  cat(sprintf("  %-3s %-22s %-46s %s\n", row$status, group, label,
              substr(row$detail, 1L, 70L)))
  invisible(val)
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

  for (d in PKGDOWN_DIRS) {
    for (p in Sys.glob(file.path(d, "*.rds"))) {
      paths <- c(paths, p)
      names_ <- c(names_,
                  paste0(sub("_cache$", "", basename(d)), ":",
                         tools::file_path_sans_ext(basename(p))))
    }
  }

  data.frame(name = names_, path = paths, stringsAsFactors = FALSE)
}

# What a given fit can legally be asked to do. Every group below gates
# on these rather than on tryCatch, so a surface that is genuinely
# inapplicable is skipped instead of logged as a failure.
probe <- function(fit) {
  fam <- tryCatch(fit$family$family, error = function(e) NA_character_)
  si <- fit$series_info
  tm <- fit$trend_metadata
  dat <- fit$data %||% fit$obs_data
  resp <- fit$response_names

  # `series_info$is_multivariate` also fires for a binomial fit,
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
  run_call(nm, "summary", "summary(include_states=TRUE)",
           summary(fit, include_states = TRUE))
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
    run_call(nm, "structure", "irf()", irf(fit, h = 5L))
    run_call(nm, "structure", "fevd()", fevd(fit, h = 5L))
    run_call(nm, "structure", "posterior_transition_matrix()",
             posterior_transition_matrix(fit))
  }
  if (cap$is_closure) {
    run_call(nm, "structure", "latent_N_saturation()",
             latent_N_saturation(fit))
  }
}

# -- Driver ------------------------------------------------------------

GROUPS <- list(
  summary = group_summary,
  predict = group_predict,
  temporal = group_temporal,
  marginaleffects = group_marginaleffects,
  ppcheck = group_ppcheck,
  criticism = group_criticism,
  draws = group_draws,
  plot = group_plot,
  structure = group_structure
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
  cat("\n==========================================================\n")
  cat(nm, " -- ", reached[[nm]], "\n", sep = "")
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

utils::write.table(res, out_path, sep = "\t", row.names = FALSE,
                   quote = TRUE)

cat("\n\n##########################################################\n")
cat("SWEEP SUMMARY\n")
cat("##########################################################\n\n")
cat("Fixtures swept :", length(reached), "\n")
cat("Fixtures skipped:", length(skipped), "\n")
cat("Calls made     :", nrow(res), "\n")
cat("Failures       :", sum(res$status == "ERR"), "\n")
cat("With warnings  :", sum(nzchar(res$warnings)), "\n\n")

if (length(skipped)) {
  cat("--- skipped fixtures ---\n")
  for (n in names(skipped)) cat(sprintf("  %-40s %s\n", n, skipped[[n]]))
  cat("\n")
}

fails <- res[res$status == "ERR", , drop = FALSE]
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

cat("\nResults written to", out_path, "\n")
