# Column names that reach the code through data masking:
# dplyr and data.table expressions, ggplot2 aesthetics, and the
# posterior-summary helpers. Declared so `R CMD check` does not
# report them as undefined globals. One list, so a name added
# for one surface is visible to every other.
#' @noRd
utils::globalVariables(c(
  ".", ".data", ".fitted", ".resid", ".resid.se", ".se.fit",
  "Contribution", "Series", "Var1", "Var2", "add_cor", "add_ma",
  "add_nmix", "analysis", "assimilated", "binomial", "by_variable",
  "byvar", "c_dark", "colour", "contribution", "correlation", "current",
  "data", "data_test", "direction", "dis_time", "draw", "draw__",
  "drop_obs_intercept", "effect", "elpds", "estimate__", "ev",
  "eval_horizon", "eval_timepoints", "evd", "fevdQ50", "fevd_Qlower",
  "fevd_Qupper", "fevd_median", "file_name", "gam", "gp_details",
  "gp_terms", "gr", "group", "hi", "horizon", "imp_resp", "in_interval",
  "index", "index..time..index", "irf_Qlower", "irf_Qupper",
  "irf_median", "jags_path", "k", "label", "lambda", "last_assim",
  "level", "lo", "lower", "lower1", "lower2", "lower3", "lower4",
  "lower__", "lower_deriv", "lv", "lw", "matches", "maxt", "mean_evd",
  "med", "med_deriv", "mgcv_model", "mgcv_plottable", "mod", "mod_call",
  "mus", "name", "needed", "nmix_trendmap", "nsp", "object", "obs",
  "orig_formula", "orig_rows", "orig_weight", "outcome", "param_name",
  "parameter", "pareto_ks", "particles", "pred_Qlower", "pred_Qupper",
  "pred_median", "preds", "q025", "q100", "q250", "q750", "q900",
  "q975", "resids", "resp_var", "row_id", "row_num", "row_number",
  "rowid", "season", "series", "shock", "sim_hilbert_gp",
  "smooth_label", "smooth_num", "smooth_vals", "subgr",
  "sum_contribution", "target", "term", "threshold", "time.",
  "time_lag", "tot_subgrs", "total", "total_evd", "trait_name", "trend",
  "trend_model", "trend_series", "trend_test", "trend_y", "trends_test",
  "trial", "unit", "upper", "upper1", "upper2", "upper3", "upper4",
  "upper__", "upper_deriv", "use_var1", "use_var1cor", "value",
  "weight", "x", "xcols_drop", "y", "year", "yhigh", "ylow", "ymidhigh",
  "ymidlow", "yqhigh", "yqlow"
))
