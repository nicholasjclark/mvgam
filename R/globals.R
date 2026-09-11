# Column names that reach the code through data masking:
# dplyr and data.table expressions, ggplot2 aesthetics, and the
# posterior-summary helpers. Declared so `R CMD check` does not
# report them as undefined globals. One list, so a name added
# for one surface is visible to every other.
#' @noRd
utils::globalVariables(c(
  ".", ".data", ".fitted", ".resid", ".resid.se", ".se.fit",
  "Series", "Var1", "Var2", "analysis", "binomial", "colour",
  "contribution", "correlation", "current", "data", "data_test",
  "direction", "dis_time", "draw", "effect", "effect1__", "elpds",
  "estimate__", "ev", "eval_horizon", "eval_timepoints", "evd",
  "fevdQ50", "fevd_Qlower", "fevd_Qupper", "gam", "gr", "group",
  "hi", "horizon", "imp_resp", "in_interval", "index", "k", "label",
  "lambda", "level", "lo", "lower", "lower__", "lv", "lw", "matches",
  "mean_evd", "med", "mgcv_model", "name", "needed", "object", "obs",
  "outcome", "param_name", "parameter", "pareto_ks", "pred_Qlower",
  "pred_Qupper", "pred_median", "preds", "q025", "q100", "q250",
  "q750", "q900", "q975", "resids", "resp_var", "row_number",
  "rowid", "sample__", "season", "series", "shock", "subgr",
  "target", "term", "threshold", "total", "trait_name", "trend",
  "trend_model", "trend_y", "trial", "unit", "upper", "upper__",
  "value", "weight", "x", "y", "year"
))
