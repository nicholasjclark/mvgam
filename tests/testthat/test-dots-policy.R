# Which methods refuse an argument nothing reads, checked from the
# package itself rather than from a list someone maintains.
#
# `tests/local/test-dots-refusal.R` calls the methods on a fitted
# model and requires the refusal. It cannot reach the result classes:
# building an `mvgam_kfold` needs refits and an `mvgam_irf` needs a
# VAR fit, so those would cost a model each. What can be checked
# without a posterior is that the guard is present, and that is what
# keeps a method added later from quietly reopening the hole.
#
# A method outside the rule is named below with the reason. The two
# reasons are that its `...` is forwarded to a callee whose formals
# are the contract, or that it is a `print` method, where R's own
# convention is to pass anything on to the formatter.

forwards_or_prints <- c(
  # `print` methods, by R's convention.
  "print.mvgam", "print.mvgam_active_factors", "print.mvgam_data",
  "print.mvgam_forecast", "print.mvgam_formula", "print.mvgam_kfold",
  "print.mvgam_latent_state", "print.mvgam_lfo",
  "print.mvgam_methods_md", "print.mvgam_pooled_summary",
  "print.mvgam_ppc_fit_stat", "print.mvgam_residcor",
  "print.mvgam_shared_variation",
  "print.mvgam_sim_closure_unit_summary", "print.mvgam_sim_summary",
  "print.mvgam_summary", "print.mvgam_trend", "print.mvgam_var_matrix",
  "print.mvgam_var_matrix_list", "print.mvgamstancode",
  "print.mvgam_conditional_effects", "print.mvgam_conditional_smooths",
  "print.mvgam_sim", "print.mvgam_var_surface_summary",
  "print.how_to_cite",
  # These hand `...` to a callee whose formals are the contract.
  "plot.mvgam", "plot.mvgam_conditional_effects",
  "summary.mvgam_pooled"
)


test_that("every drawn or reported surface refuses what it cannot read", {
  ns <- asNamespace("mvgam")
  named <- grep("^(plot|summary)\\.(mvgam|jsdgam)", ls(ns, all.names = TRUE),
                value = TRUE)
  takes_dots <- Filter(function(nm) {
    "..." %in% names(formals(get(nm, envir = ns)))
  }, named)
  # The rule is stated over what the package registers, so a method
  # added later arrives here without anyone remembering to add it.
  expect_gt(length(takes_dots), 30L)

  guards <- vapply(takes_dots, function(nm) {
    body_text <- paste(deparse(body(get(nm, envir = ns))), collapse = " ")
    grepl("check_dots_empty|UseMethod|NextMethod", body_text)
  }, logical(1L))
  unguarded <- sort(setdiff(names(guards)[!guards], forwards_or_prints))
  expect_identical(unguarded, character(0L))

  # A name on the exemption list that no longer exists is a reason
  # nothing holds, which is how such a list goes stale.
  expect_setequal(
    intersect(forwards_or_prints, ls(ns, all.names = TRUE)),
    forwards_or_prints
  )
})
