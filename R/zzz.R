#' Package startup and loading functions
#'
#' Custom startup message and dependency loading following tidyverse patterns
#' @importFrom utils packageVersion
#' @name startup
#' @keywords internal
NULL

# Core dependencies that mvgam loads
core_deps <- c(
  "brms"
)

#' Check which core dependencies are not yet loaded
#' @noRd
core_unloaded <- function() {
  search <- paste0("package:", core_deps)
  core_deps[!search %in% search()]
}

#' Quietly attach core dependencies without startup messages
#' @noRd
mvgam_attach <- function() {
  to_load <- core_unloaded()
  
  # For dependencies, attach them quietly using attachNamespace
  # This prevents startup messages while making functions available
  for (pkg in to_load) {
    if (pkg %in% loadedNamespaces()) {
      # Namespace already loaded, just attach
      suppressWarnings(attachNamespace(pkg))
    } else {
      # Load and attach namespace quietly
      suppressPackageStartupMessages({
        requireNamespace(pkg, quietly = TRUE)
        attachNamespace(pkg)
      })
    }
  }
  
  invisible(to_load)
}


#' Package attachment hook
#'
#' Called when mvgam is loaded via library() or require()
#' @param libname Library name
#' @param pkgname Package name
#' @noRd
.onAttach <- function(libname, pkgname) {
  # Set marginaleffects option
  options("marginaleffects_model_classes" = "mvgam")
  
  # Show mvgam startup message (brms is auto-loaded as dependency)
  version <- utils::packageVersion("mvgam")
  
  startup_msg <- insight::format_message(
    paste0(
      "Loading 'mvgam' (version ",
      version,
      "). Useful instructions can be found by typing help('mvgam'). ",
      "A more detailed introduction to the package is available through vignette('mvgam_overview')."
    )
  )
  
  packageStartupMessage(startup_msg)
}

#' Package loading hook
#'
#' Called when mvgam namespace is loaded
#' @param libname Library name
#' @param pkgname Package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Initialize trend registry with core trend types
  ensure_registry_initialized()

  # marginaleffects ships a per-class `type_dictionary` data.frame
  # that validates the `type` argument upstream of `get_predict()`.
  # The mvgam row that ships with marginaleffects predates the
  # predict.mvgam vocabulary standardisation and carries the stale
  # token `latent_N` instead of the modern `latent_state`. Append
  # the brms-convention "prediction" and the modern "latent_state"
  # so both pass the marginaleffects check; the per-family
  # availability check still runs inside predict.mvgam.
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    me_ns <- asNamespace("marginaleffects")
    if (exists("type_dictionary", envir = me_ns, inherits = FALSE)) {
      td <- get("type_dictionary", envir = me_ns)
      missing_types <- setdiff(
        c("prediction", "latent_state"),
        td$type[td$class == "mvgam"]
      )
      if (length(missing_types) > 0L) {
        binding_locked <- bindingIsLocked("type_dictionary", me_ns)
        if (binding_locked) unlockBinding("type_dictionary", me_ns)
        try(
          assign(
            "type_dictionary",
            rbind(td, data.frame(class = "mvgam", type = missing_types)),
            envir = me_ns
          ),
          silent = TRUE
        )
        if (binding_locked) {
          try(lockBinding("type_dictionary", me_ns), silent = TRUE)
        }
      }
    }

    # marginaleffects's `sanity_dots` warns when get_predict.mvgam
    # receives arguments not on its per-class whitelist (process_error,
    # re_formula, etc). The whitelist is a local variable inside the
    # function, so we wrap the original to short-circuit when the
    # model is mvgam and all `...` are in our known set. Falls back to
    # the original for any other class or for unknown args. Wrapped in
    # `try(silent)` so a marginaleffects-side change to `sanity_dots`
    # cannot block load.
    if (exists("sanity_dots", envir = me_ns, inherits = FALSE)) {
      original_sanity_dots <- get("sanity_dots", envir = me_ns)
      if (!isTRUE(attr(original_sanity_dots, "mvgam_patched"))) {
        # mvgam-specific args plus marginaleffects's own white_list
        # (modeldata, draw, conf.int, ... are inserted into `...` by
        # the predictions / slopes / comparisons pipeline and need to
        # pass through quietly). Inlined from
        # marginaleffects::sanity_dots so we don't risk it drifting
        # silently — the wrap intentionally over-accepts.
        mvgam_allowed_dots <- c(
          "process_error", "draw_ids", "ndraws", "re_formula",
          "allow_new_levels", "sample_new_levels", "resp",
          "incl_latent_state", "incl_autocor", "summary",
          "conf.int", "modeldata", "internal_call", "df",
          "transform", "comparison", "side", "delta", "null",
          "equivalence", "draw", "flag", "variables_grid", "at",
          "conf.level"
        )
        patched_sanity_dots <- function(model,
                                        calling_function = NULL, ...) {
          if (inherits(model, "mvgam")) {
            unknown <- setdiff(...names(), mvgam_allowed_dots)
            if (length(unknown) == 0L) {
              return(invisible(NULL))
            }
          }
          original_sanity_dots(
            model, calling_function = calling_function, ...
          )
        }
        attr(patched_sanity_dots, "mvgam_patched") <- TRUE

        binding_locked <- bindingIsLocked("sanity_dots", me_ns)
        if (binding_locked) unlockBinding("sanity_dots", me_ns)
        try(
          assign("sanity_dots", patched_sanity_dots, envir = me_ns),
          silent = TRUE
        )
        if (binding_locked) {
          try(lockBinding("sanity_dots", me_ns), silent = TRUE)
        }
      }
    }
  }

  invisible()
}
