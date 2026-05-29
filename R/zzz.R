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
      "A more detailed introduction to the package is available through vignette('mvgam_overview'). ",
      "Full brms compatibility is now enabled."
    )
  )
  
  packageStartupMessage(startup_msg)
}

#' Package loading hook
#' 
#' Called when mvgam namespace is loaded
#' @param libname Library name  
#' @param pkgname Package name
.onLoad <- function(libname, pkgname) {
  # Initialize trend registry with core trend types
  ensure_registry_initialized()

  # marginaleffects ships a per-class `type_dictionary` data.frame
  # that gates the `type` argument upstream of `get_predict()`. The
  # pre-existing mvgam row carries master's vocabulary
  # (response, link, expected, detection, latent_N); append the
  # brms-convention "prediction" so users can request
  # posterior_predict() draws via marginaleffects. Use rbind so any
  # future marginaleffects-side updates to the mvgam row remain in
  # effect.
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    me_ns <- asNamespace("marginaleffects")
    if (exists("type_dictionary", envir = me_ns, inherits = FALSE)) {
      td <- get("type_dictionary", envir = me_ns)
      if (!any(td$class == "mvgam" & td$type == "prediction")) {
        binding_locked <- bindingIsLocked("type_dictionary", me_ns)
        if (binding_locked) unlockBinding("type_dictionary", me_ns)
        try(
          assign(
            "type_dictionary",
            rbind(td, data.frame(class = "mvgam", type = "prediction")),
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
        mvgam_allowed_dots <- c(
          "process_error", "draw_ids", "ndraws", "re_formula",
          "allow_new_levels", "sample_new_levels", "resp",
          "incl_latent_state", "incl_autocor", "summary"
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
