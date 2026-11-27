# The tests of test-trend-registry.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()
# `trend_registry` is package state, and a testthat worker runs
# several files in one process. The tests below clear it. A failure
# between a clear and its re-registration would hand every later file
# in that worker an empty registry. This restores the core set when
# the file ends.
withr::defer(
  {
    rm(list = ls(envir = trend_registry), envir = trend_registry)
    register_core_trends()
  },
  teardown_env()
)


test_that("a reported prior carries the support it is sampled on", {
  # The bounds a parameter is declared with sit in the same program
  # as its prior. Reading one and not the other reported
  # `normal(0, 0.5)` unbounded for an autoregressive coefficient held
  # inside (-1, 1), and for a continuous-time one held inside
  # (0.001, 0.999), so half the reported mass lay outside the support
  # the model samples on.
  dat <- data.frame(
    time = 1:40, series = factor(rep("s1", 40)),
    y = rpois(40, 5)
  )
  declared <- function(sc, par) {
    line <- grep(paste0("[ ]", par, ";"), strsplit(sc, "\n")[[1]],
                 value = TRUE)
    c(lb = sub(".*lower[ ]*=[ ]*([^,>]+).*", "\\1", line[1]),
      ub = if (grepl("upper", line[1])) {
        sub(".*upper[ ]*=[ ]*([^,>]+).*", "\\1", line[1])
      } else "")
  }
  for (tr in list(~ AR(p = 1), ~ CAR())) {
    fit <- mvgam(y ~ 1, trend_formula = tr, data = dat,
                 family = poisson(), run_model = FALSE, silent = 2,
                 validate = FALSE)
    tab <- as.data.frame(prior_summary(fit))
    sc <- as.character(stancode(fit))
    for (par in c("ar1_trend", "sigma_trend")) {
      row <- tab[tab$class == par, , drop = FALSE]
      expect_identical(nrow(row), 1L)
      d <- declared(sc, par)
      expect_identical(row$lb[1], unname(d["lb"]))
      expect_identical(row$ub[1], unname(d["ub"]))
    }
  }
})
