
# ---- mvgam_ranef_aliases gate --------------------------------------

test_that("mvgam_ranef_aliases returns empty for fits without REs", {
  set.seed(11L)
  df <- data.frame(y = rnorm(20L), x = rnorm(20L))
  brms_form <- brms::bf(y ~ 1 + x)
  stub <- ranef_stub(brms_form, df)
  expect_identical(mvgam_ranef_aliases(stub), character(0L))
})

# ---- ranef.mvgam / VarCorr.mvgam dispatch + shape -----------------

test_that("ranef.mvgam matches brms::ranef.brmsfit signature", {
  expected <- names(formals(getS3method("ranef", "brmsfit")))
  actual <- names(formals(getS3method("ranef", "mvgam")))
  expect_identical(actual, expected)
})

test_that("VarCorr.mvgam matches brms::VarCorr.brmsfit signature", {
  expected <- names(formals(getS3method("VarCorr", "brmsfit")))
  actual <- names(formals(getS3method("VarCorr", "mvgam")))
  expect_identical(actual, expected)
})

test_that("ranef.mvgam names what to do on a fit with no REs", {
  # A stub without an observation model has no group-level table.
  stub <- structure(
    list(standata = list(N = 10L), formula = brms::bf(y ~ 1),
         data = data.frame(y = rnorm(10)),
         family = brms::brmsfamily("gaussian")),
    class = "mvgam"
  )
  expect_error(ranef.mvgam(stub), "group-level")
  expect_error(VarCorr.mvgam(stub), "group-level")
})

test_that("assemble_cor_array unpacks the upper triangle column by column", {
  # Pairs (1,2), (1,3), (2,3) for two draws
  cor_mat <- matrix(c(0.5, 0.4, 0.3, 0.6, 0.5, 0.4), nrow = 2,
                    byrow = TRUE)
  arr <- assemble_cor_array(cor_mat, size = 3L,
                            coef_names = c("a", "b", "c"))
  first <- matrix(c(1, 0.5, 0.4, 0.5, 1, 0.3, 0.4, 0.3, 1), 3L,
                  dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  expect_identical(arr[1L, , ], first)
  expect_identical(arr[2L, 1L, 2L], 0.6)
})

test_that("assemble_cov_array yields D R D per draw", {
  sd_mat <- matrix(c(2, 3), nrow = 1, dimnames = list(NULL, c("a", "b")))
  cor_arr <- assemble_cor_array(
    matrix(0.5, nrow = 1), size = 2L, coef_names = c("a", "b")
  )
  cov_arr <- assemble_cov_array(sd_mat, cor_arr, c("a", "b"))
  expect_identical(
    cov_arr[1L, , ],
    matrix(c(4, 3, 3, 9), 2L, dimnames = list(c("a", "b"), c("a", "b")))
  )
})

# ---- both sides of the group-level block --------------------------

test_that("a trend-side random effect is named, not numbered", {
  # The observation and trend blocks reach Stan under the same
  # positional names, the trend's carrying `_trend` after the id.
  # Only the observation side was aliased, so a random effect
  # declared in `trend_formula` was reported as `sd_1_trend[1]`
  # while the same effect on the observation side was reported as
  # `sd_grp__Intercept`. Nothing told a reader why they differed.
  fit <- structure(
    list(
      obs_model = structure(list(ranef = data.frame()), class = "brmsfit"),
      trend_model = structure(
        list(ranef = structure(
          data.frame(
            id = 1L, group = "grp", coef = "Intercept",
            cor = TRUE, nlpar = "", dpar = "", resp = "",
            stringsAsFactors = FALSE
          ),
          levels = list(grp = c("a", "b"))
        )),
        class = "brmsfit"
      )
    ),
    class = "mvgam"
  )
  map <- mvgam_ranef_aliases(fit)

  # The alias carries the suffix; the positional name it replaces
  # carries `_trend` after the id, which is where brms puts it.
  expect_true("sd_grp__Intercept_trend" %in% names(map))
  expect_identical(unname(map["sd_grp__Intercept_trend"]),
                     "sd_1_trend[1]")

  # `posterior` reads a name as `variable[element]`, so the suffix
  # belongs on the variable and the index stays last. Written the
  # other way round, `r_grp[a,Intercept]_trend` would never select
  # as an element of `r_grp_trend`.
  expect_true("r_grp_trend[a,Intercept]" %in% names(map))
  expect_false(any(grepl("\\]_trend$", names(map))))
})

# ---- apply_mvgam_aliases composition --------------------------

test_that("apply_mvgam_aliases handles a combined beta + ranef map", {
  vars <- c("b[1]", "r_1[1,1]", "sd_1[1]", "cor_1[1]", "lp__")
  alias_map <- c(
    "b_x"                   = "b[1]",
    "r_grp[a,Intercept]"    = "r_1[1,1]",
    "sd_grp__Intercept"     = "sd_1[1]",
    "cor_grp__Intercept__x" = "cor_1[1]"
  )
  out <- apply_mvgam_aliases(vars, alias_map)
  expect_identical(
    out,
    c(
      "b_x", "r_grp[a,Intercept]", "sd_grp__Intercept",
      "cor_grp__Intercept__x", "lp__"
    )
  )
})
