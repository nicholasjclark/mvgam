# CI-safe tests for the random-effect alias helpers
# (mvgam_ranef_aliases / apply_mvgam_beta_aliases path) and the
# ranef.mvgam / VarCorr.mvgam S3 surface. Numerical concordance
# against brms lives in tests/local/. Here we lock in the gate
# (no-RE fits return empty maps cheaply), the map shape on a
# 2-coef correlated RE, and S3 dispatch + signature parity with
# brms.


make_ranef_stub <- function(group = "grp",
                            levels = c("a", "b", "c", "d", "e", "f"),
                            include_x = TRUE) {
  set.seed(7L)
  n_obs <- 40L
  df <- data.frame(
    y = rnorm(n_obs),
    x = rnorm(n_obs),
    grp = factor(sample(levels, n_obs, replace = TRUE), levels = levels)
  )
  if (!identical(group, "grp")) {
    names(df)[3L] <- group
  }
  form <- if (include_x) {
    stats::as.formula(paste0("y ~ 1 + x + (x | ", group, ")"))
  } else {
    stats::as.formula(paste0("y ~ 1 + (1 | ", group, ")"))
  }
  # Build a brms-shaped formula and standata to satisfy the helper's
  # cheap gate (`M_<id>` keys) without requiring a real fit.
  brms_form <- brms::bf(form)
  sd_ <- brms::standata(
    brms_form, data = df, family = brms::brmsfamily("gaussian")
  )
  structure(
    list(
      formula = brms_form,
      data = df,
      family = brms::brmsfamily("gaussian"),
      standata = as.list(sd_),
      stancode = "// stub",
      backend = "cmdstanr",
      algorithm = "sampling"
    ),
    class = "mvgam"
  )
}


# ---- mvgam_ranef_aliases gate --------------------------------------

test_that("mvgam_ranef_aliases returns empty for fits without REs", {
  set.seed(11L)
  df <- data.frame(y = rnorm(20L), x = rnorm(20L))
  brms_form <- brms::bf(y ~ 1 + x)
  sd_ <- brms::standata(
    brms_form, data = df, family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(
      formula = brms_form,
      data = df,
      family = brms::brmsfamily("gaussian"),
      standata = as.list(sd_)
    ),
    class = "mvgam"
  )
  expect_identical(mvgam_ranef_aliases(stub), character(0L))
})


# ---- mvgam_ranef_aliases map shape ---------------------------------

test_that("mvgam_ranef_aliases produces r_/sd_/cor_ aliases for (x|grp)", {
  stub <- make_ranef_stub()
  map <- mvgam_ranef_aliases(stub)
  expect_true(length(map) > 0L)
  # Map shape mirrors mvgam_beta_aliases: names are aliases,
  # values are positional Stan parameter names.
  expect_identical(class(map), "character")
  expect_true(!is.null(names(map)))
  # r_<group>[<level>,<coef>] aliases for 6 levels x 2 coefs.
  r_aliases <- grep("^r_grp\\[", names(map), value = TRUE)
  expect_length(r_aliases, 12L)
  expect_true("r_grp[a,Intercept]" %in% names(map))
  expect_true("r_grp[f,x]" %in% names(map))
  # sd_<group>__<coef> aliases for each coef.
  expect_true("sd_grp__Intercept" %in% names(map))
  expect_true("sd_grp__x" %in% names(map))
  # cor_<group>__<coef1>__<coef2> alias for the single off-diagonal.
  expect_true("cor_grp__Intercept__x" %in% names(map))
  # Positional values follow the brms internal convention.
  expect_identical(map[["r_grp[a,Intercept]"]], "r_1[1,1]")
  expect_identical(map[["r_grp[f,x]"]], "r_1[6,2]")
  expect_identical(map[["sd_grp__Intercept"]], "sd_1[1]")
  expect_identical(map[["sd_grp__x"]], "sd_1[2]")
  expect_identical(map[["cor_grp__Intercept__x"]], "cor_1[1]")
})


test_that("mvgam_ranef_aliases skips cor_ block for intercept-only REs", {
  stub <- make_ranef_stub(include_x = FALSE)
  map <- mvgam_ranef_aliases(stub)
  expect_false(any(grepl("^cor_", names(map))))
  # Intercept-only: one coef per level, no correlation.
  expect_true("r_grp[a,Intercept]" %in% names(map))
  expect_true("sd_grp__Intercept" %in% names(map))
  expect_length(grep("^sd_", names(map)), 1L)
  # M = 1: brms emits the per-coef vector `r_<id>_1[<level_idx>]`,
  # not the matrix form. Lock in the positional value.
  expect_identical(map[["r_grp[a,Intercept]"]], "r_1_1[1]")
  expect_identical(map[["r_grp[f,Intercept]"]], "r_1_1[6]")
})


# ---- Edge cases ---------------------------------------------------

test_that("mvgam_ranef_aliases handles uncorrelated multi-coef REs ((x||g))", {
  set.seed(13L); n_obs <- 40L
  df <- data.frame(
    y = rnorm(n_obs),
    x = rnorm(n_obs),
    grp = factor(sample(letters[1:4], n_obs, replace = TRUE),
                 levels = letters[1:4])
  )
  brms_form <- brms::bf(y ~ 1 + x + (1 + x || grp))
  sd_ <- brms::standata(
    brms_form, data = df, family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms_form, data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  map <- mvgam_ranef_aliases(stub)
  # No correlation parameters with `||`.
  expect_false(any(grepl("^cor_", names(map))))
  # Per-coef vector positional names. brms keeps id = 1 with
  # cor = FALSE for `(1 + x || g)`, so coefs share the same id
  # and the names are `r_<id>_<coef_idx>[<level>]`.
  expect_identical(map[["r_grp[a,Intercept]"]], "r_1_1[1]")
  expect_identical(map[["r_grp[d,x]"]], "r_1_2[4]")
})


test_that("mvgam_ranef_aliases handles multiple grouping factors", {
  set.seed(17L); n_obs <- 40L
  df <- data.frame(
    y = rnorm(n_obs),
    grp = factor(sample(letters[1:3], n_obs, replace = TRUE),
                 levels = letters[1:3]),
    site = factor(sample(LETTERS[1:4], n_obs, replace = TRUE),
                  levels = LETTERS[1:4])
  )
  brms_form <- brms::bf(y ~ 1 + (1 | grp) + (1 | site))
  sd_ <- brms::standata(
    brms_form, data = df, family = brms::brmsfamily("gaussian")
  )
  stub <- structure(
    list(formula = brms_form, data = df,
         family = brms::brmsfamily("gaussian"),
         standata = as.list(sd_)),
    class = "mvgam"
  )
  map <- mvgam_ranef_aliases(stub)
  # 3 levels of grp + 4 levels of site = 7 r_ aliases; 2 sd_ aliases.
  expect_length(grep("^r_(grp|site)\\[", names(map)), 7L)
  expect_true("sd_grp__Intercept" %in% names(map))
  expect_true("sd_site__Intercept" %in% names(map))
  # No cor blocks (intercept-only).
  expect_false(any(grepl("^cor_", names(map))))
})


test_that("mvgam_ranef_aliases verifies cor index order for M = 3 and M = 4", {
  build_stub <- function(extra_coefs) {
    set.seed(21L); N <- 80L
    df <- data.frame(y = rnorm(N),
                     grp = factor(sample(letters[1:5], N, replace = TRUE),
                                  levels = letters[1:5]))
    for (cn in extra_coefs) df[[cn]] <- rnorm(N)
    rhs <- paste(extra_coefs, collapse = " + ")
    re_rhs <- paste(c("1", extra_coefs), collapse = " + ")
    form <- stats::as.formula(
      sprintf("y ~ %s + (%s | grp)", rhs, re_rhs)
    )
    brms_form <- brms::bf(form)
    sd_ <- brms::standata(brms_form, data = df,
                          family = brms::brmsfamily("gaussian"))
    structure(
      list(formula = brms_form, data = df,
           family = brms::brmsfamily("gaussian"),
           standata = as.list(sd_)),
      class = "mvgam"
    )
  }
  # M = 3: brms's column-major upper-triangle order produces
  # cor_1[1] = (Intercept,x), cor_1[2] = (Intercept,z),
  # cor_1[3] = (x,z).
  m3 <- mvgam_ranef_aliases(build_stub(c("x", "z")))
  expect_identical(m3[["cor_grp__Intercept__x"]], "cor_1[1]")
  expect_identical(m3[["cor_grp__Intercept__z"]], "cor_1[2]")
  expect_identical(m3[["cor_grp__x__z"]],         "cor_1[3]")
  # M = 4: cor index walks (1,2),(1,3),(2,3),(1,4),(2,4),(3,4).
  # Differs from naive `utils::combn(M, 2)` row-major ordering;
  # this regression test locks in the brms convention.
  m4 <- mvgam_ranef_aliases(build_stub(c("x", "z", "w")))
  expect_identical(m4[["cor_grp__Intercept__x"]], "cor_1[1]")
  expect_identical(m4[["cor_grp__Intercept__z"]], "cor_1[2]")
  expect_identical(m4[["cor_grp__x__z"]],         "cor_1[3]")
  expect_identical(m4[["cor_grp__Intercept__w"]], "cor_1[4]")
  expect_identical(m4[["cor_grp__x__w"]],         "cor_1[5]")
  expect_identical(m4[["cor_grp__z__w"]],         "cor_1[6]")
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


test_that("ranef.mvgam errors on a no-RE fit with an actionable hint", {
  # Stub: no `M_<id>` keys, so the gate fires and we never call
  # brm(empty=TRUE).
  stub <- structure(
    list(standata = list(N = 10L), formula = brms::bf(y ~ 1),
         data = data.frame(y = rnorm(10)),
         family = brms::brmsfamily("gaussian")),
    class = "mvgam"
  )
  # cli soft-wraps the message body; match on a stable single
  # token rather than a phrase that may straddle a wrap.
  expect_error(ranef.mvgam(stub), "random effects")
  expect_error(VarCorr.mvgam(stub), "random effects")
})


test_that("assemble_cor_array builds the correct symmetric structure", {
  # 2 draws, M = 3, hand-crafted correlation packing
  # (j=1,k=2)=0.5, (j=1,k=3)=0.4, (j=2,k=3)=0.3 in column-major
  # upper-triangle order.
  cor_mat <- matrix(c(0.5, 0.4, 0.3, 0.6, 0.5, 0.4), nrow = 2,
                    byrow = TRUE)
  arr <- assemble_cor_array(cor_mat, size = 3L,
                            coef_names = c("a", "b", "c"))
  expect_identical(dim(arr), c(2L, 3L, 3L))
  # Diagonal is 1.
  expect_identical(arr[1L, 1L, 1L], 1)
  expect_identical(arr[1L, 2L, 2L], 1)
  expect_identical(arr[1L, 3L, 3L], 1)
  # Symmetric and packed in brms order.
  expect_identical(arr[1L, 1L, 2L], arr[1L, 2L, 1L])
  expect_identical(arr[1L, 1L, 2L], 0.5)
  expect_identical(arr[1L, 1L, 3L], 0.4)
  expect_identical(arr[1L, 2L, 3L], 0.3)
  # Second draw differs.
  expect_identical(arr[2L, 1L, 2L], 0.6)
})


test_that("assemble_cov_array yields D R D per draw", {
  # 1 draw, M = 2 with sd = (2, 3) and cor = (1, 0.5; 0.5, 1).
  # Expected cov: (4, 3; 3, 9).
  sd_mat <- matrix(c(2, 3), nrow = 1)
  colnames(sd_mat) <- c("a", "b")
  cor_arr <- assemble_cor_array(
    matrix(0.5, nrow = 1), size = 2L, coef_names = c("a", "b")
  )
  cov_arr <- assemble_cov_array(sd_mat, cor_arr, c("a", "b"))
  expect_identical(dim(cov_arr), c(1L, 2L, 2L))
  expect_identical(cov_arr[1L, 1L, 1L], 4)
  expect_identical(cov_arr[1L, 2L, 2L], 9)
  expect_identical(cov_arr[1L, 1L, 2L], 3)
  expect_identical(cov_arr[1L, 2L, 1L], 3)
})


test_that("mvgam_ranef_aliases gate excludes trend-only standata blocks", {
  # A standata that has only `M_<id>_trend` keys (no obs RE block)
  # must skip the brm(empty=TRUE) call entirely and return char(0).
  stub <- structure(
    list(
      standata = list(
        N_trend = 30L, M_1_trend = 1L, N_1_trend = 5L,
        Z_1_1_trend = matrix(1, 30, 1), J_1_trend = rep(1:5, 6)
      ),
      formula = brms::bf(y ~ 1),
      data = data.frame(y = rnorm(5)),
      family = brms::brmsfamily("gaussian")
    ),
    class = "mvgam"
  )
  expect_identical(mvgam_ranef_aliases(stub), character(0L))
})


# ---- apply_mvgam_beta_aliases composition --------------------------

test_that("apply_mvgam_beta_aliases handles a combined beta + ranef map", {
  vars <- c("b[1]", "r_1[1,1]", "sd_1[1]", "cor_1[1]", "lp__")
  alias_map <- c(
    "b_x"                   = "b[1]",
    "r_grp[a,Intercept]"    = "r_1[1,1]",
    "sd_grp__Intercept"     = "sd_1[1]",
    "cor_grp__Intercept__x" = "cor_1[1]"
  )
  out <- apply_mvgam_beta_aliases(vars, alias_map)
  expect_identical(
    out,
    c(
      "b_x", "r_grp[a,Intercept]", "sd_grp__Intercept",
      "cor_grp__Intercept__x", "lp__"
    )
  )
})
