
# ---- Dispatch + signature parity -----------------------------------

test_that("update.mvgam signature matches brms::update.brmsfit", {
  expected <- names(formals(getS3method("update", "brmsfit")))
  actual <- names(formals(getS3method("update", "mvgam")))
  expect_identical(actual, expected)
})

# ---- Error paths ----------------------------------------------------

test_that("update.mvgam rejects `data` in dots with a newdata hint", {
  stub <- make_update_stub()
  expect_error(
    update(stub, data = data.frame(y = 1, x = 1)),
    "newdata"
  )
})

test_that("update.mvgam rejects pooled multiple-imputation fits", {
  stub <- make_update_stub()
  class(stub) <- c("mvgam_pooled", class(stub))
  expect_error(
    update(stub),
    "pooled multiple-imputation"
  )
})

test_that("mvgam_update_call overrides data when newdata is supplied", {
  stub <- make_update_stub()
  nd <- stub$data[1:5, ]
  out <- mvgam_update_call(stub, formula. = NULL, newdata = nd, dots = list())
  expect_identical(out$data, nd)
})

test_that("mvgam_update_call lets the caller override the inherited prior", {
  # A refit reuses the fitted prior table by default. Its adaptive
  # Intercept and sigma priors keep their constants. The compiled model
  # is then reused. A `prior` passed to update() replaces the table
  # for that refit.
  stub <- make_update_stub()
  new_prior <- structure(
    data.frame(
      prior = "normal(0, 0.5)", class = "b",
      coef = "", group = "", resp = "", dpar = "",
      nlpar = "", lb = NA_character_, ub = NA_character_,
      source = "user", stringsAsFactors = FALSE
    ),
    class = c("brmsprior", "data.frame")
  )
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(prior = new_prior)
  )
  expect_identical(out$prior, new_prior)
})

test_that("mvgam_update_call lets `dots` override inherited slots", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(
      family = stats::poisson(),
      backend = "rstan",
      cores = 4L
    )
  )
  # family() captures an environment. The test compares the family name.
  # identical() would also compare that environment.
  expect_identical(out$family$family, "poisson")
  expect_identical(out$backend, "rstan")
  expect_identical(out$cores, 4L)
})

test_that("mvgam_update_call passes through unrelated dots", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(seed = 42L, silent = 2L)
  )
  expect_identical(out$seed, 42L)
  expect_identical(out$silent, 2L)
})

# ---- mvgam_normalise_stancode --------------------------------------

test_that("mvgam_normalise_stancode collapses trailing whitespace", {
  s1 <- "data { int N; }\n\n"
  s2 <- "data { int N; }"
  expect_identical(
    mvgam_normalise_stancode(s1),
    mvgam_normalise_stancode(s2)
  )
})

test_that("mvgam_normalise_stancode handles NULL input", {
  expect_identical(mvgam_normalise_stancode(NULL), character(0L))
})

test_that("mvgam_normalise_stancode keeps stancode without a header", {
  s <- "data { int N; }\nparameters { real mu; }"
  expect_identical(mvgam_normalise_stancode(s), trimws(s))
})

test_that("mvgam_normalise_stancode compares code alone", {
  # A comment anywhere, of either spelling, changes no model. A string
  # literal holding a comment marker is code. A change to code is a
  # different model.
  plain <- paste(
    "data { int N; }",
    "model {",
    '  if (N < 1) reject("N // must be positive");',
    "}",
    sep = "\n"
  )
  commented <- paste(
    "data { int N; }  // the count",
    "/* a block comment",
    "   over two lines */",
    "model {",
    "  // guard",
    '  if (N < 1) reject("N // must be positive");',
    "}",
    sep = "\n"
  )
  expect_identical(mvgam_normalise_stancode(plain),
                   mvgam_normalise_stancode(commented))
  expect_match(mvgam_normalise_stancode(plain), "N // must be positive",
               fixed = TRUE)
  expect_false(identical(
    mvgam_normalise_stancode(plain),
    mvgam_normalise_stancode(sub("N < 1", "N < 2", plain, fixed = TRUE))
  ))
})

test_that("restore_trend_call_env binds what the fit already carries", {
  # A formula keeps the expression and the environment it was written in.
  # It does not keep the values that the expression names. A formula
  # written inside a function, or loaded from an `.rds` file in a fresh
  # session, has an environment without the loading matrix the constructor
  # referred to. Without restoration, `update()` fails with
  # `object 'Z_user' not found` before it reaches `mvgam()`.
  trend_call <- ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  environment(trend_call) <- new.env(parent = emptyenv())
  Z <- matrix(c(1, 0, 0.8, 0.2, 0, 1, 0.3, 0.7), nrow = 4)
  out <- restore_trend_call_env(trend_call, list(fixed_Z = Z))
  expect_equal(get("Z_user", envir = environment(out)), Z)
  # The expression itself is left as the user wrote it.
  expect_equal(deparse(out[[2L]]), deparse(trend_call[[2L]]))
})

test_that("restore_trend_call_env covers n_lv as well as trend_map", {
  trend_call <- ~ AR(p = 1, n_lv = n_lv_lp)
  environment(trend_call) <- new.env(parent = emptyenv())
  out <- restore_trend_call_env(trend_call, list(n_lv = 3L))
  expect_equal(get("n_lv_lp", envir = environment(out)), 3L)
})

test_that("restore_trend_call_env leaves resolvable calls untouched", {
  # Nothing needs restoring when every name already resolves. A literal
  # argument names nothing at all.
  env <- new.env(parent = emptyenv())
  assign("Z_user", matrix(1, 2, 2), envir = env)
  trend_call <- ~ AR(p = 1, trend_map = Z_user)
  environment(trend_call) <- env
  out <- restore_trend_call_env(trend_call, list(fixed_Z = matrix(9, 2, 2)))
  expect_identical(environment(out), env)

  literal <- ~ AR(p = 1, cor = TRUE)
  environment(literal) <- env
  expect_identical(
    environment(restore_trend_call_env(literal, list(fixed_Z = NULL))),
    env
  )
})

test_that("restore_trend_call_env leaves a name the fit does not carry", {
  # An argument mvgam stores no resolved value for reaches `mvgam()`
  # and fails there against the user's own call. The rebuild does not
  # stop part-way.
  trend_call <- ~ AR(p = 1, gr = grouping_var)
  environment(trend_call) <- new.env(parent = emptyenv())
  out <- restore_trend_call_env(trend_call, list(fixed_Z = matrix(1, 2, 2)))
  expect_false(
    exists("grouping_var", envir = environment(out), inherits = TRUE)
  )
})

test_that("every mvgam() argument is inherited or named as not", {
  # This check compares the two lists. An argument is either carried over
  # by `update_inheritance_table()` or listed in
  # `mvgam_update_uninherited` with a reason. Anything in neither fails
  # here. No refit changes silently.
  supplied_by_update <- c("formula", "data", "...")
  formals_needed <- setdiff(names(formals(mvgam)), supplied_by_update)
  inherited <- names(update_inheritance_table())
  accounted <- c(inherited, names(mvgam_update_uninherited))
  expect_identical(setdiff(formals_needed, accounted), character(0L))

  # The dots `mvgam()` forwards to the code generator are model
  # defining too. They are held to the same rule.
  generator_args <- setdiff(
    names(formals(build_stan_components)),
    c("formula", "data", "family", "...")
  )
  expect_identical(setdiff(generator_args, accounted), character(0L))

  # Nothing is claimed in both places.
  expect_identical(intersect(inherited, names(mvgam_update_uninherited)),
                   character(0L))
})

test_that("every prediction argument reaches the method's body", {
  # The prediction surface carries the same risk. An argument in a
  # signature that no line consumes is taken from the user and dropped.
  # The call then returns a plausible number computed from the default
  # it was meant to override.
  methods <- c(
    "posterior_epred.mvgam", "posterior_predict.mvgam",
    "posterior_linpred.mvgam", "predict.mvgam", "fitted.mvgam",
    "forecast.mvgam", "hindcast.mvgam", "residuals.mvgam"
  )
  for (m in methods) {
    f <- getFromNamespace(m, "mvgam")
    declared <- setdiff(names(formals(f)), c("object", "..."))
    consumed <- all.vars(body(f))
    expect_identical(
      setdiff(declared, consumed), character(0),
      label = paste(m, "arguments absent from the body")
    )
  }
})

test_that("denormalise_loadings_prior returns what mvgam accepts", {
  # The resolved spec renames two fields and adds sizes that the
  # normaliser recomputes. It cannot be handed back as it is.
  # `normalise_loadings_prior()` allow-lists the user-facing names and
  # errors on the rest.
  expect_null(denormalise_loadings_prior(NULL))

  spec <- list(
    features_mat = matrix(1:6, nrow = 3L),
    distance_mats = list(cluster = diag(3)),
    column_shrinkage = "mgp", mgp_a1 = 2, mgp_a2 = 4,
    n_series = 3L, n_features = 2L, n_distances = 1L
  )
  # The spec includes the MGP hyperparameters whatever the shrinkage.
  # `normalise_loadings_prior()` refuses them unless the shrinkage is
  # "mgp". They travel only when they have a meaning.
  not_mgp <- spec
  not_mgp$column_shrinkage <- "none"
  expect_false(any(c("mgp_a1", "mgp_a2") %in%
                     names(denormalise_loadings_prior(not_mgp))))
  out <- denormalise_loadings_prior(spec)
  expect_equal(out$mgp_a1, 2)
  allowed <- c("features", "distances", "column_shrinkage",
                "mgp_a1", "mgp_a2")
  expect_true(all(names(out) %in% allowed))
  expect_equal(out$features, spec$features_mat)
  expect_equal(out$distances, spec$distance_mats)
  # The recomputed sizes do not travel.
  expect_false(any(c("n_series", "n_features", "n_distances") %in%
                     names(out)))

  # A spec holding nothing yields NULL.
  expect_null(denormalise_loadings_prior(list(n_series = 3L)))
})

test_that("the threads getter yields a count, not a brmsthreads", {
  # brms stores threading as a `brmsthreads` object on every fit, with a
  # NULL count when the user asked for none. `mvgam()` asserts an integer.
  # Handing back the object makes every refit fail.
  entry <- update_inheritance_table()$threads
  unset <- list(obs_model = list(threads = brms::threading(NULL)))
  expect_null(entry$getter(unset))

  set <- list(obs_model = list(threads = brms::threading(2)))
  expect_equal(entry$getter(set), 2)
  # A fit with no obs_model at all yields NULL.
  expect_null(entry$getter(list()))
})

test_that("walk_trend_call_args visits every named argument once", {
  # The walk `restore_trend_call_env()` uses to pair each argument's
  # name with its value.
  seen <- list()
  walk_trend_call_args(
    ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE),
    function(name, value) seen[[name]] <<- value
  )
  expect_setequal(names(seen), c("p", "trend_map", "cor"))
  expect_equal(seen$p, 1)
  expect_true(is.symbol(seen$trend_map))
  expect_true(seen$cor)

  # Positional arguments have no name. They are not visited.
  seen2 <- character(0)
  walk_trend_call_args(~ AR(1), function(name, value) {
    seen2 <<- c(seen2, name)
  })
  expect_length(seen2, 0L)

  # Nested calls are reached.
  seen3 <- character(0)
  walk_trend_call_args(
    ~ AR(p = 1, gr = interaction(a, drop = TRUE)),
    function(name, value) seen3 <<- c(seen3, name)
  )
  expect_true(all(c("p", "gr", "drop") %in% seen3))

  # A missing call visits nothing and raises no error.
  expect_silent(walk_trend_call_args(NULL, function(...) stop("unreached")))
})

test_that("an updated fit reports the model call it now describes", {
  # `update()` refits through `do.call(mvgam, ...)`. The call that the
  # refit captures holds that frame's resolved arguments. The call an
  # updated fit reports is the original call with the arguments this
  # `update()` named written over it.
  original <- quote(
    mvgam(formula = y ~ x, data = d, family = poisson(), chains = 2L)
  )
  restated <- mvgam:::restate_updated_call(
    original,
    quote(update(object = fit, chains = 4L, recompile = TRUE))
  )
  expect_identical(restated$chains, 4L)
  # `recompile` steers the refit and does not define the model. It is not
  # part of the call the model was built from. Neither is `object`.
  expect_false("recompile" %in% names(restated))
  expect_false("object" %in% names(restated))
  # Everything the update did not name survives as written.
  expect_identical(restated$data, as.name("d"))
  expect_identical(restated$family, quote(poisson()))

  # The two arguments spelled differently on the two sides map to the
  # names `mvgam()` uses.
  renamed <- mvgam:::restate_updated_call(
    original,
    quote(update(object = fit, formula. = z ~ w, newdata = d2))
  )
  expect_identical(renamed$formula, quote(z ~ w))
  expect_identical(renamed$data, as.name("d2"))
  expect_false(any(c("formula.", "newdata") %in% names(renamed)))
})

test_that("a new iter drops a warmup the stored call recorded", {
  # `jsdgam()` records `warmup` in its stored arguments. Paired with a
  # smaller `iter`, it would leave Stan no sampling iterations.
  stored <- list(iter = 800L, warmup = 400L)
  refit <- inherit_sampler_args(
    utils::modifyList(stored, list(iter = 400L)), list(fit = NULL),
    dots = list(iter = 400L)
  )
  expect_null(refit$warmup)
  expect_identical(refit$iter, 400L)
  # A warmup the caller names stays.
  kept <- inherit_sampler_args(
    list(iter = 400L, warmup = 100L), list(fit = NULL),
    dots = list(iter = 400L, warmup = 100L)
  )
  expect_identical(kept$warmup, 100L)
})

test_that("sampler settings travel only with the original algorithm", {
  inherited <- list(chains = 2L, iter = 300L, warmup = 150L,
                    control = list(adapt_delta = 0.95, max_treedepth = 11L))
  local_mocked_bindings(mvgam_sampler_inheritance = function(object) {
    inherited
  })
  fit <- list(algorithm = "sampling", backend = "cmdstanr")

  # A variational refit takes none of them: NUTS's 300 iterations
  # would stop the optimiser early.
  vi <- inherit_sampler_args(list(algorithm = "meanfield"), fit,
                             dots = list(algorithm = "meanfield"))
  expect_null(vi$iter)
  expect_null(vi$control)

  # `control` merges entry by entry, bare settings included.
  same <- inherit_sampler_args(
    list(algorithm = "sampling", backend = "cmdstanr", adapt_delta = 0.9),
    fit, dots = list(adapt_delta = 0.9)
  )
  expect_identical(same$control,
                   list(adapt_delta = 0.9, max_treedepth = 11L))
  expect_null(same$adapt_delta)
  expect_identical(same$iter, 300L)

  # Another backend has its own control defaults.
  other <- inherit_sampler_args(
    list(algorithm = "sampling", backend = "rstan"), fit, dots = list()
  )
  expect_null(other$control)
  expect_identical(other$chains, 2L)
})

test_that("a new prior joins the priors the user set before", {
  old <- rbind(
    brms::prior_string("exponential(3)", class = "sdgp"),
    brms::prior_string("normal(0, 5)", class = "b")
  )
  # A fitted table records the declared bound of a scale. `prior()`
  # leaves it NA.
  old$lb <- c("0", "")
  new <- brms::prior(exponential(1), class = sdgp)
  merged <- merge_update_priors(new, old)
  expect_identical(merged$prior[merged$class == "sdgp"], "exponential(1)")
  expect_identical(merged$prior[merged$class == "b"], "normal(0, 5)")
  expect_identical(merge_update_priors(new, NULL), new)
})

test_that("a multivariate formula update takes the whole formula", {
  old <- brms::bf(mvbind(y1, y2) ~ 1)
  expect_error(updated_formula(old, . ~ . + x), "not supported")
  expect_identical(updated_formula(old, mvbind(y1, y2) ~ x),
                   mvbind(y1, y2) ~ x)
  # A univariate `bf()` updates through brms's own method.
  up <- updated_formula(brms::bf(y ~ 1), ~ . + x)
  expect_equal(deparse(up$formula), "y ~ x")
})

test_that("held-out data overlapping new training times is dropped", {
  fit <- list(
    test_data = data.frame(time = 11:12, y = 0),
    trend_metadata = list(axes = list(vars = list(time_var = "time")))
  )
  expect_identical(
    inherited_holdout(fit, data.frame(time = 1:10), silent = 2L),
    fit$test_data
  )
  expect_message(
    dropped <- inherited_holdout(fit, data.frame(time = 1:12), silent = 1L),
    "drops the fit's held-out data"
  )
  expect_null(dropped)
})
