# CI-safe tests for update.mvgam. Numerical refits live in
# `tests/local/test-marginaleffects-concordance.R`; here we lock in
# S3 dispatch, signature parity with brms::update.brmsfit, the
# error paths that should fire deterministically, and the
# slot-inheritance helper (mvgam_update_call) on a stub.


make_update_stub <- function(n_iter = 50L, n_chains = 2L) {
  varnames <- c(
    "b_Intercept", "b_x", "Intercept",
    "sigma", "sigma_trend[1]", "ar1_trend[1]",
    "trend[1,1]", "lp__"
  )
  set.seed(3L)
  arr <- array(
    rnorm(n_iter * n_chains * length(varnames)),
    dim = c(n_iter, n_chains, length(varnames)),
    dimnames = list(NULL, NULL, varnames)
  )
  drws <- posterior::as_draws_array(arr)
  X <- model.matrix(~ x, data.frame(x = rnorm(10)))
  colnames(X)[1L] <- "Intercept"
  structure(
    list(
      fit = drws,
      formula = structure(y ~ x, class = c("brmsformula", "formula")),
      trend_formula = NULL,
      family = stats::gaussian(),
      prior = data.frame(prior = "(flat)", class = "b"),
      data = data.frame(y = rnorm(10), x = rnorm(10)),
      standata = list(X = X, K = ncol(X), Kc = ncol(X) - 1L),
      stancode = "data { int N; } parameters { real mu; } model { mu ~ normal(0,1); }",
      backend = "cmdstanr",
      algorithm = "sampling",
      response_names = "y",
      call = call("mvgam", formula = y ~ x)
    ),
    class = "mvgam"
  )
}


# ---- Dispatch + signature parity -----------------------------------

test_that("update.mvgam has an S3 method on `mvgam`", {
  expect_true(
    !is.null(getS3method("update", "mvgam", optional = TRUE))
  )
})


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
  attr(stub, "is_pooled") <- TRUE
  expect_error(
    update(stub),
    "pooled multiple-imputation"
  )
})


test_that("update.mvgam rejects non-data.frame newdata", {
  stub <- make_update_stub()
  expect_error(
    update(stub, newdata = list(y = 1, x = 1)),
    "data.frame"
  )
})


test_that("update.mvgam rejects non-logical recompile", {
  stub <- make_update_stub()
  expect_error(
    update(stub, recompile = "yes"),
    "logical"
  )
})


# ---- mvgam_update_call helper --------------------------------------

test_that("mvgam_update_call inherits formula from object when formula. is NULL", {
  stub <- make_update_stub()
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL, dots = list())
  expect_identical(out$formula, stub$formula)
})


test_that("mvgam_update_call applies formula. via stats::update.formula", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = ~ . + z, newdata = NULL, dots = list()
  )
  expect_identical(deparse(out$formula), "y ~ x + z")
})


test_that("mvgam_update_call inherits data when newdata is NULL", {
  stub <- make_update_stub()
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL, dots = list())
  expect_identical(out$data, stub$data)
})


test_that("mvgam_update_call overrides data when newdata is supplied", {
  stub <- make_update_stub()
  nd <- stub$data[1:5, ]
  out <- mvgam_update_call(stub, formula. = NULL, newdata = nd, dots = list())
  expect_identical(out$data, nd)
})


test_that("mvgam_update_call inherits family / prior / backend / algorithm", {
  stub <- make_update_stub()
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL, dots = list())
  expect_identical(out$family, stub$family)
  expect_identical(out$prior, stub$prior)
  expect_identical(out$backend, stub$backend)
  expect_identical(out$algorithm, stub$algorithm)
})

test_that("mvgam_update_call inherits the initial-value specification", {
  # A refit that silently reverted to random starts would undo the
  # reason the original fit asked for Pathfinder in the first place
  stub <- make_update_stub()
  stub$init <- "pathfinder"
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list())
  expect_identical(out$init, "pathfinder")
})

test_that("mvgam_update_call drops the inherited warmup when iter is overridden", {
  # `warmup` cannot be inherited on its own: mvgam derives it as
  # `iter %/% 2`, so pairing the original fit's warmup with a smaller
  # user-supplied `iter` asks Stan for a negative sampling count and
  # fails with a message that names neither argument.
  stub <- make_update_stub()
  local_mocked_bindings(
    mvgam_sampler_inheritance = function(object) {
      list(chains = 2L, iter = 1500L, warmup = 750L, thin = 1L)
    },
    .package = "mvgam"
  )

  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list(iter = 400L))
  expect_identical(out$iter, 400L)
  expect_null(out$warmup)

  # An explicit warmup is still honoured alongside a new iter
  out2 <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                            dots = list(iter = 400L, warmup = 100L))
  expect_identical(out2$warmup, 100L)

  # and both are still inherited when neither is overridden
  out3 <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                            dots = list())
  expect_identical(out3$iter, 1500L)
  expect_identical(out3$warmup, 750L)
})

test_that("mvgam_update_call lets the caller override the inherited init", {
  stub <- make_update_stub()
  stub$init <- "pathfinder"
  out <- mvgam_update_call(stub, formula. = NULL, newdata = NULL,
                           dots = list(init = "0"))
  expect_identical(out$init, "0")
})

test_that("mvgam_update_call lets the caller override the inherited prior", {
  # The lfo_cv refit story relies on update.mvgam reusing the
  # original fit's literal prior table by default (so brms doesn't
  # regenerate adaptive Intercept / sigma priors and bust the
  # compiled-model cache). Conversely, users must still be able to
  # supply `prior = ...` to override that table for a single refit.
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
  expect_false(identical(out$prior, stub$prior))
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
  # family() captures an environment, so compare by family name
  # rather than via identical().
  expect_identical(out$family$family, "poisson")
  expect_identical(out$backend, "rstan")
  expect_identical(out$cores, 4L)
})


test_that("mvgam_update_call passes through unrelated dots", {
  stub <- make_update_stub()
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(seed = 42L, init = "random", silent = 2L)
  )
  expect_identical(out$seed, 42L)
  expect_identical(out$init, "random")
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


test_that("mvgam_normalise_stancode strips a leading // comment line", {
  s1 <- "// Generated with mvgam 2.0.0 using brms 2.23.0\ndata { int N; }"
  s2 <- "// Generated with mvgam 9.9.9 using brms 9.9.9\ndata { int N; }"
  expect_identical(
    mvgam_normalise_stancode(s1),
    mvgam_normalise_stancode(s2)
  )
})


test_that("mvgam_normalise_stancode keeps stancode without a header", {
  s <- "data { int N; }\nparameters { real mu; }"
  expect_identical(mvgam_normalise_stancode(s), trimws(s))
})


# ---- Legacy fit detection ------------------------------------------

test_that("update.mvgam errors on legacy fits lacking trend_call", {
  stub <- make_update_stub()
  # Simulate a legacy fit: no trend_call slot, but trend_components
  # present (the construct that flags trend dynamics).
  stub$trend_components <- list(types = "AR")
  expect_error(
    update(stub),
    "trend_call"
  )
})


test_that("update.mvgam accepts legacy fits if trend_formula is supplied", {
  stub <- make_update_stub()
  stub$trend_components <- list(types = "AR")
  # mvgam_update_call should run without error when trend_formula
  # is in dots, even if trend_call is absent.
  out <- mvgam_update_call(
    stub, formula. = NULL, newdata = NULL,
    dots = list(trend_formula = ~ AR(p = 1))
  )
  expect_identical(deparse(out$trend_formula), "~AR(p = 1)")
})


test_that("restore_trend_call_env binds what the fit already carries", {
  # A formula keeps the expression and the environment it was written
  # in, not the values it names. Written inside a function, or read
  # back from an `.rds` in a fresh session, that environment no longer
  # holds the loading matrix the constructor referred to, and
  # `update()` failed with `object 'Z_user' not found` before it
  # reached `mvgam()`.
  trend_call <- ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  environment(trend_call) <- new.env(parent = emptyenv())
  expect_false(
    exists("Z_user", envir = environment(trend_call), inherits = TRUE)
  )

  Z <- matrix(c(1, 0, 0.8, 0.2, 0, 1, 0.3, 0.7), nrow = 4)
  out <- restore_trend_call_env(trend_call, list(fixed_Z = Z))
  expect_true(
    exists("Z_user", envir = environment(out), inherits = TRUE)
  )
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
  # Nothing to put back when every name already resolves, and a
  # literal argument names nothing at all.
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
  # and fails there against the user's own call, rather than part-way
  # through the rebuild.
  trend_call <- ~ AR(p = 1, gr = grouping_var)
  environment(trend_call) <- new.env(parent = emptyenv())
  out <- restore_trend_call_env(trend_call, list(fixed_Z = matrix(1, 2, 2)))
  expect_false(
    exists("grouping_var", envir = environment(out), inherits = TRUE)
  )
})


test_that("update() refuses a jsdgam rather than dropping its structure", {
  # A jsdgam is a `c("mvgam", "jsdgam")` object, so it dispatches
  # here. Rebuilding its call reaches `mvgam()`, which knows nothing
  # of `factor_formula`, `n_lv`, `species`, `unit`, `traits`,
  # `trait_slopes` or `phylo`; the refit carried none of them and said
  # nothing. The arguments cannot be recovered either, because
  # `$call` holds the symbols the user wrote rather than their
  # values.
  stub <- structure(
    list(formula = y ~ x, trend_call = ~ -1),
    class = c("mvgam", "jsdgam")
  )
  expect_error(update(stub), "Cannot 'update\\(\\)' a 'jsdgam' fit")
  expect_error(update(stub), "not recoverable")
  # The refusal comes before anything else is attempted, so a caller
  # passing arguments still gets the real reason.
  expect_error(update(stub, formula. = ~ . + z), "jsdgam")
})


test_that("an ordinary mvgam fit is not caught by that guard", {
  stub <- structure(
    list(formula = y ~ x, trend_call = ~ AR(p = 1)),
    class = "mvgam"
  )
  # It gets past the jsdgam check and fails later for its own reasons,
  # rather than being refused as a joint model.
  err <- tryCatch(update(stub), error = function(e) conditionMessage(e))
  expect_false(grepl("jsdgam", err))
})


test_that("every mvgam() argument is inherited or named as not", {
  # `loadings_prior` went missing because nothing compared the two
  # lists. An argument is either carried over by
  # `update_inheritance_table()` or listed in
  # `mvgam_update_uninherited` with a reason; anything in neither
  # fails here rather than silently changing a refit.
  supplied_by_update <- c("formula", "data", "...")
  formals_needed <- setdiff(names(formals(mvgam)), supplied_by_update)
  inherited <- names(update_inheritance_table())
  accounted <- c(inherited, names(mvgam_update_uninherited))
  expect_true(all(formals_needed %in% accounted))

  # The dots `mvgam()` forwards to the code generator are model
  # defining too, so they are held to the same rule.
  generator_args <- setdiff(
    names(formals(build_stan_components)),
    c("formula", "data", "family", "...")
  )
  expect_true(all(generator_args %in% accounted))

  # Nothing is claimed in both places.
  expect_length(
    intersect(inherited, names(mvgam_update_uninherited)),
    0L
  )
  # Every reason says something.
  expect_true(all(nzchar(mvgam_update_uninherited)))
})


test_that("denormalise_loadings_prior returns what mvgam accepts", {
  # The resolved spec renames two fields and adds sizes the
  # normaliser recomputes, so it cannot be handed back as it stands:
  # `normalise_loadings_prior()` allow-lists the user-facing names and
  # errors on the rest.
  expect_null(denormalise_loadings_prior(NULL))

  spec <- list(
    features_mat = matrix(1:6, nrow = 3L),
    distance_mats = list(cluster = diag(3)),
    column_shrinkage = "mgp", mgp_a1 = 2, mgp_a2 = 4,
    n_series = 3L, n_features = 2L, n_distances = 1L
  )
  # The spec carries the MGP hyperparameters whatever the shrinkage,
  # and `normalise_loadings_prior()` refuses them unless it is "mgp",
  # so they travel only when they mean something.
  not_mgp <- spec
  not_mgp$column_shrinkage <- "none"
  expect_false(any(c("mgp_a1", "mgp_a2") %in%
                     names(denormalise_loadings_prior(not_mgp))))
  expect_equal(denormalise_loadings_prior(spec)$mgp_a1, 2)
  out <- denormalise_loadings_prior(spec)
  allowed <- c("features", "distances", "column_shrinkage",
                "mgp_a1", "mgp_a2")
  expect_true(all(names(out) %in% allowed))
  expect_equal(out$features, spec$features_mat)
  expect_equal(out$distances, spec$distance_mats)
  # The recomputed sizes do not travel.
  expect_false(any(c("n_series", "n_features", "n_distances") %in%
                     names(out)))

  # A spec holding nothing yields nothing rather than an empty list.
  expect_null(denormalise_loadings_prior(list(n_series = 3L)))
})


test_that("the threads getter yields a count, not a brmsthreads", {
  # brms stores threading as a `brmsthreads` object on every fit, with
  # a NULL count when the user asked for none. `mvgam()` asserts an
  # integer, so handing the object back errored on any refit.
  entry <- update_inheritance_table()$threads
  unset <- list(obs_model = list(threads = brms::threading(NULL)))
  expect_null(entry$getter(unset))

  set <- list(obs_model = list(threads = brms::threading(2)))
  expect_equal(entry$getter(set), 2)
  expect_false(inherits(entry$getter(set), "brmsthreads"))
  # A fit with no obs_model at all yields nothing rather than erroring.
  expect_null(entry$getter(list()))
})


test_that("trend_call_names_arg sees the argument, not its value", {
  # `all.vars()` and `all.names()` return the values an argument was
  # given but never the argument's own name, so the call is walked.
  tc <- ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE)
  expect_true(trend_call_names_arg(tc, "trend_map"))
  expect_true(trend_call_names_arg(tc, "cor"))
  expect_false(trend_call_names_arg(tc, "n_lv"))
  # The value's own name is not the argument's.
  expect_false(trend_call_names_arg(tc, "Z_user"))
  expect_false(trend_call_names_arg(NULL, "trend_map"))
  expect_false(trend_call_names_arg(~ AR(p = 1), "trend_map"))
})


test_that("trend_map is withheld when the constructor already has it", {
  # Supplying it at both the constructor and the top level is a
  # collision `mvgam()` refuses, so a refit must hand back only what
  # the original call put at the top level.
  entry <- update_inheritance_table()$trend_map
  Z <- matrix(c(1, 0, 0, 1), nrow = 2L)

  on_constructor <- list(
    trend_call = ~ AR(p = 1, trend_map = Z_user),
    trend_metadata = list(fixed_Z = Z)
  )
  expect_null(entry$getter(on_constructor))

  at_top_level <- list(
    trend_call = ~ AR(p = 1),
    trend_metadata = list(fixed_Z = Z)
  )
  expect_equal(entry$getter(at_top_level), Z)

  # A fit with no fixed loadings hands back nothing either way.
  expect_null(entry$getter(list(trend_call = ~ AR(p = 1),
                                 trend_metadata = list())))
})


test_that("walk_trend_call_args visits every named argument once", {
  # The one walk both readers use: `restore_trend_call_env()` looks at
  # the values, `trend_call_names_arg()` at the names.
  seen <- list()
  walk_trend_call_args(
    ~ -1 + AR(p = 1, trend_map = Z_user, cor = TRUE),
    function(name, value) seen[[name]] <<- value
  )
  expect_setequal(names(seen), c("p", "trend_map", "cor"))
  expect_equal(seen$p, 1)
  expect_true(is.symbol(seen$trend_map))
  expect_true(seen$cor)

  # Positional arguments carry no name and are not visited.
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

  # A missing call visits nothing rather than erroring.
  expect_silent(walk_trend_call_args(NULL, function(...) stop("unreached")))
})


test_that("getCall reports the call the user wrote", {
  # Every entry point reaches the fitting pipeline through
  # `do.call()`, which substitutes each argument's value for the
  # symbol. A call captured inside the pipeline therefore had the
  # function object in its head and the whole data frame in its
  # `data` slot, so `deparse(getCall(fit))` ran to 50 lines and
  # printed the training data instead of the call.
  set.seed(1L)
  d <- data.frame(time = 1:30, series = factor("s1"), x = rnorm(30))
  d$y <- rpois(30, 3)
  fit <- mvgam(y ~ x, data = d, family = poisson(), run_model = FALSE)

  cl <- getCall(fit)
  expect_true(is.name(cl[[1L]]))
  expect_identical(as.character(cl[[1L]]), "mvgam")
  # The arguments are the symbols and calls that were typed, not the
  # objects they evaluate to, which is what keeps the deparse short
  # and re-evaluable.
  expect_identical(cl$data, as.name("d"))
  expect_identical(cl$family, quote(poisson()))
  expect_lt(length(deparse(cl)), 5L)
})


test_that("an updated fit reports the model call it now describes", {
  # `update()` refits through `do.call(mvgam, ...)`, so the call the
  # refit captures is that frame's resolved arguments. The call an
  # updated fit reports is the original with the arguments this
  # `update()` named written over it.
  original <- quote(
    mvgam(formula = y ~ x, data = d, family = poisson(), chains = 2L)
  )
  restated <- mvgam:::restate_updated_call(
    original,
    quote(update(object = fit, chains = 4L, recompile = TRUE))
  )
  expect_identical(restated$chains, 4L)
  # `recompile` steers the refit, not the model, so it is not part
  # of the call the model was built from; nor is `object`.
  expect_false("recompile" %in% names(restated))
  expect_false("object" %in% names(restated))
  # Everything the update did not name survives as written.
  expect_identical(restated$data, as.name("d"))
  expect_identical(restated$family, quote(poisson()))

  # The two arguments spelled differently on the two sides land on
  # the names `mvgam()` reads.
  renamed <- mvgam:::restate_updated_call(
    original,
    quote(update(object = fit, formula. = z ~ w, newdata = d2))
  )
  expect_identical(renamed$formula, quote(z ~ w))
  expect_identical(renamed$data, as.name("d2"))
  expect_false(any(c("formula.", "newdata") %in% names(renamed)))
})


test_that("a refit states an n_lv the trend formula does not name", {
  # A factor count set by a top-level `trend_map` is on the fit and
  # nowhere in the expression the user wrote, so re-evaluating that
  # expression alone builds a different model: `by = lv_axis()` reads
  # the series axis rather than the factor axis. The refit has to say
  # what the fit resolved.
  trend_call <- ~ s(elev, k = 5, by = lv_axis()) - 1 + ZMVN(cor = TRUE)
  out <- state_resolved_trend_args(trend_call, list(n_lv = 2L))
  expect_true(trend_call_names_arg(out, "n_lv"))
  # Stated on the constructor that owns it, leaving the rest of the
  # expression as the user wrote it.
  expect_identical(
    deparse1(rlang::f_rhs(out)),
    "s(elev, k = 5, by = lv_axis()) - 1 + ZMVN(cor = TRUE, n_lv = 2L)"
  )
})


test_that("an n_lv the call already names is left as the user wrote it", {
  # The expression settles it, so there is nothing to restate; the
  # symbol is `restore_trend_call_env()`'s job, not this one's.
  trend_call <- ~ AR(p = 1, n_lv = k)
  out <- state_resolved_trend_args(trend_call, list(n_lv = 2L))
  expect_identical(deparse1(rlang::f_rhs(out)), "AR(p = 1, n_lv = k)")
})


test_that("a fit that resolved no n_lv is rebuilt unchanged", {
  # Every non-factor fit takes this path, so a value invented here
  # would turn an ordinary trend into a factor model.
  trend_call <- ~ AR(p = 1)
  expect_identical(
    state_resolved_trend_args(trend_call, list(n_lv = NULL)),
    trend_call
  )
  expect_identical(
    state_resolved_trend_args(trend_call, NULL),
    trend_call
  )
})


test_that("every trend constructor is told, not just the first", {
  # A trend formula may carry one constructor per response. Stating
  # the count on one of them would leave the others building a
  # different model, which is the fault this whole path exists to
  # prevent.
  out <- state_resolved_trend_args(
    ~ AR(p = 1) + VAR(cor = TRUE), list(n_lv = 2L)
  )
  txt <- deparse1(rlang::f_rhs(out))
  expect_match(txt, "AR(p = 1, n_lv = 2L)", fixed = TRUE)
  expect_match(txt, "VAR(cor = TRUE, n_lv = 2L)", fixed = TRUE)
})


test_that("a resolved count with nowhere to be stated is refused", {
  # Silently rebuilding a different model is what `update()` already
  # declines to do for a `jsdgam` fit, so the same answer is given
  # here rather than a refit the caller cannot tell apart.
  expect_error(
    state_resolved_trend_args(~ s(env, by = lv_axis()) - 1,
                              list(n_lv = 2L)),
    "no trend constructor"
  )
})


test_that("the arguments a refit states are ones the map knows", {
  # Two tables naming the same argument would drift. The slot each
  # one is read from lives in `trend_arg_metadata` alone.
  expect_true(
    all(trend_args_stated_on_rebuild %in% names(trend_arg_metadata))
  )
})
