# brms threading × custom_family — composition note

Scope: pin the design for GitHub #78 (within-chain multithreading) for
mvgam's closure-unit families (`nmix`, `occ`, future Royle–Nichols /
multinomial detection). All quoted brms code is from
`paul-buerkner/brms` master, fetched 2026-06-10. mvgam paths are
absolute against the local working tree.

---

## brms threading mechanism

There is no `R/threading.R` in brms. The `threading()` constructor and
`use_threading()` predicate live in `R/backends.R`; the actual Stan
emission lives in `R/stancode.R`, with helper indexers in
`R/stan-helpers.R` and per-block branches in `R/stan-predictor.R` and
`R/stan-likelihood.R`.

The user-facing constructor (`brms/R/backends.R:521-542`) returns a
`brmsthreads` object with four fields: `threads`, `grainsize`, `static`,
`force`. `use_threading(threads, force = FALSE)` (line 563) returns
`TRUE` when `threads > 0` and (unless `force = TRUE`) `force` was not
itself set on the threads object — `force = TRUE` flips threading on at
the compile/runtime level without altering Stan code, for users who
have hand-written `reduce_sum` inside `stanvars`.

The Stan-code branch is in `brms/R/stancode.R:133-191`. When
`use_threading(threads)` is true, brms wraps the entire likelihood
body in a `partial_log_lik` function and emits a `reduce_sum` call.
The literal emission (lines 160-176):

```r
partial_log_lik <- paste0(
  "// compute partial sums of the log-likelihood\n",
  "real partial_log_lik", resp, "_lpmf(array[] int seq", resp,
  ", int start, int end", pll_args$typed, ") {\n",
  "  real ptarget = 0;\n",
  "  int N = end - start + 1;\n",
  partial_log_lik,
  "  return ptarget;\n",
  "}\n"
)
# ...
scode_predictor[[i]][["model_lik"]] <- paste0(
  "  target += reduce_sum", static, "(partial_log_lik", resp, "_lpmf",
  ", seq", resp, ", grainsize", pll_args$plain, ");\n"
)
str_add(scode_predictor[[i]][["tdata_def"]]) <- glue(
  "  array[N{resp}] int seq{resp} = sequence(1, N{resp});\n"
)
```

So for the univariate case the rendered Stan is:

```
real partial_log_lik_lpmf(array[] int seq, int start, int end,
                          <pll_args>) {
  real ptarget = 0;
  int N = end - start + 1;
  // ... likelihood body, with `target +=` rewritten to `ptarget +=`
  return ptarget;
}
// ...
transformed data {
  array[N] int seq = sequence(1, N);   // [1, 2, ..., N]
}
model {
  target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, <pll_plain>);
}
```

`sequence()` is included from `inst/chunks/fun_sequence.stan`; it just
returns `[1, 2, ..., N]`.

### What gets sliced

`reduce_sum` slices the first argument (`seq`) into ranges
`seq[start:end]`. Inside `partial_log_lik_lpmf`, `N = end - start + 1`
is the *slice length*. The likelihood body uses two indexing helpers
from `R/stan-helpers.R:117-127`:

- `stan_slice(threads)` → `"[start:end]"` when threading is on,
  empty otherwise — for vectorised lpdfs that take whole arrays.
- `stan_nn(threads)` → `"[nn]"` when threading is on, `"[n]"`
  otherwise; combined with `stan_nn_def` (`int nn = n + start - 1;`)
  which translates a local loop index `n` into the *global* row index.

All `vint`/`vreal` / response / `data` arrays are passed in **whole**
(see pll_args; e.g. `data array[] int vint1` in
`brms/R/stan-response.R:269`). The slice is applied at use-site via
`[start:end]` or `[nn]`. The unit of parallelisation is therefore
**one observation row per chunk**: a Poisson likelihood becomes
`target += poisson_lpmf(Y[start:end] | mu[start:end])` inside the
partial function, with `Y` and `mu` declared as full-length arrays in
the pll_args header.

For comparison, Bollen's nmix `partial_sum` slices over closure-units
(`g in start:end` where `g` indexes units, not observation rows).
brms's auto-emitter never slices over a user-defined grouping —
the slice axis is hard-coded to `1..N{resp}`.

---

## custom_family interaction

The dispatch path for `customfamily` objects is
`stan_log_lik_family` (`brms/R/stan-likelihood.R:30-60`) →
`stan_log_lik_custom` (line 962). The custom branch builds an `sdist`
object that names the user's `<family>_lpmf` and appends `family$vars`
as bare identifiers, then hands off to `stan_log_lik_general`
(line 96-106):

```r
stan_log_lik_general <- function(ll, bterms, threads, normalize, ...) {
  require_n <- grepl(stan_nn_regex(), ll$args)
  n <- str_if(require_n, stan_nn(threads), stan_slice(threads))
  lpdf <- stan_log_lik_lpdf_name(bterms, normalize, dist = ll$dist)
  Y <- stan_log_lik_Y_name(bterms)
  resp <- usc(bterms$resp)
  tr <- stan_log_lik_trunc(ll, bterms, threads = threads, ...)
  glue("{tp()}{ll$dist}_{lpdf}({Y}{resp}{n}{ll$shift} | {ll$args}){tr};\n")
}
```

So **custom families do compose with threading automatically** — there
is no skip branch and no opt-out. The auto-emitter just rewrites
`Y` to `Y[start:end]` (for `loop = FALSE` custom families) or
`Y[nn]` (for `loop = TRUE`) and wraps the whole body in
`partial_log_lik`.

The auto-wrapping is fully mechanical: it does *not* offer a hook to
slice over a user-defined index. The slice axis is whatever brms's
`stan_slice` / `stan_nn` produce, which is always observation index
`1..N{resp}`.

### vint / vreal under threading

`brms/R/stan-response.R:251-269` shows how `vint(...)` and
`vreal(...)` adterms wire into the threading branch:

```r
if (is.formula(bframe$adforms$vreal)) {
  vreal <- eval_rhs(bframe$adforms$vreal)
  k <- length(vreal$vars)
  str_add(out$data) <- cglue(
    "  // data for custom real vectors\n",
    "  array[N{resp}] real vreal{seq_len(k)}{resp};\n"
  )
  str_add(out$pll_args) <- cglue(", data array[] real vreal{seq_len(k)}{resp}")
}
if (is.formula(bframe$adforms$vint)) {
  # ... same pattern, `array[] int vint{...}`
}
```

`vint`/`vreal` arrays do flow through into the `partial_log_lik`
signature — the whole `array[N] int vint1` is passed in, and use-site
indexing rewrites `vint1[n]` → `vint1[nn]` (the global index) via
`stan_log_lik_custom` line 996-1006. **So `vint(...)`-typed integer
arrays are first-class citizens under brms threading**, provided they
are declared as adterms (not as bare `stanvar(..., block = "data")`).

The `[nn]` rewrite uses the global index, so `vint1[nn]` reads from
the *full* array — the slice does not change the semantics, just
restricts which rows the slice will touch.

---

## mvgam's current threading state

The user-facing path: `mvgam()` (`R/mvgam_core.R:298-309`) pulls
`threads` from `...`, runs `validate_threads()` (defined in
`R/backends.R:753-765`, a near-verbatim port of brms's
`validate_threads`), and passes the resulting `brmsthreads` object to
`compile_model()` and `fit_model()` (lines 326, 345).

`R/backends.R:188-248` shows the compile-side handling. With
`use_threading(threads, force = TRUE)`, rstan sets
`rstan_options(threads_per_chain = N)` and cmdstanr sets
`args$cpp_options$stan_threads <- TRUE`. At fit time
(`R/backends.R:329-557`) the per-backend runners then pass
`threads_per_chain` / `num_threads` / `threads` to the sampler call.

**However**, the Stan code itself is generated *without* a `threads`
argument. The base-code generator at
`R/stan_assembly.R:450-457` calls:

```r
base_code <- brms::make_stancode(
  formula = obs_setup$formula,
  data = obs_setup$data,
  family = obs_setup$family,
  data2 = obs_setup$data2,
  stanvars = all_stanvars,
  prior = obs_setup$prior
)
```

with no `threads = ...` argument, so `make_stancode` falls through to
its default `threads = threading()` (no-op). And
`setup_brms_lightweight` (`R/brms_integration.R:187-197`) calls
`brms::brm(..., backend = "mock", ...)` also without `threads`, so
the cached `stancode(mock_setup)` is also non-threaded.

Net effect today: `mvgam(..., threads = 4)` flips the compile flag
and configures the sampler to *be able to* parallelise, but the
generated Stan model contains no `reduce_sum` call, so there is no
within-chain parallel work to do. The threading arg is currently a
no-op end-to-end (issue #78).

### Family-specific gaps

- **tweedie** (`R/families.R:168-201`): `brms::custom_family(name =
  "tweedie", ..., loop = FALSE, vars = "M")`. The single integer
  `M` is injected as `brms::stanvar(M, name = "M", scode = "int<lower=1> M;",
  block = "data")` via `make_tweedie_stanvars(M)`. **`pll_args` is not
  set**, so under threading the auto-wrapped partial_log_lik would
  emit `tweedie_lpdf(Y[start:end] | mu[start:end], mphi[start:end],
  mtheta[start:end], M)` — and the bare `M` would be a free symbol
  inside the function body. Stan would refuse to compile.

- **nmix / occ** (`R/families.R:746-784, 1198-1288`): `family$vars`
  is set to `c("N_unit", "n_rep", "K_max", "Y_max", "visit_idx")`
  and the closure-unit arrays are injected via
  `brms::stanvar(..., block = "data")` in
  `make_closure_unit_arrays_stanvars()`. None of those stanvar
  calls pass `pll_args`. Same failure mode as tweedie, plus the
  deeper issue that `visit_idx` holds *global* row indices into the
  full `y` array — slicing Y to `Y[start:end]` makes those indices
  garbage even if the symbols were in scope.

- **Bollen's reference partial_sum** slices over closure-units
  (`g in start:end` over a `units` sequence of length `N_unit`),
  passing the full `y` array and the unit→rows index together. brms's
  auto-emitter cannot reproduce that: its `seq` is always
  `sequence(1, N{resp})`, never `sequence(1, N_unit)`.

---

## vint() compatibility

Verdict: **`vint()`/`vreal()` arrays do survive into the
partial_log_lik scope, but they must be declared as adterms (via
the formula's `|` syntax), not as plain `stanvar(..., block = "data")`
injections.**

The adterms path (brms `R/stan-response.R:251-269`) hard-codes
`str_add(out$pll_args) <- cglue(", data array[] int vint{seq_len(k)}{resp}")`,
so any `vint(...)` declared on the formula is added to the
partial_log_lik signature for free. The custom-family lpmf can read
`vint1[nn]` inside its body and brms's `stan_log_lik_custom` indexer
rewrites the `[n]` → `[nn]` as needed.

Stanvar-injected data (mvgam's current pattern for closure-unit
arrays and tweedie's `M`) does **not** automatically populate
`pll_args`. The `stanvar()` constructor (`brms/R/stanvars.R:125-152`)
auto-infers a `pll_type` from `x`'s class for the basic
scalar/vector/array shapes — so the *symbols* would land in scope
inside `partial_log_lik` — but two problems remain:

1. `N_unit`, `n_rep`, `K_max`, `Y_max` are per-*unit* arrays of length
   `N_unit`, not per-observation arrays of length `N`. brms's slice
   semantics don't apply.
2. `visit_idx` holds global row indices into the unsliced `y` array;
   passing it through is fine, but the body must also reference the
   full `y`, not the `[start:end]` slice that brms emits.

The slice-axis mismatch kills the closure-unit model regardless of
whether the symbols are in scope.

---

## Design recommendation for mvgam

**Recommendation: (b) inject our own `partial_sum` via
`stanvar(block = "functions")` and bypass brms's threading branch.**

Justification, with the brms source pointers:

1. brms's `reduce_sum` call hard-codes the slice axis to
   `sequence(1, N{resp})` (`brms/R/stancode.R:178`). There is no
   public hook to substitute a `sequence(1, N_unit)` per-family.
   For closure-unit families, the only valid slice axis is the unit
   axis, not the observation axis, so brms's auto-thread is
   semantically wrong for nmix / occ regardless of whether it
   compiles.

2. The `force = TRUE` flag on `threading()`
   (`brms/R/backends.R:489-492, 568`) is exactly the escape hatch
   we want: it sets `stan_threads = TRUE` at compile and configures
   the runtime, **without altering brms's Stan code**. mvgam can
   then inject a hand-written `partial_sum` plus the closure-unit
   `reduce_sum(partial_sum, units, grainsize, y, mu, p, n_rep,
   K_max, Y_max, visit_idx)` via the existing
   `attach_family_stanvars()` plumbing — same channel that already
   delivers `nmix_lpmf` / `occ_lpmf` to the functions block.

3. tweedie does fit brms's slice model (slice over observation rows
   is fine — the lpdf is a row-wise sum), so a future enhancement
   could let tweedie use brms's auto-thread by routing `M` through
   `pll_args = "int M"` on its `stanvar`. But the closure-unit
   families must stay on the `force = TRUE` path. Keeping both
   tweedie and the closure-unit families on path (b) is simpler and
   consistent.

### Concrete mechanics for (b)

When `use_threading(threads)` and the family carries
`attr(family, "mvgam_closure_unit")`:

1. In `R/mvgam_core.R` / `R/stan_assembly.R`, force
   `threads$force <- TRUE` before passing into the compile / fit
   chain (or build a fresh `threading(N, force = TRUE)`). brms then
   sees `use_threading(..., force = FALSE) == FALSE` in the
   stancode branch (so no auto-emission) but `use_threading(...,
   force = TRUE) == TRUE` in the compile and fit branches (so
   `stan_threads = TRUE` and `threads_per_chain` are still set).
2. The closure-unit `_stan_funs` helpers in `R/families.R` gain a
   second function entry: `partial_sum_nmix_lpmf(units[start:end],
   start, end, y, mu, p, n_rep, K_max, Y_max, visit_idx)` returning
   the same `log_sum_exp`-marginalised lpmf, summed over the
   sliced units only.
3. `prepare_closure_unit_family()` emits an extra `stanvar(block =
   "tdata", scode = "array[N_unit] int units = sequence(1, N_unit);")`
   and a `stanvar(block = "model", scode = "target +=
   reduce_sum(partial_sum_nmix_lpmf, units, grainsize, y, mu, p,
   n_rep, K_max, Y_max, visit_idx);")`. The base brms-emitted
   `target += nmix_lpmf(Y | mu, p, N_unit, n_rep, K_max, Y_max,
   visit_idx);` is then suppressed.

Open question for the implementation pass: brms emits its model-block
likelihood unconditionally; mvgam needs a way to remove or replace
that line when the closure-unit branch is active. Either (i) the
custom family supplies `loop = FALSE` plus a no-op lpmf that brms
calls and that returns 0, with the real likelihood added via the
model-block stanvar; or (ii) mvgam post-processes the brms-emitted
stancode to strip the base lpmf call. (ii) matches the existing
`stan_assembly.R` pattern of editing brms code, so it is probably
the path of least resistance.

---

## Outstanding open items

- The `force = TRUE` semantics (`brms/R/backends.R:563-571`) — Stan-
  code branch sees `use_threading(force = FALSE) == FALSE` while the
  compile/runtime branch sees `use_threading(force = TRUE) == TRUE` —
  should be smoke-tested with a tiny model before production use.
- Multivariate mvgam fits would need per-`resp` partial_sum injection.
  brms's threading branch loops over `scode_predictor` and emits one
  `partial_log_lik{resp}` per response (`brms/R/stancode.R:135`);
  closure-unit families are single-response today, so not an immediate
  concern.
