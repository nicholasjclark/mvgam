# Bollen multithreaded N-mixture reference — investigation note

Reference material for porting Royle-Nichols (RN) and Poisson-Poisson
(PPM) likelihoods into mvgam, and for evaluating adoption of Bollen's
closure-unit `partial_sum` pattern for within-chain multithreading.

Source repo: <https://github.com/martijn-bollen/RWsim_abundance_models>
(MIT licensed). Files fetched verbatim from `Stan/` on `main`:

- `NMM_multithreaded.stan` — Poisson-binomial N-mixture (baseline; mvgam
  already ships an analogue as `nmix()`).
- `RN_multithreaded.stan` — Royle-Nichols (2003) binary-detection
  N-mixture.
- `PPM_multithreaded.stan` — Poisson-Poisson mixture (counts of
  encounters, e.g. camera-trap detections per visit).
- `ZIPNM_multithreaded.stan`, `ZIPPM_multithreaded.stan` — zero-inflated
  extensions; out of scope for v2.0, only the `partial_sum` signature was
  cross-checked.

No paper/preprint was found in the repo (README and `R/` scripts contain
no parameterisation prose). Model conventions below were inferred
directly from the Stan code. Repo `R/5_model_fitting.R` confirms RN
operates on binary detection history (`datalist[[l]][datalist[[l]]>0]
<- 1`); NMM/PPM operate on raw counts.

mvgam side referenced:

- `R/families.R:543` `build_closure_unit_arrays()` — produces
  `N_unit, n_rep, K_max, Y_max, visit_idx, max_rep, unit_labels`.
- `R/families.R:1106` `nmix_stan_funs()` — current `nmix_lpmf` Stan
  function block.
- `R/families.R:1198` `make_closure_unit_arrays_stanvars()` — shared
  data-block emitter (`brms::stanvar(...)` for each integer array).
- `R/families.R:753` `nmix()` family constructor (`dpars = c("mu", "p")`,
  `loop = FALSE`).

---

## 1. `partial_sum` signature

Verbatim from `NMM_multithreaded.stan` (sets the pattern for all five
models):

```stan
real partial_sum(int[] site,
                 int start, int end,
                 int[, ] count,
                 int max_n, real lambda, real p) {
  real lp = 0;
  for (m in start:end)
    lp = lp + pb_lpmf(count[m] | max_n, lambda, p);
  return lp;
}
```

Slice index: the slice variable is `site`, a length-`R` dummy
(`int site[R] = rep_array(0, R)` in `transformed data`). Bollen never
reads `site[m]` inside `partial_sum`; the slice variable exists solely
to give `reduce_sum` something of the right length so that
`start:end` indexes **closure units (sites)**, not per-visit
observations. The loop body uses `m` directly to index the per-site
visit-count row `count[m]`, then calls the per-site marginal
`pb_lpmf(count[m] | max_n, lambda, p)`.

This is the key design decision for our port: chunks are
**closure-unit-aligned by construction**, so `reduce_sum` cannot split a
unit's visit replicates across two threads. The per-unit
`log_sum_exp` over the latent abundance grid stays intact inside one
thread's contribution.

`reduce_sum_static` is not used; `reduce_sum` (dynamic scheduling) is
the call site in the `model` block with `grainsize = 1`:

```stan
target += reduce_sum(partial_sum, site, grainsize, y, K, lambda, p);
```

The RN and PPM files use the same template, with the lpmf swapped for
`rn_lpmf` / `pp_lpmf` and the extra `int[] occ` argument added for RN
(see §3). ZIPNM/ZIPPM differ only in that they add a `psi`
zero-inflation parameter to the trailing argument list.

## 2. Closure-unit chunking strategy

Bollen pre-shapes the data into a rectangular `int y[R, T]` matrix —
sites are rows, replicates are columns, and `T` is constant across
sites (his simulated design has equal visit counts per site).
`partial_sum` then takes a row at a time. This makes the closure-unit
chunking trivial because every row of `count` is exactly one closure
unit.

Integer arrays packaged into the Stan `data` block:

| Bollen | Purpose | mvgam analogue |
|---|---|---|
| `int R` | number of closure units (sites) | `N_unit` |
| `int T` | replicates per unit (fixed) | `max_rep` (we allow ragged units) |
| `int K` | global cap on latent abundance | `K_max[g]` (per-unit) |
| `int y[R, T]` | per-visit counts (or 0/1 for RN) | response vector `y` indexed via `visit_idx` |
| `int site[R]` | dummy slice variable | would be created in `transformed data` for our port |

mvgam is strictly richer than Bollen on data layout: we permit
ragged `n_rep[g]` and per-unit `K_max[g]`, so our `visit_idx[g, 1:n_rep[g]]`
gather replaces Bollen's `count[m]` row slice. The
slice-variable trick still works — `partial_sum` would index closure
units `g in start:end`, read `n_rep[g]`, `K_max[g]`, `Y_max[g]`, and
`visit_idx[g, 1:n_rep[g]]` from the data block (closed over from outer
scope, so they need not appear in the `partial_sum` signature).

Bollen does **not** carry an explicit `unit_labels` / `visit_idx`
array — his rectangular-matrix layout makes them unnecessary. Our
ragged layout is more general; the chunking property is the same.

## 3. Royle-Nichols likelihood as written

Verbatim core (`RN_multithreaded.stan`):

```stan
vector rn_logp(int[] det, int occ, int max_n,
               real lambda, real p) {
  vector[max_n - occ + 1] lp;
  if (occ == 0) {
    lp[1] = poisson_log_lpmf(0 | log(lambda)) + 1;
  } else {
    lp[1] = poisson_log_lpmf(1 | log(lambda)) + binomial_lpmf(det | 1, p);
  }
  for (j in 2:(max_n - occ + 1)) {
    lp[j] = poisson_log_lpmf(occ + j - 1 | log(lambda))
          + binomial_lpmf(det | 1, 1 - (1 - p)^(occ + j - 1));
  }
  return lp;
}
```

`rn_lpmf` is a wrapper around `rn_logp` that returns
`log_sum_exp(lp)`.

Parameter set:

- `lambda` (real, lower=0): mean site abundance, log link via
  `poisson_log_lpmf(... | log(lambda))`. **Site-level only** —
  constant within a closure unit.
- `p` (real in `[0,1]`): per-individual detection probability,
  identity link in this reference (a flat `[0,1]` prior is used).
  **Site-level only** in the reference; no visit-level covariates on
  detection are wired in. There is no `inv_logit`/logit transform —
  `p` is sampled in the probability scale.

Likelihood structure: a binary detection vector `det[1:T]` at site
`i`. The closure constraint enters as `occ = max(det) = z[i]` (the
"at least one detection" indicator built in `transformed data`):

```stan
for (i in 1:R) {
  z[i] = 0;
  if (sum(y[i])) z[i] = 1;
}
```

Marginalisation: over latent abundance `N in {occ, occ+1, ..., max_n}`,
where the lower bound is `occ` (1 if any detection occurred, 0
otherwise). For each candidate `N`, the joint contribution is

  log P(N | lambda) + log P(det | N) = `poisson_log_lpmf(N | log(lambda))`
                                     + `binomial_lpmf(det | 1, 1 - (1 - p)^N)`,

which is the canonical RN "at least one of N individuals detected per
visit" form. The `1 - (1 - p)^N` term collapses individual-level
detection probability `p` into a visit-level detection probability
that grows with abundance — this is the entire substantive difference
versus the standard binomial-mixture (`nmix`).

Two quirks of Bollen's code worth noting for the port:

1. **The `+ 1` constant in the `occ == 0` branch** (`lp[1] = poisson_log_lpmf(0 | log(lambda)) + 1;`)
   looks like a typo — there is no `binomial_lpmf` term added in the
   non-detection branch, but adding a literal `+ 1` to a log
   probability has no mathematical justification. The all-zeros
   binomial term `binomial_lpmf(zeros | 1, 0) = 0` would be correct.
   We should **not** carry this `+ 1` into the mvgam port; it adds
   `+ 1 * R` to the joint log-likelihood and shifts the posterior on
   `lambda` (the term is independent of `lambda`, so the effect is a
   constant target shift that drops out of MCMC — but it ruins
   `log_lik` for LOO).
2. The `if (occ == 0)` branch builds a length-1 `lp` vector instead
   of taking the general-case loop path. The general loop already
   handles `occ = 0` correctly because `binomial_lpmf(zeros | 1,
   1 - (1 - p)^N)` is zero everywhere required. The branch saves a
   few Stan operations but adds the typo risk above.

Upper truncation `max_n` is the global `K` from the data block (same
convention as `K_max` in mvgam, except mvgam allows per-unit caps).

## 4. Poisson-Poisson likelihood as written

Verbatim core (`PPM_multithreaded.stan`):

```stan
real poispois_lpmf(array[] int y, int n, real lambda, real mu) {
  if (max(y) > n) {
    return negative_infinity();
  }
  return poisson_log_lpmf(n | log(lambda))
       + poisson_log_lpmf(y | log(n + 1e-9) + logit(mu));
}
```

`pp_lpmf` wraps the same `log_sum_exp` over `k in max(y) : max_n`
that `pb_lpmf` does in the standard N-mixture.

Parameter set:

- `lambda` (real, lower=0): mean latent abundance, `log` link via
  `poisson_log_lpmf(N | log(lambda))`. Site-level only in the
  reference.
- `p` (real in `[0,1]`): per-individual encounter rate. The name is
  `p` but it is **not** a probability — see below.

The mathematical content is unusual and the parameterisation needs
care:

- `y[t] | N ~ Poisson(N * encounter_rate)`. The reference writes this
  as `poisson_log_lpmf(y | log(n + 1e-9) + logit(mu))`, which is
  algebraically `y ~ Poisson((N + 1e-9) * (mu / (1 - mu)))`. That is,
  Bollen passes a probability-scale `mu in [0, 1]` and converts to a
  positive rate by **logit** transform (mapping `[0, 1]` to the real
  line, then exponentiating gives `mu / (1 - mu)`). This is an
  unconventional choice — most PPM references (Sollmann et al. 2013;
  Mizel et al. 2018) parameterise directly with a log-scale encounter
  rate `mu > 0`. The logit choice in Bollen's code lets him reuse the
  same `real<lower=0,upper=1> p` parameter declaration across NMM,
  RN, and PPM, but it bounds the per-individual encounter rate to
  `(0, +inf)` via the odds transform, with a soft squashing as
  `mu -> 1` that has no biological motivation.

- The `1e-9` is a regularisation hack so that `log(N) = log(0) = -inf`
  is replaced by `log(1e-9) ≈ -20.7` when `N = 0`. This is needed
  because his RNG and generated quantities also evaluate at `N = 0`.
  A cleaner formulation would handle the `N = 0` case separately
  (Poisson with rate zero is a point mass at zero) and use a proper
  log-scale rate `log_mu` to skip the `log(n + eps) + logit(mu)`
  composition entirely.

- **N marginalisation IS needed.** This is the answer to the
  "or is PPM closed-form Poisson(lambda * p)?" question: it is
  closed-form only if the per-visit encounter rate is constant in
  `N`. Here `y ~ Poisson(N * encounter_rate)`, so marginalising
  `N ~ Poisson(lambda)` gives `y ~ Poisson(lambda * encounter_rate)`
  only at the **expectation** — the marginal distribution is a
  Neyman Type A (compound Poisson), not a Poisson. Bollen's
  implementation correctly handles this via the truncated
  `log_sum_exp` over `k = max(y) : max_n`, identical in structure to
  the NMM. The closure constraint `max(y) > n => -inf` also matches.

- Detection is **per-individual**, mirroring the RN
  parameterisation, not per-visit. There is no visit-level
  covariate plumbing in the reference.

For the mvgam port we should:

1. Reject Bollen's `logit(mu)` reparameterisation. Use a `log` link
   on `p` (encounter rate, positive real); brms's standard log link
   on a dpar gives this cleanly.
2. Replace `log(n + 1e-9) + logit(mu)` with the cleaner
   `log_n + log_mu` where `log_n = log(k)` for `k >= 1` and the
   `k = 0` branch is handled as a point mass at `y = 0`
   (`poisson_log_lpmf(y | log(k) + log_mu)` with `log(0) = -inf`
   gives `y = 0 => 0`, `y > 0 => -inf`, which is the correct point
   mass without the `1e-9` fudge — Stan's `poisson_log_lpmf` accepts
   `-inf` rate when `y = 0`).

## 5. Deltas vs mvgam's existing `nmix_poisson_binomial_lpmf`

Current mvgam Stan signature (`R/families.R:1114-1166`):

```stan
real nmix_lpmf(
  array[] int y, vector mu, vector p,
  int N_unit, array[] int n_rep,
  array[] int K_max, array[] int Y_max,
  array[,] int visit_idx)
```

with a scalar-`p` overload that `rep_vector`s to the per-visit form.
The linear predictors `mu` and `p` enter on the **response** and
**probability** scales respectively (brms applies `exp` /
`inv_logit`), and the lpmf converts back via `log(mu)` /
`logit(p)` for numerical stability inside
`poisson_log_lpmf` / `binomial_logit_lpmf`.

For RN — `rn_lpmf` signature (proposed):

```stan
real rn_lpmf(
  array[] int y, vector mu, vector p,
  int N_unit, array[] int n_rep,
  array[] int K_max, array[] int Y_max,
  array[,] int visit_idx)
```

Deltas relative to `nmix_lpmf`:

- Response `y` is binary {0, 1}. The closure-unit data prep would
  need a `mvgam_binary_response` attribute (already exists for
  `occ()`) so the same `Y_max <= 1` declaration is reused.
- Inside the per-unit loop, swap
  `binomial_logit_lpmf(counts | k, lp_visits)` for
  `bernoulli_logit_lpmf(counts | log_inv_logit_minus(k, lp_visits))`
  where `1 - (1 - inv_logit(lp_visits))^k` must be computed in a
  numerically stable way. A clean form is
  `log1m_exp(k * log1m_inv_logit(lp_visits))` for the "at least one
  of k detected" probability on the log scale, fed to
  `bernoulli_logit_lpmf(counts | logit_p_eff)` after converting back
  through `logit_p_eff = logit(1 - exp(k * log1m_inv_logit(lp_visits)))`.
  (Implementation detail to settle at code time; the math is
  Bollen's `1 - (1 - p)^N`.)
- `K_max` is still per-unit (we keep the mvgam convention).
- `Y_max[g]` is still the per-unit lower truncation, but it always
  equals `0` or `1` (mvgam's existing `occ()` family uses this same
  bound).
- The `cap` data column is still required (or defaults to a
  sensible upper bound, as `occ()` does for binary state).

For PPM — `ppm_lpmf` signature (proposed):

```stan
real ppm_lpmf(
  array[] int y, vector mu, vector p,
  int N_unit, array[] int n_rep,
  array[] int K_max, array[] int Y_max,
  array[,] int visit_idx)
```

Deltas:

- Response `y` is unbounded counts (same support as `nmix()`).
- `p` arrives from brms as a positive rate (log link), not a
  probability. The dpar declaration changes from
  `links = c("log", "logit")` to `links = c("log", "log")`.
- Inside the per-unit loop, swap
  `binomial_logit_lpmf(counts | k, lp_visits)` for
  `poisson_log_lpmf(counts | log(k) + log_p_visits[v])` summed over
  the unit's visits, with the `k = 0` case handled as a point mass
  at `y = 0`.
- Per-unit `cap` `K_max[g]` is still consulted as the marginalisation
  upper limit (closure-unit data prep is unchanged).

**Are the two ports additive on the existing scaffold?** Yes, with
two reservations:

1. The `links` argument in `nmix()` is hardcoded to
   `c("log", "logit")`. PPM needs `c("log", "log")`. The closure-unit
   family constructor pattern handles this trivially (each family
   sets its own `brms::custom_family(..., links = ...)`); no shared
   code needs to change. The R-side `extract_p_for_closure_unit()`
   path (`R/log_lik.mvgam.R:169`) reads `p` via brms's standard dpar
   extraction, so the link is whatever brms applies — no special
   case there.
2. The R-side latent-N samplers (`R/posterior_predict.R:1515`,
   `R/log_lik.mvgam.R:161`) currently reference Poisson-binomial
   maths. RN and PPM each need their own sampler emitting the
   correct `categorical_rng(softmax(lp))` weights; the dispatcher
   `dispatch_closure_unit_method(family, "predict")` is already
   keyed on family name so the routing is in place. The closure-unit
   per-unit residual helpers
   (`R/residuals.mvgam.R:310 compute_closure_unit_residuals()`)
   should also dispatch by family for the expected-count formula
   inside the χ² discrepancy. Both are additive — neither file would
   need restructuring, only new R-level lpmf evaluators for the new
   families.

No shared scaffold change is required. The ports slot in alongside
`nmix()` and `occ()` in `R/families.R` and add per-family helpers in
`R/log_lik.mvgam.R`, `R/posterior_predict.R`, and
`R/residuals.mvgam.R`.

## 6. Threading rollout risk for mvgam

If we lift Bollen's `partial_sum` skeleton verbatim, the Stan-side
changes for mvgam are mechanical:

- Declare a dummy `int unit_dummy[N_unit] = rep_array(0, N_unit);`
  in `transformed data` (emitted via a new `brms::stanvar(...,
  block = "tdata")`).
- Replace the per-unit `for (g in 1 : N_unit)` loop body of
  `nmix_lpmf` with the body of a `partial_sum(int[] unit_dummy, int
  start, int end, ...)`.
- In the `model` block, swap
  `target += nmix_lpmf(y | mu, p, N_unit, n_rep, K_max, Y_max, visit_idx)`
  for
  `target += reduce_sum(partial_sum_nmix, unit_dummy, grainsize, ...)`.

What requires confirmation on the brms side:

- **brms emits the lpmf call via `target += ... ~ nmix(mu, p, vint(...))`
  or directly via `target += nmix_lpmf(...)` depending on the
  custom_family's `loop` arg.** mvgam sets `loop = FALSE`, which
  produces a single batched call. The current implementation passes
  the closure-unit arrays via separate `brms::stanvar(... block =
  "data")` declarations, NOT via `vint()` (verified by
  `grep "vint(" R/` — only one hit in priors.R docs). So the lpmf
  signature can take `int[]` and `int[,]` types directly without
  brms's `vint()` packaging.
- **Does brms's `target += nmix_lpmf(...)` line cooperate with
  `reduce_sum`?** brms generates the lpmf call as a single line in
  the `model` block. To switch to `reduce_sum`, mvgam would need to
  inject an override that swaps the brms-generated call for the
  `reduce_sum` call. The cleanest hook is a `brms::stanvar(...,
  block = "model", position = "end")` that adds the
  `target += reduce_sum(...)` line, paired with suppression of the
  default lpmf call. brms supports this via a custom family stanvar
  block, but mvgam would need to verify the exact emission point —
  specifically whether `loop = FALSE` puts the lpmf call somewhere
  in the model block that mvgam can intercept, or whether the
  override has to happen via brms's `add_loglik_part` mechanism.
- **Open question (cannot resolve from Bollen's code alone):** mvgam's
  closure-unit arrays are real Stan `int[]` / `int[,]` types declared
  in the data block via `brms::stanvar()`. They are NOT brms `vint()`
  data, so they would be visible to a `partial_sum` function via
  closure (outer-scope reads) without any `vint()`-to-`int[]`
  marshalling. This is the regime Bollen's code lives in too — his
  `count[, ]`, `det[, ]`, `occ[]`, `K` are all plain Stan data block
  declarations, not brms `vint()` constructs. **So the open question
  "does brms's reduce_sum integration cooperate with stan_funs that
  have vint()-typed parameters" does not actually arise for mvgam's
  current scaffold** — we are not using `vint()`. The relevant
  question is instead: does brms permit suppressing its
  auto-generated lpmf call when `loop = FALSE`, so we can inject a
  `reduce_sum` call in its place? That needs to be checked against
  brms's current `add_funs()` / `target_format()` source (parallel
  brms investigation will resolve).

If brms refuses to let us suppress the auto-generated lpmf call, the
fallback is to keep the existing `nmix_lpmf(... )` shape but rewrite
its **body** as a `reduce_sum` over per-unit contributions —
i.e. the lpmf becomes a thin wrapper that calls `reduce_sum(partial_sum_nmix,
unit_dummy, grainsize, ...)` internally. This is the lower-risk
path: it preserves the brms emission contract and only changes the
internals of the function block. Threading then activates whenever
the user fits with `threading = brms::threading(threads_per_chain =
N)`, which `mvgam()` already plumbs through
(`R/backends.R:188-198`).

Recommended path: take the lower-risk wrapper approach unless the
brms-side investigation confirms the suppression hook is supported.

---

## Review-locked design decisions (2026-06-10)

Stats review (agent run `ace4091850806463d`) and code review (agent run
`a8e4db8054aedc00f`) on this note plus
`brms-threading-composition.md`. The following decisions are locked
and load-bearing for tasks #223-#232:

### Royle-Nichols (RN)

1. **Logit link on `r`** (per-individual detection). Default prior
   `Normal(0, 1.5)` on the intercept. Rationale beyond "matches PB
   nmix and ubms": Uniform(0,1) on `r` propagates to a near-saturated
   prior on `p_visit = 1 - (1-r)^N` for any moderate `lambda`
   (e.g. `E[p_visit | r ~ U(0,1), lambda = 5] ≈ 0.83`). Identity link
   silently pushes the sampler into a ridge where `r → 0` flattens
   the likelihood in `lambda`. Document this rationale in the family
   `@section Identification` block so future contributors do not
   revert to identity for "simplicity".

2. **Two-sided `K_max` saturation rule.** RN needs `K_max[g]` to
   satisfy BOTH (a) `ppois(K_max, lambda_hat, lower.tail=FALSE) <
   1e-4` (Poisson tail negligible) AND (b) `(1 - r_hat)^K_max <
   1e-4` (RN detection function saturated). Condition (b) is
   specific to RN: under PB nmix, large `K_max` only costs compute;
   under RN with low `r`, too-small `K_max` leaves non-negligible
   Poisson mass in cells where the detection function still varies,
   biasing the posterior on `lambda` upward.

3. **Auto-default for RN `K_max[g]`**:
   ```
   K_max[g] = max(
     Y_max[g] + ceiling(-4 / log(1 - 0.3)),   # ≈ Y_max[g] + 11
     3 * max(y_g)
   )
   ```
   Hardcodes `r ≈ 0.3` as the conservative worst-case for the
   saturation condition; the `3 * max(y_g)` floor handles the
   Poisson tail. User-supplied `cap` overrides.

4. **Post-fit saturation diagnostic.** In `summary.mvgam` (or a
   dedicated `check_closure_unit_saturation()` helper invoked from
   `summary`), evaluate `(1 - mean(p_draws))^K_max[g]` per unit and
   `rlang::warn(..., .frequency = "once")` if any unit exceeds
   `0.01`. This is the minimum acceptable guard against the silent
   bias risk in #2.

### Poisson-Poisson (PPM)

5. **Truncated-N likelihood is the ONLY supported form.** Do NOT
   ship a closed-form marginal Poisson(`lambda * p`) alternative.
   The marginal of `y_visit ~ Poisson(N * p), N ~ Poisson(lambda)`
   is the Neyman Type A distribution, which is over-dispersed
   relative to Poisson. The closed-form Poisson is **mean-equivalent
   but distribution-wrong**: it drops the over-dispersion (under-
   estimates variance, produces anti-conservative intervals) and
   blocks `posterior_latent_N()` (no path from a Poisson marginal
   to draws on `N_g`). Task #226 description is being rewritten to
   strike the marginal-Poisson option.

6. **Log link on `p`** (encounter rate on positive real line).
   Reject Bollen's `logit(p)` reparameterisation: under Uniform(0,1)
   on `p`, `logit(p)` implies a Cauchy(0,1)-tailed prior on the
   rate — improper at the upper tail, ecologically unmotivated,
   numerically unsafe.

7. **PPM identifiability guard.** `lambda * p` is the only
   identified product under intercept-only PPM. With covariate
   structure that separates `lambda` from `p`, the Neyman Type A
   variance gives weak cross-identification but the posterior
   ridge along `lambda * p = constant` remains pronounced (Kéry
   2018; "Wild posteriors in the wild" arxiv 2503.00239).
   **At fit time**: if BOTH `mu` and `p` formulae reduce to
   intercept-only, emit a `rlang::warn(...)` stating the ridge
   problem and recommending informative priors on at least one
   intercept.

8. **Tighter PPM default priors.** Default `Normal(0, 1)` on the
   log-scale `p` intercept (implying encounter rate mostly in
   `(0.1, 10)` encounters per individual per visit), tighter than
   the PB nmix default. Document the rationale in the `?nmix`
   family help under a PPM-specific subsection.

### Family-choice diagnostics (PB vs RN)

9. **Do NOT ship a diagnostic that claims to distinguish PB-vs-RN
   misspecification from data alone.** The two are not nested and
   their visit-level variance structures coincide at fixed `N` and
   `r`. Family choice is a **scientific judgement about the
   detection mechanism** (per-individual behaviour vs per-visit
   sampling), not a data-driven question. Document this in the
   `?nmix` help under "Choosing between Poisson-binomial and
   Royle-Nichols".

10. **Optional all-zero PPC.** A `pp_check` on the site-level
    all-zero-history frequency IS defensible — under fixed
    `lambda`, RN and PB nmix predict different all-zero rates as
    a function of abundance. Ship as one of the existing
    `pp_check(..., type = "stat", stat = ...)` examples rather
    than a bespoke method.

### Bollen artifacts confirmed for exclusion

11. The `+ 1` literal on a log-probability in RN's `occ == 0`
    branch (note §3) is a genuine error, not a notation quirk.
    The mvgam port must use clean `bernoulli_lpmf(0 | p_visit)` or
    `log1m(p_visit)` equivalents.

12. The `1e-9` regularisation kludge in PPM (note §4) is a code
    smell driven by Bollen's `logit(p)` reparameterisation. With
    the log link on `p` (decision #6), the `k = 0` branch is a
    clean point mass: `poisson_log_lpmf(0 | log(0))` is
    `-Inf · 1 = -Inf` only when `lambda * p = 0`, which under the
    log link requires `log_p = -Inf` — outside any valid
    parameter draw. No regularisation needed.

### Threading (cross-reference)

13. Per the stats review, the closure-unit grain in `reduce_sum`
    has **no statistical risk** — `log_sum_exp` stays inside a
    single thread per unit, only the outer per-unit sum
    accumulates across threads (floating-point exact addition).
    Performance caveat: warn when `N_unit ≪ threads_per_chain`
    (threading overhead with no benefit). See
    `brms-threading-composition.md` §"Review-locked".

---

## Files referenced

- `/home/nicholas-clark/Desktop/mvgam/R/families.R` — closure-unit
  scaffold (lines 460-540 overview, 543-650 `build_closure_unit_arrays`,
  753-784 `nmix()` constructor, 1106-1169 `nmix_stan_funs`, 1198-1270
  shared stanvars helper).
- `/home/nicholas-clark/Desktop/mvgam/R/log_lik.mvgam.R:161-180` —
  R-side closure-unit log-lik dispatch.
- `/home/nicholas-clark/Desktop/mvgam/R/posterior_predict.R:1515-1520,
  1702-1710` — R-side closure-unit predict dispatch.
- `/home/nicholas-clark/Desktop/mvgam/R/residuals.mvgam.R:262-345` —
  per-unit residual helpers.
- `/home/nicholas-clark/Desktop/mvgam/R/backends.R:182-260` — threading
  plumbing (rstan + cmdstanr backends both honour
  `threads_per_chain`).
