# Defects the local fixture assertions found

Every entry here is still open. A finding is deleted once its fix is
verified by the assertion that caught it, so the file shrinks as the
work lands rather than accumulating a record of what used to be
wrong.

Each entry names the failing file and line. It then states what the
assertion claims, against what the package does instead. Line numbers
move as the files grow, so each entry also names the test by its
description.

## mvn()

**5. `Psi` has no prior class.**

`mvn()` and `mvt()` estimate one residual scale per component, and
its prior is written into a stanvar. A user who knows their response
scale cannot set the one thing that moves the posterior. Measured
against a truth of 2.0: `gamma(4, 2)` gives 34 divergences at Psi
rhat 1.009, `exponential(1)` gives 225 at 1.084 and `gamma(4, 8)`
gives 335 at 1.396, with two species pulled to 0.9 by a prior centred
on 0.5. No fixed constant suits every response scale. A new default
needs calibrating over a grid of true `Psi` and factor share.

## com_binomial and the trials aterm

**A difference worth recording, for the family work to settle.** The
lower bound on `nu` differs between the two spellings: scalar `nu` is
truncated, reaching Stan as
`normal_lpdf(nu | 1, 1) - normal_lccdf(-5 | 1, 1)`, while a modelled
`nu` gets a plain `normal_lpdf(Intercept_nu | 1, 1)`. Whether an
intercept on the identity scale should carry the scalar's bound is a
question for the family rather than for the axis work.

## Arguments nothing reads

**76. `pp_check()` still takes an argument that reaches no one.**

Every other closed method on the post-fit surface refuses one, and
`tests/local/test-dots-refusal.R` derives that set from the S3
registry at runtime, so a method added later without the guard fails
there. `pp_check()` hands `...` to bayesplot, which names the argument
in a warning and returns the plot built on the default the caller was
overriding. The notice is bayesplot's and leaves if the route to it
changes. `test-grain-mvbf-wide.R` pins it.

## One model, two observation counts

**87. `nobs()` counts the rows supplied rather than the rows fitted,
and its two branches disagree.**

Found while checking what `summary()` prints against what the model
was given. `nobs.mvgam()` returns `nrow(object$data)` and falls back
to `standata$N` when the frame is absent, so the same function
answers with either quantity depending on which slot the object
happens to carry. On a frame with no unobserved cell the two
coincide, which is why this went unseen.

Measured on the two cached fits whose frames carry unobserved cells:

| fit | rows | fitted (`standata$N`) | `nobs()` | `summary()` prints |
|---|---|---|---|---|
| by_lv_axis | 300 | 276 | 300 | 300 |
| occ_visits_gappy | 300 | 250 | 300 | 300 |

`summary.mvgam()` reads `nobs()` for the line a reader takes to be
the size of the analysis, and the comment above that call states the
intent the code does not meet: "How many rows the model was fitted
to, which is what `nobs()` answers". It answers the other one.

brms is the convention mvgam mirrors elsewhere and it counts the
fitted rows: `nobs.brmsfit` is `nrow(model.frame(object))`, and brms
drops the rows whose response is missing. Its signature also takes
`resp`, which mvgam's does not, so a wide fit cannot be asked for one
arm's count.

What is not settled, and is why this is recorded rather than changed:
mvgam requires the frame to be rectangular so the trend grid is
complete, so an unobserved cell is part of the design in a way it is
not for brms. Whether "Number of observations" should name the design
or the likelihood is a decision for whoever owns the printed summary.
What is wrong either way is that one function gives both answers and
its own comment claims the one it does not give.

## What reading a rendered article shows

**80. The VAR article's fit asks for four chains and reports three.**

Found by reading the rendered `vignettes/articles/var.Rmd` rather than
by checking that it rendered. The article knits in 14.1 minutes with
no error and no warning.

The chunk at `var.Rmd:133` reads `chains = 4`, and the `summary()`
printed underneath it says "Draws: 3 chains" with 4500 post-warmup
draws, which is 3 x 1500 exactly. So three chains are what the numbers
rely on. `summary()` is not miscounting: fitted at 2, 3 and 4 chains
it reports 2, 3 and 4 and `ndraws()` agrees each time. A chain was
therefore lost during this fit and nothing said so, with `silent = 2`
covering whatever was raised. A quarter of a posterior leaving without
a word is worth a message the caller cannot suppress by asking for a
quiet fit. Settling it needs the article re-rendered.

**82. `posterior_summary()` prints each arm's intercept twice, and
the mvbf article corrects a sign nothing makes indeterminate.**

Both were found by reading the rendered `vignettes/articles/mvbf.Rmd`.

`posterior_summary()` on a multivariate fit carries two spellings of
every arm's intercept, `Intercept_<r>` and `b_<r>_Intercept`, with
nothing to say they sit on different scales. That is brms's centred
parameterisation rather than an mvgam fault. `Intercept_<r>` is the
intercept at the covariate mean and `b_<r>_Intercept` the intercept at
zero, so the two part company by the slope times the covariate mean.
On the article's camera arm, whose covariate `deploy_days` is drawn
`Unif(5, 20)` and not centred, the two read 0.007 and -0.882 against
a truth of -0.5. The article now reads only the `b_` spelling and
says why, but a reader of the table the package prints still has two
rows and no guide.

`recovery_summary()` multiplies each posterior by
`sign(cor(med, truth$x))`, explaining that "latent factor models
identify the trend only up to sign". None of the four fits is a
factor model. Each is an AR(1) state with an identified intercept, so
the sign is identified and the correction has nothing to fix. What it
does instead is guarantee a non-negative correlation with the truth
for every fit in the table, which can only move RMSE downward. The
comparison it feeds is the article's headline claim that the joint
fit recovers the state best.

## Debt the code carries in recognisable shapes

**89. Ten shapes account for the defects found so far, and a scan
counts six of them.**

Each shape leaves a mark in the source that a scan can find.
`tests/local/debt_scan.R` reads parse data to count the marks in
`R/`. Several shapes include false positives: the unused-argument
scan counts dispatch kernels that share a signature (`log_lik_*`
taking `trials`) and generics such as `methods_md()`, and each hit is
read before anything is removed.

| shape | the mark it leaves | count |
|---|---|---|
| one fact, several derivers | raw `[[series_var]]` / `[[time_var]]` reads; `sort(unique(...))` axis rebuilds; `inherits(..., "mvbrmsformula")` asked in place of the question meant | 53, 40, 58 |
| a literal standing in for a missing value | `%||% "y"`, `%||% "series"`, `%||% "explicit"` | 118 |
| a missing column skipped | `intersect(x, names(data))`, `if (!col %in% names(df)) next` | 21 |
| a warning silenced, not traced | `suppressWarnings()`, `suppressMessages()` | 12 |
| an argument nothing reads | accepted, asserted, never used; the scan also flags `df` on `AR()`, `RW()`, `CAR()` and `ZMVN()`, finding 6's shape if it holds | 113 non-S3 functions |
| a stored copy of a derivable fact | object slots and metadata fields written once and read in a few places | not counted |
| one condition, several refusals | the same fault refused with different wording at different layers | not counted |
| a proxy for the question meant | "the frame has no series column" standing for "the responses are the series"; `length(x) > 1` standing for "multivariate" | found by reading |

The tests carry the same debt. A stub that fakes a class with
`structure(y ~ x, class = c("brmsformula", "formula"))` lets an
assertion pass on an object no user could build. So does a stub
carrying slots a real fit no longer has. Assertions that compare
counts or use
`expect_setequal()` pass where the claim being tested is an order or
a value.

The scans are cheap, and a count falls only when code is deleted.
Each remaining shape gets one pass. A pass removes the rival, the
fallback or the proxy, adds an assertion that fails before the change
and records the count before and after.


