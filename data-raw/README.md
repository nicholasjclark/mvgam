# data-raw

Build scripts for the stock datasets shipped under `data/`. Each
script is fully deterministic (fixed seed) and writes a single
`.rda` file. Run a script from the package root:

```r
source("data-raw/build_birdsong.R")
source("data-raw/build_lake_chemistry.R")
source("data-raw/build_coral_surveys.R")
```

The scripts reuse low-level primitives from `R/sim_helpers.R`
(`sim_smooth`, `sim_re`, `rmvn`, family RNGs) but the generative
recursion / structural math for each dataset is spelled out
explicitly so the truth is auditable.

| Script | Output | Generative structure |
|---|---|---|
| `build_birdsong.R` | `data/birdsong.rda` | 4 species x 80 weeks, Poisson, shared cyclic seasonal smooth + species-specific AR(1) trends (2 declining, 2 increasing) + species random intercept |
| `build_lake_chemistry.R` | `data/lake_chemistry.rda` | 5 lakes x 60 months, Gaussian, per-lake random-walk trends + binary treatment with random slope (2 lakes positive, 3 lakes negative) |
| `build_coral_surveys.R` | `data/coral_surveys.rda` | 3 reefs x ~50 obs at irregular times (gap ~ Uniform(1, 6) months), Gaussian, per-reef CAR(1) (phi = 0.7, sigma = 0.5) + smooth SST effect |
