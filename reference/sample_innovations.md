# Process error innovation sampling for trend models

Infrastructure for sampling process errors (innovations) from trend
model distributions. All mvgam trend models use multivariate normal
innovations with different covariance parameterizations.

## Details

This module implements a pattern-based sampling system where trend types
are mapped to one of four covariance patterns:

- `none`: Deterministic trends (PW) - no innovations

- `diagonal`: Independent innovations per series, which a
  `cholesky_scaled` trend takes without `cor`

- `cholesky_scaled`: Correlated via L_Sigma = diag(sigma) \* L_Omega
  (RW, AR, ZMVN, CAR)

- `full_covariance`: Direct covariance matrix Sigma (VAR)

Each trend declares its pattern in the trend registry.
