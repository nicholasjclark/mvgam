// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <cmath>
#define _USE_MATH_DEFINES
#include <math.h>
using namespace Rcpp;

// General ARMA(p,q) recurser supporting:
//   - sparse AR lag indices (e.g. AR(p = c(1, 3, 12)))
//   - sparse MA lag indices
//   - univariate (dim = 1) and multivariate (VAR / VARMA, dim > 1)
//   - cross-dimension correlation via pre-drawn innovation covariance
//   - obs-side linear predictor offsets via the centred convention
//     (matches brms Stan code: AR operates on trend - linpred)
//
// Used by both sim_mvgam (linpreds = zero, last_trends from burn-in)
// and the future forecast.mvgam (linpreds from obs side, last_trends
// from posterior draws of latent states).
//
// Lag-history contract: the kernel seeds a "prefix" region of length
// `max_lag = max(max(ar_lags), max(ma_lags))` at the start of the
// internal state buffer. The caller MUST supply input matrices with
// at least `h + max_lag` rows so the recursion at the first forecast
// step (t = max_lag) can read history at any positive lag without
// going out of bounds.
//
// Inputs:
//   ar_lags     length-m_a integer vector of active AR lag indices
//               (1-based; e.g. c(1, 3, 12)). Empty for q-only models.
//   ma_lags     length-m_b integer vector of active MA lag indices
//               (1-based). Empty for AR-only models.
//   drift       length-dim per-series drift terms.
//   A           [dim, dim, m_a] cube of AR coefficients, one slice
//               per active AR lag.
//   B           [dim, dim, m_b] cube of MA coefficients, one slice
//               per active MA lag.
//   innovations [h + max_lag, dim] pre-drawn errors. Rows
//               0..max_lag-1 hold history (caller's responsibility:
//               for sim, fill with N(0, Sigma) draws so burn-in is
//               correct; for forecast.mvgam, fill with the posterior
//               estimate of the past `max_lag` innovations). Rows
//               max_lag..(h + max_lag - 1) are the new innovations
//               driving each forecast step.
//   linpreds    [h + max_lag, dim] obs-side linear predictor offsets
//               aligned 1:1 with the trend index. Zero matrix for
//               sim; posterior obs linpred rows for forecast.
//   last_trends [max_ar, dim] initial trend values, oldest row
//               first. These seed positions
//               (max_lag - max_ar)..(max_lag - 1) of the state
//               buffer. Empty matrix (0 x dim) when pure MA.
//   h           forecast horizon.
//
// Returns: [h, dim] matrix of trend trajectories (oldest -> newest).
//' @noRd
// [[Rcpp::export]]
arma::mat trend_arma_recursC(
    const arma::ivec& ar_lags,
    const arma::ivec& ma_lags,
    const arma::vec& drift,
    const arma::cube& A,
    const arma::cube& B,
    const arma::mat& innovations,
    const arma::mat& linpreds,
    const arma::mat& last_trends,
    int h) {

  const int dim = drift.n_elem;
  const int m_a = ar_lags.n_elem;
  const int m_b = ma_lags.n_elem;
  const int max_ar = (m_a > 0) ? ar_lags.max() : 0;
  const int max_ma = (m_b > 0) ? ma_lags.max() : 0;
  const int max_lag = std::max(max_ar, max_ma);
  const int T = h + max_lag;

  // Input-shape validation: callers must size innovations + linpreds
  // to T rows. Catch silently-wrong inputs before the loop reads
  // uninitialised memory.
  if (static_cast<int>(innovations.n_rows) != T) {
    Rcpp::stop("trend_arma_recursC: 'innovations' must have h + "
               "max(max(ar_lags), max(ma_lags)) rows.");
  }
  if (static_cast<int>(linpreds.n_rows) != T) {
    Rcpp::stop("trend_arma_recursC: 'linpreds' must have h + "
               "max(max(ar_lags), max(ma_lags)) rows.");
  }
  if (m_a > 0 &&
      static_cast<int>(last_trends.n_rows) != max_ar) {
    Rcpp::stop("trend_arma_recursC: 'last_trends' must have "
               "max(ar_lags) rows.");
  }

  // Pre-transpose A and B slices once; the hot loop reads transposed
  // slices to compute (centred-row) * A.slice(k).t().
  arma::cube At(dim, dim, m_a, arma::fill::none);
  for (int k = 0; k < m_a; ++k) {
    At.slice(k) = A.slice(k).t();
  }
  arma::cube Bt(dim, dim, m_b, arma::fill::none);
  for (int j = 0; j < m_b; ++j) {
    Bt.slice(j) = B.slice(j).t();
  }
  const arma::rowvec drift_row = drift.t();

  // State buffer: max_lag rows of seed + h rows of forecast. Seed
  // region (rows 0..max_lag-1) is zero by default; the trailing
  // max_ar rows of the seed are overwritten with `last_trends` so
  // AR history at any lag <= max_ar reads the supplied values.
  arma::mat states(T, dim, arma::fill::zeros);
  if (max_ar > 0) {
    states.rows(max_lag - max_ar, max_lag - 1) = last_trends;
  }

  for (int t = max_lag; t < T; ++t) {
    arma::rowvec next_state = linpreds.row(t) + drift_row;

    // Centred AR contribution:
    //   A_k * (trend[t - lag_k] - linpred[t - lag_k])
    for (int k = 0; k < m_a; ++k) {
      const int idx = t - ar_lags.at(k);
      const arma::rowvec centred =
        states.row(idx) - linpreds.row(idx);
      next_state += centred * At.slice(k);
    }

    // MA contribution: B_j * errors[t - lag_j]. `idx` is guaranteed
    // >= 0 because t >= max_lag >= max_ma >= ma_lags(j) for all j.
    for (int j = 0; j < m_b; ++j) {
      const int idx = t - ma_lags.at(j);
      next_state += innovations.row(idx) * Bt.slice(j);
    }

    // Current innovation.
    next_state += innovations.row(t);

    states.row(t) = next_state;
  }

  // Drop the prefix rows; return only the h forecast steps.
  return states.rows(max_lag, T - 1);
}


// Continuous-time AR(1) recurser. CAR(1) cannot be reduced to the
// general ARMA kernel because the AR coefficient depends on the
// observation time gap (phi^Δt, not phi).
//
// Conditional distribution at lag Δt (stationary CAR(1) form):
//   trend[t] = phi^Δt * trend[t-1]
//            + sigma * sqrt((1 - phi^(2*Δt)) / (1 - phi^2)) * z[t]
// where z[t] ~ N(0, 1) is the standardised innovation. The
// denominator `(1 - phi^2)` is the stationary variance factor; it
// is invariant to Δt and is precomputed once outside the loop.
//
// Stationarity guards (kernel rejects with Rcpp::stop):
//   - phi must lie strictly in (0, 1). phi = 1 is the random-walk
//     boundary where the stationary formula collapses to 0/0;
//     phi <= 0 has no real-valued continuous-time interpretation
//     for fractional Δt because `(-x)^Δt` is undefined in R for
//     non-integer Δt. mvgam's CAR(1) prior enforces phi in (0, 1)
//     so well-formed callers never hit these branches; the guard
//     catches misuse.
//   - sigma must be strictly positive.
//
// Zero-gap guard: time_dis[t] is floored at 1e-3 to avoid the noise
// term collapsing when two observations land at the same rounded
// time. Matches the reference threshold in R/trend_system.R:2271.
//
// Inputs:
//   phi          length-n_series per-series autocorrelation in (0, 1).
//   sigma        length-n_series per-series innovation SD > 0.
//   time_dis     length h time gaps Δt for the forecast steps.
//                time_dis(0) is the gap from last_trend's
//                observation to the FIRST forecast step.
//   innovations  [h, n_series] pre-drawn N(0,1) innovations.
//   last_trend   length-n_series last observed latent value.
//   h            forecast horizon.
//
// Returns: [h, n_series] matrix of forecasted trend trajectories.
//' @noRd
// [[Rcpp::export]]
arma::mat car1_recursC(
    const arma::vec& phi,
    const arma::vec& sigma,
    const arma::vec& time_dis,
    const arma::mat& innovations,
    const arma::vec& last_trend,
    int h) {

  const int n_series = phi.n_elem;
  const double min_dt = 1e-3;

  // Stationarity + positivity guards. CAR(1)'s stationary formula
  // is undefined outside the (0, 1) phi range; sigma <= 0 produces
  // degenerate (zero-variance) draws.
  if (phi.min() <= 0.0 || phi.max() >= 1.0) {
    Rcpp::stop("car1_recursC: 'phi' must lie strictly in (0, 1) for "
               "stationary CAR(1); received value outside that "
               "range.");
  }
  if (sigma.min() <= 0.0) {
    Rcpp::stop("car1_recursC: 'sigma' must be strictly positive.");
  }
  if (static_cast<int>(time_dis.n_elem) != h) {
    Rcpp::stop("car1_recursC: 'time_dis' must have length h.");
  }
  if (static_cast<int>(innovations.n_rows) != h ||
      static_cast<int>(innovations.n_cols) != n_series) {
    Rcpp::stop("car1_recursC: 'innovations' must have shape "
               "[h, n_series].");
  }
  if (static_cast<int>(last_trend.n_elem) != n_series) {
    Rcpp::stop("car1_recursC: 'last_trend' must have length "
               "n_series.");
  }

  const arma::vec stat_var_denom = 1.0 - phi % phi;

  arma::mat states(h, n_series, arma::fill::none);
  // `prev` is a row vector throughout to align with row-major
  // updates of `states`; avoids two transposes per iteration.
  arma::rowvec prev = last_trend.t();
  arma::rowvec phi_dt(n_series, arma::fill::none);
  arma::rowvec sd_step(n_series, arma::fill::none);

  for (int t = 0; t < h; ++t) {
    const double dt = std::max(time_dis.at(t), min_dt);

    for (int s = 0; s < n_series; ++s) {
      const double phi_s = phi.at(s);
      phi_dt.at(s) = std::pow(phi_s, dt);
      const double var_factor =
        (1.0 - std::pow(phi_s, 2.0 * dt)) / stat_var_denom.at(s);
      sd_step.at(s) = sigma.at(s) * std::sqrt(var_factor);
    }

    const arma::rowvec next_row =
      phi_dt % prev + sd_step % innovations.row(t);
    states.row(t) = next_row;
    prev = next_row;
  }

  return states;
}


// Piecewise trend extrapolation (Prophet-style linear or logistic).
//
// Closed-form evaluation of a piecewise-linear or piecewise-
// logistic trend at user-supplied times. Unlike the AR / CAR
// kernels, the result is deterministic given parameters --
// posterior uncertainty in the forecast comes from sampling
// (k, m, delta) across draws, not from stochastic innovations.
//
// The kernel evaluates whatever changepoint schedule is
// passed in `t_change` / `delta`. Per-draw horizon-changepoint
// sampling (Prophet-style Poisson(change_freq) for the count,
// Laplace(0, median(|delta|)) for each new rate-change, and
// uniform-in-horizon for the new times) happens in the R
// caller before invoking this kernel. That separation keeps
// the C++ deterministic and testable while leaving the
// sampling step inspectable / replaceable in R.
//
// Inputs:
//   t           length-h vector of forecast times.
//   k           length-n_series vector of base growth rates.
//   m           length-n_series vector of intercepts.
//   delta       [n_change, n_series] rate-change magnitudes
//               at each changepoint.
//   t_change    length-n_change vector of changepoint times
//               (fixed at fit time; same for every draw).
//   cap         [h, n_series] carrying capacities on the link
//               scale; required for `growth_type = "logistic"`
//               and ignored for `"linear"`.
//   growth_type "linear" or "logistic".
//
// Returns: [h, n_series] matrix of trend values on the link
// scale.
//
// Math (linear):
//   y(t, s) = (k[s] + A %*% delta[, s]) * t
//           + (m[s] + A %*% (-t_change * delta[, s]))
//   where A[i, c] = 1 iff t[i] >= t_change[c].
//
// Math (logistic, with piecewise-continuity gamma):
//   k_cum[1]   = k[s]
//   k_cum[c+1] = k_cum[c] + delta[c, s]
//   gamma[i]   = (t_change[i] - m[s] - sum(gamma[1:i-1]))
//                * (1 - k_cum[i] / k_cum[i+1])
//   y(t, s)    = cap[t, s] /
//                (1 + exp(-(k[s] + A %*% delta[, s])
//                         * (t - (m[s] + A %*% gamma))))
// [[Rcpp::export]]
arma::mat pw_trendC(
    const arma::vec& t,
    const arma::vec& k,
    const arma::vec& m,
    const arma::mat& delta,
    const arma::vec& t_change,
    const arma::mat& cap,
    const std::string& growth_type) {

  const int h = t.n_elem;
  const int n_series = k.n_elem;
  const int n_change = t_change.n_elem;

  if (static_cast<int>(m.n_elem) != n_series) {
    Rcpp::stop("pw_trendC: 'm' must have length n_series.");
  }
  if (static_cast<int>(delta.n_rows) != n_change ||
      static_cast<int>(delta.n_cols) != n_series) {
    Rcpp::stop(
      "pw_trendC: 'delta' must be [n_changepoints, n_series]."
    );
  }

  // Changepoint indicator matrix A[t_idx, c] = 1 iff
  // t[t_idx] >= t_change[c]. Built once and reused per series.
  arma::mat A(h, n_change, arma::fill::zeros);
  for (int ti = 0; ti < h; ++ti) {
    for (int c = 0; c < n_change; ++c) {
      if (t.at(ti) >= t_change.at(c)) {
        A.at(ti, c) = 1.0;
      }
    }
  }

  arma::mat out(h, n_series, arma::fill::zeros);

  if (growth_type == "linear") {
    for (int s = 0; s < n_series; ++s) {
      const arma::vec delta_s = delta.col(s);
      const arma::vec gamma_lin = -t_change % delta_s;
      const arma::vec k_t = k.at(s) + A * delta_s;
      const arma::vec m_t = m.at(s) + A * gamma_lin;
      out.col(s) = k_t % t + m_t;
    }
    return out;
  }

  if (growth_type == "logistic") {
    if (static_cast<int>(cap.n_rows) != h ||
        static_cast<int>(cap.n_cols) != n_series) {
      Rcpp::stop(
        "pw_trendC: 'cap' must be [h, n_series] for logistic."
      );
    }
    for (int s = 0; s < n_series; ++s) {
      const arma::vec delta_s = delta.col(s);

      // Cumulative growth rate at each changepoint boundary.
      arma::vec k_cum(n_change + 1);
      k_cum.at(0) = k.at(s);
      for (int c = 0; c < n_change; ++c) {
        k_cum.at(c + 1) = k_cum.at(c) + delta_s.at(c);
      }

      // Gamma updates enforce piecewise continuity at each
      // changepoint. The sum-of-prior-gammas term is
      // accumulated as we go.
      //
      // The continuity correction degenerates when:
      //   * k_cum[i+1] is near zero (we divide by it), or
      //   * k_cum[i] and k_cum[i+1] differ in sign (the
      //     ratio is negative and the "1 - ratio" term over-
      //     corrects).
      //
      // Prophet does not guard against either case; it just
      // accepts the resulting discontinuity. We skip the
      // correction (gamma_i = 0) in both degenerate regimes
      // so the trend stays finite, matching the practical
      // behavior in Taylor & Letham (2018).
      const double k_eps = 1e-10;
      arma::vec gamma_s(n_change, arma::fill::zeros);
      double sum_gamma = 0.0;
      for (int i = 0; i < n_change; ++i) {
        const double k_curr = k_cum.at(i);
        const double k_next = k_cum.at(i + 1);
        if (std::abs(k_next) < k_eps ||
            k_curr * k_next < 0.0) {
          gamma_s.at(i) = 0.0;
        } else {
          gamma_s.at(i) =
            (t_change.at(i) - m.at(s) - sum_gamma) *
            (1.0 - k_curr / k_next);
        }
        sum_gamma += gamma_s.at(i);
      }

      const arma::vec k_t = k.at(s) + A * delta_s;
      const arma::vec m_t = m.at(s) + A * gamma_s;
      out.col(s) = cap.col(s) / (1.0 + arma::exp(-k_t % (t - m_t)));
    }
    return out;
  }

  Rcpp::stop(
    "pw_trendC: 'growth_type' must be 'linear' or 'logistic'."
  );
}
