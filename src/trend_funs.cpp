// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <cmath>
#define _USE_MATH_DEFINES
#include <math.h>
using namespace Rcpp;

// function for recursively extending an AR3 process
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector ar3_recursC(double drift, double ar1,
                         double ar2, double ar3,
                         Rcpp::NumericVector linpreds,
                         Rcpp::NumericVector errors,
                         Rcpp::NumericVector last_trends, int h) {

  int T = h + 3;
  Rcpp::NumericVector states(T);
  states[1] = last_trends[1];
  states[2] = last_trends[2];
  states[3] = last_trends[3];

  for(int t = 3; t < T; ++t) {
    states[t] = drift +
      ar1 * (states[t - 1] - linpreds[t - 1]) +
      ar2 * (states[t - 2] - linpreds[t - 2]) +
      ar3 * (states[t - 3] - linpreds[t - 3]) +
      linpreds[t]  +
      errors[t];
  }
  return states[Rcpp::Range(3, T-1)];
}

// function for recursively extending an VAR1 process
//' @noRd
// [[Rcpp::export]]
arma::mat var1_recursC(arma::mat A,
                       arma::mat linpreds,
                       arma::mat errors,
                       arma::rowvec drift,
                       arma::rowvec last_trends,
                       int h) {

  int T = h + 1;
  int n_series = A.n_rows;
  arma::mat states(T, n_series);
  states.row(0) = last_trends;
  for (int t = 1; t < T; t++) {
    states.row(t) = (states.row(t-1) - linpreds.row(t-1)) * trans(A) +
                     linpreds.row(t) + drift + errors.row(t);
  }
  return states.rows(1, h);
}
