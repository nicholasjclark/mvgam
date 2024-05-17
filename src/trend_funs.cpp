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
  states(0) = last_trends(0);
  states(1) = last_trends(1);
  states(2) = last_trends(2);

  for(int t = 3; t < T; ++t) {
    states(t) = drift +
      ar1 * (states(t - 1) - linpreds(t - 1)) +
      ar2 * (states(t - 2) - linpreds(t - 2)) +
      ar3 * (states(t - 3) - linpreds(t - 3)) +
      linpreds(t)  +
      errors(t);
  }
  return states[Rcpp::Range(3, T-1)];
}

// function for recursively extending a VAR1 process
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

// function for recursively extending a VARMA(1-3,0-1) process
//' @noRd
// [[Rcpp::export]]
arma::mat varma_recursC(
    arma::mat A,
    arma::mat A2,
    arma::mat A3,
    arma::mat theta,
    arma::mat linpreds,
    arma::mat errors,
    arma::rowvec drift,
    arma::mat last_trends,
    int h) {

   // total number of timepoints
   int T = h + 3;

   // total number of series
   int n_series = A.n_rows;

   // states
   arma::mat states(T, n_series);

   // initialise states
   states.row(0) = last_trends.row(0);
   states.row(1) = last_trends.row(1);
   states.row(2) = last_trends.row(2);

   // VARMA(3,1) process
   for (int t = 3; t < T; t++) {
     states.row(t) =
       // autoregressive means
       (states.row(t-1) - linpreds.row(t-1)) * trans(A) +
       (states.row(t-2) - linpreds.row(t-2)) * trans(A2) +
       (states.row(t-3) - linpreds.row(t-3)) * trans(A3) +

       // moving averages
       errors.row(t-1) * trans(theta) +

       // linear predictor contributions
       linpreds.row(t) +

       // drift terms
       drift +

       // stochastic errors
       errors.row(t);
   }
   return states.rows(3, T-1);
 }
