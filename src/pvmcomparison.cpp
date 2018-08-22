#include <Rcpp.h>
using namespace Rcpp;

//' Generate a ROC
//' 
//' @param truth A vector with the truth
//' @param output An order vector with the output of the method. The first elements
//'               represent the items more likely to be TRUE. 
//' @param n_levels Number of unique values in the output 
//' 
//' @return \item{TPR}{Vector with the true positive rate}
//'         \item{FPR}{Vector with the false positive rate} 
//'         \item{AUC}{Area under the curve - based on the values in \code{TPR} and \code{FPR}} 
//' @export 
// [[Rcpp::export]] 
Rcpp::List rocRcpp (Rcpp::LogicalVector truth, Rcpp::NumericVector output, int n_levels) {
  
  int n_cases = truth.size() ;  // number of cases 
  int P = 0 ;                   // number of positive cases
  for (int i = 0; i < n_cases; i ++) {
    if (truth[i]) {
      P ++ ; 
    }
  }
  int N = n_cases - P ;  // number of negative cases
  
  // allocate memory 
  Rcpp::NumericVector TPR (n_levels + 2) ; 
  Rcpp::NumericVector FPR (n_levels + 2) ; 
  
  // initial beginning and end point
  TPR[0] = 0.0 ; 
  FPR[0] = 0.0 ; 
  TPR[n_levels + 1] = 1.0 ; 
  FPR[n_levels + 1] = 1.0 ; 
  
  double TP, FP ; // keeps track of true and false positives 
  double threshold = output[0] ; 
  if (truth[0]) {
    TP = 1.0 ; 
    FP = 0.0 ; 
  } else {
    TP = 0.0 ; 
    FP = 1.0 ; 
  }
  TPR[1] = TP / P ; 
  FPR[1] = FP / N ; 
  
  int index = 2 ; 
  // go through the ordered list of output. whenever the value changes, 
  // update the TPR and the FPR 
  for (int i = 1; i < n_cases; i ++) { 
    if (truth[i]) {
      TP ++ ; 
    } else {
      FP ++ ; 
    }
    
    if (threshold != output[i]) {
      
      threshold = output[i] ; 
      TPR[index] = TP / P ; 
      FPR[index] = FP / N ; 
      
      index ++ ; 
    }
  }
  
  
  // estimate the AUC
  double AUC = 0.0 ; 
  for (int i = 1; i < n_levels + 2; i ++) {
    AUC += (FPR[i] - FPR[i-1]) * (TPR[i-1] + (TPR[i] - TPR[i-1]) / 2.0) ; 
  }
  
  return List::create(Named("truth") = truth, 
                      _("output") = output, 
                      _("TPR") = TPR, 
                      _["FPR"] = FPR,
                      _["AUC"] = AUC);
}

//' Update the Tables with the Smallest Lambda
//' 
//' A function used by the \code{\link{batchLASSO}} function. 
//' It updates the column \code{smallest_lambda} in the data frame \code{tables}
//' with the regression estimates given in \code{beta}. The \code{beta}s are the 
//' result of regressing the drugs on event with id \code{event_id} using 
//' \code{lambda} as the shrinkage parameter.
//' 
//' @param lowest_lambda Vector with the lowest lambdas 
//' @param n_drugs The number of drugs
//' @param event_id The ID of the event
//' @param lambda The shrinkage parameter that was used 
//' @param beta The regression coefficients
//' 
//' @return The \code{tables} with an updated column \code{smallest_lambda} 
//' @export
// [[Rcpp::export]] 
Rcpp::NumericVector updateSmallestLambda (Rcpp::NumericVector lowest_lambda, int n_drugs, int event_id, double lambda, Rcpp::NumericVector beta) {
  // go over the betas
  for (int i = 0; i < n_drugs; i ++) {
     if (beta[i] != 0.0) {
       lowest_lambda[(event_id - 1) * n_drugs + i] = lambda ; 
     }
  }
  
  return lowest_lambda ;
}