#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

List forwardPass(NumericVector input, NumericMatrix inputToHiddenWeights, NumericVector hiddenBiasWeights, NumericMatrix hiddenToOutputWeights, NumericVector outputBiasWeights){
  
  NumericVector hidden;
  for(int i=0; i<hidden.length(); i++){
    for(int j=0; j<input.length(); j++){
      if(inputToHiddenWeights[j,i] != NAN) {
        hidden[i] += input[j] * inputToHiddenWeights[j,i];
      }
    }
  }
  
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
