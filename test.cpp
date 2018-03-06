#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
float test(NumericVector arg){
  float res = which_max(arg);
  return res;
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


