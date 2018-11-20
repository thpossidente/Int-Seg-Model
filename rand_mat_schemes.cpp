// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rand_mat_schemes(int size, int rec_field){
  
  List matrices;
  
  for(int i;i<4;i++){
    matrices[i] = matrix(sample(Range(0,1), (rec_field^rec_field), true), nrow = rec_field, ncol = rec_field);
  }
  return matrices;
    
}




