#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
float averageMIinCluster(list inputMatrices, int clusterSize){
  
  int numClusters = (inputMatrix.size() / clusterSize^2);
  
  NumericVector MIave(numClusters);
  NumericVector MI(numClusters * (numClusters - 1));
  
  for(int i=0; i<(inputMatrix.size() / clusterSize);i++){
    for(int h=0; h<(inputMatrix.size() / clusterSize);h++){
      
      for(int r=0; r<(numClusters * (numClusters - 1));r++){
        
      }
    }
  }
  
  
  return(averageMI); 
}


// inp_mat[(((rec_field*i) - (rec_field-1)):(rec_field*i)), (((rec_field*h) - (rec_field-1)):(rec_field*h))]