#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
float averageMIinCluster(List inputMatrices, int clusterSize){

  NumericMatrix oneMat = inputMatrices[0];
  int numClusters = (oneMat.size() / clusterSize^2);
  
  NumericMatrix pixelPairs(inputMatrices.size(),2);           // Initialize matrix to store pixel pairs to calculate MI with
  NumericVector MIpixel(clusterSize^2);                       // Initialize vector for MI in each pixel of a cluster
  NumericVector MIcluster(numClusters);                       // Initialize vector for MI in each cluster for each matrix
  NumericVector MIaverageCluster(inputMatrices.size());       // Initialize vector for average MI of clusters in each matrix
  float aveMIperCluster;                                      // Initialie float for average MI for each cluster for all matrices
  
  
  for(int x=0; x<(inputMatrices.size());x++){                        // for each matrix in vector of matrices
    NumericMatrix mat = inputMatrices[x];                            // Select matrix
    
    for(int i=0; i<(oneMat.size() / clusterSize);i++){               // for each cluster
      for(int j=0; j<(oneMat.size() / clusterSize);j++){ 
        NumericMatrix clusterMat = mat( Range( (0+clusterSize*i), (clusterSize+clusterSize*i) ), 
                                        Range( (0+clusterSize*j), (clusterSize+clusterSize*j)) );
        NumericVector cluster = as<NumericVector>(clusterMat);      // Select cluster within mat and make it a vector
        
        for(int h=0; h<(clusterSize^2);h++){                       // for each pixel in the cluster
          int pixel = h;                                           // Select pixel position
          
          for(int r=0; r<(oneMat.size());r++){               // for each matrix in the vector of matrices
            pixelPairs(r, 0) = cluster[h];                   // first of pixel pair will always be current pixel under examination
            NumericMatrix compare3 = inputMatrices[r];       // Getting  pixel in same position from each of all other matrices
            NumericMatrix compare2 = compare3( Range( (0+clusterSize*i), (clusterSize+clusterSize*i) ), 
                                               Range( (0+clusterSize*j), (clusterSize+clusterSize*j)) );
            NumericVector compare1 = as<NumericVector>(compare2);
            int compare = compare1[h];                     
            
            pixelPairs(r, 1) = compare;                      // second of pixel pair is pixel in same position from every other matrix
            
            // Calculate MI of this pixel pair here and put it in MIpixel then move to next pixel in cluster.
            // After all pixels of a given cluster, average MIpixel and put into MICluster, then move to next cluster.
            // After all clusters of a given matrix, average MIcluster and put result into MIaverageCluster, then move to next matrix.
            // After all matrices, average MIaverageCluster and result is aveMIperCluster, then return that value
            
          }
        }
      }
    }
  }
  
  return(aveMIperCluster); 
}


// inp_mat[(((rec_field*i) - (rec_field-1)):(rec_field*i)), (((rec_field*h) - (rec_field-1)):(rec_field*h))]