#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
float averageMIinCluster(List inputMatrices, int clusterSize){

  NumericMatrix oneMat = inputMatrices[0];
  int numClusters = (oneMat.size() / clusterSize^2);
  int num_matrices = inputMatrices.size();
  int counter = 0;
  
  NumericMatrix pixelPairs(num_matrices,2);           // Initialize matrix to store pixel pairs to calculate MI with
  NumericVector MIpixel(clusterSize^2);                       // Initialize vector for MI in each pixel of a cluster
  NumericVector MIcluster(numClusters);                       // Initialize vector for MI in each cluster for each matrix
  NumericVector MIaverageCluster(num_matrices);       // Initialize vector for average MI of clusters in each matrix
  float aveMIperCluster;                                      // Initialie float for average MI for each cluster for all matrices
  
  
  
  for(int x=0; x<(num_matrices);x++){                        // for each matrix in vector of matrices
    NumericMatrix mat = inputMatrices[x];                            // Select matrix
    
    
    for(int i=0; i<(oneMat.size() / clusterSize);i++){               // for each cluster
      for(int j=0; j<(oneMat.size() / clusterSize);j++){ 
        counter += 1;
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
          
          NumericVector prob_pixel0(2);    // Initializing prob vectors used to calculate mutual info
          NumericVector prob_pixel1(2);
          NumericVector joint_prob(4);
          NumericVector MI(4);      // initializing vector for MI per pixel pair to sum up at end
          
          prob_pixel0[0] = (num_matrices - sum(pixelPairs(_,0)))/num_matrices; // prob of 0 occuring in first col
          prob_pixel0[1] = sum(pixelPairs(_,0))/num_matrices;  // prob of 1 occuring in first col
          prob_pixel1[0] = (num_matrices - sum(pixelPairs(_,0)))/num_matrices; // prob of 0 occuring in second col
          prob_pixel1[1] = sum(pixelPairs(_,0))/num_matrices;  // prob of 1 occuring in second col
          
          for(int t=0; t<(num_matrices);t++){                   // calculating # of times 0,0 1,0 0,1 and 1,1 occur
            if(pixelPairs(t, 0) == 0 & pixelPairs(t, 1) == 0){
              joint_prob[0] += 1;
            }
            if(pixelPairs(t, 0) == 1 & pixelPairs(t, 1) == 0){
              joint_prob[0] += 1;
            }
            if(pixelPairs(t, 0) == 0 & pixelPairs(t, 1) == 1){
              joint_prob[0] += 1;
            }
            if(pixelPairs(t, 0) == 1 & pixelPairs(t, 1) == 1){
              joint_prob[0] += 1;
            }
          
          for(int z=0; z<4;z++){
            joint_prob[z] = joint_prob[z]/num_matrices;    //turning frequency into probability for each combination
          }
          
          MI[0] =  (joint_prob[0] * (log2((joint_prob[0])/(prob_pixel0[0]*prob_pixel1[0]))));  // calculating MIs
          MI[1] =  (joint_prob[1] * (log2((joint_prob[1])/(prob_pixel0[1]*prob_pixel1[0]))));
          MI[2] =  (joint_prob[2] * (log2((joint_prob[2])/(prob_pixel0[0]*prob_pixel1[1]))));
          MI[3] =  (joint_prob[3] * (log2((joint_prob[3])/(prob_pixel0[1]*prob_pixel1[1]))));
        }
        MIpixel[h] = sum(MI); // Recording total MI for current pixel
      }
      MIcluster[counter] = sum(MIpixel) / MIpixel.size(); // averaging MI of all pixels in one cluster of one matrix and storing
      }
    }
    MIaverageCluster[x] = sum(MIcluster) / MIcluster.size(); // averaging MI of all clusters in a single matrix and storing
  }
  aveMIperCluster = sum(MIaverageCluster) / MIaverageCluster.size(); // averaging MI per cluster of all matrices and then returning that value
  
  return(aveMIperCluster); 
}


