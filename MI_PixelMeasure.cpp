#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
float averageMIinCluster(List inputMatrices, int clusterSize){

  NumericMatrix oneMat = inputMatrices[0];
  int numClusters = ((oneMat.size()) / clusterSize^2);
  int num_matrices = inputMatrices.size();
  int counter = 0;

  NumericMatrix pixelPairs(num_matrices,2);           // Initialize matrix to store pixel pairs to calculate MI with
  NumericVector MIpixel(clusterSize^2);                       // Initialize vector for MI in each pixel of a cluster
  NumericVector MIcluster(numClusters);                       // Initialize vector for MI in each cluster for each matrix
  float aveMIperCluster;                                      // Initialie float for average MI for each cluster for all matrices
  

    
  for(int i=0; i<(pow(numClusters, 0.5));i++){               // for each cluster
    for(int j=0; j<(pow(numClusters, 0.5));j++){ 
      counter += 1;
      
      for(int h=0; h<(clusterSize^2);h++){                       // For each pixel combination possible
        for(int f=0; f<(clusterSize^2);f++){
          int pixel_pos1 = h;                                          // Select pixel1 position
          int pixel_pos2 = f;                                          // Select pixel2 position
          

          for(int r=0; r<((inputMatrices.size()));r++){               // for each matrix in the vector of matrices
            NumericMatrix mat = inputMatrices[r];       // getting matrix

            NumericMatrix matCluster(clusterSize, clusterSize);
            matCluster = mat(Range( (0+(clusterSize*i)) , ((clusterSize+(clusterSize*i))-1) ), //getting cluster - somehow incorrect getting weird close to 0 values for everything
                             Range( (0+(clusterSize*j)) , ((clusterSize+(clusterSize*j))-1) ) );

            if(j == 3){
              Rcout << matCluster << "\n";
            }

            }}} /*
            NumericVector matClusterVec(clusterSize^2);   // flatten cluster into vector
            
            for(int w=0; w<(clusterSize);w++){
              for(int d=0; d<(clusterSize); d++){
                matClusterVec[w] = matCluster( w , d );
              }
            } 
            
            int pixel1 = matClusterVec[pixel_pos1]; // Getting pixel1 
            int pixel2 = matClusterVec[pixel_pos2]; // Getting pixel2 
            
            pixelPairs(r, 0) = pixel1;                    // putting pixels into matrix for MI calculation  
            pixelPairs(r, 1) = pixel2;
            
          }
        }
        
        
        NumericVector prob_pixel0(2);    // Initializing probability vectors used to calculate mutual info
        NumericVector prob_pixel1(2);
        NumericVector joint_prob(4);
        NumericVector MI(4);      // initializing vector for MI 
        
          
        prob_pixel0[0] = (num_matrices - sum(pixelPairs(_,0)))/num_matrices; // prob of 0 occuring in first col
        prob_pixel0[1] = sum(pixelPairs(_,0))/num_matrices;  // prob of 1 occuring in first col
        prob_pixel1[0] = (num_matrices - sum(pixelPairs(_,0)))/num_matrices; // prob of 0 occuring in second col
        prob_pixel1[1] = sum(pixelPairs(_,0))/num_matrices;  // prob of 1 occuring in second col
          
        for(int t=0; t<(num_matrices);t++){                   // calculating # of times 0,0 1,0 0,1 and 1,1 occur
          if((pixelPairs(t, 0) == 0) & (pixelPairs(t, 1) == 0)){
            joint_prob[0] += 1;
          }
          if((pixelPairs(t, 0) == 1) & (pixelPairs(t, 1) == 0)){
            joint_prob[1] += 1;
          }
          if((pixelPairs(t, 0) == 0) & (pixelPairs(t, 1) == 1)){
            joint_prob[2] += 1;
          }
          if((pixelPairs(t, 0) == 1) & (pixelPairs(t, 1) == 1)){
            joint_prob[3] += 1;
          }
        }
          
        for(int z=0; z<4;z++){
          joint_prob[z] = joint_prob[z]/num_matrices;    //turning frequency into probability for each combination
        }

        
        MI[0] =  (joint_prob[0] * (log2((joint_prob[0])/(prob_pixel0[0]*prob_pixel1[0]))));  // calculating MIs
        MI[1] =  (joint_prob[1] * (log2((joint_prob[1])/(prob_pixel0[1]*prob_pixel1[0]))));
        MI[2] =  (joint_prob[2] * (log2((joint_prob[2])/(prob_pixel0[0]*prob_pixel1[1]))));
        MI[3] =  (joint_prob[3] * (log2((joint_prob[3])/(prob_pixel0[1]*prob_pixel1[1]))));
        
        MIpixel[h] = sum(MI); // MI of particular pixel combo in particular cluster across all matrices
    }
*/
    MIcluster[counter] = sum(MIpixel); // summing MI of all pixel combos in particular cluster across al matrices
    }
  }
  aveMIperCluster = sum(MIcluster) / (MIcluster.size()); // averaging MI per cluster of all matrices and then returning that value
  
  return(aveMIperCluster);
}
  
