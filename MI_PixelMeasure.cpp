#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
float averageMIperPixelPair(List inputMatrices, int windowSize, int stride){

  NumericMatrix oneMat = inputMatrices[0];
  int num_matrices = inputMatrices.size();
  int num_clusters = (pow((((oneMat.nrow() - windowSize)/(stride)) + 1), 2));
  

  NumericMatrix pixelPairs(num_matrices,2);           // Initialize matrix to store pixel pairs to calculate MI with
  NumericVector MIcluster(num_clusters);                       // Initialize vector for MI in each cluster for each matrix
  float aveMIperPixelPair;                                      // Initialie float for average MI for each cluster for all matrices
  
  int originX = 0;
  int originY = 0;
  

  
  for(int b=0; b<num_clusters;b++){     // for each window
    int counter = 0;
    NumericVector MIpixel(pow(windowSize,4));                       // Initialize vector for MI in each pixel of a cluster
    
    for(int h=0; h<(pow(windowSize,2));h++){                       // For each pixel combination possible in a given window
      for(int f=0; f<(pow(windowSize,2));f++){
        int pixel_pos1 = h;                                          // Select pixel1 position
        int pixel_pos2 = f;                                          // Select pixel2 position

        for(int r=0; r<((inputMatrices.size()));r++){               // for each matrix in the vector of matrices
          NumericMatrix mat = inputMatrices[r];       // getting matrix
            
          NumericMatrix matCluster(windowSize, windowSize);  // selecting cluster
          matCluster = mat(Range(originX, (originX+stride-1)), 
                           Range(originY, (originY+stride-1)));
          

          NumericVector matClusterVec(pow(windowSize,2));   // flattening cluster into vector
          int counter1 = 0;
          for(int w=0; w<(windowSize);w++){
            for(int d=0; d<(windowSize);d++){
              matClusterVec[counter1] = matCluster( w , d );
              counter1 += 1;
            }
          } 
            
          int pixel1 = matClusterVec[pixel_pos1]; // Getting pixel1 
          int pixel2 = matClusterVec[pixel_pos2]; // Getting pixel2 
          
            
          pixelPairs(r, 0) = pixel1;                    // putting pixels into matrix for MI calculation  
          pixelPairs(r, 1) = pixel2;
            
        }
        
        NumericVector prob_pixel0(2);    // Initializing probability vectors used to calculate mutual info
        NumericVector prob_pixel1(2);
        NumericVector joint_prob(4);
        NumericVector MI(4);      // initializing vector for MI 
        
        
        prob_pixel0[0] = (num_matrices - sum(pixelPairs(_,0)))/num_matrices; // prob of 0 occuring in first col
        prob_pixel0[1] = sum(pixelPairs(_,0))/num_matrices;  // prob of 1 occuring in first col
        prob_pixel1[0] = (num_matrices - sum(pixelPairs(_,1)))/num_matrices; // prob of 0 occuring in second col
        prob_pixel1[1] = sum(pixelPairs(_,1))/num_matrices;  // prob of 1 occuring in second col
        
        
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

        for(int v=0; v<4;v++){
          if((R_IsNaN(MI[v])) || (MI[v] == R_PosInf) ){
            MI[v] = 0;
          }
        }
        
        MIpixel[counter] = sum(MI); // MI of particular pixel combo in particular cluster across all matrices
        counter += 1;
      }
        
    }

  MIcluster[b] = sum(MIpixel) / (MIpixel.size()); // ave MI of all pixel combos in particular cluster across al matrices
  Rcout << MIcluster << '\n';
  
  if(originY == (oneMat.nrow() - stride)){  // updating origin of cluster for next pass
    originY = 0;
    originX += stride;
  } else{ 
    originY += stride;
  }
  }
  
  aveMIperPixelPair = sum(MIcluster) / (MIcluster.size()); // averaging MI per pixel pair for all clusters of all matrices and then returning that value
  
  return(aveMIperPixelPair);
}
  
