#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List averageMIperCluster(List inputMatrices, int windowSize, int stride){
  
  NumericMatrix oneMat = inputMatrices[0];
  int num_matrices = inputMatrices.size();
  int num_clusters = (pow((((oneMat.nrow() - windowSize)/(stride)) + 1), 2));
  
  

  NumericMatrix pixelPairs(num_clusters * ((pow(windowSize, 2))*2) * num_matrices,2);    // Initialize matrix to store pixel pairs to calculate MI with
  float aveMIperCluster;                                      // Initialie float for average MI for each cluster for all matrices

  int originX = 0;
  int originY = 0;
  
  int counter = 0;
  
  for(int b=0; b<num_clusters;b++){     // for each window
  
    
    for(int h=0; h<(pow(windowSize,2));h++){                       // For each pixel combination possible in a given window
      for(int f=0; f<(pow(windowSize,2));f++){
        int pixel_pos1 = h;                                          // Select pixel1 position
        int pixel_pos2 = f;                                          // Select pixel2 position
        
          
        if(h == f){continue;} // Skip calculating comparison b/t same pixel
        if(h > f){continue;} // Skip calculationss comparing pixels that have already been compared. 
        

        for(int r=0; r<((inputMatrices.size()));r++){               // for each matrix in the vector of matrices
          
          
          NumericMatrix mat = inputMatrices[r];       // getting matrix

          NumericMatrix matCluster(windowSize, windowSize);  // selecting cluster
          matCluster = mat(Range(originX, (originX+windowSize-1)), 
                           Range(originY, (originY+windowSize-1)));
          

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
          
          counter += 1;
          pixelPairs(counter, 0) = pixel1;                    // putting pixels into matrix for MI calculation  
          pixelPairs(counter, 1) = pixel2;
          
        }
      }
    }
    
    if(originY == (oneMat.nrow() - stride)){  // updating origin of cluster for next pass
      originY = 0;
      originX += stride;
    } else{ 
      originY += stride;
    }
    
  }
    
        
  NumericVector prob_pixel0(2);    // Initializing probability vectors used to calculate mutual info
  NumericVector prob_pixel1(2);
  NumericVector joint_prob(4);
  NumericVector MI(4);      // initializing vector for MI 
        
        
  prob_pixel0[0] = (counter - sum(pixelPairs(_,0)))/(counter); // prob of 0 occuring in first col
  prob_pixel0[1] = sum(pixelPairs(_,0))/(counter);  // prob of 1 occuring in first col
  prob_pixel1[0] = (counter - sum(pixelPairs(_,1)))/(counter); // prob of 0 occuring in second col
  prob_pixel1[1] = sum(pixelPairs(_,1))/(counter);  // prob of 1 occuring in second col
  
  
  for(int t=0; t<(counter);t++){                   // calculating # of times 0,0 1,0 0,1 and 1,1 occur
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
    joint_prob[z] = joint_prob[z]/(counter);    //turning frequency into probability for each combination
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
  
  Rcout << joint_prob << "\n" << prob_pixel0 << "\n" << prob_pixel1 << "\n";
    
  aveMIperCluster = sum(MI); // MI of particular pixel combo in particular cluster across all matrices

  List retrn = List::create(Named("Average") = aveMIperCluster);
  
  return(retrn);
}




// giant list of pixel pairs for all clusters at the end 


