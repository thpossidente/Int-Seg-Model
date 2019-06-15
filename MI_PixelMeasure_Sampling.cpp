#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List averageMIperCluster(List inputMatrices, int windowSize, int stride){
  
  
  // Initizlizing Values // 
  
  NumericMatrix oneMat = inputMatrices[0]; // Setting one sample matrix for easy reference of dims later
  int num_pixels_per_window = windowSize * windowSize;  // Number of pixels in each RF
  int num_matrices = inputMatrices.size();  // number of matrices in the input set
  int num_clusters = (pow((((oneMat.nrow() - windowSize)/(stride)) + 1), 2));  // number of total RFs based on RF size and stride
  
  int num_pixel_combos = 0;       // Initializing then calculating num of pixel combos w/o repeats or comparing pixel to self
  for(int m=1; m<(num_pixels_per_window);m++){
    num_pixel_combos += num_pixels_per_window - m;
  }
  
  NumericMatrix pixelPairs(num_pixel_combos * num_clusters * num_matrices, 2);    // Initialize matrix to store pixel pairs to calculate MI with
  float MI_per_pixel_pair = 0;                                      // Initialize float for average MI for single pixel pair accross all RFs and matrices
  NumericVector MI_all_pixel_pairs(num_pixel_combos);     // Initializing vector to store all pixel combos MIs for summing later
  float total_MI;              // Init value for sum of all pixel pair MIs
  float SD;
  
  int counter_UniquePixelPairs = 0;
  
  Rcout << "num_pixels_per_window: " << num_pixels_per_window << "\n" << 
    "num_matrices: " << num_matrices << "\n" <<
      "num_clusters: " << num_clusters << "\n" <<
        "num_pixel_combos: " << num_pixel_combos << "\n";
  
  
  // Looping through pixel pairs and calculating MI //
  
  for(int h=0; h<(pow(windowSize,2));h++){                       // For each pixel combination possible in a given window
    for(int f=0; f<(pow(windowSize,2));f++){
      int counter = 0;
      
      int originX = 0; // setting origin for scanning across RFs
      int originY = 0;
      
      int pixel_pos1 = h;                                          // Select pixel1 position
      int pixel_pos2 = f;                                          // Select pixel2 position
      
      
      if(h == f){continue;} // Skip calculating comparison b/t same pixel
      if(h > f){continue;} // Skip calculationss comparing pixels that have already been compared.
      
      counter_UniquePixelPairs += 1;
      
      
      for(int b=0;b<num_clusters; b++){          // for each RF possible in a matrix
        
        for(int r=0; r<((inputMatrices.size()));r++){               // for each matrix in the vector of matrices
          
          NumericMatrix mat = inputMatrices[r];       // getting matrix
          
          NumericMatrix matCluster(windowSize, windowSize);  // selecting cluster
          matCluster = mat(Range(originX, (originX+windowSize-1)),
                           Range(originY, (originY+windowSize-1)));
          
          
          NumericVector matClusterVec(pow(windowSize,2));   // flattening cluster into vector (next 7 lines)
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
        
        
        
        if(originY == (oneMat.nrow() - stride)){  // updating origin of RF for next pass
          originY = 0;
          originX += stride;
        } else{
          originY += stride;
        }
        
        
      }
      
      // Calculating MI for one pixel pair //
      
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
      
      for(int v=0; v<4;v++){   // Making sure infinite and NaN values from possible calculations are changed to zeros
        if((R_IsNaN(MI[v])) || (MI[v] == R_PosInf) ){
          MI[v] = 0;
        }
      }
      
      
      MI_per_pixel_pair = sum(MI); // MI of particular pixel combo across all RFs in all matrices
      MI_all_pixel_pairs[counter_UniquePixelPairs] = MI_per_pixel_pair;
      
      
    }
  }
  
  total_MI = sum(MI_all_pixel_pairs);
  
  SD = sum(abs(pow((MI_all_pixel_pairs - mean(MI_all_pixel_pairs)), 2))); // calculating SD
  SD = sqrt(SD / MI_all_pixel_pairs.size());
  
  
  List retrn = List::create(Named("MI") = total_MI,
                            _["SD"] = SD);
  
  return(retrn);
}



