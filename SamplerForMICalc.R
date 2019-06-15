
# Grabs 1 random RF size patch from X number of randomly chosen matrices (determined by total_samples) 
# Calls to MI_PixelMeasure.cpp to calculate MI per RF of those samples
# Outputs MI per RF for graphing later

sampling_MI_calc <- function(matrices, RF_size, stride, batch_number, samples_per_batch){
  
  total_samples <- batch_number*samples_per_batch      
  
  mat_indices_to__sample <- sample(length(matrices), total_samples, replace = FALSE) # randomly getting indices of matrices to sample
  x_mat_coords1 <- sample((nrow(matrices[[1]]) - RF_size), total_samples, replace = TRUE)  # Getting random x and y coordinates to sample random 
  y_mat_coords1 <- sample((ncol(matrices[[1]]) - RF_size), total_samples, replace = TRUE)  # RF sized patches
  
  x_mat_coords2 <- sample((nrow(matrices[[1]]) - RF_size), total_samples, replace = TRUE)  # Getting random x and y coordinates to sample random 
  y_mat_coords2 <- sample((ncol(matrices[[1]]) - RF_size), total_samples, replace = TRUE)  # RF sized patches
  counter <- total_samples
  
  samples <- list()
  
  for(s in 1:total_samples){
    single_mat <- matrices[[mat_indices_to__sample[s]]]
    x_coord = x_mat_coords1[s]
    y_coord = y_mat_coords1[s]
    single_sample <- single_mat[x_coord:(x_coord+RF_size-1),
                                y_coord:(y_coord+RF_size-1)]
    
    samples[[s]] = single_sample
  }
  
  for(s2 in 1:total_samples){
    single_mat <- matrices[[mat_indices_to__sample[s2]]]
    x_coord = x_mat_coords2[s2]
    y_coord = y_mat_coords2[s2]
    single_sample <- single_mat[x_coord:(x_coord+RF_size-1),
                                y_coord:(y_coord+RF_size-1)]
    counter = counter + 1
    samples[[counter]] = single_sample
  }
  
  
  MI <- averageMIperCluster(samples, RF_size, stride)[1]
  
  return(MI)
  
}


MI_samples_for_graphing <- function(matrices, RF_size, stride, samples_per_batch, number_batches){
  
  MIs <- list()
  
  for(n in 1:number_batches){
    MIs[n] = sampling_MI_calc(matrices, RF_size, stride, n, samples_per_batch)
  }
  
  return(MIs)
}








