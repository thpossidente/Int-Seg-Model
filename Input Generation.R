
generate_input1 <- function(size, rec_field, noise){ # generates inputs matrix of dimension (size, size) that have high mutual 
                                                     # information between the receptive fields regions (the area of which is
                                                     # determined by the receptive_fields arg). Smallest receptive_field_size is 2
  num_regions <- (size^2/rec_field^2)               
    
  inp_mat <- matrix(0, size, size)
    
  for(i in 1:num_regions){
    inp_mat[(((rec_field*i) - (rec_field-1)):(rec_field*i)), (((rec_field*i) - (rec_field-1)):(rec_field*i))] = 
    regions[i][1,] = 1
  }
  
}
