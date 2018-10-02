
generate_input1 <- function(size, rec_field, noise){ # generates inputs matrix of dimension (size, size) that have high mutual 
                                                     # information between the receptive fields regions (the area of which is
                                                     # determined by the receptive_fields arg). Smallest receptive_field_size 
                                                     # is 2 and must be even. Size must be even. Noise is percent of inputs
                                                     # that are flipped from 0 to 1 or 1 to 0.
  inp_mat <- matrix(0, size, size)
  
  
  for(i in 1:sqrt(num_regions)){
    for(h in 1:sqrt(num_regions)){
      inp_mat[(((rec_field*i) - (rec_field-1)):(rec_field*i)), (((rec_field*h) - (rec_field-1)):(rec_field*h))] = random_region_scheme1(rec_field)
    }
  }
  
  inp <- as.vector(inp_mat)
  
  for(b in 1:(noise*(size^2))){
    rand <- sample(c(1:(size^2)), 1, TRUE)
    if(inp[rand] > 0.5){
      inp[rand] = 0
    } else{
      inp[rand] = 1
    }
  }
  
  return(inp)
  
}


MI_region_scheme1 <- function(rec_field){
  
  MI_region_scheme <- matrix(0, rec_field, rec_field)
  
  MI_region_scheme[1,] <- 1
  MI_region_scheme[,1] <- 1
  MI_region_scheme[rec_field,] <- 1
  MI_region_scheme[,rec_field] <- 1
  
  return(MI_region_scheme)
  
}

MI_region_scheme2 <- function(rec_field){
  
  MI_region_scheme <- matrix(0, rec_field, rec_field)
  
  MI_region_scheme[1:(rec_field/2), 1:(rec_field/2)] <- 1
  MI_region_scheme[(rec_field/2+1):rec_field, (rec_field/2+1):rec_field] <- 1

  
  return(MI_region_scheme)
  
}

MI_region_scheme3 <- function(rec_field){
  
  MI_region_scheme <- matrix(0, rec_field, rec_field)
  
  MI_region_scheme[1:ceiling(rec_field*0.25),] <- 1
  
  
  return(MI_region_scheme)
  
}


MI_region_scheme4 <- function(rec_field){
  
  MI_region_scheme <- matrix(0, rec_field, rec_field)
  
  MI_region_scheme[1:(rec_field/2), 1:(rec_field/2)] <- 1
  MI_region_scheme[ceiling(rec_field*0.75):rec_field,] <- 1
  
  
  return(MI_region_scheme)
  
}


random_region_scheme1 <- function(rec_field){
  rand <- sample(c(1,2,3,4), 1, replace=T)
  
  if(rand == 1){
    return(MI_region_scheme1(rec_field))
  }
  
  if(rand == 2){
    return(MI_region_scheme2(rec_field))
  }
  
  if(rand == 3){
    return(MI_region_scheme3(rec_field))
  }
  
  if(rand == 4){
    return(MI_region_scheme4(rec_field))
  }
  
}

