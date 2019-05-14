
generate_input1 <- function(size, rec_field, noise, schemes){ # generates inputs matrix of dimension (size, size) that have high mutual 
                                                     # information between the receptive fields regions (the area of which is
                                                     # determined by the receptive_fields arg). Smallest receptive_field_size 
                                                     # is 2 and must be even. Size must be even. Noise is percent of inputs
                                                     # that are flipped from 0 to 1 or 1 to 0.
  inp_mat <- matrix(0, size, size)
  nums <- sample(1:4,((size^2)/(rec_field^2)),replace = TRUE)
  counter <- 0
  
  for(i in 1:(size/rec_field)){
    for(h in 1:(size/rec_field)){
      counter = counter + 1
      inp_mat[(((rec_field*i) - (rec_field-1)):(rec_field*i)), (((rec_field*h) - (rec_field-1)):(rec_field*h))] = schemes[[nums[counter]]]
    }
  }
  
  inp <- as.vector(inp_mat)
  
  if(noise > 0.000001){
    for(b in 1:(noise*(size^2))){
      rand <- sample(c(1:(size^2)), 1, TRUE)
      if(inp[rand] > 0.5){
        inp[rand] = 0
      } else{
        inp[rand] = 1
      }
    }
  }
  
  return(inp)
  
}





generate_MI_matrices <- function(size, rec_field, noise, num_inputs){
  
  matrices <- list()
  schemes <- rand_mat_schemes(size, rec_field)
  
  for(i in 1:num_inputs){
    matrices[[i]] <- matrix(generate_input1(size, rec_field, noise, schemes), nrow = size, ncol = size)
  }
  return(matrices)
}




random_matrix <- function(size, num_inputs){
  
  matrices <- list()
  
  for(i in 1:num_inputs){
    matrices[[i]] = matrix(sample((0:1), (size*size), replace =TRUE), nrow = size, ncol = size)
  }
  
  return(matrices)
}


rand_mat_schemes <- function(size, rec_field){
  matrices <- list()

  for(i in 1:4){
    matrices[[i]] = matrix(sample((0:1), (rec_field*rec_field), replace =TRUE), nrow = rec_field, ncol = rec_field)
    if(i > 1){
      while(matrices[i] %in% matrices[-i]){
        matrices[[i]] = matrix(sample((0:1), (rec_field*rec_field), replace =TRUE), nrow = rec_field, ncol = rec_field)
      }
    }
  }
  return(matrices)
  
}


alt_submat_schemes <- function(size, rec_field){
  matrices <- list()
  
  matrices[[1]] = matrix(0, nrow = rec_field, ncol = rec_field)
  for(i in 1:rec_field){
    for(n in 1:rec_field){
      if(i == 1 || n == 1 || i == rec_field || n == rec_field){
        matrices[[1]][i, n] = 1
      }
      else{
        matrices[[1]][i,n] = 0
      }
    }
  }
  
  matrices[[2]] = matrix(o, nrow = rec_field, ncol = rec_field)
  for(i in 1:rec_field){
    for(n in 1:rec_field){
      if(i == 1 || n == 1 || i == rec_field || n == rec_field){
        matrices[[2]][i, n] = 0
      }
      else{
        matrices[[2]][i,n] = 1
      }
    }
  }
  
  return(matrices)
  
}

generate_alt_matrix <- function(size, rec_field, noise, schemes){
  inp_mat <- matrix(0, size, size)
  nums <- sample(1:2,((size^2)/(rec_field^2)),replace = TRUE)
  counter <- 0
  
  for(i in 1:(size/rec_field)){
    for(h in 1:(size/rec_field)){
      counter = counter + 1
      inp_mat[(((rec_field*i) - (rec_field-1)):(rec_field*i)), (((rec_field*h) - (rec_field-1)):(rec_field*h))] = schemes[[nums[counter]]]
    }
  }
  
  inp <- as.vector(inp_mat)
  
  if(noise > 0.000001){
    for(b in 1:(noise*(size^2))){
      rand <- sample(c(1:(size^2)), 1, TRUE)
      if(inp[rand] > 0.5){
        inp[rand] = 0
      } else{
        inp[rand] = 1
      }
    }
  }
  
  return(inp)
}


generate_alt_MI_matrices <- function(size, rec_field, noise, num__inputs){
  matrices <- list()
  schemes <- alt_submat_schemes(size, rec_field)
  
  for(i in 1:num_inputs){
    matrices[[i]] <- matrix(generate_alt_matrix(size, rec_field, noise, schemes), nrow = size, ncol = size)
  }
  return(matrices)
}



cut_matrix_set <- function(matrices){
  set <- matrices[1:256]
  set <- rep(set, 20)
  return(set)
}


# lapply(test, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))

