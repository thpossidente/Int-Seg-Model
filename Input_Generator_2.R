generate_fourbyfourRF_matrices <- function(){
  
  inputs <- list()
  
  twobytwo_blocks <- list(matrix(c(1,1,1,1), nrow = 2, ncol = 2),
                          matrix(c(1,1,1,0), nrow = 2, ncol = 2),
                          matrix(c(1,1,0,1), nrow = 2, ncol = 2),
                          matrix(c(1,0,1,1), nrow = 2, ncol = 2),
                          matrix(c(0,1,1,1), nrow = 2, ncol = 2),
                          matrix(c(1,1,0,0), nrow = 2, ncol = 2),
                          matrix(c(1,0,1,0), nrow = 2, ncol = 2),
                          matrix(c(0,1,1,0), nrow = 2, ncol = 2),
                          matrix(c(1,0,0,1), nrow = 2, ncol = 2),
                          matrix(c(0,1,0,1), nrow = 2, ncol = 2),
                          matrix(c(0,0,1,1), nrow = 2, ncol = 2),
                          matrix(c(1,0,0,0), nrow = 2, ncol = 2),
                          matrix(c(0,1,0,0), nrow = 2, ncol = 2),
                          matrix(c(0,0,1,0), nrow = 2, ncol = 2),
                          matrix(c(0,0,0,1), nrow = 2, ncol = 2),
                          matrix(c(0,0,0,0), nrow = 2, ncol = 2))
  
  fourbyfour_blocks <- list()
  block_order <- sample(1:16 ,16 ,replace = FALSE)
  block_order_lst <- list(block_order[1:4], block_order[5:8], block_order[9:12], block_order[13:16])
  
  for(n in 1:16){  #need 16 4*4 submatrix patterns containing all possible combos of 2*2 matrices.
    fourbyfour <- matrix(0, nrow = 4, ncol = 4)
    
    counter = 0
    
    
    
    for(i in 1:2){
      for(h in 1:2){
        counter = counter + 1
        num_block = block_order_lst[[n]][counter]
        fourbyfour[(((2*i) - (2-1)):(2*i)), (((2*h) - (2-1)):(2*h))] = twobytwo_blocks[[num_block]]
      }
    }
    
    fourbyfour_blocks[[n]] = fourbyfour
  }
  
  
  for(u in 1:256){
    
    input_matrix <- matrix(0, nrow = 16, ncol = 16)
    for(j in 1:(4)){
      for(k in 1:(4)){
        input_matrix[(((4*j) - (4-1)):(4*j)), (((4*k) - (4-1)):(4*k))] = fourbyfour_blocks[[sample(1:4, 1)]]
      }
    }
    
    inputs[[u]] = input_matrix
    
  }
  
  #inputs_final <- rep(inputs, 20)
  
  
  return(inputs)
  
}