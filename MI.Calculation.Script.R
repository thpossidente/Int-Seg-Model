MI.calc <- function(im_size, rec_size0, noise, num){
  rec_size <- c(1,2,4,8,16)
  output <- vector();
  testing <- generate_MI_matrices(im_size, rec_size0, noise, num)
  for(i in 1:(length(rec_size))){
    output[i] = averageMIperCluster(testing, rec_size[i], rec_size[i])
  }
  
  return(output)
}


