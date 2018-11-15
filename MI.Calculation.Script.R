MI.calc <- function(im_size, rec_size0, noise, num){
  rec_size <- c(1,3,13,39)
  output <- vector();
  testing <- generate_MI_matrices(im_size, rec_size0, noise,num)
  for(i in 1:3){
    output[i] = averageMIperPixelPair(testing, rec_size[i], rec_size[i])
  }
  
  return(output)
}


