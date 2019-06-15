

## Loading in Dataset ##

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  test <<- load_image_file('Binary_Datasets_Testing/Datasets/Fashion_MNIST/t10k-images-idx3-ubyte')
  
  test$y <<- load_label_file('Binary_Datasets_Testing/Datasets/Fashion_MNIST/t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}





## Converting continuous to binary  ##

make_binary <- function(image){
  for(value in 1:length(image)){
    if(image[value] >= 50){
      image[value] <- 1
    } else{image[value] = 0}
  }
  return(image)
}

test$x <- apply(test$x, 2, make_binary)


## Converting to list of matrices ##


images <- list()

for(row in 1:nrow(test$x)){
  images[[row]] = matrix(test$x[row,], nrow = 28, ncol = 28)
}









