

library(tensorflow)
datasets <- tf$contrib$learn$datasets
mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)

mnist_images <- mnist$train$images[1:5000,]
mnist_labels <- mnist$train$labels[1:5000,]

for(i in 1:5000){
  for(n in 1:784){
    mnist_images[i, n] = round(mnist_images[i,n])
  }
}

mnist_images1 <- split(mnist_images, 1:5000)


for(i in 1:5000){
  mnist_images1[[i]] <- matrix(mnist_images1[[i]], nrow = 28, ncol = 28)
}

images <- mnist_images1
remove(mnist_images, datasets, i, mnist, mnist_images1, n)

#lapply(images, function(x) write.table( data.frame(x), 'mnist_images.csv'  , append= T, sep=',' ))
