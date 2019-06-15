library('png')


files <- list.files(path="Binary_Datasets_Testing/Datasets/Omniglot_Dataset/Half_1/", pattern="*.png", full.names=TRUE, recursive=TRUE)
images <- lapply(files, function(file){
  img <- readPNG(file)
  if((nrow(img) == 105) && (ncol(img) == 105)){
    #for(i in 1:50){
      #for(h in 1:50){
        #img[i,h] = round(img[i,h]) 
      #}
    #}
    return(img)
  } else{print(file)}
}
)
 

labels <- list()

counter <- 0
for(g in 1:448){
  for( h in 1:20){
    counter = counter + 1
    labels[[counter]] = g
  }
}


##  105 is divisible by: 1, 3, 5, 7, 15, 21, 35, 105 