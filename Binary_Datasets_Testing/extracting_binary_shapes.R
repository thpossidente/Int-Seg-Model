library(pixmap)



files <- list.files(path="Binary_shapes", pattern="*.pgm", full.names=TRUE, recursive=FALSE)
images <- lapply(files, function(file){
  img <- read.pnm(file)
  img <- img@grey
  if((nrow(img) == 50) && (ncol(img) == 50)){
    for(i in 1:50){
      for(h in 1:50){
       img[i,h] = round(img[i,h]) 
      }
    }
    return(img)
  } else{print(file)}
})



labels <- lapply(files, function(file){
  if(is.na(as.numeric(substr(file, start = 15, stop = 16)))){
    label <- as.numeric(substr(file, start = 15, stop = 15))
  } else {label <- as.numeric(substr(file, start = 15, stop = 16))}
  return(label)
})


# 0 bird
# 1 bone
# 2 bottle
# 3 brick
# 4 camel
# 5 car
# 6 carriage
# 7 cat
# 8 cattle
# 9 children
# 10 chopper
# 11 classic
# 12 crown
# 13 cthand
# 14 dino
# 15 dog
# 16 dude
# 17 elephant
# 18 face
# 19 fgen
# 20 fish
# 21 flatfish
# 22 flightbird
# 23 fork
# 24 fountain
# 25 glass
# 26 hammer 
# 27 heart
# 28 horse
# 29 key
# 30 kk
# 31 mgen
# 32 misk
# 33 rat 
# 34 ray
# 35 stef
# 36 testbox
# 37 tool
# 38 turtle 