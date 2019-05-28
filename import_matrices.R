
import_dataset <- function(filename){
  
  set <- read.csv(filename, header = F)
  set <- set[,-1]
  set[,1] <- as.character(set[,1])
  set <- set[!grepl("X2", set[,1]),]
  
  xsplit <- rep(1:(nrow(set)/16), times = rep(16,(nrow(set)/16)))
  new <- split(set, xsplit)
  
  for(n in 1:length(new)){
    new[[n]] <- apply(new[[n]], 2, as.numeric)
    colnames(new[[n]]) <- NULL
  }

  return(new)
  
}



