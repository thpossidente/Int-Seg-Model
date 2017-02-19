
n.inputs <- 16
n.hidden <- 100
n.outputs <- 26
learning.rate <- 0.2

alphabet.data <- read.table("C:/Users/research/Documents/GitHub/Int-Seg-Model/Int-Seg-Model/letter-recognition-data.txt", header = F)
#install.packages("splitstackshape")
library("splitstackshape")
alphabet.data <- cSplit(alphabet.data, "V1", ",", stripWhite = FALSE)
alphabet.data.n <- alphabet.data[,-1]
alphabet.data.n <- data.matrix(alphabet.data.n)

normalization.by.column <- function(alphabet.data.n){ # normalizes values (by column) to values between 0 and 1
  alphabet.data.n.1 <- alphabet.data.n
  for(j in 1:ncol(alphabet.data.n)){
    pb <- txtProgressBar(min=0, max=nrow(alphabet.data.n), style=3)
    for(i in 1:nrow(alphabet.data.n)){
      alphabet.data.n[i,j] <<- (alphabet.data.n.1[i,j] - min(alphabet.data.n.1[,j])) / (max(alphabet.data.n.1[,j] - min(alphabet.data.n.1[,j])))
    }
      setTxtProgressBar(pb, i)
  }
}
normalization.by.column(alphabet.data.n)

weights <- matrix(runif(n.inputs*n.outputs, min=0, max=1), nrow=n.inputs, ncol=n.outputs) #initialiize weights at random values between 1 and 0

sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}

forward.pass <- function(input){ #calculate output activations with "winner-takes-all" method
  outputs <- numeric(n.outputs)
  for(i in 1:n.outputs){
    outputs[i] <- sigmoid.activation(sum(input * weights[,i]))
  }
  outputs[which.max(outputs)] <- 1
  outputs[outputs != max(outputs)] <- 0
  return(outputs)
}


heb.update <- function(input){
  outputs <- forward.pass(input)
  for(i in 1:n.inputs){
    for(j in 1:n.outputs){
      change.weight <- learning.rate * outputs[j] * input[i]
      weights[i,j] <<- weights[i,j] + change.weight
    }
  }
}


batch <- function(){ 
  pb <- txtProgressBar(min=0, max=nrow(alphabet.data.n), style=3)
  for(i in 1:nrow(alphabet.data.n)){
    vector <- alphabet.data.n[i,]
    heb.update(vector)
  }
    setTxtProgressBar(pb, i)
}

batch()  #run training batches


