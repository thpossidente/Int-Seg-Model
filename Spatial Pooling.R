
n.inputs <- 1600
n.hidden <- 26
n.outputs <- 50
learning.rate <- 0.1
n.training <- 1600
n.test <- 400



#install.packages('readbitmap')
library('readbitmap')
A <- read.bitmap('C:/Users/research/Documents/GitHub/Int-Seg-Model/Int-Seg-Model/Alphabet/A.bmp')

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


heb.update <- function(input){ # Sanger's Generalization of the Hebbian Algorithm
  outputs <- forward.pass(input)
  for(i in 1:n.inputs){
    for(j in 1:n.outputs){
      change.weight <- learning.rate * ((outputs[j] * input[i]) - (outputs[j] * (sum(weights[i,] * outputs[j]))))
      weights[i,j] <<- weights[i,j] + change.weight
      if(weights[i,j] > 1){
        weights[i,j] <<- 1
      }
      if(weights[i,j] < 0){
        weights[i,j] <<- 0
      }
    }
  }
}


batch <- function(){ 
  pb <- txtProgressBar(min=0, max=n.training, style=3)
  for(i in 1:n.training){
    vector <- alphabet.data.n[i,]
    heb.update(vector)
  }
    setTxtProgressBar(pb, i)
  print(colsums.function())
  return(output.storage())
}

batch()  #run training batches
test <- output.storage()
colsums <- colsums.function()
colsums



## entropy testing functions ##

output.storage <- function(){ #stores outputs 
  outputs <- matrix(0, nrow = n.test, ncol = n.outputs)
  for(i in 1601:2000){
    one.output <- forward.pass(alphabet.data.n[i,])
    outputs[i-1600,] <- one.output
  }
  return(outputs)
}


entropy.calc <- function(v){ #function to pass in matrix and get entropy
  v <- v / sum(v)
  e.sum <- 0
  for(i in 1:length(v)){
    if(v[i] != 0){
      e.sum <- e.sum + -v[i] * log2(v[i])
    }
  }
  return(e.sum)
}



colsums.function <- function(){ #calculate average entropy of output activations
  outputs <- output.storage()
  stability.one <- colSums(outputs)
  return(stability.one)
}


entropy.measure <- function(){ #calculate average entropy of output activations for each group
  outputs <- output.storage()
  entropy <- numeric(10)
  for(i in 1:10){
    stability.one <- colSums(outputs[((i-1) * 100 + 1):(i * 100),])
    entropy[i] <- entropy.calc(stability.one)
  }
  return(mean(entropy))
}


