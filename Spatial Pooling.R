
n.input <- 1600
n.hidden <- 26
n.output <- 50
learning.rate <- 0.1
n.epochs <- 1000
trace.hidden <- rep(0, times = n.hidden)
trace.output <- rep(0, times = n.output)
trace.param <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account


#install.packages('bmp')
library('bmp')
is.bmp('C:/Users/research/Documents/GitHub/Int-Seg-Model/Int-Seg-Model/Alphabet/A.bmp')
A <- read.bmp('C:/Users/research/Documents/GitHub/Int-Seg-Model/Int-Seg-Model/Alphabet/A.bmp')



input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=1), nrow=n.input, ncol=n.hidden) #initialiize weights at random values between 1 and 0
hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=1), nrow=n.hidden, ncol=n.output)


sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}


forward.pass <- function(input){ #calculate output activations with "winner-takes-all" method
  
  hidden <- numeric(n.hidden)
  for(i in 1:n.output){
    hidden[i] <- sigmoid.activation(sum(input * weights[,i]))
  }
  hidden[which.max(outputs)] <- 1
  hidden[outputs != max(outputs)] <- 0
  return(hidden)
}


trace.update <- function(input, input.hidden.weights, hidden.output.weights, trace.hidden, trace.output){ 
  
  hidden <- forward.pass(input)
  for(i in 1:n.hidden){
    trace.hidden[i] <- (1 - trace.param) * trace.hidden[i]  + trace.param * hidden[i] 
    input.hidden.weights[,i] <- input.hidden.weights[,i] + learning.rate * trace.hidden[i] * (input - input.hidden.weights[,i])  
  }
  return(list(trace.hidden=trace.hidden, trace.ouput=trace.output, input.hidden.weights=input.hidden.weights, hidden.output.weights=hidden.output.weights))
}


batch <- function(n.epochs){ 
  pb <- txtProgressBar(min=0, max=n.training, style=3)
  for(i in 1:n.epochs){
    letter <- alphabet[sample(1:26,1,replace=T),]
    results <- trace.update(letter, input.hidden.weights, hidden.output.weights, trace.hidden, trace.output)
    input.hidden.weights <- results$input.hidden.weights
    hidden.output.weights <- results$hidden.output.weights
  }
    setTxtProgressBar(pb, i)
  print(colsums.function())
  return(output.storage())
}

batch(n.epochs)  #run training batches
test <- output.storage()
colsums <- colsums.function()
colsums
?sample


## entropy testing functions ##

output.storage <- function(){ #stores outputs 
  outputs <- matrix(0, nrow = n.test, ncol = n.output)
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


