
Rcpp::sourceCpp("forwardPassCpp.cpp")
library(RcppArmadillo)

sigmoid.activation <- function(x){
  #return(1 / (1+exp(-x)))
  return(x)
}




batch_split <- function(n.epochs, network=NA){
  
  delay = 1
  counter <- 5000 #change to start what batch 2nd layer starts learning (start at 1 will have layer start learning after 5000 epochs)
  counter.bias <- 5000 #change to start what batch output bias node starts at
  # network properties #
  #pre.input.hidden.weights <- matrix((rnorm(n.input*n.hidden) * sqrt(2/n.input)), nrow=n.input, ncol=n.hidden)    # He normalization
  #pre.hidden.output.weights <- matrix((rnorm(n.hidden*n.output) * sqrt(2/n.hidden)), nrow=n.hidden, ncol=n.output)
  pre.input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=0.5), nrow=n.input, ncol=n.hidden)   # Random normalization
  pre.hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=0.5), nrow=n.hidden, ncol=n.output)
  
  for(input in 1:(n.input/2)){
    for(hidden in (n.hidden/2 + 1):n.hidden){
      if(runif(1) > integration.parameter){
        pre.input.hidden.weights[input,hidden] <- NA
      }
    }
  }
  for(input in (n.input/2 + 1):n.input){
    for(hidden in 1:(n.hidden/2)){
      if(runif(1) > integration.parameter){
        pre.input.hidden.weights[input,hidden] <- NA
      }
    }
  }
  
  for(hidden in 1:(n.hidden/2)){
    for(output in (n.output/2 + 1):n.output){
      if(runif(1) > integration.parameter){
        pre.hidden.output.weights[hidden,output] <- NA
      }
    }
  }
  
  for(hidden in (n.hidden/2 + 1):n.hidden){
    for(output in 1:(n.output/2)){
      if(runif(1) > integration.parameter){
        pre.hidden.output.weights[hidden,output] <- NA
      }
    }
  }
  
  
  
  if(is.na(network)){
    network <- list(
      input.hidden.weights = pre.input.hidden.weights,
      hidden.bias.weights = matrix(0, nrow=n.hidden, ncol=1),
      hidden.output.weights = pre.hidden.output.weights,
      output.bias.weights = matrix(0, nrow=n.output, ncol=1),
      trace.hidden = rep(0, times = n.hidden),
      trace.output = rep(0, times = n.output))
    
    network[[1]][sample(1:(n.input*n.hidden), sparseness.percent*(n.input*n.hidden), replace=F)] <- NA
    network[[3]][sample(1:(n.output*n.hidden), sparseness.percent*(n.output*n.hidden), replace=F)] <- NA
  } else{
    network$hidden.bias.weights <- matrix(0, nrow = n.hidden, ncol = 1)
    network$output.bias.weights <- matrix(0, nrow = n.output, ncol = 1)
  }
  
  
  # tracking learning #
  history <- list(               #initializes learning data matrices
    mutual.info.spatial.track <- rep(0, times = (n.epochs/100)+1),
    learning.curve = matrix(0, nrow = (n.epochs/100)+1, ncol = n.hidden), 
    hidden.letter.similarity.tracking = matrix(0, nrow=(n.epochs/100)+1, ncol = length(letters)),
    output.trace.tracker = matrix(0, nrow = (n.epochs+1), ncol = n.output),
    output.bias.tracker = matrix(0, nrow=(n.epochs/100)+1, ncol = n.output),
    output.act.unique.tracker <- rep(0, times=(n.epochs/100)+1)
  )
  
  iter = 0
  iter1 = 0

  pb <- txtProgressBar(min=1, max=n.epochs,style=3)
  
  
  for(i in 1:n.epochs){
  
    counter = counter + 1
    counter.bias = counter.bias + 1
    word <- words[[sample(1:n.words,1, replace = T)]]
    
    if(i == 2){
      history$learning.curve[i-1,] <- learningMeasure(network$input.hidden.weights, n.hidden, alphabet)
      history$hidden.letter.similarity.tracking[i-1, ] <- batch.hidden.layer.learning(letters, network)$similarity
      history$output.trace.tracker[i-1, ] <- network$trace.output
      history$output.bias.tracker[i-1, ] <- network$output.bias.weights[,1]
      history$output.act.unique.tracker[i-1] <- output.act.unique(network, words)
      history$mutual.info.spatial.track[i-1] <- mutual.info.spatial(network)
    }
    
    if(i %% 100 == 0){
      history$learning.curve[(i / 100) + 1,] <- learningMeasure(network$input.hidden.weights, n.hidden, alphabet)
      history$hidden.letter.similarity.tracking[(i / 100) + 1, ] <- batch.hidden.layer.learning(letters, network)$similarity
      history$output.trace.tracker[(i / 100) + 1, ] <- network$trace.output
      history$output.bias.tracker[(i / 100) + 1, ] <- network$output.bias.weights[,1]
      history$output.act.unique.tracker[(i / 100) + 1] <- output.act.unique(network, words)
      history$mutual.info.spatial.track[(i / 100) + 1] <- mutual.info.spatial(network)
    }
    
    history$output.trace.tracker[i,] <- network$trace.output
    

    if(counter > 5000){  # Cosine Annealing
      iter = iter + 1
      if(iter > (n.epochs)/restarts){
        iter = 1
        learning.rate.output.min = learning.rate.output.min - (learning.rate.output.min/2)
        learning.rate.output.max = learning.rate.output.max - (learning.rate.output.max/2)
      }
      learning.rate.output = learning.rate.output.min + ((learning.rate.output.max - learning.rate.output.min)/2) * (1 + cos((iter*pi)/((n.epochs)/restarts)))
    }
    
    iter1 = iter1 + 1
    if(iter1 > (n.epochs)/restarts){
      iter1 = 1
      learning.rate.hidden.min = learning.rate.hidden.min - (learning.rate.hidden.min/2)
      learning.rate.hidden.max = learning.rate.hidden.max - (learning.rate.hidden.max/2)
    }
    learning.rate.hidden = learning.rate.hidden.min + ((learning.rate.hidden.max - learning.rate.hidden.min)/2) * (1 + cos((iter*pi)/((n.epochs)/restarts)))
  

    network$trace.output <- rep(0, times = n.output) # set trace.output to zero after each stimulus group
    
    
    
    for(b in 1:(length(word)/n.input)){
      # get input vector

      input <- word[,b]
      input <- noise.in.letter(input)
      
      # update network properties
      
      results <- traceUpdate(trace.param.hidden, trace.param.output,
                             learning.rate.hidden, learning.rate.output,
                             output.bias.param.plus, output.bias.param.minus,
                             hidden.bias.param.minus, hidden.bias.param.plus,
                             percent.act.input, percent.act.output,
                             n.output, n.hidden,
                             input, network$input.hidden.weights,
                             network$trace.hidden, network$hidden.bias.weights,
                             network$hidden.output.weights, network$trace.output,
                             network$output.bias.weights, counter, counter.bias, n.epochs)
      
      
      network$input.hidden.weights <- results$inputToHiddenWeights
      network$trace.hidden <- results$traceHidden
      network$hidden.bias.weights <- results$hiddenBiasWeights
      network$trace.output <- results$traceOutput
      network$output.bias.weights <- results$outputBiasWeights
      network$hidden.output.weights <- results$hiddenToOutputWeights
    }
    setTxtProgressBar(pb, i)
  }

  return(list(
    history = history,
    network = network
  ))
  
  
}