
Rcpp::sourceCpp("forwardPassCpp.cpp")
library(RcppArmadillo)

sigmoid.activation <- function(x){
  #return(1 / (1+exp(-x)))
  return(x)
}

# noise.in.letter <- function(input){
#   for(i in 1:(letter.noise.param*n.input)){
#     input[(sample(1:n.input,1,replace=T))] <- 1
#   }
#   return(input)
# }
# 
# learning.measure <- function(input.hidden.weights){
#   all.letters.compared <- numeric(26)
#   best.fit <- numeric(n.hidden)
#   for(i in 1:n.hidden){
#     for(h in 1:26){
#       all.letters.compared[h] <- sum(abs(input.hidden.weights[,i] - alphabet[[h]]))
#     }
#     best.fit[i] <- min(all.letters.compared)
#   }
#   return(best.fit)
# }



# 
# forward.pass <- function(input, input.hidden.weights, hidden.bias.weights, hidden.output.weights, output.bias.weights){ #calculate output activations with "winner-takes-all" method
#   
#   hidden <- numeric(n.hidden)
# 
#   for(i in 1:n.hidden){
#     hidden[i] <- sigmoid.activation(sum(na.omit(input * input.hidden.weights[,i]) + hidden.bias.weights[i,1]))
#   }
#   
#   for(c in 1:ceiling(percent.act.input*n.hidden)){
#     hidden[which.max(hidden)] <- -1
#   }
# 
# 
#   for(j in 1:n.hidden){
#    if(hidden[j] == -1){
#      hidden[j] = 1
#    } else{
#      hidden[j] = 0
#    }
#   }
#   
#   
#   output <- numeric(n.output)
#   for(b in 1:n.output){
#     output[b] <- sigmoid.activation(sum(na.omit(hidden * hidden.output.weights[,b] +  output.bias.weights[b,1])))
#   }
#   
#   for(h in 1:ceiling(percent.act.output*n.output)){
#     output[which.max(output)] <- -1
#   }
#   
#   for(k in 1:n.output){
#     if(output[k] == -1){
#       output[k] = 1
#     } else{
#       output[k] = 0
#     }
#   }
#   
#   return(list(hidden=hidden, output=output))
# }
# 
# trace.update <- function(input, input.hidden.weights,
#                          trace.hidden, hidden.bias.weights,
#                          hidden.output.weights, trace.output,
#                          output.bias.weights){
#   
#   #forward.pass.results <- forward.pass(input, input.hidden.weights, hidden.bias.weights, hidden.output.weights, output.bias.weights)
#   forward.pass.results <- forwardPass(n.output, percent.act.input,
#                                       percent.act.output, n.hidden,
#                                       input, input.hidden.weights,
#                                       hidden.bias.weights, hidden.output.weights,
#                                       output.bias.weights)
# 
#   hidden <- forward.pass.results$hidden
#   output <- forward.pass.results$output
# 
# 
#   for(h in 1:n.hidden){
#     if(hidden[h] == 1){
#       hidden.bias.weights[h,1] <- hidden.bias.weights[h,1] - hidden.bias.param.minus
#     }
#     if(hidden[h] == 0){
#       hidden.bias.weights[h,1] <- hidden.bias.weights[h,1] + hidden.bias.param.plus
#     }
#     if(hidden.bias.weights[h,1] < 0){
#       hidden.bias.weights[h,1] <- 0
#     }
#   }
#   
# 
#   for(i in 1:n.hidden){
#     trace.hidden[i] <- (1 - trace.param.hidden) * trace.hidden[i] + trace.param.hidden * hidden[i]
#     input.hidden.weights[,i] <- input.hidden.weights[,i] + learning.rate.hidden * trace.hidden[i] * (input - input.hidden.weights[,i])
#   }
# 
#   for(j in 1:n.output){
#     if(output[j] == 1){
#       output.bias.weights[j,1] <- output.bias.weights[j,1] - output.bias.param.minus
#     }
#     if(output[j] == 0){
#       output.bias.weights[j,1] <- output.bias.weights[j,1] + output.bias.param.plus
#     }
#     if(output.bias.weights[j,1] < 0){
#       output.bias.weights[j,1] <- 0
#     }
#   }
#   
# 
#   for(b in 1:n.output){
#     trace.output[b] <- (1 - trace.param.output) * trace.output[b] + trace.param.output * output[b]
#     hidden.output.weights[,b] <- hidden.output.weights[,b] + learning.rate.output * trace.output[b] * (hidden - hidden.output.weights[,b])
#   }
#   
#   return(list(
#     trace.hidden=trace.hidden, 
#     hidden = hidden,
#     input.hidden.weights=input.hidden.weights, 
#     hidden.bias.weights=hidden.bias.weights,
#     trace.output=trace.output,
#     output=output,
#     hidden.output.weights=hidden.output.weights,
#     output.bias.weights=output.bias.weights))
# }

batch <- function(n.epochs, network=NA){
  counter <- 5001    #change to start what batch 2nd layer starts learning
  # network properties #
  pre.input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=1), nrow=n.input, ncol=n.hidden)
  pre.hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=1), nrow=n.hidden, ncol=n.output)
  
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
      trace.output = rep(0, times = n.output)
    )
 ## work on correct implementation of sparseness with split network
    network[[1]][sample(1:(n.input*n.hidden), sparseness.percent*(n.input*n.hidden), replace=F)] <- NA
    network[[3]][sample(1:(n.output*n.hidden), sparseness.percent*(n.output*n.hidden), replace=F)] <- NA
  }
  
  # tracking learning #
  history <- list(               #initializes learning data matrices
    learning.curve = matrix(0, nrow = n.epochs/100, ncol = n.hidden), 
    output.match.tracker = rep(0, times = n.epochs/100),
    hidden.letter.similarity.tracking = matrix(0, nrow=n.epochs/100, ncol = length(letters)),
    output.trace.tracker = matrix(0, nrow = n.epochs/100, ncol = n.output),
    output.bias.tracker = matrix(0, nrow=n.epochs/100, ncol = n.output),
    output.act.unique.tracker <- rep(0, times=n.epochs/100)
  )
  
  pb <- txtProgressBar(min=1, max=n.epochs,style=3)
  
  for(i in 1:n.epochs){
    
    counter = counter + 1
    word <- words[[sample(1:n.words,1, replace = T)]]

    if(i == 2 || i %% 100 == 0){
      history$learning.curve[i / 100,] <- learningMeasure(network$input.hidden.weights, n.hidden, alphabet)
      history$hidden.letter.similarity.tracking[i / 100, ] <- batch.hidden.layer.learning(letters, network)$similarity
      history$output.match.tracker[i / 100] <- test.word.continuity(network, words)
      history$output.trace.tracker[i / 100, ] <- network$trace.output
      history$output.bias.tracker[i / 100, ] <- network$output.bias.weights[,1]
      history$output.act.unique.tracker[i / 100] <- output.act.unique(network, words)
    }

    for(b in 1:(length(word)/n.input)){

      # get input vector

      input <- word[,b]
      input <- noiseInLetter(input, n.input, letter.noise.param, n.epochs)
      
      # dropout output layer
      coordinates <- data.frame(hidden=sample(1:n.hidden, dropout*n.hidden*n.output, replace=T), output=sample(1:n.output, dropout*n.hidden*n.output, replace=T)) 
      for(n in 1:nrow(coordinates)){
        coordinates$weight[n] <- network$hidden.output.weights[coordinates$hidden[n], coordinates$output[n]]
      }
      for(c in 1:nrow(coordinates)){
        network$hidden.output.weights[coordinates$hidden[c], coordinates$output[c]] <- NA
      }
      

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
                             network$output.bias.weights, counter)
      

      network$input.hidden.weights <- results$inputToHiddenWeights
      network$trace.hidden <- results$traceHidden
      network$hidden.bias.weights <- results$hiddenBiasWeights
      network$trace.output <- results$traceOutput
      network$output.bias.weights <- results$outputBiasWeights
      network$hidden.output.weights <- results$hiddenToOutputWeights
      for(x in 1:nrow(coordinates)){
        network$hidden.output.weights[coordinates$hidden[x], coordinates$output[x]] <- coordinates$weight[x]
      }


    }
    setTxtProgressBar(pb, i)
  }

  # results <- batchHelp(n.epochs, words,
  #                      n.words, history, input,
  #                      n.input, network,
  #                      alphabet,letters, batch.hidden.layer.learning,
  #                      test.word.continuity, letter.noise.param,
  #                      trace.param.hidden, trace.param.output,
  #                      learning.rate.hidden, learning.rate.output,
  #                      output.bias.param.plus, output.bias.param.minus,
  #                      hidden.bias.param.minus, hidden.bias.param.plus,
  #                      percent.act.input, percent.act.output,
  #                      n.output, n.hidden,
  #                      input.hidden.weights, noiseInLetter,
  #                      trace.hidden, hidden.bias.weights,
  #                      hidden.output.weights, trace.output,
  #                      output.bias.weights)

  return(list(
    history = history,
    network = network
  ))
  

}
#test(n.epochs, history, words, network, n.hidden, alphabet, letters, batch.hidden.layer.learning, test.word.continuity)


