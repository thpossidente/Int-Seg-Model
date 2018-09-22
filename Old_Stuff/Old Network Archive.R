
forward.pass <- function(input, input.hidden.weights, hidden.bias.weights){ #calculate output activations with "winner-takes-all" method
  
  hidden <- numeric(n.hidden)
  for(i in 1:n.hidden){
    hidden[i] <- sigmoid.activation(sum(input * input.hidden.weights[,i]) + hidden.bias.weights[i,1])
  }
  hidden[hidden != max(hidden)] <- 0
  hidden[which.max(hidden)] <- 1
  return(hidden)
}



trace.update <- function(input, input.hidden.weights, trace.hidden, hidden.bias.weights){
  
  hidden <- forward.pass(input, input.hidden.weights, hidden.bias.weights)
  
  for(h in 1:n.hidden){
    if(hidden[h] == 1){
      hidden.bias.weights[h,1] <- hidden.bias.weights[h,1] - hidden.bias.param.minus
    }
    if(hidden[h] == 0){
      hidden.bias.weights[h,1] <- hidden.bias.weights[h,1] + hidden.bias.param.plus
    }
    if(hidden.bias.weights[h,1] < 0){
      hidden.bias.weights[h,1] <- 0
    }
  }
  
  for(i in 1:n.hidden){
    trace.hidden[i] <- (1 - trace.param.hidden) * trace.hidden[i] + trace.param.hidden * hidden[i] 
    input.hidden.weights[,i] <- input.hidden.weights[,i] + learning.rate * trace.hidden[i] * (input - input.hidden.weights[,i])
  }
  
  return(list(
    trace.hidden = trace.hidden,
    hidden = hidden,
    input.hidden.weights = input.hidden.weights, 
    hidden.bias.weights = hidden.bias.weights
  ))
}


batch <- function(n.epochs){
  
  # network properties #
  input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=0.05), nrow=n.input, ncol=n.hidden) #initialiize weights at random values between 0 and 0.05
  hidden.bias.weights <- matrix(0, nrow=n.hidden, ncol=1)
  
  # tracking learning #
  learning.curve <- matrix(0, nrow = n.epochs/100, ncol = n.hidden) #initializes learning data matrix
  bias.tracker <- matrix(0, nrow = n.epochs/100, ncol = n.hidden) #initializes learning data matrix
  hidden.win.tracker <- matrix(0, nrow=n.epochs, ncol= n.hidden)
  
  pb <- txtProgressBar(min=1, max=n.epochs,style=3)
  for(i in 1:n.epochs){
    letter <- alphabet[[sample(1:26,1, replace = T)]]
    results <- trace.update(letter, input.hidden.weights, trace.hidden, hidden.bias.weights)
    input.hidden.weights <- results$input.hidden.weights
    trace.hidden <- results$trace.hidden
    hidden.bias.weights <- results$hidden.bias.weights
    hidden.win.tracker[i,] <- results$hidden
    if(i %% 100 == 0){
      learning.curve[i / 100,] <- learning.measure(input.hidden.weights)
      bias.tracker[i / 100,] <- as.vector(hidden.bias.weights)
    }
    setTxtProgressBar(pb, i)
  }
  return(list(
    input.hidden.weights=input.hidden.weights, 
    learning.curve=learning.curve, 
    bias.tracker=bias.tracker,
    hidden.bias.weights=hidden.bias.weights,
    hidden.win.tracker = hidden.win.tracker
  ))
}
