
sigmoid.activation <- function(x){
  #return(1 / (1+exp(-x)))
  return(x)
}

noise.in.letter <- function(letter){
  for(i in 1:(0.1*n.input)){
    letter[(sample(1:1600,1,replace=T))] <- 1
  }
  return(letter)
}

learning.measure <- function(input.hidden.weights){
  all.letters.compared <- numeric(26)
  best.fit <- numeric(n.hidden)
  for(i in 1:n.hidden){
    for(h in 1:26){
      all.letters.compared[h] <- sum(abs(input.hidden.weights[,i] - alphabet[[h]]))
    }
    best.fit[i] <- min(all.letters.compared)
  }
  return(best.fit)
}


forward.pass <- function(input, input.hidden.weights, hidden.bias.weights, hidden.output.weights, output.bias.weights){ #calculate output activations with "winner-takes-all" method
  
  hidden <- numeric(n.hidden)
  
  for(i in 1:n.hidden){
    hidden[i] <- sigmoid.activation(sum(input * input.hidden.weights[,i]) + hidden.bias.weights[i,1])
  }
  
  #for(c in 1:ceiling(0.1*n.hidden)){
  #  hidden[which.max(hidden)] <- -1
  #}
  
  #for(j in 1:n.hidden){
  # if(hidden[j] == -1){
  #   hidden[j] = 1
  # } else{
  #   hidden[j] = 0
  # }
  #}
  
  hidden[hidden != max(hidden)] <- 0
  hidden[which.max(hidden)] <- 1
  
  output <- numeric(n.output)
  for(b in 1:n.output){
    output[b] <- sigmoid.activation(sum(hidden * hidden.output.weights[,b] +  output.bias.weights[b,1]))
  }
  
  #for(h in 1:ceiling(0.1*n.output)){
  #  output[which.max(output)] <- -1
  #}
  
  #for(k in 1:n.output){
  #  if(output[k] == -1){
  #    output[k] = 1
  #  } else{
  #    output[k] = 0
  #  }
  #}
  
  output[output != max(output)] <- 0
  output[which.max(output)] <- 1
  return(list(hidden=hidden, output=output))
}

trace.update <- function(input, input.hidden.weights, trace.hidden, hidden.bias.weights, hidden.output.weights, trace.output, output.bias.weights){
  
  forward.pass.results <- forward.pass(input, input.hidden.weights, hidden.bias.weights, hidden.output.weights, output.bias.weights)
  hidden <- forward.pass.results$hidden
  output <- forward.pass.results$output
  
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
  
  for(j in 1:n.output){
    if(output[j] == 1){
      output.bias.weights[j,1] <- output.bias.weights[j,1] - output.bias.param.minus
    }
    if(output[j] == 0){
      output.bias.weights[j,1] <- output.bias.weights[j,1] + output.bias.param.plus
    }
    if(output.bias.weights[j,1] < 0){
      output.bias.weights[j,1] <- 0
    }
  }
  
  for(b in 1:n.output){
    trace.output[b] <- (1 - trace.param.output) * trace.output[b] + trace.param.output * output[b]
    hidden.output.weights[,b] <- hidden.output.weights[,b] + learning.rate * trace.output[b] * (hidden - hidden.output.weights[,b])
  }
  
  return(list(
    trace.hidden=trace.hidden, 
    hidden = hidden,
    input.hidden.weights=input.hidden.weights, 
    hidden.bias.weights=hidden.bias.weights,
    trace.output=trace.output,
    output=output,
    hidden.output.weights=hidden.output.weights,
    output.bias.weights=output.bias.weights))
}

batch <- function(n.epochs){
  # network properties #
  network <- list(
    input.hidden.weights = matrix(runif(n.input*n.hidden, min=0, max=0.05), nrow=n.input, ncol=n.hidden), #initialiize weights at random values between 0 and 0.05
    hidden.bias.weights = matrix(0, nrow=n.hidden, ncol=1),
    hidden.output.weights = matrix(runif(n.hidden*n.output, min=0, max=0.05), nrow=n.hidden, ncol=n.output),
    output.bias.weights = matrix(0, nrow=n.output, ncol=1),
    trace.hidden = rep(0, times = n.hidden),
    trace.output = rep(0, times = n.output)
  )
 
  # tracking learning #
  history <- list(
    learning.curve = matrix(0, nrow = n.epochs/100, ncol = n.hidden), #initializes learning data matrix
    bias.tracker = matrix(0, nrow = n.epochs/100, ncol = n.hidden), #initializes learning data matrix
    output.bias.tracker = matrix(0, nrow = n.epochs/100, ncol= n.output),
    hidden.win.tracker = matrix(0, nrow=n.epochs, ncol= n.hidden)
  )
  
  pb <- txtProgressBar(min=1, max=n.epochs,style=3)
  for(i in 1:n.epochs){
    word <- words[[sample(1:9,1, replace = T)]]
    for(b in 1:(length(word)/n.input)){
      
      # get input vector
      letter <- word[,b]
      letter <- noise.in.letter(letter)
      
      # update network properties
      results <- trace.update(letter, network$input.hidden.weights, network$trace.hidden, network$hidden.bias.weights, network$hidden.output.weights, network$trace.output, network$output.bias.weights)
      network$input.hidden.weights <- results$input.hidden.weights
      network$hidden.output.weights <- results$hidden.output.weights
      network$trace.hidden <- results$trace.hidden
      network$hidden.bias.weights <- results$hidden.bias.weights
      network$hidden.output.weights <- results$hidden.output.weights
      network$trace.output <- results$trace.output
      network$output.bias.weights <- results$output.bias.weights
      network$hidden.bias.weights <- results$hidden.bias.weights
      
      # update learning history
      history$hidden.win.tracker[i,] <- results$hidden
      if(i %% 100 == 0){
        history$learning.curve[i / 100,] <- learning.measure(network$input.hidden.weights)
        history$bias.tracker[i / 100,] <- as.vector(network$hidden.bias.weights)
        history$output.bias.tracker[i / 100,] <- as.vector(network$output.bias.weights)
      }
      setTxtProgressBar(pb, i)
    }
  }
  test.word.continuity(network, words)
  return(list(
    history=history,
    network=network
  ))
}
