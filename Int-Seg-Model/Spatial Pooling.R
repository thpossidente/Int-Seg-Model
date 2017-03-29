n.input <- 1600
n.hidden <- 26
n.output <- 50
learning.rate <- 0.05
n.epochs <- 5000
n.test <- 26
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.2
hidden.bias.param.minus <- 1
hidden.bias.param.plus <- 0.05
output.bias.param.minus <- 1
output.bias.param.plus <- 0.05

#install.packages('png')
library('png')
#install.packages('abind')
library('abind')

alphabet <- list(
  a <- as.vector(t(1-adrop(readPNG('AlphabetPNG/A.png')[,,1,drop=F], drop=3))),
  b <- as.vector(t(1-adrop(readPNG('AlphabetPNG/B.png')[,,1,drop=F], drop=3))),
  c <- as.vector(t(1-adrop(readPNG('AlphabetPNG/C.png')[,,1,drop=F], drop=3))),
  d <- as.vector(t(1-adrop(readPNG('AlphabetPNG/D.png')[,,1,drop=F], drop=3))),
  e <- as.vector(t(1-adrop(readPNG('AlphabetPNG/E.png')[,,1,drop=F], drop=3))),
  f <- as.vector(t(1-adrop(readPNG('AlphabetPNG/F.png')[,,1,drop=F], drop=3))),
  g <- as.vector(t(1-adrop(readPNG('AlphabetPNG/G.png')[,,1,drop=F], drop=3))),
  h <- as.vector(t(1-adrop(readPNG('AlphabetPNG/H.png')[,,1,drop=F], drop=3))),
  i <- as.vector(t(1-adrop(readPNG('AlphabetPNG/I.png')[,,1,drop=F], drop=3))),
  j <- as.vector(t(1-adrop(readPNG('AlphabetPNG/J.png')[,,1,drop=F], drop=3))),
  k <- as.vector(t(1-adrop(readPNG('AlphabetPNG/K.png')[,,1,drop=F], drop=3))),
  l <- as.vector(t(1-adrop(readPNG('AlphabetPNG/L.png')[,,1,drop=F], drop=3))),
  m <- as.vector(t(1-adrop(readPNG('AlphabetPNG/M.png')[,,1,drop=F], drop=3))),
  n <- as.vector(t(1-adrop(readPNG('AlphabetPNG/N.png')[,,1,drop=F], drop=3))),
  o <- as.vector(t(1-adrop(readPNG('AlphabetPNG/O.png')[,,1,drop=F], drop=3))),
  p <- as.vector(t(1-adrop(readPNG('AlphabetPNG/P.png')[,,1,drop=F], drop=3))),
  q <- as.vector(t(1-adrop(readPNG('AlphabetPNG/Q.png')[,,1,drop=F], drop=3))),
  r <- as.vector(t(1-adrop(readPNG('AlphabetPNG/R.png')[,,1,drop=F], drop=3))),
  s <- as.vector(t(1-adrop(readPNG('AlphabetPNG/S.png')[,,1,drop=F], drop=3))),
  t <- as.vector(t(1-adrop(readPNG('AlphabetPNG/T.png')[,,1,drop=F], drop=3))),
  u <- as.vector(t(1-adrop(readPNG('AlphabetPNG/U.png')[,,1,drop=F], drop=3))),
  v <- as.vector(t(1-adrop(readPNG('AlphabetPNG/V.png')[,,1,drop=F], drop=3))),
  w <- as.vector(t(1-adrop(readPNG('AlphabetPNG/W.png')[,,1,drop=F], drop=3))),
  x <- as.vector(t(1-adrop(readPNG('AlphabetPNG/X.png')[,,1,drop=F], drop=3))),
  y <- as.vector(t(1-adrop(readPNG('AlphabetPNG/Y.png')[,,1,drop=F], drop=3))),
  z <- as.vector(t(1-adrop(readPNG('AlphabetPNG/Z.png')[,,1,drop=F], drop=3)))
)

words <- list(
  cat <- cbind(c,a,t), bow <- cbind(b,o,w),
  sip <- cbind(s,i,p), rad <- cbind(r,a,d),
  zen <- cbind(z,e,n), two <- cbind(t,w,o),
  rub <- cbind(r,u,b), vex <- cbind(v,e,x),
  fox <- cbind(f,o,x), wry <- cbind(w,r,y),
  vow <- cbind(v,o,w), zag <- cbind(z,a,g),
  quo <- cbind(q,u,o), fry <- cbind(f,r,y),
  the <- cbind(t,h,e), pew <- cbind(p,e,w),
  dug <- cbind(d,u,g), keg <- cbind(k,e,w),
  yak <- cbind(y,a,k), tax <- cbind(t,a,x),
  jaw <- cbind(j,a,w), who <- cbind(w,h,o),
  lax <- cbind(l,a,x), til <- cbind(t,i,l),
  sin <- cbind(s,i,n), mud <- cbind(m,u,d),
  yap <- cbind(y,a,p), orb <- cbind(o,r,b),
  ply <- cbind(p,l,y), cry <- cbind(c,r,y),
  tom <- cbind(t,o,m), coy <- cbind(c,o,y),
  any <- cbind(a,n,y), jot <- cbind(j,o,t),
  she <- cbind(s,h,e), gig <- cbind(g,i,g),
  axe <- cbind(a,x,e), icy <- cbind(i,c,y),
  elm <- cbind(e,l,m), owl <- cbind(o,w,l),
  gag <- cbind(g,a,g), nun <- cbind(n,u,n),
  jay <- cbind(j,a,y), rye <- cbind(r,y,e),
  apt <- cbind(a,p,t), sty <- cbind(s,t,y),
  lit <- cbind(l,i,t), why <- cbind(w,h,y),
  hue <- cbind(h,u,e), use <- cbind(u,s,e)
)

sigmoid.activation <- function(x){
  #return(1 / (1+exp(-x)))
  return(x)
}


forward.pass <- function(input, input.hidden.weights, hidden.bias.weights){ #calculate output activations with "winner-takes-all" method
  
  hidden <- numeric(n.hidden)
  for(i in 1:n.hidden){
    hidden[i] <- sigmoid.activation(sum(input * input.hidden.weights[,i]) + hidden.bias.weights[i,1])
  }
  hidden[hidden != max(hidden)] <- 0
  hidden[which.max(hidden)] <- 1
  return(hidden)
}

forward.pass.2 <- function(input, input.hidden.weights, hidden.bias.weights, hidden.output.weights, output.bias.weights){ #calculate output activations with "winner-takes-all" method
  
  hidden <- numeric(n.hidden)
  for(i in 1:n.hidden){
    hidden[i] <- sigmoid.activation(sum(input * input.hidden.weights[,i]) + hidden.bias.weights[i,1])
  }
  hidden[hidden != max(hidden)] <- 0
  hidden[which.max(hidden)] <- 1
  
  output <- numeric(n.output)
  for(b in 1:n.output){
    output[b] <- sigmoid.activation(sum(hidden * hidden.output.weights[,b] +  output.bias.weights[b,1]))
  }
  output[output != max(output)] <- 0
  output[which.max(output)] <- 1
  return(list(hidden=hidden, output=output))
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

display.learning.curves <- function(results){
  for(i in 1:n.hidden){
    layout(matrix(1:4, nrow=2))
    plot(results$learning.curve[,i], main=paste("Node",i))
    plot(results$bias.tracker[,i])
    image(matrix(results$input.hidden.weights[,i], nrow = 40))
  }
}

trace.update.2 <- function(input, input.hidden.weights, trace.hidden, hidden.bias.weights, hidden.output.weights, trace.output, output.bias.weights){
  
  forward.pass.results <- forward.pass.2(input, input.hidden.weights, hidden.bias.weights, hidden.output.weights, output.bias.weights)
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


display.learning.curves <- function(results){
  for(i in 1:n.hidden){
    layout(matrix(1:4, nrow=2))
    plot(results$learning.curve[,i], main=paste("Node",i))
    plot(results$bias.tracker[,i])
    image(matrix(results$input.hidden.weights[,i], nrow = 40))
  }
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


batch.2 <- function(n.epochs){ 
  # network properties #
  input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=0.05), nrow=n.input, ncol=n.hidden) #initialiize weights at random values between 0 and 0.05
  hidden.bias.weights <- matrix(0, nrow=n.hidden, ncol=1)
  hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=0.05), nrow=n.hidden, ncol=n.output)
  output.bias.weights <- matrix(0, nrow=n.output, ncol=1)
  trace.hidden <- rep(0, times = n.hidden)
  trace.output <- rep(0, times = n.output)
  
  # tracking learning #
  learning.curve <- matrix(0, nrow = n.epochs/100, ncol = n.hidden) #initializes learning data matrix
  bias.tracker <- matrix(0, nrow = n.epochs/100, ncol = n.hidden) #initializes learning data matrix
  hidden.win.tracker <- matrix(0, nrow=n.epochs, ncol= n.hidden)
  
  pb <- txtProgressBar(min=1, max=n.epochs,style=3)
  for(i in 1:n.epochs){
    word <- words[[sample(1:50,1, replace = T)]]
    for(b in 1:(length(word)/n.input)){
      letter <- word[,b]
      results <- trace.update.2(letter, input.hidden.weights, trace.hidden, hidden.bias.weights, hidden.output.weights, trace.output, output.bias.weights)
      input.hidden.weights <- results$input.hidden.weights
      hidden.output.weights <- results$hidden.output.weights
      trace.hidden <- results$trace.hidden
      hidden.bias.weights <- results$hidden.bias.weights
      hidden.output.weights <- results$hidden.output.weights
      trace.output <- results$trace.output
      output.bias.weights <- results$output.bias.weights
      setTxtProgressBar(pb, i)
    }
  }
  return(list(
    input.hidden.weights=input.hidden.weights,
    hidden.output.weights=hidden.output.weights
  ))
}




results <- batch.2(n.epochs) #run training batches
display.learning.curves(results) #visualize learning by plotting weight similarity to alphabet input every 100 epochs




## output storage func. and weight image generation ##

output.storage <- function(){ #stores outputs 
  hidden.outputs <- matrix(0, nrow = n.test, ncol = n.hidden)
  for(i in 1:26){
    one.hidden <- forward.pass(alphabet[[i]])
    hidden.outputs[i,] <- one.hidden
  }
  return(hidden.outputs)
}

weight.images <- function(){
  return(
    for(i in 1:26){
      image(matrix(input.hidden.weights[,i], nrow = 40))
    })
}

image(results$hidden.win.tracker)