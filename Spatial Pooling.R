n.input <- 1600
n.hidden <- 26
n.output <- 50
learning.rate <- 0.1
n.epochs <- 1000
n.test <- 26
trace.hidden <- rep(0, times = n.hidden)
trace.output <- rep(0, times = n.output)
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.2


#install.packages('png')
library('png')
#install.packages('abind')
library('abind')

alphabet <- list(
  a <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/A.png')[,,1,drop=F], drop=3),
  b <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/B.png')[,,1,drop=F], drop=3),
  c <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/C.png')[,,1,drop=F], drop=3),
  d <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/D.png')[,,1,drop=F], drop=3),
  e <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/E.png')[,,1,drop=F], drop=3),
  f <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/F.png')[,,1,drop=F], drop=3),
  g <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/G.png')[,,1,drop=F], drop=3),
  h <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/H.png')[,,1,drop=F], drop=3),
  i <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/I.png')[,,1,drop=F], drop=3),
  j <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/J.png')[,,1,drop=F], drop=3),
  k <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/K.png')[,,1,drop=F], drop=3),
  l <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/L.png')[,,1,drop=F], drop=3),
  m <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/M.png')[,,1,drop=F], drop=3),
  n <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/N.png')[,,1,drop=F], drop=3),
  o <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/O.png')[,,1,drop=F], drop=3),
  p <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/P.png')[,,1,drop=F], drop=3),
  q <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/Q.png')[,,1,drop=F], drop=3),
  r <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/R.png')[,,1,drop=F], drop=3),
  s <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/S.png')[,,1,drop=F], drop=3),
  t <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/T.png')[,,1,drop=F], drop=3),
  u <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/U.png')[,,1,drop=F], drop=3),
  v <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/V.png')[,,1,drop=F], drop=3),
  w <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/W.png')[,,1,drop=F], drop=3),
  x <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/X.png')[,,1,drop=F], drop=3),
  y <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/Y.png')[,,1,drop=F], drop=3),
  z <- adrop(readPNG('C:/Users/research/Documents/GitHub/Int-Seg-Model/AlphabetPNG/Z.png')[,,1,drop=F], drop=3))


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



input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=1), nrow=n.input, ncol=n.hidden) #initialiize weights at random values between 1 and 0
hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=1), nrow=n.hidden, ncol=n.output)


sigmoid.activation <- function(x){
  return(1 / (1+exp(-x)))
}


forward.pass <- function(input){ #calculate output activations with "winner-takes-all" method
  
  hidden <- numeric(n.hidden)
  for(i in 1:n.hidden){
    hidden[i] <- sigmoid.activation(sum(input * input.hidden.weights[,i]))
  }
  hidden[which.max(hidden)] <- 1
  hidden[hidden != max(hidden)] <- 0
  return(hidden)
  #output <- numeric(n.output)
  #for(b in 1:n.output){
  #  output[b] <- sigmoid.activation(sum(hidden * hidden.output.weights[,b]))
  #}
  #output[which.max(output)] <- 1
  #output[output != max(output)] <- 0
  #return(list(hidden=hidden, output=output))
}

trace.update <- function(input, input.hidden.weights, trace.hidden){
  #trace.update <- function(input, input.hidden.weights, hidden.output.weights, trace.hidden, trace.output){ 
  
  hidden <- forward.pass(input)
  #forward.pass.results <- forward.pass(input)
  #hidden <- forward.pass.results$hidden
  #output <- forward.pass.results$output
  
  for(i in 1:n.hidden){
    trace.hidden[i] <- (1 - trace.param.hidden) * trace.hidden[i]  + trace.param.hidden * hidden[i] 
    input.hidden.weights[,i] <- input.hidden.weights[,i] + learning.rate * trace.hidden[i] * (input - input.hidden.weights[,i])  
  }
  return(list(trace.hidden=trace.hidden, input.hidden.weights=input.hidden.weights))
  
  #for(b in 1:n.output){
  #  trace.output <- (1 - trace.param.output) * trace.output[b] + trace.param.output * output[b]
  #  hidden.output.weights[,b] <- hidden.output.weights[,b] + learning.rate * trace.output[i] * (hidden - hidden.output.weights[,b])
  #}
  #return(list(trace.hidden=trace.hidden, trace.ouput=trace.output, input.hidden.weights=input.hidden.weights, hidden.output.weights=hidden.output.weights))
}


batch <- function(n.epochs){ 
  
  for(i in 1:n.epochs){
    letter <- alphabet[[sample(1:26,1, replace = T)]]
    #results <- trace.update(letter, input.hidden.weights, hidden.output.weights, trace.hidden, trace.output)
    results <- trace.update(letter, input.hidden.weights, trace.hidden)
    input.hidden.weights <- results$input.hidden.weights
    #hidden.output.weights <- results$hidden.output.weights
  }
  return(output.storage())
}


#batch <- function(n.epochs){ 
  
  #for(i in 1:n.epochs){
    #word <- words[[sample(1:50,1, replace = T)]]
    #for(b in 1:(length(word)/n.input)){
      #letter[b] <- word[,1 + (1 + ((b-1) * n.input)):(b*n.input)]
      #results <- trace.update(letter[b], input.hidden.weights, hidden.output.weights, trace.hidden, trace.output)
      #results <- trace.update(letter[b], input.hidden.weights, trace.hidden)
      #input.hidden.weights <- results$input.hidden.weights
      #hidden.output.weights <- results$hidden.output.weights
    #}
  #}
  #return(output.storage())
#}

batch(n.epochs)  #run training batches
test <- output.storage()




## entropy testing functions ##

output.storage <- function(){ #stores outputs 
  hidden.outputs <- matrix(0, nrow = n.test, ncol = n.hidden)
  for(i in 1:26){
    one.hidden <- forward.pass(alphabet[[i]])
    hidden.outputs[i,] <- one.hidden
  }
  return(hidden.outputs)
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



