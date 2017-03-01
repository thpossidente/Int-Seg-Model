n.input <- 1600
n.hidden <- 26
n.output <- 50
learning.rate <- 0.05
n.epochs <- 2000
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



input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=0.05), nrow=n.input, ncol=n.hidden) #initialiize weights at random values between 0 and 0.1
hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=0.05), nrow=n.hidden, ncol=n.output)


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
    trace.hidden[i] <- (1 - trace.param.hidden) * trace.hidden[i] + trace.param.hidden * hidden[i] 
    input.hidden.weights[,i] <- input.hidden.weights[,i] + learning.rate * trace.hidden[i] * (input - input.hidden.weights[,i])  
  }
  return(list(trace.hidden = trace.hidden, input.hidden.weights = input.hidden.weights))
  
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
    trace.hidden <- results$trace.hidden
    #hidden.output.weights <- results$hidden.output.weights
  }
  return(input.hidden.weights)
}


#batch <- function(n.epochs){ 
  
  #for(i in 1:n.epochs){
    #word <- words[[sample(1:50,1, replace = T)]]
    #for(b in 1:(length(word)/n.input)){
      #letter[b] <- word[,b]
      #results <- trace.update(letter[b], input.hidden.weights, hidden.output.weights, trace.hidden, trace.output)
      #results <- trace.update(letter[b], input.hidden.weights, trace.hidden)
      #input.hidden.weights <- results$input.hidden.weights
      #hidden.output.weights <- results$hidden.output.weights
    #}
  #}
  #return(output.storage())
#}

input.hidden.weights <- batch(n.epochs)  #run training batches and save weights to global envirn.
weight.images() #connection weight visualization by row (26). Letters will appear rotated 180 degrees.




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
