display.learning.curves <- function(results){
  for(i in 1:n.hidden){
    layout(matrix(1:4, nrow=2))
    #plot(results$history$learning.curve[,i], main=paste("Node",i), ylim = 0,1000)
    plot(results$history$bias.tracker[,i])
    image(t(apply(matrix(results$network$input.hidden.weights[,i], nrow = 40),1,rev)))
  }
}

display.output.bias.tracker <- function(results){
  for(i in 1:n.output){
    plot(results$history$output.bias.tracker[,i], main=paste('Node', i))
  }
}

test.word.continuity <- function(network, words){
  
  n.letters <- 0
  for(i in 1:length(words)){
    n.letters <- n.letters + ncol(words[[i]])
  }
  
  input.matrix <- matrix(0, ncol=n.input, nrow=n.letters)
  r <- 1
  for(i in 1:length(words)){
    for(j in 1:ncol(words[[i]])){
      input.matrix[r,] <- words[[i]][,j]
      r <- r + 1
    }
  }
  
  temp.layer.activations(network, input.matrix)
} 


temp.layer.activations <- function(network, input.matrix){
  
  storing.activations <- matrix(0, nrow=nrow(input.matrix), ncol=n.output)
  
  for(i in 1:nrow(input.matrix)){
    act.results <- forward.pass(input.matrix[i,], network$input.hidden.weights, network$hidden.bias.weights, network$hidden.output.weights, network$output.bias.weights)
    storing.activations[i,] <- act.results$output
  }
  
  output.results <- data.frame(letter=numeric(),output=numeric())
  for(i in 1:nrow(storing.activations)){
    for(j in which(storing.activations[i,] == max(storing.activations[i,]))){
      output.results <- rbind(output.results, c(letter=i, output=j))  
    }
  }
  colnames(output.results) <- c("letter", "output")
  
  ## accuracy measurement ##
  n <- 1
  g <- (n.output/10) * 3
  g. <- (n.output/10) * 3
  
  counter <- 0
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:g]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:g])))
  n <- g + 1
  g <- g + g.
  
  for(h in 1:n.output){
    counter <- counter + sum(c(output.results$output[n:(g-(n.output/10))]) == h)
  }
  counter <- counter - length(unique(c(output.results$output[n:(g-(n.output/10))])))
  
  percentage <- counter/(((n.output/10)*2*26) - (n.output/10))
  ###
  
  g <- ggplot(output.results, aes(x=letter, y=output)) + 
    geom_point()+
    ylim(1,50)+
    theme_bw()
  print(g)
  
  print(storing.activations)
  sprintf("Percent paired: %f", percentage)
}

visualize.letter.activations <- function(network, input){
  result <- forward.pass(input, network$input.hidden.weights, network$hidden.bias.weights, network$hidden.output.weights, network$output.bias.weights)
  active.nodes <- which(result$hidden == max(result$hidden))
  nplots <- length(active.nodes) + 2
  nrow <- round(sqrt(nplots))
  ncol <- ceiling(nplots / nrow)
  layout(matrix(1:(nrow*ncol), nrow=nrow))
  image(t(apply(matrix(input, nrow = 40),1,rev)))
  for(act in active.nodes){
    image(t(apply(matrix(network$input.hidden.weights[,act], nrow = 40),1,rev)))
  }
  all.active.nodes <- network$input.hidden.weights[,active.nodes]
  m.fun <- function(x) { return(mean(x, na.rm=T)) }
  average.weights <- apply(all.active.nodes, 1, m.fun)
  image(t(apply(matrix(average.weights, nrow = 40),1,rev)))
}
