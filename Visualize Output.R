display.learning.curves <- function(results){
  for(i in 1:n.hidden){
    layout(matrix(1:4, nrow=2))
    plot(results$history$learning.curve[,i], main=paste("Node",i))
    plot(results$history$bias.tracker[,i])
    image(matrix(results$network$input.hidden.weights[,i], nrow = 40))
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
  
  image(storing.activations)
  print(storing.activations)
}
