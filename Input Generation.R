## Generating Inputs ##


random.inputs <- function(){
  one.zero <- c(0,1)
  input.list <- vector('list', num.inputs.generated)
  for(i in 1:num.inputs.generated){
    input.list[[i]] <- c(sample(one.zero, n.input/2, replace = TRUE, c(0.9, 0.1)))
  }
  
  grouped.inputs <- vector('list', num.inputs.generated/5)
  for(h in 1:(num.inputs.generated/5)){
    grouped.inputs[[h]] <- list(input.list[[]]) ## how to group?
  }
  
  return(input.list)
}

input.generation <- function(integration.parameter){
  input.list <- random.inputs()
  if(integration.parameter == 0){ #System 1 gets temporal pattern of ?? inputs in order. System 2 gets random input everyt ime
    inputs <- vector('list', n.epochs)
    for(i in 1:n.epochs){
      for(h in 1:5){
        inputs[[h]] <- c(input.list[[]], input.list[[sample(1:num.inputs.generated,1, replace = TRUE)]])
      }
    }
    return(inputs)
  }
  
  if(integration.parameter == 1){
    
  }
}

