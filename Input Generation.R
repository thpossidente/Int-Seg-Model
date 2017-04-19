## Generating Inputs ##


random.inputs <- function(){
  one.zero <- c(0,1)
  input.list <- vector('list', num.inputs.generated)
  for(i in 1:num.inputs.generated){
    input.list[[i]] <- c(sample(one.zero, n.input/2, replace = TRUE, c(0.9, 0.1)))
  }
  
  grouped.inputs <- vector('list', (num.inputs.generated/5))
  counter <- -1
  for(b in 1:(num.inputs.generated/5)){
    counter <- counter + 1
    grouped.inputs[[b]] <- list(input.list[[(counter*5)+1]],
                                input.list[[(counter*5)+2]],
                                input.list[[(counter*5)+3]],
                                input.list[[(counter*5)+4]],
                                input.list[[(counter*5)+5]]
                                )
  }
  return(list(input.list, grouped.inputs))
}

input.generation <- function(input.gen.parameter){
  results.random.inputs <- random.inputs()
  input.list <- results.random.inputs[1]
  grouped.inputs <- results.random.inputs[2]
  
  if(input.gen.parameter == 1){ #System 1 gets temporal patterns of 5 inputs in order. System 2 gets random input every time
    inputs <- matrix(NA, nrow=n.epochs, ncol=n.input)
    for(i in 0:((n.epochs/5)-1)){
      random.group <- grouped.inputs[[1]][[sample(1:(num.inputs.generated/5),1,replace=T)]]
      for(h in 1:5){
        inputs[(i*5)+h,] <- c(random.group[[h]], input.list[[1]][[sample(1:num.inputs.generated,1, replace = TRUE)]])
      }
    }
    return(inputs) ##inputs data not quite right
  }
  
  if(input.gen.parameter == 0.5){
    inputs <- matrix(NA, nrow=n.epochs, ncol=n.input)
    paired.inputs <- matrix(NA, nrow = num.inputs.generated, ncol = n.input)
    new.input.list1 <- input.list
    new.input.list2 <- input.list
    for(b in 1:num.inputs.generated){
      random1 <- sample(1:length(new.input.list1), 1, replace=T)
      random2 <- sample(1:length(new.input.list2), 1, replace=T)
      paired.inputs[b,] <- c(new.input.list1[[1]][[random1]], new.input.list2[[1]][[random2]])
      new.input.list1[[1]][[random1]] <- NULL
      new.input.list2[[1]][[random2]] <- NULL
    }
    for(c in 1:n.epochs){
      inputs[c,] <- paired.inputs[sample(1:num.inputs.generated,1, replace=T),]
    }
    retutrn(inputs)
  }
  
  if(input.gen.parameter == 0){
    
  }
}


### simple example of dependency

# (1, 3) -> (5, 7)
# (1, 4) -> (6, 8)
# (2, 3) -> (5, 8)
# (2, 4) -> (6, 7)