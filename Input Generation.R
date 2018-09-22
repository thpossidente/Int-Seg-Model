## Generating Inputs ##


random.inputs <- function(){
  one.zero <- c(1,0)
  input.list <- vector('list', num.inputs.generated)
  for(i in 1:num.inputs.generated){
    input.list[[i]] <- c(sample(one.zero, n.input/2, replace = TRUE, c(0.1, 0.9)))
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
    return(inputs) 
  }
  
  if(input.gen.parameter == 0.5){ #each system 1 input is always paired with the same system 2 input.
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
    counter <- -1
    grouped.paired.inputs <- vector('list', (num.inputs.generated/5))
    for(c in 1:(num.inputs.generated/5)){
      counter <- counter + 1
      grouped.paired.inputs[[c]] <- list(paired.inputs[((counter*5)+1),],
                                    paired.inputs[((counter*5)+2),],
                                    paired.inputs[((counter*5)+3),],
                                    paired.inputs[((counter*5)+4),],
                                    paired.inputs[((counter*5)+5),])
    }
    for(t in 0:((n.epochs/5)-1)){
      random.group <- grouped.paired.inputs[[sample(1:(num.inputs.generated/5),1,replace=T)]]
      for(e in 1:5){
      inputs[(t*5)+e,] <- random.group[[e]]
      }
    }
    return(inputs)
  }
  
  if(input.gen.parameter == 0){ #Second input depends on both systems' first input 
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
    for(h in seq(1, n.epochs, 2)){
      random.num.1 <- sample(1:(num.inputs.generated),1,replace = T)
      random.num.2 <- sample(1:(num.inputs.generated),1,replace = T)
      added <- random.num.1 + random.num.2
      if(added > (num.inputs.generated)){
        added <- added %% (num.inputs.generated)
        if(added == 0){
          added <- 1
        }
      }
      inputs[h,] <- c(input.list[[1]][[random.num.1]], input.list[[1]][[random.num.2]]) ## will pairs of just 2 work for learning?
      inputs[h+1,] <- paired.inputs[added,]
    }
    }
  }





### simple example of dependency

# (1, 3) -> (5, 7)
# (1, 4) -> (6, 8)
# (2, 3) -> (5, 8)
# (2, 4) -> (6, 7)