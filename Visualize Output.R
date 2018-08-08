#install.packages('dplyr')
library(dplyr)
#install.packages("magrittr")
library(magrittr)


display.learning.curves <- function(results){
  for(i in 1:n.hidden){
    layout(matrix(1:4, nrow=2))
    #plot(results$history$learning.curve[,i], main=paste("Node",i), ylim = 0,1000)
    #plot(results$history$bias.tracker[,i])
    image(t(apply(matrix(results$network$input.hidden.weights[,i], nrow = 40),1,rev)))
  }
}


display.output.bias.tracker <- function(results){
  for(i in 1:n.output){
    plot(results$history$output.bias.tracker[,i], main=paste('Node', i))
  }
}


temp.layer.many.activations <- function(network, words){
  
  input.matrix1 <- matrix(0, ncol=n.input, nrow <- length(alphabet) * 5) #5 passes through all words
  r <- 1
  for(k in 1:5){
    for(p in 1:length(words)){
      for(j in 1:ncol(words[[p]])){
        input.matrix1[r,] <- words[[p]][,j]
        r <- r + 1
      }
    }
  }
  
  storing.activations <- matrix(0, nrow=nrow(input.matrix1), ncol=n.output)
  
  for(i in 1:nrow(input.matrix1)){
    act.results <- forward.pass(input.matrix1[i,], network$input.hidden.weights, network$hidden.bias.weights, network$hidden.output.weights, network$output.bias.weights)
    storing.activations[i,] <- act.results$output
  }
  return(storing.activations)
  
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
  
  return(temp.layer.activations(network, input.matrix))
} 


temp.layer.activations <- function(network, input.matrix){
  
  storing.activations <- matrix(0, nrow=nrow(input.matrix), ncol=n.output)
  
  for(i in 1:nrow(input.matrix)){
    act.results <- forwardPass(n.output, percent.act.input, percent.act.output,
                              n.hidden, input.matrix[i,], network$input.hidden.weights, 
                              network$hidden.bias.weights, network$hidden.output.weights, 
                              network$output.bias.weights, network$hidden.activation.delay,
                              delay.param)
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
  counter <- 1
  num.matches <- 0
  act.per.word <- n.output * percent.act.output * 3
  for(b in seq(from = act.per.word, to = length(output.results$output) + act.per.word, by = act.per.word)){
    freq <- rle(sort(output.results$output[counter:b]))
    counter <- counter + act.per.word 
    for(h in 1:length(freq$lengths)){
      if(freq$lengths[h] > 1){
        num.matches = num.matches + freq$lengths[h]
      }
    }
  }

    percentage <- num.matches/(length(output.results$output))
  ###

  g <- ggplot(output.results, aes(x=letter, y=output)) + 
    geom_point()+
    ylim(1,50)+
    theme_bw()
  
  return(percentage*100)
}

test.word.continuity1 <- function(network, words){
  
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
  
  temp.layer.activations1(network, input.matrix)
} 


temp.layer.activations1 <- function(network, input.matrix){
  
  storing.activations <- matrix(0, nrow=nrow(input.matrix), ncol=n.output)
  
  for(i in 1:nrow(input.matrix)){
    act.results <- forwardPass(n.output, percent.act.input, percent.act.output,
                               n.hidden, input.matrix[i,], network$input.hidden.weights, 
                               network$hidden.bias.weights, network$hidden.output.weights, 
                               network$output.bias.weights, network$hidden.activation.delay,
                               delay.param)
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
  counter <- 1
  num.matches <- 0
  act.per.word <- n.output * percent.act.output * 3
  for(b in seq(from = act.per.word, to = length(output.results$output) + act.per.word, by = act.per.word)){
    freq <- rle(sort(output.results$output[counter:b]))
    counter <- counter + act.per.word
    for(h in 1:length(freq$lengths)){
      if(freq$lengths[h] > 1){
        num.matches = num.matches + freq$lengths[h]
      }
    }
  }
  
  percentage <- num.matches/(length(output.results$output))
  ###
  
  g <- ggplot(output.results, aes(x=letter, y=output)) + 
    geom_point()+
    ylim(1,50)+
    theme_bw()
  
  return(g)
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
  average.weights <- calculate.mean.weights(all.active.nodes)
  image(t(apply(matrix(average.weights, nrow = 40),1,rev)))
}


calculate.mean.weights <- function(active.nodes){
  m.fun <- function(x) { return(mean(x, na.rm=T)) }
  average.weights <- apply(active.nodes, 1, m.fun)
  return(average.weights)
}


hidden.layer.similarity <- function(letter, network, comparison.letter=NA){
  result <- forwardPass(n.output, percent.act.input, percent.act.output,
                        n.hidden, letter, network$input.hidden.weights, 
                        network$hidden.bias.weights, network$hidden.output.weights, 
                        network$output.bias.weights, network$hidden.activation.delay,
                        delay.param)
  active.nodes <- which(result$hidden == max(result$hidden))
  all.active.nodes <- network$input.hidden.weights[,active.nodes]
  average.weights <- calculate.mean.weights(all.active.nodes)
  if(!all(is.na(comparison.letter))){
    similarity <- sum(abs(comparison.letter - average.weights), na.rm = T)
  } else {
    similarity <- sum(abs(letter - average.weights), na.rm = T)
  }
  return(similarity)
}


batch.hidden.layer.learning <- function(letters, network){
  result <- data.frame(input=names(letters), similarity=NA)
  for(i in 1:nrow(result)){
    result[i,"similarity"] <- hidden.layer.similarity(letters[[names(letters)[i]]], network)
  }
  return(result)
}

visualize.hidden.layer.learning <- function(history){
  plotting.data <- expand.grid(letter=names(letters), time=1:nrow(history$hidden.letter.similarity.tracking))
  plotting.data$similarity <- mapply(function(l, t){
    return(history$hidden.letter.similarity.tracking[t,which(names(letters)==l)])
  }, plotting.data$letter, plotting.data$time)
  summary.data <- plotting.data %>% group_by(time) %>% summarize(mean.similarity = mean(similarity))
  ggplot(plotting.data, aes(x=time, y=similarity, color = letter))+ geom_line() + 
    geom_line(data=summary.data, aes(x=time, y=mean.similarity, color=NA), size=2)+
    labs(x='time', y='difference between network representation and input letter')
}


hidden.layer.stability <- function(letter, input, network, history){
  result <- forward.pass(n_output, percentActInput,
                         percentActOutput, n_hidden,
                         input, inputToHiddenWeights,
                         hiddenBiasWeights, hiddenToOutputWeights,
                         outputBiasWeights, hiddenActivationDelay,
                         delayParam)
  active.nodes <- which(result$hidden == max(result$hidden))
  previous.active.nodes <- history$hidden.stability.tracking[[letter]]
  change <- length(active.nodes) - sum(active.nodes %in% previous.active.nodes)
  return(change)
}


batch.hidden.layer.stability <- function(letters, network, history){
  result <- data.frame(input=names(letters), stability=NA)
  for(i in 1:nrow(result)){
    result[i,"stability"] <- hidden.layer.stability(names(letters)[i], letters[[names(letters)[i]]], network, history)
  }
  return(result$stability)
}


update.hidden.layer.stability <- function(letters, network){
  tracker <- sapply(names(letters), function(x){
    result <- forward.pass(letters[[x]], network$input.hidden.weights, network$hidden.bias.weights, network$hidden.output.weights, network$output.bias.weights)
    active.nodes <- which(result$hidden == max(result$hidden))
    return(active.nodes)
  }, USE.NAMES = T, simplify=FALSE)
  return(tracker)
}


visualize.output.act.match <- function(){
  plot(results$history$output.match.tracker, ylim = range(0, 100), type='o', ann = F)
  title(xlab = 'Time', ylab = 'Percentage of activation matches')
}


output.act.unique <- function(network, words){
  
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
  
  storing.activations <- matrix(0, nrow=nrow(input.matrix), ncol=n.output)
  
  for(i in 1:nrow(input.matrix)){
    act.results <- forwardPass(n.output, percent.act.input, percent.act.output,
                               n.hidden, input.matrix[i,], network$input.hidden.weights, 
                               network$hidden.bias.weights, network$hidden.output.weights, 
                               network$output.bias.weights, network$hidden.activation.delay,
                               delay.param)
    storing.activations[i,] <- act.results$output
  }
  
  output.results <- data.frame(letter=numeric(),output=numeric())
  for(i in 1:nrow(storing.activations)){
    for(j in which(storing.activations[i,] == max(storing.activations[i,]))){
      output.results <- rbind(output.results, c(letter=i, output=j))  
    }
  }
  colnames(output.results) <- c("letter", "output")
  
  output.results.grouped <- list(output.results$output[1:(n.output*percent.act.output*3)],
                                 output.results$output[((n.output*percent.act.output*3)+1):(2*(n.output*percent.act.output*3))],
                                 output.results$output[(2*(n.output*percent.act.output*3)+1):(3*(n.output*percent.act.output*3))],
                                 output.results$output[(3*(n.output*percent.act.output*3)+1):(4*(n.output*percent.act.output*3))],
                                 output.results$output[(4*(n.output*percent.act.output*3)+1):(5*(n.output*percent.act.output*3))],
                                 output.results$output[(5*(n.output*percent.act.output*3)+1):(6*(n.output*percent.act.output*3))],
                                 output.results$output[(6*(n.output*percent.act.output*3)+1):(7*(n.output*percent.act.output*3))],
                                 output.results$output[(7*(n.output*percent.act.output*3)+1):(8*(n.output*percent.act.output*3))],
                                 output.results$output[(8*(n.output*percent.act.output*3)+1):((9*(n.output*percent.act.output*3))-(n.output*percent.act.output))]
                                 )
  
  act.unique.perc <- numeric(nrow(output.results))
  counter1 = 0
  for(n in 1:length(words)){
    for(x in 1:(ncol(words[[n]])*(n.output*percent.act.output))){
      counter1 = counter1 + 1
      sum.in.word <- sum(output.results.grouped[[n]] == output.results$output[counter1])
      act.unique.perc[counter1] <- sum.in.word/sum(output.results$output == output.results$output[counter1])
    }
  }
  
  return(mean(act.unique.perc))
  
}



mutual.info.output <- function(network){
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
  
  storing.activations <- matrix(0, nrow=nrow(input.matrix), ncol=n.output)
  
  for(i in 1:nrow(input.matrix)){
    act.results <- forwardPass(n.output, percent.act.input,
                               percent.act.output, n.hidden,
                               input.matrix[i,], network$input.hidden.weights,
                               network$hidden.bias.weights, network$hidden.output.weights,
                               network$output.bias.weights, network$hidden.activation.delay,
                               delay.param)
    storing.activations[i,] <- act.results$output
  }
  
  output.results <- data.frame(letter=numeric(),output=numeric())
  for(i in 1:nrow(storing.activations)){
    for(j in which(storing.activations[i,] == max(storing.activations[i,]))){
      output.results <- rbind(output.results, c(letter=i, output=j))  
    }
  }
  colnames(output.results) <- c("word", "output")
  
  # word.acts <- matrix(0, nrow = 9, ncol <- 3)
  # acts <- numeric(3)
  # counter2 <- 0
  # counter3 <- 0
  # for(f in 1:24){
  #   counter2 <- counter2 + 1
  #   acts[counter2] <- output.results[f,2]
  #   if(f %% 3 == 0){
  #     counter2 = 0
  #     counter3 <- counter3 + 1
  #     word.acts[counter3,1:3] <- acts
  #     }
  # }
  # word.acts[9,1:2] <- c(output.results[25,2], output.results[26,2])
  counter5 <- 1
  for(z in 1:26){
      output.results[z,1] <- counter5
      if(z %% 3 == 0){
        counter5 <- counter5 + 1
      }
  }
  
  mutual.info <- 0
  probs.word <- numeric(9)
  probs.act <- numeric(10)
  probs.joint <- numeric(90)
  mutual.info <- numeric(90)
  count3 <- 0
  count4 <- 0
  
  for(w in 1:9){
    probs.word[w] <- sum(output.results[,1] == w) / length(output.results[,1])
  }
  for(a in 1:10){
    probs.act[a] <- sum(output.results[,2] == a) / length(output.results[,2])
  }
  
  for(k in 1:9){
    for(h in 1:10){
      count3 <- count3 + 1
      probs.joint[count3] <- sum((output.results[,1] == k) & (output.results[,2] == h)) / length(output.results[,1])
      
    }
  }
  
  for(x in 1:9){
    for(t in 1:10){
      count4 <- count4 + 1
      mutual.info[count4] = (probs.joint[count4] * (log2((probs.joint[count4])/(probs.act[t]*probs.word[x]))))
    }
  }
  
  mutual.info <- sum(mutual.info, na.rm = T)
  
  return(mutual.info)
  
}




