#install.packages('ggplot2')
#install.packages('ggplot')
#install.packages('png')
#install.packages('abind')
#install.packages('dplyr')

library('ggplot2')

source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network.R')
Rcpp::sourceCpp('forwardPassCpp.cpp')

n.input <- 1600
n.hidden <- 500 
n.output <- 10  #Must be multiple of 10 due to activation percentage calculation
learning.rate.hidden <- 0.005
learning.rate.output <- 0.009 # 0.009
n.epochs <- 7500  #10000
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.8 #0.8
hidden.bias.param.minus <- 0.05
hidden.bias.param.plus <- 0.0005
output.bias.param.minus <- 0 #0
output.bias.param.plus <- 0 #0
sparseness.percent <- 0.75  # sparseness.percent is % nodes inactive #0.75
num.inputs.generated <- n.input/2 # half of total inputs
integration.parameter <- 1 #0 is totally segregated, 1 is totally integrated
percent.act.input <- 0.05
percent.act.output <- 0.1
n.words <- length(words)
letter.noise.param <- 0.1
delay.param = 3  # number of hidden activations saved and used as input along with current input into output layer
input.gen.parameter <- 0 # if 1: temporal pattern of input for one system, random pattern for other system. (one system predicts next input) 
# if 0: Next inputs are predicted by combination of both systems' prehious inputs - one system alone cannot predict next inputs
# if 0.5: inputs for each system consistently co-occur

## RUN ##



counter = 1
res = matrix(0, nrow=100, ncol = 7)

for(i in seq(0.1, 1, 0.02)){
  trace.param.output = i
  
  res[counter, 1] = mean(results$history$mutual.info.tracker[45:50])

  res[counter, 2] = i
  
  results <- batch(n.epochs) #run training batches
  
  
  res[counter, 3] = mean(results$history$mutual.info.tracker[50:55])
  res[counter, 4] = mean(results$history$mutual.info.tracker[55:60])
  res[counter, 5] = mean(results$history$mutual.info.tracker[60:65])
  res[counter, 6] = mean(results$history$mutual.info.tracker[65:70])
  res[counter, 7] = mean(results$history$mutual.info.tracker[70:75])
  
  
  counter = counter + 1
  
}

res
