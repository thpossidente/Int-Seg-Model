#install.packages('ggplot2')
#install.packages('ggplot')
#install.packages('png')
#install.packages('abind')
#install.packages('dplyr')

library('ggplot2')

#load('trained_hidden_weights_20000_CosAnneal') # loading weights as variable 'network'


source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network-split.R')
Rcpp::sourceCpp('forwardPassCpp.cpp')

n.input <- 1600
n.hidden <- 500 
n.output <- 30  #Must be multiple of 10 due to activation percentage calculation
learning.rate.hidden.max = 0.5
learning.rate.hidden.min = 0.0001
learning.rate.hidden <- 0.00001
learning.rate.output <- 0.0001
learning.rate.output.max <- 0.5 # 0.009
learning.rate.output.min <- 0.00001
restarts <- 5 # 5
n.epochs <- 1000
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 1 
hidden.bias.param.minus <- 0 # 0.05
hidden.bias.param.plus <- 0 # 0.0005
output.bias.param.minus <- 0.25 #0.5
output.bias.param.plus <- 0.001 #0.0005
sparseness.percent <- 0.75  # sparseness.percent is % nodes inactive #0.75
num.inputs.generated <- n.input/2 # half of total inputs
integration.parameter <- 1 #0 is totally segregated, 1 is totally integrated
percent.act.input <- 0.05 # 0.05
percent.act.output <- 0.03333 # .10
n.words <- length(words)
letter.noise.param <- 0.1
input.gen.parameter <- 0 # if 1: temporal pattern of input for one system, random pattern for other system. (one system predicts next input) 
# if 0: Next inputs are predicted by combination of both systems' previous inputs - one system alone cannot predict next inputs
# if 0.5: inputs for each system consistently co-occur
  
## RUN ##
  
results <- batch_split(n.epochs, network=NA) #run training batches

visualize.hidden.layer.learning(results$history)
visualize.output.act.match()
plot(x=seq(from = 1, to = n.epochs/100, by = 1), y=results$history$output.act.unique.tracker, type='b', ylim=c(0,1))
test.word.continuity1(results$network, words)
#plot(x=seq(from=100, to=n.epochs, by=100), y=results$history$mutual.info.tracker, type = 'b', ylim=c(0,5), xlab = "Epochs", ylab = "Mutual Information")
plot(x=seq(from=0, to=n.epochs, by=100), y=results$history$mutual.info.spatial.track , type = 'b', ylim=c(0,5), xlab = "Epochs", ylab = "Mutual Information")




plot(x=seq(from=0, to=n.epochs, by=100), y=results$history$output.bias.tracker[,4], type='b', ylim=c(0,0.5))

visualize.letter.activations(results$network, b)

trace <- results$history$output.trace.tracker
plot(x = seq(1:nrow(trace)), y = trace[,3])
