library(ggplot2)

n.input <- 1600
n.hidden <- 150
n.output <- 9
learning.rate.hidden <- 0.00005
learning.rate.output <- 0.2
n.epochs <- 50000
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.6
hidden.bias.param.minus <- 2
hidden.bias.param.plus <- 0.05
output.bias.param.minus <- 0
output.bias.param.plus <- 0

source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network.R')

## RUN ##

results <- batch(n.epochs) #run training batches

display.learning.curves(results) #visualize learning by plotting weight similarity to alphabet input every 100 epochs
display.output.bias.tracker(results)
test.word.continuity(results$network, words)

results$network$hidden.output.weights

results$network$output.bias.weights

#results <- batch(n.epochs, network = results$network)
