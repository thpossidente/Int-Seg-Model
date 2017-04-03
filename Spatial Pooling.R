n.input <- 1600
n.hidden <- 26
n.output <- 20
learning.rate <- 0.05
n.epochs <- 10000
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.2
hidden.bias.param.minus <- 1
hidden.bias.param.plus <- 0.05
output.bias.param.minus <- 1
output.bias.param.plus <- 0.05

source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network.R')

## RUN ##

results <- batch(n.epochs) #run training batches

display.learning.curves(results) #visualize learning by plotting weight similarity to alphabet input every 100 epochs
display.output.bias.tracker(results)

