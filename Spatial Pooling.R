library(ggplot2)

n.input <- 1600
n.hidden <- 100
n.output <- 30
learning.rate.hidden <- 0.2
learning.rate.output <- 0.05
n.epochs <- 10000
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.5
hidden.bias.param.minus <- 2
hidden.bias.param.plus <- 0.0005
output.bias.param.minus <- 0
output.bias.param.plus <- 0
sparseness.percent <- 0.8
num.inputs.generated <- 50
integration.parameter <- 1 #0 is totally segregated, 1 is totally integrated
percent.act.input <- 0.05
percent.act.output <- 0.01
input.gen.parameter <- 0 # if 1: temporal pattern of input for one system, random pattern for other system. (one system predicts next input) 
                           # if 0: Next inputs are predicted by combination of both systems' previous inputs - one system alone cannot predict next inputs
                           # if 0.5: inputs for each system consistently co-occur

source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network.R')

## RUN ##

results <- batch(n.epochs) #run training batches

display.learning.curves(results) #visualize learning by plotting weight similarity to alphabet input every 100 epochs
display.output.bias.tracker(results)
test.word.continuity(results$network, words)
visualize.letter.activations(results$network, s)

results$network$hidden.output.weights

results$network$output.bias.weights

#results <- batch(n.epochs, network = results$network)
