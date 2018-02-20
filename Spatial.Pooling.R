#install.packages('ggplot2')
library(ggplot2)

source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network.R')

n.input <- 1600
n.hidden <- 100
n.output <- 30
learning.rate.hidden <- 0.005
learning.rate.output <- 0.005
<<<<<<< HEAD
n.epochs <- 500
=======
n.epochs <- 10000
>>>>>>> origin/master
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.75 #0.75
hidden.bias.param.minus <- 2
hidden.bias.param.plus <- 0.0005
output.bias.param.minus <- 0 #0
output.bias.param.plus <- 0 #0
sparseness.percent <- 0.75  # 1-sparseness.percent is % nodes active
num.inputs.generated <- 50
integration.parameter <- 1 #0 is totally segregated, 1 is totally integrated
percent.act.input <- 0.05
percent.act.output <- 0.1
n.words <- length(words)
input.gen.parameter <- 0 # if 1: temporal pattern of input for one system, random pattern for other system. (one system predicts next input) 
                         # if 0: Next inputs are predicted by combination of both systems' previous inputs - one system alone cannot predict next inputs
                         # if 0.5: inputs for each system consistently co-occur

## RUN ##

results <- batch(n.epochs) #run training batches

visualize.hidden.layer.learning(results$history)
display.learning.curves(results) 
display.output.bias.tracker(results)
visualize.letter.activations(results$network, q)
visualize.output.act.match()
temp.layer.activations.many <- temp.layer.many.activations(network, words)
output.trace.tracker.results <- results$history$trace.output.tracker

network <- results$network

plot(x=seq(from = 1, to = 100, by = 1), y=output.trace.tracker.results[,9], type = "b")
