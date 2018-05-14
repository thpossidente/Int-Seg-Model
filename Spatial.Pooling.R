#install.packages('ggplot2')
library(ggplot2)

source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network.R')


n.input <- 1600
n.hidden <- 250 
n.output <- 10  #Must be multiple of 10 due to activation percentage calculation
learning.rate.hidden <- 0.005
learning.rate.output <- 0.009 # 0.009
n.epochs <- 10000
trace.param.hidden <- 1 # value of 1 indicates pure hebbian learning. Closer to zero, more of 'history' of node activation is taken into account
trace.param.output <- 0.8 #0.8
hidden.bias.param.minus <- 1
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
input.gen.parameter <- 0 # if 1: temporal pattern of input for one system, random pattern for other system. (one system predicts next input) 
                         # if 0: Next inputs are predicted by combination of both systems' previous inputs - one system alone cannot predict next inputs
                         # if 0.5: inputs for each system consistently co-occur

## RUN ##

results <- batch(n.epochs) #run training batches

visualize.hidden.layer.learning(results$history)
visualize.output.act.match()
plot(x=seq(from = 1, to = n.epochs/100, by = 1), y=results$history$output.act.unique.tracker, type='b', ylim=c(0,1))
test.word.continuity1(results$network, words)
plot(x=seq(from=100, to=10000, by=100), y=results$history$mutual.info.tracker, type = 'b', ylim=c(0,3.2), xlab = "Epochs", ylab = "Mutual Information")
plot(x=seq(from=100, to=10000, by=100), y=results$history$output.bias.tracker[,6], type='b', ylim=c(0,0.02))


#display.learning.curves(results) 
#visualize.letter.activations(results$network, j)
#temp.layer.activations.many <- temp.layer.many.activations(network, words)
#output.trace.tracker.results <- results$history$output.trace.tracker
# 
# 
# plot(x=seq(from = 1, to = 100, by = 1), y=output.trace.tracker.results[,30], type = "b")


