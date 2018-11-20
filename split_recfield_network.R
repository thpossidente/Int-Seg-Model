#install.packages('ggplot2')
#install.packages('ggplot')
#install.packages('png')
#install.packages('abind')
#install.packages('dplyr')

library('ggplot2')


source('Load Letters.R')
source('Visualize Output.R')
source('multi-layer-network-split.R')
Rcpp::sourceCpp('forwardPassCpp.cpp')


split_network <- function(inputs, im_size, field_size){
  n.input <- im_size^2
  n.hidden <- n.input/2
  n.output <-  ???
  learning.rate.hidden.max = 0.5
  learning.rate.hidden.min = 0.0001
  learning.rate.hidden <- 0.00001
  learning.rate.output <- 0.0001
  learning.rate.output.max <- 0.5 # 0.009
  learning.rate.output.min <- 0.00001
  restarts <- 5 # 5
  n.epochs <- 500
  hidden.bias.param.minus <- 0.05 # 0.05
  hidden.bias.param.plus <- 0.0005 # 0.0005
  output.bias.param.minus <- 0 #0.5
  output.bias.param.plus <- 0 #0.0005
  sparseness.percent <- 0.75  # sparseness.percent is % nodes inactive #0.75
  percent.act.input <- 0.05 # 0.05
  percent.act.output <- 0.03333 # .10
  
  
  pre.input.hidden.weights <- matrix(runif(n.input*n.hidden, min=0, max=0.5), nrow=n.input, ncol=n.hidden)   # Random normalization
  pre.hidden.output.weights <- matrix(runif(n.hidden*n.output, min=0, max=0.5), nrow=n.hidden, ncol=n.output)
}


