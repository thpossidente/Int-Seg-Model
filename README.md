# Int-Seg-Model - Senior Thesis Thomas Possidente, Vassar College '19, Advisor: Joshua de Leeuw

This project is a work in progress and most code is not yet fully commented. 

The goal of this project is to develop an unsupervised artificial neural network (ANN) architecture that learns regularities in image data by optimizing mutual information via receptive field size. 

Thus far:
* A preliminary unsupervised ANN is functional and can learn spatial regularities in letters.
** Load Letters.R converts PNGs of letters of a specific font into matrices of pixel values. 
** multi-layer-network-split.R contains the parts of the ANN that were not offloaded into Rcpp for efficiency
** forwardPassCpp.cpp contains the parts of the ANN that were offloaded into Rcpp for efficiency
** Visualize Output.R contains functions that help describe, visualize, and diagnose learning in the ANN
** Split_Model.R defines parameters and runs the ANN. It also calls some of the functions from Visualize Output.R to evaluate learning

* A preliminary calculator of mutual information (MI) in matrices of pixel values is functional.
** Input Generator.R includes functions that create matrices of pixel values that have high MI within a specified receptive field size. Number of matrices, size of matrix, size of receptive field, and added noise can all be varied.
** MI_PixelMeasure.cpp includes an Rcpp function that calculates the average MI between any 2 pixels in the matrix set, based on a specified receptive field size (window), and stride. 
