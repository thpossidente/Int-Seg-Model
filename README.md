# Investigation of Optimal Receptive Field Size for Maximizing Mutual Information and Increasing Learning Efficiency

## Senior Cognitive Science Thesis - Thomas Possidente, Vassar College '19
## First Reader: Joshua de Leeuw
## Second Reader: Ken Livingston


This repo contains all the code and datasets for the "Investigation of Optimal Receptive Field Size for Maximizing Mutual Information and Increasing Learning Efficiency". 

Abstract:

## Contents of Repo 

* *Input Generation.R* 
  * An R script that contains functions to create toy image datasets of 1s and 0s that have high mutual information at a specified spatial resolution or "receptive field". Noise level, image (matrix) size, receptive field size, and number of matrices can be specified. Also contains a function to make image datasets of randomly placed 1s and 0s.
  
* *MI_PixelMeasure.cpp* 
  * An Rcpp file that contains a function to measure the mutual information in an image dataset for a specified receptive field size. 
  
* *MI.Calculation.Script.R* 
  * An R script containing a simple function to run multiple trials of mutual information measurements (MI_PixelMeasure)
  
* *Supervised_ANNs.ipynb* 
  * A python Jupyter notebook that contains code for building and testing supervised Convolutional Neural Networks on toy image datasets created by "Input Generation.R"
  
* *Unsupervised_ANNs,ipynb
  * An unfinished python Jupyter notebook that begins to implement the unsupervised version of the analyses run using "Supervised_ANNs.ipynb" using custom layers, loss, and optimizers in Keras.
  
* *Results*
  * This folder contains two excel files that show the results of Experiment 1 ("Testing MI Calculator") and Experiment 2 ("Supervised Network Outcomes")
  
* *Toy_Image_Datasets*
  * Folder containing 2 folders - one containing the datasets used in Experiment 1, the other containing the datasets used in Experiment 2
 
* *Binary_Datasets_Testing* 
  * This folder contains 3 image datasets:
    * CalTech 101 Silhouettes Data Set (called "Binary_shapes in this folder) - https://people.cs.umass.edu/~marlin/data.shtml
    * The Omniglot Dataset - https://github.com/brendenlake/omniglot
    * MNIST - http://yann.lecun.com/exdb/mnist/index.html
  * This folder also contains 2 R scripts, one for unpacking the Binary_shapes dataset and one for unpacking the MNIST dataset.
  * Lastly this folder contains "ANNs Binary Shapes Analysis.ipynb" which begins to test the binary_shapes dataset on supervised CNNs

* *Original Unsupervised Alphabet ANN* 
  * Contains a prelimary unsupervised ANN that was used to develop methods for implementing Hebbian updating and winner-take-all activation to learn letters of the alphabet of a specific font. 
  * R and Rcpp
