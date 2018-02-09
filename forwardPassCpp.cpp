#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]


List forwardPass(int n_output, int percentActInput, int percentActOutput, int n_hidden, NumericVector input, NumericMatrix inputToHiddenWeights, NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights, NumericMatrix outputBiasWeights){
  
  
  
  NumericVector hidden(n_hidden);
  for(int i=0; i<n_hidden; i++){
    hidden[i] += hiddenBiasWeights(i,0);
    for(int j=0; j<input.size(); j++){
      if(inputToHiddenWeights(j,i) != R_NaN) {
        hidden[i] += input[j] * inputToHiddenWeights(j,i);
      }
    }
  }
  
  int largest;
  int number = ceil(percentActInput * n_hidden);
  for(int c=0; c<number; c++){
    largest = which_max(hidden);
    hidden[largest] = -1;
  }
  
  
  for(int x=0; x<n_hidden; x++){
    if(hidden[x] == -1){
      hidden[x] = 1;
    } else{
      hidden[x] = 0;
    }
  }

  NumericVector output;
  for(int z=0; z<n_output; z++){
    output[z] += outputBiasWeights(z,0);
    for(int h=0; h<n_hidden; h++){
      if(hiddenToOutputWeights(h,z) != R_NaN) {
        output[h] += hidden[h] * hiddenToOutputWeights(h,z);
      }
    }
  }
  
  int largest1;
  int number1 = ceil(percentActOutput * n_output);
  for(int k=0; k<number1; k++){
    largest1 = which_max(output);
    output[largest1] = -1;
  }
  
  for(int d=0; d<n_output; d++){
    if(output[d] == -1){
      output[d] = 1;
    } else{
      output[d] = 0;
    }
  }
  
  List retrn = List::create(Named("hidden") = hidden,_["output"] = output); 
  return(retrn);
}

// [[Rcpp::export]]

List traceUpdate(int traceParamHidden, int traceParamOutput, int learningRateHidden, int learningRateOutput, int outputBiasParamPlus, int outputBiasParamMinus, int hiddenBiasParamMinus, int hiddenBiasParamPlus, int percentActInput, int percentActOutput, int n_output, int n_hidden, NumericVector input, NumericMatrix inputToHiddenWeights, NumericVector traceHidden, NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights, NumericVector traceOutput, NumericMatrix outputBiasWeights){
  
  List forwardPassResults = forwardPass(n_output, percentActInput, percentActOutput, n_hidden, input, inputToHiddenWeights, hiddenBiasWeights, hiddenToOutputWeights, outputBiasWeights);
  
  NumericVector hidden = forwardPassResults[0];
  NumericVector output = forwardPassResults[1];
  

  for(int x=0; x<n_hidden; x++){
    if(hidden[x] == 1){
      hiddenBiasWeights(x,0) = hiddenBiasWeights(x,0) - hiddenBiasParamMinus;
    }
    if(hidden[x] == 0){
      hiddenBiasWeights(x,0) = hiddenBiasWeights(x,0) + hiddenBiasParamPlus;
    }
    if(hiddenBiasWeights(x,0) < 0){
      hiddenBiasWeights(x,0) = 0;
    }
  }
  
    
  for(int i=0; i<n_hidden; i++){
    traceHidden[i] = (1 - traceParamHidden) * traceHidden[i] + traceParamHidden * hidden[i];
    inputToHiddenWeights(_,i) = inputToHiddenWeights(_,i) + learningRateHidden * traceHidden[i] * (input - inputToHiddenWeights(_,i));
  }
  
  for(int b=0; b<n_output; b++){
    if(output[b] == 1){
      outputBiasWeights(b,0) = outputBiasWeights(b,0) - outputBiasParamMinus;
    }
    if(output[b] == 0){
      outputBiasWeights(b,0) = outputBiasWeights(b,0) + outputBiasParamPlus;
    }
    if(outputBiasWeights(b,0) < 0){
      outputBiasWeights(b,0) = 0;
    }
  }
  
  for(int h=0; h<n_output; h++){
    traceOutput[h] = (1 - traceParamOutput) * traceOutput[h] + traceParamOutput * output[h];
    hiddenToOutputWeights(_, h) = hiddenToOutputWeights(_,h) + learningRateOutput * traceOutput[h] *(hidden - hiddenToOutputWeights(_,h));
  }
  
  List retrn = List::create(Named("traceHidden") = traceHidden,
                            _["hidden"] = hidden,
                            _["inputToHiddenWeights"] = inputToHiddenWeights,
                            _["hiddenBiasWeights"] = hiddenBiasWeights,
                            _["traceOutput"] = traceOutput,
                            _["output"] = output,
                            _["hiddenToOutputWeights"] = hiddenToOutputWeights,
                            _["outputBiasWeights"] = outputBiasWeights);
    return(retrn);
}

//List batch(int n_epochs, ??){
  
//  Environment env = Environment::global_env();
  
//  NumericMatrix pre_input_hidden_weights()
//  NumericMatrix pre_hidden_output_weights = 
//}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/