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


<<<<<<< HEAD
=======
NumericVector test(int n_hidden, NumericMatrix hiddenBiasWeights, NumericVector input, NumericMatrix inputToHiddenWeights, float percentActInput){
 
  NumericVector hidden(n_hidden);
  for(int i=0; i<(n_hidden); i++){
    hidden[i] += sum(na_omit(input * inputToHiddenWeights(_,i) + hiddenBiasWeights(i,0)));
  }
  
  int largest;
  int number = percentActInput * n_hidden;
  for(int c=0; c<(number); c++){
    largest = which_max(hidden);
    hidden[largest] = -1;
  }
  
  for(int x=0; x<(n_hidden); x++){
    if(hidden[x] == -1){
      hidden[x] = 1;
    } else{
      hidden[x] = 0;
    }
  }
  

  return hidden;
}
>>>>>>> origin/master

// [[Rcpp::export]]

List forwardPass(int n_output, float percentActInput, float percentActOutput, int n_hidden, NumericVector input, NumericMatrix inputToHiddenWeights, NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights, NumericMatrix outputBiasWeights){
<<<<<<< HEAD
  
=======
>>>>>>> origin/master
  
  NumericVector hidden(n_hidden);
  for(int i=0; i<(n_hidden); i++){
    hidden[i] += sum(na_omit(input * inputToHiddenWeights(_,i) + hiddenBiasWeights(i,0)));
  }
  
  int largest;
  int number = ceil(percentActInput * n_hidden);
  for(int c=0; c<(number); c++){
    largest = which_max(hidden);
    hidden[largest] = -1;
  }
  
  
  for(int x=0; x<(n_hidden); x++){
    if(hidden[x] == -1){
      hidden[x] = 1;
    } else{
      hidden[x] = 0;
    }
  }
<<<<<<< HEAD
  
=======

>>>>>>> origin/master
  NumericVector output(n_output);
  for(int i=0; i<(n_output); i++){
    output[i] += sum(na_omit(hidden * hiddenToOutputWeights(_,i) + outputBiasWeights(i,0)));
  }
  
  int largest1;
  int number1 = ceil(percentActOutput * n_output);
  for(int k=0; k<(number1); k++){
    largest1 = which_max(output);
    output[largest1] = -1;
  }
  
  for(int d=0; d<(n_output); d++){
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

NumericVector test(int n_hidden, NumericMatrix hiddenBiasWeights, NumericVector input, NumericMatrix inputToHiddenWeights, int n_output, float percentActInput, float percentActOutput, NumericMatrix hiddenToOutputWeights, NumericMatrix outputBiasWeights, float hiddenBiasParamMinus, float hiddenBiasParamPlus){
  
  List forwardPassResults = forwardPass(n_output, percentActInput, percentActOutput, n_hidden, input, inputToHiddenWeights, hiddenBiasWeights, hiddenToOutputWeights, outputBiasWeights);
  NumericVector hidden = forwardPassResults[0];
  NumericVector output = forwardPassResults[1];
  
  for(int x=0; x<(n_hidden); x++){
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
  
  
  List lst = List::create(hidden, output, hiddenBiasWeights);
  return(lst);
}





// [[Rcpp::export]]

List traceUpdate(float traceParamHidden, float traceParamOutput, float learningRateHidden, float learningRateOutput, float outputBiasParamPlus, float outputBiasParamMinus, float hiddenBiasParamMinus, float hiddenBiasParamPlus, float percentActInput, float percentActOutput, int n_output, int n_hidden, NumericVector input, NumericMatrix inputToHiddenWeights, NumericVector traceHidden, NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights, NumericVector traceOutput, NumericMatrix outputBiasWeights){
  
  List forwardPassResults = forwardPass(n_output, percentActInput, percentActOutput, n_hidden, input, inputToHiddenWeights, hiddenBiasWeights, hiddenToOutputWeights, outputBiasWeights);
  
  NumericVector hidden = forwardPassResults[0];
  NumericVector output = forwardPassResults[1];
  

  for(int x=0; x<(n_hidden-1); x++){
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
  
    
  for(int i=0; i<(n_hidden-1); i++){
    traceHidden[i] = (1 - traceParamHidden) * traceHidden[i] + traceParamHidden * hidden[i];
    inputToHiddenWeights(_,i) = inputToHiddenWeights(_,i) + learningRateHidden * traceHidden[i] * (input - inputToHiddenWeights(_,i));
  }
  
  for(int b=0; b<(n_output-1); b++){
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
  
  for(int h=0; h<(n_output-1); h++){
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