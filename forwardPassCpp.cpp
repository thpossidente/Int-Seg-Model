// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
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

List forwardPass(int n_output, float percentActInput,
                 float percentActOutput, int n_hidden,
                 NumericVector input, NumericMatrix inputToHiddenWeights,
                 NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights,
                 NumericMatrix outputBiasWeights){
  
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

List test(int n_output, float percentActInput, 
          float percentActOutput, int n_hidden, 
          NumericVector input, NumericMatrix inputToHiddenWeights, 
          NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights, 
          NumericMatrix outputBiasWeights){
  
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
  // NumericVector hidden = forwardPassResults[0];
  // NumericVector output = forwardPassResults[1];
  // 
  // for(int x=0; x<(n_hidden); x++){
  //   if(hidden[x] == 1){
  //     hiddenBiasWeights(x,0) = hiddenBiasWeights(x,0) - hiddenBiasParamMinus;
  //   }
  //   if(hidden[x] == 0){
  //     hiddenBiasWeights(x,0) = hiddenBiasWeights(x,0) + hiddenBiasParamPlus;
  //   }
  //   if(hiddenBiasWeights(x,0) < 0){
  //     hiddenBiasWeights(x,0) = 0;
  //   }
  // }
  
  // for(int i=0; i<(n_hidden); i++){
  //   traceHidden[i] = (1 - traceParamHidden) * traceHidden[i] + traceParamHidden * hidden[i];
  //   inputToHiddenWeights(_,i) = inputToHiddenWeights(_,i) + learningRateHidden * traceHidden[i] * (input - inputToHiddenWeights(_,i));
  // }
  // 
  // for(int b=0; b<(n_output); b++){
  //   if(output[b] == 1){
  //     outputBiasWeights(b,0) = outputBiasWeights(b,0) - outputBiasParamMinus;
  //   }
  //   if(output[b] == 0){
  //     outputBiasWeights(b,0) = outputBiasWeights(b,0) + outputBiasParamPlus;
  //   }
  //   if(outputBiasWeights(b,0) < 0){
  //     outputBiasWeights(b,0) = 0;
  //   }
  // }
  // 
  // for(int h=0; h<(n_output); h++){
  //   traceOutput[h] = (1 - traceParamOutput) * traceOutput[h] + traceParamOutput * output[h];
  //   hiddenToOutputWeights(_, h) = hiddenToOutputWeights(_,h) + learningRateOutput * traceOutput[h] *(hidden - hiddenToOutputWeights(_,h));
  // }
  // 
  // List lst = List::create(hidden, output, hiddenBiasWeights);
  // return(lst);
// }





// [[Rcpp::export]]

List traceUpdate(float traceParamHidden, float traceParamOutput,
                 float learningRateHidden, float learningRateOutput,
                 float outputBiasParamPlus, float outputBiasParamMinus,
                 float hiddenBiasParamMinus, float hiddenBiasParamPlus,
                 float percentActInput, float percentActOutput,
                 int n_output, int n_hidden,
                 NumericVector input, NumericMatrix inputToHiddenWeights,
                 NumericVector traceHidden, NumericMatrix hiddenBiasWeights,
                 NumericMatrix hiddenToOutputWeights, NumericVector traceOutput,
                 NumericMatrix outputBiasWeights){
  
  List forwardPassResults = forwardPass(n_output, percentActInput, 
                                        percentActOutput, n_hidden, 
                                        input, inputToHiddenWeights, 
                                        hiddenBiasWeights, hiddenToOutputWeights, 
                                        outputBiasWeights);
  
  NumericVector hidden = forwardPassResults["hidden"];
  NumericVector output = forwardPassResults["output"];
  

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
  
    
  for(int i=0; i<(n_hidden); i++){
    traceHidden[i] = (1 - traceParamHidden) * traceHidden[i] + traceParamHidden * hidden[i];
    inputToHiddenWeights(_,i) = inputToHiddenWeights(_,i) + learningRateHidden * traceHidden[i] * (input - inputToHiddenWeights(_,i));
  }
  
  for(int b=0; b<(n_output); b++){
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
  
  for(int h=0; h<(n_output); h++){
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

// [[Rcpp::export]]

// List batchHelp(int n_epochs, List words, 
//                int n_words, List history, NumericVector input, 
//                int n_input, List network, 
//                List alphabet, int n_hidden,
//                List letters, Function batchHiddenLayerLearning,
//                Function testWordContinuity, float letterNoiseParam){
// 
//   NumericVector vect(n_epochs);
//   for(int j=0; j<n_epochs; j++){
//     vect[j] = j;
//   }
// 
//   for(int i=0; i<(n_epochs); i++){
//     NumericMatrix word = words[RcppArmadillo::sample(vect,1,true)];
// 
//     if(i == 2 | i % 100 == 0){
//       history["learning.curve"][i / 100,] = learningMeasure(network["input.hidden.weights"], n_hidden, alphabet);
//       history["bias.tracker"][i / 100,] = NumericVector network["hidden.bias.weights"];
//       history["output.bias.tracker"][i / 100,] = NumericVector network["output.bias.weights"];
//       history["hidden.letter.similarity.tracking"][i / 100, ] = callFunction(letters, network, batchHiddenLayerLearning)["similarity"]
//       history["output.match.tracker"][i / 100] = callFunction2(network, words, testWordContinuity);
//       history["output.trace.tracker"][i / 100, ] = network["trace.output"];
//       history["trace.output.tracker"][i/100,] = network["trace.output"];
//     }
//     
//     for(int b=0; b<(length(words)/n_input); b++){
//       NumericVector input = word(_,b);
//       input = noiseInLetter(input, n_input, letterNoiseParam, n_epochs);
//       results = traceUpdate()
//     }
//   }
// }




// [[Rcpp::export]]


NumericVector noiseInLetter(NumericVector input, int n_input, float letterNoiseParam, int n_epochs){
  
  NumericVector vect(n_epochs);
  
  for(int j=0; j<n_epochs; j++){
    vect[j] = j;
  }
  
  for(int i=0; i<(0.1*n_input); i++){
    input[RcppArmadillo::sample(vect,1,true)];
  }
  return(input);
}



// [[Rcpp::export]]

NumericVector learningMeasure(NumericMatrix inputHiddenWeights, int n_hidden, List alphabet){
  
  NumericVector allLettersCompared(26);
  NumericVector bestFit(n_hidden);
                           
  for(int i=0; i<n_hidden; i++){
    for(int h=0; h<26; h++){
      allLettersCompared[h] = sum(abs(inputHiddenWeights(_,i) - alphabet[h]));
    }
    bestFit[i] = min(allLettersCompared);
  }
  return(bestFit);
}


// [[Rcpp::export]]

DataFrame callFunction(List letters, List network, Function batchHiddenLayerLearning) {
  DataFrame res = batchHiddenLayerLearning(letters, network);
  return res;
}

// [[Rcpp::export]]

SEXP callFunction1(List network, List words, Function testWordContinuity){
  SEXP res = testWordContinuity(network, words);
  return res;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
