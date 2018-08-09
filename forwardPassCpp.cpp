// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

// // [[Rcpp::export]]
// NumericVector coordinatesWeights(NumericMatrix coordinates, List network, int n_hidden, int n_output, float dropout){
//   
//   NumericVector weights(n_hidden*n_output*dropout);
//   for(int n=0; n<(n_hidden*n_output*dropout);n++){
//     weights[n] = network
//   }
// }


// [[Rcpp::export]]
NumericVector noiseInLetter(NumericVector input, int n_input, float letterNoiseParam, int n_epochs){
  
  NumericVector vect(n_epochs);
  
  for(int j=0; j<n_epochs; j++){
    vect[j] = j;
  }
  
  for(int i=0; i<(0.1*n_input); i++){
    input[as<int>(RcppArmadillo::sample(vect,1,true))];
  }
  return(input);
}



// [[Rcpp::export]]

NumericVector learningMeasure(NumericMatrix inputToHiddenWeights, int n_hidden, List alphabet){
  
  NumericVector allLettersCompared(alphabet.size());
  NumericVector bestFit(n_hidden);
  
  for(int i=0; i<n_hidden; i++){
    for(int h=0; h<alphabet.size(); h++){
      allLettersCompared[h] = sum(abs(inputToHiddenWeights(_,i) - as<NumericVector>(alphabet[h])));
    }
    bestFit[i] = min(allLettersCompared);
  }
  return(bestFit);
}



// // [[Rcpp::export]]
// 
// DataFrame bathHiddenLayerLearning(List letters, List network) {
//   StringVector names = StringVector::create("A", "B", "C", "D", "E", "F", "G",
//                                                   "H", "I", "J", "K", "L", "M", "N",
//                                                   "O", "P", "Q", "R", "S", "T", "U",
//                                                   "V", "W", "X", "Y", "Z");
//   NumericVector similarity(names.size());
// 
//   DataFrame res = DataFrame::create(Named("input") = names, _["similarity"] = similarity);
//   for(int i=0; i<res.nrow(); i++){
//     res(i, 2) = hiddenLayerSimilarity(letters[i], network);
//   }
//   return res;
// }
// 
// 
// // [[Rcpp::export]]
// float hiddenLayerSimilarity(NumericMatrix letter, List network, int n_output,
//                             float percentActInput, float percentActOutput, int n_hidden){
//   List results = forwardPass(n_output, percentActInput, percentActOutput,
//                         n_hidden, letter, network["inputToHiddenWeights"],
//                         network["hiddenBiasWeights"], network["hiddenToOutputWeights"],
//                         network["outputBiasWeights"]);
//   NumericVector active_nodes = which_max()
// }


// [[Rcpp::export]]

double callFunction1(List network, List words, Function testWordContinuity){
  double res = as<double>(testWordContinuity(network, words));
  return res;
}




// [[Rcpp::export]]

List forwardPass(int n_output, float percentActInput,
                 float percentActOutput, int n_hidden,
                 NumericVector input, NumericMatrix inputToHiddenWeights,
                 NumericMatrix hiddenBiasWeights, NumericMatrix hiddenToOutputWeights,
                 NumericMatrix outputBiasWeights, NumericMatrix hiddenActivationDelay,
                 int delayParam){
  
  NumericVector hidden(n_hidden);
  for(int i=0; i<(n_hidden); i++){
    //float preHidden = sum(na_omit(input * inputToHiddenWeights(_,i) + hiddenBiasWeights(i,0)));
    //NumericVector preHidden1(0, preHidden);
    //hidden[i] += max(preHidden1);
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
    int t = 6;
    output[i] += sum(na_omit(hidden * hiddenToOutputWeights(_,i) + outputBiasWeights(i,0)));
    //for(int h=0; h<(delayParam); h++){
    //  output[i] += (sum(na_omit(hiddenActivationDelay(h,_) * hiddenToOutputWeights(_,i)))) / t;
    //  t += 2;
    //}
  }
  
  float max_output = max(output);  // normalizing outputs from 0-1
  for(int x=0; x<(n_output); x++){
    output[x] = output[x] / max_output;
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

List traceUpdate(float traceParamHidden, float traceParamOutput,
                 float learningRateHidden, float learningRateOutput,
                 float outputBiasParamPlus, float outputBiasParamMinus,
                 float hiddenBiasParamMinus, float hiddenBiasParamPlus,
                 float percentActInput, float percentActOutput,
                 int n_output, int n_hidden,
                 NumericVector input, NumericMatrix inputToHiddenWeights,
                 NumericVector traceHidden, NumericMatrix hiddenBiasWeights,
                 NumericMatrix hiddenToOutputWeights, NumericVector traceOutput,
                 NumericMatrix outputBiasWeights, int counter, int counterBias, 
                 NumericMatrix hiddenActivationDelay, int delayParam){

  List forwardPassResults = forwardPass(n_output, percentActInput,
                                        percentActOutput, n_hidden,
                                        input, inputToHiddenWeights,
                                        hiddenBiasWeights, hiddenToOutputWeights,
                                        outputBiasWeights, hiddenActivationDelay,
                                        delayParam);

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

  if(counterBias > 10000){
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
  }
  
  if(counter > 5000){
    for(int h=0; h<(n_output); h++){
      //float learningRateOutputMod = ((outputBiasWeights(h,0)*(outputBiasWeights(h,0)))*10000)+learningRateOutput;
      //traceOutput[h] = (1 - traceParamOutput) * traceOutput[h] + traceParamOutput * output[h];  
      hiddenToOutputWeights(_, h) = hiddenToOutputWeights(_,h) + learningRateOutput * traceOutput[h] * (hidden - hiddenToOutputWeights(_,h));
      traceOutput[h] = (1 - traceParamOutput) * traceOutput[h] + traceParamOutput * output[h];
    }
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


//// [[Rcpp::export]]

// List batchHelp(int n_epochs, List words,
//                int n_words, List history, NumericVector input,
//                int n_input, List network,
//                List alphabet, List letters, Function batchHiddenLayerLearning,
//                Function testWordContinuity, float letterNoiseParam,
//                float traceParamHidden, float traceParamOutput,
//                float learningRateHidden, float learningRateOutput,
//                float outputBiasParamPlus, float outputBiasParamMinus,
//                float hiddenBiasParamMinus, float hiddenBiasParamPlus,
//                float percentActInput, float percentActOutput,
//                int n_output, int n_hidden,
//                NumericMatrix inputToHiddenWeights, Function noiseInletter,
//                NumericVector traceHidden, NumericMatrix hiddenBiasWeights,
//                NumericMatrix hiddenToOutputWeights, NumericVector traceOutput,
//                NumericMatrix outputBiasWeights){
// 
//   NumericVector vect(n_epochs);
//   for(int j=0; j<n_epochs; j++){
//     vect[j] = j;
//   }
//   
//   
//   NumericMatrix learningCurve = history["learning.curve"];
//   NumericMatrix biasTracker = history["bias.tracker"];
//   NumericMatrix outputBiasTracker = history["output.bias.tracker"];
//   NumericMatrix hiddenLetterSimilarityTracking = history["hidden.letter.similarity.tracking"];
//   NumericVector outputMatchTracker = history["output.match.tracker"];
//   NumericMatrix outputTraceTracker = history["output.trace.tracker"];
//   NumericMatrix traceOutputTracker = history["trace.output.tracker"];
//   
//   
//   for(int i=0; i<(n_epochs); i++){
//     NumericMatrix word = words[as<int>(RcppArmadillo::sample(vect,1,true))];
// 
//     if((i == 2) | (i % 100 == 0)){
//       
//       learningCurve(i / 100,_) = as<NumericVector>(learningMeasure(network["input.hidden.weights"], n_hidden, alphabet));
//       biasTracker(i / 100,_) = as<NumericVector>(network["hidden.bias.weights"]);
//       outputBiasTracker(i / 100,_) = as<NumericVector>(network["output.bias.weights"]);
//       hiddenLetterSimilarityTracking(i / 100,_) = as<NumericVector>(callFunction(letters, network, batchHiddenLayerLearning)["similarity"]);
//       outputMatchTracker[i / 100] = callFunction1(network, words, testWordContinuity);
//       outputTraceTracker(i / 100,_) = as<NumericVector>(network["trace.output"]);
//       traceOutputTracker(i/100,_) = as<NumericVector>(network["trace.output"]);
//     }
// 
//     for(int b=0; b<((words.length())/n_input); b++){
//       NumericVector input = word(_,b);
//       input = noiseInLetter(input, n_input, letterNoiseParam, n_epochs);
// 
//       List results = traceUpdate(traceParamHidden, traceParamOutput,
//                             learningRateHidden, learningRateOutput,
//                             outputBiasParamPlus, outputBiasParamMinus,
//                             hiddenBiasParamMinus, hiddenBiasParamPlus,
//                             percentActInput, percentActOutput,
//                             n_output, n_hidden,
//                             input, inputToHiddenWeights,
//                             traceHidden, hiddenBiasWeights,
//                             hiddenToOutputWeights, traceOutput,
//                             outputBiasWeights);
// 
// 
//       network["input.hidden.weights"] = results["inputToHiddenWeights"];
//       network["trace.hidden"] = results["traceHidden"];
//       network["hidden.bias.weights"] = results["hiddenBiasWeights"];
//       network["trace.output"] = results["traceOutput"];
//       network["output.bias.weights"] = results["outputBiasWeights"];
//       network["hidden.output.weights"] = results["hiddenToOutputWeights"];
//       
//     }
//     
//   }
// 
//   List history1 = List::create(Named("learning.curve") = learningCurve,
//                                _["bias.tracker"] = biasTracker,
//                                _["output.bias.tracker"] = outputBiasTracker,
//                                _["hidden.letter.similarity.tracking"] = hiddenLetterSimilarityTracking,
//                                _["output.match.tracker"] = outputMatchTracker,
//                                _["output.trace.tracker"] = outputTraceTracker,
//                                _["trace.output.tracker"] = traceOutputTracker);
//   
//   List retrn = List::create(Named("history") = history1,
//                             _["network"] = network);
//   return(retrn);
//     
//   
// }


// //[[Rcpp::export]]
// NumericVector test(int n_epochs, List history, List words, List network, int n_hidden, List alphabet, List letters, Function batchHiddenLayerLearning, Function testWordContinuity){
//   
//   NumericVector vect(n_epochs);
//   for(int j=0; j<n_epochs; j++){
//     vect[j] = j;
//   }
//   
//   
//   NumericMatrix learningCurve = history["learning.curve"];
//   NumericMatrix biasTracker = history["bias.tracker"];
//   NumericMatrix outputBiasTracker = history["output.bias.tracker"];
//   NumericMatrix hiddenLetterSimilarityTracking = history["hidden.letter.similarity.tracking"];
//   NumericVector outputMatchTracker = history["output.match.tracker"];
//   NumericMatrix outputTraceTracker = history["output.trace.tracker"];
//   NumericMatrix traceOutputTracker = history["trace.output.tracker"];
//   
//   for(int i=0; i<(n_epochs); i++){
//     NumericMatrix word = words[as<int>(RcppArmadillo::sample(vect,1,true))];
//     
//     if((i == 2) | (i % 100 == 0)){
//       
//       learningCurve(i / 100,_) = as<NumericVector>(learningMeasure(network["input.hidden.weights"], n_hidden, alphabet));
//       biasTracker(i / 100,_) = as<NumericVector>(network["hidden.bias.weights"]);
//       outputBiasTracker(i / 100,_) = as<NumericVector>(network["output.bias.weights"]);
//       hiddenLetterSimilarityTracking(i / 100,_) = as<NumericVector>(callFunction(letters, network, batchHiddenLayerLearning)["similarity"]);
//       outputMatchTracker[i / 100] = callFunction1(network, words, testWordContinuity);
//       outputTraceTracker(i / 100,_) = as<NumericVector>(network["trace.output"]);
//       traceOutputTracker(i/100,_) = as<NumericVector>(network["trace.output"]);
//     }
//   }
//   return(outputMatchTracker);
// }





// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
