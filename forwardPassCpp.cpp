// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;


#include <RcppCommon.h>

// Flags for C++ compiler: include Boost headers, use the C++11 standard

// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins("cpp11")]]

// Third party library includes that provide the template class of ublas
#include <boost/numeric/ublas/matrix_sparse.hpp>
#include <boost/numeric/ublas/matrix.hpp>

// Provide Forward Declarations
namespace Rcpp {

namespace traits{

// Setup non-intrusive extension via template specialization for
// 'ublas' class boost::numeric::ublas

// Support for wrap
template <typename T> SEXP wrap(const boost::numeric::ublas::vector<T> & obj);

// Support for as<T>
template <typename T> class Exporter< boost::numeric::ublas::vector<T> >;

}
}

// -------------- Stage 2: Including Rcpp.h

// ------ Place <Rcpp.h> AFTER the Forward Declaration!!!!

#include <Rcpp.h>

// ------ Place Implementations of Forward Declarations AFTER <Rcpp.h>!

// -------------- Stage 3: Implementation the Declarations

// Define template specializations for as<> and wrap
namespace Rcpp {

namespace traits{

// Defined wrap case
template <typename T> SEXP wrap(const boost::numeric::ublas::vector<T> & obj){
  const int RTYPE = Rcpp::traits::r_sexptype_traits<T>::rtype ;
  
  return Rcpp::Vector< RTYPE >(obj.begin(), obj.end());
};


// Defined as< > case
template<typename T> class Exporter< boost::numeric::ublas::vector<T> > {
  typedef typename boost::numeric::ublas::vector<T> OUT ;
  
  // Convert the type to a valid rtype. 
  const static int RTYPE = Rcpp::traits::r_sexptype_traits< T >::rtype ;
  Rcpp::Vector<RTYPE> vec;
  
public:
  Exporter(SEXP x) : vec(x) {
    if (TYPEOF(x) != RTYPE)
      throw std::invalid_argument("Wrong R type for mapped 1D array");
  }
  OUT get() {
    
    // Need to figure out a way to perhaps do a pointer pass?
    OUT x(vec.size());
    
    std::copy(vec.begin(), vec.end(), x.begin()); // have to copy data
    
    return x;
  }
};
}
}

// -------------- Stage 4: Testing

// Here we define a shortcut to the Boost ublas class to enable multiple ublas
// types via a template.
// ublas::vector<T> => ublas::vector<double>, ... , ublas::vector<int>
namespace ublas = ::boost::numeric::ublas;


// [[Rcpp::export]]
void containment_test(Rcpp::NumericVector x1) {
  
  Rcpp::Rcout << "Converting from Rcpp::NumericVector to ublas::vector<double>" << std::endl;
  
  // initialize the vector to all zero
  ublas::vector<double> x = Rcpp::as< ublas::vector<double> >(x1); 
  
  Rcpp::Rcout << "Running output test with ublas::vector<double>" << std::endl;
  
  for (unsigned i = 0; i < x.size (); ++ i)
    Rcpp::Rcout  << x(i) << std::endl;
  
  Rcpp::Rcout << "Converting from ublas::vector<double> to Rcpp::NumericVector" << std::endl;
  
  Rcpp::NumericVector test = Rcpp::wrap(x);
  
  Rcpp::Rcout << "Running output test with Rcpp::NumericVector" << std::endl;
  
  for (unsigned i = 0; i < test.size (); ++ i)
    Rcpp::Rcout  << test(i) << std::endl;
  
}




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

ublas::vector<double> callFunction1(List network, List words, Function testWordContinuity){
  ublas::vector<double> res = testWordContinuity(network, words);
  return res;
}



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

 List batchHelp(int n_epochs, List words,
               int n_words, List history, NumericVector input,
               int n_input, List network,
               List alphabet, List letters, Function batchHiddenLayerLearning,
               Function testWordContinuity, float letterNoiseParam,
               float traceParamHidden, float traceParamOutput,
               float learningRateHidden, float learningRateOutput,
               float outputBiasParamPlus, float outputBiasParamMinus,
               float hiddenBiasParamMinus, float hiddenBiasParamPlus,
               float percentActInput, float percentActOutput,
               int n_output, int n_hidden,
               NumericMatrix inputToHiddenWeights, Function noiseInletter,
               NumericVector traceHidden, NumericMatrix hiddenBiasWeights,
               NumericMatrix hiddenToOutputWeights, NumericVector traceOutput,
               NumericMatrix outputBiasWeights){

  NumericVector vect(n_epochs);
  for(int j=0; j<n_epochs; j++){
    vect[j] = j;
  }

  for(int i=0; i<(n_epochs); i++){
    NumericMatrix word = words[as<int>(RcppArmadillo::sample(vect,1,true))];

    if((i == 2) | (i % 100 == 0)){
      NumericMatrix learningCurve = history["learning.curve"];
      learningCurve(i / 100,_) = learningMeasure(network["input.hidden.weights"], n_hidden, alphabet);
      NumericMatrix biasTracker = history["bias.tracker"];
      biasTracker(i / 100,_) = network["hidden.bias.weights"];
      NumericMatrix outputBiasTracker = history["output.bias.tracker"];
      outputBiasTracker(i / 100,_) = network["output.bias.weights"];
      NumericMatrix hiddenLetterSimilarityTracking = history["hidden.letter.similarity.tracking"];
      hiddenLetterSimilarityTracking(i / 100,_) = callFunction(letters, network, batchHiddenLayerLearning)["similarity"];
      NumericVector outputMatchTracker = history["output.match.tracker"];
      outputMatchTracker[i / 100] = callFunction1(network, words, testWordContinuity);
      NumericMatrix outputTraceTracker = history["output.trace.tracker"];
      outputTraceTracker(i / 100,_) = network["trace.output"];
      NumericMatrix traceOutputTracker = history["trace.output.tracker"];
      traceOutputTracker(i/100,_) = network["trace.output"];
    }

    for(int b=0; b<((words.length())/n_input); b++){
      NumericVector input = word(_,b);
      input = noiseInLetter(input, n_input, letterNoiseParam, n_epochs);

      List results = traceUpdate(traceParamHidden, traceParamOutput,
                            learningRateHidden, learningRateOutput,
                            outputBiasParamPlus, outputBiasParamMinus,
                            hiddenBiasParamMinus, hiddenBiasParamPlus,
                            percentActInput, percentActOutput,
                            n_output, n_hidden,
                            input, inputToHiddenWeights,
                            traceHidden, hiddenBiasWeights,
                            hiddenToOutputWeights, traceOutput,
                            outputBiasWeights);


      network["input.hidden.weights"] = results["inputToHiddenWeights"];
      network["trace.hidden"] = results["traceHidden"];
      network["hidden.bias.weights"] = results["hiddenBiasWeights"];
      network["trace.output"] = results["traceOutput"];
      network["output.bias.weights"] = results["outputBiasWeights"];
      network["hidden.output.weights"] = results["hiddenToOutputWeights"];

  List history1 = List::create(Named("learning.curve") = learningCurve,
                               _["bias.tracker"] = biasTracker,
                               _["output.bias.tracker"] = outputBiasTracker,
                               _["hidden.letter.similarity.tracking"] = hiddenLetterSimilarityTracking,
                               _["output.match.tracker"] = outputMatchTracker,
                               _["output.trace.tracker"] = outputTraceTracker,
                               _["trace.output.tracker"] = traceOutputTracker);
  
  List retrn = List::create(Named("history") = history1,
                            _["network"] = network);
  return(retrn);
    }
  }
}









// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
