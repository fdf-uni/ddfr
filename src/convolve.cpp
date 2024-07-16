#include "Rcpp.h"

//' Convolve two discrete distributions
//'
//' @description
//' `convolve_cpp` returns the convolution of two discrete distributions given
//' their supports and the corresponding probabilities.
//'
//' It is not really intended for use with `ddf` objects but rather for more
//' general situations.
//' If you want to convolve two `ddf` distributions, it is recommended and
//' certainly more convenient to use [conv()] instead.
//'
//' @details
//' Some assumptions are made for the function to work properly which should be
//' followed. Most importantly, corresponding numeric vectors (e.g. `supp1` and
//' `probs1`) should have the same length.
//'
//' The function returns a list with components "support" and "probabilities"
//' which give the matching attributes of the convolution.
//'
//' @export
//' @family convolution functions
//' @param supp1,probs1 Numeric vectors of the same length.
//' @param supp2,probs2 Numeric vectors of the same length.
//' @return A list.
//' @examples
//' # Calculate the probability distribution
//' # of the sum of two six-sided dice
//' convolve_cpp((1:6), rep(1/6, 6), (1:6), rep(1/6, 6))
// [[Rcpp::export]]
Rcpp::List convolve_cpp(const Rcpp::NumericVector &supp1,
                        const Rcpp::NumericVector &probs1,
                        const Rcpp::NumericVector &supp2,
                        const Rcpp::NumericVector &probs2) {
  // Declare vector sizes
  int n1 = supp1.size(), n2 = supp2.size();
  // Declare map for convolution
  std::map<double, double> conv;

  // Looping for calculating the convolution
  for (int i = 0; i < n1; i++) {
    for (int j = 0; j < n2; j++) {
      conv[supp1[i] + supp2[j]] += probs1[i] * probs2[j];
    }
  }

  // Get result as STL vectors
  std::vector<double> supp, probs;
  supp.reserve(conv.size());
  probs.reserve(conv.size());
  for (auto const &element : conv) {
    supp.push_back(element.first);
    probs.push_back(element.second);
  }

  // Return result as R List
  return Rcpp::List::create(Rcpp::_["support"] = supp,
                            Rcpp::_["probabilities"] = probs);
}
