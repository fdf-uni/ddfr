#include "Rcpp.h"

// Calculate the convolution of two discrete random variables given
// the supports and corresponding probabilities as R numeric vectors
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
  int conv_size = conv.size();
  std::vector<double> supp, probs;
  supp.reserve(conv_size);
  probs.reserve(conv_size);
  for (auto const &element : conv) {
    supp.push_back(element.first);
    probs.push_back(element.second);
  }

  // Return result as R List
  return Rcpp::List::create(Rcpp::_["support"] = supp,
                            Rcpp::_["probabilities"] = probs);
}
