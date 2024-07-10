#' Convolve two discrete distributions
#'
#' @description
#' Using this function, two distributions, given as `ddf` objects, can be
#' convolved.
#'
#' @details
#' Under the hood, this functions uses a `C++` implementation for calculating
#' the convolution in order to get significant performance gains, compare
#' [convolve_cpp()].
#'
#' @seealso [convolve_cpp()],[conv_n()]
#'
#' @param dist1 `ddf` object, the first distribution
#' @param dist2 `ddf` object, the second distribution
#' @param desc The description for the resulting `ddf` object (optional)
#'
#' @return Convolution as a `ddf` object
#' @export
#'
#' @examples
#' # Calculate the distribution of the sum of throwing a dice twice
#' dice <- ddf((1:6))
#' conv(dice, dice, desc = "Distribution of throwing a dice twice")
conv <- function(dist1, dist2, desc = "A convolution") {
  result <- convolve_cpp(supp(dist1), probs(dist1), supp(dist2), probs(dist2))
  return(ddf(result$support, result$probabilities, desc))
}

#' Calculate n-fold convolution of a distribution with itself
#'
#' @description
#' Using this function, a distribution, given as a `ddf` object, can be
#' convolved with itself n times.
#'
#' @details
#' Under the hood, this functions uses a `C++` implementation for calculating
#' the convolution in order to get significant performance gains, compare
#' [convolve_cpp()].
#'
#' @seealso [convolve_cpp()],[conv()]
#'
#' @param dist A `ddf` distribution
#' @param n A integer specifying number of convolutions to perform
#' @param desc The description for the resulting `ddf` object (optional)
#'
#' @return
#' @export
#'
#' @examples
#' # Calculate the distribution of the sum of throwing a dice four times
#' conv_n(ddf((1:6)), 4, desc = "Distribution of throwing a dice four times")
conv_n <- function(dist, n, desc = paste(n, "-fold convolution of", desc(dist))) {
  dist_new <- dist
  for (i in 1:(n - 1)) {
    dist_new <- conv(dist, dist_new)
  }
  return(dist_new)
}
