#' Convolve two discrete distributions
#'
#' @description
#' Using this function, two distributions, given as `ddf` objects, can be
#' convolved.
#'
#' @details
#' In general, given two functions \eqn{f} and \eqn{g} defined on the set
#' \eqn{\mathbb{Z}} of integers, the **discrete convolution** of \eqn{f} and
#' \eqn{g} is given by
#' \deqn{(f \ast g)(n) = \sum_{m=-\infty}^{\infty} f(n-m) g(m).}
#' Note how the sum on the right hand side ranges over all products where the
#' arguments of \eqn{f} and \eqn{g} are integers \eqn{k,l\in\mathbb{Z}}
#' such that \eqn{k+l=n}.
#'
#' This implementation of the convolution uses this last observation as its
#' basis which makes it possible to also calculate the convolution if the
#' domains are not (subsets of) \eqn{\mathbb{Z}} (but of course still discrete).
#'
#' For a interpretation of the convolution, recall that given two independent
#' random variables \eqn{X} and \eqn{Y} with probability mass functions \eqn{p_X}
#' and \eqn{p_Y}, the sum \eqn{X + Y} has probability mass function
#' \eqn{p_X \ast p_Y}.
#' Hence, convolving two `ddf` distributions gives the distribution corresponding
#' to their sum.
#'
#' @source
#' Under the hood, this functions uses a `C++` implementation for calculating
#' the convolution in order to get significant performance gains, compare
#' [convolve_cpp()].
#'
#' @param dist1 `ddf` object, the first distribution
#' @param dist2 `ddf` object, the second distribution
#' @param desc The description for the resulting `ddf` object (optional)
#'
#' @return Convolution as a `ddf` object.
#' @export
#' @family {convolution functions}
#'
#' @examples
#' # Calculate the distribution of the sum of throwing a dice twice
#' dice <- ddf(1:6)
#' conv(dice, dice, desc = "Distribution of throwing a dice twice")
#'
#' # Note that for distributions which are approximated,
#' # errors can propagate when convolving:
#' try(conv(unif(5), geometric(0.9)))
#' # This can be corrected by using a better approximation
#' conv(unif(5), geometric(0.9, eps=1e-11))
#'
#' # When one is interested in the difference instead of
#' # the sum, one can use the generic `-` which multiplies
#' # the support of a distribution with -1
#' conv(unif(6), -unif(6))
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
#' For more details, see [conv()].
#'
#' @param dist A `ddf` distribution
#' @param n An integer specifying number of convolutions to perform
#' @param desc The description for the resulting `ddf` object (optional)
#'
#' @return n-fold convolution as a `ddf` object.
#' @export
#' @family {convolution functions}
#'
#' @examples
#' # Calculate the distribution of the sum of throwing a dice four times
#' conv_n(ddf(1:6), 4, desc = "Distribution of throwing a dice four times")
conv_n <- function(dist, n, desc = paste(n, "-fold convolution of", desc(dist))) {
  dist_new <- dist
  for (i in 1:(n - 1)) {
    dist_new <- conv(dist, dist_new)
  }
  return(dist_new)
}
