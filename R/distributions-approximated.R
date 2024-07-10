# Implementation of common discrete distributions with countably infinite support
# They're approximated by cutting off the support at a large enough number

# TODO: Add details to documentations describing what each distribution models,
#       what its support is and how PMF is given.

#' The Poisson distribution
#'
#' @description
#' Create a `ddf` object for the Poisson distribution with the given parameters.
#'
#' @details
#' As the Poisson distribution has countably infinite support and this package
#' only works with discrete distributions with finite support, the resulting
#' `ddf` object approximates the Poisson distribution.
#'
#' For this, the support is cut off at a large enough integer such that the
#' overall probability is still close to 1.
#' The cutoff is controlled via the `eps` argument which specifies how close the
#' sum of all probabilities has to be to 1. The default value is 1e-10 since
#' this is also the minimum accuracy required for creating valid `ddf` objects.
#'
#' @importFrom stats qpois dpois
#'
#' @param lambda Expected rate of occurrences
#' @param eps How close the distribution is approximated (see details, optional)
#'
#' @source In order to calculate a fitting cutoff, the quantile function
#' [stats::qpois()] is used. [stats::dpois()] is then employed to calculate
#' the distribution.
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' pois(0.3)
#' # A more accurate approximation of the same distribution
#' pois(0.3, 1e-15)
pois <- function(lambda, eps = 1e-10) {
  # Check that lambda > 0
  if (lambda <= 0)
    stop("Error: Argument 'lambda' must be a positive real number")
  # Find large enough cutoff such that probabilities still almost sum up to 1
  upper_bound <- qpois(1-eps, lambda)
  supp <- 0 : upper_bound
  probs <- dpois(supp, lambda)
  # Create fitting description
  desc <- paste(
    "(Approximation of a) poisson distribution with lambda = ", lambda
  )
  return(ddf(supp, probs, desc))
}

#' The negative binomial distribution
#'
#' @description
#' Create a `ddf` object for the negative binomial distribution with the given
#' parameters.
#'
#' @details
#' As the negative binomial distribution has countably infinite support and
#' this package only works with discrete distributions with finite support, the
#' resulting `ddf` object approximates the negative binomial distribution.
#'
#' For this, the support is cut off at a large enough integer such that the
#' overall probability is still close to 1.
#' The cutoff is controlled via the `eps` argument which specifies how close the
#' sum of all probabilities has to be to 1. The default value is 1e-10 since
#' this is also the minimum accuracy required for creating valid `ddf` objects.
#'
#' @importFrom stats qnbinom dnbinom
#'
#' @param r Number of successes until the experiment is stopped
#' @param p Success probability in each experiment
#' @param eps How close the distribution is approximated (see details, optional)
#'
#' @source In order to calculate a fitting cutoff, the quantile function
#' [stats::qnbinom()] is used. [stats::dnbinom()] is then employed to calculate
#' the distribution.
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' # 'r' does not have to be an integer
#' neg_bin(2.25, 0.95)
#' # A more accurate approximation of the same distribution
#' neg_bin(2.25, 0.95, 1e-12)
neg_bin <- function(r, p, eps = 1e-10) {
  # Check that r > 0
  if (r <= 0)
    stop("Error: Argument 'r' must be a positive real number")
  # Check that probability lies between 0 (excluded) and 1
  if (!(0<p & p<=1))
    stop("Error: Argument 'p' must be between 0 (exclusive) and 1 (inclusive)")
  # Find large enough cutoff such that probabilities still almost sum up to 1
  upper_bound <- qnbinom(1-eps, r, p)
  supp <- 0 : upper_bound
  probs <- dnbinom(supp, r, p)
  # Create fitting description
  desc <- paste(
    "(Approximation of a) negative binomial distribution with parameters r =",
    r, "and p =", p
  )
  return(ddf(supp, probs, desc))
}

#' The geometric distribution
#'
#' @description
#' Create a `ddf` object for the geometric distribution with the given
#' parameters.
#'
#' @details
#' As the geometric distribution has countably infinite support and this package
#' only works with discrete distributions with finite support, the resulting
#' `ddf` object approximates the geometric distribution.
#'
#' For this, the support is cut off at a large enough integer such that the
#' overall probability is still close to 1.
#' The cutoff is controlled via the `eps` argument which specifies how close the
#' sum of all probabilities has to be to 1. The default value is 1e-10 since
#' this is also the minimum accuracy required for creating valid `ddf` objects.
#'
#' Note that both conventions for the geometric distribution (i.e. starting at 0
#' or 1) are supported by the function via its `start_at_one` argument.
#'
#' @param p Success probability
#' @param start_at_one Whether to start the support at 0 or 1 (default: FALSE)
#' @param eps How close the distribution is approximated (see details, optional)
#'
#' @source In order to calculate a fitting cutoff, the quantile function
#' [stats::qnbinom()] is used. [stats::dnbinom()] is then employed to calculate
#' the distribution.
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' geometric(0.8)
#' # A more accurate approximation of the same distribution,
#' # starting at 0 instead of 1
#' geometric(0.8, TRUE, 1e-15)
geometric <- function(p, start_at_one = FALSE, eps = 1e-10) {
  # Geometric distribution is just a negative binomial one with r = 1
  nb <- neg_bin(1, p, eps)
  # Create fitting description
  desc <- paste(
    "(Approximation of a) geometric distribution with p = ", p,
    ", starting at ", ifelse(start_at_one, 1, 0), sep = ""
  )
  return(ddf(supp(nb) + start_at_one, probs(nb), desc))
}
