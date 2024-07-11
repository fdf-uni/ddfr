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
#' The Poisson distribution models the number of occurrences of an event within
#' a given time frame. For this, the events have to occur independently, must
#' not be able to occur at the same instant and have to occur at a constant
#' average rate, expressed by the parameter \eqn{\lambda}.
#'
#' It has support \eqn{\mathbb{N}_0} on which its probability mass function is
#' given by
#' \deqn{p(k) = e^{-\lambda} \frac{\lambda^k}{k!}.}
#'
#' As the Poisson distribution has countably infinite support and this package
#' only works with discrete distributions with finite support, the resulting
#' `ddf` object can only approximate the Poisson distribution.
#'
#' For this, the support is cut off at a large enough integer such that the
#' overall probability is still close to 1.
#' The cutoff is controlled via the `eps` argument which specifies how close the
#' sum of all probabilities has to be to 1. The default value is 1e-10 since
#' this is also the minimum accuracy required for creating valid `ddf` objects.
#'
#' @param lambda A positive number, the expected rate of occurrences.
#' @param eps A positive number, how close the distribution is approximated.
#' See ‘Details.’ (optional)
#'
#' @source In order to calculate a fitting cutoff, the quantile function
#' [stats::qpois()] is used. [stats::dpois()] is then employed to calculate
#' the distribution.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' pois(0.3)
#' # A more accurate approximation of the same distribution
#' pois(0.3, 1e-15)
pois <- function(lambda, eps = 1e-10) {
  # Check that lambda > 0
  if (lambda <= 0) {
    stop("Argument `lambda` must be a positive real number")
  }
  # Check that eps > 0
  if (eps <= 0) {
    stop("Argument `eps` must be a positive real number")
  }
  # Find large enough cutoff such that probabilities still almost sum up to 1
  upper_bound <- qpois(1 - eps, lambda)
  supp <- 0:upper_bound
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
#' The negative binomial distribution models the number of failures in a
#' sequence of iid Bernoulli trials (see also [bernoulli()]) with common success
#' probability \eqn{p}, before a specified number \eqn{r} of successes occurs.
#'
#' It has support \eqn{\mathbb{N}_0} on which its probability mass function is
#' given by
#' \deqn{p(k) = \binom{k+r-1}{k} (1-p)^k p^r.}
#' Note how the second parameter \eqn{r} can be generalized to arbitrary
#' positive reals which the currently documented function also does.
#'
#'
#' As the negative binomial distribution has countably infinite support and
#' this package only works with discrete distributions with finite support, the
#' resulting `ddf` object can only approximate the negative binomial
#' distribution.
#'
#' For this, the support is cut off at a large enough integer such that the
#' overall probability is still close to 1.
#' The cutoff is controlled via the `eps` argument which specifies how close the
#' sum of all probabilities has to be to 1. The default value is 1e-10 since
#' this is also the minimum accuracy required for creating valid `ddf` objects.
#'
#' @param r A positive number, the number of successes until the experiment is
#' stopped.
#' @param p A number between 0 and 1, the success probability in each experiment.
#' @param eps A positive number, how close the distribution is approximated.
#' See ‘Details.’ (optional)
#'
#' @source In order to calculate a fitting cutoff, the quantile function
#' [stats::qnbinom()] is used. [stats::dnbinom()] is then employed to calculate
#' the distribution.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' # 'r' does not have to be an integer
#' negative_bin(2.25, 0.95)
#' # A more accurate approximation of the same distribution
#' negative_bin(2.25, 0.95, 1e-12)
negative_bin <- function(r, p, eps = 1e-10) {
  # Check that r > 0
  if (r <= 0) {
    stop("Argument `r` must be a positive real number")
  }
  # Check that probability lies between 0 (excluded) and 1
  if (!(0 < p & p <= 1)) {
    stop("Argument `p` must be between 0 (exclusive) and 1 (inclusive)")
  }
  # Find large enough cutoff such that probabilities still almost sum up to 1
  upper_bound <- qnbinom(1 - eps, r, p)
  supp <- 0:upper_bound
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
#' The geometric distribution can refer to either of the following two
#' distributions:
#' * The probability distribution of the number of iid Bernoulli trials with
#'   common success probability \eqn{p} needed to get a success.
#'
#'   It has support \eqn{\mathbb{N}^+} on which its probability mass function is
#'   given by
#'   \deqn{p(k) = (1-p)^{k-1} p.}
#' * The probability distribution of the number of failures before the first
#'   success is observed in the same experiment as above.
#'
#'   Note that this simply corresponds to the first one by a shift of \eqn{1},
#'   i.e. its support is \eqn{\mathbb{N}_0} on which its probability mass
#'   function is given by
#'   \deqn{p(k) = (1-p)^k p.}
#'
#' The former of these two distributions is often referred to as the _shifted_
#' geometric distribution.
#' This function supports both of the above conventions via its `start_at_one`
#' argument.
#'
#'
#' Note that, as the geometric distribution has countably infinite support and
#' this package only works with discrete distributions with finite support, the
#' resulting `ddf` object can only approximate the geometric distribution.
#'
#' For this, the support is cut off at a large enough integer such that the
#' overall probability is still close to 1.
#' The cutoff is controlled via the `eps` argument which specifies how close the
#' sum of all probabilities has to be to 1. The default value is 1e-10 since
#' this is also the minimum accuracy required for creating valid `ddf` objects.
#'
#' @param p A number between 0 and 1, the success probability in each experiment.
#' @param start_at_one Logical, whether to start the support at 0 or 1.
#' (default: FALSE)
#' @param eps A positive number, how close the distribution is approximated.
#' See ‘Details.’ (optional)
#'
#' @source In order to calculate a fitting cutoff, the quantile function
#' [stats::qnbinom()] is used. [stats::dnbinom()] is then employed to calculate
#' the distribution.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' geometric(0.8)
#' # A more accurate approximation of the same distribution,
#' # starting at 1 instead of 0
#' geometric(0.8, TRUE, 1e-15)
geometric <- function(p, start_at_one = FALSE, eps = 1e-10) {
  # Geometric distribution is just a negative binomial one with r = 1
  nb <- negative_bin(1, p, eps)
  # Create fitting description
  desc <- paste(
    "(Approximation of a) geometric distribution with p = ", p,
    ", starting at ", ifelse(start_at_one, 1, 0),
    sep = ""
  )
  return(ddf(supp(nb) + start_at_one, probs(nb), desc))
}
