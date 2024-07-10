# Implementation of common discrete distributions with finite support

# TODO: Add details to documentations describing what each distribution models,
#       what its support is and how PMF is given.

#' The discrete uniform distribution
#'
#' @description
#' Create a `ddf` object for the discrete uniform distribution with the given
#' parameters.
#'
#' @details
#' This function only generates the uniform distribution on support {1, ..., n}
#' or {0, ..., n}. For more complicated supports, it's expected to be much
#' easier to simply use [ddf()], rather than using a custom function like this
#' one, see also the example below.
#'
#' @param n Upper bound of the support
#' @param start_at_one Whether to start the support at 0 or 1 (default: TRUE)
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' # Probability distribution for modelling a
#' # six-sided dice (uniform on {1, ..., 6})
#' unif(6)
#'
#' # For more complicated supports, use ddf
#' # Same distribution as above (except for description):
#' ddf(1:6)
#' # Not possible using unif:
#' ddf(seq(3,12,3))
unif <- function(n, start_at_one = TRUE) {
  # Check that n is large enough integer, compare examples of ?integer()
  if (!(abs(n - round(n)) < .Machine$double.eps^0.5 & n >= start_at_one))
    stop(paste(
      "Error: Argument 'n' must be a non-negative integer",
      "(strictly positive if start_at_one = TRUE)"
    ))
  # Create fitting description
  desc <- paste("Discrete uniform distribution on",
    ifelse(
      n - start_at_one >= 1,
      paste("{", as.numeric(start_at_one), ", ..., ", n, "}", sep=""),
      paste("{", as.numeric(start_at_one), "}", sep="")
    )
  )
  return(ddf(start_at_one:n, desc = desc))
}

#' The binomial distribution
#'
#' @description
#' Create a `ddf` object for the binomial distribution with the given parameters.
#'
#' @importFrom stats dbinom
#'
#' @param n Number of trials
#' @param p Success probability for each trial
#'
#' @source This function uses [stats::dbinom()] to calculate the distribution
#' efficiently.
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' # How often a six can be expected when
#' # throwing a six-sided dice thrice
#' bin(3, 1/6)
bin <- function(n, p) {
  # Check that n is large enough integer, compare examples of ?integer()
  if (!(abs(n - round(n)) < .Machine$double.eps^0.5 & n >= 0))
    stop("Error: Argument 'n' must be a non-negative integer")
  # Check that probability lies between 0 and 1
  if (!(0<=p & p<=1))
    stop("Error: Argument 'p' must be between 0 and 1, inclusive")
  # Ensure proper support when p = 0 or p = 1
  if (p == 0 | p == 1) {
    supp <- as.numeric(p == 1)*n
    probs <- 1
  } else {
    supp <- 0 : n
    probs <- dbinom(supp, n, p)
  }
  # Create fitting description
  desc <- paste("Binomial distribution with parameters n =", n, "and p =", p)
  return(ddf(supp, probs, desc))
}

#' The Bernoulli distribution
#'
#' @description
#' Create a `ddf` object for the Bernoulli distribution with the given
#' parameters.
#'
#' @param p Success probability
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' # Distribution modelling a fair coin toss
#' bernoulli(1/2)
bernoulli <- function(p) {
  # Check that probability lies between 0 and 1
  if (!(0<=p & p<=1))
    stop("Error: Argument 'p' must be between 0 and 1, inclusive")
  # Ensure proper support when p = 0 or p = 1
  if (p == 0 | p == 1) {
    supp <- as.numeric(p == 1)
    probs <- 1
  } else {
    supp <- 0 : 1
    probs <- c(1-p, p)
  }
  return(ddf(supp, probs, paste("Bernoulli distribution with p =", p)))
}

#' The Rademacher distribution
#'
#' @description
#' Create a `ddf` object for the Rademacher distribution with the given
#' parameters.
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' rademacher()
rademacher <- function() {
  return(ddf(c(-1, 1), rep(1/2, 2), "Rademacher distribution"))
}

#' Benford's law
#'
#' @description
#' Create a `ddf` object describing Benford's law for the given base.
#'
#' @param b Base, integer which has to be at least 2
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' # Distribution of first digits,according
#' # to Benford's law, in decimal system
#' benford(10)
benford <- function(b) {
  # Check that b is large enough integer, compare examples of ?integer()
  if (!(abs(b - round(b)) < .Machine$double.eps^0.5 & b >= 2))
    stop("Error: Argument 'b' must be an integer strictly greater than 1")
  supp <- 1 : (b-1)
  return(ddf(supp, log(1+1/supp, b), paste("Benford's law in base", b)))
}

#' The Zipf distribution
#'
#' @description
#' Create a `ddf` object for the Zipf distribution with the given parameters.
#'
#' @param N Total number of elements
#' @param s Exponent for the inverse power law (see details, default: 1)
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' zipf(5)
zipf <- function(N, s = 1) {
  # Check that N is large enough integer, compare examples of ?integer()
  if (!(abs(N - round(N)) < .Machine$double.eps^0.5 & N >= 1))
    stop("Error: Argument 'N' must be a positive integer")
  # Check that s >= 0
  if (s<0)
    stop("Error: Argument 's' must be a non-negative real number")
  supp <- 1 : N
  generalized_harmonic_number <- sum(1/supp^s)
  probs <- 1/(generalized_harmonic_number * supp^s)
  # Create fitting description
  desc <- paste("Zipf distribution on N =", N, "elements with parameter s =", s)
  return(ddf(supp, probs, desc))
}

#' The hypergeometric distribution
#'
#' @description
#' Create a `ddf` object for the hypergeometric distribution with the given
#' parameters.
#'
#' @importFrom stats dhyper
#'
#' @param N Population size
#' @param K Number of success states in the population
#' @param n Number of draws
#'
#' @source This function uses [stats::dhyper()] to calculate the distribution
#' efficiently.
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' # Model the total number of white marbles when
#' # drawing 5 marbles from an urn containing
#' # 6 white marbles and 20 marbles in total
#' hypergeometric(20, 6, 5)
hypergeometric <- function(N, K, n) {
  # Check that N is large enough integer, compare examples of ?integer()
  if (!(abs(N - round(N)) < .Machine$double.eps^0.5 & N >= 0))
    stop("Error: Argument 'N' must be a non-negative integer")
  # Check that K is integer in {0, ..., N}
  if (!(abs(K - round(K)) < .Machine$double.eps^0.5 & 0 <= K & K <= N))
    stop("Error: Argument 'K' must be an integer between 0 and 'N', inclusive")
  # Check that n is integer in {0, ..., N}
  if (!(abs(n - round(n)) < .Machine$double.eps^0.5 & 0 <= n & n <= N))
    stop("Error: Argument 'n' must be an integer between 0 and 'N', inclusive")
  supp <- max(0, n+K-N) : min(n, K)
  probs <- dhyper(supp, K, N-K, n)
  # Create fitting description
  desc <- paste(
    "Hypergeometric distribution with parameters ",
    "N = ", N, ", K = ", K, " and n = ", n, sep = ""
  )
  return(ddf(supp, probs, desc))
}

#' The negative hypergeometric distribution
#'
#' @description
#' Create a `ddf` object for the negative hypergeometric distribution with the
#' given parameters.
#'
#' @param N Population size
#' @param K Number of success states in the population
#' @param r Number of failures until the experiment is stopped
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' hypergeometric(20, 6, 5)
negative_hypergeometric <- function(N, K, r) {
  # Check that N is large enough integer, compare examples of ?integer()
  if (!(abs(N - round(N)) < .Machine$double.eps^0.5 & N >= 0))
    stop("Error: Argument 'N' must be a non-negative integer")
  # Check that K is integer in {0, ..., N}
  if (!(abs(K - round(K)) < .Machine$double.eps^0.5 & 0 <= K & K <= N))
    stop("Error: Argument 'K' must be an integer between 0 and 'N', inclusive")
  # Check that r is integer in {0, ..., N - K}
  if (!(abs(r - round(r)) < .Machine$double.eps^0.5 & 0 <= r & r <= N - K))
    stop("Error: Argument 'r' must be an integer between 0 and 'N'-'K', inclusive")
  # Ensure proper support when r = 0
  if (r == 0) {
    supp <- 0
    probs <- 1
  } else {
    supp <- 0 : K
    probs <- choose(supp+r-1, supp) * choose(N-r-supp, K-supp)/choose(N,K)
  }
  # Create fitting description
  desc <- paste(
    "Negative hypergeometric distribution with parameters ",
    "N = ", N, ", K = ", K, " and r = ", r, sep = ""
  )
  return(ddf(supp, probs, desc))
}

#' The beta-binomial distribution
#'
#' @description
#' Create a `ddf` object for the beta-binomial distribution with the given
#' parameters.
#'
#' @param n Number of trials
#' @param alpha,beta Shape parameters for the underlying beta distribution
#'
#' @return A `ddf` distribution as described above
#' @export
#'
#' @examples
#' beta_binomial(4, 0.6, 2)
beta_binomial <- function(n, alpha, beta) {
  # Check that n is large enough integer, compare examples of ?integer()
  if (!(abs(n - round(n)) < .Machine$double.eps^0.5 & n >= 0))
    stop("Error: Argument 'n' must be a non-negative integer")
  # Check that alpha > 0
  if (alpha<=0)
    stop("Error: Argument 'alpha' must be a positive real number")
  # Check that beta > 0
  if (beta<=0)
    stop("Error: Argument 'beta' must be a positive real number")
  supp <- 0:n
  probs <- choose(n, supp) * beta(supp + alpha, n - supp + beta)/beta(alpha, beta)
  # Create fitting description
  desc <- paste(
    "Beta-binomial distribution with parameters ",
    "n = ", n, ", alpha = ", alpha, " and beta = ", beta, sep = ""
  )
  return(ddf(supp, probs, desc))
}
