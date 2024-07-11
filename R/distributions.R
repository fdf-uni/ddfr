# Implementation of common discrete distributions with finite support

# Compare examples of ?integer()
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' The discrete uniform distribution
#'
#' @description
#' Create a `ddf` object for the discrete uniform distribution on the first
#' \eqn{n} natural numbers (including or excluding 0).
#'
#' @details
#' The discrete uniform distribution describes models in which a finite number
#' of possible outcomes are equally likely to happen.
#' In general, it has an arbitrary finite set, say of cardinality
#' \eqn{N\in\mathbb{N}}, as its support with every element of the support having
#' the same probability \eqn{\frac{1}{N}}.
#'
#' This function, however, only generates the uniform distribution on support
#' \eqn{\{1, \dots, n\}} or \eqn{\{0, \dots, n\}}. For more complicated supports,
#' it's expected to be much easier to simply use [ddf()] without specifying
#' probabilities, rather than using a custom function like this one.
#' For a demonstration of this, see also the ‘Examples’ below.
#'
#' @param n An integer, the upper end of the support.
#' @param start_at_one Logical, whether to start the support at 0 or 1.
#' (default: TRUE)
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
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
#' ddf(seq(3, 12, 3))
unif <- function(n, start_at_one = TRUE) {
  # Check that n is large enough integer
  if (!(is.wholenumber(n) & n >= start_at_one)) {
    stop(paste(
      "Argument `n` must be a non-negative integer",
      "(strictly positive if start_at_one = TRUE)"
    ))
  }
  # Create fitting description
  desc <- paste(
    "Discrete uniform distribution on",
    ifelse(
      n - start_at_one >= 1,
      paste("{", as.numeric(start_at_one), ", ..., ", n, "}", sep = ""),
      paste("{", as.numeric(start_at_one), "}", sep = "")
    )
  )
  return(ddf(start_at_one:n, desc = desc))
}

#' The binomial distribution
#'
#' @description
#' Create a `ddf` object for the binomial distribution with the given parameters.
#'
#' @details
#' The binomial distribution with parameters \eqn{n} and \eqn{p} models the
#' number of successes when conducting \eqn{n} independent experiments with each
#' one being distributed according to the Bernoulli distribution with success
#' probability \eqn{p} (see also [bernoulli()]).
#'
#' It has support \eqn{\{0, \dots, n\}} on which its probability mass function
#' is given by
#' \deqn{p(k) = \binom{n}{k} p^k (1-p)^{n-k}.}
#'
#' @param n An integer, the number of trials.
#' @param p A number between 0 and 1, the success probability for each trial.
#'
#' @source This function uses [stats::dbinom()] to calculate the distribution
#' efficiently.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' # How often a six can be expected when
#' # throwing a six-sided dice thrice
#' bin(3, 1 / 6)
bin <- function(n, p) {
  # Check that n is large enough integer
  if (!(is.wholenumber(n) & n >= 0)) {
    stop("Argument `n` must be a non-negative integer")
  }
  # Check that probability lies between 0 and 1
  if (!(0 <= p & p <= 1)) {
    stop("Argument `p` must be between 0 and 1, inclusive")
  }
  # Ensure proper support when p = 0 or p = 1
  if (p == 0 | p == 1) {
    supp <- as.numeric(p == 1) * n
    probs <- 1
  } else {
    supp <- 0:n
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
#' @details
#' The Bernoulli distribution with success probability \eqn{p} only has the two
#' elements \eqn{0} and \eqn{1} as its support with the corresponding
#' probability mass function being given by
#' \deqn{p(k) = \begin{cases} p & \text{if } k = 1,\\ 1-p & \text{if } k = 0. \end{cases}}
#' Hence, it can be used to model experiments asking a single yes-no question or
#' experiments whose only outcomes are "success" and "failure".
#'
#'
#' @param p A number between 0 and 1, the success probability.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' # Distribution modelling a fair coin toss
#' bernoulli(1 / 2)
bernoulli <- function(p) {
  # Check that probability lies between 0 and 1
  if (!(0 <= p & p <= 1)) {
    stop("Argument `p` must be between 0 and 1, inclusive")
  }
  # Ensure proper support when p = 0 or p = 1
  if (p == 0 | p == 1) {
    supp <- as.numeric(p == 1)
    probs <- 1
  } else {
    supp <- 0:1
    probs <- c(1 - p, p)
  }
  return(ddf(supp, probs, paste("Bernoulli distribution with p =", p)))
}

#' The Rademacher distribution
#'
#' @description
#' Create a `ddf` object for the Rademacher distribution with the given
#' parameters.
#'
#' @details
#' The Rademacher distribution has support \eqn{\{-1,1\}} with both elements
#' having probability \eqn{\frac{1}{2}} (that is, it is the discrete uniform
#' distribution on \eqn{\{-1,1\}}).
#'
#' It can be used to model simple symmetric random walks with step size 1,
#' especially when being convolved with itself (compare the ‘Examples’).
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @references \url{https://en.wikipedia.org/wiki/Rademacher_distribution}
#'
#' @examples
#' rademacher()
#'
#' # Modelling a symmetric random walk starting at 0
#' # with step size 1 of length 4
#' conv_n(rademacher(), 4)
rademacher <- function() {
  return(ddf(c(-1, 1), rep(1 / 2, 2), "Rademacher distribution"))
}

#' Benford's law
#'
#' @description
#' Create a `ddf` object describing Benford's law for the given base.
#'
#' @details
#' Benford's law describes the phenomenon that in many real-life numerical data
#' sets, leading digits tend to be small.
#'
#' For this, given an arbitrary base \eqn{b}, one can consider the distribution
#' with support \eqn{\{1, \dots, b-1\}} on which the probability mass function
#' \deqn{p(k) = \log_b \left( 1 + \frac{1}{k} \right)}
#' is defined, where the subscript \eqn{b} denotes the base of the logarithm.
#'
#' For more information, we refer to the below linked Wikipedia article.
#'
#' @param b An integer, the base of the number system. Has to be at least 2.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @references \url{https://en.wikipedia.org/wiki/Benford%27s_law}
#'
#' @examples
#' # Expected distribution of first digits,according
#' # to Benford's law, in the decimal system
#' benford(10)
benford <- function(b) {
  # Check that b is large enough integer
  if (!(is.wholenumber(b) & b >= 2)) {
    stop("Argument `b` must be an integer strictly greater than 1")
  }
  supp <- 1:(b - 1)
  return(ddf(supp, log(1 + 1 / supp, b), paste("Benford's law in base", b)))
}

#' The Zipf distribution
#'
#' @description
#' Create a `ddf` object for the Zipf distribution with the given parameters.
#'
#' @details
#' Zipf's law is an empirical law stating that the relative frequency of the
#' \eqn{n}-th entry in a list of measured values, ordered in decreasing order,
#' is inversely proportional to \eqn{n}.
#'
#' This can be generalized and formalized by the following inverse power law
#' with exponent \eqn{s}:
#'
#' The generalized Zipf distribution on \eqn{N} symbols has support
#' \eqn{\{1, \dots, N\}} on which its probability mass function is given by
#' \deqn{p(k) = \frac{1}{H_{N, s}} \frac{1}{k^s},}
#' where \eqn{H_{N, s}} is a generalized harmonic number:
#' \deqn{H_{N, s} = \sum_{k=1}^{N} \frac{1}{k^s}.}
#'
#' For some examples where Zipf's law occurs and further details, we recommend
#' the below linked Wikipedia article.
#'
#' @param N A positive integer, the total number of elements.
#' @param s A positive number, the exponent for the inverse power law.
#' See ‘Details.’ (Default: 1)
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @references \url{https://en.wikipedia.org/wiki/Zipf%27s_law}
#'
#' @examples
#' # The Zipf distribution on 5 elements
#' zipf(5)
zipf <- function(N, s = 1) {
  # Check that N is large enough integer
  if (!(is.wholenumber(N) & N >= 1)) {
    stop("Argument `N` must be a positive integer")
  }
  # Check that s >= 0
  if (s < 0) {
    stop("Argument `s` must be a non-negative real number")
  }
  supp <- 1:N
  generalized_harmonic_number <- sum(1 / supp^s)
  probs <- 1 / (generalized_harmonic_number * supp^s)
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
#' @details
#' The hypergeometric distribution models the number of successes when drawing
#' \eqn{n} elements, _without_ replacement, from a finite population of size
#' \eqn{N} which contains \eqn{K} success states.
#'
#' It has support
#' \deqn{\{\max(0, n+K-N), \dots, \min(n, K)\}}
#' on which its probability mass function is given by
#' \deqn{p(k) = \frac{\binom{K}{k} \binom{N-K}{n-k}}{\binom{N}{n}}.}
#'
#' @param N A positive integer, the population size.
#' @param K A non-negative integer, the number of success states in the
#' population. Has to be less or equal to `N`.
#' @param n A non-negative integer, the number of draws. Has to be less or equal
#' to `N`.
#'
#' @source This function uses [stats::dhyper()] to calculate the distribution
#' efficiently.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' # Model the total number of blue marbles when
#' # drawing 5 marbles from an urn containing
#' # 6 blue marbles and 20 marbles in total
#' hypergeometric(20, 6, 5)
hypergeometric <- function(N, K, n) {
  # Check that N is large enough integer
  if (!(is.wholenumber(N) & N >= 0)) {
    stop("Argument `N` must be a non-negative integer")
  }
  # Check that K is integer in {0, ..., N}
  if (!(is.wholenumber(K) & 0 <= K & K <= N)) {
    stop("Argument `K` must be an integer between 0 and `N`, inclusive")
  }
  # Check that n is integer in {0, ..., N}
  if (!(is.wholenumber(n) & 0 <= n & n <= N)) {
    stop("Argument `n` must be an integer between 0 and `N`, inclusive")
  }
  supp <- max(0, n + K - N):min(n, K)
  probs <- dhyper(supp, K, N - K, n)
  # Create fitting description
  desc <- paste(
    "Hypergeometric distribution with parameters ",
    "N = ", N, ", K = ", K, " and n = ", n,
    sep = ""
  )
  return(ddf(supp, probs, desc))
}

#' The negative hypergeometric distribution
#'
#' @description
#' Create a `ddf` object for the negative hypergeometric distribution with the
#' given parameters.
#'
#' @details
#' The negative hypergeometric distribution models the number of successes when
#' drawing, _without_ replacement, elements from a finite population of size
#' \eqn{N} which contains \eqn{K} success states _until_ precisely \eqn{r}
#' failures have been found.
#'
#' It has support \eqn{\{0, \dots, K\}} on which its probability mass function
#' is given by
#' \deqn{p(k) = \frac{\binom{k+r-1}{k} \binom{N-r-k}{K-k}}{\binom{N}{K}}.}
#'
#' The beta-binomial distribution provides a generalization of the negative
#' hypergeometric distribution, see [beta_binomial()].
#'
#' @param N A positive integer, the population size.
#' @param K A non-negative integer, the number of success states in the
#' population. Has to be less or equal to `N`.
#' @param r A non-negative integer, the number of failures until the experiment
#' is stopped. Has to be less or equal to `N`-`K`.
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @examples
#' # Model how many blue marbles are drawn from an urn containing
#' # 20 marbles of which 6 are blue, when one stops as soon
#' # as one has found 5 non-blue marbles
#' negative_hypergeometric(20, 6, 5)
negative_hypergeometric <- function(N, K, r) {
  # Check that N is large enough integer
  if (!(is.wholenumber(N) & N >= 0)) {
    stop("Argument `N` must be a non-negative integer")
  }
  # Check that K is integer in {0, ..., N}
  if (!(is.wholenumber(K) & 0 <= K & K <= N)) {
    stop("Argument `K` must be an integer between 0 and `N`, inclusive")
  }
  # Check that r is integer in {0, ..., N - K}
  if (!(is.wholenumber(r) & 0 <= r & r <= N - K)) {
    stop("Argument `r` must be an integer between 0 and `N`-`K`, inclusive")
  }
  # Ensure proper support when r = 0
  if (r == 0) {
    supp <- 0
    probs <- 1
  } else {
    supp <- 0:K
    probs <- choose(supp + r - 1, supp) * choose(N - r - supp, K - supp) / choose(N, K)
  }
  # Create fitting description
  desc <- paste(
    "Negative hypergeometric distribution with parameters ",
    "N = ", N, ", K = ", K, " and r = ", r,
    sep = ""
  )
  return(ddf(supp, probs, desc))
}

#' The beta-binomial distribution
#'
#' @description
#' Create a `ddf` object for the beta-binomial distribution with the given
#' parameters.
#'
#' @details
#' The beta-binomial distribution is a modification of the binomial distribution
#' (see also [bin()]) in which the success probability of every experiment is
#' not a fixed number \eqn{p}, but instead randomly drawn from a beta
#' distribution in every experiment.
#'
#' Said rigorously, it has support \eqn{\{0, \dots, n\}} where \eqn{n} is the
#' number of trials.
#' On its support, its probability mass function is given by
#' \deqn{p(k) = \binom{n}{k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}.}
#' Here, \eqn{B(a, b) = \frac{\Gamma(a) \Gamma(b)}{\Gamma(a + b)}} denotes the
#' beta function (see also [base::beta()]).
#'
#' Note that if \eqn{\alpha} and \eqn{\beta} are both integers, it coincides
#' with a negative hypergeometric distribution with parameters
#' \eqn{N = n + \alpha + \beta - 1}, \eqn{K = n} and \eqn{r = \alpha},
#' i.e. it generalizes the negative hypergeometric distribution
#' (described in [negative_hypergeometric()]).
#' This is also explored a bit in the ‘Examples’ below.
#'
#' @param n A non-negative integer, the number of trials.
#' @param alpha,beta Positive numbers, the shape parameters for the underlying
#' beta distribution. See ‘Details.’
#'
#' @return A `ddf` distribution as described above.
#' @export
#' @family distributions
#'
#' @references \url{https://en.wikipedia.org/wiki/Beta-binomial_distribution}
#'
#' @examples
#' beta_binomial(4, 0.6, 2)
#'
#' # If all arguments are integers, it coincides with
#' # a negative hypergeometric distribution
#' beta_binomial(8, 5, 2)
#' negative_hypergeometric(14, 8, 5)
beta_binomial <- function(n, alpha, beta) {
  # Check that n is large enough integer
  if (!(is.wholenumber(n) & n >= 0)) {
    stop("Argument `n` must be a non-negative integer")
  }
  # Check that alpha > 0
  if (alpha <= 0) {
    stop("Argument `alpha` must be a positive real number")
  }
  # Check that beta > 0
  if (beta <= 0) {
    stop("Argument `beta` must be a positive real number")
  }
  supp <- 0:n
  probs <- choose(n, supp) * beta(supp + alpha, n - supp + beta) / beta(alpha, beta)
  # Create fitting description
  desc <- paste(
    "Beta-binomial distribution with parameters ",
    "n = ", n, ", alpha = ", alpha, " and beta = ", beta,
    sep = ""
  )
  return(ddf(supp, probs, desc))
}
