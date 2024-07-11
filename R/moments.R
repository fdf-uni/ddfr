# Calculate various types of moments for ddf objects

#' Calculate moments of distributions
#'
#' @description
#' This function calculates the \eqn{n}-th (raw) moment, also known as the
#' \eqn{n}-th moment about zero, of a `ddf` distribution.
#'
#' @details
#' The \eqn{n}-th (raw) moment of a random variable \eqn{X} is given by
#' \deqn{E[X^n],}
#' where \eqn{E} is the expectation operator.
#'
#' @param dist A `ddf` object, the distribution.
#' @param n An integer, the order of the moment.
#'
#' @return A double.
#' @export
#' @family moments
#'
#' @examples
#' # Expected value when throwing a six-sided dice
#' moment(unif(6), 1)
moment <- function(dist, n) {
  return(sum(supp(dist)^n * probs(dist)))
}

#' Calculate central moments of distributions
#'
#' @description
#' This function calculates the \eqn{n}-th central moment, also known as the
#' \eqn{n}-th moment about the mean, of a `ddf` distribution.
#'
#' @details
#' The \eqn{n}-th central moment of a random variable \eqn{X} is given by
#' \deqn{E[(X - E[X])^n],}
#' where \eqn{E} is the expectation operator.
#'
#' @param dist A `ddf` object, the distribution.
#' @param n An integer, the order of the moment.
#'
#' @return A double.
#' @export
#' @family moments
#'
#' @examples
#' # The zeroth central moment is always 1
#' central_moment(bin(5, 0.4), 0)
#'
#' # The first central moment is always 0
#' central_moment(hypergeometric(10, 7, 5), n = 1)
#'
#' # The second central moment is the variance
#' central_moment(unif(10), 2)
#' # Result using the formula for the uniform distribution
#' (10^2 - 1) / 12
central_moment <- function(dist, n) {
  mu <- moment(dist, 1)
  return(sum((supp(dist) - mu)^n * probs(dist)))
}

#' Calculate standardized moments of distributions
#'
#' @description
#' This function calculates the \eqn{n}-th standardized moment of a `ddf`
#' distribution.
#'
#' @details
#' The \eqn{n}-th standardized moment of a random variable \eqn{X} is given by
#' \deqn{\mu_n/\sigma^n,}
#' where \eqn{\mu_n} denotes the \eqn{n}-th moment about the mean (see also
#' [central_moment()]) and \eqn{\sigma^n} is the \eqn{n}-th power of the standard
#' deviation.
#'
#' Note that we here normalize with \eqn{\sigma^n}, although sometimes other
#' normalizations are used.
#'
#' @param dist A `ddf` object, the distribution.
#' @param n An integer, the order of the moment.
#'
#' @return A double.
#' @export
#' @family moments
#'
#' @examples
#' # The second standardized moment is always 1
#' standardized_moment(unif(10), 2)
#'
#' # The third standardized moment measures skewness
#' # (compare ?skew())
#' standardized_moment(bin(10, 0.8), 3)
standardized_moment <- function(dist, n) {
  return(central_moment(dist, n) / central_moment(dist, 2)^(n / 2))
}
