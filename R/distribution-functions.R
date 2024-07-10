#' Probability mass function
#'
#' @description
#' Create an `R` function which behaves like the probability mass function of a
#' given `ddf` distribution.
#'
#' @details
#' The probability mass function \eqn{p_X} of a discrete random variable \eqn{X}
#' is given by
#' \deqn{p_X(x) = \mathbb{P}[X = x],}
#' where \eqn{\mathbb{P}} is the probability operator.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A function.
#' @export
#'
#' @examples
#' # Get probability of 5 in a geometric distribution
#' pmf(geometric(0.4))(5)
pmf <- function(dist) {
  return(function(x) {
    return(ifelse(x %in% supp(dist), probs(dist)[match(x, supp(dist))], 0))
  })
}

#' Cumulative distribution function
#'
#' @description
#' Create an `R` function which behaves like the cumulative distribution
#' function of a given `ddf` distribution.
#'
#' @details
#' The cumulative distribution function \eqn{F_X} of a discrete random variable
#' \eqn{X} is given by
#' \deqn{F_X(x) = \mathbb{P}[X \le x],}
#' where \eqn{\mathbb{P}} is the probability operator.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A function.
#' @export
#'
#' @examples
#' # Get value at 8 of the CDF of a geometric distribution
#' cdf(geometric(0.4))(8)
cdf <- function(dist) {
  return(function(x) {
    return(sum(probs(dist)[supp(dist) <= x]))
  })
}
