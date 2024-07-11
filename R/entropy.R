#' Calculate the entropy of distributions
#'
#' @description
#' This function calculates the entropy of a `ddf` distribution.
#'
#' @details
#' The entropy \eqn{\Eta} of a discrete random variable \eqn{X} with image
#' \eqn{\mathcal{X}} and probability mass function \eqn{p} is defined as
#' \deqn{\Eta = - \sum_{x\in\mathcal{X}} p(x) \log_b(p(x)),}
#' where \eqn{b} denotes the base of the logarithm being used.
#' Common values for \eqn{b} are \eqn{2}, Euler's constant \eqn{e} or \eqn{10}
#' and the corresponding units of entropy are "bits" (or "shannons"), "nats" and
#' "bans" (also called "hartleys" or "dits"), respectively.
#'
#' Entropy measures the level of "surprise" of the possible outcomes.
#'
#' @references \url{https://en.wikipedia.org/wiki/Entropy_(information_theory)}
#'
#' @param dist A `ddf` object, the distribution.
#' @param base A positive real number, the base for the logarithm. See ‘Details.’
#'
#' @return A double.
#' @export
#'
#' @examples
#' # The entropy of two fair coin tosses in "bits" is 2
#' entropy(unif(4))
entropy <- function(dist, base = 2) {
  if(base <= 0)
    stop("Error: Argument 'base' must be a positive real number")
  p <- probs(dist)
  return(-sum(p * log(p, base = base)))
}
