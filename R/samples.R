#' Random samples from a distribution
#'
#' @description
#' This function generates random samples of a given size based on a `ddf`
#' distribution.
#'
#' @source To generate the samples, [base::sample()] is used.
#'
#' @param dist A `ddf` object, the distribution.
#' @param n A non-negative integer giving the number of items to choose.
#'
#' @return A vector of length `n` with elements drawn from the distributions
#' support.
#' @export
#'
#' @examples
#' # Simulate throwing a dice 5 times
#' samples(unif(6), 5)
samples <- function(dist, n) {
  return(sample(supp(dist), n, replace = TRUE, prob = probs(dist)))
}
