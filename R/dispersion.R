#' Calculate the variance of distributions
#'
#' @description
#' This function calculates the variance of a `ddf` distribution.
#'
#' @details
#' The variance is the second central moment. For more details see the
#' corresponding help, i.e. [central_moment()].
#'
#' It is a measure of dispersions, i.e. it measures how much the mass of a
#' distribution is spread around its mean. A lower variance indicates that
#' values are to be expected close to the mean.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # Variance of a binomial distribution
#' variance(bin(10, 0.3))
#' # Using the corresponding formula
#' 10*0.3*(1-0.3)
variance <- function(dist) {
  return(central_moment(dist, 2))
}

#' Calculate the standard deviation of distributions
#'
#' @description
#' This function calculates the standard deviation of a `ddf` distribution.
#'
#' @details
#' The standard deviation is the square root of the variance (which itself is
#' the 2nd central moment). For more details see [variance()] or better directly
#' [central_moment()].
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # Standard deviation of a hypergeometric distribution
#' standard_deviation(hypergeometric(20, 7, 3))
standard_deviation <- function(dist) {
  return(sqrt(variance(dist)))
}

#' Calculate the range of distributions
#'
#' @description
#' This function calculates the range of a `ddf` distribution.
#'
#' @details
#' The range is the length of the narrowest interval containing the entire
#' support.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # Range of the Rademacher distribution
#' distribution_range(rademacher())
distribution_range <- function(dist) {
  return(max(supp(dist)) - min(supp(dist)))
}

#' Calculate the interquartile range of distributions
#'
#' @description
#' This function calculates the interquartile range of a `ddf` distribution.
#'
#' @details
#' The interquartile range is defined as the difference \eqn{Q_3 - Q_1}, where
#' \eqn{Q_1} and \eqn{Q_3} are the first and third quartiles, respectively (for
#' more information see [quartile()] and [quantile()]).
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # Interquartile range of a uniform distribution
#' iqr(unif(10))
iqr <- function(dist) {
  return(quantiles(dist, 0.75, "max") - quantiles(dist, 0.25, "min"))
}
