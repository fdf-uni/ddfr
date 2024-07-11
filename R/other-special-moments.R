#' Calculate the skewness of distributions
#'
#' @description
#' This function calculates the skewness of a `ddf` distribution.
#'
#' @details
#' "Skewness" here refers to **Fisher's moment coefficient** of skewness, also
#' known under **Pearson's** instead of Fisher's name or simply as the
#' **moment coefficient of skewness**. It is given by the third standardized
#' moment, see also the help [standardized_moment()].
#'
#' A distribution with negative skew has a longer left tail, its mass is
#' concentrated to the right end of its support and when plotting its
#' probability mass function one gets a right-leaning curve.
#'
#' Analogously, a distribution with positive skew has a longer right tail, its
#' mass is concentrated to the left end of its support and when plotting its
#' probability mass function one gets a left-leaning curve.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # A binomial distribution with success probability
#' # p < 0.5 has positive skew
#' skew(bin(10, 0.25))
#' # whereas one with p > 0.5 has negative skew
#' skew(bin(10, 0.75))
skew <- function(dist) {
  return(standardized_moment(dist, 3))
}

#' Calculate the kurtosis of distributions
#'
#' @description
#' This function calculates the kurtosis of a `ddf` distribution.
#'
#' @details
#' The kurtosis is given by the fourth standardized moment, thus for some more
#' details one can consult [standardized_moment()].
#'
#' It measures the "tailedness" of a distribution, i.e. the heaviness
#' of its tails. A higher kurtosis indicates a higher propensity to produce
#' outliers.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # The rademacher distribution has a very low kurtosis
#' # (in fact it is as minimal as possible)
#' kurtosis(rademacher())
#' # A binomial distribution with relatively low probability
#' # has rather high kurtosis
#' kurtosis(bin(10, 1e-4))
kurtosis <- function(dist) {
  return(standardized_moment(dist, 4))
}

#' Calculate the excess kurtosis of distributions
#'
#' @description
#' This function calculates the excess kurtosis of a `ddf` distribution.
#'
#' @details
#' The excess kurtosis is simply defined as the [kurtosis()] minus 3, hence we
#' only refer to the linked help page for some more details.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' excess_kurtosis(unif(8))
excess_kurtosis <- function(dist) {
  return(kurtosis(dist) - 3)
}
