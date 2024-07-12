#' Generate a report for a distribution
#'
#' @description
#' This function writes a detailed report given a `ddf` distribution involving
#' most properties such as various measures of central tendency and dispersion,
#' skewness and kurtosis and more.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A character.
#' @export
#'
#' @examples
#' # Write a report on a binomial distribution
#' # and output it using cat
#' cat(report(bin(20, 0.4)))
report <- function(dist) {
  # Store these two values as variables as they are used multiple times
  modes_l <- length(modes(dist))
  ek <- excess_kurtosis(dist)
  # Replace ek by zero if it is close to zero (inspired from ?integer() example)
  ek <- ifelse(abs(ek) < .Machine$double.eps^0.5, 0, ek)
  # Generate text for report
  r <- paste(
    "The given distribution could be described as \"",
    desc(dist),
    "\".\n\nIt has a mean/expected value of ",
    mean(dist),
    ", the average of its mode(s) is given by ",
    modes(dist, method = "mean"),
    " and the average of its median(s) is ",
    medians(dist, "mean"),
    ". It is a ",
    ifelse(
      modes_l == 1, "unimodal",
      ifelse(modes_l == 2, "bimodal", "multimodal")
    ),
    " distribution.\n\n",
    "Regarding its dispersion, calculating its variance yields ",
    variance(dist),
    " which implies a standard deviation of ",
    standard_deviation(dist),
    ". When talking about other measures of variability, one can assert that",
    " the distribution's range constitutes ",
    distribution_range(dist),
    " over its ",
    length(supp(dist)),
    " elements, whereas its interquartile range is given by ",
    iqr(dist),
    " since the (smallest) first quartile is ",
    quartile(dist, 1, "min"),
    " and the (largest) third quartile is ",
    quartile(dist, 3, "max"),
    ".\n\n",
    "It has a skewness of ",
    skew(dist),
    " (measured using Fisher's moment coefficient of skewness)",
    " and with an excess kurtosis of ",
    ek,
    " (and hence kurtosis of ",
    kurtosis(dist),
    ") it is a ",
    ifelse(
      ek == 0,
      "mesokurtic, also called mesokurtotic,",
      ifelse(ek > 0,
        "leptokurtic, also called leptokurtotic,",
        "platykurtic, also called platykurtotic,"
      )
    ),
    " distribution.\n\n",
    "Lastly, it can be noted that its entropy, measured in bits, is ",
    entropy(dist),
    ".",
    sep = ""
  )
  return(r)
}
