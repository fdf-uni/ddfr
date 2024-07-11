#' Calculate expected values of distributions
#'
#' @description
#' This function calculates the mean/expected value of a discrete distribution
#' with finite support given as a `ddf` object.
#'
#' @details
#' The expected value of a discrete random variable \eqn{X} with finite support
#' \eqn{\{x_1, \dots, x_n\}} and corresponding probabilities
#' \eqn{p_1, \dots, p_n} is defined as the weighted average of the \eqn{x_k}
#' values with weights \eqn{p_k},
#' \deqn{E[X] := \sum_{k=1}^n x_k p_k.}
#' As we only work with discrete distributions with finite support, we don't
#' provide a more general definition using the tools from measure theory and
#' instead content ourselves with the basic one above.
#'
#' Note that there are two ways to use this function, one being
#' `expected_value()` and the other one being the S4 generic method `mean()`.
#' See also ‘Examples.’
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A double.
#' @export
#'
#' @examples
#' # Expected value when throwing a six-sided dice
#' expected_value(unif(6))
#' # Equivalent result using generic method
#' mean(unif(6))
#' @seealso [mean()]
expected_value <- function(dist) {
  return(moment(dist, 1))
}

#' Calculate modes of distributions
#'
#' @description
#' This function calculates the mode of a distribution, given as a `ddf` object.
#' In the case of the mode not being unique, by default all possible values are
#' returned. This behaviour can be controlled using the `methods` argument.
#'
#' @details
#' A mode of a discrete random variable \eqn{X} is a value at which the
#' probability mass function of \eqn{X} takes its maximal value.
#'
#' In general, it is not unique which is the reason for this function to have
#' the `method` argument.
#' * If `method` is set to "all", all values in the support which are a mode of
#' the given distribution are returned. This is the default behaviour.
#' * If `method` is set to "mean", the mean of all such values is returned.
#' * If `method` is set to "min" or "max", the respective elements of all
#'   modes are returned.
#'
#' @param dist A `ddf` object, the distribution.
#' @param method A character specifying the output format.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' # In case the mode is not unique, by default all
#' # possible modes are returned
#' modes(unif(3))
#' # This can be controlled using the method argument
#' modes(unif(3), method = "mean")
modes <- function(dist, method = "all") {
  methods <- c("all", "mean", "min", "max")
  # Ensure that method is implemented
  if (!(method %in% methods)) {
    stop(paste(
      "Error: Argument 'method' must be one of the following:",
      paste(methods, collapse = " ")
    ))
  }
  m <- supp(dist)[probs(dist) == max(probs(dist))]
  if (method == "all") {
    return(m)
  } else if (method == "mean") {
    return(mean(m))
  } else if (method == "min") {
    return(min(m))
  } else if (method == "max") {
    return(max(m))
  } else {
    # This shouldn't ever happen, but better be safe than sorry
    stop("Error: Unknown 'method'")
  }
}


#' Calculate medians of distributions
#'
#' @description
#' This function calculates the median(s) of a distribution, given as a `ddf`
#' object. In the case of the median not being unique, by default all possible
#' values are returned. This behaviour can be controlled using the `methods`
#' argument.
#'
#' @details
#' For details consult the details of [quantile()] and recall that a median is a
#' \eqn{\frac{1}{2}}-quantile and vice versa.
#'
#' @param dist A `ddf` object, the distribution.
#' @param method A character specifying output format.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' # In case the mode is not unique, by default all
#' # possible modes are returned
#' medians(unif(4))
#' # This can be controlled using the method argument
#' medians(unif(4), method = "mean")
medians <- function(dist, method = "all") {
  return(quantiles(dist, 1 / 2, method))
}
