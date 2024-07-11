#' Calculate quantiles of distributions
#'
#' @description
#' This function calculates \eqn{p}-quantiles of a `ddf` distribution.
#'
#' @details
#' A \eqn{p}-quantile of a random variable \eqn{X} is a value \eqn{x} such
#' that
#' \deqn{\mathbb{P}[ X \le x] \ge p \qquad\text{ and }\qquad \mathbb{P}[X \ge x] \ge 1-p,}
#' where \eqn{\mathbb{P}} denotes the probability operator.
#'
#' In general, it is not unique which is the reason for this function to have
#' the `method` argument.
#' * If `method` is set to "all", all values in the support which are a \eqn{p}-
#' quantile of the given distribution are returned. This is the default behaviour.
#' * If `method` is set to "mean", the mean of all such values is returned.
#' * If `method` is set to "min" or "max", the respective elements of the
#'   \eqn{p}-quantiles in the support are returned.
#'
#' @param dist A `ddf` object, the distribution.
#' @param p A number between 0 and 1, exclusive.
#' @param method A character specifying the output format.
#'
#' @return A numeric vector.
#' @export
#' @family quantiles
#'
#' @examples
#' quantiles(unif(6), 1/2)
#' # Using another method for output
#' quantiles(unif(6), 1/2, method = "mean")
quantiles <- function(dist, p, method = "all") {
  methods <- c("all", "mean", "min", "max")
  # Ensure that method is implemented
  if(!(method %in% methods))
    stop(paste(
      "Error: Argument 'method' must be one of the following:",
      paste(methods, collapse = " ")
    ))
  # Ensure that p is between 0 and 1
  if(!(0 < p & p < 1))
    stop("Error: Argument 'p' must be between 0 and 1, exclusive")
  # Calculate quantiles
  q <- supp(dist)[
    (cumsum(probs(dist)) >= p) & (rev(cumsum(rev(probs(dist)))) >= 1 - p)
  ]
  # Return quantiles in format matching the method
  if(method == "all") {
    return(q)
  } else if (method == "mean") {
    return(mean(q))
  } else if (method == "min") {
    return(min(q))
  } else if (method == "max") {
    return(max(q))
  } else {
    # This shouldn't ever happen, but better be safe than sorry
    stop("Error: Unknown 'method'")
  }
}

#' Calculate percentiles of distributions
#'
#' @description
#' This function calculates the \eqn{k}-th percentile of a `ddf` distribution.
#'
#' @details
#' For details consult the details of [quantiles()] and recall that a \eqn{k}-th
#' percentile is a \eqn{k/100}-quantile and vice versa.
#'
#' @param dist A `ddf` object, the distribution.
#' @param k An integer between 0 and 100, exclusive.
#' @param method A character specifying the output format.
#'
#' @return A numeric vector.
#' @export
#' @family quantiles
#'
#' @examples
#' percentile(bin(10, 0.3), 30)
percentile <- function(dist, k, method = "all") {
  return(quantiles(dist, k/100, method))
}

#' Calculate deciles of distributions
#'
#' @description
#' This function calculates the \eqn{k}-th decile of a `ddf` distribution.
#'
#' @details
#' For details consult the details of [quantiles()] and recall that a \eqn{k}-th
#' decile is a \eqn{k/10}-quantile and vice versa.
#'
#' @param dist A `ddf` object, the distribution.
#' @param k An integer between 0 and 10, exclusive.
#' @param method A character specifying the output format.
#'
#' @return A numeric vector.
#' @export
#' @family quantiles
#'
#' @examples
#' decile(bin(10, 0.3), 3)
decile <- function(dist, k, method = "all") {
  return(quantiles(dist, k/10, method))
}

#' Calculate quartiles of distributions
#'
#' @description
#' This function calculates the \eqn{k}-th quartile of a `ddf` distribution.
#'
#' @details
#' For details consult the details of [quantiles()] and recall that a \eqn{k}-th
#' quartile is a \eqn{k/4}-quantile and vice versa.
#'
#' @param dist A `ddf` object, the distribution.
#' @param k An integer between 0 and 4, exclusive.
#' @param method A character specifying the output format.
#'
#' @return A numeric vector.
#' @export
#' @family quantiles
#'
#' @examples
#' quartile(bin(10, 0.3), 3)
quartile <- function(dist, k, method = "all") {
  return(quantiles(dist, k/4, method))
}
