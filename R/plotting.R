# See also method for generic plotting via `plot()` in "ddf-class.R"

#' Plot the PMF of a distribution
#'
#' @description
#' This function creates a plot of the probability mass function of a `ddf`
#' distribution.
#'
#' @details
#' For further details on the probability mass function, you may consult [pmf()].
#'
#' You might also be interested in the generic [base::plot()] method for `ddf`
#' objects.
#'
#' @param dist A `ddf` object, the distribution.
#' @param xlab The label for the \eqn{x}-axis.
#' @param ylab The label for the \eqn{y}-axis.
#' @param col The color for the points of the plot.
#' @param main An overall title of the plot.
#' @param sub A subtitle for the plot.
#'
#' @return A `ggplot` object displaying the PMF of the given distribution.
#' @export
#' @family {plotting functions}
#'
#' @examples
#' # Plot the PMF of a binomial distribution
#' plot_pmf(bin(20, 0.8))
#' # A similar plot using the S4 generic
#' plot(bin(20, 0.8))
plot_pmf <- function(
    dist, xlab = "x", ylab = "\u2119[X = x]",
    col = "deepskyblue3", main = NULL, sub = NULL) {
  p <- ggplot(mapping = aes(x = supp(dist), y = probs(dist))) +
    geom_line() +
    geom_point(color = col, size = 2) +
    labs(x = xlab, y = ylab, title = main, subtitle = sub) +
    theme_light()
  return(p)
}


#' Plot the CDF of a distribution
#'
#' @description
#' This function creates a plot of the cumulative distribution function (CDF) of
#' a `ddf` distribution.
#'
#' @details
#' For further details on the cumulative density function, you may consult [cdf()].
#'
#' @param dist A `ddf` object, the distribution.
#' @param xlab The label for the \eqn{x}-axis.
#' @param ylab The label for the \eqn{y}-axis.
#' @param col The color for the points of the plot.
#' @param main An overall title of the plot.
#' @param sub A subtitle for the plot.
#'
#' @return A `ggplot` object displaying the CDF of the given distribution.
#' @export
#' @family {plotting functions}
#'
#' @examples
#' # Plot the CDF of a binomial distribution
#' plot_cdf(bin(20, 0.8))
plot_cdf <- function(
    dist, xlab = "x", ylab = "\u2119[X \u2264 x]",
    col = "deepskyblue3", main = NULL, sub = NULL) {
  supp <- supp(dist)
  probs <- probs(dist)
  p <- ggplot(mapping = aes(x = supp, y = cumsum(probs))) +
    geom_step() +
    # Add step going from (-\infty, 0) to first point
    geom_segment(aes(x = -Inf, y = 0, xend = supp[1], yend = 0)) +
    geom_segment(aes(x = supp[1], y = 0, xend = supp[1], yend = probs[1])) +
    # Extend plot on the right to (~1, \infty)
    geom_segment(aes(x = supp[length(supp)], y = sum(probs), xend = Inf, yend = sum(probs))) +
    geom_point(color = col, size = 2) +
    labs(x = xlab, y = ylab, title = main, subtitle = sub) +
    theme_light()
  return(p)
}
