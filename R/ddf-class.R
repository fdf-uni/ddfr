# == Class ==

#' An S4 class to represent a discrete distribution with finite support
#'
#' @description
#' A discrete distribution with finite support is here represented by its
#' probability mass function (PMF). For this, it has one slot which stores the
#' support of the PMF as a numeric vector and another slot for storing the value
#' of the PMF at the corresponding point in the support.
#' There is one further slot for a description.
#'
#' @details
#' New `ddf` objects are checked for validity to ensure that they fulfill basic
#' properties of a discrete probability distribution with finite support.
#' The relevant checks for this are:
#' * Support and probability vectors must have the same length.
#' * All probabilities have to lie between 0 (excluded) and 1 (included).
#'   Here, 0 is not included as elements of the support have, by definition,
#'   non-zero probability.
#' * The probabilities have to sum up to approximately 1. Currently the
#'   tolerance for this is set to `1e-10`.
#'
#' @slot supp A numeric vector, the support.
#' @slot probs A numeric vector, the corresponding probabilities.
#' @slot desc A character, a short description of the discrete distribution.
setClass(
  "ddf",
  slots = list(
    supp = "numeric",
    probs = "numeric",
    desc = "character"
  ),
  prototype = list(
    supp = NA_real_,
    probs = NA_real_,
    desc = "A discrete distribution with finite support"
  ),
  validity = function(object) {
    if (length(object@supp) != length(object@probs)) {
      "support and probabilities must have the same length"
    } else if (abs(1 - sum(object@probs)) >= 1e-10) {
      "probabilities have to sum up to approximately 1"
    } else if (any(duplicated(object@supp))) {
      "support may only contain unique elements"
    } else if (any(object@probs == 0)) {
      "by definition, probabilities of elements in the support have to be non-zero"
    } else if (any(object@probs > 1) | any(object@probs < 0)) {
      "probabilities have to be between 0 and 1"
    } else {
      TRUE
    }
  }
)



# == Constructors ==

#' Create new `ddf` objects
#'
#' @description
#' Use this function to create a new `ddf` object by passing the support and
#' the corresponding probabilities as vectors to it.
#'
#' @details
#' This function makes sure that your `ddf` objects are "clean" by ensuring the
#' following two conditions:
#' * The passed support is ordered in ascending order.
#' * Duplicates within the support are removed and the probabilities of the
#'   affected elements are simply summed up.
#' * Elements of the passed "support" that have probability 0 are removed as
#'   they then, by definition, are not contained in the support.
#'
#' If the probabilities aren't supplied, they are distributed uniformly over the
#' specified support.
#'
#' @param supp A numeric vector, support.
#' @param probs A numeric vector, the corresponding probabilities
#' @param desc A character, a short description of the discrete distribution.
#'
#' @return A `ddf` object with the specified attributes.
#' @export
#'
#' @examples
#' # Create ddf object for an ordinary six-sided dice
#' dice <- ddf(1:6, desc = "Distribution modelling a six-sided dice")
#' dice
#' # Create ddf object for an unfair coin toss without custom description
#' coin_toss <- ddf(c(1, 2), c(1 / 4, 3 / 4))
#' coin_toss
ddf <- function(supp, probs = rep(1 / length(supp), length(supp)), desc = "A discrete distribution with finite support") {
  # This might be unnecessary, but we leave it here just to be sure
  if (length(supp) != length(probs)) {
    stop("Support and probabilities must have the same length")
  }
  # Remove possible duplicates from the support
  if (any(duplicated(supp))) {
    cleaned <- aggregate(probs, by = list(supp = supp), FUN = sum)
    supp <- cleaned$supp
    probs <- cleaned$x
  }
  # Remove supp elements with probability zero
  supp <- supp[probs != 0]
  probs <- probs[probs != 0]
  return(
    new(
      "ddf",
      # Order support and probabilities
      supp = sort(supp), probs = probs[order(supp)],
      desc = desc
    )
  )
}


#' Create new `ddf` objects from absolute frequencies
#'
#' @description
#' Use this function to create a new `ddf` object based on absolute frequencies
#' by passing the occurred observations and corresponding absolute frequencies
#' as vectors to it.
#'
#' @details
#' Although the purpose of this package is to work with discrete distributions
#' with finite support, it might still be helpful to use some of its functions
#' for working with frequency counts. For example, one might want to calculate
#' an interquartile range or the excess kurtosis.
#'
#' The purpose of this function is to make this possible in a convenient way.
#' A new `ddf` object is created by dividing all frequency counts by the total
#' number of observations such that the resulting vector sums up to 1.
#'
#' This function furthermore ensures the same quality checks as [ddf()] (see the
#' ‘Details.’ section) and has the same behaviour when no frequencies are
#' passed, i.e. the probabilities are distributed uniformly over the specified
#' observations.
#'
#' @param events A numeric vector, the event space.
#' @param frequencies A numeric vector, the corresponding absolute frequencies.
#' @param desc A character, a short description of the discrete distribution.
#'
#' @return A `ddf` object as described above.
#' @export
#'
#' @examples
#' # Create ddf object from (hypothetical) frequencies
#' # of tossing a fair coin 10 times
#' coin_tosses <- ddf_from_frequencies(c(1, 2), c(3, 7))
#' coin_tosses
ddf_from_frequencies <- function(
    events, frequencies = rep(1 / length(events), length(events)),
    desc = paste(
      "A discrete distribution with finite support,",
      "generated from frequencies"
    )) {
  return(ddf(events, frequencies / sum(frequencies), desc))
}



# == Getters ==

#' Get the support of a `ddf` object
#'
#' @description
#' This function returns the support of a given distribution
#' as a numerical vector.
#'
#' @details
#' The support of a function is defined as the points in the
#' function's domain where it is non-zero.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return Support as a numerical vector.
#' @export
#'
#' @examples
#' supp(ddf(1:3))
setGeneric("supp", function(dist) standardGeneric("supp"))
#' @export
#' @rdname supp
setMethod("supp", "ddf", function(dist) dist@supp)


#' Get the probabilities of a `ddf` object
#'
#' @description
#' This function returns the probabilities of elements of the
#' support of a given distribution as a numerical vector.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return Probabilities as a numerical vector.
#' @export
#'
#' @examples
#' probs(ddf(1:3))
#' probs(ddf(1:3, c(1 / 2, 1 / 4, 1 / 4)))
setGeneric("probs", function(dist) standardGeneric("probs"))
#' @export
#' @rdname probs
setMethod("probs", "ddf", function(dist) dist@probs)


#' Get the description of a `ddf` object
#'
#' @description
#' This function returns the description of a given distribution.
#'
#' @param dist A `ddf` object, the distribution.
#'
#' @return A character, the description text of the `ddf` object.
#' @export
#'
#' @examples
#' desc(ddf(1:3))
#' desc(ddf(1:3, desc = "My custom description"))
setGeneric("desc", function(dist) standardGeneric("desc"))
#' @export
#' @rdname desc
setMethod("desc", "ddf", function(dist) dist@desc)


# == Setters ==

#' Set the description of a `ddf` object
#'
#' @param dist A `ddf` object, the distribution.
#' @param value A character, new description.
#'
#' @return New description text of the `ddf` object.
#' @export
#'
#' @examples
#' mydist <- unif(8)
#' print(mydist)
#' desc(mydist) <- "This distribution needs a better description"
#' print(mydist)
setGeneric("desc<-", function(dist, value) standardGeneric("desc<-"))
#' @export
#' @rdname desc-set
setMethod("desc<-", "ddf", function(dist, value) {
  dist@desc <- value
  validObject(dist)
  dist
})



# == Further methods ==

#' Multiply the support of a distribution with -1
#'
#' @param e1 A `ddf` object, the distribution.
#' @param e2 Unused argument. Has to be missing.
#'
#' @return A `ddf` distribution.
#' @export
#'
#' @examples
#' -bin(5, 0.1)
setMethod("-", c("ddf", "missing"), function(e1, e2) {
  return(ddf(
    -supp(e1), probs(e1),
    paste(desc(e1), ", multiplied with -1", sep = "")
  ))
})

#' @export
#' @rdname conv
#' @param e1 `ddf` object, the first distribution.
#' @param e2 `ddf` object, the second distribution.
setMethod("*", c("ddf", "ddf"), function(e1, e2) {
  return(conv(e1, e2))
})

# Create custom text when showing/printing a `ddf` object
setMethod("show", "ddf", function(object) {
  cat(object@desc, "\n\n")
  cat("Support:\n")
  print(object@supp)
  cat("\nProbabilities:\n")
  print(object@probs)
})

# Calculate mean
#' @export
#' @rdname expected_value
#' @param x `ddf` object, the distribution.
setMethod("mean", "ddf", function(x) {
  moment(x, 1)
})

# Plotting
#' Plot a distribution
#'
#' @description
#' Create a plot of a `ddf` distribution.
#'
#' @param x A `ddf` object, the distribution.
#' @param xlab The label for the \eqn{x}-axis.
#' @param ylab The label for the \eqn{y}-axis.
#' @param col The color for the points of the plot.
#' @param main An overall title of the plot.
#' @param sub A subtitle for the plot.
#' @param y Unused argument. Has to be missing.
#'
#' @return A `ggplot` object displaying the given distribution.
#' @export
#' @family plotting functions
#'
#' @examples
#' plot(pois(8))
setMethod("plot", c("ddf", "missing"), function(
    x, xlab = "support", ylab = "probabilities",
    col = "deepskyblue3", main = NULL, sub = NULL, y) {
  ggplot(mapping = aes(x = supp(x), y = probs(x))) +
    geom_col(fill = col) +
    labs(x = xlab, y = ylab, title = main, subtitle = sub) +
    theme_light()
})
