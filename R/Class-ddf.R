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
#' @slot supp A numeric vector representing the support
#' @slot probs A numeric vector containing the probabilities
#' corresponding to the elements of the support
#' @slot desc A short description of the discrete distribution
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



#' Create new `ddf` objects
#' @importFrom methods new
#' @importFrom stats aggregate
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
#'
#' If the probabilities aren't supplied, they are distributed uniformly over the
#' specified support.
#'
#' @param supp A numeric vector representing the support
#' @param probs A numeric vector containing the probabilities
#' corresponding to the elements of the support
#' @param desc A short description of the discrete distribution
#'
#' @return A `ddf` object with the specified properties
#' @export
#'
#' @examples
#' # Create ddf object for an ordinary six-sided dice
#' dice <- ddf(1:6, desc = "Distribution modelling a six-sided dice")
#' # Create ddf object for an unfair coin toss without custom description
#' coin_toss <- ddf(c(1,2), c(1/4, 3/4))
ddf <- function(supp, probs = rep(1/length(supp), length(supp)), desc = "A discrete distribution with finite support") {
  # Remove possible duplicates from the support
  if(any(duplicated(supp))) {
    cleaned <- aggregate(probs, by = list(supp = supp), FUN = sum)
    supp <- cleaned$supp
    probs <- cleaned$x
  }
  return(
    new(
      "ddf",
      # Order support and probabilities
      supp = sort(supp), probs = probs[order(supp)],
      desc = desc
    )
  )
}



# == Getters ==

#' Get the support of a `ddf` object
#'
#' @param dist A `ddf` object
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
#' @param dist A `ddf` object
#'
#' @return Probabilities on the support as a numerical vector.
#' @export
#'
#' @examples
#' probs(ddf(1:3))
#' probs(ddf(1:3, c(1/2, 1/4, 1/4)))
setGeneric("probs", function(dist) standardGeneric("probs"))
#' @export
#' @rdname probs
setMethod("probs", "ddf", function(dist) dist@probs)


#' Get the description of a `ddf` object
#'
#' @param dist A `ddf` object
#'
#' @return Description text of the `ddf` object.
#' @export
#'
#' @examples
#' desc(ddf(1:3))
#' desc(ddf(1:3, desc = "My custom description"))
setGeneric("desc", function(dist) standardGeneric("desc"))
#' @export
#' @rdname desc
setMethod("desc", "ddf", function(dist) dist@desc)



# Create custom text when showing/printing a `ddf` object
setMethod("show", "ddf", function(object) {
  cat(
    object@desc, "\n",
    "  Support:", object@supp, "\n",
    "  Probabilities:", object@probs, "\n"
  )
})
