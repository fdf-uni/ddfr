#' Shift a distribution
#'
#' @description
#' Shift (the support of) a `ddf` distribution by the specified amount.
#'
#' @param dist A `ddf` object, the distribution.
#' @param s A number, the shift to be applied to the distribution.
#' @param desc A character, the description for the shifted distribution.
#' (optional)
#'
#' @return A `ddf` object, the shifted distribution.
#' @export
#'
#' @examples
#' # Distribution for a simple symmetric random walk with
#' # step size 1 of length 4, centered at 3 (instead of 0)
#' shift(conv_n(rademacher(), 4), 3)
shift <- function(dist, s, desc = NULL) {
  return(ddf(
    supp(dist) + s,
    probs(dist),
    ifelse(is.null(desc), paste(desc(dist), ", shifted by ", s, sep = ""), desc)
  ))
}
