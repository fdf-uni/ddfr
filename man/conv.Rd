% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convolve.R, R/ddf-class.R
\name{conv}
\alias{conv}
\alias{*,ddf,ddf-method}
\title{Convolve two discrete distributions}
\source{
Under the hood, this functions uses a \verb{C++} implementation for calculating
the convolution in order to get significant performance gains, compare
\code{\link[=convolve_cpp]{convolve_cpp()}}.
}
\usage{
conv(dist1, dist2, desc = "A convolution")

\S4method{*}{ddf,ddf}(e1, e2)
}
\arguments{
\item{dist1}{A \code{ddf} object, the first distribution.}

\item{dist2}{A \code{ddf} object, the second distribution.}

\item{desc}{A character, the description for the resulting \code{ddf} object (optional)}

\item{e1}{\code{ddf} object, the first distribution.}

\item{e2}{\code{ddf} object, the second distribution.}
}
\value{
Convolution as a \code{ddf} object.
}
\description{
Using this function, two distributions, given as \code{ddf} objects, can be
convolved.
}
\details{
In general, given two functions \eqn{f} and \eqn{g} defined on the set
\eqn{\mathbb{Z}} of integers, the \strong{discrete convolution} of \eqn{f} and
\eqn{g} is given by
\deqn{(f \ast g)(n) = \sum_{m=-\infty}^{\infty} f(n-m) g(m).}
Note how the sum on the right hand side ranges over all products where the
arguments of \eqn{f} and \eqn{g} are integers \eqn{k,l\in\mathbb{Z}}
such that \eqn{k+l=n}.

This implementation of the convolution uses this last observation as its
basis which makes it possible to also calculate the convolution if the
domains are not (subsets of) \eqn{\mathbb{Z}} (but of course still discrete).

For a stochastic interpretation of the convolution, recall that given two
independent random variables \eqn{X} and \eqn{Y} with probability mass
functions \eqn{p_X} and \eqn{p_Y}, the sum \eqn{X + Y} has probability mass
function \eqn{p_X \ast p_Y}.
Hence, convolving two \code{ddf} distributions gives the distribution corresponding
to their sum.
}
\examples{
# Calculate the distribution of the sum of throwing a dice twice
dice <- ddf(1:6)
conv(dice, dice, desc = "Distribution of throwing a dice twice")
# (Up to description) equivalent call using generic method
dice * dice


# Note that for distributions which are approximated without
# also being normalized, approximation errors can propagate
# when convolving:
try(conv(unif(5), geometric(0.9, normalize = FALSE)))

# This can be corrected by using a better approximation
conv(unif(5), geometric(0.9, eps = 1e-11, normalize = FALSE))

# Or simply by normalizing
conv(unif(5), geometric(0.9))


# When one is interested in the difference instead of
# the sum, one can use the generic `-` which multiplies
# the support of a distribution with -1
conv(unif(6), -unif(6))
}
\seealso{
Other convolution functions: 
\code{\link{conv_n}()},
\code{\link{convolve_cpp}()}
}
\concept{convolution functions}
