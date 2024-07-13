
# ddfr <a href="https://fdf-uni.github.io/ddfr/"><img src="man/figures/logo.png" align="right" height="116" alt="ddfr website" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of ddfr is to make it easier to work with *D*iscrete *D*istributions with *F*inite support in *R*.

For specific details, you can check out the [website](https://fdf-uni.github.io/ddfr) which is also linked in the description of this repository.

Especially the [Introduction](https://fdf-uni.github.io/ddfr/articles/introduction) should provide you with all relevant information to get started with the package.
You can also access it within `R` using:
``` r
vignette("introduction", "ddfr")
```
Alternatively, if you'd prefer to view it in your browser, there is also:
``` r
browseVignettes("ddfr")
```

## Installation

You can install the development version of ddfr like so:

``` r
remotes::install_github("fdf-uni/ddfr")
```

## Example

This is a basic example which shows some of `ddfr`'s features:

``` r
library(ddfr)
# Create uniform distribution on {1, ..., 6}
dist <- unif(6)

# Get some information about the distribution
expected_value(dist)
variance(dist)

# Convolve it with itself
conv(dist, dist)
# This can also be done a bit quicker
dist * dist

# Plot the PMF of the distribution
plot_pmf(dist)
```
