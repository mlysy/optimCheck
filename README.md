
<!-- README.md is generated from README.Rmd. Please edit that file -->

# optimCheck: Graphical and Numerical Checks for Mode-Finding Routines

*Martin Lysy*

<!-- badges: start -->
<!-- badges: end -->

------------------------------------------------------------------------

### Description

Tools for checking that the output of an optimization algorithm is
indeed at a local mode of the objective function. This is accomplished
graphically by calculating all one-dimensional “projection plots” of the
objective function, i.e., varying each input variable one at a time with
all other elements of the potential solution being fixed. The numerical
values in these plots can be readily extracted for the purpose of
automated and systematic unit-testing of optimization routines.

### Installation

To install the CRAN version (1.0.1):

``` r
install.packages("optimCheck", INSTALL_opts = "--install-tests")
```

To install the latest development version: first install the
[**devtools**](https://CRAN.R-project.org/package=devtools) package,
then:

``` r
devtools::install_github("mlysy/optimCheck", INSTALL_opts = "--install-tests")
```

### Usage

A quick tutorial is provided in the package vignette:
`vignette("optimCheck")`.

### Unit Tests

To verify that the package has been installed correctly, you can run its
unit tests. First install the
[**testthat**](https://CRAN.R-project.org/package=testthat) package,
then:

``` r
testthat::test_package("optimCheck", reporter = "progress")
```
