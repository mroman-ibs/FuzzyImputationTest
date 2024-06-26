
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FuzzyImputationTest

<!-- badges: start -->
<!-- badges: end -->

The goal of FuzzyImputationTest is to impute (i.e., replace missing
values given by NAs) dataset that consists of triangular or trapezoidal
fuzzy numbers and check quality of such an imputation. To impute fuzzy
values, various imputation methods - both general (like miceRanger,
missingForest, VIM), and specific ones (DIMP) - are used. To check the
quality of the imputation process, the dataset without missing values
can be used, and then it is tested with the whole pack of procedures.
These procedures are related to calculation of statistics like the mean,
standard deviation, and distance measures for fuzzy numbers, together
with obtaining different error values, and conduction of statistical
tests based on the epistemic bootstrap.

## Installation

You can install the development version of FuzzyImputationTest from
[GitHub](https://github.com/) with:

## Example

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
