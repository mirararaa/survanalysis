
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survanalysis

<!-- badges: start -->
<!-- badges: end -->

The goal of survanalysis is to provide tools for analyzing the
rectangularity of survival curves and comparing multiple survival
curves.

Rectangularity metrics and calculations are based on the 1999 paper by
John R. Wilmoth and Shiro Horiuchi, [“Rectangularization Revisited:
Variability at Age of Death Within Human
Populations.”](https://link.springer.com/article/10.2307/2648085) These
metrics and their brief explanations are described below.

The following will approach 0 as the survival curve becomes more
rectangular and variability at age of death decreases.

Standard Deviation (SD)

Interquartile Range (IQR)

Gini coefficient:

Keyfitz’s H:

The following will approach 1 as the survival curve becomes more
rectangular and variability at age of death decreases.

Fixed Rectangle: The fraction of a rectangle with fixed height and width
that is covered by the area under the survival curve.

Moving Rectangle: The fraction of a rectangle with fixed height and
variable width that is covered by the area under the survival curve.

Fastest Decline: The absolute value of the slope of the survival curve
at the point of fastest decline.

Sharpest Corner: The absolute value of the second derivative of the
survival curve, at the point where the curve makes the sharpest turn
downward.

Quickest Plateau: The second derivative of the survival curve, at the
point where the curve makes the sharpest turn towards a horizontal line.

Prolate Index: The cosine of the angle between the vertical line at the
point of quickest plateau, and the line between the point of the
sharpest corner and the quickest plateau; a measure of how vertical the
slope of the survival curve is.

For more information on metric calculations, see function documentation.

## Installation

You can install the development version of survanalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mirararaa/survanalysis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(survanalysis)
#> 
#> Attaching package: 'survanalysis'
#> The following object is masked from 'package:graphics':
#> 
#>     rect
## basic example code
```

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
