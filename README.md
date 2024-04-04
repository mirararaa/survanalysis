
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survanalysis

<!-- badges: start -->
<!-- badges: end -->

The goal of survanalysis is to provide basic tools for analyzing the rectangularity of survival curves and comparing multiple survival curves. 

Rectangularity metrics and calculations are based on the 1999 paper by John R. Wilmoth and Shiro Horiuchi, ["Rectangularization Revisited: Variability at Age of Death Within Human Populations."](https://link.springer.com/article/10.2307/2648085) These metrics and their brief explanations are described below.

The following will approach 0 as the survival curve becomes more rectangular and variability at age of death decreases.

- **Standard Deviation (SD)**
- ***Interquartile Range (IQR):*** Ideal to be used as a single measure of rectangularity, if needed.
- **Gini coefficient:** As presented by [Hanada (1983)](https://www.jstage.jst.go.jp/article/jjss1970/13/2/13_2_95/_pdf), a measure of variability in time of death.  
- **Keyfitz's H:** As described by [Keyfitz (1985)](https://link.springer.com/book/10.1007/978-1-4757-1879-9), a measure of change in life expectancy with respect to change in death rate (or, with respect to change in "force of mortality", as described by Wilmoth and Horiuchi). 


The following will approach 1 as the survival curve becomes more rectangular and variability at age of death decreases.
- **Fixed Rectangle:** The fraction of a rectangle with fixed height and width that is covered by the area under the survival curve.
- **Moving Rectangle:** The fraction of a rectangle with fixed height and variable width that is covered by the area under the survival curve. The width is determined by the time point at which 1% of the population remains.
- **Fastest Decline:** The absolute value of the slope of the survival curve at the point of fastest decline.
- **Sharpest Corner:** The absolute value of the second derivative of the survival curve, at the point where the curve makes the sharpest turn downward. Referred to as the minimum curvature by [Eakin and Witten (1995)](https://pubmed.ncbi.nlm.nih.gov/7758536/).
- **Quickest Plateau:** The second derivative of the survival curve, at the point where the curve makes the sharpest turn towards a horizontal line. Referred to as the maximum curvature by [Eakin and Witten (1995)](https://pubmed.ncbi.nlm.nih.gov/7758536/).
- **Prolate Index:** As presented by [Eakin and Witten (1995)](https://pubmed.ncbi.nlm.nih.gov/7758536/), the cosine of the angle between the vertical line at the point of quickest plateau, and the line between the point of the sharpest corner and the quickest plateau; a measure of how vertical the slope of the survival curve is between the minimum and maximum curvature.

For more information on metric calculations, see function documentation.

## Installation

You can install the development version of survanalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mirararaa/survanalysis")
```
