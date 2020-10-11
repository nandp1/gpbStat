
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpbStat

<!-- badges: start -->

<!-- badges: end -->

The goal of gpbStat is to …

## Installation

You can install the released version of gpbStat from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gpbStat")
```

## Example

Sample example for RCBD analysis

```r

library(gpbStat)

## Load data

data("alphalt")

results1 = ltcalpha(alphalt, replication, block, line, tester, yield)
```

