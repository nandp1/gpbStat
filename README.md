
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gpbStat

<!-- badges: start -->

<!-- badges: end -->

The package is used for analyzing the various Plant Breeding experiments. 

## Installation

You can install the released version of gpbStat from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gpbStat")
```

You can install the released version of gpbStat from
[Github](https://github.com/nandp1/gpbStat) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("nandp1/gpbStat")
```

## Example

Sample example for RCBD analysis

```r
library(gpbStat)

# Loading data
data(rcbdltc)

#Analysing data
results = ltc(rcbdltc, replication, line, tester, check, yield)

#View results
results
```

