
# gpbStat

<!-- badges: start -->

<!-- badges: end -->

The package is used for statistical analysis of Plant Breeding
experiments.

Package Website <https://nandp1.github.io/gpbStat/>

Note: In the latest version 0.3.1 estimation of Kings Variance is not
included.

## Installation

You can install package from Github through

``` r
install.packages("devtools")
library(devtools)
install_github("nandp1/gpbStat")
```

Install gpbStat from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gpbStat")
```

## Example

Line by Tester analysis (only crosses).

``` r

# Loading the gpbStat package
library(gpbStat)
#> Authors Nandan Patil and Lakshmi Gangavati

# Loading dataset
data(rcbdltc)


## Now by using function ltc we analyze the data.
## The first parameter of `ltc`  function is "data" followed by replication, line, tester and dependent variable(yield)
results1 = ltc(rcbdltc, replication, line, tester, yield)
#> 
#> Analysis of Line x Tester:  yield

## Viewing the results
results1
#> $Means
#>      Testers
#> Lines       6       7       8
#>     1  68.550 107.640  52.640
#>     2  73.265  97.640  85.650
#>     3 100.885 111.540 117.735
#>     4 105.795  64.450  46.855
#>     5  84.150  81.935  94.820
#> 
#> $`Overall ANOVA`
#>                 Df    Sum Sq    Mean Sq   F value       Pr(>F)
#> Replication      3   148.436   49.47866  0.509612 6.778194e-01
#> Crosses         14 26199.654 1871.40388 19.274772 6.737492e-14
#> Lines            4 10318.361 2579.59035 27.466791 1.421271e-11
#> Testers          2  1718.926  859.46289  9.151332 4.626865e-04
#> Lines X Testers  8 14162.367 1770.29589 18.849639 4.973396e-12
#> Error           42  4077.815   97.09084        NA           NA
#> Total           59 30425.906         NA        NA           NA
#> 
#> $`Coefficient of Variation`
#> [1] 11.42608
#> 
#> $`Genetic Variance`
#>     Genotypic Variance    Phenotypic Variance Environmental Variance 
#>              455.48131              552.57215               97.09084 
#> 
#> $`Genetic Variability `
#>    Phenotypic coefficient of Variation     Genotypic coefficient of Variation 
#>                             27.2585365                             24.7481829 
#> Environmental coefficient of Variation                                   <NA> 
#>                             11.4260778                              0.8242929 
#> 
#> $`Line x Tester ANOVA`
#>                 Df    Sum Sq    Mean Sq   F value       Pr(>F)
#> Lines            4 10318.361 2579.59035 27.466791 1.421271e-11
#> Testers          2  1718.926  859.46289  9.151332 4.626865e-04
#> Lines X Testers  8 14162.367 1770.29589 18.849639 4.973396e-12
#> Error           42  4077.815   97.09084        NA           NA
#> 
#> $`GCA lines`
#>       1       2       3       4       5 
#>  -9.960  -0.718  23.817 -13.870   0.732 
#> 
#> $`GCA testers`
#>      6      7      8 
#>  0.292  6.404 -6.697 
#> 
#> $`SCA crosses`
#>      Testers
#> Lines       6       7       8
#>     1  -8.019  24.959 -16.940
#>     2 -12.546   5.717   6.828
#>     3  -9.461  -4.918  14.378
#>     4  33.136 -14.321 -18.815
#>     5  -3.111 -11.438  14.548
#> 
#> $`Proportional Contribution`
#>          Lines         Tester  Line x Tester 
#>      39.383578       6.560872      54.055550 
#> 
#> $`GV Singh & Chaudhary`
#>                  Cov H.S. (line)                Cov H.S. (tester) 
#>                        67.441205                       -45.541650 
#>               Cov H.S. (average)               Cov F.S. (average) 
#>                         2.680894                       408.052454 
#> F = 0, Adittive genetic variance F = 1, Adittive genetic variance 
#>                        10.723574                         5.361787 
#> F = 0, Variance due to Dominance F = 1, Variance due to Dominance 
#>                       836.602526                       418.301263 
#> 
#> $`Standard Errors`
#>      S.E. gca for line    S.E. gca for tester        S.E. sca effect 
#>               2.844451               2.203303               4.926734 
#>     S.E. (gi - gj)line   S.E. (gi - gj)tester S.E. (sij - skl)tester 
#>               4.022662               3.115940               6.967454 
#> 
#> $`Critical differance`
#>      C.D. gca for line    C.D. gca for tester        C.D. sca effect 
#>               5.740335               4.446445               9.942552 
#>     C.D. (gi - gj)line   C.D. (gi - gj)tester C.D. (sij - skl)tester 
#>               8.118060               6.288222              14.060892
```

``` r
# Similarly we analyze the line tester data containing only crosses laid out in Alpha lattice design.
# Loading dataset
data("alphaltc")

# Viewing the Structure of dataset
str(alphaltc)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    60 obs. of  5 variables:
#>  $ replication: num  1 2 3 4 1 2 3 4 1 2 ...
#>  $ block      : num  7 1 6 1 9 6 4 7 6 5 ...
#>  $ line       : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ tester     : num  6 6 6 6 7 7 7 7 8 8 ...
#>  $ yield      : num  93 105.7 82.1 65.2 70.7 ...
# There are five columns replication, block, line, tester and yield.


## Now by using function ltc we analyze the data.
## The first parameter of `ltc`  function is "data" followed by replication, line, tester, dependent variable(yield) and block.
## Note: The "block" parameter comes at the end.
results2 = ltc(alphaltc, replication, line, tester, yield, block)
#> 
#> Analysis of Line x Tester: yield

## Viewing the results
results2
#> $Means
#>      Testers
#> Lines        6        7        8
#>     1 86.47500 88.95833 89.55000
#>     2 88.64667 55.48000 50.12667
#>     3 51.19917 53.28417 36.91583
#>     4 33.47500 34.29833 50.78417
#>     5 45.30417 42.14500 49.98000
#> 
#> $`Overall ANOVA`
#>                           Df     Sum Sq   Mean Sq   F value       Pr(>F)
#> Replication                3  1586.4934  528.8311  3.566866 5.496129e-02
#> Crosses                   14 23862.0199 1704.4300 11.496059 2.347628e-04
#> Blocks within Replication 32  5446.5151  170.2036  1.147991 4.316074e-01
#> Lines                      4 18835.3119 4708.8280 24.883334 6.536498e-11
#> Testers                    2   463.1458  231.5729  1.223724 3.037332e-01
#> Lines X Testers            8  4563.5622  570.4453  3.014461 8.508293e-03
#> Error                     10  1482.6212  148.2621        NA           NA
#> Total                     59  2551.7268        NA        NA           NA
#> 
#> $`Coefficient of Variation`
#> [1] 21.32146
#> 
#> $`Genetic Variance`
#>     Genotypic Variance    Phenotypic Variance Environmental Variance 
#>               293.8997               442.1618               148.2621 
#> 
#> $`Genetic Variability `
#>    Phenotypic coefficient of Variation     Genotypic coefficient of Variation 
#>                             36.8207313                             30.0193557 
#> Environmental coefficient of Variation                                   <NA> 
#>                             21.3214571                              0.6646881 
#> 
#> $`Line x Tester ANOVA`
#>                 Df     Sum Sq   Mean Sq   F value       Pr(>F)
#> Lines            4 18835.3119 4708.8280 24.883334 6.536498e-11
#> Testers          2   463.1458  231.5729  1.223724 3.037332e-01
#> Lines X Testers  8  4563.5622  570.4453  3.014461 8.508293e-03
#> Error           10  1482.6212  148.2621        NA           NA
#> 
#> $`GCA lines`
#>       1       2       3       4       5 
#>  31.220   7.643  -9.975 -17.589 -11.298 
#> 
#> $`GCA testers`
#>      6      7      8 
#>  3.912 -2.275 -1.637 
#> 
#> $`SCA crosses`
#>      Testers
#> Lines      6      7       8
#>     1 -5.765  2.906   2.859
#>     2 19.984 -6.996 -12.988
#>     3  0.154  8.426  -8.580
#>     4 -9.956 -2.946  12.902
#>     5 -4.417 -1.390   5.807
#> 
#> $`Proportional Contribution`
#>          Lines         Tester  Line x Tester 
#>      78.934273       1.940933      19.124794 
#> 
#> $`GV Singh & Chaudhary`
#>                  Cov H.S. (line)                Cov H.S. (tester) 
#>                        344.86523                        -16.94362 
#>               Cov H.S. (average)               Cov F.S. (average) 
#>                         30.06778                        263.22720 
#> F = 0, Adittive genetic variance F = 1, Adittive genetic variance 
#>                        120.27111                         60.13555 
#> F = 0, Variance due to Dominance F = 1, Variance due to Dominance 
#>                        211.09158                         20.82769 
#> 
#> $`Standard Errors`
#>      S.E. gca for line    S.E. gca for tester        S.E. sca effect 
#>               3.514993               2.722702               6.088147 
#>     S.E. (gi - gj)line   S.E. (gi - gj)tester S.E. (sij - skl)tester 
#>               4.970951               3.850482               8.609940 
#> 
#> $`Critical differance`
#>      C.D. gca for line    C.D. gca for tester        C.D. sca effect 
#>               7.831893               6.066558              13.565236 
#>     C.D. (gi - gj)line   C.D. (gi - gj)tester C.D. (sij - skl)tester 
#>              11.075969               8.579409              19.184141
```
