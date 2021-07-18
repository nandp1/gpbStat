
# gpbStat

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/gpbStat)](https://cran.r-project.org/package=gpbStat)
[![cran
checks](https://cranchecks.info/badges/summary/gpbStat)](https://cran.r-project.org/web/checks/check_results_gpbStat.html)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
![Downloads](http://cranlogs.r-pkg.org/badges/gpbStat)
[![](https://cranlogs.r-pkg.org/badges/grand-total/gpbStat)](https://cran.r-project.org/package=gpbStat)
<!-- badges: end -->

The package is used for statistical analysis of Plant Breeding
experiments.

Package Website <https://nandp1.github.io/gpbStat/>

Note: In the latest version 0.3.1 estimation of Kings Variance is not
included.

## Installation

Install latest package from Github through

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
# Load the package
library(gpbStat)

# Loading dataset
data("alphaltc")

# Viewing the Structure of dataset
str(alphaltc)
#> 'data.frame':    60 obs. of  5 variables:
#>  $ replication: chr  "r1" "r1" "r1" "r1" ...
#>  $ block      : chr  "b1" "b1" "b1" "b2" ...
#>  $ line       : int  5 1 4 4 1 2 2 5 3 1 ...
#>  $ tester     : int  7 8 8 6 7 7 6 6 8 6 ...
#>  $ yield      : num  47.3 109.4 36.3 36.2 70.7 ...
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
#>                           Df     Sum Sq   Mean Sq    F value       Pr(>F)
#> Replication                3  1586.4934  528.8311  3.1440495 4.213104e-02
#> Crosses                   14 23862.0199 1704.4300 10.1333150 3.161969e-07
#> Blocks within Replication 16  2555.9198  159.7450  0.9497288 5.307851e-01
#> Lines                      4 18835.3119 4708.8280 24.8833344 6.536498e-11
#> Testers                    2   463.1458  231.5729  1.2237239 3.037332e-01
#> Lines X Testers            8  4563.5622  570.4453  3.0144615 8.508293e-03
#> Error                     26  4373.2165  168.2006         NA           NA
#> Total                     59  2561.2067        NA         NA           NA
#> 
#> $`Coefficient of Variation`
#> [1] 22.70992
#> 
#> $`Genetic Variance`
#>     Genotypic Variance    Phenotypic Variance Environmental Variance 
#>               293.8997               462.1004               168.2006 
#> 
#> $`Genetic Variability `
#>    Phenotypic coefficient of Variation     Genotypic coefficient of Variation 
#>                             37.6417608                             30.0193557 
#> Environmental coefficient of Variation                                   <NA> 
#>                             22.7099195                              0.6360084 
#> 
#> $`Line x Tester ANOVA`
#>                 Df     Sum Sq   Mean Sq   F value       Pr(>F)
#> Lines            4 18835.3119 4708.8280 24.883334 6.536498e-11
#> Testers          2   463.1458  231.5729  1.223724 3.037332e-01
#> Lines X Testers  8  4563.5622  570.4453  3.014461 8.508293e-03
#> Error           26  4373.2165  168.2006        NA           NA
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
#>                         30.06778                        262.35565 
#> F = 0, Adittive genetic variance F = 1, Adittive genetic variance 
#>                        120.27111                         60.13555 
#> F = 0, Variance due to Dominance F = 1, Variance due to Dominance 
#>                        201.12232                         15.84306 
#> 
#> $`Standard Errors`
#>      S.E. gca for line    S.E. gca for tester        S.E. sca effect 
#>               3.743891               2.900005               6.484609 
#>     S.E. (gi - gj)line   S.E. (gi - gj)tester S.E. (sij - skl)tester 
#>               5.294661               4.101227               9.170622 
#> 
#> $`Critical differance`
#>      C.D. gca for line    C.D. gca for tester        C.D. sca effect 
#>               7.695678               5.961047              13.329305 
#>     C.D. (gi - gj)line   C.D. (gi - gj)tester C.D. (sij - skl)tester 
#>              10.883332               8.430193              18.850484
```

``` r
# Line x Tester analysis for multiple traits laid in Alpha lattice design.  
# Load the package
library(gpbStat)

#Load the dataset
data("alphaltcmt")

# View the structure of dataframe. 
str(alphaltcmt)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    60 obs. of  7 variables:
#>  $ replication: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ block      : num  1 1 1 2 2 2 3 3 3 4 ...
#>  $ line       : chr  "l5" "l1" "l2" "l2" ...
#>  $ tester     : chr  "t1" "t3" "t3" "t1" ...
#>  $ hsw        : num  26.7 22.1 26.2 25.7 18 ...
#>  $ sh         : num  82.2 83.6 83.8 81.7 81.6 ...
#>  $ gy         : num  61.3 30.7 48.1 25.9 29.1 ...

# Conduct Line x Tester analysis
result3 = ltcmt(alphaltcmt, replication, line, tester, alphaltcmt[,5:7], block)
#> 
#> Analysis of Line x Tester for Multiple traits
#> Warning in sqrt(x): NaNs produced

#> Warning in sqrt(x): NaNs produced

#> Warning in sqrt(x): NaNs produced

#> Warning in sqrt(x): NaNs produced

#> Warning in sqrt(x): NaNs produced

#> Warning in sqrt(x): NaNs produced

# View the output
result3
#> $Mean
#> $Mean$hsw
#>     Tester
#> Line        1        2        3
#>    1 24.28100 24.38975 26.43250
#>    2 24.75150 23.17850 23.85100
#>    3 22.12950 25.09375 25.46600
#>    4 25.36125 26.52300 26.32225
#>    5 24.40525 23.86375 22.90450
#> 
#> $Mean$sh
#>     Tester
#> Line        1        2        3
#>    1 82.93436 83.87124 84.20399
#>    2 83.89508 84.62547 83.77366
#>    3 83.61044 84.45869 83.04424
#>    4 84.27547 84.32636 81.81483
#>    5 83.04301 82.58873 84.83067
#> 
#> $Mean$gy
#>     Tester
#> Line        1        2        3
#>    1 54.26683 48.86251 44.75305
#>    2 44.95867 45.31223 47.39452
#>    3 46.06275 54.77228 55.05693
#>    4 60.56487 52.13965 53.79695
#>    5 58.26799 53.53054 53.55139
#> 
#> 
#> $ANOVA
#> $ANOVA$hsw
#>                           Df     Sum Sq   Mean Sq   F value      Pr(>F)
#> Replication                3 123.547315 41.182438 5.2008347 0.006007617
#> Blocks within Replication 16 159.485732  9.967858 1.2588177 0.292524662
#> Crosses                   14  95.615586  6.829685 0.8625051 0.603263868
#> Lines                      4  44.431866 11.107966 1.0223891 0.406049177
#> Testers                    2   6.558666  3.279333 0.3018333 0.740946613
#> Lines X Testers            8  44.625055  5.578132 0.5134172 0.839950285
#> Error                     26 205.879143  7.918429        NA          NA
#> Total                     59 584.527775        NA        NA          NA
#> 
#> $ANOVA$sh
#>                           Df     Sum Sq    Mean Sq   F value      Pr(>F)
#> Replication                3  47.865214 15.9550714 5.5805022 0.004306487
#> Blocks within Replication 16  61.859599  3.8662250 1.3522645 0.240056532
#> Crosses                   14  40.010784  2.8579131 0.9995938 0.481506718
#> Lines                      4   3.066186  0.7665466 0.1874088 0.943757507
#> Testers                    2   2.486129  1.2430645 0.3039100 0.739429879
#> Lines X Testers            8  34.458468  4.3073085 1.0530702 0.412196780
#> Error                     26  74.335936  2.8590745        NA          NA
#> Total                     59 224.071534         NA        NA          NA
#> 
#> $ANOVA$gy
#>                           Df      Sum Sq    Mean Sq   F value       Pr(>F)
#> Replication                3  3170.89296 1056.96432 7.6637547 0.0007890292
#> Blocks within Replication 16  2338.16012  146.13501 1.0595843 0.4350901435
#> Crosses                   14  1411.76346  100.84025 0.7311646 0.7260111510
#> Lines                      4   787.68515  196.92129 0.9743323 0.4310135285
#> Testers                    2    48.50139   24.25070 0.1199882 0.8872136703
#> Lines X Testers            8   575.57692   71.94711 0.3559818 0.9379857942
#> Error                     26  3585.84969  137.91730        NA           NA
#> Total                     59 10506.66623         NA        NA           NA
#> 
#> 
#> $GCA.Line
#>           Trait 1     Trait 2   Trait 3
#> Line 1  0.4375167 -0.01655394 -2.258613
#> Line 2 -0.6699000  0.41165231 -5.664270
#> Line 3 -0.3671500  0.01804113  0.411244
#> Line 4  1.4719333 -0.21419481  3.947743
#> Line 5 -0.8724000 -0.19894469  3.563895
#> 
#> $GCA.Tester
#>           Trait 1    Trait 2    Trait 3
#> Tester 1 -0.41120 -0.1347434  1.2714786
#> Tester 2  0.01285  0.2876815 -0.6293023
#> Tester 3  0.39835 -0.1529380 -0.6421764
#> 
#> $SCA
#> $SCA$`Trait 1`
#>     Tester
#> Line          1          2          3
#>    1 -0.3422167 -0.6575167  0.9997333
#>    2  1.2357000 -0.7613500 -0.4743500
#>    3 -1.6890500  0.8511500  0.8379000
#>    4 -0.2963833  0.4413167 -0.1449333
#>    5  1.0919500  0.1264000 -1.2183500
#> 
#> $SCA$`Trait 2`
#>     Tester
#> Line           1           2          3
#>    1 -0.60075619 -0.08630744  0.6870636
#>    2 -0.06824822  0.23971724 -0.1714690
#>    3  0.04072451  0.46655392 -0.5072784
#>    4  0.93799581  0.56645558 -1.5044514
#>    5 -0.30971591 -1.18641930  1.4961352
#> 
#> $SCA$`Trait 3`
#>     Tester
#> Line         1           2          3
#>    1  3.701222  0.19768507 -3.8989074
#>    2 -2.201279  0.05305477  2.1482244
#>    3 -7.172713  3.43759274  3.7351205
#>    4  3.792902 -2.73153863 -1.0613638
#>    5  1.879868 -0.95679396 -0.9230737
#> 
#> 
#> $CV
#>    Trait1    Trait2    Trait3 
#> 11.440345  2.020495 22.780202 
#> 
#> $Genetic.Variance.Covariance.
#>         Phenotypic Variance Genotypic Variance Environmental Variance
#> Trait 1          -0.6697598          -8.588188               7.918429
#> Trait 2          -0.4152151          -3.274290               2.859074
#> Trait 3        -101.1137222        -239.031018             137.917296
#>         Phenotypic coefficient of Variation Genotypic coefficient of Variation
#> Trait 1                                 NaN                                NaN
#> Trait 2                                 NaN                                NaN
#> Trait 3                                 NaN                                NaN
#>         Environmental coefficient of Variation Broad sense heritability
#> Trait 1                              11.440345                12.822788
#> Trait 2                               2.020495                 7.885767
#> Trait 3                              22.780202                 2.363982
#> 
#> $Std.Error
#>         S.E. gca for line S.E. gca for tester S.E. sca effect
#> Trait 1         0.8123232           0.6292229       1.4069851
#> Trait 2         0.4881150           0.3780922       0.8454399
#> Trait 3         3.3901487           2.6259979       5.8719097
#>         S.E. (gi - gj)line S.E. (gi - gj)tester S.E. (sij - skl)tester
#> Trait 1          1.1487985            0.8898555               1.989777
#> Trait 2          0.6902988            0.5347031               1.195633
#> Trait 3          4.7943942            3.7137218               8.304134
#> 
#> $C.D.
#>         C.D. gca for line C.D. gca for tester C.D. sca effect
#> Trait 1          1.669754           1.2933861        2.892099
#> Trait 2          1.003335           0.7771797        1.737827
#> Trait 3          6.968550           5.3978159       12.069883
#>         C.D. (gi - gj)line C.D. (gi - gj)tester C.D. (sij - skl)tester
#> Trait 1           2.361389             1.829124               4.090046
#> Trait 2           1.418929             1.099098               2.457658
#> Trait 3           9.855018             7.633664              17.069393
#> 
#> $Add.Dom.Var
#>         Cov H.S. (line) Cov H.S. (tester) Cov H.S. (average) Cov F.S. (average)
#> Trait 1       0.4608195        -0.1149399         0.03318511         -0.3379446
#> Trait 2      -0.2950635        -0.1532122        -0.03843094         -0.1627380
#> Trait 3      10.4145144        -2.3848209         0.76610579        -10.5634695
#>         Addittive Variance(F=0) Addittive Variance(F=1) Dominance Variance(F=0)
#> Trait 1               0.1327405              0.06637023               -1.170148
#> Trait 2              -0.1537238             -0.07686188                0.724117
#> Trait 3               3.0644232              1.53221158              -32.985091
#>         Dominance Variance(F=1)
#> Trait 1              -0.5850742
#> Trait 2               0.3620585
#> Trait 3             -16.4925453
#> 
#> $Contribution.of.Line.Tester
#>            Lines   Tester  Line x Tester
#> Trait 1 46.46927 6.859411       46.67132
#> Trait 2  7.66340 6.213647       86.12295
#> Trait 3 55.79441 3.435518       40.77007
```
