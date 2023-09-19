
# gpbStat

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/gpbStat)](https://cran.r-project.org/package=gpbStat)
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
#> Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 60 obs. of  7 variables:
#>  $ replication: chr  "r1" "r3" "r2" "r4" ...
#>  $ block      : chr  "b2" "b2" "b4" "b5" ...
#>  $ line       : chr  "DIL 2" "DIL 2" "DIL 2" "DIL 2" ...
#>  $ tester     : chr  "DIL-101" "DIL-101" "DIL-101" "DIL-101" ...
#>  $ hsw        : num  25.7 24.5 23.7 25.1 23 ...
#>  $ sh         : num  81.7 83.3 86 84.6 85.5 ...
#>  $ gy         : num  25.9 41 65.7 47.3 30.8 ...
#>  - attr(*, "spec")=List of 3
#>   ..$ cols   :List of 7
#>   .. ..$ replication: list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ block      : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ line       : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ tester     : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ hsw        : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ sh         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ gy         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   ..$ default: list()
#>   .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
#>   ..$ delim  : chr ","
#>   ..- attr(*, "class")= chr "col_spec"
#>  - attr(*, "problems")=<externalptr>

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
#>        Tester
#> Line    DIL-101 DIL-103 DIL 102
#>   DIL-1 24.2800 26.4325 24.3900
#>   DIL-4 25.3625 26.3225 26.5250
#>   DIL 2 24.7525 23.8525 23.1800
#>   DIL 3 22.1300 25.4675 25.0975
#>   DIL 5 24.4075 22.9050 23.8625
#> 
#> $Mean$sh
#>        Tester
#> Line    DIL-101 DIL-103 DIL 102
#>   DIL-1 82.9375 84.2025 83.8700
#>   DIL-4 84.2775 81.8175 84.3250
#>   DIL 2 83.8950 83.7725 84.6225
#>   DIL 3 83.6100 83.0450 84.4600
#>   DIL 5 83.0425 84.8300 82.5875
#> 
#> $Mean$gy
#>        Tester
#> Line    DIL-101 DIL-103 DIL 102
#>   DIL-1 54.2675 44.7525 48.8625
#>   DIL-4 60.5650 53.7975 52.1400
#>   DIL 2 44.9575 47.3975 45.3125
#>   DIL 3 46.0625 55.0550 54.7700
#>   DIL 5 58.2675 53.5525 53.5300
#> 
#> 
#> $ANOVA
#> $ANOVA$hsw
#>                           Df     Sum Sq   Mean Sq   F value      Pr(>F)
#> Replication                3 123.534952 41.178317 5.2008236 0.006007676
#> Blocks within Replication 16 159.578141  9.973634 1.2596705 0.292005429
#> Crosses                   14  95.647543  6.831967 0.8628778 0.602918614
#> Lines                      4  44.421693 11.105423 1.0220298 0.406231362
#> Testers                    2   6.558103  3.279052 0.3017705 0.740992561
#> Lines X Testers            8  44.667747  5.583468 0.5138454 0.839635289
#> Error                     26 205.858982  7.917653        NA          NA
#> Total                     59 584.619618        NA        NA          NA
#> 
#> $ANOVA$sh
#>                           Df     Sum Sq    Mean Sq   F value      Pr(>F)
#> Replication                3  47.847660 15.9492200 5.5792805 0.004311049
#> Blocks within Replication 16  61.895494  3.8684684 1.3532492 0.239549969
#> Crosses                   14  39.935293  2.8525210 0.9978553 0.482967180
#> Lines                      4   3.050693  0.7626733 0.1864544 0.944255260
#> Testers                    2   2.468943  1.2344717 0.3017971 0.740973054
#> Lines X Testers            8  34.415657  4.3019571 1.0517198 0.413116072
#> Error                     26  74.324946  2.8586518        NA          NA
#> Total                     59 224.003393         NA        NA          NA
#> 
#> $ANOVA$gy
#>                           Df      Sum Sq    Mean Sq   F value       Pr(>F)
#> Replication                3  3171.01367 1057.00456 7.6631523 0.0007893935
#> Blocks within Replication 16  2338.12660  146.13291 1.0594455 0.4352040161
#> Crosses                   14  1411.65982  100.83284 0.7310257 0.7261397075
#> Lines                      4   787.60961  196.90240 0.9741847 0.4310920496
#> Testers                    2    48.49009   24.24505 0.1199536 0.8872442280
#> Lines X Testers            8   575.56012   71.94502 0.3559517 0.9380005166
#> Error                     26  3586.26808  137.93339        NA           NA
#> Total                     59 10507.06817         NA        NA           NA
#> 
#> 
#> $GCA.Line
#>              hsw          sh         gy
#> DIL-1  0.4363333 -0.01633333 -2.2585000
#> DIL-4  1.4721667 -0.21300000  3.9481667
#> DIL 2 -0.6695000  0.41033333 -5.6635000
#> DIL 3 -0.3661667  0.01866667  0.4098333
#> DIL 5 -0.8728333 -0.19966667  3.5640000
#> 
#> $GCA.Tester
#>                 hsw         sh         gy
#> DIL-101 -0.41133333 -0.1338333  1.2713333
#> DIL-103  0.39816667 -0.1528333 -0.6416667
#> DIL 102  0.01316667  0.2866667 -0.6296667
#> 
#> $SCA
#> $SCA$hsw
#>        Tester
#> Line       DIL-101    DIL-103    DIL 102
#>   DIL-1 -0.3428333  1.0001667 -0.6573333
#>   DIL-4 -0.2961667 -0.1456667  0.4418333
#>   DIL 2  1.2355000 -0.4740000 -0.7615000
#>   DIL 3 -1.6903333  0.8376667  0.8526667
#>   DIL 5  1.0938333 -1.2181667  0.1243333
#> 
#> $SCA$sh
#>        Tester
#> Line        DIL-101    DIL-103     DIL 102
#>   DIL-1 -0.59866667  0.6853333 -0.08666667
#>   DIL-4  0.93800000 -1.5030000  0.56500000
#>   DIL 2 -0.06783333 -0.1713333  0.23916667
#>   DIL 3  0.03883333 -0.5071667  0.46833333
#>   DIL 5 -0.31033333  1.4961667 -1.18583333
#> 
#> $SCA$gy
#>        Tester
#> Line      DIL-101   DIL-103   DIL 102
#>   DIL-1  3.702000 -3.900000  0.198000
#>   DIL-4  3.792833 -1.061667 -2.731167
#>   DIL 2 -2.203000  2.150000  0.053000
#>   DIL 3 -7.171333  3.734167  3.437167
#>   DIL 5  1.879500 -0.922500 -0.957000
#> 
#> 
#> $CV
#>       hsw        sh        gy 
#> 11.439351  2.020348 22.781566 
#> 
#> $Genetic.Variance.Covariance.
#>     Phenotypic Variance Genotypic Variance Environmental Variance
#> hsw          -0.6689343          -8.586587               7.917653
#> sh           -0.4155230          -3.274175               2.858652
#> gy         -101.1095400        -239.042928             137.933388
#>     Phenotypic coefficient of Variation Genotypic coefficient of Variation
#> hsw                                 NaN                                NaN
#> sh                                  NaN                                NaN
#> gy                                  NaN                                NaN
#>     Environmental coefficient of Variation Broad sense heritability
#> hsw                              11.439351                12.836220
#> sh                                2.020348                 7.879648
#> gy                               22.781566                 2.364198
#> 
#> $Std.Error
#>     S.E. gca for line S.E. gca for tester S.E. sca effect S.E. (gi - gj)line
#> hsw         0.8122835           0.6291921       1.4069162          1.1487423
#> sh          0.4880789           0.3780643       0.8453774          0.6902478
#> gy          3.3903464           2.6261511       5.8722523          4.7946739
#>     S.E. (gi - gj)tester S.E. (sij - skl)tester
#> hsw            0.8898120               1.989680
#> sh             0.5346636               1.195544
#> gy             3.7139384               8.304619
#> 
#> $C.D.
#>     C.D. gca for line C.D. gca for tester C.D. sca effect C.D. (gi - gj)line
#> hsw          1.669673           1.2933228        2.891958           2.361274
#> sh           1.003260           0.7771222        1.737698           1.418825
#> gy           6.968957           5.3981308       12.070587           9.855593
#>     C.D. (gi - gj)tester C.D. (sij - skl)tester
#> hsw             1.829035               4.089846
#> sh              1.099017               2.457476
#> gy              7.634110              17.070388
#> 
#> $Add.Dom.Var
#>     Cov H.S. (line) Cov H.S. (tester) Cov H.S. (average) Cov F.S. (average)
#> hsw       0.4601629        -0.1152208         0.03310414         -0.3374874
#> sh       -0.2949403        -0.1533743        -0.03843202         -0.1641164
#> gy       10.4131155        -2.3849984         0.76596517        -10.5696184
#>     Addittive Variance(F=0) Addittive Variance(F=1) Dominance Variance(F=0)
#> hsw               0.1324166              0.06620828              -1.1670924
#> sh               -0.1537281             -0.07686404               0.7216527
#> gy                3.0638607              1.53193033             -32.9941861
#>     Dominance Variance(F=1)
#> hsw              -0.5835462
#> sh                0.3608263
#> gy              -16.4970931
#> 
#> $Contribution.of.Line.Tester
#>         Lines   Tester  Line x Tester
#> hsw 46.443110 6.856531       46.70036
#> sh   7.639091 6.182359       86.17855
#> gy  55.793159 3.434970       40.77187
```
