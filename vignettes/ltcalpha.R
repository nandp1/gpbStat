## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gpbStat)

## Loading inbuilt dataset
data("alphalt")

## Now by using function ltcalpha we analyze the data.
results1 = ltcalpha(alphalt, replication, block, line, tester, yield)

## Viewing the results
results1

