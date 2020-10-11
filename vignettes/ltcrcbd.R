## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gpbStat)

## Loading inbuilt dataset
data("rcbdlt")

## Now by using function ltcalpha we analyze the data.
results2 = ltcrcbd(rcbdlt, replication, line, tester, yield)

## Viewing the results
results2

