## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("alphaltcs")

# Conduct Line x Tester analysis on single plant basis
result = ltcs(alphaltcs, replication, line, tester, obs, yield, block)

# View the output
result

## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("rcbdltcs")

# Conduct Line x Tester analysis on single plant basis
result1 = ltcs(rcbdltcs, replication, line, tester, obs, yield)

# View the output
result1

