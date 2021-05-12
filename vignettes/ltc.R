## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("alphaltc")

# View the structure of dataframe. 
str(alphaltc)

# Conduct Line x Tester analysis
result = ltc(alphaltc, replication, line, tester, yield, block)

# View the output
result

## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("rcbdltc")

# View the structure of dataframe. 
str(rcbdltc)

# Conduct Line x Tester analysis
result1 = ltc(rcbdltc, replication, line, tester, yield)

# View the output
result1

