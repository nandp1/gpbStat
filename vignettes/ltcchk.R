## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data(alphaltcchk)

# View the structure of dataframe. 
str(alphaltcchk)

# Conduct Line x Tester analysis
result = ltcchk(alphaltcchk, replication, line, tester, check, yield, block)

# View the output
result

## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("rcbdltcchk")

# View the structure of dataframe. 
str(rcbdltcchk)

# Conduct Line x Tester analysis
result1 = ltcchk(rcbdltcchk, replication, line, tester, check, yield)

# View the output
result1

