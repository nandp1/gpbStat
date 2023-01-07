## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("alphaltcmt")

# View the structure of dataframe. 
str(alphaltcmt)

# Conduct Line x Tester analysis
result  = ltcmt(alphaltcmt, replication, line, tester, alphaltcmt[,5:7], block)

# View the output
result

## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data("rcbdltcmt")

# View the structure of dataframe. 
str(rcbdltc)

# Conduct Line x Tester analysis
result1 = ltcmt(rcbdltcmt, replication, line, tester, rcbdltcmt[,4:5])

# View the output
result1

