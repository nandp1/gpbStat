## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data(rcbdltcchk)

# View the structure of dataframe. 
str(rcbdltcchk)

# Conduct Line x Tester analysis
result = ltcchk(alphaltcchk, replication, line, tester, check, yield)

# View the output
result

