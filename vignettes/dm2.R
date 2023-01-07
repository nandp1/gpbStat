## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data(dm2rcbd)

# View the structure of dataframe. 
str(dm2rcbd)

# Conduct Line x Tester analysis
result = dm2(dm2rcbd, rep, parent1, parent2, DTP)

# View the output
result


## -----------------------------------------------------------------------------
# Load the package
library(gpbStat)

#Load the dataset
data(dm2alpha)

# View the structure of dataframe. 
str(dm2alpha)

# Conduct Diallel Analysis
result1 = dm2(dm2alpha, replication, parent1, parent2, TW, block)

# View the output
result1

