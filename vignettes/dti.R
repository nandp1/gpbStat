## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gpbStat)
data(datdti)
result1 =  dti(datdti, environment = ENV, genotype = GEN, datdti[,3:8],
               ns = 'NS-DWR', st = 'ST-DWR')
result1

data(datrdti)
result2 = dti(datrdti, environment = ENV, genotype = GEN, datrdti[,4:9],
             ns = 'NS-DWR', st = 'ST-DWR')
result2

