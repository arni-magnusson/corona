## Preprocess data, write TAF data tables

library(TAF)

mkdir("data")

sourceTAF("data_jh_cases.R")
sourceTAF("data_jh_deaths.R")
