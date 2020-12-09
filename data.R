## Preprocess data, write TAF data tables

library(icesTAF)

mkdir("data")

sourceTAF("data_ecdc.R")
sourceTAF("data_jh_cases.R")
sourceTAF("data_jh_deaths.R")
