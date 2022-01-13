## Run analysis, write model results

library(TAF)

mkdir("model")

sourceTAF("model_ecdc.R")
sourceTAF("model_jh.R")
sourceTAF("model_jh_week.R")
