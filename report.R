## Prepare plots and tables for report

library(TAF)

mkdir("report")

sourceTAF("report_ecdc.R")
sourceTAF("report_focus.R")
sourceTAF("report_jh_cases.R")
sourceTAF("report_jh_deaths.R")
sourceTAF("report_jh_week.R")
