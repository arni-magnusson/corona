## Extract results of interest, write TAF output tables

## Before: countries.RData, week.csv, week_continent.csv, week_full.csv (model)
## After:  countries.RData, week.csv, week_continent.csv, week_full.csv (output)

library(TAF)

mkdir("output")

cp("model/*", "output")
