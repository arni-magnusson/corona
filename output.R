## Extract results of interest, write TAF output tables

## Before: *.csv (model)
## After:  *.csv (output)

library(TAF)

mkdir("output")

cp("model/*", "output")
