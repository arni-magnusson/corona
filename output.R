## Extract results of interest, write TAF output tables

## Before: *.csv (model)
## After:  *.csv (output)

library(icesTAF)

mkdir("output")

cp("model/*", "output")
