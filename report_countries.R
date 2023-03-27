## Prepare plots and tables for countries

## Before: deaths_tseries.csv (data)
## After:  countries.png (report)

library(TAF)

tseries <- read.taf("data/deaths_tseries.csv")
tseries$Date <- as.Date(tseries$Date)

plotCountry <- function(country)
  plot(Deaths~Date, data=tseries, subset=Country==country, main=country)

taf.png("countries")
par(mfrow=c(3,3))
plotCountry("Iceland")
plotCountry("France")
plotCountry("Denmark")
plotCountry("US")
plotCountry("Italy")
plotCountry("Germany")
plotCountry("Chile")
plotCountry("Peru")
plotCountry("China")
dev.off()
