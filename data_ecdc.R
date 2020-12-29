## Preprocess ECDC data, write TAF data tables

## Before: data.csv (bootstrap/ecdc)
## After:  ecdc_population.csv, ecdc_timeline.csv (data)

library(icesTAF)

mkdir("data")

## Read data
ecdc <- read.taf("bootstrap/data/ecdc/data.csv")

## Format data
ecdc.timeline <- data.frame(Date=as.Date(ecdc$date, format="%d/%m/%Y"),
                            Country=ecdc$countries, Continent=ecdc$continent,
                            Population=ecdc$pop, Cases=ecdc$cases,
                            Deaths=ecdc$deaths,
                            TwoWeeks=ecdc$"notification_rate_per_100000_population_14-days")

## Write tables
write.taf(ecdc.timeline, dir="data", quote=TRUE)
