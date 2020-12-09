## Preprocess Johns Hopkins deaths, write TAF data tables

## Before: time_series_covid19_deaths_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (bootstrap/jh)
## After:  current.csv, doubling.csv, rate.csv, timeline.csv (data)

library(icesTAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
deaths.global <- read.taf(file.path("bootstrap/data/jh",
                                    "time_series_covid19_deaths_global.csv"))
lookup <- read.taf("bootstrap/data/jh/UID_ISO_FIPS_LookUp_Table.csv")

## Population
pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"

## Reshape and calculate daily statistics
timeline <- rearrange(deaths.global, "Deaths")
timeline$Deaths[timeline$Country=="Iceland" & timeline$Date=="2020-03-15"] <- 0
timeline$Daily <- c(timeline$Deaths[1], diff(timeline$Deaths))
timeline$Daily[timeline$Date == min(timeline$Date)] <-
  timeline$Deaths[timeline$Date == min(timeline$Date)]

## Current
deaths <- aggregate(Deaths~Country, timeline, tail, 1)
deaths <- deaths[deaths$Deaths>0,]
current <- merge(pop, deaths)
current <- na.omit(current)

## Rate (deaths per million) and doubling time
current$Rate <- round(current$Deaths / current$Population * 1e6, 1)
current$Doubling <- sapply(current$Country, doubling.time, column="Deaths")

rate <- current[current$Population>=1e5,]
rate <- tail(current[order(current$Rate),], 25)

doubling <- current[current$Deaths>=100,]
doubling <- current[order(current$Doubling),]
doubling <- doubling[doubling$Doubling<=doubling$Doubling[20],]

## Calculate rank and color
rate$Rank <- rate$Doubling - min(rate$Doubling) + 1
rate$Color <- rich.colors(max(rate$Rank), "blues")[rate$Rank]

doubling$Rank <- floor(log(doubling$Deaths))
doubling$Rank <- doubling$Rank - min(doubling$Rank) + 1
doubling$Color <- rev(rich.colors(max(doubling$Rank), "blues"))[doubling$Rank]
doubling <- doubling[order(-doubling$Doubling,doubling$Rank),]

## Write tables
write.taf(current, "data/jh_deaths_current.csv", quote=TRUE)   # all countries
write.taf(doubling, "data/jh_deaths_doubling.csv", quote=TRUE) # lowest doubling
write.taf(rate, "data/jh_deaths_rate.csv", quote=TRUE)         # highest rate
write.taf(timeline, "data/jh_deaths_timeline.csv", quote=TRUE) # timeline
