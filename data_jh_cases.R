## Preprocess Johns Hopkins cases, write TAF data tables

## Before: time_series_covid19_confirmed_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (bootstrap/jh)
## After:  jh_cases_current.csv, jh_cases_doubling.csv, jh_cases_rate.csv,
##         jh_cases_timeline.csv (data)

library(icesTAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
cases.global <- read.taf(file.path("bootstrap/data/jh",
                                   "time_series_covid19_confirmed_global.csv"))
lookup <- read.taf("bootstrap/data/jh/UID_ISO_FIPS_LookUp_Table.csv")

## Population
pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"

## Reshape and calculate daily statistics
timeline <- rearrange(cases.global, "Cases")
timeline$Daily <- c(timeline$Cases[1], diff(timeline$Cases))
timeline$Daily[timeline$Date == min(timeline$Date)] <-
  timeline$Cases[timeline$Date == min(timeline$Date)]

## Current
cases <- aggregate(Cases~Country, timeline, tail, 1)
cases <- cases[cases$Cases>0,]
current <- merge(pop, cases)
current <- na.omit(current)

## Rate (cases percent) and doubling time
current$Rate <- round(current$Cases / current$Population * 100, 2)
current$Doubling <- sapply(current$Country, doubling.time, column="Cases")

rate <- current[current$Population>=1e5,]
rate <- tail(rate[order(rate$Rate),], 25)

doubling <- current[current$Cases>=100,]
doubling <- doubling[order(doubling$Doubling),]
doubling <- doubling[doubling$Doubling<=doubling$Doubling[20],]

## Calculate rank and color
rate$Rank <- rate$Doubling - min(rate$Doubling) + 1
rate$Color <- rich.colors(max(rate$Rank), "blues")[rate$Rank]

doubling$Rank <- floor(log(doubling$Cases))
doubling$Rank <- doubling$Rank - min(doubling$Rank) + 1
doubling$Color <- rev(rich.colors(max(doubling$Rank), "blues"))[doubling$Rank]
doubling <- doubling[order(-doubling$Doubling,doubling$Rank),]

## Write tables
write.taf(current, "data/jh_cases_current.csv", quote=TRUE)    # all countries
write.taf(doubling, "data/jh_cases_doubling.csv", quote=TRUE)  # lowest doubling
write.taf(rate, "data/jh_cases_rate.csv", quote=TRUE)          # highest rate
write.taf(timeline, "data/jh_cases_timeline.csv", quote=TRUE)  # timeline
