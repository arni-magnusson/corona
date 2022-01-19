## Preprocess deaths, write TAF data tables

## Before: time_series_covid19_deaths_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (bootstrap/data)
## After:  deaths_current.csv, deaths_doubling.csv, deaths_rate.csv,
##         deaths_timeline.csv (data)

library(TAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
deaths.global <-
  read.taf("bootstrap/data/time_series_covid19_deaths_global.csv")
lookup <- read.taf("bootstrap/data/UID_ISO_FIPS_LookUp_Table.csv")

## Population
pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"
## Omit Summer Olympics 2020, MS Zaandam, Diamond Princess
countries <- pop$Country[!is.na(pop$Population)]

## Reshape and calculate daily statistics
timeline <- rearrange(deaths.global, "Deaths")
timeline$Deaths[timeline$Country=="Iceland" & timeline$Date=="2020-03-15"] <- 0
timeline$Daily <- c(timeline$Deaths[1], diff(timeline$Deaths))
timeline$Daily[timeline$Date == min(timeline$Date)] <-
  timeline$Deaths[timeline$Date == min(timeline$Date)]
timeline <- timeline[timeline$Country %in% countries,]  # actual countries

## Current
deaths <- aggregate(Deaths~Country, timeline, tail, 1)
deaths <- deaths[deaths$Deaths>0,]
current <- merge(pop, deaths)
current <- na.omit(current)

## Rate (deaths per million) and doubling time
current$Rate <- round(current$Deaths / current$Population * 1e6, 1)
current$Doubling <- sapply(current$Country, doubling.time, column="Deaths")

rate <- current[current$Population>=1e5,]
rate <- tail(rate[order(rate$Rate),], 25)

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
write.taf(current, "data/deaths_current.csv", quote=TRUE)   # all countries
write.taf(doubling, "data/deaths_doubling.csv", quote=TRUE) # lowest doubling
write.taf(rate, "data/deaths_rate.csv", quote=TRUE)         # highest rate
write.taf(timeline, "data/deaths_timeline.csv", quote=TRUE) # timeline
