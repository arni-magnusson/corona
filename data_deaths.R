## Preprocess deaths, write TAF data tables

## Before: continents.csv, time_series_covid19_deaths_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (bootstrap/data)
## After:  deaths_current.csv, deaths_current_continent.csv,
##         deaths_doubling.csv, deaths_rate.csv, deaths_timeline.csv,
##         deaths_timeline_continent.csv (data)

library(TAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
deaths.global <-
  read.taf("bootstrap/data/time_series_covid19_deaths_global.csv")
lookup <- read.taf("bootstrap/data/UID_ISO_FIPS_LookUp_Table.csv")
continents <- read.taf("bootstrap/data/continents.csv")

## Population
pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"
## Omit Summer Olympics 2020, MS Zaandam, Diamond Princess
countries <- sort(pop$Country[!is.na(pop$Population)])
stopifnot(identical(countries, continents$Country))

## Reshape and calculate daily statistics
timeline <- rearrange(deaths.global, "Deaths")
timeline <- timeline[timeline$Country %in% countries,]  # actual countries
timeline$Deaths[timeline$Country=="Iceland" & timeline$Date=="2020-03-15"] <- 0
timeline <- merge(timeline, continents)
timeline <- timeline[c("Country", "Continent", "Date", "Deaths")]
timeline$Daily <- c(timeline$Deaths[1], diff(timeline$Deaths))
timeline$Daily[timeline$Date == min(timeline$Date)] <-
  timeline$Deaths[timeline$Date == min(timeline$Date)]
timeline.c <- aggregate(cbind(Deaths,Daily)~Date+Continent, timeline, sum)
timeline.c <- timeline.c[c("Continent", "Date", "Deaths", "Daily")]

## Current
deaths <- aggregate(Deaths~Country, timeline, tail, 1)
current <- merge(pop, deaths)
current <- merge(current, continents)
current <- na.omit(current)
current.c <- aggregate(cbind(Population,Deaths)~Continent, current, sum)

## Rate (deaths per million) and doubling time
current$Rate <- round(current$Deaths / current$Population * 1e6, 1)
current$Doubling <- sapply(current$Country, doubling.time, column="Deaths")
current.c$Rate <- round(current.c$Deaths / current.c$Population * 1e6, 1)

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
write.taf(current, "data/deaths_current.csv", quote=TRUE)    # all countries
write.taf(doubling, "data/deaths_doubling.csv", quote=TRUE)  # lowest doubling
write.taf(rate, "data/deaths_rate.csv", quote=TRUE)          # highest rate
write.taf(timeline, "data/deaths_timeline.csv", quote=TRUE)  # timeline
write.taf(current.c, "data/deaths_current_continent.csv", quote=TRUE)
write.taf(timeline.c, "data/deaths_timeline_continent.csv", quote=TRUE)
