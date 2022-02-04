## Preprocess cases, write TAF data tables

## Before: continents.csv, time_series_covid19_confirmed_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (bootstrap/data)
## After:  cases_current.csv, cases_current_continent.csv,
##         cases_doubling.csv, cases_rate.csv, cases_timeline.csv,
##         cases_timeline_continent.csv (data)

library(TAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
cases.global <-
  read.taf("bootstrap/data/time_series_covid19_confirmed_global.csv")
lookup <- read.taf("bootstrap/data/UID_ISO_FIPS_LookUp_Table.csv")
continents <- read.taf("bootstrap/data/continents.csv")

## Population
pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"
## Omit Summer Olympics 2020, MS Zaandam, Diamond Princess
countries <- sort(pop$Country[!is.na(pop$Population)])
stopifnot(identical(countries, continents$Country))

## Reshape and calculate daily statistics
timeline <- rearrange(cases.global, "Cases")
timeline <- timeline[timeline$Country %in% countries,]  # actual countries
timeline <- merge(timeline, continents)
timeline <- timeline[c("Country", "Continent", "Date", "Cases")]
timeline$Daily <- c(timeline$Cases[1], diff(timeline$Cases))
timeline$Daily[timeline$Date == min(timeline$Date)] <-
  timeline$Cases[timeline$Date == min(timeline$Date)]
timeline.c <- aggregate(cbind(Cases,Daily)~Date+Continent, timeline, sum)
timeline.c <- timeline.c[c("Continent", "Date", "Cases", "Daily")]


## Current
cases <- aggregate(Cases~Country, timeline, tail, 1)
current <- merge(pop, cases)
current <- merge(current, continents)
current <- na.omit(current)
current.c <- aggregate(cbind(Population,Cases)~Continent, current, sum)

## Rate (cases percent) and doubling time
current$Rate <- round(current$Cases / current$Population * 100, 2)
current$Doubling <- sapply(current$Country, doubling.time, column="Cases")
current.c$Rate <- round(current.c$Cases / current.c$Population * 100, 2)

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
write.taf(current, "data/cases_current.csv", quote=TRUE)    # all countries
write.taf(doubling, "data/cases_doubling.csv", quote=TRUE)  # lowest doubling
write.taf(rate, "data/cases_rate.csv", quote=TRUE)          # highest rate
write.taf(timeline, "data/cases_timeline.csv", quote=TRUE)  # timeline
write.taf(current.c, "data/cases_current_continent.csv", quote=TRUE)
write.taf(timeline.c, "data/cases_timeline_continent.csv", quote=TRUE)
