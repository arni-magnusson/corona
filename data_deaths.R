## Preprocess deaths, write TAF data tables

## Before: continents.csv, time_series_covid19_deaths_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (bootstrap/data)
## After:  deaths_doubling.csv, deaths_rate.csv, deaths_total.csv,
##         deaths_total_continent.csv, deaths_tseries.csv,
##         deaths_tseries_continent.csv (data)

library(TAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
deaths.global <-
  read.taf("bootstrap/data/jh/time_series_covid19_deaths_global.csv")
lookup <- read.taf("bootstrap/data/jh/UID_ISO_FIPS_LookUp_Table.csv")
continents <- read.taf("bootstrap/data/continents.csv")
continents$Continent <-
  ordered(continents$Continent,
          c("Oceania", "Asia", "Africa", "S America", "N America", "Europe"))

## Population
pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"
## Omit Summer Olympics 2020, MS Zaandam, Diamond Princess
countries <- sort(pop$Country[!is.na(pop$Population)])
stopifnot(identical(countries, continents$Country))

## Reshape and calculate daily statistics
tseries <- rearrange(deaths.global, "Deaths")
tseries <- tseries[tseries$Country %in% countries,]  # actual countries
tseries$Deaths[tseries$Country=="Iceland" & tseries$Date=="2020-03-15"] <- 0
tseries <- merge(tseries, continents)
tseries <- tseries[c("Country", "Continent", "Date", "Deaths")]
tseries$Daily <- c(tseries$Deaths[1], diff(tseries$Deaths))
tseries$Daily[tseries$Date == min(tseries$Date)] <-
  tseries$Deaths[tseries$Date == min(tseries$Date)]
tseries.c <- aggregate(cbind(Deaths,Daily)~Date+Continent, tseries, sum)
tseries.c <- tseries.c[c("Continent", "Date", "Deaths", "Daily")]

## Total
deaths <- aggregate(Deaths~Country, tseries, tail, 1)
total <- merge(pop, deaths)
total <- merge(total, continents)
total <- na.omit(total)
total.c <- aggregate(cbind(Population,Deaths)~Continent, total, sum)

## Rate (deaths per million) and doubling time
total$Rate <- round(total$Deaths / total$Population * 1e6, 1)
total$Doubling <- sapply(total$Country, doubling.time, column="Deaths")
total.c$Rate <- round(total.c$Deaths / total.c$Population * 1e6, 1)

rate <- total[total$Population>=1e5,]
rate <- tail(rate[order(rate$Rate),], 25)

doubling <- total[total$Deaths>=100,]
doubling <- total[order(total$Doubling),]
doubling <- doubling[doubling$Doubling<=doubling$Doubling[20],]

## Calculate rank and color
rate$Rank <- rate$Doubling - min(rate$Doubling) + 1
rate$Color <- rich.colors(max(rate$Rank), "blues")[rate$Rank]

doubling$Rank <- floor(log(doubling$Deaths))
doubling$Rank <- doubling$Rank - min(doubling$Rank) + 1
doubling$Color <- rev(rich.colors(max(doubling$Rank), "blues"))[doubling$Rank]
doubling <- doubling[order(-doubling$Doubling,doubling$Rank),]

## Write tables
write.taf(total, "data/deaths_total.csv", quote=TRUE)        # all countries
write.taf(doubling, "data/deaths_doubling.csv", quote=TRUE)  # lowest doubling
write.taf(rate, "data/deaths_rate.csv", quote=TRUE)          # highest rate
write.taf(tseries, "data/deaths_tseries.csv", quote=TRUE)    # tseries
write.taf(total.c, "data/deaths_total_continent.csv", quote=TRUE)
write.taf(tseries.c, "data/deaths_tseries_continent.csv", quote=TRUE)
