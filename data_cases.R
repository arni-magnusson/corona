## Preprocess cases, write TAF data tables

## Before: continents.csv, time_series_covid19_confirmed_global.csv,
##         UID_ISO_FIPS_LookUp_Table.csv (boot/data)
## After:  cases_doubling.csv, cases_rate.csv, cases_total.csv,
##         cases_total_continent.csv, cases_tseries.csv,
##         cases_tseries_continent.csv (data)

library(TAF)
suppressPackageStartupMessages(library(gplots))  # rich.colors
source("utilities.R")                            # doubling.time, rearrange

mkdir("data")

## Read data
cases.global <-
  read.taf("boot/data/jh/time_series_covid19_confirmed_global.csv")
lookup <- read.taf("boot/data/jh/UID_ISO_FIPS_LookUp_Table.csv")
continents <- read.taf("boot/data/continents.csv")
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
tseries <- rearrange(cases.global, "Cases")
tseries <- tseries[tseries$Country %in% countries,]  # actual countries
tseries <- merge(tseries, continents)
tseries <- tseries[c("Country", "Continent", "Date", "Cases")]
tseries$Daily <- c(tseries$Cases[1], diff(tseries$Cases))
tseries$Daily[tseries$Date == min(tseries$Date)] <-
  tseries$Cases[tseries$Date == min(tseries$Date)]
tseries.c <- aggregate(cbind(Cases,Daily)~Date+Continent, tseries, sum)
tseries.c <- tseries.c[c("Continent", "Date", "Cases", "Daily")]


## Total
cases <- aggregate(Cases~Country, tseries, tail, 1)
total <- merge(pop, cases)
total <- merge(total, continents)
total <- na.omit(total)
total.c <- aggregate(cbind(Population,Cases)~Continent, total, sum)

## Rate (cases percent) and doubling time
total$Rate <- round(total$Cases / total$Population * 100, 2)
total$Doubling <- sapply(total$Country, doubling.time, column="Cases")
total.c$Rate <- round(total.c$Cases / total.c$Population * 100, 2)

rate <- total[total$Population>=1e5,]
rate <- tail(rate[order(rate$Rate),], 25)

doubling <- total[total$Cases>=100,]
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
write.taf(total, "data/cases_total.csv", quote=TRUE)        # all countries
write.taf(doubling, "data/cases_doubling.csv", quote=TRUE)  # lowest doubling
write.taf(rate, "data/cases_rate.csv", quote=TRUE)          # highest rate
write.taf(tseries, "data/cases_tseries.csv", quote=TRUE)    # tseries
write.taf(total.c, "data/cases_total_continent.csv", quote=TRUE)
write.taf(tseries.c, "data/cases_tseries_continent.csv", quote=TRUE)
