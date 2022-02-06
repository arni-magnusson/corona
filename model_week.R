## Run analysis of last week, write model results

## Before: deaths_total.csv, deaths_tseries.csv (data)
## After:  week.csv, week_continent.csv, week_full.csv (model)

library(TAF)

mkdir("model")

## Read data
total <- read.taf("data/deaths_total.csv")
tseries <- read.taf("data/deaths_tseries.csv")
total.c <- read.taf("data/deaths_total_continent.csv")
tseries.c <- read.taf("data/deaths_tseries_continent.csv")
tseries.c$Continent <- ordered(tseries.c$Continent, total.c$Continent)
tseries$Date <- as.Date(tseries$Date)
tseries.c$Date <- as.Date(tseries.c$Date)

## Find last seven days
dates <- sort(unique(tseries$Date[tseries$Date>max(tseries$Date)-7]))

## Calculate last week's deaths and death rate, as well as previous deaths
week <- aggregate(Daily~Country, tseries, sum, subset=Date%in%dates)
names(week) <- c("Country", "WeekDeaths")
week <- merge(total, week)
week$WeekRate <- round(week$WeekDeaths / week$Population * 1e6, 1)
week$PrevDeaths <- week$Deaths - week$WeekDeaths
week$PrevRate <- week$Rate - week$WeekRate
week.full <- week

## Select countries
week <- week[week$Population>=1e5,]
week <- tail(week[order(week$WeekRate),], 25)

## By continent
week.c <- aggregate(Daily~Continent, tseries.c, sum, subset=Date%in%dates)
names(week.c) <- c("Continent", "WeekDeaths")
week.c <- merge(total.c, week.c, sort=FALSE)
week.c$WeekRate <- round(week.c$WeekDeaths / week.c$Population * 1e6, 1)
week.c$PrevDeaths <- week.c$Deaths - week.c$WeekDeaths
week.c$PrevRate <- week.c$Rate - week.c$WeekRate

## Write tables
write.taf(week, "model/week.csv", quote=TRUE)
write.taf(week.full, "model/week_full.csv", quote=TRUE)
write.taf(week.c, "model/week_continent.csv")
