## Run analysis of last week, write model results

## Before: deaths_total.csv, deaths_timeline.csv (data)
## After:  week.csv, week_full.csv (model)

library(TAF)

mkdir("model")

## Read data
total <- read.taf("data/deaths_total.csv")
timeline <- read.taf("data/deaths_timeline.csv")
timeline$Date <- as.Date(timeline$Date)

## Find last seven days
dates <- sort(unique(timeline$Date[timeline$Date>max(timeline$Date)-7]))

## Calculate last week's deaths and death rate, as well as previous deaths
week <- aggregate(Daily~Country, timeline, sum, subset=Date%in%dates)
names(week) <- c("Country", "WeekDeaths")
week <- merge(total, week)
week$WeekRate <- round(week$WeekDeaths / week$Population * 1e6, 1)
week$PrevDeaths <- week$Deaths - week$WeekDeaths
week$PrevRate <- week$Rate - week$WeekRate
week.full <- week

## Select countries
week <- week[week$Population>=1e5,]
week <- tail(week[order(week$WeekRate),], 25)

## Write tables
write.taf(week, "model/week.csv", quote=TRUE)
write.taf(week.full, "model/week_full.csv", quote=TRUE)
