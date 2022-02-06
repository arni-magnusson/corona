## Prepare plots and tables for week

## Before: deaths_tseries.csv (data), week.csv, week_continent.csv,
##         week_full.csv (output)
## After:  deaths_week.pdf (report)

library(TAF)
source("utilities.R")  # barplotCorona

mkdir("report")

## Read data
tseries <- read.taf("data/deaths_tseries.csv")
tseries$Date <- as.Date(tseries$Date)
week <- read.taf("output/week.csv")
week.full <- read.taf("output/week_full.csv")
week.c <- read.taf("output/week_continent.csv")

## Prepare label
dates <- sort(unique(tseries$Date[tseries$Date>max(tseries$Date)-7]))
main.week <- paste0("Last week ", "(",
                    paste(range(dates), collapse=" to "), ")")

## Death rate last week
pdf("report/deaths_week.pdf")
barplotCorona(week$WeekRate, names=week$Country, col="brown", main=main.week,
              xlab="Deaths per million inhabitants")

## Death rate last week and before
barplotCorona(t(week[c("WeekRate","PrevRate")]), names=week$Country,
              col=c("brown","gray95"), main=main.week,
              xlab="Deaths per million inhabitants")

## Deaths last week
w <- week.full[c("Country","WeekDeaths")]
cutoff <- sort(w$WeekDeaths, decreasing=TRUE)[10]
w$Country[w$WeekDeaths < cutoff] <- "Other"
w <- aggregate(WeekDeaths~Country, w, sum)

w <- w[order(w$WeekDeaths),]
barplotCorona(w$WeekDeaths[w$Country!="Other"]/7,
              names=w$Country[w$Country!="Other"], col="orange", main=main.week,
              xlab="Average daily deaths")

## By continent
barplotCorona(week.c$WeekRate, names=week.c$Continent, col="brown", main=main.week,
              xlab="Deaths per million inhabitants")
barplotCorona(t(week.c[c("WeekRate","PrevRate")]), names=week.c$Continent,
              col=c("brown","gray95"), main=main.week,
              xlab="Deaths per million inhabitants")
barplotCorona(week.c$WeekDeaths/7, names=week.c$Continent, col="orange",
              main=main.week, xlab="Average daily deaths")

dev.off()
