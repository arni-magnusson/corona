## Prepare ECDC plots and tables for report

## Before: ecdc_timeline.csv (data), ecdc_peak.csv,
##         ecdc_countries.RData (output)
## After:  ecdc_current.pdf, ecdc_timeline.pdf (report)

library(TAF)
source("utilities.R")  # barCurrent, plotTimeLattice

mkdir("report")

## Read data
timeline <- read.taf("data/ecdc_timeline.csv")
peak <- read.taf("output/ecdc_peak.csv")
load("output/ecdc_countries.RData")  # worst,europe,nordic,america,asia,africa
timeline$Date <- as.Date(timeline$Date)
timeline$Ordered <- ordered(timeline$Country, levels=rev(peak$Country))

## Get current levels
current <- aggregate(TwoWeeks~Country+Continent+Population, timeline, head, 1)
current <- current[order(current$TwoWeeks),]
current.worst <- tail(current[current$Population > 2e5,], 25)
current.europe <- current[current$Country %in% europe,]
current.nordic <- current[current$Country %in% nordic,]
current.america <- current[current$Country %in% america,]
current.asia <- current[current$Country %in% asia,]
current.africa <- current[current$Country %in% africa,]

## Plot current levels
pdf("report/ecdc_current.pdf")
opar <- par(plt=c(0.34, 0.94, 0.15, 0.88))
barCurrent(current.worst, main="World", col=factor(current.worst$Continent))
barCurrent(current.europe, main="Europe")
barCurrent(current.nordic, main="Nordic")
barCurrent(current.america, main="America")
barCurrent(current.asia, main="Asia")
barCurrent(current.africa, main="Africa")
dev.off()

## Plot timeline
pdf("report/ecdc_timeline.pdf")
plotTimeLattice(worst, "World", layout=c(3,5))
plotTimeLattice(europe, "Europe", layout=c(3,5))
plotTimeLattice(europe, "Europe", layout=c(3,5), scales=list(relation="free"))
plotTimeLattice(nordic, "Nordic", layout=c(2,3))
plotTimeLattice(america, "America", layout=c(3,5))
plotTimeLattice(asia, "Asia", layout=c(3,4))
plotTimeLattice(africa, "Africa", layout=c(3,4))
dev.off()
