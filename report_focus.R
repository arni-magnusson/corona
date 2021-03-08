## Prepare ECDC plots and tables for report

## Before: ecdc_timeline.csv (data), ecdc_peak.csv,
##         ecdc_countries.RData (output)
## After:  ecdc_current.pdf, ecdc_timeline.pdf (report)

library(icesTAF)
source("utilities.R")  # plotFocus

mkdir("report")

## Read data
timeline <- read.taf("data/ecdc_timeline.csv")
peak <- read.taf("output/ecdc_peak.csv")
load("output/ecdc_countries.RData")  # worst,europe,nordic,america,asia,africa
timeline$Date <- as.Date(timeline$Date)
timeline$Ordered <- ordered(timeline$Country, levels=rev(peak$Country))

## Get current levels
current <- aggregate(TwoWeeks~Country+Continent+Population, timeline, head, 1)


focus <- c("Chile", "Denmark", "Georgia", "Iceland", "Italy",
           "United_States_of_America")

ecdc <- read.taf("data/ecdc_timeline.csv")
ecdc$Date <- as.Date(ecdc$Date)

plotFocus <- function(country, what, data=ecdc)
{
  data <- data[data$Country==country,]
  ylim <- lim(data[data$Country==country,what])
  z <- barplot(data[[what]], names=data$Date, col=NA, border=FALSE,
          ylim=ylim, main=country, axisnames=FALSE)
  ## barplot(Cases~Date, data=data, subset=Country==country, main=country, col=NA,
          ## border=FALSE, ylim=lim(ecdc$Cases[ecdc$Country==country]))
  grid(nx=NA, ny=NULL, lty=1, lwd=1)
  ## barplot(Cases~Date, data=data, subset=Country==country, add=TRUE)
  box()
  z
}

par(mfrow=c(2,3))
z <- sapply(focus, plotFocus, what="Cases")

par(mfrow=c(1,1))
x <- ecdc$Date[ecdc$Country=="Italy"]
y <- ecdc$Cases[ecdc$Country=="Italy"]
z <- plotFocus("Italy", "Cases")
points(rev(z),y)

Args(axis.Date)
axis(side=1, at=z[x %in% pretty(x)], labels=format(pretty(x), "%b\n%Y"))



z <- plotFocus("Italy", "Cases")
points(rev(z),y)
