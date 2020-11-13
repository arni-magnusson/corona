library(arni)  # sort.data.frame
library(lattice)  # xyplot

## 1  Fetch data

ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

## 2  Format data

timeline <- data.frame(Date=as.Date(ecdc$date, format="%d/%m/%Y"),
                       Country=ecdc$countries, Continent=ecdc$continent,
                       Population=ecdc$pop, Cases=ecdc$cases,
                       Deaths=ecdc$deaths, TwoWeeks=ecdc$Cumulative)

## 3  Order countries by maximum two-week cases per capita

mtw <- aggregate(TwoWeeks~Country+Continent+Population, timeline, max)
mtw <- sort(mtw, by="TwoWeeks")
timeline$Ordered <- ordered(timeline$Country, levels=rev(mtw$Country))
pop <- aggregate(Population~Country, timeline, max)
## mtw[mtw$Continent=="Europe" & mtw$Population>2e5,]

## 4  Sets of countries

world <- tail(mtw[mtw$Population > 2e5,], 15)$Country
europe <- c("Austria", "Belgium", "Czechia", "France", "Georgia", "Germany",
            "Hungary", "Italy", "Luxembourg", "Netherlands", "Poland",
            "Portugal", "Spain", "Switzerland", "United_Kingdom")
nordic <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")
america <- c("Argentina", "Bolivia", "Brazil", "Canada", "Chile", "Colombia",
             "Cuba", "Ecuador", "Mexico", "Panama", "Paraguay", "Peru",
             "United_States_of_America", "Uruguay", "Venezuela")
asia <- c("Bangladesh", "China", "India", "Indonesia", "Iran", "Israel",
          "Japan", "Pakistan", "Philippines", "Russia", "Singapore", "Turkey")
africa <- c("Algeria", "Cape_Verde", "Congo", "Egypt", "Eswatini", "Ethiopia",
            "Kenya", "Libya", "Morocco", "Nigeria", "South_Africa", "Tunisia")

## 5  Plot timeline

pars <- list(axis.text=list(cex=0.7), par.xlab.text=list(cex=0.7),
             par.ylab.text=list(cex=0.7))
plotTime <- function(x, main="", ...)
{
  xyplot(TwoWeeks~Date|Ordered, timeline, subset=Country %in% x, type="l",
         lwd=2, ylim=c(0, NA), as.table=TRUE, par.settings=pars,
         par.strip.text=list(cex=0.7), main=main, ...)
}

pdf("ecdc_timeline.pdf")
plotTime(world, "World", layout=c(3,5))
plotTime(europe, "Europe", layout=c(3,5))
plotTime(europe, "Europe", layout=c(3,5), scales=list(relation="free"))
plotTime(nordic, "Nordic", layout=c(2,3))
plotTime(america, "America", layout=c(3,5))
plotTime(asia, "Asia", layout=c(3,4))
plotTime(africa, "Africa", layout=c(3,4))
dev.off()

## 6  Plot current levels

current <- aggregate(TwoWeeks~Country+Continent+Population, timeline, head, 1)
current <- sort(current, by="TwoWeeks")

current.world <- tail(current[current$Population > 2e5,], 25)
current.europe <- current[current$Country %in% europe,]
current.nordic <- current[current$Country %in% nordic,]
current.america <- current[current$Country %in% america,]
current.asia <- current[current$Country %in% asia,]
current.africa <- current[current$Country %in% africa,]

barCurrent <- function(x, main="", ...)
{
  barplot(x$TwoWeeks, names=x$Country, horiz=TRUE, las=1, main=main,
          xlab="Current index", ...)
}

pdf("ecdc_current.pdf")
opar <- par(plt=c(0.34, 0.94, 0.15, 0.88))
barCurrent(current.world, main="World", col=factor(current.world$Continent))
barCurrent(current.europe, main="Europe")
barCurrent(current.nordic, main="Nordic")
barCurrent(current.america, main="America")
barCurrent(current.asia, main="Asia")
barCurrent(current.africa, main="Africa")
dev.off()
