## Prepare plots and tables for deaths

## Before: deaths_doubling.csv, deaths_rate.csv, deaths_total.csv,
##         deaths_total_continent.csv, deaths_tseries.csv,
##         deaths_tseries.continent (data), countries.RData (output)
## After:  deaths_total.pdf, deaths_tseries.pdf (report)

library(TAF)
source("utilities.R")  # barplotCorona, plotTimeBase, plotXY

mkdir("report")

## Read data
total <- read.taf("data/deaths_total.csv")
doubling <- read.taf("data/deaths_doubling.csv")
rate <- read.taf("data/deaths_rate.csv")
tseries <- read.taf("data/deaths_tseries.csv")
total.c <- read.taf("data/deaths_total_continent.csv")
tseries.c <- read.taf("data/deaths_tseries_continent.csv")
## Country sets: africa, asia, e.europe, euro5, n.america, nordic, oceania,
## s.america, w.europe
load("output/countries.RData")
tseries.c$Continent <-
  ordered(tseries.c$Continent,
          c("N America", "Europe", "S America", "Africa", "Oceania", "Asia"))
tseries$Date <- as.Date(tseries$Date)
tseries.c$Date <- as.Date(tseries.c$Date)
total$Rate <- total$Rate / 1000      # plot per million
total.c$Rate <- total.c$Rate / 1000  # plot per million
rate$Rate <- rate$Rate / 1000        # plot per million

## World
world <- aggregate(cbind(Deaths,Daily)~Date, data=tseries, sum)
worst <- tail(rate$Country, 9)

## Europe vs US
euro5 <- tseries[tseries$Country %in% euro5,]
euro5 <- aggregate(Deaths~Date, euro5, sum)
onset.euro5 <- min(euro5$Date[euro5$Deaths>=100])
us <- tseries[tseries$Country=="US",]
onset.us <- min(us$Date[us$Deaths>=100])

## Total
total.worst <- total[total$Country %in% worst,]
total.nordic <- total[total$Country %in% nordic,]
total.w.europe <- total[total$Country %in% w.europe,]
total.e.europe <- total[total$Country %in% e.europe,]
total.n.america <- total[total$Country %in% n.america,]
total.s.america <- total[total$Country %in% s.america,]
total.asia <- total[total$Country %in% asia,]
total.africa <- total[total$Country %in% africa,]
total.oceania <- total[total$Country %in% oceania,]

## Timeline
tseries.worst <- tseries[tseries$Country %in% worst,]
tseries.nordic <- tseries[tseries$Country %in% nordic,]
tseries.w.europe <- tseries[tseries$Country %in% w.europe,]
tseries.e.europe <- tseries[tseries$Country %in% e.europe,]
tseries.n.america <- tseries[tseries$Country %in% n.america,]
tseries.s.america <- tseries[tseries$Country %in% s.america,]
tseries.asia <- tseries[tseries$Country %in% asia,]
tseries.africa <- tseries[tseries$Country %in% africa,]
tseries.oceania <- tseries[tseries$Country %in% oceania,]

## Total worst deaths
pdf("report/deaths_total.pdf")
barplotCorona(rate$Rate, names=rate$Country, col=rate$Color,
              xlab="Deaths per 1000 inhabitants")

## Total worst doubling time
barplotCorona(doubling$Doubling, names=doubling$Country, col=doubling$Color,
              xlab="Doubling time of deaths (days)")

## Total scatterplots
ylab <- "Deaths per 1000"
plotXY(total.worst,  ylab=ylab, main="Worst hit")
plotXY(total.nordic, ylab=ylab, main="Nordic countries")
plotXY(total.w.europe, ylab=ylab, main="Western Europe")
plotXY(total.e.europe, ylab=ylab, main="Eastern Europe")
plotXY(total.n.america,  ylab=ylab, main="North America")
plotXY(total.s.america,  ylab=ylab, main="South America")
plotXY(total.asia,   ylab=ylab, main="Asia")
plotXY(total.africa, ylab=ylab, main="Africa")
plotXY(total.oceania, ylab=ylab, main="Oceania")

## Total by continent
barplotCorona(total.c$Rate, names=total.c$Continent, col="orange",
              xlab="Deaths per 1000 inhabitants")
dev.off()

## Timeline trajectories
pdf("report/deaths_tseries.pdf")
split.worst <- split(tseries.worst, tseries.worst$Country)
plot(log10(Deaths)~Date, tseries.worst, xlab="Date", ylab="log10(Deaths)",
     type="n")
col <- c(palette(), "red")
for(i in seq_along(split.worst))
  lines(log10(Deaths)~Date, data=split.worst[[i]], lwd=2, col=col[i])
legend("bottomright", names(split.worst), lwd=2, col=col, bty="n", inset=0.02,
       y.intersp=1.1)

## Timeline Europe vs USA
plot(Deaths/1000~I(Date-onset.euro5), data=euro5, subset=Date>=onset.euro5,
     type="l", ylim=lim(c(euro5$Deaths, us$Deaths)/1000), col=2, lwd=3,
     main="Europe (de, uk, fr, it, sp) vs. USA",
     xlab="Days after 100 deaths", ylab="Deaths (thousands)")
lines(Deaths/1000~I(Date-onset.us), data=us, subset=Date>=onset.us, col=4,
      lwd=4, lty=3)
legend("bottomright", c("Europe","USA"), lwd=c(3,4), lty=c(1,3), col=c(2,4),
       bty="n", inset=0.04)

plot(log10(Deaths)~I(Date-onset.euro5), data=euro5, subset=Date>=onset.euro5,
     type="l", ylim=c(2, log10(1.05*max(c(euro5$deaths, us$Deaths)))), col=2,
     lwd=3, main="Europe (de, uk, fr, it, sp) vs. USA",
     xlab="Days after 100 deaths", yaxt="n")
axis(2, seq(floor(par("usr")[3]), floor(par("usr")[4])))
lines(log10(Deaths)~I(Date-onset.us), data=us, subset=Date>=onset.us, col=4,
      lwd=4, lty=3)
legend("bottomright", c("Europe","USA"), lwd=c(3,4), lty=c(1,3), col=c(2,4),
       bty="n", inset=0.04)

## Timeline daily deaths by country
oplt <- par("plt")
par(mfrow=c(3,3))
out <- lapply(split(tseries.worst, tseries.worst$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.nordic, tseries.nordic$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.w.europe, tseries.w.europe$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.e.europe, tseries.e.europe$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.n.america, tseries.n.america$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.s.america, tseries.s.america$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.asia, tseries.asia$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(tseries.africa, tseries.africa$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(4,3))
out <- lapply(split(tseries.oceania, tseries.oceania$Country),
              plotTimeBase, span=0.25)

## Timeline deaths by continent
par(mfrow=c(3,2))
out <- lapply(split(tseries.c, tseries.c$Continent), plotTimeBase, span=0.10)

## Timeline deaths worldwide
par(mfrow=c(1,1))
par(plt=oplt)
plot(log10(Deaths)~Date, world, main="Total deaths worldwide")

plot(Daily~Date, world, main="Daily deaths worldwide",
     ylab="Deaths")
lines(world$Date, fitted(loess(Daily~as.integer(Date), world, span=0.10)),
      lwd=2, col="darkgreen")
dev.off()
