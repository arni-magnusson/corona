## Prepare plots and tables for deaths

## Before: deaths_total.csv, deaths_doubling.csv, deaths_rate.csv,
##         deaths_timeline.csv (data), countries.RData (output)
## After:  deaths_total.pdf, deaths_timeline.pdf (report)

library(TAF)
source("utilities.R")  # plotTimeBase, plotXY

mkdir("report")

## Read data
total <- read.taf("data/deaths_total.csv")
doubling <- read.taf("data/deaths_doubling.csv")
rate <- read.taf("data/deaths_rate.csv")
timeline <- read.taf("data/deaths_timeline.csv")
## Country sets: africa, asia, e.europe, euro5, n.america, nordic, oceania,
## s.america, w.europe
load("output/countries.RData")
timeline$Date <- as.Date(timeline$Date)
total$Rate <- total$Rate / 1000  # plot per million
rate$Rate <- rate$Rate / 1000  # plot per million

## World
world <- aggregate(cbind(Deaths,Daily)~Date, data=timeline, sum)
worst <- tail(rate$Country, 9)

## Europe vs US
euro5 <- timeline[timeline$Country %in% euro5,]
euro5 <- aggregate(Deaths~Date, euro5, sum)
onset.euro5 <- min(euro5$Date[euro5$Deaths>=100])
us <- timeline[timeline$Country=="US",]
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
timeline.worst <- timeline[timeline$Country %in% worst,]
timeline.nordic <- timeline[timeline$Country %in% nordic,]
timeline.w.europe <- timeline[timeline$Country %in% w.europe,]
timeline.e.europe <- timeline[timeline$Country %in% e.europe,]
timeline.n.america <- timeline[timeline$Country %in% n.america,]
timeline.s.america <- timeline[timeline$Country %in% s.america,]
timeline.asia <- timeline[timeline$Country %in% asia,]
timeline.africa <- timeline[timeline$Country %in% africa,]
timeline.oceania <- timeline[timeline$Country %in% oceania,]

## Total worst deaths
pdf("report/deaths_total.pdf")
opar <- par(plt=c(0.30, 0.94, 0.15, 0.88))
barplot(rate$Rate, names=rate$Country, horiz=TRUE, las=1, col=NA, border=FALSE,
        xlab="Deaths per 1000 inhabitants")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(rate$Rate, horiz=TRUE, axes=FALSE, col=rate$Color, add=TRUE)
par(opar)

## Total worst doubling time
par(plt=c(0.30, 0.94, 0.15, 0.88))
barplot(doubling$Doubling, names=doubling$Country, horiz=TRUE, las=1, col=NA,
        border=FALSE, xlab="Doubling time of deaths (days)")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(doubling$Doubling, horiz=TRUE, axes=FALSE, col=doubling$Color, add=TRUE)
par(opar)

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
dev.off()

## Timeline trajectories
pdf("report/deaths_timeline.pdf")
split.worst <- split(timeline.worst, timeline.worst$Country)
plot(log10(Deaths)~Date, timeline.worst, xlab="Date", ylab="log10(Deaths)",
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
out <- lapply(split(timeline.worst, timeline.worst$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.nordic, timeline.nordic$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.w.europe, timeline.w.europe$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.e.europe, timeline.e.europe$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.n.america, timeline.n.america$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.s.america, timeline.s.america$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.asia, timeline.asia$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.africa, timeline.africa$Country),
              plotTimeBase, span=0.25)
par(mfrow=c(3,4))
out <- lapply(split(timeline.oceania, timeline.oceania$Country),
              plotTimeBase, span=0.25)

## Timeline deaths worldwide
par(mfrow=c(1,1))
par(plt=oplt)
plot(log10(Deaths)~Date, world, main="Total deaths worldwide")

plot(Daily~Date, world, main="Daily deaths worldwide",
     ylab="Deaths")
lines(world$Date, fitted(loess(Daily~as.integer(Date), world, span=0.10)),
      lwd=2, col="darkgreen")
dev.off()
