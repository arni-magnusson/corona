source("script_both.R")

## 1  Fetch data

deaths.global <- read.csv(file.path(ts,"time_series_covid19_deaths_global.csv"),
                          check.names=FALSE)

## 2  Reshape

timeline <- rearrange(deaths.global, "Deaths")

## Correction
timeline$Deaths[timeline$Country=="Iceland" & timeline$Date=="2020-03-15"] <- 0

## 3  Calculate daily statistics

timeline$Daily <- c(timeline$Deaths[1], diff(timeline$Deaths))
timeline$Daily[timeline$Date == min(timeline$Date)] <-
  timeline$Deaths[timeline$Date == min(timeline$Date)]

## 4  Current

deaths <- aggregate(Deaths~Country, timeline, tail, 1)
deaths <- deaths[deaths$Deaths>0,]

current <- merge(pop, deaths)
current <- na.clean(current)
row.names(current) <- NULL

## 5  Rate (deaths per million) and doubling time

current$Rate <- round(current$Deaths / current$Population * 1e6, 1)
current$Doubling <- sapply(current$Country, doubling.time, column="Deaths")

rate <- tail(sort(current[current$Population>=1e5,], by="Rate"), 25)
row.names(rate) <- NULL

doubling <- sort(current[current$Deaths>=100,], by="Doubling")
doubling <- doubling[doubling$Doubling<=doubling$Doubling[20],]
row.names(doubling) <- NULL

## Calculate rank and color
rate$Rank <- rate$Doubling - min(rate$Doubling) + 1
rate$Color <- rich.colors(max(rate$Rank), "blues")[rate$Rank]

doubling$Rank <- floor(log(doubling$Deaths))
doubling$Rank <- doubling$Rank - min(doubling$Rank) + 1
doubling$Color <- rev(rich.colors(max(doubling$Rank), "blues"))[doubling$Rank]

by <- c(-1,1) * match(c("Doubling","Rank"), names(doubling))
doubling <- sort(doubling, by=by)

## 6  Sets of countries

world <- aggregate(cbind(Deaths,Daily)~Date, data=timeline, sum)

euro5 <- timeline[timeline$Country %in% euro5,]
euro5 <- aggregate(Deaths~Date, euro5, sum)
onset.euro5 <- min(euro5$Date[euro5$Deaths>=100])
us <- timeline[timeline$Country=="US",]
onset.us <- min(us$Date[us$Deaths>=100])

worst <- tail(rate$Country, 9)
current.worst <- current[current$Country %in% worst,]
timeline.worst <- timeline[timeline$Country %in% current.worst$Country,]

current.nordic <- current[current$Country %in% nordic,]
timeline.nordic <- timeline[timeline$Country %in% nordic,]

current.latin <- current[current$Country %in% latin,]
timeline.latin <- timeline[timeline$Country %in% latin,]

current.europe <- current[current$Country %in% europe,]
timeline.europe <- timeline[timeline$Country %in% europe,]

current.asia <- current[current$Country %in% asia,]
timeline.asia <- timeline[timeline$Country %in% asia,]

current.africa <- current[current$Country %in% africa,]
timeline.africa <- timeline[timeline$Country %in% africa,]

## 7  Plot current

pdf("deaths_current.pdf")

## Worst deaths
opar <- par(plt=c(0.30, 0.94, 0.15, 0.88))
barplot(rate$Rate, names=rate$Country, horiz=TRUE, las=1, col=NA, border=FALSE,
        xlab="Deaths per million inhabitants")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(rate$Rate, horiz=TRUE, axes=FALSE, col=rate$Color, add=TRUE)
par(opar)

## Worst doubling time
par(plt=c(0.30, 0.94, 0.15, 0.88))
barplot(doubling$Doubling, names=doubling$Country, horiz=TRUE, las=1, col=NA,
        border=FALSE, xlab="Doubling time of deaths (days)")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(doubling$Doubling, horiz=TRUE, axes=FALSE, col=doubling$Color, add=TRUE)
par(opar)

## Scatterplots
ylab <- "Deaths per million"
plotXY(current.worst,  ylab=ylab, main="Worst hit")
plotXY(current.nordic, ylab=ylab, main="Nordic countries")
plotXY(current.latin,  ylab=ylab, main="Latin America")
plotXY(current.europe, ylab=ylab, main="Europe")
plotXY(current.asia,   ylab=ylab, main="Asia")
plotXY(current.africa, ylab=ylab, main="Africa")
dev.off()

## 8  Plot timeline

pdf("deaths_timeline.pdf")

## Trajectories
split.worst <- split(timeline.worst, timeline.worst$Country)
plot(NA, xaxt="n", xlab="Date", ylab="log10(Deaths)",
     xlim=range(timeline.worst$Date), ylim=lim(log10(timeline.worst$Deaths)))
axt <- pretty(timeline.worst$Date)
axis(1, axt, format(axt, "1 %b"))
col <- c(palette(), "red")
for(i in seq_along(split.worst))
  lines(log10(Deaths)~Date, data=split.worst[[i]], lwd=2, col=col[i])
legend("bottomright", names(split.worst), lwd=2, col=col, bty="n", inset=0.02,
       y.intersp=1.1)

## Europe vs USA
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

## Daily deaths by country
oplt <- par("plt")
par(mfrow=c(3,3))
out <- lapply(split(timeline.worst, timeline.worst$Country), plotTimeline,
              span=0.30)
par(mfrow=c(3,3))
out <- lapply(split(timeline.nordic, timeline.nordic$Country), plotTimeline,
              span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.latin, timeline.latin$Country), plotTimeline,
              span=0.35)
par(mfrow=c(3,3))
out <- lapply(split(timeline.europe, timeline.europe$Country), plotTimeline,
              span=0.35)
par(mfrow=c(3,3))
out <- lapply(split(timeline.asia, timeline.asia$Country), plotTimeline,
              span=0.30)
par(mfrow=c(3,3))
out <- lapply(split(timeline.africa, timeline.africa$Country), plotTimeline,
              span=0.30)

## Deaths worldwide
par(mfrow=c(1,1))
par(plt=oplt)
plot(log10(Deaths)~Date, world, main="Total deaths worldwide")

plot(Daily~Date, world, main="Daily deaths worldwide", ylab="Deaths")
lines(world$Date, fitted(loess(Daily~as.integer(Date), world, span=0.30)),
      lwd=2, col="darkgreen")
dev.off()

## 9  Last week

pdf("deaths_week.pdf")
opar <- par(plt=c(0.34, 0.94, 0.15, 0.88))
dates <- sort(unique(timeline$Date[timeline$Date>max(timeline$Date)-7]))

week <- aggregate(Daily~Country, timeline, sum, subset=Date%in%dates)
names(week) <- c("Country", "WeekDeaths")
week <- merge(current, week)
week$WeekRate <- round(week$WeekDeaths / week$Population * 1e6, 1)
week$PrevDeaths <- week$Deaths - week$WeekDeaths
week$PrevRate <- week$Rate - week$WeekRate
week.all <- week
week <- tail(sort(week[week$Population>=1e5,], by="WeekRate"), 25)
row.names(week) <- NULL

main.week <- paste0("Last week ", "(",
                    paste(range(dates), collapse=" to "), ")")

## Death rate last week
barplot(week$WeekRate, names=week$Country, horiz=TRUE, las=1, col=NA,
        border=FALSE, main=main.week, xlab="Deaths per million inhabitants")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(week$WeekRate, horiz=TRUE, axes=FALSE, col="brown", add=TRUE)

## Death rate last week and before
barplot(week$Rate, names=week$Country, horiz=TRUE, las=1, col=NA, border=FALSE,
        main=main.week, xlab="Deaths per million inhabitants")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(t(week[c("WeekRate","PrevRate")]), horiz=TRUE, axes=FALSE,
        col=c("brown","gray95"), add=TRUE)

## Deaths last week
w <- week.all[c("Country","WeekDeaths")]
cutoff <- sort(w$WeekDeaths, decreasing=TRUE)[10]
w$Country[w$WeekDeaths < cutoff] <- "Other"
w <- aggregate(WeekDeaths~Country, w, sum)

w <- sort(w, by="WeekDeaths")
barplot(w$WeekDeaths[w$Country!="Other"]/7, names=w$Country[w$Country!="Other"],
        horiz=TRUE, las=1, col=NA, border=FALSE, main=main.week,
        xlab="Average daily deaths")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(w$WeekDeaths[w$Country!="Other"]/7, horiz=TRUE, axes=FALSE,
        col="orange", add=TRUE)

par(opar)
dev.off()
