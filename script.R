library(arni)                      # na.clean, sort.data.frame
suppressMessages(library(gplots))  # rich.colors
library(icesTAF)                   # lim
library(reshape2)                  # melt

source("functions.R")

## 1  Fetch data

data <- file.path("https://raw.githubusercontent.com/CSSEGISandData/COVID-19",
                  "master/csse_covid_19_data")
ts <- file.path(data, "csse_covid_19_time_series")

lookup <- read.csv(file.path(data,"UID_ISO_FIPS_LookUp_Table.csv"))
deaths.global <- read.csv(file.path(ts,"time_series_covid19_deaths_global.csv"),
                          check.names=FALSE)
deaths.us <- read.csv(file.path(ts,"time_series_covid19_deaths_US.csv"),
                      check.names=FALSE)

## 2  Reshape

global <- deaths.global[-c(1,3,4)]
names(global)[1] <- "Country"
global <- melt(global, "Country")
names(global) <- c("Country", "Date", "Deaths")
global$Date <- as.Date(global$Date, "%m/%d/%y")
global <- aggregate(Deaths~Country+Date, global, sum)
global <- sort(global, by=1:2)
row.names(global) <- NULL

## Correction
global$Deaths[global$Country=="Iceland" & global$Date=="2020-03-15"] <- 0

## 3  Calculate daily deaths

global$Daily <- c(global$Deaths[1], diff(global$Deaths))
global$Daily[global$Date == min(global$Date)] <-
  global$Deaths[global$Date == min(global$Date)]

world <- aggregate(cbind(Deaths,Daily)~Date, data=global, sum)

## 4  Current

deaths <- aggregate(Deaths~Country, global, tail, 1)
deaths <- deaths[deaths$Deaths>0,]

pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"
row.names(pop) <- NULL

corona <- merge(pop, deaths)
corona <- na.clean(corona)
row.names(corona) <- NULL

## 5  Rate (deaths per million) and doubling time

corona$Rate <- round(corona$Deaths / corona$Population * 1e6, 1)
corona$Doubling <- sapply(corona$Country, doubling.time)

rate <- tail(sort(corona[corona$Population>=1e5,], by="Rate"), 25)
row.names(rate) <- NULL

doubling <- sort(corona[corona$Deaths>=100,], by="Doubling")
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

europe <- c("Germany", "UK", "France", "Italy", "Spain")
europe <- global[global$Country %in% europe,]
europe <- aggregate(Deaths~Date, europe, sum)
onset.europe <- min(europe$Date[europe$Deaths>=100])
us <- global[global$Country=="US",]
onset.us <- min(us$Date[us$Deaths>=100])

worst <- tail(rate$Country, 9)
current.worst <- corona[corona$Country %in% worst,]
timeline.worst <- global[global$Country %in% current.worst$Country,]

nordic <- c("Sweden", "Denmark", "Finland", "Norway", "Iceland")
current.nordic <- corona[corona$Country %in% nordic,]
timeline.nordic <- global[global$Country %in% nordic,]

latin <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
           "Mexico", "Panama", "Peru")
current.latin <- corona[corona$Country %in% latin,]
timeline.latin <- global[global$Country %in% latin,]

asia <- c("China", "Japan", "Indonesia", "India", "Pakistan", "Bangladesh",
          "Iran", "Russia", "Turkey")
current.asia <- corona[corona$Country %in% asia,]
timeline.asia <- global[global$Country %in% asia,]

africa <- c("Algeria", "Congo (Kinshasa)", "Eswatini", "Ethiopia", "Kenya",
            "Morocco", "Nigeria", "South Africa", "Sudan")
current.africa <- corona[corona$Country %in% africa,]
timeline.africa <- global[global$Country %in% africa,]

## 7  Plot current

pdf("plots_current.pdf")

## Worst deaths
opar <- par(plt=c(0.28, 0.94, 0.15, 0.88))
barplot(rate$Rate, names=rate$Country, horiz=TRUE, las=1, col=NA, border=FALSE,
        xlab="Deaths per million inhabitants")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(rate$Rate, horiz=TRUE, axes=FALSE, col=rate$Color, add=TRUE)
par(opar)

## Worst doubling time
par(plt=c(0.28, 0.94, 0.15, 0.88))
barplot(doubling$Doubling, names=doubling$Country, horiz=TRUE, las=1, col=NA,
        border=FALSE, xlab="Doubling time of deaths (days)")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(doubling$Doubling, horiz=TRUE, axes=FALSE, col=doubling$Color, add=TRUE)
par(opar)

## Scatterplots
plotXY(current.worst, main="Worst hit")
plotXY(current.nordic, main="Nordic countries")
plotXY(current.latin, main="Latin America")
plotXY(current.asia, main="Asia")
plotXY(current.africa, main="Africa")
dev.off()

## 8  Plot timeline

pdf("plots_timeline.pdf")

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
plot(Deaths/1000~I(Date-onset.europe), data=europe, subset=Date>=onset.europe,
     type="l", ylim=lim(c(europe$Deaths, us$Deaths)/1000), col=2, lwd=3,
     main="Europe (de, uk, fr, it, sp) vs. USA",
     xlab="Days after 100 deaths", ylab="Deaths (thousands)")
lines(Deaths/1000~I(Date-onset.us), data=us, subset=Date>=onset.us, col=4,
      lwd=4, lty=3)
legend("bottomright", c("Europe","USA"), lwd=c(3,4), lty=c(1,3), col=c(2,4),
       bty="n", inset=0.04)

plot(log10(Deaths)~I(Date-onset.europe), data=europe, subset=Date>=onset.europe,
     type="l", ylim=c(2, log10(1.05*max(c(europe$deaths, us$Deaths)))), col=2,
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
out <- lapply(split(timeline.nordic, timeline.nordic$Country), plotTimeline,
              span=0.25)
par(mfrow=c(3,3))
out <- lapply(split(timeline.latin, timeline.latin$Country), plotTimeline,
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

pdf("plots_week.pdf")
opar <- par(plt=c(0.34, 0.94, 0.15, 0.88))
dates <- sort(unique(global$Date[global$Date>max(global$Date)-7]))

week <- aggregate(Daily~Country, global, sum, subset=Date%in%dates)
names(week) <- c("Country", "WeekDeaths")
week <- merge(corona, week)
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
