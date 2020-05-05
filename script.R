library(arni)                       # na.clean, sort.data.frame
suppressMessages(library(gplots))   # rich.colors
library(icesTAF)                    # zoom
library(lattice)                    # xyplot
library(reshape2)                   # melt

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
rownames(pop) <- NULL

corona <- merge(pop, deaths)
corona <- na.clean(corona)
row.names(corona) <- NULL

## 5  Rate (deaths per million) and doubling time

doubling.time <- function(country, data=global)
{
  x <- data[data$Country==country,]
  sum(x$Deaths >= tail(x$Deaths,1) / 2)
}

corona$Rate <- round(corona$Deaths / corona$Population * 1e6, 1)
corona$Doubling <- sapply(corona$Country, doubling.time)

rate <- sort(corona, by="Rate")
rate <- rate[rate$Rate>=40 & rate$Population>1e5,]
row.names(rate) <- NULL

doubling <- sort(corona, by="Doubling")
doubling <- doubling[doubling$Doubling <= 10 & doubling$Deaths >= 20,]
row.names(doubling) <- NULL

## 6  Calculate rank and color

rate$Rank <- rate$Doubling - min(rate$Doubling) + 1
rate$Color <- rich.colors(max(rate$Rank), "blues")[rate$Rank]

doubling$Rank <- floor(log10(doubling$Deaths))
doubling$Rank <- doubling$Rank - min(doubling$Rank) + 1
doubling$Color <- rev(rich.colors(max(doubling$Rank), "blues"))[doubling$Rank]

by <- c(-1,1) * match(c("Doubling","Rank"), names(doubling))
doubling <- sort(doubling, by=by)

## 7  Plot

pdf("plots_current.pdf")
par(plt=c(0.28, 0.94, 0.15, 0.88))
barplot(rate$Rate, names=rate$Country, horiz=TRUE, las=1, col=NA, border=FALSE)
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(rate$Rate, horiz=TRUE, axes=FALSE, col=rate$Color, add=TRUE)
title(xlab="Deaths per million inhabitants")

par(plt=c(0.28, 0.94, 0.15, 0.88))
barplot(doubling$Doubling, names=doubling$Country, horiz=TRUE, las=1, col=NA,
        border=FALSE)
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(doubling$Doubling, horiz=TRUE, axes=FALSE, col=doubling$Color, add=TRUE)
title(xlab="Doubling time of deaths (days)")
dev.off()

## 8  Timeline

pdf("plots_timeline.pdf")
countries <- c("US", "Italy", "Spain", "France", "United Kingdom", "Belgium",
               "Germany", "Sweden", "Denmark")
timeline <- global[global$Country %in% countries,]
start <- as.Date("2020-02-14")
xyplot(log10(Deaths)~Date, groups=Country, data=timeline, subset=Date>start,
       auto.key=TRUE, type="l")

europe <- c("Germany", "UK", "France", "Italy", "Spain")
europe <- global[global$Country %in% europe,]
europe <- aggregate(Deaths~Date, europe, sum)
onset.europe <- min(europe$Date[europe$Deaths>=100])
us <- timeline[timeline$Country=="US",]
onset.us <- min(us$Date[us$Deaths>=100])
plot(Deaths~I(Date-onset.europe), data=europe, subset=Date>=onset.europe,
     type="l", col=2, lwd=3, main="Europe (de, uk, fr, it, sp) vs. USA",
     xlab="Days after 100 deaths")
lines(Deaths~I(Date-onset.us), data=us, subset=Date>=onset.us, col=4, lwd=4,
      lty=3)
legend("bottomright", c("Europe","USA"), lwd=c(3,4), lty=c(1,3), col=c(2,4),
       bty="n", inset=0.04)
plot(log10(Deaths)~I(Date-onset.europe), data=europe, subset=Date>=onset.europe,
     type="l", col=2, lwd=3, main="Europe (de, uk, fr, it, sp) vs. USA",
     xlab="Days after 100 deaths", yaxt="n")
axis(2, seq(floor(par("usr")[3]), floor(par("usr")[4])))
lines(log10(Deaths)~I(Date-onset.us), data=us, subset=Date>=onset.us, col=4,
      lwd=4, lty=3)
legend("bottomright", c("Europe","USA"), lwd=c(3,4), lty=c(1,3), col=c(2,4),
       bty="n", inset=0.04)

zoom(xyplot(Daily~Date|Country, global,
            subset=Country %in% countries & Date>="2020-03-01",
            panel=panel.loess, layout=c(3,3), ylim=c(0,NA), scales="free",
            ylab="Daily deaths"), 0.6)

plot(log10(Deaths)~Date, world, main="Total deaths worldwide")

plot(Daily~Date, world, main="Daily deaths worldwide", ylab="Deaths")
lines(world$Date, fitted(loess(Daily~as.integer(Date), world, span=0.3)),
      lwd=2, col="darkgreen")
dev.off()
