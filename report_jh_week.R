## Prepare plots and tables for Johns Hopkins week

## Before: jh_deaths_timeline.csv (data), jh_week.csv, jh_week_full.csv (output)
## After:  jh_deaths_current.pdf, jh_deaths_timeline.pdf, jh_week.pdf (report)

library(icesTAF)

mkdir("report")

## Read data
timeline <- read.taf("data/jh_deaths_timeline.csv")
timeline$Date <- as.Date(timeline$Date)
week <- read.taf("output/jh_week.csv")
week.full <- read.taf("output/jh_week_full.csv")

## Prepare label
dates <- sort(unique(timeline$Date[timeline$Date>max(timeline$Date)-7]))
main.week <- paste0("Last week ", "(",
                    paste(range(dates), collapse=" to "), ")")

## Death rate last week
pdf("report/jh_week.pdf")
opar <- par(plt=c(0.34, 0.94, 0.15, 0.88))
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
w <- week.full[c("Country","WeekDeaths")]
cutoff <- sort(w$WeekDeaths, decreasing=TRUE)[10]
w$Country[w$WeekDeaths < cutoff] <- "Other"
w <- aggregate(WeekDeaths~Country, w, sum)

w <- w[order(w$WeekDeaths),]
barplot(w$WeekDeaths[w$Country!="Other"]/7, names=w$Country[w$Country!="Other"],
        horiz=TRUE, las=1, col=NA, border=FALSE, main=main.week,
        xlab="Average daily deaths")
grid(nx=NULL, ny=NA, lty=1, lwd=1)
barplot(w$WeekDeaths[w$Country!="Other"]/7, horiz=TRUE, axes=FALSE,
        col="orange", add=TRUE)

par(opar)
dev.off()
