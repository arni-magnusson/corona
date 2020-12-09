library(lattice)   # xyplot
library(reshape2)  # melt

doubling.time <- function(country, column, data=timeline)
{
  x <- data[data$Country==country,]
  sum(x[[column]] >= tail(x[[column]],1) / 2)
}

plotTimeBase <- function(data, start="2020-03-01", ylim=NULL, lwd=3, col.line=4,
                         col.points=8, cex.label=1.2, points=FALSE, ...)
{
  ## data contains Country, Date, Daily
  opar <- par(plt=c(0.15,0.9,0.15,0.9))
  data <- data[data$Date >= start,]
  fit <- fitted(loess(Daily~as.integer(Date), data, ...))
  if(is.null(ylim))
    ylim <- if(points) lim(c(fit, data$Daily), mult=1.2) else lim(fit, mult=1.2)
  plot(data$Date, fit, type="n", ylim=ylim, ann=FALSE)
  if(points)
    points(data$Date, data$Daily, col=col.points)
  lines(data$Date, fit, lwd=lwd, col=col.line)
  text(as.Date(start)+2, 0.9*par("usr")[4], data$Country[1], adj=0, cex=cex.label)
  par(opar)
}

plotTimeLattice <- function(x, main="", ...)
{
  print(xyplot(TwoWeeks~Date|Ordered, timeline, subset=Country %in% x, type="l",
               ylim=c(0, NA), as.table=TRUE, par.settings=pars,
               par.strip.text=list(cex=0.7), main=main, ylab="Index", ...))
}

plotXY <- function(data, ylab="", ...)
{
  ## data contains Country, Doubling, Rate
  plot(Rate~Doubling, data, type="n", xlim=c(range(data$Doubling) + c(-2,2)),
       ylim=lim(data$Rate), xlab="Doubling time (days)", ylab=ylab, ...)
  text(Rate~Doubling, data, data$Country, ...)
}

rearrange <- function(x, colname="Count")
{
  x <- x[-c(1,3,4)]
  names(x)[1] <- "Country"
  x <- melt(x, "Country")
  names(x) <- c("Country", "Date", "Value")
  x$Date <- as.Date(x$Date, "%m/%d/%y")
  x <- aggregate(Value~Country+Date, x, sum)
  x <- x[order(x[1],x[2]),]
  row.names(x) <- NULL
  names(x) <- c("Country", "Date", colname)
  x
}
