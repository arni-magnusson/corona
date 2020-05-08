doubling.time <- function(country, data=global)
{
  x <- data[data$Country==country,]
  sum(x$Deaths >= tail(x$Deaths,1) / 2)
}

plotTimeline <- function(data, start="2020-03-01", ylim=NULL, lwd=3, col.line=4,
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

plotXY <- function(data, ...)
{
  ## data contains Country, Doubling, Rate
  plot(Rate~Doubling, data, type="n", xlim=c(range(data$Doubling) + c(-2,2)),
       ylim=lim(data$Rate), xlab="Doubling time (days)",
       ylab="Deaths per million", ...)
  text(Rate~Doubling, data, data$Country, ...)
}
