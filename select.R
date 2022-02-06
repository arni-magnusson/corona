## Select plots for email

## Before: deaths_total.pdf, deaths_tseries.pdf, week.pdf (report)
## After:  total.pdf, tseries.pdf, week.pdf (report)

library(TAF)

mkdir("report")

## Total
##   1=worst, 5=W Europe, 7=N America, 8=S America, 11=Oceania, 12=continents
system(paste("pdftk report/deaths_total.pdf cat 1 5 7 8 11 12",
             "output report/total.pdf"))
## Timeline
##   2=EUvsUS, 6=W Europe, 8=N America, 9=S America, 12=Oceania, 13=continents,
##   15=world by year, 16=world daily
system(paste("pdftk report/deaths_tseries.pdf cat 2 6 8 9 12 13 15 16",
             "output report/tseries.pdf"))

## Week
##   3=worst daily, 1=worst per capita, 4=continents
system(paste("pdftk report/deaths_week.pdf cat 3 1 4",
             "output report/week.pdf"))
