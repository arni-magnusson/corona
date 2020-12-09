## Run ECDC analysis, write model results

## Before: ecdc_timeline.csv (data)
## After:  ecdc_current_africa.csv, ecdc_current_america.csv,
##         ecdc_current_asia.csv, ecdc_current_europe.csv,
##         ecdc_current_nordic.csv, ecdc_current_worst.csv (model)

library(icesTAF)

mkdir("model")

## Read data
timeline <- read.taf("data/ecdc_timeline.csv")

## Order countries by highest index
peak <- aggregate(TwoWeeks~Country+Continent+Population, timeline, max)
peak <- peak[order(peak$TwoWeeks),]
## peak[peak$Continent=="Europe" & peak$Population>2e5,]

## Sets of countries
worst <- tail(peak[peak$Population > 2e5,], 15)$Country
europe <- c("Austria", "Belgium", "Czechia", "France", "Georgia", "Germany",
            "Hungary", "Italy", "Luxembourg", "Netherlands", "Poland",
            "Portugal", "Spain", "Switzerland", "United_Kingdom")
nordic <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")
america <- c("Argentina", "Bolivia", "Brazil", "Canada", "Chile", "Colombia",
             "Cuba", "Ecuador", "Mexico", "Panama", "Paraguay", "Peru",
             "United_States_of_America", "Uruguay", "Venezuela")
asia <- c("Bangladesh", "China", "India", "Indonesia", "Iran", "Israel",
          "Japan", "Pakistan", "Philippines", "Russia", "Singapore", "Turkey")
africa <- c("Algeria", "Cape_Verde", "Democratic_Republic_of_the_Congo",
            "Egypt", "Eswatini", "Ethiopia", "Kenya", "Libya", "Morocco",
            "Nigeria", "South_Africa", "Tunisia")

## Save sets
write.taf(peak, "model/ecdc_peak.csv", quote=TRUE)
save("worst", "europe", "nordic", "america", "asia", "africa",
     file="model/ecdc_countries.RData")
