## Define sets of countries, write model results

## Before: (nothing)
## After:  countries.RData (model)

library(TAF)

mkdir("model")

## Sets of countries
euro5 <- c("Germany", "UK", "France", "Italy", "Spain")
nordic <- c("Sweden", "Denmark", "Finland", "Norway", "Iceland")
latin <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
           "Mexico", "Panama", "Peru")
europe <- c("Belgium", "France", "Germany", "Italy", "Netherlands", "Portugal",
            "Spain", "Switzerland", "United Kingdom")
asia <- c("China", "Japan", "Indonesia", "India", "Pakistan", "Bangladesh",
          "Iran", "Russia", "Turkey")
africa <- c("Algeria", "Congo (Kinshasa)", "Eswatini", "Ethiopia", "Kenya",
            "Morocco", "Nigeria", "South Africa", "Sudan")

## Save sets
save("euro5", "nordic", "latin", "europe", "asia", "africa",
     file="model/countries.RData")
