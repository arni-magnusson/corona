## Define sets of countries, write model results

## Before: (nothing)
## After:  countries.RData (model)

library(TAF)

mkdir("model")

## Sets of countries
euro5 <- c("Germany", "United Kingdom", "France", "Italy", "Spain")
nordic <- c("Sweden", "Denmark", "Finland", "Norway", "Iceland")
latin <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
           "Mexico", "Panama", "Peru")
w.europe <- c("Belgium", "France", "Germany", "Italy", "Netherlands",
              "Portugal", "Spain", "Switzerland", "United Kingdom")
e.europe <- c("Russia", "Turkey", "Poland", "Romania", "Czechia", "Greece",
              "Hungary", "Ukraine", "Bulgaria")
asia <- c("China", "Japan", "India", "South Korea", "Indonesia", "Saudi Arabia",
          "Iran", "Pakistan", "Bangladesh")
africa <- c("Nigeria", "Egypt", "South Africa", "Algeria", "Morocco", "Kenya",
            "Ethiopia", "Congo (Kinshasa)", "Tanzania")
spc <- c("Papua New Guinea", "Fiji", "Solomon Islands", "Vanuatu", "Samoa",
         "Kiribati", "Micronesia", "Tonga", "Marshall Islands", "Palau")

## Save sets
save("euro5", "nordic", "latin", "w.europe", "e.europe", "asia", "africa",
     "spc", file="model/countries.RData")
