## Define sets of countries, write model results

## Before: (nothing)
## After:  countries.RData (model)

library(TAF)

mkdir("model")

## Sets of countries
euro5 <- c("Germany", "United Kingdom", "France", "Italy", "Spain")
nordic <- c("Sweden", "Denmark", "Finland", "Norway", "Iceland")
w.europe <- c("Belgium", "France", "Germany", "Italy", "Netherlands",
              "Portugal", "Spain", "Switzerland", "United Kingdom")
e.europe <- c("Russia", "Turkey", "Poland", "Romania", "Czechia", "Greece",
              "Hungary", "Ukraine", "Bulgaria")
n.america <- c("US", "Canada", "Mexico", "Guatemala", "Dominican Republic",
               "Cuba", "Panama", "Costa Rica", "Haiti")
s.america <- c("Brazil", "Argentina", "Colombia", "Chile", "Peru", "Venezuela",
               "Ecuador", "Bolivia", "Uruguay")
asia <- c("China", "Japan", "India", "South Korea", "Indonesia", "Saudi Arabia",
          "Iran", "Pakistan", "Bangladesh")
africa <- c("Nigeria", "Egypt", "South Africa", "Algeria", "Morocco", "Kenya",
            "Ethiopia", "Congo (Kinshasa)", "Tanzania")
oceania <- c("Papua New Guinea", "Fiji", "Solomon Islands", "Vanuatu", "Samoa",
             "Kiribati", "Micronesia", "Tonga", "Marshall Islands", "Palau",
             "Australia", "New Zealand")

## Save sets
save("euro5", "nordic", "w.europe", "e.europe", "n.america", "s.america",
     "asia", "africa", "oceania", file="model/countries.RData")
