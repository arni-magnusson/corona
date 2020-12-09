library(arni)                      # na.clean, sort.data.frame
suppressMessages(library(gplots))  # rich.colors
library(icesTAF)                   # lim

source("utilities.R")

## 1  Fetch data

data <- file.path("https://raw.githubusercontent.com/CSSEGISandData/COVID-19",
                  "master/csse_covid_19_data")
ts <- file.path(data, "csse_covid_19_time_series")

lookup <- read.csv(file.path(data,"UID_ISO_FIPS_LookUp_Table.csv"))

## 4  Current

pop <- lookup[lookup$Province_State=="", c("Country_Region","Population")]
names(pop)[names(pop)=="Country_Region"] <- "Country"
row.names(pop) <- NULL

## 6  Sets of countries

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
