# COVID Plots: Cases and Deaths

This TAF workflow downloads the newest COVID data from Johns Hopkins University
and produces a variety of plots showing how different countries and continents
have been affected.

## How to run

Install the TAF package from CRAN.

Then open R in the `corona` directory and run:

```
library(TAF)
taf.bootstrap()
sourceAll()
```

## Explore results

The plot files (in PDF format) appear inside the `report` folder. All tables and
intermediate calculations can be found in the `data`, `model`, and `output`
folders.
