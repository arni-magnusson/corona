installed <- rownames(installed.packages())

if(!any(installed == "remotes")) install.packages("remotes")

if(!any(installed == "remotes")) remotes::install_github("arni-magnusson/arni")
if(!any(installed == "gplots"))  install.packages("gplots")
if(!any(installed == "icesTAF")) install.packages("icesTAF")
if(!any(installed == "lattice")) install.packages("lattice")
if(!any(installed == "reshape2")) install.packages("reshape2")
