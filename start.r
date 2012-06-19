setwd ("C:/Documents and Settings/socalar/My Documents/My Dropbox/R/soc.ca.dev")
setwd("/home/grau/Dropbox/R/soc.ca.dev")


rm(list=ls())
detach()
detach(data)

source("soc.ca.r")
source("example.r")

result <- soc.ca(analyse, sup, identifier)

result <- soc.ca(analyse)