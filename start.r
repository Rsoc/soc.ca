setwd ("C:/Documents and Settings/socalar/My Documents/My Dropbox/R/soc.ca")
setwd ("/home/rask/Dropbox/R/soc.ca")
setwd ("/home/grau/Dropbox/R/soc.ca")


rm(list=ls())
detach()
detach(data)

source("soc.ca.r")
source("example.r")

result <- soc.ca(analyse, sup, identifier)