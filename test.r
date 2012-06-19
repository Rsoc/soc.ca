setwd ("C:/Documents and Settings/socalar/My Documents/My Dropbox/R/stable")
setwd ("/home/grau/Dropbox/R/soc.ca.dev")

rm(list=ls())
detach()
detach(data)
#Sys.setlocale(category = "LC_ALL", locale = "C")

source("soc.ca.r")
source("example.r")

analyse <- data.frame(ekstra_udd, vidart, lederfoer)
sup <- data.frame(phd_udd, koen)

############################# Soc.ca with a subset
set.passive(c("MISSING", "Missing", "missing", "Irrelevant", "8888", "Uoplyst", "Ved ikke om har været udstationeret som embedsperson", "stillingfoerUdlandet"))
soc.ca(analyse)
soc.ca(analyse, sup)
soc.ca(analyse, sup, identifier)
soc.ca(analyse, identifier) # ERROR

xa      <- soc.ca(analyse)
xas     <- soc.ca(analyse, sup)
xasi    <- soc.ca(analyse, sup, identifier)

# invert
a <- xa$coord[,1:5]
xb <- invert(xa, dim=1:5)
b <- xb$coord[,1:5]
identical(sum(a+b, na.rm=TRUE), 0) # Pass = TRUE

# contribution #! Needs a test value
contribution(xa)
contribution(xasi, dim=1)

# add.n #! Needs a test value
add.n(xas)
a <- add.n(xas, add=TRUE)
a$names
p.active(a)
# Soc.ca without a subset
set.passive("ASDFASDFASDFASDFASDF")
soc.ca(analyse)
soc.ca(analyse, sup)
soc.ca(analyse, sup, identifier)
soc.ca(analyse, identifier) # ERROR




# Test af Burt Supmat og Subset.ca
B <- burt(analyse)

# Defining the subset
sub         <- grepl(paste(passive, collapse="|"),colnames(B))
set         <- 1:ncol(B)
subset         <- set[!sub]

# Test af sup.burt
s.burt <- sup.burt(analyse) # Error in is.null(x) : 'x' is missing
s.burt <- sup.burt(analyse, sup)

# Test af subset.ca
resultat <- subset.ca(B, subset, s.burt, nvar=ncol(analyse))

# Test af soc.ca
resultat <- soc.ca(analyse,) # Error in apply(supmat, 1, sum) : dim(X) must have a positive length
resultat <- soc.ca(analyse, sup)
resultat <- soc.ca(analyse, sup=NA, identifier)
resultat <- soc.ca(analyse, sup, identifier)

# Test af printfunktion
# Scree
scree(resultat)
# Balance
balance.ctr(resultat)
# Print
resultat

# Test af tools
# Bidrag
contribution(resultat)
con <- contribution(resultat,3)

# tab.dim
tab.dim(resultat)
dimtab <- tab.dim(resultat, 2)

# Export
export(resultat)
export(resultat, file="test.csv", nd=1:2)
export(con)
export(dimtab)

# invert
a <- resultat$coord[,1]
resultat <- invert(resultat,1)
b <- resultat$coord[,1]
sum(a+b)==0

a <- resultat$coord[,2]
resultat <- invert(resultat,2)
b <- resultat$coord[,2]
sum(a+b)==0
rm(a,b)

# coordinates
coordinates(resultat)
coordinates(resultat, dim=2:3)

# Test af label funktioner

# add.n
add.n(resultat)
resultat <- add.n(resultat, add=TRUE)
resultat$names

# get.label
test.label <- get.label(resultat)

# get.varlabel
get.varlabel(resultat)

# export.label
export.label(test.label)

#import.label
import.label("test.label")

#assign.label
assign.label(resultat, test.label)

# Test af plotfunktioner

p.all(resultat)
p.all(resultat, dim=c(3,2))

p.ctr(resultat)
p.ctr(resultat, ctr.dim=2, dim=c(3,2))

p.active(resultat)
p.active(resultat, dim=c(2,1))

p.sup(resultat)
p.sup(resultat, dim=c(4,1))

p.id(resultat)
p.id(resultat, dim=c(2,1), point.label=TRUE)

p.dim(resultat)
p.dim(resultat, dim=3) # Error in max(dim) : invalid 'type' (builtin) of argument

### Test med Diamonds
library(Hmisc)
detach(data)
attach(diamonds)

idento     	<- as.factor(1:nrow(diamonds))

carat2 <- cut2(carat, g=5)
depth2 <- cut2(depth, g=5)
price2 <- cut2(price, g=5)
table2 <- cut2(table, g=5)


analyse <- data.frame(cut, color, clarity, carat2, depth2, price2)
sup <- table2
identifier <- idento
#soc.ca(analyse, sup, identifier)


passive <- c("MISSING", "Missing", "missing", "Irrelevant", "8888", "Uoplyst", "Ved ikke om har været udstationeret som embedsperson", "stillingfoerUdlandet")

# Test af Burt Supmat og Subset.ca
B <- burt(analyse)

#
sub     <- grep(paste(passive, collapse="|"),colnames(B))
set     	<- 1:ncol(B)
subset 		<- set[-c(sub)]

# Test af sup.burt
supmat <- sup.burt(analyse, sup, identifier)

# Test af subset.ca
resultat <- subset.ca(B, subset, s.burt, nvar=ncol(analyse))

