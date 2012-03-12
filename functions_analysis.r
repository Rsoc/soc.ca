# Functions analysis

#'Specific Multiple Correspondence Analysis
#'
#'
#'@param x Defines the active modalities in a data.frame with rows of individuals and columns of factors, without NA's' 
#'@param sup Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
#'@param identifier A single factor containing a single value for each row/individual in x and sup. Typically a name or an id.number.
#'
#'@return sv Singular values
#'@return nd Number of dimensions NB! 
#'@return ev Eigenvectors
#'@return totin Total inertia
#'@return names Names of the modalities
#'@return mass Mass for each modality
#'@return chidist Chi-square distances to centroid 
#'@return inertia Inertia for each modality
#'@return explained.inertia A matrix containing the explained dimensions, Sv, percentage of explained inertia and the cumulative percentage
#'@return adj.inertia A matrix containing the adjusted (Rouanet-adjustment) dimensions, Sv, percentage of explained inertia and the cumulative percentage
#'@return contrib The contribution of each modality to each dimension
#'@return cor
#'@return coord Principal coordinates
#'@return average.contrib 
#'@return standard.coord Standard coordinates
#'@return active A vector with rownumbers for the active modalities
#'@return sup A vector with rownumbers for the supplementary modalities
#'@return identifier A vector with rownumbers for the individuals
#'@return Burt The burt matrix containing the active modalities
#'@return modal
#'
#'
#'
#'@ References Greenacre NB!!!!
#'@name smca
#'@author Anton Grau Larsen and Stefan Bastholm Andrade
#'

soc.ca <- function(x, sup=NULL, identifier=NULL, passive="Missing"){

x       <- data.frame(lapply(x, factor))            # turn active variables into factor
a.r     <- nrow(x)                                  # Number of active rows or the number of individuals
sup.n   <- sum(unlist(lapply(as.data.frame(sup), nlevels))) # Number of supplementary modalities

B           <- burt(x)                              # The active burt matrix is created    
supmat <- matrix(, nrow=0, ncol=ncol(B))

if (identical(sup, NULL)==FALSE){
sup     <- data.frame(lapply(sup, factor))          # turn supplementary variables into factor
supmat      <- sup.burt(x, sup)                     # The supplementary burt matrix is created
}                                                    

if (identical(identifier, NULL)==FALSE){
supmat                <- rbind(supmat, indicator(x, id=identifier)) # The supplementary indicatormatrix is created
}

if ((identical(identifier, NULL)==TRUE)&(identical(sup, NULL)==TRUE)){
supmat <- matrix(, nrow=2, ncol=ncol(B))
rownames(supmat) <- c("No Supplementary points", "No Supplementary points" )
}

sub         <- grepl(paste(passive, collapse="|"),colnames(B))
set         <- 1:ncol(B)
subset      <- set[!sub]

result      <- subset.ca(B, subset, supmat, nvar=ncol(x))

l.active    <- length(subset)
l.id        <- nlevels(identifier)
l.sup 	    <- nrow(supmat)-l.id
i.active    <- (1:l.active)
i.sup	    <- (l.active+1):(l.active+l.sup)
i.id 	    <- (l.active+l.sup+1):(l.active+l.sup+l.id)

# List of descriptive values

result$nid      <- a.r
result$npassive <- length(which(sub==TRUE)) 
result$passive  <- colnames(B)[sub] # Could be a matrix with the amount of individuals in each modality

result$active 	<- i.active
result$sup		<- i.sup
result$identifier	<- i.id
result$Burt 		<- B[subset, subset] 

varnames    <- colnames(x)
ml          <- vector()
for (i in 1:ncol(x)){
 ml         <- c(ml, rep(varnames[i], nlevels(x[,i])))
}
ml          <- ml[!sub]
mm          <- as.matrix(cbind(ml, 1:length(ml)))
md          <- matrix(, nrow=ncol(x), ncol=3)
rownames(md) <- varnames
colnames(md) <- c("Start", "End", "Modalities")
md          <- as.data.frame(md)

for (i in 1:ncol(x)){
 mr         <- as.numeric(mm[,2][mm[,1]==varnames[i]])
 md[i,1]    <- min(mr)
 md[i,2]    <- max(mr)
 md[i,3]    <- length(mr)
}
md[,1]      <- as.numeric(md[,1])
md[,2]      <- as.numeric(md[,2])

result$modal    <- md

class(result)   <- "soc.ca"
return(result)
}

################ Burt Matrix

# Han laver Burt matricen udfra en indikatormatrice - den virker lidt som spild, men jeg kan ikke lige lave en anderledes en
burt    <- function(x, ps=": "){
obj     <- x
obj     <- data.frame(lapply(data.frame(obj), as.factor))
I       <- nrow(obj)
levels.n <- unlist(lapply(obj, nlevels))
n       <- cumsum(levels.n)
Q       <- ncol(obj)
Z       <- matrix(0, nrow = I, ncol = n[length(n)])
newdat  <- lapply(obj, as.numeric)
offset  <- (c(0, n[-length(n)]))
for (i in 1:Q) Z[1:I + (I * (offset[i] + newdat[[i]] - 1))] <- 1
fn      <- rep(names(obj), unlist(lapply(obj, nlevels)))
ln      <- unlist(lapply(obj, levels))
B       <- t(Z) %*% Z
col.names        <- paste(fn, ln, sep = ps)
dimnames(Z)[[2]] <- col.names
dimnames(Z)[[1]] <- as.character(1:I)
# Z er en f?rdig indikatormatrice - s? den kan jo bruges hvis man har lyst
dimnames(B)[[2]] <- col.names
dimnames(B)[[1]] <- col.names
return(B)
}


##################### Supplementary Burt matrix
# Burt og sup.burt, kan forudsat at de er lavet p? det samme x objekt, kombineres med f?lgende kommando:
# rbind(burt, sup.burt)
# sup.burt(x, sup, identifier=NULL, ps=": ")
# x is a soc.ca object
# sup is a data.frame of vectors containing the supplementary variables
# ps is the seperator used in the creation of the names of the columns (modalities).

sup.burt <- function(x, sup, ps=": "){

supvar <- as.data.frame(sup) 
addburt <- function(supvariable, x){
  a.l <- ncol(x)
  fburt       <- table(supvariable, x[,1])
  for (i in 2:a.l){
    temp     	<- table(supvariable, x[,i])
    fburt 		<- cbind(fburt, temp)
  }
  return(fburt)
}

sup.n <- sum(unlist(lapply(supvar, nlevels))) # The number of supplementary modalities
active.n <- sum(unlist(lapply(x, nlevels))) # The number of active modalities

fburt       <- matrix(,ncol=active.n, nrow=sup.n)
counter     <- 1
for (i in seq(supvar)){
position    <- seq(counter, (counter+nlevels(supvar[,i])-1))
fburt[position,] <-  addburt(supvar[,i], x)
counter     <- counter+nlevels(supvar[,i])
}

# Her s?tter vi navne p? de supplement?re variable
fn <- rep(names(as.data.frame(sup)), unlist(lapply(as.data.frame(sup), nlevels)))
ln <- unlist(lapply(as.data.frame(sup), levels))
col.names <- paste(fn, ln, sep = ps)
rownames(fburt) <- col.names

# Her s?tter vi navne p? de aktive variable
fn <- rep(names(x), unlist(lapply(x, nlevels)))
ln <- unlist(lapply(x, levels))
col.names <- paste(fn, ln, sep = ps)
colnames(fburt) <- col.names

return(fburt)
}


#################### Correspondence analysis on a Burt matrix
# B is a Burt matrix
# supmat is a supplementary burt matrix
# nvar ???
subset.ca   <- function(B, subset, supmat, nvar=ncol(x)){
obj 		<- B
nd 		<- ncol(obj)
modal.names 	<- colnames(obj)[subset]
N		<- matrix(as.matrix(obj), nrow = nd, ncol = nd)
n 		<- sum(N) 				# summen af Burtmatricen
P 		<- N/n 					# Den relative frekvens i forhold til totalen for Burt Matricen
mass 		<- apply(P, 1, sum)			# Summen af hver r?kkes relative frekvenser eller massen
s.mass 		<- mass[subset]
eP 		<- mass %*% t(mass) 			# Han ganger massen med den transponerede masse - som er den samme
eN 		<- eP * n 				# Fra relative procenter, til relative frekvenser
S 		<- (P - eP)/sqrt(eP)			# RESIDUALMATRICEN: givet ved skaleringsproduktet - her skabes den f?lles skala, p? tv?rs af dimensioner
S 		<- S[subset, subset]
dec 		<- svd(S)				# Singul?r v?rdi dekomposition
sv 		<- dec$d				# Her hentes de singul?re v?rdier (En for hver dimension)
u 		<- dec$u				# Her er de singul?re vectorer - (En modalitets position p? samtlige dimensioner i det dekompositionerede rum)
ev 		<- sv^2 				# EigenVEKTOR (Principal inertias)    
totin 		<- sum(ev)				# Den totale inerti eller den samlede m?ngde variation/afstande
inertia 	<- apply(S^2, 1, sum) 			# Inerti for hver modalitet over dimensionerne
chidist 	<- sqrt(inertia/s.mass) 		# Chi afstande fra hver modalitet til origo/centriden
phi 		<- as.matrix(u)/sqrt(s.mass) 		# Standard koordinaterne
nd 		<- nd-(nd-length(subset))

rs.sum	 	<- apply(supmat, 1, sum)		# R?kkesummen af de supplement?re
supmat	 	<- supmat[,subset] 			# Her fjernes subsettet p? de aktive kolonner

supchi		<- supmat / apply(supmat, 1, sum)	# Chi afstande for de supplement?re punkter
supchi		<- t((t(supchi) - s.mass) / sqrt(s.mass))
supchidist	<- sqrt(apply(supchi^2, 1, sum))
chidist 	<- c(chidist, as.numeric(supchidist))

cs		<- mass[subset]				# Coordinater for de supplement?re punkter
gam.00		<- phi
base2		<- supmat / matrix(rs.sum, nrow = nrow(supmat), ncol = ncol(supmat))
base2 		<- t(base2)
cs.0		<- matrix(cs, nrow = nrow(base2), ncol = ncol(base2))
svphi		<- matrix(sv[1:nd], nrow = nrow(supmat), ncol = nd, byrow = T) 
base2		<- base2 - cs.0
phi2		<- (t(as.matrix(base2)) %*% gam.00) / svphi

phi 	 	<- rbind(phi, phi2)
modal.names	<- c(modal.names, rownames(supmat))
rownames(phi) 	<- modal.names

# a.aktive 	<- ncol(supmat)
# a.sup		<- nrow(supmat)
# Mass og Inertia har ikke samme l?ngde som coord - fordi de ikke har NA til de supplement?re.

K   		<- nd # Her udregnes principal coordinaterne
I   		<- dim(phi)[1] 
svF 		<- matrix(rep(sv, I), I, K, byrow = TRUE)
rpc 		<- phi[,1:K] * svF


values     <- sv^2
values2    <- 100*(sv^2)/sum(sv^2)
values3    <- cumsum(100*(sv^2)/sum(sv^2))
explained.inertia  <- cbind(1:length(sv), values, values2, values3)
colnames(explained.inertia) <- c("Dim","Sv", "%", "Cum%")

sing     	<- values2
Dim 		<- which(as.matrix(sing) >= mean(sing, na.rm=TRUE)) # Rouanet adjustment
cumulative 	<- sing[Dim]
percent 	<- cumulative/sum(cumulative)*100
cumpercent	<- cumsum(cumulative/sum(cumulative)*100)
dimension 	<- 1:length(percent)
adj.inertia 	<- cbind(dimension, sv[Dim] ,percent,cumpercent) # De Rouanet adjustede inertier - skal nok rykkes ned.
colnames(adj.inertia) <- c("Dim", "Sv", "%", "Cum%")


submass 	<- mass[subset]
rpcakt 		<- rpc[1:nd, 1:nd]
cor 		<- rpc^2 / chidist^2
ctr 		<- matrix(ncol=nd, nrow=nd)
for (i in 1:nd){
  ctr[i,] 	<- as.matrix(submass * rpcakt[,i]^2 /sv[i]^2)
}
ctr 		<- t(ctr)

#av.ctr 		<- list()
#for (i in 1:nd){
#  av.ctr[[i]]	<- which(ctr[,i] >= mean(ctr[,i], na.rm=TRUE))
#}

## Vi prøver at adjuste

nd.max <- nd - nvar
Q <- nvar
cm <- mass
cm <- mass[subset]
J <- length(subset) # Her laver vi en Christoph adjustment af den subsettede matrice.
#J <- length(set) Den her giver negative lambda.e

# nd <- nd.max
lambda0 <- dec$d[1:nd.max]^2
nd.max <- sum(sqrt(lambda0) >= 1/Q)
lambda.adj <- ((Q/(Q - 1))^2 * (sqrt(lambda0)[1:nd.max] - 1/Q)^2)

# Sakset fra Greenacre 

            nd.max <- sum(sqrt(lambda0) >= 1/Q)
            if (is.na(nd) | nd > nd.max) 
                nd <- nd.max
            lambda.adj <- ((Q/(Q - 1))^2 * (sqrt(lambda0)[1:nd.max] - 
                1/Q)^2)
            lambda.t <- (Q/(Q - 1)) * (sum(lambda0) - ((J - Q)/Q^2))
            lambda.e <- lambda.adj/lambda.t # Det her giver et tal for forklaret inerti, der ikke summer til 1.
#            lambda.et <- NA
            lambda0 <- lambda.adj
            colinertia <- (Q/(Q - 1))^2 * (inertia - (1/Q) *  # Det her er ikke godt - den kræver en subsettet mass, hvilket er ok, men så spytte den negative værdier ud.
                ((1/Q) - cm))
            colcoord <- as.matrix(dec$v[, 1:nd.max])/sqrt(cm) # Er det samme som vi har regnet ud, which is good.
#            rowcoord <- rowcoord[, 1:nd.max]




    ca.output <- list(sv = sv, nd = nd, ev = ev, totin = totin , names = modal.names, mass = mass[subset], 
        chidist = chidist, inertia = inertia, explained.inertia = explained.inertia ,
        adj.inertia=adj.inertia, contrib=ctr, cor=cor,  coord = rpc, standard.coord=phi)
        return(ca.output)
}


################################### Indicator matrix

indicator <- function(x, id=NULL, ps=": "){
obj     <- x
#obj     <- data.frame(lapply(data.frame(obj), as.factor)) # Gør hele analyse objektet til factor - kan sættes ind et andet sted.
I       <- nrow(obj)                                      # Number of individuals
levels.n <- unlist(lapply(obj, nlevels))
n       <- cumsum(levels.n)                               # Number of modalities for each question
m       <- max(n)                                         # Total number of modalities
Q       <- ncol(obj)                                      # Number of questions
Z       <- matrix(0, nrow = I, ncol = m)                  # Predefinition of the indicatormatrix
newdat  <- lapply(obj, as.numeric)
offset  <- (c(0, n[-length(n)]))
for (i in seq(Q)) Z[seq(I) + (I * (offset[i] + newdat[[i]] - 1))] <- 1 # Indicator matrix
fn      <- rep(names(obj), unlist(lapply(obj, nlevels)))  
ln      <- unlist(lapply(obj, levels))
col.names   <- paste(fn, ln, sep = ps)
colnames(Z) <- col.names

if (identical(id, NULL)==TRUE){
rownames(Z) <- as.character(seq(I))
}else{
rownames(Z) <- id
}    
return(Z)

# Creates a indicator matrix from a data.frame with questions as columns and individuals as rows.
# indicator(x, id=NULL, ps=": ")
# x is a data.frame
# id is a vector defining the labels for the individuals. If id = NULL row number is used.
# ps is the seperator used in the creation of the names of the columns (modalities).
}
