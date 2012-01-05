#### Functions for describtion and printing


# Print objects from the soc.ca function
print.soc.ca <- function(object){
Nmodal       <- length(object$active) 
Nsup     	<- length(object$sup) # S?t if ind
Nid 		<- object$nid
Vnames 		<- paste(rownames(object$modal), " (",object$modal[,3], ")", sep="")
Submass 	<- round(sum(object$mass), digits=2) # Er det her i Procent aka. summer mass altid til 1.

act.dim 	<- nrow(object$adj.inertia)
dim80 		<- which.min(object$adj.inertia[,4] < 80)
scree.dim	<- 6
stars 		<- scree(object, scree.dim)
adj.dim   <- 1:scree.dim
#adj.dim 	<- object$adj.inertia[1:scree.dim,1]
dim.a   <- ifelse((scree.dim<nrow(object$adj.inertia)), scree.dim, nrow(object$adj.inertia))
adj <- vector(mode="numeric", length=scree.dim) # Måske skal den ikke smide 0 som output men noget bedre, når der ikke er flere dim end 6
adj[1:dim.a] <- object$adj.inertia[1:dim.a,3]
adj	<- paste(formatC(adj,format="f",digits=1), sep="", collide="%")
## Tests
pm 		<- balance.ctr(object, act.dim)
pm.minus <- which(as.numeric(pm[,3])<0.5)
pm.plus <- which(as.numeric(pm[,3])>2)

## Output
cat(format("Specific Correspondence Analysis:", 	width=90, justify="centre"),"\n", "\n", 
format("Statistics", 					width=50, justify="centre"), format("Scree plot", width=40, justify="centre"),"\n",

format("	Active dimensions: ", 			width=40,), format(act.dim, 	width=10, justify="right"),
format("|  1.", width=10, justify="centre" ), format(adj[1], width=10, justify="centre"), format(paste(stars[[1]]), width=1), "\n",

format("	Dimensions explaining 80% of inertia: ",width=40,), format(dim80, 	width=10, justify="right"), 
format("|  2.", width=10, justify="centre" ), format(adj[2], width=10, justify="centre"), format(paste(stars[[2]]), width=1), "\n",

format("	Active modalities: ", 			width=40,), format(Nmodal, 	width=10, justify="right"), 
format("|  3.", width=10, justify="centre" ), format(adj[3], width=10, justify="centre"), format(paste(stars[[3]]), width=1), "\n",

format("	Supplementary modalities: ",		width=40,), format(Nsup, 	width=10, justify="right"), 
format("|  4.", width=10, justify="centre" ), format(adj[4], width=10, justify="centre"), format(paste(stars[[4]]), width=1), "\n",

format("	Individuals: ",		 		width=40,), format(Nid, 	width=10, justify="right"), 
format("|  5.", width=10, justify="centre" ), format(adj[5], width=10, justify="centre"), format(paste(stars[[5]]), width=1), "\n",

format("	Mass in subset",	 		width=40,), format(Submass, 	width=10, justify="right"), 
format("|  6.", width=10, justify="centre" ), format(adj[6], width=10, justify="centre"), format(paste(stars[[6]]), width=1), "\n",

"\n",
format("The active variables:", 			width=100, justify="centre" ),
"\n",
sep="")
cat(format(Vnames, width=25, justify="right"), fill=100)

# Test


if (length(pm.plus)>0){
  p.out <- matrix(, ncol=3, nrow=length(pm.plus))
  p.out[,] <- formatC(pm[pm.plus,], format="f", digits=2)
  rownames(p.out) <- rep(format("", width=35), times=length(pm.plus))  
  p.out <- cbind(paste(pm.plus, ".", sep=""), p.out)
  colnames(p.out) <- c("Dim.", "+", "-", "+/-")
  cat("\n",format("These dimensions contributions are skewed towards (+): ", width=100, justify="centre"), "\n")
  print(p.out, quote=FALSE, right=TRUE)
}      

if (length(pm.minus)>0){
  p.out <- matrix(, ncol=3, nrow=length(pm.minus))
  p.out[,] <- formatC(pm[pm.minus,], format="f", digits=2)
  rownames(p.out) <- rep(format("", width=35), times=length(pm.minus))
  p.out <- cbind(paste(pm.minus, ".", sep=""), p.out)
  colnames(p.out) <- c("Dim.", "+", "-", "+/-")
  cat("\n",format("These dimensions contributions are skewed towards (-): ", width=100, justify="centre"), "\n")
  print(p.out, quote=FALSE, right=TRUE)
}

}



#################### Balance measure for contributing modalities
# Object is a soc.ca class object
# act.dim is the number of active dimensions to be measured

balance.ctr <- function(object, act.dim=object$nd){
active <- object$active
coord <- object$coord[active, 1:act.dim]
contrib <- object$contrib[active, 1:act.dim]
pm <- matrix(, nrow=act.dim, ncol=3)
for (i in 1:act.dim){
temp <- cbind(coord[,i], contrib[,i])
temp <- temp[order(temp[,1]),]
plus <-  temp[which(temp[,1] >= 0), ]
minus <- temp[which(temp[,1] <= 0), ]
pcontrib <- sum(plus[,2])
mcontrib <- sum(minus[,2])
pm[i,1] <- pcontrib
pm[i,2] <- mcontrib
pm[i,3] <- pcontrib/mcontrib
}
colnames(pm) <- c("+ Contrib.", "- Contrib.", "Balance (+/-)")
return(pm)
}

#################### Screeplot
# Object is a soc.ca class object
# Dim is the number of dimensions included in the plot

scree <- function(object, dim=6){
set.dim  <- dim
dim <- ifelse((nrow(object$adj.inertia)<dim)==TRUE, nrow(object$adj.inertia), dim)
adj <- round(object$adj.inertia[1:dim,3], digits=1)
stars <- round(round(adj)/2)
starscree <- vector("list", set.dim)
for (i in 1:length(stars)){
starscree[[i]] <- noquote(rep("*", stars[i]))
}
return(starscree)
}



# En funktion der undersøger om der er modaliteter der styrer en dimension for meget. 
#Kritik - den vil spytte noget ud i et væk, hvis ikke den holdes nede til kun at undersøge et meget begrænset subset. Måske skal den gemmes til senere.
# Modkritik: Hvis en modalitet opsnapper al inertien på en dimension, så behøver de andre dimensioner ikke at tage højde for den på samme måde og det kan give et skævt kort.??

############################ The most contributing modalities
contribution <- function(x, dim=1){
ctr <- round(1000*x$contrib[,dim])
cor <- round(1000*x$cor[,dim])
coord <- round(x$coord[,dim], digits=2)
names <- x$names
av.ctr <- unlist(x$average.contrib[dim])
out <- data.frame(ctr[av.ctr], cor[av.ctr], coord[av.ctr])
rownames(out) <- names[av.ctr]
ctr.lab <- paste("Ctr. dim:", dim)
cor.lab <- paste("Cor. dim:", dim)
coord.lab <- paste("Coord. dim:", dim)
colnames(out)<-c(ctr.lab, cor.lab, coord.lab)
out <- out[order(-out[,1]), ]
as.matrix(out)
# Returns the modalities with above average contribution to the selected dimension
# x is a soc.ca object
# dim is the included dimensions
# Ctr is the contribution in 1000
# Cor is the correlation with the dimension
# Coord is the principal coordinate
}

############################## The most contributing modalities according to direction on dimension
# x is a soc.ca object
# dim is the dimension

# Er ikke den bedste implementation - jeg vil helst have dem ved siden af hinanden og uden det latterlige nummer. M?ske skulle der ogs? v?re akselabels? M?ske istedetfor det t?belige nummer?
tab.dim <- function(x, dim=1){
ctr <- round(1000*x$contrib[,dim])
coord <- round(x$coord[,dim], digits=2)
names <- x$names
av.ctr <- unlist(x$average.contrib[dim])
out <- data.frame(ctr[av.ctr], coord[av.ctr])
rownames(out) <- names[av.ctr]
ctr.lab   <- paste("Ctr. dim:", dim)
coord.lab 	<- paste("Coord. dim:", dim)
colnames(out)	<-c(ctr.lab, coord.lab)
out 		<- out[order(-out[,1]), ]
out.label 	<- c(ctr.lab, coord.lab)
outminus	<- out[which(out[,2]<=0),]
outplus 	<- out[which(out[,2]>=0),]
out <- rbind(outplus, out.label, outminus)
noquote(as.matrix(out))
}

