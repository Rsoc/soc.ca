# Tools, exports and other helpful functions

## Export
# Der er fuckup i contrib - det er fedest hvis output har den rigtige l?ngde. Fuckker
# Standard er ikke lavet endnu - men er oplagt bare standard.koord istedetfor
# Round er ikke lavet endnu
export <- function(x, file="export.csv", nd=1:5, standard=FALSE, Round=TRUE) {
if (is.matrix(x)==TRUE|is.data.frame(x)==TRUE){
write.csv(x, file)}
else{
coord.x <- round(x$coord[,nd], digits=2)
colnames(coord.x) <- paste("Principal coord. dim:", nd)
row <-nrow(coord.x)

ctr.x <- matrix(, ncol=length(nd), nrow=row)
ctr.x[1:nrow(x$contrib),] <- round(1000*x$contrib[,nd])
colnames(ctr.x) <- paste("Contribution dim:", nd)

cor.x <- matrix(, ncol=length(nd), nrow=row)
cor.x[1:nrow(x$cor),] <- round(x$cor[,nd], digits=2)
colnames(cor.x) <- paste("Correlation dim:", nd)

inertia <- vector(mode="numeric", length=row)
inertia[1:length(x$inertia)] <- x$inertia

ex <-cbind(inertia, coord.x, ctr.x, cor.x)
rownames(ex) <- x$names
write.csv(ex, file)
}}

### Vis koordinater
# Col navnene er ikke gode
coordinates <- function(x, dim=c(1,2,3)){
# coord <- x$coord[,dim]
# colnames(coord) <- rep("coord", length(dim))
# ctr <- round(1000*x$contrib[,dim])
# cor <- round(1000*x$cor[,dim])
# ex <- data.frame(coord, ctr, cor)
nd <- dim
  coord.x <- round(x$coord[,nd], digits=2)
colnames(coord.x) <- paste("Principal coord. dim:", nd)
row <-nrow(coord.x)

ctr.x <- matrix(, ncol=length(nd), nrow=row)
ctr.x[1:nrow(x$contrib),] <- round(1000*x$contrib[,nd])
colnames(ctr.x) <- paste("Contribution dim:", nd)

cor.x <- matrix(, ncol=length(nd), nrow=row)
cor.x[1:nrow(x$cor),] <- round(x$cor[,nd], digits=2)
colnames(cor.x) <- paste("Correlation dim:", nd)

inertia <- vector(mode="numeric", length=row)
inertia[1:length(x$inertia)] <- x$inertia

ex <-cbind(inertia, coord.x, ctr.x, cor.x)
rownames(ex) <- x$names
  
  
  
  fix(ex)
}

## Vend
# Skal også vende standard.coord !
invert <- function(x, dimension=1) {
x$coord[,dimension] <- x$coord[,dimension] * -1
x
}

