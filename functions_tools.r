# Tools, exports and other helpful functions


## Export
#! Der er fuckup i contrib - det er fedest hvis output har den rigtige l?ngde. Fuckker
#! Standard er ikke lavet endnu - men er oplagt bare standard.koord istedetfor
#! Round er ikke lavet endnu
#! Define classes
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
}

# Export objects from the soc.ca package to csv files.
# export(x, file="export.csv", nd=1:5, standard=FALSE, Round=TRUE)
# x is a soc.ca class object or a contribution class object 
# nd is the dimensions to be exported
# standard if TRUE standard coordinates are exported if FALSE principal coordinates are used instead.
# Round a number for the amount of digits in the exported values if set to NULL values will not be rounded.
}

### Vis koordinater
# Col navnene er ikke gode
# Skal kunne sortere på typer af koordinater
# Skelne mellem standard og principal

coordinates <- function(x, dim=c(1,2,3)){

nd                  <- dim
coord.x             <- round(x$coord[,nd], digits=2)
colnames(coord.x)   <- paste("Principal coord. dim:", nd)
row                 <-nrow(coord.x)

ctr.x               <- matrix(, ncol=length(nd), nrow=row)
ctr.x[1:nrow(x$contrib),] <- round(1000*x$contrib[,nd])
colnames(ctr.x)     <- paste("Contribution dim:", nd)

cor.x               <- matrix(, ncol=length(nd), nrow=row)
cor.x[1:nrow(x$cor),] <- round(x$cor[,nd], digits=2)
colnames(cor.x)     <- paste("Correlation dim:", nd)

inertia             <- vector(mode="numeric", length=row)
inertia[1:length(x$inertia)] <- x$inertia

ex                  <-cbind(inertia, coord.x, ctr.x, cor.x)
rownames(ex)        <- x$names
  fix(ex)

# Show the coordinates of a correspondence analysis in an editor for easy analysis. No changes made in the editor are saved.
# coordinates(x, dim=c(1,2,3))
# x is a soc.ca object
# dim is the dimensions of the analysis to be shown

}

## Vend
# Skal også vende standard.coord !

invert <- function(x, dim=1) {
x$coord[,dim] <- x$coord[,dim] * -1
return(x)

# Invert one or more axis of a correspondence analysis. The principal coordinates of the analysis are multiplied by -1
# invert(x, dim=1)
# x is a soc.ca object
# dim is the dimensions to be inverted
}

#### Set passive
set.passive <- function(x){
    formals(soc.ca)$passive <<- as.character(x)
# Defines the default for the passive modalities in a soc.ca.
# x is the list of passive modalities
    }

