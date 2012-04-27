# Tools, exports and other helpful functions


## Export
#! Der er fuckup i contrib - det er fedest hvis output har den rigtige l?ngde. Fuckker
#! Standard er ikke lavet endnu - men er oplagt bare standard.koord istedetfor
#! Round er ikke lavet endnu
#! Define classes
export <- function(object, file="export.csv", dim=1:5) {
  if (is.matrix(object)==TRUE|is.data.frame(object)==TRUE){
    write.csv(object, file, fileEncoding="UTF-8")}
  else{
    coord.mod     <- object$coord.mod[,dim]
    coord.sup     <- object$coord.sup[,dim]
    coord.ind     <- object$coord.ind[,dim]
    names.mod		  <- object$names.mod
    names.sup  	  <- object$names.sup
    names.ind     <- object$names.ind
    coord         <- round(rbind(coord.mod, coord.sup, coord.ind), 2)
    names         <- c(names.mod, names.sup, names.ind)
    rownames(coord) <- names
    
    ctr.mod       <- object$ctr.mod[,dim]
    ctr.sup       <- matrix(nrow=nrow(coord.sup), ncol=ncol(coord.sup))
    ctr.ind       <- object$ctr.ind[,dim]
    ctr           <- round(1000*rbind(ctr.mod, ctr.sup, ctr.ind))
    
    cor.mod       <- round(100*object$cor.mod[,dim], 1)
    cor.sup       <- matrix(nrow=nrow(coord.sup), ncol=ncol(coord.sup))
    cor.ind       <- matrix(nrow=nrow(coord.ind), ncol=ncol(coord.ind))
    cor           <- rbind(cor.mod, cor.sup, cor.ind)
    
    out           <- cbind(coord, ctr, cor)
    colnames(out) <- c(paste("Coord:", dim), paste("Ctr:", dim), paste("Cor:", dim))
    write.csv(out, file, fileEncoding="UTF-8")
  }
  
  # Export objects from the soc.ca package to csv files.
  # export(object, file="export.csv", dim=1:5)
  # object is a soc.ca class object or a contribution class object 
  # dim is the dimensions to be exported
}

## Invert

invert <- function(x, dim=1) {
  x$coord.mod[,dim] <- x$coord.mod[,dim] * -1
  x$coord.ind[,dim] <- x$coord.ind[,dim] * -1
  x$coord.sub[,dim] <- x$coord.sub[,dim] * -1
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

