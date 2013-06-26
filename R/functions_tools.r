# Tools, exports and other helpful functions


#' Export from soc.mca
#'
#' Export objects from the soc.ca package to csv files.
#' @param object is a soc.mca class object or a contribution class object 
#' @param dim is the dimensions to be exported
#' @param file is the path and name of the .csv values are to be exported to
#' @return A .csv file with various values in UTF-8 encoding
#' @seealso \link{soc.mca}, \link{contribution}
#' @export

export <- function(object, file="export.csv", dim=1:5) {
  if (is.matrix(object)==TRUE|is.data.frame(object)==TRUE){
    write.csv(object, file, fileEncoding="UTF-8")}
    
  # Export soc.ca
    if ((class(object)=="tab.variable")==TRUE){
      
      ll    <- length(object)
      nam   <- names(object)
      a     <- object[[1]]
      coln  <- ncol(a)
      line  <- c(rep("", coln))
      line2 <- c(rep("", coln))
      a     <- rbind(line, a, line2)      
      
    for (i in 2:ll){
      line <- c(rep("", coln))
      line2 <- c(rep("", coln))
      a <- rbind(a,line, object[[i]], line2)
      line2  <- c(rep("", coln))
    }
    rownames(a)[rownames(a)=="line"] <- nam
    rownames(a)[rownames(a)=="line2"] <- ""
    out <- a
    write.csv(out, file, fileEncoding="UTF-8")  
    }
    # Export soc.ca
    if ((class(object)=="soc.mca")==TRUE){
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
  
}

#' Invert
#' 
#' Invert one or more axis of a correspondence analysis. The principal coordinates of the analysis are multiplied by -1.
#' @details This is a convienient function as you would have to modify coord.mod, coord.ind and coord.sup in the soc.mca object. This is more likely to provoke human errors.
#' 
#' @param x is a soc.mca object
#' @param dim is the dimensions to be inverted
#' @seealso \link{soc.mca}, \link{add.to.label}
#' @export

invert <- function(x, dim=1) {
  x$coord.mod[,dim] <- x$coord.mod[,dim] * -1
  x$coord.ind[,dim] <- x$coord.ind[,dim] * -1
  x$coord.sup[,dim] <- x$coord.sup[,dim] * -1
  return(x)
}