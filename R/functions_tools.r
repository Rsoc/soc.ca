# Tools, exports and other helpful functions

#' Cut a continious variable into categories with a specified minimum
#' 
#' Many continious variables are very uneqaully distributed, often with many individuals in the lower categories and fewer in the top.
#' As a result it is often difficult to create groups of equal size, with unique cut-points.
#' By defining the wanted minimum of individuals in each category, but still allowing this minimum to be surpassed, it is easy to create ordinal variables from continious variables. 
#' The last category will not neccessarily have the minimum number of individuals.
#' 
#' @param x is a continious numerical variable
#' @param min.size is the minimum number of individuals in each category
#' @return a numerical vector with the number of each category
#' @export
#' @examples
#' a <- 1:1000
#' table(cut.min(a))
#' b <- c(rep(0, 50), 1:500)
#' table(cut.min(b, min.size=20))

cut.min <- function(x, min.size=length(x)/10){
  
  x.na <- x[is.na(x) == FALSE]
  p.x <- cumsum(prop.table(table(x.na)))
  t.x <- cumsum(table(x.na))
  bm  <- cbind(table(x.na),t.x)
  dif <- vector(length=nrow(bm))
  for (i in 2:length(dif)) dif[i] <- bm[i,2] - bm[i-1,2]
  dif[1] <- bm[1, 2]
  bm <- cbind(bm, dif)
  
  group <- vector(length=nrow(bm))
  g <- 1 
  collect <- 0
  for (i in 1:nrow(bm)){
    if (dif[i] >= min.size | collect >= min.size-1){ # collect skal måske være mindre end min.size
      group[i] <- g
      g <- g + 1
      collect <- 0
    }else{
      group[i] <- g
      collect  <- collect + dif[i]
    }
  }
  
  x.group <- vector(length=length(x))
  group.levels <- as.numeric(levels(as.factor(group)))
  values <- as.numeric(rownames(bm))
  levs   <- vector()
  # Assigning group to the original x
  for (i in 1:length(group.levels)){
    g   <- group.levels[i]
    val <- values[group == g]
    g   <- paste(min(val), ":", max(val), sep="")
    x.group[x %in% val]  <- g
    levs                 <- c(levs, paste(min(val), ":", max(val), sep=""))
  }
  x.group[is.na(x)] <- NA
  factor(x.group, labels=levs, ordered=TRUE)
}

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
#   if(inherits(x, "soc.csa")==TRUE) {
#     x$cor.dim[dim,] <- x$cor.dim[dim,] * -1
#     x$cosines[dim,] <- x$cosines[dim,] * -1
#   }
  return(x)
}