#######

# library(soc.ca)
# library(gridExtra)
# 
# example(soc.mca)
# a <- map.ind(result)
# b <- map.ctr(result)
# c <- map.sup(result)
# x <- list(a,b,c)


#' Facetplot from list
#'
#' This function takes a list of plot objects and arranges them into a grid

map.array <- function(x, ncol=1, title=""){
  
do.call(grid.arrange, c(x, ncol=ncol, main=title))
}


#' Ellipse array
#'
#' Create seperate maps with ellipses for each level in a factor arranged in an array.
#' @export
#' 


map.ellipse.array <- function(object, variable, dim=c(1,2), draw.ellipses=TRUE, ncol=2, titles=levels(variable), main.title="",  ...){
  
  var.levels    <- levels(variable)
  list.of.maps  <- list()
  minimums.x    <- vector(, length=length(var.levels)) 
  minimums.y    <- vector(, length=length(var.levels))
  maximums.x    <- vector(, length=length(var.levels))
  maximums.y    <- vector(, length=length(var.levels))
  
  for (i in 1:length(var.levels)){
  ind.ind     <- which(variable == var.levels[i]) 
  ellipse.ind <- variable == var.levels[i]
  ellipse.ind[ellipse.ind == FALSE] <- NA 
  
  p          <- map.select(object, dim=c(1,2), list.ind=ind.ind, map.title=titles[i], point.label=FALSE)
  
  if(identical(draw.ellipses, TRUE)) p    <- map.ellipse(object, p, ellipse.ind, label=FALSE)
  list.of.maps[[i]] <- p
  
  minimums.x[i]    <- p$ca.scales$lim.min.x
  minimums.y[i]    <- p$ca.scales$lim.min.y
  maximums.x[i]    <- p$ca.scales$lim.max.x
  maximums.y[i]    <- p$ca.scales$lim.max.y
  }
  
  # Standardize the coordinates
  xlim <- c(max(maximums.x), min(minimums.x))
  ylim <- c(max(maximums.y), min(minimums.y))
  
  for ( i in 1:length(var.levels)){
  list.of.maps[[i]]  <- list.of.maps[[i]] + coord_fixed(xlim=xlim, ylim=ylim)
  }
  
  # Plot the maps
  map.array(list.of.maps, ncol=ncol, title=title)
}

map.ellipse.array(result, active[,1])
