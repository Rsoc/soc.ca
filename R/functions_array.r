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
#' @param x is a plot object, created by one of the mapping functions in the soc.ca package or any other ggplot2 created plot
#' @param ncol is the number of columns the plots are arranged into
#' @param title is the main title of all the plots
#' @examples
#' \dontrun{
#' example(soc.mca)
#' map.array(list(map.ind(result), map.mod(result)), ncol=2)
#' }
#' @export
map.array <- function(x, ncol=1, title=""){
  
do.call(grid.arrange, c(x, ncol=ncol, main=title))
}


#' Ellipse array
#'
#' Create seperate maps with ellipses for each level in a factor arranged in an array.
#' @param object is a soc.mca or soc.csa class object
#' @param variable is a factor of the same length as the data.frame used to create object
#' @param dim indicates what dimensions to plot and in which order to plot them
#' @param draw.ellipses if set as TRUE ellipses are drawn
#' @param ncol is the number of columns the plots are arranged into
#' @param titles is a vector of the same length as the number of levels in variable. These are the titles given to each subplot
#' @param main.title is the main title for all the plots
#' @param ... sends any further arguments to \link{map.select}. 
#' @examples
#' \dontrun{
#' example(soc.mca)
#' map.ellipse.array(result, active[,1])
#' }
#' @export
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
  
  p          <- map.select(object, dim=c(1,2), list.ind=ind.ind, map.title=titles[i], point.label=FALSE, ...)
  
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
  map.array(list.of.maps, ncol=ncol, title=main.title)
}
