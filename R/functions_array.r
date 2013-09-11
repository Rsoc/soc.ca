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
#' @param fixed.coord if TRUE the limits of all plots are set to the same as the largest plot
#' @param padding is the distance between the most extreme position and the axis limit
#' @examples
#' \dontrun{
#' example(soc.mca)
#' map.array(list(map.ind(result), map.mod(result)), ncol=2)
#' }
#' @export
map.array <- function(x, ncol=1, title="", fixed.coord=TRUE, padding=0.15){

  if (identical(fixed.coord,TRUE)) x <- fix.coords(x, padding=padding)
  
  
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
    
  for (i in 1:length(var.levels)){
  ind.ind     <- which(variable == var.levels[i]) 
  ellipse.ind <- variable == var.levels[i]
  ellipse.ind[ellipse.ind == FALSE] <- NA 
  
  p          <- map.select(object, dim=c(1,2), list.ind=ind.ind, map.title=titles[i], point.label=FALSE, ...)
  
  if(identical(draw.ellipses, TRUE)) p    <- map.ellipse(object, p, ellipse.ind, label=FALSE)
  list.of.maps[[i]] <- p
  
  }
  
  # Standardize the coordinates
  list.of.maps <- fix.coords(list.of.maps)
  # Plot the maps
  map.array(list.of.maps, ncol=ncol, title=main.title)
}

#################################################################################################
#' Array of several CSA
#' 
#' Creates an array of Class Specific Mulitple Correspondence analysis's
#' 
#' @param object is a \link{soc.mca} result object
#' @param variable is a factor with the same order and length as those used for the active modalities in object
#' @param dim indicates what dimensions to plot and in which order to plot them
#' @param ncol is the number of columns the plots are arranged into
#' @param titles is a vector of the same length as the number of levels in variable. These are the titles given to each subplot
#' @param main.title is the main title for all the plots
#' @param FUN is the mapping function used for the plots; \link{map.active}, \link{map.ctr}, \link{map.ind}, \link{map.select}
#' @param fixed.coord if TRUE the limits of all plots are set to the same as the largest plot
#' @param ... sends any further arguments to the mapping functions
#' @export  
#' @examples
#' \dontrun{
#' example(soc.csa)
#' object   <- result
#' variable <-  active[,1]
#' map.csa.all(object, variable)
#' map.csa.all(object, variable, FUN=map.ctr, ctr.dim=1)
#' }

map.csa.all <- function(object, variable, dim=c(1,2), ncol=2, FUN=map.ind, fixed.coord=TRUE, main.title="", titles=levels(variable),...){
  
  csa.list    <- csa.all(object, variable)
  csa.results  <- csa.list$results
  plot.list    <- lapply(csa.results, FUN, dim=dim, ...) 
  
  # Map titles
  
  if(length(titles) > 1){
  for(i in 1:length(titles)){
  plot.list[[i]] <- plot.list[[i]] + ggtitle(titles[i])
  }
  }
  
  # Coord standardization
  if (identical(fixed.coord, TRUE)) plot.list <- fix.coords(plot.list)  
    
  map.array(plot.list, ncol=2, title=main.title)
}

#######################################################################3
### Standardize coordinates

fix.coords <- function(plot.list, padding=0.15){

minimums.x    <- vector(, length=length(plot.list)) 
minimums.y    <- vector(, length=length(plot.list))
maximums.x    <- vector(, length=length(plot.list))
maximums.y    <- vector(, length=length(plot.list))

for ( i in 1:length(plot.list)){
p                <- plot.list[[i]]
minimums.x[i]    <- p$ca.scales$lim.min.x
minimums.y[i]    <- p$ca.scales$lim.min.y
maximums.x[i]    <- p$ca.scales$lim.max.x
maximums.y[i]    <- p$ca.scales$lim.max.y
}

# Standardize the coordinates
xlim <- c(max(maximums.x)+padding, min(minimums.x)-padding)
ylim <- c(max(maximums.y)+padding, min(minimums.y)-padding) 

for ( i in 1:length(plot.list)){
  plot.list[[i]]  <- plot.list[[i]] + coord_fixed(xlim=xlim, ylim=ylim)
}
return(plot.list)
}