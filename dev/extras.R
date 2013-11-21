
#' Add a new layer of points on top of an existing plot with output from the cut.min function
#' @param x a matrix created by the cut.min function
#' @param p is a ggplot object, preferably from one of the mapping functions in soc.ca
#' @param label if TRUE the labels of points will be shown

add.count <- function(x, p=map.ind(object), label=TRUE, ...){
  p <- p + geom_point(data=x, x=x$X, y=x$Y, ...) + geom_path(data=x, x=x$X, y=x$Y, ...)
  if (identical(label, TRUE)) p <- p + geom_text(data=x, x=x$X, y=x$Y, label=x$label, vjust=0.2, ...)
}

#' Add a path along an ordered variable onto a map
#'
#' Plot a path along an ordered variable. If the variable is numerical it is cut into groups by the \link{cut.min} function.
#'  
#' @param object is a soc.ca result object
#' @param x is an ordered vector, either numerical or factor
#' @param map is a plot object created with one of the mapping functions in the soc.ca package
#' @param dim is the two dimensions plotted
#' @param label if TRUE the label of the points are shown
#' @param min.size is the minimum size given to the groups of a numerical variable, see \link{cut.min}.
#' @param ... further arguments are passed onto geom_path, geom_point and geom_text from the ggplot2 package
#' @export
#' example(soc.mca)
#' map <- map.ind(result, point.colour=as.numeric(sup$Age)) + scale_color_continuous(high="Red", low="yellow")
#' add.path(result, sup$Age, map)

# Find ud af om ... kan give forskellige argumenter til forskellige funktioner.
add.path  <- function(object, x, map = map.ind(object, dim), dim=c(1,2), label=TRUE, min.size=length(x)/10, ...){
  
  x.c                        <- x
  if (is.numeric(x)) x.c     <- cut.min(x, min.size=min.size) 
  
  x.av    <- average.coord(object=object, x=x.c, dim=dim)  
  map.p   <- add.count(x.av, map, label, ...) 
  map.p
}

######################################################################
## Interactive plot


### Googlevis
library(googleVis)
library(soc.ca)

example(soc.mca)

gdat <- data.frame(result$names.mod, result$coord.mod[,1:2], result$freq.mod , result$variable, result$ctr.mod[,1])
rownames(gdat) <- result$names.mod
colnames(gdat) <- c("Names", "Dim1", "Dim2", "Freq", "Variable", "Ctr.1")

gopt <- list(gvis.editor="Edit")

nif <- gvisBubbleChart(gdat, idvar="Names", xvar="Dim1", yvar="Dim2", sizevar="Freq", colorvar="Variable", options=gopt)

plot(nif)

#### Gridsvg

#### Animint
library(soc.ca)
library(reshape)
example(soc.mca)
p  <- map.ind(result)

interactive.points <- data.plot(result, "ind", dim=c(1,2))

des.data <- data.frame(names=result$names.ind, result$ctr.ind[,1:5])

des.data <- melt(des.data, id.vars="names")

q <- ggplot() + geom_line(data=des.data, aes(x=value, y=variable, showSelected=names)) + coord_flip()

q <- q + make_text(des.data, max(des.data$value) * 1.1, 3, "names")

p <- p  + geom_point(data=interactive.points, aes(x=x, y=y, clickSelects=names))
p <- p  + geom_point(data=interactive.points, aes(x=x, y=y, showSelected=names), color="red")

gg2animint(list(plot1=p, plot2=q), out.dir = "test.p")




