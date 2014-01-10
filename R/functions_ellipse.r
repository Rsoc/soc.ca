######################### ELLIPSES

#' Concentration ellipses
#' 
#' Add ellipses for each level in a factor to a plot made from a \link{soc.ca} 
#' object.
#' @param object is a \link{soc.ca} class object
#' @param ca.plot is a plot made from a \link{soc.ca} object
#' @param variable is a factor of the same length and in the same order as the 
#'   active varibles used for the \link{soc.ca} object.
#' @param ellipse.label if TRUE the labels are included in the map
#' @param line.color defines the color of the ellipses
#' @param label.size defines the size of the labels
#' @return a plot with a concentration ellipse containing 80\% of the 
#'   individuals for each modality
#' @seealso \link{map.ind}, \link{map.ctr}
#' @examples
#' example(soc.mca)
#' map <- map.ind(result)
#' map.ellipse(result, map, active[,2])
#' @export

map.ellipse <- function(object, ca.plot = map.ind(object), variable, ellipse.label = TRUE,
                        line.color = "black", label.size = 4){ 

variable    <- as.factor(variable) 
dim         <- ca.plot$dimensions 
id.coord    <- object$coord.ind[, dim]
lev.var     <- levels(variable)
id          <- object$names.ind

ellipse.data <- list()
for (i in 1:nlevels(variable)){
binvar            <- which(variable == lev.var[i])
id.coord.var      <- id.coord[binvar, ]
label             <- lev.var[i]
el.coord          <- ellipse.coord(id, id.coord.var)
el.axis           <- ellipse.axis(el.coord)
el.origo          <- ellipse.origo(el.axis)
el.origo          <- data.frame(x = el.origo[1], y = el.origo[2], label)
ellipse.data[[i]] <- list(label = label, el.coord = el.coord, el.axis = el.axis, el.origo = el.origo)
}

ellipse.data      <- ellipse.data

# Her plottes.

for (i in 1:nlevels(variable)){
# Ellipserne
ca.plot <- ca.plot + geom_path(data = ellipse.data[[i]]$el.coord, aes(x = x, y = y), colour = line.color, size = 0.33)
# Akserne
ca.plot <- ca.plot + geom_path(data = ellipse.data[[i]]$el.axis[[1]], aes(x = x, y = y),
                               colour = line.color, size = 0.33, linetype = 2, na.rm = TRUE)
ca.plot <- ca.plot + geom_path(data = ellipse.data[[i]]$el.axis[[2]], aes(x = x, y = y),
                               colour = line.color, size = 0.33, linetype = 2, na.rm = TRUE)
# Origo
ca.plot <- ca.plot + geom_point(data = ellipse.data[[i]]$el.origo, aes(x = x, y = y),
                                size = 3, shape = 21, fill = "white", colour = line.color, na.rm = TRUE)
# Origo label
if(identical(ellipse.label, TRUE)) ca.plot <- ca.plot + geom_text(aes(x = x, y = y, label = label), data = ellipse.data[[i]]$el.origo, size = label.size, hjust = -0.15, fontface = "italic")
}

ca.plot
}

################### Ellipse akser funktionen
ellipse.axis <- function(el.dat){
n            <- length(na.omit(el.dat[, 1]))
p.pairs      <- el.dat[1, ]
  for (i in 1:(n/2)){
p.pairs[i, ] <- c(i, i + (n / 2))
}
p.pairs      <- cbind(p.pairs, rep(0, n / 2))
colnames(p.pairs) <- c("x", "y", "distance")

for (i in 1:(n / 2)){
a             <- el.dat[i, ]
b             <- el.dat[i + (n / 2), ]
p.pairs[i, 3] <- ((b$x - a$x)^2 + (b$y - a$y)^2)^0.5
}
p.min         <- which.min(p.pairs[, 3])
p.max         <- which.max(p.pairs[, 3])

mintemp       <- p.pairs[p.min, 1:2]
minpath       <- as.data.frame(el.dat[c(mintemp$x, mintemp$y), ], row.names = c("1","2"))


maxtemp       <- p.pairs[p.max, 1:2]
maxpath       <- as.data.frame(el.dat[c(maxtemp$x, maxtemp$y), ], row.names = c("1","2"))

ellipse.axis.points <- list(maxpath, minpath)
ellipse.axis.points
}

############## Ellipse.coord
ellipse.coord <- function(id, id.coord.var){
id.var        <- as.data.frame(id.coord.var)
colnames(id.var) <- c("x", "y")

xdata         <- with(as.data.frame(id.var), xyTable(x, y))
xframe        <- cbind.data.frame(x = xdata$x, y = xdata$y, n = xdata$number)
data.ell      <- as.data.frame(with(id.var, ellipse(cor(x, y), 
                                    scale = c(sd(x), sd(y)), 
                                    centre = c(mean(x), mean(y)), npoints = 1000)))
data.ell
}

#############################3#### Ellipse origo
ellipse.origo <- function(el.axis){
 temp         <- el.axis[[1]][c(1, 2), ]
 x1           <- temp[1, 1]
 x2           <- temp[2, 1]
 y1           <- temp[1, 2]
 y2           <- temp[2, 2]
el.origo      <- c((x1 + x2) / 2, (y1 + y2) / 2)
el.origo
}
