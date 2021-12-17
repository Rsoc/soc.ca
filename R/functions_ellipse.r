######################### ELLIPSES
#' Concentration ellipses
#' 
#' Add ellipses for each level in a factor to a plot made from a \link{soc.ca} 
#' object.
#' @param object is a \link{soc.ca} class object.
#' @param ca.plot is a plot made from a \link{soc.ca} object.
#' @param variable is a factor of the same length and in the same order as the 
#'   active varibles used for the \link{soc.ca} object.
#' @param ellipse.label if TRUE the labels are included in the map.
#' @param ellipse.color defines the color of the ellipses. If "default" the globally defined default colors are used. Ellipse.color can be either length of 1 or equal to the number of drawn levels.
#' @param label.size defines the size of the labels.
#' @param draw.levels indicates the levels in the variable for which a ellipse is drawn.
#' @param ellipse.line defines the type of line used for the ellipses.
#' @return a plot with a concentration ellipse containing 80\% of the 
#'   individuals for each modality.
#' @seealso \link{map.ind}, \link{map.ctr}
#' @examples
#' example(soc.ca)
#' map <- map.ind(result)
#' map.ellipse(result, map, active[,2])
#' @export

map.ellipse <- function(object, ca.plot = map.ind(object), variable, ellipse.label = TRUE,
                        ellipse.color = "default", label.size = 4, draw.levels = 1:nlevels(variable), ellipse.line = "solid"
){
  dim         <- ca.plot$dimensions 
  id.coord    <- as.data.frame(object$coord.ind[, dim])
  lev.var     <- levels(variable)[draw.levels]
  levels(variable)[-which(levels(variable) %in% lev.var)] <- NA
  id          <- object$names.ind
  
  if(identical(ellipse.color, "default")){
    ellipse.color        <- getOption(x = "soc.ca.colors")
    ellipse.color        <- ellipse.color[1:length(draw.levels)]
  }
  
  ellipse.colors   <- vector(length=length(draw.levels))
  ellipse.colors[] <- ellipse.color
  coord.list       <- split(id.coord, variable)
  
  ellipse.data              <- function(coord, id){
    out.list                <- list()
    out.list$ellipse.coord  <- ellipse.coord(id, coord)
    out.list$ellipse.axis   <- ellipse.axis(out.list$ellipse.coord)
    out.list$ellipse.origo  <- ellipse.origo(out.list$ellipse.axis)
    out.list
  }
  
  ellipse.list              <- lapply(coord.list, ellipse.data, id = id)
  
  for ( i in 1:length(ellipse.list)){
    ellipse.list[[i]]$color <- ellipse.colors[i] 
    ellipse.list[[i]]$label <- lev.var[i]
  }
  
  
  ellipse.annotate          <- function(ellipse.data, ca.plot, ellipse.label = TRUE, label.size = 4, ellipse.line = "solid"){
    e.plot     <- ca.plot + annotate(geom = "path", x = ellipse.data$ellipse.coord[,1], y = ellipse.data$ellipse.coord[,2],
                                     color = ellipse.data$color, linetype = ellipse.line)
    e.plot     <- e.plot + annotate(geom = "line", x = ellipse.data$ellipse.axis[[1]]$x, y = ellipse.data$ellipse.axis[[1]]$y,
                                    color = ellipse.data$color, , linetype = ellipse.line)
    e.plot     <- e.plot + annotate(geom = "line", x = ellipse.data$ellipse.axis[[2]]$x, y = ellipse.data$ellipse.axis[[2]]$y,
                                    color = ellipse.data$color, linetype = ellipse.line)
    e.plot     <- e.plot + annotate(geom = "point", x = ellipse.data$ellipse.origo[[1]], y = ellipse.data$ellipse.origo[[2]],
                                    color = ellipse.data$color)
    if(identical(ellipse.label, TRUE)){
      e.plot     <- e.plot + annotate(geom = "text", x = ellipse.data$ellipse.origo[[1]], y = ellipse.data$ellipse.origo[[2]],
                                      label = ellipse.data$label, hjust = -0.15, fontface = "italic", size = label.size)
    }
    
    e.plot
  }
  
  for (i in 1:length(ellipse.list)) ca.plot <- ellipse.annotate(ellipse.list[[i]], ca.plot,
                                                                ellipse.label = ellipse.label, label.size = label.size,
                                                                ellipse.line = ellipse.line)
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


#' Calculate concentraion ellipses
#'
#' @param object 
#' @param var 
#' @param dim 
#' @param kappa 
#' @param npoints 
#'
#' @return
#' @export ellipses
#'
#' @examples
#' example(soc.mca)
#' ellipses(result, active[,1])

ellipses <- function(object, var, dim = c(1, 2), 
                     kappa = 2, npoints = 1000) {
  var <- factor(var)
  m   <- supplementary_variable(object, var)$coord[, dim]
  m[, 1] <- m[, 1] * object$eigen[dim[1]] %>% sqrt()
  m[, 2] <- m[, 2] * object$eigen[dim[2]] %>% sqrt()
  v <- supplementary_variable(object, var)$var[1:length(levels(var)), dim]
  
  varb <- var
  wt <- object$weight
  
  c <- vector(length = length(levels(var)))
  i <- 1
  for (i in 1:length(c)) {
    temp1 <- matrix(object$coord.ind[varb == levels(varb)[i], 
                                  dim], ncol = 2)
    temp1[, 1] <- temp1[, 1] - m[i, 1]
    temp1[, 2] <- temp1[, 2] - m[i, 2]
    temp2 <- wt[varb == levels(varb)[i]] * temp1[, 1] * 
      temp1[, 2]
    c[i] <- sum(temp2)/sum(wt[varb == levels(varb)[i]])
  }
  g1 <- 0.5 * (v[, 1] + v[, 2]) + 0.5 * sqrt((v[, 1] - v[, 
                                                         2])^2 + 4 * c^2)
  g2 <- 0.5 * (v[, 1] + v[, 2]) - 0.5 * sqrt((v[, 1] - v[, 
                                                         2])^2 + 4 * c^2)
  sa1 <- kappa * sqrt(g1)
  sa2 <- kappa * sqrt(g2)
  alph <- atan((g1 - v[, 1])/c)
  theta <- seq(0, 2 * pi, length = npoints)
  
  modalities <- list()
  for (i in 1:length(levels(varb))) {
    x0 <- m[i, 1]
    y0 <- m[i, 2]
    alpha <- alph[i]
    a <- sa1[i]
    b <- sa2[i]
    x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * 
      sin(alpha)
    y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * 
      cos(alpha)
    z1 <- x0 + c(a, b, a, b) * cos(alpha + c(0, pi/2, 
                                             pi, 3 * pi/2))
    z2 <- y0 + c(a, b, a, b) * sin(alpha + c(0, pi/2, 
                                             pi, 3 * pi/2))
    z <- cbind(z1, z2)
    
    origo <- data.frame(x = x0, y = y0)  
    pc1 <- data.frame(z[c(1, 3), ])
    colnames(pc1) <- c("x", "y")
    pc2 <- data.frame(z[c(2, 4), ])   
    colnames(pc2) <- c("x", "y")
    a <- sqrt((pc1$x[1]- pc1$x[2])^2 + (pc1$y[2]- pc1$y[2])^2)/2
    b <- sqrt((pc2$x[1]- pc2$x[2])^2 + (pc2$y[1]- pc2$y[2])^2)/2
    
    slope <- (pc1$y[2] - pc1$y[1]) / (pc1$x[2] - pc1$x[1])
    angle <- atan(slope) * 180/pi
    if( a > b) {
      c <- 1-(b^2 / a^2)
    }else{
      c <- 1-(a^2 / b^2)
    }
    
    out <- list()
    out$principal_dim_1 <- pc1
    out$principal_dim_2 <- pc2
    out$origo           <- origo
    out$ellipse        <- tibble(x, y)
    out$`eccentricity coefficient` <- c
    out$`orientation in plane` <- angle
    modalities[[i]] <- out
    
  }
  names(modalities) <- levels(varb)

  modalities <- modalities %>% map(~.x %>% enframe() %>% pivot_wider(names_from = name, values_from = value)) %>% bind_rows(.id = "Category")
  modalities$`eccentricity coefficient` <- modalities$`eccentricity coefficient` %>% as.numeric()
  modalities$`orientation in plane` <- modalities$`orientation in plane` %>% as.numeric()
  modalities
}


