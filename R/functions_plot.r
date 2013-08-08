### Plot for all modalitites
# Dev init:
# object <- result
# dim=c(1,2)
# map.title="all"
# labelx=NULL
# labely=NULL
# point.label=TRUE
# point.shape=21
# point.size="freq"
# text.size=3.3
# colour="black"
# ctr.dim = 1


#' Map all modalities
#' 
#' Creates a map of all active and supplementary modalities on two selected dimension.
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param dim gives the dimensions in the order they are to be plotted. The first number defines the horizontal axis and the second number defines the vertical axis.
#' @param map.title is the title of the map. If set to its default the standard title is used.
#' @param labelx is the label of the horizontal axis. If set to NULL a standard label is used.
#' @param labely is the label of the vertical axis. If set to NULL a standard label is used.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.size is a numerical value defining the size of the points. If set to its default, the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param colour defines the colour of both label and the line of the points. The fill is set to a standard grey.
#' @examples
#' example(soc.mca)
#' map.mod(result)
#' map.mod(result, dim=c(3,2), point.size=2)
#' @export 
map.mod  <- function(object, dim=c(1,2), map.title="mod", labelx=NULL, labely=NULL, point.label=TRUE, point.shape="variable", point.size="freq", text.size=3.3, colour="black"){

gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="mod", dim, ctr.dim=NULL, point.size=point.size) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title) # Plot title
scales      <- breaksandscales(gg.data) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc, 
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label, point.size=point.size)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.shape=point.shape, colour=colour) # Plot
t.plot      <- b.plot + theme_min()
t.plot$dimensions <- dim
return(t.plot)
}

#' Map the most contributing modalities
#' 
#' Creates a map of the modalities contributing above average to one or more dimensions on two selected dimension.
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param ctr.dim is dimensions from which the contribution values are taken.
#' @param dim gives the dimensions in the order they are to be plotted. The first number defines the horizontal axis and the second number defines the vertical axis.
#' @param map.title is the title of the map. If set to its default the standard title is used.
#' @param labelx is the label of the horizontal axis. If set to NULL a standard label is used.
#' @param labely is the label of the vertical axis. If set to NULL a standard label is used.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.size is a numerical value defining the size of the points. If set to its default, the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param colour defines the colour of both label and the line of the points. The fill is set to a standard grey.
#' @examples
#' example(soc.mca)
#' map.ctr(result)
#' map.ctr(result, ctr.dim=c(1,2))
#' @export 

map.ctr        <- function(object, ctr.dim=1, dim=c(1,2), map.title="ctr", labelx=NULL, labely=NULL, point.label=TRUE, point.shape="variable", point.size="freq", text.size=3.3, colour="black"){

gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="ctr", dim, ctr.dim=ctr.dim, point.size=point.size) # Data selection

axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title, ctr.dim=ctr.dim) # Plot title
scales      <- breaksandscales(gg.data) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label, point.size=point.size)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.shape=point.shape, colour=colour) # Plot
t.plot      <- b.plot + theme_min() # Theme
t.plot$dimensions <- dim 
return(t.plot)
}

#' Map the active modalities
#' 
#' Creates a map of the active modalities on two selected dimension.
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param dim gives the dimensions in the order they are to be plotted. The first number defines the horizontal axis and the second number defines the vertical axis.
#' @param map.title is the title of the map. If set to its default the standard title is used.
#' @param labelx is the label of the horizontal axis. If set to NULL a standard label is used.
#' @param labely is the label of the vertical axis. If set to NULL a standard label is used.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.size is a numerical value defining the size of the points. If set to its default, the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param colour defines the colour of both label and the line of the points. The fill is set to a standard grey.
#' @examples
#' example(soc.mca)
#' map.active(result)
#' map.active(result, dim=c(2,1))
#' map.active(result, point.size=result$ctr.mod[,1], map.title="All active modalities with size according to contribution")
#' @export 

map.active     <- function(object, dim=c(1,2), map.title="active", labelx=NULL, labely=NULL, point.label=TRUE, point.shape="variable", point.size="freq", text.size=3.3, colour="black"){

gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="active", dim, ctr.dim=NULL, point.size=point.size) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title) # Plot title
scales      <- breaksandscales(gg.data) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label, point.size=point.size)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.shape=point.shape, colour=colour) # Plot
t.plot      <- b.plot + theme_min() # Theme
t.plot$dimensions <- dim
return(t.plot)
}

#' Map the supplementary modalities
#' 
#' Creates a map of the supplementary modalities on two selected dimension.
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param dim gives the dimensions in the order they are to be plotted. The first number defines the horizontal axis and the second number defines the vertical axis.
#' @param map.title is the title of the map. If set to its default the standard title is used.
#' @param labelx is the label of the horizontal axis. If set to NULL a standard label is used.
#' @param labely is the label of the vertical axis. If set to NULL a standard label is used.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.size is a numerical value defining the size of the points. If set to its default, the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param colour defines the colour of both label and the line of the points. The fill is set to a standard grey.
#' @examples
#' example(soc.mca)
#' map.sup(result)
#' map.sup(result, dim=c(2,1))
#' map.sup(result, point.size=result$coord.sup[,4], map.title="All supplementary modalities with size according to coordinate on the 4th dimension")
#' @export 
map.sup     	<- function(object, dim=c(1,2), map.title="sup", labelx=NULL, labely=NULL, point.label=TRUE, point.shape="variable", point.size="freq", text.size=3.3, colour="black"){
gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="sup", dim, ctr.dim=NULL, point.size=point.size) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title) # Plot title
scales      <- breaksandscales(gg.data) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label, point.size=point.size)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.shape=point.shape, colour=colour) # Plot
t.plot      <- b.plot + theme_min() # Theme
t.plot$dimensions <- dim
return(t.plot)
}

#' Map the individuals of a soc.mca analysis
#' 
#' Creates a map of the individuals on two selected dimension.
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param dim gives the dimensions in the order they are to be plotted. The first number defines the horizontal axis and the second number defines the vertical axis.
#' @param map.title is the title of the map. If set to its default the standard title is used.
#' @param labelx is the label of the horizontal axis. If set to NULL a standard label is used.
#' @param labely is the label of the vertical axis. If set to NULL a standard label is used.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.variable is a factor of the samel length and order as the ones used for the soc.mca object. Each level is given its own shape.
#' @param point.size is a numerical value defining the size of the points. If set to its default, the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param legend if set to TRUE a legend is provided
#' @param colour defines the colour of the text labels
#' @param point.colour is a factor of the same length and order as the ones used for the soc.mca object. 
#' @examples
#' example(soc.mca)
#' map.ind(result)
#' map.ind(result, map.title="Each individual is given its shape according to a value in a factor", point.variable=active[,1], legend=TRUE)
#' map.ind(result, map.title="The contribution of the individuals with a new color scale defined by ggplot2", point.colour=result$ctr.ind[,1], point.shape=18) + scale_color_continuous(low="white", high="red")
#' quad   <- create.quadrant(result)
#' map.ind(result, map.title="Individuals in the space given shape and colour by their quadrant", point.variable=quad, point.colour=quad)
#' @export
map.ind         <- function(object, dim=c(1,2), map.title="ind", labelx=NULL, labely=NULL, point.label=FALSE, point.shape="variable", point.variable=NULL, point.colour=NULL, point.size=3, text.size=3.3, colour="black", legend=NULL){

gg.proc 	  <- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="ind", dim, ctr.dim=NULL, point.size=point.size, point.variable=point.variable, point.colour=point.colour) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title) # Plot title
scales      <- breaksandscales(gg.data) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label, point.size=point.size)
b.plot      <- basic.plot(gg.input, text.size=text.size, point.shape=point.shape, colour=colour, point.colour=point.colour) # Plot
t.plot      <- b.plot + theme_min() # Theme

if(is.null(legend)) t.plot <- t.plot + theme(legend.position="none")

t.plot$dimensions <- dim
return(t.plot)
}

############################### List map
#' Map select modalities and individuals
#' 
#' Creates a map of selected modalities or individuals 
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param dim gives the dimensions in the order they are to be plotted. The first number defines the horizontal axis and the second number defines the vertical axis.
#' @param map.title is the title of the map. If set to its default the standard title is used.
#' @param labelx is the label of the horizontal axis. If set to NULL a standard label is used.
#' @param labely is the label of the vertical axis. If set to NULL a standard label is used.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.size is a numerical value defining the size of the points. If set to "freq", the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param colour defines the colour of the text labels
#' @param list.mod is a numerical vector indicating which active modalities to plot. It may also be a logical vector of the same length and order as the modalities in object$names.mod.
#' @param list.sup is a numerical vector indicating which supplementary modalities to plot. It may also be a logical vector of the same length and order as the modalities in object$names.sup.
#' @param list.ind is a numerical vector indicating which individuals to plot. It may also be a logical vector of the same length and order as the modalities in object$names.ind.
#' @examples
#' example(soc.mca)
#' map.select(result, map.title="Map of the first ten modalities",list.mod=1:10)
#' select   <- active[,3]
#' select   <- select == levels(select)[2]
#' map.select(result, map.title="Map of all individuals sharing a particular value", list.ind=select, point.size=3)
#' map.select(result, map.title="Map of both select individuals and modalities", list.ind=select, list.mod=1:10)
#' @export
map.select    <- function(object, list.mod=NULL, list.sup=NULL, list.ind=NULL, dim=c(1,2), map.title="select", labelx=NULL, labely=NULL, point.label=TRUE, point.shape="variable", point.size=3, text.size=3.3, colour="black"){
  
  modal.list  <- list(list.mod=list.mod, list.sup=list.sup, list.ind=list.ind)
  
  gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
  gg.data     <- data.plot(object, plot.type="select", dim, ctr.dim=NULL, modal.list=modal.list, point.size=point.size) # Data selection
  axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
  map.title   <- plot.title(map.title=map.title) # Plot title
  scales      <- breaksandscales(gg.data) # Scales og breaks
  
  gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                      map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                      scales=scales, point.label=point.label, point.size=point.size)
  
  b.plot      <- basic.plot(gg.input, text.size=text.size, point.shape=point.shape, colour=colour) # Plot
  t.plot      <- b.plot + theme_min() # Theme
  t.plot$dimensions <- dim
  return(t.plot)
}



#' Add points to an existing map
#' @param object is a soc.mca class object as created by \link{soc.mca} and \link{soc.csa}
#' @param ca.map is a map created using one of the soc.mca map functions
#' @param data.type defines which type of points to add to the map. Accepted values are: "mod", "sup", "ind", "ctr". These values correspond to the different forms of 
#' @param list.mod is a numerical vector indicating which active modalities to plot. It may also be a logical vector of the same length and order as the modalities in object$names.mod.
#' @param list.sup is a numerical vector indicating which supplementary modalities to plot. It may also be a logical vector of the same length and order as the modalities in object$names.sup.
#' @param list.ind is a numerical vector indicating which individuals to plot. It may also be a logical vector of the same length and order as the modalities in object$names.ind.
#' @param ctr.dim is dimensions from which the contribution values are taken.
#' @param point.label if set to TRUE each point is assigned its label, defined in the soc.mca object. See \link{assign.label} and \link{add.to.label} for ways to alter the labels.
#' @param point.shape is a numerical value defining the shape of the points. If set to its default, the default scale is used. It may be defined by a variable with a suitable length.
#' @param point.size is a numerical value defining the size of the points. If set to its default, the size is determined by the frequency of each modality. It may be defined by a variable with a suitable length.
#' @param text.size defines the size of the labels
#' @param colour defines the colour of the text labels
#' @examples
#' example(soc.mca)
#' original.map    <- map.sup(result)
#' map.add(result, original.map, data.type="ctr", ctr.dim=2)
#' map.add(result, map.ind(result), data.type="ind",list.ind=1:50, colour="red", point.label=FALSE, point.size=12)
#' @export
map.add        <- function(object, ca.map, data.type=NULL, list.mod=NULL, list.sup=NULL, list.ind=NULL, ctr.dim=NULL, point.label=TRUE, point.shape="variable", point.size="freq", text.size=3.3, colour="black"){
  
  p           <- ca.map
  dim         <- ca.map$dimensions
  org.scales  <- ca.map$ca.scales
  modal.list  <- list(list.mod=list.mod, list.sup=list.sup, list.ind=list.ind)
  add.data    <- data.plot(object, plot.type=data.type, dim, ctr.dim=ctr.dim, modal.list=modal.list, point.size=point.size)
  
  # Points
  p           <- p + geom_point(data=add.data, aes(x=x, y=y, size=freq, shape=variable), colour=colour, fill=alpha("white", 0.5))
  
  # Point size
  if (length(add.data$point.size)==1 & is.numeric(add.data$point.size)==TRUE){
    p         <- p + scale_size_identity(guide="none")  }else{
    p         <- p + scale_size(guide="none")      
    }
  
  # Point shape scale
  if (identical(point.shape, "variable")==FALSE){
    p         <- p + scale_shape_manual(guide="none", values=point.shape)  }else{
    shapes    <- c(21, 22, 23, 0, 3, 1, 2, 5, 4, 10, 14, 12, 35, 64,  36:120)   
    p         <- p + scale_shape_manual(guide="none", values=shapes)   
   }
  
  
  if (identical(point.label, TRUE)==TRUE){
    p         <- p + geom_text(data=add.data, aes(x=x, y=y, label=names), size=text.size, vjust=1.4, colour=colour, lineheight=0.9)
  }
  
  # Scales and labels
  
  org.max     <- org.scales$lim.max
  org.min     <- org.scales$lim.min
  n.max       <- max(c(max(add.data[,1:2]), org.max))
  n.min       <- (c(min(add.data[,1:2]), org.min))
  tscales <- add.data
  tscales[1,1:2] <- n.max
  tscales[2,1:2] <- n.min
  scales <- breaksandscales(tscales)
  
  p     <- p + scale_x_continuous(breaks=scales$scalebreaks, labels= scales$breaklabel)
  p   	<- p + scale_y_continuous(breaks=scales$scalebreaks, labels= scales$breaklabel)
  p$ca.scales <- scales
  return(p)
}

############################## Plot delfunktioner ########################################


############################# basic.plot 
basic.plot <- function(gg.input, point.shape="variable", text.size=3.3, colour="black", fill="grey80", point.colour=NULL){ 
  # Defining point.size
  p       <- ggplot()   
  # The middle plot axis
  p       <- p + geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="solid")
  p       <- p + geom_vline(xintercept = 0, colour = "grey50", size = 0.5, linetype="solid")
  
  # Points
  if (is.null(point.colour)){
  p         <- p + geom_point(data=gg.input$gg.data, aes(x=x, y=y, size=freq, shape=variable), colour=colour, fill=alpha(fill, 0.5)) 
  }
  # Points for a colour variable
  if (is.null(point.colour)==FALSE){
  p         <- p + geom_point(data=gg.input$gg.data, aes(x=x, y=y, size=freq, shape=variable, colour=point.colour), fill=alpha(fill, 0.5)) 
  }
  # Point size scale
  if (length(gg.input$point.size)==1 & is.numeric(gg.input$point.size)==TRUE){
    p         <- p + scale_size_identity(guide="none")  }else{
    p         <- p + scale_size(guide="none", range=c(2,12)) 
    }
  # Point shape scale
  if (identical(point.shape, "variable")==FALSE){
    p         <- p + scale_shape_manual(guide="none", values=point.shape)  }else{
    shapes    <- c(21, 22, 23, 24, 25, 6, 0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 15, 16, 17, 18, 42, 45, 61, 48, 50:120)   
    p         <- p + scale_shape_manual(guide="none", values=shapes)   
  }
  
    # Text labels for all points
  if (identical(gg.input$point.label, TRUE)==TRUE){
    p       <- p + geom_text(data=gg.input$gg.data, aes(x=x, y=y, label=names),size=text.size, vjust=1.4, colour=colour, family="sans", lineheight=0.9)
  }
  # Title and axis labels
  p   	<- p + ggtitle(label=gg.input$map.title)
  p 		<- p + xlab(gg.input$labelx) + ylab(gg.input$labely)
  # Scales and breaks
  
  p     <- p + scale_x_continuous(breaks=gg.input$scales$scalebreaks, labels=gg.input$scales$breaklabel)
  p 		<- p + scale_y_continuous(breaks=gg.input$scales$scalebreaks, labels=gg.input$scales$breaklabel)
  p     <- p + coord_fixed()
  p$ca.scales <- gg.input$scales
  
  return(p)
}
####################### Plot title

plot.title  <- function(map.title=NULL, ctr.dim=NULL){
    # Map title for ctr plot    
    if (identical(map.title, "ctr")==TRUE) {
        map.title     <- paste("The modalities contributing above average to dimension " , paste(ctr.dim, collapse=" & "), sep="")
    }
    # Map title for all plot
    if (identical(map.title, "mod")==TRUE) {
        map.title     <- "Map of all modalities"
    }
    # Map title for active plot
    if (identical(map.title, "active")==TRUE) {
        map.title     <- "Map of active modalities"
    }
    # Map title for sup plot
    if (identical(map.title, "sup")==TRUE) {
        map.title     <- "Map of supplementary modalities"
    }
    # Map title for id plot
    if (identical(map.title, "ind")==TRUE) {
        map.title     <- "Map of individuals"
    }
    # Map title for list plot
    if (identical(map.title, "select")==TRUE) {
        map.title     <- "Map of selected modalities"
    }
    return(map.title)
}

############# Axis labels
plot.axis       <- function(labelx=NULL, labely=NULL, gg.proc=NA, dim=NULL){
    if (identical(labelx, NULL)==TRUE) {
labelx         <- paste(dim[1], ". Dimension: ",gg.proc[dim[1]], "%", sep="")
}
    if (identical(labely, NULL)==TRUE) {
labely         <- paste(dim[2], ". Dimension: ",gg.proc[dim[2]], "%", sep="")
}
axis.labels <- list(x=labelx, y=labely)
return(axis.labels)    
}

####################### breaksandscales
breaksandscales <- function(gg.data){
  # Hjælpe funktion
  mround <- function(x,base){
    base*round(x/base)
  }
  
  lim.min.x   	<- min(gg.data[,1])
  lim.min.y   	<- min(gg.data[,2])
  lim.max.x   	<- max(gg.data[,1])
  lim.max.y   	<- max(gg.data[,2])
  
  scalebreaks <- seq(-10,10, by=0.25)
  nolabel     <-  seq(-10, 10, by=0.5)
  inter       <- intersect(scalebreaks, nolabel)
  truelabel   <- is.element(scalebreaks, nolabel)
  breaklabel  <- scalebreaks  
  breaklabel[truelabel==FALSE] <-  ""
    
  length.x <- sqrt(lim.min.x^2) + sqrt(lim.max.x^2)
  length.y <- sqrt(lim.min.y^2) + sqrt(lim.max.y^2)
  
  scales <- list(scalebreaks=scalebreaks, breaklabel=breaklabel, lim.min.x=lim.min.x, lim.min.y=lim.min.y,
                 lim.max.x=lim.max.x, lim.max.y=lim.max.y, length.x=length.x, length.y=length.y)
  return(scales)
}

#################### data.plot v.2

data.plot   <- function(object, plot.type, dim, ctr.dim=NULL, modal.list=NULL, point.size="freq", point.variable=NULL, point.colour=NULL){
  # Types: mod, active, sup, ctr, ind, list
  
  # mod
  if (identical(plot.type, "mod")==TRUE){
    coord       <- rbind(object$coord.mod, object$coord.sup)
    mnames  	  <- c(object$names.mod, object$names.sup)
    freq        <- c(object$freq.mod, object$freq.sup)
    variable    <- c(object$variable, object$variable.sup)
  }
  
  # ctr
  if (identical(plot.type, "ctr")==TRUE){
    av.ctr      <- contribution(object, ctr.dim, indices=TRUE, mode="mod")
    coord     	<- object$coord.mod[av.ctr,]
    mnames		  <- object$names.mod[av.ctr]
    freq        <- object$freq.mod[av.ctr]
    variable    <- object$variable[av.ctr]
  }
  # active
  if (identical(plot.type, "active")==TRUE){
    coord 		  <- object$coord.mod
    mnames		  <- object$names.mod
    freq        <- object$freq.mod
    variable    <- object$variable
  }
  # sup
  if (identical(plot.type, "sup")==TRUE){
    coord 		<- object$coord.sup
    mnames		<- object$names.sup
    freq      <- object$freq.sup
    variable  <- object$variable.sup
  }
  # ind
  if (identical(plot.type, "ind")==TRUE){
    coord 		<- object$coord.ind
    mnames		<- object$names.ind
    freq      <- rep(1, object$n.ind)
     if(identical(point.variable, NULL)){
       variable  <- rep("ind",object$n.ind)
     }else{ 
       variable  <- point.variable
    }
    if(identical(point.colour, NULL)==FALSE)
    point.colour <- point.colour
  }
  
  # select
  if (identical(plot.type, "select")==TRUE){
    coord.mod 		<- object$coord.mod[modal.list$list.mod,]
    coord.sup     <- object$coord.sup[modal.list$list.sup,]
    coord.ind     <- object$coord.ind[modal.list$list.ind,]
    names.mod		  <- object$names.mod[modal.list$list.mod]
    names.sup  	  <- object$names.sup[modal.list$list.sup]
    names.ind     <- object$names.ind[modal.list$list.ind]
    coord         <- rbind(coord.mod, coord.sup, coord.ind)
    rownames(coord) <- NULL
    mnames        <- c(names.mod, names.sup, names.ind)
    freq.mod      <- object$freq.mod[modal.list$list.mod]
    freq.sup      <- object$freq.sup[modal.list$list.sup]
    freq.ind      <- rep(1, object$n.ind)[modal.list$list.ind]
    freq          <- c(freq.mod, freq.sup, freq.ind)
    variable.mod  <- object$variable[modal.list$list.mod]
    variable.sup  <- object$variable.sup[modal.list$list.sup]
    variable.ind  <- rep("ind", object$n.ind)[modal.list$list.ind]
    variable      <- c(variable.mod, variable.sup, variable.ind)
  }
  
  if(is.numeric(point.size))  freq         <- rep(point.size, length.out=nrow(coord))
  if(is.null(point.colour))   point.colour <- rep("Nothing", length.out=nrow(coord))
  
  
  gg.data             <- data.frame(cbind(coord[,dim[1]], coord[,dim[2]]), mnames, freq, variable, point.colour)
  colnames(gg.data)   <- c("x", "y", "names", "freq", "variable", "point.colour")
  return(gg.data)
}
