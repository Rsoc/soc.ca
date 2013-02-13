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

p.all <- function(object, dim=c(1,2), map.title="all", labelx=NULL, labely=NULL, point.label=TRUE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){

gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="all", dim, ctr.dim=NULL, point.size=point.size) # Data selection
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

#################### The most contributing modalities
p.ctr        <- function(object, ctr.dim=1, dim=c(1,2), map.title="ctr", labelx=NULL, labely=NULL, point.label=TRUE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){

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

######################## Active map

p.active     <- function(object, dim=c(1,2), map.title="active", labelx=NULL, labely=NULL, point.label=TRUE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){

gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="active", dim, ctr.dim=NULL, point.size=point.size) # Data selection
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

###################### Supplementary map

p.sup     	<- function(object, dim=c(1,2), map.title="sup", labelx=NULL, labely=NULL, point.label=TRUE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){
gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="sup", dim, ctr.dim=NULL, point.size=point.size) # Data selection
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

######################## Id map
p.id         <- function(object, dim=c(1,2), map.title="id", labelx=NULL, labely=NULL, point.label=FALSE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){

gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="id", dim, ctr.dim=NULL, point.size=point.size) # Data selection
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

############################### List map
p.list        <- function(object, list.mod=NULL, list.sup=NULL, list.ind=NULL, dim=c(1,2), map.title="list", labelx=NULL, labely=NULL, point.label=TRUE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){
  
  modal.list  <- list(list.mod=list.mod, list.sup=list.sup, list.ind=list.ind)
  
  gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
  gg.data     <- data.plot(object, plot.type="list", dim, ctr.dim=NULL, modal.list=modal.list, point.size=point.size) # Data selection
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


############### Add new points to an existing plot

add.points <- function(object, ca.plot, data.type=NULL, list.mod=NULL, list.sup=NULL, list.ind=NULL, ctr.dim=NULL, point.label=TRUE, point.shape=21, point.size="freq", text.size=3.3, colour="black"){
  
  p           <- ca.plot
  dim         <- ca.plot$dimensions
  org.scales  <- ca.plot$ca.scales
  modal.list  <- list(list.mod=list.mod, list.sup=list.sup, list.ind=list.ind)
  add.data    <- data.plot(object, plot.type=data.type, dim, ctr.dim=ctr.dim, modal.list=modal.list, point.size=point.size)
  
  p           <- p + geom_point(data=add.data, aes(x=x, y=y, size=freq), shape=point.shape, colour=colour, fill=alpha("white", 0.5))
  
  if (length(add.data$point.size)==1 & is.numeric(add.data$point.size)==TRUE){
    p         <- p + scale_size_identity(guide="none")  }else{
    p         <- p + scale_size(guide="none")      
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
basic.plot <- function(gg.input, point.shape=21, text.size=3.3, colour="black", fill="grey80"){ 
  # Defining point.size
  p       <- ggplot()   
  # The middle plot axis
  p       <- p + geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="solid")
  p       <- p + geom_vline(xintercept = 0, colour = "grey50", size = 0.5, linetype="solid")
  # Points
  p         <- p + geom_point(data=gg.input$gg.data, aes(x=x, y=y, size=freq), shape=point.shape, colour=colour, fill=alpha(fill, 0.5)) 

  if (length(gg.input$point.size)==1 & is.numeric(gg.input$point.size)==TRUE){
    p         <- p + scale_size_identity(guide="none")  }else{
    p         <- p + scale_size(guide="none", range=c(2,12)) 
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
    if (identical(map.title, "all")==TRUE) {
        map.title     <- "Cloud of active modalities"
    }
    # Map title for active plot
    if (identical(map.title, "active")==TRUE) {
        map.title     <- "Cloud of active modalities"
    }
    # Map title for sup plot
    if (identical(map.title, "sup")==TRUE) {
        map.title     <- "Cloud of supplementary modalities"
    }
    # Map title for id plot
    if (identical(map.title, "id")==TRUE) {
        map.title     <- "Cloud of individuals"
    }
    # Map title for list plot
    if (identical(map.title, "list")==TRUE) {
        map.title     <- "Cloud of selected modalities"
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

data.plot   <- function(object, plot.type, dim, ctr.dim=NULL, modal.list=NULL, point.size="freq"){
  # Types: all, active, sup, ctr, id, list
  
  # all
  if (identical(plot.type, "all")==TRUE){
    coord       <- rbind(object$coord.mod, object$coord.sup, object$coord.ind)
    mnames  	  <- c(object$names.mod, object$names.sup, object$names.ind)
    freq        <- c(object$freq.mod, object$freq.sup, rep(1,object$n.ind))   
  }
  
  # ctr
  if (identical(plot.type, "ctr")==TRUE){
    av.ctr      <- contribution(object, ctr.dim, modality.indices=TRUE)
    coord     	<- object$coord.mod[av.ctr,]
    mnames		  <- object$names.mod[av.ctr]
    freq        <- object$freq.mod[av.ctr]
  }
  # active
  if (identical(plot.type, "active")==TRUE){
    coord 		  <- object$coord.mod
    mnames		  <- object$names.mod
    freq        <- object$freq.mod
  }
  # sup
  if (identical(plot.type, "sup")==TRUE){
    coord 		<- object$coord.sup
    mnames		<- object$names.sup
    freq      <- object$freq.sup
  }
  # id
  if (identical(plot.type, "id")==TRUE){
    coord 		<- object$coord.ind
    mnames		<- object$names.ind
    freq      <- rep(1, object$n.ind)
  }
  
  # list
  if (identical(plot.type, "list")==TRUE){
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
  }
  
  if(is.numeric(point.size))  freq <- rep(point.size, length.out=nrow(coord))
  
  gg.data             <- data.frame(cbind(coord[,dim[1]], coord[,dim[2]]), mnames, freq)
  colnames(gg.data)   <- c("x", "y", "names", "freq")
  return(gg.data)
}
