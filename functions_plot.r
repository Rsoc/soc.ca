# De relativt f?rdige plotfunktioner:
# Vi mangler at f? en pch funktion p? alle mapene.
# Vi skulle m?ske i virkeligheden arbejde med fire titler +/- kapital i hver sin ende af dimensionen
# Size som funktion af quality
# Standard.coord !!!

# Se dev_plot.r

### Funktioner der mangler:
# Tilføj illustrative punkter
# ! enkelte, udfra label eller position
# ! alle af en type
# ! alle individer med et bestemt udfald
# ! 


# Start up

# source("pakken.r")
# source("eksempel.r")
# source("plothelp.r")
# identifier <- idento
# object <- soc.ca(analyse, sup, identifier)

### Plot for all modalitites
p.all <- function(object, dim=c(1,2), map.title="all", labelx=NULL, labely=NULL, scale.interval=NULL, point.label=TRUE, point.shape=15, point.size=1.6, text.size=2.5){

gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="all", dim, ctr.dim=NULL) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title) # Plot title
scales      <- breaksandscales(gg.data, scale.interval) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc, 
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.size=point.size, point.shape=point.shape) # Plot
t.plot      <- b.plot + theme_min()
return(t.plot)
}

#################### De mest bidragene modaliteter
p.ctr        <- function(object, ctr.dim=1, dim=c(1,2), map.title="ctr", labelx=NULL, labely=NULL, scale.interval=NULL, point.label=TRUE, point.shape=15, point.size=1.6, text.size=2.5){

gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="ctr", dim, ctr.dim=ctr.dim) # Data selection

axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title, ctr.dim=ctr.dim) # Plot title
scales      <- breaksandscales(gg.data, scale.interval) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.size=point.size, point.shape=point.shape) # Plot
t.plot      <- b.plot + theme_min() # Theme
return(t.plot)
}

######################## Active map

p.active     <- function(object, dim=c(1,2), map.title="active", labelx=NULL, labely=NULL, scale.interval=NULL, point.label=TRUE, point.shape=15, point.size=1.6, text.size=2.5){

gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="active", dim, ctr.dim=NULL) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title, ctr.dim=ctr.dim) # Plot title
scales      <- breaksandscales(gg.data, scale.interval) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.size=point.size, point.shape=point.shape) # Plot
t.plot      <- b.plot + theme_min() # Theme
return(t.plot)
}

###################### Supplementary map

p.sup     	<- function(object, dim=c(1,2), map.title="sup", labelx=NULL, labely=NULL, scale.interval=NULL, point.label=TRUE, point.shape=15, point.size=1.6, text.size=2.5){
gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="sup", dim, ctr.dim=NULL) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title, ctr.dim=ctr.dim) # Plot title
scales      <- breaksandscales(gg.data, scale.interval) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label)

b.plot      <- basic.plot(gg.input, text.size=text.size, point.size=point.size, point.shape=point.shape) # Plot
t.plot      <- b.plot + theme_min() # Theme
return(t.plot)
}

######################## Id map
p.id         <- function(object, dim=c(1,2), map.title="id", labelx=NULL, labely=NULL, scale.interval=NULL, point.label=FALSE, point.shape=15, point.size=1.6, text.size=2.5){

gg.proc 	<- round(object$adj.inertia[,4]) # Adjusted inertia
gg.data     <- data.plot(object, plot.type="id", dim, ctr.dim=NULL) # Data selection
axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
map.title   <- plot.title(map.title=map.title, ctr.dim=ctr.dim) # Plot title
scales      <- breaksandscales(gg.data, scale.interval) # Scales og breaks

gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                  map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                  scales=scales, point.label=point.label)
b.plot      <- basic.plot(gg.input, text.size=text.size, point.size=point.size, point.shape=point.shape) # Plot
t.plot      <- b.plot + theme_min() # Theme
return(t.plot)
}

############################### List map
p.list          <- function(object, modal.list=rep(TRUE, length(object$names)), dim=c(1,2), map.title="list", labelx=NULL, labely=NULL, scale.interval=NULL, point.label=TRUE, point.shape=15, point.size=1.6, text.size=2.5){
    gg.proc     <- round(object$adj.inertia[,4]) # Adjusted inertia
    gg.data     <- data.plot(object, plot.type="list", dim, ctr.dim=NULL, modal.list=modal.list) # Data selection
    axis.labels <- plot.axis(labelx=labelx, labely=labely, gg.proc=gg.proc, dim=dim) # Axis labels
    map.title   <- plot.title(map.title=map.title, ctr.dim=ctr.dim) # Plot title
    scales      <- breaksandscales(gg.data, scale.interval) # Scales og breaks
    
    gg.input    <- list(gg.data=gg.data, axis.inertia=gg.proc,
                        map.title=map.title, labelx=axis.labels$x, labely=axis.labels$y,
                        scales=scales, point.label=point.label)
    
    b.plot      <- basic.plot(gg.input, text.size=text.size, point.size=point.size, point.shape=point.shape) # Plot
    t.plot      <- b.plot + theme_min() # Theme
    return(t.plot)
}



################## plot over dimensionerne
#! Mangler at blive ryddet op i.
# Overvej om det kan betale sig at smide dens værdier ind i de andre funktioner - det skal nok gøres for at den kan blive en del af en større plot funktion
# Percent - skal ud og så skal der sættes % tegn på tallene på y dim.
# Funktion der vælger om det skal være almindelig eller adjusted inertia

p.dim <- function(object, dim=NA, map.title="The share of inertia per axis", labelx="Dimension", labely="Percent"){
gg.input 	<<- as.data.frame(object$adj.inertia)
gg.title 	<<- map.title
gg.axis1	<<- labelx
gg.axis2	<<- labely
gg.dim		<<- dim

p 		<- ggplot(gg.input, aes(gg.input[,1], gg.input[,3], width=0.1))
p 		<- p + geom_linerange(aes(x=gg.input[,1], ymax=gg.input[,3], ymin=0), stat="identity", fill="black", colour="black")
p 		<- p + coord_flip()
p 		<- p + scale_x_continuous(breaks=1:length(gg.input[,1]), trans="reverse")

if (is.na(dim)==FALSE){
  p 		<- p + geom_text(aes(max(dim), max(gg.input[,3])-3, vjust=1.5, label=paste("Total: ", round(sum(gg.input[,3][1:max(gg.dim)])), "%", sep=""), size= 0.8, family="F"))
  p 		<- p + geom_vline(xintercept = max(dim)+0.5, colour = "#999999", size = 0.4, linetype="dashed")
}
p 		<- p + opts(title=gg.title)
p 		<- p + xlab(gg.axis1) + ylab(gg.axis2)
p 		<- p + theme_min_bar(gridColor="white")
p		<- p + opts(legend.position = "none") 

return(p)
}

############################## Plot delfunktioner ########################################


############################# basic.plot 
basic.plot <- function(gg.input, point.shape=15, point.size=1.6, text.size=2.5){ 
p       <- ggplot()   
# The middle plot axis
p       <- p + geom_hline(yintercept = 0, colour = "grey90", size = 0.3, linetype="solid")
p       <- p + geom_vline(xintercept = 0, colour = "grey90", size = 0.3, linetype="solid")
# Points
if (length(point.shape)==1){
p         <- p + geom_point(data=gg.input$gg.data, aes(x=x, y=y), shape=point.shape, colour="black", fill=alpha("white", 0.5), size=point.size) 
}else{
# Points according to shape
p         <- p + geom_point(data=gg.input$gg.data, aes(x=x, y=y, shape=point.shape), colour="black", fill=alpha("white", 0.5), size=point.size)
}
# Text labels for all points
if (identical(gg.input$point.label, TRUE)==TRUE){
p     	<- p + geom_text(data=gg.input$gg.data, aes(x=x, y=y, label=names),size=text.size, vjust=1.5)
    }
# Title and axis labels
p 		<- p + opts(title=gg.input$map.title)
p 		<- p + xlab(gg.input$labelx) + ylab(gg.input$labely)
# Scales and breaks
p   	<- p + scale_x_continuous(breaks=gg.input$scales$scalebreaks, limits=c(gg.input$scales$lim.min, gg.input$scales$lim.max), labels= gg.input$scales$breaklabel)
p 		<- p + scale_y_continuous(breaks=gg.input$scales$scalebreaks, limits=c(gg.input$scales$lim.min, gg.input$scales$lim.max), labels= gg.input$scales$breaklabel)
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
breaksandscales <- function(gg.data, scale.interval=NULL){
# Hjælpe funktion
mround <- function(x,base){
  base*round(x/base)
}

mini   	<- mround(min(c(min(gg.data[,1]), min(gg.data[,2]))), 0.5)
maxi 		<- mround(max(c(max(gg.data[,1]), max(gg.data[,2]))), 0.5)
lim.min 		<- min(c(min(gg.data[,1]), min(gg.data[,2])))
lim.max 		<- max(c(max(gg.data[,1]), max(gg.data[,2])))

if (identical(scale.interval,NULL)==TRUE){
  scalebreaks 	<- round(seq(mini, maxi, by=0.25), digits=2)
} else {
  scalebreaks 	<- scale.interval
}

nolabel <-  seq(-5,5, by=0.5)
inter <- intersect(scalebreaks, nolabel)
truelabel <- is.element(scalebreaks, nolabel)
breaklabel <- scalebreaks
breaklabel[truelabel==FALSE] <-  ""

scales <- list(scalebreaks=scalebreaks, breaklabel=breaklabel, lim.min=lim.min, lim.max=lim.max)
return(scales)
}

#################### data.plot v.2

data.plot   <- function(object, plot.type, dim, ctr.dim=NULL, modal.list=NULL){
    # Types: all, active, sup, ctr, id
    
    # all
    if (identical(plot.type, "all")==TRUE){
        id             <- object$identifier
        coord     	<- object$coord[-id,]
        mnames		<- object$names[-id]
    }
    
    # ctr
    if (identical(plot.type, "ctr")==TRUE){
        id          <- object$identifier
        av.ctr      <- contribution(object, ctr.dim, modality.indices=TRUE)
        coord     	<- object$coord[av.ctr,]
        mnames		<- object$names[av.ctr]
    }
    # active
    if (identical(plot.type, "active")==TRUE){
        active 		<- object$active
        coord 		<- object$coord[active,]
        mnames		<- object$names[active]
    }
    # sup
    if (identical(plot.type, "sup")==TRUE){
        supp 		<- object$sup
        coord 		<- object$coord[supp,]
        mnames		<- object$names[supp]
    }
    # id
    if (identical(plot.type, "id")==TRUE){
        id     	    <- object$identifier
        coord 		<- object$coord[id,]
        mnames		<- object$names[id]
    }
    
    # list
    if (identical(plot.type, "list")==TRUE){
        coord 		<- object$coord[modal.list,]
        mnames		<- object$names[modal.list]
    }
    
    gg.data             <- data.frame(cbind(coord[,dim[1]], coord[,dim[2]]), mnames)
    colnames(gg.data)   <- c("x", "y", "names")
    return(gg.data)
}

