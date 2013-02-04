######################## Ellipse tegne funktionen
p.ellipse <- function(object, ca.plot, variabel){ # Skal gg.input være defineret som default?
# De indledende øvelser

dim         <- ca.plot$dimensions 
id.coord    <- object$coord.ind[, dim]
lev.var     <- levels(variabel)
id          <- object$names.ind
# En liste, af lister-objekter med samtlige nødvendige informationer
# Label
# Kryds koordinater
# Midterpunkt koordinat
# Ellipse koordinater
## Fremtid: freq til at afgøre origo størrelse.
ellipse.data <- list()
for (i in 1:nlevels(variabel)){
binvar            <- which(variabel==lev.var[i])
id.coord.var      <- id.coord[binvar,]
label             <- lev.var[i]
el.coord          <- ellipse.coord(id, id.coord.var)
el.axis           <- ellipse.axis(el.coord)
el.origo          <- ellipse.origo(el.axis)
el.origo          <- data.frame(x=el.origo[1], y=el.origo[2], label)
ellipse.data[[i]] <- list(label=label, el.coord=el.coord, el.axis=el.axis, el.origo=el.origo)
}

ellipse.data      <- ellipse.data

# Her plottes.

for (i in 1:nlevels(variabel)){
# Ellipserne
ca.plot <- ca.plot + geom_path(data=ellipse.data[[i]]$el.coord, aes(x=x, y=y), colour="grey70", size=0.33)
# Akserne
ca.plot <- ca.plot + geom_path(data=ellipse.data[[i]]$el.axis[[1]],aes(x=x, y=y), colour="grey70", size=0.33, linetype=2, na.rm=TRUE) # Det er muligt at vi kan tegne med aes istedet og undgå at det der rækkeshit
ca.plot <- ca.plot + geom_path(data=ellipse.data[[i]]$el.axis[[2]],aes(x=x, y=y), colour="grey70", size=0.33, linetype=2, na.rm=TRUE)
# Origo
ca.plot <- ca.plot + geom_point(data=ellipse.data[[i]]$el.origo, aes(x=x, y=y), size=2, shape=21, fill="white", colour="grey70", na.rm=TRUE)
# Origo label
#! Måske annotate ville være en god funktion??
ca.plot <- ca.plot + geom_text(aes(x=x, y=y, label=label), data=ellipse.data[[i]]$el.origo, size=3.3, hjust=-0.15, fontface="italic")
}

ca.plot
}


################### Ellipse akser funktionen
ellipse.axis <- function(el.dat){
# Her defineres antallet af punkter
n           <- length(na.omit(el.dat[,1]))
# Her parrer vi punkter med hinanden
p.pairs     <- el.dat[1,]
  for (i in 1:(n/2)){
p.pairs[i,] <- c(i,i+(n/2))
}
p.pairs <- cbind(p.pairs, rep(0,n/2))
colnames(p.pairs) <- c("x", "y", "distance")

# Her udregner vi afstandene mellem hvert par af punkter
for (i in 1:(n/2)){
a       <- el.dat[i,]
b       <- el.dat[i+(n/2),]
p.pairs[i,3] <- ((b$x-a$x)^2+(b$y-a$y)^2)^0.5
}
# Her defineres den korteste og længste path
p.min   <- which.min(p.pairs[,3])
p.max   <- which.max(p.pairs[,3])

#l.id <- 3:nrow(gg.input)
mintemp <- p.pairs[p.min, 1:2]
minpath <- as.data.frame(el.dat[c(mintemp$x, mintemp$y),], row.names=c("1","2"))
#minpath[l.id,] <- NA

maxtemp <- p.pairs[p.max, 1:2]
maxpath <- as.data.frame(el.dat[c(maxtemp$x, maxtemp$y),], row.names=c("1","2"))
#maxpath[l.id,] <- NA
ellipse.axis.points <- list(maxpath, minpath)
ellipse.axis.points
}

############## Ellipse.coord
ellipse.coord <- function(id, id.coord.var){
# Her laves de binære variable der skal bruges til at tegne koordinaterne. Det kan gøres mere abstrakt.
#variabel01 <- which(variabel==levels(variabel)[1])
id.var    <- as.data.frame(id.coord.var)
colnames(id.var) <- c("x","y")

xdata     <- with(as.data.frame(id.var), xyTable(x,y))# Her laves en bestemt binning - det er uklart for mig hvad det faktisk er.
xframe    <- cbind.data.frame(x=xdata$x, y=xdata$y, n=xdata$number)
data.ell  <- as.data.frame(with(id.var, ellipse(cor(x, y), 
                                         scale=c(sd(x),sd(y)), 
                                         centre=c(mean(x),mean(y)), npoints = 1000)))
# if ((length(id)>360)==TRUE){
# l.id <- 361:length(id)
# data.ell[l.id,] <- NA
# }
data.ell
}

#############################3#### Ellipse origo
ellipse.origo <- function(el.axis){
 temp <- el.axis[[1]][c(1,2),]
 x1 <- temp[1,1]
 x2 <- temp[2,1]
 y1 <- temp[1,2]
 y2 <- temp[2,2]
el.origo <<-c((x1+x2)/2, (y1+y2)/2)
el.origo
}
