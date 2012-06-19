## Diverse ubrugte funktioner

# Balancemål for mass
balance.mass <- function(object, act.dim=object$nd){
aktive <- object$analyse
coord <- object$coord[aktive, 1:act.dim]
mass <- object$mass
pm <- matrix(, nrow=act.dim, ncol=3)
for (i in 1:act.dim){
nif <- cbind(coord[,i], mass)
nif <- nif[order(nif[,1]),]
plus <-  nif[which(nif[,1] >= 0), ]
minus <- nif[which(nif[,1] <= 0), ]
pmass <- sum(plus[,2])
mmass <- sum(minus[,2])
pm[i,1] <- pmass
pm[i,2] <- mmass
pm[i,3] <- pmass/mmass
}
colnames(pm) <- c("+ Mass", "- Mass", "+/-")
return(pm)
}

# Den her funktion viser hvor meget af dim80 inertien der forklares af variablene
#
varinerti <- function(object, dim=6){
modal   	<- object$modal
inerti 		<- object$inertia[1:dim]
sumine 		<- sum(inerti)

ctr 		<- round(100*object$contrib[,1:dim], digits=2)
tab 		<- matrix(, ncol=length(1:dim), nrow=nrow(modal))
for (i in 1:nrow(modal)){
  mf 		<- modal[i,1]:modal[i,2]
  tab[i,] 	<- apply(ctr[mf,], 2, sum)
}
}

##### Linje graf 

glinje   	<- function(object, dim=1, labelx=NA){
gg.proc 	<- round(object$adj.inertia[,3])
id 		<- object$identifier
koord 		<- object$coord[-id,]
navne		<- object$names[-id]
gg.input 	<<- as.data.frame(cbind(koord[,dim[1]], rep(0, nrow(koord))), navne)

if (is.na(labelx)==TRUE) {
  labelx 	<- paste(dim[1], ". Dimension: ",gg.proc[dim[1]], "%", sep="")
}

# Her er de globalt definerede objekter - Det er noget makv?rk der gerne m? fjernes n?r GGplot2 underst?tter det.
gg.akse1	<<- labelx

p 		<- ggplot(gg.input, aes(gg.input[,2], gg.input[,1], label=rownames(gg.input)))
p		<- p + ylab(rownames(gg.input))
p		<- p + xlab("")
p 		<- p + geom_point(shape="-", colour="black", fill=alpha("cornflowerblue" , 0.5))
p 		<- p + geom_text(size=2.5, hjust=1.1)
p 		<- p + scale_y_continuous(breaks=seq(round(min(gg.input[,1])), round(max(gg.input[,1])), by=1))
p 		<- p + theme_sing_v()

return(p)
}

theme_sing = function (size=10, font="F", face='plain', 
  backgroundColor='white', panelColor='white', 
    axisColor='#999999', gridColor='white', textColor='black') 
{
    opts(
        axis.text.x = theme_text(vjust=1, hjust=0.5, colour=axisColor, family=font, face=face, size=size),
        axis.text.y = theme_text(hjust=1, vjust=0.5, colour='white', family=font, face=face, size=size),
        axis.title.x = theme_text(family=font, face=face, colour=textColor, size=size),
        axis.title.y = theme_text(angle=90, family=font, face=face, colour=textColor, size=size),
        axis.line = theme_blank(),
        axis.ticks = theme_segment(colour=axisColor, size=0.25),
        panel.border = theme_rect(colour='white', linetype="dashed"),
        legend.background = theme_blank(),
        legend.key = theme_blank(),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        legend.title = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        panel.background = theme_rect(fill=panelColor, colour=NA),
        plot.background = theme_rect(fill=backgroundColor, colour=NA),
        panel.grid.major = theme_line(colour=gridColor, size=0.33, linetype="dotted"),
        panel.grid.minor = theme_blank(),
        strip.background = theme_rect(fill=NA, colour=NA),
        strip.text.x = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        strip.text.y = theme_text(angle=-90, family=font, face=face, colour=textColor, size=size),
        plot.title = theme_text(hjust=0.5 , vjust=1, family=font, face=face, colour=textColor, size=13),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
}


# Vertical singular akse

theme_sing_v = function (size=10, font="F", face='plain', 
	backgroundColor='white', panelColor='white', 
    axisColor='#999999', gridColor='white', textColor='black') 
{
    opts(
        axis.text.x = theme_text(vjust=1, hjust=0.5, colour='white', family=font, face=face, size=size),
        axis.text.y = theme_text(hjust=1, vjust=0.5, colour=axisColor, family=font, face=face, size=size),
        axis.title.x = theme_text(family=font, face=face, colour=textColor, size=size),
        axis.title.y = theme_text(angle=90, family=font, face=face, colour=textColor, size=size),
        axis.line = theme_blank(),
        axis.ticks = theme_segment(colour=axisColor, size=0.25),
        panel.border = theme_rect(colour='white', linetype="dashed"),
        legend.background = theme_blank(),
        legend.key = theme_blank(),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        legend.title = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        panel.background = theme_rect(fill=panelColor, colour=NA),
        plot.background = theme_rect(fill=backgroundColor, colour=NA),
        panel.grid.major = theme_line(colour=gridColor, size=0.33, linetype="dotted"),
        panel.grid.minor = theme_blank(),
        strip.background = theme_rect(fill=NA, colour=NA),
        strip.text.x = theme_text(hjust=0, family=font, face=face, colour=textColor, size=size),
        strip.text.y = theme_text(angle=-90, family=font, face=face, colour=textColor, size=size),
        plot.title = theme_text(hjust=0.5 , vjust=1, family=font, face=face, colour=textColor, size=13),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))
}

### lav variabel 

# NB her tager vi kun 5 dimensioner med - hvis vi vil noget andet skal der ?ndres i standarden for smca
ca.var <- function(object, data, identifier, dim=1:5){
df <- data
id <- object$identifier
id.label <- object$names[id]
coord <- object$coord[id,]
coord <- coord[dim]
dat.mat <- as.matrix(cbind(id.label, coord))
order(df, df$id)
df.order <- df[order(df[,1]), ]

return(df.order)
}


## Test bidrag
test.contrib   <- function(object, dim=1){
ctr 		<- as.matrix(object$contrib[,dim])
a.m 		<- object$active
modal		<- as.data.frame(object$modal)
sub		<- which(modal$Slut <= length(a.m))
modal		<- modal[sub,]
varnames		<- rownames(modal)
snit 		<- mean(ctr[,dim])

bidrag.test 	<- data.frame(row.names=varnames)

	for (i in 1:nrow(modal)){
  interval 	<- modal$Start[i]:modal$Slut[i]
  vctr		<- as.matrix(ctr[interval,] )
  oversnit	<- which(vctr[,dim] >=snit)
  bidrag.test[i,1] <- length(oversnit)
  bidrag.test[i,2] <- modal$Antal[i]
  bidrag.test[i,3] <- round(1000*sum(ctr[interval,dim]))
}
colnames(bidrag.test) <- c("Modaliteter over gns.", "Antal modaliteter", "Bidrag til dim.")
return(bidrag.test)
}
 

## Variabel contributions tabel tabel
tab.varcontrib <- function(object, tabel.label, dim=(1:3), heading=NA){
modal   	<- object$modal
ctr 		<- round(100*object$contrib[,dim], digits=2)
tab 		<- matrix(, ncol=length(dim), nrow=nrow(modal))
for (i in 1:nrow(modal)){
  mf 		<- modal[i,1]:modal[i,2]
  tab[i,] 	<- apply(ctr[mf,], 2, sum)
}
new.label 	<- as.character(tabel.label[,3])
old.label 	<- as.character(tabel.label[,2])
obj.label 	<- rownames(modal)
obj.heading <- vector(mode="character", length=length(obj.label))
new.heading <- as.character(tabel.label[,4])
for (i in 1:length(old.label)){
  #indices 	<- grep(old.label[i], obj.label, fixed=TRUE)
  indices   <- which(old.label[i]==obj.label) 
    # Vi skal matche kun p? exact
  obj.label[indices] <- new.label[i]
  obj.heading[indices]   <- new.heading[i]
}

dimproc 	<- round(object$adj.inertia[dim,3])
col.label 	<- c("Modalities", paste("Dim.", dim," (", dimproc,"%)", sep=""))
tab 		<- cbind(modal[,3],tab)
tab     <- as.data.frame(round(tab, digits=1))
Total   <- apply(tab, 2, sum)
tab  	<- as.data.frame(rbind(tab, Total))
rownames(tab)   <- c(obj.label,"Total")
if (identical(heading, NA)==FALSE){
col.label <- c(col.label, "Heading")
obj.heading <- c(obj.heading, "")
tab <- cbind(tab, obj.heading)
}
colnames(tab) 	<- col.label

return(noquote(tab))
}


