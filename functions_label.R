## Labels 

## Modalitetsfrekvenser
# Kan kun s?tte p? aktive modaliteter
add.n <- function(object, add=FALSE, text=" (n:"){
a.m <- object$active
mnames <- object$names[a.m]
b <- object$Burt
freq <- matrix(data=NA, nrow=ncol(b), ncol=1)
for (i in 1:ncol(b)){
freq[i,1] <- b[i,i] 
}
freq <- as.data.frame(freq)
rownames(freq) <- mnames
colnames(freq) <- c("Number")
if (add==FALSE){
return(freq)
}

if (add==TRUE){
tname <- matrix(data=NA, nrow=ncol(b), ncol=1)
for (i in 1:ncol(b)){
tname[i,1]<- paste(mnames[i],text,freq[i,1],")", sep="")
}
object$names[a.m] <- tname[,1]
}
return(object)
}



## Labelovers?tteren:
# Skaber et labelobjekt der kan gemmes i en fil og importeres. - S? label export er ogs? n?dvendig.
# Skal der ?ndres direkte i resultat-objektet? Det er m?ske det smarteste, for s? kan beh?ver al anden kode ikke at forholde sig til de labels. 
# I konstruktionen af ggplot datas?ttet med individskyen - m? labels til modaliteterne hentes fra labelobjektet. - Hvis det kan g?res...
# Create labelobjekt
# Search and replace in resultat objekt
# export and preserve

get.label <- function(object){
ca.label <- cbind(object$names, object$names)
colnames(ca.label) <- c("New label", "Old label")
temp <- ca.label
ca.label <- edit(ca.label)
ca.label[,2] <- temp[,2]
ca.label
}

get.varlabel <- function(object){
varname <- rownames(object$modal)
tab.label <- cbind(varname, varname, 1:length(varname))
colnames(tab.label) <- c("Old label", "New label", "Heading")
temp <- tab.label
tab.label <- edit(tab.label)
tab.label[,1] <- temp[,1]
tab.label
}

export.label <- function(ca.label){
l.fil <- deparse(substitute(ca.label)) 
write.csv(ca.label, file=l.fil)
}


#indf?r en quote funktion
import.label <- function(file="ca.label"){
read.csv(file)
}

edit.label <- function(ca.label){
temp <- ca.label
ca.label <- edit(ca.label)
ca.label[,2] <- temp[,2]
ca.label
}

assign.label <- function(object, ca.label){
obj.label <- as.character(object$names)
new.label <- as.character(ca.label[,2])
old.label <- as.character(ca.label[,3])
for (i in 1:length(old.label)){
#indices <- grep(old.label[i], obj.label, fixed=TRUE)
indices   <- which(old.label[i]==obj.label)
obj.label[indices] <- new.label[i]
}
object$names <- obj.label
object
}


