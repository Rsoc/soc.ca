## Labels 

## Modalitetsfrekvenser
# Kan kun s?tte p? aktive modaliteter
# Kan forbedres med diag()
add.n   <- function(object, add=TRUE, text=" (n:"){
a.m     <- object$active
mnames  <- object$names[a.m]
b       <- object$Burt
freq    <- matrix(data=NA, nrow=ncol(b), ncol=1)
for (i in 1:ncol(b)){
freq[i,1] <- b[i,i] 
}
freq    <- as.data.frame(freq)
rownames(freq) <- mnames
colnames(freq) <- c("Number")
if (add==FALSE){
return(freq)
}

if (add==TRUE){
    tname         <- matrix(data=NA, nrow=ncol(b), ncol=1)
for (i in 1:ncol(b)){
    tname[i,1]    <- paste(mnames[i],text,freq[i,1],")", sep="")
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

get.varlabel <- function(object){
varname <- rownames(object$modal)
tab.label <- cbind(varname, varname, 1:length(varname))
colnames(tab.label) <- c("Old label", "New label", "Heading")
temp <- tab.label
tab.label <- edit(tab.label)
tab.label[,1] <- temp[,1]
tab.label
}

export.label    <- function(object, file=FALSE, encoding="UTF-8", overwrite=FALSE){
ca.label        <- cbind(object$names, object$names)
colnames(ca.label)  <- c("New label", "Old label")
    if (identical(file, FALSE)==TRUE){
        file    <- paste("label_",deparse(substitute(object)), ".csv", sep="")
    }
    if(file.exists(file)==TRUE & identical(overwrite, FALSE)) stop("File already exists")
    write.csv(ca.label, file=file, fileEncoding=encoding)
}

assign.label <- function(object, file=FALSE, encoding="UTF-8"){
  if (identical(file, FALSE)==TRUE){
    file <- paste("label_",deparse(substitute(object)), ".csv", sep="")
}
      label     <- read.csv(file, encoding=encoding)
      obj.label <- as.character(object$names)
      new.label <- as.character(label$New.label)
      old.label <- as.character(label$Old.label)
      for (i in 1:length(old.label)){
      indices   <- which(old.label[i]==obj.label)
      obj.label[indices] <- new.label[i]
      }
      object$names <- obj.label
      return(object)
}