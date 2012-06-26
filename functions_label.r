## Labels 

## Modalitetsfrekvenser
add.n   <- function(object, text=" (n:"){
  
  freq.mod  <- object$freq.mod
  freq.sup  <- object$freq.sup
  names.mod <- object$names.mod
  names.sup <- object$names.sup
  
  names.mod <- paste(names.mod, text, freq.mod, ")", sep="")
  names.sup <- paste(names.sup, text, freq.sup, ")", sep="")
  
  object$names.mod <- names.mod
  object$names.sup <- names.sup
  
  return(object)
}


##### Export label
export.label    <- function(object, file=FALSE, encoding="UTF-8", overwrite=FALSE){
  
  names         <- c(object$names.mod, object$names.sup, object$names.ind)
  ca.label      <- cbind(names, names)
  colnames(ca.label)  <- c("New label", "Old label")
  
  if (identical(file, FALSE)==TRUE){
    file    <- paste("label_",deparse(substitute(object)), ".csv", sep="")
  }
  if(file.exists(file)==TRUE & identical(overwrite, FALSE)) stop("File already exists")
  write.csv(ca.label, file=file, fileEncoding=encoding)
}


#####  Assign.label

assign.label <- function(object, file=FALSE, encoding = "UTF-8", sep = ","){
  if (identical(file, FALSE)==TRUE){
    file <- paste("label_", deparse(substitute(object)), ".csv", sep = "")
  }
  label     <- read.csv(file, encoding=encoding, sep = sep)
  
  names.mod <- as.character(object$names.mod)
  names.sup <- as.character(object$names.sup)
  names.ind <- as.character(object$names.ind)
  
  new.label <- as.character(label$New.label)
  old.label <- as.character(label$Old.label)
  
  for (i in 1:length(old.label)){
    indices   <- which(old.label[i]==names.mod)
    names.mod[indices] <- new.label[i]
  }
  
  for (i in 1:length(old.label)){
    indices   <- which(old.label[i]==names.sup)
    names.sup[indices] <- new.label[i]
  }
  
  for (i in 1:length(old.label)){
    indices   <- which(old.label[i]==names.ind)
    names.ind[indices] <- new.label[i]
  }
  
  
  object$names.mod <- names.mod
  object$names.sup <- names.sup
  object$names.ind <- names.ind
  return(object)
}