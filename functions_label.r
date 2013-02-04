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
# Add frequencies to the end of the label of each modality.
# object is a soc.ca object
# text is the prefix used to construct the added text.
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

# Exports the labels of a soc.ca object into a csv file.
# This function allows easy translation and renaming of modalities
# object is a soc.ca object
# file is the name and path of the exported file
# encoding is the character encoding of the exported file
# overwrite decides whether to overwrite already existing files
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

  # Assigns new labels to an soc.ca object. The input labels are defined in a .csv file created by the export.label() function.
  # object is a soc.ca object
  # file is the path of the .csv file with the new labels. The file is preferably created by the export.label() function
  # encoding is the encoding of the imported file
  # sep is the seperator used to create the .csv file  
}