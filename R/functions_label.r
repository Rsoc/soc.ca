## Labels 

#' Add to label
#'
#' Adds values to the end of the label of each modality.
#' @param  object is a soc.mca object
#' @param  text is the prefix used to construct the added text.
#' @return a soc.mca object with altered labels in names.mod and names.sup.
#' @export

add.to.label <- function(object, text=" (n:"){
  
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

#' Exports the labels of a soc.mca object into a csv file.
#' 
#' This function allows easy translation and renaming of modalities by exporting the labels into a .csv file that is easier to work with.
#' 
#' Two columns are created within the .csv: 'New label' and 'Old label'. In the 'New label' column you write the new labels. Remember to leave 'Old label' unchanged as this column is used for matching.
#' 
#' If you want to add frequencies to the labels with the \link{add.to.label} function you should do this after exporting and assigning labels with the \link{assign.label} function.
#' Otherwise the matching of the labels is likely to fail.
#' @param object is a soc.mca object
#' @param file is the name and path of the exported file
#' @param encoding is the character encoding of the exported file
#' @param overwrite decides whether to overwrite already existing files
#' @return A .csv with two columns and preferably UTF-8 encoding.
#' @export

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


#'  Assign.label
#'  
#' Assigns new labels to an soc.mca object. The input labels are defined in a .csv file created by the export.label() function.
#' @param object is a soc.mca object
#' @param file is the path of the .csv file with the new labels. The file is preferably created by the export.label() function
#' @param encoding is the encoding of the imported file
#' @param sep is the seperator used to create the imported .csv file  
#' @return a soc.mca object with altered labels in names.mod, names.ind and names.sup
#' @details To use this function first export the labels from your soc.mca analysis with the \link{export.label} function.
#' Then open and edit the created file with your favorite spreadsheet editor, fx. LibreOffice Calc. Change the new.label column to the desired values and save.
#' Use the assign.label function but remember to assign the results into a new object or overwrite the existing object.
#' @seealso \link{export.label}, \link{add.to.label}
#' @export

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