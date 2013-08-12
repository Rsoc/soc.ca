\name{assign.label}
\alias{assign.label}
\title{Assign.label}
\usage{
  assign.label(object, file = FALSE, encoding = "UTF-8",
    sep = ",")
}
\arguments{
  \item{object}{is a soc.mca object}

  \item{file}{is the path of the .csv file with the new
  labels. The file is preferably created by the
  export.label() function}

  \item{encoding}{is the encoding of the imported file}

  \item{sep}{is the seperator used to create the imported
  .csv file}
}
\value{
  a soc.mca object with altered labels in names.mod,
  names.ind and names.sup
}
\description{
  Assigns new labels to an soc.mca object. The input labels
  are defined in a .csv file created by the export.label()
  function.
}
\details{
  To use this function first export the labels from your
  soc.mca analysis with the \link{export.label} function.
  Then open and edit the created file with your favorite
  spreadsheet editor, fx. LibreOffice Calc. Change the
  new.label column to the desired values and save. Use the
  assign.label function but remember to assign the results
  into a new object or overwrite the existing object.
}
\seealso{
  \link{export.label}, \link{add.to.label}
}

