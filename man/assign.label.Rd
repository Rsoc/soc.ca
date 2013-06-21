\name{assign.label}
\alias{assign.label}
\title{Assign.label}
\usage{
  assign.label(object, file = FALSE, encoding = "UTF-8",
    sep = ",")
}
\arguments{
  \item{object}{is a soc.ca object}

  \item{file}{is the path of the .csv file with the new
  labels. The file is preferably created by the
  export.label() function}

  \item{encoding}{is the encoding of the imported file}

  \item{sep}{is the seperator used to create the imported
  .csv file}
}
\value{
  a soc.ca object with altered labels in names.mod,
  names.ind and names.sup
}
\description{
  Assigns new labels to an soc.ca object. The input labels
  are defined in a .csv file created by the export.label()
  function.
}
\seealso{
  \link{export.label}, \link{add.to.label}
}

