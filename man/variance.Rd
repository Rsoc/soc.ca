\name{variance}
\alias{variance}
\title{Variance tabel}
\usage{
  variance(object, dim = NULL)
}
\arguments{
  \item{object}{is a soc.ca object}

  \item{dim}{is the included dimensions, if set to NULL,
  then only the dimensions explaining approx. 90% of the
  adjusted variance are included}
}
\value{
  If assigned using <- variance returns a matrix version of
  the table of variance
}
\description{
  variance returns a table of variance for the selected
  dimensions.
}
\examples{
example(soc.mca)
variance(result)
variance(result, dim = 1:4)
}
\seealso{
  \link{soc.mca}, \link{print.soc.mca}
}

