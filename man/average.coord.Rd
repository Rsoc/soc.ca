\name{average.coord}
\alias{average.coord}
\title{Find the average.coordinate for each category in a variable on two dimensions}
\usage{
  average.coord(object, x, dim = c(1, 2))
}
\arguments{
  \item{object}{is soc.ca result object}

  \item{x}{is a variable of the same length as the active
  variables used to construct the soc.ca object}

  \item{dim}{is the two dimensions used}
}
\value{
  a matrix with the mean points and frequencies of the
  given variable
}
\description{
  Find the average.coordinate for each category in a
  variable on two dimensions
}
\examples{
example(soc.mca)
average.coord(result, taste$Income)
}

