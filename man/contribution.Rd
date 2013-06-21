\name{contribution}
\alias{contribution}
\title{The modalities or individuals with contributions above average}
\usage{
  contribution(object, dim = 1, all = FALSE,
    indices = FALSE, mode = "sort")
}
\arguments{
  \item{object}{is a soc.ca object}

  \item{dim}{is the included dimensions}

  \item{all:}{If TRUE returns all modalities instead of
  just those that contribute above average}

  \item{indices:}{If TRUE; returns a vector with the row
  indices of the modalities}

  \item{mode}{indicates which form of output. Possible
  values: "sort", "mod", "ind".}
}
\value{
  Ctr is the contribution in percentage

  Cor is the correlation with the dimension

  Coord is the principal coordinate

  Contribution values for individuals are in permille.
}
\description{
  Returns the modalities with above average contribution to
  the selected dimension
}
\examples{
example(soc.ca)
contribution(result)
contribution(result, 2)
contribution(result, dim=3, all=TRUE)
contribution(result, indices=TRUE)
contribution(result, mode="ind")
contribution(result, mode="ind", indices=TRUE)
}
\seealso{
  \link{map.ctr}
}

