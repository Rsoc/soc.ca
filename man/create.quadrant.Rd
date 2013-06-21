\name{create.quadrant}
\alias{create.quadrant}
\title{Create quadrants}
\usage{
  create.quadrant(result, dim = c(1, 2), cut.min = -0.125,
    cut.max = 0.125, cut.radius = 0.25)
}
\arguments{
  \item{result}{a soc.ca object}

  \item{dim}{the dimensions}

  \item{cut.min}{Minimum cut value}

  \item{cut.max}{Maximum cut value}

  \item{cut.radius}{Radius of the center category}
}
\value{
  Returns a character vector
}
\description{
  Creates a vector from two dimensions from a soc.ca
  object. Labels are the cardinal directions with the first
  designated dimension running East - West.
}
\examples{
example(soc.ca)
create.quadrant(result, dim=c(2,1))
table(create.quadrant(result, dim=c(1,3), cut.radius=0.5))
}
\seealso{
  \link{soc.ca}
}

