\name{map.ellipse}
\alias{map.ellipse}
\title{Concentration ellipses}
\usage{
  map.ellipse(object, ca.plot, variable)
}
\arguments{
  \item{object}{is a soc.ca class object}

  \item{ca.plot}{is a plot made from a soc.ca object}

  \item{variable}{is a factor of the same length and in the
  same order as the active varibles used for the soc.ca
  object.}
}
\value{
  a plot with a concentration ellipse containing 80% of the
  individuals for each modality
}
\description{
  Add ellipses for each level in a factor to a plot made
  from a soc.ca object.
}
\examples{
example(soc.ca)
map.ind(result)
}
\seealso{
  \link{map.ind}, \link{map.ctr}, \link{soc.ca}
}

