\name{map.ellipse}
\alias{map.ellipse}
\title{Concentration ellipses}
\usage{
  map.ellipse(object, ca.plot, variable)
}
\arguments{
  \item{object}{is a soc.mca class object}

  \item{ca.plot}{is a plot made from a soc.mca object}

  \item{variable}{is a factor of the same length and in the
  same order as the active varibles used for the soc.mca
  object.}
}
\value{
  a plot with a concentration ellipse containing 80% of the
  individuals for each modality
}
\description{
  Add ellipses for each level in a factor to a plot made
  from a soc.mca object.
}
\examples{
example(soc.mca)
map <- map.ind(result)
map.ellipse(result, map, active[,2])
}
\seealso{
  \link{map.ind}, \link{map.ctr}, \link{soc.mca}
}

