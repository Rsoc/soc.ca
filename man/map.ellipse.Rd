\name{map.ellipse}
\alias{map.ellipse}
\title{Concentration ellipses}
\usage{
  map.ellipse(object, ca.plot = map.ind(object), variable,
    ellipse.label = TRUE, line.color = "black",
    label.size = 4)
}
\arguments{
  \item{object}{is a soc.mca class object}

  \item{ca.plot}{is a plot made from a soc.mca object}

  \item{variable}{is a factor of the same length and in the
  same order as the active varibles used for the soc.mca
  object.}

  \item{ellipse.label}{if TRUE the labels are included in
  the map}

  \item{line.color}{defines the color of the ellipses}

  \item{label.size}{defines the size of the labels}
}
\value{
  a plot with a concentration ellipse containing 80\% of
  the individuals for each modality
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

