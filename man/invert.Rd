\name{invert}
\alias{invert}
\title{Invert}
\usage{
  invert(x, dim = 1)
}
\arguments{
  \item{x}{is a soc.ca object}

  \item{dim}{is the dimensions to be inverted}
}
\description{
  Invert one or more axis of a correspondence analysis. The
  principal coordinates of the analysis are multiplied by
  -1.
}
\details{
  This is a convienient function as you would have to
  modify coord.mod, coord.ind and coord.sup in the soc.ca
  object. This is more likely to provoke human errors.
}
\seealso{
  \link{soc.ca}, \link{add.to.label}
}

