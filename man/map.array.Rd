\name{map.array}
\alias{map.array}
\title{Facetplot from list}
\usage{
  map.array(x, ncol = 1, title = "")
}
\description{
  This function takes a list of plot objects and arranges
  them into a grid
}
\examples{
\dontrun{
example(soc.mca)
map.array(list(map.ind(result), map.mod(result)), ncol=2)
}
}

