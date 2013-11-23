\name{map.csa.all}
\alias{map.csa.all}
\title{Array of several CSA maps}
\usage{
  map.csa.all(object, variable, dim = c(1, 2), ncol = 2,
    FUN = map.ind, fixed.coord = TRUE, main.title = "",
    titles = levels(variable), ...)
}
\arguments{
  \item{object}{is a \link{soc.mca} result object}

  \item{variable}{is a factor with the same order and
  length as those used for the active modalities in object}

  \item{dim}{indicates what dimensions to map and in which
  order to plot them}

  \item{ncol}{is the number of columns the maps are
  arranged into}

  \item{titles}{is a vector of the same length as the
  number of levels in variable. These are the titles given
  to each subplot}

  \item{main.title}{is the main title for all the maps}

  \item{FUN}{is the mapping function used for the plots;
  \link{map.active}, \link{map.ctr}, \link{map.ind},
  \link{map.select}}

  \item{fixed.coord}{if TRUE the limits of all plots are
  set to the same as the largest plot}

  \item{...}{sends any further arguments to the mapping
  functions}
}
\description{
  Creates an array of Class Specific Mulitple
  Correspondence analysis's
}
\examples{
\dontrun{
example(soc.csa)
map.csa.all(result, active[,1])
map.csa.all(result, active[,1], FUN=map.ctr, ctr.dim=1)
}
}

