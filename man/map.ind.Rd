\name{map.ind}
\alias{map.ind}
\title{Map the individuals of a soc.ca analysis}
\usage{
  map.ind(object, dim = c(1, 2), map.title = "ind",
    labelx = NULL, labely = NULL, point.label = FALSE,
    point.shape = "variable", point.variable = NULL,
    point.colour = NULL, point.size = 3, text.size = 3.3,
    colour = "black", legend = NULL)
}
\arguments{
  \item{object}{is a soc.ca class object as created by
  \link{soc.ca} and \link{soc.csca}}

  \item{dim}{gives the dimensions in the order they are to
  be plotted. The first number defines the horizontal axis
  and the second number defines the vertical axis.}

  \item{map.title}{is the title of the map. If set to its
  default the standard title is used.}

  \item{labelx}{is the label of the horizontal axis. If set
  to NULL a standard label is used.}

  \item{labely}{is the label of the vertical axis. If set
  to NULL a standard label is used.}

  \item{point.label}{if set to TRUE each point is assigned
  its label, defined in the soc.ca object. See
  \link{assign.label} and \link{add.to.label} for ways to
  alter the labels.}

  \item{point.shape}{is a numerical value defining the
  shape of the points. If set to its default, the default
  scale is used. It may be defined by a variable with a
  suitable length.}

  \item{point.variable}{is a factor of the samel length and
  order as the ones used for the soc.ca object. Each level
  is given its own shape.}

  \item{point.size}{is a numerical value defining the size
  of the points. If set to its default, the size is
  determined by the frequency of each modality. It may be
  defined by a variable with a suitable length.}

  \item{text.size}{defines the size of the labels}

  \item{legend}{if set to TRUE a legend is provided}

  \item{colour}{defines the colour of the text labels}

  \item{point.colour}{is a factor of the same length and
  order as the ones used for the soc.ca object.}
}
\description{
  Creates a map of the individuals on two selected
  dimension.
}
\examples{
example(soc.ca)
map.ind(result)
map.ind(result, map.title="Each individual is given its shape according to a value in a factor", point.variable=active[,1], legend=TRUE)
map.ind(result, map.title="The contribution of the individuals with a new color scale defined by ggplot2", point.colour=result$ctr.ind[,1], point.shape=18) + scale_color_continuous(low="white", high="red")
quad   <- create.quadrant(result)
map.ind(result, map.title="Individuals in the space given shape and colour by their quadrant", point.variable=quad, point.colour=quad)
}

