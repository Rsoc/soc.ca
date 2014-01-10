\name{map.mod}
\alias{map.mod}
\title{Map all modalities}
\usage{
map.mod(object, dim = c(1, 2), point.shape = "variable",
  point.alpha = 0.8, point.fill = "grey80", point.color = "black",
  point.size = "freq", label = TRUE, label.alpha = 0.8,
  label.color = "black", label.size = 3.3, map.title = "mod",
  labelx = "default", labely = "default", legend = NULL)
}
\arguments{
  \item{object}{a soc.ca class object as created by
  \link{soc.mca} and \link{soc.csa}}

  \item{dim}{the dimensions in the order they are to be
  plotted. The first number defines the horizontal axis and
  the second number defines the vertical axis.}

  \item{map.title}{the title of the map. If set to its
  default the standard title is used.}

  \item{labelx}{the label of the horizontal axis. If set to
  NULL a standard label is used.}

  \item{labely}{the label of the vertical axis. If set to
  NULL a standard label is used.}

  \item{point.shape}{a numerical value defining the shape
  of the points. If set to its default, the default scale
  is used. It may be mapped to a variable with a suitable
  length and order.}

  \item{point.alpha}{defines the alpha of the points.
  Values range from 0 to 1.  It may be mapped to a variable
  with a suitable length and order.}

  \item{point.fill}{defines the fill color of the points.
  It may be mapped to a variable with a suitable length and
  order.}

  \item{point.color}{defines the color of the points. It
  may be mapped to a variable with a suitable length and
  order. See \link{colors} for some of the valid values.}

  \item{point.size}{a numerical value defining the size of
  the points. If set to its default, the size is determined
  by the frequency of each modality.  It may be defined by
  a variable with a suitable length.}

  \item{label}{if TRUE each point is assigned its label,
  defined in the soc.ca object. See \link{assign.label} and
  \link{add.to.label} for ways to alter the labels.}

  \item{label.alpha}{defines the alpha of the labels.
  Values range from 0 to 1.  It may be mapped to a variable
  with a suitable length and order.}

  \item{label.color}{defines the color of the labels. It
  may be mapped to a variable with a suitable length and
  order. See \link{colors} for some of the valid values.}

  \item{label.size}{defines the size of the labels. It may
  be mapped to a variable with a suitable length and
  order.}

  \item{legend}{if set to TRUE a legend is provided. Change
  the legend with the \link{guides}, \link{theme} and
  link{guide_legend} functions from the ggplot2 package.}
}
\description{
Creates a map of all active and supplementary modalities on
two selected dimension.
}
\examples{
example(soc.mca)
map.mod(result)
map.mod(result, dim = c(3, 2), point.size = 2)
}

