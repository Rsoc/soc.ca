\name{map.add}
\alias{map.add}
\title{Add points to an existing map created by one of the soc.ca mapping functions.}
\usage{
map.add(object, ca.map, plot.type = NULL, ctr.dim = 1, list.mod = NULL,
  list.sup = NULL, list.ind = NULL, point.shape = "variable",
  point.alpha = 0.8, point.fill = "whitesmoke", point.color = "black",
  point.size = "freq", label = TRUE, label.alpha = 0.8,
  label.color = "black", label.size = 4, labelx = "default",
  labely = "default", legend = NULL)
}
\arguments{
  \item{object}{a soc.ca class object as created by
  \link{soc.mca} and \link{soc.csa}}

  \item{ca.map}{a map created using one of the soc.ca map
  functions}

  \item{plot.type}{defines which type of points to add to
  the map. Accepted values are: "mod", "sup", "ind", "ctr".
  These values correspond to the different forms of}

  \item{list.mod}{a numerical vector indicating which
  active modalities to plot. It may also be a logical
  vector of the same length and order as the modalities in
  object$names.mod.}

  \item{list.sup}{a numerical vector indicating which
  supplementary modalities to plot. It may also be a
  logical vector of the same length and order as the
  modalities in object$names.sup.}

  \item{list.ind}{a numerical vector indicating which
  individuals to plot. It may also be a logical vector of
  the same length and order as the modalities in
  object$names.ind.}

  \item{dim}{the dimensions in the order they are to be
  plotted. The first number defines the horizontal axis and
  the second number defines the vertical axis.}

  \item{ctr.dim}{the dimensions of the contribution values}

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
Add points to an existing map created by one of the soc.ca
mapping functions.
}
\examples{
example(soc.ca)
original.map    <- map.sup(result)
map.add(result, original.map, plot.type = "ctr", ctr.dim = 2)
map.add(result, map.ind(result), plot.type = "select",list.ind = 1:50,
 point.color = "red", label = FALSE, point.size = result$ctr.ind[1:50, 1]*2000)
}

