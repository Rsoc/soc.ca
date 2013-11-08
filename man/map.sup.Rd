\name{map.sup}
\alias{map.sup}
\title{Map the supplementary modalities}
\usage{
  map.sup(object, dim = c(1, 2), point.shape = "variable",
    point.alpha = 0.8, point.fill = "grey80",
    point.color = "black", point.size = "freq",
    text = TRUE, text.alpha = 0.8, text.color = "black",
    text.size = 3.3, map.title = "sup", labelx = NULL,
    labely = NULL, legend = NULL)
}
\arguments{
  \item{object}{is a soc.mca class object as created by
  \link{soc.mca} and \link{soc.csa}}

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
  its label, defined in the soc.mca object. See
  \link{assign.label} and \link{add.to.label} for ways to
  alter the labels.}

  \item{point.shape}{is a numerical value defining the
  shape of the points. If set to its default, the default
  scale is used. It may be defined by a variable with a
  suitable length.}

  \item{point.size}{is a numerical value defining the size
  of the points. If set to its default, the size is
  determined by the frequency of each modality. It may be
  defined by a variable with a suitable length.}

  \item{text.size}{defines the size of the labels}

  \item{color}{defines the color of both label and the line
  of the points. The fill is set to a standard grey.}
}
\description{
  Creates a map of the supplementary modalities on two
  selected dimension.
}
\examples{
example(soc.mca)
map.sup(result)
map.sup(result, dim=c(2,1))
map.sup(result, point.size=result$coord.sup[,4], map.title="All supplementary modalities with size according to coordinate on the 4th dimension")
}

