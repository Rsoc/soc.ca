\name{map.select}
\alias{map.select}
\title{Map select modalities and individuals}
\usage{
  map.select(object, dim = c(1, 2), ctr.dim = 1,
    list.mod = NULL, list.sup = NULL, list.ind = NULL,
    point.shape = "variable", point.alpha = 0.8,
    point.fill = "grey80", point.color = "black",
    point.size = "freq", text = TRUE, text.alpha = 0.8,
    text.color = "black", text.size = 3.3,
    map.title = "select", labelx = NULL, labely = NULL,
    legend = NULL)
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
  of the points. If set to "freq", the size is determined
  by the frequency of each modality. It may be defined by a
  variable with a suitable length.}

  \item{text.size}{defines the size of the labels}

  \item{color}{defines the color of the text labels}

  \item{list.mod}{is a numerical vector indicating which
  active modalities to plot. It may also be a logical
  vector of the same length and order as the modalities in
  object$names.mod.}

  \item{list.sup}{is a numerical vector indicating which
  supplementary modalities to plot. It may also be a
  logical vector of the same length and order as the
  modalities in object$names.sup.}

  \item{list.ind}{is a numerical vector indicating which
  individuals to plot. It may also be a logical vector of
  the same length and order as the modalities in
  object$names.ind.}
}
\description{
  Creates a map of selected modalities or individuals
}
\examples{
example(soc.mca)
map.select(result, map.title="Map of the first ten modalities",list.mod=1:10)
select   <- active[,3]
select   <- select == levels(select)[2]
map.select(result, map.title="Map of all individuals sharing a particular value", list.ind=select, point.size=3)
map.select(result, map.title="Map of both select individuals and modalities", list.ind=select, list.mod=1:10)
}

