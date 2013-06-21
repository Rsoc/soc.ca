\name{map.select}
\alias{map.select}
\title{Map select modalities and individuals}
\usage{
  map.select(object, list.mod = NULL, list.sup = NULL,
    list.ind = NULL, dim = c(1, 2), map.title = "select",
    labelx = NULL, labely = NULL, point.label = TRUE,
    point.shape = "variable", point.size = "freq",
    text.size = 3.3, colour = "black")
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

  \item{point.size}{is a numerical value defining the size
  of the points. If set to its default, the size is
  determined by the frequency of each modality. It may be
  defined by a variable with a suitable length.}

  \item{text.size}{defines the size of the labels}

  \item{legend}{if set to TRUE a legend is provided}

  \item{colour}{defines the colour of the text labels}

  \item{point.colour}{is a factor of the same length and
  order as the ones used for the soc.ca object.}

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
example(soc.ca)
map.select(result, map.title="Map of the first ten modalities",list.mod=1:10)
select   <- active[,3]
select   <- select == levels(select)[2]
map.select(result, map.title="Map of all individuals sharing a particular value", list.ind=select, point.size=3)
map.select(result, map.title="Map of both select individuals and modalities", list.ind=select, list.mod=1:10)
}

