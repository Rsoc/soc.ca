\name{map.add}
\alias{map.add}
\title{Add points to an existing map}
\usage{
  map.add(object, ca.map, data.type = NULL,
    list.mod = NULL, list.sup = NULL, list.ind = NULL,
    ctr.dim = NULL, point.label = TRUE,
    point.shape = "variable", point.size = "freq",
    text.size = 3.3, colour = "black")
}
\arguments{
  \item{object}{is a soc.mca class object as created by
  \link{soc.mca} and \link{soc.csa}}

  \item{ca.map}{is a map created using one of the soc.mca
  map functions}

  \item{data.type}{defines which type of points to add to
  the map. Accepted values are: "mod", "sup", "ind", "ctr".
  These values correspond to the different forms of}

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

  \item{ctr.dim}{is dimensions from which the contribution
  values are taken.}

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

  \item{colour}{defines the colour of the text labels}
}
\description{
  Add points to an existing map
}
\examples{
example(soc.mca)
original.map    <- map.sup(result)
map.add(result, original.map, data.type="ctr", ctr.dim=2)
map.add(result, map.ind(result), data.type="ind",list.ind=1:50, colour="red", point.label=FALSE, point.size=12)
}

