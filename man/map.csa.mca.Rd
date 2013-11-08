\name{map.csa.mca}
\alias{map.csa.mca}
\title{Map the coordinates of the individuals in a CSA and its MCA}
\usage{
  map.csa.mca(csa.object, mca.dim = 1, csa.dim = 1,
    smooth = TRUE, method = "auto")
}
\arguments{
  \item{csa.object}{is a result object created by the
  \link{soc.csa} function}

  \item{mca.dim}{is the dimension from the original MCA}

  \item{csa.dim}{is the dimension from the CSA}

  \item{smooth}{if TRUE a line is added to the plot}

  \item{method}{is the method used by ggplot to set the
  line see \link{geom_smooth}}
}
\description{
  Map the coordinates of the individuals in a CSA and its
  MCA
}
\examples{
example(soc.csa)
csa.res <- soc.csa(result, class.age)
map.csa.mca(csa.res, mca.dim=2, csa.dim=1)
}

