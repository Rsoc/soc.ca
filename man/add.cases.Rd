% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_build_plot.r
\name{add.cases}
\alias{add.cases}
\alias{add.ind}
\title{Add a layer of cases (individuals) to an mca map}
\usage{
add.cases(
  object,
  dim = c(1, 2),
  ind = extract_ind(object, dim),
  mapping = aes(),
  ...
)

add.ind(
  object,
  dim = c(1, 2),
  ind = extract_ind(object, dim),
  mapping = aes(),
  ...
)
}
\arguments{
\item{object}{a soc.mca result object}

\item{dim}{a numeric vector with the plotted dimensions}

\item{ind}{a data.frame with coordinates of cases as produced by \link{extract_ind}. This controls the plotted points.}

\item{mapping}{a call to \link{aes} from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and shape.}

\item{...}{further arguments are passed on to geom_point()}
}
\value{
a ggplot2 object that can be added to an existing plot like those produced by \link{map.ca.base}
}
\description{
Add a layer of cases (individuals) to an mca map
}
\examples{
example(soc.mca)
map.ca.base() + add.cases(result)
}
