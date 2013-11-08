\name{add.path}
\alias{add.path}
\title{Add a path along an ordered variable onto a map}
\usage{
  add.path(object, x, map = map.ind(object, dim),
    dim = c(1, 2), label = TRUE, min.size = length(x)/10,
    ...)
}
\arguments{
  \item{object}{is a soc.ca result object}

  \item{x}{is an ordered vector, either numerical or
  factor}

  \item{map}{is a plot object created with one of the
  mapping functions in the soc.ca package}

  \item{dim}{is the two dimensions plotted}

  \item{label}{if TRUE the label of the points are shown}

  \item{min.size}{is the minimum size given to the groups
  of a numerical variable, see \link{cut.min}.}

  \item{...}{further arguments are passed onto geom_path,
  geom_point and geom_text from the ggplot2 package}
}
\description{
  Plot a path along an ordered variable. If the variable is
  numerical it is cut into groups by the \link{cut.min}
  function.
}
\examples{
example(soc.mca)
map <- map.ind(result, point.color=as.numeric(sup$Age)) + scale_color_continuous(high="red", low="yellow")
add.path(result, sup$Age, map)
}

