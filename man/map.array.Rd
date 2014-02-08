\name{map.array}
\alias{map.array}
\title{Array of maps}
\usage{
map.array(x, ncol = 1, title = "", fixed.coord = TRUE, padding = 0.15)
}
\arguments{
  \item{x}{a list of objects created by one of the mapping
  functions in the soc.ca package or any other ggplot2
  plot}

  \item{ncol}{the number of columns the plots are arranged
  into}

  \item{title}{the main title of the array}

  \item{fixed.coord}{if TRUE the limits of all plots are
  set to the same as the largest plot}

  \item{padding}{the distance between the most extreme
  position and the axis limit}
}
\description{
This function takes a list of map objects and arranges them
into an array.
}
\examples{
\dontrun{
example(soc.ca)
map.array(list(map.ind(result), map.mod(result)), ncol = 2)
}
}

