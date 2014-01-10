\name{map.ellipse.array}
\alias{map.ellipse.array}
\title{Ellipse array}
\usage{
map.ellipse.array(object, variable, dim = c(1, 2), draw.ellipses = TRUE,
  ncol = 2, titles = levels(variable), main.title = "", ...)
}
\arguments{
  \item{object}{a soc.ca class object}

  \item{variable}{a factor of the same length as the
  data.frame used to create object}

  \item{dim}{the dimensions in the order they are to be
  plotted. The first number defines the horizontal axis and
  the second number defines the vertical axis.}

  \item{draw.ellipses}{if TRUE ellipses are drawn}

  \item{ncol}{the number of columns the plots are arranged
  into}

  \item{titles}{a vector of the same length as the number
  of levels in variable. These are the titles given to each
  subplot}

  \item{main.title}{the main title for all the plots}

  \item{...}{sends any further arguments to
  \link{map.select}.}
}
\description{
Create seperate maps with ellipses for each level in a
factor arranged in an array.
}
\examples{
\dontrun{
example(soc.mca)
map.ellipse.array(result, active[, 1])
}
}

