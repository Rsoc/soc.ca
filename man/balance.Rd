\name{balance}
\alias{balance}
\title{Contribution balance}
\usage{
  balance(object, act.dim = object$nd)
}
\arguments{
  \item{object}{is a soc.ca class object}

  \item{act.dim}{is the number of active dimensions to be
  measured}
}
\value{
  A matrix with the share of contribution on each side of 0
  and their balance (+/-)
}
\description{
  Calculates the balance of the contribution of each
  dimension. This measure indicates whether too much of a
  dimensions contribution is placed on either the + or -
  side of the dimension.
}
\examples{
example(soc.mca)
balance(result)
balance(result, act.dim = 3)
}
\seealso{
  \link{soc.mca}, \link{contribution}
}

