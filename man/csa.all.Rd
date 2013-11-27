\name{csa.all}
\alias{csa.all}
\title{csa.all}
\usage{
  csa.all(object, variable, dim = 1:5, ...)
}
\arguments{
  \item{object}{is a soc.mca class object created with
  \link{soc.mca}}

  \item{variable}{a factor with the same length and order
  as the active variables that created the soc.mca object}

  \item{dim}{is the dimensions included in the correlation
  matrixes}

  \item{...}{further arguments are directed to
  \link{csa.measures}}
}
\value{
  \item{results}{a list of \link{soc.csa} result objects}

  \item{cor}{a list of correlation matrixes}

  \item{cosines}{a list of matrixes with cosine values}

  \item{angles}{a list of correlation matrixes with angles
  between dimensions}
}
\description{
  \code{csa.all} performs a class specific correspondence
  analysis for each level in a factor variable
}
\examples{
example(soc.mca)
csa.all(result, taste$Age)
}
\seealso{
  \link{soc.mca}, \link{soc.csa}, \link{cor},
  \link{csa.measures}
}

