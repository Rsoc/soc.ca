\name{csa.measures}
\alias{csa.measures}
\title{CSA measures}
\usage{
csa.measures(csa.object, correlations = TRUE, cosines = TRUE,
  cosine.angles = TRUE, dim = 1:5, format = TRUE, ...)
}
\arguments{
  \item{csa.object}{is a "soc.csa" class object created by
  the \link{soc.csa} function}

  \item{correlations}{if TRUE correlations calculated by
  the \link{cor} function is returned}

  \item{cosines}{if TRUE cosine similarities are returned}

  \item{cosine.angles}{if TRUE angles are calculated in the
  basis of the cosine values}

  \item{dim}{is the dimensions included}

  \item{format}{if TRUE results are formatted, rounded and
  printed for screen reading, if FALSE the raw numbers are
  returned}

  \item{...}{furhter arguments are send to the \link{cor}
  function}
}
\value{
A list of measures in either formatted or raw form.
}
\description{
Several measures for the evaluation of the relations
between the dimensions of the CSA and the dimensions the of
original MCA
}
\examples{
example(soc.csa)
csa.measures(res.csa)
csa.measures(res.csa, correlations = FALSE, cosine.angles = FALSE, dim = 1:10, format = FALSE)
}

