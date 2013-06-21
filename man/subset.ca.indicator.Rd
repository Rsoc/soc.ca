\name{subset.ca.indicator}
\alias{subset.ca.indicator}
\title{Correspondence analysis on a indicator matrix}
\usage{
  subset.ca.indicator(indicator.act, indicator.sup, subset,
    Q, Qm)
}
\arguments{
  \item{indicator.act}{An indicator matrix of all the
  active modalities (including those that are to be set as
  passive)}

  \item{indicator.sup}{An indicator matrix of the
  supplementary modalities}

  \item{subset}{A vector containing column indices of
  passive modalities}

  \item{Q}{The number of variables}

  \item{Qm}{The number of variables without passive
  modalities}
}
\value{
  a list of various results. See \link{soc.ca}
  documentation
}
\description{
  This function is part of the soc.ca function but allows
  for manipulation of the indicator matrix before analysis.
  Most users will not need this function.
}

