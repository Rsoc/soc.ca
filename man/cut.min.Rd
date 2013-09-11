\name{cut.min}
\alias{cut.min}
\title{Cut a continious variable into categories with a specified minimum}
\usage{
  cut.min(x, min.size = length(x)/10)
}
\arguments{
  \item{x}{is a continious numerical variable}

  \item{min.size}{is the minimum number of individuals in
  each category}
}
\value{
  a numerical vector with the number of each category
}
\description{
  Many continious variables are very uneqaully distributed,
  often with many individuals in the lower categories and
  fewer in the top. As a result it is often difficult to
  create groups of equal size, with unique cut-points. By
  defining the wanted minimum of individuals in each
  category, but still allowing this minimum to be
  surpassed, it is easy to create ordinal variables from
  continious variables. The last category will not
  neccessarily have the minimum number of individuals.
}
\examples{
a <- 1:1000
table(cut.min(a))
b <- c(rep(0, 50), 1:500)
table(cut.min(b, min.size=20))
}

