\name{print.soc.ca}
\alias{print.soc.ca}
\title{Print soc.ca objects}
\usage{
\method{print}{soc.ca}(x, ...)
}
\arguments{
  \item{x}{is a soc.ca class object}

  \item{...}{further arguments are ignored}
}
\value{
Active dimensions is the number of dimensions remaining
after the reduction of the dimensionality of the analysis.

Active modalities is the number of modalities that are not
set as passive.

Share of passive mass is the percentage of the total mass
that is represented by the passive modalities.

The values represented in the scree plot are the adjusted
inertias, see \link{variance}

The active variables are represented with their number of
active modalities and their share of the total
variance/inertia.
}
\description{
Prints commonly used measures used in the analysis of
multiple correspondence analysis
}
\examples{
example(soc.mca)
print(result)
}
\seealso{
\link{soc.mca}, \link{contribution}
}

