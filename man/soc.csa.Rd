\name{soc.csa}
\alias{soc.csa}
\title{Class Specific Multiple Correspondence Analysis}
\usage{
  soc.csa(object, class.indicator, sup = NULL)
}
\arguments{
  \item{object}{is a soc.mca class object created with
  \link{soc.mca}}

  \item{class.indicator}{the row indices of the class
  specific individuals}

  \item{sup}{Defines the supplementary modalities in a
  data.frame with rows of individuals and columns of
  factors, without NA's}
}
\value{
  \item{nd}{Number of active dimensions: # Check the actual
  definition and make a reference to le roux}
  \item{n.ind}{The number of active individuals}
  \item{n.mod}{The number of active modalities}
  \item{eigen}{Eigenvectors} \item{total.inertia}{The sum
  of inertia} \item{adj.inertia}{A matrix with all active
  dimensions, adjusted and unadjusted inertias. See
  \link{variance}} \item{freq.mod}{Frequencies for the
  active modalities. See also \link{add.to.label}}
  \item{freq.sup}{Frequencies for the supplementary
  modalities. See also \link{add.to.label}}
  \item{ctr.mod}{A matrix with the contribution values of
  the active modalities per dimension. See
  \link{contribution}} \item{ctr.ind}{A matrix with the
  contribution values of the individuals per dimension.}
  \item{cor.mod}{The correlation or quality of each
  modality per dimension.} \item{cor.ind}{The correlation
  or quality of each individual per dimension. # This may
  be defunct!} \item{mass.mod}{The mass of each modality}
  \item{coord.mod}{A matrix with the principal coordinates
  of each active modality per dimension.}
  \item{coord.ind}{A matrix with the principal coordinates
  of each individual per dimension.} \item{coord.sup}{A
  matrix with the principal coordinates of each
  supplementary modality per dimension. Notice that the
  position of the supplementary modalities in class
  specific analysis is the mean point of the individuals,
  which is not directly comparable with the cloud of the
  active modalities.} \item{names.mod}{The names of the
  active modalities} \item{names.ind}{The names of the
  individuals} \item{names.sup}{The names of the
  supplementary modalities} \item{names.passive}{The names
  of the passive modalities} \item{modal}{A matrix with the
  number of modalities per variable and their location}
  \item{variable}{A vector with the name of the variable of
  the active modalities} \item{cor.dim}{A correlation
  matrix with the correlations between the original
  dimensions and the class specific dimensions.
  Correlations are Spearman correlations, see \link{cor}}
  \item{cosines}{A matrix of cosine similiarites between
  the original dimensions and the class specific
  dimensions} \item{angles}{A matrix of angles between the
  original dimensions and the class specific dimensions
  calculated on the basis of the cosine matrix}
}
\description{
  \code{soc.csa} performs a class specific multiple
  correspondence analysis on a data.frame of factors, where
  cases are rows and columns are variabels. Most
  descriptive and analytical functions that work for
  \link{soc.mca}, also work for \code{soc.csa}
}
\examples{
# This example can be found in further detail at our wiki on github - https://github.com/Rsoc/soc.mca/wiki/How-to-use-soc.mca
example(soc.mca)
class.age    <- which(taste$Age =='55-64')
soc.csa(result, class.age)
}
\author{
  Anton Grau Larsen, University of Copenhagen

  Stefan Bastholm Andrade, University of Copenhagen

  Christoph Ellersgaard, University of Copenhagen
}
\references{
  Le Roux, B., og H. Rouanet. 2010. Multiple correspondence
  analysis. Thousand Oaks: Sage.
}
\seealso{
  \link{add.to.label}, \link{contribution}
}

