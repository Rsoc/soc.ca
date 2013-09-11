\name{soc.mca}
\alias{soc.mca}
\title{Specific Multiple Correspondence Analysis}
\usage{
  soc.mca(active, sup = NULL, identifier = NULL,
    passive = getOption("passive", default = "Missing"))
}
\arguments{
  \item{active}{Defines the active modalities in a
  data.frame with rows of individuals and columns of
  factors, without NA's'}

  \item{sup}{Defines the supplementary modalities in a
  data.frame with rows of individuals and columns of
  factors, without NA's}

  \item{identifier}{A single vector containing a single
  value for each row/individual in x and sup. Typically a
  name or an id.number.}

  \item{passive}{A single character vector with the full or
  partial names of the passive modalities. All names that
  have a full or partial match will be set as passive.}
}
\value{
  \item{nd}{Number of active dimensions} \item{n.ind}{The
  number of active individuals} \item{n.mod}{The number of
  active modalities} \item{eigen}{Eigenvectors}
  \item{total.inertia}{The sum of inertia}
  \item{adj.inertia}{A matrix with all active dimensions,
  adjusted and unadjusted inertias. See \link{variance}}
  \item{freq.mod}{Frequencies for the active modalities.
  See also \link{add.to.label}} \item{freq.sup}{Frequencies
  for the supplementary modalities. See also
  \link{add.to.label}} \item{ctr.mod}{A matrix with the
  contribution values of the active modalities per
  dimension. See \link{contribution}} \item{ctr.ind}{A
  matrix with the contribution values of the individuals
  per dimension.} \item{cor.mod}{The correlation or quality
  of each modality per dimension.} \item{cor.ind}{The
  correlation or quality of each individual per dimension.
  # This may be defunct!} \item{mass.mod}{The mass of each
  modality} \item{coord.mod}{A matrix with the principal
  coordinates of each active modality per dimension.}
  \item{coord.ind}{A matrix with the principal coordinates
  of each individual per dimension.} \item{coord.sup}{A
  matrix with the principal coordinates of each
  supplementary modality per dimension.}
  \item{names.mod}{The names of the active modalities}
  \item{names.ind}{The names of the individuals}
  \item{names.sup}{The names of the supplementary
  modalities} \item{names.passive}{The names of the passive
  modalities} \item{modal}{A matrix with the number of
  modalities per variable and their location}
  \item{variable}{A vector with the name of the variable of
  the active modalities}
}
\description{
  \code{soc.mca} performs a specific multiple
  correspondence analysis on a data.frame of factors, where
  cases are rows and columns are variables.
}
\examples{
# This example can be found in further detail at our wiki on github - https://github.com/Rsoc/soc.mca/wiki/How-to-use-soc.mca

# Loads the "taste" dataset included in this package
data(taste)
attach(taste)
# Create a data frame of factors containing all the active variables
taste          <- taste[which(taste$Isup =='Active'),]

attach(taste)
active         <- data.frame(TV, Film, Art, Eat)
sup            <- data.frame(Gender, Age, Income)
detach(taste)

# Runs the analysis
result      <- soc.mca(active, sup)

# Prints the results
result

# A specific multiple correspondence analysis
# options defines what words or phrases that are looked for in the labels of the active modalities.
options(passive= c("Film: CostumeDrama", "TV: Tv-Sport"))
soc.mca(active, sup)
options(passive=NULL)
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
  \link{soc.csa}, \link{contribution}
}

