\name{soc.ca}
\alias{soc.ca}
\title{Specific Multiple Correspondence Analysis}
\usage{
  soc.ca(active, sup = NULL, identifier = NULL,
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
  have a full or partial match will be set as passive. See
  \link{set.passive}}
}
\value{
  sv Singular values

  nd Number of active dimensions: #¤ Check the actual
  definition and make a reference to le roux

  n.ind The number of active individuals

  n.mod The number of active modalities

  eigen Eigenvectors

  total.inertia The sum of inertia

  adj.inertia A matrix with all active dimensions, adjusted
  and unadjusted inertias. See \link{variance}

  freq.mod Frequencies for the active modalities. See also
  \link{add.to.label}

  freq.sup Frequencies for the supplementary modalities.
  See also \link{add.to.label}

  ctr.mod A matrix with the contribution values of the
  active modalities per dimension. See \link{contribution}

  ctr.ind A matrix with the contribution values of the
  individuals per dimension. See \link{individuals}

  cor.mod The correlation or quality of each modality per
  dimension.

  cor.ind The correlation or quality of each individual per
  dimension. #¤ This may be defunct!

  mass.mod The mass of each modality

  coord.mod A matrix with the principal coordinates of each
  active modality per dimension.

  coord.ind A matrix with the principal coordinates of each
  individual per dimension.

  coord.sup A matrix with the principal coordinates of each
  supplementary modality per dimension.

  names.mod The names of the active modalities

  names.ind The names of the individuals

  names.sup The names of the supplementary modalities

  names.passive The names of the passive modalities

  modal A matrix with the number of modalities per variable
  and their location

  variable A vector with the name of the variable of the
  active modalities
}
\description{
  \code{soc.ca} performs a specific multiple correspondence
  analysis on a data.frame of factors, where cases are rows
  and columns are variables.
}
\examples{
# This example can be found in further detail at our wiki on github - https://github.com/Rsoc/soc.ca/wiki/How-to-use-soc.ca

# Loads the "directors" dataset included in this package
data(directors)
attach(directors)
# Create a data frame of factors containing all the active variables
active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
                          years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
                          age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
                          education, author, placeofbirth, familyclass_bourdieu,
                          partnersfamily_in_whoswho, family_in_whoswho)
# Create a data frame of factors containing all the supplementary variables
sup       	 <- data.frame(size_prestige, ownership_cat_2, sector, location)

# This is a vector containing names of the cases
id          <- navn

detach(directors)

# Defines what words or phrases that are looked for in the labels of the active modalities.
options(passive= c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))
# Runs the analysis
result      <- soc.ca(active, sup, id)

# Prints the results
result
}
\author{
  Anton Grau Larsen, University of Copenhagen

  Stefan Bastholm Andrade, University of Copenhagen

  Christoph Ellersgaard, University of Copenhagen
}
\seealso{
  \link{add.to.label}, \link{contribution}
}

