#' Soc.ca a package for specific correspondence analysis
#' 
#' This package is optimized to the needs of scientists within the social 
#' sciences. The soc.ca package produces specific and class specific multiple
#' correspondence analysis on survey-like data. Soc.ca is optimized to only give
#' the most essential statistical output sorted so as to help in analysis. 
#' Seperate functions exists for near publication-ready plots and tables.
#' 
#' We are in debt to the work of others, especially Brigitte Le Roux and Henry
#' Rouanet for the mathematical definitions of the method and their examples.
#' Furthermore this package was initially based on code from the ca package
#' written by Michael Greenacre and Oleg Nenadic.
#' 
#' If you are looking for features that are absent in soc.ca, it may be 
#' available in some of these packages for correspondence analysis: \pkg{ca},
#' \pkg{anacor} and \pkg{FactoMineR}.
#' 
#' @references Le Roux, Brigitte, and Henry Rouanet. 2010. Multiple 
#'   correspondence analysis. Thousand Oaks: Sage.
#' @references Le Roux, Brigitte, and Henry Rouanet. 2004. Geometric Data 
#'   Analysis from Correspondence Analysis to Structured Data Analysis. 
#'   Dordrecht: Kluwer Academic Publishers.
#'   
#' @docType package
#' @name soc.ca
#' @import ggplot2
#' @import gridExtra
#' @import ellipse
#' @import stats
#' @import utils
#' @import shiny
#' @import reshape2
#' @import ggrepel
#' @examples
#' data(taste)
#' # Create a data frame of factors containing all the active variables 
#' taste          <- taste[which(taste$Isup == 'Active'), ]
#'
#' attach(taste)
#' active         <- data.frame(TV, Film, Art, Eat)
#' sup            <- data.frame(Gender, Age, Income)
#' detach(taste)
#' 
#' # Runs the analysis
#' result         <- soc.mca(active, sup)
NULL

#'Directors dataset
#'
#'Prosopographical data on the top 100 CEO's from the 82 largest Danish 
#'corporations.
#'@details The directors dataset is prosopographical data collected from a wide 
#'  array of sources on biographic and corporate information. Sources include 
#'  the Danish variant of Who's Who (Blaa Bog), a private business information 
#'  database (Greens Erhvervsinformation), journalistic portrait articles, 
#'  article search engines, bibliographic databases and financial reports. CEOs 
#'  from 82 corporations were selected according to their position as CEO in 
#'  December 2007. 18 executives are included on other criteria, taking into 
#'  account the magnitude of the corporations and issues regarding ownership and
#'  control, resulting in a final population of 100 CEOs. The 82 corporations 
#'  have formal ownership and management located in Denmark and were selected 
#'  through either financial capital, measured as having a turnover of over five
#'  billion DKK (650 million Eur.), or organizational capital, defined as having
#'  at least 5000 employees; 34 corporations were included on both criteria, 45 
#'  on financial capital and three on organizational capital alone. To avoid 
#'  including investors, rather than executives, a minimum of 500 employees was 
#'  also required, excluding 12 firms. Companies acting only as subsidiaries 
#'  were also excluded. Data is for public use  and no author permission is 
#'  needed, but we would love to hear from you if you find the data useful. The 
#'  following example is based on the analysis from the article: "A Very 
#'  Economic Elite: The Case of the Danish Top CEOs".
#'  
#'@name directors
#'@docType data
#'@author Christoph Ellersgaard
#'@author Anton Grau Larsen
#'@references Ellersgaard, Christoph, Anton Grau Larsen, og Martin D. Munk. 
#'  2012. "A Very Economic Elite: The Case of the Danish Top CEOs". Sociology.
#'@references Ellersgaard, Christoph Houman, og Anton Grau Larsen. 2010. 
#'  "Firmaets Maend". Master Thesis, Copenhagen: University of Copenhagen.
#'@references Ellersgaard, Christoph Houman, og Anton Grau Larsen. 2011. 
#'  "Kulturel kapital blandt topdirektoerer i Danmark - En domineret 
#'  kapitalform?" Dansk Sociologi 22(3):9-29.
#'@references Larsen, Anton Grau, og Christoph Houman Ellersgaard. 2012. "Status
#'  og integration paa magtens felt for danske topdirektoerer". Praktiske 
#'  Grunde. Nordisk tidsskrift for kultur- og samfundsvidenskab 2012(2-3).
#'@keywords data
#' @examples
#' \dontrun{
#' data(directors)
#' attach(directors)
#' 
#' 
#' active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
#'                           years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
#'                           age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
#'                           education, author, placeofbirth, familyclass_bourdieu,
#'                           partnersfamily_in_whoswho, family_in_whoswho)
#' 
#' sup       	<- data.frame(size_prestige, ownership_cat_2, sector, location)
#' 
#' id          <- navn
#' 
#' options(passive = c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))
#' 
#' result      <- soc.mca(active, sup, id)
#' 
#' result
#' 
#' # Contribution
#' contribution(result, 1)
#' contribution(result, 2)
#' contribution(result, 3)
#' contribution(result, 1, all = TRUE)
#' contribution(result, 1, indices = TRUE)
#' contribution(result, 1, mode = "mod")
#' contribution(result, mode = "variable")
#' 
#' # Individuals
#' contribution(result, 1, mode = "ind")
#' contribution(result, 2, mode = "ind")
#' 
#' 
#' # Table of variance
#' variance(result)
#' 
#' # Invert
#' result      <- invert(result, c(1, 2, 3))
#' 
#' # Export and assign label
#' # export.label(result)
#' 
#' # result      <- assign.label(result,
#' #  file = "https://raw.github.com/Rsoc/soc.ca/master/extra/director_labels.csv")
#' 
#' 
#' 
#' # Add.n
#' result      <- add.to.label(result)
#' contribution(result, 2)
#' 
#' 
#' # The result object or "soc.ca" object
#' str(result)
#' dim1 <- result$coord.ind[, 1]
#' qplot(dim1)
#' 
#' # Quadrant
#' quad      <- create.quadrant(result)
#' table(quad)
#' quad      <- create.quadrant(result, cut.min = 0, cut.max = 0)
#' table(quad)
#' 
#' 
#' # Map of individuals
#' map.ind(result)
#' map.ind(result, dim = c(2, 1), label = TRUE)
#' map.ind(result, dim = c(2, 1), point.size = 3, point.shape = 2)
#' map.ind(result, dim = c(2, 1), map.title = "The top 100 Danish CEO's",
#' point.color = quad)
#' # Map of the individuals colored by contribution
#' map.ind(result, point.color = result$ctr.ind[, 1],
#'  point.shape = 18) + scale_color_continuous(low = "white", high = "red")
#' 
#' 
#' # Map of contributing modalities
#' map.ctr(result, dim = c(2, 1))
#' map.ctr(result, dim = c(2, 1), ctr.dim = 2)
#' map.ctr(result, point.size = 3)
#'
#' map.active(result, dim = c(2, 1))
#' map.sup(result, dim = c(2, 1))
#' 
#' # Plot.list
#' 
#' # Selecting specific active modalities
#' select      <- c("Career start: Corporation (n:57)", "No Phd (n:92)")
#' boo.select  <- match(select, result$names.mod)
#' map.select(result, list.mod = boo.select)
#' 
#' highcor     <- which(result$cor.mod[, 1] >= 0.2) 
#' map.select(result, list.mod = highcor)
#' 
#' # Selecting specific supplementary modalities
#' 
#' highdim3    <- which(sqrt(result$coord.sup[, 3]^2) >= 0.5)
#' map.select(result, list.sup = highdim3)
#' 
#' # Selecting specific individuals based on a certain criteria
#' 
#' forfatter   <- author == "Forfatter"
#' map.select(result, list.ind = forfatter)
#' 
#' # Combining it all
#' map.select(result, list.mod = highcor, list.sup = highdim3, list.ind = forfatter)
#' 
#' # Add points to an existing plot
#' ctrplot     <- map.ctr(result, ctr.dim = 1, point.color = "red")
#' map.add(result, ctrplot, data.type = "ctr", ctr.dim = 2, point.color = "blue")
#' 
#' # Using the list option in add.points
#' forfatter    <- author == "Forfatter"
#' map.add(result, ctrplot, data.type = "select", list.ind = forfatter, colour = "purple")
#' 
#' # Using the list option in add.points to add labels to only a part of the cloud of individuals
#' forfatter     <- author == "Forfatter"
#' notforfatter  <- author != "Forfatter"
#' map.forfatter <- map.select(result, list.ind = notforfatter, label = FALSE)
#' map.forfatter
#' map.forfatter <- map.add(result, map.forfatter, data.type = "select", list.ind = forfatter)
#' map.forfatter
#' 
#' # Plotting all the modalities of one individual
#' result2       <- soc.ca(active, sup, id)
#' individual    <- which(id == "Lars Larsen")
#' ind.mat       <- indicator(active)
#' modalities    <- names(which(ind.mat[individual, ] == 1))
#' mod.ind       <- match(modalities, result2$names.mod)
#' 
#' lars          <- map.select(result2, list.mod = mod.ind)
#' map.add(result2, lars, data.type = "select", list.ind = individual, colour = "red")
#' 
#' # Adding concentration ellipses to an existing plot
#' el.forfatter  <- map.ellipse(result, map.forfatter, author)
#' el.forfatter
#' }
NULL

#' Taste dataset
#' 
#' The taste example dataset used by Le Roux & Rouanet(2010):
#' @return
#' The variables included in the dataset:
#' \item{Preferred TV program}{(8 categories): news, comedy, police, nature, sport, films, drama, soap operas}
#' \item{Preferred Film}{(8 categories): action, comedy, costume drama, documentary, horror, musical, romance, SciFi}
#' \item{Preferred type of Art}{(7 categories): performance, landscape, renaissance, still life, portrait, modern, impressionsism}
#' \item{Preferred place to Eat out}{(6 categories): fish & chips, pub, Indian restuarant, Italian restaurant, French restaurant, steak house}
#' @name taste
#' @docType data
#' @author Brigitte Le Roux
#' @references Le Roux, Brigitte, Henry Rouanet, Mike Savage, og Alan Warde. 2008. "Class and Cultural Division in the UK". Sociology 42(6):1049-1071.
#' @references Le Roux, B., og H. Rouanet. 2010. Multiple correspondence analysis. Thousand Oaks: Sage.
#' @keywords data
#' @examples
#' \dontrun{
#' # The taste example
#' data(taste)
#' data_taste           <- taste[which(taste$Isup == 'Active'), ]
#' active               <- data.frame(data_taste$TV, data_taste$Film, data_taste$Art, data_taste$Eat)
#' sup                  <- data.frame(data_taste$Gender, data_taste$Age, data_taste$Income)
#' 
#' # Multiple Correspondence Analysis
#' result.mca     <- soc.mca(active, sup)
#' str(result.mca)
#' result.mca
#' 
#' variance(result.mca) # See p.46 in Le Roux(2010)
#' 
#' contribution(result.mca, 1)
#' contribution(result.mca, 2)
#' contribution(result.mca, 1:3, mode = "variable")
#' 
#' map.active(result.mca, point.fill = result.mca$variable)
#' map.active(result.mca,
#'  map.title="Map of active modalities with size of contribution to 1. dimension",
#'  point.size=result.mca$ctr.mod[, 1])
#' map.active(result.mca, 
#'  map.title="Map of active modalities with size of contribution to 2. dimension",
#'  point.size=result.mca$ctr.mod[, 2])
#' 
#' map.ind(result.mca)
#' map.ind(result.mca, dim=c(1, 2), point.color=result.mca$ctr.ind[, 1],
#'  point.shape=18) + scale_color_continuous(low="white", high="black")
#' 
#' # Plot of all dublets
#' map.ind(result.mca, map.title="Map of all unique individuals", point.color=duplicated(active))
#' map.ind(result.mca, map.title="Map with individuals colored by the TV variable",
#'  point.color=active$TV)
#' 
#' # Ellipse 
#' map             <- map.ind(result.mca)
#' map.ellipse(result.mca, map, as.factor(data_taste$Age == '55-64'))
#' 
#' ##### Specific Multiple Correspondence Analysis
#' options(passive= c("Film: CostumeDrama", "TV: Tv-Sport"))
#' result.smca     <- soc.mca(active, sup)
#' result.smca
#' result.smca$names.passive
#' 
#' ##### Class Specific Correspondence Analysis
#' options(passive=NULL)
#' 
#' class.age     <- which(data_taste$Age == '55-64')
#' result.csca   <- soc.csa(result.mca, class.age, sup)
#' str(result.csca)
#' # Correlations
#' csa.measures(result.csca)
#' variance(result.csca)
#' contribution(result.csca, 1)
#' contribution(result.csca, 2)
#' contribution(result.csca, 1:3, mode = "variable")
#' 
#' # Plots
#' map.ind(result.csca)
#' map.csa.mca(result.csca)
#' map.csa.mca.array(result.csca)
#' }
NULL

#' Moschidis example
#' 
#' The example dataset used by Odysseas E. Moschidis (2009):
#' @name moschidis
#' @docType data
#' @author Odysseas E. Moschidis
#' @references Moschidis, Odysseas E. “A Different Approach to Multiple Correspondence Analysis (MCA) than That of Specific MCA.” Mathématiques et Sciences Humaines / Mathematics and Social Sciences 47, no. 186 (October 15, 2009): 77–88. https://doi.org/10.4000/msh.11091.
#' @keywords data
#' @examples
#' # The moschidis example
#' data(moschidis)
#' active    <- moschidis[, c("E1","E2", "E3")]
#' id        <- moschidis[, c("ID")]
#' result    <- soc.mca(active, identifier = id, Moschidis = FALSE)
#' 
#' # Compare output to Moschidis (2009, p. 85)
#' result$inertia_full
#' # In the analysis of the 'real' data the modality 'E1: 1' with a low mass (fr/Q) has a very high contribution to the fourth axis
#' result$ctr.mod[, 4]
#' 
#' # Using the transformed model suggested by Moschidis (2009) that takes into account the number of modalities per question in order to balance the contribution of the modalities 
#' result_trans    <- soc.mca(active, identifier = id, Moschidis = TRUE)
#' result_trans$inertia_full
#' result_trans$ctr.mod[, 4]
NULL


#' The Field of the Danish Power Elite
#' 
#' This dataset was used to construct a field of the Danish Power Elite from 2013
#' @name pe13
#' @docType data
#' @author Jacob Lunding, Anton Grau Larsen and Christoph Ellersgaard
#' @keywords data
#' @examples
NULL

#' French Political Space example
#' 
#' The example dataset used by Brigitte Le Roux & Henry Rouanet (2004):
#' @name political_space97
#' @docType data
#' @author Brigitte Le Roux
#' @references Perrineau, Pascal, Jean Chiche, Brigitte Le Roux, and Henry Rouanet. “L’espace politique des électeurs français à la fin des années 1990: nouveaux et anciens clivages, hétérogénéité des électorats.” Revue Francaise de Science Politique, no. 3 (June 2000): 463–88.
#' @references Le Roux, Brigitte, and Henry Rouanet. Multiple Correspondence Analysis. Thousand Oaks, Calif.: Sage Publications, 2010.
#' @keywords data
#' @examples
#' # French Political Space example
#' data(political_space97)
#' #Recoding
#' political_space97$Democracy <- ifelse(political_space97$Democracy %in% 1:2, "1_2", political_space97$Democracy)
#' political_space97$Politicians <- ifelse(political_space97$Politicians %in% 1:2, "1_2", political_space97$Politicians)
#' 
#' #Assigning questions to themes
#' ethno   <- data.frame(Immigrants = political_space97$Immigrants, "North-Africans" = political_space97$NorthAfricans, 
#'                       Races = political_space97$Races, "At home" = political_space97$AtHome, check.names = FALSE)
#' autho   <- data.frame("Death Penalty" = political_space97$DeathPenalty, School = political_space97$School, check.names = FALSE)
#' social  <- data.frame("Strike Effectiveness" = political_space97$StrikeEffectivness, "Strike 95" = political_space97$Strike95,
#'                       "Unions" = political_space97$Unions, "Public services" = political_space97$PublicServices, check.names = FALSE)
#' 
#' economy <- data.frame(Liberalism = political_space97$Liberalism, Profit = political_space97$Profit, Privatization = political_space97$Privatization,
#'                       Globalization = political_space97$Globalization, check.names = FALSE)
#' politics <- data.frame(Democracy = political_space97$Democracy, Politicians = political_space97$Politicians, check.names = FALSE)
#' supranat <- data.frame(Euro = political_space97$Euro, "EU Power" = political_space97$EUpower, 
#'                        "End EU" = political_space97$EndEU, "EU protection" = political_space97$EUprotection, check.names = FALSE)
#' 
#' # Creating and naming list of headings
#' active <- list(ethno, autho, social, economy, politics, supranat)
#' names(active) <- c("Ethnocentrism", "Authoritarianism", "Social", "Economy", "Politics", "Supranationality")
#' sup    <- data.frame(political_space97$Vote)
#' 
#' result <- soc.mca(active, sup = sup, passive = ": 5")
#' headings(result)
#' map.active(result, point.color = result$headings, point.shape = result$headings, label.color = result$headings)
NULL
files <- list.files(path = "R/", full.names = T)
lapply(files, readLines)
# Functions analysis

#' Specific Multiple Correspondence Analysis
#'
#' \code{soc.mca} performs a specific multiple correspondence analysis on a data.frame of factors, where cases are rows and columns are variables.
#' @param active       Defines the active modalities in a data.frame with rows of individuals and columns of factors, without NA's'. Active can also be a named list of data.frames. The data.frames will correspond to the analytical headings.
#' @param sup          Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
#' @param identifier   A single vector containing a single value for each row/individual in x and sup. Typically a name or an id.number.
#' @param passive      A single character vector with the full or partial names of the passive modalities. All names that have a full or partial match will be set as passive.
#' @param Moschidis    If TRUE adjusts contribution values for rare modalities. see \link{moschidis}.
#' @param detailed.results If FALSE the result object is trimmed to reduce its memory footprint.
#' 
#' @return \item{nd}{Number of active dimensions}
#'  \item{n.ind}{The number of active individuals}
#'  \item{n.mod}{The number of active modalities}
#'  \item{eigen}{Eigenvectors}
#'  \item{total.inertia}{The sum of inertia}
#'  \item{adj.inertia}{A matrix with all active dimensions, adjusted and unadjusted inertias. See \link{variance}}
#'  \item{freq.mod}{Frequencies for the active modalities. See \link{add.to.label}}
#'  \item{freq.sup}{Frequencies for the supplementary modalities. See \link{add.to.label}}
#'  \item{ctr.mod}{A matrix with the contribution values of the active modalities per dimension. See \link{contribution}}
#'  \item{ctr.ind}{A matrix with the contribution values of the individuals per dimension.}
#'  \item{cor.mod}{The correlation or quality of each modality per dimension.}
#'  \item{cor.ind}{The correlation or quality of each individual per dimension.}
#'  \item{mass.mod}{The mass of each modality}
#'  \item{coord.mod}{A matrix with the principal coordinates of each active modality per dimension.}
#'  \item{coord.ind}{A matrix with the principal coordinates of each individual per dimension.}
#'  \item{coord.sup}{A matrix with the principal coordinates of each supplementary modality per dimension.}
#'  \item{names.mod}{The names of the active modalities}
#'  \item{names.ind}{The names of the individuals}
#'  \item{names.sup}{The names of the supplementary modalities}
#'  \item{names.passive}{The names of the passive modalities}
#'  \item{modal}{A matrix with the number of modalities per variable and their location}
#'  \item{variable}{A character vector with the name of the variable of the active modalities}
#'  \item{Rosenlund.tresh}{A numeric vector with the contribution values adjusted with the Rosenlund threshold, see:  see p 92 in: Rosenlund, Lennart. Exploring the City with Bourdieu: Applying Pierre Bourdieu’s Theories and Methods to Study the Community. Saarbrücken: VDM Verlag Dr. Müller, 2009.}
#'  \item{t.test.sup}{A matrix with a the student t-test of the coordinates of the supplementary variables}
#'  \item{Share.of.var}{A matrix the share of variance for each variable}
#'  
#' @name soc.mca
#' @references Le Roux, B., og H. Rouanet. 2010. Multiple correspondence analysis. Thousand Oaks: Sage.
#' @author Anton Grau Larsen
#' @author Jacob Lunding
#' @author Stefan Bastholm Andrade
#' @author Christoph Ellersgaard
#' @seealso \link{soc.csa}, \link{contribution}
#' @examples
#' # Loads the "taste" dataset included in this package
#' data(taste)
#' # Create a data frame of factors containing all the active variables 
#' taste          <- taste[which(taste$Isup == 'Active'), ]
#'
#' attach(taste)
#' active         <- data.frame(TV, Film, Art, Eat)
#' sup            <- data.frame(Gender, Age, Income)
#' detach(taste)
#' 
#' # Runs the analysis
#' result         <- soc.mca(active, sup)
#' 
#' # Prints the results
#' result
#' 
#' # A specific multiple correspondence analysis
#' # options defines what words or phrases that are looked for in the labels of the active modalities.
#' options(passive = c("Film: CostumeDrama", "TV: Tv-Sport"))
#' soc.mca(active, sup)
#' options(passive = NULL)
#' @export

soc.mca <- function(active, sup = NULL, identifier = NULL, passive = getOption("passive", default = "Missing"), Moschidis = FALSE,
                    detailed.results = FALSE) {

  # Preparing data 
  data.type <- what.is.x(active)
  
  ############################################################################
  # If active is not a list and input data is not an indicator.matrix (which is default)
  ############################################################################
  if (data.type == "data.frame") {
    active  <- data.frame(lapply(active, factor), check.names = F)
    a.r     <- nrow(active)
    ind.act <- indicator(active)
    headings = NULL                      # As default, no headings are defined
        }
  
  ############################################################################
  # If active is not a list and input data is an indicator.matrix, active is used as it is
  ############################################################################
  
  if (data.type == "indicator") {
    a.r <- nrow(active)
    ind.act <- data.frame(active, check.names = F)
    headings = NULL                      # As default, no headings are defined
    }
  
  ############################################################################
  # If active is a list and input data is an identicator.matrix, headings are created and active used as it is
  ############################################################################
  
  if (data.type == "list.indicators") {
    headings <- rep(names(active), sapply(active, ncol))
    names(active) <- NULL
    ind.act <- do.call("cbind", active)
  }
  
  ############################################################################
  # If active is a list but input data is not an indicator.matrix, headings are created and active is binarized
  ############################################################################
  
  if (data.type == "list.data.frame") {
    headings  <- rep(names(active), sapply(active, length))
    names(active) <- NULL
    active    <- do.call("cbind", active)
    active    <- data.frame(lapply(active, factor), check.names = F)
    nl        <- sapply(active, nlevels)
    headings  <- rep(headings, nl)
    a.r       <- nrow(active)
    ind.act   <- indicator(active)
  }
  
  #########################################
  # Creating lists of variable names...
  #########################################
  
  varlist      <- unique(gsub(": .*", "", colnames(ind.act)))             # Unique varnames
  varlist.long <- gsub(": .*", "", colnames(ind.act))                     # Vector of varnames matching the list of modalities
  Q            <- median(rowSums(ind.act))                                # Number of Questions [we use the median in order to allow for questions that do not sum to one]
  
  passive.set <- grepl(paste(passive, collapse = "|"), colnames(ind.act)) # Defining the set of passive modalities, default is no passive
  set <- 1:ncol(ind.act)                                                  # set is all
  active.set <- set[!passive.set]                                         # active.set is active modalities
  
  ind.reduced <- ind.act[ ,active.set]                                    # Reducing the indicator matrix to the active set of modalities
  varlist.long.red <- gsub(": .*", "", colnames(ind.reduced))             # The reduced vector of var names, matching the reduced modality name vector
  
  tmpx <- vector()
  
  #for each variable, 
  for (i in 1:length(unique(varlist))) {
    t1 <- ind.act[,varlist.long == unique(varlist)[i]]
    t2 <- ind.reduced[,varlist.long.red == unique(varlist)[i]]
    if (is.null(dim(t1)[2])) {
      tmpx[i] <- max(ind.act[,varlist.long == unique(varlist)[i]] - ind.reduced[,varlist.long.red == unique(varlist)[i]])  
    }else{
      tmpx[i] <- max(rowSums(ind.act[,varlist.long == unique(varlist)[i]]) - rowSums(ind.reduced[,varlist.long.red == unique(varlist)[i]]))
    }
  }
  Qm <- Q - sum(tmpx)                                                     # Calculating the number of questions with passive modalities
  
  
  ###############################################
  # Handling supplementary variables
  ###############################################
  
  # If sup is empty no supplementary data is added
  if (identical(sup, NULL) == TRUE) {
    sup <- matrix(0, nrow = nrow(ind.act), ncol = 2)
    sup[, 1:2] <- cbind(rep(0, nrow(ind.act)), rep(0, nrow(ind.act)))
    colnames(sup) <- c("No supplementary points defined 1", 
                       "No supplementary points defined 2")
    ind.sup <- sup
  }
  
  # If sup is not empty a dataframe of supplementary data is created
  if ((nrow(sup) == 0) == FALSE) {
    if (all(sapply(sup, is.numeric))) {          # if indidactor sup is used as is
      ind.sup <- sup
    }else{                                       # else an indicator is created
      ind.sup <- indicator(sup)  
    } }
  
  
  
  
  
  ################################################################
  #      The actual analysis, a CA of the indicator matrix       #
  ################################################################
  
  result <- subset.ca.indicator(ind.act, ind.sup, active.set, passive.set, Q = Q, Qm = Qm, Moschidis = Moschidis, detailed.results = detailed.results)
  
  result$variable.all <- varlist.long
  
  # Creating 'numeric' identifier if no identifier is given (Default)
  if (identical(identifier, NULL) == TRUE) {
    identifier <- 1:nrow(ind.act)
  }
  
  result$names.mod.all            <- colnames(ind.act)
  result$names.mod                <- colnames(ind.act)[active.set]
  result$names.ind                <- as.character(identifier)
  result$names.sup                <- colnames(ind.sup)
  result$names.passive            <- colnames(ind.act)[passive.set]
  result$headings.all             <- headings
  result$headings                 <- headings[active.set]
  result$indicator.matrix.passive <- ind.act[, passive.set]
  result$indicator.matrix.active  <- ind.act[, active.set]
  result$indicator.matrix.all     <- ind.act
  
  
  #######################################################
  # Creating modality overview table to output
  #######################################################
  
  x                               <- colnames(ind.act[, active.set])
  varlist                         <- gsub(": .*", "", x)
  x.all                           <- colnames(ind.act)
  varlist.all                     <- gsub(": .*", "", x.all)
  mm                              <- as.matrix(cbind(varlist, 1:length(varlist)))
  mmx                             <- as.matrix(cbind(varlist.all, 1:length(varlist.all)))
  md                              <- matrix(, nrow = length(unique(unlist(varlist))), ncol = 4)
  rownames(md)                    <- unique(unlist(varlist))
  colnames(md)                    <- c("Total nb. modalities", "Nb. active modalities", "Start", "End")
  md                              <- as.data.frame(md)
  for (i in 1:length(unique(unlist(varlist)))) {
    mr              <- as.numeric(mm[, 2][mm[, 1] == unique(unlist(varlist))[i]])
    mx              <- as.numeric(mmx[, 2][mmx[, 1] == unique(unlist(varlist.all))[i]])
    md[i, 1]        <- length(mx)
    md[i, 2]        <- length(mr)
    md[i, 3]        <- min(mr)
    md[i, 4]        <- max(mr)
  }
  md[, 1]                         <- as.numeric(md[, 1])
  md[, 2]                         <- as.numeric(md[, 2])
  result$modal                    <- md
  
  ###########################################################################
  # Calculating 'The Rosenlund treshold' for each modality 
  ###########################################################################

  result$Rosenlund.tresh          <- rep(1/result$Q/result$modal$`Nb. active modalities`, times = result$modal$`Nb. active modalities`)
  result$Rosenlund.tresh.all      <- rep(1/result$Q/result$modal$`Total nb. modalities`, times = result$modal$`Total nb. modalities`)
  
  #############################################
  # The variable vectors
  #############################################
  
  variable <- vector()
  for (i in 1:nrow(md)) {
    variable <- c(variable, rep(rownames(md)[i], md[i, "Nb. active modalities"]))
  }
  result$variable   <- variable
  
  if (identical(sup, NULL) == FALSE) {
    varnames <- colnames(sup)
    ml <- vector()
    for (i in 1:ncol(sup)) {
      ml <- c(ml, rep(varnames[i], nlevels(sup[[i]])))
    }
    result$variable.sup <- ml
  }
  
  result$subset.var <- Qm
  
  
  Nmodal                 <- result$n.mod
  Nsup                   <- sum(result$freq.sup != 0)
  Nid                    <- result$n.ind
  
  
  #######################################################
  # Mass matrix
  #######################################################
  tmp                    <- data.frame(result$variable.all, result$mass.mod.all)
  mass.all               <- matrix( , nrow = length(unique(varlist)), ncol = 4)
  for (i in 1:length(unique(varlist))) {
    mass.all[i,4] <- as.numeric(sum(tmp[which(result$variable.all == unique(result$variable.all)[i]),2]))
  }
  
  tmp2                   <- data.frame(result$variable, result$mass.mod)
  mass.act <- vector()
  for (i in 1:length(unique(varlist))) {
    mass.act[i] <- as.numeric(sum(tmp2[which(result$variable == unique(result$variable)[i]),2]))
  }
  mass.all[,1] <- mass.act
  mass.all[,2] <- mass.all[,4] - mass.all[,1]
  mass.all[,3] <- mass.all[,2] / mass.all[,4] * 100
  rownames(mass.all) <- unique(result$variable.all)
  
  colnames(mass.all) <- c("Active mass", "Passive mass", "% of mass passive" ,"Total mass")  
  result$mass        <- round(mass.all,10)
  
  tmp           <- data.frame(result$variable, rowSums(result$ctr.mod.raw))
  Share.of.var  <- vector()
  for (i in 1:length(unique(varlist))) {
    Share.of.var[i]                 <- sum(tmp[which(result$variable == unique(result$variable)[i]),2]) / (result$total.inertia.raw)
  }
  names(Share.of.var) <- unique(result$variable)
  
  Share.of.var           <- data.frame("Active modalities" = result$modal[, "Nb. active modalities"], "Share of variance" = round(Share.of.var,3), check.names = F)
  rownames(Share.of.var) <- rownames(result$modal)
  result$Share.of.var    <- Share.of.var
  
  median.standard <- function(result) {
    coord.ind <- result$coord.ind
    coord.median <- apply(coord.ind, 2, median)
    dim.ind <- seq(ncol(coord.ind))[coord.median > 0]
    result <- invert(result, dim.ind)
    return(result)
  }
  
  result <- median.standard(result)
  
  # Cleaning
  
  if(identical(detailed.results, FALSE)){
  result$svd.u                  <- NULL
  result$cor.mod.all.raw        <- NULL
  result$ctr.mod.all.raw        <- NULL
  #result$ctr.mod.raw            <- NULL
  result$eigen.raw              <- NULL
  result$indicator.matrix.all   <- NULL
  result$indicator.matrix.trans <- NULL
  }
  
  class(result) <- "soc.mca"
  return(result)
}


# ' Correspondence analysis on a indicator matrix
# ' 
# ' This function is part of the soc.mca function but allows for manipulation of the indicator matrix before analysis.
# ' Most users will not need this function.
# ' 
# ' @param ind.act   An indicator matrix of all the active modalities (including those that are to be set as passive)
# ' @param ind.sup   An indicator matrix of the supplementary modalities
# ' @param subset    A vector containing column indices of passive modalities
# ' @param Q       The number of variables
# ' @param Qm      The number of variables without passive modalities
# ' #@export
# ' @return a list of various results. See \link{soc.mca} documentation

subset.ca.indicator <- function(ind.act, ind.sup, active.set, passive.set, Q, Qm, Moschidis, detailed.results = FALSE){
  
  
  Z.act     <- ind.act
  Z.sup     <- ind.sup
  colZ      <- colSums(Z.act)
  
  # If moschidis is TRUE, the method from Moschidis, Odysseas E.
  
  if (identical(Moschidis, TRUE)){
    Y <- vector()
    cnZ.act <- colnames(Z.act)
    varlist <- gsub(pattern = ": .*", "" , cnZ.act) 
    
    for (i in seq(Q)) {
      x <- rep(table(varlist)[i], table(varlist)[i])
      Y <- c(Y, x)
    }
    Z.act <- Z.act%*%diag(1/(Y-1))
    colnames(Z.act) <- cnZ.act
  }
  
  I         <- dim(Z.act)[1]  # Number of individuals
  J         <- dim(Z.act)[2]  # Number of modalities >> Subset
  Q         <- Q              # Number of variables
  
  # Inertias
  P            <- Z.act / sum(Z.act)      # The full correspondence matrix 
  cm.all       <- colSums(P)              # Column (modality) mass
  mass.mod.all <- cm.all                  # keeping the full column mass vector for later use
  rm           <- rowSums(P)              # Row (individual) mass
  
  diag.cm.all   <- diag(1/ sqrt(cm.all))       # A J x J diagonal matrix of 1/sqrt(column masses)
  
  eP        <- rm %*% t(cm.all)            # Expected distances
  S         <- (P - eP) / sqrt(eP)         # Euclidian distances, a matrix of standardized residuals
  
  ## Subsetting 
  K         <- length(active.set)
  S         <- S[, active.set]
  cm        <- cm.all[active.set]
  cm.all[passive.set]                     <- 0
  diag.cm   <- diag.cm.all[active.set, active.set]
  diag.cm.all[passive.set, passive.set]   <- 0
  
  ##################################
  ## Singular value decomposition ##
  ##################################
  
  # Decomposition and eigenvectors
  dec       <- svd(S)                                             # Singular decomposition
  eigen     <- dec$d^2                                            # Eigenvalues from singularvalues
  
  # Principal coordinates
  pc.mod    <- diag.cm %*% dec$v %*% diag(dec$d)   # Principal coordinates for modalities
  
  # Principal coordinates for individuals
  #diag.rm   <- diag(1/ sqrt(rm))  
  diag.rm   <- Matrix::Diagonal(x = 1/ sqrt(rm))
  pc.ind    <- diag.rm %*% dec$u %*% diag(dec$d)   # Principal coordinates for individuals # This is a slow process, but it scales ok # Anders Holm adjustment  
  
  # Inertias for rows and column profiles
  inr.ind   <- Matrix::Diagonal(x =rm) %*% pc.ind^2     # Inertia for row (Individuals) (mass x principal coordinates) # This is a slow process and it scales badly - diag(rm) is a individual X individual matrix. It is also sparse - so it might be possible to do it quicker.
  inr.mod   <- diag(cm) %*% pc.mod^2     # Inertia for columns (Modalities)
  
  # Relative contributions, 'point inertia on axis' / total inertia of axis
  ctr.ind   <- Matrix::t(Matrix::t(inr.ind) / dec$d^2)   # Contribution for the individuals (inertia / eigenvalue)
  ctr.mod   <- t(t(inr.mod) / dec$d^2)   # Contribution for the modalities
  ctr.mod.raw   <- inr.mod               # Contribution for the modalities
  
  # Squared cosines or correlations
  cor.ind   <- inr.ind/Matrix::rowSums(inr.ind)  # Squared cosines for individuals
  cor.mod   <- inr.mod/rowSums(inr.mod)  # Squared cosines for modalities
  cor.mod.raw   <- inr.mod               # Squared cosines for modalities
  
  
  ############################################
  #         Supplementary points             #
  ############################################
  
  # Supplementary principal coordinates
  if (sum(ind.sup) >0 ) {
    Z.sup     <- Z.sup[,which(colSums(Z.sup) > 0)]
    Z.star    <- Z.sup
    I.star    <- dim(Z.sup)[1]
    cs.star   <- apply(Z.sup, 2, sum)
    
    base      <- Z.star / matrix(rep(cs.star, I.star), nrow = I.star, byrow = TRUE)
    f.s1      <- dec$u * sqrt(eigen) / sqrt(rm)   
    a.s1      <- f.s1 / sqrt(eigen)               
    pc.sup    <- t(base) %*% a.s1
    
    # t.test for supplementary points
    
    t       <- matrix(NA, nrow = nrow(pc.sup), ncol = ncol(pc.sup))
    
    
    for (j in 1:ncol(pc.sup)) {
      for (i in 1:nrow(pc.sup)) {
        #t[i,j] <- round(pnorm(-abs(sqrt(cs.star[i]*((I-1)/(I-cs.star[i])))*pc.sup[i,j])),5) # produces warning for freq.sup = 0
        t[i,j] <- round(sqrt(cs.star[i]*((I-1)/(I-cs.star[i])))*pc.sup[i,j],5) # produces warning for freq.sup = 0
      }
    }
    
  }else{
    pc.sup  <- NULL
    t.plane <- NULL
    t       <- NULL
  }
  
  # principle coordinates for active and passive modalities, in case we want to plot the passive modalities
  pc.all            <- pc.mod
  ctr.mod.all       <- ctr.mod
  cor.mod.all       <- cor.mod
  ctr.mod.all.raw   <- ctr.mod.raw
  cor.mod.all.raw   <- cor.mod.raw
  
  # Calculating midpoint coordinates of passive modalities
  if (length(active.set) < ncol(Z.act)) {
    cs.star.all       <- colZ
    tmp.ctr <- ctr.mod
    tmp.cor <- cor.mod
    tmp.ctr.raw <- ctr.mod.raw
    tmp.cor.raw <- cor.mod.raw
    rownames(tmp.ctr) <- colnames(Z.act)[active.set]
    rownames(tmp.cor) <- colnames(Z.act)[active.set]
    rownames(tmp.ctr.raw) <- colnames(Z.act)[active.set]
    rownames(tmp.cor.raw) <- colnames(Z.act)[active.set]
    passive.mod <- setdiff(names(cs.star.all), rownames(tmp.ctr))
    passive.modalities <- matrix(0, nrow = length(passive.mod), ncol = length(eigen))
    rownames(passive.modalities) <- passive.mod
    tmp.ctr <- rbind(tmp.ctr, passive.modalities)
    tmp.cor <- rbind(tmp.cor, passive.modalities)
    tmp.ctr.raw <- rbind(tmp.ctr.raw, passive.modalities)
    tmp.cor.raw <- rbind(tmp.cor.raw, passive.modalities)
    
    pc.all            <- matrix(0, nrow = dim(Z.act)[2], ncol = length(eigen))
    ctr.mod.all       <- matrix(0, nrow = dim(Z.act)[2], ncol = length(eigen))
    cor.mod.all       <- matrix(0, nrow = dim(Z.act)[2], ncol = length(eigen))
    ctr.mod.all.raw   <- matrix(0, nrow = dim(Z.act)[2], ncol = length(eigen))
    cor.mod.all.raw   <- matrix(0, nrow = dim(Z.act)[2], ncol = length(eigen))
    
    # scaling midpoint coordinates along axes
    for (j in 1:length(eigen)) {
      for (i in 1:length(colnames(Z.act))){
        pc.all[i,j]            <- sum(pc.ind[Z.act[,i] > 0,j]) / cs.star.all[i] / sqrt(eigen[j])
        ctr.mod.all[i,j]       <- tmp.ctr[rownames(tmp.ctr) == colnames(Z.act)[i], j]
        cor.mod.all[i,j]       <- tmp.cor[rownames(tmp.cor) == colnames(Z.act)[i], j]
        ctr.mod.all.raw[i,j]   <- tmp.ctr.raw[rownames(tmp.ctr.raw) == colnames(Z.act)[i], j]
        cor.mod.all.raw[i,j]   <- tmp.cor.raw[rownames(tmp.cor.raw) == colnames(Z.act)[i], j]
      }}
    
  }
  
  #################################
  #     Preparing output          #
  #################################
  
  # First reduction of dimensionality - 'La dimension du suppport': 
  R1.dim        <- K-Qm
  R1.eigen.val  <- eigen[1:R1.dim]
  
  # Second reduction of dimensionality - dimensions above average
  Mean.r.eigen  <- ((K/Q) - sum(cm)) / R1.dim
  R2.eigen.val  <- eigen[eigen >= Mean.r.eigen]
  R2.dim        <- length(R2.eigen.val)
  
  # Explained variance
  unadj.var       <- 100*(eigen[1:R1.dim])/sum(eigen[1:R1.dim]) # Unadjusted rates of variance
  adj.var.mod     <-(Q/(Q-1))^2 * (eigen[1:R2.dim]-Mean.r.eigen)^2
  sum.adj.var     <- sum(adj.var.mod)
  adj.var         <- round(((adj.var.mod/sum.adj.var)*100), digits = 1) # The adjusted rates of variance
  cumpercent      <- cumsum(adj.var)
  adj.inertia     <- cbind(1:R2.dim, round(eigen[1:R2.dim], 3), round(unadj.var[1:R2.dim], 1), adj.var ,cumpercent)
  colnames(adj.inertia) <- c("Dim", "Eigen", "Var" ,"Adj.Var", "Cum%")
  
  
  ### Inerti, total and expressed in rates of 1) Sum of adj.var greater than avr eigenval (Benzecri) and of 2) acerage off-diagonal inertia of the Burt-table (Greenacre)
  # unadj.var     <- 100*(eigen[1:R1.dim])/sum(eigen[1:R1.dim]) # Unadjusted rates of variance
  
  # nz <- nrow(ind.act[,subset])/sum(ind.act[,subset])
  # adj.var.Zarrag <- (sum(ind.act[,subset])/(nrow(ind.act[,subset])*(Q-1)))^2 * (eigen[eigen > nz]- nz)^2
  # sum.adj.var.Zarrag <- sum(adj.var.Zarrag[adj.var.Zarrag>0])
  # adj.var.rate.Zarrag <- round(((adj.var.Zarrag/sum.adj.var.Zarrag)*100), digits=1)
  # cumpercent.Zarrag <- cumsum(adj.var.rate.Zarrag)
  # stop <- length(adj.var.rate.Zarrag)
  # adj.inertia2   <- cbind(1:stop, round(eigen[1:stop], 3), round(unadj.var[1:stop], 1), adj.var.rate.Zarrag, cumpercent.Zarrag) # De Rouanet adjustede inertier - skal nok rykkes ned.
  adj.inertia2   <- cbind(1:R1.dim, round(eigen[1:R1.dim], 3), round(unadj.var, 1),  round(cumsum(unadj.var),1)) # De Rouanet adjustede inertier - skal nok rykkes ned.
  colnames(adj.inertia2) <- c("Dim", "Eigen", "Unadj.Var%", "Cum%")
  
  freq.mod.all  <- colZ
  freq.mod      <- colZ[active.set]
  if (sum(ind.sup) >0 ) {
    freq.sup      <- colSums(Z.sup)
  }else{
    freq.sup      <- NULL
  }
  
  
  # Output
  ca.output <- list(nd        = R2.dim,
                    n.ind     = nrow(Z.act),
                    n.mod     = length(active.set),
                    eigen     = eigen[1:R2.dim],
                    eigen.raw = eigen,
                    Qm        = Qm,
                    Q         = Q,
                    total.inertia = sum(eigen[1:R2.dim]),
                    total.inertia.raw = sum(eigen),
                    adj.inertia = adj.inertia,
                    #   adj.inertia.alt.green = adj.inertia1,
                    inertia_full = adj.inertia2,
                    freq.mod  = freq.mod,
                    freq.mod.all = freq.mod.all,
                    freq.sup  = freq.sup,
                    ctr.mod.raw   = ctr.mod.raw,
                    ctr.mod.all = ctr.mod.all,
                    ctr.mod.all.raw = ctr.mod.all.raw,
                    ctr.ind   = ctr.ind,
                    cor.mod   = cor.mod,
                    cor.mod.raw   = cor.mod.raw,
                    cor.mod.all = cor.mod.all,
                    cor.mod.all.raw = cor.mod.all.raw,
                    cor.ind   = cor.ind,
                    mass.mod  = cm,
                    mass.mod.all = mass.mod.all,
                    coord.mod = pc.mod,
                    coord.ind = pc.ind,
                    coord.all = pc.all, 
                    coord.sup = pc.sup,
                    t.test.sup = t,
                    ctr.mod   = ctr.mod,
                    indicator.matrix.trans   = Z.act
  )
  
  if(identical(detailed.results, TRUE)){
    dec.full  <- svd(S, nu = nrow(S), nv = ncol(S))   # Singular decomposition keeping all vectors
    full      <- data.frame(svd.d     = dec.full$d,
                            svd.u     = dec.full$u,
                            svd.v     = dec.full$v)
    ca.output <- cbind(ca.output, full)
  }
  
  
  # Cleanup
  names(ca.output$mass.mod)         <- NULL
  dimnames(ca.output$coord.sup)     <- NULL
  names(ca.output$freq.mod)         <- NULL
  names(ca.output$freq.sup)         <- NULL
  
  return(ca.output)
}


#' Indicator matrix
#' 
#' Creates an indicator matrix from a data.frame with the categories of the questions as columns and individuals as rows.
#' 
#' @param x   a data.frame of factors
#' @param id  a vector defining the labels for the individuals. If id = NULL row number is used.
#' @param ps  the seperator used in the creation of the names of the columns.
#'
#' @return Returns a indicator matrix
#' @seealso \link{soc.mca}
#' @examples 
#' a  <- rep(c("A","B"), 5)
#' b  <- rep(c("C", "D"), 5)
#' indicator(data.frame(a,b))
#' @export indicator

indicator  <- function(x, id = NULL, ps = ": "){

obj         <- x
I           <- nrow(obj)                                      # Number of individuals
levels.n    <- unlist(lapply(obj, nlevels))
n           <- cumsum(levels.n)                               # Number of modalities for each question
m           <- max(n)                                         # Total number of modalities
Q           <- ncol(obj)                                      # Number of questions
Z           <- matrix(0, nrow = I, ncol = m)                  # Predefinition of the indicatormatrix
newdat      <- lapply(obj, as.numeric)
offset      <- (c(0, n[-length(n)]))
for (i in seq(Q)) Z[seq(I) + (I * (offset[i] + newdat[[i]] - 1))] <- 1 # Indicator matrix
fn          <- rep(names(obj), unlist(lapply(obj, nlevels)))  
ln          <- unlist(lapply(obj, levels))
col.names   <- paste(fn, ln, sep = ps)
colnames(Z) <- col.names

if (identical(id, NULL) == TRUE){
rownames(Z) <- as.character(seq(I))
}else{
rownames(Z) <- id
}    
return(Z)

}

#' Class Specific Multiple Correspondence Analysis
#'
#' \code{soc.csa} performs a class specific multiple correspondence analysis on a data.frame of factors, where cases are rows and columns are variables. Most descriptive and analytical functions that work for \link{soc.mca}, also work for \code{soc.csa}
#' @param object  is a soc.ca class object created with \link{soc.mca}
#' @param class.indicator the row indices of the class specific individuals
#' @param sup          Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
#' @return \item{nd}{Number of active dimensions}
#'  \item{n.ind}{The number of active individuals}
#'  \item{n.mod}{The number of active modalities}
#'  \item{eigen}{Eigenvectors}
#'  \item{total.inertia}{The sum of inertia}
#'  \item{adj.inertia}{A matrix with all active dimensions, adjusted and unadjusted inertias. See \link{variance}}
#'  \item{freq.mod}{Frequencies for the active modalities. See \link{add.to.label}}
#'  \item{freq.sup}{Frequencies for the supplementary modalities. See \link{add.to.label}}
#'  \item{ctr.mod}{A matrix with the contribution values of the active modalities per dimension. See \link{contribution}}
#'  \item{ctr.ind}{A matrix with the contribution values of the individuals per dimension.}
#'  \item{cor.mod}{The correlation or quality of each modality per dimension.}
#'  \item{cor.ind}{The correlation or quality of each individual per dimension.}
#'  \item{mass.mod}{The mass of each modality}
#'  \item{coord.mod}{A matrix with the principal coordinates of each active modality per dimension.}
#'  \item{coord.ind}{A matrix with the principal coordinates of each individual per dimension.}
#'  \item{coord.sup}{A matrix with the principal coordinates of each supplementary modality per dimension. Notice that the position of the supplementary modalities in class specific analysis is the mean point of the individuals, which is not directly comparable with the cloud of the active modalities.}
#'  \item{indicator.matrix}{A indicator matrix. See \link{indicator}}
#'  \item{names.mod}{The names of the active modalities}
#'  \item{names.ind}{The names of the individuals}
#'  \item{names.sup}{The names of the supplementary modalities}
#'  \item{names.passive}{The names of the passive modalities}
#'  \item{modal}{A matrix with the number of modalities per variable and their location}
#'  \item{variable}{A vector with the name of the variable for each of the active modalities}
#'  \item{variable.sup}{A vector with the name of the variable for each of the supplementary modalities}
#'  \item{original.class.indicator}{The class indicator}
#'  \item{original.result}{The original soc.ca object used for the CSA}
#' @name soc.csa
#' @export soc.csa
#' @author Anton Grau Larsen, University of Copenhagen
#' @author Stefan Bastholm Andrade, University of Copenhagen
#' @author Christoph Ellersgaard, University of Copenhagen
#' @seealso \link{add.to.label}, \link{contribution}
#' @references Le Roux, B., og H. Rouanet. 2010. Multiple correspondence analysis. Thousand Oaks: Sage.
#' @examples 
#' example(soc.ca)
#' class.age    <- which(taste$Age == '55-64')
#' res.csa      <- soc.csa(result, class.age)
#' res.csa

soc.csa   <- function(object, class.indicator, sup = NULL){
  
  
  Z.act   <- object$indicator.matrix.active         # Original indicator matrix
  Q       <- nlevels(as.factor(object$variable))    # Number of questions
  I       <- nrow(Z.act)                            # Original number of individuals
  
  Z.hat   <- Z.act[class.indicator, ]               # Indicator matrix for the CSA
  i       <- length(class.indicator)                # Number of individuals in CSA
  
  cm      <- apply(Z.hat, 2, sum)
  CM      <- apply(Z.act, 2, sum)
  P       <- Z.hat / sum(Z.hat)      
  cmpc    <- colSums(P)              # Column (modality) mass
  rmpc    <- rep(1/i,i) 
  
  
  H.hat   <- matrix(, nrow = i, ncol = length(cm))
  for (k in seq(cm)){
    H.hat[,k] <- (1/sqrt(Q)) * (sqrt(I/i)) * (Z.hat[, k]-(cm[k]/i)) * (1/sqrt(CM[k]))
  }
  
  colnames(H.hat)   <- colnames(Z.hat)
  modal.names       <- colnames(Z.hat)
  
  H.svd             <- svd(H.hat)
  dec               <- H.svd
  
  ### Modalitetskoordinater
  csa.m.coord            <- matrix(nrow = nrow(H.svd$v), ncol = ncol(H.svd$v))
  for (ff in 1:length(H.svd$d)){
    csa.m.coord[, ff]      <- (sqrt(Q*I)) * (1/sqrt(CM)) * H.svd$v[, ff] * H.svd$d[ff]
  } 
  rownames(csa.m.coord)  <- modal.names
  
  ### Modalitetsbidrag
  csa.m.ctr              <- matrix(nrow = nrow(H.svd$v), ncol = ncol(H.svd$v))
  for (ff in 1:length(H.svd$d)){
    csa.m.ctr[, ff]        <- (((CM/I)/Q) * (csa.m.coord[, ff])^2)/(H.svd$d[ff]^2)
  }
  
  csa.m.ctr.raw            <- ((CM/I)/Q) * (csa.m.coord^2) 
  csa.m.cor                <- csa.m.ctr.raw/rowSums(csa.m.ctr.raw)
  
  
  ### Individkoordinater
  csa.i.coord            <- matrix(nrow = nrow(H.svd$u), ncol = ncol(H.svd$u))
  for (ff in 1:length(H.svd$d)){
    csa.i.coord[, ff]      <- sqrt(i) * H.svd$u[, ff] * H.svd$d[ff]
  }
  
  ### Individbidrag
  
  csa.i.ctr              <- matrix(nrow = nrow(H.svd$u), ncol = ncol(H.svd$u))
  for (ff in 1:length(H.svd$d)){
    csa.i.ctr[, ff]        <- ((1/i) * (csa.i.coord[, ff])^2)/(H.svd$d[ff]^2)
  }
  
  csa.i.ctr.raw           <- (1/i) * (csa.i.coord^2)
  csa.i.cor               <- csa.i.ctr.raw/rowSums(csa.i.ctr.raw)
  
  
  # Eigenvectors
  eigen            <- H.svd$d^2 
  
  ###############################################
  #### Dimension reduction and explained variance
  K                <- ncol(Z.act)
  Qm               <- object$subset.var
  
  # First reduction of dimensionality
  R1.dim           <- K-Qm
  R1.eigen.val     <- eigen[1:R1.dim]
  
  # Second reduction of dimensionality
  Mean.r.eigen     <- mean(R1.eigen.val, na.rm = TRUE)
  R2.eigen.val     <- eigen[eigen >= Mean.r.eigen]
  R2.dim           <- which(R2.eigen.val >= Mean.r.eigen)
  
  # Explained variance
  unadj.var        <- 100*(eigen[1:R1.dim])/sum(eigen[1:R1.dim])               # Unadjusted rates of variance
  sum.adj.var.mod  <- (eigen[R2.dim]-Mean.r.eigen)^2
  sum.adj.var      <- sum(sum.adj.var.mod)
  adj.var          <- round(((sum.adj.var.mod/sum.adj.var)*100), digits = 1)   # The adjusted rates of variance
  cumpercent       <- cumsum(adj.var)
  adj.inertia      <- cbind(R2.dim, round(eigen[R2.dim], 3), round(unadj.var[R2.dim], 1), adj.var ,cumpercent)
  colnames(adj.inertia) <- c("Dim", "Eigen", "Var" ,"Adj.Var", "Cum%")
  
  ###################
  
  freq.mod         <- cm
  
  ###############
  
  ############
  # Preparing for object object
  names.mod      <- modal.names
  names.ind      <- object$names.ind[class.indicator]
  names.passive  <- object$names.passive
  
  
  ############
  # Supplementary variables
  # They are projected into the cloud of individuals - NOT the cloud of modalities
  
  sup.coord                 <- NULL
  freq.sup                  <- 0
  names.sup                 <- NULL
  if(identical(sup,NULL) == FALSE){
    sup.coord               <- matrix(NA, nrow= ncol(sup), ncol = ncol(csa.i.coord))
    rownames(sup.coord)     <- colnames(sup)
    freq.sup                <- colSums(sup)
    names.sup                 <- colnames(sup)
    
    for (j in 1:ncol(csa.i.coord)){
      d1                <- csa.i.coord[,j] #/ H.svd$d[j]
      sup.c             <- sup
      sup.c[]           <- 1
      sup.c[sup == 0]   <- 0
      sup.c             <- sup.c * d1
      sup.coord[, j]    <- (1 / freq.sup) * apply(sup.c, 2, sum, na.rm = TRUE) 
    }
    sup.coord[which(freq.sup == 0),] <- 0
    
  }
  
  
  
  ############# 
  # Result object
  
  csca.result <- list(
    nd          = object$nd,
    n.ind       = i, 
    n.ind.org   = I,
    n.mod       = ncol(Z.hat),
    eigen       = eigen,
    total.inertia = sum(eigen[R2.dim]),
    adj.inertia = adj.inertia,
    freq.mod    = freq.mod,
    freq.mod.org = object$freq.mod,
    freq.sup    = freq.sup,
    ctr.mod     = csa.m.ctr,
    ctr.ind     = csa.i.ctr,
    cor.mod     = csa.m.cor, #cor.mod[, R2.dim], cor.ind = cor.ind[, R2.dim], # Not implemented
    cor.ind     = csa.i.cor,
    mass.mod    = cmpc, # Massen er etchy - se nedenfor
    mass.ind    = rmpc, # Massen er etchy - se nedenfor
    coord.mod   = csa.m.coord,
    #coord.mod  = pc.mod,
    coord.ind   = csa.i.coord,
    coord.sup   = sup.coord,
    names.mod   = names.mod,
    names.ind   = names.ind,
    names.sup   = names.sup,
    names.passive = names.passive,
    indicator.matrix = Z.hat,
    modal       = object$modal,
    variable    = object$variable,
    variable.sup = "Not Implemented",
    variable.all = object$variable.all,
    headings.all = object$headings.all,
    headings    = object$headings,
    subset.var  = object$subset.var,
    original.result = object,
    original.class.indicator = class.indicator,
    svd.d       = H.svd$d,
    svd.u       = H.svd$u,
    svd.v       = H.svd$v
  )
  
  median.standard <- function(object){
    
    coord.ind     <- object$coord.ind
    coord.median  <- apply(coord.ind, 2, median)  
    dim.ind       <- seq(ncol(coord.ind))[coord.median > 0]
    object        <- invert(object, dim.ind)
    return(object)
  }
  
  csca.result     <- median.standard(csca.result)
  
  #####################################
  # Class and return
  
  class(csca.result)    <- c("soc.mca", "soc.csa")
  return(csca.result)
  
} 

#' Create categories according to the quadrant position of each individual
#' 
#' Creates a vector from two dimensions from a soc.ca object. Labels are the 
#' cardinal directions with the first designated dimension running East - West.
#' The center category is a circle defined by \code{cut.radius}.
#'
#' @param object a soc.ca class object
#' @param dim the dimensions
#' @param cut.min Minimum cut value
#' @param cut.max Maximum cut value
#' @param cut.radius Radius of the center category
#'
#' @return Returns a character vector with category memberships
#' @seealso \link{soc.mca}
#' @export create.quadrant
#' 
#' @examples 
#' example(soc.ca)
#' create.quadrant(result, dim = c(2, 1))
#' table(create.quadrant(result, dim = c(1, 3), cut.radius = 0.5))
#' 
create.quadrant <- function(object, dim = c(1,2), cut.min = -0.125, cut.max = 0.125, cut.radius = 0.25){
  
  coord                      <- object$coord.ind
  coord.cut                  <- coord
  coord.cut[coord < cut.min] <- "Min"
  coord.cut[coord > cut.max] <- "Max"
  coord.cut[coord <= cut.max & coord >=cut.min]  <- "Medium"
  
  dim1     <- coord.cut[,dim[1]]
  dim2     <- coord.cut[,dim[2]]
  distance <- sqrt(((coord[, dim[1]]^2) + (coord[, dim[2]]^2)))
  position <- dim1
  
  position[dim1 == "Max" & dim2 == "Max"]        <- "North-East"
  position[dim1 == "Max" & dim2 == "Medium"]     <- "East"
  position[dim1 == "Max" & dim2 == "Min"]        <- "South-East"
  
  position[dim1 == "Medium" & dim2 == "Medium"]  <- "Center"
  position[dim1 == "Medium" & dim2 == "Min"]     <- "South"
  position[dim1 == "Medium" & dim2 == "Max"]     <- "North"
  
  position[dim1 == "Min" & dim2 == "Max"]        <- "North-West"
  position[dim1 == "Min" & dim2 == "Medium"]     <- "West"
  position[dim1 == "Min" & dim2 == "Min"]        <- "South-West"
  
  position[distance < cut.radius]                <- "Center"
  
  return(position)
}


#' Multiple Class Specific Correspondence Analysis on all values in a factor
#' 
#' \code{csa.all} performs a class specific correspondence analysis for each
#' level in a factor variable. Returns a list with soc.csa objects and a list of
#' measures defined by \link{csa.measures}
#' @param object  is a soc.ca class object created with \link{soc.mca}
#' @param variable a factor with the same length and order as the active
#'   variables that created the soc.ca object
#' @param dim is the dimension analyzed
#' @param ... further arguments are directed to \link{csa.measures}
#' @return \item{results}{a list of \link{soc.csa} result objects}
#' @return \item{cor}{a list of correlation matrixes}
#' @return \item{cosines}{a list of matrixes with cosine values}
#' @return \item{angles}{a list of matrixes with cosine angles between
#'   dimensions}
#' @export
#' @examples
#' example(soc.ca)
#' csa.all(result, taste$Age)
#' csa.all(result, taste$Age)$measures
#' @seealso \link{soc.csa}, \link{cor}, \link{csa.measures}

csa.all <- function(object, variable, dim = 1:5, ...){
  lev.variable <- levels(variable)
  result.list     <- list()
  for (i in 1:length(lev.variable)){
    dummy.class         <- which(variable == lev.variable[i])
    result.list[[i]]    <- soc.csa(object, class.indicator = dummy.class)
  }
  
  names(result.list) <- lev.variable
  
  measure.list <- lapply(result.list, csa.measures, format = FALSE, dim.csa = dim, ...)
  
  list(results = result.list, measures = measure.list)
}

#' Add supplementary individuals to a result object
#' 
#' @param object is a soc.ca class object created with \link{soc.mca}
#' @param sup.indicator is a indicator matrix for the supplementary individuals with the same columns as the active variables in object.
#' @param replace if TRUE the coordinates of the active individuals are discarded. If FALSE the coordinates of the supplementary and active individuals are combined. The factor \code{object$supplementary.individuals} marks the supplementary individuals.
#' @return  a soc.ca class object created with \link{soc.mca}
#' @export
#' @examples
#' example(soc.mca)
#' res.pas   <- soc.mca(active, passive = "Costume")
#' res.sup   <- supplementary.individuals(res.pas, sup.indicator = indicator(active))
#' a         <- res.sup$coord.ind[res.sup$supplementary.individuals == "Supplementary",]
#' b         <- res.pas$coord.ind
#' all.equal(as.vector(a), as.vector(b))
#' map.ind(res.sup)

supplementary.individuals <- function(object, sup.indicator, replace = FALSE){
  
  if (length(object$names.passive) > 0) sup.indicator   <- sup.indicator[, -which(colnames(sup.indicator) %in% object$names.passive)]
  
#   sup.ind.dim     <- function(object, sup.indicator, dim){
#     Q             <- length(table(object$variable))
#     yk            <- t(object$coord.mod[, dim] * t(sup.indicator))
#     #Q.ind         <- yk / rowSums(sup.indicator)
#     Q.ind         <- yk / Q
#     out           <- 1/sqrt(object$eigen[dim]) * rowSums(Q.ind)
#     out
#   }
  
  sup.ind.dim     <- function(object, sup.indicator, dim){
    Q             <- length(table(object$variable))
    yk            <- t(object$coord.mod[, dim] * t(sup.indicator))
    pk            <- (colSums(object$indicator.matrix.active)/object$n.ind) / Q
    sas           <- pk * object$coord.mod[, dim]
    Q.ind         <- yk / Q
    out           <- 1/sqrt(object$eigen[dim]) * (rowSums(Q.ind) - sum(sas))
    out
  }

  
  ndim            <- 1:ncol(object$coord.ind)
  sup.ind.coord   <- sapply(ndim, sup.ind.dim, object = object, sup.indicator = sup.indicator)
  
  if(identical(replace, FALSE)){
  object$coord.ind                 <- rbind(object$coord.ind, sup.ind.coord)
  rownames(object$coord.ind)       <- NULL
  object$supplementary.individuals <- c(rep("Active", object$n.ind), rep("Supplementary", nrow(sup.indicator)))
  object$names.ind                 <- c(object$names.ind, rownames(sup.indicator))
  }
  
  if(identical(replace, TRUE)){
    object$coord.ind                 <- sup.ind.coord
    rownames(object$coord.ind)       <- NULL
    object$names.ind                 <- rownames(sup.indicator)
  }
 object
}
 
# # Test of supplementary individuals
# example(soc.mca)
# 
# # Ingen passive modaliteter
# res.sup   <- supplementary.individuals(result, sup.indicator = indicator(active))
# a         <- res.sup$coord.ind[res.sup$supplementary.individuals == "Supplementary",]
# b         <- result$coord.ind
# all.equal(as.vector(a), as.vector(b))
# map.ind(res.sup)
# 
# # Med passive modaliteter
# res.pas   <- soc.mca(active, passive = "Costume")
# res.sup   <- supplementary.individuals(res.pas, sup.indicator = indicator(active))
# a         <- res.sup$coord.ind[res.sup$supplementary.individuals == "Supplementary",]
# b         <- res.pas$coord.ind
# all.equal(as.vector(a), as.vector(b))
# map.ind(res.sup)
# 
# # Med passive modaliteter 
# res.pas   <- soc.mca(active, passive = "Costume")
# res.sup   <- supplementary.individuals(res.pas, sup.indicator = indicator(active)[1:10,])
# a         <- res.sup$coord.ind[res.sup$supplementary.individuals == "Supplementary",]
# b         <- res.pas$coord.ind[1:10,]
# all.equal(as.vector(a), as.vector(b))
# map.ind(res.sup, point.fill = res.sup$supplementary.individuals, label = T)
# 
# # Med passive modaliteter
# sup.indicator <- indicator(active)[1:10,]
# rownames(sup.indicator) <- paste("Sup", 1:10)
# res.pas   <- soc.mca(active, passive = "Costume")
# res.sup   <- supplementary.individuals(res.pas, sup.indicator = sup.indicator)
# a         <- res.sup$coord.ind[res.sup$supplementary.individuals == "Supplementary",]
# b         <- res.pas$coord.ind[1:10,]
# all.equal(as.vector(a), as.vector(b))
# map.ind(res.sup, point.fill = res.sup$supplementary.individuals, label = T)

#' Check if data is valid for soc.mca
#'
#' @param x 
#'
#' @return
#'
#' @examples
#' \dontrun{
#'   # Valid scenarios ----
#' 
#' # X is a valid data.frame
#' x <- taste[, 2:7]
#' what.is.x(x)  
#' 
#' # X is a valid indicator
#' x <- indicator(taste[, 2:7])
#' what.is.x(x)
#' 
#' # X is a valid list of data.frames with names
#' x <- list(nif = taste[, 2:3], hurma = taste[, 4:5])
#' what.is.x(x)  
#' 
#' # X is a valid list of indicators
#' x <- list(nif = indicator(taste[, 2:3]), hurma = indicator(taste[, 4:5]))
#' what.is.x(x)
#' 
#' # Invalid scenarios ----
#' 
#' # X is a matrix - but not numeric
#' x <- as.matrix(taste[, 2:7])
#' what.is.x(x)  
#' 
#' # X is a of data.frames list but does not have names
#' x <- list(taste[, 1:3], taste[, 4:5])
#' what.is.x(x)
#' 
#' # X is a list of indicators but does not have names
#' x <- list(indicator(taste[, 2:3]), indicator(taste[, 4:5]))
#' what.is.x(x)
#' 
#' # X is a data.frame and contains NA
#' x <- taste[, 2:7]
#' x[1,1] <- NA
#' what.is.x(x)
#' 
#' # X is a list of indicators and contains NA
#' x <- list(nif = indicator(taste[, 2:3]), hurma = indicator(taste[, 4:5]))
#' x[[1]][1,1] <- NA
#' what.is.x(x)
#' 
#' # X contains elements that are neither a matrix nor a data.frame
#' x <- list(nif = 1:10, taste[, 1:3], taste[, 4:7])
#' what.is.x(x)
#' 
#' # X contains both indicators and matrixes
#' x <- list(nif = taste[, 2:3], hurma = indicator(taste[, 5:6]))
#' what.is.x(x)
#' }


what.is.x  <- function(x){
  # Is it a list of data.frames?
  is.d    <- is.data.frame(x)
  is.l    <- is.list(x)
  is.m    <- is.matrix(x)
  is.lm   <- all(unlist(lapply(x, is.matrix)))
  is.ld   <- is.d == FALSE & is.l == TRUE & is.lm == FALSE
  
  # Scenario: List
  if(is.ld | is.lm){
    not.all.data.frames <- any(!unlist(lapply(x, is.data.frame)))
    not.all.matrix      <- any(!unlist(lapply(x, is.matrix)))
    if(all(not.all.data.frames, not.all.matrix)) stop("x is a list, but not all elements are the same valid type (aka. data.frame or indicator matrix).")
    l.nam    <- names(x)
    if(unique(length(l.nam)) != length(x)) stop("x is a list, but each element (heading) does not have a unique name.")
  }
  
  # Scenario: Indicator
  if(is.m == TRUE & is.numeric(x) == FALSE) stop("Your data is a matrix, but it is not numeric.") 
  
  # Check for missing
  if(anyNA(x, recursive = TRUE)) stop("Your data includes NA. Try recoding to missing.")
  
  # Is it a list of only matrixes?
  o <- NA
  
  # It is surely a data.frame
  if(is.d)  o <- "data.frame"
  # It is surely an indicator matrix
  if(is.m)  o <- "indicator"  
  # It is surely a list of data.frames
  if(is.ld) o <- "list.data.frame"
  # It is surely a list of indicators
  if(is.lm) o <- "list.indicators"
  
  o
}

balance.headings <- function(indicator, headings, Q) {
  heads         <- unique(headings)
  n_head        <- length(heads)
  mass_by_head  <-  Q / n_head
  Q_prH         <- sapply(heads, function(x) max(rowSums(indicator[, headings == x])))
  weight        <- Q_prH / mass_by_head 
  sum(Z.act)
  for(i in 1:length(heads)) {
    indicator[ ,headings == heads[i]] <- indicator[ ,headings == heads[i]] / weight[i]
  }
}

#' Array of maps
#' 
#' This function takes a list of map objects and arranges them into an array.
#' @param x a list of objects created by one of the mapping functions in the
#'   soc.ca package or any other ggplot2 plot
#' @param ncol the number of columns the plots are arranged into
#' @param title the main title of the array
#' @param fixed.coord if TRUE the limits of all plots are set to the same as the
#'   largest plot
#' @param padding the distance between the most extreme position and the axis
#'   limit
#' @examples
#' \dontrun{
#' example(soc.ca)
#' map.array(list(map.ind(result), map.mod(result)), ncol = 2)
#' }
#' @export
map.array   <- function(x, ncol = 1, title = "", fixed.coord = TRUE, padding = 0.15){

if (identical(fixed.coord, TRUE))  x  <- fix.coords(x, padding = padding)

do.call(grid.arrange, c(x, ncol = ncol, top = title))
}


#' Ellipse array
#' 
#' Create seperate maps with ellipses for each level in a factor arranged in an
#' array.
#' @param object a soc.ca class object
#' @param variable a factor of the same length as the data.frame used to create
#'   object
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param draw.ellipses if TRUE ellipses are drawn
#' @param ncol the number of columns the plots are arranged into
#' @param titles a vector of the same length as the number of levels in
#'   variable. These are the titles given to each subplot
#' @param main.title the main title for all the plots
#' @param ... sends any further arguments to \link{map.select} and \link{map.ellipse}.
#' @examples
#' \dontrun{
#' example(soc.ca)
#' map.ellipse.array(result, active[, 1])
#' }
#' @export
map.ellipse.array <- function(object, variable, dim = c(1,2),
                              draw.ellipses = TRUE, ncol = 2,
                              titles = levels(variable), main.title = "",  ...){
  
  var.levels      <- levels(variable)
  list.of.maps    <- list()
    
  for (i in 1:length(var.levels)){
  ind.ind         <- which(variable == var.levels[i]) 
  ellipse.ind     <- variable == var.levels[i]
  ellipse.ind[ellipse.ind == FALSE]  <- NA 
  
  p               <- map.select(object, dim = dim, list.ind = ind.ind, map.title = titles[i], label = FALSE, ...)
  
  if(identical(draw.ellipses, TRUE)) p  <- map.ellipse(object, p, ellipse.ind, ellipse.label = FALSE, label.size = 4, ...)
  list.of.maps[[i]]  <- p
  }
  
  # Standardize the coordinates
  list.of.maps <- fix.coords(list.of.maps)
  # Plot the maps
  map.array(list.of.maps, ncol = ncol, title = main.title)
  return(invisible(list.of.maps))
}

#################################################################################################
#' Array of several CSA maps
#' 
#' Creates an array of Class Specific Mulitple Correspondence analysises
#' 
#' @param object a \link{soc.ca} result object
#' @param variable a factor with the same order and length as those used for the active modalities in object
#' @param dim indicates what dimensions to map and in which order to plot them
#' @param ncol the number of columns the maps are arranged into
#' @param titles a vector of the same length as the number of levels in \code{variable}. These are the titles given to each subplot
#' @param main.title the main title for all the maps
#' @param FUN the mapping function used for the plots; \link{map.active}, \link{map.ctr}, \link{map.ind}, \link{map.select} or \link{map.sup}
#' @param fixed.coord if TRUE the limits of all plots are set to the same as the largest plot
#' @param ... sends any further arguments to the mapping functions
#' @export  
#' @examples
#' \dontrun{
#' example(soc.csa)
#' map.csa.all(result, active[, 1])
#' map.csa.all(result, active[, 1], FUN = map.ctr, ctr.dim = 1)
#' }

map.csa.all   <- function(object, variable, dim = c(1,2), ncol = 2, FUN = map.ind,
                          fixed.coord = TRUE, main.title = "", titles = levels(variable),...){
  
  csa.list     <- csa.all(object, variable)
  csa.results  <- csa.list$results
  plot.list    <- lapply(csa.results, FUN, dim = dim, ...) 
  
  # Map titles
  if (length(titles) > 1){
  for (i in 1:length(titles)){
  plot.list[[i]]  <- plot.list[[i]] + ggtitle(titles[i])
  }
  }
  
  # Coord standardization
  if (identical(fixed.coord, TRUE)) plot.list  <- fix.coords(plot.list)  
  map.array(plot.list, ncol = 2, title = main.title)
}

#######################################################################3
### Standardize coordinates

fix.coords <- function(plot.list, padding = 0.15){

minimums.x    <- vector(, length = length(plot.list)) 
minimums.y    <- vector(, length = length(plot.list))
maximums.x    <- vector(, length = length(plot.list))
maximums.y    <- vector(, length = length(plot.list))

for ( i in 1:length(plot.list)){
p                <- plot.list[[i]]
minimums.x[i]    <- p$ca.scales$lim.min.x
minimums.y[i]    <- p$ca.scales$lim.min.y
maximums.x[i]    <- p$ca.scales$lim.max.x
maximums.y[i]    <- p$ca.scales$lim.max.y
}

# Standardize the coordinates
xlim         <- c(max(maximums.x) + padding, min(minimums.x) - padding)
ylim         <- c(max(maximums.y) + padding, min(minimums.y) - padding) 

for ( i in 1:length(plot.list)){
  plot.list[[i]]  <- plot.list[[i]] + coord_fixed(xlim = xlim, ylim = ylim)
}
return(plot.list)
}

#' Map the coordinates of the individuals in a CSA and its MCA
#'
#' @param csa.object a result object created by the \link{soc.csa} function
#' @param mca.dim the dimension from the original MCA
#' @param csa.dim the dimension from the CSA
#' @param smooth if TRUE a line is added to the plot
#' @param method the method used by ggplot to set the line see \link{geom_smooth}
#' @seealso \link{soc.csa}, \link{map.csa.all}, link{map.csa.mca.array}
#' @export
#' @examples
#' example(soc.csa)
#' csa.res  <- soc.csa(result, class.age)
#' map.csa.mca(csa.res, mca.dim = 2, csa.dim = 1)
map.csa.mca <- function(csa.object, mca.dim = 1, csa.dim = 1, smooth = TRUE, method = "auto"){
  mca.res           <- csa.object$original.result
  class.indicator   <- csa.object$original.class.indicator
  mca.coord         <- mca.res$coord.ind[class.indicator, mca.dim]
  mca               <- mca.coord
  csa               <- csa.object$coord.ind[, csa.dim]
  ggdata            <- data.frame(mca = mca, csa = csa)
  titles            <- paste(c("CSA Dim:", "MCA Dim:"), c(csa.dim, mca.dim))
  
  p                 <- ggplot(ggdata, aes(x = mca, y = csa))
  if(smooth == TRUE) p   <- p +  geom_smooth(fill = "grey90", color = "red", method = method)
  p                 <- p + geom_point(shape = 21, size = 3, alpha = 0.8) + theme_min() + xlab(titles[2]) + ylab(titles[1])
  
  p$ca.scales       <- breaksandscales(ggdata)
  p
}

#' CSA-MCA array
#' 
#' Create an array of \link{map.csa.mca} maps
#' 
#' @param csa.object a result object created by the \link{soc.csa} function
#' @param ndim the number of dimensions to include in the array, starting from 1
#' @param fixed.coord if TRUE the limits of all plots are set to the same as the largest plot
#' @param ... for further arguments see \link{map.csa.mca}
#' @export
#' @examples
#' example(soc.csa)
#' csa.res <- soc.csa(result, class.age)
#' map.csa.mca.array(csa.res, ndim = 3)

map.csa.mca.array <- function(csa.object, ndim = 3, fixed.coord = TRUE, ...){
  
  plot.list  <- list()
  count      <- 1
  for(j in 1:ndim){
    for (i in 1:ndim){
      plot.list[[count]]  <- map.csa.mca(csa.object, mca.dim = i, csa.dim = j, ...)
      count  <- count + 1
    }
  }
  
  suppressMessages(print(map.array(plot.list, ncol = ndim, fixed.coord = fixed.coord)))
}
#' Extract categories
#'
#' @param result 
#' @param dim 
#'
#' @return
#' @export
#'
#' @examples

extract_mod         <- function(result, dim = 1:2){
  coord.mod           <- result$coord.mod[, dim]
  rownames(coord.mod) <- result$names.mod
  coord.mod           <- coord.mod[,]
  colnames(coord.mod) <- c("X", "Y")
  
  md            <- coord.mod %>% data.frame() %>% rownames_to_column(var = "Modality")
  ctr           <- result$ctr.mod[, dim]
  md$ctr.x      <- ctr[, 1]
  md$ctr.y      <- ctr[, 2]
  md$ctr        <- rowSums(ctr) / 2
  md$ctr.set    <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md$Frequency  <- result$freq.mod
  md$Variable   <- result$variable
  md
}

#' Extract supplementary categories
#'
#' @param result 
#' @param dim 
#'
#' @return
#' @export
#'
#' @examples
extract_sup         <- function(result, dim = 1:2){
  coord.sup           <- result$coord.sup[, dim]
  rownames(coord.sup) <- result$names.sup
  coord.sup           <- coord.sup[,]
  colnames(coord.sup) <- c("X", "Y")
  
  md                  <- coord.sup %>% data.frame() %>% rownames_to_column(var = "Modality")
  md$Frequency        <- result$freq.sup
  md$Variable         <- result$variable.sup
  md
}

#' Extract individuals
#'
#' @param result 
#' @param dim 
#'
#' @return
#' @export
#'
#' @examples
extract_ind         <- function(result, dim = 1:2){
  coord.ind           <- result$coord.ind[, dim]
  rownames(coord.ind) <- result$names.ind
  coord.ind           <- coord.ind[,]
  colnames(coord.ind) <- c("X", "Y")
  
  md           <- coord.ind %>% data.frame() %>% rownames_to_column(var = "Individual")
  ctr          <- result$ctr.ind[, dim]
  md$ctr.x     <- ctr[, 1]
  md$ctr.y     <- ctr[, 2]
  md$ctr       <- rowSums(ctr) / 2
  md$ctr.set   <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md
}


#' Create the base of a soc.ca map
#'
#' @param up 
#' @param down 
#' @param right 
#' @param left 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples

map.ca.base <- function(up = NULL, down = NULL, right = NULL, left = NULL, ...){
  
  breaks.major <- seq(-100, 100, by = 0.25)
  labels       <- breaks.major
  labels[c(FALSE, TRUE)] <- ""
  
  p      <- ggplot(...) + geom_vline(xintercept = 0, size = 0.2) + geom_hline(yintercept = 0, size = 0.2)
  p      <- p + scale_x_continuous(sec.axis = sec_axis(~.*1, name = up, breaks = breaks.major, labels = labels),  
                                   name = down, breaks = breaks.major, labels = labels)
  
  
  p      <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = right, breaks = breaks.major, labels = labels),  
                                   name = left, breaks = breaks.major, labels = labels)  
  
  p      <- p + theme(axis.title.y.left = element_text(size = 16), axis.title.y.right =  element_text(size = 16))
  p      <- p + theme(axis.title.x.top = element_text(size = 16), axis.title.x.bottom =  element_text(size = 16))
  
  
  theme_ca_base <- function (base_size = 15, base_family = "serif", ticks = TRUE) 
  {
    ret <- theme_bw(base_family = base_family, base_size = base_size) + 
      theme(legend.background = element_blank(), legend.key = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            strip.background = element_blank(), plot.background = element_blank(), 
            axis.line = element_blank(), panel.grid = element_blank())
    if (!ticks) {
      ret <- ret + theme(axis.ticks = element_blank())
    }
    ret
  }
  
  p      <- p + theme_ca_base()
  
  
  p      <- p + scale_size_continuous(range = c(0.1, 2))
  p      <- p + theme(legend.position = "bottom")
  p
}
#### Functions for describtion and printing

#' Print soc.ca objects
#' 
#' Prints commonly used measures used in the analysis of multiple correspondence
#' analysis
#' @param x is a soc.ca class object
#' @param ... further arguments are ignored
#' @return Active dimensions is the number of dimensions remaining after the
#'   reduction of the dimensionality of the analysis.
#' @return Active modalities is the number of modalities that are not set as
#'   passive.
#' @return Share of passive mass is the percentage of the total mass that is
#'   represented by the passive modalities.
#' @return The values represented in the scree plot are the adjusted inertias,
#'   see \link{variance}
#' @return The active variables are represented with their number of active
#'   modalities and their share of the total variance/inertia.
#' @export print.soc.mca
#' @seealso \link{soc.mca}, \link{contribution}
#'   
#' @examples
#' example(soc.ca)
#' print(result)
#' @export

print.soc.mca  <- function(x, ...){
  
  # Help functions
  scree       <- function(x, dim = 6){
    set.dim   <- dim
    dim       <- ifelse((nrow(x$adj.inertia)<dim) == TRUE, nrow(x$adj.inertia), dim)
    adj       <- round(x$adj.inertia[1:dim, 4], digits = 1)
    stars     <- round(round(adj)/2)
    starscree <- vector("list", set.dim)
    for (i in 1:length(stars)){
      starscree[[i]] <- noquote(rep("*", stars[i]))
    }
    return(starscree)
    # x is a soc.ca class object
    # Dim is the number of dimensions included in the plot
  }
    
  Nmodal       <- x$n.mod
  Nsup         <- sum(x$freq.sup != 0)
  Nid          <- x$n.ind
  Share.of.var <- round((x$modal[, "Nb. active modalities"] - 1) / (length(x$names.passive) + Nmodal - nrow(x$modal)), 2) * 100
  Vnames       <- paste(rownames(x$modal), " [", x$modal[, "Nb. active modalities"], " - ", format(Share.of.var), "%]", sep = "")
  Vnames       <- Vnames[order(Share.of.var, decreasing = TRUE)]
  Submass 	   <- 1 - round(sum(x$mass.mod), digits = 2) 
  act.dim 	   <- nrow(x$adj.inertia)
  dim80 		   <- which.min(x$adj.inertia[, 5] < 80)
  scree.dim	   <- 7
  N.pas.mod    <- length(x$names.passive)
  stars 		   <- scree(x, scree.dim)
  adj.dim      <- 1:scree.dim
  
  dim.a        <- ifelse((scree.dim < nrow(x$adj.inertia)), scree.dim, nrow(x$adj.inertia))
  adj          <- vector(mode = "numeric", length = scree.dim)
  adj[1:dim.a] <- x$adj.inertia[1:dim.a, 4]
  adj	         <- paste(formatC(adj,format = "f",digits = 1), sep = "", collide = "%")
  
  ## Output
  # Soc.csa title
  if (inherits(x, "soc.csa") == TRUE) cat(format("Class Specific Multiple Correspondence Analysis:", 	width = 90, justify = "centre"),"\n", "\n")
  # Soc.mca title
  if (inherits(x, "soc.csa") == FALSE) cat(format("Specific Multiple Correspondence Analysis:",   width = 90, justify = "centre"),"\n", "\n")
  
  cat(format("Statistics",  width = 50, justify = "centre"), format("Scree plot", width = 40, justify = "centre"),"\n",
      
      format("	Active dimensions: ", 			width = 40,), format(act.dim, 	width = 10, justify = "right"),
      format("|  1.", width = 10, justify = "centre" ), format(adj[1], width = 10, justify = "centre"), format(paste(stars[[1]]), width = 1), "\n",

      format("	Dimensions explaining 80% of inertia: ",width = 40,), format(dim80, 	width = 10, justify = "right"), 
      format("|  2.", width = 10, justify = "centre" ), format(adj[2], width = 10, justify = "centre"), format(paste(stars[[2]]), width = 1), "\n",
      
      format("	Active modalities: ", 			width = 40,), format(Nmodal, 	width = 10, justify = "right"), 
      format("|  3.", width = 10, justify = "centre" ), format(adj[3], width = 10, justify = "centre"), format(paste(stars[[3]]), width = 1), "\n",
      
      format("	Supplementary modalities: ",		width = 40,), format(Nsup, 	width = 10, justify = "right"), 
      format("|  4.", width = 10, justify = "centre" ), format(adj[4], width = 10, justify = "centre"), format(paste(stars[[4]]), width = 1), "\n",
      
      format("	Individuals: ",		 		width = 40,), format(Nid, 	width = 10, justify = "right"), 
      format("|  5.", width = 10, justify = "centre" ), format(adj[5], width = 10, justify = "centre"), format(paste(stars[[5]]), width = 1), "\n",
      
      format("	Share of passive mass:",	 		width = 40,), format(Submass, 	width = 10, justify = "right"), 
      format("|  6.", width = 10, justify = "centre" ), format(adj[6], width = 10, justify = "centre"), format(paste(stars[[6]]), width = 1), "\n",
      
      format("\tNumber of passive modalities:",	 		width = 40, ), format(N.pas.mod, 	width = 10, justify = "right"), 
      format("|  7.", width = 10, justify = "centre" ), format(adj[7], width = 10, justify = "centre"), format(paste(stars[[7]]), width = 1), "\n",
      
      "\n",
      format(paste("The", length(Vnames),"active variables: [No. modalities - share of variance]"), 			width = 100, justify = "centre" ),
      "\n",
      "\n",
      sep = "")
  
  Vnames <- Vnames[order(Share.of.var, decreasing = TRUE)]
  
  if(length(Vnames) > 20) Vnames <- c(head(Vnames, 20), "[...]")
  
  cat(format(Vnames, width = 25, justify = "right"), fill = 100)
}

#' Contribution balance
#' 
#' Calculates the balance of the contribution of each dimension. This measure
#' indicates whether too much of a dimensions contribution is placed on either
#' the + or - side of the dimension.
#' @param object is a soc.ca class object
#' @param act.dim is the number of active dimensions to be measured
#' @return A matrix with the share of contribution on each side of 0 and their
#'   balance (+/-)
#' @seealso \link{soc.mca}, \link{contribution}
#' @examples
#' example(soc.ca)
#' balance(result)
#' balance(result, act.dim = 3)
#' @export

balance   <- function(object, act.dim = object$nd){
  coord   <- object$coord.mod[, 1:act.dim]
  contrib <- object$ctr.mod[, 1:act.dim]
  pm      <- matrix(, nrow = act.dim, ncol = 3)
  for (i in 1:act.dim){
    temp  <- cbind(coord[, i], contrib[, i])
    temp  <- temp[order(temp[, 1]), ]
    plus  <- temp[which(temp[, 1] >= 0), ]
    minus <- temp[which(temp[, 1] <= 0), ]
    pcontrib <- sum(plus[, 2])
    mcontrib <- sum(minus[, 2])
    pm[i, 1] <- pcontrib
    pm[i, 2] <- mcontrib
    pm[i, 3] <- pcontrib/mcontrib
  }
  colnames(pm) <- c("+ Contrib.", "- Contrib.", "Balance (+/-)")
  return(pm)
}

#' Summaries of contribution values
#' 
#' Different forms of contribution summaries for \link{soc.ca} objects. Results
#' are presented according to the specified \code{mode}
#' @param object a \link{soc.ca} object
#' @param dim the included dimensions
#' @param all If TRUE returns all modalities instead of just those that
#'   contribute above average
#' @param indices If TRUE; returns a vector with the row indices of the
#'   modalities or individuals
#' @param mode indicates which form of output. Possible values: \code{"sort"},
#'   \code{"mod"}, \code{"ind"}, \code{"variable"}. If the mode is
#'   \code{"variable"}, \code{dim} can be a sequence of dimensions: \code{1:5}
#' @param if TRUE; returns output as a matrix instead of as printed output.
#' @return Each mode prints different results:
#' @return   \item{"mod"}{Ranks all modalities according to their contribution}
#'   \item{"sort"}{Ranks all modalities according to their contribution and then sorts them according to their coordinates}
#'   \item{"ind"}{Ranks all individuals according to their contribution}
#'   \item{"variable"}{Sorts all modalities according to their variable and sums the contributions per variable}
#' @return The values reported:
#' \item{Ctr}{Contribution values in percentage. Contribution values for individuals are reported in permille}
#' \item{Coord}{Principal coordinates}
#' \item{Cor}{The correlation with the dimension}
#' @seealso \link{map.ctr}
#' @examples
#' 
#' example(soc.ca)
#' contribution(result)
#' contribution(result, 2)
#' contribution(result, dim = 3, all = TRUE)
#' contribution(result, indices = TRUE)
#' contribution(result, 1:2, mode = "variable")
#' @export

contribution <- function(object, dim = 1, all = FALSE, indices = FALSE, mode = "sort", matrix.output = FALSE){
  
  if (indices == TRUE & mode == "mod"){
    ctr     <- object$ctr.mod[,dim]
    av.ctr  <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm = TRUE))))
    if (is.list(av.ctr) == TRUE) av.ctr  <- unlist(av.ctr[dim], use.names = FALSE)
    av.ctr  <- av.ctr[duplicated(av.ctr) == FALSE]    
    return(av.ctr)
  }
    
  # Modalities
  if (identical(mode, "mod")){
    if (length(dim) > 1 ) stop("This mode does not support more than 1 dimension")
    ctr     <- round(100 * object$ctr.mod[, dim], digits = 1)
    cor     <- round(100 * object$cor.mod[, dim], digits = 1)
    coord   <- round(object$coord.mod[, dim], digits = 2)
    names   <- object$names.mod
    if (identical(all, FALSE) == TRUE){
      av.ctr <- contribution(object, dim = dim, indices = TRUE, mode = mode)    
      header <- paste("The modalities contributing above average to dimension: ", dim, ".", sep = "")
    }
    if (identical(all, TRUE) == TRUE){
      av.ctr <- 1:length(ctr)
      header <- paste("The contribution of all modalities to dimension: ", dim, ".", sep = "")
    }
    
    out           <- data.frame(ctr[av.ctr], cor[av.ctr], coord[av.ctr])
    rownames(out) <- names[av.ctr]
    colnames(out) <- c("   Ctr.", "   Cor." , "   Coord")
    out           <- out[order(-out[, 1]), ]
    maxwidth      <- max(nchar(names)) + sum(nchar(colnames(out)))
    
    cat("\n", format(header, width = maxwidth, justify = "centre"), "\n", "\n")
    print(out)
  }
  
  # Individuals  
  if (identical(mode, "ind")){
    individuals(object, dim, indices = indices, all = FALSE)
  }
  # Side sorted modalities
  if (identical(mode, "sort")){
    if(length(dim) > 1 ) stop("Sort mode does not support more than 1 dimension")
    tab.dim(object, dim)
  }
  # Variables
  if (identical(mode, "variable")){
    tab.variable(object, dim)
  }
}

#' Contribution for headings
#'
#' Contribution.headings sums the contribution of the active categories according to the heading of their variables.
#'
#' @param object a \link{soc.ca} object
#' @param dim the included dimensions
#'
#' @return a matrix with contribution values for each heading per dimension
#' @export
#'
#' @examples
#' 
#' 
contribution.headings <- function(object, dim = 1:3){
  ctr                 <- object$ctr.mod[, dim]
  colnames(ctr)       <- paste("Dim.", dim)
  out                 <- aggregate(ctr, by = list(Heading = object$headings), FUN = sum)
  out[,-1]            <- round(out[-1], 2)
  out
}


# ' The most contributing individuals
# ' 
# ' Returns the individuals with above average contribution to the selected dimension
# ' @param object is a soc.ca object
# ' @param dim is the included dimensions
# ' @param all: If TRUE returns all individuals instead of just those that contribute above average
# ' @param ind.indices: If TRUE returns a vector with the row indices of the individuals
# ' @return Ctr is the contribution in 1000
# ' @return Cor is the correlation with the dimension
# ' @return Coord is the principal coordinate
# ' @seealso \link{tab.dim}, \link{soc.mca}, \link{contribution}, \link{p.id}
# ' @export

individuals <- function(object, dim = 1, all = FALSE, indices = FALSE){
  
  if (identical(indices, TRUE) == TRUE){
    ctr        <- object$ctr.ind[,dim]
    av.ctr     <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm = TRUE))))
    if(is.list(av.ctr) == TRUE) av.ctr  <- unlist(av.ctr[dim], use.names = FALSE)
    av.ctr     <- unique(av.ctr)
    return(av.ctr)
  }else{
    ctr        <- object$ctr.ind[, dim]
    ctr.round  <- round(100 * object$ctr.ind[, dim], 2)
    coord      <- round(object$coord.ind[, dim], 2)
    names      <- object$names.ind
    if (identical(all, FALSE) == TRUE){
      av.ctr   <- individuals(object, dim = dim, indices = TRUE, all = FALSE)    
      header   <- paste("The individuals contributing above average to dimension: ",
                        paste(dim, collapse = ", "), ".", sep = "")
    }
    if (identical(all, TRUE) == TRUE){
      av.ctr   <- 1:length(ctr)
      header   <- paste("The contribution of all individuals to dimension: ",
                        paste(dim, collapse = ", "), ".", sep = "")
    }
    
    out        <- data.frame(ctr.round, coord)[av.ctr, ]
    rownames(out) <- names[av.ctr]
    colnames(out) <- c(paste("   Ctr.", dim), paste("   Coord.", dim))
    out        <- out[order(-out[, 1]), ]
    
    maxwidth   <- max(nchar(names)) + sum(nchar(colnames(out)))
    cat("\n", format(header, width = maxwidth, justify = "centre"), "\n", "\n")
    print(out)
  }

}

# ' The most contributing modalities according to direction on dimension
# ' 
# ' Gives the most contributing modalities sorted according to direction on dimension
# ' @param x is a soc.ca object
# ' @param dim is the dimension
# ' @param label.plus is the label of the dimensions plus side
# ' @param label.minus is the label of the dimensions minus side
# ' @param all defines whether all modalities are to be printed
# ' @seealso \link{contribution}, \link{soc.mca}, \link{p.ctr}
# ' @examples
# ' example(soc.ca)
# ' tab.dim(result, 2)
# ' tab.dim(result, 2, label.plus = "Technical capital", label.minus = "Organizational capital")
# ' @export


tab.dim <- function(x, dim = 1, label.plus = NULL, label.minus = NULL, all = FALSE){
  
  if (identical(label.plus, NULL) == TRUE){
    label.plus    <- paste("Dimension ", dim ,". (+)", sep = "")
  }
  
  if (identical(label.minus, NULL) == TRUE){
    label.minus   <- paste("Dimension ", dim ,". (-)", sep = "")
  }
  
  ctr             <- round(100 * x$ctr.mod[, dim], digits = 1)
  coord           <- round(x$coord.mod[, dim], digits = 2)
  names           <- x$names.mod
  
  if (identical(all, FALSE) == TRUE){
    av.ctr        <- contribution(x, dim = dim, indices = TRUE, mode = "mod")    
  }
  if (identical(all, TRUE) == TRUE){
    av.ctr        <- seq(x$n.mod)
  }
  
  out             <- data.frame(ctr[av.ctr], coord[av.ctr])
  names           <- names[av.ctr]
  maxwidth        <- max(nchar(names))
  
  for (i in seq(names)){
    width         <- maxwidth-nchar(names[i])
    fill          <- paste(rep(" ", width), sep = "", collapse = "")
    names[i]      <- paste(names[i], fill, sep = "", collapse = "")
  }
  rownames(out)   <- names
  ctr.lab         <- paste("Ctr")
  coord.lab       <- paste("Coord")
  colnames(out)   <- c(ctr.lab, coord.lab)
  out             <- out[order(-out[, 1]), ]
  out.label       <- c(ctr.lab, coord.lab)
  outminus        <- out[which(out[, 2] <= 0), ]
  outplus         <- out[which(out[, 2] >= 0), ]
  
  cat("\n", format(label.plus, width = maxwidth, justify = "centre"), "\n")
  print(format(outplus, justify = "centre", width = 8))
  cat("\n", format(label.minus, width = maxwidth, justify = "centre"), "\n")
  print(format(outminus, justify = "centre", width = 8))
  
  }

# ' Contribution per variabel
# ' 
# ' tab.variable returns the contribution values of all modalities ordered by variable
# ' 
# ' @param object is a soc.ca object
# ' @param dim is the included dimensions. The default is 1:3
# ' @param If sup = TRUE the coordinates of the supplementary variables are given instead
# ' @return If assigned using <- tab.variable returns a list of matrixes with the contribution values
# ' @return The returned list is a tab.variable class object and can be exported with the \link{export} function included in the soc.mca package.  
# ' @seealso \link{export}, \link{contribution}
# ' @examples
# ' example(soc.ca)
# ' tab.variable(result)
# ' tab.variable(result, dim = c(1, 3))
# ' tab.variable(result, sup = TRUE)
# ' @export

tab.variable    <- function(object, dim = 1:3, sup = FALSE){
    variable    <- as.factor(object$variable)
    ctr.mod     <- as.matrix(object$ctr.mod[, dim])
    lev.var     <- levels(variable)
    names.mod   <- object$names.mod
    freq.mod    <- object$freq.mod
    
    var.list    <- list()
    for (i in seq(length(lev.var))){
        var.ctr           <- round(ctr.mod[variable == lev.var[i], ] * 100, digits = 1)
        var.ctr           <- cbind(var.ctr, freq.mod[variable == lev.var[i]])
        var.ctr           <- rbind(var.ctr, colSums(var.ctr))
        rownames(var.ctr) <- c(names.mod[variable == lev.var[i]], "Total")
        colnames(var.ctr) <- c(paste(" Dim.", dim, sep = ""), "  Freq")
        
        var.list[[paste(lev.var[i])]] <- var.ctr
    }
    
    ### Supplementary modalities
    
    if (identical(sup, TRUE)){
      
      variable    <- as.factor(object$variable.sup)
      coord.sup   <- object$coord.sup[, dim]
      lev.var     <- levels(variable)
      names.mod   <- object$names.sup
      freq.mod    <- object$freq.sup
      
      var.list    <- list()
      for (i in seq(length(lev.var))){
        var.ctr           <- round(coord.sup[variable == lev.var[i], ], digits = 2)
        var.ctr           <- cbind(var.ctr, freq.mod[variable == lev.var[i]])
        rownames(var.ctr) <- c(names.mod[variable == lev.var[i]])
        colnames(var.ctr) <- c(paste(" Dim.", dim, sep = ""), "  Freq")
        
        var.list[[paste(lev.var[i])]] <- var.ctr
      }
          
      
    }
    
    # The printing
    
    av.ctr      <- round(100/object$n.mod, digits = 1)
    maxwidth    <- max(nchar(names.mod))
    l           <- ncol(var.ctr)
        
    if (identical(sup, FALSE)) cat("The contribution of the active variables")
    if (identical(sup, TRUE))  cat("The coordinates of the supplementary variables")
    
    # Beautiful printing!
    for (i in seq(length(lev.var))){
        var.ctr <- var.list[[i]]
        cat("\n", "\n", format(lev.var[i], width = maxwidth), colnames(var.ctr))
        
        for (q in seq(nrow(var.ctr))){
            cat("\n", format(rownames(var.ctr)[q], width = maxwidth),
                format(var.ctr[q, -l], width = 6), format(var.ctr[q, length(dim) + 1], width = 6, drop0trailing = TRUE))
        }
        
    }
    
    
    if (identical(sup, FALSE)) cat("\n", "Average contribution per modality: ", av.ctr, sep = "")
    cat("\n", "Total number of individuals: ", object$n.ind, sep = "")
    
    class(var.list) <- "tab.variable"
    invisible(var.list)
}


#' Variance table
#' 
#' variance returns a table of variance for the selected dimensions.
#' 
#' @param object is a soc.ca object
#' @param dim is the included dimensions, if set to NULL, then only the
#'   dimensions explaining approx. more than 0.90 of the adjusted variance are included
#' @return If assigned variance returns a matrix version of the table
#'   of variance.
#' @seealso \link{soc.mca}, \link{print.soc.mca}
#' @examples
#' example(soc.ca)
#' variance(result)
#' variance(result, dim = 1:4)
#' @export

variance    <- function(object, dim = NULL){
    
    variance <- object$adj.inertia
    if (identical(dim, NULL) == TRUE){
        dim  <- variance[, 5] <= 91
    }
    variance <- t(variance[dim, ])
    line.dim <- paste(1:ncol(variance), ".", sep = "")
    cat("\n", "Dim        ", format(line.dim, width = 6), sep = "")
    cat("\n", "Eigen    ", format(round(variance[2, ], 2), width = 6), sep = "")
    cat("\n", "Var     ", format(round(variance[3, ], 2), width = 6), sep = "")
    cat("\n", "Adj.Var ", format(round(variance[4, ], 1), width = 6), sep = "")
    cat("\n", "Cum %   ", format(round(variance[5, ], 1), width = 6), sep = "")
    
    invisible(variance)
}



#' Average coordinates
#' 
#' Find the average coordinates for each category in a variable on two dimensions.
#' 
#' @param object is soc.ca result object
#' @param x is a variable of the same length and order as the active variables used to construct the soc.ca object
#' @param dim is the two dimensions used
#' @return a matrix with the mean points and frequencies of the given variable
#' @export
#' @examples
#' example(soc.ca)
#' average.coord(result, sup$Income)

average.coord <- function(object, x, dim = c(1, 2)){
  
  coord             <- object$coord.ind[, dim]
  point.coord       <- aggregate(coord, list(x), mean)
  point.coord[, 2]  <- point.coord[, 2] / sqrt(object$eigen[dim[1]])
  point.coord[, 3]  <- point.coord[, 3] / sqrt(object$eigen[dim[2]])
  point.coord       <- cbind(point.coord, table(x)) 
  
  colnames(point.coord) <- c("label", "X", "Y", "group", "Freq")
  point.coord
}

#' CSA measures
#' 
#' Several measures for the evaluation of the relations between the dimensions of the CSA and the dimensions the of original MCA
#' 
#' @param csa.object is a "soc.csa" class object created by the \link{soc.csa} function
#' @param correlations if TRUE correlations calculated by the \link{cor} function is returned
#' @param cosines if TRUE cosine similarities are returned
#' @param cosine.angles if TRUE angles are calculated in the basis of the cosine values
#' @param dim.csa the dimensions included from the csa
#' @param dim.mca the dimensions included from the original mca
#' @param format if TRUE results are formatted, rounded and printed for screen reading, if FALSE the raw numbers are returned
#' @param ... furhter arguments are send to the \link{cor} function
#' @return A list of measures in either formatted or raw form.
#' @export
#' @examples
#' example(soc.csa)
#' csa.measures(res.csa)
#' csa.measures(res.csa, correlations = FALSE, cosine.angles = FALSE, dim.mca = 1:5, format = FALSE)

csa.measures      <- function(csa.object, correlations = FALSE, cosines = TRUE, cosine.angles = TRUE,
                               dim.mca = 1:5, dim.csa= 1:5, format = TRUE, ...){
  
  csca.coord      <- csa.object$coord.ind
  object          <- csa.object$original.result
  class.indicator <- csa.object$original.class.indicator
  ca.coord        <- object$coord.ind[class.indicator, ]
  csca.coord      <- csca.coord[, dim.mca]
  ca.coord        <- ca.coord[, dim.csa]
  
  ##################################
  # Correlations
  cor.mat             <- cor(csca.coord, ca.coord, ...)
  rownames(cor.mat)   <- paste("CSA:", dim.mca)
  colnames(cor.mat)   <- paste("MCA:", dim.csa)
  
  ####################################
  # Cosines similarity
  cosine.similarity      <- function(x, y) x %*% y / sqrt(x %*% x * y %*% y)
  cosine.mat             <- matrix(ncol = ncol(ca.coord), nrow = ncol(csca.coord))
  rownames(cosine.mat)   <- paste("CSA:", dim.mca)
  colnames(cosine.mat)   <- paste("MCA:", dim.csa)
  
  for (i in 1:ncol(csca.coord)){
    cosine.mat[i,]       <- apply(ca.coord, 2, cosine.similarity, csca.coord[, i])
  }
  
  cosine.mat <- cbind(cosine.mat, 
                      sqrt(cosine.mat[,1]^2 + cosine.mat[,2]^2), 
                      sqrt(cosine.mat[,1]^2 + cosine.mat[,3]^2),
                      sqrt(cosine.mat[,1]^2 + cosine.mat[,4]^2),
                      sqrt(cosine.mat[,1]^2 + cosine.mat[,5]^2),
                      sqrt(cosine.mat[,2]^2 + cosine.mat[,3]^2),
                      sqrt(cosine.mat[,2]^2 + cosine.mat[,4]^2),
                      sqrt(cosine.mat[,2]^2 + cosine.mat[,5]^2),
                      sqrt(cosine.mat[,3]^2 + cosine.mat[,4]^2),
                      sqrt(cosine.mat[,3]^2 + cosine.mat[,5]^2),
                      sqrt(cosine.mat[,4]^2 + cosine.mat[,5]^2),
                      sqrt(cosine.mat[,1]^2 + cosine.mat[,2]^2 + cosine.mat[,3]^2),
                      sqrt(cosine.mat[,2]^2 + cosine.mat[,3]^2 + cosine.mat[,4]^2),
                      sqrt(cosine.mat[,3]^2 + cosine.mat[,4]^2 + cosine.mat[,5]^2))
  
  colnames(cosine.mat)[length(dim.csa)+1]  <- "MCA: 1&2"
  colnames(cosine.mat)[length(dim.csa)+2]  <- "MCA: 1&3"
  colnames(cosine.mat)[length(dim.csa)+3]  <- "MCA: 1&4"
  colnames(cosine.mat)[length(dim.csa)+4]  <- "MCA: 1&5"
  colnames(cosine.mat)[length(dim.csa)+5]  <- "MCA: 2&3"
  colnames(cosine.mat)[length(dim.csa)+6]  <- "MCA: 2&4"
  colnames(cosine.mat)[length(dim.csa)+7]  <- "MCA: 2&5"
  colnames(cosine.mat)[length(dim.csa)+8]  <- "MCA: 3&4"
  colnames(cosine.mat)[length(dim.csa)+9]  <- "MCA: 3&5"
  colnames(cosine.mat)[length(dim.csa)+10] <- "MCA: 4&5"
  colnames(cosine.mat)[length(dim.csa)+11] <- "MCA: 1&2&3"
  colnames(cosine.mat)[length(dim.csa)+12] <- "MCA: 2&3&4"
  colnames(cosine.mat)[length(dim.csa)+13] <- "MCA: 3&4&5"
  #View(cosine.mat)
  #####################################
  # Angles
  cosine.to.angle       <- function(x) acos(abs(x))/pi * 180 
  angles                <- suppressWarnings(cosine.to.angle(cosine.mat))
  #View(angles)
  ####################################
  # Out.list
  out.list              <- list() 
  if (cosines == TRUE)        out.list$cosines      <- cosine.mat
  if (cosine.angles == TRUE)  out.list$angles       <- angles
  if (correlations == TRUE)   out.list$cor          <- cor.mat
  
  if (identical(format, FALSE)) return(out.list)
  
  ###################################
  # Formatted output
  if (identical(format, TRUE)){
    
    cat("\n", format("Measures for Class Specific Multiple Correspondence Analysis:",
                     width = 90, justify = "centre"), "\n", "\n")
    
    #############
    # Cosines
    if (cosines == TRUE){
      
      cat("\n", format("Cosine similarity:",   width = 10, justify = "right"), "\n", "\n")
      cosine.fat           <- cosine.mat
      rownames(cosine.fat) <- format(rownames(cosine.fat), width = 10, justify = "centre")
      colnames(cosine.fat) <- format(colnames(cosine.fat), width = 8, justify = "right")
      cosine.fat           <- format(cosine.fat, width = 8, justify = "left")
      print(noquote(cosine.fat))
      cat("\n", "\n")
    }
    
    #############
    # Angles
    if (cosine.angles == TRUE){
      cat("\n", format("Cosine angles:",   width = 10, justify = "right"), "\n", "\n")
      
      angles.fat           <- round(angles, 1) 
      rownames(angles.fat) <- format(rownames(angles.fat), width = 10, justify = "centre")
      colnames(angles.fat) <- format(colnames(angles.fat), width = 8, justify = "right")
      angles.fat           <- format(angles.fat, width = 8, justify = "left")
      print(noquote(angles.fat))
      cat("\n", "\n") 
    }
    
    ##############
    # Correlations
    if(correlations == TRUE){
      cat("\n", format("Correlations:",   width = 10, justify = "right"), "\n", "\n")
      cor.fat           <- round(cor.mat, 2) 
      rownames(cor.fat) <- format(rownames(cor.mat), width = 10, justify = "centre")
      colnames(cor.fat) <- format(colnames(cor.mat), width = 8, justify = "right")
      cor.fat           <- format(cor.fat, width = 8, justify = "left")
      print(noquote(cor.fat))
      cat("\n", "\n")
    }
  }
}


#' Calculate contributions per heading
#'
#' @param object a soc.ca object with headings
#' @param dim a numeric vector with the dimensions
#'
#' @return a matrix
#' @export
#'
#' @examples
#' data(taste)
#' active.headings <- list()
#' active.headings$Consumption <- na.omit(taste)[, c("TV", "Film", "Art", "Eat")]
#' active.headings$Background  <- na.omit(taste)[, c("Gender", "Age", "Income")]
#' result.headings <- soc.mca(active.headings)
#' headings(result.headings)

headings      <- function(object, dim = 1:3) {
  if(identical(object$headings, NULL) == TRUE) stop("You have defined no headings")
  
  headings    <- object$headings
  lev.head    <- unique(headings)
  variable    <- object$variable
#  lev.var     <- unique(variable)
  head.var    <- cbind(object$headings, object$variable)
  head.var    <- head.var[!duplicated(head.var[,2]),]
  
  head.ctr.total <- rep(1, (max(dim) + 3))
  
  tot    <- aggregate(rowSums(object$ctr.mod.raw), by = list(object$headings), sum)
  tot$x  <- round(tot$x / sum(tot$x) * 100,1)
  
  for (i in seq(length(lev.head))) {
    var.under.head     <- head.var[which(head.var[,1] == lev.head[i]),2]
    head.ctr           <- object$ctr.mod[which(variable %in% var.under.head),dim]
    head.ctr.total2    <- colSums(head.ctr)
    head.ctr.total2    <- c(length(var.under.head), nrow(head.ctr), tot$x[i], round(head.ctr.total2*100,1))
    head.ctr.total     <- rbind(head.ctr.total, head.ctr.total2)
  }
  
  head.ctr.total <- head.ctr.total[-1,]
  rownames(head.ctr.total) <- lev.head
  colnames(head.ctr.total) <- c("Variables", "Active Modalities", "Ctr. total",  paste("Ctr. dim: ", dim, sep = "")) 
  head.ctr.total
}


#' Breakdown of variance by group
#' 
#' Defining a partition of the cloud of individuals into groups, one can calculate the midpoints of the various groups. 
#' The total variance of the cloud of individuals can then be broken down to between–within variances, i.e. variance between the groups partitioning the cloud, and variance within the groups
#' The ratio of the between-variance to the total variance is denoted by η2 (eta-square), and accounts for the percentage of variance 'explained' by the group-variable.
#' (see Le Roux & Rouanet 2010, p. 20ff, 69, 114)
#' 
#' @param object is a soc.ca class object
#' @param dim the dimensions
#' @param variable a factor in the same length and order as the active variables
#'
#' @return a matrix
#' @references Le Roux, Brigitte, and Henry Rouanet. 2010. Multiple Correspondence Analysis. Thousand Oaks, Calif.: Sage Publications.
#' @export
#' @examples
#' example(soc.ca)
#' breakdown.variance(result, dim = 1:3, variable = sup$Gender)

breakdown.variance          <- function(object, dim = 1:3, variable) {

  if (anyNA(variable)) stop(substitute(variable), " contains NA - convert to missing")
  
  s          <- levels(variable)
  coords     <- matrix(NA, nrow = length(s), ncol = length(dim))
  var        <- matrix(NA, nrow = length(s), ncol = length(dim))
  weights    <- matrix(NA, nrow = length(s), ncol = 2)
  
  for (i in 1:length(s)) {
    weights[i,1] <- nrow(object$coord.ind[variable == s[i], dim]) 
    weights[i,2] <- nrow(object$coord.ind[variable == s[i], dim]) / length(variable)
  }
  
  for (i in 1:length(s)) {
    coords[i, 1:(length(dim))] <- colSums(object$coord.ind[variable == s[i], dim] / sum(variable == s[i]))
    
  }
  rownames(coords) <- s
  
  var        <- matrix(NA, nrow = length(s), ncol = length(dim))
  
  for (j in 1:length(dim)) {
    for (i in 1:length(s)) {
      var[i,j] <- sum((object$coord.ind[variable == s[i],j] - coords[i,j])^2)/sum(variable == s[i])
    }}
  
  within      <- colSums(weights[,2]*var)
  bet         <- colSums(coords[,dim]^2 * weights[,2])
  total       <- within + bet
  n2          <- bet / total
  variance    <- rbind(var, within, bet, total, n2)
  weights[,2] <- round(100*weights[,2], 1)
  meanpoints  <- rbind(cbind(weights, round(coords,3)), matrix(NA, ncol = ncol(weights) + ncol(coords), nrow= 4))
  
  out <- cbind(meanpoints, round(variance,4))
  
  rownames(out) <- c(s, "Within", "Between", "Total", "Correlation ratio")
  colnames(out) <- c("freq", "rel.freq", paste("Meanpoint: Axis ", dim), paste("Variance: Axis ", dim))
  out
  
}

######################### ELLIPSES
#' Concentration ellipses
#' 
#' Add ellipses for each level in a factor to a plot made from a \link{soc.ca} 
#' object.
#' @param object is a \link{soc.ca} class object.
#' @param ca.plot is a plot made from a \link{soc.ca} object.
#' @param variable is a factor of the same length and in the same order as the 
#'   active varibles used for the \link{soc.ca} object.
#' @param ellipse.label if TRUE the labels are included in the map.
#' @param ellipse.color defines the color of the ellipses. If "default" the globally defined default colors are used. Ellipse.color can be either length of 1 or equal to the number of drawn levels.
#' @param label.size defines the size of the labels.
#' @param draw.levels indicates the levels in the variable for which a ellipse is drawn.
#' @param ellipse.line defines the type of line used for the ellipses.
#' @return a plot with a concentration ellipse containing 80\% of the 
#'   individuals for each modality.
#' @seealso \link{map.ind}, \link{map.ctr}
#' @examples
#' example(soc.ca)
#' map <- map.ind(result)
#' map.ellipse(result, map, active[,2])
#' @export

map.ellipse <- function(object, ca.plot = map.ind(object), variable, ellipse.label = TRUE,
                        ellipse.color = "default", label.size = 4, draw.levels = 1:nlevels(variable), ellipse.line = "solid"
){
  dim         <- ca.plot$dimensions 
  id.coord    <- as.data.frame(object$coord.ind[, dim])
  lev.var     <- levels(variable)[draw.levels]
  levels(variable)[-which(levels(variable) %in% lev.var)] <- NA
  id          <- object$names.ind
  
  if(identical(ellipse.color, "default")){
    ellipse.color        <- getOption(x = "soc.ca.colors")
    ellipse.color        <- ellipse.color[1:length(draw.levels)]
  }
  
  ellipse.colors   <- vector(length=length(draw.levels))
  ellipse.colors[] <- ellipse.color
  coord.list       <- split(id.coord, variable)
  
  ellipse.data              <- function(coord, id){
    out.list                <- list()
    out.list$ellipse.coord  <- ellipse.coord(id, coord)
    out.list$ellipse.axis   <- ellipse.axis(out.list$ellipse.coord)
    out.list$ellipse.origo  <- ellipse.origo(out.list$ellipse.axis)
    out.list
  }
  
  ellipse.list              <- lapply(coord.list, ellipse.data, id = id)
  
  for ( i in 1:length(ellipse.list)){
    ellipse.list[[i]]$color <- ellipse.colors[i] 
    ellipse.list[[i]]$label <- lev.var[i]
  }
  
  
  ellipse.annotate          <- function(ellipse.data, ca.plot, ellipse.label = TRUE, label.size = 4, ellipse.line = "solid"){
    e.plot     <- ca.plot + annotate(geom = "path", x = ellipse.data$ellipse.coord[,1], y = ellipse.data$ellipse.coord[,2],
                                     color = ellipse.data$color, linetype = ellipse.line)
    e.plot     <- e.plot + annotate(geom = "line", x = ellipse.data$ellipse.axis[[1]]$x, y = ellipse.data$ellipse.axis[[1]]$y,
                                    color = ellipse.data$color, , linetype = ellipse.line)
    e.plot     <- e.plot + annotate(geom = "line", x = ellipse.data$ellipse.axis[[2]]$x, y = ellipse.data$ellipse.axis[[2]]$y,
                                    color = ellipse.data$color, linetype = ellipse.line)
    e.plot     <- e.plot + annotate(geom = "point", x = ellipse.data$ellipse.origo[[1]], y = ellipse.data$ellipse.origo[[2]],
                                    color = ellipse.data$color)
    if(identical(ellipse.label, TRUE)){
      e.plot     <- e.plot + annotate(geom = "text", x = ellipse.data$ellipse.origo[[1]], y = ellipse.data$ellipse.origo[[2]],
                                      label = ellipse.data$label, hjust = -0.15, fontface = "italic", size = label.size)
    }
    
    e.plot
  }
  
  for (i in 1:length(ellipse.list)) ca.plot <- ellipse.annotate(ellipse.list[[i]], ca.plot,
                                                                ellipse.label = ellipse.label, label.size = label.size,
                                                                ellipse.line = ellipse.line)
  ca.plot
}



################### Ellipse akser funktionen
ellipse.axis <- function(el.dat){
n            <- length(na.omit(el.dat[, 1]))
p.pairs      <- el.dat[1, ]
  for (i in 1:(n/2)){
p.pairs[i, ] <- c(i, i + (n / 2))
}
p.pairs      <- cbind(p.pairs, rep(0, n / 2))
colnames(p.pairs) <- c("x", "y", "distance")

for (i in 1:(n / 2)){
a             <- el.dat[i, ]
b             <- el.dat[i + (n / 2), ]
p.pairs[i, 3] <- ((b$x - a$x)^2 + (b$y - a$y)^2)^0.5
}
p.min         <- which.min(p.pairs[, 3])
p.max         <- which.max(p.pairs[, 3])

mintemp       <- p.pairs[p.min, 1:2]
minpath       <- as.data.frame(el.dat[c(mintemp$x, mintemp$y), ], row.names = c("1","2"))


maxtemp       <- p.pairs[p.max, 1:2]
maxpath       <- as.data.frame(el.dat[c(maxtemp$x, maxtemp$y), ], row.names = c("1","2"))

ellipse.axis.points <- list(maxpath, minpath)
ellipse.axis.points
}

############## Ellipse.coord
ellipse.coord <- function(id, id.coord.var){
id.var        <- as.data.frame(id.coord.var)
colnames(id.var) <- c("x", "y")

xdata         <- with(as.data.frame(id.var), xyTable(x, y))
xframe        <- cbind.data.frame(x = xdata$x, y = xdata$y, n = xdata$number)
data.ell      <- as.data.frame(with(id.var, ellipse(cor(x, y), 
                                    scale = c(sd(x), sd(y)), 
                                    centre = c(mean(x), mean(y)), npoints = 1000)))
data.ell
}

#############################3#### Ellipse origo
ellipse.origo <- function(el.axis){
 temp         <- el.axis[[1]][c(1, 2), ]
 x1           <- temp[1, 1]
 x2           <- temp[2, 1]
 y1           <- temp[1, 2]
 y2           <- temp[2, 2]
el.origo      <- c((x1 + x2) / 2, (y1 + y2) / 2)
el.origo
}
## Labels 

#' Add values to label
#' 
#' Adds values to the end of the label of each modality.
#' @param  object is a soc.ca object
#' @param  value the type of values added to the labels. "freq" adds
#'   frequencies, "mass" adds mass values to the active modalities, "ctr" adds contribution values to the active modalities, "cor" adds correlation values.
#'   value also accepts any vector with the length of the number of active
#'   modalities. "linebreak" adds a linebreak \code{\\n} after the first ":" in the label.
#' @param  prefix if "default" an appropriate prefix is used
#' @param  suffix the suffix
#' @param  dim the dimension from which values are retrieved
#' @return a soc.ca object with altered labels in names.mod and names.sup
#' @export
#' @examples
#' example(soc.ca)
#' result.label  <- add.to.label(result)
#' result.label$names.mod
#' result.label  <- add.to.label(result, value = "ctr", dim = 2)
#' result.label$names.mod
#' result.label  <- add.to.label(result, value = result$variable, prefix = " - ", suffix = "")
#' result.label$names.mod
#' result.label  <- add.to.label(result, value = "linebreak")
#' result.label$names.mod
#' map.ctr(result.label)

add.to.label <- function(object, value = "freq", prefix = "default", suffix = ")", dim = 1){
  
  # Names
  names.mod           <- object$names.mod
  names.sup           <- object$names.sup
  
  # Prefix
  if (identical(prefix, "default") & identical(value, "freq")) prefix   <- " (n:"
  if (identical(prefix, "default") & identical(value, "mass")) prefix   <- " (mass:"
  if (identical(prefix, "default") & identical(value, "ctr"))  prefix   <- " (ctr:"
  if (identical(prefix, "default") & identical(value, "cor"))  prefix   <- " (cor:"
  if (identical(prefix, "default") & length(value) > 1 )       prefix   <- " ("
  
  # Values
  if (identical(value, "freq")){
  val.mod             <- object$freq.mod
  val.sup             <- object$freq.sup
  object$names.mod    <- paste(names.mod, prefix, val.mod, suffix, sep = "")
  object$names.sup    <- paste(names.sup, prefix, val.sup, suffix, sep = "")
  }
  
  if (identical(value, "ctr")){
  val.mod             <- round(object$ctr.mod[dim, ] * 100, 1)
  object$names.mod    <- paste(names.mod, prefix, val.mod, suffix, sep = "")
  }
  
  if (identical(value, "mass")){
    val.mod           <- round(object$mass.mod * 100, 1)
    object$names.mod  <- paste(names.mod, prefix, val.mod, suffix, sep = "")
  }
  
  if (identical(value, "cor")){
    val.mod           <- round(object$cor.mod[, dim], 2)
    val.sup           <- round(object$cor.sup[, dim], 2)
    object$names.mod  <- paste(names.mod, prefix, val.mod, suffix, sep = "")
    object$names.sup  <- paste(names.sup, prefix, val.sup, suffix, sep = "")
  }
  
  if (length(value) > 1){
    object$names.mod  <- paste(names.mod, prefix, value, suffix, sep = "")
  }

  if (identical(value, "linebreak")){
    object$names.mod  <- sub(":", ":\n", names.mod)
    object$names.sup  <- sub(":", ":\n", names.sup)
    
  }
  
  colnames(object$indicator.matrix.active) <- object$names.mod
  
  return(object)
}

#' Exports the labels of a soc.ca object into a csv file.
#' 
#' This function allows easy translation and renaming of modalities by exporting
#' the labels into a .csv file that is easier to work with.
#' 
#' Two columns are created within the .csv: 'New label' and 'Old label'. In the
#' 'New label' column you write the new labels. Remember to leave 'Old label'
#' unchanged as this column is used for matching.
#' 
#' If you want to add frequencies to the labels with the \link{add.to.label}
#' function you should do this after exporting and assigning labels with the
#' \link{assign.label} function. Otherwise the matching of the labels is likely
#' to fail.
#' @param object is a soc.ca object
#' @param file is the name and path of the exported file
#' @param encoding is the character encoding of the exported file
#' @param overwrite decides whether to overwrite already existing files
#' @return A .csv with two columns and preferably UTF-8 encoding.
#' @export

export.label    <- function(object, file = FALSE, encoding = "UTF-8", overwrite = FALSE){
  
  names         <- c(object$names.mod, object$names.sup, object$names.ind)
  ca.label      <- cbind(names, names)
  colnames(ca.label)  <- c("New.label", "Old.label")
  
  if (identical(file, FALSE) == TRUE){
    file    <- paste("label_",deparse(substitute(object)), ".csv", sep = "")
  }
  if(file.exists(file) == TRUE & identical(overwrite, FALSE)) stop("File already exists")
  write.csv(ca.label, file = file, fileEncoding = encoding)

}


#' Assign new labels
#' 
#' Assigns new labels to a soc.ca object. The input labels are defined in a .csv
#' file created by the \link{export.label} function.
#' @param object is a soc.ca object
#' @param file is the path of the .csv file with the new labels. The file is
#'   preferably created by the \link{export.label} function
#' @param encoding is the encoding of the imported file
#' @param sep is the seperator used to create the imported .csv file
#' @return a soc.ca object with altered labels in \code{object$names.mod}, \code{object$names.ind} and
#'   \code{object$names.sup}
#' @details To use this function first export the labels from your soc.mca
#'   analysis with the \link{export.label} function. Then open and edit the
#'   created file with your favorite spreadsheet editor, like LibreOffice Calc.
#'   Change labels in the "new.label" column to the desired values and save. Use the
#'   assign.label function but remember to assign the results into a new object
#'   or overwrite the existing object.
#' @seealso \link{export.label}, \link{add.to.label}
#' @export

assign.label <- function(object, file = FALSE, encoding = "UTF-8", sep = ","){
  if (identical(file, FALSE) == TRUE){
    file <- paste("label_", deparse(substitute(object)), ".csv", sep = "")
  }
  label     <- read.csv(file, encoding = encoding, sep  =  sep)
  
  names.mod <- as.character(object$names.mod)
  names.sup <- as.character(object$names.sup)
  names.ind <- as.character(object$names.ind)
  
  new.label <- as.character(label$New.label)
  old.label <- as.character(label$Old.label)
  
  for (i in 1:length(old.label)){
    indices   <- which(old.label[i] == names.mod)
    names.mod[indices] <- new.label[i]
  }
  
  for (i in 1:length(old.label)){
    indices   <- which(old.label[i] == names.sup)
    names.sup[indices] <- new.label[i]
  }
  
  for (i in 1:length(old.label)){
    indices   <- which(old.label[i] == names.ind)
    names.ind[indices] <- new.label[i]
  }
  
  
  object$names.mod <- names.mod
  object$names.sup <- names.sup
  object$names.ind <- names.ind
  return(object)

}
### Plot for all modalitites
#' Map all modalities
#' 
#' Creates a map of all active and supplementary modalities on two selected 
#' dimension.
#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param map.title the title of the map. If set to its default the standard 
#'   title is used.
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. If set
#'   to its default, the default scale is used. It may be mapped to a variable 
#'   with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. If set 
#'   to its default, the size is determined by the frequency of each modality. 
#'   It may be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and link{guide_legend} functions from the
#'   ggplot2 package.
#' @examples
#' example(soc.ca)
#' map.mod(result)
#' map.mod(result, dim = c(3, 2), point.size = 2)
#' @export
map.mod         <- function(object, dim = c(1, 2),
                            point.shape = "variable", point.alpha = 0.8,
                            point.fill = "whitesmoke", point.color = "black", point.size = "freq",
                            label = TRUE, label.repel = FALSE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                            map.title = "mod", labelx = "default", labely = "default", legend = NULL){
  
  plot.type   <- "mod"

  plot.flow(object,
            dim         = dim,
            point.shape = point.shape,
            point.alpha = point.alpha,
            point.fill  = point.fill,
            point.color = point.color,
            point.size  = point.size,
            label       = label,
            label.repel = label.repel,
            label.alpha = label.alpha,
            label.color = label.color, 
            label.size  = label.size,
            label.fill  = label.fill,
            map.title   = map.title,
            labelx      = labelx,
            labely      = labely,
            legend      = legend,
            plot.type   = plot.type)
}

#' Map the most contributing modalities
#' 
#' Creates a map of the modalities contributing above average to one or more 
#' dimensions on two selected dimension.
#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param ctr.dim the dimensions of the contribution values
#' @param map.title the title of the map. If set to its default the standard 
#'   title is used.
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. If set
#'   to its default, the default scale is used. It may be mapped to a variable 
#'   with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. If set 
#'   to its default, the size is determined by the frequency of each modality. 
#'   It may be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.   
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and link{guide_legend} functions from the
#'   ggplot2 package.
#' @examples
#' example(soc.ca)
#' map.ctr(result)
#' map.ctr(result, ctr.dim = c(1, 2))
#' @export

map.ctr         <- function(object, dim = c(1, 2), ctr.dim = 1,
                            point.shape = "variable", point.alpha = 0.8, point.fill = "whitesmoke",
                            point.color = "black", point.size = "freq",
                            label = TRUE, label.repel = TRUE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                            map.title = "ctr", labelx = "default", labely = "default", legend = NULL){
  
  plot.type   <- "ctr"
  
  
  plot.flow(object,
            dim         = dim,
            ctr.dim     = ctr.dim,
            point.shape = point.shape,
            point.alpha = point.alpha,
            point.fill  = point.fill,
            point.color = point.color,
            point.size  = point.size,
            label       = label,
            label.repel = label.repel,
            label.alpha = label.alpha,
            label.color = label.color, 
            label.size  = label.size,
            label.fill  = label.fill,
            map.title   = map.title,
            labelx      = labelx,
            labely      = labely,
            legend      = legend,
            plot.type   = plot.type)
}

#' Map the active modalities
#' 
#' Creates a map of the active modalities on two selected dimensions.
#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param map.title the title of the map. If set to its default the standard 
#'   title is used.
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. If set
#'   to its default, the default scale is used. It may be mapped to a variable 
#'   with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. If set 
#'   to its default, the size is determined by the frequency of each modality. 
#'   It may be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and link{guide_legend} functions from the
#'   ggplot2 package.
#' @examples
#' example(soc.ca)
#' map.active(result)
#' map.active(result, dim = c(2, 1))
#' map.active(result, point.size = result$ctr.mod[, 1],
#'  map.title = "All active modalities with size according to contribution")
#' @export

map.active         <- function(object, dim = c(1, 2),
                               point.shape = "variable", point.alpha = 0.8, point.fill = "whitesmoke",
                               point.color = "black", point.size = "freq",
                               label = TRUE, label.repel = FALSE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                               map.title = "active", labelx = "default", labely = "default", legend = NULL){
  
  plot.type   <- "active"
  
  plot.flow(object,
            dim         = dim,
            point.shape = point.shape,
            point.alpha = point.alpha,
            point.fill  = point.fill,
            point.color = point.color,
            point.size  = point.size,
            label       = label,
            label.repel = label.repel,
            label.alpha = label.alpha,
            label.color = label.color, 
            label.size  = label.size,
            label.fill  = label.fill,
            map.title   = map.title,
            labelx      = labelx,
            labely      = labely,
            legend      = legend,
            plot.type   = plot.type)
}


#' Map the supplementary modalities
#' 
#' Creates a map of the supplementary modalities on two selected dimension.
#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param map.title the title of the map. If set to its default the standard 
#'   title is used.
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. If set
#'   to its default, the default scale is used. It may be mapped to a variable 
#'   with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. If set 
#'   to its default, the size is determined by the frequency of each modality. 
#'   It may be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and link{guide_legend} functions from the
#'   ggplot2 package.
#' @examples
#' example(soc.ca)
#' map.sup(result)
#' map.sup(result, dim = c(2, 1))
#' map.sup(result, point.size = result$coord.sup[, 4],
#'  map.title = "All supplementary modalities with size according to coordinate on the 4th dimension")
#' @export

map.sup         <- function(object, dim = c(1, 2),
                            point.shape = "variable", point.alpha = 0.8, point.fill = "whitesmoke",
                            point.color = "black", point.size = "freq",
                            label = TRUE, label.repel = TRUE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                            map.title = "sup", labelx = "default", labely = "default", legend = NULL){
  
  plot.type     <- "sup"
    
  plot.flow(object,
            dim         = dim,
            point.shape = point.shape,
            point.alpha = point.alpha,
            point.fill  = point.fill,
            point.color = point.color,
            point.size  = point.size,
            label       = label,
            label.repel = label.repel,
            label.alpha = label.alpha,
            label.color = label.color, 
            label.size  = label.size,
            label.fill  = label.fill,
            map.title   = map.title,
            labelx      = labelx,
            labely      = labely,
            legend      = legend,
            plot.type   = plot.type)
}

#' Map the individuals of a soc.ca analysis
#' 
#' Creates a map of the individuals on two selected dimension.
#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param map.title the title of the map. If set to its default the standard 
#'   title is used.
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. It may
#'   be mapped to a variable with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. It may
#'   be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and link{guide_legend} functions from the
#'   ggplot2 package.
#' @examples
#' example(soc.ca)
#' map.ind(result)
#' map.ind(result, map.title = "Each individual is given its shape according to a value in a factor",
#'  point.shape = active[, 1], legend = TRUE)
#' map  <- map.ind(result, map.title = "The contribution of the individuals with new scale",
#'  point.color = result$ctr.ind[, 1], point.shape = 18) 
#' map + scale_color_continuous(low = "white", high = "red")
#' quad   <- create.quadrant(result)
#' map.ind(result, map.title = "Individuals in the space given shape and color by their quadrant",
#'  point.shape = quad, point.color = quad)
#' @export

map.ind         <- function(object, dim = c(1, 2),
                            point.shape = 21, point.alpha = 0.8, point.fill = "whitesmoke",
                            point.color = "black", point.size = 3,
                            label = FALSE, label.repel = FALSE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                            map.title = "ind", labelx = "default", labely = "default", legend = NULL){
  
  plot.type   <- "ind"
  
  plot.flow(object,
            dim         = dim,
            point.shape = point.shape,
            point.alpha = point.alpha,
            point.fill  = point.fill,
            point.color = point.color,
            point.size  = point.size,
            label       = label,
            label.repel = label.repel,
            label.alpha = label.alpha,
            label.color = label.color, 
            label.size  = label.size,
            label.fill  = label.fill,
            map.title   = map.title,
            labelx      = labelx,
            labely      = labely,
            legend      = legend,
            plot.type   = plot.type)
}

#' Map select modalities and individuals
#' 
#' Creates a map of selected modalities or individuals
#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param list.mod a numerical vector indicating which active modalities to 
#'   plot. It may also be a logical vector of the same length and order as the 
#'   modalities in object$names.mod.
#' @param list.sup a numerical vector indicating which supplementary modalities 
#'   to plot. It may also be a logical vector of the same length and order as 
#'   the modalities in object$names.sup.
#' @param list.ind a numerical vector indicating which individuals to plot. It 
#'   may also be a logical vector of the same length and order as the modalities
#'   in object$names.ind.
#' @param ctr.dim the dimensions of the contribution values
#' @param map.title the title of the map. If set to its default the standard 
#'   title is used.
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. If set
#'   to its default, the default scale is used. It may be mapped to a variable 
#'   with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. If set 
#'   to its default, the size is determined by the frequency of each modality. 
#'   It may be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and \link{guide_legend} functions from the
#'   ggplot2 package.
#' @param ... further arguments are currently ignored.
#' @examples
#' example(soc.ca)
#' map.select(result, map.title = "Map of the first ten modalities", list.mod = 1:10)
#' select   <- active[, 3]
#' select   <- select == levels(select)[2]
#' map.select(result, map.title = "Map of all individuals sharing a particular value",
#'  list.ind = select, point.size = 3)
#' map.select(result, map.title = "Map of both select individuals and modalities",
#'  list.ind = select, list.mod = 1:10)
#' @export

map.select         <- function(object, dim = c(1, 2), ctr.dim = 1,
                               list.mod = NULL, list.sup = NULL, list.ind = NULL,
                               point.shape = "variable", point.alpha = 0.8, point.fill = "whitesmoke",
                               point.color = "black", point.size = "freq",
                               label = TRUE, label.repel = FALSE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                               map.title = "select", labelx = "default", labely = "default", legend = NULL, ...){
  
  modal.list  <- list(list.mod = list.mod, list.sup = list.sup, list.ind = list.ind)
  
  plot.type   <- "select"
  
  plot.flow(object,
            dim         = dim,
            ctr.dim     = ctr.dim,
            modal.list = modal.list,
            point.shape = point.shape,
            point.alpha = point.alpha,
            point.fill  = point.fill,
            point.color = point.color,
            point.size  = point.size,
            label       = label,
            label.repel = label.repel,
            label.alpha = label.alpha,
            label.color = label.color, 
            label.size  = label.size,
            label.fill  = label.fill,
            map.title   = map.title,
            labelx      = labelx,
            labely      = labely,
            legend      = legend,
            plot.type   = plot.type)
}


#' Add points to an existing map created by one of the soc.ca mapping functions.

#' @param object a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param ca.map a map created using one of the soc.ca map functions
#' @param plot.type defines which type of points to add to the map. Accepted 
#'   values are: "mod", "sup", "ind", "ctr". These values correspond to the 
#'   different forms of
#' @param list.mod a numerical vector indicating which active modalities to 
#'   plot. It may also be a logical vector of the same length and order as the 
#'   modalities in object$names.mod.
#' @param list.sup a numerical vector indicating which supplementary modalities 
#'   to plot. It may also be a logical vector of the same length and order as 
#'   the modalities in object$names.sup.
#' @param list.ind a numerical vector indicating which individuals to plot. It 
#'   may also be a logical vector of the same length and order as the modalities
#'   in object$names.ind.
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param ctr.dim the dimensions of the contribution values
#' @param labelx the label of the horizontal axis. If set to NULL a standard 
#'   label is used.
#' @param labely the label of the vertical axis. If set to NULL a standard label
#'   is used.
#' @param point.shape a numerical value defining the shape of the points. If set
#'   to its default, the default scale is used. It may be mapped to a variable 
#'   with a suitable length and order.
#' @param point.alpha defines the alpha of the points. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param point.fill defines the fill color of the points. It may be mapped to a
#'   variable with a suitable length and order.
#' @param point.color defines the color of the points. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param point.size a numerical value defining the size of the points. If set 
#'   to its default, the size is determined by the frequency of each modality. 
#'   It may be defined by a variable with a suitable length.
#' @param label if TRUE each point is assigned its label, defined in the soc.ca 
#'   object. See \link{assign.label} and \link{add.to.label} for ways to alter 
#'   the labels.
#' @param label.repel if TRUE overlapping labels are rearranged, see \link{geom_text_repel} or \link{geom_label_repel}.
#' @param label.alpha defines the alpha of the labels. Values range from 0 to 1.
#'   It may be mapped to a variable with a suitable length and order.
#' @param label.color defines the color of the labels. It may be mapped to a
#'   variable with a suitable length and order. See \link{colors} for some of
#'   the valid values.
#' @param label.size defines the size of the labels. It may be mapped to a
#'   variable with a suitable length and order.
#' @param label.fill defines the color of the box behind the labels. It may be mapped to a
#'   variable with a suitable length and order. This only works if label.repel is TRUE. See \link{geom_label_repel}.
#' @param legend if set to TRUE a legend is provided. Change the legend with the
#'   \link{guides}, \link{theme} and link{guide_legend} functions from the
#'   ggplot2 package.
#' @examples
#' example(soc.ca)
#' original.map    <- map.sup(result)
#' map.add(result, original.map, plot.type = "ctr", ctr.dim = 2)
#' map.add(result, map.ind(result), plot.type = "select",list.ind = 1:50,
#'  point.color = "red", label = FALSE, point.size = result$ctr.ind[1:50, 1]*2000)
#' @export

map.add         <- function(object, ca.map, plot.type = NULL,
                            ctr.dim = 1, list.mod = NULL, list.sup = NULL, list.ind = NULL,
                            point.shape = "variable", point.alpha = 0.8, point.fill = "whitesmoke",
                            point.color = "black", point.size = "freq",
                            label = TRUE, label.repel = TRUE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                            labelx = "default", labely = "default", legend = NULL){
  p           <- ca.map
  dim         <- ca.map$dimensions
  org.scales  <- ca.map$ca.scales
  modal.list  <- list(list.mod = list.mod, list.sup = list.sup, list.ind = list.ind)
  
  gg.data     <- data.plot(object,
                           plot.type      = plot.type,
                           dim            = dim,
                           ctr.dim        = ctr.dim,
                           modal.list     = modal.list,
                           point.size     = point.size,
                           point.variable = point.shape)
  
  if(identical(point.shape,"variable")) point.shape <- gg.data$variable
  if(identical(point.size,"freq"))      point.size  <- gg.data$freq
  
  point.l            <- list(x    = gg.data$x,
                            y     = gg.data$y,
                            shape = point.shape,
                            alpha = point.alpha,
                            fill  = point.fill,
                            color = point.color,
                            size  = point.size)
  
  p.i                <- unlist(lapply(point.l, length)) == 1
  point.attributes   <- point.l[p.i == TRUE]
  point.aes          <- point.l[p.i == FALSE]
  
  label.l            <- list(x     = gg.data$x,
                             y     = gg.data$y,
                             label = gg.data$names,
                             alpha = label.alpha,
                             color = label.color,
                             fill  = label.fill,
                             size  = label.size)
  
  t.i                <- unlist(lapply(label.l, length)) == 1
  label.attributes   <- label.l[t.i == TRUE]
  label.aes          <- label.l[t.i == FALSE]
  
  gg.input           <- list(gg.data     = gg.data,
                             label       = label,
                             repel         = label.repel,
                             point.aes   = point.aes,
                             point.attributes = point.attributes,
                             label.aes    = label.aes,
                             label.attributes = label.attributes)
  
  # Plot
  # Points
  point.attributes             <- gg.input$point.attributes
  point.attributes$mapping     <- do.call("aes", gg.input$point.aes)
  p                            <- p + do.call("geom_point", point.attributes, quote = TRUE)
  shapes                       <- c(21, 22, 23, 24, 25, 6, 0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 12, 15, 16, 17, 18,
                                    42, 45, 61, 48, 50:120)   
  p                            <- p + scale_shape_manual(values = shapes)   
  
  # label
  if (gg.input$label == TRUE){
    label.attributes             <- gg.input$label.attributes
    label.attributes$vjust       <- 1.8
    label.attributes$family      <- "sans"
    label.attributes$lineheight  <- 0.9
    label.attributes$mapping     <- do.call("aes", gg.input$label.aes)
    
    if(is.null(gg.input$label.aes$fill) & gg.input$repel == TRUE & is.null(label.attributes$fill)){
      label.attributes$vjust       <- NULL
      p                            <- p + do.call("geom_text_repel", label.attributes, quote = TRUE)
    }
    
    if(is.null(gg.input$label.aes$fill) == FALSE | is.null(label.attributes$fill) == FALSE){
      label.attributes$vjust       <- NULL
      p                            <- p + do.call("geom_label_repel", label.attributes, quote = TRUE)
    }
    
    if(is.null(gg.input$label.aes$fill) & gg.input$repel == FALSE){
      p                            <- p + do.call("geom_text", label.attributes, quote = TRUE)
    }
    
  }
  
  # Scales and labels
  
  org.max     <- org.scales$lim.max
  org.min     <- org.scales$lim.min
  n.max       <- max(c(max(gg.data[,1:2]), org.max))
  n.min       <- (c(min(gg.data[,1:2]), org.min))
  tscales     <- gg.data
  tscales[1, 1:2] <- n.max
  tscales[2, 1:2] <- n.min
  scales      <- breaksandscales(tscales)
  
  p           <- p + scale_x_continuous(breaks = scales$scalebreaks, labels = scales$breaklabel)
  p           <- p + scale_y_continuous(breaks = scales$scalebreaks, labels = scales$breaklabel)
  p$ca.scales <- scales
  
  #### Legend
  if(is.null(legend))            p  <- p + theme(legend.position = "none")
  if(identical(legend,"bottom")) p  <- p + theme(legend.position = 'bottom',
                                                 legend.direction = "horizontal", legend.box = "horizontal")
  p
}


#' Density plot for the cloud of individuals
#' 
#' Draws a 2d density plot on top of an existing soc.ca map. The density is 
#' calculated by the \link[MASS]{kde2d} function from MASS and plotted by
#' \link[ggplot2]{geom_density2d} from \code{ggplot2} \code{map.density} uses the
#' coordinates of the individuals as a basis for the density calculation. 
#' Borders are arbitrary.
#' 
#' @param object a soc.ca class object
#' @param map a soc.ca map object created by one of the soc.ca mapping functions
#' @param group a factor determining group membership. Density is mapped for
#'   each group individually.
#' @param color a single value or vector determining the color. See the scale
#'   functions of \code{ggplot2} for ways to alter the scales.
#' @param alpha a single value or vector determining the alpha.
#' @param size a single value or vector determining the size of the lines.
#' @param linetype a single value or vector determining the linetype
#' @export
#' @examples
#' example(soc.ca)
#' map.density(result, map.ind(result, dim = 2:3, point.alpha = 0.2))
#' map.density(result, map.ind(result, legend = TRUE, point.alpha = 0.2),
#'  group = duplicated(active), color = duplicated(active),
#'  linetype = duplicated(active))
#' map.density(result, map.ctr(result))

map.density  <- function(object, map = map.ind(object), group = NULL,
                         color = "red", alpha = 0.8, size = 0.5, linetype = "solid"){
  
  dim                        <- map$dimensions 
  dens.data                  <- as.data.frame(object$coord.ind[, dim])
  colnames(dens.data)        <- c("x", "y")
  
  density.l                  <- list(color = color,
                                     alpha = alpha,
                                     size  = size,
                                     linetype = linetype,
                                     n  = 100,
                                     group = group,
                                     na.rm = TRUE)
  
  d.i                        <- unlist(lapply(density.l, length)) == 1
  density.attributes         <- density.l[d.i]
  density.aes                <- density.l[unlist(lapply(density.l, length)) > 1]
  density.aes$x              <- dens.data$x
  density.aes$y              <- dens.data$y
  
  
  density.attributes$mapping <- do.call("aes", density.aes)
  p     <- map + do.call("geom_density2d", density.attributes, quote = TRUE)
  p
}





############################## Plot delfunktioner ########################################

plot.flow   <- function(object, dim = c(1, 2), ctr.dim = NULL, modal.list = NULL,
                        point.shape = 21, point.alpha = 0.8, point.fill = "whitesmoke",
                        point.color = "black", point.size = 3,
                        label = FALSE, label.repel = FALSE, label.alpha = 0.8, label.color = "black", label.size = 4, label.fill = NULL,
                        map.title = map.title, labelx = "default", labely = "default", legend = NULL,
                        plot.type = plot.type){
  
  
  gg.proc     <- round(object$adj.inertia[,4])                        # Adjusted inertia
  gg.data     <- data.plot(object,
                           plot.type   = plot.type,
                           dim,
                           ctr.dim     = ctr.dim,
                           modal.list  = modal.list,
                           point.size  = point.size,
                           point.color = point.color)                 # Data selection
  
  axis.labels <- plot.axis(labelx = labelx, labely = labely, gg.proc = gg.proc, dim = dim) # Axis labels
  map.title   <- plot.title(map.title = map.title, ctr.dim = ctr.dim) # Plot title
  scales      <- breaksandscales(gg.data)                             # Scales og breaks
  
  if (identical(point.shape,"variable")) point.shape <- gg.data$variable
  if (identical(point.size,"freq"))      point.size  <- gg.data$freq
  
  point.l            <- list(x    = gg.data$x,
                            y     = gg.data$y,
                            shape = point.shape,
                            alpha = point.alpha,
                            fill  = point.fill,
                            color = point.color,
                            size  = point.size)
  
  p.i                <- unlist(lapply(point.l, length)) == 1
  point.attributes   <- point.l[p.i == TRUE]
  point.aes          <- point.l[p.i == FALSE]
  
  label.l            <- list(x     = gg.data$x,
                             y     = gg.data$y,
                             label = gg.data$names,
                             alpha = label.alpha,
                             color = label.color,
                             fill  = label.fill,
                             size  = label.size)
  
  t.i                <- unlist(lapply(label.l, length)) == 1
  label.attributes   <- label.l[t.i == TRUE]
  label.aes          <- label.l[t.i == FALSE]
  
  gg.input           <- list(gg.data       = gg.data,
                             axis.inertia  = gg.proc,
                             map.title     = map.title,
                             labelx        = axis.labels$x,
                             labely        = axis.labels$y,
                             scales        = scales,
                             label         = label,
                             repel         = label.repel,
                             point.aes     = point.aes,
                             point.attributes = point.attributes,
                             label.aes     = label.aes,
                             label.attributes = label.attributes)
  
  b.plot              <- basic.plot(gg.input) 
  t.plot              <- b.plot + theme_min() 
  
  if(is.null(legend)) t.plot   <- t.plot + theme(legend.position = "none")
  if(identical(legend,"bottom")) t.plot  <- t.plot + theme(legend.position = 'bottom',
                                                           legend.direction = "horizontal", legend.box = "horizontal")
  t.plot$dimensions <- dim
  return(t.plot)
}

#################################################################################
# Soc.ca.graphics environment

# soc.ca.scales                  <- function(...){
# soc.ca.graphics                <- new.env()
# assign("scale_colour_continuous", function(...) ggplot2::scale_colour_continuous(..., low = getOption("soc.ca.gradient")[1], high = getOption("soc.ca.gradient")[2]), envir = soc.ca.graphics)
# assign("scale_color_continuous",  function(...) ggplot2::scale_colour_continuous(..., low = getOption("soc.ca.gradient")[1], high = getOption("soc.ca.gradient")[2]), envir = soc.ca.graphics)
# assign("scale_fill_continuous",   function(...) ggplot2::scale_fill_continuous(...,   low = getOption("soc.ca.gradient")[1], high = getOption("soc.ca.gradient")[2]), envir = soc.ca.graphics)
# 
# assign("scale_colour_gradient",   function(...) ggplot2::scale_colour_continuous(..., low = getOption("soc.ca.gradient")[1], high = getOption("soc.ca.gradient")[2]), envir = soc.ca.graphics)
# assign("scale_color_gradient",    function(...) ggplot2::scale_colour_continuous(..., low = getOption("soc.ca.gradient")[1], high = getOption("soc.ca.gradient")[2]), envir = soc.ca.graphics)
# assign("scale_fill_gradient",     function(...) ggplot2::scale_fill_continuous(...,   low = getOption("soc.ca.gradient")[1], high = getOption("soc.ca.gradient")[2]), envir = soc.ca.graphics)
# 
# assign("scale_colour_discrete",   function(...) ggplot2::scale_colour_manual(..., values = getOption("soc.ca.colors")), envir = soc.ca.graphics)
# assign("scale_color_discrete",    function(...) ggplot2::scale_colour_manual(..., values = getOption("soc.ca.colors")), envir = soc.ca.graphics)
# assign("scale_fill_discrete",     function(...) ggplot2::scale_fill_manual(...,   values = getOption("soc.ca.colors")), envir = soc.ca.graphics)
# if(("soc.ca.graphics" %in% search()) == FALSE) attach(soc.ca.graphics)
# 
# 
# }

basic.plot <- function(gg.input){ 
  
  #####################
  # Changing default ggplot2 behaviour
 # soc.ca.scales() # When loading the soc.ca.graphics environment, certain functions are masked from ggplot2. This is intended.
  # Her skal der perhaps en on.exit
  # Defining point.size
  p       <- ggplot()   
  # The middle plot axis
  p       <- p + geom_hline(yintercept = 0, color = "grey50", size = 0.5, linetype = "solid")
  p       <- p + geom_vline(xintercept = 0, color = "grey50", size = 0.5, linetype = "solid")
  
  # Points
  point.attributes             <- gg.input$point.attributes
  point.attributes$mapping     <- do.call("aes", gg.input$point.aes)
  p                            <- p + do.call("geom_point", point.attributes, quote = TRUE)
  shapes                       <- getOption("soc.ca.shape")  
  p                            <- p + scale_shape_manual(values = shapes)   
  
  # label
  if (gg.input$label == TRUE){
    label.attributes             <- gg.input$label.attributes
    label.attributes$vjust       <- 1.8
    label.attributes$family      <- "sans"
    label.attributes$lineheight  <- 0.9
    label.attributes$mapping     <- do.call("aes", gg.input$label.aes)
    
    if(is.null(gg.input$label.aes$fill) & gg.input$repel == TRUE & is.null(label.attributes$fill)){
    label.attributes$vjust       <- NULL
    label.attributes$max.iter    <- 2000
    p                            <- p + do.call("geom_text_repel", label.attributes, quote = TRUE)
    }
    
    if(is.null(gg.input$label.aes$fill) == FALSE | is.null(label.attributes$fill) == FALSE){
      label.attributes$vjust       <- NULL
      label.attributes$max.iter    <- 2000
      p                            <- p + do.call("geom_label_repel", label.attributes, quote = TRUE)
    }
    
    if(is.null(gg.input$label.aes$fill) & gg.input$repel == FALSE){
      p                            <- p + do.call("geom_text", label.attributes, quote = TRUE)
    }
    
  }
  
  # Title and axis labels
  p     <- p + ggtitle(label = gg.input$map.title)
  p     <- p + xlab(gg.input$labelx) + ylab(gg.input$labely)
  p     <- p + labs(alpha = "Alpha", shape = "Shape",
                    color = "Color", linetype = "Linetype", size = "Size", fill = "Fill")
  # Scales and breaks
  p     <- p + scale_x_continuous(breaks = gg.input$scales$scalebreaks, labels = gg.input$scales$breaklabel)
  p 		<- p + scale_y_continuous(breaks = gg.input$scales$scalebreaks, labels = gg.input$scales$breaklabel)
  p     <- p + coord_fixed()
  p$ca.scales <- gg.input$scales
  
 # detach(soc.ca.graphics) # Detaches the environment containing the soc.ca version of the scales, see(soc.ca.scales)
  
  p
}




####################### Plot title

plot.title  <- function(map.title = NULL, ctr.dim = NULL){
    # Map title for ctr plot    
    if (identical(map.title, "ctr") == TRUE) {
        map.title     <- paste("The modalities contributing above average to dimension ",
                               paste(ctr.dim, collapse = " & "), sep = "")
    }
    # Map title for all plot
    if (identical(map.title, "mod") == TRUE) {
        map.title     <- "Map of all modalities"
    }
    # Map title for active plot
    if (identical(map.title, "active") == TRUE) {
        map.title     <- "Map of active modalities"
    }
    # Map title for sup plot
    if (identical(map.title, "sup") == TRUE) {
        map.title     <- "Map of supplementary modalities"
    }
    # Map title for id plot
    if (identical(map.title, "ind") == TRUE) {
        map.title     <- "Map of individuals"
    }
    # Map title for list plot
    if (identical(map.title, "select") == TRUE) {
        map.title     <- "Map of selected modalities"
    }
    return(map.title)
}

############# Axis labels
plot.axis       <- function(labelx = "default", labely = "default", gg.proc = NA, dim = NULL){
    if (identical(labelx, "default") == TRUE) {
labelx         <- paste(dim[1], ". Dimension: ", gg.proc[dim[1]], "%", sep = "")
}
    if (identical(labely, "default") == TRUE) {
labely         <- paste(dim[2], ". Dimension: ", gg.proc[dim[2]], "%", sep = "")
}
axis.labels <- list(x = labelx, y = labely)
return(axis.labels)    
}

####################### breaksandscales
breaksandscales <- function(gg.data){
    mround <- function(x, base){
    base*round(x/base)
  }
  
  lim.min.x   	<- min(gg.data[, 1])
  lim.min.y   	<- min(gg.data[, 2])
  lim.max.x   	<- max(gg.data[, 1])
  lim.max.y   	<- max(gg.data[, 2])
  
  scalebreaks   <- seq(-10,10, by = 0.25)
  nolabel       <- seq(-10, 10, by = 0.5)
  inter         <- intersect(scalebreaks, nolabel)
  truelabel     <- is.element(scalebreaks, nolabel)
  breaklabel    <- scalebreaks  
  breaklabel[truelabel == FALSE] <-  ""
    
  length.x      <- sqrt(lim.min.x^2) + sqrt(lim.max.x^2)
  length.y      <- sqrt(lim.min.y^2) + sqrt(lim.max.y^2)
  
  scales        <- list(scalebreaks = scalebreaks,
                        breaklabel  = breaklabel,
                        lim.min.x   = lim.min.x,
                        lim.min.y   = lim.min.y,
                        lim.max.x   = lim.max.x,
                        lim.max.y   = lim.max.y,
                        length.x    = length.x,
                        length.y    = length.y)
  return(scales)
}

#################### data.plot v.2

data.plot   <- function(object, plot.type, dim, ctr.dim = NULL,
                        modal.list = NULL, point.size = "freq", point.variable = NULL, point.color = NULL){
  # Types: mod, active, sup, ctr, ind, list
  
  # mod
  if (identical(plot.type, "mod") == TRUE){
    coord       <- rbind(object$coord.mod, object$coord.sup)
    mnames  	  <- c(object$names.mod, object$names.sup)
    freq        <- c(object$freq.mod, object$freq.sup)
    variable    <- c(object$variable, object$variable.sup)
  }
  
  # ctr
  if (identical(plot.type, "ctr") == TRUE){
    av.ctr      <- contribution(object, ctr.dim, indices = TRUE, mode = "mod")
    coord     	<- object$coord.mod[av.ctr, ]
    mnames		  <- object$names.mod[av.ctr]
    freq        <- object$freq.mod[av.ctr]
    variable    <- object$variable[av.ctr]
  }
  # active
  if (identical(plot.type, "active") == TRUE){
    coord 		  <- object$coord.mod
    mnames		  <- object$names.mod
    freq        <- object$freq.mod
    variable    <- object$variable
  }
  # sup
  if (identical(plot.type, "sup") == TRUE){
    coord 		<- object$coord.sup
    mnames		<- object$names.sup
    freq      <- object$freq.sup
    variable  <- object$variable.sup
  }
  # ind
  if (identical(plot.type, "ind") == TRUE){
    coord 		<- object$coord.ind
    mnames		<- object$names.ind
    freq      <- rep(1, object$n.ind)
     if(identical(point.variable, NULL)){
       variable  <- rep("ind", nrow(object$coord.ind))
     }else{ 
       variable  <- point.variable
    }
    if(identical(point.color, NULL) == FALSE)
    point.color  <- point.color
  }
  
  # select
  if (identical(plot.type, "select") == TRUE){
    coord.mod 		<- object$coord.mod[modal.list$list.mod, ]
    coord.sup     <- object$coord.sup[modal.list$list.sup, ]
    coord.ind     <- object$coord.ind[modal.list$list.ind, ]
    names.mod		  <- object$names.mod[modal.list$list.mod]
    names.sup  	  <- object$names.sup[modal.list$list.sup]
    names.ind     <- object$names.ind[modal.list$list.ind]
    coord         <- rbind(coord.mod, coord.sup, coord.ind)
    rownames(coord) <- NULL
    mnames        <- c(names.mod, names.sup, names.ind)
    freq.mod      <- object$freq.mod[modal.list$list.mod]
    freq.sup      <- object$freq.sup[modal.list$list.sup]
    freq.ind      <- rep(1, object$n.ind)[modal.list$list.ind]
    freq          <- c(freq.mod, freq.sup, freq.ind)
    variable.mod  <- object$variable[modal.list$list.mod]
    variable.sup  <- object$variable.sup[modal.list$list.sup]
    variable.ind  <- rep("ind", object$n.ind)[modal.list$list.ind]
    variable      <- c(variable.mod, variable.sup, variable.ind)
  }
  
  if(is.numeric(point.size))  freq         <- rep(point.size, length.out = nrow(coord))
  if(is.null(point.color))    point.color  <- rep("Nothing", length.out = nrow(coord))
  
  
  gg.data             <- data.frame(cbind(coord[, dim[1]], coord[,dim[2]]), mnames, freq, variable, point.color)
  colnames(gg.data)   <- c("x", "y", "names", "freq", "variable", "point.color")
  return(gg.data)
}

#' Add a new layer of points on top of an existing plot with output from the 
#' min_cut function
#' @param x a matrix created by the min_cut function
#' @param p is a ggplot object, preferably from one of the mapping functions in 
#'   soc.ca
#' @param label if TRUE the labels of points will be shown
#' @param ... further arguments are passed on to geom_path, geom_point and 
#'   geom_text

add.count <- function(x, p, label = TRUE, ...){
  p   <- p + geom_point(data = x, x = x$X, y = x$Y, ...) + geom_path(data = x, x = x$X, y = x$Y, ...)
  if (identical(label, TRUE)) p <- p + geom_text(data = x, x = x$X, y = x$Y, label = x$label, vjust = 0.2, ...)
}

#' Map path along an ordered variable
#' 
#' Plot a path along an ordered variable. If the variable is numerical it is cut
#' into groups by the \link{min_cut} function.
#' 
#' @param object is a soc.ca result object
#' @param x is an ordered vector, either numerical or factor
#' @param map is a plot object created with one of the mapping functions in the 
#'   soc.ca package
#' @param dim the dimensions in the order they are to be plotted. The first 
#'   number defines the horizontal axis and the second number defines the 
#'   vertical axis.
#' @param label if TRUE the label of the points are shown
#' @param min.size is the minimum size given to the groups of a numerical 
#'   variable, see \link{min_cut}.
#' @param ... further arguments are passed onto \link{geom_path},
#'   \link{geom_point} and \link{geom_text} from the ggplot2 package
#' @export
#' @examples
#' example(soc.ca)
#' map <- map.ind(result, point.color = as.numeric(sup$Age))
#' map <- map + scale_color_continuous(high = "red", low = "yellow")
#' map.path(result, sup$Age, map)

map.path  <- function(object, x, map = map.ind(object, dim), dim = c(1, 2),
                          label = TRUE, min.size = length(x)/10, ...){
  
  x.c     <- x
  if (is.numeric(x)) x.c     <- min_cut(x, min.size = min.size) 
  
  x.av    <- average.coord(object = object, x = x.c, dim = dim) 
  x.av["X"] <- x.av["X"] * sqrt(object$eigen[dim[1]])
  x.av["Y"] <- x.av["Y"] * sqrt(object$eigen[dim[2]])
  map.p   <- add.count(x.av, map, label, ...) 
  map.p
}



map.top.ind <- function(result, ctr.dim = 1, dim = c(1,2), top = 15){
  sc            <- list()
  sc$fill       <- scale_fill_continuous(low = "white", high = "darkblue")
  sc$alpha      <- scale_alpha_manual(values = c(0.5, 1))
  
  
  ctr           <- result$ctr.ind[, ctr.dim]
  above.average <- ctr >= mean(ctr)
  important     <- rank(ctr) > length(ctr) - top
  r             <- result
  r$names.ind[-which(important)] <- NA
  #r             <- add.to.label(r, value = "linebreak")
  m             <- map.ind(r, point.alpha = above.average, label = TRUE, point.shape = 21, point.fill = ctr, label.repel = TRUE, dim = dim, label.size = 3)
  m + sc + ggtitle(paste("Map of the", top, "most contributing individuals for dim.", ctr.dim))
}
#' Explore the cloud of individuals
#'
#' @param object a a soc.ca class object as created by \link{soc.mca} and 
#'   \link{soc.csa}
#' @param active Defines the active modalities in a data.frame with rows of individuals and columns of factors, without NA's' 
#' @param sup Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
#'
#' @return an html application
#' @export
#'
#' @examples
#' \dontrun{
#' example(soc.mca)
#' ind.explorer(result, active, sup)
#' }

ind.explorer     <- function(object, active, sup = NULL){
  
  # -----------
  # Creating data
  
  data              <- data.frame("Dimension" = object$coord.ind, active)
  indicator.matrix  <- object$indicator.matrix
  if(!is.null(sup)) indicator.matrix  <- cbind(indicator.matrix, indicator(sup))
  if(!is.null(sup)) data              <- cbind(data, sup)
  rownames(data)    <- object$names.ind
  
  # ------------
  # UI 
  
  ui  <- shinyUI(
    fluidPage(
      titlePanel(
        "Explore"
        ), 
      fluidRow(
        column(3, selectInput(inputId = "dim.plot.x", label = "Dim X-axis", choices = 1:object$nd, selected = 1, width = 100),
                  selectInput(inputId = "dim.plot.y", label = "Dim Y-axis", choices = 1:object$nd, selected = 2, width = 100),
                  selectInput("fill", "Fill", choices = colnames(indicator.matrix), width = 150),
                  checkboxInput(inputId = "ellipse", "Draw ellipse", value = FALSE),
                  checkboxInput(inputId = "density", "Draw density", value = FALSE),
                  checkboxInput(inputId = "labels", "Modality labels", value = TRUE)
               ),
        column(8, plotOutput("map.ind", click = "plot_click"))
        
      ),
      fluidRow(
        column(3),
        column(8, plotOutput("map.mod", height = 600))
      ),
      fluidRow(
        column(3),
        column(8, tableOutput("info"))
        )
  )
  )
  
  # ---------------
  # Server
  server <- shinyServer(function(input, output) {
    
    # Click
    output$map.ind <- renderPlot({
      
      dim.plot       <- c(as.numeric(input$dim.plot.x), as.numeric(input$dim.plot.y))
      fill.ind       <- subset(indicator.matrix, select = input$fill)
      fill.var       <- factor(fill.ind, levels = c(NA, 1), labels = c(input$fill))
      
     # if(input$csa) object         <- soc.csa(object, class.indicator = which(fill.ind == 1), sup = sup)
      
      p <- map.ind(object, dim = dim.plot, point.fill = fill.var) + coord_cartesian()
      p <- p + scale_fill_manual(values = c("black", "white"), na.value = "white")
      if(input$ellipse == TRUE) p <- map.ellipse(object, ca.plot = p, variable = fill.var)
      if(input$density == TRUE) p <- map.density(object, map = p)
      p
    })
    
    output$info <- renderTable({
      np <- nearPoints(data, input$plot_click, xvar = "Dimension.1", yvar = "Dimension.2", threshold = 10)
      if(is.null(input$plot_click)) np <- data[1,]
      t(np[, -(1:object$nd)])
    })
    
    output$map.mod <- renderPlot({
      dim.plot       <- c(as.numeric(input$dim.plot.x), as.numeric(input$dim.plot.y))
      np <- nearPoints(data, input$plot_click, xvar = "Dimension.1", yvar = "Dimension.2", threshold = 10)
      if(is.null(input$plot_click)) np <- data[1,]
      ind           <- which(object$names.ind %in% rownames(np))
      mod.names     <- colnames(indicator.matrix)
      mod.names.ind <- mod.names[colSums(indicator.matrix[ind, , drop = FALSE] == 1) > 0]
      active.mod.ind <- object$names.mod %in% mod.names.ind
      sup.mod.ind   <- object$names.sup %in% mod.names.ind
      map.title     <- paste(rownames(np), collapse = " & ")
      variable      <- c(object$variable[active.mod.ind], object$variable.sup[sup.mod.ind])
      
      map.select(object, dim = dim.plot, list.mod = active.mod.ind,
                 list.sup = sup.mod.ind, map.title = map.title,
                 label = input$labels, label.repel = TRUE,
                 label.fill = variable, point.color = variable)
    })})
  
  
  # ---------------
  # runApp
  
  app <- list(ui = ui, server = server)
  runApp(app)
}

# coords.ind.plot <- function(object, ind, dim = 1:9){
#              
# Name       <- object$names.ind[ind]  
# coords     <- data.frame(Name, Dimension = object$coord.ind[ind, dim])
# coords     <- melt(coords, id.vars = "Name")
# if(length(ind) == 1) coords$variable <- dim
# 
# ctr        <- data.frame(Name, Ctr = object$ctr.ind[ind, dim])
# ctr        <- melt(ctr, id.vars = "Name")
# gd         <- data.frame(coords, Contribution = ctr$value)
# gd$Name    <- as.factor(gd$Name)
# 
# levels(gd$variable) <- dim
# 
# p <- ggplot(gd, aes(x = variable, y = value, size = Contribution, fill = Name, group = Name)) 
# p <- p + geom_line(aes(color = Name), size = 0.5) + geom_point(shape = 21)
# p + xlab(label = "Dimension") + ylab(label = "Coordinate") + theme_minimal()
# }
# Tools, exports and other helpful functions

#' Cut a continuous variable into categories with a specified minimum
#' 
#' Many continuous variables are very unequally distributed, often with many individuals in the lower categories and fewer in the top.
#' As a result it is often difficult to create groups of equal size, with unique cut-points.
#' By defining the wanted minimum of individuals in each category, but still allowing this minimum to be surpassed, it is easy to create ordinal variables from continuous variables. 
#' The last category will not neccessarily have the minimum number of individuals.
#' 
#' @param x is a continuous numerical variable
#' @param min.size is the minimum number of individuals in each category
#' @return a numerical vector with the number of each category
#' @export
#' @examples
#' a <- 1:1000
#' table(min_cut(a))
#' b <- c(rep(0, 50), 1:500)
#' table(min_cut(b, min.size = 20))
#' 

min_cut <- function(x, min.size = length(x)/10){
  
  x.na <- x[is.na(x) == FALSE]
  p.x <- cumsum(prop.table(table(x.na)))
  t.x <- cumsum(table(x.na))
  bm  <- cbind(table(x.na),t.x)
  dif <- vector(length = nrow(bm))
  for (i in 2:length(dif)) dif[i] <- bm[i,2] - bm[i-1,2]
  dif[1] <- bm[1, 2]
  bm <- cbind(bm, dif)
  
  group <- vector(length = nrow(bm))
  g <- 1 
  collect <- 0
  for (i in 1:nrow(bm)){
    if (dif[i] >= min.size | collect >= min.size-1){
      group[i] <- g
      g <- g + 1
      collect <- 0
    }else{
      group[i] <- g
      collect  <- collect + dif[i]
    }
  }
  
  x.group <- vector(length = length(x))
  group.levels <- as.numeric(levels(as.factor(group)))
  values <- as.numeric(rownames(bm))
  levs   <- vector()
  # Assigning group to the original x
  for (i in 1:length(group.levels)){
    g   <- group.levels[i]
    val <- values[group == g]
    g   <- paste(min(val), ":", max(val), sep = "")
    x.group[x %in% val]  <- g
    levs                 <- c(levs, paste(min(val), ":", max(val), sep = ""))
  }
  x.group[is.na(x)] <- NA
  factor(x.group, labels = levs, ordered = TRUE)
}

#' Cut ordinal variables
#' 
#' If we are in a hurry and need to cut a lot of likert-scale or similar type of variables into MCA-friendly ordered factors this function comes in handy.
#' cowboy_cut will try its best to create approx 3-5 categories, where the top and the bottom are smaller than the middle. Missing or other unwanted categories are recoded but still influence the categorization. So that when cowboy_cut tries to part the top of a variable with a threshold around 10% of cases it is 10% including the missing or NA cases.
#' Make sure that levels are in the right order before cutting. 
#' @param x a factor
#' @param top.share approximate share in top category
#' @param bottom.share approximate share in bottom category
#' @param missing a character vector with all the missing or unwanted categories.
#'
#' @return
#' @export
#'
#' @examples
cowboy_cut <- function(x, top.share = 0.10, bottom.share = 0.10,  missing = "Missing"){
  x[x %in% missing] <- NA
  x <- droplevels(x)
  x <- as.ordered(x)
  x <- as.numeric(x)
  
  x.top <- x
  x.bottom <- x
  x.top[is.na(x)] <- -Inf
  x.bottom[is.na(x)] <- Inf
  
  top <- quantile(x.top, probs = seq(0, 1, by = top.share), na.rm = TRUE, type = 3) %>% tail(2) %>% head(1)
  bottom <- quantile(x.bottom, probs = seq(0, 1, by = bottom.share), na.rm = TRUE, type = 3) %>% head(2) %>% tail(1)
  mid  <- x[x != 0 & x < top] %>% quantile(probs = seq(0, 1, by = 0.33), type = 3, na.rm = TRUE)
  mid  <- mid[-1]
  mid  <- mid[-3]
  breaks <- c(-Inf, bottom, mid, top, Inf)
  o <- x %>% as.numeric() %>% cut(breaks = unique(breaks), include.lowest = TRUE)
  levels(o) <- paste0(1:nlevels(o), "/", nlevels(o)) 
  o %>% fct_explicit_na(na_level = "Missing")
}


#' Export results from soc.ca
#'
#' Export objects from the soc.ca package to csv files.
#' @param object is a soc.ca class object
#' @param dim is the dimensions to be exported
#' @param file is the path and name of the .csv values are to be exported to
#' @return A .csv file with various values in UTF-8 encoding
#' @seealso \link{soc.mca}, \link{contribution}
#' @export

export <- function(object, file = "export.csv", dim = 1:5) {
  if (is.matrix(object) == TRUE|is.data.frame(object) == TRUE){
    write.csv(object, file, fileEncoding = "UTF-8")}

    if ((class(object) == "tab.variable") == TRUE){
      
      ll    <- length(object)
      nam   <- names(object)
      a     <- object[[1]]
      coln  <- ncol(a)
      line  <- c(rep("", coln))
      line2 <- c(rep("", coln))
      a     <- rbind(line, a, line2)      
      
    for (i in 2:ll){
      line <- c(rep("", coln))
      line2 <- c(rep("", coln))
      a <- rbind(a,line, object[[i]], line2)
      line2  <- c(rep("", coln))
    }
    rownames(a)[rownames(a) == "line"] <- nam
    rownames(a)[rownames(a) == "line2"] <- ""
    out <- a
    write.csv(out, file, fileEncoding = "UTF-8")  
    }

    if ((class(object) == "soc.mca") == TRUE){
    coord.mod     <- object$coord.mod[,dim]
    coord.sup     <- object$coord.sup[,dim]
    coord.ind     <- object$coord.ind[,dim]
    names.mod		  <- object$names.mod
    names.sup  	  <- object$names.sup
    names.ind     <- object$names.ind
    coord         <- round(rbind(coord.mod, coord.sup, coord.ind), 2)
    names         <- c(names.mod, names.sup, names.ind)
    rownames(coord) <- names
    
    ctr.mod       <- object$ctr.mod[,dim]
    ctr.sup       <- matrix(nrow = nrow(coord.sup), ncol = ncol(coord.sup))
    ctr.ind       <- object$ctr.ind[,dim]
    ctr           <- round(1000*rbind(ctr.mod, ctr.sup, ctr.ind))
    
    cor.mod       <- round(100*object$cor.mod[,dim], 1)
    cor.sup       <- matrix(nrow = nrow(coord.sup), ncol = ncol(coord.sup))
    cor.ind       <- matrix(nrow = nrow(coord.ind), ncol = ncol(coord.ind))
    cor           <- rbind(cor.mod, cor.sup, cor.ind)
    
    out           <- cbind(coord, ctr, cor)
    colnames(out) <- c(paste("Coord:", dim), paste("Ctr:", dim), paste("Cor:", dim))
    write.csv(out, file, fileEncoding = "UTF-8")
  
  }
  
}

#' Invert the direction of coordinates
#' 
#' Invert one or more axes of a correspondence analysis. The principal coordinates of the analysis are multiplied by -1.
#' @details This is a convieniency function as you would have to modify coord.mod, coord.ind and coord.sup in the soc.ca object.
#' 
#' @param x is a soc.ca object
#' @param dim is the dimensions to be inverted
#' @return a soc.ca object with inverted coordinates on the specified dimensions
#' @seealso \link{soc.mca}, \link{add.to.label}
#' @examples
#' example(soc.ca)
#' inverted.result  <- invert(result, 1:2)
#' result$coord.ind[1, 1:2]
#' inverted.result$coord.ind[1, 1:2]
#' @export

invert <- function(x, dim = 1) {
  x$coord.mod[,dim] <- x$coord.mod[,dim] * -1
  x$coord.all[, dim] <- x$coord.all[, dim] * -1
  x$coord.ind[,dim] <- x$coord.ind[,dim] * -1
  x$coord.sup[,dim] <- x$coord.sup[,dim] * -1
  return(x)
}


#' Convert to MCA class from FactoMineR
#'
#' @param object is a soc.ca object
#' @param active the active variables
#' @param dim a numeric vector
#'
#' @return
to.MCA <- function(object, active, dim = 1:5) {
  
  rownames(active)         <- object$names.ind
  rownames(object$coord.ind) <- object$names.ind
  
  var                      <- list(coord = object$coord.mod[,dim], 
                                   cos2  = object$cor.mod[,dim], 
                                   contrib = object$ctr.mod[,dim])
  ind <- list(coord = object$coord.ind[,dim], 
              cos2 = object$cor.ind[,dim], 
              contrib = object$ctr.ind[,dim])
  call <- list(marge.col = object$mass.mod, 
               marge.row = 1/object$n.ind,
               row.w = rep(1, object$n.ind),
               X = active,
               ind.sup = NULL)
  eig <- matrix(0, nrow = length(dim), ncol = 3)
  rownames(eig) <- paste("dim ", dim, sep="")
  colnames(eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
  eig[,1] <- object$eigen[dim]
  eig[,2] <- eig[,1]/sum(object$eigen) *100
  eig[,3] <- cumsum(eig[,2])
  res <- list(eig=eig, var=var, ind=ind, call=call)                                          
  class(res) <- c("MCA", "list")
  return(res)
}

barycenter <- function(object, mods = NULL, dim = 1){
  new.coord <- sum(object$mass.mod[mods] * object$coord.mod[mods, dim]) / sum(object$mass.mod[mods])
  new.coord
}

#' MCA Eigenvalue check
#'
#' Two variables that have perfectly or almost perfectly overlapping sets of categories will skew an mca analysis. This function tries to find the variables that do that so that we may remove them from the analysis or set some of the categories as passive.
#' An MCA is run on all pairs of variables in the active dataset and we take first and strongest eigenvalue for each pair.
#' Values range from 0.5 to 1, where 1 signifies a perfect or near perfect overlap between sets of categories while 0.5 is the opposite - a near orthogonal relationship between the two variables.
#' While a eigenvalue of 1 is a strong candidate for intervention, probably exclusion of one of the variables, it is less clear what the lower bound is. But values around 0.8 are also strong candidates for further inspection.  

#' @param active a data.frame of factors
#' @param passive a character vector with the full or partial names of categories to be set as passive. Each element in passive is passed to a grep function.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' example(soc.ca)
#' mca.eigen.check(active)
mca.eigen.check <- function(active, passive = "Missing"){
  
  get.eigen <- function(x, y, passive = "Missing"){
    d <- data.frame(x, y)
    r <- soc.mca(d, passive = passive)
    
    burt <- t(r$indicator.matrix.active) %*% r$indicator.matrix.active
    
    burt.s <- burt / diag(burt)
    diag(burt.s) <- 0
    max(burt.s)
    
    o <- c("First eigen" = r$eigen[1], "Passive categories" = length(r$names.passive), "Max overlap (%)" = max(burt.s))
    o
  }
  
  comb <- active %>% colnames() %>% combn(., 2, simplify = T) %>% t() %>% as_tibble()
  colnames(comb) <- c("x", "y")
  
  values           <- map2_df(.x = comb$x, .y = comb$y,  ~get.eigen(x = active[[.x]], y = active[[.y]], passive = passive))
  o                <- bind_cols(comb, values)
  o
}
triangle.coordinates <- function(results, tri, dim = 1:2){
  ind                  <- result$indicator.matrix.active != 0
  ind                  <- ind[, tri]
  tri.members          <- ind %>% rowSums() == length(tri)
  coord.tri.members    <- average.coord(result, tri.members, dim = dim)[2,]
  l.tri.coords         <- lapply(as.data.frame(ind), average.coord, object = result, dim = dim)
  tri.coords           <- bind_rows(l.tri.coords, .id = "id")
  tri.coords           <- tri.coords[tri.coords$label == TRUE, ]
  av.point             <- c(X = mean(tri.coords$X), Y = mean(tri.coords$Y))
  coords               <- bind_rows(tri.coords, av.point, coord.tri.members)
  coords$label         <- c(tri, "Center", "Combined point")
  coords$is.tri        <- !is.na(coords$id)
  coords$is.center     <- coords$label == "Center"
  coords$type          <- c("tri", "tri", "tri", "center", "combined")
  coords$"Distance between center and combined" <- coords %>% filter(type != "tri") %>% select(X, Y) %>% dist()
  coords
}

mca.triad.array <- function(l.mca.triads){
  colors  <- c("black",
               brewer.pal(9, "Set1"))
  shapes  <- LETTERS
  
  coords   <-  l.mca.triads$l.sup.coord %>% bind_rows(.id = "mca")
  coords   <- coords[order(coords$triangle),]
  coords$name <- coords$name %>% as_factor() %>% fct_reorder(as.numeric(coords$triangle))
  coords$mca  <- coords$mca %>% as_factor() %>% fct_relevel(names(l.mca.triads$l.sup.coord))
  
  p <- map.ca.base(data = coords, mapping = aes(x = X, y = Y, shape = name, group = triangle, color = triangle))
  p <- p + geom_polygon(fill = NA, size = 0.3)
  p <- p + geom_point(size = 5, shape = 21, fill = "white", color = "white") + geom_point(size = 3, fill = "black")
  p <- p + facet_wrap(~mca) + theme_bw() + theme(strip.background = element_rect(fill = "white"),
                                                 panel.grid.minor = element_blank(),
                                                 panel.grid.major = element_line(linetype = "dotted", color = "grey90"))
  p <- p + scale_color_manual(values = colors) + scale_shape_manual(values = LETTERS)
  p + coord_fixed()
}

#' Title
#'
#' @param l.mca a list of soc.mca objects
#' @param l.triads 
#' @param dim 
#' @param fix.mca 
#'
#' @return
#' @export
#'
#' @examples

mca.triads <- function(l.mca, l.triads, dim = c(1,2), fix.mca = 1){
  
  tri.names <- map(l.triads, .f = function(x) x %>% select(-starts_with("id")) %>% colnames()) %>% stack() %>% rename(name = values, triangle = ind)
  
  # List of joined supplementary variables
  create_l.sup <- function(mca, l.triads){
    f          <- function(mca, tri) tibble(id  = mca$names.ind) %>% left_join(tri, by = "id")
    l.triads %>% map(f, mca = mca) %>% reduce(full_join, by = "id")
  }
  
  l.sup        <- l.mca %>% map(create_l.sup, l.triads)
  
  # Sup coordinates
  get.sup   <- function(mca, sup, dim){
    sup.coord <- sup %>% select(-id) %>% map(~average.coord(mca, .x, dim = dim)) %>% bind_rows(.id = "name") %>% dplyr::filter(label)
    sup.coord <- sup.coord %>% left_join(., tri.names, by = "name") %>% select(-group, -label)
    sup.coord
  }
  
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  # Aligning coordinates
  coords     <- bind_rows(l.sup.coord, .id = "mca")
  max.coords <- coords %>% select(name, X, Y) %>% gather(key = "key", value = "value", -name) %>% group_by(name) %>%
    summarise(median = median(sqrt(value^2)))
  
  coords        <- coords[coords$name == max.coords$name[which.max(max.coords$median)],]
  cat("\n", "Category used for fixing: ", coords$name[1], "\n")
  
  coords        <- coords %>% select(X, Y)
  direction     <- coords > 0
  fix.direction <- coords[fix.mca,] > 0
  
  flip                 <- t(direction) != as.vector(fix.direction)
  flip                 <- lapply(data.frame(flip), which)
  flip                 <- flip %>% map(~dim[.x])
  
  l.mca                <- map2(l.mca, flip, invert)
  
  # Recalculate sup coordinates
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  list(l.mca = l.mca, l.sup.coord= l.sup.coord)
}


theme_min   <- function (size = 11, font = "sans", face = 'plain',
                         backgroundColor = 'white', panelColor = 'white', 
                         axisColor = 'black', gridColor = 'grey70', textColor = 'black'){
    theme(
        panel.border        = element_rect(colour = gridColor, linetype = "solid", fill = NA),
        axis.text.x         = element_text(vjust = 1, hjust = 0.5,
                                           colour = axisColor, family = font, face = face, size = 9),
        axis.text.y         = element_text(hjust = 1, vjust = 0.5,
                                           colour = axisColor, family = font, face = face, size = 9),
	      #axis.text.x        = theme_text(vjust = 1, hjust = 0.5,
        #                                colour = axisColor, family = font, face = face, size = size),
        #axis.text.y        = theme_text(hjust = 1, vjust = 0.5,
        #                                colour = axisColor, family = font, face = face, size = size),
        axis.title.x        = element_text(family = font, face = face, colour = axisColor, size = size),
        axis.title.y        = element_text(angle = 90, family = font, face = face, colour = axisColor, size = size),
        axis.line           = element_blank(),
        #axis.ticks         = theme_segment(colour = axisColor, size = 0.25),
	      axis.ticks          = element_blank(),
        #panel.border       = theme_rect(colour = axisColor, linetype = "dashed"),
	      #panel.border       = theme_rect(colour = axisColor, linetype = "dashed"),
        legend.background   = element_rect(fill = NA, colour = gridColor),
        legend.key          = element_blank(),
        legend.key.size     = unit(1.5, 'lines'),
        legend.text         = element_text(hjust = 0, family = font, face = face, colour = textColor, size = size),
        legend.title        = element_text(hjust = 0, family = font, face = face, colour = textColor, size = size),
        panel.background    = element_rect(fill = panelColor, colour = NA),
        plot.background     = element_rect(fill = backgroundColor, colour = NA),
        panel.grid.major    = element_line(colour = gridColor, size = 0.33, linetype = "dotted"),
        panel.grid.minor    = element_blank(),
        strip.background    = element_rect(fill = NA, colour = NA),
        strip.text.x        = element_text(hjust = 0, family = font, face = face, colour = textColor, size = size),
        strip.text.y        = element_text(angle = -90, family = font, face = face, colour = textColor, size = size),
        plot.title          = element_text(hjust = 0, vjust = 1, family = font, face = face, colour = textColor, size = 15),
        plot.margin         = unit(c(0.3, 0.1, 0.1, 0.1), 'lines'))
}
# Options
.onLoad <- function(libname, pkgname){
options(soc.ca.colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
                          "#A65628", "#F781BF", "#999999", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", 
                          "#BF5B17", "#666666", "#FF0000FF", "#FF2600FF", "#FF4D00FF", "#FF7300FF", "#FF9900FF", 
                          "#FFBF00FF", "#FFE500FF", "#F2FF00FF", "#CCFF00FF", "#A6FF00FF", 
                          "#80FF00FF", "#59FF00FF", "#33FF00FF", "#0DFF00FF", "#00FF19FF", 
                          "#00FF40FF", "#00FF66FF", "#00FF8CFF", "#00FFB2FF", "#00FFD9FF", 
                          "#00FFFFFF", "#00D9FFFF", "#00B3FFFF", "#008CFFFF", "#0066FFFF", 
                          "#0040FFFF", "#001AFFFF", "#0D00FFFF", "#3300FFFF", "#5900FFFF", 
                          "#7F00FFFF", "#A600FFFF", "#CC00FFFF", "#F200FFFF", "#FF00E6FF", 
                          "#FF00BFFF", "#FF0099FF", "#FF0073FF", "#FF004DFF", "#FF0026FF"))
options(soc.ca.gradient = c(low = "papayawhip", high = "darkblue"))
options(soc.ca.shape    = c(21, 22, 23, 24, 25, 6, 0, 1, 2, 3, 4, 5, 7, 8, 9, 10,
                            12, 15, 16, 17, 18, 42, 45, 61, 48, 50:120))
}

################################################################################


