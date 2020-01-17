# Functions analysis

# ' @docType package
# ' ...
# ' @import ggplot2 ellipse grid scales


#' Specific Multiple Correspondence Analysis
#'
#' \code{soc.mca} performs a specific multiple correspondence analysis on a data.frame of factors, where cases are rows and columns are variables.
#' @param active       Defines the active modalities in a data.frame with rows of individuals and columns of factors, without NA's'. Active can also be a named list of data.frames. The data.frames will correspond to the analytical headings.
#' @param sup          Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
#' @param identifier   A single vector containing a single value for each row/individual in x and sup. Typically a name or an id.number.
#' @param passive      A single character vector with the full or partial names of the passive modalities. All names that have a full or partial match will be set as passive.
#' @param Moschidis    If TRUE adjusts contribution values for rare modalities. see \link{moschidis}.
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

soc.mca <- function(active, sup = NULL, identifier = NULL, passive = getOption("passive", default = "Missing"), 
                    balance.headings = FALSE, Moschidis = FALSE) {

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
    headings <- rep(names(active), sapply(active, length))
    names(active) <- NULL
    active <- do.call("cbind", active)
    active <- data.frame(lapply(active, factor), check.names = F)
    nl     <- sapply(active, nlevels)
    headings <- rep(headings, nl)
    a.r <- nrow(active)
    ind.act <- indicator(active)
  }
  
  #########################################
  # Creating lists of variable names...
  #########################################
  
  varlist <- unique(gsub(": .*", "", colnames(ind.act)))                  # Unique varnames
  varlist.long <- gsub(": .*", "", colnames(ind.act))                     # Vector of varnames matching the list of modalities
  Q <- median(rowSums(ind.act))                                           # Number of Questions [we use the median in order to allow for questions that do not sum to one]
  
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
  
  result <- subset.ca.indicator(ind.act, ind.sup, active.set, passive.set, Q = Q, Qm = Qm, Moschidis = Moschidis)
  
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
  
  
  variable <- vector()
  for (i in 1:nrow(md)) {
    variable <- c(variable, rep(rownames(md)[i], md[i, "Nb. active modalities"]))
  }
  result$variable   <- variable
  
  if (identical(sup, NULL) == FALSE) {
    varnames <- colnames(sup)
    ml <- vector()
    for (i in 1:ncol(sup)) {
      ml <- c(ml, rep(varnames[i], nlevels(sup[, i])))
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
  #result <- median.standard(result)
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

subset.ca.indicator <- function(ind.act, ind.sup, active.set, passive.set, Q, Qm, Moschidis){
  
  
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
  dec.full  <- svd(S, nu = nrow(S), nv = ncol(S))                 # Singular decomposition keeping all vectors
  dec       <- svd(S)                                             # Singular decomposition
  eigen     <- dec$d^2                                            # Eigenvalues from singularvalues
  
  # Principal coordinates
  pc.mod    <- diag.cm %*% dec$v %*% diag(dec$d)   # Principal coordinates for modalities
  
  # Principal coordinates for individuals
  diag.rm   <- diag(1/ sqrt(rm))  
  pc.ind    <- diag.rm %*% dec$u %*% diag(dec$d)   # Principal coordinates for individuals # This is a slow process, but it scales ok # Anders Holm adjustment  
  
  # Inertias for rows and column profiles
  inr.ind   <- diag(rm) %*% pc.ind^2     # Inertia for row (Individuals) (mass x principal coordinates) # This is a slow process and it scales badly - diag(rm) is a individual X individual matrix. It is also sparse - so it might be possible to do it quicker.
  inr.mod   <- diag(cm) %*% pc.mod^2     # Inertia for columns (Modalities)
  
  # Relative contributions, 'point inertia on axis' / total inertia of axis
  ctr.ind   <- t(t(inr.ind) / dec$d^2)   # Contribution for the individuals (inertia / eigenvalue)
  ctr.mod   <- t(t(inr.mod) / dec$d^2)   # Contribution for the modalities
  ctr.mod.raw   <- inr.mod               # Contribution for the modalities
  
  # Squared cosines or correlations
  cor.ind   <- inr.ind/rowSums(inr.ind)  # Squared cosines for individuals
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
                    svd.d     = dec.full$d,
                    svd.u     = dec.full$u,
                    svd.v     = dec.full$v,
                    indicator.matrix.trans   = Z.act
  )
  
  # Cleanup
  names(ca.output$mass.mod)         <- NULL
  dimnames(ca.output$coord.sup)     <- NULL
  names(ca.output$freq.mod)         <- NULL
  names(ca.output$freq.sup)         <- NULL
  
  return(ca.output)
}
################################### Indicator matrix

#' Indicator matrix
#' 
#' Creates an indicator matrix from a data.frame with questions as columns and individuals as rows.
#' 
#' @param x   a data.frame of factors
#' @param id  a vector defining the labels for the individuals. If id = NULL row number is used.
#' @param ps  the seperator used in the creation of the names of the columns (modalities).
#'
#' @return Returns a indicator matrix
#' @seealso \link{soc.mca}
#' @examples 
#' a  <- rep(c("A","B"), 5)
#' b  <- rep(c("C", "D"), 5)
#' indicator(data.frame(a,b))
#' @export

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
#' @export
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

####################################################
#### Create Quadrant

#' Create categories according to the quadrant position of each individual
#' 
#' Creates a vector from two dimensions from a soc.ca object. Labels are the 
#' cardinal directions with the first designated dimension running East - West.
#' The center category is a circle defined by \code{cut.radius}.
#' 
#' 
#' @param object a soc.ca class object
#' @param dim  the dimensions
#' @param cut.min  Minimum cut value
#' @param cut.max  Maximum cut value
#' @param cut.radius  Radius of the center category
#' @return Returns a character vector with category memberships
#' @seealso \link{soc.mca}
#' @examples 
#' example(soc.ca)
#' create.quadrant(result, dim = c(2, 1))
#' table(create.quadrant(result, dim = c(1, 3), cut.radius = 0.5))
#' @export

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
    if(all(not.all.data.frames, not.all.matrix)) stop("x is a list, but not all elements are not the same valid type (aka. data.frame or indicator matrix).")
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


