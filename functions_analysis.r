# Functions analysis
# Specific Multiple Correspondence Analysis
#  sup=NULL
#  identifier=NULL
#  passive="Missing"

soc.ca <- function(active, sup=NULL, identifier=NULL, passive="Missing"){
  
  active  <- data.frame(lapply(active, factor))               # Turn active variables into factor ## Slet det her og sæt en advarsel ind i stedet
  sup     <- data.frame(lapply(sup, factor))                  # Turn sup variables into factor    ## Slet det her og sæt en advarsel ind i stedet
  Q       <- ncol(active)                                     # Number of active variables 
  a.r     <- nrow(active)                                     # Number of active rows or the number of individuals
  sup.n   <- sum(unlist(lapply(as.data.frame(sup), nlevels))) # Number of supplementary modalities
  
  if ((nrow(sup)==0)==TRUE){                                  # This process is slow at + 150.000 individuals
    sup             <- matrix(0, nrow=nrow(active), ncol=2)
    sup[,1:2]       <- cbind(rep(0, nrow(active)), rep(0, nrow(active)))
    colnames(sup)   <- c("No supplementary points defined 1", "No supplementary points defined 2")
    ind.sup         <- sup
  }else{
    ind.sup <- indicator(sup)  
  }
    
  # Creating the indicatormatrix for active and supplementary variables
  ind.act <- indicator(active)
      
  # Finding the subset
  sub         <- grepl(paste(passive, collapse="|"), colnames(ind.act))
  set         <- 1:ncol(ind.act)
  subset      <- set[!sub]
  
  # Finds the amount of variables without passive modalities
  Qm    <- Q
  for (i in seq(Q)){
    lev <- levels(active[,i])
    pasQ <- grepl(paste(passive, collapse="|"), lev)
    if (any(pasQ==TRUE)==TRUE){
      Qm <- Qm - 1
    }
  }
  
  result      <- subset.ca.indicator(ind.act, ind.sup, subset, Q=Q , Qm=Qm)
  
  if (identical(identifier, NULL)==TRUE){
    identifier <- 1:nrow(active)
  }
  
  # Names
  result$names.mod      <- colnames(ind.act)[subset]
  result$names.ind      <- as.character(identifier)
  result$names.sup      <- colnames(ind.sup)
  result$names.passive  <- colnames(ind.act)[sub]
  
  # List of descriptive values
  varnames    <- colnames(active)
  ml          <- vector()
  for (i in 1:ncol(active)){
    ml         <- c(ml, rep(varnames[i], nlevels(active[,i])))
  }
  ml          <- ml[!sub]
  mm          <- as.matrix(cbind(ml, 1:length(ml)))
  md          <- matrix(, nrow=ncol(active), ncol=3)
  rownames(md) <- varnames
  colnames(md) <- c("Start", "End", "Modalities")
  md          <- as.data.frame(md)
  
  for (i in 1:ncol(active)){
    mr         <- as.numeric(mm[,2][mm[,1]==varnames[i]])
    md[i,1]    <- min(mr)
    md[i,2]    <- max(mr)
    md[i,3]    <- length(mr)
  }
  md[,1]      <- as.numeric(md[,1])
  md[,2]      <- as.numeric(md[,2])
  
  result$modal    <- md
  
  variable <- vector()
  for (i in 1:nrow(md)){
    variable <- c(variable, rep(rownames(md)[i], md[i,3]))
  }
  
  result$variable <- variable
  
  class(result)   <- "soc.ca"
  return(result)

  #'Specific Multiple Correspondence Analysis
  #'
  #'soc.ca(active, sup=NULL, identifier=NULL, passive="Missing")
  #'
  #'@param active       Defines the active modalities in a data.frame with rows of individuals and columns of factors, without NA's' 
  #'@param sup          Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
  #'@param identifier   A single vector containing a single value for each row/individual in x and sup. Typically a name or an id.number.
  #'@param passive      A single character vector with the full or partial names of the passive modalities. All names that have a full or partial match will be set as passive. see also set.passive()
  #'
  #'@return sv          Singular values
  #'@return nd          Number of active dimensions: #¤ Check the actual definition and make a reference to le roux
  #'@return n.ind       The number of active individuals
  #'@return n.mod       The number of active modalities
  #'@return eigen       Eigenvectors
  #'@return total.inertia The sum of inertia
  #'@return adj.inertia A matrix with all active dimensions, adjusted and unadjusted inertias. See also tab.variance()
  #'@return freq.mod    Frequencies for the active modalities. See also add.n()
  #'@return freq.sup    Frequencies for the supplementary modalities. See also add.n()
  #'@return ctr.mod     A matrix with the contribution values of the active modalities per dimension. See also contribution()
  #'@return ctr.ind     A matrix with the contribution values of the individuals per dimension. See also individuals()
  #'@return cor.mod     The correlation or quality of each modality per dimension.
  #'@return cor.ind     The correlation or quality of each individual per dimension. #¤ This may be defunct!
  #'@return mass.mod    The mass of each modality
  #'@return mass.mod    The mass of each individual #¤ Why is this even here?? Because we might weight the individuals later??
  #'@return coord.mod   A matrix with the principal coordinates of each active modality per dimension.
  #'@return coord.ind   A matrix with the principal coordinates of each individual per dimension.
  #'@return coord.sup   A matrix with the principal coordinates of each supplementary modality per dimension.
  #'@return names.mod   The names of the active modalities
  #'@return names.ind   The names of the individuals
  #'@return names.sup   The names of the supplementary modalities
  #'@return names.passive The names of the passive modalities
  #'@return modal       A matrix with the number of modalities per variable and their location
  #'@return variable    A vector with the name of the variable of the active modalities
  
  #'@ References Greenacre NB!!!! And Le Roux #¤
  #'@name soc.ca
  #'@author Anton Grau Larsen and Stefan Bastholm Andrade
    
}


#################### Correspondence analysis on a indicator matrix
# Z.act = ind.act
# Z.sup = ind.sup
# subset
# Q=Q
# Qm=Qm

subset.ca.indicator <- function(Z.act, Z.sup, subset, Q, Qm){
  
  I <- dim(Z.act)[1]  # Number of individuals
  J <- dim(Z.act)[2]  # Number of modalities >> Subset
  Q <- Q              # Number of variables
  
  # Inertias
  P <- Z.act / sum(Z.act)       # 
  cm <- colSums(P)              # Column (modality) mass
  rm <- rowSums(P)              # Row (individual) mass
  
  diag.cm <- diag(1/ sqrt(cm))  # This commmand scales badly because it creates a individual X individual matrix - If this number could be obtained differently - for instance - through the Burt matrix - there is a substantial speed gain.
  
  eP    <- rm %*% t(cm)           # Expected distances
  S     <- (P - eP) / sqrt(eP)    # Euclidian distances
  
  # Subsetting
  K   <- length(subset)
  S   <- S[ ,subset]
  cm  <- cm[subset]
  diag.cm <- diag.cm[subset, subset]
  
  # Decomposition and eigenvectors
  dec   <- svd(S)                 # Singular decomposition
  eigen <- dec$d^2                # Eigenvector
    
  # Principal coordinates
  pc.mod <- diag.cm %*% dec$v %*% diag(dec$d)   # Principal coordinates for modalities
  
  # Fast principal coordinates for individuals
  if (identical(var(rm), 0)){
  sqrm       <- 1/ sqrt(rm)
  pc.ind     <- (sqrm[1] * dec$u) %*% diag(dec$d)    
  }else{
    # Original principal coordinates for individuals
    diag.rm <- diag(1/ sqrt(rm))  
    pc.ind  <- diag.rm %*% dec$u %*% diag(dec$d)   # Principal coordinates for individuals # This is a slow process, but it scales ok # Anders Holm adjustment  
  }
  
    
  # Fast inertias
  if (identical(var(rm), 0)){
  inr.ind <- rm[1] * pc.ind^2
  inr.mod <- diag(cm) %*% pc.mod^2
  }else{
    # Original inertias
    inr.ind <- diag(rm) %*% pc.ind^2 # Inertia for row (Individuals) (mass x principal coordinates) # This is a slow process and it scales badly - diag(rm) is a individual X individual matrix. It is also sparse - so it might be possible to do it quicker.
    inr.mod <- diag(cm) %*% pc.mod^2 # Inertia for columns (Modalities)
  }
  
  # Contributions
  ctr.ind <- t(t(inr.ind) / dec$d^2) # Contribution for the individuals (inertia / eigenvalue)
  ctr.mod <- t(t(inr.mod) / dec$d^2) # Contribution for the modalities
  
  # Squared cosines or correlations
  cor.ind <- inr.ind/colSums(inr.ind)  # Squared cosines for individuals
  cor.mod <- inr.mod/rowSums(inr.mod)  # Squared cosines for modalities
    
  # Chi-distances
  
  # Supplementary principal coordinates
  Z.star  <- Z.sup
  I.star  <- dim(Z.sup)[1]
  cs.star <- apply(Z.sup, 2, sum)
  base    <- Z.star / matrix(rep(cs.star, I.star), nrow = I.star, byrow = TRUE)
  f.s1    <- dec$u * sqrt(eigen) / sqrt(rm)   # Hvad er det her?
  a.s1    <- f.s1 / sqrt(eigen)               # Og har vi det et andet sted - til når der skal subsettes???
  pc.sup  <- t(base) %*% a.s1
  
  # Supplementary squared cosines or correlations
  ### Det her mangler
  #cor.sup <- inr.mod/apply(inr.mod, 1, sum)   # Squared cosines for modalities
  
  # First reduction of dimensionality
  R1.dim        <- K-Qm
  R1.eigen.val  <- eigen[1:R1.dim]
  
  # Second reduction of dimensionality
  Mean.r.eigen  <- mean(R1.eigen.val, na.rm=TRUE)
  R2.eigen.val  <- eigen[eigen>=Mean.r.eigen]
  R2.dim        <- which(R2.eigen.val >= Mean.r.eigen)
  
  # Explained variance
  unadj.var     <- 100*(eigen[1:R1.dim])/sum(eigen[1:R1.dim]) # Unadjusted rates of variance
  sum.adj.var.mod <- (eigen[R2.dim]-Mean.r.eigen)^2
  sum.adj.var   <- sum(sum.adj.var.mod)
  adj.var       <- round(((sum.adj.var.mod/sum.adj.var)*100), digits=1) # The adjusted rates of variance
  cumpercent    <- cumsum(adj.var)
  adj.inertia   <- cbind(R2.dim, round(eigen[R2.dim], 3), round(unadj.var[R2.dim], 1), adj.var ,cumpercent) # De Rouanet adjustede inertier - skal nok rykkes ned.
  colnames(adj.inertia) <- c("Dim", "Sv", "Var" ,"Adj.Var", "Cum%")
  
  freq.mod <- colSums(Z.act)[subset]
  freq.sup <- colSums(Z.sup)
  
  # Output
  ca.output <- list(nd = max(R2.dim), n.ind = nrow(Z.act), n.mod = length(subset),
                    eigen = eigen[R2.dim], total.inertia = sum(eigen[R2.dim]),
                    adj.inertia=adj.inertia,
                    freq.mod = freq.mod, freq.sup = freq.sup,
                    ctr.mod = ctr.mod[, R2.dim], ctr.ind = ctr.ind[, R2.dim],
                    cor.mod = cor.mod[, R2.dim], cor.ind = cor.ind[, R2.dim],
                    mass.mod = cm, mass.ind = rm,
                    coord.mod = pc.mod[, R2.dim], coord.ind = pc.ind[, R2.dim], coord.sup = pc.sup[, R2.dim]
                    )
  # Cleanup
  names(ca.output$mass.mod)         <- NULL
  names(ca.output$mass.ind)         <- NULL
  dimnames(ca.output$coord.sup)     <- NULL
  names(ca.output$freq.mod)         <- NULL
  names(ca.output$freq.sup)         <- NULL
  
  
  return(ca.output)

  # Subset.ca.indicator
  # This function is part of the soc.ca function but allows for manipulation of the indicator matrix before analysis. Most users will not need this function.
  # subset.ca.indicator(Z.act, Z.sup, subset, Q, Qm)
  # Inputs
  # Z.act   An indicator matrix of all the active modalities (including those that are to be set as passive)
  # Z.sup   An indicator matrix of the supplementary modalities
  # subset  A vector containing column indices of passive modalities
  # Q       The number of variables
  # Qm      The number of variables without passive modalities
  # Outputs
  # See soc.ca() documentation

}


################################### Indicator matrix

indicator <- function(x, id=NULL, ps=": "){
obj     <- x
#obj     <- data.frame(lapply(data.frame(obj), as.factor)) # Gør hele analyse objektet til factor - kan sættes ind et andet sted.
I       <- nrow(obj)                                      # Number of individuals
levels.n <- unlist(lapply(obj, nlevels))
n       <- cumsum(levels.n)                               # Number of modalities for each question
m       <- max(n)                                         # Total number of modalities
Q       <- ncol(obj)                                      # Number of questions
Z       <- matrix(0, nrow = I, ncol = m)                  # Predefinition of the indicatormatrix
newdat  <- lapply(obj, as.numeric)
offset  <- (c(0, n[-length(n)]))
for (i in seq(Q)) Z[seq(I) + (I * (offset[i] + newdat[[i]] - 1))] <- 1 # Indicator matrix
fn      <- rep(names(obj), unlist(lapply(obj, nlevels)))  
ln      <- unlist(lapply(obj, levels))
col.names   <- paste(fn, ln, sep = ps)
colnames(Z) <- col.names

if (identical(id, NULL)==TRUE){
rownames(Z) <- as.character(seq(I))
}else{
rownames(Z) <- id
}    
return(Z)

# indicator()
# Creates an indicator matrix from a data.frame with questions as columns and individuals as rows.
# indicator(x, id=NULL, ps=": ")
# Inputs
# x is a data.frame
# id is a vector defining the labels for the individuals. If id = NULL row number is used.
# ps is the seperator used in the creation of the names of the columns (modalities).
# Output
# Returns an indicator matrix
}
