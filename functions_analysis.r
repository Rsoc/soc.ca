# Functions analysis

#'Specific Multiple Correspondence Analysis
#'
#'
#'@param x Defines the active modalities in a data.frame with rows of individuals and columns of factors, without NA's' 
#'@param sup Defines the supplementary modalities in a data.frame with rows of individuals and columns of factors, without NA's 
#'@param identifier A single factor containing a single value for each row/individual in x and sup. Typically a name or an id.number.
#'
#'@return sv Singular values
#'@return nd Number of dimensions NB! 
#'@return ev Eigenvectors
#'@return totin Total inertia
#'@return names Names of the modalities
#'@return mass Mass for each modality
#'@return chidist Chi-square distances to centroid 
#'@return inertia Inertia for each modality
#'@return explained.inertia A matrix containing the explained dimensions, Sv, percentage of explained inertia and the cumulative percentage
#'@return adj.inertia A matrix containing the adjusted (Rouanet-adjustment) dimensions, Sv, percentage of explained inertia and the cumulative percentage
#'@return contrib The contribution of each modality to each dimension
#'@return cor
#'@return coord Principal coordinates
#'@return average.contrib 
#'@return standard.coord Standard coordinates
#'@return active A vector with rownumbers for the active modalities
#'@return sup A vector with rownumbers for the supplementary modalities
#'@return identifier A vector with rownumbers for the individuals
#'@return Burt The burt matrix containing the active modalities
#'@return modal
#'
#'
#'
#'@ References Greenacre NB!!!!
#'@name smca
#'@author Anton Grau Larsen and Stefan Bastholm Andrade
#'

soc.ca <- function(active, sup=NULL, identifier=NULL, passive="Missing"){
  
  active  <- data.frame(lapply(active, factor))               # Turn active variables into factor
  sup     <- data.frame(lapply(sup, factor))                  # Turn sup variables into factor
  Q       <- ncol(active)                                     # Number of active variables 
  a.r     <- nrow(active)                                     # Number of active rows or the number of individuals
  sup.n   <- sum(unlist(lapply(as.data.frame(sup), nlevels))) # Number of supplementary modalities
  
  if ((nrow(sup)==0)==TRUE){
    sup             <- matrix(, nrow=nrow(active), ncol=2)
    sup[,1:2]       <- cbind(rep(0, nrow(active)), rep(0, nrow(active)))
    colnames(sup)   <- c("No supplementary points defined 1", "No supplementary points defined 2")
    Z.sup           <- indicator(sup)    
  }
  
  
  
  # Creating the indicatormatrix for active and supplementary variables
  ind.act <- indicator(active)
  ind.sup <- indicator(sup)
  
  # Finding the subset
  sub         <- grepl(paste(passive, collapse="|"),colnames(ind.act))
  set         <- 1:ncol(ind.act)
  subset      <- set[!sub]
  
  # Finds the amount of variables without passive modalities
  Qm    <- ncol(active)
  for (i in seq(ncol(active))){
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
  
  class(result)   <- "soc.ca"
  return(result)
}


#################### Correspondence analysis on a Burt matrix
# B is a Burt matrix
# supmat is a supplementary burt matrix
# nvar ???
subset.ca.indicator <- function(Z.act, Z.sup, subset, Q, Qm){
  
  #Z.act <- indicator(active) # The indicator matrix for the active modalities
  #Z.sup <- indicator(sup) # The indicator matrix for the supplementary modalities
  
  I <- dim(Z.act)[1] # Number of individuals
  J <- dim(Z.act)[2]  # Number of modalities >> Subset
  Q <- ncol(active)   # Number of variables
  
  # Inertias
  P <- Z.act / sum(Z.act)       # 
  cm <- apply(P, 2, sum)        # Column (modality) mass
  rm <- apply(P, 1, sum)        # Row (individual) mass
  
  diag.rm <- diag(1/ sqrt(rm))
  diag.cm <- diag(1/ sqrt(cm))
  
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
  pc.ind <- diag.rm %*% dec$u %*% diag(dec$d)   # Principal coordinates for individuals
  pc.mod <- diag.cm %*% dec$v %*% diag(dec$d)   # Principal coordinates for modalities
  
  # Inertias
  inr.ind <- diag(rm) %*% pc.ind^2 # Inertia for row (Individuals) (mass x principal coordinates)
  inr.mod <- diag(cm) %*% pc.mod^2 # Inertia for columns (Modalities)
  
  # Contributions
  ctr.ind <- t(t(inr.ind) / dec$d^2) # Contribution for the individuals (inertia / eigenvalue)
  ctr.mod <- t(t(inr.mod) / dec$d^2) # Contribution for the modalities
  
  # Squared cosines or correlations
  cor.ind <- inr.ind/apply(inr.ind, 2, sum)  # Squared cosines for individuals
  cor.mod <- inr.mod/apply(inr.mod, 1, sum)  # Squared cosines for modalities
  
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
  
  freq.mod <- apply(Z.act, 2, sum)[subset]
  freq.sup <- apply(Z.sup, 2, sum)
  
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

# Creates a indicator matrix from a data.frame with questions as columns and individuals as rows.
# indicator(x, id=NULL, ps=": ")
# x is a data.frame
# id is a vector defining the labels for the individuals. If id = NULL row number is used.
# ps is the seperator used in the creation of the names of the columns (modalities).
}
