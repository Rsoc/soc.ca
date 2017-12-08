mca.jacob <- function(active, sup = NULL, identifier = NULL, passive = "NO PASSIVE MODALITES XYZ", 
          indicator.matrix = FALSE, active.is.list = FALSE, balance.headings = FALSE, Moschidis = FALSE) {
  
  
  headings = NULL
  if (identical(active.is.list, FALSE) & identical(indicator.matrix, TRUE)) {
    a.r <- nrow(active)
    ind.act <- data.frame(active, check.names = F)
    sup.n <- sum(unlist(lapply(as.data.frame(sup), nlevels)))
    if (identical(sup, NULL) == TRUE) {
      sup <- matrix(0, nrow = nrow(active), ncol = 2)
      sup[, 1:2] <- cbind(rep(0, nrow(active)), rep(0, nrow(active)))
      colnames(sup) <- c("No supplementary points defined 1", 
                         "No supplementary points defined 2")
      ind.sup <- sup
    }
    if ((nrow(sup) == 0) == FALSE) {
      ind.sup <- indicator_jacob(sup)
    }
  }
  
  if (identical(active.is.list, FALSE) & identical(indicator.matrix, FALSE)) {
    active <- data.frame(lapply(active, factor), check.names = F)
    a.r <- nrow(active)
    ind.act <- indicator_jacob(active)
    sup.n <- sum(unlist(lapply(as.data.frame(sup), nlevels)))
    if (identical(sup, NULL) == TRUE) {
      sup <- matrix(0, nrow = nrow(active), ncol = 2)
      sup[, 1:2] <- cbind(rep(0, nrow(active)), rep(0, nrow(active)))
      colnames(sup) <- c("No supplementary points defined 1", 
                         "No supplementary points defined 2")
      ind.sup <- sup
    }
    if ((nrow(sup) == 0) == FALSE) {
      ind.sup <- indicator_jacob(sup)
    }
  }
  
  if (identical(active.is.list, TRUE) & identical(indicator.matrix, TRUE)) {
    headings <- rep(names(active), sapply(active, ncol))
    names(active) <- NULL
    ind.act <- do.call("cbind", active)

    if (identical(sup, NULL)) {
      sup <- matrix(0, nrow = nrow(ind.act), ncol = 2)
      sup[, 1:2] <- cbind(rep(0, nrow(ind.act)), rep(0, nrow(ind.act)))
      colnames(sup) <- c("No supplementary points defined 1", 
                         "No supplementary points defined 2")
      ind.sup <- sup
    } else {
      ind.sup <- sup
    }
     }
    
  if (identical(active.is.list, TRUE) & identical(indicator.matrix, FALSE)) {
    headings <- rep(names(active), sapply(active, length))
    names(active) <- NULL
    active <- do.call("cbind", active)
    a <- aggregate(sapply(active, function(x) length(table(x))), 
                   by = list(headings), sum)
    headings <- rep(a[, 1], a[, 2])
  
  active <- data.frame(lapply(active, factor), check.names = F)
  sup <- data.frame(lapply(sup, factor), check.names = F)
  a.r <- nrow(active)
  ind.act <- indicator_jacob(active)
  sup.n <- sum(unlist(lapply(as.data.frame(sup), nlevels)))
  if (identical(sup, NULL) == TRUE) {
    sup <- matrix(0, nrow = nrow(active), ncol = 2)
    sup[, 1:2] <- cbind(rep(0, nrow(active)), rep(0, nrow(active)))
    colnames(sup) <- c("No supplementary points defined 1", 
                       "No supplementary points defined 2")
    ind.sup <- sup
  }
  else {
    ind.sup <- indicator_jacob(sup)
  }
  }
  
  varlist <- unique(gsub(": .*", "", colnames(ind.act)))
  varlist.long <- gsub(": .*", "", colnames(ind.act))
  Q <- mean(rowSums(ind.act))

  
  passive.set <- grepl(paste(passive, collapse = "|"), colnames(ind.act))
  set <- 1:ncol(ind.act)
  subset <- set[!passive.set]
  
  ind.reduced <- ind.act[,subset]
  varlist.long.red <- gsub(": .*", "", colnames(ind.reduced))
  
  
  tmpx <- vector()
  to <- length(unique(varlist))
  for (i in 1:to) {
    t1 <- ind.act[,varlist.long == unique(varlist)[i]]
    t2 <- ind.reduced[,varlist.long.red == unique(varlist)[i]]
    if (is.null(dim(t1)[2])) {
      tmpx[i] <- max(ind.act[,varlist.long == unique(varlist)[i]] - ind.reduced[,varlist.long.red == unique(varlist)[i]])  
    }else{
    tmpx[i] <- max(rowSums(ind.act[,varlist.long == unique(varlist)[i]]) - rowSums(ind.reduced[,varlist.long.red == unique(varlist)[i]]))
    }
    }
  Qm <- Q - sum(tmpx)
  
  
  result <- subset.ca.indicator_jacob(ind.act, ind.sup, subset, passive.set, Q = Q, Qm = Qm, Moschidis = Moschidis)
  
  result$variable.all <- varlist.long
  if (identical(identifier, NULL) == TRUE) {
    identifier <- 1:nrow(ind.act)
  }
  
  result$names.mod.all <- colnames(ind.act)
  result$names.mod <- colnames(ind.act)[subset]
  result$names.ind <- as.character(identifier)
  result$names.sup <- colnames(ind.sup)
  result$names.passive <- colnames(ind.act)[passive.set]
  result$headings.all <- headings
  result$headings <- headings[subset]
  result$indicator.matrix.passive <- ind.act[, passive.set]
  result$indicator.matrix.active <- ind.act[, subset]
  result$indicator.matrix.all <- ind.act
  x <- colnames(ind.act[, subset])
  varlist <- gsub(": .*", "", x)
  x.all <- colnames(ind.act)
  varlist.all <- gsub(": .*", "", x.all)
  mm <- as.matrix(cbind(varlist, 1:length(varlist)))
  mmx <- as.matrix(cbind(varlist.all, 1:length(varlist.all)))
  md <- matrix(, nrow = length(unique(unlist(varlist))), ncol = 4)
  rownames(md) <- unique(unlist(varlist))
  colnames(md) <- c("Total nb. modalities", "Nb. active modalities", "Start", "End")
  md <- as.data.frame(md)
  for (i in 1:length(unique(unlist(varlist)))) {
    mr <- as.numeric(mm[, 2][mm[, 1] == unique(unlist(varlist))[i]])
    mx <- as.numeric(mmx[, 2][mmx[, 1] == unique(unlist(varlist.all))[i]])
    md[i, 1] <- length(mx)
    md[i, 2] <- length(mr)
    md[i, 3] <- min(mr)
    md[i, 4] <- max(mr)
    
      }
  md[, 1] <- as.numeric(md[, 1])
  md[, 2] <- as.numeric(md[, 2])
  result$modal <- md
  result$Rosenlund.tresh <- rep(1/result$Q/result$modal$`Nb. active modalities`, times = result$modal$`Nb. active modalities`)
  result$Rosenlund.tresh.all <- rep(1/result$Q/result$modal$`Total nb. modalities`, times = result$modal$`Total nb. modalities`)
  variable <- vector()
  for (i in 1:nrow(md)) {
    variable <- c(variable, rep(rownames(md)[i], md[i, "Nb. active modalities"]))
  }
  result$variable <- variable
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
  
  #Mass matrix
  tmp                    <- data.frame(result$variable.all, result$mass.mod.all)
  mass.all <- matrix( , nrow = length(unique(varlist)), ncol = 4)
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

subset.ca.indicator_jacob <- function(ind.act, ind.sup, subset, passive.set, Q, Qm, Moschidis){
  Z.act     <- ind.act
  Z.sup     <- ind.sup
  colZ      <- colSums(Z.act)
  if (identical(Moschidis, TRUE)) {
    Y <- vector()
    x <- colnames(Z.act)
    varlist <- gsub(': [A-zæøå1-9&/ ]*', '' , x)
    for (i in seq(Q)) {
      x <- rep(table(varlist)[i], table(varlist)[i])
      Y <- c(Y, x)
    }
    Z.act <- Z.act%*%diag(1/(Y-1))
  }
  
  I         <- dim(Z.act)[1]  # Number of individuals
  J         <- dim(Z.act)[2]  # Number of modalities >> Subset
  Q         <- Q              # Number of variables
  
    # Inertias
  P         <- Z.act / sum(Z.act)      
  cm.all    <- colSums(P)              # Column (modality) mass
  mass.mod.all <- cm.all
  rm        <- rowSums(P)              # Row (individual) mass
  
  diag.cm.all   <- diag(1/ sqrt(cm.all))       # This commmand scales badly because it creates a individual X individual matrix - If this number could be obtained differently - for instance - through the Burt matrix - there is a substantial speed gain.
  
  eP        <- rm %*% t(cm.all)            # Expected distances
  S         <- (P - eP) / sqrt(eP)     # Euclidian distances
  
  # Subsetting
  K         <- length(subset)
  S         <- S[, subset]
  cm        <- cm.all[subset]
  cm.all[passive.set]                     <- 0
  diag.cm   <- diag.cm.all[subset, subset]
  diag.cm.all[passive.set, passive.set]   <- 0
  # Decomposition and eigenvectors
  dec.full  <- svd(S, nu = nrow(S), nv = ncol(S))                 # Singular decomposition
  dec       <- svd(S)                                             # Singular decomposition
  eigen     <- dec$d^2                                            # Eigenvector
  
  # Principal coordinates
  pc.mod    <- diag.cm %*% dec$v %*% diag(dec$d)   # Principal coordinates for modalities
  
  # Fast principal coordinates for individuals
#  if (identical(var(rm), 0)){
#    sqrm      <- 1/ sqrt(rm)
#    pc.ind    <- (sqrm[1] * dec$u) %*% diag(dec$d)    
#  }else{
    # Original principal coordinates for individuals
    diag.rm   <- diag(1/ sqrt(rm))  
    pc.ind    <- diag.rm %*% dec$u %*% diag(dec$d)   # Principal coordinates for individuals # This is a slow process, but it scales ok # Anders Holm adjustment  
 # }
  
  
  # Fast inertias
#  if (identical(var(rm), 0)){
#    inr.ind   <- rm[1] * pc.ind^2
#    inr.mod   <- diag(cm) %*% pc.mod^2
#  }else{
    # Original inertias
    inr.ind   <- diag(rm) %*% pc.ind^2     # Inertia for row (Individuals) (mass x principal coordinates) # This is a slow process and it scales badly - diag(rm) is a individual X individual matrix. It is also sparse - so it might be possible to do it quicker.
    inr.mod   <- diag(cm) %*% pc.mod^2     # Inertia for columns (Modalities)
 # }
  
  # Contributions
  ctr.ind   <- t(t(inr.ind) / dec$d^2)   # Contribution for the individuals (inertia / eigenvalue)
  ctr.mod   <- t(t(inr.mod) / dec$d^2)   # Contribution for the modalities
  ctr.mod.raw   <- inr.mod               # Contribution for the modalities
  
  # Squared cosines or correlations
  cor.ind   <- inr.ind/rowSums(inr.ind)  # Squared cosines for individuals
  cor.mod   <- inr.mod/rowSums(inr.mod)  # Squared cosines for modalities
  cor.mod.raw   <- inr.mod  # Squared cosines for modalities
  
  # Chi-distances
  
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
  
  t <- matrix(NA, nrow = nrow(pc.sup), ncol = ncol(pc.sup))
  t.plane <- matrix(NA, nrow = nrow(pc.sup), ncol = 3)
 
  for (j in 1:ncol(pc.sup)) {
  for (i in 1:nrow(pc.sup)) {
  #t[i,j] <- round(pnorm(-abs(sqrt(cs.star[i]*((I-1)/(I-cs.star[i])))*pc.sup[i,j])),5) # produces warning for freq.sup = 0
  t[i,j] <- round(sqrt(cs.star[i]*((I-1)/(I-cs.star[i])))*pc.sup[i,j],5) # produces warning for freq.sup = 0
    }
  }
  t.plane[,1] <- round(pchisq(((pc.sup[,1]^2 + pc.sup[,2]^2)*(cs.star*((I-1)/(I-cs.star)))), df= 2, lower.tail=FALSE),5)
  t.plane[,2] <- round(pchisq(((pc.sup[,1]^2 + pc.sup[,3]^2)*(cs.star*((I-1)/(I-cs.star)))), df= 2, lower.tail=FALSE),5)
  t.plane[,3] <- round(pchisq(((pc.sup[,2]^2 + pc.sup[,3]^2)*(cs.star*((I-1)/(I-cs.star)))), df= 2, lower.tail=FALSE),5) 
  }else{
   pc.sup  <- NULL
   t.plane <- NULL
   t       <- NULL
  }
  # principle coordinates for active and passive modalities
  pc.all            <- pc.mod
  ctr.mod.all       <- ctr.mod
  cor.mod.all       <- cor.mod
  ctr.mod.all.raw   <- ctr.mod.raw
  cor.mod.all.raw   <- cor.mod.raw
  
  if (length(subset) < ncol(Z.act)) {
  cs.star.all       <- colZ
  tmp.ctr <- ctr.mod
  tmp.cor <- cor.mod
  tmp.ctr.raw <- ctr.mod.raw
  tmp.cor.raw <- cor.mod.raw
  rownames(tmp.ctr) <- colnames(Z.act)[subset]
  rownames(tmp.cor) <- colnames(Z.act)[subset]
  rownames(tmp.ctr.raw) <- colnames(Z.act)[subset]
  rownames(tmp.cor.raw) <- colnames(Z.act)[subset]
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
  
  
  for (j in 1:length(eigen)) {
  for (i in 1:length(colnames(Z.act))){
  pc.all[i,j]            <- sum(pc.ind[Z.act[,i] > 0,j]) / cs.star.all[i] / sqrt(eigen[j])
  ctr.mod.all[i,j]       <- tmp.ctr[rownames(tmp.ctr) == colnames(Z.act)[i], j]
  cor.mod.all[i,j]       <- tmp.cor[rownames(tmp.cor) == colnames(Z.act)[i], j]
  ctr.mod.all.raw[i,j]   <- tmp.ctr.raw[rownames(tmp.ctr.raw) == colnames(Z.act)[i], j]
  cor.mod.all.raw[i,j]   <- tmp.cor.raw[rownames(tmp.cor.raw) == colnames(Z.act)[i], j]
    }}

  }
  # Supplementary squared cosines or correlations
  #cor.sup <- inr.mod/apply(inr.mod, 1, sum)   # Squared cosines for modalities
  
  # First reduction of dimensionality - La dimension du suppport: 
  R1.dim        <- K-Qm
  R1.eigen.val  <- eigen[1:R1.dim]
  
  # Second reduction of dimensionality - dimensions above average
  Mean.r.eigen  <- ((K/Q) - sum(cm)) / R1.dim
  R2.eigen.val  <- eigen[eigen >= Mean.r.eigen]
  R2.dim        <- length(R2.eigen.val)
  
  # Explained variance
  unadj.var     <- 100*(eigen[1:R1.dim])/sum(eigen[1:R1.dim]) # Unadjusted rates of variance
  sum.adj.var.mod <-(Q/(Q-1))^2 * (eigen[1:R2.dim]-Mean.r.eigen)^2
  sum.adj.var   <- sum(sum.adj.var.mod)
  adj.var       <- round(((sum.adj.var.mod/sum.adj.var)*100), digits = 1) # The adjusted rates of variance
  cumpercent    <- cumsum(adj.var)
  adj.inertia   <- cbind(1:R2.dim, round(eigen[1:R2.dim], 3), round(unadj.var[1:R2.dim], 1), adj.var ,cumpercent)
  colnames(adj.inertia) <- c("Dim", "Eigen", "Var" ,"Adj.Var", "Cum%")
  
  
  ### Inerti, total and expressed in rates of 1) Sum of adj.var greater than avr eigenval (Benzecri) and of 2) acerage off-diagonal inertia of the Burt-table (Greenacre)
  unadj.var     <- 100*(eigen[1:R1.dim])/sum(eigen[1:R1.dim]) # Unadjusted rates of variance

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
  freq.mod      <- colZ[subset]
  if (sum(ind.sup) >0 ) {
  freq.sup      <- colSums(Z.sup)
  }else{
  freq.sup      <- NULL
  }
  
  
  
  # Output
  ca.output <- list(nd        = R2.dim,
                    n.ind     = nrow(Z.act),
                    n.mod     = length(subset),
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
                    t.test.sup.planes = t.plane,
                    ctr.mod   = ctr.mod,
                    svd.d     = dec.full$d,
                    svd.u     = dec.full$u,
                    svd.v     = dec.full$v
                    
  )

  # Cleanup
  names(ca.output$mass.mod)         <- NULL
  dimnames(ca.output$coord.sup)     <- NULL
  names(ca.output$freq.mod)         <- NULL
  names(ca.output$freq.sup)         <- NULL
  
  return(ca.output)
}



add.count <- function(x, p, label = TRUE, ...){
  p   <- p + geom_point(data = x, x = x$X, y = x$Y, ...) + geom_path(data = x, x = x$X, y = x$Y, ...)
  if (identical(label, TRUE)) p <- p + geom_text(data = x, x = x$X, y = x$Y, label = x$label, vjust = 0.2, ...)
}


map.path_alt  <- function(object, x, map = map.ind(object, dim), dim = c(1, 2),
                          label = TRUE, min.size = length(x)/10, ...){
  
  x.c     <- x
  if (is.numeric(x)) x.c     <- min_cut(x, min.size = min.size) 
  
  x.av    <- average.coord(object = object, x = x.c, dim = dim) 
  x.av["X"] <- x.av["X"] * sqrt(object$eigen[dim[1]])
  x.av["Y"] <- x.av["Y"] * sqrt(object$eigen[dim[2]])
  map.p   <- add.count(x.av, map, label, ...) 
  map.p
}

add.count2 <- function(x, p, label = TRUE, ...){
  p   <- p + geom_point(data = x, x = x$X, y = x$Y, ...)
  if (identical(label, TRUE)) p <- p + geom_text(data = x, x = x$X, y = x$Y, label = x$label, vjust = 0.2, ...)
}


map.alt  <- function(object, x, map = map.ind(object, dim), dim = c(1, 2),
                          label = TRUE, min.size = length(x)/10, ...){
  
  x.c     <- x
  if (is.numeric(x)) x.c     <- min_cut(x, min.size = min.size) 
  
  x.av    <- average.coord(object = object, x = x.c, dim = dim) 
  x.av["X"] <- x.av["X"] * sqrt(object$eigen[dim[1]])
  x.av["Y"] <- x.av["Y"] * sqrt(object$eigen[dim[2]])
  map.p   <- add.count2(x.av, map, label, ...) 
  map.p
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

contribution <- function(object, dim = 1, all = FALSE, indices = FALSE, mode = "sort", format = FALSE){
  
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
    tab.dim_jacob(object, dim)
  }
  # Variables
  if (identical(mode, "variable")){
    tab.variable(object, dim)
  }

  if (identical(mode, "complete")){
    tab.complete(object, dim, format)
  }
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

individuals <- function(object, dim = 1, all = FALSE, indices = FALSE, label.plus = NULL, label.minus = NULL){
 
  if (identical(label.plus, NULL) == TRUE){
    label.plus    <- paste("Individuals contribution to dimension ", dim ,". (+)", sep = "")
  }
  
  if (identical(label.minus, NULL) == TRUE){
    label.minus   <- paste("Individuals contribution to dimension ", dim ,". (-)", sep = "")
  }
  
   
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
    
    outminus        <- out[which(out[, 2] < 0), ]
    outplus         <- out[which(out[, 2] >= 0), ]
   
    maxwidth   <- max(nchar(names)) + sum(nchar(colnames(out))) 
    cat("\n", format(label.plus, width = maxwidth, justify = "centre"), "\n")
    print(format(outplus, justify = "centre", width = 8))
    cat("\n", format(label.minus, width = maxwidth, justify = "centre"), "\n")
    print(format(outminus, justify = "centre", width = 8))
    
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

tab.dim_jacob <- function(x, dim = 1, label.plus = NULL, label.minus = NULL, all = FALSE){
  
  if (identical(label.plus, NULL) == TRUE){
    label.plus    <- paste("Dimension ", dim ,". (+)", sep = "")
  }
  
  if (identical(label.minus, NULL) == TRUE){
    label.minus   <- paste("Dimension ", dim ,". (-)", sep = "")
  }
  
  ctr             <- round(100 * x$ctr.mod[, dim], digits = 1)
  coord           <- round(x$coord.mod[, dim], digits = 2)
  freq            <- round(x$freq.mod, digits = 1)
  names           <- x$names.mod
  
  if (identical(all, FALSE) == TRUE){
    av.ctr        <- contribution(x, dim = dim, indices = TRUE, mode = "mod")    
  }
  if (identical(all, TRUE) == TRUE){
    av.ctr        <- seq(x$n.mod)
  }
  
  out             <- data.frame(ctr[av.ctr], coord[av.ctr], freq[av.ctr])
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
  freq.lab        <- paste("Freq")
  colnames(out)   <- c(ctr.lab, coord.lab, freq.lab)
  out             <- out[order(-out[, 1]), ]
  out.label       <- c(ctr.lab, coord.lab, freq.lab)
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

tab.complete  <- function(object, dim = 1:5, sup = FALSE, format = FALSE){
  
  headings    <- object$headings.all
  lev.head    <- unique(headings)
  variable    <- object$variable.all
  lev.var     <- unique(variable)
  head.var    <- cbind(object$headings.all, object$variable.all)
  head.var    <- head.var[!duplicated(head.var[,2]),]
  ctr.mod     <- as.matrix(object$ctr.mod.all[, dim])
  cor.mod     <- as.matrix(object$ctr.mod.all.raw[, dim])/rowSums(object$ctr.mod.all.raw)*100
  cor.mod[is.nan(cor.mod)] <- NA
  names.mod   <- object$names.mod.all
  freq.mod    <- round(object$freq.mod.all, 1)
  coord.mod <- matrix(0, nrow=ncol(object$indicator.matrix.all), ncol = length(dim))
  
  for (j in 1:length(dim)) {
    for (i in 1:ncol(object$indicator.matrix.all)) {
      col <- i
      coord.mod[i,j] <- sum(object$indicator.matrix.all[object$indicator.matrix.all[,col]>0,col]*object$coord.ind[object$indicator.matrix.all[,col]>0,j])/ sum(object$indicator.matrix.all[object$indicator.matrix.all[,col]>0,col]) /sqrt(object$eigen.raw[j])
    }
  }
  coord.mod <- coord.mod
  rownames(coord.mod) <- NULL
  names(freq.mod)  <- NULL
  var.list    <- list()
  
  for (i in seq(length(lev.var))){
    var.ctr           <- ctr.mod[variable == lev.var[i], ] * 100
    var.ctr           <- cbind(freq.mod[variable == lev.var[i]], var.ctr)
    sum               <- c("", "", colSums(var.ctr), rep("", 2 * length(dim)))
    cor               <- cbind(cor.mod[variable == lev.var[i],], rowSums(cor.mod[variable == lev.var[i],]))
    var.ctr           <- data.frame(" " = rep("", nrow(var.ctr))," " = rep("", nrow(var.ctr)), var.ctr, cor,
                                    coord.mod[variable == lev.var[i],], check.names = F)
    var.ctr[1:2]      <- lapply(var.ctr[1:2], as.character) 
    var.ctr[which(names.mod[variable == lev.var[i]] %in% object$names.passive),1] <- "(p)"
    #var.ctr           <- rbind(var.ctr, sum)
    var.ctr           <- rbind(sum, var.ctr)
    var.ctr[1, 1] <- object$Share.of.var[which(rownames(object$Share.of.var) == lev.var[i]), "Active modalities"]
    var.ctr[1, 2] <- object$Share.of.var[which(rownames(object$Share.of.var) == lev.var[i]), "Share of variance"]
    rownames(var.ctr) <- c(paste(paste(lev.var[i], "Total", sep = ": "), sep = ""), names.mod[variable == lev.var[i]])
    #rownames(var.ctr) <- c(lev.var[i], gsub(pattern = paste(lev.var[i], ": ", sep=""), replacement = "", names.mod[variable == lev.var[i]]))
    colnames(var.ctr) <- c("  Act.mod"," %tot.var", " Freq", paste("Ctr.", dim, sep = ""), paste("Cor.", dim, sep = ""), "Cor.sum", paste("Coord.", dim, sep = ""))
    var.ctr      <- data.frame(var.ctr, check.names = F)
    var.ctr[]      <- lapply(var.ctr, as.character)
    var.ctr[2:ncol(var.ctr)]   <- lapply(var.ctr[2:ncol(var.ctr)], as.numeric)
    var.list[[paste(lev.var[i])]] <- var.ctr
  }
  
  
  head.list    <- list()
  
  for (i in seq(length(lev.head))){
    head.ctr           <- var.list[head.var[which(head.var[,1] == lev.head[i]),2]]
    names(head.ctr)    <- NULL
    head.ctr           <- do.call(rbind,head.ctr)
    head.ctr.total     <- head.ctr[(which(grepl(": Total", rownames(head.ctr)))),]    
    head.ctr.total[,1] <- as.numeric(head.ctr.total[,1])
    head.ctr.total     <- colSums(head.ctr.total)
    head.ctr.total[3]  <- NA
    head.ctr           <- rbind(head.ctr.total,head.ctr)
    rownames(head.ctr)[1] <- paste("Total", lev.head[i], sep = ": ")
    colnames(head.ctr) <- c("  Act.mod"," %tot.var", " Freq", paste("Ctr.", dim, sep = ""), paste("Cor.", dim, sep = ""), "Cor.sum", paste("Coord.", dim, sep = ""))
    
    head.list[[paste(lev.head[i])]] <- head.ctr
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
      colnames(var.ctr) <- c(paste("    Dim.", dim, sep = ""), "    Freq")
      
      var.list[[paste(lev.var[i])]] <- var.ctr
    }
    
    
  }
  
  if (identical(format, TRUE)) {
  
  # The printing
  
  av.ctr      <- round(100/object$n.mod, digits = 2)
  maxwidth    <- max(nchar(names.mod))
  l           <- ncol(head.ctr)
  
  if (identical(sup, FALSE)) cat("The contribution of the active variables")
  if (identical(sup, TRUE))  cat("The coordinates of the supplementary variables")
  
  # Beautiful printing!
  act.mod <- 1
  tot.var <- 2
  freq <- 3
  ctr <- 4:((4+length(dim))-1)
  cor <- (4+length(dim)):((4+2*length(dim)-1))
  corsum <- 4+2*length(dim)
  coord <- (4+2*length(dim)+1):((4+2*length(dim)+1)+(length(dim))-1)
  
  
  
  
  for (i in seq(length(lev.head))){
    head.ctr <- head.list[[i]]
    cat("\n", "\n", format(lev.head[i], width = maxwidth), colnames(head.ctr))
    
    for (q in seq(nrow(head.ctr))){
      cat("\n", format(rownames(head.ctr)[q], width = maxwidth),
          format(as.character(head.ctr[q, act.mod]), width = 9, justify = "right"),
          format(as.character(format(head.ctr[q, tot.var], nsmall = 3)), width = 9, justify = "right"),
          format(as.character(format(head.ctr[q, freq], nsmall = 1)), width = 5, justify = "right"),
          format(as.character(format(head.ctr[q, ctr], nsmall=1)), width = 5, drop0trailing = TRUE, justify = "right"),
          format(as.character(format(head.ctr[q, cor], nsmall = 1)), width = 5, drop0trailing = TRUE, justify = "right"),
          format(as.character(format(head.ctr[q, corsum]), nsmall=1), width = 5, drop0trailing = TRUE, justify = "right"),
          format(as.character(format(head.ctr[q, coord]), nsmall=3), width = 5, drop0trailing = TRUE, justify = "right"))
            }
    
  }
  
  
  if (identical(sup, FALSE)) cat("\n", "\n", "Average contribution per modality: ", av.ctr, sep = "")
  if (identical(sup, FALSE)) cat("\n", "Average contribution per variable: ", round(100/length(lev.var),1), sep = "")
  if (identical(sup, FALSE)) cat("\n", "Average contribution per heading: ", round(100/length(lev.head),1), sep = "")
  cat("\n", "Total number of individuals: ", object$n.ind, sep = "")
  }else{
    
head.list2 <- do.call(rbind,head.list)    
rn <- unlist(lapply(head.list, rownames))
rownames(head.list2) <- c(rn)   
  }
  class(head.list) <- "tab.complete"
  invisible(head.list)
}


indicator_jacob  <- function(x, id = NULL, ps = ": "){
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

csa.all_jacob <- function(object, variable, dim = 1:5, ...){
  lev.variable <- levels(variable)
  result.list     <- list()
  for (i in 1:length(lev.variable)){
    dummy.class         <- which(variable == lev.variable[i])
    result.list[[i]]    <- soc.csa_jacob(object, class.indicator = dummy.class)
  }
  
  names(result.list) <- lev.variable
  
  measure.list <- lapply(result.list, csa.measures, format = FALSE, dim = dim, ...)
  
  list(results = result.list, measures = measure.list)
}



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
  Share.of.var <- round((x$modal[, "Nb. active modalities"] - 1)/ (length(x$names.passive) + Nmodal - nrow(x$modal)) * 100, 1) 
  Vnames       <- paste(rownames(x$modal), " [", x$modal[, "Nb. active modalities"], " - ", format(Share.of.var), "%]", sep = "")
  Vnames       <- Vnames[order(Share.of.var, decreasing = TRUE)]
  Submass 	   <- 100 - round(sum(x$mass.mod) *100, digits = 1) 
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
      
      format("	Share of passive mass in %:",	 		width = 40,), format(Submass, 	width = 10, justify = "right"), 
      format("|  6.", width = 10, justify = "centre" ), format(adj[6], width = 10, justify = "centre"), format(paste(stars[[6]]), width = 1), "\n",
      
      format("\tNumber of passive modalities:",	 		width = 40, ), format(N.pas.mod, 	width = 10, justify = "right"), 
      format("|  7.", width = 10, justify = "centre" ), format(adj[7], width = 10, justify = "centre"), format(paste(stars[[7]]), width = 1), "\n",
      
      "\n",
      format(paste("The", length(Vnames),"active variables: [No. modalities - share of variance]"), 			width = 100, justify = "centre" ),
      "\n",
      "\n",
      sep = "")
  cat(format(Vnames, width = 25, justify = "right"), fill = 100)
  
}








outtable.mod <- function(object, dim = c(1:3)){
  
names <- names(object)  

x <- lapply(object, function(x) apply(round(x$ctr.mod[,dim]*100, 2), 1, max))
for (i in 1:length(x)) {
x[[i]][which(x[[i]] >= 100/length(x[[i]]))] <- "Yes"  
x[[i]][which(x[[i]] < 100/length(x[[i]]))] <- "No"  
  }
  
res <- lapply(object, function(x) data.frame(round(x$freq.mod,1), round(x$coord.mod[,dim],3), round(x$ctr.mod[,dim]*100,2), round(x$cor.mod[,dim]*100,1)))

  #out <- data.frame(result.all, result.CSA1, result.CSA2, result.CSA3, result.CSA4, result.CSA5, x1, x2, x3, x4, x5, x6)

tmp1 <- do.call("cbind", res)
tmp2 <- do.call("cbind", x)
out <- cbind(tmp1, tmp2)

colnam <- NULL
for (i in 1:length(names)){
c <- c(paste(names[i], ": freq"), paste(names[i], dim, sep = ": coord, dim"), paste(names[i], dim, sep = ": ctr, dim"), paste(names[i], dim, sep = ": cor, dim"))
colnam <- c(colnam, c)
}
above <- paste(names, "mod > avr on any dim", sep = ": ")
colnam <- c(colnam, above)
colnames(out) <- colnam

out  
}


outtable.var <- function(objects, dim = 1:3) {
  
  names <- names(objects)
  
  tab    <- function(object, dim = dim, sup = FALSE){
    
    
    variable    <- as.factor(object$variable)
    ctr.mod     <- as.matrix(object$ctr.mod[, dim])
    lev.var     <- levels(variable)
    names.mod   <- object$names.mod
    freq.mod    <- object$freq.mod
    
    var.list    <- matrix(,nrow=length(lev.var), ncol=length(dim))
    
    for (i in seq(length(lev.var))){
      var.ctr           <- round(ctr.mod[variable == lev.var[i], ] * 100, digits = 1)
      var.ctr           <- colSums(var.ctr)
      var.list[i,]      <- var.ctr
      colnames(var.list) <- c(paste(" Dim.", dim, sep = ""))
      rownames(var.list) <- lev.var
    }
    
    var.list
  }
  
  v <- vector(,length = length(unique((result$variable))))
  for (i in 1:length(tmp)) {
    x <- tab(tmp[[i]], dim=1:3)
    colnames(x) <- paste(paste(names[i], "dim: "), dim, sep="")
    v <- cbind(v, x)
  }
  
  v <- v[,-1]
  
  write.csv(v, file = "Output/MCA og CSA var.csv")
}



transition.coord <- function(object, modality, mode = "regular", dim = 1:3){
  if (mode == "regular") {
    coord.i <- object$coord.ind[object$indicator.matrix.active[,modality] > 0,dim]
    coord <- 1/sqrt(object$eigen[dim]) * (colSums(coord.i) / object$freq.mod[object$names.mod %in% modality])
  }
  if (mode == "specific") {
    coord.i <- object$coord.ind[object$indicator.matrix.active[,modality] > 0,dim]
    coord <- 1/sqrt(object$eigen[dim]) * (colSums(coord.i) / object$freq.mod[object$names.mod %in% modality])
  }
  if (mode == "class specific") {
    coord.i <- object$coord.ind[object$indicator.matrix[,modality] > 0,dim]
    coord <- (1/sqrt(object$eigen[dim])) * colSums(coord.i) / (object$n.ind * (object$freq.mod.org[object$names.mod %in% modality]/object$n.ind.org))
  }
  coord
}


reconstruct <- function(obj, nd = 1:length(obj$svd.d)) {
  d <- matrix(0, nrow = obj$n.ind, ncol = obj$n.mod)
  diag(d) <- c(obj$svd.d[nd], rep(0, length(diag(d)) - length(nd)))
  S  <- obj$svd.u %*% d %*% t(obj$svd.v)
  cm <- colSums(obj$indicator.matrix.all) / (obj$n.ind*obj$Q)
  rm <- rowSums(obj$indicator.matrix.all) / (obj$n.ind*obj$Q)
  eP <- rm %*% t(cm)
  P  <- S * sqrt(eP) + eP
  data <- round(P * (obj$n.ind*obj$Q),5)
  data
  }