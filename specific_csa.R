##### CSA

# Problemer

# Er det et problem at indikatormatricen ikke indeholder de subsettede modaliteter?

soc.csca <- function(result, class.indicator){
  
#   dat.act <- active
#   dat.sup <- sup
#   
#   I       <- dim(dat.act)[1]
#   Q       <- dim(dat.act)[2]
#   
#   lev.n  <- unlist(lapply(dat, nlevels))      				# Variable + modaliteter
#   n      <- cumsum(lev.n)								# Kumulerede sum af modaliteter
#   J.t    <- sum(lev.n)									# Summen
#   Q.t    <- dim(dat)[2]									# Variable
#   Z      <- matrix(0, nrow = I, ncol = J.t)
#   newdat <- lapply(dat, as.numeric)
#   offset <- (c(0, n[-length(n)]))
#   for (i in 1:Q.t) Z[1:I + (I * (offset[i] + newdat[[i]] - 1))] <- 1
#   fn     <- rep(names(dat), unlist(lapply(dat, nlevels)))
#   ln     <- unlist(lapply(dat, levels))
#   dimnames(Z)[[2]] <- paste(fn, ln, sep = "")
#   dimnames(Z)[[1]] <- as.character(1:I)
#   ind.temp  <- range(n[sup.ind])
#   Z.sup.ind <- (ind.temp[1]-1):ind.temp[2]
#   Z.act     <- Z[,-Z.sup.ind]
#   J         <- dim(Z.act)[2]
  
  
  Z.act  <- result$indicator.matrix 
  Q      <- nlevels(as.factor(result$variable))
  I      <- nrow(Z.act)
  ## Eksempel ud fra alder = "55-64?rige"
  # Her er vi ikke længere abstrakte...
  #Z.hat = Z.act[which(dat$Age =='55-64'),]
  #i     = dim(dat.sup[which(dat$Age =='55-64'),])[1]
  
  Z.hat = Z.act[class.indicator,]
  i     = length(class.indicator)
  
  # i <- length(class.indicator) 
  cm   <- apply(Z.hat, 2, sum)
  CM   <- apply(Z.act, 2, sum)
  
  H.hat <- matrix(,nrow=i, ncol=length(cm))
  # Nu gør vi det i et loop, men det kan nok gøres med apply
  for (k in seq(cm)){
    H.hat[,k] <- (1/sqrt(Q)) * (sqrt(I/i)) * (Z.hat[, k]-(cm[k]/i)) * (1/sqrt(CM[k]))
  }
  
  colnames(H.hat) = colnames(Z.hat)
  modal.names     <- colnames(Z.hat)
  
  H.svd = svd(H.hat)
  ### Modalitetskoordinater
  #csa.m.coord     	= H.svd$d[1] * H.svd$v[,1] * (1/sqrt(CM )) * (sqrt(Q*I)) # Kun en dim
  
  csa.m.coord     <- matrix(nrow=nrow(H.svd$v), ncol=ncol(H.svd$v))
  for (ff in 1:length(H.svd$d)){
    csa.m.coord[,ff]         = H.svd$d[ff] * H.svd$v[,ff] * (1/sqrt(CM )) * (sqrt(Q*I))
  } 
  
  rownames(csa.m.coord) <- modal.names
  ### Individkoordinater
  # csa.i.coord     	= sqrt(i) * H.svd$u[,1] * H.svd$d[1] # Kun en dim
  csa.i.coord     <- matrix(nrow=nrow(H.svd$u), ncol=ncol(H.svd$u))
  for (ff in 1:length(H.svd$d)){
    csa.i.coord[,ff]     = sqrt(i) * H.svd$u[,ff] * H.svd$d[ff]
  }
  
  ### Modalitetsbidrag
  csa.m.ctr = (((CM/I)/Q) * (csa.m.coord)^2)/(H.svd$d[1]^2)*100
  
  csca.resultat <- list(modal.coord=csa.m.coord, individ.coord=csa.i.coord, modal.ctr=csa.m.ctr)
  return(csca.resultat)
} 



