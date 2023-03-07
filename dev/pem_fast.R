example(soc.mca)
x <- active$TV
y <- active$Film

pem_fast<- function(x, y, weights = rep(1, length(x)), digits = 1, sort = FALSE){
  idnona <- !is.na(x) & !is.na(y)
  X <- x[idnona]
  Y <- y[idnona]
  W <- weights[idnona]
  # cont <- t(as.matrix(GDAtools::dichotom(X, out = "numeric"))) %*% 
  #   diag(W) %*% as.matrix(GDAtools::dichotom(Y, out = "numeric"))
  cont <- xtabs(data = data.frame(X, Y), ~X+Y)
  
  # dimnames(cont2) <- dimnames(cont)
  # all.equal(cont, as.matrix(cont2))
  # cont2 - cont
  # 
  tota <- colSums(cont)
  totb <- rowSums(cont)
  total <- sum(cont)
  theo <- matrix(nrow = nrow(cont), ncol = ncol(cont))
  for (i in 1:nrow(cont)) {
    for (j in 1:ncol(cont)) theo[i, j] <- tota[j] * totb[i]/total
  }
  ecart <- cont - theo
  max <- matrix(nrow = nrow(cont), ncol = ncol(cont))
  emax <- matrix(nrow = nrow(cont), ncol = ncol(cont))
  pem <- matrix(nrow = nrow(cont), ncol = ncol(cont))
  for (i in 1:nrow(cont)) {
    for (j in 1:ncol(cont)) {
      if (ecart[i, j] >= 0) 
        max[i, j] <- min(tota[j], totb[i])
      if (ecart[i, j] < 0 & tota[j] <= (total - totb[i])) 
        max[i, j] <- 0
      if (ecart[i, j] < 0 & tota[j] > (total - totb[i])) 
        max[i, j] <- tota[j] + totb[i] - total
      emax[i, j] <- max[i, j] - theo[i, j]
      pem[i, j] <- ifelse(ecart[i, j] >= 0, ecart[i, j]/emax[i, 
                                                             j] * 100, 0 - ecart[i, j]/emax[i, j] * 100)
    }
  }
  dimnames(pem) <- dimnames(cont)
 
  pem <- as.table(pem)
  rownames(pem) <- gsub("data.", "", rownames(pem))
  colnames(pem) <- gsub("data.", "", colnames(pem))
  round(as.table(pem), digits)
}

pem_fast(x,y) - pem(x,y)$peml


