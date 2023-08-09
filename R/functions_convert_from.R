from.pca.to.soc.ca <- function(result){
  
  result$coord.mod  <- result$var$coord
  result$coord.ind  <- result$ind$coord
  
  result$ctr.mod    <- result$var$contrib
  result$ctr.ind    <- result$ind$contrib
  
  result$weight     <- result$call$row.w
  
  result$freq.mod   <- rep(nrow(result$var$coord), ncol(result$var$coord))
  result$names.mod  <- rownames(result$var$coord)
  
  result$variable   <- rownames(result$var$coord)
  result$labels.mod <- rownames(result$var$coord)
  
  result$eigen      <- result$eig[,"eigenvalue"] %>% as.numeric()
  result$n.ind      <- nrow(result$call$X)
  result
  
} 
