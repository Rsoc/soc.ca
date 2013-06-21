
sample.test <- function(result, active, dim=1:5, sample.sizes=seq(0.01, 0.5, by=0.01)){ 
  
  
  sub.sample.test <- function(result, active, dim=1:10, sample.size){
    sample.id   <- 1:nrow(active)
    sample.id   <- sample.id[-sample(sample.id, size=nrow(active)*sample.size, replace=TRUE)]
    active.sample <- active[sample.id, ]
    result.sample <- soc.ca(active.sample)
    
    test             <- matrix(ncol=2, nrow=max(dim))
    colnames(test)   <- c("Ctr","Av")
    ctr              <- result$ctr.mod[,dim]
    ctr.sample       <- result.sample$ctr.mod[,dim]
    
    
    # Ctr
    ctr.diff <- ctr - ctr.sample
    ctr.diff[ctr.diff < 0] <- NA
    test[,1] <- round(colSums(ctr.diff, na.rm=TRUE) * 100, 1)
    
    # Above average
    av          <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
    av.sample   <- as.vector(apply(as.matrix(ctr.sample), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
    av.res <- vector(length=max(dim))
    for ( i in dim){
      a            <- av[[i]]
      b            <- av.sample[[i]]
      av.res[i]    <- round((1 - sum(a %in% b)/length(a))*100, 1)
    }
    test[,2]     <- av.res
    test
  }
  
  require(reshape)
  
  ctr.test <- matrix(nrow=length(dim), ncol=length(sample.sizes))
  av.test  <- matrix(nrow=length(dim), ncol=length(sample.sizes))
  for( i in seq(sample.sizes)){
    test.res  <-  sub.sample.test(result, active, dim=dim, sample.size=sample.sizes[i])
    ctr.test[,i] <- test.res[,1]
    av.test[,i]  <- test.res[,2]
  }
  
  
  ctr.test <- t(as.data.frame(cbind(ctr.test)))
  rownames(ctr.test) <- sample.sizes
  melt.ctr <- melt(ctr.test)
  p.c   <- ggplot(data=melt.ctr, aes(x=X1, y=value, colour=as.factor(X2), group=X2)) + geom_smooth() + theme_bw() + ggtitle("Test of all ctr.") #+ geom_point(shape=4, alpha=0.5)
    
  av.test <- t(as.data.frame(cbind(av.test)))
  rownames(av.test) <- sample.sizes
  melt.av <- melt(av.test)
  p.av<- ggplot(data=melt.av, aes(x=X1, y=value, colour=as.factor(X2), group=X2)) + geom_smooth() + theme_bw() + ggtitle("Test of changes across the above average threshold")
  
  print(p.av)
  print(p.c)
    
}

