## Diverse ubrugte funktioner


# Balancemål for mass
balance.mass <- function(object, act.dim=object$nd){
aktive <- object$analyse
coord <- object$coord[aktive, 1:act.dim]
mass <- object$mass
pm <- matrix(, nrow=act.dim, ncol=3)
for (i in 1:act.dim){
nif <- cbind(coord[,i], mass)
nif <- nif[order(nif[,1]),]
plus <-  nif[which(nif[,1] >= 0), ]
minus <- nif[which(nif[,1] <= 0), ]
pmass <- sum(plus[,2])
mmass <- sum(minus[,2])
pm[i,1] <- pmass
pm[i,2] <- mmass
pm[i,3] <- pmass/mmass
}
colnames(pm) <- c("+ Mass", "- Mass", "+/-")
return(pm)
}

# Den her funktion viser hvor meget af dim80 inertien der forklares af variablene
#
varinerti <- function(object, dim=6){
modal   	<- object$modal
inerti 		<- object$inertia[1:dim]
sumine 		<- sum(inerti)

ctr 		<- round(100*object$contrib[,1:dim], digits=2)
tab 		<- matrix(, ncol=length(1:dim), nrow=nrow(modal))
for (i in 1:nrow(modal)){
  mf 		<- modal[i,1]:modal[i,2]
  tab[i,] 	<- apply(ctr[mf,], 2, sum)
}
}

























##################### Test.data 

test.data <- function(x, sup=NULL, identifier=NULL, passive="default"){
  
  # Defining the passive modalities
  if (identical(passive, "default")==TRUE){
    passive <-  formals(soc.ca)$passive
  }
  
  # Factor test
  active.names <- colnames(x)    
  fact <- unlist(lapply(x, is.factor))
  fact <- active.names[which(fact==FALSE)]
  
  if (identical(sup, NULL)==FALSE){
    sup.names <- colnames(sup)    
    fact.sup <- unlist(lapply(sup, is.factor))
    fact.sup <- active.names[which(fact.sup==FALSE)]
  }
  
  # Test unique identifier
  
  if (identical(identifier, NULL)==FALSE){
    n.dup <- length(identifier[duplicated(identifier)])
  }
  
  # Test for rare modalities
  # This test only makes sense if all active modalities are factors.
  n5test      <- integer(0) 
  if (length(fact)==0){
    act.ind     <- indicator(x)
    
    sub         <- grepl(paste(passive, collapse="|"), colnames(act.ind)) 
    sub.ind     <- act.ind[, sub==TRUE]
    act.ind     <- act.ind[, sub==FALSE] # Subsetting
    
    modal.names <- colnames(act.ind)
    n           <- nrow(x)
    col.sum     <- apply(act.ind, 2, sum)
    col.per     <- col.sum/n
    n5          <- which(col.sum <= 4)     
    n5per       <- which(col.per <= 0.05)
    n5test      <- unique(c(n5, n5per))
    n5names     <- modal.names[n5test] 
  }
  
  
  ######### Number of passive modalities per. individual
  sub.mat     <- integer(0) 
  if (length(fact)==0){
    row.freq    <- table(apply(sub.ind, 1, sum))
    row.prop    <- round(prop.table(row.freq), 2)
    row.cum     <- cumsum(row.prop)
    sub.mat     <- matrix(nrow=length(row.prop), ncol=3) 
    rownames(sub.mat) <- dimnames(row.freq)[[1]]
    sub.mat[,1] <- row.freq
    sub.mat[,2] <- row.prop
    sub.mat[,3] <- row.cum
    colnames(sub.mat) <- c("Freq.", "%", "Cum.%" )
  }
  
  
  
  # Test for "logical" correlations between modalities
  # This test only makes sense if all active modalities are factors.
  
  var.test    <- matrix(nrow=0, ncol=3)  
  colnames(var.test) <- c("X", "Y", "%")
  if (length(fact)==0){
    burt.abc    <- burt(x)
    # Subsetting
    sub         <- grepl(paste(passive, collapse="|"), colnames(burt.abc))
    burt.abc    <- burt.abc[sub==FALSE, sub==FALSE]
    
    d           <- diag(burt.abc)
    bd          <- round((burt.abc/d)*100) # We take the diagonal and divide the burt matrix with it.
    bnames      <- colnames(burt.abc)
    for (i in 1:nrow(bd)){
      bd1     <- which(bd[i,]>=95) # This sets the threshold
      bd1     <- bd1[bd1!=i]
      if (length(bd1)>=1){
        varrow  <- cbind(bnames[i], bnames[bd1], bd[i, bd1])
        var.test <- rbind(var.test, varrow)
      }
    }
    rownames(var.test) <- rep("", nrow(var.test))
  }
  
  
  ######### Print test-results
  
  # Factor test
  if (length(fact)>=1){
    cat("\n",format("Following active variables are not factors: ", width=100, justify="centre"), "\n", "\n")
    cat(format(fact, width=25, justify="right"), fill=100)
    cat("\n", "\n",format("The active variables were not tested for rare modalities or strong correlations; make them factors and try again.", width=100, justify="centre"), "\n")
  }
  
  if (length(fact)==0){
    cat("\n",format("All active variables are factors", width=100, justify="centre"), "\n")
  }
  
  if (length(fact.sup)>=1){
    cat("\n",format("Following supplementary variables are not factors: ", width=100, justify="centre"), "\n", "\n")
    cat(format(fact.sup, width=25, justify="right"), fill=100)
  }
  
  if (length(fact.sup)==0 & (identical(sup, NULL)==FALSE)){
    cat("\n",format("All supplementary variables are factors", width=100, justify="centre"), "\n")
  }
  
  # Print rare modalities
  
  if (((length(fact)==0) & (length(n5test)>=1))==TRUE){
    cat("\n",format("Following modalities are rarer than 5% or 5 individuals: ", width=100, justify="centre"), "\n", "\n")
    cat(format(n5names, width=25, justify="right"), fill=100)
  }    
  
  if ((length(n5test)==0) & (length(fact)==0)) {
    cat("\n",format("No modalities are rarer than 5% or 5 individuals.", width=100, justify="centre"), "\n")
  }    
  
  # Print duplicated
  if ((identical(identifier, NULL)==FALSE) & (length(n.dup)>=1)){
    cat("\n",format(paste("There are ", n.dup, " duplicates in the identifier variable", sep="") , width=100, justify="centre"), "\n")
  }
  
  if ((identical(identifier, NULL)==FALSE) & (length(n.dup)==0)){
    cat("\n", format("There are no duplicates in the identifier variable", width=100, justify="left"), "\n")
  }
  
  # Print correlation
  
  if (nrow(var.test)>=1){
    cat("\n",format("Following modalities are too strongly related: ", width=100, justify="centre"), "\n", "\n")
    print(var.test, quote=FALSE, right=TRUE) # Bedre layout
  }
  
  if ((nrow(var.test)==0) & (length(fact)==0)) {
    cat("\n",format("No modalities are too strongly related.", width=100, justify="centre"), "\n")
  }    
  
  # Print number of passive modalities
  
  if (identical(sub.mat, integer(0))==FALSE) {
    cat("\n",format("Number of individuals per amount of passive modalities:", width=100, justify="centre"), "\n")
    print(sub.mat, quote=FALSE, right=TRUE, print.gap=3) # Bedre layout
  }
  
  # End of function - insert documentation
}



