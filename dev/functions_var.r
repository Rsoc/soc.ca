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
library(soc.ca)
example(soc.ca)

# When test.data is done - remove the forced conversions to factor in soc.mca - it affects the order of the levels.



x             <- active
x[, 2]        <- as.character(x[, 2])   # Fail: Not a factor
x[5:6, 1:3]   <- NA                     # Fail: NA's
levels(x[,3]) <- c(levels(x[,3]), "Empty") # Tomme levels 
identifier    <- 1:nrow(active)
identifier[3:4] <- 1:2                  # Fail: Not unique

sup[,2]       <- as.character(sup[,2])  # Fail: Not a factor


nrow(x)
soc.mca(x, sup, identifier)

test.data <- function(x, identifier = 1:nrow(x), passive = getOption("passive", default = "Missing")){
  
  # Factor test
  not.factor             <- function(x){
    
  active.names           <- colnames(x)    
  fact                   <- unlist(lapply(x, is.factor))
  fact                   <- active.names[which(fact==FALSE)]
  fact
  }

  
  # Test unique identifier
unique.identifier        <- function(identifier){
      tab.id             <- table(identifier)
      mat.id             <- as.matrix(tab.id[tab.id > 1])
      mat.id             <- data.frame("Id" = rownames(mat.id),  "Duplicates" = mat.id)
      rownames(mat.id)   <- NULL
      if (sum(tab.id > 1) == 0) mat.id <- "No duplicated identifiers"
      mat.id
  }

  # Test for rare modalities
  # This test only makes sense if all active modalities are factors.
x <- active
n.cut = 5
share.cut = 0.05
passive = getOption("passive", default = "Missing")

 too.small     <- function(x, n.cut = 5, share.cut = 0.05, passive = getOption("passive", default = "Missing")){

    act.ind     <- indicator(x)
    sub         <- grepl(paste(passive, collapse="|"), colnames(act.ind)) 
    sub.ind     <- act.ind[, sub == TRUE]
    act.ind     <- act.ind[, sub == FALSE] # Subsetting
    modal.names <- colnames(act.ind)
    n           <- nrow(x)
    col.sum     <- apply(act.ind, 2, sum)
    col.per     <- col.sum/n
    
    out         <- data.frame("Name" = modal.names,"N" = col.sum, "Share" = round(col.per, 2), row.names = NULL)
    out[col.sum < n.cut | col.per < share.cut,]
    }
  
  
  ######### Number of passive modalities per. individual
 passive.categories.per.individual <- function(x, identifier, share.cut = 1/3, passive = getOption("passive", default = "Missing")) 
 
   act.ind     <- indicator(x)
   sub         <- grepl(paste(passive, collapse="|"), colnames(act.ind)) 
   sub.ind     <- act.ind[, sub]
   n.passive   <- rowSums(sub.ind)
   share.passive <- n.passive / ncol(x)
   passive.mat   <- data.frame("Id" = identifier, "Passive" = n.passive, "Share" = round(share.passive, 2))
   passive.mat[share.passive > share.cut, ]
   
  }
  
  overlapping.categories <- function(x, passive = passive = getOption("passive", default = "Missing")){
   ind.all               <- indicator(x)
   sub                   <- grepl(paste(passive, collapse="|"), colnames(act.ind)) 
   ind.act               <- ind.all[, -sub]
   burt                  <- t(ind.act) %*% ind.act  
   View(burt / diag(burt))
    
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
  not.factor(x)
  
  # Unique identifier
  unique.identifier(identifier)
  
  # Too small categories
  too.small(x.factor, n.cut = 5, share.cut = 0.05, passive = passive)
  
  # Too many passive categories for an individual
  passive.categories.per.individual(x.factor, share.cut = 1/3, passive = passive)
  
 
  # Print correlation
  
  if (nrow(var.test)>=1){
    cat("\n",format("Following modalities are too strongly related: ", width=100, justify="centre"), "\n", "\n")
    print(var.test, quote=FALSE, right=TRUE) # Bedre layout
  }
  
  if ((nrow(var.test)==0) & (length(fact)==0)) {
    cat("\n",format("No modalities are too strongly related.", width=100, justify="centre"), "\n")
  }    
  

}



