#### Functions for describtion and printing


# Print objects from the soc.ca function
print.soc.ca <- function(object){
Nmodal       <- length(object$active) 
Nsup     	<- length(object$sup) # S?t if ind
Nid 		<- object$nid
Vnames 		<- paste(rownames(object$modal), " (",object$modal[,3], ")", sep="")
Submass 	<- round(sum(object$mass), digits=2) # Er det her i Procent aka. summer mass altid til 1.

act.dim 	<- nrow(object$adj.inertia)
dim80 		<- which.min(object$adj.inertia[,4] < 80)
scree.dim	<- 6
stars 		<- scree(object, scree.dim)
adj.dim   <- 1:scree.dim
#adj.dim 	<- object$adj.inertia[1:scree.dim,1]
dim.a   <- ifelse((scree.dim<nrow(object$adj.inertia)), scree.dim, nrow(object$adj.inertia))
adj <- vector(mode="numeric", length=scree.dim) # Måske skal den ikke smide 0 som output men noget bedre, når der ikke er flere dim end 6
adj[1:dim.a] <- object$adj.inertia[1:dim.a,3]
adj	<- paste(formatC(adj,format="f",digits=1), sep="", collide="%")
## Tests
pm 		<- balance.ctr(object, act.dim)
pm.minus <- which(as.numeric(pm[,3])<0.5)
pm.plus <- which(as.numeric(pm[,3])>2)

## Output
cat(format("Specific Correspondence Analysis:", 	width=90, justify="centre"),"\n", "\n", 
format("Statistics", 					width=50, justify="centre"), format("Scree plot", width=40, justify="centre"),"\n",

format("	Active dimensions: ", 			width=40,), format(act.dim, 	width=10, justify="right"),
format("|  1.", width=10, justify="centre" ), format(adj[1], width=10, justify="centre"), format(paste(stars[[1]]), width=1), "\n",

format("	Dimensions explaining 80% of inertia: ",width=40,), format(dim80, 	width=10, justify="right"), 
format("|  2.", width=10, justify="centre" ), format(adj[2], width=10, justify="centre"), format(paste(stars[[2]]), width=1), "\n",

format("	Active modalities: ", 			width=40,), format(Nmodal, 	width=10, justify="right"), 
format("|  3.", width=10, justify="centre" ), format(adj[3], width=10, justify="centre"), format(paste(stars[[3]]), width=1), "\n",

format("	Supplementary modalities: ",		width=40,), format(Nsup, 	width=10, justify="right"), 
format("|  4.", width=10, justify="centre" ), format(adj[4], width=10, justify="centre"), format(paste(stars[[4]]), width=1), "\n",

format("	Individuals: ",		 		width=40,), format(Nid, 	width=10, justify="right"), 
format("|  5.", width=10, justify="centre" ), format(adj[5], width=10, justify="centre"), format(paste(stars[[5]]), width=1), "\n",

format("	Mass in subset",	 		width=40,), format(Submass, 	width=10, justify="right"), 
format("|  6.", width=10, justify="centre" ), format(adj[6], width=10, justify="centre"), format(paste(stars[[6]]), width=1), "\n",

"\n",
format("The active variables:", 			width=100, justify="centre" ),
"\n",
sep="")
cat(format(Vnames, width=25, justify="right"), fill=100)

# Test


if (length(pm.plus)>0){
  p.out <- matrix(, ncol=3, nrow=length(pm.plus))
  p.out[,] <- formatC(pm[pm.plus,], format="f", digits=2)
  rownames(p.out) <- rep(format("", width=35), times=length(pm.plus))  
  p.out <- cbind(paste(pm.plus, ".", sep=""), p.out)
  colnames(p.out) <- c("Dim.", "+", "-", "+/-")
  cat("\n",format("These dimensions contributions are skewed towards (+): ", width=100, justify="centre"), "\n")
  print(p.out, quote=FALSE, right=TRUE)
}      

if (length(pm.minus)>0){
  p.out <- matrix(, ncol=3, nrow=length(pm.minus))
  p.out[,] <- formatC(pm[pm.minus,], format="f", digits=2)
  rownames(p.out) <- rep(format("", width=35), times=length(pm.minus))
  p.out <- cbind(paste(pm.minus, ".", sep=""), p.out)
  colnames(p.out) <- c("Dim.", "+", "-", "+/-")
  cat("\n",format("These dimensions contributions are skewed towards (-): ", width=100, justify="centre"), "\n")
  print(p.out, quote=FALSE, right=TRUE)
}

}



#################### Balance measure for contributing modalities
# Object is a soc.ca class object
# act.dim is the number of active dimensions to be measured

balance.ctr <- function(object, act.dim=object$nd){
active <- object$active
coord <- object$coord[active, 1:act.dim]
contrib <- object$contrib[active, 1:act.dim]
pm <- matrix(, nrow=act.dim, ncol=3)
for (i in 1:act.dim){
temp <- cbind(coord[,i], contrib[,i])
temp <- temp[order(temp[,1]),]
plus <-  temp[which(temp[,1] >= 0), ]
minus <- temp[which(temp[,1] <= 0), ]
pcontrib <- sum(plus[,2])
mcontrib <- sum(minus[,2])
pm[i,1] <- pcontrib
pm[i,2] <- mcontrib
pm[i,3] <- pcontrib/mcontrib
}
colnames(pm) <- c("+ Contrib.", "- Contrib.", "Balance (+/-)")
return(pm)
}

#################### Screeplot
# Object is a soc.ca class object
# Dim is the number of dimensions included in the plot

scree <- function(object, dim=6){
set.dim  <- dim
dim <- ifelse((nrow(object$adj.inertia)<dim)==TRUE, nrow(object$adj.inertia), dim)
adj <- round(object$adj.inertia[1:dim,3], digits=1)
stars <- round(round(adj)/2)
starscree <- vector("list", set.dim)
for (i in 1:length(stars)){
starscree[[i]] <- noquote(rep("*", stars[i]))
}
return(starscree)
}

# En funktion der undersøger om der er modaliteter der styrer en dimension for meget. 
#Kritik - den vil spytte noget ud i et væk, hvis ikke den holdes nede til kun at undersøge et meget begrænset subset. Måske skal den gemmes til senere.
# Modkritik: Hvis en modalitet opsnapper al inertien på en dimension, så behøver de andre dimensioner ikke at tage højde for den på samme måde og det kan give et skævt kort.??

############################ The most contributing modalities
contribution <- function(object, dim=1, all=FALSE, modality.indices=FALSE){
    if(identical(modality.indices, TRUE)==TRUE){
        ctr     <- object$contrib[,dim]
        av.ctr  <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
        if(is.list(av.ctr)==TRUE) av.ctr  <- unlist(av.ctr[dim], use.names=FALSE)
        av.ctr     <- av.ctr[duplicated(av.ctr)==FALSE]    
        return(av.ctr)
    }else{
        ctr <- round(1000*object$contrib[,dim])
        cor <- round(1000*object$cor[,dim])
        coord <- round(object$coord[,dim], digits=2)
        names <- object$names
        if (identical(all, FALSE)==TRUE){
            av.ctr<- contribution(object, dim=dim, modality.indices=TRUE)    
            header <- paste("The modalities contributing above average to dimension: ", dim, ".", sep="")
        }
        if (identical(all, TRUE)==TRUE){
            av.ctr<- object$active  
            header <- paste("The contribution of all modalities to dimension: ", dim, ".", sep="")
        }
        out <- data.frame(ctr[av.ctr], cor[av.ctr], coord[av.ctr])
        rownames(out) <- names[av.ctr]
        colnames(out) <- c("   Ctr.", "   Cor." , "   Coord")
        out <- out[order(-out[,1]), ]
        maxwidth <- max(nchar(names))+sum(nchar(colnames(out)))
        cat("\n", format(header, width=maxwidth, justify="centre"), "\n", "\n")
        print(out)
    }
    # Returns the modalities with above average contribution to the selected dimension
    # object is a soc.ca object
    # dim is the included dimensions
    # all: If TRUE returns all modalities instead of just those that contribute above average
    # modality.indices: If TRUE returns a vector with the row indices of the modalities
    # Ctr is the contribution in 1000
    # Cor is the correlation with the dimension
    # Coord is the principal coordinate
}

############################## The most contributing modalities according to direction on dimension
# x is a soc.ca object
# dim is the dimension

tab.dim <- function(x, dim=1, label.plus=NULL, label.minus=NULL, all=FALSE){
    
    if (identical(label.plus, NULL)==TRUE){
        label.plus    <- paste("Dimension ", dim ,". (+)", sep="")
    }
    
    if (identical(label.minus, NULL)==TRUE){
        label.minus   <- paste("Dimension ", dim ,". (-)", sep="")
    }
    
    ctr <- round(1000*x$contrib[,dim])
    coord <- round(x$coord[,dim], digits=2)
    names <- x$names
    
    if (identical(all, FALSE)==TRUE){
        av.ctr<- contribution(object, dim=dim, modality.indices=TRUE)    
    }
    if (identical(all, TRUE)==TRUE){
        av.ctr <- object$active  
    }
    
    out <- data.frame(ctr[av.ctr], coord[av.ctr])
    names <- names[av.ctr]
    maxwidth    <- max(nchar(rownames(out)))
    
    for (i in seq(names)){
        width       <- maxwidth-nchar(names[i])
        fill        <- paste(rep(" ", width), sep="", collapse="")
        names[i]    <- paste(names[i], fill, sep="", collapse="")
    }
    rownames(out) <- names
    ctr.lab     <- paste("Ctr")
    coord.lab   <- paste("Coord")
    colnames(out)  <- c(ctr.lab, coord.lab)
    out         <- out[order(-out[,1]), ]
    out.label     <- c(ctr.lab, coord.lab)
    outminus    <- out[which(out[,2]<=0),]
    outplus 	<- out[which(out[,2]>=0),]
    
    
    #outputwidth <- maxwidth+8*3
    cat("\n",format(label.plus, width=maxwidth, justify="centre"), "\n")
    print(format(outplus, justify="centre", width=8))
    cat("\n",format(label.minus, width=maxwidth, justify="centre"), "\n")
    format(outminus, justify="centre", width=8)
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


