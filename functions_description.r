#### Functions for describtion and printing

# Print objects from the soc.ca function
print.soc.ca  <- function(object){
  
  # Help functions
  scree <- function(object, dim=6){
    set.dim  <- dim
    dim <- ifelse((nrow(object$adj.inertia)<dim)==TRUE, nrow(object$adj.inertia), dim)
    adj <- round(object$adj.inertia[1:dim,4], digits=1)
    stars <- round(round(adj)/2)
    starscree <- vector("list", set.dim)
    for (i in 1:length(stars)){
      starscree[[i]] <- noquote(rep("*", stars[i]))
    }
    return(starscree)
    # Object is a soc.ca class object
    # Dim is the number of dimensions included in the plot
  }
    
  Nmodal      <- object$n.mod
  Nsup        <- nrow(object$coord.sup) # S?t if ind
  Nid         <- object$n.ind
  Vnames   	  <- paste(rownames(object$modal), " (",object$modal[,3], ")", sep="")
  Submass 	  <- round(sum(object$mass.mod), digits=2) # Er det her i Procent aka. summer mass altid til 1.
  
  act.dim 	<- nrow(object$adj.inertia)
  dim80 		<- which.min(object$adj.inertia[,5] < 80)
  scree.dim	<- 6
  stars 		<- scree(object, scree.dim)
  adj.dim   <- 1:scree.dim
  #adj.dim 	<- object$adj.inertia[1:scree.dim,1]
  dim.a   <- ifelse((scree.dim<nrow(object$adj.inertia)), scree.dim, nrow(object$adj.inertia))
  adj <- vector(mode="numeric", length=scree.dim) # Måske skal den ikke smide 0 som output men noget bedre, når der ikke er flere dim end 6
  adj[1:dim.a] <- object$adj.inertia[1:dim.a,4]
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

balance.ctr <- function(object, act.dim=object$nd){
  coord   <- object$coord.mod[, 1:act.dim]
  contrib <- object$ctr.mod[, 1:act.dim]
  pm      <- matrix(, nrow=act.dim, ncol=3)
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

# Calculates the balance of the contribution of each dimension. 
# This measure indicates whether too much of a dimensions contribution is placed on either the + or - side of the dimension.
# Object is a soc.ca class object
# act.dim is the number of active dimensions to be measured
}

############################ The most contributing modalities
contribution <- function(object, dim=1, all=FALSE, modality.indices=FALSE){
  if(identical(modality.indices, TRUE)==TRUE){
    ctr     <- object$ctr.mod[,dim]
    av.ctr  <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
    if(is.list(av.ctr)==TRUE) av.ctr  <- unlist(av.ctr[dim], use.names=FALSE)
    av.ctr     <- av.ctr[duplicated(av.ctr)==FALSE]    
    return(av.ctr)
  }else{
    ctr <- round(100*object$ctr.mod[,dim], digits=1)
    cor <- round(100*object$cor.mod[,dim], digits=1)
    coord <- round(object$coord.mod[,dim], digits=2)
    names <- object$names.mod
    if (identical(all, FALSE)==TRUE){
      av.ctr<- contribution(object, dim=dim, modality.indices=TRUE)    
      header <- paste("The modalities contributing above average to dimension: ", dim, ".", sep="")
    }
    if (identical(all, TRUE)==TRUE){
      av.ctr <- 1:length(ctr)
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
  # Ctr is the contribution in percentage
  # Cor is the correlation with the dimension
  # Coord is the principal coordinate
}

################### The most contributing individuals

individuals <- function(object, dim=1, all=FALSE, ind.indices=FALSE){
  if(identical(ind.indices, TRUE)==TRUE){
    ctr     <- object$ctr.ind[,dim]
    av.ctr  <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
    if(is.list(av.ctr)==TRUE) av.ctr  <- unlist(av.ctr[dim], use.names=FALSE)
    av.ctr     <- av.ctr[duplicated(av.ctr)==FALSE]    
    return(av.ctr)
  }else{
    ctr <- round(100*object$ctr.ind[,dim], digits=1)
    # cor <- round(1000*object$cor.ind[,dim])
    coord <- round(object$coord.ind[,dim], digits=2)
    names <- object$names.ind
    if (identical(all, FALSE)==TRUE){
      av.ctr<- individuals(object, dim=dim, ind.indices=TRUE)    
      header <- paste("The individuals contributing above average to dimension: ", dim, ".", sep="")
    }
    if (identical(all, TRUE)==TRUE){
      av.ctr <- 1:length(ctr)
      header <- paste("The contribution of all individuals to dimension: ", dim, ".", sep="")
    }
    #out <- data.frame(ctr[av.ctr], cor[av.ctr], coord[av.ctr])
    out <- data.frame(ctr[av.ctr], coord[av.ctr])
    rownames(out) <- names[av.ctr]
    colnames(out) <- c("   Ctr.", "   Coord")
    out <- out[order(-out[,1]), ]
    maxwidth <- max(nchar(names))+sum(nchar(colnames(out)))
    cat("\n", format(header, width=maxwidth, justify="centre"), "\n", "\n")
    print(out)
  }
  # Returns the individuals with above average contribution to the selected dimension
  # object is a soc.ca object
  # dim is the included dimensions
  # all: If TRUE returns all individuals instead of just those that contribute above average
  # ind.indices: If TRUE returns a vector with the row indices of the individuals
  # Ctr is the contribution in 1000
  # Cor is the correlation with the dimension
  # Coord is the principal coordinate
}

############################## The most contributing modalities according to direction on dimension

tab.dim <- function(x, dim=1, label.plus=NULL, label.minus=NULL, all=FALSE){
  
  if (identical(label.plus, NULL)==TRUE){
    label.plus    <- paste("Dimension ", dim ,". (+)", sep="")
  }
  
  if (identical(label.minus, NULL)==TRUE){
    label.minus   <- paste("Dimension ", dim ,". (-)", sep="")
  }
  
  ctr     <- round(100*x$ctr.mod[,dim], digits=1)
  coord   <- round(x$coord.mod[,dim], digits=2)
  names   <- x$names.mod
  
  if (identical(all, FALSE)==TRUE){
    av.ctr<- contribution(x, dim=dim, modality.indices=TRUE)    
  }
  if (identical(all, TRUE)==TRUE){
    av.ctr <- seq(x$n.mod)
  }
  
  out         <- data.frame(ctr[av.ctr], coord[av.ctr])
  names       <- names[av.ctr]
  maxwidth    <- max(nchar(names))
  
  for (i in seq(names)){
    width       <- maxwidth-nchar(names[i])
    fill        <- paste(rep(" ", width), sep="", collapse="")
    names[i]    <- paste(names[i], fill, sep="", collapse="")
  }
  rownames(out) <- names
  ctr.lab       <- paste("Ctr")
  coord.lab     <- paste("Coord")
  colnames(out) <- c(ctr.lab, coord.lab)
  out           <- out[order(-out[,1]), ]
  out.label     <- c(ctr.lab, coord.lab)
  outminus      <- out[which(out[,2]<=0),]
  outplus       <- out[which(out[,2]>=0),]
  
  
  #outputwidth <- maxwidth+8*3
  cat("\n",format(label.plus, width=maxwidth, justify="centre"), "\n")
  print(format(outplus, justify="centre", width=8))
  cat("\n",format(label.minus, width=maxwidth, justify="centre"), "\n")
  format(outminus, justify="centre", width=8)
# tab.dim

  # Gives the most contributing modalities sorted according to direction on dimension
  # x is a soc.ca object
  # dim is the dimension
  # label.plus is the label of the dimensions plus side
  # label.minus is the label of the dimensions minus side
  # all defines whether all modalities are to be printed
  
  }

##########################      ctr.var       #### Contribution per variabel
ctr.var     <- function(object, dim=1:3){
    variable    <- as.factor(object$variable)
    ctr.mod     <- object$ctr.mod[,dim]
    lev.var     <- levels(variable)
    names.mod   <- object$names.mod
    freq.mod    <- object$freq.mod
    
    var.list    <- list()
    for (i in seq(length(lev.var))){
        var.ctr           <- round(ctr.mod[variable==lev.var[i],]*100, digits=1)
        var.ctr           <- cbind(var.ctr, freq.mod[variable==lev.var[i]])
        var.ctr           <- rbind(var.ctr, colSums(var.ctr))
        rownames(var.ctr) <- c(names.mod[variable==lev.var[i]], "Total")
        colnames(var.ctr) <- c(paste(" Dim.", dim, sep=""), "  Freq")
        
        var.list[[paste(lev.var[i])]] <- var.ctr
    }
    
    # The printing
    
    av.ctr <- round(100/object$n.mod, digits=1)
    maxwidth <- max(nchar(names.mod))
    
    cat("The contribution of the active variables")
    # Beautiful printing!
    for (i in seq(length(lev.var))){
        var.ctr <- var.list[[i]]
        cat("\n", "\n", format(lev.var[i], width=maxwidth), colnames(var.ctr))
        
        for (q in seq(nrow(var.ctr))){
            cat("\n", format(rownames(var.ctr)[q], width=maxwidth), format(var.ctr[q,dim], width=6), format(var.ctr[q,length(dim)+1], width=6, drop0trailing=TRUE))
        }
        
    }
    cat("\n","Average contribution per modality: ", av.ctr, sep="")
    cat("\n", "Total number of individuals: ", object$n.ind, sep="")
    
    class(var.list) <- "ctr.var"
    invisible(var.list)
    # ctr.var returns the contribution values of all modalities ordered by variable
    # object is a soc.ca object
    # dim is the included dimensions. The default is 1:3
    # If assigned using <- ctr.var returns a list of matrixes with the contribution values
}






##########################    tab.variance    #### Variance tabel
tab.variance <- function(object, dim=NULL){
    
    variance <- object$adj.inertia
    if (identical(dim, NULL)==TRUE){
        dim <- variance[,5]<=91
    }
    variance <- t(variance[dim,])
    line.dim <- paste(1:ncol(variance) ,".", sep="")
    cat("\n", "Dim        ", format(line.dim, width=6), sep="")
    cat("\n", "Sv       ", format(round(variance[2,], 2), width=6), sep="")
    cat("\n", "Var     ", format(round(variance[3,], 2), width=6), sep="")
    cat("\n", "Adj.Var ", format(round(variance[4,], 1), width=6), sep="")
    cat("\n", "Cum %   ", format(round(variance[5,], 1), width=6), sep="")
    
    invisible(variance)
    # tab.variance returns a table of variance for the dimensions
    # object is a soc.ca object
    # dim is the included dimensions, if set to NULL (default),
    # then only the dimensions explaining approx. 90% of the adjusted variance are included
    # If assigned using <- tab.variance returns a matrix version of the table of variance    
}



