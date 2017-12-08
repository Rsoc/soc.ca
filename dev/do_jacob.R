# Sæt den ind i balance som et argument

balance.ind <- function (object, act.dim = object$nd) 
{
  coord <- object$coord.ind[, 1:act.dim]
  contrib <- object$ctr.ind[, 1:act.dim]
  pm <- matrix(, nrow = act.dim, ncol = 3)
  for (i in 1:act.dim) {
    temp <- cbind(coord[, i], contrib[, i])
    temp <- temp[order(temp[, 1]), ]
    plus <- temp[which(temp[, 1] >= 0), ]
    minus <- temp[which(temp[, 1] <= 0), ]
    pcontrib <- sum(plus[, 2])
    mcontrib <- sum(minus[, 2])
    pm[i, 1] <- pcontrib
    pm[i, 2] <- mcontrib
    pm[i, 3] <- pcontrib/mcontrib
  }
  colnames(pm) <- c("+ Contrib.", "- Contrib.", "Balance (+/-)")
  return(pm)
}

pem <- function(x){
  library("FactoMineR")
  tota <- colSums(x)
  totb <- rowSums(x)
  total <- sum(x)
  theo <- matrix(nrow=nrow(x), ncol=ncol(x))
  for(i in 1: nrow(x)) {for(j in 1: ncol(x)) theo[i, j] <- tota[j]*totb[i]/total}
  ecart <- x-theo
  max <- matrix(nrow=nrow(x), ncol=ncol(x))
  emax <- matrix(nrow=nrow(x), ncol=ncol(x))
  pem <- matrix(nrow=nrow(x), ncol=ncol(x))
  for(i in 1: nrow(x)) { for(j in 1: ncol(x)) {
    if(ecart[i, j]>=0) max[i, j] <- min(tota[j], totb[i])
    if(ecart[i, j]<0&tota[j]<=(total-totb[i])) max[i, j] <- 0
    if(ecart[i, j]<0&tota[j]>(total-totb[i])) max[i, j] <- tota[j]+totb[i]-total
    emax[i, j] <- max[i, j] - theo[i, j]
    pem[i, j] <- ifelse(ecart[i, j]>=0, ecart[i, j]/emax[i, j]*100,0-ecart[i, j]/emax[i, j]*100) }}
  dimnames(pem) <- dimnames(x)
  cor <- CA(x, ncp = 1)
  z <- x[order(cor$row$coord), order(as.vector(cor$col$coord))]
  tota <- colSums(z)
  totb <- rowSums(z)
  maxc <- matrix(0, nrow=nrow(z), ncol=ncol(z))
  i<- 1;j<- 1
  repeat {
    m <- min(tota[j], totb[i])
    maxc[i, j] <- m
    tota[j] <- tota[j] - m
    totb[i] <- totb[i] - m
    if(sum(tota)+sum(totb)<0.00001) break
    if(tota[j]<0.00001) j<-j+1
    if(totb[i]<0.00001) i<-i+1
  }
  pemg  <- (sum(ecart)+sum(abs(ecart)))/(sum(maxc-theo[order(cor$row$coord),
                                                       order(cor$col$coord)])+sum(abs(maxc-theo[order(cor$row$coord), order(cor$col$coord)]))) 
  pemg2 <- matrix(pemg, nrow = 1, ncol = 1)
  dimnames(pemg2) <- list(unique(gsub(":.*", "", rownames(z))),unique(gsub(":.*", "",colnames(z))))
  
  rm(tota, totb, total, theo, ecart, max, emax, cor, z, m, maxc, i, j)
  PEM <- list(peml=round(pem,1), pemg=round(100*pemg2,1))
  return(PEM)
}



quadrant.description <- function(input.active = NULL, input.sup.cat = NULL, input.sup.cont = NULL, groups = NULL, cutoff = 100) {
  groups <- factor(groups)
  M <- matrix(NA, ncol = (2*(length(unique(groups))) +1))
  describe.act <- list()
  describe.sup.cat <- list() 
  describe.sup.cont <- list()
  
  
  if (is.null(input.sup.cat) == FALSE){
    if (sum(sapply(input.sup.cat, is.character))>0) {
      input.sup.cat[is.na(input.sup.cat)] <- "MISSING"
      input.sup.cat <- indicator(as.data.frame(unclass(input.sup.cat)))
    }
    ind.sup.cat <- input.sup.cat[,which(colSums(input.sup.cat)>=4)]
  }
  if (is.null(input.sup.cont) == FALSE){
    ind.sup.cont <- input.sup.cont
  }
  
  
  #items
  if (is.null(input.active) == FALSE) {
    ind.active <- input.active[,which(colSums(input.active)>=4)]  
    for (i in 1:(ncol(ind.active))) {
      x <- ind.active[,i]
      x[x==0] <- NA
      p <- table(x, groups)
      rownames(p) <- NULL
      f <- sum(x,na.rm = T)
      t <- (as.vector(p) - (table(groups) * (f/nrow(ind.active))))
      b <- (table(groups)*((nrow(ind.active)-table(groups))/(nrow(ind.active)-1))*(f/nrow(ind.active))*(1-(f/nrow(ind.active))))
      test <- round(as.vector(t)/as.vector(b),3)
      names(test) <- paste("test",names(b), sep = "-")
      p <- cbind(p,f, t(test))
      M <- rbind(M, p)
    }
    M <- M[-1,]
    rownames(M) <- colnames(ind.active)
    M <- data.frame(M, check.names = F)
    
    describe.act <- list()
    for (i in 1:(length(unique(groups)))) {
      x <- data.frame(Modalities = rownames(M)[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff],
                      "Freq.grp" = M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]],
                      "% of grp" = round(M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]] / sum(groups == levels(groups)[i], na.rm = T) *100,1),
                      "% of mod" = round(M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]] / M$f[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff] *100,1),
                      "Freq.all" = M$f[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff],
                      "% of all" = round(M$f[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff] / nrow(ind.active) *100,1),
                      "Test.value" = M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, paste("test", levels(groups)[i], sep="-")], check.names = F)
      describe.act[[i]] <- x#[with(x, order(-Freq.grp)), ] #order
      names(describe.act)[i] <- paste(levels(groups)[i], " n: ", table(groups)[i], sep = "")
    }
  }
  if (is.null(input.sup.cat) == FALSE) {
    #items
    M <- matrix(NA, ncol = (2*(length(unique(groups))) +1))
    
    for (i in 1:(ncol(ind.sup.cat))) {
      x <- ind.sup.cat[,i]
      x[x==0] <- NA
      p <- aggregate(x, by = list(groups), FUN = sum, na.rm=TRUE)
      p <- p$x
      f <- sum(x,na.rm = T)
      t <- (as.vector(p) - (table(groups) * (f/nrow(ind.sup.cat))))
      b <- (table(groups)*((nrow(ind.sup.cat)-table(groups))/(nrow(ind.sup.cat)-1))*(f/nrow(ind.sup.cat))*(1-(f/nrow(ind.sup.cat))))
      test <- round(as.vector(t)/as.vector(b),3)
      p <- c(p, f, t(test))
      M <- rbind(M, p)
    }
    M <- M[-1,]
    rownames(M) <- colnames(ind.sup.cat)
    colnames(M) <- c(names(b), "f", paste("test",names(b), sep = "-"))
    
    M <- data.frame(M, check.names = F)
    
    describe.sup.cat <- list()
    i <- 4
    for (i in 1:(length(unique(groups)))) {
      x <- data.frame(Modalities = rownames(M)[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff],
                      "Freq.grp" = M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]],
                      "% of grp" = round(M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]] / sum(groups == levels(groups)[i], na.rm = T) *100,1),
                      "% of mod" = round(M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]] / M$f[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff] *100,1),
                      "Freq.all" = M$f[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff],
                      "% of all" = round(M$f[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff] / nrow(ind.sup.cat) *100,1),
                      "Test.value" = M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, paste("test", levels(groups)[i], sep="-")], check.names = F)
      describe.sup.cat[[i]] <- x#[with(x, order(-Freq.grp)), ] order
      names(describe.sup.cat)[i] <- paste(levels(groups)[i], " n: ", table(groups)[i], sep = "")
    }
  }
  if (is.null(input.sup.cont) == FALSE){
    #items
    M <- matrix(NA, ncol = (2*length(unique(groups))))
    
    
    for (i in 1:(ncol(ind.sup.cont))) {
      x <- as.numeric(ind.sup.cont[,i])
      p <- t(aggregate(x, by = list(groups), FUN = mean, na.rm = T))
      colnames.p <- p[1,]
      p <- p[-1,]
      p <- as.numeric(p)
      t <- p - mean(x, na.rm = T)
      b <- ((nrow(ind.sup.cont) - table(groups))/(nrow(ind.sup.cont)-1)) * (var(x, na.rm = T)/table(groups))
      test <- round(t/sqrt(b), 3) 
      names(test) <- paste("test",names(b), sep = "-")
      names(p) <- colnames.p
      z <- c(p, test)
      M <- rbind(M, z)
    }
    M <- M[-1,]
    rownames(M) <- colnames(ind.sup.cont)
    M <- data.frame(M, check.names = F)
    M[is.na(M)] <- 0
    
    avr <- apply(ind.sup.cont, 2, function(x) mean(x, na.rm=TRUE))
    
    describe.sup.cont <- list()
    for (i in 1:(length(unique(groups)))) {
      mods <- rownames(M)[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff]
      x <- data.frame(Modalities = mods,
                      Mean = round(M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, levels(groups)[i]],2),
                      Overall.mean = round(avr[names(avr) %in% mods],2),
                      Test.value = round(M[M[,paste("test", levels(groups)[i], sep="-")] < -cutoff | M[,paste("test", levels(groups)[i], sep="-")] > cutoff, paste("test", levels(groups)[i], sep="-")],3), check.names = F)
      
      rownames(x) <- NULL
      describe.sup.cont[[i]] <- x# [with(x, order(-Test.value)), ] #order
      names(describe.sup.cont)[i] <- paste(levels(groups)[i], " n: ", table(groups)[i], sep = "")
    }  
    
  }
  
  
  
  out <- list("Active" = describe.act, "Supplementary.cat" = describe.sup.cat, "Supplementary.cont" = describe.sup.cont)
  return(out)
}

PCbiplot <- function(res.pca, dim = c(1,2), ind.lab = FALSE) {
  if(!require(ggplot2)) install.packages("ggplot2")
  library(ggrepel)
  x <- paste("Dim.", dim[1], sep ="")
  y <- paste("Dim.", dim[2], sep ="")
  # res.pca being a PCA object
  data <- data.frame(obsnames=row.names(res.pca$ind$coord), res.pca$ind$coord)
  if (identical(res.pca$quali.sup, NULL) == FALSE) {
    datasup <- data.frame(obsnames=row.names(res.pca$quali.sup$coord), res.pca$quali.sup$coord)
    w       <- log(do.call("c", lapply(res.pca$call$X[,!colnames(res.pca$call$X)%in%rownames(res.pca$var$coord)], table)))
  }
  max <- max(c(abs(res.pca$ind$coord[,x])),abs(res.pca$ind$coord[,y]))
  
  if (identical(ind.lab, TRUE)) {
    plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames)) + coord_fixed(xlim = c(-max, max), ylim = c(-max, max))
  }else{
    plot <- ggplot(data, aes_string(x=x, y=y)) + geom_point(alpha= 0.5, size = 1) + coord_fixed(xlim = c(-max, max), ylim = c(-max, max))
  }
  if (identical(res.pca$quali.sup, NULL) == FALSE) {
    plot <- plot + geom_point(data = datasup, aes_string(x=x, y=y), color = "blue", size = w) + geom_text_repel(data = datasup, alpha = 0.5, size = 4, aes(label=obsnames), color = "blue")  
  }
  plot <- plot + geom_hline(yintercept = 0, size = 0.2,color = "grey50") + geom_vline(xintercept = 0, size = 0.2,color = "grey50")
  
  datapc <- data.frame(varnames=rownames(res.pca$var$coord), res.pca$var$coord)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text_repel(data=datapc, aes(x=v1, y=v2, label=varnames), size = 4,  color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot <- plot + xlab(paste(dim[1], ".Dimension: ", round(res.pca$eig$`percentage of variance`[dim[1]],1), "%", sep = ""))
  plot <- plot + ylab(paste(dim[2], ".Dimension: ", round(res.pca$eig$`percentage of variance`[dim[2]],1), "%", sep = ""))
  #plot <- plot + xlim(c(-ceiling(max), ceiling(max))) + ylim(c(-ceiling(max), ceiling(max))) 
  plot <- plot + scale_y_continuous(breaks = c(-ceiling(max):ceiling(max)), minor_breaks = c(-ceiling(max):ceiling(max))) + scale_x_continuous(breaks = c(-ceiling(max):ceiling(max)), minor_breaks = c(-ceiling(max):ceiling(max)))
  plot
}


mapCAsup.qual <- function(res.ca, variable = variable, weight = NULL, drop = NULL, max = max, dim = c(1,2)){
  colvar   = variable
  variable = paste(variable, ".", sep = "")
  dd <- data.frame(res.ca$quali.sup$coord[,c(dim[1], dim[2])], check.names = F)
  dd <- dd[grepl(toupper(variable), toupper(rownames(dd))),]
  if (identical(drop, NULL)== FALSE) dd <- dd[-grep(drop, rownames(dd)),]
  
  if (identical(drop, NULL)== FALSE) sel <- which(grepl(toupper(variable), toupper(rownames(res.ca$quali.sup$v.test))) & !grepl(drop, rownames(res.ca$quali.sup$v.test)))
  if (identical(drop, NULL)== TRUE) sel <- which(grepl(toupper(variable), toupper(rownames(res.ca$quali.sup$v.test))))
  
  sign <- abs(res.ca$quali.sup$v.test[sel,c(dim[1])]) > 1.976 | abs(res.ca$quali.sup$v.test[sel,c(dim[2])]) > 1.976
  sign[sign == TRUE] <- 2
  sign[sign == FALSE] <- 1
  
  if (identical(drop, NULL)== FALSE) sel <- which(grepl(toupper(colvar), toupper(colnames(res.ca$call$Xtot))) & !grepl(drop, colnames(res.ca$call$Xtot)))
  if (identical(drop, NULL)== TRUE) sel <- which(grepl(toupper(colvar), toupper(colnames(res.ca$call$Xtot))))
  if (identical(weight, NULL)) weight <- table(res.ca$call$Xtot[,sel])
  p  <- ggplot(data = dd, aes(x= dd[,1], y = dd[,2])) + geom_point(size = log(weight), color = sign) + geom_text_repel(alpha=.4, size=3, aes(label=gsub(variable, "", rownames(dd)))) + coord_fixed()
  p  <- p + ggtitle(paste(gsub("\\.", " ", colvar), " as supplementary variable", sep = ""))
  p <-  p + xlab(paste(dim[1], ". Dimension: ", round(res.ca$eig$`percentage of variance`[dim[1]],1), "%", sep = ""))
  p <-  p + ylab(paste(dim[2], ". Dimension: ", round(res.ca$eig$`percentage of variance`[dim[2]],1), "%", sep = ""))
  p <-  p+ geom_hline(yintercept = 0, size = 0.2,color = "grey50") + geom_vline(xintercept = 0, size = 0.2,color = "grey50")
  p <-  p + xlim(c(-max, max)) + ylim(c(-max, max)) + coord_fixed()
  p
  
}

mapCAsup.quant <- function(res.ca, variable = variable, dim = c(1,2)){
  quant.sup <- data.frame(res.ca$quanti.sup)
  quant.sup <- quant.sup[grepl(variable, rownames(quant.sup)), ]
  quant.sup <- quant.sup[,dim]
  colnames(quant.sup) <- c("x", "y")
  
  p <- ggplot(data = quant.sup , mapping = aes(x= x, y= y)) + geom_point(color = "red") + geom_text_repel(size=3, aes(label=rownames(quant.sup)), color = "red")
  p <- p + geom_segment(data=quant.sup, aes(x=0, y=0, xend=x, yend=y), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  p <- p + xlim(c(-1.5, 1.5))+ ylim(c(-1.5, 1.5)) + coord_fixed()
  p <- p + geom_hline(yintercept = 0, size = 0.2,color = "grey50") + geom_vline(xintercept = 0, size = 0.2,color = "grey50")
  
  angle <- seq(-pi, pi, length = 50)
  circ <- data.frame(x = sin(angle), y = cos(angle))
  
  p <- p + geom_polygon(aes(x,y), data = circ, fill = NA, color = "red", linetype = 2)
  p <-  p + xlab(paste(dim[1], ". Dimension: ", round(res.ca$eig$`percentage of variance`[dim[1]],1), "%", sep = ""))
  p <-  p + ylab(paste(dim[2], ". Dimension: ", round(res.ca$eig$`percentage of variance`[dim[2]],1), "%", sep = ""))
  p
}


mapCArow.sup <- function(res.ca, Title = "Lifestyles", Var = "xxx", weight = 3, max = max, dim = c(1,2)){
  sel <- grep(Var, rownames(res.ca$row.sup$coord))
  dd <- data.frame(res.ca$row.sup$coord[sel,c(dim[1], dim[2])], check.names = F)
  
  p  <- ggplot(data = dd, aes(x= dd[,1], y = dd[,2])) + geom_point(size = log(weight)) + geom_text_repel(alpha=.4, size=3, aes(label=gsub(".*: ","", rownames(dd)))) + coord_fixed()
  p  <- p + ggtitle(paste(Var, " as supplementary variable", sep = ""))
  p <-  p + xlab(paste(dim[1], ". Dimension: ", round(res.ca$eig$`percentage of variance`[dim[1]],1), "%", sep = ""))
  p <-  p + ylab(paste(dim[2], ". Dimension: ", round(res.ca$eig$`percentage of variance`[dim[2]],1), "%", sep = ""))
  p <-  p+ geom_hline(yintercept = 0, size = 0.2,color = "grey50") + geom_vline(xintercept = 0, size = 0.2,color = "grey50")
  p <-  p + xlim(c(-max, max)) + ylim(c(-max, max)) + coord_fixed()
  p
  
}

contribution.dif  <- function(object = result, points1 = NULL, points2 = NULL, dim = 1) {
  
  p1 <- object$mass.mod[points1]
  y1 <- sum(p1 * object$coord.mod[points1, dim]) / sum(p1)
  
  p2 <- object$mass.mod[points2]
  y2 <- sum(p2 * object$coord.mod[points2, dim]) / sum(p2)
  
  cti <- ((sum(p1) * sum(p2)) / (sum(p1) + sum(p2)))*(y1 - y2)^2
  cti <- cti / object$eigen[dim]
  cti
}

