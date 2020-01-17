pem <- function(x) {
  require("FactoMineR")
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


pem.table <- function(object) {
  require(reshape2)
  require(stringr)
  require(htmlTable)
  
  B   <- t(as.matrix(object$indicator.matrix.all))%*%as.matrix(object$indicator.matrix.all)
  nam <- gsub(":.*", "", rownames(B))
  cc  <- combn(unique(nam), 2)
  x   <- list()
  for (i in 1:ncol(cc)) {
    x[[i]] <- pem(B[which(nam == cc[1,i]), which(nam == cc[2,i])])
  }
  
  PEM  <- lapply(x, melt)
  PEM  <- do.call(rbind, PEM)
  
  pm                       <- dcast(data = PEM[PEM$L1 == "pemg",],formula = Var1~Var2)
  rownames(pm)             <- pm$Var1
  pm$Var1                  <- NA
  colnames(pm)[1]          <- rownames(pm)[1]
  pm                       <- rbind(pm, rep(NA, ncol(pm)))
  rownames(pm)[ncol(pm)]   <- colnames(pm)[ncol(pm)]
  css.pm                   <- matrix("padding-right: 10px;",  nrow = nrow(pm), ncol = ncol(pm))
  css.pm[pm < 50 & pm >10] <- "padding-right: 10px; font-weight: 900;background-color: rgb(191, 242, 150);"
  css.pm[pm >= 50]         <- "padding-right: 10px; font-weight: 900;background-color: rgb(94, 165, 38);"
  css.pm[pm <10]           <- "padding-right: 10px; font-weight: 900;background-color: rgb(193, 193, 193);"
  
  if(identical(object$headings, NULL)==TRUE) {
  QprH <- vector()
  for (i in 1:length(unique(result$headings))) {
    QprH[i] <- length(unique(object$variable[object$headings == unique(object$headings)[i]]))
  }
  
  al <- vector()
  for (i in 1: length(QprH)) {
    al <- c(al, paste(paste(c(rep("c", times = QprH[i])), collapse = ""), "|", collapse = ""))
  }
  al <- paste(al, collapse = "")
  al <- gsub(" ", "", al)
  al <- str_sub(al, 1, nchar(al) -1)
  }
  if(identical(object$headings, NULL)==FALSE) htmlTable(pm, rgroup = unique(object$headings), align = al, cgroup = unique(object$headings), n.rgroup = QprH, n.cgroup = QprH, css.cgroup = c("padding-right: 15px;"), css.cell = css.pm, tfoot = "<10% probably no ties, 10% < > 50% interesting ties, >50% probably due to redundancy in indicator (Cibois 2013: 41)")  
  if(identical(object$headings, NULL)==TRUE)  htmlTable(pm, css.cell = css.pm, tfoot = "<10% probably no ties, 10% < > 50% interesting ties, >50% probably due to redundancy in indicator (Cibois 2013: 41)")  
  }

pem.plot <- function(object, treshold = "mean", plane = c(1,2), dim = 1, cut.pem = 25) {
  require(reshape2)
  require(stringr)
  require(htmlTable)
  
  B <-   t(as.matrix(object$indicator.matrix.all))%*%as.matrix(object$indicator.matrix.all)
  nam <- gsub(":.*", "", rownames(B))
  cc <- combn(unique(nam), 2)
  x <- list()
  for (i in 1:ncol(cc)) {
    x[[i]] <- pem(B[which(nam == cc[1,i]), which(nam == cc[2,i])])
  }
  
  PEM  <- lapply(x, melt)
  PEM  <- do.call(rbind, PEM)
  if(treshold == "rosenlund") s    <-  object$names.mod[object$ctr.mod[,dim[1]] >= object$Rosenlund.tresh]
  if(treshold == "mean")      s    <-  object$names.mod[object$ctr.mod[,dim[1]] >= mean(object$ctr.mod[,dim[1]])]
  if(is.numeric(treshold))    s    <-  object$names.mod[object$ctr.mod[,dim[1]] >= treshold]
  PEM1 <- PEM[PEM$Var1 %in% s & PEM$Var2 %in% s, ]
  PEM1 <- PEM1[PEM1$value > cut.pem,]
  data <- object$coord.mod[,plane]
  rownames(data) <- object$names.mod
  PEM1$xstart <- data[as.character(PEM1$Var1),1]
  PEM1$xend   <- data[as.character(PEM1$Var2),1]
  PEM1$ystart <- data[as.character(PEM1$Var1),2]
  PEM1$yend   <- data[as.character(PEM1$Var2),2]
  
  ### TO BE CONTINUED........
  p     <- map.select(object, list.mod = which(object$names.mod %in% s), dim =plane, map.title = "", point.shape = 21, label.repel = T)
  p     <- p + geom_segment(data = PEM1, aes(x = xstart, xend = xend, y = ystart, yend = yend, alpha = value), color = "red", size = 0.5) + scale_alpha_continuous(range = c(0.001, 1), name= "PEM") + guides(size = FALSE) + theme(legend.position = "right")
  p     <- p + labs(title = "Degree of association between modalities", subtitle = "Percentage of maximum deviation from independence")
p 
  }

pem.plot(result, plane = c(1,3), dim = 3)


