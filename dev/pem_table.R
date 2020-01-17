pem <- function(x){
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
  PEM
}


pem.table <- function(object, output = c("html", 
                                         "matrix")) {
  output <- match.arg(output)
  
  B    <- t(as.matrix(object$indicator.matrix.all)) %*% as.matrix(object$indicator.matrix.all)
  nam  <- object$variable
  cc   <- combn(unique(nam), 2)
  x    <- list()

  for (i in 1:ncol(cc)) {
    x[[i]] <- pem(B[which(nam == cc[1,i]), which(nam == cc[2,i])])$pemg
  }

  x.pemg     <- lapply(x, function(x) as.data.frame(as.table(x))) %>% do.call(rbind, .)
  pm         <- pivot_wider(x.pemg, names_from = Var2, values_from = Freq)
  rn         <- pm$Var1
  pm         <- pm %>% select(-Var1) %>% as.matrix()
  rownames(pm)         <- rn
  #pm                       <- reshape2::dcast(data = x.pemg,formula = Var1~Var2)
  
  if(output == "matrix") return(pm)
  
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

pem.local.scores <- function(object, plane = 1:2){
  B    <- t(as.matrix(object$indicator.matrix.all)) %*% as.matrix(object$indicator.matrix.all)
  nam  <- object$variable.all
  cc   <- combn(unique(nam), 2)
  x    <- list()
  for (i in 1:ncol(cc)) {
    x[[i]] <- pem(B[which(nam == cc[1,i]), which(nam == cc[2,i])])$peml
  }
  
  PEM     <- lapply(x, function(x) as.data.frame(as.table(x))) %>% do.call(rbind, .) %>% rename(value = Freq)
  PEM     <- PEM %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2))
  
  # Join the coordinates
  coord   <- tibble(name = object$names.mod.all, X = object$coord.all[, plane[1]], Y = object$coord.all[, plane[2]])
  
  o       <- left_join(PEM, coord, by = c("Var1" = "name")) %>% left_join(., coord, by = c("Var2" = "name"), suffix = c(".Var1", ".Var2"))
  o$same.side.X <- (o$X.Var1 >= 0) == (o$X.Var2 >= 0)
  o$same.side.Y <- (o$Y.Var1 >= 0) == (o$Y.Var2 >= 0)
  
  o
}





pem.plot <- function(object, threshold = "mean", plane = c(1,2), dim = 1, cut.pem = 25) {

  PEM    <- pem.local.scores(object, plane = plane)  
  
  if(threshold == "rosenlund") s    <-  which(rowSums(object$ctr.mod[, plane] >= object$Rosenlund.tresh) > 0)
  if(threshold == "mean")      s    <-  which(rowSums(object$ctr.mod[, plane] >= 1/object$n.mod) > 0)
  if(is.numeric(threshold))    s    <-  which(rowSums(object$ctr.mod[, plane] >= threshold) > 0)
  
  s    <- object$names.mod[s]
  
  PEM1 <- PEM %>% filter(Var1 %in% s & Var2 %in% s) %>% filter(abs(value) >= cut.pem)
  
  PEM1 <- PEM1 %>% filter(value > 0 | (same.side.X + same.side.Y) > 0 & value < 0)
  
  
  ### TO BE CONTINUED........
  p     <- map.select(object, list.mod = which(object$names.mod %in% s), dim = plane, map.title = "", point.shape = 21, label.repel = T)
  p     <- p + geom_segment(data = PEM1, aes(x = X.Var1, xend = X.Var2, y = Y.Var1, yend = Y.Var2, color = value), size = 0.5)
  p     <- p + scale_alpha_continuous(range = c(0.001, 1), name= "PEM") + guides(size = FALSE) + theme(legend.position = "right")
  p     <- p + labs(title = "Degree of association between modalities", subtitle = "Percentage of maximum deviation from independence")
p + scale_color_gradientn(colours = rev(brewer.pal(9, name = "RdYlBu")), limits = c(-100, 100))
  }

pem.table(result)
pem.table(result, output = "matrix")

pem.plot(result, plane = c(1,2), dim = 1)


