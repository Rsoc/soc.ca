# Tools, exports and other helpful functions

#' Cut a continuous variable into categories with a specified minimum
#' 
#' Many continuous variables are very unequally distributed, often with many individuals in the lower categories and fewer in the top.
#' As a result it is often difficult to create groups of equal size, with unique cut-points.
#' By defining the wanted minimum of individuals in each category, but still allowing this minimum to be surpassed, it is easy to create ordinal variables from continuous variables. 
#' The last category will not neccessarily have the minimum number of individuals.
#' 
#' @param x is a continuous numerical variable
#' @param min.size is the minimum number of individuals in each category
#' @return a numerical vector with the number of each category
#' @export
#' @examples
#' a <- 1:1000
#' table(min_cut(a))
#' b <- c(rep(0, 50), 1:500)
#' table(min_cut(b, min.size = 20))
#' 

min_cut <- function(x, min.size = length(x)/10){
  
  x.na <- x[is.na(x) == FALSE]
  p.x <- cumsum(prop.table(table(x.na)))
  t.x <- cumsum(table(x.na))
  bm  <- cbind(table(x.na),t.x)
  dif <- vector(length = nrow(bm))
  for (i in 2:length(dif)) dif[i] <- bm[i,2] - bm[i-1,2]
  dif[1] <- bm[1, 2]
  bm <- cbind(bm, dif)
  
  group <- vector(length = nrow(bm))
  g <- 1 
  collect <- 0
  for (i in 1:nrow(bm)){
    if (dif[i] >= min.size | collect >= min.size-1){
      group[i] <- g
      g <- g + 1
      collect <- 0
    }else{
      group[i] <- g
      collect  <- collect + dif[i]
    }
  }
  
  x.group <- vector(length = length(x))
  group.levels <- as.numeric(levels(as.factor(group)))
  values <- as.numeric(rownames(bm))
  levs   <- vector()
  # Assigning group to the original x
  for (i in 1:length(group.levels)){
    g   <- group.levels[i]
    val <- values[group == g]
    g   <- paste(min(val), ":", max(val), sep = "")
    x.group[x %in% val]  <- g
    levs                 <- c(levs, paste(min(val), ":", max(val), sep = ""))
  }
  x.group[is.na(x)] <- NA
  factor(x.group, labels = levs, ordered = TRUE)
}

#' Cut ordinal variables
#' 
#' If we are in a hurry and need to cut a lot of likert-scale or similar type of variables into MCA-friendly ordered factors this function comes in handy.
#' cowboy_cut will try its best to create approx 3-5 categories, where the top and the bottom are smaller than the middle. Missing or other unwanted categories are recoded but still influence the categorization. So that when cowboy_cut tries to part the top of a variable with a threshold around 10% of cases it is 10% including the missing or NA cases.
#' Make sure that levels are in the right order before cutting. 
#' @param x a factor
#' @param top.share approximate share in top category
#' @param bottom.share approximate share in bottom category
#' @param missing a character vector with all the missing or unwanted categories.
#'
#' @return a recoded factor
#' @export

cowboy_cut <- function(x, top.share = 0.10, bottom.share = 0.10,  missing = "Missing"){
  x[x %in% missing] <- NA
  x <- droplevels(x)
  x <- as.ordered(x)
  x <- as.numeric(x)
  
  x.top <- x
  x.bottom <- x
  x.top[is.na(x)] <- -Inf
  x.bottom[is.na(x)] <- Inf
  
  top <- quantile(x.top, probs = seq(0, 1, by = top.share), na.rm = TRUE, type = 3) %>% tail(2) %>% head(1)
  bottom <- quantile(x.bottom, probs = seq(0, 1, by = bottom.share), na.rm = TRUE, type = 3) %>% head(2) %>% tail(1)
  mid  <- x[x != 0 & x < top] %>% quantile(probs = seq(0, 1, by = 0.33), type = 3, na.rm = TRUE)
  mid  <- mid[-1]
  mid  <- mid[-3]
  breaks <- c(-Inf, bottom, mid, top, Inf)
  o <- x %>% as.numeric() %>% cut(breaks = unique(breaks), include.lowest = TRUE)
  levels(o) <- paste0(1:nlevels(o), "/", nlevels(o)) 
  o %>% fct_explicit_na(na_level = "Missing")
}


#' Export results from soc.ca
#'
#' Export objects from the soc.ca package to csv files.
#' @param object is a soc.ca class object
#' @param dim is the dimensions to be exported
#' @param file is the path and name of the .csv values are to be exported to
#' @return A .csv file with various values in UTF-8 encoding
#' @seealso \link{soc.mca}, \link{contribution}
#' @export

export <- function(object, file = "export.csv", dim = 1:5) {
  if (is.matrix(object) == TRUE|is.data.frame(object) == TRUE){
    write.csv(object, file, fileEncoding = "UTF-8")}

    if ((class(object) == "tab.variable") == TRUE){
      
      ll    <- length(object)
      nam   <- names(object)
      a     <- object[[1]]
      coln  <- ncol(a)
      line  <- c(rep("", coln))
      line2 <- c(rep("", coln))
      a     <- rbind(line, a, line2)      
      
    for (i in 2:ll){
      line <- c(rep("", coln))
      line2 <- c(rep("", coln))
      a <- rbind(a,line, object[[i]], line2)
      line2  <- c(rep("", coln))
    }
    rownames(a)[rownames(a) == "line"] <- nam
    rownames(a)[rownames(a) == "line2"] <- ""
    out <- a
    write.csv(out, file, fileEncoding = "UTF-8")  
    }

    if ((class(object) == "soc.mca") == TRUE){
    coord.mod     <- object$coord.mod[,dim]
    coord.sup     <- object$coord.sup[,dim]
    coord.ind     <- object$coord.ind[,dim]
    names.mod		  <- object$names.mod
    names.sup  	  <- object$names.sup
    names.ind     <- object$names.ind
    coord         <- round(rbind(coord.mod, coord.sup, coord.ind), 2)
    names         <- c(names.mod, names.sup, names.ind)
    rownames(coord) <- names
    
    ctr.mod       <- object$ctr.mod[,dim]
    ctr.sup       <- matrix(nrow = nrow(coord.sup), ncol = ncol(coord.sup))
    ctr.ind       <- object$ctr.ind[,dim]
    ctr           <- round(1000*rbind(ctr.mod, ctr.sup, ctr.ind))
    
    cor.mod       <- round(100*object$cor.mod[,dim], 1)
    cor.sup       <- matrix(nrow = nrow(coord.sup), ncol = ncol(coord.sup))
    cor.ind       <- matrix(nrow = nrow(coord.ind), ncol = ncol(coord.ind))
    cor           <- rbind(cor.mod, cor.sup, cor.ind)
    
    out           <- cbind(coord, ctr, cor)
    colnames(out) <- c(paste("Coord:", dim), paste("Ctr:", dim), paste("Cor:", dim))
    write.csv(out, file, fileEncoding = "UTF-8")
  
  }
  
}

#' Invert the direction of coordinates
#' 
#' Invert one or more axes of a correspondence analysis. The principal coordinates of the analysis are multiplied by -1.
#' @details This is a convieniency function as you would have to modify coord.mod, coord.ind and coord.sup in the soc.ca object.
#' 
#' @param x is a soc.ca object
#' @param dim is the dimensions to be inverted
#' @return a soc.ca object with inverted coordinates on the specified dimensions
#' @seealso \link{soc.mca}, \link{add.to.label}
#' @examples
#' example(soc.ca)
#' inverted.result  <- invert(result, 1:2)
#' result$coord.ind[1, 1:2]
#' inverted.result$coord.ind[1, 1:2]
#' @export

invert <- function(x, dim = 1) {
  x$coord.mod[,dim] <- x$coord.mod[,dim] * -1
  x$coord.all[, dim] <- x$coord.all[, dim] * -1
  x$coord.ind[,dim] <- x$coord.ind[,dim] * -1
  x$coord.sup[,dim] <- x$coord.sup[,dim] * -1
  return(x)
}


#' Convert to MCA class from FactoMineR
#'
#' @param object is a soc.ca object
#' @param active the active variables
#' @param dim a numeric vector
#'
#' @return an FactoMineR class object
to.MCA <- function(object, active, dim = 1:5) {
  
  rownames(active)         <- object$names.ind
  rownames(object$coord.ind) <- object$names.ind
  
  var                      <- list(coord = object$coord.mod[,dim], 
                                   cos2  = object$cor.mod[,dim], 
                                   contrib = object$ctr.mod[,dim])
  ind <- list(coord = object$coord.ind[,dim], 
              cos2 = object$cor.ind[,dim], 
              contrib = object$ctr.ind[,dim])
  call <- list(marge.col = object$mass.mod, 
               marge.row = 1/object$n.ind,
               row.w = rep(1, object$n.ind),
               X = active,
               ind.sup = NULL)
  eig <- matrix(0, nrow = length(dim), ncol = 3)
  rownames(eig) <- paste("dim ", dim, sep="")
  colnames(eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
  eig[,1] <- object$eigen[dim]
  eig[,2] <- eig[,1]/sum(object$eigen) *100
  eig[,3] <- cumsum(eig[,2])
  res <- list(eig=eig, var=var, ind=ind, call=call)                                          
  class(res) <- c("MCA", "list")
  return(res)
}

barycenter <- function(object, mods = NULL, dim = 1){
  new.coord <- sum(object$mass.mod[mods] * object$coord.mod[mods, dim]) / sum(object$mass.mod[mods])
  new.coord
}

#' MCA Eigenvalue check
#'
#' Two variables that have perfectly or almost perfectly overlapping sets of categories will skew an mca analysis. This function tries to find the variables that do that so that we may remove them from the analysis or set some of the categories as passive.
#' An MCA is run on all pairs of variables in the active dataset and we take first and strongest eigenvalue for each pair.
#' Values range from 0.5 to 1, where 1 signifies a perfect or near perfect overlap between sets of categories while 0.5 is the opposite - a near orthogonal relationship between the two variables.
#' While a eigenvalue of 1 is a strong candidate for intervention, probably exclusion of one of the variables, it is less clear what the lower bound is. But values around 0.8 are also strong candidates for further inspection.  

#' @param x a data.frame of factors or a result object from soc.mca
#' @param passive a character vector with the full or partial names of categories to be set as passive. Each element in passive is passed to a grep function.
#'
#' @return a tibble
#' @export
#'
#' @examples
#' example(soc.mca)
#' mca.eigen.check(active)
#' mca.eigen.check(result)

mca.eigen.check <- function(x, passive = "Missing"){
  x.is  <- what.is.x(x)
  
  if(x.is == "soc.mca")  return(mca.eigen.check.soc.mca(x))
  if(x.is == "data.frame") mca.eigen.check.active(x, passive = passive)
  
}

mca.eigen.check.active <- function(active, passive = "Missing"){
  
  get.eigen <- function(x, y, passive = "Missing"){
    d <- data.frame(x, y)
    r <- soc.mca(d, passive = passive)
    
    burt <- t(r$indicator.matrix.active) %*% r$indicator.matrix.active
    
    burt.s <- burt / diag(burt)
    diag(burt.s) <- 0
    max(burt.s)
    
    o <- c("First eigen" = r$eigen[1], "Passive categories" = length(r$names.passive), "Max overlap (%)" = max(burt.s))
    o
  }
  
  comb           <- active %>% colnames() %>% combn(., 2, simplify = T) %>% t() %>% as_tibble(.name_repair = 'minimal')
  colnames(comb) <- c("x", "y")
  
  values           <- purrr::map2_df(.x = comb$x, .y = comb$y,  ~get.eigen(x = active[[.x]], y = active[[.y]], passive = passive))
  o                <- bind_cols(comb, values) |> arrange(-`First eigen`)
  o
}

mca.eigen.check.soc.mca <- function(r){
  
  ind              <- cbind(r$indicator.matrix.active, r$indicator.matrix.passive)
  ind              <- ind[, r$names.mod.all]
  var              <- r$variable.all
  
  get.eigen <- function(x, y, passive = "Missing"){
    d <- cbind(x, y) %>% as.matrix()
    r <- soc.mca(d)
    
    burt <- t(r$indicator.matrix.active) %*% as.matrix(r$indicator.matrix.active)
    
    burt.s <- burt / diag(burt)
    diag(burt.s) <- 0
    max(burt.s)
    
    o <- c("First eigen" = r$eigen[1], "Passive categories" = length(r$names.passive), "Max overlap (%)" = max(burt.s))
    o
  }
  
  comb <- r$variable %>% unique() %>% combn(., 2, simplify = T) %>% t() %>% as_tibble(.name_repair = 'minimal')
  colnames(comb) <- c("x", "y")
  
  values           <- purrr::map2_df(.x = comb$x, .y = comb$y,  ~get.eigen(x = ind[, var == .x], y = ind[, var == .y], passive = r$names.passive))
  o                <- bind_cols(comb, values) |> arrange(-`First eigen`)
  o
}

#' Pivot the indicator matrix from an MCA to long format
#' 
#' Sometimes we want the indicator matrix from an mca in long format and this function delivers just that. The results contain both active and passive categories.
#'
#' @param r an result object of class soc.mca
#'
#' @return a tibble in long format
#' @export
#'
#' @examples
#' example(soc.mca)
#' result |> indicator.to.long() 

indicator.to.long <- function(r){
  ind           <- r$indicator.matrix.active
  rownames(ind) <- r$names.ind
  
  if(ncol(r$indicator.matrix.passive) > 0){
    
    pas           <- r$indicator.matrix.passive
    rownames(pas) <- r$names.ind
    ind           <- cbind(ind, pas)  
  }
  
  m    <- as.data.frame.table(ind) %>% filter(Freq != 0) %>% as_tibble() |> select(case = Var1, category = Var2, value = "Freq")
  cats <- tibble(category = r$names.mod.all, variable = r$variable.all, label = names(r$names.mod.all), passive = category %in% r$names.passive)
  o    <- left_join(m, cats, by = "category")
  o
}

#' Create a randomized mca on the basis of an existing mca
#'
#' We sample from each of the active variables independently removing the
#' original correlations but retaining the frequencies of the categories. This
#' function is useful to see the extent to which the mca solution reflects the
#' correlations between variables or the frequency distribution between the
#' active categories. Passive categories are inherited from the original analysis.
#'
#'
#' @param r a result object from soc.mca
#' @param replace
#'
#' @return a soc.mca object
#' @export
#'
#' @examples
#' example(soc.mca)
#' randomize.mca(result)

randomize.mca <- function(r, replace = FALSE){
  
  im <- r |> indicator.to.long()
  iw <- pivot_wider(im, id_cols = case, names_from = variable, values_from = label)
  ir <- map_df(iw, ~.x[sample(length(.x), replace = replace)] )
  passive <- im$category[im$passive] %>% unique()
  if(length(passive) == 0) passive <- NA
  soc.mca(ir |> select(-case), passive = passive)
}

#' Remove unnecessary variables from an MCA
#' 
#' This function tests and removes variables that have no or too few relations with other variables. In other words variables that only contribute with random noise to the analysis. Removing these variables will tend to increase the strength of the first dimensions and give a wider dispersion of the cloud of cases on the first dimensions. Removing these variables can also give a simpler analysis that is easier to interpret and communicate. The core of the pruning procedure uses the \link(mca.eigen.check) to construct a weighted network of relations between variables. Tie strength is measured by the first eigenvalue of an MCA between the two variables. Ties between variables with a weak relationship  are removed and variables with few connections to other variables are discarded. With the default values a analysis without irrelevant variables is unchanged. Note that passive categories are inherited from the original analysis and are not included in the \link{mca.eigen.check}. This procedure does not help with variables that are too strongly related.
#'
#' @param r a result object from \link{soc.mca}
#' @param eigen.cut.off the cut.off for the first eigen value from \link{mca.eigen.check}
#' @param network.pruning If TRUE variables are pruned on the basis their degree
#' @param average.pruning If TRUE variables with a sum of ties below average are discarded. This 
#' @param min.degree the minimum number of ties a variable has to have to remain in the analysis
#'
#' @return A list containing:
#'  \item{var}{a tibble with the weighted degree of the variables} 
#'  \item{mca.eigen.check}{The results from \link{mca.eigen.check}}
#'  \item{g}{a network graph - see \link{igraph}}
#'  \item{remaining.var}{a character vector with the names of the remaining variables}
#'  \item{removed}{a character vector with the names of the removed variables}
#'  \item{pruned.r}{A pruned version of the original soc.mca object}                
#' @export
#' @references Inspired by: Durand, Jean-Luc, and Brigitte Le Roux. 2018. “Linkage Index of Variables and its Relationship with Variance of Eigenvalues in PCA and MCA.” Statistica Applicata 29(2):123–35. doi: 10.26398/ijas.0029-006.
#' @examples
#' example(soc.mca)      
#' pr <- prune.mca(result)  
#' pr$removed               # This example has no irrelevant variables so nothing is removed

prune.mca <- function(r, eigen.cut.off = 0.55, network.pruning = TRUE, average.pruning = FALSE, min.degree = 1){
  require(igraph) 
  
  mec <- mca.eigen.check(r, passive = r$names.passive)
  g   <- graph_from_data_frame(mec, directed = FALSE)
  gso  <- graph.strength(g, weights = E(g)$"First eigen")
  
  gi <- delete.edges(g, edges = which(E(g)$"First eigen" < eigen.cut.off))
  
  if(identical(average.pruning, TRUE)){
    kill.their.edges <- which(gso < mean(gso))
    gi[, kill.their.edges] <- 0
    gi[kill.their.edges, ] <- 0
  }
  
  if(identical(network.pruning, TRUE)){
    kill.their.edges <- which(degree(gi) < min.degree)
    gi[, kill.their.edges] <- 0
    gi[kill.their.edges, ] <- 0
  }
  
  gs  <- graph.strength(gi, weights = E(gi)$"First eigen")
  set <- names(gs)[gs >= min.degree] 
  
  if(length(set) == 0) {
    
    cat("\n", "No variables survived the pruning procedure")
    return(NULL)
  }
  
  un.set <- names(gs)[gs < min.degree] 
  
  # Redo the mca
  im <- indicator.to.long(r)
  iw <- pivot_wider(im, id_cols = case, names_from = variable, 
                    values_from = label)
  passive <- im$category[im$passive] %>% unique()
  if (length(passive) == 0) passive <- NA
  
  iw.set <- iw[, set]
  ri    <- soc.mca(iw.set, passive = passive)
  
  
  o      <- list()
  o$var  <- tibble(variable = V(g)$name, "graph.strength.original" = gso, "graph.strength.pruned" = gs, "below.average" = gso < mean(gso))
  o$mca.eigen.check <- mec
  o$g    <- gi
  o$remaining.var <- set
  o$removed <- un.set
  o$pruned.r <- ri
  
  o
}

