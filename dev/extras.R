library(soc.ca)
library(tidyverse)
# example(soc.mca)
data("gss_cat")
table(gss_cat$age) %>% prop.table()
x <- gss_cat$age

x <- gss_cat$rincome

missing <- c("Missing", "No answer", "Don't know", "Refused", "Not applicable")
x <- as.factor(x)
o <- cowboy_cut(x, missing = missing, top.share = 0.10)
table(x, o, useNA = "always")
table(o) %>% prop.table()

library(profvis)

profvis(soc.mca(data.frame(x,o), passive = missing))

soc.mca(data.frame(x,o), passive = missing)
debug(soc.ca:::subset.ca.indicator)

example(soc.mca)




profvis(soc.mca(active, sup))

d <- data.frame(a = cut.cowboy(taste$Age), b = taste$Age)
d <- data.frame(a = taste$Age, b = taste$Age)
d <- data.frame(a = taste$Age, b = taste$Income) %>% map_df(~fct_explicit_na(.x, na_level = "Missing"))

d <- data.frame(a = taste$Income %>% cut.cowboy(), b = taste$Income) %>% map_df(~fct_explicit_na(.x, na_level = "Missing"))

data(directors)

# To helt ens variable
d <- tibble(a = taste$Eat, b= taste$Eat)
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv
r <- soc.mca(d)
r
r$eigen # 1. eigen 1
r$total.inertia

# To ens variable - hvor den ene er en omkodet variant af den samme
d <- tibble(a = taste$Age %>% cut.cowboy(), b= taste$Age) %>% droplevels()
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen 1
r <- soc.mca(d)
r
r$eigen # 1. eigen 1

# To næsten ens variable
cor(directors$turnover2004, directors$turnover2007, method = "spearman") # Cor 0.96
d <- tibble(a = directors$turnover2007 %>% as.factor %>% cut.cowboy, b= directors$turnover2004 %>% as.factor %>% cut.cowboy)
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen 0.95
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.97

# To lidt mere forskellige men meget ens variable
cor(directors$balance2007, directors$turnover2004, method = "spearman") # Cor 0.73
d <- tibble(a = directors$balance2007 %>% as.factor %>% cut.cowboy, b= directors$turnover2004 %>% as.factor %>% cut.cowboy)
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen 0.71
FactoMineR::CA(table(d$a, d$b), graph = F) %>% .$eig # 1. eigen er 0.5!
FactoMineR::CA(table(d$a, d$b), graph = F) %>% summary()
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.85

# To lidt mere forskellige men meget ens variable
cor(directors$equity2007, directors$turnover2004, method = "spearman") # Cor 0.66
d <- tibble(a = directors$equity2007 %>% as.factor %>% cut.cowboy, b= directors$turnover2004 %>% as.factor %>% cut.cowboy)
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen = 0.89
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.94 - Den er så høj fordi 5/5 på begge variable er næsten perfekt overlappende.
contribution(r)


# To forskellige variable
d <- tibble(a = directors$sector, b = directors$size_prestige)
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen = 0.46
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.73
contribution(r) # Forskellige kategorier er på hver sin side

# En kategori er perfekt indlejret i en anden
d <- data.frame(a = taste$Age, b = taste$Income) %>% map_df(~fct_explicit_na(.x, na_level = "Missing"))
d$b[d$a == "18-24"] <- "GBP: <9"
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen 0.55
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.77
contribution(r) # De to kategorier ekstreme og på samme side af 1.dim

# En kategori er perfekt indlejret - begge veje
d <- data.frame(a = taste$Age, b = taste$Income) %>% map_df(~fct_explicit_na(.x, na_level = "Missing"))
d$b[d$a == "18-24"] <- "GBP: <9"
d$a[d$b == "GBP: <9"] <- "18-24"
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen er 1
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.99
contribution(r) 

# En kategori er perfekt afvist 
d <- data.frame(a = taste$Age, b = taste$Income) %>% map_df(~fct_explicit_na(.x, na_level = "Missing"))
d <- d[!(d$a == "18-24" & d$b == "GBP: <9"),]
table(d$a, d$b)
table(d$a, d$b) %>% ca::ca() %>% .$sv # 1. eigen 0.37
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.67
contribution(r) # Det kan den ikke arbejde med

# Det ser ikke ud til at den fanger alt - men måske er en eigen værdi på over 0.8 et godt sted at undersøge.


# Hvad med tre identiske variable?
d <- tibble(a = taste$Eat, b= taste$Eat, c = taste$Eat)
r <- soc.mca(d)
r
r$eigen # 1. eigen 1

# Hvad med tre næsten ens? Det kan den også
d <- tibble(a = directors$equity2007 %>% as.factor %>% cut.cowboy, b= directors$turnover2004 %>% as.factor %>% cut.cowboy,
            c = directors$employees %>% as.factor %>% cut.cowboy)
r <- soc.mca(d)
r
r$eigen # 1. eigen 0.81
contribution(r)


comb <- active %>% colnames() %>% combn(., 2, simplify = T) %>% t() %>% as_tibble()
colnames(comb) <- c("x", "y")

values           <- map2_df(.x = comb$x, .y = comb$y,  ~get.eigen(x = active[[.x]], y = active[[.y]], passive = passive))
o                <- bind_cols(comb, values)
o
}

c<- mca.eigen.check(bind_cols(active))


#' Add a new layer of points on top of an existing plot with output from the cut.min function
#' @param x a matrix created by the cut.min function
#' @param p is a ggplot object, preferably from one of the mapping functions in soc.ca
#' @param label if TRUE the labels of points will be shown

add.count <- function(x, p=map.ind(object), label=TRUE, ...){
  p <- p + geom_point(data=x, x=x$X, y=x$Y, ...) + geom_path(data=x, x=x$X, y=x$Y, ...)
  if (identical(label, TRUE)) p <- p + geom_text(data=x, x=x$X, y=x$Y, label=x$label, vjust=0.2, ...)
}

#' Add a path along an ordered variable onto a map
#'
#' Plot a path along an ordered variable. If the variable is numerical it is cut into groups by the \link{cut.min} function.
#'  
#' @param object is a soc.ca result object
#' @param x is an ordered vector, either numerical or factor
#' @param map is a plot object created with one of the mapping functions in the soc.ca package
#' @param dim is the two dimensions plotted
#' @param label if TRUE the label of the points are shown
#' @param min.size is the minimum size given to the groups of a numerical variable, see \link{cut.min}.
#' @param ... further arguments are passed onto geom_path, geom_point and geom_text from the ggplot2 package
#' @export
#' example(soc.mca)
#' map <- map.ind(result, point.colour=as.numeric(sup$Age)) + scale_color_continuous(high="Red", low="yellow")
#' add.path(result, sup$Age, map)

# Find ud af om ... kan give forskellige argumenter til forskellige funktioner.
add.path  <- function(object, x, map = map.ind(object, dim), dim=c(1,2), label=TRUE, min.size=length(x)/10, ...){
  
  x.c                        <- x
  if (is.numeric(x)) x.c     <- cut.min(x, min.size=min.size) 
  
  x.av    <- average.coord(object=object, x=x.c, dim=dim)  
  map.p   <- add.count(x.av, map, label, ...) 
  map.p
}

######################################################################
## Interactive plot


### Googlevis
library(googleVis)
library(soc.ca)

example(soc.mca)

gdat <- data.frame(result$names.mod, result$coord.mod[,1:2], result$freq.mod , result$variable, result$ctr.mod[,1])
rownames(gdat) <- result$names.mod
colnames(gdat) <- c("Names", "Dim1", "Dim2", "Freq", "Variable", "Ctr.1")

gopt <- list(gvis.editor="Edit")

nif <- gvisBubbleChart(gdat, idvar="Names", xvar="Dim1", yvar="Dim2", sizevar="Freq", colorvar="Variable", options=gopt)

plot(nif)

#### Gridsvg

# #### Animint
# library(soc.ca)
# library(reshape)
# library(animint)
# example(soc.mca)
# p  <- map.ind(result)
# 
# interactive.points <- data.plot(result, "ind", dim=c(1,2))
# 
# # Cloud of modalities
# 
# mod.coord <- data.plot(result, "active", dim=c(1,2))
# 
# im        <- indicator(active)
# im        <- im == 1
# names.mod <- as.character(mod.coord$names)
# out.mat   <- matrix(ncol=ncol(mod.coord))
# colnames(out.mat) <- colnames(mod.coord)
# id.name   <- as.character(interactive.points$names)
# id.name.coord <- vector()
# 
# for ( i in 1:length(id.name)){
# ind.mat        <- rbind(mod.coord[im[i,],])
# id.name.coord  <- c(id.name.coord, rep(id.name[i], nrow(ind.mat)))
# out.mat        <- rbind(out.mat, ind.mat)
# }
# mod.coord.all  <- cbind(out.mat[-1,], names=id.name.coord)
# colnames(mod.coord.all) <- c("x", "y", "label", "freq", "variable", "point.color", "names")
# 
# 
# des.data <- data.frame(names=result$names.ind, result$ctr.ind[,1:5])
# 
# des.data <- melt(des.data, id.vars="names")
# 
# q <- ggplot() + geom_line(data=des.data, aes(x=value, y=variable, showSelected=names)) + coord_flip()
# q <- q + make_text(des.data, max(des.data$value) * 1.1, 3, "names")
# 
# p <- p  + geom_point(data=interactive.points, aes(x=x, y=y, clickSelects=names))
# p <- p  + geom_point(data=interactive.points, aes(x=x, y=y, showSelected=names), color="red")
# 
# z <- ggplot() + geom_point(data=mod.coord.all, aes(x=x, y=y, showSelected=names))
# z <- z + geom_text(data=mod.coord.all, aes(x=x, y=y, label=label, showSelected=names), vjust=0.5)
# 
# gg2animint(list(plot1=p, plot2=q, plot3=z), out.dir = "test.p")
# 
# 
# 
# 
