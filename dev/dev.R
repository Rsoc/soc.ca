library(soc.ca)
library(tidyverse)
example(soc.mca)



add.hex.summary <- function(object, var, dim = c(1, 2), ind = extract_ind(object, dim), summary_fun = mean, ... ){
  
  stopifnot(is.logical(var) | is.numeric(var))
  ind$var <- var

  o <- list()
  o$hex  <-  geom_hex(data = ind, mapping = aes(x = X, y = Y, z = var*1), stat = "summary_hex", fun = summary_fun, ...) 
  o$fill <- scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, name = "Spectral"), guide = "legend")
  o
}



map.ca.base() + add.categories(result)                               # Alle aktive kategorier
map.ca.base() + add.categories(result, repel = TRUE)                 # Med repel
map.ca.base() + add.categories(result, check_overlap = TRUE)         # Med check overlap
map.ca.base() + add.categories(result, "ctr", size = 2)              # De der bidrager over gennemsnit til planet og nu ganske små
map.ca.base() + add.categories(result, "sup")                        # De prædefinerede supplementære
map.ca.base() + add.categories(result, cats = supplementary.categories(result, active[, 1:2])) # Selvstændigt definerede supplementære
map.ca.base() + add.categories(result, "sup", color = "red") + add.categories(result, "all", color = "black")  

map.ca.base() + add.categories(result, mapping = aes(color = Variable, label = label))
map.ca.base() + add.categories(result, "sup", points = TRUE)

map.ca.base() + add.ind(result, mapping = aes(color = active$Film)) 
map.ca.base() + add.ind(result) + add.density(result) 
map.ca.base() + add.ind(result, mapping = aes(color = active$Film)) + add.density(result, mapping = aes(color = active$Film))

map.ca.base() + add.ind(result, mapping = aes(color = active$TV))



var <- sup$Gender == "Women"
var[1:1200] <- NA
var[is.na(var)] <- FALSE
var %in% "nif"
var <- sup$Income == "GBP: >=60"
var <- sup$Income == "GBP: <9"
var <- active$TV == "Tv-Sport"

map.ca.base() + add.ind(result, mapping = aes(color = var)) + add.ellipse(result, var = var, draw = "TRUE") 

var <- active$TV == "Tv-Sport"
map.ca.base() + add.hex.summary(result, var, bins = 10, na.rm = T)

map.ca.base() + add.hex.summary(result, var, alpha = 0.5) + add.categories(object, cats = supplementary.categories(result, sup$Gender))
example(headings)

cats <- extract_mod(result.headings)

map.ca.base() + add.categories(cats = cats, mapping = aes(group = headings, color = Variable, label = label)) + facet_wrap(~headings)

x <- 1:10

p <- map.ca.base(up = "North", down = "south", right = "East", left = "West")
p + add.categories(result)






# select_proper_scale <- function(x){
# s <- list()  
# 
# # Binary?
# if (n_distinct(x)  == 2){
#   s <- scale_color_manual(values = getOption("soc.ca.colors.binary"))
#   return(s)
# }
# 
# 
# # Is it numerical?
# if(is.numeric(x){
# 
# s <- scale_color_viridis_c()  
# # Discrete or really numeric?
# 
# }
#   
# # Is it a categorical?
# if(is.factor(x)){
#   s <- scale_color_manual(values = getOption("soc.ca.colors"))
# 
#   # Ordered?
#   if(is.ordered(x)) s <- scale_color_brewer(type = "seq", palette = "YlOrRd")
#   return(s)
# }  
#   
# 
# s
# }
# select_proper_scale(1:10)
# x <- active$TV


