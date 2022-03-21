library(soc.ca)
library(tidyverse)
example(soc.mca)

#' Add a layer of cases (individuals) to an mca map
#'
#' @param object a soc.mca result object
#' @param dim a numeric vector with the plotted dimensions
#' @param ind a data.frame with coordinates of cases as produced by \link(extract_ind). This controls the plotted points.
#' @param mapping a call to \link(aes) from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and shape.
#' @param ... further arguments are passed on to geom_point()
#'
#' @return a ggplot2 object that can be added to an existing plot like those produced by \link(map.ca.base)
#' @export add.ind
#'
#' @examples
#' example(soc.mca)
#' map.ca.base() + add.ind(result)

add.ind    <- function(object, dim = c(1, 2), ind = extract_ind(object, dim), mapping = aes(), ...){

mapping    <- add_modify_aes(mapping, aes(x = X, y = Y))

o <- list()
o$points    <- geom_point(data = ind, mapping = mapping, ...)
o

}

add_modify_aes <- function(mapping, ...) {
  ggplot2:::rename_aes(modifyList(mapping, ...))  
}

#' Add a layer of categories (modalities) to an mca map
#'
#' @param object a soc.mca result object
#' @param preset a character string selecting among presets. If "active" - no
#'   change is made to "cats".
#' @param dim a numeric vector with the dimensions for the plane
#' @param cats a data.frame with coordinates of categories as produced by \link(extract_mod) or \link(extract_sup). This controls the plotted points.
#' @param mapping a call to \link(aes) from the ggplot2 package. Here you can map aesthetics to variables such as color, alpha, size and family.
#' @param repel if TRUE label position is adjusted to lower overlap
#' @param check_overlap if TRUE overlapping categories are removed
#' @param points if TRUE points are plotted
#' @param ... further arguments are passed onto \link(geom_text) or \link(geom_text_repel)
#'
#' @return a ggplot2 object that can be added to an existing plot like those produced by \link(map.ca.base)
#' @usage The presets adds, replaces or filters from the categories in cats.
#'   "ctr" returns all categories contributing above average to the plane
#'   defined in dim. "sup" returns the supplementary categories from object.
#'   "all" returns both supplementary and active categories.
#' @export add.categories
#'
#' @examples
#' example(soc.mca)
#' map.ca.base() + add.categories(result, check_overlap = TRUE)
#' map.ca.base() + add.categories(result, preset = "all", mapping = aes(color = type, label = Modality), repel = TRUE)
add.categories  <- function(object, preset = c("active", "ctr", "sup", "all"), dim = c(1, 2),
                            cats = extract_mod(object, dim),
                            mapping = aes(label = Modality), repel = FALSE, check_overlap = FALSE,
                            points = FALSE,
                            ...){
  
  if(identical(preset, "ctr")) cats <- cats %>% filter(ctr.set)
  if(identical(preset, "sup")) cats <- extract_sup(object, dim)
  if(identical(preset, "all")){
    cats <- bind_rows(active = cats, sup = extract_sup(object, dim), .id = "type")
  }
  mapping <- add_modify_aes(mapping, aes(x = X, y = Y))
  
  o <- list()
  
  if(identical(points, TRUE)){
    mapping_points <- add_modify_aes(mapping, aes(label = NULL))
    o$points    <- geom_point(data = cats, mapping = mapping_points, ...)
  }
  
  o$text    <- geom_text(data = cats, mapping = mapping, check_overlap = check_overlap, ...)

  if(identical(repel, TRUE)){
  o$text    <- ggrepel::geom_text_repel(data = cats, mapping = mapping, ...)
  }
  o
}

#' Add a layer with concentration ellipses to an mca map.
#'
#' @param object a soc.mca result object
#' @param var a factor 
#' @param draw a character vector with the levels to draw ellipses for
#' @param dim a numeric vector with the dimensions for the plane
#' @param el a data.frame produced by the \link(ellipses) function.
#' @param mapping a call to \link(aes) from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and linetype.
#' @param draw.axis if TRUE the axis within the concentration ellipse is drawn.
#' @param ... further arguments is passed onto \link(geom_path) and \link(geom_line)
#'
#' @return a ggplot2 object that can be added to an existing plot like those produced by \link(map.ca.base)
#' @export add.ellipse
#'
#' @examples
#' example(soc.mca)
#' map.ca.base() + add.ind(result, mapping = aes(color = sup$Gender)) + add.ellipse(result, sup$Gender)
#' map.ca.base() + add.ind(result, mapping = aes(color = sup$Age == "65+")) + add.ellipse(result, sup$Age == "65+", draw = "TRUE")

add.ellipse <- function(object, var = NULL, draw = unique(var), dim = c(1, 2), el = ellipses(object, var = var, dim = dim),
                        mapping = aes(color = Category), draw.axis = TRUE, ...){
  
  el <- el %>% filter(Category %in% draw)
  
  ec        <- el$ellipse
  names(ec) <- el$Category
  ec        <-  bind_rows(ec, .id = "Category")
  
  p1 <- el$principal_dim_1
  names(p1) <- el$Category
  p1 <- bind_rows(p1, .id = "Category")
  
  p2 <- el$principal_dim_2
  names(p2) <- el$Category
  p2 <- bind_rows(p2, .id = "Category")
  
  p12 <- bind_rows(list(p1 = p1, p2 = p2), .id = "type")
  p12$type <- paste(p12$type, p12$Category)
  
  mapping <- add_modify_aes(mapping, aes(x = x, y = y))
  mapping.p12 <- add_modify_aes(mapping, aes(group = type))
  
  o <- list()
  o$ellipse.path <- geom_path(data = ec, mapping = mapping, ...)
  o$ellipse.line.x <- geom_line(data = p12, mapping = mapping.p12, ...)
  o
}

#' Title
#'
#' @param object 
#' @param dim 
#' @param ind 
#' @param mapping 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
add.density <-  function(object, dim = c(1, 2), ind = extract_ind(object, dim), mapping = aes(), ...){

  mapping    <- add_modify_aes(mapping, aes(x = X, y = Y))
  o <- list()
  o$density    <- geom_density_2d(data = ind, mapping = mapping, ...)
  o
}

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
var <- sup$Income == "GBP: >=60"
var <- sup$Income == "GBP: <9"
var <- active$TV == "Tv-Sport"

map.ca.base() + add.ind(result, mapping = aes(color = var)) + add.ellipse(result, var = var, draw = "TRUE") 

map.ca.base() + add.hex.summary(result, var, bins = 10)
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


