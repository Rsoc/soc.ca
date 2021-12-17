library(soc.ca)
library(tidyverse)
example(soc.mca)

add.ind    <- function(object, dim = c(1, 2), ind = extract_ind(object, dim), mapping = aes(), ...){

mapping    <- add_modify_aes(mapping, aes(x = X, y = Y))

o <- list()
o$points    <- geom_point(data = ind, mapping = mapping, ...)
o

}

add_modify_aes <- function(mapping, ...) {
  ggplot2:::rename_aes(modifyList(mapping, ...))  
}

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


map.ca.base() + add.categories(result)                               # Alle aktive kategorier
map.ca.base() + add.categories(result, repel = TRUE)                 # Med repel
map.ca.base() + add.categories(result, check_overlap = TRUE)         # Med check overlap
map.ca.base() + add.categories(result, "ctr")                        # De der bidrager over gennemsnit til planet
map.ca.base() + add.categories(result, "sup")                        # De prædefinerede supplementære
map.ca.base() + add.categories(result, cats = supplementary.categories(result, active[, 1:2])) # Selvstændigt definerede supplementære
map.ca.base() + add.categories(result, "sup", color = "red") + add.categories(result, "all", color = "black")  

map.ca.base() + add.categories(result, mapping = aes(color = Variable, label = label))
map.ca.base() + add.categories(result, "sup", points = TRUE)

map.ca.base() + add.ind(result, mapping = aes(color = active$Film))
add.ind(result)

map.ca.base() + add.ind(result, mapping = aes(color = active$TV))
var <- active$TV == "Tv-Comedy" 
map.ca.base() + add.ind(result, mapping = aes(color = var)) + add.ellipse(result, var = var, draw = "TRUE") 

example(headings)

cats <- extract_mod(result.headings)

map.ca.base() + add.categories(cats = cats, mapping = aes(group = headings, color = Variable, label = label)) + facet_wrap(~headings)
