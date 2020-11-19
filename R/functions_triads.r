triangle.coordinates <- function(results, tri, dim = 1:2){
  ind                  <- result$indicator.matrix.active != 0
  ind                  <- ind[, tri]
  tri.members          <- ind %>% rowSums() == length(tri)
  coord.tri.members    <- average.coord(result, tri.members, dim = dim)[2,]
  l.tri.coords         <- lapply(as.data.frame(ind), average.coord, object = result, dim = dim)
  tri.coords           <- bind_rows(l.tri.coords, .id = "id")
  tri.coords           <- tri.coords[tri.coords$label == TRUE, ]
  av.point             <- c(X = mean(tri.coords$X), Y = mean(tri.coords$Y))
  coords               <- bind_rows(tri.coords, av.point, coord.tri.members)
  coords$label         <- c(tri, "Center", "Combined point")
  coords$is.tri        <- !is.na(coords$id)
  coords$is.center     <- coords$label == "Center"
  coords$type          <- c("tri", "tri", "tri", "center", "combined")
  coords$"Distance between center and combined" <- coords %>% filter(type != "tri") %>% select(X, Y) %>% dist()
  coords
}

mca.triad.array <- function(l.mca.triads){
  colors  <- c("black",
               brewer.pal(9, "Set1"))
  shapes  <- LETTERS
  
  coords   <-  l.mca.triads$l.sup.coord %>% bind_rows(.id = "mca")
  coords   <- coords[order(coords$triangle),]
  coords$name <- coords$name %>% as_factor() %>% fct_reorder(as.numeric(coords$triangle))
  coords$mca  <- coords$mca %>% as_factor() %>% fct_relevel(names(l.mca.triads$l.sup.coord))
  
  p <- map.ca.base(data = coords, mapping = aes(x = X, y = Y, shape = name, group = triangle, color = triangle))
  p <- p + geom_polygon(fill = NA, size = 0.3)
  p <- p + geom_point(size = 5, shape = 21, fill = "white", color = "white") + geom_point(size = 3, fill = "black")
  p <- p + facet_wrap(~mca) + theme_bw() + theme(strip.background = element_rect(fill = "white"),
                                                 panel.grid.minor = element_blank(),
                                                 panel.grid.major = element_line(linetype = "dotted", color = "grey90"))
  p <- p + scale_color_manual(values = colors) + scale_shape_manual(values = LETTERS)
  p + coord_fixed()
}

#' Title
#'
#' @param l.mca a list of soc.mca objects
#' @param l.triads 
#' @param dim 
#' @param fix.mca 
#'
#' @return
#' @export
#'
#' @examples

mca.triads <- function(l.mca, l.triads, dim = c(1,2), fix.mca = 1){
  
  tri.names <- map(l.triads, .f = function(x) x %>% select(-starts_with("id")) %>% colnames()) %>% stack() %>% rename(name = values, triangle = ind)
  
  # List of joined supplementary variables
  create_l.sup <- function(mca, l.triads){
    f          <- function(mca, tri) tibble(id  = mca$names.ind) %>% left_join(tri, by = "id")
    l.triads %>% map(f, mca = mca) %>% reduce(full_join, by = "id")
  }
  
  l.sup        <- l.mca %>% map(create_l.sup, l.triads)
  
  # Sup coordinates
  get.sup   <- function(mca, sup, dim){
    sup.coord <- sup %>% select(-id) %>% map(~average.coord(mca, .x, dim = dim)) %>% bind_rows(.id = "name") %>% dplyr::filter(label)
    sup.coord <- sup.coord %>% left_join(., tri.names, by = "name") %>% select(-group, -label)
    sup.coord
  }
  
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  # Aligning coordinates
  coords     <- bind_rows(l.sup.coord, .id = "mca")
  max.coords <- coords %>% select(name, X, Y) %>% gather(key = "key", value = "value", -name) %>% group_by(name) %>%
    summarise(median = median(sqrt(value^2)))
  
  coords        <- coords[coords$name == max.coords$name[which.max(max.coords$median)],]
  cat("\n", "Category used for fixing: ", coords$name[1], "\n")
  
  coords        <- coords %>% select(X, Y)
  direction     <- coords > 0
  fix.direction <- coords[fix.mca,] > 0
  
  flip                 <- t(direction) != as.vector(fix.direction)
  flip                 <- lapply(data.frame(flip), which)
  flip                 <- flip %>% map(~dim[.x])
  
  l.mca                <- map2(l.mca, flip, invert)
  
  # Recalculate sup coordinates
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  list(l.mca = l.mca, l.sup.coord= l.sup.coord)
}

