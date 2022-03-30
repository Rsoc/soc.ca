#' Extract coordinates for the categories from an soc.mca
#'
#' @param result a soc.mca object
#' @param dim the dimension
#'
#' @return a data.frame with coordinates and frequences
#' @export
#' @examples 
#' example(soc.mca)
#' extract_mod(result)

extract_mod         <- function(result, dim = 1:2){
  coord.mod           <- result$coord.mod[, dim]
  rownames(coord.mod) <- result$names.mod
  coord.mod           <- coord.mod[,]
  colnames(coord.mod) <- c("X", "Y")
  
  md            <- coord.mod %>% data.frame() %>% tibble::rownames_to_column(var = "Modality")
  ctr           <- result$ctr.mod[, dim]
  md$ctr.x      <- ctr[, 1]
  md$ctr.y      <- ctr[, 2]
  md$ctr        <- rowSums(ctr) / 2
  md$ctr.set    <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md$Frequency  <- result$freq.mod
  md$Variable   <- result$variable
  md$label      <- result$labels.mod
  if(!is.null(result$headings)) md$headings = result$headings
  md            <- md %>% tibble()
  md
}

#' Extract supplementary categories from an soc.mca
#'
#' @param result a soc.mca object
#' @param dim the dimensions
#'
#' @return a data.frame with coordinates and frequences
#' @export
#' @examples
#' example(soc.mca)
#' extract_sup(result)
 
extract_sup         <- function(result, dim = 1:2){
  coord.sup           <- result$coord.sup[, dim]
  rownames(coord.sup) <- result$names.sup
  coord.sup           <- coord.sup[,]
  colnames(coord.sup) <- c("X", "Y")
  
  md                  <- coord.sup %>% data.frame() %>% rownames_to_column(var = "Modality")
  md$Frequency        <- result$freq.sup
  md$Variable         <- result$variable.sup
  md                  <- md %>% tibble()
  md
}

#' Extract individuals
#'
#' @param result a soc.ca object 
#' @param dim the dimensions
#'
#' @return a data.frame with coordinates and frequences
#' @export
#' @examples 
#' example(soc.mca)
#' extract_ind(result)

extract_ind         <- function(result, dim = 1:2){
  coord.ind           <- result$coord.ind[, dim]
  rownames(coord.ind) <- result$names.ind
  coord.ind           <- coord.ind[,]
  colnames(coord.ind) <- c("X", "Y")
  
  md           <- coord.ind %>% data.frame() %>% rownames_to_column(var = "Individual")
  ctr          <- result$ctr.ind[, dim]
  md$ctr.x     <- ctr[, 1]
  md$ctr.y     <- ctr[, 2]
  md$ctr       <- rowSums(ctr) / 2
  md$ctr.set   <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md           <- md %>% tibble()
  md
}


#' Create the base of a soc.ca map
#'
#' @param up the name of + pole on the vertical axis - "North"
#' @param down the name of the - pole on the vertical axis - "South"
#' @param right the name of the + pole on horizontal axis - "East"
#' @param left the name of the - pole on the horizontal axis - "West"
#' @param base_size controls the text size of themed labels
#' @param ... further arguments are passed onto ggplot()
#'
#' @return a ggplot2 object
#' @export

map.ca.base <- function(up = NULL, down = NULL, right = NULL, left = NULL, base_size = 15, ...){
  
  breaks.major <- seq(-100, 100, by = 0.25)
  labels       <- breaks.major
  labels[c(FALSE, TRUE)] <- ""
  
  p      <- ggplot(...) + geom_vline(xintercept = 0, size = 0.2) + geom_hline(yintercept = 0, size = 0.2)
  p      <- p + scale_x_continuous(sec.axis = sec_axis(~.*1, name = up, breaks = breaks.major, labels = labels),  
                                   name = down, breaks = breaks.major, labels = labels)
  
  
  p      <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = right, breaks = breaks.major, labels = labels),  
                                   name = left, breaks = breaks.major, labels = labels)  
  
  p      <- p + theme(axis.title.y.left = element_text(size = 16), axis.title.y.right =  element_text(size = 16))
  p      <- p + theme(axis.title.x.top = element_text(size = 16), axis.title.x.bottom =  element_text(size = 16))
  
  
  theme_ca_base <- function (base_size = 15, base_family = "serif", ticks = TRUE) 
  {
    ret <- theme_bw(base_family = base_family, base_size = base_size) + 
      theme(legend.background = element_blank(), legend.key = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            strip.background = element_blank(), plot.background = element_blank(), 
            axis.line = element_blank(), panel.grid = element_blank())
    if (!ticks) {
      ret <- ret + theme(axis.ticks = element_blank())
    }
    ret
  }
  
  p      <- p + theme_ca_base(base_size = base_size)
  
  
  p      <- p + scale_size_continuous(range = c(0.1, 2))
  p      <- p + theme(legend.position = "bottom")
  p      <- p + coord_fixed()
  p
}


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

#' Add a layer with density curves to an mca map.
#'
#' @param object a soc.mca result object
#' @param dim a numeric vector with the dimensions for the plane
#' @param ind a data.frame with coordinates of cases as produced by \link(extract_ind). This controls the points that are used for the density curves.
#' @param mapping a call to \link(aes) from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and linetype.
#' @param ... further arguments is passed onto \link(geom_density_2d)
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
