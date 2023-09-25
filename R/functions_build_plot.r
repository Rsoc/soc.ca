#' Extract coordinates for the categories from an soc.mca
#'
#' @param result a soc.mca object or a PCA
#' @param dim the dimensions
#'
#' @return a data.frame with coordinates and frequences
#' @export
#' @examples 
#' example(soc.mca)
#' extract_cats(result)

extract_cats         <- function(result, dim = 1:3){
  
  if(what.is.x(result) %in% c("PCA", "gPCA")){
    result <- from.pca.to.soc.ca(result)
  }
  
  
  coord.mod           <- result$coord.mod[, dim]
  rownames(coord.mod) <- result$names.mod
  coord.mod           <- coord.mod[,]
  colnames(coord.mod) <- c("X", "Y", "Z", LETTERS)[1:length(dim)]
  
  md            <- coord.mod %>% data.frame() %>% tibble::rownames_to_column(var = "Modality")
  ctr           <- result$ctr.mod[, dim]
  
  colnames(ctr) <- paste0("ctr.", colnames(coord.mod))
  md            <- bind_cols(md, ctr)
  
  md$ctr        <- rowSums(ctr) / length(dim)
  md$ctr.set    <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md$Frequency  <- result$freq.mod
  md$Variable   <- result$variable
  md$label      <- result$labels.mod
  if(!is.null(result$headings)) md$headings = result$headings
  md            <- md %>% dplyr::tibble()
  md
}

#' @rdname extract_cats
#' @export

extract_mod <- extract_cats   


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
 
extract_sup         <- function(result, dim = 1:3){

  coord.sup           <- result$coord.sup[, dim]
  rownames(coord.sup) <- result$names.sup
  coord.sup           <- coord.sup[,]
  colnames(coord.sup) <- c("X", "Y", "Z", LETTERS)[1:length(dim)]
  
  md                  <- coord.sup %>% data.frame() %>% rownames_to_column(var = "Modality")
  md$Frequency        <- result$freq.sup
  md$Variable         <- result$variable.sup
  md                  <- md %>% tibble()
  md
}

#' Extract individuals
#'
#' @param result a soc.ca object or a PCA
#' @param dim the dimensions
#'
#' @return a data.frame with coordinates and frequences
#' @export
#' @examples 
#' example(soc.mca)
#' extract_cases(result)

extract_cases         <- function(result, dim = 1:3){
  
  # If the result is a PCA
  if(what.is.x(result) %in% c("PCA", "gPCA")){
    result <- from.pca.to.soc.ca(result)
  }
  
  
  coord.ind           <- result$coord.ind[, dim]
  rownames(coord.ind) <- result$names.ind
  coord.ind           <- coord.ind[,]
  colnames(coord.ind) <- c("X", "Y", "Z", LETTERS)[1:length(dim)]
  
  md           <- coord.ind %>% data.frame() %>% rownames_to_column(var = "Individual")
  ctr          <- result$ctr.ind[, dim]
  
  colnames(ctr) <- paste0("ctr.", colnames(coord.ind))
  md           <- tibble(md, ctr)
  
  md$ctr       <- rowSums(ctr) / length(dim)
  md$ctr.set   <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  
  md$weight    <- result$weight
  
  md           <- md %>% tibble()
  md
}

#' @rdname extract_cases
#' @export

extract_ind <- extract_cases   

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
#' @param ind a data.frame with coordinates of cases as produced by \link{extract_ind}. This controls the plotted points.
#' @param mapping a call to \link{aes} from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and shape.
#' @param ... further arguments are passed on to geom_point()
#'
#' @return a ggplot2 object that can be added to an existing plot like those produced by \link{map.ca.base}
#' @export add.ind
#'
#' @examples
#' example(soc.mca)
#' map.ca.base() + add.cases(result)

add.cases    <- function(object, dim = c(1, 2), ind = extract_ind(object, dim), mapping = aes(), ...){
  
  mapping    <- add_modify_aes(mapping, aes(x = X, y = Y))
  
  o <- list()
  o$points    <- geom_point(data = ind, mapping = mapping, ...)
  o
  
}

#' @rdname add.cases
#' @export

add.ind <- add.cases   



add_modify_aes <- function(mapping, ...) {
  ggplot2:::rename_aes(modifyList(mapping, ...))  
}

#' Add a layer of categories (modalities) to an mca map
#'
#' @param object a soc.mca result object
#' @param preset a character string selecting among presets. If "active" - no
#'   change is made to "cats".
#' @param dim a numeric vector with the dimensions for the plane
#' @param cats a data.frame with coordinates of categories as produced by \link{extract_mod} or \link{extract_sup}. This controls the plotted points.
#' @param mapping a call to \link{aes} from the ggplot2 package. Here you can map aesthetics to variables such as color, alpha, size and family.
#' @param repel if TRUE label position is adjusted to lower overlap
#' @param check_overlap if TRUE overlapping categories are removed
#' @param points if TRUE points are plotted
#' @param ... further arguments are passed onto \link{geom_text} or \link{geom_text_repel}
#'
#' @return a ggplot2 object that can be added to an existing plot like those produced by \link{map.ca.base}
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
                            points = FALSE, draw.labels = TRUE,
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
  
  if(identical(draw.labels, TRUE)){
  o$text    <- geom_text(data = cats, mapping = mapping, check_overlap = check_overlap, ...)
  }
  
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
#' @param el a data.frame produced by the \link{ellipses} function.
#' @param mapping a call to \link[ggplot2]{aes} from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and linetype.
#' @param draw.axis if TRUE the axis within the concentration ellipse is drawn.
#' @param ... further arguments is passed onto \link[ggplot2]{geom_path} and \link[ggplot2]{geom_line}
#'
#' @return a ggplot2 object that can be added to an existing plot like those produced by \link{map.ca.base}
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
#' @param ind a data.frame with coordinates of cases as produced by \link{extract_ind}. This controls the points that are used for the density curves.
#' @param mapping a call to \link[ggplot2]{aes} from the ggplot2 package. Here you can map aesthetics to variables such as color, fill, alpha, size and linetype.
#' @param ... further arguments are passed onto \link[ggplot2]{geom_density_2d}
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


#' Annotate labels to the quadrants of an MCA or any ggplot2 based quadrant plot.
#' 
#' This function is a convience function that uses \link[ggpp]{annotate} to easily create labels for the quadrants.
#'
#' @param quadrant.labels 
#' @param distance if equal to "npc" labels are positioned dynamically at the edges of the plot. see \link[ggpp]{annotate}. If a numeric vector it is interpreted as the distance to 0 on both X and Y.
#' @param geom controls the annotation geom; usually you would use "text" or "label".
#' @param color either a single value or 4 values that control the color of the labels
#' @param ... further arguments are passed onto \link[ggpp]{annotate}
#'
#' @return a ggplot2 layer that can be added to an existing ggplot object.
#' @export
#'
#' @examples
#' example(soc.mca)
#' map.ind(result, point.size = 1) + add.quadrant.labels()
#' labels <- c("Dominant:\nCultural fraction", "Dominant:\nEconomic fraction", "Dominated:\nEconomic fraction", "Dominated:\nCultural fraction")
#' map.ca.base() + add.quadrant.labels(labels, geom = "text")
#' map.ca.base() + add.ind(result, color = "grey80") + add.quadrant.labels(labels, geom = "text", distance = 1) 
#' map.ca.base() + add.categories(result, color = "grey50", check_overlap = TRUE) + add.quadrant.labels(labels, geom = "label", distance = 0.5, fill = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"), alpha = 0.3) 

# It could have origo labels.
add.quadrant.labels <- function(quadrant.labels = c("A", "B", "C", "D"), distance = "npc", geom = "label", color = "black", ...){
  
  if(is.numeric(distance)){
    ql <- tibble(quadrant.labels, X = distance, Y = distance) %>% mutate(X = X * c(-1, 1, 1, -1), Y = Y * c(1, 1, -1, -1))
    a <- ggpp::annotate(geom = geom, x = ql$X, y = ql$Y, label = ql$quadrant.labels, color = color, ...)
  }
  
  if(identical(distance, "npc")){
    geom <- paste0(geom, "_npc")
    a <- ggpp::annotate(geom = geom, npcx = c("left", "right", "right", "left"), npcy = c("top", "top", "bottom", "bottom"), label = quadrant.labels, color = color, ...)
  }
  a
}


add.category.relations <- function(r, cat.rel = get.category.relations(r, dim = dim) |> filter(valid.correlation), dim = c(1, 2),  mapping = aes(alpha = pem), ...){
  
  mapping <- add_modify_aes(mapping, aes(x = X, xend = Xend, y = Y, yend = Yend))
  o <- list()
  o$points <- geom_segment(data = cat.rel, mapping = mapping, ...)
  o
  
}
