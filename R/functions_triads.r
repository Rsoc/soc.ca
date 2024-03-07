triangle.coordinates <- function(result, tri, dim = 1:2){
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
  coords$"Distance between center and combined" <- coords %>% filter(.data$type != "tri") %>% select(.data$X, .data$Y) %>% dist()
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
  
  p <- map.ca.base(data = coords, mapping = aes(x = .data$X, y = .data$Y, shape = .data$name, group = .data$triangle, color = .data$triangle))
  p <- p + geom_polygon(fill = NA, size = 0.3)
  p <- p + geom_point(size = 5, shape = 21, fill = "white", color = "white") + geom_point(size = 3, fill = "black")
  p <- p + facet_wrap(~mca) + theme_bw() + theme(strip.background = element_rect(fill = "white"),
                                                 panel.grid.minor = element_blank(),
                                                 panel.grid.major = element_line(linetype = "dotted", color = "grey90"))
  p <- p + scale_color_manual(values = colors) + scale_shape_manual(values = LETTERS)
  p + coord_fixed()
}



soc.mca.triads <- function(r,
                           triads = list("A" = c(), "B" = c(), "C" = c(), "D" = c()),
                           ind = r$indicator.matrix.active,
                           dim = c(1, 2)
){
  
  # Triads as a tibble
  td       <- triads %>% stack() %>% select(category = values, triad = ind)
  
  # The indicator matrix
  ind      <- ind[, colnames(ind) %in% td$category]
  
  # Coordinates of each point
  cat.coords   <- supplementary.categories(r, ind, dim = dim) %>% left_join(td, by = c("Modality" = "category")) %>% mutate(category = Modality)
  
  # Test: Are some of the categories from the same variable. That is not allowed since then they cannot have a positive relation
  test <- cat.coords %>% group_by(triad) %>% summarise(variables = n_distinct(Variable), n = n())
  if(any(test$variables < test$n)) warning("One of your triads has more than 1 category from the same variable")
  
  # Category relations
  rel       <- triads %>% map(~ .x %>% combn(2) %>% t() %>% data.frame() %>% select(A = X1, B = X2)) %>% bind_rows(.id = "triad")
  cat.rel   <- get.category.relations(r, ind = ind, dim = dim, rel = rel[, -1])
  cat.rel   <- inner_join(cat.rel, rel, by = c("A", "B"))
  
  # Combination points
  combination.points <- calculate.combination.points(triads, r, dim, ind)
  
  # Point shapes
  cat.coords <- left_join(td, cat.coords,  by = c("category", "triad"))
  
  lets <- c(LETTERS, letters)
  lets <- c(lets, paste0(lets, lets))
  
  cat.coords$shape     <- lets[1:nrow(cat.coords)]
  
  r$triads             <- triads
  r$triads.ind         <- ind
  r$triads.dim         <- dim
  r$triads.coords      <- cat.coords
  r$triads.edges       <- cat.rel
  r$combination.points <- combination.points
  r
}





#' Compare MCA's with triads
#'
#' @param l.mca a list of soc.mca objects
#' @param l.triads a list of triads
#' @param dim the dimensions of the plane
#' @param fix.mca the indice of the mca that is used as a fixpoint for the axis across mca's
#'
#' @return a triad object
#' @export

mca.triads <- function(l.mca, l.triads, dim = c(1,2), fix.mca = 1){
  
  tri.names <- map(l.triads, .f = function(x) x %>% select(-starts_with("id")) %>% colnames()) %>% stack() %>% rename(name = .data$values, triangle = .data$ind)
  
  # List of joined supplementary variables
  create_l.sup <- function(mca, l.triads){
    f          <- function(mca, tri) tibble(id  = mca$names.ind) %>% left_join(tri, by = "id")
    l.triads %>% map(f, mca = mca) %>% reduce(full_join, by = "id")
  }
  
  l.sup        <- l.mca %>% map(create_l.sup, l.triads)
  
  # Sup coordinates
  get.sup   <- function(mca, sup, dim){
    sup.coord <- sup %>% select(-id) %>% map(~average.coord(mca, .x, dim = dim)) %>% bind_rows(.id = "name") %>% dplyr::filter(.data$label)
    sup.coord <- sup.coord %>% left_join(., tri.names, by = "name") %>% select(-.data$group, -.data$label)
    sup.coord
  }
  
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  # Aligning coordinates
  coords     <- bind_rows(l.sup.coord, .id = "mca")
  max.coords <- coords %>% select(.data$name, .data$X, .data$Y) %>% gather(key = "key", value = "value", -.data$name) %>% group_by(.data$name) %>%
    summarise(median = median(sqrt(.data$value^2)))
  
  coords        <- coords[coords$name == max.coords$name[which.max(max.coords$median)],]
  cat("\n", "Category used for fixing: ", coords$name[1], "\n")
  
  coords        <- coords %>% select(.data$X, .data$Y)
  direction     <- coords > 0
  fix.direction <- coords[fix.mca,] > 0
  
  flip                 <- t(direction) != as.vector(fix.direction)
  flip                 <- lapply(data.frame(flip), which)
  flip                 <- flip %>% map(~dim[.x])
  
  l.mca                <- map2(l.mca, flip, invert)
  
  # Recalculate sup coordinates
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  list(l.mca = l.mca, l.sup.coord= l.sup.coord, l.ind = l.sup)
}

#' Get and calculate the relationships and oppositions between each pair of categories
#' 
#' Use this function to calculate PEM \link[GDAtools]{pem} values, chisq, distance and coordinates for each pair of categories in either an indicator matrix or the categories from an soc.mca result object. These relationship are usefull for both diagnostics, analysis, interpretation and plotting. 
#' For plotting combine with \link{add.category.relations} to build your plot. 
#' 
#'
#' @param r an soc.mca result object
#' @param ind an indicator matrix, see \link{indicator}
#' @param dim a numeric vector with the dimensions for the coordinates. This is only sent to \link{extract_mod}.
#' @param variable a character vector with the variable where each category in ind came from. If ind was created directly with \link{indicator} you can use names(colnames(ind)).
#' @param coords a data.frame with coordinates - similar to those produced by \link{extract_mod}
#' @param rel a matrix with pairs of categories
#'
#' @return a tibble
#' @export
#'
#' @examples
#' example(soc.mca)
#' get.category.relations(result)

get.category.relations <- function(r, ind = r$indicator.matrix.active, dim = c(1, 2),
                                   rel = t(combn(colnames(ind),2))
){
  
  coords           <- supplementary.categories(r, ind, dim)
  coords$category  <- coords$Modality
  
  el         <- rel 
  v          <- coords %>% select(category, variable = Variable)
  el         <- tibble(x = el[,1], y = el[,2])
  
  
  el <- left_join(el, v, by = c("x" = "category")) %>% rename(variable.x = variable) %>% left_join(v, by = c("y" = "category")) %>% rename(variable.y = variable)
  
  el         <- el %>% filter(variable.x != variable.y)
  
  f.pem <- function(x, y){
    a <- ind[, x] %>% factor(levels = c("0", "1"))
    b <- ind[, y] %>% factor(levels = c("0", "1"))
    o <- soc.ca:::pem_fast(a, b)
    o[2,2]
  }
  
  f.chisq <- function(x, y){
    a <- ind[, x] %>% factor(levels = c("0", "1"))
    b <- ind[, y] %>% factor(levels = c("0", "1"))
    o <- suppressWarnings(chisq.test(table(a, b)))
    o
  }
  
  o          <- tibble(A = el$x, B = el$y)
  c.A        <- coords %>% select(A = category, X, Y, label, Variable)
  c.B        <- coords %>% select(B = category, Xend = X, Yend = Y, label_end = label, variable_end = Variable)
  o          <- left_join(o, c.A, by = "A") |> left_join(c.B, by = "B")
  
  cross <- function(x, y, xend, yend){
    px <- prod(x, xend)
    py <- prod(y, yend)
    cross <- px < 0 | py < 0
    cross
  }
  
  calc.dist <- function(x, y, xend, yend){
    #cat("x", x, "y", y, "xend", xend, "yend", yend)
    d <- cbind(x = c(x,xend), y = c(y, yend)) |> dist()
    as.numeric(d)
  }
  
  o <- o %>% rowwise %>% mutate(pem = f.pem(A, B)) |> mutate(chisq = list(f.chisq(A, B)))
  o <- o %>% rowwise %>% mutate("N both" = chisq$observed[2, 2], expected = chisq$expected[2, 2], chisq.p.value = chisq$p.value)
  o <- o %>% rowwise %>% mutate(opposed = cross(X, Y, Xend, Yend)) |> mutate(distance =  calc.dist(X, Y, Xend, Yend))
  o <- o |> mutate(valid.opposition = expected > 5 &  chisq.p.value < 0.05 & distance > 0.75 &  pem < -25,
                   valid.correlation = expected > 5 &  chisq.p.value < 0.05 & pem > 10)
  o
}

add.combination.points <- function (r, combined.coord = r$combination.points$combined.coord, dim = c(1, 2), draw.levels = c("1", "+2"), mapping = aes(color = triad, label = category, group = triad, size = Frequency), 
                                    points = TRUE, draw.labels = FALSE, linetype = "dashed", linesize = 0.3,
                                    ...) 
{
  
  cats <- combined.coord %>% filter(label %in% draw.levels)
  mapping <- soc.ca:::add_modify_aes(mapping, aes(x = X, y = Y))
  o <- list()
  
  mapping_lines <- soc.ca:::add_modify_aes(mapping, aes(label = NULL, size = NULL))
  o$lines       <- geom_path(data = cats, mapping = mapping_lines, linetype = linetype,
                             ...)
  
  if (identical(points, TRUE)) {
    mapping_points <- soc.ca:::add_modify_aes(mapping, aes(label = NULL))
    o$points <- geom_point(data = cats, mapping = mapping_points, 
                           ...)
  }
  
  if (identical(draw.labels, TRUE)) {
    o$text <- geom_text(data = cats, mapping = mapping, check_overlap = check_overlap, 
                        ...)
  }
  o
}

add.triads <- function(r, triads.coords = r$triads.coords, triad.edges = r$triads.edges, mapping = aes(shape = category, color = triad), mapping_triads = aes(color = triad, alpha = pem),
                       point.size = 3, pem.cut = 0.5, valid.correlations.only = FALSE,
                       ...){

  tec           <- triad.edges %>% group_by(triad) %>% summarise(edges = sum(pem > 0), .groups = "drop_last") %>% ungroup()
  triads.coords <- left_join(triads.coords, tec, by = c("triad"))

  if(valid.correlations.only == TRUE){
  triad.edges$pem[triad.edges$valid.correlation == FALSE] <- 0
  }
  
  triad.edges$pem[triad.edges$pem <= 0] <- 0
  triad.edges$pem[triad.edges$pem > pem.cut] <- pem.cut

  mapping        <- soc.ca:::add_modify_aes(mapping, aes(x = X, y = Y))
  mapping_triads <- soc.ca:::add_modify_aes(mapping_triads, aes(x = X, xend = Xend, y = Y, yend = Yend))

  shape.scale <- LETTERS
  if(!is.null(triads.coords$shape)) shape.scale <- setNames(triads.coords$shape, triads.coords$category) 
  
  
  o <- list()

  o$segment       <- geom_segment(data = triad.edges, mapping = mapping_triads, size = 0.3, ...)

  o$background <- geom_point(data = triads.coords, mapping = mapping, size = point.size + 2, shape = 21, alpha = 0.8, fill = "white", color = "white")
  o$ring       <- geom_point(data = triads.coords, mapping = mapping, size = point.size + 2, shape = 21, alpha = 0.8, fill = "white")
  o$points     <- geom_point(data = triads.coords, mapping = mapping, size = point.size, ...)
  o$shape      <- scale_shape_manual(values = shape.scale)
  o$alpha      <- scale_alpha(range = c(0,1), limits = c(0, pem.cut), guide = "none")
  o
}

calculate.combination.points <- function(triads, r, dim = c(1,2), ind = r$indicator.matrix.active){
  
  comb.fact  <- map(triads, ~rowSums(ind[, colnames(ind) %in% .x])) |> bind_cols()
  comb.fact[comb.fact > 2] <- 2
  comb.fact  <- comb.fact %>% modify(~as.character(.x) %>% fct(, levels = as.character(0:2)) %>% fct_recode("+2" = "2")) 
  
  comb.coord <- soc.ca::supplementary.categories(r, comb.fact)
  comb.coord <- comb.coord %>% mutate(triad = Variable, category = Modality)
  # PEM network
  ind.comb    <- indicator(comb.fact)
  rel         <- t(combn(colnames(ind.comb),2))
  comb.pem    <- get.category.relations(r, ind = ind.comb, dim = dim, rel = t(combn(colnames(ind.comb),2)))
  
  o                  <- list()
  o$combined.factors <- comb.fact
  o$combined.coord   <- comb.coord
  o$combined.pem     <- comb.pem
  o
}

fixate.coordinates <- function(l.mca, fix.mca = 1){
  
  l.coords <- map(l.mca, ~ .x$triads.coords)
  dim      <- l.mca[[1]]$triads.dim
  
  # Aligning coordinates
  coords     <- bind_rows(l.coords, .id = "mca")
  max.coords <- coords %>% select("Modality", "X", "Y") %>% gather(key = "key", value = "value", -Modality) %>% group_by(Modality) %>%
    summarise(median = median(sqrt(value^2)))
  
  coords        <- coords[coords$Modality == max.coords$Modality[which.max(max.coords$median)],]
  cat("\n", "Category used for fixing: ", coords$Modality[1], "\n")
  
  coords        <- coords %>% select("X", "Y")
  direction     <- coords > 0
  fix.direction <- coords[fix.mca,] > 0
  
  flip                 <- t(direction) != as.vector(fix.direction)
  flip                 <- lapply(data.frame(flip), which)
  flip                 <- flip %>% map(~dim[.x])
  
  l.mca                <- map2(l.mca, flip, invert)
  
  # Recalculate triads
  l.mca <- l.mca %>% map(~ soc.mca.triads(r = .x, ind = .x$triads.ind, triads = .x$triads, dim = .x$triads.dim))
  
  l.mca
}

