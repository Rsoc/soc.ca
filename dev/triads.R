# Triads functions -----

#' Title
#'
#' @param result 
#' @param dim 
#'
#' @return
#' @export
#'
#' @examples

extract_mod         <- function(result, dim = 1:2){
  coord.mod           <- result$coord.mod[, dim]
  rownames(coord.mod) <- result$names.mod
  coord.mod           <- coord.mod[,]
  colnames(coord.mod) <- c("X", "Y")
  
  md            <- coord.mod %>% data.frame() %>% rownames_to_column(var = "Modality")
  ctr           <- result$ctr.mod[, dim]
  md$ctr.x      <- ctr[, 1]
  md$ctr.y      <- ctr[, 2]
  md$ctr        <- rowSums(ctr) / 2
  md$ctr.set    <- (apply(ctr, 2, function(x) x >= mean(x)) %>% rowSums()) > 0
  md$Frequency  <- result$freq.mod
  md$Variable   <- result$variable
  md
}

extract_sup         <- function(result, dim = 1:2){
  coord.sup           <- result$coord.sup[, dim]
  rownames(coord.sup) <- result$names.sup
  coord.sup           <- coord.sup[,]
  colnames(coord.sup) <- c("X", "Y")
  
  md                  <- coord.sup %>% data.frame() %>% rownames_to_column(var = "Modality")
  md$Frequency        <- result$freq.sup
  md$Variable         <- result$variable.sup
  md
}

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
  md
}

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
    l.triads %>% map(f, mca = mca) %>% bind_cols() %>% select(-num_range("id", 1:1000)) # Removing duplicate id columns
  }
  
  l.sup        <- l.mca %>% map(create_l.sup, l.triads)
  
  # Sup coordinates
  get.sup   <- function(mca, sup, dim){
    #browser(skipCalls = 2)
    sup.coord <- sup %>% select(-id) %>% map(~average.coord(mca, .x, dim = dim)) %>% bind_rows(.id = "name") %>% filter(label)
    sup.coord <- sup.coord %>% left_join(., tri.names, by = "name") %>% select(-group, -label)
    sup.coord
  }
  
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  # Aligning coordinates
  coords     <- bind_rows(l.sup.coord, .id = "mca")
  max.coords <- coords %>% select(name, X, Y) %>% gather(key = "key", value = "value", -name) %>% group_by(name) %>%
    summarise(mean = mean(sqrt(value^2)))
  
  coords        <- coords[coords$name == max.coords$name[which.max(max.coords$mean)],] %>% select(X, Y)
  direction     <- coords > 0
  fix.direction <- coords[fix.mca,] > 0
  
  flip                 <- t(direction) != as.vector(fix.direction)
  flip                 <- lapply(data.frame(flip), which)
  l.mca                <- map2(l.mca, flip, invert)
  
  # Recalculate sup coordinates
  l.sup.coord   <- map2(l.mca, l.sup, get.sup, dim = dim)
  
  list(l.mca = l.mca, l.sup.coord= l.sup.coord)
}

map.ca.base <- function(up = NULL, down = NULL, right = NULL, left = NULL, ...){
  
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
  
  p      <- p + theme_ca_base()
  
  
  p      <- p + scale_size_continuous(range = c(0.1, 2))
  p      <- p + theme(legend.position = "bottom")
  p
}



# Triad taste example ----
data(taste)

d                    <- taste[which(taste$Isup == 'Active'), ]
a0                   <- d %>% select(TV, Film, Art, Eat)

d1                   <- d[which(d$Gender == 'Women'), ]
a1                   <- d1 %>% select(TV, Film, Art, Eat)

d2                   <- d[which(d$Gender != 'Women'), ]
a2                   <- d2 %>% select(TV, Film, Art, Eat)

d3                    <- taste
a3                   <- d3 %>% select(TV, Film, Art, Eat)




l.mca                <- list("Original" = soc.mca(a0, identifier = d$ID),
                             "No Passive" = soc.mca(a3, identifier = d3$ID),
                             "Women" = soc.mca(a1, identifier = d1$ID),
                             "Men" = soc.mca(a2, identifier = d2$ID)
                            )

l.triads             <- list()

l.triads$A           <- d %>% transmute(id = as.character(ID), 
                               "TV: Comedy" = TV == "Tv-Comedy",
                               "Eat: IndianRest" = Eat == "IndianRest",
                               "Art: StillLife" = Art == "StillLife")

l.triads$B           <- d %>% transmute(id = as.character(ID), 
                                        "Art: Impressionism" = Art == "Impressionism",
                                        "Eat: FrenchRest" = Eat == "FrenchRest",
                                        "Art: RenaissanceArt" = Art == "RenaissanceArt")


l.triads$C           <- d %>% transmute(id = as.character(ID), 
                                        "Film: Romance" = Film == "Romance",
                                        "TV: Tv-Soap" = TV == "Tv-Soap",
                                        "Art: Portrait" = Art == "Portrait")

l.triads$D           <- d %>% transmute(id = as.character(ID), 
                                        "Eat: SteakHouse"   = Eat == "SteakHouse",
                                        "Art: Landscape"    = Art == "Landscape",
                                        "Film: Documentary" = Film == "Documentary")



l.mca.triads         <- mca.triads(l.mca, l.triads, dim = c(1, 2), fix.mca = 1)



library(RColorBrewer)
mca.triad.array(l.mca.triads)


