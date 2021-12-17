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
