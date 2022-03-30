headings.ft  <- function(result, dim = 1:3, caption = "Headings"){
  h          <- headings(result, dim = dim)
  h          <- tibble(Heading = rownames(h), data.frame(h, check.names = F))
  h          <- h %>% rename("Active Categories" = `Active Modalities`)
  colnames(h)[4: (5 + length(dim)-1)] <- c("Total", paste(dim))
  
  ft         <- flextable(h) %>% add_header_row(values = c("", "Contributions"), colwidths = c(3,  ncol(h) - 3)) %>% autofit() %>% theme_booktabs()
  ft         <- ft %>% align(part = "header", align = "center", i = 1) %>% bold(j = "Total") 
  ft         <- ft %>% set_caption(caption)
  ft
}

variance.ft   <- function(result, dim = 1:5, caption = "Explained variance"){
  var           <- result$adj.inertia[dim, -1] %>% t()
  colnames(var) <- paste(dim)
  var           <- tibble(Dimension = rownames(var), data.frame(var, check.names = F))
  ft            <- flextable(var) 
  ft            <- ft %>% colformat_num(i = 2:4, j = -1, digits = 1) %>% set_caption(caption)
  ft
}

contribution.cat.ft <- function(result, dim = 1, caption = paste("Categories contributing above average to dim.", dim)){
  t   <-   extract_mod(result, c(dim, 2)) %>% as_tibble() %>% filter(ctr.x >= mean(ctr.x)) %>% arrange(-ctr.x)
  t   <- t %>% transmute(Category = Modality, "Coord +" = round(X, 2), "Coord -" = round(X, 2), "Ctr %" = round(ctr.x * 100, 1), n = Frequency)
  t$`Coord +`[t$`Coord +` >= 0] <- NA
  t$`Coord -`[t$`Coord -` <= 0] <- NA
  ft <- flextable(t) %>% autofit() %>% set_caption(caption)
  ft
}

grand.table.with.headings <- function(result, dim = 1:3, headings = TRUE){
  
  cat           <- extract_mod(result, dim = 1:2)
  cat           <- tibble(heading = result$headings, cat)
  cat$label     <- cat$Modality %>% str_split(string = ., fixed(paste0(cat$Variable, ":")), n = 2) %>% map_chr(~tail(.x, 1) %>% trimws())
  ctr           <- (result$ctr.mod[, dim] * 100) %>% data.frame() %>% round(2)
  colnames(ctr) <- as.character(dim)
  cat           <- cat %>% select(Heading = heading, Variable, Category = label, Frequency) %>% bind_cols(., ctr)
  cl            <- cat %>% split.data.frame(f = cat$Variable)
  
  x <- cl[[1]]
  
  xf <- function(x){
    var.sum        <- x[, 5:ncol(x)] %>% colSums() %>% as.list()
    
    var.sum        <- c("Heading" = x$Heading[1], "Variable" = x$Variable[1], "Category" = NA, "Frequency" = NA, var.sum)
    y              <- bind_rows(var.sum, x)
  }
  
  cl            <- map(cl, xf) %>% bind_rows()
  cl            <- cl %>% split.data.frame(f = cl$Heading)
  
  x <- cl[[1]]
  yf <- function(x){
    var.sum        <- x[is.na(x$Category) , 5:ncol(x)] %>% colSums() %>% as.list()
    
    var.sum        <- c("Heading" = x$Heading[1], "Variable" = NA, "Category" = NA, "Frequency" = NA, var.sum)
    y              <- bind_rows(var.sum, x)
  }
  o               <- map(cl, yf) %>% bind_rows()
  
  hl              <- as.list(colnames(o)) 
  names(hl)       <- colnames(o)
  hl[5:ncol(o)]   <- "Contribution"
  
  ft               <- flextable(o) %>% merge_v() %>% valign(valign = "top")
  ft               <- ft %>% hline(i = is.na(o$Category) & !is.na(o$Variable), part = "body", ) %>% autofit() %>% fix_border_issues()
  ft               <- ft %>% bold(i = is.na(o$Variable)) %>% italic(i = !is.na(o$Variable) & is.na(o$Category))
  ft               <- ft %>% set_header_labels(values = hl) %>% merge_h(part = "header") %>% align(i = 1, j = 5:ncol(o), part = "header", align = "center")
  ft               <- ft %>% add_header_row(values = c(rep("", 4), dim), top = F)
  ft               <- ft %>% hline(i = 2, j = 5:ncol(x), part = "header") %>% align(i = 2, j = 5:ncol(x), part = "header", align = "center")
  ft
}



