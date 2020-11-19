# Art!

library(soc.ca)
library(tidyverse)

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


example(soc.ca)
cord <- result$coord.ind

card <- matrix(cord[], ncol = 2)
card <- card %>% as.tibble()

p <- ggplot(card, aes(x = V1, y = V2)) + theme_void()
p + geom_point(size = 0.1, alpha = 0.1, color = "black")

l.cord <- map(2:20, ~extract_ind(result, dim = c(.x-1, .x)))
card   <- bind_rows(l.cord)
              
p <- ggplot(card, aes(x = X, y = Y)) + theme_void()
p + geom_point(size = 2, color = "black", shape = "x", alpha = 0.4) + coord_polar()


# The same person through all the dimensions

data(directors)
attach(directors)


active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
                          years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
                          age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
                          education, author, placeofbirth, familyclass_bourdieu,
                          partnersfamily_in_whoswho, family_in_whoswho)

sup       	<- data.frame(size_prestige, ownership_cat_2, sector, location)

id          <- navn

options(passive = c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))

result      <- soc.mca(active, sup, id)

result


l.cord <- map(2:20, ~extract_ind(result, dim = c(.x-1, .x)))
card   <- bind_rows(l.cord) %>% filter(Individual == "Peter Straarup")

p <- ggplot(card, aes(x = X, y = Y, group = Individual)) 
p + geom_line(size = 0.1, color = "black")  + facet_wrap(~Individual) + theme_void() + coord_polar()

# Indicator matrix 
ind <- result$indicator.matrix.active
c1  <- result$coord.ind[,1]
cind <- ind*c1
cind <- cind %>% as_tibble() %>% mutate(Ind = result$names.ind) %>% gather(key = "x", value = "value", -Ind)
cind$value[cind$value == 0] <- NA

p <- ggplot(cind, aes(x = x, y = Ind, color = value)) + geom_point(shape = "X") + theme_void() 
p <- p + scale_color_gradient(high = "darkred", low = "papayawhip", guide = "none", na.value = "white")
p + coord_polar()
# Indinet
adj <- ind %*% t(ind)
colnames(adj) <- result$names.ind
cadj <- adj %>% as_tibble() %>% mutate(Ind = result$names.ind, C1 = c1) %>% gather(key = "x", value = "value", -Ind, -C1)
cadj <- cadj %>% filter(value > 3)

p <- ggplot(cadj, aes(x = x, y = Ind, color = C1)) + geom_point(shape = "X") + theme_void() 
p <- p + scale_color_gradient(high = "papayawhip", low = "darkblue", guide = "none", na.value = "white")
p + coord_polar() + theme(plot.background = element_rect(color = "black", fill = "black"))

