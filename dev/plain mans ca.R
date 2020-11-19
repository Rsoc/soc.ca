library(soc.ca)
library(tidyverse)
example(soc.mca)
example(directors, run.dontrun = TRUE)
data("pe13")

# How many share X variables?

ind <- result$indicator.matrix.active

rownames(ind) <- result$names.ind

adj.ind <- ind %*% t(ind)
adj.cat <- t(ind) %*% ind
rs <- rowSums(adj.ind) 

rs %>% enframe() %>% View()

summary(rs)

# Weirdest profile
ind[which.min(rs)[1],] %>% stack() %>% filter(values == 1)

# Most normal profile
ind[which.max(rs)[1],]  %>% stack() %>% filter(values == 1)

map.ind(result, point.fill = rs, legend = "bottom", point.size = rs) + scale_fill_continuous(high = "darkred", low = "white")

map.ind(result, dim = c(1,3), point.fill = rs, legend = "bottom", point.size = rs) + scale_fill_continuous(high = "darkred", low = "white")

cor(cbind(rs, result$coord.ind[, 1:3])) %>% round(2)



# adj.cat

dia <- diag(adj.cat)
diag(adj.cat) <- 0

rs <- rowSums(adj.cat)
adj.cat

# Max weight er antallet af variable x antallet af individer
# 