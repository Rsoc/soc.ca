library(soc.ca)
library(tidyverse)
library(ggrepel)
example(soc.mca)


one.plus       <- "+ North"
one.minus      <- "- South"
two.plus       <- "+ East"
two.minus      <- "- West"

# This gives us all the active categories and their coordinates on dim 1 and 2
cats            <- extract_mod(result) %>% as_tibble() 
sup.cats        <- extract_sup(result) %>% as_tibble() 


# The base plot - with labels on the axis
p.base        <- map.ca.base(right = two.plus, left = two.minus, up = one.plus, down = one.minus)
p.base


# Adding points for the categories
p       <- p.base + geom_point(data = cats, mapping = aes(x = X, y = Y, shape = Variable, color = Variable, size = Frequency)) 
p

# Adding text with repel
p      <- p + geom_text_repel(data = cats, mapping = aes(x = X, y = Y, color = Variable, label = Modality), size = 3)
p

# Making a supplementary map
p      <- p.base + geom_text_repel(data = sup.cats, mapping = aes(x = X, y = Y, color = Variable, label = Modality), size = 3)
p


# Both but colored nicely
pd <- bind_rows(active = cats, sup = sup.cats, .id = "type")

p      <- p.base + geom_text_repel(data = pd, mapping = aes(x = X, y = Y, color = type, label = Modality), size = 3)
p + scale_color_manual(values = c("black", "darkred"))

