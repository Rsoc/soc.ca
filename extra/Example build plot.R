library(soc.ca)
library(tidyverse)
library(ggrepel)
example(soc.mca)


one.plus       <- "+ North"
one.minus      <- "- South"
two.plus       <- "+ East"
two.minus      <- "- West"

# This gives us all the active categories and their coordinates on dim 1 and 2
cats            <- extract_mod(result)
sup.cats        <- extract_sup(result)


# The base plot - with labels on the axis
p.base        <- map.ca.base(right = two.plus, left = two.minus, up = one.plus, down = one.minus)
p.base


# Adding points for the categories
p       <- p.base + add.ind(result)
p

# Adding text 
p.base + add.categories(result)

# Adding text with no overlap
p.base + add.categories(result, check_overlap = T)

# Adding text with repel
p.base + add.categories(result, repel = T)

# Making a supplementary map
p.base + add.categories(result, "sup")

# Adding supplementary categories calculated post analysis
p.base + add.categories(result, cats = supplementary.categories(result, active[, 1:2]))

# Labels colored by variable
map.ca.base() + add.categories(result, mapping = aes(color = Variable, label = label))


# Both but colored nicely
pd <- bind_rows(active = cats, sup = sup.cats, .id = "type")

p      <- p.base + geom_text_repel(data = pd, mapping = aes(x = X, y = Y, color = type, label = Modality), size = 3)
p + scale_color_manual(values = c("black", "darkred"))
