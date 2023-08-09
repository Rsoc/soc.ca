# Install from the development version from github
# remotes::install_github("Rsoc/soc.ca")


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


# Adding points for the cases
p       <- p.base + add.ind(result)
p

# Adding a density function
p + add.density(result)

# Concentration ellipses
ind <- extract_ind(result)
ind$var <- active$TV == "Tv-Drama"

p   <- p.base + add.ind(ind = ind, mapping = aes(color = var), size = 0.5) + add.ellipse(result, var = var) + scale_color_manual(values = c("black", "red"))

# Adding text 
p.base + add.categories(result)

# Adding text with no overlap
p.base + add.categories(result, check_overlap = T)

# Adding text with repel
p.base + add.categories(result, repel = T)

# Filtering by contribution and coloring by variable
cats <- extract_mod(result)
cats <- cats %>% filter(ctr.set) 
p.base + add.categories(cats = cats, mapping = aes(color = Variable, label = Modality), repel = T)

# Facetting by variable 
cats <- extract_mod(result)
p.base + add.categories(cats = cats, mapping = aes(label = label), repel = T, size = 3) + facet_wrap(~Variable)

# Making a supplementary map
p.base + add.categories(result, "sup")

# Adding supplementary categories calculated post analysis
sup.cats <-  supplementary.categories(result, active[, 1:2])
p.base + add.categories(cats = sup.cats)

# Filtering supplementary categories
p.base + add.categories(cats = sup.cats %>% filter(Frequency < 150))

# Size by Frequency
map.ca.base() + add.categories(result, mapping = aes(size = Frequency, label = label)) + scale_size(range = c(1,5))

# Both active and supplementary but colored nicely
pd <- bind_rows(active = cats, sup = sup.cats, .id = "type")

p      <- p.base + geom_text_repel(data = pd, mapping = aes(x = X, y = Y, color = type, label = Modality), size = 3)
p      <- p + scale_color_manual(values = c("black", "darkred"))
p

# Adding quadrant labels
p       <- p.base + add.ind(result)
p + add.quadrant.labels(quadrant.labels = c("North-West", "North-East", "South-East", "South-West"))


