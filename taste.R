# The taste example
library(soc.ca)

data(taste)

data           <- taste
data           <- data[which(data$Isup =='Active'),]

attach(data)
active         <- data.frame(TV, Film, Art, Eat)
sup            <- data.frame(Gender, Age, Income)
detach(data)


# Multiple Correspondence Analysis
result.mca     <- soc.mca(active, sup)
str(result.mca)
result.mca

variance(result.mca) # See p.46 in Le Roux(2010)

contribution(result.mca, 1)
contribution(result.mca, 2)
contribution(result.mca, 1:3, mode="variable")


map.active(result.mca)
map.active(result.mca, map.title="Map of active modalities with size of contribution to 1. dimension", point.size=result.mca$ctr.mod[,1])
map.active(result.mca, map.title="Map of active modalities with size of contribution to 2. dimension", point.size=result.mca$ctr.mod[,2])
map.ind(result.mca)
map.ind(result.mca, dim=c(1,2), point.colour=result.mca$ctr.ind[,1], point.shape=18) + scale_color_continuous(low="white", high="black")

# Specific Multiple Correspondence Analysis
options(passive= c("Film: CostumeDrama", "TV: Tv-Sport"))
result.smca     <- soc.mca(active, sup)
result.smca
result.smca$names.passive

# Class Specific Correspondence Analysis
options(passive=NULL)

class.age     <- which(data$Age =='55-64')

result.csca   <- soc.csa(result.mca, class.age, sup)
str(result.csca)
# Correlations
round(result.csca$cor.dim,2)[1:5,1:5]

variance(result.csca)
contribution(result.csca, 1)
contribution(result.csca, 2)
contribution(result.csca, 1:3, mode="variable")

map.ind(result.csca)

# Plot examples

# Plot of all dublets
map.ind(result.mca, map.title="Map of all unique individuals", point.colour=duplicated(active))
map.ind(result.mca, map.title="Map with individuals colored by the TV variable", point.colour=active$TV)

# Ellipse 
map        <- map.ind(result.mca)
map.ellipse(result.mca, map, as.factor(data$Age =='55-64'))


