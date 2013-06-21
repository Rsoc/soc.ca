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
result.mca     <- soc.ca(active, sup)
str(result.mca)
result.mca

variance(result.mca) # See p.46 in Le Roux(2010)

contribution(result.mca, 1)
contribution(result.mca, 2)

contribution(result.mca, 1:2, mode="variable")

# Her mangler contribution variables

map.active(result.mca)
map.active(result.mca, map.title="Map of active modalities with size of contribution to 1. dimension", point.size=result.mca$ctr.mod[,1])
map.active(result.mca, map.title="Map of active modalities with size of contribution to 2. dimension", point.size=result.mca$ctr.mod[,2])
map.ind(result.mca, dim=c(1,2), point.colour=result.mca$ctr.ind[,1], point.shape=18) + scale_color_continuous(low="white", high="black")

# Specific Multiple Correspondence Analysis
options(passive= c("Film: CostumeDrama", "TV: Tv-Sport"))
result.smca     <- soc.ca(active, sup)
result.smca
result.smca$names.passive

# Class Specific Correspondence Analysis
options(passive=c("Nothing"))

class.age     <- which(data$Age =='55-64')

result.csca   <- soc.csca(result.mca, class.age, sup)
str(result.csca)
# Correlations
round(result.csca$cor.dim,2)[1:5,1:5]

variance(result.csca)
contribution(result.csca, 1)
contribution(result.csca, 2)
contribution(result.csca, 1:2, mode="variable")

map.ind(result.csca)

# Plot examples

# Plot of all dublets
map.ind(result.mca, map.title="Map of all unique individuals", point.colour=duplicated(active))

map.ind(result.mca, map.title="Map of with individuals colored by the TV variable", point.colour=active$TV)

# Ellipse 


#####################################
### csca modelsøgning

object <- result.mca

soc.csca.variable <- function(object, variable, dim=1:5){
lev.variable <- levels(variable)
result.list     <- list()
for (i in 1:length(lev.variable)){
dummy.class         <- which(variable == lev.variable[i])
result.list[[i]]    <- soc.csca(object, class.indicator=dummy.class)
}

names(result.list) <- lev.variable

cor.list <- list()
  for (i in 1:length(result.list)){
  cor.list[[i]]<- result.list[[i]]$cor.dim[dim,dim]
  }
names(cor.list) <- lev.variable

list(results=result.list, cor=cor.list)
}

nif <- soc.csca.variable(result.mca, data$Age, dim=1:5)
nif$cor

