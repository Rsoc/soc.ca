# The taste example

library(soc.ca)

data(taste)

data           <- taste
data           <- data[ which(data$Isup =='Active'),]


attach(data)
active         <- data.frame(TV, Film, Art, Eat, Gender)
sup            <- data.frame(Age, Income)
detach(data)


# Multiple Correspondence Analysis
result.mca     <- soc.ca(active, sup)
str(result.mca)
result.mca

variance(result.mca)

contribution(result.mca, 1)
contribution(result.mca, 2)

# Her mangler contribution variables




map.active(result.mca)
map.active(result.mca, map.title="Map of active modalities with size of contribution to 1. dimension", point.size=result.mca$ctr.mod[,1])
map.active(result.mca, map.title="Map of active modalities with size of contribution to 2. dimension", point.size=result.mca$ctr.mod[,2])
map.ind(result.mca)

# Specific Multiple Correspondence Analysis
options(passive= c("Film: CostumeDrama", "TV: Tv-Sport"))
result.smca     <- soc.ca(active, sup)
result.smca
result.smca$names.passive




class.age    <- which(dat$Age =='55-64')

result       <- soc.ca(aktive)
result

sup <- super[,c(1,3)]

soc.res      <- soc.csca(result, class.age)
soc.res      <- invert(soc.res, 2)

soc.res      <- soc.csca(result, class.age, sup)
soc.res
soc.res      <- invert(soc.res, 2)


tab.variable(soc.res)
# TV       1:    13.7    2:   60.9
# Film     1:    45.3    2:   29.2
# Art      1:    19.2    2:    3.8
# Eat out  1:    29.7    2:    6.0

quad <- create.quadrant(soc.res)

p.id(soc.res, point.shape=as.factor(quad))
p.active(soc.res)


soc.res$coord.ind[1:10,1:2]
soc.res$coord.ind.standard[1:10,1:2]


#Funktioner der ikke virker endnu
#p.ctr - Sære errors
#contribution - ingen freq
#print.soc.ca - sup


######################################
### dimension standard efter median

median.standard <- function(result){
  coord.ind     <- result$coord.ind
  coord.median  <- apply(coord.ind, 2, median)  
  dim.ind       <- seq(ncol(coord.ind))[coord.median > 0]
  result        <- invert(result, dim.ind)
  return(result)
}


########################################
### Print correlations

round(soc.res$cor.dim,2)

