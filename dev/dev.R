library(soc.ca)
example(soc.mca)

active$TV <- active$TV == "Tv-Sport" 
active$TV <- as.factor(active$TV)

active$Film <- active$Film == "Action" 
active$Film <- as.factor(active$Film)

active$Art <- active$Art == "StillLife" 
active$Art <- as.factor(active$Art)

active$Eat <- active$Eat == "Pub" 
active$Eat <- as.factor(active$Eat)


r <- soc.mca(active, passive = "FALSE")
map.ind(r)
map.ctr(r)
contribution(r)
