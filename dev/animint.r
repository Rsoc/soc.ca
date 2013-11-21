install.packages("devtools")
library(devtools)
install_github("animint", "tdhock")
library(animint)

library(ggplot2)
library(animint)
library(plyr)
set.seed(33)

boxplotdata <- rbind(data.frame(x = 1:50, y = sort(rnorm(50, 3, 1)), group = "N(3,1)"), 
                     data.frame(x = 1:50, y = sort(rnorm(50, 0, 1)), group = "N(0,1)"), data.frame(x = 1:50, 
                                                                                                   y = sort(rgamma(50, 2, 1/3)), group = "Gamma(2,1/3)"))
boxplotdata <- ddply(boxplotdata, .(group), transform, ymax = max(y), ymin = min(y), 
                     med = median(y))

g1 <- ggplot() + geom_density(data = boxplotdata, aes(x = y, group = group, 
                                                      fill = group), alpha = 0.5) + scale_fill_discrete("Distribution") + xlab("x") + 
  ggtitle("geom_density")

g2 <- ggplot() + geom_density(data = boxplotdata, aes(x = y, group = group, 
                                                      fill = group), alpha = 0.5) + scale_fill_discrete("Distribution") + xlab("x") + 
  ggtitle("nif nif")
g2

gg2animint(list(plot1 = g1, plot2=g2), out.dir = "./g1")

############
library(ggplot2)
library(plyr)
library(animint)
library(maps)
data(UStornadoes) # load the Tornadoes data from the animint package

USpolygons <- map_data("state")
USpolygons$state = state.abb[match(USpolygons$region, tolower(state.name))]

map <- ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group), data=USpolygons, fill="black", colour="grey") +
  geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat, showSelected=year), 
               colour="#55B1F7", data=UStornadoes) +
  ggtitle("Tornadoes in the US")

ts <- ggplot() + 
  stat_summary(aes(year, year, clickSelects=year), data=UStornadoes, fun.y=length, geom="bar") + 
  ggtitle("Number of Recorded Tornadoes, 1950-2006") + 
  ylab("Number of Tornadoes") + 
  xlab("Year")

tornado.bar <- list(map = map, ts = ts, width=list(map = 970, ts = 400),  height=list(400)) 
# specify plot widths to be 970px and 400px respectively, 
# and specify 400 px as the plot height for both plots

gg2animint(tornado.bar, out.dir = "tornado-bar")




