library(tidyverse)
library(soc.ca)

# Chapter 2: The Geometry of a cloud of points -----

# Cloud of points: Target example ----

# Table 2.1 p. 16-17

target <- tibble("x1" = c(0, 6, 14, 6, 12, -8, 2, 6, 10, 12),
                 "x2" = c(-12, -10, -6, -2, 0, 2, 4, 4, 10, 10))

target

plot(target)

x1 <- target$x1
x2 <- target$x2

# Mean
mean.x1 <- x1 %>% sum() / nrow(target)
mean.x2 <- x2 %>% sum() / nrow(target)
mean.x1
mean.x2

# Variance
var    <- (mean.x1 - x1)^2
var
var.x1 <- sum(var) / length(x1)
var.x1

var.x2 <- (mean.x2 - target$x2)^2 %>% sum(.)/length(x2)
var.x2

x1-x2

# Covariance
covariance  <- sum((x1 - mean.x1) * (x2 - mean.x2)/10)
correlation <- covariance/sqrt(var.x1 * var.x2)

# Mean point of a cloud
G <- c(mean(x1), mean(x2))

# Distances between points -----

# Distance between M1 and M2
m1 <- target[1,]
m2 <- target[2,]
squared.distance <- (m2-m1)^2 %>% sum()
distance <- sqrt(squared.distance)

# Variance of the cloud
squared.distances        <- tibble(x1 = (x1-mean.x1)^2, x2 = (x2-mean.x2)^2)
sum.of.squared.distances <- squared.distances %>% sum()
cloud.variance           <- sum.of.squared.distances/nrow(target)
standard.deviation       <- sqrt(cloud.variance)

# Huygens property
P  <- m1
PG <- (P-G)^2
v1 <- sum((x1-m1$x1)^2)/10 - PG$x1
v2 <- sum((x2-m1$x2)^2)/10 - PG$x2
(v1+v2) - PG$x1 # Which is equal to the cloud variance


# 2.3 Subclouds and Partition of a Cloud ----

A <- target[1:2,]
B <- target[6,]
C <- target[c(3:5,7:10),]

# Table 2.2 Coordinates of mean points A, B and C.
coord.A <- colMeans(A)
coord.B <- colMeans(B)
coord.C <- colMeans(C)


weights <- c(nrow(A), nrow(B), nrow(C))
coord   <- bind_rows(coord.A, coord.B, coord.C)
tibble(coord, weights)

# Variance of subclouds
cloud.variance <- function(x){
  G <- colMeans(x)
  squared.distances        <- t(t(x)-G)^2
  sum.of.squared.distances <- squared.distances %>% sum()
  cloud.variance           <- sum.of.squared.distances/nrow(x)
  cloud.variance
}

cloud.variance(A)
cloud.variance(B)
cloud.variance(C)

# Partition and between-cloud
# The mean of the between cloud - that is the cloud of mean points for A, B and C is equal to the mean point of the whole cloud (G)
((coord)*weights) %>% colSums(.)/nrow(target)
G

# The between variance
# NB jeg kan ikke få det til at virke - måske er der en fejl i bogen? 
# Jeg kan få A og C til at være rigtige men ikke B. Der står at squared variance for B - som kun har et punkt skulle være 200, men jeg får det til 80 - som er mere realistisk.
(colMeans(A) - G)^2
vb <- ((coord-G)^2) * weights
rowSums(vb)/nrow(target)

