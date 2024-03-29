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


# 2.5 Principal Axes of a Cloud ----


# Figure 2.8 Projected clouds on six lines with their variances
variance.of.angle <- function(a, x, y){
v1 <- x
v2 <- y
c  <- covariance
cos.a <- cos(a * (pi/180))
sin.a <- sin(a * (pi/180))
(v1 * cos.a^2) + (2 * c * sin.a * cos.a) + (v2 * sin.a^2)
}

angles <- c(L1 = -90, L2 = -60, L3 = -30, L4 = 0, L5 = 30, L6 = 60, L1 = 90)
var.angles  <- map_dbl(angles, variance.of.angle, x = var.x1, y = var.x2)
var.angles

# Figure 2.9 Variance of projected clouds as a function of angle
angles <- -90:90
var.angles  <- map_dbl(angles, variance.of.angle, x = var.x1, y = var.x2)
var.angles
plot(var.angles, x = -90:90, xlab = "Degree", main = "Variance of projected cloud", ylab = "Variance")

# Figure 2.10 First principal axis

label <- paste("M", "^", 1:10, "", sep = "")
xintercept <- 6 * tan(63.44 * (pi/180))

p <- ggplot(target, aes(x = x1, y = x2, label = label)) + geom_point()
p <- p + geom_hline(yintercept = 0, size = 0.3) + geom_vline(xintercept = 0, size = 0.3)
p <- p + annotate(y = 0, x = 6, geom = "point", shape = 1)
p <- p + geom_text(parse = TRUE, nudge_y = -0.5, nudge_x = 0.5)
p <- p + geom_abline(intercept = -xintercept, slope = tan(63.44 * (pi/180)))
p <- p + theme_void() + xlab("") + ylab("") 
p

# First principal axis
variance.of.angle(a = 63.44, x = var.x1, y = var.x2)


# Rotation of the cloud # Not in the book
target.p1 <- rotate_coord(x1, x2, angle = 63.44, type = "degrees", center = c(6,0))
colnames(target.p1) <- c("x1", "x2")

p <- ggplot(target.p1 %>% as_tibble, aes(x = x1, y = x2, label = label)) + geom_point()
p <- p + geom_hline(yintercept = 0, size = 0.3) + geom_vline(xintercept = 0, size = 0.3)
p <- p + geom_text(parse = TRUE, nudge_y = -0.5, nudge_x = 0.5)
p <- p + theme_void() + xlab("") + ylab("") 
p


