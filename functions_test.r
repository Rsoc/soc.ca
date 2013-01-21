#################################################
# Soc.test

setwd("~/My Dropbox/R/soc.ca")

source("soc.ca.r")

# Setting your working directory
data        <- read.csv(file="wiki_data.csv", sep=";", encoding="UTF-8")

attach(data)
active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
                          years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
                          age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
                          education, author, placeofbirth, familyclass_bourdieu,
                          partnersfamily_in_whoswho, family_in_whoswho)

sup       	<- data.frame(size_prestige, ownership_cat_2, sector, location)
id          <- navn
set.passive(c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))
result      <- soc.ca(active, sup, id)
result

#########################
# Sample test



# # Sub sample
# dim              <- 1:10
# sample.size      <- 0.01
# active
# result

sub.sample.test <- function(result, active, dim=1:10, sample.size){
sample.id   <- 1:nrow(active)
sample.id   <- sample.id[-sample(sample.id, size=nrow(active)*sample.size, replace=TRUE)]
active.sample <- active[sample.id, ]
result.sample <- soc.ca(active.sample)

test             <- matrix(ncol=2, nrow=max(dim))
colnames(test)   <- c("Ctr","Av")
ctr              <- result$ctr.mod[,dim]
ctr.sample       <- result.sample$ctr.mod[,dim]


# Ctr
ctr.diff <- ctr - ctr.sample
ctr.diff[ctr.diff < 0] <- NA
test[,1] <- round(colSums(ctr.diff, na.rm=TRUE) * 100, 1)



# Above average
av          <- as.vector(apply(as.matrix(ctr), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
av.sample   <- as.vector(apply(as.matrix(ctr.sample), 2, function(x) which(x >= mean(x, na.rm=TRUE))))
av.res <- vector(length=max(dim))
for ( i in dim){
a            <- av[[i]]
b            <- av.sample[[i]]
av.res[i]    <- round((1 - sum(a %in% b)/length(a))*100, 1)
}
test[,2]     <- av.res
test
}

######################
# Automated test
library(reshape)
#
sample.sizes <- seq(0.01, 0.5, by=0.01)
 

dim              <- 1:5
# sample.size      <- 0.01
# active
# result

ctr.test <- matrix(nrow=length(dim), ncol=length(sample.sizes))
av.test  <- matrix(nrow=length(dim), ncol=length(sample.sizes))
for( i in seq(sample.sizes)){
test.res  <-  sub.sample.test(result, active, dim=dim, sample.size=sample.sizes[i])
ctr.test[,i] <- test.res[,1]
av.test[,i]  <- test.res[,2]
}

ctr.test <- t(as.data.frame(cbind(ctr.test)))
rownames(ctr.test) <- sample.sizes
melt.ctr <- melt(ctr.test)
ggplot(data=melt.ctr, aes(x=X1, y=value, colour=as.factor(X2), group=X2)) + geom_smooth() + theme_bw()
ggplot(data=melt.ctr, aes(x=X1, y=value, colour=as.factor(X2), group=X2)) + geom_line() + theme_bw()

av.test <- t(as.data.frame(cbind(av.test)))
rownames(av.test) <- sample.sizes
melt.av <- melt(av.test)
ggplot(data=melt.av, aes(x=X1, y=value, colour=as.factor(X2), group=X2)) + geom_smooth() + theme_bw()

##########################################################################################################
### Hastighedsoptimering

sample.id   <- 1:nrow(active)
sample.id   <- sample(sample.id, size=1500, replace=TRUE)
active.sample <- active[sample.id, ]

system.time(soc.ca(active.sample))
# user  system elapsed 
# 0.436   0.000   0.435 

sample.id   <- 1:nrow(active)
sample.id   <- sample(sample.id, size=5000, replace=TRUE)
active.sample <- active[sample.id, ]

system.time(soc.ca(active.sample))
# user  system elapsed 
# 3.713   0.260   3.981 

sample.id   <- 1:nrow(active)
sample.id   <- sample(sample.id, size=10000, replace=TRUE)
active.sample <- active[sample.id, ]

system.time(soc.ca(active.sample))
# user  system elapsed 
# 13.636   1.100  14.780 

sample.id   <- 1:nrow(active)
sample.id   <- sample(sample.id, size=15000, replace=TRUE)
active.sample <- active[sample.id, ]

system.time(soc.ca(active.sample))
# user  system elapsed 
# 32.038   6.200 258.134 

sample.id   <- 1:nrow(active)
sample.id   <- sample(sample.id, size=300000, replace=TRUE)
active.sample <- active[sample.id, ]

system.time(soc.ca(active.sample))
# user  system elapsed 
# 5.029   0.376   5.411 

active <- active.sample

