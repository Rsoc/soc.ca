# Example
# This example is used in the example on the soc.ca wiki.
# See https://github.com/Rsoc/soc.ca/wiki

# Loading the package through the web. 
library(devtools)
install_github("soc.ca", "Rsoc")
library(soc.ca)

# Setting your working directory

data(directors)

attach(directors)


active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
                        years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
                        age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
                        education, author, placeofbirth, familyclass_bourdieu,
                        partnersfamily_in_whoswho, family_in_whoswho)

sup     		<- data.frame(size_prestige, ownership_cat_2, sector, location)

id          <- navn

options(passive=c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))

result      <- soc.ca(active, sup, id)

result

# Contribution
contribution(result, 1)
contribution(result, 2)
contribution(result, 3)
contribution(result, 1, all=TRUE)
contribution(result, 1, indices=TRUE)
contribution(result, 1, mode="mod")

# Individuals
contribution(result, 1, mode="ind")
contribution(result, 2, mode="ind")


# Table of variance
variance(result)

# Invert
result      <- invert(result, c(1,2,3))

# Export and assign label
export.label(result)

result      <- assign.label(result, file="https://raw.github.com/Rsoc/soc.ca/master/wiki_labels.csv")



# Add.n
result      <- add.to.label(result)
contribution(result, 2)


# The result object or "soc.ca" object
str(result)
dim1 <- result$coord.ind[,1]
qplot(dim1)

# Quadrant
quad      <- create.quadrant(result)
table(quad)
quad      <- create.quadrant(result, cut.min=0, cut.max=0)
table(quad)


# Map of individuals
map.ind(result)
map.ind(result, dim=c(2,1), point.label=TRUE)
map.ind(result, dim=c(2,1), point.size=3, point.shape=2)
map.ind(result, dim=c(2,1), map.title="The top 100 Danish CEO's", point.variable=quad, point.colour=quad)
# Map of the individuals colored by contribution
map.ind(result, point.colour=result$ctr.ind[,1], point.shape=18) + scale_color_continuous(low="white", high="red")


# Map of contributing modalities
map.ctr(result, dim=c(2,1))
map.ctr(result, dim=c(2,1), ctr.dim=2)
map.ctr(result, point.size=3)

map.active(result, dim=c(2,1))
map.sup(result, dim=c(2,1))

# Plot.list

# Selecting specific active modalities
select      <- c("Career start: Corporation (n:57)", "No Phd (n:92)")
boo.select  <- match(select, result$names.mod)
map.select(result, list.mod=boo.select)

highcor     <- which(result$cor.mod[,1] >= 0.2) 
map.select(result, list.mod=highcor)

# Selecting specific supplementary modalities

highdim3    <- which(sqrt(result$coord.sup[,3]^2)>= 0.5)
map.select(result, list.sup=highdim3)

# Selecting specific individuals based on a certain criteria

forfatter <- author=="Forfatter"
map.select(result, list.ind=forfatter)

# Combining it all
map.select(result, list.mod=highcor, list.sup=highdim3, list.ind=forfatter)

# Add points to an existing plot
ctrplot <- map.ctr(result, ctr.dim=1, colour="red")
map.add(result, ctrplot, data.type="ctr", ctr.dim=2, colour="blue")

# Using the list option in add.points
forfatter <- author=="Forfatter"
map.add(result, ctrplot, data.type="select", list.ind=forfatter, colour="purple")

# Using the list option in add.points to add labels to only a part of the cloud of individuals
forfatter     <- author=="Forfatter"
notforfatter  <- author!="Forfatter"
map.forfatter <- map.select(result, list.ind=notforfatter, point.label=FALSE)
map.forfatter
map.forfatter <- map.add(result, map.forfatter, data.type="select", list.ind=forfatter)
map.forfatter

# Plotting all the modalities of one individual
result2     <- soc.ca(active, sup, id)
individual  <- which(id=="Lars Larsen")
ind.mat     <- indicator(active)
modalities  <- names(which(ind.mat[individual,]==1))
mod.ind     <- match(modalities, result2$names.mod)

lars <- map.select(result2, list.mod=mod.ind)
map.add(result2, lars, data.type="select", list.ind=individual, colour="red")

# Adding concentration ellipses to an existing plot
el.forfatter <- map.ellipse(result, map.forfatter, author)
el.forfatter



