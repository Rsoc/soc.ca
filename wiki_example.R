# Example
# This example is used in the example on the soc.ca wiki.
# See https://github.com/Rsoc/soc.ca/wiki

# Loading the package through the web. 
source("https://raw.github.com/Rsoc/soc.ca/master/web.soc.ca.r")

# Setting your working directory

data        <- read.csv(file="https://raw.github.com/Rsoc/soc.ca/master/wiki_data.csv", sep=";", encoding="UTF-8")

attach(data)


active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
                        years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
                        age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
                        education, author, placeofbirth, familyclass_bourdieu,
                        partnersfamily_in_whoswho, family_in_whoswho)

sup     		<- data.frame(size_prestige, ownership_cat_2, sector, location)

id          <- navn

set.passive(c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))

result      <- soc.ca(active, sup, id)

result

# Contribution
contribution(result, 1)
contribution(result, 1, all=TRUE)
contribution(result, 1, modality.indices=TRUE)

# Tab.dim
tab.dim(result, 1)
tab.dim(result, 1, all=TRUE)

# Individuals
individuals(result, 1)

# Contribution of the variables
ctr.var(result)
ctr.var(result, 2:4)

# Table of variance
tab.variance(result)

# Invert
result      <- invert(result, c(1,2,3))

# Export and assign label
export.label(result)

result      <- assign.label(result, file="https://raw.github.com/Rsoc/soc.ca/master/wiki_labels.csv")


# Add.n
result      <- add.n(result)

contribution(result, 2)


# The result object or "soc.ca" object

str(result)
dim1 <- result$coord.ind[,1]
qplot(dim1)

###### Plotting functions
p.id(result)
p.id(result, dim=c(2,1), point.label=TRUE)
p.id(result, dim=c(2,1), point.size=3, point.shape=2)
p.id(result, dim=c(2,1), map.title="The top 100 Danish CEO's")

p.ctr(result, dim=c(2,1))
p.ctr(result, dim=c(2,1), ctr.dim=2)

p.active(result, dim=c(2,1))
p.sup(result, dim=c(2,1))

# Plot.list

# Selecting specific active modalities
select      <- c("Career start: Corporation (n:57)", "No Phd (n:92)")
boo.select  <- match(select, result$names.mod)
p.list(result, list.mod=boo.select)

highcor     <- which(result$cor.mod[,1] >= 0.2) 
p.list(result, list.mod=highcor)

# Selecting specific supplementary modalities

highdim3    <- which(sqrt(result$coord.sup[,3]^2)>= 0.5)
p.list(result, list.sup=highdim3)

# Selecting specific individuals based on a certain criteria

forfatter <- author=="Forfatter"
p.list(result, list.ind=forfatter)

# Combining it all
p.list(result, list.mod=highcor, list.sup=highdim3, list.ind=forfatter)

# Add points to an existing plot
ctrplot <- p.ctr(result, ctr.dim=1, colour="red")
add.points(result, ctrplot, data.type="ctr", ctr.dim=2, colour="blue")

# Using the list option in add.points
forfatter <- author=="Forfatter"
add.points(result, ctrplot, data.type="list", list.ind=forfatter, colour="purple")

# Using the list option in add.points to add labels to only a part of the cloud of individuals
forfatter     <- author=="Forfatter"
notforfatter  <- author!="Forfatter"
map.forfatter <- p.list(result, list.ind=notforfatter, point.label=FALSE)
map.forfatter
map.forfatter <- add.points(result, map.forfatter, data.type="list", list.ind=forfatter)
map.forfatter


# Plotting all the modalities of one individual
result2     <- soc.ca(active, sup, id)
individual  <- which(id=="Lars Larsen")
ind.mat     <- indicator(active)
modalities  <- names(which(ind.mat[individual,]==1))
mod.ind     <- match(modalities, result2$names.mod)

lars <- p.list(result2, list.mod=mod.ind)
add.points(result2, lars, data.type="list", list.ind=individual, colour="red")

# Adding concentration ellipses to an existing plot
el.forfatter <- p.ellipse(result, map.forfatter, author)
el.forfatter

