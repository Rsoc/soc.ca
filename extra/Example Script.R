# Installing the soc.ca package
# You only need to do this the first time you want to use the package - but remember to update it!
# R may want you to choose a repository, just select one geographically close to you or use the Rstudio repository.
install.packages("soc.ca")

# Load the package
# You have to load the package for each R session
library(soc.ca)

# Load the data ----
# Normally you would import your data from a data file, preferably a comma seperated file (.csv). You can use the read.csv function.
# In Rstudio you can import a data set with the "Import Dataset" button under the "Environment" Pane. When you are done clicking, you can save the line of code Rstudio produces for you.
data(directors)
View(directors)


# Attach the dataset 
# By attaching the dataset we are allowed to simply name the variables in the dataset in our commands.
# Normally you would have call them in their full "path" with a dollar sign. Like "directors$phd"
# But attaching is a bit tricky... so when you become more comfortable with R, then you should stop doing it.
attach(directors)

# Defining the active variables
# You create a data frame with all the active variables, seperated with commas. And then you assign it into an object for later use, with the <- assign arrow.
# The name of the new dataframe is not important. And you can have as many different sets of active variables you want.
active      <- data.frame(careerprofile_maclean_cat, careerfoundation_maclean_cat,
                          years_between_edu_dir_cat, time_in_corp_before_ceo_cat,
                          age_as_ceo_cat, career_changes_cat2, mba, abroad, hd, phd,
                          education, author, placeofbirth, familyclass_bourdieu,
                          partnersfamily_in_whoswho, family_in_whoswho)

# Defining the supplementary variables
# The same procedure. If you want another name for your variable you can assign your variable into a variable with a new name inside your new dataframe.
# It is optional if you want supplementary variables
sup         <- data.frame(prestige = size_prestige, ownership = ownership_cat_2, sector, location)

# Defining the identifier variable
# This is typically the name or id number of the individuals in the dataset. If it is not defined each individual is assigned a row number as identifier
id          <- navn

# Defining the passive categories/modalities
# The passive option sets what categories to set as passive. The soc.ca functions search for category labels that include a full or partial match to any of the values given to options.
# All subsequent analysis in this R session will look for these labels and set them as passive.
# The default is "Missing" 
options(passive = c("MISSING", "Missing", "Irrelevant", "residence_value_cat2: Udlandet"))

# Running the analysis -----
# If you do not want an identifier or supplementary variables, leave them out.
result      <- soc.mca(active, sup, id)

# Viewing the results
result

# The structure of the result object
str(result)

# Getting help
?soc.mca

# Getting the mass
# You can access the different parts in the result object with the $ 
result$mass.mod

# Variance
variance(result)

# Contribution values -----
# The contribution values can be sorted in different ways by changing the mode option. See ?contribution for the different values.
# Here we look at the the first three dimensions
contribution(result, 1)
contribution(result, 2)
contribution(result, 3)

contribution(result, dim = 1:3, mode = "variable")

# Contributing individuals ----
contribution(result, mode = "ind")

# Maps and Plots ------
# All plots or maps use the plotting package ggplot2.
# ggplot2 is a graphical language that is very widely used in the R community. While it is somewhat complicated to learn it is well worth the effort.
# All plots created by soc.ca functions can be altered, themed etc. with ggplot2 functions.

# Cloud of individuals
# Here we plot the cloud of individuals.
# We do that to inspect the shape of the cloud and if we care about the names of the individuals we can give them labels.
map.ind(result)
map.ind(result, label = TRUE)

# We choose different dimensions with the dim argument and we decide the order with c()
map.ind(result, dim = c(1,3))

# We can change the plotting order of the dimensions if we want to. If you have a volume of capital it might help interpretation to have it run vertically instead of horisontically.
map.ind(result, dim = c(3,1))

# Here we use a variable for the fill color of the points. In this case whether the directors have a phd or not.
# You can use any variable from your dataset - continouos, ordinal and nominal. 
map.ind(result, point.fill = phd)

# Here we use the contribution to the first dimension as the fill color.
# We adjust the scale with a ggplot2 command by "adding" it.
map.ind(result, point.fill = result$ctr.ind[,1]) + scale_fill_continuous(high = "darkblue", low = "papayawhip")

# A concentration ellipse ----
# The map.ellipse function takes another plot object and puts concentration ellipses on top of it.
# The concentration ellipse contains at least 80% of the individuals of a category.
p      <- map.ind(result, point.fill = phd)
map.ellipse(result, p, phd)

# Ellipse array
# If you have a lot of categories it might be better to put them in an array
map.ellipse(result, variable = education)
map.ellipse.array(result, variable = education, ncol = 3)

# Cloud of contributing modalites ----
# This map is one of the most commonly reported plots. But sometimes labels may get too crowded.
# If you want to see more the most contributing modalities across several dimensions, use ctr.dim = 1:2
map.ctr(result, ctr.dim = 1)
map.ctr(result, ctr.dim = 2)
map.ctr(result, dim = c(1,3), ctr.dim = c(1,3))

# Cloud of active modalities
map.active(result, point.color = result$variable, point.fill = result$variable, label = FALSE)

result     <- add.to.label(result, value = "linebreak" )
map.active(result, point.color = result$variable, point.fill = result$variable)

# Exporting some results ----
# The export function gives us a csv file with coordinates and contributions.
export(result)

# Export a plot ----
# You can slowly build a plot and adjust it with ggplot2 functions. There are a ton of them and you can find plenty of guides for them elsewhere.
# When you are satisfied with the plot you export it. There are three ways to export.
# 1. By clicking save on the plot viewer.
# 2. With ggsave - which saves a single file
# 3. With devices. Here you open a "connection"
p   <- map.ind(result, point.fill = active$mba, point.shape = active$mba, legend = "bottom")
p   <- p + scale_fill_manual(values = c("grey50", "white", "black"), name = "MBA") + scale_shape_manual(values = c(21, 22, 23), guide = "none")
p   <- p + ggtitle(label = "Danish directors with MBA's", subtitle = "In the space of careers")
p

ggsave(p, filename = "plot.png", width = 10, height = 10)

pdf(file = "plot.pdf", height = 10, width = 10)
p
map.ctr(result) + coord_fixed()
dev.off()

# Class Specific Multiple Correspondence Analysis -----
# You give the soc.csa a variable with the rownumbers for the class you want to analyse.
royal       <- which(royal_invitation == "Royal invitation")
csa.royal   <- soc.csa(result, royal)
csa.measures(csa.royal)

contribution(csa.royal, 1)
contribution(csa.royal, 2)

map.ind(csa.royal)
map.ctr(csa.royal)

# We can also make several CSA's at once 
csa.all(result, partnersfamily_in_whoswho, dim = 1:3)
map.csa.all(result, partnersfamily_in_whoswho)

