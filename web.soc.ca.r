# Load the package with the help of the internet

library(ggplot2)
library(ellipse)
library(scales)
library(grid)
options(width=250)

#These are the functions that are related to the analysis: soc.ca, burt, subset.ca ...
source("https://raw.github.com/Rsoc/soc.ca/master/functions_analysis.r")
# These are the functions related to description: print.soc.ca, contribution ...
source("https://raw.github.com/Rsoc/soc.ca/master/functions_description.r")
# These are the functions related to label management: add.n, get.label, assign.label ...
source("https://raw.github.com/Rsoc/soc.ca/master/functions_label.r")
# These are the functions related to plotting: p.all, p.ctr, p.active ...
source("https://raw.github.com/Rsoc/soc.ca/master/functions_plot.r")
# These are the functions related to ellipses and the plotting of ellipses
source("https://raw.github.com/Rsoc/soc.ca/master/functions_ellipse.r")
# These are helpful functions that allow you to manipulate or export soc.ca objects: invert, export ...
source("https://raw.github.com/Rsoc/soc.ca/master/functions_tools.r")
# These functions define the plot themes
source("https://raw.github.com/Rsoc/soc.ca/master/plot_theme.r")