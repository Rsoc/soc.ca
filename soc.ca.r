## This loads the entire package.
library(ggplot2)
library(ellipse)
library(scales)
options(width=250)

# These are the functions that are related to the analysis: soc.ca, burt, subset.ca ...
source("functions_analysis.r")
# These are the functions related to description: print.soc.ca, contribution ...
source("functions_description.r")
# These are the functions related to label management: add.n, get.label, assign.label ...
source("functions_label.r")
# These are the functions related to plotting: p.all, p.ctr, p.active ...
source("functions_plot.r")
# These are the functions related to ellipses and the plotting of ellipses
source("functions_ellipse.r")
# These are helpful functions that allow you to manipulate or export soc.ca objects: invert, export ...
source("functions_tools.r")
# These functions define the plot themes
source("plot_theme.r")

# Various unfinished functions are available in functions_var
#source("functions_var.r")

# An example is available in example.r
# test is a test suite for the entire package




#library(devtools)
#install_github("antongrau/soc.ca", username="antongrau")