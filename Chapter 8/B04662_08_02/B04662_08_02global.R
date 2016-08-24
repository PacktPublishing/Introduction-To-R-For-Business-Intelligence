# campaign-creator-app/global.R

# default strings to characters to 
# avoid potential factor-related issues
options(stringsAsFactors = FALSE)
if (!require(devtools)) install.packages("devtools")
suppressWarnings(suppressPackageStartupMessages(library(devtools)))

# load all packages used by the shiny app here in a central location
# suppressing warnings and messages are simply for convenience

# shiny package contains core functions for the app
if(!require("shiny")) install.packages("shiny")
suppressWarnings(suppressPackageStartupMessages(library(shiny)))

# shinysky package contains an easy to implement progress wheel
# so that users can be alerted when the app is running a calculation
# and need to wait for it to finish before proceeding
if (!require(devtools)) install.packages("devtools")
devtools::install_github("AnalytixWare/ShinySky")
suppressWarnings(suppressPackageStartupMessages(library(shinysky)))

# DT package provides an interface to the jQuery Plug-in DataTables,
# which is a feature-rich, interactive framework for displaying tables
# inside web applications
install_github(repo = 'rstudio/DT')
suppressWarnings(suppressPackageStartupMessages(library(DT)))

# this code requires a version of DT that is not yet available via CRAN as of 5/9/16
# so check that the installed version is new enough
stopifnot(packageVersion("DT")[1,2] >= 1, 
          packageVersion("DT")[1,3] >= 38)

# dplyr package contains easy to use functions for manipulating
# data, which is a common task inside Shiny apps so that elements
# render properly
if(!require("dplyr")) install.packages("dplyr")
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))

# ggplot2 package for creating graphics
if(!require("ggplot2")) install.packages("ggplot2")
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))

# scales package for formatting the ggplot graphics
if(!require("scales")) install.packages("scales")
suppressWarnings(suppressPackageStartupMessages(library(scales)))

# dendextend package for creating a dendrogram plot
# that has more features than base plotting
if(!require("denextend")) install.packages("dendextend")
suppressWarnings(suppressPackageStartupMessages(library(dendextend)))

# RColorBrewer for creating a length 10 color scale
# to overwrite the default coloring of the dendextend package
if(!require("RColorBrewer")) install.packages("RColorBrewer")
suppressWarnings(suppressPackageStartupMessages(library(RColorBrewer)))

dendrogram_color_scheme <- c(head(brewer.pal(8, 'Set1'), 2), brewer.pal(8, "Dark2"))