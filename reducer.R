# Script used to reduce isotope data based on the custom function 'iso.reduce'

# Import required libraries
library(tidyverse)
library(readxl)
library(here)
library(plotly)

# Select the .csv file containing the final raw data as outputed by the Picarro
files.paths = file.choose()


# Select the .xlsx file containing the properly formatted and standardized tray template for this run
template.path = file.choose()

# Source and run the isotope reduction functions on given input files
source('R-functions/iso.reduce.R')
reduced.data = iso.reduce(files.paths, template.path)

# Plot d18O values of reduced data
d18O.plt = reduced.data %>% ggplot(., aes(x=1:nrow(.), y=d18O.correct, label = Sample_ID)) + geom_point()
ggplotly(d18O.plt)

# Plot dD values of reduced data
reduced.data %>% ggplot(., aes(x=1:nrow(.), y=dD.correct)) + geom_point()
