# Script used to reduce isotope data based on the custom function 'iso.reduce'

# Import required libraries
library(tidyverse)
library(readxl)
library(here)
library(tcltk)

# Select the .csv file containing the final raw data as outputed by the Picarro
files.paths = tk_choose.files(caption = "Select .csv Picarro file to reduce")

# Select the .xlsx file containing the properly formatted and standardized tray template for this run
template.path = tk_choose.files(caption = "Select .xslx Tray Template for selected Picarro data")

# Source and run the isotope reduction functions on given input files
source(here('R-functions/iso.reduce.R'))
reduced.data = iso.reduce(files.paths, template.path)
