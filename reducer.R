# Script used to reduce isotope data based on the custom function 'iso.reduce'



# # Set working directory to location of this source file (the 'reducer.R' wrapper script)
# script.dir = dirname(parent.frame(2)$ofile)
# setwd(script.dir)

# Input the directory containing the raw isotope data (subdirectory of 'Raw data')
files.paths = choose.files(caption = "Select .csv files of raw isotope data")
# files.paths = choose.files(caption = "Select .csv files of raw isotope data", 
#                            filters = Filters[".csv"])

# Input the .Rdata file that contains the tray template sample/standards locations
template.path = choose.files(caption = 'Select Excel sample tray template file')

# Source and run the isotope reduction functions on given input files
source('R-functions/iso.reduce.R')
data = iso.reduce(files.paths, template.path)
