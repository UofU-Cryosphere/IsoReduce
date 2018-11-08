# Script used to reduce isotope data based on the custom function 'iso.reduce'



# Set working directory to location of this source file (the 'reducer.R' wrapper script)
script.dir = dirname(parent.frame(2)$ofile)
setwd(script.dir)

# Input the directory containing the raw isotope data (subdirectory of 'Raw data')
files.path = choose.dir(caption = "Select folder containing raw isotope data")

# Input the .Rdata file that contains the tray template sample/standards locations
template.path = choose.files(caption = 'Select sample tray template file')

# Source and run the isotope reduction functions on given input files
source('iso.reduce.R')
data = iso.reduce(files.path, template.path)
