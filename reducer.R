# Script used to reduce isotope data based on the custom function 'iso.reduce'

# These are variables to update 
  # folder.name is the directory name containing the raw isotope data (within the 'Raw data' folder)
  # template.name is the name of the .Rdata template file within the 'Tray templates' folder
  # The third line sets the working directory, corresponding to the location of this script file
folder.name = 'house_calib_2017Aug08'
template.name = 'house.calibrate'
# setwd('/Volumes/WARP DRIVE/Research/Lab Management/Picarro/IsotopeData/')




# Sources and runs the isotope reduction functions based on input variables above
source('iso.reduce.R')
data = iso.reduce(folder.name, template.name)
