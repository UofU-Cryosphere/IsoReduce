# Function for reducing isotope data within a given folder, based on a given tray template
iso.reduce = function(folder.name, template.name) {
  
  # Source data reduction functions
  source('iso.combine.R')
  source('iso.loop.R')
  source('drift.correct.R')
  source('bias.correct.R')
  
  # Names/path of files to be analyzed
  path.full = paste('Raw data', folder.name, sep = '/')
  files = list.files(path.full, pattern = '.csv', full.names = TRUE)
  
  # Concatenates an arbitrary number of Picarro .csv files into a single R dataframe
  data.all = iso.combine(files)
  
  # Loop through sample injections to correct the data for sample memory effects
  data.reduce = iso.loop(data.all)
  
  # Load template-specific data (location of standards/QC, standards values, etc.)
  template.path = paste('Tray templates/', template.name, '.Rdata', sep = '')
  load(template.path)
  
  # Drift correct isotope data
  drift.correction = drift.correct(data.reduce, QC.loc)
  d18O.drift.correct = unlist(drift.correction[1])
  dD.drift.correct = unlist(drift.correction[2])
  
  # Correct linear offset in isotope data using values of known standards
  bias.correction = bias.correct(d18O.drift.correct, dD.drift.correct, STND.loc, STND.val)
  
  # Add drift- and offset- corrected data to 'data.reduce', and reorder for easier viewing
  data.reduce$d18O.correct = unlist(bias.correction[1])
  data.reduce$dD.correct = unlist(bias.correction[2])
  data.reduce = data.reduce[c('Sample.port', 'd18O.correct', 'd18O.sigma', 'd18O.method', 
                              'd18O.predict', 'dD.correct', 'dD.sigma', 'dD.method','dD.predict')]

  # # Diagnostics to check drif corrections
  # plot(dD.drift.correct[unlist(STND.loc$P.mid)])
  # plot(dD.drift.correct[unlist(STND.loc$P.zero)])
  # plot(dD.drift.correct[unlist(STND.loc$P.depl)])
  # plot(d18O.drift.correct[unlist(STND.loc$P.mid)])
  # plot(d18O.drift.correct[unlist(STND.loc$P.zero)])
  # plot(d18O.drift.correct[unlist(STND.loc$P.depl)])
  
  return(data.reduce)
}
