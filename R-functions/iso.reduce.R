# Function for reducing isotope data within a given folder, based on a given tray template
iso.reduce = function(files.paths, template.path) {
  
  # Source data reduction functions
  source('R-functions/iso.combine.R')
  source('R-functions/iso.loop.R')
  source('R-functions/drift.correct.R')
  source('R-functions/bias.correct.R')
  source('R-functions/template.read.R')
  
  # Concatenates an arbitrary number of Picarro .csv files into a single R dataframe
  data.all = iso.combine(files.paths)
  
  # Loop through sample injections to correct the data for sample memory effects
  data.reduce = iso.loop(data.all)
  
  # Load template-specific data (location of standards/QC, standards values, etc.)
  template.data = template.read(template.path)
  STND.loc = template.data[[1]]
  STND.val = template.data[[2]]
  QC.loc = template.data[[3]]
  QC.val = template.data[[4]]
  
  # Drift correct isotope data
  drift.correction = drift.correct(data.reduce, STND.loc)
  d18O.drift.correct = unlist(drift.correction[1])
  dD.drift.correct = unlist(drift.correction[2])
  
  
  STND.idx = vector(mode = "list", length = length(STND.loc))
  for (i in 1:length(STND.loc)) {
    STND.idx[[i]] = match(STND.loc[[i]], data.reduce$Sample.port)
  }
  
  
  
  # Correct linear offset in isotope data using values of known standards
  bias.correction = bias.correct(d18O.drift.correct, dD.drift.correct, STND.idx, STND.val)
  
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
  d18O.error = (data.reduce$d18O.correct[match(QC.loc, data.reduce$Sample.port)] - QC.val[1])
  dD.error = (data.reduce$dD.correct[match(QC.loc, data.reduce$Sample.port)] - QC.val[2])
  
  
  # Compare corrected QC sample values to their true values
  
  
  
  
  
  return(data.reduce)
}
