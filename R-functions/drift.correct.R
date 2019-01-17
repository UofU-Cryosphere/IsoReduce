## Custom function to correct drift in isotope values due to intra-run instrument drift
drift.correct = function(iso.data, STNDs) {
  
  # Preallocate data frames for the value of the slope of the instrument drift for each of the 
  # isotope standards
  d18O.differential = data.frame(val = as.numeric(), slope = as.numeric())
  dD.differential = data.frame(val = as.numeric(), slope = as.numeric())
  
  for (i in 1:length(STNDs)) {
    
    # Iteratively extract indices for the different standards in the data set
    stnd.idx = match(STNDs[[i]], iso.data$Sample.port)
    data.stnd = iso.data[stnd.idx,]
    data.stnd$Sample.sequence = stnd.idx
    # data.QC = iso.data[unlist(QC[i]),]
    # data.QC$Sample.sequence = unlist(QC[i])
    
    # Iteratively estimate the drift coefficient (the slope) for each standard
    d18O.drift.trend = lm(d18O.predict ~ Sample.port, data = data.stnd)
    d18O.drift.m = as.numeric(d18O.drift.trend$coefficients[2])
    dD.drift.trend = lm(dD.predict ~ Sample.sequence, data = data.stnd)
    dD.drift.m = as.numeric(dD.drift.trend$coefficients[2])
    
    # Iteratively bind each standards value and drift coefficient to preallocated data frame
    d18O.differential = rbind(d18O.differential, c(mean(data.stnd$d18O.predict), d18O.drift.m))
    dD.differential = rbind(dD.differential, c(mean(data.stnd$dD.predict), dD.drift.m))
  }
  
  # Rename columns and rows of dataframes for improved readability
  colnames(d18O.differential) = c('val', 'slope')
  rownames(d18O.differential) = names(STNDs)
  colnames(dD.differential) = c('val', 'slope')
  rownames(dD.differential) = names(STNDs)
  
  # Estimate the trend and intercept for the change in drift coefficient with isotopic value
  d18O.diff.trend = lm(slope ~ val, data = d18O.differential)
  d18O.diff.intercept = d18O.diff.trend$coefficients[1]
  d18O.diff.slope = d18O.diff.trend$coefficients[2]
  dD.diff.trend = lm(slope ~ val, data = dD.differential)
  dD.diff.intercept = dD.diff.trend$coefficients[1]
  dD.diff.slope = dD.diff.trend$coefficients[2]
  
  # Calculate drift corrected isotopic values based on isotopic value and position in instrument 
  # run sequence (in the future, may want to change this to be based off of $Time rather than 
  # run sequence)
  d18O.drift.correct = iso.data$d18O.predict - 
    (d18O.diff.slope*iso.data$d18O.predict + d18O.diff.intercept)*seq(1:nrow(iso.data))
  dD.drift.correct = iso.data$dD.predict - 
    (dD.diff.slope*iso.data$dD.predict + dD.diff.intercept)*seq(1:nrow(iso.data))
  
  
  # ### TEST OF IGNORING EFFECT OF TREND WITH ISOTOPE VALUE (MAY BE COVERED IN NORMALIZATION?)
  # d18O.drift.correct = iso.data$d18O.predict - mean(d18O.differential$slope)*seq(1:nrow(iso.data))
  # dD.drift.correct = iso.data$dD.predict - mean(dD.differential$slope)*seq(1:nrow(iso.data))
  # ### TEST OF IGNORING EFFECT OF TREND WITH ISOTOPE VALUE (MAY BE COVERED IN NORMALIZATION?)
  
  
  # Reformat drift-corrected results as list for export
  output = list(d18O.drift.correct, dD.drift.correct)
  
  return(output)
}