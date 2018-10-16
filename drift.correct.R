## Custom function to correct drift in isotope values due to intra-run instrument drift
drift.correct = function(iso.data, QC) {
  
  # Preallocate data frames for the value of the slope of the instrument drift for each of the 
  # isotope standards
  d18O.differential = data.frame(val = as.numeric(), slope = as.numeric())
  dD.differential = data.frame(val = as.numeric(), slope = as.numeric())
  
  for (i in 1:length(QC)) {
    
    # Iteratively extract indices for the different standards in the data set
    data.QC = iso.data[unlist(QC[i]),]
    data.QC$Sample.sequence = unlist(QC[i])
    
    # Iteratively estimate the drift coefficient (the slope) for each standard
    d18O.drift.trend = lm(d18O.predict ~ Sample.sequence, data = data.QC)
    d18O.drift.m = as.numeric(d18O.drift.trend$coefficients[2])
    dD.drift.trend = lm(dD.predict ~ Sample.sequence, data = data.QC)
    dD.drift.m = as.numeric(dD.drift.trend$coefficients[2])
    
    # Iteratively bind each standards value and drift coefficient to preallocated data frame
    d18O.differential = rbind(d18O.differential, c(mean(data.QC$d18O.predict), d18O.drift.m))
    dD.differential = rbind(dD.differential, c(mean(data.QC$dD.predict), dD.drift.m))
  }
  
  # Rename columns and rows of dataframes for improved readability
  colnames(d18O.differential) = c('val', 'slope')
  rownames(d18O.differential) = rownames(QC)
  colnames(dD.differential) = c('val', 'slope')
  rownames(dD.differential) = rownames(QC)
  
  # Estimate the trend and intercept for the change in drift coefficient with isotopic value
  d18O.diff.trend = lm(slope ~ val, data = d18O.differential)
  d18O.diff.intercept = d18O.diff.trend$coefficients[1]
  d18O.diff.slope = d18O.diff.trend$coefficients[2]
  dD.diff.trend = lm(slope ~ val, data = dD.differential)
  dD.diff.intercept = dD.diff.trend$coefficients[1]
  dD.diff.slope = dD.diff.trend$coefficients[2]
  
  # Calculate drift corrected isotopic values based on isotopic value and position in instrument 
  # run sequence
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