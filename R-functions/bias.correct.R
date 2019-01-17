# Function to correct linear bias in isotopes based on known standards values
bias.correct = function(d18O.data, dD.data, STND.idx, STND.val) {
  
  # Preallocate matrix for measured standards values
  STND.measure = matrix(data = NA, nrow = nrow(STND.val), ncol = ncol(STND.val))
  
  # Iteratively calculate the mean measured isotopic values for each standard
  for (i in 1:length(STND.idx)) {
    
    STND.measure[i,] = c(mean(d18O.data[STND.idx[[i]]]), 
                         mean(dD.data[STND.idx[[i]]]))
  }
  
  # Rename measured standards matrix for improved readability
  rownames(STND.measure) = rownames(STND.val)
  colnames(STND.measure) = colnames(STND.val)
  
  # Normalize the measured d18O values based on the trend in known standards values
  d18O.trend = lm(STND.measure[,1]-STND.val[,1] ~ STND.val[,1])
  d18O.intercept = d18O.trend$coefficients[1]
  d18O.slope = d18O.trend$coefficients[2]
  d18O.bias.correct = d18O.data - (d18O.slope*d18O.data + d18O.intercept)
  
  dD.trend = lm(STND.measure[,2]-STND.val[,2] ~ STND.val[,2])
  dD.intercept = dD.trend$coefficients[1]
  dD.slope = dD.trend$coefficients[2]
  dD.bias.correct = dD.data - (dD.slope*dD.data + dD.intercept)
  
  output = list(d18O.bias.correct, dD.bias.correct)
  return(output)
}