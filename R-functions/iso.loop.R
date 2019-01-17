## For loop function to calculate mean isotope values for all samples using iso.curve function

iso.loop = function(data.all) {
  
  # Source the memory correction function
  source('R-functions/memory.correct.R')
  
  # Preallocate arrays for reduced data
  Sample.port = numeric()
  d18O.method = character()
  d18O.predict = numeric()
  d18O.sigma = numeric()
  dD.method = character()
  dD.predict = numeric()
  dD.sigma = numeric()
  
  # Define starting values for use in for loop
  matrix.row = 1
  sample.nr = data.all$Sample[1]
  sample.row = 1
  
  # Iteratively calculate final estimate for isotope ratios for each sample
  for (i in 1:nrow(data.all)) {
    if (data.all$Sample[i] != sample.nr) { # Execute after change in sample number
      data.i = data.all[sample.row:(i-1),]
      output = memory.correct(data.i)
      Sample.port = rbind(Sample.port, sample.nr)
      d18O.method = rbind(d18O.method, output[1])
      d18O.predict = rbind(d18O.predict, output[2])
      d18O.sigma = rbind(d18O.sigma, output[3])
      dD.method = rbind(dD.method, output[4])
      dD.predict = rbind(dD.predict, output[5])
      dD.sigma = rbind(dD.sigma, output[6])
      sample.row = i
      sample.nr = data.all$Sample[i]
      matrix.row = matrix.row+1
    } 
    else if (i == nrow(data.all)) { # Execute if final row of data
      data.i = data.all[sample.row:i,]
      output = memory.correct(data.i)
      Sample.port = rbind(Sample.port, sample.nr)
      d18O.method = rbind(d18O.method, output[1])
      d18O.predict = rbind(d18O.predict, output[2])
      d18O.sigma = rbind(d18O.sigma, output[3])
      dD.method = rbind(dD.method, output[4])
      dD.predict = rbind(dD.predict, output[5])
      dD.sigma = rbind(dD.sigma, output[6])
      #data.reduce[matrix.row,] = c(sample.nr, iso.curve(data.i))
    }
  }
  
  # Place reduced data into data frame (numeric and factor classes)
  data.reduce = data.frame(Sample.port = as.numeric(Sample.port), d18O.method = 
                             factor(d18O.method, c('Curve', 'Mixing', 'Mean')), d18O.predict = 
                             as.numeric(d18O.predict), d18O.sigma = as.numeric(d18O.sigma), 
                           dD.method = factor(dD.method, c('Curve', 'Mixing', 'Mean')), dD.predict = 
                             as.numeric(dD.predict), dD.sigma = as.numeric(dD.sigma))
  return(data.reduce)
}