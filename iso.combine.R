iso.combine = function(...) {
  vararg = (...)
  num.arg = length(vararg)
  data.run = read.csv(as.character(vararg[1]))
  if (num.arg == 1) {
  } 
  else {
    for (i in 2:num.arg) {
      data.add = read.csv(as.character(vararg[i]))
      data.run = rbind(data.run, data.add)
    }
  }
  data = data.frame(data.run$Timestamp.Mean, data.run$Sample, data.run$Inj.Nr, 
                    data.run$d.18_16.Mean, data.run$d.D_H.Mean)
  names(data) = c('Timestamp', 'Sample', 'Inj.Nr', 'd.18_16.Mean', 'd.D_H.Mean')
  
  # Removes missed injections (in the future this will change to flag missed injections)
  data = data[!(is.na(data[,1])),]
  rownames(data) = NULL
  return(data)
}