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
  data = data.frame(Time = strptime(as.character(data.run$Time.Code), format = "%Y/%m/%d %H:%M:%S"), 
                    Sample = as.integer(data.run$Sample), Inj.Nr = as.integer(data.run$Inj.Nr), 
                    d.18_16.Mean = as.numeric(data.run$d.18_16.Mean), 
                    d.D_H.Mean = as.numeric(data.run$d.D_H.Mean))
  names(data) = c('Time', 'Sample', 'Inj.Nr', 'd.18_16.Mean', 'd.D_H.Mean')
  
  # Removes missed injections (in the future this will change to flag missed injections)
  data = data[!(is.na(data[,1])),]
  rownames(data) = NULL
  return(data)
}