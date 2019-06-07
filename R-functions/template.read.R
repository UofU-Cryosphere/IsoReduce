template.read = function(template.path) {

  # Read in the template data from the .xlsx template file for the given run
  library(readxl)
  template.data = read_excel(template.path, range = "L2:P107")

  # Remove ports with no samples
  template.data = template.data[complete.cases(template.data$Sample_num),]

  # Find the locations of all standards
  stnd.idx = grep("STND", template.data$Sample_num)

  # Find sample numbers of unique standards in data
  stnd.unique = unique(template.data$Sample_num[stnd.idx])

  # Find locations and values of the all standards used in run
  STND.loc = vector(mode = "list", length = length(stnd.unique))
  STND.names = character()
  STND.val = numeric()
  for (i in 1:length(STND.loc)) {
    STND.loc[[i]] = as.integer(template.data$Port_num[which(template.data$Sample_num ==
                                                              stnd.unique[i])])
    STND.names = c(STND.names,
                   template.data$Sample_ID[which(template.data$Sample_num == stnd.unique[i])[1]])
    stnd.d180 = mean(template.data$d18O[which(template.data$Sample_num == stnd.unique[i])])
    stnd.dD = mean(template.data$dD[which(template.data$Sample_num == stnd.unique[i])])
    STND.val = rbind(STND.val, c(stnd.d180, stnd.dD))

  }

  # Add names and variable names to the standards arrays
  colnames(STND.val) = c('d18O', 'dD')
  rownames(STND.val) = STND.names
  names(STND.loc) = STND.names


  # Find the locations of all quality control samples
  QC.idx = grep("QC", template.data$Sample_num)

  # Get port locations of all QC samples
  QC.loc = as.integer(template.data$Port_num[QC.idx])

  # Get true isotope values for all QC samples
  QC.val = cbind(template.data$d18O[QC.idx], template.data$dD[QC.idx])

  # Get sample names for all QC samples
  QC.names = template.data$Sample_ID[QC.idx]

  # Add names and variable names to the QC arrays
  colnames(QC.val) = c('d18O', 'dD')
  rownames(QC.val) = QC.names
  names(QC.loc) = QC.names



  IDs = as_tibble(template.data) %>% dplyr::select(Port_num, Sample_num, Sample_ID)

  return(list(STND.loc, STND.val, QC.loc, QC.val, IDs))

}


