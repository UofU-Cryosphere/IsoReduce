template.read = function(template.path) {
  
  # Read in the template data from the .xlsx template file for the given run
  library(readxl)
  template.data = read_excel(template.path, range = "L2:P107")
  
  # Remove ports with no samples
  template.data = template.data[complete.cases(template.data$`Sample No.`),]
  
  # Define column data types
  template.data$`Sample No.` = factor(template.data$`Sample No.`)
  
  # Find the locations of all standards
  stnd.idx = grep("STND", template.data$`Sample No.`)
  
  # Find sample ID numbers of unique standards in data
  stnd.unique = unique(template.data$`Sample No.`[stnd.idx])
  
  # Add locations of the different standards to preallocated list
  stnd.loc = vector(mode = "list", length = length(stnd.unique))
  for (i in 1:length(stnd.loc)) {
    stnd.loc[[i]] = template.data$`Port No.`[which(template.data$`Sample No.` == stnd.unique[i])]
    
  }
}