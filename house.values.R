# Script to calculate values for inhouse standards

# Working directory must be at the top level of the repository
script.dir = dirname(parent.frame(2)$ofile)
setwd(script.dir)

# Isotope files and corresponding tray templates to loop through in script
files = list('Init_calib_17Oct2016', 'Init_calib2_20Jan2017', 'house_calib_2017Aug08', 
             'house_calib_2017Aug17')
templates = list('Init.calib.17Oct2016', 'Init.calib2.20Jan2017', 'house.calibrate',
                 'house.calibrate')

# Preallocate data frames for isotope classes
P.mid.all = list()
P.zero.all = list()
P.depl.all = list()
CIFA.all = list()
HOTH.all = list()
WAIS.all = list()

for (i in 1:length(files)) {
  # folder.name = unlist(files[i])
  # template.name = unlist(templates[i])
  files.path_i = choose.dir(caption = "Select folder containing raw isotope data")
  template.path_i = choose.files(caption = 'Select sample tray template file')
  
  # Sources and runs the isotope reduction functions based on input variables above
  source('iso.reduce.R')
  data = iso.reduce(files.path_i, template.path_i)
  
  # Load template-specific data (location of standards/QC, standards values, etc.)
  load(template.path_i)
  
  # Primary standards
  P.mid.all[[i]] = data[unlist(STND.loc[1]),]
  P.zero.all[[i]] = data[unlist(STND.loc[2]),]
  P.depl.all[[i]] = data[unlist(STND.loc[3]),]
  
  # Lab Standards
  WAIS.all[[i]] = data[WAIS,]
  CIFA.all[[i]]= data[CIFA,]
  HOTH.all[[i]] = data[HOTH,]
}

# Combine lists into dataframes for each isotope set
P.mid = do.call(rbind, P.mid.all)
P.zero = do.call(rbind, P.zero.all)
P.depl = do.call(rbind, P.depl.all)
CIFA = do.call(rbind, CIFA.all)
HOTH = do.call(rbind, HOTH.all)
WAIS = do.call(rbind, WAIS.all)

# Remove outliers from isotope datasets
P.mid.d18O = P.mid$d18O.correct[!P.mid$d18O.correct %in% boxplot.stats(P.mid$d18O.correct)$out]
P.mid.dD = P.mid$dD.correct[!P.mid$dD.correct %in% boxplot.stats(P.mid$dD.correct)$out]
P.zero.d18O = P.zero$d18O.correct[!P.zero$d18O.correct %in% boxplot.stats(P.zero$d18O.correct)$out]
P.zero.dD = P.zero$dD.correct[!P.zero$dD.correct %in% boxplot.stats(P.zero$dD.correct)$out]
P.depl.d18O = P.depl$d18O.correct[!P.depl$d18O.correct %in% boxplot.stats(P.depl$d18O.correct)$out]
P.depl.dD = P.depl$dD.correct[!P.depl$dD.correct %in% boxplot.stats(P.depl$dD.correct)$out]
WAIS.d18O = WAIS$d18O.correct[!WAIS$d18O.correct %in% boxplot.stats(WAIS$d18O.correct)$out]
WAIS.dD = WAIS$dD.correct[!WAIS$dD.correct %in% boxplot.stats(WAIS$dD.correct)$out]
CIFA.d18O = CIFA$d18O.correct[!CIFA$d18O.correct %in% boxplot.stats(CIFA$d18O.correct)$out]
CIFA.dD = CIFA$dD.correct[!CIFA$dD.correct %in% boxplot.stats(CIFA$dD.correct)$out]
HOTH.d18O = HOTH$d18O.correct[!HOTH$d18O.correct %in% boxplot.stats(HOTH$d18O.correct)$out]
HOTH.dD = HOTH$dD.correct[!HOTH$dD.correct %in% boxplot.stats(HOTH$dD.correct)$out]

# Mean standards values and uncertainty (99% condifence interval), calculated on datasets with 
# the outliers removed
values = matrix(data = NA, nrow = 6, ncol = 4)
rownames(values) = c('P.mid', 'P.zero', 'P.depl', 'WAIS', 'CIFA', 'HOTH')
colnames(values) = c('d18O.val', 'd18O.CI99', 'dD.val', 'dD.CI99')
values[1,] = c(mean(P.mid.d18O), 2.58*sd(P.mid.d18O)/sqrt(length(P.mid.d18O)), 
               mean(P.mid.dD), 2.58*sd(P.mid.dD)/sqrt(length(P.mid.dD)))
values[2,] = c(mean(P.zero.d18O), 2.58*sd(P.zero.d18O)/sqrt(length(P.zero.d18O)), 
               mean(P.zero.dD), 2.58*sd(P.zero.dD)/sqrt(length(P.zero.dD)))
values[3,] = c(mean(P.depl.d18O), 2.58*sd(P.depl.d18O)/sqrt(length(P.depl.d18O)), 
               mean(P.depl.dD), 2.58*sd(P.depl.dD)/sqrt(length(P.depl.dD)))
values[4,] = c(mean(WAIS.d18O), 2.58*sd(WAIS.d18O)/sqrt(length(WAIS.d18O)), 
               mean(WAIS.dD), 2.58*sd(WAIS.dD)/sqrt(length(WAIS.dD)))
values[5,] = c(mean(CIFA.d18O), 2.58*sd(CIFA.d18O)/sqrt(length(CIFA.d18O)), 
               mean(CIFA.dD), 2.58*sd(CIFA.dD)/sqrt(length(CIFA.dD)))
values[6,] = c(mean(HOTH.d18O), 2.58*sd(HOTH.d18O)/sqrt(length(HOTH.d18O)), 
               mean(HOTH.dD), 2.58*sd(HOTH.dD)/sqrt(length(HOTH.dD)))
values = as.data.frame(values)

