# Function for reducing isotope data within a given folder, based on a given tray template
iso.reduce = function(files.paths, template.path) {

  # Source data reduction functions
  source(here('R-functions', 'iso.combine.R'))
  source(here('R-functions', 'iso.loop.R'))
  source(here('R-functions', 'drift.correct.R'))
  source(here('R-functions', 'bias.correct.R'))
  source(here('R-functions', 'template.read.R'))

  # Concatenates an arbitrary number of Picarro .csv files into a single R dataframe
  data.all = as_tibble(iso.combine(files.paths))

  # Loop through sample injections to correct the data for sample memory effects
  data.reduce = iso.loop(data.all)

  # Load template-specific data (location of standards/QC, standards values, etc.)
  template.data = template.read(template.path)
  STND.loc = template.data[[1]]
  STND.val = template.data[[2]]
  QC.loc = template.data[[3]]
  QC.val = template.data[[4]]
  IDs = template.data[[5]]

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
  # (also drop the memory-only corrected values)
  data.reduce = data.reduce %>% mutate(d18O.correct = unlist(bias.correction[1]),
                                       dD.correct = unlist(bias.correction[2]))

  # # Diagnostics to check drif corrections
  # plot(dD.drift.correct[unlist(STND.loc$P.mid)])
  # plot(dD.drift.correct[unlist(STND.loc$P.zero)])
  # plot(dD.drift.correct[unlist(STND.loc$P.depl)])
  # plot(d18O.drift.correct[unlist(STND.loc$P.mid)])
  # plot(d18O.drift.correct[unlist(STND.loc$P.zero)])
  # plot(d18O.drift.correct[unlist(STND.loc$P.depl)])


  ## Compare corrected QC sample values to their true values

  # Get index of QC samples within data
  QC.idx = match(QC.loc, data.reduce$Sample.port)


  d18O.error = rep(0, times = length(QC.idx))
  dD.error = rep(0, times = length(QC.idx))
  if (length(QC.idx) > 1) {
    for (i in 1:length(QC.idx)) {

      QC.i = QC.val[i,]
      d18O.error[i] = data.reduce$d18O.correct[QC.idx[i]] - QC.i[1]
      dD.error[i] = data.reduce$dD.correct[QC.idx[i]] - QC.i[2]
    }
  } else {

    d18O.error[i] = data.reduce$d18O.correct[QC.idx] - QC.val[1]
    dD.error[i] = data.reduce$dD.correct[QC.idx] - QC.val[2]
  }

  # Add Sample numbers and IDs via left-join,
  # and select only columns of interest to export
  data.reduce = data.reduce %>%
    left_join(IDs, by = c("Sample.port" = "Port_num")) %>%
    select(Sample_num, Sample_ID, d18O.correct, d18O.sigma, d18O.method, dD.correct, dD.sigma, dD.method)




  return(data.reduce)
}
