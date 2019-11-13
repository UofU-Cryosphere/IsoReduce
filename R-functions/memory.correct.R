# Function to correct isotope values of individual sample injections for memory
# effects from the previous injections.
# 'iso.data' are all injections from a given sample, while 'data.tail' are
# trailing injections from the last sample.
# For combined datasets with sufficiently small spreads in isotope values, this
# function uses a multi-component geometric series mixing model.
# For datasets with a large spread in values, it switches to a curve-fitting routine
# based on a power series.

memory.correct = function(iso.data) {

  source(here("R-functions/mixing_model.R"))
  source(here("R-functions/curve_fit.R"))
  r = 1/2
  num.components = 3

  if (abs(iso.data$d.18_16.Mean[1]-iso.data$d.18_16.Mean[nrow(iso.data)]) < 0.1) {

    d18O.method = as.character("Mix")
    d18O.correct = mixing_model(iso.data$d.18_16.Mean, r, num.components)

    result = list(d18O.method, d18O.correct)

    } else {

      # Attempt memory correction using a power series optimized using the individual
      # injection estimates for each sample
      result = tryCatch(
        {

          d18O.method = as.character("Curve")
          d18O.correct = curve_fit(iso.data$d.18_16.Mean)

          result = list(d18O.method, d18O.correct)
        },

        # If the power series model fails for any reason, switch to using a geometric
        # series mixing model instead
        error = function(err) {
          d18O.method = as.character("Curve-fail")

          d18O.correct = vector(mode = 'numeric', length = nrow(iso.data)-3)
          d18O.correct = mixing_model(iso.data$d.18_16.Mean, r, num.components)


          result = list(d18O.method, d18O.correct)
          return(result)
        }
      )
    }

  d18O.method = as.character(result[1])
  d18O.correct = as.numeric(unlist(result[2]))
  d18O.predict = mean(d18O.correct)
  d18O.sigma = sd(d18O.correct)


  if (abs(iso.data$d.D_H.Mean[1]-iso.data$d.D_H.Mean[nrow(iso.data)]) < 1.5) {

    dD.method = as.character("Mix")
    dD.correct = mixing_model(iso.data$d.D_H.Mean, r, num.components)

    result = list(dD.method, dD.correct)

  } else {

    # Attempt memory correction using a power series optimized using the individual
    # injection estimates for each sample
    result = tryCatch(
      {

        dD.method = as.character("Curve")
        dD.correct = curve_fit(iso.data$d.D_H.Mean)

        result = list(dD.method, dD.correct)
      },

      # If the power series model fails for any reason, switch to using a geometric
      # series mixing model instead
      error = function(err) {
        dD.method = as.character("Curve-fail")

        dD.correct = mixing_model(iso.data$d.D_H.Mean, r, num.components)


        result = list(dD.method, dD.correct)
        return(result)
      }
    )
  }

  dD.method = as.character(result[1])
  dD.correct = as.numeric(unlist(result[2]))
  dD.predict = mean(dD.correct)
  dD.sigma = sd(dD.correct)

  # # Diagnostic plots
  # plot(iso.data$Inj.Nr, iso.data$d.18_16.Mean, col = 'black', pch = 16,
  #      ylim = c(min(min(iso.data$d.18_16.Mean),min(d18O.correct)),
  #               max(max(iso.data$d.18_16.Mean),max(d18O.correct))))
  # points((nrow(iso.data)-length(d18O.correct)+1):nrow(iso.data), d18O.correct, col = 'red', pch = 16)
  # lines(iso.data$Inj.Nr, rep(d18O.predict, nrow(iso.data)), col = 'red', lty = 2)
  #
  # plot(iso.data$Inj.Nr, iso.data$d.D_H.Mean, col = 'black', pch = 16,
  #      ylim = c(min(min(iso.data$d.D_H.Mean),min(dD.correct)),
  #               max(max(iso.data$d.D_H.Mean),max(dD.correct))))
  # points((nrow(iso.data)-length(dD.correct)+1):nrow(iso.data), dD.correct, col = 'red', pch = 16)
  # lines(iso.data$Inj.Nr, rep(dD.predict, nrow(iso.data)), col = 'red', lty = 2)
  # browser()

  # Return results as a list of final method used, corrected isotope values and estimated
  # error on correction for d18O and dD values
  output = list(d18O.method, d18O.predict, d18O.sigma, dD.method, dD.predict, dD.sigma)
  return(output)

}
