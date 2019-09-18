# Function to correct isotope values of individual sample injections for memory
# effects from the previous injections.
# 'iso.data' are all injections from a given sample, while 'data.tail' are the
# trailing 3 injections from the last sample.
# For combined datasets with sufficiently small spreads in isotope values, this
# function uses a 4-component geometric series mixing model.
# For datasets with a large spread in values, it switches to a curve-fitting routine
# based on a power series.

memory.correct = function(iso.data, data.tail) {

  # Define coefficient weights based off the first 4 terms of a geometric series
  k = 1/4
  c0 = k
  c1 = k^2
  c2 = k^3
  c3 = k^4
  norm.factor = c0 + c1 + c2 + c3

  F0 = c0/norm.factor
  F1 = c1/norm.factor
  F2 = c2/norm.factor
  F3 = c3/norm.factor


  data.bind = rbind(data.tail, iso.data)



  if (nrow(data.bind) > nrow(iso.data) &&
    max(data.bind$d.18_16.Mean)-min(data.bind$d.18_16.Mean) < 3) {

    d18O.method = as.character("Mix")
    d18O.correct = vector(mode = 'numeric', length = nrow(iso.data))

      for (i in 4:nrow(data.bind)) {

        d18O.correct[i-3] = (data.bind$d.18_16.Mean[i] - F1*data.bind$d.18_16.Mean[i-1] -
                               F2*data.bind$d.18_16.Mean[i-2] - F3*data.bind$d.18_16.Mean[i-3])/F0

        result = list(d18O.method, d18O.correct)
      }
    } else {

      # Attempt memory correction using a power series optimized using the individual
      # injection estimates for each sample
      result = tryCatch(
        {

          d18O.method = as.character("Curve")
          data.tmp = data.frame(Inj.Nr = 1:nrow(iso.data))
          data.tmp$iso = iso.data$d.18_16.Mean - tail(iso.data$d.18_16.Mean, 1)
          d18O.curve = nls(iso ~ a*Inj.Nr^(b), data = data.tmp,
                           start = list(a = head(data.tmp$iso, 1), b = -2))
          d18O.summ = summary(d18O.curve)
          a = d18O.summ$coefficients[1,1]
          b = d18O.summ$coefficients[2,1]

          d18O.correct = iso.data$d.18_16.Mean - a*iso.data$Inj.Nr^(b)

          result = list(d18O.method, d18O.correct)
        },

        # If the power series model fails for any reason, switch to using a geometric
        # series mixing model instead
        error = function(err) {
          d18O.method = as.character("Curve-fail")

          d18O.correct = vector(mode = 'numeric', length = nrow(iso.data)-3)

          for (i in 4:nrow(iso.data)) {

            d18O.correct[i-3] = (iso.data$d.18_16.Mean[i] - F1*iso.data$d.18_16.Mean[i-1] -
                                   F2*iso.data$d.18_16.Mean[i-2] - F3*iso.data$d.18_16.Mean[i-3])/F0
          }


          result = list(d18O.method, d18O.correct)
          return(result)
        }
      )
    }

  d18O.method = as.character(result[1])
  d18O.correct = as.numeric(unlist(result[2]))
  d18O.predict = mean(d18O.correct)
  d18O.sigma = sd(d18O.correct)



  if (nrow(data.bind) > nrow(iso.data) &&
      max(data.bind$d.D_H.Mean)-min(data.bind$d.D_H.Mean) < 25) {

    dD.method = as.character("Mix")
    dD.correct = vector(mode = 'numeric', length = nrow(iso.data))

    for (i in 4:nrow(data.bind)) {

      dD.correct[i-3] = (data.bind$d.D_H.Mean[i] - F1*data.bind$d.D_H.Mean[i-1] -
                             F2*data.bind$d.D_H.Mean[i-2] - F3*data.bind$d.D_H.Mean[i-3])/F0

      result = list(dD.method, dD.correct)
    }
  } else {

    # Attempt memory correction using a power series optimized using the individual
    # injection estimates for each sample
    result = tryCatch(
      {

        dD.method = as.character("Curve")
        data.tmp = data.frame(Inj.Nr = 1:nrow(iso.data))
        data.tmp$iso = iso.data$d.D_H.Mean - tail(iso.data$d.D_H.Mean, 1)
        dD.curve = nls(iso ~ a*Inj.Nr^(b), data = data.tmp,
                         start = list(a = head(data.tmp$iso, 1), b = -2))
        dD.summ = summary(dD.curve)
        a = dD.summ$coefficients[1,1]
        b = dD.summ$coefficients[2,1]

        dD.correct = iso.data$d.D_H.Mean - a*iso.data$Inj.Nr^(b)

        result = list(dD.method, dD.correct)
      },

      # If the power series model fails for any reason, switch to using a geometric
      # series mixing model instead
      error = function(err) {
        dD.method = as.character("Curve-fail")

        dD.correct = vector(mode = 'numeric', length = nrow(iso.data)-3)

        for (i in 4:nrow(iso.data)) {

          dD.correct[i-3] = (iso.data$d.D_H.Mean[i] - F1*iso.data$d.D_H.Mean[i-1] -
                                 F2*iso.data$d.D_H.Mean[i-2] - F3*iso.data$d.D_H.Mean[i-3])/F0
        }


        result = list(dD.method, dD.correct)
        return(result)
      }
    )
  }

  dD.method = as.character(result[1])
  dD.correct = as.numeric(unlist(result[2]))
  dD.predict = mean(dD.correct)
  dD.sigma = sd(dD.correct)




  # # Diagnostic plots for debugging
  # plot(iso.data$d.18_16.Mean,
  #      ylim = c(min(c(min(iso.data$d.18_16.Mean), min(d18O.correct))),
  #               max(c(max(iso.data$d.18_16.Mean), max(d18O.correct)))))
  # points(d18O.correct, col = 'red')
  # lines(iso.data$Inj.Nr, rep(d18O.predict, times = nrow(iso.data)), col = 'red')
  #
  # plot(iso.data$d.D_H.Mean,
  #      ylim = c(min(c(min(iso.data$d.D_H.Mean), min(dD.correct))),
  #               max(c(max(iso.data$d.D_H.Mean), max(dD.correct)))))
  # points(dD.correct, col = 'red')
  # lines(iso.data$Inj.Nr, rep(dD.predict, times = nrow(iso.data)), col = 'red')



  # Return results as a list of final method used, corrected isotope values and estimated
  # error on correction for d18O and dD values
  output = list(d18O.method, d18O.predict, d18O.sigma, dD.method, dD.predict, dD.sigma)
  return(output)

}
