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
  r = 1/2
  num.components = 3

  if (max(iso.data$d.18_16.Mean)-min(iso.data$d.18_16.Mean) < 0.5) {

    d18O.method = as.character("Mix")
    d18O.correct = mixing_model(iso.data$d.18_16.Mean, r, num.components)

    result = list(d18O.method, d18O.correct)

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


  if (max(iso.data$d.D_H.Mean)-min(iso.data$d.D_H.Mean) < 4) {

    dD.method = as.character("Mix")
    dD.correct = mixing_model(iso.data$d.D_H.Mean, r, num.components)

    result = list(dD.method, dD.correct)

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


  # # Diagnostic plots for debugging
  # plot(iso.data$Inj.Nr, iso.data$d.18_16.Mean,
  #      ylim = c(min(c(min(iso.data$d.18_16.Mean), min(d18O.correct))),
  #               max(c(max(iso.data$d.18_16.Mean), max(d18O.correct)))))
  # if (length(d18O.correct) < nrow(iso.data)) {
  #   points(num.components:nrow(iso.data), d18O.correct, col = 'red')
  # } else {
  #   points(iso.data$Inj.Nr, d18O.correct, col = 'red')
  # }
  # lines(iso.data$Inj.Nr, rep(d18O.predict, times = nrow(iso.data)), col = 'red')
  #
  # plot(iso.data$Inj.Nr ,iso.data$d.D_H.Mean,
  #      ylim = c(min(c(min(iso.data$d.D_H.Mean), min(dD.correct))),
  #               max(c(max(iso.data$d.D_H.Mean), max(dD.correct)))))
  # if (length(dD.correct) < nrow(iso.data)) {
  #   points(num.components:nrow(iso.data), dD.correct, col = 'red')
  # } else {
  #   points(iso.data$Inj.Nr, dD.correct, col = 'red')
  # }
  # lines(iso.data$Inj.Nr, rep(dD.predict, times = nrow(iso.data)), col = 'red')
  # browser()

  # Return results as a list of final method used, corrected isotope values and estimated
  # error on correction for d18O and dD values
  output = list(d18O.method, d18O.predict, d18O.sigma, dD.method, dD.predict, dD.sigma)
  return(output)

}
