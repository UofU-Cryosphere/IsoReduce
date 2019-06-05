
# Custom function to correct isotope values of individual sample injection for memory
# effects from the previous injections. 'iso.data' are all injections from a given sample
memory.correct = function(iso.data) {

  # If the max difference in values is below a given threshold, do not correct injections
  # for memory effects, but output the raw injection values
  if (abs(max(iso.data$d.18_16.Mean) - min(iso.data$d.18_16.Mean)) < 0.2) {
    d18O.method = as.character('Mean')
    d18O.correct = iso.data$d.18_16.Mean
    result = list(d18O.method, d18O.correct)
    # }
  } else {

    # Attempt memory correction using a power series optimized using the individual
    # injection estimates for each sample
    result = tryCatch(
      {
        d18O.method = as.character('Curve')
        data.temp = data.frame(iso=numeric(nrow(iso.data)-1),
                               Inj.Nr = integer(nrow(iso.data)-1))
        data.temp$iso = diff(iso.data$d.18_16.Mean)
        data.temp$Inj.Nr = seq(1:nrow(data.temp))
        d18O.curve = nls(iso ~ a*Inj.Nr^(b), data = data.temp,
                         start = list(a = head(data.temp$iso, 1)
                                      - tail(data.temp$iso, 1) ,b = -2))
        d18O.summ = summary(d18O.curve)
        a = d18O.summ$coefficients[1,1]
        b = d18O.summ$coefficients[2,1]
        e = a*b*20^(b+1)
        d18O.correct = iso.data$d.18_16.Mean - a*b*iso.data$Inj.Nr^(b+1) + e

        result = list(d18O.method, d18O.correct)
      },

      # If the power series model fails for any reason, switch to using a geometric
      # series mixing model instead
      error = function(err) {
        d18O.method = as.character('Mixing')
        b = -1
        k = 2^b
        Fx = k
        Fy = k^2
        Fz = k^3
        coeff = Fx + Fy + Fz
        Fx = Fx/coeff
        Fy = Fy/coeff
        Fz = Fz/coeff

        d18O.correct = vector(mode = 'numeric', length = nrow(iso.data)-2)
        for (i in 3:nrow(iso.data)) {
          d18O.correct[i-2] = (iso.data$d.18_16.Mean[i] - Fy*iso.data$d.18_16.Mean[i-1]
                               - Fz*iso.data$d.18_16.Mean[i-2])/Fx
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


  ## Same subroutine as that performed on d18O values, applied to the dD values

  # If the max difference in values is below a given threshold, do not correct injections
  # for memory effects, but output the raw injection values
  if (abs(max(iso.data$d.D_H.Mean) - min(iso.data$d.D_H.Mean)) < 0.2) {
    dD.method = as.character('Mean')
    dD.correct = iso.data$d.D_H.Mean
    result = list(dD.method, dD.correct)
    # }
  } else {

    # Attempt memory correction using a power series optimized using the individual
    # injection estimates for each sample
    result = tryCatch(
      {
        dD.method = as.character('Curve')
        data.temp = data.frame(iso=numeric(nrow(iso.data)-1),
                               Inj.Nr = integer(nrow(iso.data)-1))
        data.temp$iso = diff(iso.data$d.D_H.Mean)
        data.temp$Inj.Nr = seq(1:length(data.temp$iso))
        dD.curve = nls(iso ~ a*Inj.Nr^(b), data = data.temp,
                       start = list(a = head(data.temp$iso, 1) - tail(data.temp$iso, 1),
                                    b = -2))
        dD.summ = summary(dD.curve)
        a = dD.summ$coefficients[1,1]
        b = dD.summ$coefficients[2,1]
        e = a*b*20^(b+1)
        dD.correct = iso.data$d.D_H.Mean - a*b*iso.data$Inj.Nr^(b+1) + e

        result = list(dD.method, dD.correct)
      },

      # If the power series model fails for any reason, switch to using a geometric
      # series mixing model instead
      error = function(err) {
        dD.method = as.character('Mixing')
        b = -1
        k = 2^b
        Fx = k
        Fy = k^2
        Fz = k^3
        coeff = Fx + Fy + Fz
        Fx = Fx/coeff
        Fy = Fy/coeff
        Fz = Fz/coeff

        dD.correct = vector(mode = 'numeric', length = nrow(iso.data)-2)
        for (i in 3:nrow(iso.data)) {
          dD.correct[i-2] = (iso.data$d.D_H.Mean[i] - Fy*iso.data$d.D_H.Mean[i-1]
                             - Fz*iso.data$d.D_H.Mean[i-2])/Fx
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

  # Diagnostic plots for debugging
  plot(iso.data$d.18_16.Mean,
       ylim = c(min(c(min(iso.data$d.18_16.Mean), min(d18O.correct))),
                max(c(max(iso.data$d.18_16.Mean), max(d18O.correct)))))
  points(d18O.correct, col = 'red')
  plot(iso.data$d.D_H.Mean,
       ylim = c(min(c(min(iso.data$d.D_H.Mean), min(dD.correct))),
                max(c(max(iso.data$d.D_H.Mean), max(dD.correct)))))
  points(dD.correct, col = 'red')

  # Return results as a list of final method used, corrected isotope values and estimated
  # error on correction for d18O and dD values
  output = list(d18O.method, d18O.predict, d18O.sigma, dD.method, dD.predict, dD.sigma)
  return(output)
}
