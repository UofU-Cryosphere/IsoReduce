# Function to correct isotope data for memory effects based on fitting a power series to the isotope data

curve_fit = function (iso.data) {

  data.tmp = data.frame(Inj.Nr = 1:length(iso.data))
  data.tmp$iso = iso.data - tail(iso.data, 1)
  iso.curve = nls(iso ~ a*Inj.Nr^(b), data = data.tmp,
                  start = list(a = head(data.tmp$iso, 1), b = -1.5))



  iso.summ = summary(iso.curve)
  a = iso.summ$coefficients[1,1]
  b = iso.summ$coefficients[2,1]

  iso.correct = iso.data - a*data.tmp$Inj.Nr^(b)

  # # Diagnostic plots
  # plot(iso.data, ylim = c(min(c(iso.data,iso.correct)), max(c(iso.data, iso.correct))))
  # points(iso.correct, col = 'red', pch = 16)
  # lines(1:10, rep(mean(iso.correct),10), lty = 2, col = 'red')
  # # lines(1:10, rep(median(iso.correct),10), lty = 2, col = 'blue')
  # print(b)
  # browser()

  return(iso.correct)

}
