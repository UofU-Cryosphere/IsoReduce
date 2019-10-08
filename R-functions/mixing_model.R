# Function to correct isotope data based on geometric mixing model

mixing_model = function(iso.data, r, num.components) {

  # Define normalizing factor for mixing model components
  num.prev = num.components-1
  norm.factor = sum(r^(0:num.prev))
  mix.factor = (r^(1:num.prev))


  iso.correct = vector(mode = 'numeric', length = (length(iso.data)-num.prev))
  for (i in num.components:length(iso.data)) {

    prev.idx = seq(from = (i-1), to = (i-num.prev))
    iso.correct[i-num.prev] = norm.factor*iso.data[i] - sum(mix.factor*iso.data[prev.idx])
  }

  return(iso.correct)

}
