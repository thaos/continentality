devtools::load_all()

compute_continentality(
  X1 = 10,
  X2 = 40,
  Y1 = 10,
  Y2 = 40,
  lon = 0:50,
  lat = 0:50,
  Udsc = array(1, dim = c(51, 51, 2)),
  Vdsc = array(1, dim = c(51, 51, 2)),
  mask = array(seq(0, 1, length.out = 51*51) > 0.5, dim = c(51, 51))
)
  
