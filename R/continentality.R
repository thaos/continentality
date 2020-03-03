compute_continentality <- function(X1, X2, Y1, Y2, lon, lat, Udsc, Vdsc, mask) {
  NX  <- length(lon)
  NY  <- length(lat)
  NBmois  <- dim(Udsc)[3]
  stopifnot(all(NX == c(nrow(Udsc), nrow(Vdsc), nrow(mask))))
  stopifnot(all(NY == c(ncol(Udsc), ncol(Vdsc), ncol(mask))))
  stopifnot(NBmois == dim(Vdsc)[3])
  dlon <- diff(lon)
  stopifnot(all(abs(dlon - mean(dlon)) < 1E-2))
  dlat <- diff(lat)
  stopifnot(all(abs(dlat - mean(dlat)) < 1E-2))
  dlon <- mean(dlon)
  dlon = 1/6
  dlat = 1/6
  dlat <- mean(dlat)
  if (!is.integer(NX)) {storage.mode(NX) <- 'integer'}
  if (!is.integer(NY)) {storage.mode(NY) <- 'integer'}
  if (!is.integer(NBmois)) {storage.mode(NBmois) <- 'integer'}
  if (!is.double(dlon)) {storage.mode(dlon) <- 'double'}
  if (!is.double(dlat)) {storage.mode(dlat) <- 'double'}
  if (!is.double(X1)) {storage.mode(X1) <- 'double'}
  if (!is.double(X2)) {storage.mode(X2) <- 'double'}
  if (!is.double(Y1)) {storage.mode(Y1) <- 'double'}
  if (!is.double(Y2)) {storage.mode(Y2) <- 'double'}
  if (!is.double(lon)) {storage.mode(lon) <- 'double'}
  if (!is.double(lat)) {storage.mode(lat) <- 'double'}
  if (!is.double(Udsc)) {storage.mode(Udsc) <- 'double'}
  if (!is.double(Vdsc)) {storage.mode(Vdsc) <- 'double'}
  if (!is.integer(mask)) {storage.mode(mask) <- 'integer'}
  cat(NX, " ", NY, " ", NBmois, "\n")
  # Reformat for C-style indexing
  #   mask <- t(mask)
  #   Udsc <- aperm(Udsc, c(2, 1, 3))
  #   Vdsc <- aperm(Vdsc, c(2, 1, 3))
  cat("Udsc[1:3]", Udsc[1:3], "\n")
  cat("Udsc[10, 9, 5]", Udsc[10, 9, 5], "\n")
  cat("Vdsc[1:3]", Vdsc[1:3], "\n")
  cat("Vdsc[10, 9, 5]", Vdsc[10, 9, 5], "\n")
  cat("mask[1:3]", mask[1:3], "\n")
  cat("mask[10, 9, 5]", mask[10, 9], "\n")
  continentality  <- .Call(
    c_compute_continentality_f,
    NX, NY, NBmois,
    X1, X2, Y1, Y2,
    dlon, dlat,
    lon, lat,
    Udsc, Vdsc,
    mask
  )
  print(str(continentality))
  print(str(Udsc))
  Aco <- structure(continentality[[1]], dim = dim(Udsc))
  #   Aco <- aperm(Aco, c(2, 1, 3))
  Dco <- structure(continentality[[2]], dim = dim(mask))
  #   Dco <- t(Dco) 
  return(
    list(Aco = Aco, Dco = Dco)
  )
}
