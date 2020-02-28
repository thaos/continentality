compute_continentality <- function(X1, X2, Y1, Y2, lon, lat, Udsc, Vdsc, mask) {
  NX  <- length(lon)
  NY  <- length(lat)
  NBmois  <- dim(Udsc)[3]
  stopifnot(all(NX == c(nrow(Udsc), nrow(Vdsc), nrow(mask))))
  stopifnot(all(NY == c(ncol(Udsc), ncol(Vdsc), ncol(mask))))
  stopifnot(NBmois == dim(Vdsc)[3])
  dlon <- diff(lon)
  stopifnot(length(unique(dlon)) == 1)
  dlat <- diff(lat)
  stopifnot(length(unique(dlat)) == 1)
  dlon <- dlon[1]
  dlat <- dlat[1]
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
  if (!is.double(mask)) {storage.mode(mask) <- 'double'}
  cat(NX, " ", NY, " ", NBmois, "\n")
  .Call(
    c_compute_continentality_f,
    NX, NY, NBmois,
    X1, X2, Y1, Y2,
    dlon, dlat,
    lon, lat,
    Udsc, Vdsc,
    mask
  )
}
