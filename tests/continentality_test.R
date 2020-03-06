devtools::load_all()

# lon = 0:50
# lat = 0:50
# Udsc = array(1, dim = c(51, 51, 2))
# Vdsc = array(1, dim = c(51, 51, 2))
# mask = array(seq(0, 1, length.out = 51*51) > 0.5, dim = c(51, 51))
# continentality <- compute_continentality(
#   X1 = 10,
#   X2 = 40,
#   Y1 = 10,
#   Y2 = 40,
#   lon = lon,
#   lat = lat,
#   Udsc = Udsc,
#   Vdsc = Vdsc,
#   mask = mask
# )
# print(str(continentality))
# 
# par(mfrow = c(1, 3))
# image(mask)
# image(continentality$Dco)
# image(continentality$Aco[,, 1])

library(ncdf4)
library(maps)
world = map("world", plot = FALSE)
filein <-  '/home/starmip/sthao/2_Code/Kageyama/Downscaling_Sept_2014_lonlat/Inputs/CRU_Europe_30Eplus.nc' 
nc   <- nc_open(filein)
lon  <- ncvar_get(nc, 'longitude')
lat  <- ncvar_get(nc, 'latitude')
mask  <- ncvar_get(nc, 'elv')
mask[] <- ifelse(is.na(mask) | mask <= 0, 0, 1) 
nc_close(nc)
# Correction du mask pour pas avoir de mer entre scandinavie et europe
for (j in seq_along(lat)){
  for (i in seq_along(lon)){
        if (lat[j] > 55 & lat[j]  < 60 & lon[i] > 5 & lon[i] < 15){
          mask[i, j] = 1.0
        }
  }
}


model_input <- '/home/starmip/sthao/2_Code/Kageyama/Downscaling_Sept_2014_lonlat/Inputs/Masa-IPSL_CM5_CTRL_historical_interpos_vents_uv10m/IPSL_CM5_historical_interpos_vents_u10m_y1950.nc' 
nc   <- nc_open(model_input)
Udsc  <- ncvar_get(nc, 'u10m_exp2')
Vdsc  <- ncvar_get(nc, 'v10m_exp2')
lon  <- ncvar_get(nc, 'lon')
lat  <- ncvar_get(nc, 'lat')
nc_close(nc)

fileout <- '/home/starmip/sthao/2_Code/Kageyama/Downscaling_Sept_2014_lonlat/SOURCES-OPENMP-Masa/output_predictors.nc' 
nc   <- nc_open(fileout)
Aco <- ncvar_get(nc, 'Aco')
Dco  <- ncvar_get(nc, 'Dco')
maskout  <- ncvar_get(nc, 'mask')
nc_close(nc)

par(mfrow = c(2, 1))
image(lon, lat, mask)
lines(world)
image(lon, lat, maskout)
lines(world)
print(all.equal(mask, maskout))

# Fenetre de calcul
X1 <- -10                          # Longitude Ouest de la fenêtre de calcul
X2 <- 25                           # Longitude Est de la fenêtre de calcul
Y1 <- 33                           # Latitude Sud de la feneêtre de calcul
Y2 <- 60                            # Latitude Nord de la fenêtre de calcul

continentality <- compute_continentality(
  X1 = X1,
  X2 = X2,
  Y1 = Y1,
  Y2 = Y2,
  lon = lon,
  lat = lat,
  Udsc = Udsc,
  Vdsc = Vdsc,
  mask = mask
  )
print(str(continentality))

par(mfrow = c(1, 3))
image(lon, lat, mask)
lines(world)
image(lon, lat, continentality$Dco)
lines(world)
image(lon, lat, continentality$Aco[,, 1])
lines(world)


par(mfrow = c(2, 2))
image(lon, lat, continentality$Dco)
lines(world)
image(lon, lat, continentality$Aco[,, 1])
lines(world)
image(lon, lat, Dco)
lines(world)
image(lon, lat, Aco[,, 1])
lines(world)
