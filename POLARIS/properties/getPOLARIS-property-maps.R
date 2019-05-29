library(raster)
source('local-functions.R')

# generate 
r <- raster('E:/gis_data/gSSURGO/pH_300m_AggregateMean/SoilRas_SDV_pHwater_WTA_5to15.tif')

u <- generateLinks(r, property='ph', depth='5_15')


## note: not all of these are valid tiles: lat4748_lon-85-84.tif

## TODO: test for file before downloading

## do this on soilmap2-1

# go get 'em!
for(i in seq_along(url)) {
  of <- paste0('source/', f[i])
  # !!! set download mode to 'binary' or else the resulting files are corrupt
  try(download.file(url[i], destfile = of, mode = 'wb'))
}


## mosaic
# ~ instant
# gdalbuildvrt wi-example.vrt source/*.tif

## TODO: 
# reset notdata and re-scale data to integers

## warp, aggregate, compress
# ~ 5 minutes for native resolution (~30m), 1.4Gb (float32 -> too much precision)
# ~ 2 minutes for 100m res, 326Mb

# gdalwarp -tr 100 100 -t_srs '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs' -multi -co "COMPRESS=LZW" wi-example.vrt polaris-mean-pH-5-15-100m.tif



# 
# aggregate to 100m via mean
# 

# test results
library(raster)
library(sp)
library(rgdal)

# seems to work
GDALinfo('POLARIS/lat4243_lon-92-91.tif')
r <- raster('POLARIS/lat4243_lon-91-90.tif')
plot(r)

# note: get / mosiac / compress on soilmap2-1




# library(sp)
# library(rgdal)
# 
# # make 2 points, offset by 0.00001 degree in longitude and latitude
# p1 <- SpatialPoints(cbind(-119, 38), proj4string = CRS('+proj=longlat +datum=WGS84'))
# p2 <- SpatialPoints(cbind(-119.00001, 38.00001), proj4string = CRS('+proj=longlat +datum=WGS84'))
# p <- rbind(p1, p2)
# 
# # convert to projected coordinate system with horizontal units of meters
# pp <- spTransform(p, CRS('+proj=utm +zone=10'))
# 
# # distance is the hypotenuse of the right triangle connecting the points
# xy <- coordinates(pp)
# 
# # distance = sqrt( dx^2 + dy^2 )
# dxy <- apply(xy, 2, diff)
# sqrt(sum(dxy^2))
# 
# 
# # simpler approach
# library(geosphere)
# geodesic_inverse(p1, p2)
# 
# 
