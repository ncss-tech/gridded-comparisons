
## get the latest rgeedim
# remotes::install_github("brownag/rgeedim")

library(rgeedim)
library(soilDB)
library(aqp)
library(terra)
library(viridis)
library(rasterVis)
library(latticeExtra)
library(tactile)


source('local-functions.R')

# soil texture color table
txt.lut <- read.csv('texture-color-table.csv')

# this should bring up a web page with steps to setup temporary auth
# use the default EE project
# TODO: document this!

# use as needed
# gd_authenticate(auth_mode = "notebook")

# init python back-end
gd_initialize()


# # variable of interest
# .variable <- 'awc_r'
# .SOLUS_variable <- 'AWC'
# .rescale_value <- 0.01
# 
# .variable <- 'claytotal_r'
# .SOLUS_variable <- 'Clay'
# .rescale_value <- 1
# 
# .variable <- 'sandtotal_r'
# .SOLUS_variable <- 'Sand'
# .rescale_value <- 1
# 
# .variable <- 'ec_r'
# .SOLUS_variable <- 'EC'
# .rescale_value <- 0.1
# 
# .variable <- 'ph1to1h2o_r'
# .SOLUS_variable <- 'pH'
# .rescale_value <- 0.01




## SoilWeb style bounding boxes



# MO: Kevin's Farm
bb <- '-90.0847 37.3575,-90.0847 37.4228,-89.9681 37.4228,-89.9681 37.3575,-90.0847 37.3575'
bb <- '-90.2596 37.2571,-90.2596 37.5182,-89.7930 37.5182,-89.7930 37.2571,-90.2596 37.2571'


# MO
# bb <- '-92.5354 38.6244,-92.5354 38.8817,-91.9453 38.8817,-91.9453 38.6244,-92.5354 38.6244'
# bb <- '-91.5736 39.0029,-91.5736 39.1310,-91.2785 39.1310,-91.2785 39.0029,-91.5736 39.0029'
bb <- '-92.2418 38.7255,-92.2418 38.7497,-92.1759 38.7497,-92.1759 38.7255,-92.2418 38.7255'

# MN examples from Joe Brennan
bb <- '-96.4939 46.4092,-96.4939 46.6174,-95.8348 46.6174,-95.8348 46.4092,-96.4939 46.4092'


# Capitol Reef
# there should be abrupt changes in AWC
bb <- '-111.3555 38.2650,-111.3555 38.3944,-111.0927 38.3944,-111.0927 38.2650,-111.3555 38.2650'


# TX
bb <- '-97.3052 30.5714,-97.3052 30.8551,-96.7796 30.8551,-96.7796 30.5714,-97.3052 30.5714'


# CA630
bb <- '-121 37.9901,-121 38.1200,-120.2 38.1200,-120.2 37.9901,-121 37.9901'

# Sand Hills, NE southern interface
bb <- '-101.4416 41,-101.4416 41.3678,-100.9159 41.3678,-100.9159 41,-101.4416 41'


# Tulare Lake basin, CA
bb <- '-120.2591 35.9366,-120.2591 36.2033,-119.7335 36.2033,-119.7335 35.9366,-120.2591 35.9366'


# western Fresno county
bb <- '-120.6989 36.4359,-120.6989 36.7009,-120.1733 36.7009,-120.1733 36.4359,-120.6989 36.4359'


## check assumptions on misc. areas etc.
# high sierra
bb <- '-119.6 36.1152,-119.6 36.8857,-118.5 36.8857,-118.5 36.1152,-119.6 36.1152'


# near Chesapeake Bay
bb <- '-76.0056 38.7209,-76.0056 38.8495,-75.7428 38.8495,-75.7428 38.7209,-76.0056 38.7209'


# Mississippi river
bb <- '-90.4532 33.5122,-90.4532 33.6496,-90.1904 33.6496,-90.1904 33.5122,-90.4532 33.5122'
bb <- '-90.5847 33.4435,-90.5847 33.7183,-90.0591 33.7183,-90.0591 33.4435,-90.5847 33.4435'

# from Chad, coastal plain
bb <- '-80.3176 33.9643,-80.3176 34.0977,-79.9880 34.0977,-79.9880 33.9643,-80.3176 33.9643'


# favorite landscapes
bb <- '-90.1898 41.8027,-90.1898 41.8642,-90.0584 41.8642,-90.0584 41.8027,-90.1898 41.8027'

# Red Barn
bb <- '-119.7765 36.8951,-119.7765 36.9611,-119.6451 36.9611,-119.6451 36.8951,-119.7765 36.8951'


# LA, Gulf interface
# bb <- '-91.8066 29.4575,-91.8066 30.0305,-90.7553 30.0305,-90.7553 29.4575,-91.8066 29.4575'

# Putah Creek
bb <- '-122.2131 38.6740,-122.2131 38.8027,-121.9503 38.8027,-121.9503 38.6740,-122.2131 38.6740'


# TX045
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.53435,-101.25026,z14
bb <- '-101.4026 34.4733,-101.4026 34.6082,-101.1056 34.6082,-101.1056 34.4733,-101.4026 34.4733'

# Newport News
bb <- '-77.2112 36.8763,-77.2112 37.4023,-76.2891 37.4023,-76.2891 36.8763,-77.2112 36.8763'


# create WKT from SoilWeb style BBOX
bb <- sprintf("POLYGON((%s))", bb)
bb <- vect(bb, crs = "OGC:CRS84")

# ragg::agg_png(filename = 'MI147-texture-15cm.png', width = 2200, height = 900, scaling = 2)

z <- compareAOI(bb, figTitle = 'Newport News')

# dev.off()

# check
levelplot(c(z$clay$gNATSGO, z$clay$SOLUS), 
          scales = list(alternating = 1), 
          col.regions = magma, 
          main = sprintf('%s near 15cm', 'Clay'), 
          names.attr = c('gNATSGO', 'SOLUSv2'), 
          sub = 'EPSG 5070, 30m grid cell size',
          maxpixels = 1e6
)


## save results
writeRaster(z$clay$gNATSGO, filename = 'export/wv-gnatsgo-clay-15cm.tif')
writeRaster(z$clay$SOLUS, filename = 'export/wv-solusv2-clay-15cm.tif')

writeRaster(z$sand$gNATSGO, filename = 'export/wv-gnatsgo-sand-15cm.tif')
writeRaster(z$sand$SOLUS, filename = 'export/wv-solusv2-sand-15cm.tif')

writeRaster(z$gNATSGO.texture, filename = 'export/wv-gnatsgo-texture-15cm.tif')
writeRaster(z$SOLUS.texture, filename = 'export/wv-solusv2-texture-15cm.tif')






## any spatial data
aoi_1deg <- vect('e:/working_copies/compare-psm/AOI_management/geom/AOI_1_wgs84.shp')
aoi0.2deg <- vect('e:/working_copies/compare-psm/AOI_management/geom/AOI_0.2_wgs84.shp')

plot(aoi_1deg[3, ])
lines(aoi0.2deg[3, ])

z <- compareAOI(aoi0.2deg[3, ])


# https://r-spatial.github.io/mapview/

library(leafsync)
library(mapview)
library(raster)

gg <- raster(g.texture)
ss <- raster(s.texture)

.rat <- cats(g.texture)[[1]]
names(.rat)[1] <- 'ID'
levels(gg) <- .rat

.rat <- cats(s.texture)[[1]]
names(.rat)[1] <- 'ID'
levels(ss) <- .rat

m1 <- mapview(gg, col.regions = g.cols, method = 'ngb', layer.name = 'gNATSGO', map.types = 'OpenStreetMap')
m2 <- mapview(ss, col.regions = s.cols, method = 'ngb', layer.name = 'SOLUS', map.types = 'OpenStreetMap')

sync(m1, m2)



# https://r-spatial.github.io/slideview/





