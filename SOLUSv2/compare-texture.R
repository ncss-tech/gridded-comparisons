
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


# FL
bb <- '-81.0511 27.3155,-81.0511 27.4620,-80.7560 27.4620,-80.7560 27.3155,-81.0511 27.3155'


# southern IL
bb <- '-89.2633 37.9833,-89.2633 38.0151,-89.1809 38.0151,-89.1809 37.9833,-89.2633 37.9833'


# MO
# bb <- '-92.5354 38.6244,-92.5354 38.8817,-91.9453 38.8817,-91.9453 38.6244,-92.5354 38.6244'
# bb <- '-91.5736 39.0029,-91.5736 39.1310,-91.2785 39.1310,-91.2785 39.0029,-91.5736 39.0029'
bb <- '-92.2418 38.7255,-92.2418 38.7497,-92.1759 38.7497,-92.1759 38.7255,-92.2418 38.7255'

# near Minden, NV
bb <- '-119.8402 38.9222,-119.8402 38.9863,-119.6911 38.9863,-119.6911 38.9222,-119.8402 38.9222'

# MN examples from Joe Brennan
bb <- '-96.4939 46.4092,-96.4939 46.6174,-95.8348 46.6174,-95.8348 46.4092,-96.4939 46.4092'

# College Park, MD,
bb <- '-76.9845 38.9764,-76.9845 39.0084,-76.9100 39.0084,-76.9100 38.9764,-76.9845 38.9764'

# nearby, to the west
bb <- '-77.2527 38.9417,-77.2527 39.0058,-77.1036 39.0058,-77.1036 38.9417,-77.2527 38.9417'


# TX027
bb <- '-97.4535 30.8660,-97.4535 31.1488,-96.9279 31.1488,-96.9279 30.8660,-97.4535 30.8660'
# zoom
bb <- '-96.7651 30.8599,-96.7651 30.9307,-96.6337 30.9307,-96.6337 30.8599,-96.7651 30.8599'


# TX331 | TX395
bb <- '-97.0010 30.7474,-97.0010 31.0306,-96.4754 31.0306,-96.4754 30.7474,-97.0010 30.7474'

# Capitol Reef
# there should be abrupt changes in AWC
bb <- '-111.3555 38.2650,-111.3555 38.3944,-111.0927 38.3944,-111.0927 38.2650,-111.3555 38.2650'


# TX
bb <- '-97.3052 30.5714,-97.3052 30.8551,-96.7796 30.8551,-96.7796 30.5714,-97.3052 30.5714'


# CA630
bb <- '-121 37.9901,-121 38.1200,-120.2 38.1200,-120.2 37.9901,-121 37.9901'

# sand hills, southern interface
bb <- '-101.4416 41,-101.4416 41.3678,-100.9159 41.3678,-100.9159 41,-101.4416 41'


# Tulare lake basin
bb <- '-120.2591 35.9366,-120.2591 36.2033,-119.7335 36.2033,-119.7335 35.9366,-120.2591 35.9366'


# western Fresno county
bb <- '-120.6989 36.4359,-120.6989 36.7009,-120.1733 36.7009,-120.1733 36.4359,-120.6989 36.4359'


## check assumptions on misc. areas etc.
# high sierra
bb <- '-119.6 36.1152,-119.6 36.8857,-118.5 36.8857,-118.5 36.1152,-119.6 36.1152'


# RI600
bb <- '-71.8018 41.3583,-71.8018 41.6054,-71.2762 41.6054,-71.2762 41.3583,-71.8018 41.3583'

# Valley Springs
bb <- '-121.0269 38.0948,-121.0269 38.2246,-120.7641 38.2246,-120.7641 38.0948,-121.0269 38.0948'

# near Chesapeak Bay
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

# Pinnacles National Park
bb <- '-121.3049 36.4195,-121.3049 36.5521,-121.0420 36.5521,-121.0420 36.4195,-121.3049 36.4195'

# Salinas Valley
bb <- '-121.6715 36.4873,-121.6715 36.6198,-121.4087 36.6198,-121.4087 36.4873,-121.6715 36.4873'

# Kings River alluvial fan / outwash sequences
bb <- '-119.7997 36.6051,-119.7997 36.8695,-119.2741 36.8695,-119.2741 36.6051,-119.7997 36.6051'

# Gabbro + vertisols near Sanger, CA
bb <- '-119.5323 36.6515,-119.5323 36.7837,-119.2695 36.7837,-119.2695 36.6515,-119.5323 36.6515'


# Turlock Lake, CA
bb <- '-120.7047 37.5502,-120.7047 37.6808,-120.4419 37.6808,-120.4419 37.5502,-120.7047 37.5502'

# Chico, CA
bb <- '-121.9556 39.6609,-121.9556 39.7878,-121.6928 39.7878,-121.6928 39.6609,-121.9556 39.6609'

# Sacramento River, Glenn | Butte co. boundary
bb <- '-122.1354 39.5018,-122.1354 39.6290,-121.8725 39.6290,-121.8725 39.5018,-122.1354 39.5018'

# Putah Creek
bb <- '-122.2131 38.6740,-122.2131 38.8027,-121.9503 38.8027,-121.9503 38.6740,-122.2131 38.6740'


# Sacramento River, Glenn County
# major join issues
# https://casoilresource.lawr.ucdavis.edu/soil-properties/?prop=texture_025&lat=39.4129&lon=-122.0746&z=9
bb <- '-122.1755 39.3733,-122.1755 39.5007,-121.8804 39.5007,-121.8804 39.3733,-122.1755 39.3733'


# WI073
bb <- '-89.9464 44.9456,-89.9464 45.1578,-89.4448 45.1578,-89.4448 44.9456,-89.9464 44.9456'

# MI147
bb <- '-82.8417 42.9094,-82.8417 43.1291,-82.3401 43.1291,-82.3401 42.9094,-82.8417 42.9094'

# VA037 | VA147
# https://casoilresource.lawr.ucdavis.edu/gmap/
bb <- '-78.6197 37.0657,-78.6197 37.1972,-78.3892 37.1972,-78.3892 37.0657,-78.6197 37.0657'

# VA653
bb <- '-77.7619 37.0396,-77.7619 37.1711,-77.5314 37.1711,-77.5314 37.0396,-77.7619 37.0396'

# VA800
bb <- '-76.7537 36.6033,-76.7537 36.7356,-76.5232 36.7356,-76.5232 36.6033,-76.7537 36.6033'


# VA, Newport News
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.14144,-76.75289,z11
bb <- '-77.2112 36.8763,-77.2112 37.4023,-76.2891 37.4023,-76.2891 36.8763,-77.2112 36.8763'


# Pawnee Lake, NE
bb <- '-96.8927 40.8209,-96.8927 40.8833,-96.7774 40.8833,-96.7774 40.8209,-96.8927 40.8209'


# Renville County, MN
bb <- '-95.1268 44.6068,-95.1268 44.8413,-94.6658 44.8413,-94.6658 44.6068,-95.1268 44.6068'


# Blacklands, TX
# c/o Edwin Winzler
# 
bb <- '-97.4697 30.9909,-97.4697 31.1322,-97.2391 31.1322,-97.2391 30.9909,-97.4697 30.9909'

## KS155
# John Warner
bb <- '-98.1161 37.8675,-98.1161 38.0816,-97.5998 38.0816,-97.5998 37.8675,-98.1161 37.8675'
bb <- '-98.5020 37.9187,-98.5020 38.2026,-98.0846 38.2026,-98.0846 37.9187,-98.5020 37.9187'

## MD001
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.62949,-78.56873,z12
# Aaron Friend
bb <- '-78.6990 39.5652,-78.6990 39.6923,-78.4482 39.6923,-78.4482 39.5652,-78.6990 39.5652'

# MA | WV, Potomac River
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.5818,-78.426,z11
bb <- '-78.5514 39.5182,-78.5514 39.6454,-78.3006 39.6454,-78.3006 39.5182,-78.5514 39.5182'

# Manhattan, KS
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.17325,-96.51421,z11
bb <- '-97.0893 39.0262,-97.0893 39.3938,-96.0347 39.3938,-96.0347 39.0262,-97.0893 39.0262'




# create WKT from SoilWeb style BBOX
bb <- sprintf("POLYGON((%s))", bb)
bb <- vect(bb, crs = "OGC:CRS84")

# ragg::agg_png(filename = 'MI147-texture-15cm.png', width = 2200, height = 900, scaling = 2)

z <- compareAOI(bb, figTitle = 'Chico, CA')

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





