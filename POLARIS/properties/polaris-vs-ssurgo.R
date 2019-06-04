library(raster)
library(viridis)
library(leafsync)
library(mapview)
library(snow)
library(hexbin)

# does this actually help?
beginCluster(n=8)

## all share the same grid system

polaris <- raster('S:/NRCS/Archive_Dylan_Beaudette/NSSC/gridded-maps/from-steve/Polaris_lat3940_lon10099_pH0_15.tif')
ssurgo <- raster('S:/NRCS/Archive_Dylan_Beaudette/NSSC/gridded-maps/from-steve/gSSURGO_pHwater_DCP_5to15cm_30Meter.tif')



## view different aspects of same data set
pH.leg <- seq(from=5, to=8.5, by=0.5)

m1 <- mapview(ssurgo, at=pH.leg, col.regions=viridis, layer.name='SSURGO', map.types='OpenStreetMap', maxpixels=1e6)
m2 <- mapview(polaris, at=pH.leg, col.regions=viridis, layer.name='POLARIS', map.types='OpenStreetMap', maxpixels=1e6)

sync(m1, m2)
