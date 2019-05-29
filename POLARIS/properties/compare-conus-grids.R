library(raster)
library(viridis)
library(leafsync)
library(mapview)
library(snow)
library(hexbin)

# does this actually help?
beginCluster(n=8)

## all share the same grid system

# soil grids, roughly the same depth
# pH multiplied by 10
sg <- raster('S:/NRCS/Archive_Dylan_Beaudette/SoilGrids/100m/ph_h2o_M_sl3_100m.tif')
sg <- calc(sg, function(i) i / 10.0)

# mean value within 300m cells
r300 <- raster('E:/gis_data/gSSURGO/pH_300m_AggregateMean/SoilRas_SDV_pHwater_WTA_5to15.tif')


# ISSR-800
issr <- raster('E:/gis_data/FY2019-800m-rasters/rasters/ph_025.tif')
type <- raster('E:/gis_data/FY2019-800m-rasters/rasters/survey_type_int.tif')
levels(type) <- data.frame(ID=1:2, attr=c('SSURGO', 'STATSGO'))


## view different aspects of same data set
pH.leg <- seq(from=3.5, to=8, by=0.5)

m1 <- mapview(crop(issr, r300), at=pH.leg, col.regions=viridis, layer.name='ISSR-800', map.types='OpenStreetMap')
m2 <- mapview(crop(sg, r300), at=pH.leg, col.regions=viridis, layer.name='SoilGrids+, 100m', map.types='OpenStreetMap')
m3 <- mapview(crop(type, r300), layer.name='ISSR-800, Survey Type', map.types='OpenStreetMap', method='ngb')
m4 <- mapview(r300, at=pH.leg, col.regions=viridis, layer.name='gSSURGO, 300m', map.types='OpenStreetMap')


sync(m4, m1, m2, m3)


sync(m1, m2)

sync(m1, m3)

sync(m1, m3, m2)


## CONUS overview

pH.leg <- seq(from=3, to=10, by=0.5)
m1 <- mapview(issr, at=pH.leg, col.regions=viridis, layer.name='ISSR-800', map.types='OpenStreetMap', maxpixels=1e6)
m2 <- mapview(sg, at=pH.leg, col.regions=viridis, layer.name='SoilGrids+, 100m', map.types='OpenStreetMap', maxpixels=1e6)

sync(m1, m2)


## sample / compare
# 2-passes: grids are of different cell sizes
x <- sampleRegular(sg, size = 10000, sp=TRUE)
names(x) <- 'SoilGrids_100m'

x$ISSR_100m <- extract(issr, x)

# fascinating...
png(file='compare-ISSR800-SG100-pH-splom.png', width=900, height=900, res=90, type='cairo', antialias='subpixel')

hexplom(x@data, trans=log, colramp=viridis, upper.panel = panel.hexboxplot)

dev.off()


endCluster()

