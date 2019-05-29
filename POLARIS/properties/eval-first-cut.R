library(raster)
library(hexbin)
library(rasterVis)
library(snow)
library(viridis)
library(leafsync)
library(mapview)

beginCluster(n=4)


## all share the same grid system

# soil grids, roughly the same depth
# pH multiplied by 10
sg <- raster('S:/NRCS/Archive_Dylan_Beaudette/SoilGrids/100m/ph_h2o_M_sl3_100m.tif')

# ISSR-800
issr <- raster('E:/gis_data/FY2019-800m-rasters/rasters/ph_025.tif')
type <- raster('E:/gis_data/FY2019-800m-rasters/rasters/survey_type_int.tif')

# WI rasters from Steve, gSSURGO (?)
r30 <- raster('E:/gis_data/gSSURGO/SoilRas_SDV_pHwater_WTA_5to15.tif')
# mean value within 300m cells
r300 <- raster('E:/gis_data/gSSURGO/pH_300m_AggregateMean/SoilRas_SDV_pHwater_WTA_5to15.tif')

# POLARIS properties, mean, 5-15cm pH
polaris <- raster('E:/gis_data/POLARIS/polaris-mean-pH-5-15-100m.tif')

# tiny region of interest
e <- extent(c(295043, 403318, 2524882, 2593041))

# WI
# e <- extent(r300)

# crop for faster processing
r30 <- crop(r30, e)
r300 <- crop(r300, e)
issr <- crop(issr, e)
type <- crop(type, e)
sg <- crop(sg, e)
polaris <- crop(polaris, e)

# convert soilgrids pH to actual scale
sg <- sg / 10.0

# resample / stack for combined visualization
s <- stack(r30, resample(r300, r30), resample(issr, r30), resample(sg, r30), resample(polaris, r30))
names(s) <- c('gSSURGO 30m', 'gSSURGO 300m', 'ISSR 800m', 'SoilGrids 100', 'POLARIS (100m)')

## differences between source vs POLARIS
hist(s[[1]] - s[[5]], breaks=50)


# not so quick map
png(file='compare-grids.png', width=1200, height=800, type='cairo', antialias='subpixel')

levelplot(s, maxpixels=1e6, col.regions=viridis(14), cuts=13, margin=FALSE, scales=list(y=list(draw=FALSE), x=list(draw=FALSE)), layout=c(3,2))

dev.off()

# not so quick map
png(file='s30-vs-polaris-100.png', width=1800, height=2400, res=90, type='cairo', antialias='subpixel')

levelplot(s[[c(1,5)]], maxpixels=1e6, col.regions=viridis(14), cuts=13, margin=FALSE, scales=list(y=list(draw=FALSE), x=list(draw=FALSE)), layout=c(1,2))
png(file='s300-vs-polaris-100.png', width=1800, height=2400, res=90, type='cairo', antialias='subpixel')

levelplot(s[[c(2,5)]], maxpixels=1e6, col.regions=viridis(14), cuts=13, margin=FALSE, scales=list(y=list(draw=FALSE), x=list(draw=FALSE)), layout=c(1,2))

dev.off()

dev.off()




## TODO: compare original data at full extent
## sample and compare
x <- sampleRegular(s, size = 5000, sp=TRUE)
head(x@data, 30)

# fascinating...
png(file='compare-grids-splom.png', width=1000, height=1000, res=90, type='cairo', antialias='subpixel')

hexplom(x@data, trans=log, colramp=viridis, upper.panel = panel.hexboxplot)

dev.off()


## TODO: compare differences
levelplot((s[[1]] - s[[5]]), maxpixels=1e5, col.regions=viridis(14), cuts=13, margin=FALSE, scales=list(y=list(draw=FALSE), x=list(draw=FALSE)))

## thoughts:
# * a water mask at suitable resolution might be a good idea


# variance across scale / product
sapply(x@data, sd, na.rm=TRUE)

# NA across scale / product
lapply(x@data, function(i) table(is.na(i)))


# hmm.. back-filling with STATSGO due to water features... is this ideal?
plot(type)



## view different aspects of same data set
pH.leg <- seq(from=3.5, to=7.25, by=0.25)
m1 <- mapview(r30, at=pH.leg, col.regions=viridis, layer.name='gSSURGO, 30m', map.types='OpenStreetMap')
m2 <- mapview(r300, at=pH.leg, col.regions=viridis, layer.name='gSSURGO, 300m', map.types='OpenStreetMap')
m3 <- mapview(issr, at=pH.leg, col.regions=viridis, layer.name='ISSR-800', map.types='OpenStreetMap')
m4 <- mapview(sg, at=pH.leg, col.regions=viridis, layer.name='SoilGrids+, 100m', map.types='OpenStreetMap')
m5 <- mapview(polaris, at=pH.leg, col.regions=viridis, layer.name='POLARIS, resampled to 100m', map.types='OpenStreetMap')

sync(m1, m2, m4, m5) # 4 panels synchronised



endCluster()

