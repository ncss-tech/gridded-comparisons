## have to do this on local machine for now... can't install rgdal on soilweb server

## make comparisons by tile
library(raster)
library(e1071)
library(plyr)
library(maps)
library(maptools)

# sample tiles
# source('sample-tiles.R')

# load sampled data
load('samples.rda')

# quick summary
summary(d[, 1:4])


# get polygons
l.p <- list()
for(i in 1:nrow(d)){
  p.i <- Polygon(with(d[i, ],
                      matrix(c(x0, y0, 
                               x0, y1,
                               x1, y1,
                               x1, y0,
                               x0, y0), ncol=2, byrow=TRUE)
  ))

  spdf.i <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(p.i), ID=i)), proj4string=CRS('+proj=longlat +datum=NAD83')), data=d[i, 1:4])
  
  l.p[[i]] <- spdf.i
}

d.sp <- do.call('rbind', l.p)

map('usa')
plot(d.sp, add=TRUE)


# get conus map
m <- map('usa')
m.ids <- 1:length(maptools:::.NAmat2xyList(cbind(m$x, m$y)))
usa <- map2SpatialPolygons(m, IDs=m.ids)
proj4string(usa) <- '+proj=longlat +datum=NAD83'

pdf(file='tile-metrics.pdf', width=10, height=8)
spplot(d.sp, zcol='diag.1', sp.layout=list('sp.polygons', usa), main='Percent Correctly Classified', col.regions=rainbow(100, start=0, end=0.9), scales=list(tick.number=10))
spplot(d.sp, zcol='kappa.1', sp.layout=list('sp.polygons', usa), main='Kappa', col.regions=rainbow(100, start=0, end=0.8), scales=list(tick.number=10))
spplot(d.sp, zcol='crand.1', sp.layout=list('sp.polygons', usa), main='Corrected Rand Index', col.regions=rainbow(100, start=0, end=0.8), scales=list(tick.number=10))
dev.off()




