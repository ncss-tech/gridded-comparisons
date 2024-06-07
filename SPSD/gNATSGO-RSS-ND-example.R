library(terra)
library(soilDB)
library(sf)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=48.01037,-98.31562,z15
bb <- '-98.3466 47.9967,-98.3466 48.0232,-98.2844 48.0232,-98.2844 47.9967,-98.3466 47.9967'

bb <- '-98.7274 48.7777,-98.7274 48.7907,-98.6962 48.7907,-98.6962 48.7777,-98.7274 48.7777'

## no RSS here, but linework looks similar to RSS -> SSURGO conversion
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=47.23227,-99.77036,z14
bb <- '-99.8344 47.2056,-99.8344 47.2595,-99.7099 47.2595,-99.7099 47.2056,-99.8344 47.2056'


## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
# WGS84 GCS
x <- vect(wkt, crs = 'epsg:4326')

ssurgo <- mukey.wcs(x, db = 'gSSURGO', res = 30)
gnatsgo <- mukey.wcs(x, db = 'gNATSGO', res = 30)
rss <-  mukey.wcs(x, db = 'RSS', res = 10)

p <- SDA_spatialQuery(ssurgo, what = 'mupolygon', geomIntersection = TRUE)

# transform to AEA coordinate reference system used by gSSURGO / gNATSGO
# this is EPSG:5070
p <- project(p, crs(ssurgo))

par(mfcol = c(1, 3))
plot(ssurgo, axes = FALSE, legend = FALSE, col = hcl.colors(100, 'mako'), main = 'gSSURGO (30m)')
plot(gnatsgo, axes = FALSE, legend = FALSE, col = hcl.colors(100, 'mako'), main = 'gNATSGO (30m)')
plot(rss, axes = FALSE, legend = FALSE, col = hcl.colors(100, 'mako'), main = 'RSS (10m)')

par(mfcol = c(1, 2))

plot(rss, legend = FALSE, axes = FALSE, col = hcl.colors(100, 'mako'), mar = c(1, 1, 1, 1))
lines(p)

plot(gnatsgo, legend = FALSE, axes = FALSE, col = hcl.colors(100, 'mako'), mar = c(1, 1, 1, 1))
lines(p)
